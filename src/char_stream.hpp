#pragma once

#include <cassert>
#include <coroutine>
#include <cstring>
#include <exception>
#include <string>
#include <string_view>
#include <utility>

namespace detail {
template <class T>
concept Searchable = requires(T obj) {
  { std::declval<std::string>().find(obj) } -> std::same_as<size_t>;
  { std::size(obj) } -> std::same_as<size_t>;
};
template <class T>
concept MultiSearchable = requires(T obj) {
  { std::declval<std::string>().find_first_of(obj) } -> std::same_as<size_t>;
};
} // namespace detail

// Buffered stream of chars, may or may not be finite, but better be
struct char_stream {

  struct overwriter {
    size_t size;
    const char *buf;
    size_t read_bytes;

    inline auto operator()(char *ptr, [[maybe_unused]] size_t count) const
        -> size_t {
      assert(size + read_bytes <= count);
      memcpy(ptr + size, buf, read_bytes);
      return size + read_bytes;
    }
  };

  struct promise_type;
  using handle_type = std::coroutine_handle<promise_type>;

  enum class status {
    reading,
    failed,
    eof,
  };

  struct promise_type {

    std::suspend_never initial_suspend() noexcept { return {}; }
    std::suspend_always final_suspend() noexcept { return {}; }
    auto get_return_object() noexcept {
      return handle_type::from_promise(*this);
    }
    void unhandled_exception() noexcept { std::terminate(); }

    void return_value(bool success) noexcept {
      status_ = success ? status::eof : status::failed;
    }

    std::suspend_always yield_value(std::string_view sv) {

      buffer_.resize_and_overwrite(buffer_.size() + sv.size(),
                                   overwriter{.size = buffer_.size(),
                                              .buf = sv.data(),
                                              .read_bytes = sv.size()});

      if (on_data_available != nullptr) {
        auto to_resume = on_data_available;
        on_data_available = nullptr;
        to_resume.resume();
      }
      return {};
    }

    void prune(size_t first) {
      assert(first <= buffer_.size());
      auto new_size = buffer_.size() - first;
      for (int i = 0; i < new_size; ++i) {
        buffer_[i] = buffer_[first+i];
      }
      buffer_.resize(new_size);
    }

    status status_{status::reading};

    std::string buffer_;
    std::coroutine_handle<> on_data_available;
  };

  explicit(false) char_stream(handle_type h) noexcept
      : handle_{h}, current_{0} {}
  char_stream(const char_stream &other) = delete;
  char_stream(char_stream &&other) noexcept
      : handle_{std::exchange(other.handle_, nullptr)},
        current_{other.current_} {}
  char_stream &operator=(const char_stream &other) = delete;
  char_stream &operator=(char_stream &&other) noexcept {
    this->handle_ = std::exchange(other.handle_, nullptr);
    this->current_ = other.current_;
    return *this;
  }
  ~char_stream() noexcept {
    if (handle_ != nullptr) {
      handle_.destroy();
    }
  }

  size_t cursor() const noexcept { return current_; }
  bool seek(size_t pos) noexcept {
    force_resize_(pos);
    if (pos > buf_().size()) {
      current_ = buf_().size();
      return false;
    }
    current_ = pos;
    return true;
  }

  template<std::invocable<char> F> requires (std::is_convertible_v<std::invoke_result_t<F, bool>, bool>)
  bool seek(F&& func) {
    auto pos = find(std::forward<F>(func));
    if (pos == std::string::npos) {
      current_ = buf_().size();
      return false;
    }
    current_ = pos;
    return true;
  }

  size_t advance(size_t n = 1) noexcept {
    auto last = current_;
    seek(current_ + n);
    return current_ - last;
  }

  char peek() const noexcept {
    assert(handle_ != nullptr);
    assert(can_peek_());

    if (current_ == buf_().size()) {
      handle_.resume();
    }

    // technically .data() doesn't guarantee 0 termination
    // and we may be at the end of the buffer
    return buf_().c_str()[current_];
  }

  char peek_at(size_t n) noexcept {
    if (!force_resize_(n)) {
      return 0;
    }
    return buf_()[n];
  }

  char read_char() noexcept {
    assert(handle_ != nullptr);
    assert(can_peek_());

    char c = peek();
    ++current_;
    return c;
  }

  void drop_to(size_t n) {
    assert(handle_ != nullptr);
    assert(buf_().size() >= n);
  }

  template<std::invocable<char> F>
  size_t find(F&& func, size_t beg) const noexcept {
    assert(handle_ != nullptr);

    auto pos = beg;
    while (pos < buf_().size() || !handle_.done()) {
      if (pos >= buf_().size()) {
        handle_.resume();
      }
      if (pos >= buf_().size()) {
        continue;
      }

      if (func(buf_()[pos])) {
        return pos;
      }
      pos++;
    }
    return std::string::npos;
  }

  template<std::invocable<char> F>
  size_t find(F&& func) const noexcept {
    return find(std::forward<F>(func), current_);
  }

  size_t find(char chr) const noexcept {
    assert(handle_ != nullptr);

    auto pos = buf_().find(chr, current_);
    while (pos == std::string::npos) {
      pos = buf_().size();
      if (!handle_.done()) {
        handle_.resume();
        pos = buf_().find(chr, pos);
      } else {
        return std::string::npos;
      }
    }
    return pos;
  }

  template <class T>
    requires detail::Searchable<T>
  size_t find(T &&chr) const noexcept {
    assert(handle_ != nullptr);

    auto pos = buf_().find(chr, current_);
    auto searched_size = std::size(chr);

    while (pos == std::string::npos) {
      pos = buf_().size();
      if (!handle_.done()) {
        handle_.resume();
        auto start = searched_size > pos ? 0 : pos - searched_size;
        pos = buf_().find(chr, start);
      } else {
        return std::string::npos;
      }
    }
    return pos;
  }

  std::string_view peek(size_t n) noexcept {
    assert(handle_ != nullptr);
    assert(can_peek_());
    return std::string_view{begin_(), n};
  }

  template <typename T>
    requires detail::MultiSearchable<T>
  size_t find_first_of(T &&s) noexcept {
    assert(handle_ != nullptr);
    auto pos = buf_().find_first_of(s, current_);
    while (pos == std::string::npos) {
      pos = buf_().size();
      if (!handle_.done()) {
        handle_.resume();
        pos = buf_().find_first_of(s, pos);
      } else {
        return std::string::npos;
      }
    }

    return pos;
  }

  std::string_view substring(size_t first, size_t n) {
    auto pos = force_resize_(first + n);
    return substring_(first, pos);
  }

  template <typename T>
    requires detail::MultiSearchable<T>
  size_t find_first_not_of(T &&s) noexcept {
    assert(handle_ != nullptr);
    auto pos = buf_().find_first_not_of(s, current_);
    while (pos == std::string::npos) {
      pos = buf_().size();
      if (!handle_.done()) {
        handle_.resume();
        pos = buf_().find_first_not_of(s, pos);
      } else {
        return std::string::npos;
      }
    }

    return pos;
  }

  std::string_view consume(size_t n) noexcept {
    assert(handle_ != nullptr);
    auto last = current_;
    current_ = force_resize_(current_ + n);
    return substring_(last, current_);
  }

  std::string_view consume_to(size_t pos) noexcept {
    assert(handle_ != nullptr);
    auto last = current_;
    current_ = force_resize_(pos);
    return substring_(last, current_);
  }

  std::string_view readline() noexcept {
    assert(handle_ != nullptr);
    auto start = current_;
    current_ = find('\n');
    if (current_ != std::string::npos) {
      current_++;
    } else {
      current_ = buf_().size();
    }
    return substring_(start, current_);
  }

  std::string_view peek_line() noexcept {
    auto cur = current_;
    auto result = readline();
    current_ = cur;
    return result;
  }
  std::string_view peek_to(size_t n) noexcept {
    n = force_resize_(n);
    return substring_(current_, n);
  }

  operator bool() const noexcept {
    assert(handle_ != nullptr);
    return !at_eos();
  }

  bool at_eos() const noexcept {
    assert(handle_ != nullptr);
    if (current_ < buf_().size()) {
      return false;
    }
    if (handle_.done()) {
      return true;
    }
    handle_.resume();
    return current_ == buf_().size();
  }

  void prune() {
    p_().prune(current_);
    current_ = 0;
  }

  bool buffer_end() const {
    return current_ == buf_().size();
  }

  auto operator co_await() {
    struct awaitable {
      char_stream* self;
      bool await_ready() noexcept { return !self->buffer_end(); }
      bool await_resume() { return !self->at_eos(); }
      void await_suspend(std::coroutine_handle<> caller) {
        self->p_().on_data_available = caller;
      }
    };
    return awaitable{this};
  }

private:
  bool stream_ended_() const noexcept {
    assert(handle_ != nullptr);
    return handle_.done() || handle_.promise().status_ != status::reading;
  }

  promise_type &p_() const noexcept { return handle_.promise(); }
  std::string &buf_() const noexcept { return p_().buffer_; }

  inline bool can_peek_() const noexcept {
    return (current_ < buf_().size() || !handle_.done());
  }
  inline const char *begin_() const noexcept {
    return buf_().data() + current_;
  }

  inline std::string_view substring_(size_t start, size_t stop) const noexcept {
    if (stop > buf_().size()) {
      stop = buf_().size();
    }
    return std::string_view{buf_().data() + start, stop - start};
  }

  inline size_t force_resize_(size_t pos) const noexcept {
    while (pos >= buf_().size() && !handle_.done()) {
      handle_.resume();
    }
    return pos < buf_().size() ? pos : buf_().size();
  }

  handle_type handle_;
  size_t current_;
};

char_stream slurp_file(const char *filename);
char_stream slurp_file(const std::string& filename);
char_stream standard_input();
char_stream string_stream(std::string_view sv);

