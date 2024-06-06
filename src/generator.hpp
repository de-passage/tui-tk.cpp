#pragma once

#include <coroutine>
#include <exception>
#include <type_traits>

namespace dpsg {

template <class T, class R = void> struct generator {
  struct promise_type;
  using handle_type = std::coroutine_handle<promise_type>;

  // Static coroutine members would yield unresolvable warnings on usage
  // NOLINTBEGIN(readability-convert-member-functions-to-static)
  struct promise_type {
    using result_type = R;

    auto initial_suspend() noexcept -> std::suspend_always { return {}; }
    auto final_suspend() noexcept -> std::suspend_always { return {}; }

    auto get_return_object() noexcept -> generator {
      return generator{handle_type::from_promise(*this)};
    }
    void unhandled_exception() { exception_ = std::current_exception(); }

    template<class U> requires std::is_convertible_v<U, T>
    auto yield_value(U &&str) -> std::suspend_always {
      value = std::forward<U>(str);
      return {};
    }

    T value;
    R return_val;
    std::exception_ptr exception_;

    void return_value(R&& val) {
      return_val = std::forward<T>(val);
    }
  };
  // NOLINTEND(readability-convert-member-functions-to-static)

  constexpr explicit generator(handle_type handler) noexcept : h_(handler) {}
  constexpr generator(const generator &other) = delete;
  constexpr generator(generator &&other) noexcept = default;
  constexpr auto operator=(const generator &other) -> generator & = delete;
  auto operator=(generator &&other) noexcept -> generator & = default;

  constexpr operator bool() {
    fill_();
    return !h_.done();
  }
  auto operator()() -> T && {
    fill_();
    full_ = false;
    return std::move(h_.promise().value);
  }

  ~generator() { h_.destroy(); }

private:
  handle_type h_;
  bool full_ = false;
  void fill_() {
    if (!full_) {
      h_();
      if (h_.promise().exception_) {
        std::rethrow_exception(h_.promise().exception_);
      }
      full_ = true;
    }
  }
};

template<class T>
struct generator<T, void> {
  struct promise_type;
  using handle_type = std::coroutine_handle<promise_type>;

  // Static coroutine members would yield unresolvable warnings on usage
  // NOLINTBEGIN(readability-convert-member-functions-to-static)
  struct promise_type {
    auto return_void() noexcept {}
    auto initial_suspend() noexcept -> std::suspend_always { return {}; }
    auto final_suspend() noexcept -> std::suspend_always { return {}; }

    auto get_return_object() noexcept -> generator {
      return generator{handle_type::from_promise(*this)};
    }
    void unhandled_exception() { exception_ = std::current_exception(); }
    template<class U> requires std::is_convertible_v<U, T>
    auto yield_value(U &&str) -> std::suspend_always {
      value = std::forward<U>(str);
      return {};
    }

    T value;
    std::exception_ptr exception_;
  };
  // NOLINTEND(readability-convert-member-functions-to-static)

  constexpr explicit generator(handle_type handler) noexcept : h_(handler) {}
  constexpr generator(const generator &other) = delete;
  constexpr generator(generator &&other) noexcept = default;
  constexpr auto operator=(const generator &other) -> generator & = delete;
  auto operator=(generator &&other) noexcept -> generator & = default;

  constexpr operator bool() {
    fill_();
    return !h_.done();
  }
  auto operator()() -> T && {
    fill_();
    full_ = false;
    return std::move(h_.promise().value);
  }

  ~generator() { h_.destroy(); }

private:
  handle_type h_;
  bool full_ = false;
  void fill_() {
    if (!full_) {
      h_();
      if (h_.promise().exception_) {
        std::rethrow_exception(h_.promise().exception_);
      }
      full_ = true;
    }
  }
};

} // namespace dpsg
