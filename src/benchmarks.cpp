#include "char_stream.hpp"
#include "linux_term.hpp"
#include "print_events.hpp"
#include "vt100.hpp"
#include <atomic>
#include <chrono>
#include <coroutine>
#include <fmt/core.h>
#include <fstream>
#include <sys/poll.h>
#include <thread>
#include <vector>

struct task {
  struct promise_type;
  using handle_type = std::coroutine_handle<promise_type>;

  struct promise_type {
    task get_return_object() noexcept {
      return {handle_type::from_promise(*this)};
    }
  };

  task(handle_type h) : handle_{h} {}
  task(const task &other) = delete;
  task(task &&other) noexcept
      : handle_{std::exchange(other.handle_, nullptr)} {}
  task &operator=(const task &other) = delete;
  task &operator=(task &&other) noexcept {
    handle_ = std::exchange(other.handle_, nullptr);
    return *this;
  }

private:
  handle_type handle_;
};

template <class E> struct basic_ui_thread {
  using event_t = E;

  template <class F> static void start(F &&print) {
    assert(!th_.joinable());
    th_ = std::thread{[print = std::forward<F>(print)](auto (*dequeue)(...)) {

                      },
                      &dequeue};
  }

  static void join() { th_.join(); }

  static void enqueue(event_t ev) {
    std::lock_guard lock{mut_};
    events_.emplace_back(std::move(ev));
  }

private:
  static std::vector<event_t> dequeue() {
    std::lock_guard lock{mut_};
    return std::move(events_);
  }
  static std::thread th_;
  static std::vector<event_t> events_;
  static std::mutex mut_;
};
template <class E> std::thread basic_ui_thread<E>::th_{};
template <class E> std::vector<E> basic_ui_thread<E>::events_;
template <class E> std::mutex basic_ui_thread<E>::mut_;
using ui_thread = basic_ui_thread<std::coroutine_handle<>>;

struct task_on_ui_thread {
  struct promise_type;
  using handle_type = std::coroutine_handle<promise_type>;
  struct promise_type {
    std::suspend_never initial_suspend() noexcept { return {}; }
    std::suspend_never final_suspend() noexcept { return {}; }
    task_on_ui_thread get_return_object() noexcept { return {}; }
    auto await_transform(ui_thread) {
      struct awaitable {
        bool await_ready() noexcept { return false; }
        void await_suspend(std::coroutine_handle<> h) noexcept {
          ui_thread::enqueue(h);
        }
        void await_resume() noexcept {}
      };
      return awaitable{};
    }
  };
};

struct color {
  u8 r{0};
  u8 g{0};
  u8 b{0};
  u8 a{0};
};

bool operator==(const color &left, const color &right) {
  return left.r == right.r && left.b == right.b && left.g == right.g;
}

u8 lerp(u8 source, u8 target, u8 distance) {
  double nsource = (double)(source) / 255.;
  double ntarget = (double)(target) / 255.;
  double ndistance = (double)(distance) / 255.;
  return (u8)((source * distance + target * (1 - distance)) * 255);
}

color lerp(color source, color target, u8 distance) {
  return color{
      .r = lerp(source.r, target.r, distance),
      .g = lerp(source.g, target.g, distance),
      .b = lerp(source.b, target.b, distance),
      .a = lerp(source.a, target.a, distance),
  };
}

struct pixel {
  char code[5] = {0, 0, 0, 0, 0};
  color bg{.r = 0, .g = 0, .b = 0, .a = 0};
  color fg{.r = 255, .g = 255, .b = 255, .a = 255};
  i8 z{std::numeric_limits<i8>::min()};
  struct flags {
    bool bold : 1 = false;
    bool faint : 1 = false;
    bool underline : 1 = false;
    bool reverse : 1 = false;
    i8 padding : 4 = 0;

    friend bool operator==(flags left, flags right) {
      return *(i8 *)&left == *(i8 *)&right;
    }
  } flags;

  pixel &character(char c) {
    code[0] = c;
    code[1] = 0;
    return *this;
  }

  template <size_t S>
    requires(S <= 4)
  pixel &character(char (&c)[S]) {
    for (int i = 0; i < S; ++i) {
      code[i] = c[i];
    }
    code[S] = 0;
    return *this;
  }

  friend bool same_colors(pixel left, pixel right) {
    return left.bg == right.bg && left.fg == right.fg && left.flags == right.flags;
  }

  static bool compare_code_(const pixel &left, const pixel &right) {
    return strcmp(left.code, right.code) == 0;
  }
};

bool is_unicode(char code) { return (((u32)code >> 6) & 0b11) == 0b11; }

u8 codepoint_count(char code) { return std::countl_one((u8)code); }

struct screen {

private:
  static void window_resized(int) { tainted_.store(true); }
  screen(const screen &other) = delete;
  screen(screen &&other) noexcept = delete;
  screen &operator=(const screen &other) = delete;
  screen &operator=(screen &&other) noexcept = delete;

  dpsg::terminal_size size_;
  std::vector<pixel> pixels_;

  static std::atomic<bool> tainted_;
  static screen *screen_; // there's only one

  void resize() {
    size_ = dpsg::get_terminal_size();
    if (size_.row == 0 || size_.col == 0) {
      std::cerr << "Failed to retrieve terminal size. Is stdout a tty?"
                << std::endl;
      exit(1);
    }
    tainted_.store(false);
    pixels_.resize(size().row * size().col);
  }

  auto &_at(u16 x, u16 y) {
    const auto idx = x + y * size().col;
    assert(idx < pixels_.size());
    return pixels_[idx];
  }
  auto _at(u16 x, u16 y) const {
    const auto idx = x + y * size().col;
    assert(idx < pixels_.size());
    return pixels_[idx];
  }

public:
  screen() {
    assert(screen_ == nullptr);
    screen_ = this;
    resize();
    std::signal(SIGWINCH, window_resized);
  }

#define DPSG_USE_STRING

  void redraw(auto &&f) {
    if (tainted_.load()) {
      screen_->resize();
    }
    f(*this);
    static std::string output;
    output.clear();
    output.reserve(64 * size().row * size().col);
    output += "\033[0m\033[1;1H";
    auto last = pixel{};

    static int counter = 0;
    static double average = 0;
    static std::fstream LOG{"similars.txt", std::ios_base::out};
    int same = 0;
    int different = 0;

    for (auto i = 0; i < size().row; ++i) {
      for (auto j = 0; j < size().col; ++j) {
        auto &p = _at(j, i);

        if (!same_colors(p, last)) {
          different++;
          fmt::format_to(std::back_inserter(output),
                         "\033[0;48;2;{};{};{};38;2;{};{};{}", p.bg.r, p.bg.g,
                         p.bg.b, p.fg.r, p.fg.g, p.fg.b);
          if (p.flags.bold) {
            output += (";1");
          } else if (p.flags.faint) {
            output += (";2");
          }
          if (p.flags.reverse) {
            output += (";7");
          }
          if (p.flags.underline) {
            output += (";4");
          }
          if (p.code[0] == 033 || p.code[0] == 0) {
            output += ("m ");
          } else {
            fmt::format_to(std::back_inserter(output), "m{}", p.code);
          }
        } else {
          same++;
          if (p.code[0] == 033 || p.code[0] == 0) {
            output += (" ");
          } else {
            fmt::format_to(std::back_inserter(output), "{}", p.code);
          }
        }
        last = p;
      }
    }
    fmt::print("{}", output);
  }

  void redraw() {
    redraw([](auto &&) {});
  }

  auto set_pixel(u16 x, u16 y, pixel p) {
    auto &init = _at(x, y);
    if (p.z >= init.z) {
      init = p;
    }
  }

  auto clear(pixel pix) {
    for (auto &p : pixels_) {
      p = pix;
      p.z = std::numeric_limits<i8>::min();
    }
  }

  void write(u16 x, u16 y, std::string_view sv) {
    auto pos = pixels_.begin() + y * this->size().col + x;
    for (size_t i = 0; i < sv.size(); ++i, ++pos) {
      char code = sv[i];
      if (code == 033) {
        continue;
      }
      u8 nr_codepoints = codepoint_count(code);
      if (nr_codepoints == 0) {
        nr_codepoints++;
      }

      for (int j = i, x = 0; j < sv.size() && x < nr_codepoints; ++j, ++x) {
        pos->code[x] = sv[j];
      }
      pos->code[nr_codepoints] = 0;
      i += (nr_codepoints - 1);
    }
  }

  void write(u16 x, u16 y, char c) {
    auto &r = _at(x, y);
    r.code[0] = c;
    r.code[1] = 0;
  }

  dpsg::terminal_size size() const noexcept { return size_; }
};
std::atomic<bool> screen::tainted_{false};
std::atomic<bool> running_{false};
screen *screen::screen_{nullptr};

struct box {
  dpsg::term_position position{.x = 0, .y = 0};
  u16 height = 10;
  u16 width = 20;

  color bg{.r = 0, .g = 0, .b = 0, .a = 255};
  color fg{.r = 255, .g = 255, .b = 255, .a = 0};

  i8 z;

  box &at(dpsg::term_position p) {
    position = p;
    return *this;
  }
  box &fg_color(color p) {
    fg = p;
    return *this;
  }
  box &bg_color(color p) {
    bg = p;
    return *this;
  }
  box &z_index(i8 p) {
    z = p;
    return *this;
  }
  box &dimensions(dpsg::term_position p) {
    height = p.row;
    width = p.col;
    return *this;
  }
};

void display_ui();
void basic_test();
void reference_test();
void reference_color_test();

int main(int argc, char *argv[]) {
  dpsg::with_raw_mode([](dpsg::cbreak_mode_context &ctx) {
    std::thread ui_thread{display_ui};

    auto input = ctx.event_stream(standard_input());
    while (input) {
      auto event = input();
      using namespace dpsg::term_events;
      if (event == ctrl + 'd') {
        break;
      } else if (event == "z"_key) {
      }
    }

    running_.store(false);
    ui_thread.join();
  });
}

struct ui_representation {
  std::vector<box> boxes;
  color bg_color;
  color fg_color;
};

void display_loop(auto &&f) {
  screen s;
  std::cout << dpsg::vt100::hide_cursor;
  running_.store(true);
  while (running_.load()) {
    auto start_time = std::chrono::high_resolution_clock::now();
    s.redraw(f);
    auto end_time = std::chrono::high_resolution_clock::now();
    constexpr auto target_microsec = 1000000. / 60.;
    auto diff = std::chrono::microseconds(static_cast<long>(target_microsec)) -
                (end_time - start_time);
    if (diff > std::chrono::milliseconds(0)) {
      std::this_thread::sleep_for(diff);
    }
  }
  std::cout << dpsg::vt100::show_cursor;
}

void fps_counting_loop(auto &&f) {
  screen s;
  std::cout << dpsg::vt100::hide_cursor;
  running_.store(true);
  using namespace std::chrono;
  auto last_print = high_resolution_clock::now();

  std::fstream LOG{"timings.log", std::ios_base::out};

  nanoseconds average{0};
  int count = 0;

  while (running_.load()) {
    auto start_time = high_resolution_clock::now();
    s.redraw(f);
    auto end_time = high_resolution_clock::now();

    if (count == 0) {
      average = (end_time - start_time);
      count = 1;
    } else {
      average = ((average * count) + (end_time - start_time)) / (count + 1);
      count++;
    }
    if (end_time - last_print > 1s) {
      last_print = end_time;
      LOG << "Average frame time (ns): " << average
          << "\nAverage FPS: " << (1s / average) << std::endl;
    }
  }
  std::cout << dpsg::vt100::show_cursor;
}

void display_size() {
  using namespace std::chrono;
  const auto now = [] { return system_clock::now(); };
  auto start = now();
  display_loop([&](screen &sc) {
    std::cout << dpsg::vt100::clear
              << "Elapsed time: " << duration_cast<milliseconds>(now() - start)
              << std::endl;
    std::cout << "Terminal height: " << sc.size().row << std::endl;
    std::cout << "Terminal width: " << sc.size().col << std::endl;
  });
}

int x = 0;
void display(const ui_representation &ui) {
  using namespace std::chrono;
  auto start = system_clock::now();
  fps_counting_loop([&ui, start](screen &sc) {
    sc.clear(pixel{.code = {' ', 0},
                   .bg = ui.bg_color,
                   .fg = ui.fg_color,
                   .flags = {
                       .bold = false,
                       .faint = true,
                       .underline = false,
                       .reverse = false,
                   }});

    for (auto &box : ui.boxes) {
      for (int i = 0; i < box.height; ++i) {
        for (int j = 0; j < box.width; ++j) {
          sc.set_pixel(j, i,
                       pixel{.code = {' ', 0},
                             .bg = box.bg,
                             .fg = box.fg,
                             .flags = {.bold = true}});
        }
      }
    }

    sc.write(2, 2, "Héllo TUI in Uni€ode!");
    sc.write(duration_cast<seconds>(system_clock::now() - start).count() % 10,
             1, '!');
  });
}

void display_ui() {
  ui_representation rep;
  rep.bg_color = {.r = 124, .g = 80, .b = 20};
  rep.fg_color = {.r = 0, .g = 255, .b = 120};
  rep.boxes.push_back(box{
      .position = {.x = 0, .y = 0},
      .height = 3,
      .width = 6,
      .bg = {.r = 0, .g = 255, .b = 255},
      .fg = {.r = 255, .g = 0, .b = 0},
  });
  display(rep);
}

void basic_test() {
  screen sc;
  color bg_color = {.r = 124, .g = 80, .b = 20};
  color fg_color = {.r = 0, .g = 255, .b = 120};
  sc.clear(pixel{.code = {' ', 0},
                 .bg = bg_color,
                 .fg = fg_color,
                 .flags = {
                     .bold = false,
                     .faint = true,
                     .underline = false,
                     .reverse = false,
                 }});

  using namespace std::chrono;
  std::fstream LOG{"timings.log", std::ios_base::out};
  std::cout << dpsg::vt100::hide_cursor;
  running_.store(true);
  auto last_print = high_resolution_clock::now();
  nanoseconds average{0};
  int count = 0;

  while (running_.load()) {
    auto start_time = high_resolution_clock::now();
    sc.redraw();
    auto end_time = high_resolution_clock::now();
    if (count == 0) {
      average = (end_time - start_time);
      count = 1;
    } else {
      average = ((average * count) + (end_time - start_time)) / (count + 1);
      count++;
    }
    if (end_time - last_print > 1s) {
      last_print = end_time;
      LOG << "Average frame time (ns): " << average
          << "\nAverage FPS: " << (1s / average) << std::endl;
    }
  }
}

void reference_test() {
  using namespace std::chrono;
  std::fstream LOG{"reference.timing.log", std::ios_base::out};
  std::cout << dpsg::vt100::hide_cursor;
  running_.store(true);
  auto last_print = high_resolution_clock::now();
  nanoseconds average{0};
  int count = 0;
  auto screen = dpsg::get_terminal_size();
  std::string str;
  str.resize(screen.row * screen.col);
  for (auto &c : str) {
    c = ' ';
  }
  str[0] = '1';

  while (running_.load()) {
    auto start_time = high_resolution_clock::now();
    fmt::print("\033[1;1H\033[m{}", str);
    auto end_time = high_resolution_clock::now();
    if (count == 0) {
      average = (end_time - start_time);
      count = 1;
    } else {
      average = ((average * count) + (end_time - start_time)) / (count + 1);
      count++;
    }
    if (end_time - last_print > 1s) {
      last_print = end_time;
      LOG << "Average frame time (ns): " << average
          << "\nAverage FPS: " << (1s / average) << std::endl;
    }
  }
}

void reference_color_test() {
  using namespace std::chrono;
  std::fstream LOG{"reference_color.timing.log", std::ios_base::out};
  std::cout << dpsg::vt100::hide_cursor;
  running_.store(true);
  auto last_print = high_resolution_clock::now();
  nanoseconds average{0};
  int count = 0;
  auto screen = dpsg::get_terminal_size();
  std::string str;
  str.resize(screen.row * screen.col);
  for (auto &c : str) {
    c = ' ';
  }
  str[0] = '1';
  str[str.size() - 1] = '1';

  while (running_.load()) {
    auto start_time = high_resolution_clock::now();
    fmt::print("\033[m\033[1;1m\033[38;2;223;12;123;48;2;43;23;12m");
    fmt::print("{}", str);
    auto end_time = high_resolution_clock::now();
    if (count == 0) {
      average = (end_time - start_time);
      count = 1;
    } else {
      average = ((average * count) + (end_time - start_time)) / (count + 1);
      count++;
    }
    if (end_time - last_print > 1s) {
      last_print = end_time;
      LOG << "Average frame time (ns): " << average
          << "\nAverage FPS: " << (1s / average) << std::endl;
    }
  }
}
