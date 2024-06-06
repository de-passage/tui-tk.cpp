#include "linux_term.hpp"
#include "vt100.hpp"

#include <algorithm>
#include <concepts>
#include <fmt/core.h>
#include <vector>

// description de l'interface
//  boxes ? -> lines
//  with events ?

using ctx_t = dpsg::raw_mode_context;

struct screen {
  dpsg::terminal_size max_value{dpsg::get_terminal_size()};
};

namespace shapes {
struct box {
  dpsg::term_position top_left{};
  u16 height;
  u16 width;

  dpsg::term_position bot_right() const {
    return {
        .col = (u16)(top_left.col + width),
        .row = (u16)(top_left.row + (i32)height),
    };
  }

  std::string background_color;
  std::string background{' '};
};

void print(screen scr, box b) {
  auto max_x = std::min(b.bot_right().col, scr.max_value.col);
  auto max_y = std::min(b.bot_right().row, scr.max_value.row);

  if (b.top_left.row > max_y || b.top_left.col > max_x) {
    return;
  }

  fmt::print("{}", b.background_color);
  for (int y = b.top_left.row; y < max_y; ++y) {
    std::cout << dpsg::vt100::set_cursor(y, b.top_left.col);
    for (int x = b.top_left.x; x < max_x; ++x) {
      fmt::print("{}", b.background);
    }
  }
  std::cout << dpsg::vt100::reset << std::flush;
}

struct bordered_box : box {
  std::array<char, 8> borders{'+', '-', '+', '|', '+', '-', '+', '|'};
  std::string border_color;
};

void print(screen scr, bordered_box b) {
  auto max_x = std::min(b.bot_right().col, scr.max_value.col);
  auto max_y = std::min(b.bot_right().row, scr.max_value.row);
  auto h = b.height;
  auto w = b.width;

  if (b.top_left.row > max_y || b.top_left.col > max_x || h <= 0 || w <= 0) {
    return;
  }

  int y = b.top_left.row;
  std::cout << dpsg::vt100::set_cursor(y++, b.top_left.col);

  // upper border
  std::cout << b.border_color << b.borders[0];
  for (int i = 1; i < w - 1; ++i) {
    std::cout << b.borders[1];
  }
  std::cout << b.borders[2];

  for (; y < max_y - 1; ++y) {
    std::cout << dpsg::vt100::set_cursor(y, b.top_left.col);
    std::cout << b.border_color << b.borders[3] << b.background_color;

    for (int x = b.top_left.x; x < max_x - 2; ++x) {
      fmt::print("{}", b.background);
    }

    std::cout << b.border_color << b.borders[7];
  }

  std::cout << dpsg::vt100::set_cursor(y, b.top_left.col);
  std::cout << b.border_color << b.borders[4];
  for (int i = 1; i < w - 1; ++i) {
    std::cout << b.borders[5];
  }
  std::cout << b.borders[6];
  std::cout << dpsg::vt100::reset << std::flush;
}

} // namespace shapes

struct printable {
  virtual void move(i16 x_offset, i16 y_offset) = 0;
  virtual void print(screen s) const = 0;
  virtual bool contains(dpsg::term_position p) const = 0;
  virtual ~printable() = default;
};

template <class T> struct printable_shape : printable {
  T shape;

  printable_shape(std::same_as<T> auto &&s)
      : shape{std::forward<decltype(s)>(s)} {}

  void move(i16 x_offset, i16 y_offset) override {
    shape.top_left.x += x_offset;
    shape.top_left.y += y_offset;
  }
  void print(screen s) const override { shapes::print(s, shape); }
  bool contains(dpsg::term_position p) const override {
    return p.x >= shape.top_left.x && p.y >= shape.top_left.y &&
           p.x <= shape.bot_right().x && p.y <= shape.bot_right().y;
  }
};
using box = printable_shape<shapes::box>;
using bordered_box = printable_shape<shapes::bordered_box>;

void clear() { std::cout << dpsg::vt100::clear << std::flush; }

void move_to_back(auto &&container, auto &&it) {
  auto val = std::move(*it);
  auto last = it;
  while (++it != std::end(container)) {
    *last = std::move(*it);
    last = it;
  }
  *last = std::move(val);
}

int main() {
  std::cout << dpsg::vt100::clear_screen() << std::flush;
  dpsg::with_raw_mode([&](ctx_t &ctx) {
    screen scr;
    auto mouse = ctx.enable_mouse_tracking();
    auto input = ctx.event_stream();
    box b{shapes::box{
        .top_left = {10, 10},
        .height = 10,
        .width = 20,
        .background_color = "\033[41m",
        .background = " ",
    }};

    shapes::bordered_box sbb{b.shape};
    sbb.border_color = "\033[2;45;32m";
    sbb.top_left = {30, 10};
    sbb.background_color = "\033[45m";
    bordered_box bb{std::move(sbb)};

    box b2{shapes::box{
        .top_left = {10, 21},
        .height = 4,
        .width = 8,
        .background_color = "\033[46m",
        .background = "▒",
    }};

    box b3{shapes::box{
        .top_left = {17, 21},
        .height = 4,
        .width = 8,
        .background_color = "\033[43;36m",
        .background = "░",
    }};

    std::vector<printable *> printables = {&b, &bb, &b2, &b3};

    std::optional<dpsg::term_position> mouse_drag_start;
    dpsg::term_position mouse_position;

    printable *dragged = nullptr;

    std::cout << dpsg::vt100::hide_cursor << std::flush;

    while (input) {
      clear();

      for (auto &p : printables) {
        p->print(scr);
      }

      dpsg::event ev = input();
      namespace e = dpsg::term_events;
      if (ev.is_key_event() && (ev == e::ctrl + 'd') || (ev == e::ctrl + 'c')) {
        break;
      } else if (ev.is_mouse_event()) {
        auto m = ev.get_mouse();
        if (m.button() == dpsg::event::mouse::buttons::Left) {

          auto current_cursor = m.cursor();
          if (m.is_released()) {
            if (dragged != nullptr) {
              i32 diff_x = (i32)current_cursor.x - (i32)mouse_position.x;
              i32 diff_y = (i32)current_cursor.y - (i32)mouse_position.y;

              dragged->move(diff_x, diff_y);
            }
          } else if (m.is_pressed()) {
            auto it = std::ranges::find_if(printables, [m](auto *ptr) {
              return ptr->contains(m.cursor());
            });
            if (it != std::end(printables)) {
              dragged = *it;
              move_to_back(printables, it);
            }
          } else {
            dragged = nullptr;
          }

          mouse_position = current_cursor;
        } else {
          dragged = nullptr;
        }
      }
    };
  });
  std::cout << dpsg::vt100::show_cursor << std::endl;
}
