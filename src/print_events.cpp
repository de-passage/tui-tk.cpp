#include "print_events.hpp"
#include "vt100.hpp"
#include <sstream>

std::string print_code(dpsg::event::key key) {
  std::stringstream iss;
  using namespace dpsg::term_events;
  if (key.same_key(arrow_up)) {
    return "<UP>";
  }
  if (key.same_key(arrow_left)) {
    return "<LEFT>";
  }
  if (key.same_key(arrow_down)) {
    return "<DOWN>";
  }
  if (key.same_key(arrow_right)) {
    return "<RIGHT>";
  }
  if (key.same_key(f1)) {
    return "<F1>";
  }
  if (key.same_key(f2)) {
    return "<F2>";
  }
  if (key.same_key(f3)) {
    return "<F3>";
  }
  if (key.same_key(f4)) {
    return "<F4>";
  }
  if (key.same_key(f5)) {
    return "<F5>";
  }
  if (key.same_key(f6)) {
    return "<F6>";
  }
  if (key.same_key(f7)) {
    return "<F7>";
  }
  if (key.same_key(f8)) {
    return "<F8>";
  }
  if (key.same_key(f9)) {
    return "<F9>";
  }
  if (key.same_key(f10)) {
    return "<F10>";
  }
  if (key.same_key(f11)) {
    return "<F11>";
  }
  if (key.same_key(f12)) {
    return "<F12>";
  }
  if (key == enter) {
    return "<CR>";
  }
  if (key.same_key(backspace)) {
    return "<BS>";
  }
  if (key.same_key(esc)) {
    return "<ESC>";
  }
  if (key.same_key(ins)) {
    return "<INS>";
  }
  if (key.same_key(del)) {
    return "<DEL>";
  }
  if (key.same_key(page_up)) {
    return "<PGUP>";
  }
  if (key.same_key(page_down)) {
    return "<PGDWN>";
  }
  if (key.same_key(home)) {
    return "<HOME>";
  }
  if (key.same_key(end)) {
    return "<END>";
  }
  if (key.is_unicode()) {
    iss << dpsg::vt100::magenta << key.code_points()
        << (dpsg::vt100::white | dpsg::vt100::bold);
  } else {
    iss << dpsg::vt100::cyan << key.code
        << (dpsg::vt100::white | dpsg::vt100::bold);
  }

  return iss.str();
}

void print_key(dpsg::event::key in) {
  std::cout << (dpsg::vt100::white | dpsg::vt100::bold) << std::format("Key: \"{}\"\n\t- Unicode: {}\n\t- Alt: {}\n\t- "
                           "Ctrl: {}\n\t- Shift: {}\n",
                           print_code(in), in.is_unicode(), in.alt_pressed(),
                           in.ctrl_pressed(), in.shift_pressed());
}

void print_mouse(dpsg::event::mouse in) {
  std::cout << (dpsg::vt100::white | dpsg::vt100::bold) << std::format("Mouse: {}\n\t- x: {}\n\t- y: {}\n", (int)in.mods,
                           (int)in.x, (int)in.y);
}

void print_event(dpsg::event in) {
  if (in.is_key_event()) {
    print_key(in.get_key());
  } else if (in.is_mouse_event()) {
    print_mouse(in.get_mouse());
  } else {
    std::cout << (dpsg::vt100::cyan|dpsg::vt100::bold) << "Special Event" << std::endl;
  }
}
