#include "linux_term.hpp"
#include "char_stream.hpp"

#include "vt100.hpp"
#include <sstream>
#include "print_events.hpp"

int main(int argc, char *argv[])
{
  dpsg::with_raw_mode([](dpsg::raw_mode_context& ctx) {
    auto mouse_enabled = ctx.enable_mouse_tracking();
    auto input = ctx.event_stream(standard_input());
    int x = 0;
    while (input) {
      auto in = input();
      if (in.is_key_event()) {
        print_key(in.get_key());
      } else if (in.is_mouse_event()) {
        print_mouse(in.get_mouse());
      }

      if (in == dpsg::term_events::ctrl + 'd') {
        return;
      }
    }
  });
}
