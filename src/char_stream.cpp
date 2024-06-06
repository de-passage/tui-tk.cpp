#include "char_stream.hpp"
#include "common.hpp"
#include <sys/poll.h>
#include <unistd.h>

char_stream slurp_file(const std::string &str) {
  return slurp_file(str.c_str());
}
char_stream slurp_file(const char *filename) {
  constexpr auto BUFFER_SIZE = 4096;

  FILE *file = fopen(filename, "r");
  if (file == nullptr) {
    co_return false;
  }

  DEFER { fclose(file); };

  char buf[BUFFER_SIZE];
  size_t nread;
  while ((nread = fread(buf, sizeof(char), sizeof(buf), file))) {
    if (ferror(file)) {
      co_return false;
    }

    co_yield std::string_view{buf, nread};

    if (nread < BUFFER_SIZE) {
      co_return true;
    }
  }

  co_return true;
}

char_stream standard_input() {
  constexpr auto BUFFER_SIZE = 4096;
  char buffer[BUFFER_SIZE];
  pollfd fds;
  fds.fd = STDIN_FILENO;
  fds.events = POLLIN;

  while (true) {
    auto poll_result = poll(&fds, 1, 0);
    if (poll_result == -1) {
      if (errno == EINTR) {
        continue;
      }
      perror("Poll error");
      co_return false;
    }

    if (poll_result == 0) {
      continue;
    }

    auto last = read(STDIN_FILENO, buffer, sizeof(buffer));
    if (last == -1 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
      continue;
    }

    co_yield std::string_view{buffer, static_cast<size_t>(last)};
  }
}

char_stream stream_string(std::string_view sv) {
  co_yield sv;
  co_return true;
}
