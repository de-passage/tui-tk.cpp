#pragma once

#include "generator.hpp"
#include "types.hpp"
#include <concepts>
#include <iostream>

#include <bit>
#include <cassert>
#include <cctype>
#include <cerrno>
#include <csignal>
#include <cstring>
#include <format>
#include <stdexcept>
#include <type_traits>

extern "C" {
#include <poll.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>
}

namespace dpsg {

namespace detail {
template<class T>
concept InfiniteCharacterSequence = requires(T obj, const T& cobj) {
  requires std::convertible_to<T, bool>;
  { obj.read_char() } -> std::same_as<char>;
  { cobj.buffer_end() } -> std::same_as<bool>;
  { obj.operator co_await().await_resume() } -> std::same_as<bool>;
};
}

/**
 * @brief Exception thrown when a POSIX syscall fails and sets errno.
 *
 * Wraps the C `errno` value and presents the platform error string
 * via `std::runtime_error`'s message.
 */
struct errno_exception : std::runtime_error {
  errno_exception() : std::runtime_error(strerror(errno)) {}

  /** The raw errno value captured at construction. */
  int code{errno};
};

/**
 * @brief Base class for errors encountered while parsing terminal
 *        control (VT100/ANSI) sequences.
 *
 * Subclasses provide a small buffer view of the bytes that produced
 * the error via `buffer()`.
 */
struct invalid_sequence : std::runtime_error {

  template <typename T>
    requires std::is_constructible_v<std::runtime_error, T>
  invalid_sequence(T &&param) : std::runtime_error(std::forward<T>(param)) {}

  /**
   * @brief Returns a view of the input bytes that caused the parse error.
   *
   * Implementations must return a contiguous view to aid diagnostics.
   */
  [[nodiscard]] virtual std::string_view buffer() const = 0;
};

namespace detail {
template <size_t BufSize> struct invalid_sequence_impl : invalid_sequence {
  template <typename T>
    requires std::is_constructible_v<std::runtime_error, T>
  invalid_sequence_impl(const char (&buf)[BufSize], T &&param)
      : invalid_sequence{std::forward<T>(param)} {
    memcpy(buffer_, buf, last_);
  }
  template <typename T>
    requires std::is_constructible_v<std::runtime_error, T>
  invalid_sequence_impl(const char *buf, size_t last, T &&param)
      : invalid_sequence{std::forward<T>(param)}, last_{last} {
    memcpy(buffer_, buf, last_);
  }

private:
  char buffer_[BufSize];
  size_t last_{BufSize};

public:
  [[nodiscard]] std::string_view buffer() const override {
    return {buffer_, last_};
  }
};
} // namespace detail

/**
 * @brief Error for an incomplete numeric parameter sequence inside an
 *        escape/control sequence (e.g. CSI sequences that end unexpectedly).
 *
 * The template parameter `BufSize` controls the internal copy buffer
 * size used for diagnostics.
 */
template <size_t BufSize>
struct unfinished_numeric_sequence : detail::invalid_sequence_impl<BufSize> {
  unfinished_numeric_sequence(const char *buf, size_t last, const u16 *beg,
                              const u16 *end, char er)
      : detail::invalid_sequence_impl<BufSize>(
            buf, last,
            std::format("Unfinished numeric sequence in terminal control "
                        "output (terminate with '<{}>')",
                        (int)er)),
        size{end - beg}, numeric_values{(u16 *)malloc(sizeof(*beg) * size)},
        error_character{er} {
    memcpy(numeric_values, beg, size * sizeof(*beg));
  }
  unfinished_numeric_sequence(char (&buf)[BufSize], const u16 *beg,
                              const u16 *end, char er)
      : unfinished_numeric_sequence{buf, BufSize, beg, end, er} {}
  ~unfinished_numeric_sequence() override { free(numeric_values); }

  unfinished_numeric_sequence(const unfinished_numeric_sequence &) = delete;
  unfinished_numeric_sequence(unfinished_numeric_sequence &&) noexcept = delete;
  unfinished_numeric_sequence &
  operator=(const unfinished_numeric_sequence &) = delete;
  unfinished_numeric_sequence &
  operator=(unfinished_numeric_sequence &&) noexcept = delete;

  ptrdiff_t size;
  u16 *numeric_values;
  char error_character;
};
template <size_t BufSize>
unfinished_numeric_sequence(char (&)[BufSize], const u16 *beg, const u16 *end,
                            char er) -> unfinished_numeric_sequence<BufSize>;
template <size_t BufSize>
unfinished_numeric_sequence(char (&)[BufSize], size_t last, const u16 *beg,
                            const u16 *end, char er)
    -> unfinished_numeric_sequence<BufSize>;

template <size_t BufSize>
struct invalid_sequence_start : detail::invalid_sequence_impl<BufSize> {
  /**
   * @brief Construct from a fixed-size buffer and the offending start char.
   * @param buf Diagnostic buffer containing the input bytes.
   * @param c The unexpected character that started the invalid sequence.
   */
  invalid_sequence_start(const char (&buf)[BufSize], char c)
      : invalid_sequence_start{buf, BufSize, c} {}
  invalid_sequence_start(const char *buf, size_t last, char c)
      : detail::invalid_sequence_impl<BufSize>(
            buf, last,
            std::format(
                "Invalid sequence start <{}> in terminal control output",
                (int)c)) {}
};
template <size_t BufSize>
invalid_sequence_start(char (&)[BufSize], char c)
    -> invalid_sequence_start<BufSize>;
template <size_t BufSize>
invalid_sequence_start(char (&)[BufSize], size_t last, char c)
    -> invalid_sequence_start<BufSize>;

template <size_t BufSize>
struct invalid_function_key : detail::invalid_sequence_impl<BufSize> {
  /**
   * @brief Error type used when a function-key escape sequence is malformed.
   * @param buf Diagnostic buffer containing the input bytes.
   * @param c The unexpected character that terminated the function key.
   */
  invalid_function_key(const char (&buf)[BufSize], char c)
      : invalid_function_key{buf, BufSize, c} {}
  invalid_function_key(const char *buf, size_t last, char c)
      : detail::invalid_sequence_impl<BufSize>(
            buf, last,
            std::format("Invalid function key ending with <{}> in terminal "
                        "control output",
                        (int)c)) {}
};
template <size_t BufSize>
invalid_function_key(char (&)[BufSize], char c)
    -> invalid_function_key<BufSize>;
template <size_t BufSize>
invalid_function_key(char (&)[BufSize], size_t last, char c)
    -> invalid_function_key<BufSize>;

/**
 * @brief Enable raw-ish terminal mode by clearing the given local flags.
 *
 * This helper reads the current terminal attributes into `ctx`, clears the
 * provided `new_mode` bits from `c_lflag` and applies the modified attributes
 * with `tcsetattr`. On failure the process will `exit(1)` after printing an
 * error; callers should prefer the RAII wrapper `raw_mode_context_basic`.
 *
 * @param ctx Pointer to a `termios` struct that will be populated with the
 *            previous terminal attributes (and used to restore them).
 * @param new_mode Bitmask of local `c_lflag` flags to clear.
 */
inline void raw_mode_enable(struct termios *ctx, int new_mode) {
  if (tcgetattr(STDIN_FILENO, ctx) < 0) {
    perror("tcgetattr");
    exit(1);
  }
  struct termios raw = *ctx;
  raw.c_lflag &= ~(new_mode);
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) < 0) {
    perror("tcsetattr");
    exit(1);
  }
}

/**
 * @brief Restore terminal attributes from `ctx`.
 *
 * This is the inverse of `raw_mode_enable` and writes the provided
 * `termios` settings back to `STDIN_FILENO`.
 */
inline void raw_mode_disable(struct termios *ctx) {
  tcsetattr(STDIN_FILENO, TCSAFLUSH, ctx);
}

// XTERM Mouse codes
// 1002 SET_BTN_EVENT_MOUSE // Buttons only, no mouse position tracking
// 1003 SET_ANY_EVENT_MOUSE // Limited to 255-32 positions, unsuited to larger
// terminals
// 1005 SET_EXT_MODE_MOUSE // UTF-8 encoding scheme
// 1006 SET_SGR_EXT_MODE_MOUSE // Same as default mode but positions are encoded
// in ASCII, allowing for arbitrary positions

/**
 * @name Mouse / cursor helpers
 * Helpers that write the appropriate VT100/DECRST/DECSGR sequences to
 * stdout to enable/disable mouse tracking or query the cursor position.
 *@{
 */

/**
 * @brief Enable XTerm-style mouse tracking (SET_ANY_EVENT + SGR mode).
 *
 * Sends the escape sequences required by many terminals to enable mouse
 * position+button reporting. This is a low-level helper; prefer the
 * RAII `raw_mode_context_basic::enable_mouse_t` for scoped usage.
 */
inline void enable_mouse_tracking() {
  write(STDOUT_FILENO, "\033[?1003h", 8); // enable SET_ANY_EVENT_MOUSE
  write(STDOUT_FILENO, "\033[?1006h", 8); // enable SET_SGR_EXT_MODE_MOUSE
  fsync(STDOUT_FILENO);
}

/**
 * @brief Disable previously enabled mouse tracking modes.
 */
inline void disable_mouse_tracking() {
  write(STDOUT_FILENO, "\033[?1006l", 8); // disable SET_SGR_EXT_MODE_MOUSE
  write(STDOUT_FILENO, "\033[?1003l", 8); // disable SET_ANY_EVENT_MOUSE
  fsync(STDOUT_FILENO);
}

/**
 * @brief Query the terminal for the current cursor position (CSI 6n).
 *
 * The response is sent to stdin as a control sequence and parsed by the
 * event stream code paths into a `event::special` with a `cursor_position`.
 */
inline void query_cursor_position() { write(STDOUT_FILENO, "\033[6n", 4); }

/*@}*/

/**
 * @brief Simple (column,row) or (x,y) terminal co-ordinate pair.
 *
 * The union provides both names: `col,row` and `x,y` depending on the
 * calling code's preference. Values are stored in `u16`.
 */
struct term_position {
  union {
    struct {
      u16 col;
      u16 row;
    };
    struct {
      u16 x;
      u16 y;
    };
  };
};
/**
 * @brief Terminal size in columns and rows.
 */
struct terminal_size {
  u16 col;
  u16 row;
};

/**
 * @brief A compact 8-byte tagged union representing an input event.
 *
 * Events are one of `key`, `mouse` or `special`. The compact layout keeps
 * instances small for efficient queueing and copying between threads.
 */
struct event {

  /**
   * @brief Keyboard event representation.
   *
   * Stores a single UTF-8 codepoint (up to 4 bytes) in `data` along with
   * modifier flags. `Key_Marker` in `mods` is used to distinguish key
   * events from other union variants.
   */
  struct key {
    enum class funckey_modifiers : u8 {
      Shift = 2,
      Alt = 3,
      Shift_Alt = 4,
      Control = 5,
      Shift_Control = 6,
      Alt_Control = 7,
      Shift_Alt_Control = 8
    };

    /**
     * @brief Modifier flags for keys.
     */
    enum class modifiers : u8 {
      None = 0,
      Shift = 4,
      Alt = 8,
      Ctrl = 16,
      Unicode = 32,
      Special = 64,
      Key_Marker = 1 << 7,
    };

    /** Default construct an empty key. */
    explicit constexpr key() = default;
    explicit constexpr key(char value,
                           key::modifiers mods = key::modifiers::None) noexcept
        : code{value}, mods{mods | key::modifiers::Key_Marker} {}

    union {
      struct {
        char code;
        char cont[3]{};
        u8 _padding[3]{};
        modifiers mods;
      };
      char data[8];
    };

    friend constexpr modifiers operator&(modifiers left,
                                         modifiers right) noexcept {
      return static_cast<modifiers>(static_cast<u8>(left) &
                                    static_cast<u8>(right));
    }
    friend constexpr modifiers operator|(modifiers left,
                                         modifiers right) noexcept {
      return static_cast<modifiers>(static_cast<u8>(left) |
                                    static_cast<u8>(right));
    }
    friend constexpr modifiers operator~(modifiers mod) noexcept {
      return static_cast<modifiers>(~static_cast<u8>(mod));
    }
    friend constexpr key operator|(key left, modifiers right) noexcept {
      left.mods = left.mods | right;
      return left;
    }

    /**
     * @brief True if the stored bytes represent a multi-byte UTF-8 lead byte.
     */
    [[nodiscard]] constexpr bool is_unicode() const {
      return (((u32)code >> 6) & 0b11) == 0b11;
    }

    /**
     * @brief Number of bytes in the stored UTF-8 codepoint.
     */
    [[nodiscard]] constexpr size_t code_point_count() const {
      if (code >= 0) {
        return 1;
      }
      return std::countl_one((unsigned char)code);
    }

    /**
     * @brief Returns a view over the underlying bytes that make up the
     *        key's codepoint(s).
     */
    [[nodiscard]] constexpr std::string_view code_points() const {
      return std::string_view{data, code_point_count()};
    }

    /**
     * @brief Compares codepoints and whether the key is a function key.
     */
    [[nodiscard]] constexpr bool same_key(key other) const {
      return code_points() == other.code_points() &&
             is_function_key() == other.is_function_key();
    }

    /**
     * @brief True if this `key` represents a terminal function key.
     */
    [[nodiscard]] constexpr bool is_function_key() const {
      return (mods & modifiers::Special) == modifiers::Special;
    }

    /** @name Modifier accessors */
    /** @{ */
    [[nodiscard]] constexpr bool ctrl_pressed() const {
      return (mods & modifiers::Ctrl) == modifiers::Ctrl;
    }

    [[nodiscard]] constexpr bool alt_pressed() const {
      return (mods & modifiers::Alt) == modifiers::Alt;
    }

    [[nodiscard]] constexpr bool shift_pressed() const {
    /** @} */
      return (mods & modifiers::Shift) == modifiers::Shift;
    }
  }; // struct key

  /**
   * @brief Mouse event representation.
   *
   * Contains cursor coordinates and a small modifiers bitset which encodes
   * button, shift/alt/ctrl and release/move indicators.
   */
  struct mouse {
    enum class buttons {
      Left = 0,
      Middle = 1,
      Right = 2,
      Move = 3,
      WheelUp = 65,
      WheelDown = 66,
    };
    enum class modifiers : u8 {
      None = 0,
      Button1 = 0,
      Button2 = 1,
      Button3 = 2,
      Unused = 3, // Extended mode only use this to indicate drag
      Shift = 4,
      Alt = 8,
      Ctrl = 16,
      Release = 32,
      Move = 35, // Release + Release (Unused) == drag
      WheelUp = 64,
      WheelDown = 65,
      Key_Marker = 1 << 7,
    };

    /** Default construct an empty mouse event. */
    explicit constexpr mouse() = default;
    explicit constexpr mouse(modifiers magic, term_position pos) noexcept
        : x{pos.col}, y{pos.row}, mods(magic) {}
    explicit constexpr mouse(buttons button, term_position pos) noexcept
        : x{pos.col}, y{pos.row}, mods((modifiers)button) {}

    u16 x;
    u16 y;
    u8 _padding[3];
    modifiers mods;

    /**
     * @brief Returns the canonical button encoded in `mods`.
     */
    [[nodiscard]] constexpr buttons button() const noexcept {
      return (buttons)(mods &
                       (modifiers) ~(u8)(modifiers::Release | modifiers::Shift |
                                         modifiers::Alt | modifiers::Ctrl));
    }

    /** True if the event corresponds to a press (not a release). */
    [[nodiscard]] constexpr bool is_pressed() const noexcept {
      return (mods & modifiers::Release) == modifiers::None;
    }

    /** True if the event corresponds to a release. */
    [[nodiscard]] constexpr bool is_released() const noexcept {
      return (mods & modifiers::Release) != modifiers::None;
    }

    /** Bitwise operators for the modifiers enum. */
    friend constexpr modifiers operator&(modifiers left,
                       modifiers right) noexcept {
      return static_cast<modifiers>(static_cast<u8>(left) &
                                    static_cast<u8>(right));
    }
    friend constexpr modifiers operator|(modifiers left,
                                         modifiers right) noexcept {
      return static_cast<modifiers>(static_cast<u8>(left) |
                                    static_cast<u8>(right));
    }
    friend constexpr modifiers operator^(modifiers left,
                                         modifiers right) noexcept {
      return static_cast<modifiers>(static_cast<u8>(left) ^
                                    static_cast<u8>(right));
    }

    /** Compare only the button part of two mouse events. */
    friend constexpr bool same_button(mouse left, mouse right) noexcept {
      return left.button() == right.button();
    }

    [[nodiscard]] constexpr bool ctrl_pressed() const {
      return (mods & modifiers::Ctrl) == modifiers::Ctrl;
    }

    [[nodiscard]] constexpr bool alt_pressed() const {
      return (mods & modifiers::Alt) == modifiers::Alt;
    }

    [[nodiscard]] constexpr bool shift_pressed() const {
      return (mods & modifiers::Shift) == modifiers::Shift;
    }

    /**
     * @brief Returns the cursor position as a `term_position`.
     */
    [[nodiscard]] constexpr term_position cursor() const noexcept {
      return {.x = x, .y = y};
    }
  }; // struct mouse

  /**
   * @brief Special non-key/mouse events carried inside the `event` union.
   *
   * Examples: cursor position responses from the terminal.
   */
  struct special {
    constexpr special(term_position p) : pos{.position = p} {}
    enum class type : u8 {
      none = 0,
      cursor_position = 1,
    };
    struct cursor_position {
      term_position position;
      u8 unused[2]{};
      enum type type { type::cursor_position };
    };
    struct untyped {
      u8 data_[6];
      type type_{0};
      u8 mods_{marker};
    };
    constexpr static inline u8 marker = 127;

    union {
      cursor_position pos;
      untyped any;
    };

    enum type type() { return any.type_; }

    constexpr term_position cursor_position() const noexcept {
      return pos.position;
    }
  };

  /** Default construct an empty event (zeroed). */
  explicit constexpr event() noexcept : _cheat_{0} {}
  constexpr event(key key) noexcept : key_{key} {}
  constexpr event(special spec) noexcept : special_{spec} {}
  constexpr event(mouse mouse) noexcept : mouse_{mouse} {}
  union {
    key key_;
    mouse mouse_;
    special special_;
    struct {
      char data_[7];
      u8 mods_;
    };
    u64 _cheat_;
  };

  /** @name Event type helpers
   * Convenience accessors to identify the stored variant.
   */
  /** @{ */
  [[nodiscard]] bool is_key_event() const noexcept {
    return ((u8)event::mouse::modifiers::Key_Marker & mods_) != 0;
  }

  [[nodiscard]] bool is_mouse_event() const noexcept {
    return !is_key_event() && !is_special_event();
  }

  [[nodiscard]] bool is_special_event() const noexcept {
    return (u8)mods_ == special::marker;
  }

  [[nodiscard]] bool alt_pressed() const noexcept {
    return ((u8)mouse::modifiers::Alt & mods_) != 0;
  }

  [[nodiscard]] bool ctrl_pressed() const noexcept {
    return ((u8)mouse::modifiers::Ctrl & mods_) != 0;
  }

  [[nodiscard]] bool shift_pressed() const noexcept {
    return ((u8)mouse::modifiers::Shift & mods_) != 0;
  }

  /** Compare two events for equality. */
  friend constexpr bool operator==(event left, event right) noexcept {
    if (std::is_constant_evaluated()) {
      if (left.is_key_event()) {
        return right.is_key_event() && left.key_.code == right.key_.code &&
               left.key_.mods == right.key_.mods;
      }
      return false;
    }
    return left._cheat_ == right._cheat_;
  }

  /**
   * @brief Access the event as a `key`.
   *
   * Asserts in non-constexpr execution if the stored variant is not a key.
   */
  [[nodiscard]] constexpr key get_key() const noexcept {
    if (!std::is_constant_evaluated()) {
      assert(is_key_event());
    }
    return key_;
  }
  /** Mutable access to the stored `key`. */
  [[nodiscard]] constexpr key &get_key() noexcept {
    if (!std::is_constant_evaluated()) {
      assert(is_key_event());
    }
    return key_;
  }

  /**
   * @brief Access the event as a `mouse`.
   *
   * Asserts in non-constexpr execution if the stored variant is not a mouse.
   */
  [[nodiscard]] constexpr mouse get_mouse() const noexcept {
    if (!std::is_constant_evaluated()) {
      assert(is_mouse_event());
    }
    return mouse_;
  }
  /** Mutable access to the stored `mouse`. */
  [[nodiscard]] constexpr mouse &get_mouse() noexcept {
    if (!std::is_constant_evaluated()) {
      assert(is_mouse_event());
    }
    return mouse_;
  }
};
static_assert(sizeof(event) == 8);
/**
 * @brief Generator type that yields parsed `event` values.
 */
using event_stream = ::dpsg::generator<event>;

/**
 * @brief Predefined events and small DSL helpers for constructing
 *        `event::key` values used throughout the codebase.
 */
namespace term_events {

template <typename EvType, typename EvType::modifiers Mod>
struct dsl_event_modifier {};
template <event::key::modifiers mod>
using dsl_key_modifier = dsl_event_modifier<event::key, mod>;

template <event::key::modifiers left, event::key::modifiers right>
constexpr dsl_key_modifier<left | right>
operator+(dsl_key_modifier<left> /*ignored*/,
          dsl_key_modifier<right> /*ignored*/) noexcept {
  return {};
}

constexpr static inline auto ctrl =
    dsl_key_modifier<event::key::modifiers::Ctrl>{};
constexpr static inline auto alt =
    dsl_key_modifier<event::key::modifiers::Alt>{};
constexpr static inline auto shift =
    dsl_key_modifier<event::key::modifiers::Shift>{};
constexpr static inline auto meta = alt;

template <event::key::modifiers mod>
constexpr event::key operator+(dsl_key_modifier<mod> /*ignored*/,
                               event::key event) noexcept {
  event.mods = mod | event.mods;
  return event;
}

template <event::key::modifiers mod>
constexpr event::key operator+(dsl_key_modifier<mod> /*ignored*/,
                               char c) noexcept {
  return event::key{c, mod};
}

template <class T> struct dsl_modifier_fuzzy {
  using type = T;
};

template <event::key::modifiers M>
dsl_modifier_fuzzy<dsl_key_modifier<M>>
operator~(dsl_key_modifier<M> /*ignored*/) {
  return {};
}

template <event::key::modifiers M>
constexpr bool operator==(event::key key,
                          dsl_modifier_fuzzy<dsl_key_modifier<M>> mod) {
  return (key.mods & M) == M;
}

constexpr static inline event::key arrow_up{'A',
                                            event::key::modifiers::Special};
constexpr static inline event::key arrow_down{'B',
                                              event::key::modifiers::Special};
constexpr static inline event::key arrow_right{'C',
                                               event::key::modifiers::Special};
constexpr static inline event::key arrow_left{'D',
                                              event::key::modifiers::Special};
constexpr static inline event::key f1{'P', event::key::modifiers::Special};
constexpr static inline event::key f2{'Q', event::key::modifiers::Special};
constexpr static inline event::key f3{'R', event::key::modifiers::Special};
constexpr static inline event::key f4{'S', event::key::modifiers::Special};
constexpr static inline event::key f5{15, event::key::modifiers::Special};
constexpr static inline event::key f6{17, event::key::modifiers::Special};
constexpr static inline event::key f7{18, event::key::modifiers::Special};
constexpr static inline event::key f8{19, event::key::modifiers::Special};
constexpr static inline event::key f9{20, event::key::modifiers::Special};
constexpr static inline event::key f10{21, event::key::modifiers::Special};
constexpr static inline event::key f11{23, event::key::modifiers::Special};
constexpr static inline event::key f12{24, event::key::modifiers::Special};
constexpr static inline event::key backspace{127};
constexpr static inline event::key enter{'j', event::key::modifiers::Ctrl};
constexpr static inline event::key esc{27};
constexpr static inline event::key home{1, event::key::modifiers::Special};
constexpr static inline event::key ins{2, event::key::modifiers::Special};
constexpr static inline event::key del{3, event::key::modifiers::Special};
constexpr static inline event::key end{4, event::key::modifiers::Special};
constexpr static inline event::key page_up{5, event::key::modifiers::Special};
constexpr static inline event::key page_down{6, event::key::modifiers::Special};
constexpr static inline event::key special_event{
    (char)0xFF, event::key::modifiers::Special};

template <size_t N> struct fixed_string {
  constexpr fixed_string(const char (&b)[N]) noexcept {
    for (size_t i = 0; i < N; ++i) {
      value[i] = b[i];
    }
  }
  char value[N];
};
template <size_t N> fixed_string(const char (&)[N]) -> fixed_string<N>;

template <fixed_string F> constexpr event::key operator""_key() {
  constexpr auto f = F;
  return event::key{f.value[0]};
}

} // namespace term_events

namespace detail {

constexpr static inline std::initializer_list<int> HANDLED_SIGNALS = {
    SIGINT, SIGSEGV, SIGTERM, SIGILL, SIGFPE, SIGABRT};
constexpr static inline auto MAX_SIGNAL =
    HANDLED_SIGNALS.size() + 2; // SIGCONT & SIGTSTP
constexpr static inline auto INDEX_HANDLER_SIGTSTP = MAX_SIGNAL - 1;
constexpr static inline auto INDEX_HANDLER_SIGCONT = INDEX_HANDLER_SIGTSTP - 1;
constexpr size_t index_of(int signal) {
  size_t i = 0;
  for (auto sig : HANDLED_SIGNALS) {
    if (sig == signal) {
      return i;
    }
    ++i;
  }
  if constexpr (!std::is_constant_evaluated()) {
    assert(false && "Tried to access an unhandled signal...");
  }
  return -1; // Just don't let it happen
}
extern struct sigaction new_sa[MAX_SIGNAL], old_sa[MAX_SIGNAL];
extern struct termios orig_termios;
extern bool require_mouse;
} // namespace detail

/**
 * @brief RAII helper that enables a chosen set of local terminal flags
 *        (raw/cbreak styles) for the scope of the object.
 *
 * The template parameter `Mode` is a bitmask of `c_lflag` bits that will be
 * cleared when the object is constructed and restored on destruction. The
 * constructor also installs signal handlers to ensure the terminal is
 * restored on fatal signals or stop/continue events.
 */
template <int Mode> struct raw_mode_context_basic {

  /**
   * @brief Enter raw/cbreak mode and register signal handlers.
   *
   * The previous terminal attributes are stored in `detail::orig_termios`.
   */
  raw_mode_context_basic() noexcept {
    raw_mode_enable(&detail::orig_termios, Mode);
    register_signal_handlers();
  }

private:
  static void set_handler(int signal, void (*func)(int), size_t index) {
    memset(&detail::new_sa[index], 0, sizeof(detail::new_sa[index]));
    detail::new_sa[index].sa_handler = func;
    sigaction(signal, &detail::new_sa[index], &detail::old_sa[index]);
  }

  static void restore_old_and_raise(int sig, size_t idx) {
    sigaction(sig, &detail::old_sa[idx], nullptr);
    raise(sig);
  }

  void register_signal_handlers() {
    for (int signal : detail::HANDLED_SIGNALS) {
      set_handler(signal, &raw_mode_context_basic::handle_fatal_signal,
                  detail::index_of(signal));
    }
    set_handler(SIGTSTP, &raw_mode_context_basic::handle_sigtstp,
                detail::INDEX_HANDLER_SIGTSTP);
  }

  /**
   * @brief Signal handler for SIGTSTP (suspend via Ctrl-Z).
   *
   * Restores terminal state and disables mouse tracking before re-raising
   * the signal so the default behaviour occurs.
   */
  static void handle_sigtstp(int sig) {
    set_handler(SIGCONT, &raw_mode_context_basic::handle_sigcont,
                detail::INDEX_HANDLER_SIGCONT);
    raw_mode_disable(&detail::orig_termios);
    if (detail::require_mouse) {
      ::dpsg::disable_mouse_tracking();
    }
    restore_old_and_raise(sig, detail::INDEX_HANDLER_SIGCONT);
  }

  /**
   * @brief Signal handler for SIGCONT (continue after suspend).
   *
   * Re-enables raw mode and mouse tracking as needed, then re-raises the
   * signal to restore default behaviour.
   */
  static void handle_sigcont(int sig) {
    set_handler(SIGTSTP, &raw_mode_context_basic::handle_sigtstp,
                detail::INDEX_HANDLER_SIGTSTP);
    raw_mode_enable(&detail::orig_termios, Mode);
    if (detail::require_mouse) {
      ::dpsg::enable_mouse_tracking();
    }
    restore_old_and_raise(sig, detail::INDEX_HANDLER_SIGTSTP);
  }

  /**
   * @brief Handler for fatal signals (SIGINT, SIGSEGV, etc.).
   *
   * Restores terminal attributes and mouse tracking before re-raising the
   * fatal signal so default platform handlers (core dump, terminate, etc.)
   * can proceed.
   */
  static void handle_fatal_signal(int sig) {
    raw_mode_disable(&detail::orig_termios);
    if (detail::require_mouse) {
      ::dpsg::disable_mouse_tracking();
    }

    restore_old_and_raise(sig, detail::index_of(sig));
  }

public:
  raw_mode_context_basic(const raw_mode_context_basic &) = delete;
  raw_mode_context_basic &operator=(const raw_mode_context_basic &) = delete;
  raw_mode_context_basic(raw_mode_context_basic &&) = delete;
  raw_mode_context_basic &operator=(raw_mode_context_basic &&) = delete;

  /**
   * @brief Restore previous terminal attributes on destruction.
   */
  ~raw_mode_context_basic() noexcept {
    raw_mode_disable(&detail::orig_termios);
  }

  /**
   * @brief RAII return type from `enable_mouse_tracking()` that keeps
   *        mouse tracking active while it is alive.
   */
  struct enable_mouse_t {
    enable_mouse_t() noexcept {
      ::dpsg::enable_mouse_tracking();
      detail::require_mouse = true;
    }
    ~enable_mouse_t() noexcept {
      ::dpsg::disable_mouse_tracking();
      detail::require_mouse = false;
    }
    enable_mouse_t(const enable_mouse_t &) noexcept = delete;
    enable_mouse_t(enable_mouse_t &&) noexcept = delete;
    const enable_mouse_t &operator=(const enable_mouse_t &) noexcept = delete;
    const enable_mouse_t &operator=(enable_mouse_t &&) noexcept = delete;
  };

  /**
   * @brief Keep mouse tracking enabled while the returned guard is alive.
   *
   * Example: `auto g = ctx.enable_mouse_tracking();` — mouse reporting will
   * remain active until `g` is destroyed.
   */
  [[nodiscard]] enable_mouse_t enable_mouse_tracking() { return {}; }

  /**
   * @brief Coroutine generator that yields raw bytes read from `STDIN_FILENO`.
   *
   * This is a low-level blocking input generator implemented with `poll`
   * and `read`. It yields each read character as a `char` and throws
   * `errno_exception` on unrecoverable I/O errors.
   *
   * @tparam BufSize Size of the internal read buffer used for each `read(2)`.
   * @tparam Timeout Poll timeout in milliseconds (`-1` = infinite).
   */
  template <size_t BufSize = 32, int Timeout = -1>
  ::dpsg::generator<char> input_stream() {
    (void)this;
    pollfd fds;
    fds.fd = STDIN_FILENO;
    fds.events = POLLIN;

    for (;;) {
      auto poll_result = poll(&fds, 1, Timeout);
      if (poll_result == -1) {
        if (errno == EINTR) {
          continue;
        }
        throw errno_exception{};
      }

      if (poll_result == 0) {
        continue;
      }

      int last = 0;
      int current = 0;
      char buffer[BufSize];
      last = read(STDIN_FILENO, buffer, sizeof(buffer));
      if (last == -1 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
        continue;
      }

      current = 0;
      while (current < last) {
        char c = buffer[current++];
        co_yield (char) c;
      }
    }

    throw errno_exception{};
  }

private:
  constexpr static inline u8 UPPER_BOUND_CTRL_CHARACTERS =
      32; // 32 first values represent ctrl+<char>. 0 is ctrl+` for some reason

  /**
   * @brief Convert a raw input byte into an `event::key` and return the
   *        number of expected UTF-8 continuation bytes.
   *
   * Handles control characters (Ctrl-encoded) and multi-byte UTF-8
   * continuation logic. The constructed `event` is written to `out`.
   *
   * @param c Input byte.
   * @param mod Initial modifier flags (e.g. Alt when ESC prefix present).
   * @param out Output event to populate.
   * @return Number of additional UTF-8 bytes expected after this lead byte.
   */
  static size_t from_character(char c, event::key::modifiers mod, event &out) {
    // CTRL+<char> is sent as (<char> - 'A' + 1). For some reason, '`' is sent
    // as 0.
    if ((((u32)c >> 6) & 0b11) == 0b11) { // unicode continuation
      out = event{event::key{(char)c, mod}};
      return out.get_key().code_point_count() - 1;
    }
    if (c < UPPER_BOUND_CTRL_CHARACTERS) {
      out = event{event::key{(char)(c == 0 ? '`' : c + 'a' - 1),
                             mod | event::key::modifiers::Ctrl}};
      return 0;
    }

    out = event{event::key{(char)c, mod}};
    return 0;
  };

  /**
   * @brief Parse a 3-value mouse numeric sequence into an `event::mouse`.
   *
   * `numbers` holds [code,x,y] as emitted by the terminal's mouse SGR/CSI
   * sequences. `mods` is an additional modifier mask applied by the parser.
   */
  static void parse_mouse(const u16 *numbers, event::mouse::modifiers mods,
                          event &ev) {
    auto magic = (event::mouse::modifiers)numbers[0];
    auto x = numbers[1];
    auto y = numbers[2];
    ev = event::mouse{mods | magic, {.x = x, .y = y}};
  }

  /**
   * @brief Apply function-key modifier encoding to a base function key.
   *
   * Many terminal function keys include a numeric modifier (1..8) that
   * encodes Shift/Alt/Ctrl combinations. This helper maps that numeric
   * modifier into the `event::key::modifiers` bitset.
   *
   * @param c Final character code for the function key (e.g. 'A'..'D').
   * @param base Base `event` representing the function key without extra
   *             modifiers.
   * @param modifiers Numeric modifier value parsed from the control sequence.
   */
  static event parse_function_key(char c, event base, u16 modifiers) {
    base.get_key().code = c;
    switch ((event::key::funckey_modifiers)modifiers) {
    case event::key::funckey_modifiers::Alt:
      base.get_key().mods = base.get_key().mods | event::key::modifiers::Alt;
      break;
    case event::key::funckey_modifiers::Shift:
      base.get_key().mods = base.get_key().mods | event::key::modifiers::Shift;
      break;
    case event::key::funckey_modifiers::Shift_Alt:
      base.get_key().mods = base.get_key().mods | event::key::modifiers::Shift;
      base.get_key().mods = base.get_key().mods | event::key::modifiers::Alt;
      break;
    case event::key::funckey_modifiers::Control:
      base.get_key().mods = base.get_key().mods | event::key::modifiers::Ctrl;
      break;
    case event::key::funckey_modifiers::Shift_Control:
      base.get_key().mods = base.get_key().mods | event::key::modifiers::Shift;
      base.get_key().mods = base.get_key().mods | event::key::modifiers::Ctrl;
      break;
    case event::key::funckey_modifiers::Alt_Control:
      base.get_key().mods = base.get_key().mods | event::key::modifiers::Alt;
      base.get_key().mods = base.get_key().mods | event::key::modifiers::Ctrl;
      break;
    case event::key::funckey_modifiers::Shift_Alt_Control:
      base.get_key().mods = base.get_key().mods | event::key::modifiers::Alt;
      base.get_key().mods = base.get_key().mods | event::key::modifiers::Ctrl;
      base.get_key().mods = base.get_key().mods | event::key::modifiers::Shift;
      break;
    }
    return base;
  }

  term_position cursor_position_{0xFFFF, 0xFFFF};

public:
  /**
   * @brief Query the terminal for the current cursor position (CSI 6n).
   *
   * This method sends the query sequence; the response will be parsed into
   * the event stream and eventually update `cursor_position_`.
   */
  void query_cursor_position() const {
    (void)this;
    ::dpsg::query_cursor_position();
  }

  /**
   * @brief Return the last-observed cursor position reported by the
   *        terminal (or 0xFFFF if unknown).
   */
  term_position cursor_position() const { return cursor_position_; }

  /**
   * @brief Coroutine that polls `STDIN_FILENO` and parses incoming bytes
   *        into high-level `event` values.
   *
   * This generator uses `poll()` + `read()` and performs a finite-state
   * machine parse of VT100 control sequences into `event::key`,
   * `event::mouse` and `event::special` events.
   *
   * @tparam BufSize Buffer size used for raw reads.
   * @tparam Timeout Poll timeout in milliseconds (0 = non-blocking).
   */
  template <size_t BufSize = 32, int Timeout = 0>
  event_stream event_stream() {
    (void)this;
    pollfd fds;
    fds.fd = STDIN_FILENO;
    fds.events = POLLIN;

    enum class parse_state {
      expecting_first,
      expecting_control_character,
      expecting_control_sequence,
      parsing_number,
      expecting_unicode,
      expecting_sun_function_key,
      discard_to_c,
    };

    int first_buffer_char = 0;
    u8 expected_code_points = 0;
    u8 current_code_point = 0;
    event result;

    for (;;) { // BEGIN LOOP_OVER_POLL
      auto poll_result = poll(&fds, 1, Timeout);
      if (poll_result == -1) {
        if (errno == EINTR) {
          continue;
        }
        throw errno_exception{};
      }

      if (poll_result == 0) {
        continue;
      }

      int last = 0;
      char buffer[BufSize];
      last = read(STDIN_FILENO, buffer + first_buffer_char,
                  sizeof(buffer) - first_buffer_char);
      if (last == -1 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
        continue;
      }
      last += first_buffer_char;

      parse_state state{parse_state::expecting_first}; // FSM state
      int current = 0; // Index of the current character in the buffer
      int start_of_new_sequence =
          0; // Where is the start of the current control sequence
      char control_character = 0; // What's the character right after ^[
      u16 num_parameters[4] = {
          0}; // Numeric parameters parsed from a control sequence
      u16 *current_param =
          num_parameters; // Pointer to the number being parsed. Keep it inside
                          // num_parameters

      const auto finalize_parse = [&] {
        // Reset state for the next parse
        state = parse_state::expecting_first;
        do { // reset current_param and num_parameters to 0 (yes, it works)
          *current_param = 0;
        } while ((current_param != num_parameters) && (current_param--, true));

        start_of_new_sequence = current;
        auto temp = result;
        result = event{};
        current_code_point = 0;
        first_buffer_char = 0;
        return temp;
      };
      const auto yield = finalize_parse;

      // BEGIN LOOP_OVER_BUFFER
      while (current < last) {
        char c = buffer[current++];

        if (expected_code_points != 0) {

          result.get_key().data[++current_code_point] = c;
          expected_code_points--;

          if (expected_code_points == 0) {
            result.get_key().mods =
                result.get_key().mods | event::key::modifiers::Unicode;
            co_yield yield();
          }

          continue;
        }

        switch (state) {

        case parse_state::expecting_first: {
          if (c == '\033') {
            // Control character, expect this to be a control sequence
            // If it isn't (ESC or Alt char), the buffer will end there, and
            // we'll deal with the event at the end of the loop
            state = parse_state::expecting_control_character;
          } else {
            expected_code_points =
                from_character(c, event::key::modifiers::None, result);
            if (expected_code_points == 0) {
              co_yield yield();
            }
          }
          break;
        } // parse_state::expecting_first:

        case parse_state::expecting_control_character: {
          switch (c) {
          case '[': { // Control !
            state = parse_state::expecting_control_sequence;
            control_character = c;
            break;
          }
          case 'O': {
            state = parse_state::expecting_sun_function_key;
            break;
          }
          default: {
            expected_code_points =
                from_character(c, event::key::modifiers::Alt, result);
            if (expected_code_points == 0) {
              co_yield yield();
            }
          }
          }
          break;
        } // parse_state::expecting_control_character

        case parse_state::expecting_control_sequence: {
          if (isdigit(c)) {
            state = parse_state::parsing_number;
            *current_param = c - '0';
          } else {
            switch (c) {
            case '<': { // mouse sequence
              state = parse_state::parsing_number;
              break;
            }
            case 'A': {
              result = term_events::arrow_up;
              co_yield yield();
              break;
            }
            case 'B': {
              result = term_events::arrow_down;
              co_yield yield();
              break;
            }
            case 'C': {
              result = term_events::arrow_right;
              co_yield yield();
              break;
            }
            case 'D': {
              result = term_events::arrow_left;
              co_yield yield();
              break;
            }
            case '?': {
              state = parse_state::parsing_number;
              break;
            }
            default: {
              throw invalid_sequence_start(buffer, last, c);
            }
            }
          }
          break;
        } // case parse_state::expecting_control_sequence

        case parse_state::parsing_number: {
          if (isdigit(c)) {
            *current_param = (*current_param * 10) + (c - '0');
          } else {
            switch (c) {
            case ';': {
              current_param++;
              assert(current_param < num_parameters + 4 &&
                     "More than 4 numeric characters in terminal control "
                     "sequence!");
              break;
            }
            case 'm': {
              assert(current_param == num_parameters + 2 &&
                     "Mouse events require exactly 3 values");
              parse_mouse(num_parameters, event::mouse::modifiers::Release,
                          result);
              co_yield yield();
              break;
            }
            case 'M': {
              assert(current_param == num_parameters + 2 &&
                     "Mouse events require exactly 3 values");
              parse_mouse(num_parameters, event::mouse::modifiers::None,
                          result);
              co_yield yield();
              break;
            }
            case 'A': {
              assert(num_parameters[0] == 1 &&
                     current_param == num_parameters + 1 &&
                     "Unknown sequence for arrow key!");
              result = parse_function_key(c, term_events::arrow_up,
                                          num_parameters[1]);
              co_yield yield();
              break;
            }
            case 'B': {
              assert(num_parameters[0] == 1 &&
                     current_param == num_parameters + 1 &&
                     "Unknown sequence for arrow key!");
              result = parse_function_key(c, term_events::arrow_down,
                                          num_parameters[1]);
              co_yield yield();
              break;
            }
            case 'C': {
              assert(num_parameters[0] == 1 &&
                     current_param == num_parameters + 1 &&
                     "Unknown sequence for arrow key!");
              result = parse_function_key(c, term_events::arrow_right,
                                          num_parameters[1]);
              co_yield yield();
              break;
            }
            case 'D': {
              assert(num_parameters[0] == 1 &&
                     current_param == num_parameters + 1 &&
                     "Unknown sequence for arrow key!");
              result = parse_function_key(c, term_events::arrow_left,
                                          num_parameters[1]);
              co_yield yield();
              break;
            }
            case 'P': {
              assert(num_parameters[0] == 1 &&
                     current_param == num_parameters + 1 &&
                     "Unknown sequence for function key!");
              result =
                  parse_function_key(c, term_events::f1, num_parameters[1]);
              co_yield yield();
              break;
            }
            case 'Q': {
              assert(num_parameters[0] == 1 &&
                     current_param == num_parameters + 1 &&
                     "Unknown sequence for function key!");
              result =
                  parse_function_key(c, term_events::f2, num_parameters[1]);
              co_yield yield();
              break;
            }
            case 'R': {
              if (current_param == num_parameters + 1) {
                assert(num_parameters[0] == 1 &&
                       "Unknown sequence for function key!");
                result =
                    parse_function_key(c, term_events::f3, num_parameters[1]);
                co_yield yield();
              } else if (current_param ==
                         num_parameters + 2) { // Cursor position
                cursor_position_ = term_position{.x = num_parameters[1],
                                                 .y = num_parameters[0]};
                finalize_parse();
                co_yield event::special{cursor_position_};
              }
              break;
            }
            case 'S': {
              assert(num_parameters[0] == 1 &&
                     current_param == num_parameters + 1 &&
                     "Unknown sequence for function key!");
              result =
                  parse_function_key(c, term_events::f4, num_parameters[1]);
              co_yield yield();
              break;
            }
            case 'c': {
              assert(num_parameters[0] == 1 &&
                     current_param == num_parameters + 1 &&
                     "Device attribute expects 2 parameters");
              result = term_events::special_event;
              co_yield yield();
              break;
            }
            case '~': { // extension function key
              assert(current_param <= num_parameters + 1 &&
                     "Unknown numeric sequence for extended function key");

              result = parse_function_key(
                  (char)num_parameters[0],
                  // This doesn't matter, the actual code is drawn from the
                  // first parameter
                  term_events::f4,
                  // If we didn't get a modifier key, send 1 (no mod)
                  (current_param == num_parameters + 1) ? num_parameters[1]

                                                        : 1);
              co_yield yield();
              break;
            }
            default: {
              throw unfinished_numeric_sequence(buffer, last, num_parameters,
                                                current_param + 1, c);
            }
            } // switch(c)
            break;
          }
          break;
        } // case parse_state::parsing_number
        case parse_state::expecting_sun_function_key: {
          switch (c) {
          case 'P':
            result = term_events::f1;
            co_yield yield();
            break;
          case 'Q':
            result = term_events::f2;
            co_yield yield();
            break;
          case 'R':
            result = term_events::f3;
            co_yield yield();
            break;
          case 'S':
            result = term_events::f4;
            co_yield yield();
            break;
          default:
            throw invalid_function_key(buffer, last, c);
          }
          break;
        } // case parse_state::expecting_sun_function_key
        case parse_state::discard_to_c:
          if (c == 'c') {
            finalize_parse();
          }
          break;

        } // switch(state)

      } // END LOOP_OVER_BUFFER

      // Alt keys are sent as ^[<char>. If events come really fast, or
      // accumulate in the buffer a long time, it may be difficult to
      // differentiate between the start of a control sequence and a regular
      // Alt/Esc char. Using a bigger buffer may help. In the absence of a
      // better solution, we'll consider that an alt character cannot appear at
      // the end of the buffer That's obviously not true, but should be enough
      // for most cases
      const auto copy_leftover_to_beginning = [&] {
        // End of sequence is 1 behind beginning of new one
        for (auto x = 0; x < (last - start_of_new_sequence + 1); ++x) {
          buffer[x] = buffer[start_of_new_sequence + x];
        }
        first_buffer_char = last - start_of_new_sequence;
      };

      switch (state) {
      case parse_state::expecting_control_sequence: {
        // This is a legitimate Alt char.
        if (last != BufSize) {
          from_character(control_character, event::key::modifiers::Alt, result);
          co_yield yield();
        } else {
          // We have and unfinished sequence on our hands. We'll copy it at the
          // beginning and tell read() to use a smaller buffer for the next pass
          copy_leftover_to_beginning();
        }
        break;
      } // case parse_state::expecting_control_character

      case parse_state::expecting_control_character: {
        // Legitimate Esc
        if (last != BufSize) {
          result = event::key{'\033'};
          co_yield yield();
        }
      } // case parse_state::expecting_control_character

      case parse_state::expecting_first: {
        break; // We're not in the middle of a parse, nothing to do at this
               // point.
      }        // case parse_state::expecting_first

      default:
        // We're in the middle of a parse, but the result is entirely determined
        // by what follows
        copy_leftover_to_beginning();

      } // switch(state)

    } // END LOOP_OVER_POLL

    throw errno_exception{};
  }

  /**
   * @brief Parse events from an `InfiniteCharacterSequence` input source.
   *
   * This variant of `event_stream` accepts any object satisfying the
   * `InfiniteCharacterSequence` concept (see top of file) and parses the
   * incoming characters into `event` values using the same FSM as the
   * `poll`-based generator.
   */
  ::dpsg::generator<event> event_stream(detail::InfiniteCharacterSequence auto input) {

    enum class parse_state {
      expecting_first,
      expecting_control_character,
      expecting_control_sequence,
      parsing_number,
      expecting_unicode,
      expecting_sun_function_key,
      discard_to_c,
    };

    u8 expected_code_points = 0;
    u8 current_code_point = 0;
    event result;
    parse_state state{parse_state::expecting_first}; // FSM state
    char control_character = 0; // What's the character right after ^[
    u16 num_parameters[4] = {
        0}; // Numeric parameters parsed from a control sequence
    u16 *current_param = num_parameters; // Pointer to the number being parsed.
                                         // Keep it inside num_parameters

    const auto finalize_parse = [&] {
      // Reset state for the next parse
      state = parse_state::expecting_first;
      do { // reset current_param and num_parameters to 0 (yes, it works)
        *current_param = 0;
      } while ((current_param != num_parameters) && (current_param--, true));

      auto temp = result;
      result = event{};
      current_code_point = 0;
      return temp;
    };
    const auto yield = finalize_parse;

    // BEGIN LOOP_OVER_BUFFER
    while(input) {
      char c = input.read_char();

      if (expected_code_points != 0) {

        result.get_key().data[++current_code_point] = c;
        expected_code_points--;

        if (expected_code_points == 0) {
          result.get_key().mods =
              result.get_key().mods | event::key::modifiers::Unicode;
          co_yield yield();
        }

        continue;
      }

      switch (state) {

      case parse_state::expecting_first: {
        if (c == '\033') {
          // Control character, expect this to be a control sequence
          // If it isn't (ESC or Alt char), the buffer will end there, and
          // we'll deal with the event at the end of the loop
          state = parse_state::expecting_control_character;
        } else {
          expected_code_points =
              from_character(c, event::key::modifiers::None, result);
          if (expected_code_points == 0) {
            co_yield yield();
          }
        }
        break;
      } // parse_state::expecting_first:

      case parse_state::expecting_control_character: {
        switch (c) {
        case '[': { // Control !
          state = parse_state::expecting_control_sequence;
          control_character = c;
          break;
        }
        case 'O': {
          state = parse_state::expecting_sun_function_key;
          break;
        }
        default: {
          expected_code_points =
              from_character(c, event::key::modifiers::Alt, result);
          if (expected_code_points == 0) {
            co_yield yield();
          }
        }
        }
        break;
      } // parse_state::expecting_control_character

      case parse_state::expecting_control_sequence: {
        if (isdigit(c)) {
          state = parse_state::parsing_number;
          *current_param = c - '0';
        } else {
          switch (c) {
          case '<': { // mouse sequence
            state = parse_state::parsing_number;
            break;
          }
          case 'A': {
            result = term_events::arrow_up;
            co_yield yield();
            break;
          }
          case 'B': {
            result = term_events::arrow_down;
            co_yield yield();
            break;
          }
          case 'C': {
            result = term_events::arrow_right;
            co_yield yield();
            break;
          }
          case 'D': {
            result = term_events::arrow_left;
            co_yield yield();
            break;
          }
          case '?': {
            state = parse_state::parsing_number;
            break;
          }
          default: {
            throw std::runtime_error("Unhandled sequence");
          }
          }
        }
        break;
      } // case parse_state::expecting_control_sequence

      case parse_state::parsing_number: {
        if (isdigit(c)) {
          *current_param = (*current_param * 10) + (c - '0');
        } else {
          switch (c) {
          case ';': {
            current_param++;
            assert(current_param < num_parameters + 4 &&
                   "More than 4 numeric characters in terminal control "
                   "sequence!");
            break;
          }
          case 'm': {
            assert(current_param == num_parameters + 2 &&
                   "Mouse events require exactly 3 values");
            parse_mouse(num_parameters, event::mouse::modifiers::Release,
                        result);
            co_yield yield();
            break;
          }
          case 'M': {
            assert(current_param == num_parameters + 2 &&
                   "Mouse events require exactly 3 values");
            parse_mouse(num_parameters, event::mouse::modifiers::None, result);
            co_yield yield();
            break;
          }
          case 'A': {
            assert(num_parameters[0] == 1 &&
                   current_param == num_parameters + 1 &&
                   "Unknown sequence for arrow key!");
            result =
                parse_function_key(c, term_events::arrow_up, num_parameters[1]);
            co_yield yield();
            break;
          }
          case 'B': {
            assert(num_parameters[0] == 1 &&
                   current_param == num_parameters + 1 &&
                   "Unknown sequence for arrow key!");
            result = parse_function_key(c, term_events::arrow_down,
                                        num_parameters[1]);
            co_yield yield();
            break;
          }
          case 'C': {
            assert(num_parameters[0] == 1 &&
                   current_param == num_parameters + 1 &&
                   "Unknown sequence for arrow key!");
            result = parse_function_key(c, term_events::arrow_right,
                                        num_parameters[1]);
            co_yield yield();
            break;
          }
          case 'D': {
            assert(num_parameters[0] == 1 &&
                   current_param == num_parameters + 1 &&
                   "Unknown sequence for arrow key!");
            result = parse_function_key(c, term_events::arrow_left,
                                        num_parameters[1]);
            co_yield yield();
            break;
          }
          case 'P': {
            assert(num_parameters[0] == 1 &&
                   current_param == num_parameters + 1 &&
                   "Unknown sequence for function key!");
            result = parse_function_key(c, term_events::f1, num_parameters[1]);
            co_yield yield();
            break;
          }
          case 'Q': {
            assert(num_parameters[0] == 1 &&
                   current_param == num_parameters + 1 &&
                   "Unknown sequence for function key!");
            result = parse_function_key(c, term_events::f2, num_parameters[1]);
            co_yield yield();
            break;
          }
          case 'R': {
            if (current_param == num_parameters + 1) {
              assert(num_parameters[0] == 1 &&
                     "Unknown sequence for function key!");
              result =
                  parse_function_key(c, term_events::f3, num_parameters[1]);
              co_yield yield();
            } else if (current_param == num_parameters + 2) { // Cursor position
              cursor_position_ =
                  term_position{.x = num_parameters[1], .y = num_parameters[0]};
              finalize_parse();
              co_yield event::special{cursor_position_};
            }
            break;
          }
          case 'S': {
            assert(num_parameters[0] == 1 &&
                   current_param == num_parameters + 1 &&
                   "Unknown sequence for function key!");
            result = parse_function_key(c, term_events::f4, num_parameters[1]);
            co_yield yield();
            break;
          }
          case 'c': {
            assert(num_parameters[0] == 1 &&
                   current_param == num_parameters + 1 &&
                   "Device attribute expects 2 parameters");
            result = term_events::special_event;
            co_yield yield();
            break;
          }
          case '~': { // extension function key
            assert(current_param <= num_parameters + 1 &&
                   "Unknown numeric sequence for extended function key");

            result = parse_function_key(
                (char)num_parameters[0],
                // This doesn't matter, the actual code is drawn from the
                // first parameter
                term_events::f4,
                // If we didn't get a modifier key, send 1 (no mod)
                (current_param == num_parameters + 1) ? num_parameters[1]

                                                      : 1);
            co_yield yield();
            break;
          }
          default: {
            throw std::runtime_error("Unhandled sequence");
          }
          } // switch(c)
          break;
        }
        break;
      } // case parse_state::parsing_number
      case parse_state::expecting_sun_function_key: {
        switch (c) {
        case 'P':
          result = term_events::f1;
          co_yield yield();
          break;
        case 'Q':
          result = term_events::f2;
          co_yield yield();
          break;
        case 'R':
          result = term_events::f3;
          co_yield yield();
          break;
        case 'S':
          result = term_events::f4;
          co_yield yield();
          break;
        default:
          throw std::runtime_error("Unhandled sequence");
        }
        break;
      } // case parse_state::expecting_sun_function_key
      case parse_state::discard_to_c:
        // seems to be device capabilities
        if (c == 'c') {
          finalize_parse();
        }
        break;

      } // switch(state)

      if (input.buffer_end()) {
        switch (state) {
        case parse_state::expecting_control_sequence: {
          // This is a legitimate Alt char.
          from_character(control_character, event::key::modifiers::Alt, result);
          co_yield yield();
          break;
        } // case parse_state::expecting_control_character

        case parse_state::expecting_control_character: {
          // Legitimate Esc
          result = event::key{'\033'};
          co_yield yield();
        } // case parse_state::expecting_control_character

        case parse_state::expecting_first: {
          break; // We're not in the middle of a parse, nothing to do at this
                 // point.
        }        // case parse_state::expecting_first

        default:
          // We're in the middle of a parse, but the result is entirely
          // determined by what follows
          break;
        } // switch(state)
      }
    } // END LOOP_OVER_BUFFER
  }
};

/**
 * @brief Common RAII typedefs for typical terminal modes.
 *
 * `raw_mode_context` disables signals, echo and canonical mode. `cbreak_mode_context`
 * disables echo and canonical mode only.
 */
using raw_mode_context = raw_mode_context_basic<ISIG | ECHO | ICANON>;
using cbreak_mode_context = raw_mode_context_basic<ECHO | ICANON>;

#ifdef DPSG_COMPILE_LINUX_TERM
struct termios detail::orig_termios {};
bool detail::require_mouse{};
struct sigaction detail::new_sa[MAX_SIGNAL]{}, detail::old_sa[MAX_SIGNAL]{};
#endif

/**
 * @brief Convenience helper that constructs a `raw_mode_context`, runs `f`
 *        and returns its result.
 */
template <std::invocable<raw_mode_context &> F>
std::invoke_result_t<F, raw_mode_context &> with_raw_mode(F &&f) {
  raw_mode_context ctx;
  return f(ctx);
}

/**
 * @brief Convenience helper that constructs a `cbreak_mode_context`, runs `f`
 *        and returns its result.
 */
template <std::invocable<cbreak_mode_context &> F>
std::invoke_result_t<F, cbreak_mode_context &> with_raw_mode(F &&f) {
  cbreak_mode_context ctx;
  return f(ctx);
}

/**
 * @brief Query the current terminal dimensions.
 *
 * First attempts to read `COLUMNS`/`LINES` environment variables, falling
 * back to an `ioctl(TIOCGWINSZ)` call. On failure returns {u16(-1), u16(-1)}.
 */
inline terminal_size get_terminal_size() {
  struct winsize w;
  char *col = getenv("COLUMNS");
  char *row = getenv("LINES");
  if (col != nullptr && row != nullptr) {
    return {(u16)std::atoi(col), (u16)std::atoi(row)};
  }

  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) == -1) {
    return {static_cast<u16>(-1), static_cast<u16>(-1)};
  }
  return {w.ws_col, w.ws_row};
}

} // namespace dpsg
