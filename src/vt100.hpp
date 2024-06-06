#ifndef HEADER_GUARD_DPSG_VT100_HPP
#define HEADER_GUARD_DPSG_VT100_HPP

#include <cstdint>
#include <iostream>
#include <ostream>
#include <tuple>

namespace dpsg::vt100 {
// TYPES & OPERATIONS
template <std::size_t S, char End = 0, char Begin = 0>
struct termcode_sequence {
  uint8_t codes[S];
};

template <char End, char Begin> struct termcode_sequence<0, End, Begin> {};

namespace detail {
template <char B, char E, class... Chars>
inline constexpr termcode_sequence<sizeof...(Chars), E, B>
termcode_sequence_from(Chars... chars) noexcept {
  return termcode_sequence<sizeof...(Chars), E, B>{(uint8_t)chars...};
}
} // namespace detail

template <char End> using termcode = termcode_sequence<1, End>;

template <char End> using single_termcode = termcode_sequence<0, End>;

namespace detail {
template <class... Ts> struct termcode_tuple : std::tuple<Ts...> {};
template <class... Ts>
termcode_tuple(std::tuple<Ts...>) -> termcode_tuple<Ts...>;

template <class... Ts, class... Us>
constexpr auto operator|(termcode_tuple<Ts...> t1,
                         termcode_tuple<Us...> t2) noexcept {
  return termcode_tuple{std::tuple_cat(static_cast<std::tuple<Ts...>>(t1),
                                       static_cas<std::tuple<Us...>>(t2))};
}

template <class... Ts, size_t S, char E, char B>
constexpr auto operator|(termcode_tuple<Ts...> t1,
                         termcode_sequence<S, E, B> t2) noexcept {
  return termcode_tuple{
      std::tuple_cat(static_cast<std::tuple<Ts...>>(t1), std::make_tuple(t2))};
}

template <class... Ts, size_t S, char E, char B>
constexpr auto operator|(termcode_sequence<S, E, B> t1,
                         termcode_tuple<Ts...> t2) noexcept {
  return termcode_tuple{
      std::tuple_cat(std::make_tuple(t1), static_cast<std::tuple<Ts...>>(t2))};
}

template <std::size_t... I1, std::size_t... I2, char End, char Begin>
constexpr auto concat_impl(termcode_sequence<sizeof...(I1), End, Begin> t1,
                           termcode_sequence<sizeof...(I2), End, Begin> t2,
                           std::index_sequence<I1...> /*unused*/,
                           std::index_sequence<I2...> /*unused*/) noexcept {
  return termcode_sequence<sizeof...(I1) + sizeof...(I2), End, Begin>{
      .codes = {t1.codes[I1]..., t2.codes[I2]...}};
}
} // namespace detail

template <std::size_t S1, std::size_t S2, char End, char Begin,
          std::enable_if_t<End != 0, int> = 0>
constexpr auto operator|(termcode_sequence<S1, End, Begin> s1,
                         termcode_sequence<S2, End, Begin> s2) noexcept {
  return detail::concat_impl(s1, s2, std::make_index_sequence<S1>{},
                             std::make_index_sequence<S2>{});
}

template <size_t S1, size_t S2, char E, char F, char B, char C,
          std::enable_if_t<(E != F || E == 0), int> = 0>
constexpr auto operator|(termcode_sequence<S1, E, B> t1,
                         termcode_sequence<S2, F, C> t2) noexcept {
  return detail::termcode_tuple{std::make_tuple(t1, t2)};
}

template <class C, std::size_t S, char End, char Begin>
std::basic_ostream<C> &operator<<(std::basic_ostream<C> &os,
                                  const termcode_sequence<S, End, Begin> &s) {
  os << "\033";
  if constexpr (S > 0) {
    os << "[";
  }
  if constexpr (Begin != 0) {
    os << Begin;
  }
  if constexpr (S > 0) {
    os << static_cast<int>(s.codes[0]);
  }
  for (std::size_t i = 1; i < S; ++i) {
    os << ';' << static_cast<int>(s.codes[i]);
  }
  if constexpr (End != 0) {
    os << End;
  }
  return os;
}

template <class... Ts>
constexpr decltype(auto) operator<<(auto &os, detail::termcode_tuple<Ts...> t) {
  ((os << std::get<Ts>(t)), ...);
  return os;
}

// BASIC COLORS
enum class color : uint8_t {
  black = 0,
  red,
  green,
  yellow,
  blue,
  magenta,
  cyan,
  white,
};

using color_termcode = termcode<'m'>;
template <size_t S> using color_termcode_sequence = termcode_sequence<S, 'm'>;

static constexpr color_termcode reset{0};

inline constexpr color_termcode bg(color c) noexcept {
  return color_termcode{static_cast<uint8_t>(static_cast<uint8_t>(c) + 40)};
}

inline constexpr color_termcode fg(color c) noexcept {
  return color_termcode{static_cast<uint8_t>(static_cast<uint8_t>(c) + 30)};
}

static constexpr inline auto green = fg(color::green);
static constexpr inline auto red = fg(color::red);
static constexpr inline auto black = fg(color::black);
static constexpr inline auto blue = fg(color::blue);
static constexpr inline auto cyan = fg(color::cyan);
static constexpr inline auto magenta = fg(color::magenta);
static constexpr inline auto yellow = fg(color::yellow);
static constexpr inline auto white = fg(color::white);
static constexpr inline color_termcode bold{1};
static constexpr inline color_termcode faint{2};
static constexpr inline color_termcode italic{3};
static constexpr inline color_termcode underline{4};
static constexpr inline color_termcode blink{5};
static constexpr inline color_termcode reverse{7};
static constexpr inline color_termcode conceal{8};
static constexpr inline color_termcode crossed{9};

// SCREEN MANIPULATION

static constexpr inline single_termcode<'c'> reset_init{};

enum class clear_mode : uint8_t {
  from_cursor = 0,
  to_cursor = 1,
  all = 2,
};
inline constexpr auto clear_screen(clear_mode m = clear_mode::all) noexcept {
  return termcode_sequence<1, 'J'>{static_cast<uint8_t>(m)};
}
inline constexpr auto clear_line(clear_mode m) noexcept {
  return termcode_sequence<1, 'K'>{static_cast<uint8_t>(m)};
}

static constexpr inline auto clear = clear_screen(clear_mode::all);

// CURSOR MANIPULATION

inline constexpr auto cursor_up(uint8_t n) noexcept {
  return termcode_sequence<1, 'A'>{n};
}
inline constexpr auto cursor_down(uint8_t n) noexcept {
  return termcode_sequence<1, 'B'>{n};
}
inline constexpr auto cursor_forward(uint8_t n) noexcept {
  return termcode_sequence<1, 'C'>{n};
}
inline constexpr auto cursor_backward(uint8_t n) noexcept {
  return termcode_sequence<1, 'D'>{n};
}

inline constexpr auto set_cursor(uint8_t x, uint8_t y) noexcept {
  return termcode_sequence<2, 'H'>{x, y};
}

static inline constexpr auto hide_cursor =
    detail::termcode_sequence_from<'?', 'l'>(25);
static inline constexpr auto show_cursor =
    detail::termcode_sequence_from<'?', 'h'>(25);

static constexpr inline single_termcode<'H'> home_cursor{};

// TRUE COLORS
namespace detail {
struct rgb : termcode_sequence<5, 'm'> {
  uint8_t &r() { return codes[2]; }
  uint8_t &g() { return codes[3]; }
  uint8_t &b() { return codes[4]; }
  [[nodiscard]] uint8_t r() const { return codes[2]; }
  [[nodiscard]] uint8_t g() const { return codes[3]; }
  [[nodiscard]] uint8_t b() const { return codes[4]; }
};
} // namespace detail
constexpr inline detail::rgb setf(uint8_t r, uint8_t g, uint8_t b) noexcept {
  return detail::rgb{{38, 2, r, g, b}};
}
constexpr inline detail::rgb setb(uint8_t r, uint8_t g, uint8_t b) noexcept {
  return detail::rgb{{48, 2, r, g, b}};
}

// UTILITY
template <size_t S, typename T> struct generic_decorate {
  termcode_sequence<S, 'm'> codes;
  T value;
};
template <size_t S, typename T>
generic_decorate(color_termcode_sequence<S>, T) -> generic_decorate<S, T>;

template <size_t S>
struct decorate_sw : generic_decorate<S, std::string_view> {};
template <size_t S>
decorate_sw(color_termcode_sequence<S>, std::string_view) -> decorate_sw<S>;
template <size_t S>
decorate_sw(color_termcode_sequence<S>, const char *) -> decorate_sw<S>;
template <size_t S>
decorate_sw(color_termcode_sequence<S>, const std::string &) -> decorate_sw<S>;

template <size_t S>
inline constexpr auto decorate(color_termcode_sequence<S> ct,
                               const std::string &s) {
  return decorate_sw<S>{{ct, s}};
}
template <size_t S>
inline constexpr auto decorate(color_termcode_sequence<S> ct,
                               std::string_view s) {
  return decorate_sw<S>{{ct, s}};
}
template <size_t S>
inline constexpr auto decorate(color_termcode_sequence<S> ct, const char *s) {
  return decorate_sw<S>{{ct, s}};
}
template <size_t S, typename T>
inline constexpr auto decorate(color_termcode_sequence<S> ct, T c) {
  return generic_decorate<S, T>{ct, c};
}

template <size_t S, typename T>
std::ostream &operator<<(std::ostream &os, const generic_decorate<S, T> &d) {
  return os << d.codes << d.value << reset;
}
} // namespace dpsg::vt100

#endif // HEADER_GUARD_DPSG_VT100_HPP
