#pragma once

#include <utility>

#define FWD(...) std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

template <class F> struct defered {
  F f;
  defered(std::same_as<F> auto &&f) : f{FWD(f)} {}
  ~defered() { f(); }
};
template <class F> defered(F &&) -> defered<std::decay_t<F>>;

#define DEFER defered delay_##__COUNTER__ = [&]()
