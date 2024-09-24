#ifndef HEADER_GUARD_DPSG_FMT_DEFINITIONS_HPP
#define HEADER_GUARD_DPSG_FMT_DEFINITIONS_HPP
#ifdef  DPSG_NO_FMT
#include <print>
#include <format>
#define dpsg_print(...) ::std::print(__VA_ARGS__)
#define dpsg_format(...) ::std::format(__VA_ARGS__)
#define dpsg_format_to(...) ::std::format_to(__VA_ARGS__)
#else
#include <fmt/core.h>
#include <fmt/chrono.h>
#define dpsg_print(...) ::fmt::print(__VA_ARGS__)
#define dpsg_format(...) ::fmt::format(__VA_ARGS__)
#define dpsg_format_to(...) ::fmt::format_to(__VA_ARGS__)
#endif
#endif // HEADER_GUARD_DPSG_FMT_DEFINITIONS_HPP
