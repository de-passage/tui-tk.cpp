# tui-tk.cpp Copilot Instructions

## Project Overview
**tui-tk.cpp** is a C++23 Terminal User Interface (TUI) toolkit that provides asynchronous event handling, terminal control, and character stream processing for building interactive terminal applications on Linux.

## Architecture

### Core Components

1. **Event System** ([linux_term.hpp](../src/linux_term.hpp#L207))
   - `event::key`: Keyboard input with Unicode support and modifier flags (Shift, Alt, Ctrl)
   - `event::mouse`: Mouse events with position tracking and button states
   - Events flow from terminal input → character stream parsing → event delivery

2. **Character Stream Processing** ([char_stream.hpp](../src/char_stream.hpp))
   - Coroutine-based buffered stream for parsing terminal escape sequences
   - Uses `yield_value()` to emit parsed data and internal buffering
   - Pattern: Generator coroutine yields string views, caller consumes and prunes buffer
   - Example: `char_stream` holds buffer, yields available data when ready

3. **VT100 Escape Sequence Handling** ([vt100.hpp](../src/vt100.hpp), [linux_term.hpp](../src/linux_term.hpp))
   - Compile-time terminal code generation using template sequences
   - `termcode_sequence<Size, End, Begin>` encodes escape codes with compile-time constraints
   - Parses key sequences, mouse events, and terminal status from stdin
   - Exceptions: `invalid_sequence`, `unfinished_numeric_sequence` for malformed input

4. **Linux Terminal Interface** ([linux_term.hpp](../src/linux_term.hpp), [linux_term.cpp](../src/linux_term.cpp))
   - Manages raw terminal mode, signal handlers, and POSIX syscalls (`ioctl`, `poll`, `termios`)
   - Generators (`generator<T>`) yield parsed events from input stream
   - Concept `InfiniteCharacterSequence` defines parser contract

5. **Async UI Thread** ([main.cpp](../src/main.cpp#L38-L70))
   - `basic_ui_thread<E>`: Thread-safe event queue using `std::mutex`
   - `task_on_ui_thread`: Coroutine type for scheduling work on UI thread
   - Pattern: `co_await ui_thread` to suspend and enqueue task for UI thread processing

### Data Flow
```
stdin (raw bytes) → poll() → char_stream (buffered) 
→ parse VT100 sequences → event::key or event::mouse 
→ ui_thread event queue → coroutine resumption
```

## Key Patterns & Conventions

### Coroutine Usage
- **Generators**: Use `generator<T>` for async parsers that yield partial results
- **Infinite Sequences**: Implement `InfiniteCharacterSequence` concept for character sources
- **UI Tasks**: Return `task_on_ui_thread` from coroutines that need UI thread serialization
- Mark coroutine handles carefully; use move semantics (no copy)

### Type Aliases
[types.hpp](../src/types.hpp) defines compact integer types: `i8`/`u8`, `i16`/`u16`, `i32`/`u32`, `i64`/`u64`. Use these in binary-packed structs like `event::key` and `event::mouse`.

### Error Handling
- Terminal parsing errors raise `invalid_sequence` subclasses (not thrown to caller by default)
- Check stream status via `status_` field in promise: `status::reading`, `status::eof`, `status::failed`
- Use `errno_exception` for system call failures

### Memory Layout
- `event::key` and `event::mouse` pack into 8 bytes with union overlays for compact event delivery
- `key.data[8]` stores code point bytes; `key.code_point_count()` determines valid length
- Mouse position stored as `u16 x, y` with bounds from terminal size

### Formatting & Output
- Optional `fmt` library support (CMake detects presence; fallback to `std::format`)
- Macro guard: `USE_FMT` controls linking; `DPSG_NO_FMT` disables in code

## Build & Test

### Build
```bash
cmake -B build -DCMAKE_CXX_COMPILER=g++ -DCMAKE_BUILD_TYPE=Release
cmake --build build
./build/tui  # Main executable
```

### Dependencies
- **C++23 compiler**: GCC ≥13 or Clang ≥17 (coroutines, `co_await`, `std::format`)
- **pthread**: Linked for thread support
- **fmt** (optional): High-performance formatting library

### Test Structure
- Tests exist in [tests/](../tests/) but CMakeLists.txt is minimal/empty
- `src/` contains POC and benchmark files: `poc_char_stream.cpp`, `benchmarks.cpp` (not linked by default)

## Common Development Tasks

### Adding a New Event Type
1. Define struct in [linux_term.hpp](../src/linux_term.hpp) event union (keep ≤8 bytes)
2. Add parsing generator in `linux_term.hpp` that yields the event
3. Wire generator into input polling loop in [linux_term.cpp](../src/linux_term.cpp)
4. Update [print_events.cpp](../src/print_events.cpp) for debug output

### Parsing a New Escape Sequence
1. Study VT100 spec (ESC + parameters)
2. Create `char_stream` coroutine in `linux_term.hpp` that parses incrementally
3. Yield parsed struct when complete, handle `unfinished_numeric_sequence`
4. Integrate into main event loop—generator yields events in parse order

### Modifying Terminal State
- Use `ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws)` for window size
- Call `tcsetattr()` to restore terminal attributes on exit
- Raw mode setup: disable canonical mode, echo, signals

## Important Files Reference
- Core event loop: [main.cpp](../src/main.cpp)
- Terminal abstraction: [linux_term.hpp](../src/linux_term.hpp) (~1587 lines)
- Character stream coroutine: [char_stream.hpp](../src/char_stream.hpp)
- Event printing (debug): [print_events.cpp](../src/print_events.cpp)
- VT100 code generation: [vt100.hpp](../src/vt100.hpp)
