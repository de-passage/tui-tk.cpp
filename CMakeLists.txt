cmake_minimum_required(VERSION 3.29)

project(tui LANGUAGES CXX VERSION 0.1.0)

find_package(fmt)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(SRC_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/src/)

set(SRC_FILES main.cpp linux_term.cpp char_stream.cpp print_events.cpp)

list(TRANSFORM SRC_FILES PREPEND ${SRC_DIRECTORY})

add_executable(tui ${SRC_FILES})
target_link_libraries(tui fmt::fmt pthread)

