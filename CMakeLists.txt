cmake_minimum_required(VERSION 3.26)
project(AstraLang)

set(CMAKE_CXX_STANDARD 23)

# Specify the path to LLVM
set(LLVM_DIR "/opt/homebrew/opt/llvm/lib/cmake/llvm")

# Find LLVM package
find_package(LLVM REQUIRED CONFIG)

# Include LLVM directories
include_directories(${LLVM_INCLUDE_DIRS})

# Add LLVM definitions
add_definitions(${LLVM_DEFINITIONS})

# Specify the executable and its source files
add_executable(AstraLang Compiler/main.cpp)

# Map LLVM component names to library names and link against LLVM libraries
llvm_map_components_to_libnames(llvm_libs support core irreader)
target_link_libraries(AstraLang ${llvm_libs})
