cmake_minimum_required(VERSION 3.26)
project(AstraLang)

set(CMAKE_CXX_STANDARD 14)

add_executable(AstraLang
        Lexer&Parser/AstraParser.cpp)