cmake_minimum_required(VERSION 3.14)
project(LispyCpp)

set(CMAKE_CXX_STANDARD 17)

add_executable(LispyCpp main.cpp mpc.c)
link_libraries(edit)
target_link_libraries(LispyCpp edit)
