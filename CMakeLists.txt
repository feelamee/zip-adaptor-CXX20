cmake_minimum_required(VERSION 3.27)
project(zip_adaptor)

add_executable(zip_adaptor)
target_compile_features(zip_adaptor PUBLIC cxx_std_20)
target_sources(zip_adaptor PRIVATE main.cpp)
