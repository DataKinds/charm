#pragma once
#include <stdexcept>
#include <iostream>
static inline void runtime_die(std::string arg) {
    std::cout << "[RUNTIME ERROR]: " << arg << std::endl;
    throw std::runtime_error(arg);
}
static inline void parsetime_die(std::string arg) {
    std::cout << "[PARSE ERROR]: " << arg << std::endl;
    throw std::runtime_error(arg);
}
