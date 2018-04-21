#pragma once
#include <stdexcept>
#if USE_READLINE == true
#define runtime_die(arg) throw std::runtime_error(arg)
#else
#include <iostream>
//we may be in a noninteractive environment, so manually print the errors
static inline void runtime_die(std::string arg) {
    std::cout << arg << std::endl;
    throw std::runtime_error(arg);
}
#endif
