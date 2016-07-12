#pragma once
#include <stdexcept>

void runtime_die(const std::string arg) {
	throw std::runtime_error(arg);
}
