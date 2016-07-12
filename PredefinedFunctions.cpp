#include <string>
#include <vector>

#include "PredefinedFunctions.h"

const std::vector<std::string> PredefinedFunctions::cppFunctionNames = {
	//STACK MANIPLULATIONS
	"dup", "pop", "swap",
	//CONTROL FLOW
	"i", "ifthen",
	//MATH OPS AND TYPE COERCION
	"+", "-", "/", "*", "tofloat", "toint"
};
