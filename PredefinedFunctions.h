#pragma once
#include <vector>
#include <string>

#include "Runner.h"
#include "ParserTypes.h"

class PredefinedFunctions {
private:
	//helper function to ensure the function is 2 things:
	//a) a number
	//b) an int
	static bool isInt(CharmFunction f);
public:
	static const std::vector<std::string> cppFunctionNames;
	static void functionLookup(std::string functionName, Runner* r);
	//INPUT / OUTPUT
	static void print(CharmFunction f1);
	static void p(Runner* r);
	//STACK MANIPULATIONS
	static void dup(Runner* r);
	static void pop(Runner* r);
	static void swap(Runner* r);
};
