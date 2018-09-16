#pragma once
#include <vector>
#include <string>
#include <any>
#include <unordered_map>
#include <functional>

#include "ParserTypes.h"


//In Runner.h
class Runner;
class FunctionDefinition;
class RunnerContext;

class PredefinedFunctions {
private:
	//helper function to ensure the function is 2 things:
	//a) a number
	//b) an int
	static bool isInt(CharmFunction f);
public:
	std::unordered_map<std::string, std::function<void(Runner*)>> nativeFunctions;
	PredefinedFunctions();
	void functionLookup(std::string functionName, Runner& r);
	void addBuiltinFunction(std::string n, std::function<void(Runner*)> f);
};
