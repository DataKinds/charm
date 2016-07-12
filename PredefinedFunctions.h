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
	static void newline(Runner* r);
	//STACK MANIPULATIONS
	static void dup(Runner* r);
	static void pop(Runner* r);
	static void swap(Runner* r);
	//LIST / STRING MANIPULATIONS
	static void len(Runner* r);
	static void at(Runner* r);
	static void insert(Runner* r);
	static void concat(Runner* r);
	//CONTROL FLOW
	static void i(Runner* r);
	static void ifthen(Runner* r);
	//BOOLEAN OPS
	static void nor(Runner* r);
	//TYPE INSPECIFIC MATH
	static void abs(Runner* r);
	//INTEGER OPS
	static void plusI(Runner* r);
	static void minusI(Runner* r);
	static void divI(Runner* r);
	static void timesI(Runner* r);
	static void modulo(Runner* r);
	static void toInt(Runner* r);
};
