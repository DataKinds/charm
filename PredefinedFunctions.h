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
	static void functionLookup(std::string functionName, Runner* r, FunctionDefinition* context);
	//INPUT / OUTPUT
	static inline void print(CharmFunction f1);
	static inline void p(Runner* r);
	static inline void newline(Runner* r);
	//STACK MANIPULATIONS
	static inline void dup(Runner* r);
	static inline void pop(Runner* r);
	static inline void swap(Runner* r);
	//LIST / STRING MANIPULATIONS
	static inline void len(Runner* r);
	static inline void at(Runner* r);
	static inline void insert(Runner* r);
	static inline void concat(Runner* r);
	//CONTROL FLOW
	static inline void i(Runner* r);
	static inline void ifthen(Runner* r, FunctionDefinition* context);
	//BOOLEAN OPS
	static inline void nor(Runner* r);
	//TYPE INSPECIFIC MATH
	static inline void abs(Runner* r);
	//INTEGER OPS
	static inline void plusI(Runner* r);
	static inline void minusI(Runner* r);
	static inline void divI(Runner* r);
	static inline void timesI(Runner* r);
	static inline void toInt(Runner* r);
	//STACK CREATION/DESTRUCTION
	static inline void createStack(Runner* r);
	static inline void switchStack(Runner* r);
	//REF GETTING/SETTING
	static inline void getRef(Runner* r);
	static inline void setRef(Runner* r);
};
