#pragma once
#include <vector>
#include "ParserTypes.h"

struct FunctionDefinition {
	std::string functionName;
	std::vector<CharmFunction> functionBody;
};

class Runner {
private:
	//the stack is automatically initialized to 8192 zero ints
	std::vector<CharmFunction> stack;
	//says how much of the stack was changed, for printing n stuff
	unsigned int modifiedStackArea;
	//update the modifiedStackArea, really only called on swap
	//because swap is the only one that's hard to predict
	void updateModifiedStackArea();
	//alright, this is the nitty gritty
	//here is the table of function definitions:
	std::vector<FunctionDefinition> functionDefinitions;
	//and this is how you add them
	void addFunctionDefinition(FunctionDefinition fD);
	//return a CharmFunction that for all intents and purposes is zero
	CharmFunction zeroF();
	//handle the functions that we don't know about
	//and / or handle built in functions
	void handleDefinedFunctions(CharmFunction f);
public:
	Runner();
	const unsigned int MAX_STACK = 8192;
	//a helper function to see if a charm function is a number / an int
	bool isInt(CharmFunction f);
	std::vector<CharmFunction> getStack();
	unsigned int getModifiedStackArea();
	std::vector<FunctionDefinition> getFunctionDefinitions();
	//push to top of stack
	void push(CharmFunction f);
	//pop off top of stack
	CharmFunction pop();
	//swap values at index n1 and n2 from the top (zero-indexed)
	void swap(unsigned long long n1, unsigned long long n2);
	void run(std::vector<CharmFunction> parsedProgram);
};
