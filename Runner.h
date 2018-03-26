#pragma once
#include <vector>
#include "ParserTypes.h"
#include "Stack.h"

struct FunctionDefinition {
	std::string functionName;
	std::vector<CharmFunction> functionBody;
};

class Runner {
private:
	//alright, this is the nitty gritty
	//here is the table of function definitions:
	std::vector<FunctionDefinition> functionDefinitions;
	//and this is how you add them
	void addFunctionDefinition(FunctionDefinition fD);

	//handle the functions that we don't know about
	//and / or handle built in functions
	void handleDefinedFunctions(CharmFunction f);
	//this is the name of the current stack that we
	//are working with. by default, this is stack 0
	CharmFunction currentStackName;
	//and here is the list of all of our stacks
	std::vector<Stack> stacks;
public:
	Runner();
	const unsigned int MAX_STACK = 20000;
	bool doesStackExist(CharmFunction name);
	Stack* getCurrentStack();
	std::vector<FunctionDefinition> getFunctionDefinitions();
	void switchCurrentStack(CharmFunction name);
	void createStack(unsigned long long length, CharmFunction name);

	void run(std::vector<CharmFunction> parsedProgram);
};
