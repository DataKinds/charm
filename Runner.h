#pragma once
#include <vector>
#include "ParserTypes.h"
#include "Stack.h"

struct FunctionDefinition {
	std::string functionName;
	std::vector<CharmFunction> functionBody;
	CharmFunctionDefinitionInfo definitionInfo;
};

struct Reference {
	CharmFunction key;
	CharmFunction value;
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
	void handleDefinedFunctions(CharmFunction f, FunctionDefinition* context);
	//this is the name of the current stack that we
	//are working with. by default, this is stack 0
	CharmFunction currentStackName;
	//and here is the list of all of our stacks
	std::vector<Stack> stacks;
	//and the list of all of our references
	std::vector<Reference> references;
public:
	Runner();
	std::vector<FunctionDefinition> getFunctionDefinitions();

	const unsigned int MAX_STACK = 20000;
	bool doesStackExist(CharmFunction name);
	Stack* getCurrentStack();
	void switchCurrentStack(CharmFunction name);
	void createStack(unsigned long long length, CharmFunction name);

	CharmFunction getReference(CharmFunction key);
	void setReference(CharmFunction key, CharmFunction value);

	void runWithDefinitionContext(std::vector<CharmFunction> parsedProgram, FunctionDefinition* context);
	void run(std::vector<CharmFunction> parsedProgram);
};
