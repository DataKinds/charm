#pragma once
#include <vector>
#include "ParserTypes.h"
#include "Stack.h"

//in PredefinedFunctions.h
class PredefinedFunctions;

struct FunctionDefinition {
	std::string functionName;
	CHARM_LIST_TYPE functionBody;
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
	void handleDefinedFunctions(CharmFunction f, RunnerContext* context);
	//this is the name of the current stack that we
	//are working with. by default, this is stack 0
	CharmFunction currentStackName;
	//and here is the list of all of our stacks
	std::vector<Stack> stacks;
	//and the list of all of our references
	std::vector<Reference> references;
	//a private instance of PredefinedFunctions
	PredefinedFunctions* pF;
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

	void runWithContext(CHARM_LIST_TYPE parsedProgram, RunnerContext* context);
	void run(std::pair<CHARM_LIST_TYPE, FunctionAnalyzer*> parsedProgramWithAnalyzer);
};
