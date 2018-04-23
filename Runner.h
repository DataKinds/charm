#pragma once
#include <vector>
#include <unordered_map>
#include "ParserTypes.h"
#include "Stack.h"

//in PredefinedFunctions.h
class PredefinedFunctions;

//in FFI.h
class FFI;

struct FunctionDefinition {
	std::string functionName;
	CHARM_LIST_TYPE functionBody;
	CharmFunctionDefinitionInfo definitionInfo;
};

struct Reference {
	CharmFunction key;
	CharmFunction value;
};

extern "C"
class Runner {
private:
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
public:
	Runner();
	//and this is how you add them
	void addFunctionDefinition(FunctionDefinition fD);

	//all of our instances containing any sort of functions are right here:
	PredefinedFunctions* pF;
	FFI* ffi;
	std::unordered_map<std::string, FunctionDefinition> functionDefinitions;
	
	//type signature runtime checking
	std::optional<std::vector<CharmFunction>> typeSignatureTick(std::string name, RunnerContext* context);
	void typeSignatureTock(std::vector<CharmFunction> tick);

	const unsigned int MAX_STACK = 20000;
	bool doesStackExist(CharmFunction name);
	Stack* getCurrentStack();
	void switchCurrentStack(CharmFunction name);
	void createStack(unsigned long long length, CharmFunction name);

	CharmFunction getReference(CharmFunction key);
	void setReference(CharmFunction key, CharmFunction value);

	void addNamespacePrefix(CharmFunction& f, std::string ns);
	void runWithContext(CHARM_LIST_TYPE parsedProgram, RunnerContext* context, std::string ns = "");
	void run(std::pair<CHARM_LIST_TYPE, FunctionAnalyzer*> parsedProgramWithAnalyzer, std::string ns = "");
};
