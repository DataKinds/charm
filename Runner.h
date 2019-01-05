#pragma once
#include <vector>
#include <unordered_map>
#include <memory>
#include "ParserTypes.h"
#include "Types.h"
#include "Stack.h"

//in PredefinedFunctions.h
class PredefinedFunctions;

//in FFI.h
class FFI;

struct FunctionDefinition {
	std::string functionName;
	std::vector<Token> functionBody;
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
	void handleDefinedFunctions(CharmFunction f);
	//this is the name of the current stack that we
	//are working with. by default, this is stack 0
	CharmFunction currentStackName;
	//and here is the list of all of our stacks
	std::vector<Stack> stacks;
	//and the list of all of our references
	std::vector<Reference> references;

	std::string ns;
public:
	//and this is how you add them
	void addFunctionDefinition(FunctionDefinition fD);

	const unsigned int MAX_STACK = 20000;
	bool doesStackExist(CharmFunction name);
	Stack& getCurrentStack();
	void switchCurrentStack(CharmFunction name);
	void createStack(CharmFunction name);

	CharmFunction getReference(CharmFunction key);
	void setReference(CharmFunction key, CharmFunction value);

	void addNamespacePrefix(CharmFunction& f, std::string ns);

	std::unique_ptr<PredefinedFunctions> pF;
	std::unique_ptr<FFI> ffi;
	std::unordered_map<std::string, FunctionDefinition> definitions;

	Runner(std::string ns);
	void run(std::vector<Token> tokens);
	void runFunction(std::string f);
};
