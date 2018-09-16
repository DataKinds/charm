#pragma once
#include <vector>
#include <unordered_map>
#include "ParserTypes.h"
#include "Types.h"
#include "Stack.h"

//in PredefinedFunctions.h
class PredefinedFunctions;

//in FFI.h
class FFI;

struct FunctionDefinition {
	std::string functionName;
	std:vector<Token> functionBody;
	CharmFunctionDefinitionInfo definitionInfo;
};

struct Reference {
	CharmFunction key;
	CharmFunction value;
};

void addNamespacePrefix(CharmFunction& f, std::string ns) {
	if (ns == "") {
		return;
	}
	if (f.functionType == NUMBER_FUNCTION) {
		return;
	} else if (f.functionType == STRING_FUNCTION) {
		return;
	} else if (f.functionType == LIST_FUNCTION) {
		for (CharmFunction& currentFunction : f.literalFunctions) {
			addNamespacePrefix(currentFunction, ns);
		}
		return;
	} else if (f.functionType == FUNCTION_DEFINITION) {
		f.functionName = ns + f.functionName;
		for (CharmFunction& currentFunction : f.literalFunctions) {
			addNamespacePrefix(currentFunction, ns);
		}
		return;
	} else if (f.functionType == DEFINED_FUNCTION) {
		bool isAlreadyDefined = (functionDefinitions.find(f.functionName) != functionDefinitions.end());
		bool isPredefinedFunction = (pF->cppFunctionNames.find(f.functionName) != pF->cppFunctionNames.end());
		bool isFFIFunction = (ffi->mutateFFIFuncs.find(f.functionName) != ffi->mutateFFIFuncs.end());
		ONLYDEBUG printf("isAlreadyDefined: %s, isPredefinedFunction: %s, isFFIFunction: %s\n", isAlreadyDefined ? "Yes" : "No", isPredefinedFunction ? "Yes" : "No", isFFIFunction ? "Yes" : "No");
		if (isPredefinedFunction || isFFIFunction || isAlreadyDefined) {
			//don't rename the function if it was defined globally outside of this file
			//or it was already defined (aka: in the prelude)
			//note: adding the "if already defined" clause ensures that functions from its own file don't trip the system,
			//as those functions were already transformed and had their namespace prepended.
		} else {
			f.functionName = ns + f.functionName;
		}
	}
}

extern "C"
class Runner{
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

	PredefinedFunctions& pF;
	FFI& ffi;
	std::unordered_map<std::string, FunctionDefinition> definitions;

	Runner(std::string ns);
	void run(std::vector<Token> tokens);
	void runFunction(std::string f);
}
