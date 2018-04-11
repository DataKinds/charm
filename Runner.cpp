#include <vector>

#include "Runner.h"
#include "ParserTypes.h"
#include "PredefinedFunctions.h"
#include "Error.h"
#include "Debug.h"

void Runner::addFunctionDefinition(FunctionDefinition fD) {
	//first, check and make sure there's no other definition with
	//the same name. if there is, overwrite it. if not, just push_back
	//this definition.
	bool functionExists = false;
	for (unsigned long long definitionIndex = 0; definitionIndex < Runner::functionDefinitions.size(); definitionIndex++) {
		if (functionDefinitions[definitionIndex].functionName == fD.functionName) {
			functionDefinitions[definitionIndex] = fD;
			functionExists = true;
		}
	}
	if (!functionExists) {
		functionDefinitions.push_back(fD);
	}
}

Runner::Runner() {
	//initialize the stacks
	CharmFunction zero = Stack::zeroF();
	currentStackName = zero;
	stacks.push_back(Stack(MAX_STACK, zero));
	pF = new PredefinedFunctions();
}

bool Runner::doesStackExist(CharmFunction name) {
	for (Stack stack : stacks) {
		if (stack.isNameEqualTo(name)) {
			return true;
		}
	}
	return false;
}
Stack* Runner::getCurrentStack() {
	for (unsigned int s = 0; s < stacks.size(); s++) {
		if (stacks[s].isNameEqualTo(Runner::currentStackName)) {
			return &(stacks[s]);
		}
	}
	return &(stacks[0]);
}

void Runner::switchCurrentStack(CharmFunction name) {
	if (Runner::doesStackExist(name)) {
		Runner::currentStackName = name;
	} else {
		runtime_die("Tried to switch to stack which does not exist.");
	}
}

void Runner::createStack(unsigned long long length, CharmFunction name) {
	if (Runner::doesStackExist(name)) {
		runtime_die("Tried to create stack that already exists.");
	} else {
		stacks.push_back(Stack(length, name));
	}
}

CharmFunction Runner::getReference(CharmFunction key) {
	for (Reference r : references) {
		if (r.key == key) {
			return r.value;
		}
	}
	return Stack::zeroF();
}

void Runner::setReference(CharmFunction key, CharmFunction value) {
	Reference newRef;
	newRef.key = key;
	newRef.value = value;
	for (unsigned long long n = 0; n < references.size(); n++) {
		if (references[n].key == key) {
			//if the ref was previously defined
			references[n] = newRef;
			return;
		}
	}
	//if it wasn't previously defined then
	references.push_back(newRef);
}

std::vector<FunctionDefinition> Runner::getFunctionDefinitions() {
	return Runner::functionDefinitions;
}

void Runner::handleDefinedFunctions(CharmFunction f, FunctionDefinition* context) {
	//PredefinedFunctions.h holds all the functions written in C++
	//other than that, if these functions aren't built in, they are run through
	//the functionDefinitions table.

	//first, make sure that the function we're trying to run exists in the PredefinedFunctions
	//table. if it doesn't - assume it's defined in Charm and run through the
	//functionDefinitions table.
	bool isPredefinedFunction = (pF->cppFunctionNames.find(f.functionName) != pF->cppFunctionNames.end());
	if (isPredefinedFunction) {
		//run the predefined function!
		//(note: the function context AKA the definition we are running code from
		//is passed in for tail call optimization in PredefinedFunctions.cpp::ifthen())
		pF->functionLookup(f.functionName, this, context);
	} else {
		//alright, now we get down and dirty
		//look through the functionDefinitions table for a function with
		//a matching name, and run that. if there are no functions - throw
		//an error.
		bool functionFound = false;
		for (FunctionDefinition fD : functionDefinitions) {
			if (fD.functionName == f.functionName) {
				functionFound = true;
				//wait! before we run it, check and make sure this function isn't tail recursive
				if (fD.definitionInfo.tailCallRecursive) {
					//if it is, drop the last call to itself and just run it in a loop
					//TODO: exiting a tail-call loop?
					CHARM_LIST_TYPE functionBodyCopy = fD.functionBody;
					functionBodyCopy.pop_back();
					while (1) {
						Runner::run(functionBodyCopy);
					}
				}
				//ooh. the only time we use this call!
				FunctionDefinition fDCopy = fD;
				Runner::runWithDefinitionContext(fD.functionBody, &fDCopy);
			}
		}
		if (!functionFound) {
			runtime_die("Unknown function `" + f.functionName + "`.");
		}
	}
}

void Runner::runWithDefinitionContext(CHARM_LIST_TYPE parsedProgram, FunctionDefinition* context) {
	for (CharmFunction currentFunction : parsedProgram) {
		//alright, now we get into the running portion
		if (currentFunction.functionType == NUMBER_FUNCTION) {
			ONLYDEBUG puts("RUNNING AS NUMBER_FUNCTION");
			//first, let's do the numbers
			Runner::getCurrentStack()->push(currentFunction);
			//easy, right? let's do more
		} else if (currentFunction.functionType == STRING_FUNCTION) {
			ONLYDEBUG puts("RUNNING AS STRING_FUNCTION");
			//now we push strings onto the stack
			Runner::getCurrentStack()->push(currentFunction);
			//still p easy ye
		} else if (currentFunction.functionType == LIST_FUNCTION) {
			ONLYDEBUG puts("RUNNING AS LIST_FUNCTION");
			//now we push on the lists
			Runner::getCurrentStack()->push(currentFunction);
			//wow this is easy right? now get ready baby
		} else if (currentFunction.functionType == FUNCTION_DEFINITION) {
			ONLYDEBUG puts("RUNNING AS FUNCTION_DEFINTION");
			//lets define some functions bruh
			FunctionDefinition tempFunction;
			tempFunction.functionName = currentFunction.functionName;
			tempFunction.functionBody = currentFunction.literalFunctions;
			tempFunction.definitionInfo = currentFunction.definitionInfo;
			Runner::addFunctionDefinition(tempFunction);
			ONLYDEBUG printf("ADDED FUNCTION DEFINITION FOR %s\n", tempFunction.functionName.c_str());
			//that was easy too! oh no...
		} else if (currentFunction.functionType == DEFINED_FUNCTION) {
			ONLYDEBUG puts("RUNNING AS DEFINED_FUNCTION");
			//let's do these defined functions now
			Runner::handleDefinedFunctions(currentFunction, context);
			//lol you thought i'd do it here
		}
	}
	ONLYDEBUG puts("EXITING RUNNER::RUN");
}

void Runner::run(CHARM_LIST_TYPE parsedProgram) {
	Runner::runWithDefinitionContext(parsedProgram, nullptr);
}
