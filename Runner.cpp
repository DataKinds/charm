#include <vector>

#include "Runner.h"
#include "ParserTypes.h"
#include "PredefinedFunctions.h"
#include "Error.h"

CharmFunction Runner::zeroF() {
	CharmFunction zeroFunction;
	CharmNumber zeroNumber;
	zeroNumber.whichType = INTEGER_VALUE;
	zeroNumber.integerValue = 0;
	zeroFunction.functionType = NUMBER_FUNCTION;
	zeroFunction.numberValue = zeroNumber;
	return zeroFunction;
}

CharmFunction Runner::pop() {
	//ensure that the stack never changes size
	//this is by placing more zeroes at the start
	//as stuff is popped off the end
	Runner::stack.insert(Runner::stack.begin(), Runner::zeroF());
	CharmFunction tempCharmF = Runner::stack.at(Runner::stack.size() - 1);
	Runner::stack.pop_back();
	if (Runner::modifiedStackArea != 0) Runner::modifiedStackArea--;
	return tempCharmF;
}

void Runner::push(CharmFunction f) {
	//ensure the stack never changes size again
	//pop an element off the back of the stack
	Runner::stack.push_back(f);
	Runner::stack.erase(stack.begin());
	Runner::modifiedStackArea++;
}

void Runner::swap(unsigned long long n1, unsigned long long n2) {
	CharmFunction tempFromN1 = Runner::stack.at(n1);
	CharmFunction tempFromN2 = Runner::stack.at(n2);
	Runner::stack[n1] = tempFromN2;
	Runner::stack[n2] = tempFromN1;
	Runner::updateModifiedStackArea();
}

bool Runner::isInt(CharmFunction f) {
	if (f.functionType == NUMBER_FUNCTION) {
		if (f.numberValue.whichType == INTEGER_VALUE) {
			return true;
		}
	}
	return false;
}

bool Runner::isFloat(CharmFunction f) {
	if (f.functionType == NUMBER_FUNCTION) {
		if (f.numberValue.whichType == FLOAT_VALUE) {
			return true;
		}
	}
	return false;
}

void Runner::updateModifiedStackArea() {
	//go from the front of the stack to the back
	//then set the modifiedStackArea accordingly
	for (unsigned int stackIndex = 0; stackIndex < MAX_STACK; stackIndex++) {
		//do all the checks to make sure it's unchanged
		if (isInt(Runner::stack[stackIndex])) {
			if (Runner::stack[stackIndex].numberValue.integerValue == 0) {
				//the stack cell is unchanged, keep going
				//also i hate how i have to make all these calls in order
				//looks freakin disgusting, but there's some unknown behavior
				//if i dont (accessing an uninitialized value in a struct)
			} else {
				Runner::modifiedStackArea = MAX_STACK - stackIndex;
				break;
			}
		} else {
			Runner::modifiedStackArea = MAX_STACK - stackIndex;
			break;
		}
	}
}

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
	//initialize the stack
	Runner::modifiedStackArea = 0;
	for (unsigned int stackIndex = 0; stackIndex < Runner::MAX_STACK; stackIndex++) {
		Runner::stack.push_back(Runner::zeroF());
	}
}

std::vector<CharmFunction> Runner::getStack() {
	return Runner::stack;
}

unsigned int Runner::getModifiedStackArea() {
	return Runner::modifiedStackArea;
}

std::vector<FunctionDefinition> Runner::getFunctionDefinitions() {
	return Runner::functionDefinitions;
}

void Runner::handleDefinedFunctions(CharmFunction f) {
	//PredefinedFunctions.h holds all the functions written in C++
	//other than that, if these functions aren't built in, they are run through
	//the functionDefinitions table.

	//first, make sure that the function we're trying to run exists in the PredefinedFunctions
	//table. if it doesn't - assume it's defined in Charm and run through the
	//functionDefinitions table.
	bool isPredefinedFunction = false;
	for (auto predefinedFunctionName : PredefinedFunctions::cppFunctionNames) {
		if (predefinedFunctionName == f.functionName) {
			isPredefinedFunction = true;
			break;
		}
	}
	if (isPredefinedFunction) {
		//run the predefined function!
		PredefinedFunctions::functionLookup(f.functionName, this);
	} else {
		//alright, now we get down and dirty
		//look through the functionDefinitions table for a function with
		//a matching name, and run that. if there are no functions - throw
		//an error.
		bool functionFound = false;
		for (FunctionDefinition fD : functionDefinitions) {
			if (fD.functionName == f.functionName) {
				functionFound = true;
				Runner::run(fD.functionBody);
			}
		}
		if (!functionFound) {
			runtime_die("Unknown function `" + f.functionName + "`.");
		}
	}
}

void Runner::run(std::vector<CharmFunction> parsedProgram) {
	for (CharmFunction currentFunction : parsedProgram) {
		//alright, now we get into the running portion
		if (currentFunction.functionType == NUMBER_FUNCTION) {
			//first, let's do the numbers
			Runner::push(currentFunction);
			//easy, right? let's do more
		} else if (currentFunction.functionType == STRING_FUNCTION) {
			//now we push strings onto the stack
			Runner::push(currentFunction);
			//still p easy ye
		} else if (currentFunction.functionType == LIST_FUNCTION) {
			//now we push on the lists
			Runner::push(currentFunction);
			//wow this is easy right? now get ready baby
		} else if (currentFunction.functionType == FUNCTION_DEFINITION) {
			//lets define some functions bruh
			FunctionDefinition tempFunction;
			tempFunction.functionName = currentFunction.functionName;
			tempFunction.functionBody = currentFunction.literalFunctions;
			Runner::addFunctionDefinition(tempFunction);
			//that was easy too! oh no...
		} else if (currentFunction.functionType == DEFINED_FUNCTION) {
			//let's do these defined functions now
			Runner::handleDefinedFunctions(currentFunction);
			//lol you thought i'd do it here
		}
	}
}
