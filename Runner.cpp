#include <vector>

#include "Runner.h"
#include "ParserTypes.h"

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

void Runner::swap(unsigned int n1, unsigned int n2) {
	CharmFunction tempFromN1 = Runner::stack.at(n1);
	CharmFunction tempFromN2 = Runner::stack.at(n2);
	Runner::stack[n1] = tempFromN2;
	Runner::stack[n2] = tempFromN1;
	Runner::updateModifiedStackArea();
}

void Runner::updateModifiedStackArea() {
	//go from the front of the stack to the back
	//then set the modifiedStackArea accordingly
	for (unsigned int stackIndex = 0; stackIndex < MAX_STACK; stackIndex++) {
		//do all the checks to make sure it's unchanged
		if (Runner::stack[stackIndex].functionType == NUMBER_FUNCTION) {
			if (Runner::stack[stackIndex].numberValue.whichType == INTEGER_VALUE) {
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
		} else {
			Runner::modifiedStackArea = MAX_STACK - stackIndex;
			break;
		}
	}
}

void Runner::handleDefinedFunctions(CharmFunction f) {
	printf("UNIMPLEMENTED: running function %s\n", f.functionName.c_str());
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
		} else if (currentFunction.functionType == DEFINED_FUNCTION) {
			//let's do these defined functions now
			Runner::handleDefinedFunctions(currentFunction);
			//lol you thought i'd do it here
		}
	}
}
