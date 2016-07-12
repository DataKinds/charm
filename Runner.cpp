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
	stack.insert(stack.begin(), Runner::zeroF());
	CharmFunction tempCharmF = stack.at(stack.size() - 1);
	stack.pop_back();
	return tempCharmF;
}

void Runner::push(CharmFunction f) {
	//ensure the stack never changes size again
	//pop an element off the back of the stack
	stack.push_back(f);
	stack.erase(stack.begin());
}

void Runner::swap(unsigned int n1, unsigned int n2) {
	CharmFunction tempFromN1 = stack.at(n1);
	CharmFunction tempFromN2 = stack.at(n2);
	stack[n1] = tempFromN2;
	stack[n2] = tempFromN1;
}

void Runner::handleDefinedFunctions(CharmFunction f) {
	printf("UNIMPLEMENTED: running function %s\n", f.functionName.c_str());
}

Runner::Runner() {
	//initialize the stack
	for (unsigned int stackIndex = 0; stackIndex < Runner::MAX_STACK; stackIndex++) {
		Runner::push(Runner::zeroF());
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
		} else if (currentFunction.functionType == DEFINED_FUNCTION) {
			//let's do these defined functions now
			Runner::handleDefinedFunctions(currentFunction);
			//lol you thought i'd do it here
		}
	}
}
