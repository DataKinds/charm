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

Runner::Runner() {
	//initialize the stack
	for (unsigned int stackIndex = 0; stackIndex < Runner::MAX_STACK; stackIndex++) {

		Runner::push(Runner::zeroF());
	}
}

void Runner::run(std::vector<CharmFunction> parsedProgram) {

}
