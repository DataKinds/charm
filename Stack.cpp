#include "Stack.h"
#include "ParserTypes.h"

#include <algorithm>

CharmFunction Stack::zeroF() {
	CharmFunction zeroFunction;
	CharmNumber zeroNumber;
	zeroNumber.whichType = INTEGER_VALUE;
	zeroNumber.integerValue = 0;
	zeroFunction.functionType = NUMBER_FUNCTION;
	zeroFunction.numberValue = zeroNumber;
	return zeroFunction;
}

bool Stack::isInt(CharmFunction f) {
	if (f.functionType == NUMBER_FUNCTION) {
		if (f.numberValue.whichType == INTEGER_VALUE) {
			return true;
		}
	}
	return false;
}

bool Stack::isFloat(CharmFunction f) {
	if (f.functionType == NUMBER_FUNCTION) {
		if (f.numberValue.whichType == FLOAT_VALUE) {
			return true;
		}
	}
	return false;
}

bool Stack::isNameEqualTo(CharmFunction f) {
    return (Stack::name == f);
}

Stack::Stack(unsigned long long size, CharmFunction name) {
    Stack::modifiedStackArea = 0;
	for (unsigned long long stackIndex = 0; stackIndex < size; stackIndex++) {
		Stack::stack.push_back(Stack::zeroF());
	}
    Stack::name = name;
}


unsigned int Stack::getModifiedStackArea() {
    return Stack::modifiedStackArea;
}


CharmFunction Stack::pop() {
	//ensure that the stack never changes size
	//this is by placing more zeroes at the start
	//as stuff is popped off the end
	Stack::stack.insert(Stack::stack.begin(), Stack::zeroF());
	CharmFunction tempCharmF = Stack::stack.at(Stack::stack.size() - 1);
	Stack::stack.pop_back();
	if (Stack::modifiedStackArea != 0) Stack::modifiedStackArea--;
	return tempCharmF;
}

void Stack::push(CharmFunction f) {
	//ensure the stack never changes size again
	//pop an element off the back of the stack
	Stack::stack.push_back(f);
	Stack::stack.erase(Stack::stack.begin());
	Stack::modifiedStackArea++;
}

void Stack::swap(unsigned long long n1, unsigned long long n2) {
	/*CharmFunction tempFromN1 = Stack::stack.at(Stack::stack.size() - n1 - 1);
	CharmFunction tempFromN2 = Stack::stack.at(Stack::stack.size() - n2 - 1);
	Stack::stack[Stack::stack.size() - n1 - 1] = tempFromN2;
	Stack::stack[Stack::stack.size() - n2 - 1] = tempFromN1;
	Stack::updateModifiedStackArea();
	*/
	std::iter_swap(Stack::stack.end() - n1 - 1, Stack::stack.end() - n2 - 1);
	if (n1 + 1 > Stack::modifiedStackArea) Stack::modifiedStackArea = n1;
	if (n2 + 1 > Stack::modifiedStackArea) Stack::modifiedStackArea = n2;
}
