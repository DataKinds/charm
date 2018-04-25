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

Stack::Stack(CharmFunction name) {
	Stack::stack.push_back(Stack::zeroF());
    Stack::name = name;
}

CharmFunction Stack::pop() {
	// if the stack is large enough to support it,
	// simply pop. else, we return a zero function
	if (Stack::stack.size() > 0) {
		auto temp = *std::prev(Stack::stack.end());
		Stack::stack.pop_back();
		return temp;
	}
	return Stack::zeroF();
}

void Stack::push(CharmFunction f) {
	Stack::stack.push_back(f);
}

void Stack::swap(unsigned long long n1, unsigned long long n2) {
	// trivial case
	if (n1 == n2) {
		return;
	}
	unsigned long stackLastElem = Stack::stack.size() - 1;
	// we gotta handle 4 cases as follows:
	// the first one: both n1 and n2 are outside the actual stack size
	// in this case, we do nothing (you swap a 0 with a 0)
	if (n1 > stackLastElem && n2 > stackLastElem) {
		return;
	}
	// now just n1 is greater than the last elem, so we push until n2 and then swap
	else if (n1 > stackLastElem) {
		for (unsigned long i = 0; i < n1 - stackLastElem; i++) {
			Stack::stack.push_front(Stack::zeroF());
		}
	}
	// ... or the other way around
	else if (n2 > stackLastElem) {
		for (unsigned long i = 0; i < n2 - stackLastElem; i++) {
			Stack::stack.push_front(Stack::zeroF());
		}
	}
	// then we swap
	std::iter_swap(Stack::stack.end() - n1 - 1, Stack::stack.end() - n2 - 1);
	// and we're on our merry way
}
