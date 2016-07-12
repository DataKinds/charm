#include <string>
#include <vector>

#include "PredefinedFunctions.h"
#include "ParserTypes.h"
#include "Error.h"

const std::vector<std::string> PredefinedFunctions::cppFunctionNames = {
	//INPUT / OUTPUT
	"p",
	//STACK MANIPULATIONS
	"dup", "pop", "swap",
	//LIST MANIPULATIONS
	"at", "cons", "concat",
	//CONTROL FLOW
	"i", "ifthen",
	//MATH OPS AND TYPE COERCION
	"=", "<", ">", "+", "-", "/", "*", "tofloat", "toint"
};

void PredefinedFunctions::functionLookup(std::string functionName, Runner* r) {
	if (functionName == "p") PredefinedFunctions::p(r);
	if (functionName == "dup") PredefinedFunctions::dup(r);
	if (functionName == "pop") PredefinedFunctions::pop(r);
	if (functionName == "swap") PredefinedFunctions::swap(r);
}

void PredefinedFunctions::p(Runner* r) {
	PredefinedFunctions::print(r->pop());
}

void PredefinedFunctions::print(CharmFunction f1) {
	if (f1.functionType == NUMBER_FUNCTION) {
		if (f1.numberValue.whichType == INTEGER_VALUE) {
			printf("%Li", f1.numberValue.integerValue);
		} else if (f1.numberValue.whichType == FLOAT_VALUE) {
			printf("%Lf", f1.numberValue.floatValue);
		}
	} else if (f1.functionType == STRING_FUNCTION) {
		printf("%s", f1.stringValue.c_str());
	} else if (f1.functionType == DEFINED_FUNCTION) {
		printf("%s", f1.functionName.c_str());
	} else if (f1.functionType == LIST_FUNCTION) {
		//oh boi a recursive print call
		printf("[ ");
		for (CharmFunction f : f1.literalFunctions) {
			PredefinedFunctions::print(f);
		}
		printf(" ]");
	}
}

void PredefinedFunctions::dup(Runner* r) {
	CharmFunction f1 = r->pop();
	r->push(f1);
	r->push(f1);
}

void PredefinedFunctions::pop(Runner* r) {
	r->pop();
}

//this function is really quite obscene tbh
void PredefinedFunctions::swap(Runner* r) {
	CharmFunction f1 = r->pop();
	CharmFunction f2 = r->pop();
	//check to make sure we've got ints that are positive and below MAX_STACK
	if (r->isInt(f1) && r->isInt(f2)) {
		if ((f1.numberValue.integerValue < 0) || (f2.numberValue.integerValue < 0)) {
			runtime_die("Negative int passed to `swap`.");
		}
		if ((f1.numberValue.integerValue >= r->MAX_STACK) || (f2.numberValue.integerValue >= r->MAX_STACK)) {
			runtime_die("Overflowing pointers passed to `swap`.");
		}
		r->swap(r->MAX_STACK - ((unsigned long long)f1.numberValue.integerValue), r->MAX_STACK - ((unsigned long long)f2.numberValue.integerValue));
	} else {
		runtime_die("Float passed to `swap`.");
	}
}
