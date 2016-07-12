#include <string>
#include <vector>

#include "PredefinedFunctions.h"
#include "ParserTypes.h"
#include "Error.h"

const std::vector<std::string> PredefinedFunctions::cppFunctionNames = {
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
	
}

void PredefinedFunctions::dup(Runner* r) {
	CharmFunction f1 = r->pop();
	r->push(f1);
	r->push(f1);
}

void PredefinedFunctions::pop(Runner* r) {
	r->pop();
}

void PredefinedFunctions::swap(Runner* r) {
	CharmFunction f1 = r->pop();
	CharmFunction f2 = r->pop();
	//check to make sure we've got ints that are positive and below MAX_STACK
	if (r->isInt(f1) && r->isInt(f2)) {
		if ((f1.numberValue.integerValue < 0) || (f2.numberValue.integerValue < 0)) {
			runtime_die("Negative int passed to `swap`.");
		}
		r->swap((unsigned long long)f1.numberValue.integerValue, (unsigned long long)f2.numberValue.integerValue);
	} else {
		runtime_die("Float passed to `swap`.");
	}
}
