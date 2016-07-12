#include <string>
#include <vector>

#include "PredefinedFunctions.h"
#include "ParserTypes.h"
#include "Error.h"

const std::vector<std::string> PredefinedFunctions::cppFunctionNames = {
	//INPUT / OUTPUT
	"p", "newline",
	//STACK MANIPULATIONS
	"dup", "pop", "swap",
	//LIST MANIPULATIONS
	"len", "at", "insert", "concat",
	//CONTROL FLOW
	"i", "ifthen",
	//MATH OPS AND TYPE COERCION
	"=", "<", ">", "+", "-", "/", "*", "%", "abs", "tofloat", "toint"
};

void PredefinedFunctions::functionLookup(std::string functionName, Runner* r) {
	//INPUT / OUTPUT
	if (functionName == "p") PredefinedFunctions::p(r);
	//STACK MANIPULATIONS
	if (functionName == "dup") PredefinedFunctions::dup(r);
	if (functionName == "pop") PredefinedFunctions::pop(r);
	if (functionName == "swap") PredefinedFunctions::swap(r);
	//LIST / STRING MANIPULATIONS
	if (functionName == "len") PredefinedFunctions::len(r);
	if (functionName == "at") PredefinedFunctions::at(r);
	if (functionName == "insert") PredefinedFunctions::insert(r);
	if (functionName == "concat") PredefinedFunctions::concat(r);
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
		runtime_die("Non integer passed to `swap`.");
	}
}

void PredefinedFunctions::len(Runner* r) {
	//list to check length of
	CharmFunction f1 = r->pop();
	//push list back on because we dont need to get rid of it
	r->push(f1);
	CharmFunction out;
	out.functionType = NUMBER_FUNCTION;
	CharmNumber num;
	num.whichType = INTEGER_VALUE;
	//make sure f1 is a list or string
	if (f1.functionType == LIST_FUNCTION) {
		num.integerValue = f1.literalFunctions.size();
	} else if (f1.functionType == STRING_FUNCTION) {
		num.integerValue = f1.stringValue.size();
	} else {
		//so if it's a bad type, i was going to just report a len of 0 or 1
		//but i feel like that would be really misleading. eh, i'll just do 1
		num.integerValue = 1;
	}
	out.numberValue = num;
	r->push(out);
}

void PredefinedFunctions::at(Runner* r) {
	//index number
	CharmFunction f1 = r->pop();
	//list / string
	CharmFunction f2 = r->pop();
	if (r->isInt(f1)) {
		CharmFunction out;
		if (f2.functionType == LIST_FUNCTION) {
			out = f2.literalFunctions.at(f1.numberValue.integerValue % f2.literalFunctions.size());
		} else if (f2.functionType == STRING_FUNCTION) {
			out.functionType = STRING_FUNCTION;
			out.stringValue = f2.stringValue[f1.numberValue.integerValue % f2.stringValue.size()];
		} else {
			runtime_die("Neither a list nor a string was passed to `at`");
		}
		r->push(out);
	} else {
		runtime_die("Non integer index passed to `at`");
	}
}

void PredefinedFunctions::insert(Runner* r) {
	//get index to insert in
	CharmFunction f1 = r->pop();
	//get element to insert
	CharmFunction f2 = r->pop();
	//get list or string
	CharmFunction f3 = r->pop();
	//make sure f1 is an int
	if (!r->isInt(f1))
		runtime_die("Non integer index passed to `insert`.");
	if (f3.functionType == LIST_FUNCTION) {
		f3.literalFunctions.insert(f3.literalFunctions.begin() + (f1.numberValue.integerValue % f3.literalFunctions.size()), f2);
	}
}

void PredefinedFunctions::concat(Runner* r) {

}
