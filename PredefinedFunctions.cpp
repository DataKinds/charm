#include <string>
#include <vector>

#include "PredefinedFunctions.h"
#include "ParserTypes.h"
#include "Error.h"

const std::vector<std::string> PredefinedFunctions::cppFunctionNames = {
	//INPUT / OUTPUT
	"pp", "newline",
	//STACK MANIPULATIONS
	"dup", "pop", "swap",
	//TRAVERSABLE (STRING/LIST) MANIPULATIONS
	"len", "at", "insert", "concat",
	//LIST MANIPULATIONS
	//TODO
	"fold", "map", "zip",
	//STRING MANIPULATIONS
	//TODO
	"tocharlist", "fromcharlist",
	//CONTROL FLOW
	"i", "ifthen",
	//BOOLEAN OPS - TRUE: >=1, FALSE: <1 - INTEGER ONLY
	"nor",
	//TYPE INSPECIFIC MATH
	"abs",
	//INTEGER OPS
	"+", "-", "/", "*", "%", "toint",
	//FLOAT OPS
	"+f", "-f", "/f", "*f", "tofloat",
	//STACK CREATION/DESTRUCTION
	"createstack", "switchstack",
	//REF GETTING/SETTING
	"getref", "setref"
};

void PredefinedFunctions::functionLookup(std::string functionName, Runner* r) {
	//INPUT / OUTPUT
	if (functionName == "pp") PredefinedFunctions::p(r);
	if (functionName == "newline") PredefinedFunctions::newline(r);
	//STACK MANIPULATIONS
	if (functionName == "dup") PredefinedFunctions::dup(r);
	if (functionName == "pop") PredefinedFunctions::pop(r);
	if (functionName == "swap") PredefinedFunctions::swap(r);
	//TRAVERSABLE (STRING / LIST) MANIPULATIONS
	if (functionName == "len") PredefinedFunctions::len(r);
	if (functionName == "at") PredefinedFunctions::at(r);
	if (functionName == "insert") PredefinedFunctions::insert(r);
	if (functionName == "concat") PredefinedFunctions::concat(r);
	//CONTROL FLOW
	if (functionName == "i") PredefinedFunctions::i(r);
	if (functionName == "ifthen") PredefinedFunctions::ifthen(r);
	//BOOLEAN OPS
	if (functionName == "nor") PredefinedFunctions::nor(r); //you can make all logic out of this
	//TYPE INSPECIFIC MATH
	if (functionName == "abs") PredefinedFunctions::abs(r);
	//INTEGER OPS
	if (functionName == "+") PredefinedFunctions::plusI(r);
	if (functionName == "-") PredefinedFunctions::minusI(r);
	if (functionName == "/") PredefinedFunctions::divI(r);
	if (functionName == "*") PredefinedFunctions::timesI(r);
	if (functionName == "toint") PredefinedFunctions::toInt(r);
	//STACK CREATION/DESTRUCTION
	if (functionName == "createstack") PredefinedFunctions::createStack(r);
	if (functionName == "switchstack") PredefinedFunctions::switchStack(r);
	//REF GETTING/SETTING
	if (functionName == "getref") PredefinedFunctions::getRef(r);
	if (functionName == "setref") PredefinedFunctions::setRef(r);
}

void PredefinedFunctions::p(Runner* r) {
	PredefinedFunctions::print(r->getCurrentStack()->pop());
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
			printf(" ");
		}
		printf("]");
	}
}

void PredefinedFunctions::newline(Runner* r) {
	printf("\n");
}

void PredefinedFunctions::dup(Runner* r) {
	CharmFunction f1 = r->getCurrentStack()->pop();
	r->getCurrentStack()->push(f1);
	r->getCurrentStack()->push(f1);
}

void PredefinedFunctions::pop(Runner* r) {
	r->getCurrentStack()->pop();
}

//this function is really quite obscene tbh
void PredefinedFunctions::swap(Runner* r) {
	CharmFunction f1 = r->getCurrentStack()->pop();
	CharmFunction f2 = r->getCurrentStack()->pop();
	//check to make sure we've got ints that are positive and below MAX_STACK
	if (Stack::isInt(f1) && Stack::isInt(f2)) {
		if ((f1.numberValue.integerValue < 0) || (f2.numberValue.integerValue < 0)) {
			runtime_die("Negative int passed to `swap`.");
		}
		if ((f1.numberValue.integerValue >= r->MAX_STACK) || (f2.numberValue.integerValue >= r->MAX_STACK)) {
			runtime_die("Overflowing pointers passed to `swap`.");
		}
		r->getCurrentStack()->swap((unsigned long long)f1.numberValue.integerValue, (unsigned long long)f2.numberValue.integerValue);
	} else {
		runtime_die("Non integer passed to `swap`.");
	}
}

void PredefinedFunctions::len(Runner* r) {
	//list to check length of
	CharmFunction f1 = r->getCurrentStack()->pop();
	//push list back on because we dont need to get rid of it
	r->getCurrentStack()->push(f1);
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
	r->getCurrentStack()->push(out);
}

void PredefinedFunctions::at(Runner* r) {
	//index number
	CharmFunction f1 = r->getCurrentStack()->pop();
	//list / string
	CharmFunction f2 = r->getCurrentStack()->pop();
	r->getCurrentStack()->push(f2);
	if (Stack::isInt(f1)) {
		CharmFunction out;
		if (f2.functionType == LIST_FUNCTION) {
			out = f2.literalFunctions.at(f1.numberValue.integerValue % f2.literalFunctions.size());
		} else if (f2.functionType == STRING_FUNCTION) {
			out.functionType = STRING_FUNCTION;
			out.stringValue = f2.stringValue[f1.numberValue.integerValue % f2.stringValue.size()];
		} else {
			runtime_die("Neither a list nor a string was passed to `at`");
		}
		r->getCurrentStack()->push(out);
	} else {
		runtime_die("Non integer index passed to `at`");
	}
}

void PredefinedFunctions::insert(Runner* r) {
	//get index to insert in
	CharmFunction f1 = r->getCurrentStack()->pop();
	//get element to insert
	CharmFunction f2 = r->getCurrentStack()->pop();
	//get list or string
	CharmFunction f3 = r->getCurrentStack()->pop();
	//make sure f1 is an int
	if (!Stack::isInt(f1))
		runtime_die("Non integer index passed to `insert`.");
	if (f3.functionType == LIST_FUNCTION) {
		f3.literalFunctions.insert(f3.literalFunctions.begin() + (f1.numberValue.integerValue % f3.literalFunctions.size()), f2);
	} else if (f3.functionType == STRING_FUNCTION) {
		//only allow a string to be inserted into another string
		if (f3.functionType == STRING_FUNCTION) {
			f3.stringValue.insert(f1.numberValue.integerValue % f3.stringValue.size(), f2.stringValue);
		} else {
			runtime_die("Attempted to `insert` a non string into a string.");
		}
	}
	r->getCurrentStack()->push(f3);
}

void PredefinedFunctions::concat(Runner* r) {
	//get first list
	CharmFunction f1 = r->getCurrentStack()->pop();
	//get second list (first in order of concatination)
	CharmFunction f2 = r->getCurrentStack()->pop();
	//make sure they're both lists or strings
	if ((f1.functionType == LIST_FUNCTION) && (f2.functionType == LIST_FUNCTION)) {
		f2.literalFunctions.insert(f2.literalFunctions.end(), f1.literalFunctions.begin(), f1.literalFunctions.end());
	} else if ((f1.functionType == STRING_FUNCTION) && (f2.functionType == STRING_FUNCTION)) {
		f2.stringValue = f2.stringValue + f1.stringValue;
	} else {
		runtime_die("Unmatching types passed to `concat`.");
	}
	r->getCurrentStack()->push(f2);
}

void PredefinedFunctions::i(Runner* r) {
	//pop the top of the stack and run it
	CharmFunction f1 = r->getCurrentStack()->pop();
	if (f1.functionType == LIST_FUNCTION) {
		r->run(f1.literalFunctions);
	} else {
		runtime_die("Non list passed to `i`.");
	}
}

void PredefinedFunctions::ifthen(Runner* r) {
	//this one is gonna take 3 arguments:
	//stack[2] = condition to run truthy section
	//stack[1] = truthy section (if...)
	//stack[0] = falsy section (else...)
	//have to reverse it because popping is weird
	CharmFunction falsy = r->getCurrentStack()->pop();
	CharmFunction truthy = r->getCurrentStack()->pop();
	CharmFunction condFunction = r->getCurrentStack()->pop();
	if ((condFunction.functionType == LIST_FUNCTION) &&
		(truthy.functionType == LIST_FUNCTION) &&
		(falsy.functionType == LIST_FUNCTION)) {
			r->run(condFunction.literalFunctions);
			//now we check the top of the stack to see if it's truthy or falsy
			CharmFunction cond = r->getCurrentStack()->pop();
			if (Stack::isInt(cond)) {
				if (cond.numberValue.integerValue > 0) {
					r->run(truthy.literalFunctions);
				} else {
					r->run(falsy.literalFunctions);
				}
			} else {
				runtime_die("`ifthen` condition returned non integer.");
			}
		} else {
			runtime_die("Non list passed to `ifthen`.");
		}
}



void PredefinedFunctions::nor(Runner* r) {
	CharmFunction f1 = r->getCurrentStack()->pop();
	CharmFunction f2 = r->getCurrentStack()->pop();
	if (Stack::isInt(f1) && Stack::isInt(f2)) {
		CharmFunction out;
		out.functionType = NUMBER_FUNCTION;
		CharmNumber outNum;
		outNum.whichType = INTEGER_VALUE;
		//cancer incoming
		outNum.integerValue = ((f1.numberValue.integerValue > 0) || (f2.numberValue.integerValue > 0));
		//no more cancer
		out.numberValue = outNum;
		r->getCurrentStack()->push(out);
	} else {
		runtime_die("Non integer passed to logic function.");
	}
}

void PredefinedFunctions::abs(Runner* r) {
	CharmFunction f1 = r->getCurrentStack()->pop();
	if (Stack::isInt(f1)) {
		std::abs(f1.numberValue.integerValue);
	} else if (Stack::isFloat(f1)) {
		if (f1.numberValue.floatValue < 0) {
			f1.numberValue.floatValue = -f1.numberValue.floatValue;
		}
	} else {
		runtime_die("Non number passed to `abs`.");
	}
}

void PredefinedFunctions::plusI(Runner* r) {
	CharmFunction f1 = r->getCurrentStack()->pop();
	CharmFunction f2 = r->getCurrentStack()->pop();
	if (Stack::isInt(f1) && Stack::isInt(f2)) {
		f1.numberValue.integerValue = f1.numberValue.integerValue + f2.numberValue.integerValue;
	} else {
		runtime_die("Non integer passed to `+`.");
	}
	r->getCurrentStack()->push(f1);
}

void PredefinedFunctions::minusI(Runner* r) {
	CharmFunction f1 = r->getCurrentStack()->pop();
	CharmFunction f2 = r->getCurrentStack()->pop();
	if (Stack::isInt(f1) && Stack::isInt(f2)) {
		f1.numberValue.integerValue = f2.numberValue.integerValue - f1.numberValue.integerValue;
	} else {
		runtime_die("Non integer passed to `-`.");
	}
	r->getCurrentStack()->push(f1);
}

void PredefinedFunctions::divI(Runner* r) {
	CharmFunction f1 = r->getCurrentStack()->pop();
	CharmFunction f2 = r->getCurrentStack()->pop();
	if (Stack::isInt(f1) && Stack::isInt(f2)) {
		//f1 used as answer
		f1.numberValue.integerValue = f2.numberValue.integerValue / f1.numberValue.integerValue;
		//f2 used as modulus
		f2.numberValue.integerValue = f2.numberValue.integerValue % f1.numberValue.integerValue;
	} else {
		runtime_die("Non integer passed to `+`.");
	}
	r->getCurrentStack()->push(f2);
	r->getCurrentStack()->push(f1);
}

void PredefinedFunctions::timesI(Runner* r) {
	CharmFunction f1 = r->getCurrentStack()->pop();
	CharmFunction f2 = r->getCurrentStack()->pop();
	if (Stack::isInt(f1) && Stack::isInt(f2)) {
		f1.numberValue.integerValue = f1.numberValue.integerValue * f2.numberValue.integerValue;
	} else {
		runtime_die("Non integer passed to `*`.");
	}
	r->getCurrentStack()->push(f1);
}

void PredefinedFunctions::toInt(Runner* r) {
	CharmFunction f1 = r->getCurrentStack()->pop();
	if (Stack::isFloat(f1)) {
		f1.numberValue.whichType = INTEGER_VALUE;
		f1.numberValue.integerValue = (long long)f1.numberValue.floatValue;
	} else if (Stack::isInt(f1)) {
		//do nothing, it's already an int
	} else {
		runtime_die("Non number passed to `toInt`.");
	}
}

void PredefinedFunctions::createStack(Runner* r) {
	//name of the stack
	CharmFunction f1 = r->getCurrentStack()->pop();
	//length of the stack
	CharmFunction f2 = r->getCurrentStack()->pop();
	if (Stack::isInt(f2)) {
		if (f2.numberValue.integerValue > 0) {
			r->createStack(f2.numberValue.integerValue, f1);
		} else {
			runtime_die("Negative integer or zero passed to `createStack`.");
		}
	} else {
		runtime_die("Non integer passed to `createStack`.");
	}
}

void PredefinedFunctions::switchStack(Runner* r) {
	CharmFunction f1 = r->getCurrentStack()->pop();
	r->switchCurrentStack(f1);
}

void PredefinedFunctions::getRef(Runner* r) {
	CharmFunction f1 = r->getCurrentStack()->pop();
	r->getCurrentStack()->push(r->getReference(f1));
}

void PredefinedFunctions::setRef(Runner* r) {
	//the value of the reference
	CharmFunction f1 = r->getCurrentStack()->pop();
	//the name of the reference
	CharmFunction f2 = r->getCurrentStack()->pop();
	r->setReference(f2, f1);
}
