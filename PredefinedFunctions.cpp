#include <string>
#include <vector>
#include <iostream>
#include <variant>
#include <unordered_map>
#include <functional>

#include "PredefinedFunctions.h"
#include "ParserTypes.h"
#include "Error.h"
#include "Debug.h"
#include "Runner.h"


void PredefinedFunctions::addBuiltinFunction(std::string n, std::function<void(Runner*)> f) {
	BuiltinFunction bf;
	bf.f = f; bf.takesContext = false;
	cppFunctionNames[n] = bf;
}
void PredefinedFunctions::addBuiltinFunction(std::string n, std::function<void(Runner*, FunctionDefinition*)> f) {
	BuiltinFunction bf;
	bf.f = f; bf.takesContext = true;
	cppFunctionNames[n] = bf;
}
/*
static std::unordered_map<std::string, BuiltinFunction> PredefinedFunctions::cppFunctionNames = {
	//TODO FUNCTIONS
	//INPUT/OUTPUT
	"putstring",
	//STRING MANIPULATIONS
	"tocharlist", "fromcharlist",
	//METAPROGRAMMING
	"inline"
};
*/
void PredefinedFunctions::functionLookup(std::string functionName, Runner* r, FunctionDefinition* context) {
	auto f = cppFunctionNames.at(functionName);
	if (f.takesContext) {
		auto castF = std::get<std::function<void(Runner*, FunctionDefinition*)>>(f.f);
		castF(r, context);
	} else {
		auto castF = std::get<std::function<void(Runner*)>>(f.f);
		castF(r);
	}
}

PredefinedFunctions::PredefinedFunctions() {
	//INPUT / OUTPUT
	addBuiltinFunction("p", [](Runner* r) {
		printf("%s", charmFunctionToString(r->getCurrentStack()->pop()).c_str());
	});
	addBuiltinFunction("pstring", [](Runner* r) {
		CharmFunction f1 = r->getCurrentStack()->pop();
		if (f1.functionType == STRING_FUNCTION) {
			printf("%s", f1.stringValue.c_str());
		} else {
			runtime_die("Non string passed to `pstring`.");
		}
	});
	addBuiltinFunction("newline", [](Runner* r) {
		printf("\n");
	});
	addBuiltinFunction("getline", [](Runner* r) {
		CharmFunction input;
		input.functionType = STRING_FUNCTION;
		std::getline(std::cin, input.stringValue);
		r->getCurrentStack()->push(input);
	});
	//STACK MANIPULATIONS
	addBuiltinFunction("dup", [](Runner* r) {
		CharmFunction f1 = r->getCurrentStack()->pop();
		r->getCurrentStack()->push(f1);
		r->getCurrentStack()->push(f1);
	});
	addBuiltinFunction("pop", [](Runner* r) {
		r->getCurrentStack()->pop();
	});
	addBuiltinFunction("swap", [](Runner* r) {
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
	});
	//TRAVERSABLE (STRING / LIST) MANIPULATIONS
	addBuiltinFunction("len", [](Runner* r) {
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
	});
	addBuiltinFunction("at", [](Runner* r) {
		//index number
		CharmFunction f1 = r->getCurrentStack()->pop();
		//list / string
		CharmFunction f2 = r->getCurrentStack()->pop();
		r->getCurrentStack()->push(f2);
		if (Stack::isInt(f1)) {
			CharmFunction out;
			if (f2.functionType == LIST_FUNCTION) {
				out.functionType = LIST_FUNCTION;
				out.literalFunctions = { f2.literalFunctions.at(f1.numberValue.integerValue % f2.literalFunctions.size()) };
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
	});
	addBuiltinFunction("insert", [](Runner* r) {
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
			//only allow a list to be inserted into a list
			if (f2.functionType == LIST_FUNCTION) {
				f3.literalFunctions.insert(
					f3.literalFunctions.begin() + (f1.numberValue.integerValue % f3.literalFunctions.size()),
					f2.literalFunctions.begin(),
					f2.literalFunctions.end()
				);
			} else {
				runtime_die("Attempted to `insert` a non list into a list.");
			}
		} else if (f3.functionType == STRING_FUNCTION) {
			//only allow a string to be inserted into another string
			if (f2.functionType == STRING_FUNCTION) {
				f3.stringValue.insert(
					f1.numberValue.integerValue % f3.stringValue.size(),
					f2.stringValue
				);
			} else {
				runtime_die("Attempted to `insert` a non string into a string.");
			}
		}
		r->getCurrentStack()->push(f3);
	});
	addBuiltinFunction("concat", [](Runner* r) {
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
	});
	addBuiltinFunction("split", [](Runner* r) {
		//get split index
		CharmFunction f1 = r->getCurrentStack()->pop();
		//get list/string to split
		CharmFunction f2 = r->getCurrentStack()->pop();
		CharmFunction lowOut;
		CharmFunction highOut;
		if (f1.functionType == NUMBER_FUNCTION && f1.numberValue.whichType == INTEGER_VALUE) {
			//bounds checking
			if (f1.numberValue.integerValue < 0 || (unsigned long long)f1.numberValue.integerValue > f2.literalFunctions.size()) {
				runtime_die("Out of bounds error on the number passed to `split`.");
			}
			if (f2.functionType == LIST_FUNCTION) {
				lowOut.functionType = LIST_FUNCTION;
				lowOut.literalFunctions.assign(f2.literalFunctions.begin(), f2.literalFunctions.begin() + f1.numberValue.integerValue);
				highOut.functionType = LIST_FUNCTION;
				highOut.literalFunctions.assign(f2.literalFunctions.begin() + f1.numberValue.integerValue, f2.literalFunctions.end());
			} else if (f2.functionType == STRING_FUNCTION) {
				lowOut.functionType = STRING_FUNCTION;
				lowOut.stringValue.assign(f2.stringValue.begin(), f2.stringValue.begin() + f1.numberValue.integerValue);
				highOut.functionType = STRING_FUNCTION;
				highOut.stringValue.assign(f2.stringValue.begin() + f1.numberValue.integerValue, f2.stringValue.end());
			} else {
				runtime_die("Non list/string passed to `split`.");
			}
		} else {
			runtime_die("Non integer passed to `split`.");
		}
		r->getCurrentStack()->push(lowOut);
		r->getCurrentStack()->push(highOut);
	});
	addBuiltinFunction("tostring", [](Runner* r) {
		CharmFunction f1 = r->getCurrentStack()->pop();
		CharmFunction out;
		out.functionType = STRING_FUNCTION;
		out.stringValue = charmFunctionToString(f1);
		r->getCurrentStack()->push(out);
	});
	//CONTROL FLOW
	addBuiltinFunction("i", [](Runner* r) {
		//pop the top of the stack and run it
		CharmFunction f1 = r->getCurrentStack()->pop();
		if (f1.functionType == LIST_FUNCTION) {
			r->run(f1.literalFunctions);
		} else {
			runtime_die("Non list passed to `i`.");
		}
	});
	addBuiltinFunction("q", [](Runner* r) {
		CharmFunction f1 = r->getCurrentStack()->pop();
		CharmFunction list;
		list.functionType = LIST_FUNCTION;
		list.literalFunctions.push_back(f1);
		r->getCurrentStack()->push(list);
	});
	addBuiltinFunction("ifthen", [](Runner* r, FunctionDefinition* context) {
		//the arguments to this function are a little different...
		//ifthen performs very basic tail-call optimization on its two sections (truthy/falsy)
		//if truthy (or falsy) end with the function itself (found through fD.functionName), then
		//the tail-call optimizer kicks in and the function simply loops instead of
		//creating a new stack frame by calling r->run()

		//this one is gonna take 3 arguments:
		//stack[2] = condition to run truthy section
		//stack[1] = truthy section (if...)
		//stack[0] = falsy section (else...)
		//have to reverse it because popping is weird
		CharmFunction falsy = r->getCurrentStack()->pop();
		bool falsyTailCall = false;
		CharmFunction truthy = r->getCurrentStack()->pop();
		bool truthyTailCall = false;
		CharmFunction condFunction = r->getCurrentStack()->pop();
		if ((condFunction.functionType == LIST_FUNCTION) &&
			(truthy.functionType == LIST_FUNCTION) &&
			(falsy.functionType == LIST_FUNCTION)) {
				//first, we run checks to set the tail call bools
				if (context != nullptr) {
					std::string defName = context->functionName;
					if (truthy.literalFunctions.size() > 0 && truthy.literalFunctions.back().functionName == defName) {
						truthyTailCall = true;
					}
					if (falsy.literalFunctions.size() > 0 && falsy.literalFunctions.back().functionName == defName) {
						falsyTailCall = true;
					}
					//now, if we _DO_ have a tail call, modify truthy/falsy and enter a loop instead
					//there are 3 seperate cases here: truthy tail call, falsy tail call, or both
					if (truthyTailCall) {
						ONLYDEBUG printf("ENGAGING TRUTHY IF/THEN TAIL CALL OPTIMIZATION\n");
						//remove the tail call
						truthy.literalFunctions.pop_back();
						while (1) {
							r->run(condFunction.literalFunctions);
							CharmFunction cond = r->getCurrentStack()->pop();
							if (Stack::isInt(cond)) {
								if (cond.numberValue.integerValue > 0) {
									r->run(truthy.literalFunctions);
								} else {
									r->run(falsy.literalFunctions);
									//end this function immediately once the tail call loop ends
									ONLYDEBUG printf("DISENGAGING TRUTHY IF/THEN TAIL CALL OPTIMIZATION\n");
									return;
								}
							} else {
								runtime_die("`ifthen` condition returned non integer.");
							}
						}
					}
					if (falsyTailCall) {
						ONLYDEBUG printf("ENGAGING FALSY IF/THEN TAIL CALL OPTIMIZATION\n");
						//remove the tail call
						falsy.literalFunctions.pop_back();
						while (1) {
							r->run(condFunction.literalFunctions);
							CharmFunction cond = r->getCurrentStack()->pop();
							if (Stack::isInt(cond)) {
								if (cond.numberValue.integerValue > 0) {
									r->run(truthy.literalFunctions);
									//end this function immediately once the tail call loop ends
									ONLYDEBUG printf("DISENGAGING FALSY IF/THEN TAIL CALL OPTIMIZATION\n");
									return;
								} else {
									r->run(falsy.literalFunctions);
								}
							} else {
								runtime_die("`ifthen` condition returned non integer.");
							}
						}
					}
					//here's an extra special case: if both truthy and falsy have a tail call
					//th(at) is: `f := [ <cond> ] [ <code> f ] [ <code> f ] ifthen`
					//this is equivalent to `f := [ <cond> ] [ <code> ] [ <code> ] ifthen f`
					//so we run it as an infinite loop
					if (truthyTailCall && falsyTailCall) {
						ONLYDEBUG printf("ENGAGING TRUTHY/FALSY IF/THEN TAIL CALL OPTIMIZATION\n");
						//remove the tail calls
						truthy.literalFunctions.pop_back();
						falsy.literalFunctions.pop_back();
						while (1) {
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
						}
					}
					//good joke
					ONLYDEBUG printf("DISENGAGING TRUTHY/FALSY IF/THEN TAIL CALL OPTIMIZATION\n");
				}
				//but if not (or context was nullptr), continue execution as normal
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
	});
	//BOOLEAN OPS
	addBuiltinFunction("xor", [](Runner* r) {
		CharmFunction f1 = r->getCurrentStack()->pop();
		CharmFunction f2 = r->getCurrentStack()->pop();
		if (Stack::isInt(f1) && Stack::isInt(f2)) {
			CharmFunction out;
			out.functionType = NUMBER_FUNCTION;
			CharmNumber outNum;
			outNum.whichType = INTEGER_VALUE;
			//cancer incoming
			outNum.integerValue = ((f1.numberValue.integerValue > 0) ^ (f2.numberValue.integerValue > 0));
			//no more cancer
			out.numberValue = outNum;
			r->getCurrentStack()->push(out);
		} else {
			runtime_die("Non integer passed to logic function.");
		}
	});
	//TYPE INSPECIFIC MATH
	addBuiltinFunction("abs", [](Runner* r) {
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
	});
	//INTEGER OPS
	addBuiltinFunction("+", [](Runner* r) {
		CharmFunction f1 = r->getCurrentStack()->pop();
		CharmFunction f2 = r->getCurrentStack()->pop();
		if (Stack::isInt(f1) && Stack::isInt(f2)) {
			f1.numberValue.integerValue = f1.numberValue.integerValue + f2.numberValue.integerValue;
		} else {
			runtime_die("Non integer passed to `+`.");
		}
		r->getCurrentStack()->push(f1);
	});
	addBuiltinFunction("-", [](Runner* r) {
		CharmFunction f1 = r->getCurrentStack()->pop();
		CharmFunction f2 = r->getCurrentStack()->pop();
		if (Stack::isInt(f1) && Stack::isInt(f2)) {
			f1.numberValue.integerValue = f2.numberValue.integerValue - f1.numberValue.integerValue;
		} else {
			runtime_die("Non integer passed to `-`.");
		}
		r->getCurrentStack()->push(f1);
	});
	addBuiltinFunction("/", [](Runner* r) {
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
	});
	addBuiltinFunction("*", [](Runner* r) {
		CharmFunction f1 = r->getCurrentStack()->pop();
		CharmFunction f2 = r->getCurrentStack()->pop();
		if (Stack::isInt(f1) && Stack::isInt(f2)) {
			f1.numberValue.integerValue = f1.numberValue.integerValue * f2.numberValue.integerValue;
		} else {
			runtime_die("Non integer passed to `*`.");
		}
		r->getCurrentStack()->push(f1);
	});
	addBuiltinFunction("toint", [](Runner* r) {
		CharmFunction f1 = r->getCurrentStack()->pop();
		if (Stack::isFloat(f1)) {
			f1.numberValue.whichType = INTEGER_VALUE;
			f1.numberValue.integerValue = (long long)f1.numberValue.floatValue;
		} else if (Stack::isInt(f1)) {
			//do nothing, it's already an int
		} else {
			runtime_die("Non number passed to `toInt`.");
		}
	});
	//STACK CREATION/DESTRUCTION
	addBuiltinFunction("createstack", [](Runner* r) {
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
	});
	addBuiltinFunction("switchstack", [](Runner* r) {
		CharmFunction f1 = r->getCurrentStack()->pop();
		r->switchCurrentStack(f1);
	});
	//REF GETTING/SETTING
	addBuiltinFunction("getref", [](Runner* r) {
		CharmFunction f1 = r->getCurrentStack()->pop();
		r->getCurrentStack()->push(r->getReference(f1));
	});
	addBuiltinFunction("setref", [](Runner* r) {
		//the value of the reference
		CharmFunction f1 = r->getCurrentStack()->pop();
		//the name of the reference
		CharmFunction f2 = r->getCurrentStack()->pop();
		r->setReference(f2, f1);
	});
}
