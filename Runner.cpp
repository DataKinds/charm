#include <vector>

#include "Runner.h"
#include "ParserTypes.h"
#include "PredefinedFunctions.h"
#include "Error.h"
#include "Debug.h"
#include "FFI.h"
#include "FunctionAnalyzer.h"

void Runner::addFunctionDefinition(FunctionDefinition fD) {
	//first, check and make sure there's no other definition with
	//the same name. if there is, overwrite it. if not, just push_back
	//this definition.
	if (functionDefinitions.find(fD.functionName) == functionDefinitions.end()) {
		functionDefinitions[fD.functionName] = fD;
	}
}

Runner::Runner() {
	//initialize the stacks
	CharmFunction zero = Stack::zeroF();
	currentStackName = zero;
	stacks.push_back(Stack(zero));
	pF = new PredefinedFunctions();
	ffi = new FFI();
}

bool Runner::doesStackExist(CharmFunction name) {
	for (Stack stack : stacks) {
		if (stack.isNameEqualTo(name)) {
			return true;
		}
	}
	return false;
}
Stack* Runner::getCurrentStack() {
	for (unsigned int s = 0; s < stacks.size(); s++) {
		if (stacks[s].isNameEqualTo(Runner::currentStackName)) {
			return &(stacks[s]);
		}
	}
	return &(stacks[0]);
}

void Runner::switchCurrentStack(CharmFunction name) {
	if (Runner::doesStackExist(name)) {
		Runner::currentStackName = name;
	} else {
		runtime_die("Tried to switch to stack which does not exist.");
	}
}

void Runner::createStack(CharmFunction name) {
	if (Runner::doesStackExist(name)) {
		runtime_die("Tried to create stack that already exists.");
	} else {
		stacks.push_back(Stack(name));
	}
}

CharmFunction Runner::getReference(CharmFunction key) {
	for (Reference r : references) {
		if (r.key == key) {
			return r.value;
		}
	}
	return Stack::zeroF();
}

void Runner::setReference(CharmFunction key, CharmFunction value) {
	Reference newRef;
	newRef.key = key;
	newRef.value = value;
	for (unsigned long long n = 0; n < references.size(); n++) {
		if (references[n].key == key) {
			//if the ref was previously defined
			references[n] = newRef;
			return;
		}
	}
	//if it wasn't previously defined then
	references.push_back(newRef);
}

std::optional<std::vector<CharmFunction>> Runner::typeSignatureTick(std::string name, RunnerContext* context) {
	std::optional<std::vector<CharmFunction>> out;
	std::optional<CharmTypeSignature> type = context->fA->getTypeSignature(name);
	ONLYDEBUG printf("RUNNING TYPESIGNATURETICK FOR %s\n", name.c_str());
	if (type) {
		//assign a vector to be checked by
		unsigned int maxLength = FunctionAnalyzer::maxTypeSignatureLength(*type);
		ONLYDEBUG printf("FOUND A TYPE SIGNATURE FOR %s OF MAX LENGTH %i\n", name.c_str(), maxLength);
		out = std::vector<CharmFunction>(Runner::getCurrentStack()->stack.rbegin(), Runner::getCurrentStack()->stack.rbegin() + maxLength);
		//check the popped values to see if they match up with a type signature
		//NOTE: this is repeated in typeSignatureTock
		//TODO: DRY
		for (auto unit : type->units) {
			bool isSignatureValid = true;
			//check the type signature against the stack
			for (unsigned int i = 0; i < unit.pops.size(); i++) {
				auto popSig = unit.pops.at(i);
				auto popStack = out->at(i);
				if (popSig == TYPESIG_ANY) {
					isSignatureValid = true;
				} else if (popSig == TYPESIG_LIST) {
					isSignatureValid = TYPESIG_LIST == charmFunctionToType(popStack);
				} else if (popSig == TYPESIG_LISTSTRING) {
					isSignatureValid = (TYPESIG_LIST == charmFunctionToType(popStack) || TYPESIG_STRING == charmFunctionToType(popStack));
				} else if (popSig == TYPESIG_STRING) {
					isSignatureValid = TYPESIG_STRING == charmFunctionToType(popStack);
				} else if (popSig == TYPESIG_INT) {
					isSignatureValid = TYPESIG_INT == charmFunctionToType(popStack);
				} else if (popSig == TYPESIG_FLOAT) {
					isSignatureValid = TYPESIG_FLOAT == charmFunctionToType(popStack);
				}
				if (!isSignatureValid) {
					break;
				}
			}
			// and if it's still valid by the time the types have been checked, then
			// we return and don't error
			if (isSignatureValid) {
				return out;
			}
		}
		//if we exited the type signature checking function without finding a valid type signature
		std::stringstream typeSigError;
		typeSigError << "Type signature check for function " << name << " failed." << std::endl;
		typeSigError << "The function wanted types `" << charmTypeSignatureToString(*type) << "` but it got types `" << "TODO" << "`" << std::endl;
		runtime_die(typeSigError.str());
	}
	//this returns an empty std::option
	return out;
}
void Runner::typeSignatureTock(std::vector<CharmFunction> tick) {

}


void Runner::handleDefinedFunctions(CharmFunction f, RunnerContext* context) {
	//PredefinedFunctions.h holds all the functions written in C++
	//other than that, if these functions aren't built in, they are run through
	//the functionDefinitions table.

	//first, make sure that the function we're trying to run exists in the PredefinedFunctions
	//table. if it doesn't - assume it's defined in Charm and run through the
	//functionDefinitions table.
	if (DEBUGMODE) {
		puts("ALL PREDEFINED FUNCTIONS: ");
		for (auto f : pF->cppFunctionNames) {
			printf("%s ", f.first.c_str());
		}
		puts("");
	}
	bool isPredefinedFunction = (pF->cppFunctionNames.find(f.functionName) != pF->cppFunctionNames.end());
	bool isFFIFunction = (ffi->mutateFFIFuncs.find(f.functionName) != ffi->mutateFFIFuncs.end());
	ONLYDEBUG printf("isPredefinedFunction? %s. isFFIFunction? %s\n", isPredefinedFunction ? "Yes" : "No", isFFIFunction ? "Yes" : "No");
	if (isPredefinedFunction) {
		//run the predefined function!
		//(note: the function context AKA the definition we are running code from
		//is passed in for tail call optimization in PredefinedFunctions.cpp::ifthen())
		pF->functionLookup(f.functionName, this, context);
	} else if (isFFIFunction) {
		ffi->runFFI(f.functionName, this);
	} else {
		//alright, now we get down and dirty
		//look through the functionDefinitions table for a function with
		//a matching name, and run that. if there are no functions - throw
		//an error.
		if (DEBUGMODE) {
			puts("ALL DEFINED FUNCTIONS: ");
			for (auto f : functionDefinitions) {
				printf("%s ", f.first.c_str());
			}
			puts("");
		}
		auto possibleFunction = functionDefinitions.find(f.functionName);
		if (possibleFunction != functionDefinitions.end()) {
			auto fD = possibleFunction->second;
			//wait! before we run it, check and make sure this function isn't tail recursive
			if (fD.definitionInfo.tailCallRecursive) {
				//if it is, drop the last call to itself and just run it in a loop
				//TODO: exiting a tail-call loop?
				CHARM_LIST_TYPE functionBodyCopy = fD.functionBody;
				functionBodyCopy.pop_back();
				while (1) {
					Runner::run(std::pair<CHARM_LIST_TYPE, FunctionAnalyzer*>(functionBodyCopy, context->fA));
				}
			}
			//ooh. the only time we use this call!
			context->fD = &fD;
			Runner::runWithContext(fD.functionBody, context);
		} else {
			runtime_die("Unknown function `" + f.functionName + "`.");
		}
	}
}

void Runner::addNamespacePrefix(CharmFunction& f, std::string ns) {
	if (ns == "") {
		return;
	}
	if (f.functionType == NUMBER_FUNCTION) {
		return;
	} else if (f.functionType == STRING_FUNCTION) {
		return;
	} else if (f.functionType == LIST_FUNCTION) {
		for (CharmFunction& currentFunction : f.literalFunctions) {
			addNamespacePrefix(currentFunction, ns);
		}
		return;
	} else if (f.functionType == FUNCTION_DEFINITION) {
		f.functionName = ns + f.functionName;
		for (CharmFunction& currentFunction : f.literalFunctions) {
			addNamespacePrefix(currentFunction, ns);
		}
		return;
	} else if (f.functionType == DEFINED_FUNCTION) {
		bool isAlreadyDefined = (functionDefinitions.find(f.functionName) != functionDefinitions.end());
		bool isPredefinedFunction = (pF->cppFunctionNames.find(f.functionName) != pF->cppFunctionNames.end());
		bool isFFIFunction = (ffi->mutateFFIFuncs.find(f.functionName) != ffi->mutateFFIFuncs.end());
		ONLYDEBUG printf("isAlreadyDefined: %s, isPredefinedFunction: %s, isFFIFunction: %s\n", isAlreadyDefined ? "Yes" : "No", isPredefinedFunction ? "Yes" : "No", isFFIFunction ? "Yes" : "No");
		if (isPredefinedFunction || isFFIFunction || isAlreadyDefined) {
			//don't rename the function if it was defined globally outside of this file
			//or it was already defined (aka: in the prelude)
			//note: adding the "if already defined" clause ensures that functions from its own file don't trip the system,
			//as those functions were already transformed and had their namespace prepended.
		} else {
			f.functionName = ns + f.functionName;
		}
	}
}

void Runner::runWithContext(CHARM_LIST_TYPE parsedProgram, RunnerContext* context, std::string ns) {
	for (CharmFunction currentFunction : parsedProgram) {
		if (ns != "") {
			ONLYDEBUG printf("ADDING NAMESPACE %s\n", ns.c_str());
			Runner::addNamespacePrefix(currentFunction, ns);
		}
		//alright, now we get into the running portion
		if (currentFunction.functionType == NUMBER_FUNCTION) {
			ONLYDEBUG puts("RUNNING AS NUMBER_FUNCTION");
			//first, let's do the numbers
			Runner::getCurrentStack()->push(currentFunction);
			//easy, right? let's do more
		} else if (currentFunction.functionType == STRING_FUNCTION) {
			ONLYDEBUG puts("RUNNING AS STRING_FUNCTION");
			//now we push strings onto the stack
			Runner::getCurrentStack()->push(currentFunction);
			//still p easy ye
		} else if (currentFunction.functionType == LIST_FUNCTION) {
			ONLYDEBUG puts("RUNNING AS LIST_FUNCTION");
			//now we push on the lists
			Runner::getCurrentStack()->push(currentFunction);
			//wow this is easy right? now get ready baby
		} else if (currentFunction.functionType == FUNCTION_DEFINITION) {
			ONLYDEBUG puts("RUNNING AS FUNCTION_DEFINTION");
			//lets define some functions bruh
			FunctionDefinition tempFunction;
			tempFunction.functionName = currentFunction.functionName;
			tempFunction.functionBody = currentFunction.literalFunctions;
			tempFunction.definitionInfo = currentFunction.definitionInfo;
			Runner::addFunctionDefinition(tempFunction);
			ONLYDEBUG printf("ADDED FUNCTION DEFINITION FOR %s\n", tempFunction.functionName.c_str());
			//that was easy too! oh no...
		} else if (currentFunction.functionType == DEFINED_FUNCTION) {
			ONLYDEBUG puts("RUNNING AS DEFINED_FUNCTION");
			//check the top of the stack before the function itself runs
			//TODO: the function gets optimized out before it comes here.
			//what can I do about that? i have no idea. i'll do it tomorrow.
			auto tick = Runner::typeSignatureTick(currentFunction.functionName, context);
			//let's do these defined functions now
			Runner::handleDefinedFunctions(currentFunction, context);
			//lol you thought i'd do it here
			//check the stack at function's exit to make sure the type signature holds up
			//if the optional is unset, there was no type sig provided so don't bother running this
			if (tick) {
				Runner::typeSignatureTock(*tick);
			}
		}
	}
	ONLYDEBUG puts("EXITING RUNNER::RUN");
}

void Runner::run(std::pair<CHARM_LIST_TYPE, FunctionAnalyzer*> parsedProgramWithAnalyzer, std::string ns) {
	RunnerContext rC;
	rC.fA = parsedProgramWithAnalyzer.second;
	rC.fD = nullptr;
	Runner::runWithContext(parsedProgramWithAnalyzer.first, &rC, ns);
}
