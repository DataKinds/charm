#include <vector>

#include "Runner.h"
#include "ParserTypes.h"
#include "PredefinedFunctions.h"
#include "Error.h"
#include "Debug.h"
#include "FFI.h"
#include "FunctionAnalyzer.h"

Runner::Runner() {
	//initialize the stacks
	CharmFunction zero = Stack::zeroF();
	currentStackName = zero;
	stacks.push_back(Stack(zero));
	pF = new PredefinedFunctions();
	ffi = new FFI();
}

void Runner::addFunctionDefinition(FunctionDefinition fD) {
	//first, check and make sure there's no other definition with
	//the same name. if there is, overwrite it. if not, just push_back
	//this definition.
	if (functionDefinitions.find(fD.functionName) == functionDefinitions.end()) {
		functionDefinitions[fD.functionName] = fD;
	}
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
/*
void Runner::handleDefinedFunctions(CharmFunction f) {
	//PredefinedFunctions.h holds all the functions written in C++
	//other than that, if these functions aren't built in, they are run through
	//the functionDefinitions table.

	//first, make sure that the function we're trying to run exists in the PredefinedFunctions
	//table. if it doesn't - assume it's defined in Charm and run through the
	//functionDefinitions table.
	if (DEBUGMODE) {
		puts("ALL PREDEFINED FUNCTIONS: ");
		for (auto f : pF.nativeFunctions) {
			printf("%s ", f.first.c_str());
		}
		puts("");
	}
	bool isPredefinedFunction = (pF.nativeFunctions.find(f.functionName) != pF.nativeFunctions.end());
	bool isFFIFunction = (ffi.mutateFFIFuncs.find(f.functionName) != ffi.mutateFFIFuncs.end());
	ONLYDEBUG printf("isPredefinedFunction? %s. isFFIFunction? %s\n", isPredefinedFunction ? "Yes" : "No", isFFIFunction ? "Yes" : "No");
	if (isPredefinedFunction) {
		//run the predefined function!
		//(note: the function context AKA the definition we are running code from
		//is passed in for tail call optimization in PredefinedFunctions.cpp::ifthen())
		pF.functionLookup(f.functionName, this, context);
	} else if (isFFIFunction) {
		ffi.runFFI(f.functionName, this);
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
			FunctionDefinition fD = possibleFunction->second;
			fD.functionBody = possibleFunction->second.functionBody;
			//wait! before we run it, check and make sure this function isn't tail recursive
			if (fD.definitionInfo.tailCallRecursive) {
				//if it is, drop the last call to itself and just run it in a loop
				//TODO: exiting a tail-call loop?
				CHARM_LIST_TYPE functionBodyCopy = fD.functionBody;
				functionBodyCopy.pop_back();
				while (1) {
					Runner::run(std::pair<CHARM_LIST_TYPE, FunctionAnalyzer*>(functionBodyCopy, context.fA));
				}
			}
			ONLYDEBUG printf("SETTING CONTEXT fD FOR FUNCTION %s\n    ", f.functionName.c_str());
			for (auto currentFunction : fD.functionBody) {
				ONLYDEBUG printf("%s ", charmFunctionToString(currentFunction).c_str());
			}
			context.fD = fD;
			context.inDefinition = true;
			//ooh. the only time we use this call!
			Runner::runWithContext(fD.functionBody, context);
		} else {
			runtime_die("Unknown function `" + f.functionName + "`.");
		}
	}
}

*/

void runFunction(std::string f) {
	//PredefinedFunctions.h holds all the functions written in C++
	//other than that, if these functions aren't built in, they are run through
	//the functionDefinitions table.

	//first, make sure that the function we're trying to run exists in the PredefinedFunctions
	//table. if it doesn't - assume it's defined in Charm and run through the
	//functionDefinitions table.
	if (DEBUGMODE) {
		puts("ALL PREDEFINED FUNCTIONS: ");
		for (auto f_ : pF.nativeFunctions) {
			printf("%s ", f_.first.c_str());
		}
		puts("");
	}
	ONLYDEBUG printf("isPredefinedFunction? %s. isFFIFunction? %s\n", isPredefinedFunction ? "Yes" : "No", isFFIFunction ? "Yes" : "No");
	// if it's predefined nattively
	if (pF.nativeFunctions.find(f) != pF.nativeFunctions.end()) {
		//run the predefined function!
		//(note: the function context AKA the definition we are running code from
		//is passed in for tail call optimization in PredefinedFunctions.cpp::ifthen())
		pF.functionLookup(f, this, context);
	// else it's defined through FFI
	} else if (ffi.mutateFFIFuncs.find(f) != ffi.mutateFFIFuncs.end()) {
		ffi.runFFI(f, this);
	} else {
		// TODO: tail call elimination
		auto possibleFunction = functionDefinitions.find(f.functionName);
		if (possibleFunction != functionDefinitions.end()) {
			Runner::run(possibleFunction->second.functionBody);
		}
	}
}
void Runner::run(std::vector<Token> tokens) {
	for (Token tok : tokens) {
		std::visit([this](auto& arg){
			using T = std::decay_t<decltype(arg)>;
			if constexpr (std::is_same_v<T, Token::TypeSignature>) {
				// pass
			}
			if constexpr (std::is_same_v<T, Token::List>) {
				// TODO
				CharmFunction f = {
					.functionType = LIST_FUNCTION,
					.literalFunctions = {}
				};
				this->getCurrentStack()->push(f);
			}
			if constexpr (std::is_same_v<T, Token::String>) {
				CharmFunction f = {
					.functionType = STRING_FUNCTION,
					.stringValue = arg.string
				};
				this->getCurrentStack()->push(f);
			}
			if constexpr (std::is_same_v<T, Token::Number>) {
				// TODO
				CharmFunction f = {
					.functionType = NUMBER_FUNCTION,
					.numberValue = {
						.whichType = FLOAT_VALUE,
						.floatValue = arg.number
					}
				};
				this->getCurrentStack()->push(f);
			}
			if constexpr (std::is_same_v<T, Token::Definition>) {
				// TODO
				FunctionDefinition f = {
					.functionName = arg.functionName,
					.functionBody = {},
					.definitionInfo = {}
				};
				this->addFunctionDefinition(f);
			}
			if constexpr (std::is_same_v<T, Token::Function>) {
				Runner::runFunction(arg.function);
			}
		}, tok.token);
	}
}
