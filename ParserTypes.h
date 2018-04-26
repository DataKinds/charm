#pragma once
#include <string>
#include <sstream>
#include <vector>
#include <deque>
#include <variant>
#include <gmpxx.h>

#ifndef CHARM_STACK_TYPE
	#define CHARM_STACK_TYPE std::deque<CharmFunction>
#endif

#ifndef CHARM_LIST_TYPE
	#define CHARM_LIST_TYPE std::vector<CharmFunction>
#endif

enum CharmTypes {
	TYPESIG_ANY,
	TYPESIG_LIST,
	TYPESIG_LISTSTRING,
	TYPESIG_STRING,
	TYPESIG_INT,
	TYPESIG_FLOAT
};
struct CharmTypeSignatureUnit {
	std::vector<CharmTypes> pops;
	std::vector<CharmTypes> pushes;
};
struct CharmTypeSignature {
	std::string functionName;
	std::vector<CharmTypeSignatureUnit> units;
};

enum CharmFunctionType {
	FUNCTION_DEFINITION, //not a function, gets removed upon running
						 //serves to create static function definitions
	LIST_FUNCTION,  //not really a function, but
	                //if the thing is in brackets,
				    //this makes it a lot easier
				    //to deal with
	NUMBER_FUNCTION, //pushes number on stack
	STRING_FUNCTION, //pushes string on stack
	DEFINED_FUNCTION //built in function like
	                 //dup, pop, i
					 //or for preprocessing the
					 //definitions
};

enum CharmNumberType {
	INTEGER_VALUE,
	FLOAT_VALUE
};

struct CharmNumber {
	CharmNumberType whichType;
	mpz_class integerValue;
	mpf_class floatValue;
	//to allow Charm to support both floats
	//and integers, we add both
};

struct CharmFunctionDefinitionInfo {
	bool inlineable;
	bool tailCallRecursive;
};
struct CharmFunction {
	CharmFunctionType functionType;
	//ONLY USED WITH STRING_FUNCTION
	std::string stringValue;
	//ONLY USED WITH NUMBER_FUNCTION
	CharmNumber numberValue;
	//ONLY USED WITH LIST_FUNCTION AND FUNCTION_DEFINITION
	CHARM_LIST_TYPE literalFunctions;
	//ONLY USED WITH DEFINED_FUNCTION AND FUNCTION_DEFINITION
	std::string functionName;
	//ONLY USED WITH FUNCTION_DEFINITION
	CharmFunctionDefinitionInfo definitionInfo;
};
inline std::string charmTypeToString(CharmTypes t) {
	switch (t) {
		case TYPESIG_ANY:
		return "any";
		break;
		case TYPESIG_LIST:
		return "list";
		break;
		case TYPESIG_LISTSTRING:
		return "list/string";
		break;
		case TYPESIG_STRING:
		return "string";
		break;
		case TYPESIG_INT:
		return "int";
		break;
		case TYPESIG_FLOAT:
		return "float";
		break;
	}
}
inline std::string charmTypeSignatureToString(CharmTypeSignature t) {
	std::stringstream out;
	for (auto unitIter = t.units.begin(); unitIter != t.units.end(); ++unitIter) {
		for (auto& type : unitIter->pops) {
			out << charmTypeToString(type) << " ";
		}
		out << "-> ";
		for (auto& type : unitIter->pushes) {
			out << charmTypeToString(type) << " ";
		}
		// only add the pipe if we're not at the end
		if (unitIter != std::prev(t.units.end())) {
			out << "| ";
			continue;
		}
	}
	return out.str();
}
inline CharmTypes charmFunctionToType(CharmFunction f) {
	switch (f.functionType) {
		case LIST_FUNCTION:
		return TYPESIG_LIST;
		break;

		case NUMBER_FUNCTION:
		switch (f.numberValue.whichType) {
			case INTEGER_VALUE:
			return TYPESIG_INT;
			break;

			case FLOAT_VALUE:
			return TYPESIG_FLOAT;
			break;
		}
		break;

		case STRING_FUNCTION:
		return TYPESIG_STRING;
		break;

		//these shouldn't happen in the course of normal execution
		case FUNCTION_DEFINITION:
		return TYPESIG_ANY;
		break;
		case DEFINED_FUNCTION:
		return TYPESIG_ANY;
		break;
	}
	return TYPESIG_ANY;
}
inline std::string charmFunctionToString(CharmFunction f) {
	std::stringstream out;
	switch (f.functionType) {
		case FUNCTION_DEFINITION:
		out << f.functionName << ":=";
		for (CharmFunction fs : f.literalFunctions) {
			out << charmFunctionToString(fs) << " ";
		}
		break;

		case LIST_FUNCTION:
		out << "[ ";
		for (CharmFunction fs : f.literalFunctions) {
			out << charmFunctionToString(fs) << " ";
		}
		out << "]";
		break;

		case NUMBER_FUNCTION:
		switch (f.numberValue.whichType) {
			case INTEGER_VALUE:
			out << f.numberValue.integerValue.get_str();
			break;

			case FLOAT_VALUE:
			long exponent;
			std::string outString = f.numberValue.floatValue.get_str(exponent);
			if (exponent < 0) {
				out << "0." << outString.insert(0, std::abs(exponent), '0');
			} else if (exponent == 0) {
				out << "0." << outString;
			} else if (exponent > 0) {
				out << outString.insert(exponent, 1, '.');
			}
			break;
		}
		break;

		case STRING_FUNCTION:
		out << "\" " << f.stringValue << " \"";
		break;

		case DEFINED_FUNCTION:
		out << f.functionName;
		break;
	}
	return out.str();
}

inline bool operator==(const CharmFunction& lhs, const CharmFunction& rhs){
	if (lhs.functionType == rhs.functionType) {
		switch (lhs.functionType) {
			case LIST_FUNCTION:
			if (lhs.literalFunctions.size() != rhs.literalFunctions.size()) {
				return false;
			} else {
				for (unsigned long long n = 0; n < lhs.literalFunctions.size(); n++) {
					if (!(lhs.literalFunctions[n] == rhs.literalFunctions[n])) {
						return false;
					}
				}
				return true;
			}
			break;

			case NUMBER_FUNCTION:
			if (rhs.numberValue.whichType == lhs.numberValue.whichType) {
				if (lhs.numberValue.whichType == INTEGER_VALUE) {
					return (lhs.numberValue.integerValue == rhs.numberValue.integerValue);
				} else {
					return (lhs.numberValue.floatValue == rhs.numberValue.floatValue);
				}
			} else {
				return false;
			}
			break;

			case STRING_FUNCTION:
			return (lhs.stringValue == rhs.stringValue);
			break;

			case DEFINED_FUNCTION:
			return (lhs.functionName == rhs.functionName);
			break;

			case FUNCTION_DEFINITION:
			return (lhs.functionName == rhs.functionName);
			break;
		}
	} else {
		return false;
	}
	//somehow, you skirted around the entire if/else. impressive?
	return false;
}


//ALL DEFINITIONS ARE CONSTANTS
struct CharmDefinition {
	std::string constantName;
	CHARM_LIST_TYPE definition;
};

class FunctionDefinition;
class FunctionAnalyzer;
//USED IN RUNNER.CPP AND PREDEFINEDFUNCTIONS.CPP
struct RunnerContext {
	FunctionDefinition* fD;
	FunctionAnalyzer* fA;
	unsigned long fIndex;
};
