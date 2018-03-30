#pragma once
#include <string>
#include <vector>

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
	long long integerValue;
	long double floatValue;
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
	std::vector<CharmFunction> literalFunctions;
	//ONLY USED WITH DEFINED_FUNCTION AND FUNCTION_DEFINITION
	std::string functionName;
	//ONLY USED WITH FUNCTION_DEFINITION
	CharmFunctionDefinitionInfo definitionInfo;
};


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
			//should never happen
			return false;
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
	std::vector<CharmFunction> definition;
};
