#pragma once
#include <string>
#include <vector>

enum CharmFunctionType {
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

struct CharmFunction {
	CharmFunctionType functionType;
	//ONLY USED WITH STRING_FUNCTION
	std::string stringValue;
	//ONLY USED WITH NUMBER_FUNCTION
	CharmNumber numberValue;
	//ONLY USED WITH QUOTE FUNCTION
	std::vector<CharmFunction> literalFunctions;
	//ONLY USED WITH DEFINED FUNCTION
	std::string functionName;
};

//ALL DEFINITIONS ARE CONSTANTS
struct CharmDefinition {
	std::string constantName;
	std::vector<CharmFunction> definition;
};
