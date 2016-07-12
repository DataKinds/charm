#include <vector>
#include <sstream>
#include "Parser.h"
#include "ParserTypes.h"

Parser::Parser() {
}

//http://stackoverflow.com/a/236803
std::vector<std::string> Parser::splitString(const std::string s, char delim) {
	std::vector<std::string> out;
	std::stringstream ss(s);
	std::string value;
	while (std::getline(ss, value, delim)) {
		if (!value.empty())
			out.push_back(value);
	}
	return out;
}

bool Parser::isCharDigit(char c) {
	std::string acceptableNumberChars = "-.0123456789";
	for (auto checkNum : acceptableNumberChars) {
		if (c == checkNum) return true;
	}
	return false;
}

bool Parser::isStringNumber(std::string str) {
	for (auto c : str) {
		if (!Parser::isCharDigit(c)) {
			return false;
		}
	}
	return true;
}

CharmFunctionType Parser::recognizeFunction(std::string s) {
	if (s == "[") return LIST_FUNCTION;
	if (s == "\"") return STRING_FUNCTION;
	//if it's not any of those two
	if (Parser::isStringNumber(s)) return NUMBER_FUNCTION;
	//if it's just a string, it's just a function
	return DEFINED_FUNCTION;
}

std::vector<CharmFunction> Parser::parse(const std::string charmInput) {
	std::vector<CharmFunction> out;
	std::vector<std::string> tokenizedString = Parser::splitString(charmInput, ' ');
	for (unsigned long long tokenPointer = 0; tokenPointer < tokenizedString.size(); tokenPointer++) {
		CharmFunction currentFunction;
		currentFunction.functionType = Parser::recognizeFunction(tokenizedString[tokenPointer]);
		if (currentFunction.functionType == DEFINED_FUNCTION) {
			//deal with DEFINED_FUNCTION first, easiest to deal with
			currentFunction.functionName = tokenizedString[tokenPointer];
		} else if (currentFunction.functionType == NUMBER_FUNCTION) {
			//next deal with NUMBER_FUNCTION
			CharmNumber numberValue;
			//if it contains a '.' it's a long double
			//if not it's a long long
			if (tokenizedString[tokenPointer].find('.') != std::string::npos) {
				numberValue.whichType = FLOAT_VALUE;
				numberValue.floatValue = std::stold(tokenizedString[tokenPointer]);
			} else {
				numberValue.whichType = INTEGER_VALUE;
				numberValue.integerValue = std::stoi(tokenizedString[tokenPointer]);
			}
			currentFunction.numberValue = numberValue;
		} else if (currentFunction.functionType == STRING_FUNCTION) {
			//next deal with STRING_FUNCTION
			//now it's getting hard: a string continues
			//until it hits the next STRING_FUNCTION
			//then, that second STRING_FUNCTION is popped
			//and we continue. if there are no other STRING_FUNCTIONs,
			//then we just end the string at the end of the line
			unsigned long long nextStringFunctionPointer = tokenPointer + 1;
			while (nextStringFunctionPointer < tokenizedString.size()) {
				if (Parser::recognizeFunction(tokenizedString[nextStringFunctionPointer]) == STRING_FUNCTION) {
					break;
				}
				nextStringFunctionPointer++;
			}
		} else if (currentFunction.functionType == LIST_FUNCTION) {
			//same thing as before, except it's a list
			//and not a string
		}
		out.push_back(currentFunction);
	}
}
