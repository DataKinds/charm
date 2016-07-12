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
	//first split the string on newlines
	std::vector<std::string> newlineSplitString = Parser::splitString(charmInput, '\n');
	std::vector<std::vector<std::string>> tokenizedString;
	//then split it on spaces
	for (auto s : newlineSplitString) {
		tokenizedString.push_back(splitString(s, ' '));
	}
	for (unsigned long long lineNum = 0; lineNum < tokenizedString.size(); lineNum++) {
		for (unsigned long long tokenNum = 0; tokenNum < tokenizedString[lineNum].size(); tokenNum++) {

			CharmFunction currentFunction;
			currentFunction.functionType = Parser::recognizeFunction(tokenizedString[lineNum][tokenNum]);
			if (currentFunction.functionType == DEFINED_FUNCTION) {
				//deal with DEFINED_FUNCTION first, easiest to deal with
				currentFunction.functionName = tokenizedString[lineNum][tokenNum];
			} else if (currentFunction.functionType == NUMBER_FUNCTION) {
				//next deal with NUMBER_FUNCTION
				CharmNumber numberValue;
				//if it contains a '.' it's a long double
				//if not it's a long long
				if (tokenizedString[lineNum][tokenNum].find('.') != std::string::npos) {
					numberValue.whichType = FLOAT_VALUE;
					numberValue.floatValue = std::stold(tokenizedString[lineNum][tokenNum]);
				} else {
					numberValue.whichType = INTEGER_VALUE;
					numberValue.integerValue = std::stoi(tokenizedString[lineNum][tokenNum]);
				}
				currentFunction.numberValue = numberValue;
			} else if (currentFunction.functionType == STRING_FUNCTION) {
				//next deal with STRING_FUNCTION
				//now it's getting hard: a string continues
				//until it hits the next STRING_FUNCTION
				//then, that second STRING_FUNCTION is popped
				//and we continue. if there are no other STRING_FUNCTIONs,
				//then we just end the string at the end of the line
				unsigned long long nextTokenNum = tokenNum + 1;
				while (nextTokenNum < tokenizedString[lineNum].size()) {
					if (Parser::recognizeFunction(tokenizedString[lineNum][nextTokenNum]) == STRING_FUNCTION) {
						break;
					}
					nextTokenNum++;
				}
				//create a string stream to compile the string for the funcion
				std::stringstream ss;
				//fill in the stringstream
				for (unsigned long long stringIndex = tokenNum; stringIndex < nextTokenNum; stringIndex++) {
					ss << tokenizedString[lineNum][stringIndex] << " ";
				}
				//FINALLY we can fill in currentFunction
				currentFunction.stringValue = ss.str();
			} else if (currentFunction.functionType == LIST_FUNCTION) {
				//same thing as before, except it's a list
				//and not a string. this time, we look for a "]"
				//to end the list (or a new line. that works too)
				unsigned long long nextTokenNum = tokenNum + 1;
				while (nextTokenNum < tokenizedString[lineNum].size()) {
					if (tokenizedString[lineNum][nextTokenNum] == "]") {
						break;
					}
					nextTokenNum++;
				}
				//first, we have to make another string with the contents
				//this is just like the string
				std::stringstream ss;
				for (unsigned long long tokenIndex = tokenNum; tokenIndex < nextTokenNum; tokenIndex++) {
					ss << tokenizedString[lineNum][tokenIndex] << " ";
				}
				//finally, we can put it into the currentFunction
				currentFunction.literalFunctions = parse(ss.str());
			}
			out.push_back(currentFunction);
		}
	}
	//wow, we're finally done with this abomination of a function
	return out;
}
