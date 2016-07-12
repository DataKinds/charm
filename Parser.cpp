#include <vector>
#include <sstream>

#include "Parser.h"
#include "ParserTypes.h"
#include "Debug.h"

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

bool Parser::isLineFunctionDefinition(std::vector<std::string> line) {
	for (unsigned long long tokenIndex = 0; tokenIndex < line.size(); tokenIndex++) {
		if (Parser::recognizeFunction(line[tokenIndex]) == FUNCTION_DEFINITION) {
			return true;
		}
	}
	return false;
}

CharmFunctionType Parser::recognizeFunction(std::string s) {
	if (s == "[") return LIST_FUNCTION;
	if (s == "\"") return STRING_FUNCTION;
	if (s == ":=") return FUNCTION_DEFINITION;
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
		//first, check and make sure that this line doesn't
		//contain a function definition before parsing it
		if (isLineFunctionDefinition(tokenizedString[lineNum])) {
			//if there was a function definition, do some weird stuff
			//set functionType to FUNCTION_DEFINITION (duh)
			//take the first token before the := and set it to the functionName
			//take all the tokens after the :=, parse them, and make them the literalFunctions
			CharmFunction currentFunction;
			currentFunction.functionType = FUNCTION_DEFINITION;
			unsigned long long equalsIndex = 0;
			while (Parser::recognizeFunction(tokenizedString[lineNum][equalsIndex]) != FUNCTION_DEFINITION) {
				equalsIndex++;
			}
			//alright, now we found the :=
			//let's make sure that it's not the first or last element
			if (!((equalsIndex == 0) || (equalsIndex >= (tokenizedString[lineNum].size() - 1)))) {
				//now we set the stuff!
				currentFunction.functionName = tokenizedString[lineNum][equalsIndex - 1];
				std::stringstream ss;
				//populate the stringstream with the tokens after the :=
				//this call ensures that the first token won't be a :=, but rather,
				//the first token past that
				equalsIndex++;
				for (; equalsIndex < tokenizedString[lineNum].size(); equalsIndex++) {
					ss << tokenizedString[lineNum][equalsIndex] << " ";
				}
				ONLYDEBUG printf("FUNCTION IS NAMED %s\n", currentFunction.functionName.c_str());
				ONLYDEBUG printf("FUNCTION BODY IS %s\n", ss.str().c_str());
				currentFunction.literalFunctions = parse(ss.str());
				//we outta here!
				out.push_back(currentFunction);
			}
		} else {
			for (unsigned long long tokenNum = 0; tokenNum < tokenizedString[lineNum].size(); tokenNum++) {
				ONLYDEBUG printf("PARSING %s\n", tokenizedString[lineNum][tokenNum].c_str());
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
					//until it hits the next STRING_FUNCTION, popping along the way
					//then, that second STRING_FUNCTION is popped
					//and we continue. if there are no other STRING_FUNCTIONs,
					//then we just end the string at the end of the line
					//create a string stream to compile the string for the funcion
					std::stringstream ss;
					//fill in the stringstream
					//until the line ends or another " occurs
					tokenNum++;
					while ((tokenNum < tokenizedString[lineNum].size()) &&
				           (Parser::recognizeFunction(tokenizedString[lineNum][tokenNum]) != STRING_FUNCTION)) {
							   ss << tokenizedString[lineNum][tokenNum] << " ";
							   ONLYDEBUG printf("ERASING %s\n", tokenizedString[lineNum][tokenNum].c_str());
							   tokenizedString[lineNum].erase(tokenizedString[lineNum].begin() + tokenNum);
					}
					//make sure that the final quote was removed if it exists
					//(AKA we're not at the end of the line)
					//if (nextTokenNum < tokenizedString[lineNum].size()) {
					//	printf("ERASING \": %s\n", tokenizedString[lineNum][nextTokenNum].c_str());
					//	tokenizedString[lineNum].erase(tokenizedString[lineNum].begin() + nextTokenNum);
					//}
					//FINALLY we can fill in currentFunction
					currentFunction.stringValue = ss.str();
				} else if (currentFunction.functionType == LIST_FUNCTION) {
					//same thing as before, except it's a list
					//and not a string. this time, we look for a "]"
					//to end the list (or a new line. that works too)
					//first, we have to make another string with the contents
					//this is just like the string
					std::stringstream ss;
					tokenNum++;
					while ((tokenNum < tokenizedString[lineNum].size()) &&
						   (tokenizedString[lineNum][tokenNum] != "]")) {
							   ss << tokenizedString[lineNum][tokenNum] << " ";
							   tokenizedString[lineNum].erase(tokenizedString[lineNum].begin() + tokenNum);
					}
					//finally, we can put it into the currentFunction
					currentFunction.literalFunctions = parse(ss.str());
				}
				out.push_back(currentFunction);
			}
		}
	}
	//wow, we're finally done with this abomination of a function
	return out;
}
