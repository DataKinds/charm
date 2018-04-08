#include <vector>
#include <sstream>

#include "Parser.h"
#include "ParserTypes.h"
#include "Debug.h"
#include "FunctionAnalyzer.h"

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
	bool hasDigit = false;
	bool hasPunctuation = false;
	for (auto c : str) {
		if (!Parser::isCharDigit(c)) {
			return false;
		}
		if (c == '.' || c == '-') {
			hasPunctuation = true;
		} else {
			hasDigit = true;
		}
	}
	return (hasDigit && hasPunctuation) || hasDigit;
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

CharmFunctionDefinitionInfo Parser::analyzeDefinition(CharmFunction f) {
	CharmFunctionDefinitionInfo out;
	//create an analyzer
	FunctionAnalyzer fA(f);
	//first, we fill in the info and see if the function is not recursive/inlineable
	out.inlineable = fA.isInlinable();
	//then we fill in the inlineDefinitions deque, for parsing future DEFINED_FUNCTIONs
	if (out.inlineable) {
		inlineDefinitions.push_back(f);
	}
	out.tailCallRecursive = fA.isTailCallRecursive();
	return out;
}

CharmFunction Parser::parseDefinition(std::vector<std::string> line) {
	//if there was a function definition, do some weird stuff
	//set functionType to FUNCTION_DEFINITION (duh)
	//take the first token before the := and set it to the functionName
	//take all the tokens after the :=, parse them, and make them the literalFunctions
	CharmFunction currentFunction;
	currentFunction.functionType = FUNCTION_DEFINITION;
	CharmFunctionDefinitionInfo functionInfo = Parser::analyzeDefinition(currentFunction);
	currentFunction.definitionInfo = functionInfo;
	unsigned long long equalsIndex = 0;
	while (Parser::recognizeFunction(line[equalsIndex]) != FUNCTION_DEFINITION) {
		equalsIndex++;
	}
	//alright, now we found the :=
	//let's make sure that it's not the first or last element
	if (!((equalsIndex == 0) || (equalsIndex >= (line.size() - 1)))) {
		//now we set the stuff!
		currentFunction.functionName = line[equalsIndex - 1];
		std::stringstream ss;
		//populate the stringstream with the tokens after the :=
		//this call ensures that the first token won't be a :=, but rather,
		//the first token past that
		equalsIndex++;
		for (; equalsIndex < line.size(); equalsIndex++) {
			ss << line[equalsIndex] << " ";
		}
		ONLYDEBUG printf("FUNCTION IS NAMED %s\n", currentFunction.functionName.c_str());
		ONLYDEBUG printf("FUNCTION BODY IS %s\n", ss.str().c_str());
		//DIRTY HACK ALERT
		currentFunction.literalFunctions = Parser::lex(ss.str());
		//END DIRTY HACK
		//we outta here!
	}

	ONLYDEBUG printf("IS %s INLINEABLE? %s\n", currentFunction.functionName.c_str(), currentFunction.definitionInfo.inlineable ? "Yes" : "No");
	ONLYDEBUG printf("IS %s TAIL CALL RECURSIVE? %s\n", currentFunction.functionName.c_str(), currentFunction.definitionInfo.tailCallRecursive ? "Yes" : "No");
	return currentFunction;
}

CharmFunction Parser::parseDefinedFunction(std::string tok) {
	CharmFunction out;
	out.functionType = DEFINED_FUNCTION;
	out.functionName = tok;
	return out;
}

CharmFunction Parser::parseNumberFunction(std::string tok) {
	CharmFunction out;
	out.functionType = NUMBER_FUNCTION;
	CharmNumber numberValue;
	//if it contains a '.' it's a long double
	//if not it's a long long
	if (tok.find('.') != std::string::npos) {
		numberValue.whichType = FLOAT_VALUE;
		numberValue.floatValue = std::stold(tok);
	} else {
		numberValue.whichType = INTEGER_VALUE;
		numberValue.integerValue = std::stoll(tok);
	}
	out.numberValue = numberValue;
	return out;
}

CharmFunction Parser::parseStringFunction(std::vector<std::string> *line, unsigned long long *tokenNum) {
	CharmFunction out;
	out.functionType = STRING_FUNCTION;
	//now it's getting hard: a string continues
	//until it hits the next STRING_FUNCTION, popping along the way
	//then, that second STRING_FUNCTION is popped
	//and we continue. if there are no other STRING_FUNCTIONs,
	//then we just end the string at the end of the line
	//create a string stream to compile the string for the funcion
	std::stringstream ss;
	//also: we have to skip the first space in the output, so keep a bool for that
	bool skippedSpace = false;
	//fill in the stringstream
	//until the line ends or another " occurs
	(*tokenNum)++;
	while (((*tokenNum) < line->size()) &&
		   (Parser::recognizeFunction((*line)[*tokenNum]) != STRING_FUNCTION)) {
			   if (skippedSpace) {
				   ss << " ";
			   } else {
				   skippedSpace = true;
			   }
			   ss << (*line)[*tokenNum];
			   ONLYDEBUG printf("ERASING %s\n", (*line)[*tokenNum].c_str());
			   (*line).erase(line->begin() + (*tokenNum));
	}
	//make sure that the final quote was removed if it exists
	//(AKA we're not at the end of the line)
	//FINALLY we can fill in out
	out.stringValue = ss.str();
	return out;
}

CharmFunction Parser::parseListFunction(std::vector<std::string> *line, unsigned long long *tokenNum) {
	CharmFunction out;
	out.functionType = LIST_FUNCTION;
	//and not a string. this time, we look for a "]"
	//to end the list (or a new line. that works too)
	//first, we have to make another string with the contents
	//this is just like the string
	std::stringstream ss;
	(*tokenNum)++;
	int listDepth = 1;
	while ((*tokenNum) < line->size()) {
		ONLYDEBUG printf("LIST DEPTH %i\n", listDepth);
		std::string token = (*line)[*tokenNum];
		line->erase(line->begin() + (*tokenNum));
		if (Parser::recognizeFunction(token) == LIST_FUNCTION) {
		   //if we see another "[" inside of here, we increase listDepth in order to not break on the first ]
		   listDepth++;
		} else if (token == "]") {
		   //else, we decrease listDepth
		   //remember, the loop ends when listDepth is zero, and it starts at one.
		   //additionally: ] is NOT a function and is not parsed as one, and weirdness ensues if it is
		   listDepth--;
		   if (listDepth <= 0) {
			   break;
		   }
		   //don't push the ] to the string to recursively parse
		   continue;
		}
		ss << token << " ";
	}
	//finally, we can put the inside of the [ ] into the out
	out.literalFunctions = Parser::lex(ss.str());
	ONLYDEBUG printf("CONTINUING PARSING AT TOKEN NUM %llu, WHICH IS %s\n", (*tokenNum), (*line)[*tokenNum].c_str());
	//NOTE: this is after hours of debugging, I've deemed this necessary
	//tl;dr version: the call to `erase` a few lines above mutates the vector
	//to the point where tokenNum actually refers to the next token ready to be
	//consumed, but parse() doesn't know that and will increment it anyway.
	//thus, we decrement it in order to counteract that. hopefully it doesn't
	//cause any meaningful bugs.
	(*tokenNum)--;
	return out;
}


CHARM_LIST_TYPE Parser::lex(const std::string charmInput) {
	ONLYDEBUG printf("WILL PARSE %s\n", charmInput.c_str());
	CHARM_LIST_TYPE out;
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
			//deal with FUNCTION_DEFINITION
			out.push_back(Parser::parseDefinition(tokenizedString[lineNum]));
		} else {
			for (unsigned long long tokenNum = 0; tokenNum < tokenizedString[lineNum].size(); tokenNum++) {
				ONLYDEBUG printf("PARSING %s\n", tokenizedString[lineNum][tokenNum].c_str());
				CharmFunction currentFunction;
				CharmFunctionType type = Parser::recognizeFunction(tokenizedString[lineNum][tokenNum]);
				if (type == DEFINED_FUNCTION) {
					//deal with DEFINED_FUNCTION first, easiest to deal with
					currentFunction = Parser::parseDefinedFunction(tokenizedString[lineNum][tokenNum]);
					//if we're doing inline optimizations, do them here:
					if (OPTIMIZE_INLINE) {
						bool couldInline = false;
						//search through the inline definitions that have been parsed to see if this function is inlineable
						for (CharmFunction possibleInlineF : inlineDefinitions) {
							if (currentFunction.functionName == possibleInlineF.functionName) {
								ONLYDEBUG printf("PERFORMING INLINE REPLACEMENT FOR %s\n    %s -> ", currentFunction.functionName.c_str(), currentFunction.functionName.c_str());
								for (unsigned long long inlineIndex = 0; inlineIndex < possibleInlineF.literalFunctions.size(); inlineIndex++) {
									out.push_back(possibleInlineF.literalFunctions[inlineIndex]);
									ONLYDEBUG printf("%s ", charmFunctionToString(possibleInlineF.literalFunctions[inlineIndex]).c_str());
								}
								ONLYDEBUG printf("\n");
								if (DEBUGMODE) {
									printf("AFTER INLINE OPTIMIZATION, OUT NOW LOOKS LIKE THIS:\n     ");
									for (CharmFunction f : out) {
										printf("%s ", charmFunctionToString(f).c_str());
									}
									printf("\n");
								}
								//break out of the inline function search once a matching func was found
								couldInline = true;
								break;
							}
						}
						if (couldInline) {
							//skip over the function's own out.push_back
							continue;
						}
					}
				} else if (type == NUMBER_FUNCTION) {
					//next deal with NUMBER_FUNCTION
					currentFunction = Parser::parseNumberFunction(tokenizedString[lineNum][tokenNum]);
				} else if (type == STRING_FUNCTION) {
					//next deal with STRING_FUNCTION
					currentFunction = Parser::parseStringFunction(&tokenizedString[lineNum], &tokenNum);
				} else if (type == LIST_FUNCTION) {
					//same thing as before, except it's a list
					currentFunction = Parser::parseListFunction(&tokenizedString[lineNum], &tokenNum);
				}
				out.push_back(currentFunction);
				if (DEBUGMODE) {
					printf("AFTER 1 TOKEN, OUT NOW LOOKS LIKE THIS:\n     ");
					for (CharmFunction f : out) {
						printf("%s ", charmFunctionToString(f).c_str());
					}
					printf("\n");
				}
			}
		}
	}
	//wow, we're finally done with this abomination of a function
	return out;
}
