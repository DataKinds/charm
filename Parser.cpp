#include <vector>
#include <sstream>
#include <utility>

#include "Parser.h"
#include "ParserTypes.h"
#include "Debug.h"
#include "FunctionAnalyzer.h"
#include "Error.h"


//thank u https://stackoverflow.com/questions/216823/whats-the-best-way-to-trim-stdstring?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
#include <algorithm>
#include <cctype>
#include <locale>

// trim from start (in place)
inline void Parser::ltrim(std::string &s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
        return !std::isspace(ch);
    }));
}

// trim from end (in place)
inline void Parser::rtrim(std::string &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
        return !std::isspace(ch);
    }).base(), s.end());
}

Parser::Parser() {
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

bool Parser::isLineFunctionDefinition(std::string line) {
	std::stringstream lineS(line);
	std::string f;
	while (std::getline(lineS, f, ' ')) {
		if (Parser::recognizeFunction(f) == FUNCTION_DEFINITION) {
			return true;
		}
	}
	return false;
}

bool Parser::isLineTypeSignature(std::string line) {
	std::stringstream lineS(line);
	std::string f;
	while (std::getline(lineS, f, ' ')) {
		if (f == "::") {
			return true;
		}
	}
	return false;
}

CharmTypes Parser::tokenToType(std::string token) {
    if (token == "any") {
        return TYPESIG_ANY;
    } else if (token == "list") {
        return TYPESIG_LIST;
    } else if (token == "list/string") {
        return TYPESIG_LISTSTRING;
    } else if (token == "string") {
        return TYPESIG_STRING;
    } else if (token == "int") {
        return TYPESIG_INT;
    } else if (token == "float") {
        return TYPESIG_FLOAT;
    } else {
        std::stringstream errorOut;
        errorOut << "Unrecognized type: " << token << std::endl;
        runtime_die(errorOut.str());
    }
}
CharmTypeSignature Parser::parseTypeSignature(std::string line) {
	CharmTypeSignature typeSignature;
	//this is called only if Parser::isLineFunctionDefinition was true, so that guarentees that
	//the string " := " is somewhere in this string
	auto colonIndex = line.find("::");
	typeSignature.functionName = line.substr(0, colonIndex);
	Parser::rtrim(typeSignature.functionName);
	Parser::ltrim(typeSignature.functionName);
	std::string typeStringRest = line.substr(colonIndex + 2);
    std::string typeStringToken;

    //first, parse the popped types
    while (Parser::advanceParse(typeStringToken, typeStringRest)) {
        if (typeStringToken == "") {
            continue;
        }
        if (typeStringToken == "->") {
            break;
        }
        typeSignature.pops.push_back(Parser::tokenToType(typeStringToken));
    }

    //then, parse the pushed types
    while (Parser::advanceParse(typeStringToken, typeStringRest)) {
        if (typeStringToken == "") {
            continue;
        }
        typeSignature.pushes.push_back(Parser::tokenToType(typeStringToken));
    }
	return typeSignature;
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
	//first, we fill in the info and see if the function is not recursive/inlineable
	out.inlineable = fA.isInlinable(f);
	//then we fill in the inlineDefinitions deque, for parsing future DEFINED_FUNCTIONs
	if (out.inlineable) {
		fA.addToInlineDefinitions(f);
	}
	out.tailCallRecursive = fA.isTailCallRecursive(f);
	return out;
}

CharmFunction Parser::parseDefinition(std::string line) {
	//if there was a function definition, do some weird stuff
	//set functionType to FUNCTION_DEFINITION (duh)
	//take the first token before the := and set it to the functionName
	//take all the tokens after the :=, parse them, and make them the literalFunctions
	CharmFunction currentFunction;
	currentFunction.functionType = FUNCTION_DEFINITION;
	std::pair<std::string, std::string> nameAndDef;

	//this is called only if Parser::isLineFunctionDefinition was true, so that guarentees that
	//the string " := " is somewhere in this string
	auto equalsIndex = line.find(":=");
	nameAndDef.first = line.substr(0, equalsIndex);
	Parser::rtrim(nameAndDef.first);
	Parser::ltrim(nameAndDef.first);
	nameAndDef.second = line.substr(equalsIndex + 2);
	//now we set the stuff!
	currentFunction.functionName = nameAndDef.first;
	ONLYDEBUG printf("FUNCTION IS NAMED %s\n", currentFunction.functionName.c_str());
	ONLYDEBUG printf("FUNCTION BODY IS %s\n", nameAndDef.second.c_str());
	currentFunction.literalFunctions = Parser::lex(nameAndDef.second).first;
	//we outta here!

	//then, we analyze the function before returning it
	CharmFunctionDefinitionInfo functionInfo = Parser::analyzeDefinition(currentFunction);
	currentFunction.definitionInfo = functionInfo;
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

CharmFunction Parser::parseStringFunction(std::string& token, std::string& rest) {
	CharmFunction out;
	out.functionType = STRING_FUNCTION;
    //a string continues until it hits a " \" " token
    std::stringstream outS;
    while (Parser::advanceParse(token, rest)) {
        if (token == "\"") {
            break;
        }
        outS << token << " ";
    }
    out.stringValue = outS.str();
    //if our string is non-empty, there will be a final space pushed to it that
    //we don't want. delete it here.
    if (out.stringValue.size() > 0) {
        out.stringValue.erase(std::prev(out.stringValue.end()));
    }
	//make sure that the final quote was removed if it exists
	//(AKA we're not at the end of the line)
	//FINALLY we can fill in out
	return out;
}

CharmFunction Parser::parseListFunction(std::string& token, std::string& rest) {
	CharmFunction out;
	out.functionType = LIST_FUNCTION;
	//and not a string. this time, we look for a "]"
	//to end the list (or a new line. that works too)
	//first, we have to make another string with the contents
	//this is just like the string
	std::stringstream outS;
	std::stringstream listS(rest);
	int listDepth = 1;
	std::string f;
	while (std::getline(listS, f, ' ')) {
		Parser::advanceParse(token, rest);
		ONLYDEBUG printf("LIST DEPTH %i\n", listDepth);
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
		}
		outS << token << " ";
	}
	//finally, we can put the inside of the [ ] into the out
	out.literalFunctions = Parser::lexAskToInline(outS.str(), false).first;
	return out;
}

void Parser::delegateParsing(CHARM_LIST_TYPE& out, std::string& token, std::string& rest, bool willInline) {
	ONLYDEBUG printf("DELEGATE PARSING %s\n", token.c_str());
	CharmFunction currentFunction;
	CharmFunctionType type = Parser::recognizeFunction(token);
	if (type == DEFINED_FUNCTION) {
		//deal with DEFINED_FUNCTION first, easiest to deal with
		currentFunction = Parser::parseDefinedFunction(token);
		//if we're doing inline optimizations, do them here:
		if (OPTIMIZE_INLINE && willInline) {
			ONLYDEBUG puts("WE ARE DOING INLINE DEFINITIONS");
			if (fA.doInline(out, currentFunction)) {
				//if the function was able to be inline optimized, skip the final push_back
				//this means that we don't push a duplicate currentFunction
				return;
			}
		}
	} else if (type == NUMBER_FUNCTION) {
		//next deal with NUMBER_FUNCTION
		currentFunction = Parser::parseNumberFunction(token);
	} else if (type == STRING_FUNCTION) {
		//next deal with STRING_FUNCTION
		currentFunction = Parser::parseStringFunction(token, rest);
	} else if (type == LIST_FUNCTION) {
		//same thing as before, except it's a list
		currentFunction = Parser::parseListFunction(token, rest);
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

bool Parser::advanceParse(std::string& token, std::string& rest) {
    if (rest == "") {
        return false;
    }
	auto nextSpace = rest.find_first_of(' ');
	if (nextSpace == std::string::npos) {
		token = rest;
		rest = "";
	} else {
		token = rest.substr(0, nextSpace);
		rest = rest.substr(nextSpace + 1);
	}
    return true;
}

std::pair<CHARM_LIST_TYPE, FunctionAnalyzer*> Parser::lexAskToInline(const std::string charmInput, bool willInline) {
	ONLYDEBUG printf("WILL PARSE %s\n", charmInput.c_str());
	CHARM_LIST_TYPE out;

	std::stringstream charmInputS(charmInput);
	std::string line;
	while (std::getline(charmInputS, line, '\n')) {
		//first, check and make sure that this line doesn't
		//contain a function definition before parsing it
		if (isLineFunctionDefinition(line)) {
			//deal with FUNCTION_DEFINITION
			out.push_back(Parser::parseDefinition(line));
		} else if (isLineTypeSignature(line)) {
            fA.addTypeSignature(Parser::parseTypeSignature(line));
        } else {
			std::string rest = line;
			std::string token;
			while (Parser::advanceParse(token, rest)) {
				if (token == "") {
					//if the token is empty bc multiple spaces
					continue;
				}
				delegateParsing(out, token, rest, willInline);
			}
		}
	}
	//wow, we're finally done with this abomination of a function
	std::pair<CHARM_LIST_TYPE, FunctionAnalyzer*> outPair(out, &fA);
	return outPair;
}
std::pair<CHARM_LIST_TYPE, FunctionAnalyzer*> Parser::lex(const std::string charmInput) {
	return Parser::lexAskToInline(charmInput, true);
}
