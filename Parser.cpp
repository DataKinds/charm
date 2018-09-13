#include <vector>
#include <sstream>
#include <utility>

#include "Parser.h"
#include "ParserTypes.h"
#include "Debug.h"
#include "FunctionAnalyzer.h"
#include "Error.h"
#include "Lexer.h"

#include <algorithm>
#include <cctype>
#include <locale>
#include <optional>
#include <variant>

Parser::Parser(std::vector<Lexeme> rest) {
	this->rest = rest;
}

bool Parser::isLineFunctionDefinition(std::string line) {
	std::stringstream lineS(line);
	std::string f;
    bool stringDepth = false;
    int listDepth = 0;
	while (std::getline(lineS, f, ' ')) {
        if (Parser::recognizeFunction(f) == LIST_FUNCTION) {
            listDepth++;
        } else if (f == "]") {
            listDepth--;
        } else if (Parser::recognizeFunction(f) == STRING_FUNCTION) {
            stringDepth = !stringDepth;
        }
        if (listDepth == 0 && !stringDepth) {
            if (Parser::recognizeFunction(f) == FUNCTION_DEFINITION) {
                return true;
            }
        }
	}
	return false;
}

bool Parser::isLineTypeSignature(std::string line) {
	std::stringstream lineS(line);
	std::string f;
    bool stringDepth = false;
    int listDepth = 0;
	while (std::getline(lineS, f, ' ')) {
        if (Parser::recognizeFunction(f) == LIST_FUNCTION) {
            listDepth++;
        } else if (f == "]") {
            listDepth--;
        } else if (Parser::recognizeFunction(f) == STRING_FUNCTION) {
            stringDepth = !stringDepth;
        }
        if (listDepth == 0 && !stringDepth) {
            if (f == "::") {
                return true;
            }
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
        parsetime_die(errorOut.str());
    }
    return TYPESIG_ANY;
}
CharmTypeSignature Parser::parseTypeSignature(std::string line) {
	CharmTypeSignature typeSignature;
	//this is called only if Parser::isLineFunctionDefinition was true, so that guarentees that
	//the string " := " is somewhere in this string
	auto colonIndex = line.find("::");
	typeSignature.functionName = line.substr(0, colonIndex);
	Parser::rtrim(typeSignature.functionName);
	Parser::ltrim(typeSignature.functionName);

    ParseUnit:
    CharmTypeSignatureUnit unit;
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
        if (typeStringToken == "|") {
            //this is only valid after an entire type signature has been specified.
            //thus, using it before a -> is invalid
            parsetime_die("Type alternative specified before completion of type.");
        }
        unit.pops.push_back(Parser::tokenToType(typeStringToken));
    }

    //then, parse the pushed types
    while (Parser::advanceParse(typeStringToken, typeStringRest)) {
        if (typeStringToken == "") {
            continue;
        }
        if (typeStringToken == "|") {
            Parser::advanceParse(typeStringToken, typeStringRest);
            typeSignature.units.push_back(unit);
            goto ParseUnit;
        }
        unit.pushes.push_back(Parser::tokenToType(typeStringToken));
    }
    typeSignature.units.push_back(unit);
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
	//then we fill in the inlineDefinitions deque (ignoring type signatures), for parsing future DEFINED_FUNCTIONs or for using the `inline` function
	if (fA.isInlinableIgnoringTypeSignature(f)) {
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
    definitionInfoCache[currentFunction.functionName] = functionInfo;
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
        //TODO: FIND AN EASIER WAY TO SPECIFY FLOAT PRECISION
		numberValue.floatValue = mpf_class(tok.c_str());
	} else {
		numberValue.whichType = INTEGER_VALUE;
		numberValue.integerValue = mpz_class(tok.c_str());
	}
	out.numberValue = numberValue;
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
    if (listDepth > 0) {
        parsetime_die("Expected a close bracket before the end of the line. Perhaps you missed a space?");
    }
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
            // only do inlining if the function says we can -- not just if it's possible
            // many functions _aren't_ inlineable because they have type signatures, but
            // they still have inlineDefinition's (in order to be able to use `inline`)
            auto defInfo = definitionInfoCache.find(currentFunction.functionName);
            if (defInfo != definitionInfoCache.end() && defInfo->second.inlineable) {
                ONLYDEBUG printf("YES, %s IS INLINEABLE SO WE'RE DOING IT\n", currentFunction.functionName.c_str());
    			if (fA.doInline(out, currentFunction)) {
    				//if the function was able to be inline optimized, skip the final push_back
    				//this means that we don't push a duplicate currentFunction
    				return;
    			}
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


CHARM_LIST_TYPE Parser::lexAskToInline(const std::string charmInput, bool willInline) {
/*
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
*/
}

std::optional<Token> Parser::consumeList() {
	// ensure the first Lexeme is an open bracket
	if (!std::holds_alternative<Lexeme::OpenBracket>(this->rest.at(0))) {
		return std::nullopt;
	}
	// consume that first open bracket
	this->rest.erase(this->rest.begin())
	// if it is an open bracket, we consume to
	// the end of the list, collecting lexemes as we go
	std::vector<Lexeme> descent;
	int depth = 1;
	while (depth) {
		if (this->rest.size() == 0) {
			parsetime_die("Unclosed list");
		}
		std::visit(overloaded {
			[](Lexeme::OpenBracket arg) {
				depth++;
			},
			[](Lexeme::CloseBracket arg) {
				depth--;
			},
			[](auto arg) {}
		}, this->rest.begin());
		if (!depth) break;
		descent.push_back(this->rest.begin());
		this->rest.erase(this->rest.begin());
	}
	// erase the closing bracket
	this->rest.erase(this->rest.begin());
	// then finally prepare the token for returning
	Token token;
	Parser descentParser = Parser(descent);
	token.token = (struct Parser::List){ .list = descentParser.consumeAllTokens() };
	return token;
}
std::optional<Token> Parser::consumeTypeSignature() {
	// TODO: actual type signature parser
	bool isTypeSignature = false;
	for (auto& lexeme : this->rest) {
		isTypeSignature = isTypeSignature || std::holds_alternative<Lexeme::SingleColon>(lexeme);
	}
	if (isTypeSignature) {
		// TODO: just consume until the end of the line for now
	}
}
std::optional<Token> Parser::consumeString() {
	// TODO: stub
}
std::optional<Token> Parser::consumeNumber() {
	// TODO: stub
}
std::optional<Token> Parser::consumeFunction() {
	// TODO: stub
}
Token Parser::consumeToken() {
	std::vector<Lexeme> backtrack = this->rest;
	Token token;
	// all of these require backtracking
	if (token = Parser::consumeList()) {
		goto success;
	}
	if (token = Parser::consumeTypeSignature()) {
		goto success;
	}
	if (token = Parser::consumeString()) {
		goto success;
	}
	if (token = Parser::consumeNumber()) {
		goto success;
	}
	if (token = Parser::consumeFunction()) {
		goto success;
	}
	parsetime_die("BBBBB");
	success:
	return token;
}

std::vector<Token> Parser::consumeAllTokens() {
	std::vector<Token> out;
	while (this->rest.size() > 0) {
		// TODO: stub
	}
}
