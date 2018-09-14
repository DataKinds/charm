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
	if (this->rest.size() < 3) {
		return std::nullopt;
	}
	// TODO: actual type signature parser
	bool isTypeSignature = false;
	for (auto& lexeme : this->rest) {
		isTypeSignature = isTypeSignature || std::holds_alternative<Lexeme::SingleColon>(lexeme);
	}

	if (isTypeSignature) {
		// TODO: just consume until the end of the line for now
		while (this->rest.size() != 0) {
			if (std::holds_alternative<Lexeme::LineBreak>(this->rest.at(0))) break;
			this->rest.erase(this->rest.begin());
		}
	} else {
		return std::nullopt;
	}
	Token token;
	token.token = (struct Parser::TypeSignature){
		.functionName = "",
		.unit = { }
	};
	return token;
}
std::optional<Token> Parser::consumeDefinition() {
	// a definition has to be at LEAST
	// <function name> := <definition>
	if (this->rest.size() < 3) {
		return std::nullopt;
	}
	if (!std::holds_alternative<Lexeme::Ident>(this->rest.at(0))
		|| !std::holds_alternative<Lexeme::ColonEqual>(this->rest.at(1))) {
		return std::nullopt;
	}
	Token token;
	token.token = (struct Parser::Definition){
		.functionName = this->rest.at(0).function,
		.definition = { }
	};
	// erase the function name and the :=
	this->rest.erase(this->rest.begin());
	this->rest.erase(this->rest.begin());
	// then consume the definition
	auto definitionEnd = this->rest.begin();
	//TODO
	//TODO
	//TODO
	while (this->rest.size() != 0) {
		// TODO: this is _so_ ineffecient, we should
		// be using consumeAllTokens with a sublist instead
		Parser parser = Parser({ this->rest.at(0) });
		token.token.definition.push_back(parser.consumeAllTokens());
		this->rest.erase(this->rest.begin());
	}
	return token;
}
std::optional<Token> Parser::consumeString() {
	if (std::holds_alternative<Lexeme::String>(this->rest.at(0))) {
		Token token;
		token.token = (struct Parser::String){ .string = this->rest.at(0).string };
		this->rest.erase(this->rest.begin());
		return token;
	} else {
		return std::nullopt;
	}
}
std::optional<Token> Parser::consumeNumber() {
	if (std::holds_alternative<Lexeme::Number>(this->rest.at(0))) {
		Token token;
		token.token = (struct Parser::Number){ .number = mpf_class(this->rest.at(0).number.c_str()) };
		this->rest.erase(this->rest.begin());
		return token;
	} else {
		return std::nullopt;
	}
}
std::optional<Token> Parser::consumeFunction() {
	if (std::holds_alternative<Lexeme::Ident>(this->rest.at(0))) {
		Token token;
		token.token = (struct Parser::Function){ .function = this->rest.at(0).function };
		this->rest.erase(this->rest.begin());
		return token;
	} else {
		return std::nullopt;
	}
}
bool Parser::skipLineBreak() {
	if (std::holds_alternative<Lexeme::LineBreak>(this->rest.at(0))) {
		this->rest.erase(this->rest.begin());
		return true;
	} else {
		return false;
	}
}
Token Parser::consumeToken() {
	std::vector<Lexeme> backtrack = this->rest;
	Token token;
	// all of these require backtracking
	// (this is as a safety precaution)
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
		// discard extra whitespace
		while (Parser::skipLineBreak()) {}
		out.push_back(Lexer::consumeLexeme());
	}
}
