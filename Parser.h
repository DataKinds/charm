#pragma once

#include <vector>
#include <deque>
#include <string>
#include <unordered_map>
#include <gmpxx.h>

#include "ParserTypes.h"
#include "FunctionAnalyzer.h"

class Token {
	struct TypeSignature {
		std::string functionName;
		std::vector<CharmTypeSignatureUnit> unit;
	};
	struct Definition {
		std::string functionName;
		std::vector<Token> definition;
	};
	struct List { std::vector<Token> list; };
	struct String { std::string string; };
	struct Number { mpf_class number; };
	struct Function { std::string function; };

	std::variant<
		TypeSignature,
		List,
		String,
		Number,
		Definition,
		Function> token;
}

class Parser {
private:
	std::optional<Token> consumeList();
	std::optional<Token> consumeTypeSignature();
	std::optional<Token> consumeString();
	std::optional<Token> consumeNumber();
	std::optional<Token> consumeFunction();

	bool skipLineBreak();

	Token consumeToken();
	std::vector<Lexeme> rest;
public:
	Parser(std::vector<Lexeme> rest);
	std::vector<Token> consumeAllTokens();
};
