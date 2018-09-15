#pragma once

#include <vector>
#include <deque>
#include <string>
#include <unordered_map>
#include <gmpxx.h>

#include "ParserTypes.h"
#include "FunctionAnalyzer.h"
#include "Types.h"


class Parser {
private:
	std::optional<Token> consumeList();
	std::optional<Token> consumeTypeSignature();
	std::optional<Token> consumeDefinition();
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
