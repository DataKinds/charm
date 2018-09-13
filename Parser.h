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
	struct List { std::vector<Token> list; };
	struct String { std::string string; };
	struct Number { mpf_class number; };
	struct Function { std::string function; };

	std::variant<
		TypeSignature,
		List,
		String,
		Number,
		Function> token;
}

class Parser {
private:
	bool isCharDigit(char c);
	bool isStringNumber(std::string str);
	bool isLineFunctionDefinition(std::string line);
	CharmFunctionType recognizeFunction(std::string s);

	bool isLineTypeSignature(std::string line);
	CharmTypes tokenToType(std::string token);
	CharmTypeSignature parseTypeSignature(std::string line);
	CharmFunctionDefinitionInfo analyzeDefinition(CharmFunction f);

	FunctionAnalyzer fA;
	std::unordered_map<std::string, CharmFunctionDefinitionInfo> definitionInfoCache;

	bool advanceParse(std::string& token, std::string& rest);
	void delegateParsing(CHARM_LIST_TYPE& out, std::string& token, std::string& rest, bool willInline);

	CharmFunction parseDefinition(std::string line);
	CharmFunction parseDefinedFunction(std::string tok);
	CharmFunction parseNumberFunction(std::string tok);
	std::string escapeString(std::string tok);
	CharmFunction parseStringFunction(std::string& token, std::string& rest);
	CharmFunction parseListFunction(std::string& token, std::string& rest);

	std::vector<Lexeme> rest;
public:
	Parser(std::vector<Lexeme> rest);
	std::pair<CHARM_LIST_TYPE, FunctionAnalyzer*> lex(const std::string charmInput);
	std::pair<CHARM_LIST_TYPE, FunctionAnalyzer*> lexAskToInline(const std::string charmInput, bool willInline);
};
