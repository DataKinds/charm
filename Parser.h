#pragma once

#include <vector>
#include <deque>

#include "ParserTypes.h"
#include "FunctionAnalyzer.h"

class Parser {
private:
	std::vector<std::string> splitString(const std::string s, char delim);
	bool isCharDigit(char c);
	bool isStringNumber(std::string str);
	bool isLineFunctionDefinition(std::vector<std::string> line);
	CharmFunctionType recognizeFunction(std::string s);

	CharmFunctionDefinitionInfo analyzeDefinition(CharmFunction f);

	FunctionAnalyzer fA;

	CharmFunction parseDefinition(std::vector<std::string> line);
	CharmFunction parseDefinedFunction(std::string tok);
	CharmFunction parseNumberFunction(std::string tok);
	CharmFunction parseStringFunction(std::vector<std::string> *line, unsigned long long *tokenNum);
	CharmFunction parseListFunction(std::vector<std::string> *line, unsigned long long *tokenNum);

public:
	Parser();
	std::pair<CHARM_LIST_TYPE, FunctionAnalyzer*> lex(std::string charmInput);
};
