#pragma once

#include <vector>
#include <deque>

#include "ParserTypes.h"
#include "FunctionAnalyzer.h"

class Parser {
private:
	static inline void ltrim(std::string &s);
	static inline void rtrim(std::string &s);

	bool isCharDigit(char c);
	bool isStringNumber(std::string str);
	bool isLineFunctionDefinition(std::string line);
	CharmFunctionType recognizeFunction(std::string s);

	bool isLineTypeSignature(std::string line);
	CharmTypes tokenToType(std::string token);
	CharmTypeSignature parseTypeSignature(std::string line, std::string ns);
	CharmFunctionDefinitionInfo analyzeDefinition(CharmFunction f);

	FunctionAnalyzer fA;

	bool advanceParse(std::string& token, std::string& rest);
	void delegateParsing(CHARM_LIST_TYPE& out, std::string& token, std::string& rest, bool willInline, const std::string ns);

	CharmFunction parseDefinition(std::string line, std::string ns);
	CharmFunction parseDefinedFunction(std::string tok, std::string ns);
	CharmFunction parseNumberFunction(std::string tok);
	std::string escapeString(std::string tok);
	CharmFunction parseStringFunction(std::string& token, std::string& rest);
	CharmFunction parseListFunction(std::string& token, std::string& rest, std::string ns);
public:
	Parser();
	std::pair<CHARM_LIST_TYPE, FunctionAnalyzer*> lex(const std::string charmInput, bool willInline = true, const std::string ns = "");
};
