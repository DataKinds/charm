#include <vector>
#include <deque>
#include "ParserTypes.h"

class Parser {
private:
	std::vector<std::string> splitString(const std::string s, char delim);
	bool isCharDigit(char c);
	bool isStringNumber(std::string str);
	bool isLineFunctionDefinition(std::vector<std::string> line);
	CharmFunctionType recognizeFunction(std::string s);

	CharmFunctionDefinitionInfo analyzeDefinition(CharmFunction f);

	std::deque<CharmFunction> inlineDefinitions;

	CharmFunction parseDefinition(std::vector<std::string> line);
	CharmFunction parseDefinedFunction(std::string tok);
	CharmFunction parseNumberFunction(std::string tok);
	CharmFunction parseStringFunction(std::vector<std::string> *line, unsigned long long *tokenNum);
	CharmFunction parseListFunction(std::vector<std::string> *line, unsigned long long *tokenNum);

public:
	Parser();
	CHARM_LIST_TYPE lex(std::string charmInput);
};
