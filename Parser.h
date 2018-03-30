#include <vector>
#include "ParserTypes.h"

class Parser {
private:
	std::vector<std::string> splitString(const std::string s, char delim);
	bool isCharDigit(char c);
	bool isStringNumber(std::string str);
	bool isLineFunctionDefinition(std::vector<std::string> line);
	CharmFunctionType recognizeFunction(std::string s);

	bool _analyzeIsFunctionInlineable(std::string fName, CharmFunction f);
	bool analyzeIsFunctionInlineable(CharmFunction f);
	bool analyzeIsFunctionTailCallRecursive(CharmFunction f);
	void analyzeDefinition(CharmFunction *f);

	CharmFunction parseDefinition(std::vector<std::string> line);
	CharmFunction parseDefinedFunction(std::string tok);
	CharmFunction parseNumberFunction(std::string tok);
	CharmFunction parseStringFunction(std::vector<std::string> *line, unsigned long long *tokenNum);
	CharmFunction parseListFunction(std::vector<std::string> *line, unsigned long long *tokenNum);

public:
	Parser();
	std::vector<CharmFunction> lex(std::string charmInput);
};
