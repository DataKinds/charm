#include <vector>
#include "ParserTypes.h"

class Parser {
private:
	std::vector<std::string> splitString(const std::string s, char delim);
	bool isCharDigit(char c);
	bool isStringNumber(std::string str);
	CharmFunctionType recognizeFunction(std::string s);
public:
	Parser();
	std::vector<CharmFunction> parse(std::string charmInput);
};
