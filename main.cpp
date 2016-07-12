#include <string>

#include <readline/readline.h>
#include <readline/history.h>

#include "Parser.h"

int main(int argc, char const *argv[]) {
	Parser parser = Parser();
	std::string codeInput(readline("Charm$ "));
	std::vector<CharmFunction> parsedProgram = parser.parse(codeInput);
	for (auto currentFunction : parsedProgram) {
		printf("%i ", currentFunction.functionType);
	}
	printf("\n");
	return 0;
}
