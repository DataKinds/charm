#include <string>

#include <readline/readline.h>
#include <readline/history.h>

#include "Parser.h"
#include "Runner.h"

int main(int argc, char const *argv[]) {
	Parser parser = Parser();
	Runner runner = Runner();
	std::string codeInput(readline("Charm$ "));
	std::vector<CharmFunction> parsedProgram = parser.parse(codeInput);
	printf("TOKEN TYPES: ");
	for (auto currentFunction : parsedProgram) {
		printf("%i ", currentFunction.functionType);
	}
	runner.run(parsedProgram);
	
	printf("\n");
	return 0;
}
