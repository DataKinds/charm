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
	printf("\n");
	runner.run(parsedProgram);
	printf("MODIFIED STACK AREA: %i\n", runner.getModifiedStackArea());
	printf("THE STACK (just the types):\n");
	std::vector<CharmFunction> postStack = runner.getStack();
	for (unsigned int stackIndex = 0; stackIndex < runner.getModifiedStackArea(); stackIndex++) {
		printf("%i ", postStack.at(postStack.size() - stackIndex - 1).functionType);
	}
	printf("\n");
	return 0;
}
