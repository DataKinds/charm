#include <string>
#include <stdexcept>

#include <readline/readline.h>
#include <readline/history.h>

#include "Parser.h"
#include "Runner.h"

int main(int argc, char const *argv[]) {
	Parser parser = Parser();
	Runner runner = Runner();
	//begin the interactive loop
	while (true) {
		std::string codeInput(readline("Charm$ "));
		std::vector<CharmFunction> parsedProgram = parser.parse(codeInput);
		printf("TOKEN TYPES: ");
		for (auto currentFunction : parsedProgram) {
			printf("%i ", currentFunction.functionType);
		}
		printf("\n");
		try {
			runner.run(parsedProgram);
		} catch (const std::runtime_error& e) {
			printf("ERRROR: %s\n", e.what());
			return -1;
		}
		printf("MODIFIED STACK AREA: %i\n", runner.getModifiedStackArea());
		printf("THE STACK (just the types): ");
		std::vector<CharmFunction> postStack = runner.getStack();
		for (unsigned int stackIndex = runner.getModifiedStackArea(); stackIndex > 0; stackIndex--) {
			printf("%i ", postStack.at(postStack.size() - stackIndex).functionType);
		}
		printf("\n");
		printf("DEFINED FUNCTIONS: ");
		auto functionDefinitions = runner.getFunctionDefinitions();
		for (auto currentFunction : functionDefinitions) {
			printf("%s ", currentFunction.functionName.c_str());
		}
		printf("\n");
	}
	return 0;
}
