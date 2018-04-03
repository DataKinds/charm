
#include <string>
#include <stdexcept>
#include <fstream>
#include <sstream>
#include <deque>

#include <readline/readline.h>
#include <readline/history.h>

//sorry! this is just the easiest thing to do
#include "Prelude.charm.cpp"

#include "Parser.h"
#include "Runner.h"
#include "Debug.h"

int main(int argc, char const *argv[]) {
	Parser parser = Parser();
	Runner runner = Runner();

	//detect if the user passed in a file
	bool fileLoaded = false;
	std::string fileName;
	if (argc > 1) {
	  fileLoaded = true;
	  fileName = argv[1];
	}
	//first, we print fun info and load the prelude
	if (!fileLoaded) {
	  printf("Charm Interpreter v%s\n", "0.0.1");
	  printf("Made by @Aearnus\n");
	}
	try {
		//load up the Prelude.charm file
		runner.run(parser.lex(prelude));
		if (!fileLoaded) {
		  printf("Prelude.charm loaded.\n\n");
		}
	} catch (std::exception &e) {
		printf("Prelude.charm nonexistant or unopenable. This shouldn't happen!\n");
		printf("Error: %s\n\n", e.what());
	}

	//if theres a file to run, load it and run it
	if (fileLoaded) {
	  std::string line;
	  std::ifstream inFile(fileName);
	  while (std::getline(inFile, line)) {
	    runner.run(parser.lex(line));
	  }
	}
	//begin the interactive loop if there isnt a file to run
	while (true && (!fileLoaded)) {
		std::string codeInput(readline("Charm$ "));
		add_history(codeInput.c_str());
		std::vector<CharmFunction> parsedProgram = parser.lex(codeInput);
		ONLYDEBUG printf("TOKEN TYPES: ");
		for (auto currentFunction : parsedProgram) {
			ONLYDEBUG printf("%i ", currentFunction.functionType);
		}
		ONLYDEBUG printf("\n");
		try {
			runner.run(parsedProgram);
		} catch (const std::runtime_error& e) {
			printf("ERRROR: %s\n", e.what());
			//return -1;
		}
		ONLYDEBUG printf("MODIFIED STACK AREA: %i\n", runner.getCurrentStack()->getModifiedStackArea());
		ONLYDEBUG printf("THE STACK (just the types): ");
		std::deque<CharmFunction> postStack = runner.getCurrentStack()->stack;
		for (unsigned int stackIndex = runner.getCurrentStack()->getModifiedStackArea(); stackIndex > 0; stackIndex--) {
			ONLYDEBUG printf("%i ", postStack.at(postStack.size() - stackIndex).functionType);
		}
		ONLYDEBUG printf("\n");
		ONLYDEBUG printf("DEFINED FUNCTIONS: ");
		auto functionDefinitions = runner.getFunctionDefinitions();
		for (auto currentFunction : functionDefinitions) {
			ONLYDEBUG printf("%s ", currentFunction.functionName.c_str());
		}
		ONLYDEBUG printf("\n");
	}
	return 0;
}
