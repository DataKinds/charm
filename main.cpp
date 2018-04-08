
#include <string>
#include <stdexcept>
#include <fstream>
#include <sstream>
#include <deque>
#include <optional>
#include <algorithm>
#include <string_view>
#include <functional>

#include <readline/readline.h>
#include <readline/history.h>

//sorry! this is just the easiest thing to do
#include "Prelude.charm.cpp"

#include "Parser.h"
#include "Runner.h"
#include "Debug.h"

const std::string VERSION = "0.0.1";

template<std::vector<std::string>* arg, std::string_view* flag, std::function<void()>* f>
struct CommandLineLambda {
	//returns whether or not the argument was called
	static inline bool runArg() {
		auto iter = std::find(arg->begin(), arg->end(), *flag);
		if (iter != arg->end()) {
			(*f)();
			return true;
		}
		return false;
	}
};

template<std::vector<std::string>* arg, std::string_view* flag, std::optional<std::string>* var>
struct CommandLineOptional {
	//returns the success of parsing the argument
	static inline bool runArg() {
		auto iter = std::find(arg->begin(), arg->end(), *flag);
		if (iter != arg->end()) {
			if (
				iter == std::prev(arg->end()) ||
				std::next(iter)->front() == '-'
			) {
				printf("No argument supplied to %s\n", "-a");
				return false;
			} else {
				(*var) = *(std::next(iter));
			}
		}
		return true;
	}
};


int main(int argc, char const *argv[]) {
	Parser parser = Parser();
	Runner runner = Runner();

	//parse command line arguments
	static std::vector<std::string> args;
	args.assign(argv + 1, argv + argc);

	static std::string_view helpFlag("-h");
	static std::function<void()> helpF = []() {
		printf("Charm Interpreter v%s\n", VERSION.c_str());
		puts("By @Aearnus");
		puts("Usage:");
		puts("    charm [flags] [input file]");
		puts("Note:");
		puts("    Calling charm without an input file starts a REPL in most situations.");
		puts("Flags:");
		puts("    -h: Print this help message.");
		puts("    -v: Print the version.");
		puts("    -a <function name>: Analyze a function from the input file and print out information about it.");
	};
	CommandLineLambda<&args, &helpFlag, &helpF> helpArg;
	if (helpArg.runArg()) {
		return 0;
	}

	static std::string_view versionFlag("-v");
	static std::function<void()> versionF = []() {
		printf("charm version %s, built on %s at %s.\n", VERSION.c_str(), __DATE__, __TIME__);
	};
	CommandLineLambda<&args, &versionFlag, &versionF> versionArg;
	if (versionArg.runArg()) {
		return 0;
	}

	static std::optional<std::string> analyzeFunctionOpt;
	static std::string_view analyzeFunctionFlag("-a");
	CommandLineOptional<&args, &analyzeFunctionFlag, &analyzeFunctionOpt> analyzeFunctionArg;
	if (!analyzeFunctionArg.runArg()) {
		return -1;
	}

	//parse input file
	std::optional<std::string> optFileName;
	if (args.size() > 1) {
		optFileName = args.back();
	}

	//if theres a file to run, load it and run it
	if (optFileName) {
		//first, load the prelude
		try {
			//load up the Prelude.charm file
			runner.run(parser.lex(prelude));
		} catch (std::exception &e) {
			printf("Prelude.charm nonexistant or unopenable. This shouldn't ever happen! Please report it to the charm devs.\n");
			printf("Error: %s\n\n", e.what());
		}
		std::string line;
		std::ifstream inFile(*optFileName);
		while (std::getline(inFile, line)) {
			runner.run(parser.lex(line));
		}
  	} else {
		printf("Charm Interpreter v%s\n", VERSION.c_str());
		printf("Made by @Aearnus\n");
		//first, load the prelude
		try {
			//load up the Prelude.charm file
			runner.run(parser.lex(prelude));
			printf("Prelude.charm loaded.\n\n");
		} catch (std::exception &e) {
			printf("Prelude.charm nonexistant or unopenable. This shouldn't ever happen! Please report it to the charm devs.\n");
			printf("Error: %s\n\n", e.what());
		}
		//begin the interactive loop if there isnt a file to run
		while (true) {
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
	}
	return 0;
}
