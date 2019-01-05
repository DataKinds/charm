#include <iostream>
#include <string>
#include <stdexcept>
#include <fstream>
#include <sstream>
#include <deque>
#include <optional>
#include <algorithm>
#include <string_view>
#include <functional>

#ifdef CHARM_GUI
	#include "gui.h"
#else
	#include <readline/readline.h>
	#include <readline/history.h>
#endif

#include "Prelude.charm.h"

#include "Lexer.h"
#include "Parser.h"
#include "Runner.h"
#include "Debug.h"

#define VERSION "1.0.0"

template<std::vector<std::string>* arg, std::string* flag, std::function<void()>* f>
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

template<std::vector<std::string>* arg, std::string* flag, std::optional<std::string>* var>
struct CommandLineOptional {
	//returns the success of parsing the argument
	static inline bool runArg() {
		auto iter = std::find(arg->begin(), arg->end(), *flag);
		if (iter != arg->end()) {
			if (
				iter == std::prev(arg->end()) ||
				std::next(iter)->front() == '-'
			) {
				std::cout << "No argument supplied to " << *flag << std::endl;
				return false;
			} else {
				//if the argument is properly formed
				(*var) = *(std::next(iter));
				arg->erase(iter, iter + 2);
			}
		}
		return true;
	}
};


int main(int argc, char const *argv[]) {
	//parse command line arguments
	static std::vector<std::string> args;
	args.assign(argv + 1, argv + argc);

	static std::string helpFlag("-h");
	static std::function<void()> helpF = []() {
		printf("Charm Interpreter v%s\n", VERSION);
		puts("By @Aearnus");
		puts("Usage:");
		puts("    charm [flags] [input file]");
		puts("Note:");
		puts("    Calling charm without an input file starts a REPL in most situations.");
		puts("Flags:");
		puts("    -h: Print this help message.");
		puts("    -v: Print the version.");
		puts("    -a <function name>: Analyze a function from the input file and print out information about it.");
		puts("    -f <file path>: Load up a file to be used interactively in the REPL.");
	};
	CommandLineLambda<&args, &helpFlag, &helpF> helpArg;
	if (helpArg.runArg()) {
		return 0;
	}

	static std::string versionFlag("-v");
	static std::function<void()> versionF = []() {
		printf("charm version %s, built on %s at %s.\n", VERSION, __DATE__, __TIME__);
	};
	CommandLineLambda<&args, &versionFlag, &versionF> versionArg;
	if (versionArg.runArg()) {
		return 0;
	}

	static std::optional<std::string> analyzeFunctionOpt;
	static std::string analyzeFunctionFlag("-a");
	CommandLineOptional<&args, &analyzeFunctionFlag, &analyzeFunctionOpt> analyzeFunctionArg;
	if (!analyzeFunctionArg.runArg()) {
		return -1;
	}

	static std::optional<std::string> interactiveFileOpt;
	static std::string interactiveFileFlag("-f");
	CommandLineOptional<&args, &interactiveFileFlag, &interactiveFileOpt> interactiveFileArg;
	if (!interactiveFileArg.runArg()) {
		return -1;
	}

	//initialize the runtime and load the prelude
	Runner runner = Runner("");
	try {
		//load up the Prelude.charm file
		Lexer lexer = Lexer(prelude);
		Parser parser = Parser(lexer.consumeAllLexemes());
		runner.run(parser.consumeAllTokens());
	} catch (std::exception &e) {
		printf("Prelude.charm nonexistant or unopenable. This shouldn't ever happen! Please report it to the charm devs.\n");
		printf("Error: %s\n\n", e.what());
		return -1;
	}
	//parse input file
	std::optional<std::string> optFileName;
	if (args.size() > 0) {
		optFileName = args.back();
	}
	//if theres a file to run, load it and run it
	if (optFileName) {
		try {
			//then load up the input file
			std::string line;
			std::ifstream inFile(*optFileName);
			while (std::getline(inFile, line)) {
				Lexer lexer = Lexer(line);
				Parser parser = Parser(lexer.consumeAllLexemes());
				runner.run(parser.consumeAllTokens());
			}
		} catch (std::exception &e) {
			printf("%s nonexistant or unopenable.\n", (*optFileName).c_str());
			printf("Error: %s\n", e.what());
			return -1;
		}
  	} else {
		printf("Charm Interpreter v%s\n", VERSION);
		printf("Made by @Aearnus\n");
		try {
			//if one was supplied, load up an extra interactive file
			if (interactiveFileOpt) {
				std::string line;
				std::ifstream interactiveFile(*interactiveFileOpt);
				while (std::getline(interactiveFile, line)) {
					Lexer lexer = Lexer(line);
					Parser parser = Parser(lexer.consumeAllLexemes());
					runner.run(parser.consumeAllTokens());
				}
				printf("%s loaded.\n", (*interactiveFileOpt).c_str());
			}
		} catch (std::exception &e) {
			printf("%s nonexistant or unopenable.\n", (*interactiveFileOpt).c_str());
			printf("Error: %s\n", e.what());
			return -1;
		}
#ifdef CHARM_GUI
		// start up the GUI if there isn't a file to run
		charm_gui_init(parser, runner);
#else
		//begin the interactive loop if there isnt a file to run
		while (true) {
			std::stringstream prompt;
			prompt << "Charm (Stack " << charmFunctionToString(runner.getCurrentStack().name) << ")$ ";
#if USE_READLINE == true
			std::string codeInput(readline(prompt.str().c_str()));
			add_history(codeInput.c_str());
#else
			std::string codeInput;
			std::cout << prompt.str();
			std::cin >> codeInput;
#endif
			try {
				Lexer lexer = Lexer(codeInput);
				Parser parser = Parser(lexer.consumeAllLexemes());
				auto parsedProgram = parser.consumeAllTokens();
				runner.run(parsedProgram);
			} catch (const std::runtime_error& e) {
				//don't do anything, the parsetime_die and runtime_die macros will always spit out messages.
				//printf("ERROR: %s\n", e.what());
				//return -1;
			}
		}
#endif
	}
	return 0;
}
