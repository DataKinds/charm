#include <vector>
#include "ParserTypes.h"

class Runner {
private:
	//the stack is automatically initialized to 8192 zero ints
	//pushing the stack above 8192 crashes the program
	//it's a feature, not a bug
	//no, really
	//there's a special error message and everything
	std::vector<CharmFunction> stack;
	const unsigned int MAX_STACK = 8192;
	//return a CharmFunction that for all intents and purposes is zero
	CharmFunction zeroF();
	//push to top of stack
	void push(CharmFunction f);
	//pop off top of stack
	CharmFunction pop();
	//swap values at index n1 and n2 from the top (zero-indexed)
	void swap(unsigned int n1, unsigned int n2);
	//handle the functions that we don't know about
	//and / or handle built in functions
	void handleDefinedFunctions(CharmFunction f);
public:
	Runner();
	void run(std::vector<CharmFunction> parsedProgram);
};
