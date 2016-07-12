#include <vector>
#include "ParserTypes.h"

struct FunctionDefinition {
	std::string functionName;
	std::vector<CharmFunction> functionBody;
};

class Runner {
private:
	//the stack is automatically initialized to 8192 zero ints
	std::vector<CharmFunction> stack;
	const unsigned int MAX_STACK = 8192;
	//says how much of the stack was changed, for printing n stuff
	unsigned int modifiedStackArea;
	//update the modifiedStackArea, really only called on swap
	//because swap is the only one that's hard to predict
	void updateModifiedStackArea();
	//alright, this is the nitty gritty
	//here is the table of function definitions:
	std::vector<FunctionDefinition> functionDefinitions;
	//and this is how you add them
	void addFunctionDefinition(FunctionDefinition fD);
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
	std::vector<CharmFunction> getStack();
	unsigned int getModifiedStackArea();
	void run(std::vector<CharmFunction> parsedProgram);
};
