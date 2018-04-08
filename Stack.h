#pragma once
#include "ParserTypes.h"
#include <deque>

class Stack {
private:
    CharmFunction name;
	//says how much of the stack was changed, for printing n stuff
	unsigned long long modifiedStackArea;
public:
    Stack(unsigned long long size, CharmFunction name);
    //the stack is automatically initialized to MAX_INT zero ints
    CHARM_STACK_TYPE stack;
    //check to see if the stack name is equal
    //to some CharmFunction passed in. this is so
    //runner can properly select its current stack
    bool isNameEqualTo(CharmFunction f);
    //update the modifiedStackArea, really only called on swap
    //because swap is the only one that's hard to predict
	void updateModifiedStackArea();
    //a helper function to see if a charm function is a number / an int
    static bool isInt(CharmFunction f);
    static bool isFloat(CharmFunction f);
    //return a CharmFunction that for all intents and purposes is zero
    static CharmFunction zeroF();
    //push to top of stack
    void push(CharmFunction f);
    //pop off top of stack
    CharmFunction pop();
    //swap values at index n1 and n2 from the top (zero-indexed)
    void swap(unsigned long long n1, unsigned long long n2);
    unsigned int getModifiedStackArea();
};
