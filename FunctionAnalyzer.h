#pragma once

#include "ParserTypes.h"

class FunctionAnalyzer {
private:
    CharmFunction function;
    bool _isInlineable(std::string fName, CharmFunction f);
public:
    FunctionAnalyzer(CharmFunction f);

    bool isInlinable();
    bool isTailCallRecursive();
};
