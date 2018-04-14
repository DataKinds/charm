#pragma once

#include <unordered_map>
#include <string>

#include "ParserTypes.h"

class FunctionAnalyzer {
private:
    bool _isInlineable(std::string fName, CharmFunction f);
    std::unordered_map<std::string, CharmFunction> inlineDefinitions;
    std::unordered_map<std::string, CharmTypeSignature> typeSignatures;
public:
    FunctionAnalyzer();

    bool isInlinable(CharmFunction f);
    bool isTailCallRecursive(CharmFunction f);

    void addToInlineDefinitions(CharmFunction f);
    bool doInline(CHARM_LIST_TYPE& out, CharmFunction currentFunction);
};
