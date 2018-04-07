#include "FunctionAnalyzer.h"
#include "ParserTypes.h"

FunctionAnalyzer::FunctionAnalyzer(CharmFunction f) {
    function = f;
}


bool FunctionAnalyzer::_isInlineable(std::string fName, CharmFunction f) {
	//if the function calls itself, it's recursive and not inlineable
	bool recursive = false;
	for (unsigned long long fIndex = 0; fIndex < f.literalFunctions.size(); fIndex++) {
		if (f.literalFunctions[fIndex].functionType == LIST_FUNCTION) {
			recursive = recursive || (!_isInlineable(fName, f.literalFunctions[fIndex]));
		} else {
			recursive = recursive || (fName == f.literalFunctions[fIndex].functionName);
		}
		if (recursive) {
			break;
		}
	}
	return !recursive;
}
bool FunctionAnalyzer::isInlinable() {
	return (_isInlineable(function.functionName, function));
}

bool FunctionAnalyzer::isTailCallRecursive() {
	//this is only static, basic tail call recursion analysis.
	//this only catches functions of form `f := <code> f`
	//and sends this flag off to Runner.cpp::handleDefinedFunctions()
	//the rest of the tail call recursion code happens within PredefinedFunctions.cpp::ifthen
	//over there, the usual case is caught, with code of the form `f := [ <cond> ] [ <code> f] [ <code> f ] ifthen`
    if (function.literalFunctions.size() > 0) {
       return (function.functionName == function.literalFunctions.back().functionName);
    } else {
        return false;
    }
}
