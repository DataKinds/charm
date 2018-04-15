#pragma once

#include <unordered_map>
#include <functional>

class Runner;
// Returns an array of CharmFunctions to push,
// Takes in a variadic amount of CharmFunctions
//typedef std::function<CharmFunction*(CharmFunction ...)> PureFFI;

// Takes in a pointer to the Runner
typedef void (*MutateFFI)(Runner*);

class FFI {
private:
public:
    FFI();
    //    std::unordered_map<std::string, PureFFI> pureFFIFuncs;
    std::unordered_map<std::string, MutateFFI> mutateFFIFuncs;

//    void loadPureFFI(std::string charmName, std::string libPath, std::string sym);
    void loadMutateFFI(std::string charmName, std::string libPath, std::string sym);

    void runFFI(std::string f, Runner* r);
};
