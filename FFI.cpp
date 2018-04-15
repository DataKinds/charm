#include <dlfcn.h>
#include <sstream>

#include "FFI.h"
#include "Error.h"
#include "Runner.h"

// TODO: MAKE THIS WORK ON MORE THAN JUST LINUX

FFI::FFI() {

}

void FFI::loadMutateFFI(std::string charmName, std::string libPath, std::string sym) {
    void* library = dlopen(libPath.c_str(), RTLD_NOW);
    if (library == nullptr) {
        std::ostringstream e;
        e << "FFI: Couldn't find library " << libPath;
        runtime_die(e.str());
    }
    void* func = dlsym(library, sym.c_str());
    if (func == nullptr) {
        std::ostringstream e;
        e << "FFI: Couldn't find function " << sym;
        runtime_die(e.str());
    }
    mutateFFIFuncs[sym] = reinterpret_cast<MutateFFI>(func);
}

void FFI::runFFI(std::string f, Runner* r) {
    auto func = mutateFFIFuncs.find(f);
    if (func == mutateFFIFuncs.end()) {
        std::ostringstream e;
        e << "FFI: Couldn't find supposedly loaded function " << f;
        runtime_die(e.str());
    }
    try {
        (func->second)(r);
    } catch (std::exception &e) {
        printf("FFI: Failed to run FFI function %s.\n", f.c_str());
        printf("Error: %s\n\n", e.what());
    }

}
