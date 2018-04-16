#include <dlfcn.h>
#include <sstream>
#include <iostream>

#include "FFI.h"
#include "Error.h"
#include "Runner.h"
#include "Debug.h"

// TODO: MAKE THIS WORK ON MORE THAN JUST LINUX

FFI::FFI() {

}

void FFI::loadMutateFFI(std::string charmName, std::string libPath, std::string sym) {
    void* library = dlopen(libPath.c_str(), RTLD_NOW);
    if (library == nullptr) {
        std::ostringstream e;
        e << "FFI: Couldn't open library `" << libPath << "`" << std::endl;
        e << dlerror();
        runtime_die(e.str());
    }
    void* func = dlsym(library, sym.c_str());
    if (func == nullptr) {
        std::ostringstream e;
        e << "FFI: Couldn't find function `" << sym << "`" << std::endl;
        e << dlerror();
        runtime_die(e.str());
    }
    ONLYDEBUG printf("ADDING FUNCTION %s TO mutateFFIFuncs\n", charmName.c_str());
    mutateFFIFuncs[charmName] = reinterpret_cast<MutateFFI>(func);
    if (DEBUGMODE) {
        puts("mutateFFIFuncs now contains:");
        for (auto it : mutateFFIFuncs)
            std::cout << "    " << it.first << std::endl;
    }
}

void FFI::runFFI(std::string f, Runner* r) {
    ONLYDEBUG printf("LOOKING FOR FFI FUNCTION %s\n", f.c_str());
    auto func = mutateFFIFuncs.find(f);
    if (func == mutateFFIFuncs.end()) {
        std::ostringstream e;
        e << "FFI: Couldn't find supposedly loaded function `" << f << "`";
        runtime_die(e.str());
    }
    try {
        (func->second)(r);
    } catch (std::exception &e) {
        printf("FFI: Failed to run FFI function `%s`.\n", f.c_str());
        printf("Error: %s\n\n", e.what());
    }

}
