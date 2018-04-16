#include "charm/CharmFFI.h"
#include <iostream>

extern "C"
MutateFFI charmFFIHelloWorld(Runner* r) {
    std::cout << "Hello from C++!" << std::endl;
}
