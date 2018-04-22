#include <charm/CharmFFI.h>
#include <iostream>

extern "C"
MutateFFI charmFFIHelloWorld(Runner* r) {
    CharmFunction f1 = r->getCurrentStack()->pop();
    std::cout << "Hello from C++!" << std::endl;
}
