#include <charm/CharmFFI.hpp>
#include <iostream>

extern "C"
MutateFFI charmFFIHelloWorld(Runner* r) {
    CharmFunction f1 = r->getCurrentStack()->pop();
    std::cout << "Hello from C++!" << std::endl;
    std::cout << "This is what I saw on the top of the stack: " << charmFunctionToString(f1) << std::endl;
}
