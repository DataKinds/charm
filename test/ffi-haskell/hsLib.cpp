#include <HsFFI.h>
#include "doubleLib_stub.h"
extern "C" {
extern void __stginit_Doubler(void);
}
#include <charm/CharmFFI.hpp>
#include <iostream>

extern "C"
MutateFFI haskellInit(Runner* r) {
    hs_init(0, nullptr);
    hs_add_root(__stginit_Doubler);
}

extern "C"
MutateFFI haskellExit(Runner* r) {
    hs_exit();
}

extern "C"
MutateFFI haskellDoubler(Runner* r) {
    CharmFunction f1 = r->getCurrentStack()->pop();
    if (f1.functionType == NUMBER_FUNCTION && f1.numberValue.whichType == INTEGER_VALUE) {
        std::cout << "Running doubleLongLong_hs with argument " << f1.numberValue.integerValue << std::endl;
        f1.numberValue.integerValue = doubleLongLong_hs(f1.numberValue.integerValue);
        r->getCurrentStack()->push(f1);
    } else {
        std::cout << "Non integer passed to FFI function Haskell Doubler\n";
    }
}
