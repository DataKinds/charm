#include "Parser.h"
#include "Runner.h"

#include <iostream>
#include <sstream>

extern "C" {
    class CInterpretationCapsule {
    private:
        Parser* p;
        Runner* r;
    public:
        CInterpretationCapsule() {
            p = new Parser();
            r = new Runner();
        };
        void run(char* in) {
            std::string inS(in);
            r->run(p->lex(inS));
        };
    };
    
    CInterpretationCapsule* initCapsule() {
        return new CInterpretationCapsule();
    }
    
    const char* runCapsule(CInterpretationCapsule* c, char* in) {
        //redirect cout to our stringstream
        std::streambuf* oldCout = std::cout.rdbuf();
        std::ostringstream newCout;
        std::cout.rdbuf(newCout.rdbuf());
        //then run
        c->run(in);
        //restore cout
        std::cout.rdbuf(oldCout);
        //and return the output
        return newCout.str().c_str();
    }
};

