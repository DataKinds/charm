#include "Parser.h"
#include "Runner.h"
#include "Prelude.charm.h"

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
            this->run(prelude.c_str());
        };
        void run(const char* in) {
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
        try {
            c->run(in);
        } catch (std::exception& e) {
            std::cout << e.what();
        }
        //restore cout
        std::cout.rdbuf(oldCout);
        //and return the output
        return newCout.str().c_str();
    }
};

