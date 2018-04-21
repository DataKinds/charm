#include "Parser.h"
#include "Runner.h"

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
    
    void runCapsule(CInterpretationCapsule* c, char* in) {
        c->run(in);
        // TODO: return the print values
    }
};

