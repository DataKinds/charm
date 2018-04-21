#/bin/bash
make clean
#EMCC_DEBUG=1 ASSERTIONS=1 emmake make USE_READLINE=true INCLUDEDIR=-I../emlibs/ OUT_FILE=charm.html CPPFLAGS="-s ASSERTIONS=2 -s DISABLE_EXCEPTION_CATCHING=0 -g4" LDLIBS="../emlibs/libhistory.a ../emlibs/libreadline.a ../emlibs/libtermcap.a"
EMCC_DEBUG=1 ASSERTIONS=1 EMAR=emar emmake make emscripten-release USE_READLINE=false INCLUDEDIR=-I../emlibs/ OUT_FILE=docs/charm.js CPPFLAGS="-g4 -s ASSERTIONS=2 -s DISABLE_EXCEPTION_CATCHING=0 -s \"EXTRA_EXPORTED_RUNTIME_METHODS=['ccall', 'cwrap']\" -s \"EXPORTED_FUNCTIONS=['_initCapsule', '_runCapsule']\""
