#/bin/bash
make clean
#working line:
#EMCC_DEBUG=1 ASSERTIONS=1 EMAR=emar emmake make emscripten-release USE_READLINE=false INCLUDEDIR=-I../emlibs/ OUT_FILE=docs/charm.js CPPFLAGS="-g4 -s ASSERTIONS=2 -s DISABLE_EXCEPTION_CATCHING=0 -s LINKABLE=1 -s \"EXTRA_EXPORTED_RUNTIME_METHODS=['ccall', 'cwrap']\" -s \"EXPORTED_FUNCTIONS=['_initCapsule', '_runCapsule']\""
EMCC_DEBUG=1 ASSERTIONS=1 EMAR=emar emmake make emscripten-release DEBUG=true USE_READLINE=false INCLUDEDIR=-I../emlibs/ OUT_FILE=docs/charm.js CPPFLAGS="-g4 -s ASSERTIONS=2 -s DISABLE_EXCEPTION_CATCHING=0 -s LINKABLE=1 -s \"EXTRA_EXPORTED_RUNTIME_METHODS=['ccall', 'cwrap']\" -s \"EXPORTED_FUNCTIONS=['_initCapsule', '_runCapsule']\""
