#/bin/bash
make clean
EMCC_DEBUG=1 ASSERTIONS=1 emmake make INCLUDEDIR=-I/home/tyler/packages/emscriptenlibs LIBDIR=-L/home/tyler/packages/emscriptenlibs OUT_FILE=charm.html CPPFLAGS="-s ASSERTIONS=2 -g4" LDLIBS="/home/tyler/packages/emscriptenlibs/libhistory.a /home/tyler/packages/emscriptenlibs/libreadline.a /home/tyler/packages/emscriptenlibs/libtermcap.a"
