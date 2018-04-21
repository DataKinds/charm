LIB_OBJECT_FILES = Runner.o Stack.o PredefinedFunctions.o FunctionAnalyzer.o FFI.o
OBJECT_FILES = main.o Parser.o Prelude.charm.o $(LIB_OBJECT_FILES)

OUT_FILE ?= charm

ifeq ($(GUI),)
LDLIBS += -lreadline -lhistory -ldl
else
LDLIBS += -lreadline -lhistory -ltermcap -lncurses -ldl
CPPFLAGS += -DCHARM_GUI=1
OBJECT_FILES += gui.o
endif

# Include directories and library search paths
INCLUDEDIR ?=
LIBDIR ?=

# Compilation flags
DEBUG ?= false
OPTIMIZE_INLINE ?= true

DEFAULT_EXECUTABLE_LINE = $(CXX) -Wall -O3 --std=c++1z -DDEBUGMODE=$(DEBUG) -DOPTIMIZE_INLINE=$(OPTIMIZE_INLINE) $(CPPFLAGS) $(CXXFLAGS) $(INCLUDEDIR) $(LIBDIR) $(LDFLAGS) -o $(OUT_FILE) $(LDLIBS)
DEFAULT_OBJECT_LINE = $(CXX) -c -Wall -O3 --std=c++1z -DDEBUGMODE=$(DEBUG) -DOPTIMIZE_INLINE=$(OPTIMIZE_INLINE) $(CPPFLAGS) $(CXXFLAGS) $(INCLUDEDIR) $(LIBDIR) $(LDFLAGS)

release: $(OBJECT_FILES)
	$(DEFAULT_EXECUTABLE_LINE) $(OBJECT_FILES) $(LDLIBS)
install:
	chmod +x charm
	cp charm /usr/bin/charm

ffi-lib: clean
	make ffi-build-objects CPPFLAGS=-fPIC
	ar rvs libcharmffi.a $(LIB_OBJECT_FILES)
ffi-build-objects: $(LIB_OBJECT_FILES)
install-lib:
	cp libcharmffi.a /usr/lib/
	-mkdir /usr/include/charm
	cp ParserTypes.h /usr/include/charm/
	cp Runner.h /usr/include/charm/
	cp Stack.h /usr/include/charm/
	cp PredefinedFunctions.h /usr/include/charm/
	cp FunctionAnalyzer.h /usr/include/charm/
	cp FFI.h /usr/include/charm/
	cp CharmFFI.h /usr/include/charm/

main.o: main.cpp
	$(DEFAULT_OBJECT_LINE) main.cpp
Parser.o: Parser.cpp
	$(DEFAULT_OBJECT_LINE) Parser.cpp
Runner.o: Runner.cpp
	$(DEFAULT_OBJECT_LINE) Runner.cpp
Stack.o: Stack.cpp
	$(DEFAULT_OBJECT_LINE) Stack.cpp
PredefinedFunctions.o: PredefinedFunctions.cpp
	$(DEFAULT_OBJECT_LINE) PredefinedFunctions.cpp
FunctionAnalyzer.o: FunctionAnalyzer.cpp
	$(DEFAULT_OBJECT_LINE) FunctionAnalyzer.cpp
Prelude.charm.o: Prelude.charm.cpp
	$(CXX) -c -Wall -O3 --std=c++1z Prelude.charm.cpp
gui.o: gui.cpp
	$(DEFAULT_OBJECT_LINE) gui.cpp
FFI.o: FFI.cpp
	$(DEFAULT_OBJECT_LINE) FFI.cpp

clean:
	-rm $(OBJECT_FILES)
	-rm gui.o
	-rm libcharmffi.a
	-rm charm
	-rm charm-release
	-rm libhistory.o
	-rm libreadline.o
	-rm libtermcap.o
	-rm libtermcap.o
	-rm charm.html*
	-rm charm.js

reload-prelude:
	rm Prelude.charm.o
	make

.PHONY: release install ffi-lib install-lib clean reload-prelude ffi-build-objects
