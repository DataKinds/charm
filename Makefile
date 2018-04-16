LIB_OBJECT_FILES = Runner.o Stack.o PredefinedFunctions.o FunctionAnalyzer.o FFI.o
OBJECT_FILES = main.o Parser.o Prelude.charm.o $(LIB_OBJECT_FILES)

OUT_FILE ?= charm

ifeq ($(GUI),)
LDLIBS ?= -lreadline -lhistory -ldl
else
LDLIBS ?= -lreadline -lhistory -lncurses -ldl
CPPFLAGS += -DCHARM_GUI=1
OBJECT_FILES += gui.o
endif

# Compilation flags
DEBUG ?= false
OPTIMIZE_INLINE ?= true

DEFAULT_EXECUTABLE_LINE = $(CXX) -Wall -O3 --std=c++17 -DDEBUGMODE=$(DEBUG) -DOPTIMIZE_INLINE=$(OPTIMIZE_INLINE) $(CPPFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $(OUT_FILE)
DEFAULT_OBJECT_LINE = $(CXX) -c -Wall -O3 --std=c++17 -DDEBUGMODE=$(DEBUG) -DOPTIMIZE_INLINE=$(OPTIMIZE_INLINE) $(CPPFLAGS) $(CXXFLAGS) $(LDFLAGS) $(LDLIBS)

release: $(OBJECT_FILES)
	$(DEFAULT_EXECUTABLE_LINE) $(OBJECT_FILES) $(LDLIBS)
install: release
	cp charm /usr/bin/charm

ffi-lib: $(LIB_OBJECT_FILES)
	# TODO: add -fPIC to all the build commands??
	ar rvs libcharmffi.a $(LIB_OBJECT_FILES)
install-lib: ffi-lib
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
	$(CXX) -c -Wall -O3 --std=c++17 Prelude.charm.cpp
gui.o: gui.cpp
	$(DEFAULT_OBJECT_LINE) gui.cpp
FFI.o: FFI.cpp
	$(DEFAULT_OBJECT_LINE) FFI.cpp

clean:
	-rm $(OBJECT_FILES)
	-rm gui.o
	-rm libcharmffi.a

reload-prelude:
	rm Prelude.charm.o
	make

.PHONY: release install clean reload-prelude ffi-lib
