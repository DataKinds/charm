OBJECT_FILES = main.o Parser.o Runner.o Stack.o PredefinedFunctions.o FunctionAnalyzer.o Prelude.charm.o

OUT_FILE ?= charm

ifeq ($(GUI),)
LDLIBS ?= -lreadline -lhistory
else
LDLIBS ?= -lreadline -lhistory -lncurses
CPPFLAGS += -DCHARM_GUI=1
OBJECT_FILES += gui.o
endif

# Compilation flags
DEBUG ?= false
OPTIMIZE_INLINE ?= true

DEFAULT_EXECUTABLE_LINE = $(CXX) -Wall -g --std=c++17 -DDEBUGMODE=$(DEBUG) -DOPTIMIZE_INLINE=$(OPTIMIZE_INLINE) $(CPPFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $(OUT_FILE)
DEFAULT_OBJECT_LINE = $(CXX) -c -Wall -g --std=c++17 -DDEBUGMODE=$(DEBUG) -DOPTIMIZE_INLINE=$(OPTIMIZE_INLINE) $(CPPFLAGS) $(CXXFLAGS) $(LDFLAGS) $(LDLIBS)

release: $(OBJECT_FILES)
	$(DEFAULT_EXECUTABLE_LINE) $(OBJECT_FILES) $(LDLIBS)

debug: $(OBJECT_FILES)
	$(CXX) -Wall -g --std=c++17 -DDEBUGMODE=$(DEBUG) -DOPTIMIZE_INLINE=$(OPTIMIZE_INLINE) $(CFLAGS) $(OBJECT_FILES) $(LDLIBS) -o charm-debug

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

clean:
	rm $(OBJECT_FILES)
reload-prelude:
	rm Prelude.charm.o
	make

.PHONY: release debug clean reload-prelude
