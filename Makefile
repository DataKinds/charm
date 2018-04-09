OBJECT_FILES = main.o Parser.o Runner.o Stack.o PredefinedFunctions.o FunctionAnalyzer.o Prelude.charm.o
LIBS = -lreadline
CFLAGS =
CPP = g++

OUT_FILE ?= charm

# Compilation flags
DEBUG ?= false
OPTIMIZE_INLINE ?= true

DEFAULT_EXECUTABLE_LINE = $(CPP) -Wall -g --std=c++17 -DDEBUGMODE=$(DEBUG) -DOPTIMIZE_INLINE=$(OPTIMIZE_INLINE) $(CFLAGS) -o $(OUT_FILE) $(LIBS)
DEFAULT_OBJECT_LINE = $(CPP) -c -Wall -g --std=c++17 -DDEBUGMODE=$(DEBUG) -DOPTIMIZE_INLINE=$(OPTIMIZE_INLINE) $(CFLAGS) $(LIBS)

release: $(OBJECT_FILES)
	$(DEFAULT_EXECUTABLE_LINE) $(OBJECT_FILES)

debug: $(OBJECT_FILES)
	g++ -Wall -g --std=c++17 -DDEBUGMODE=$(DEBUG) -DOPTIMIZE_INLINE=$(OPTIMIZE_INLINE) $(CFLAGS) $(OBJECT_FILES) $(LIBS) -o charm-debug

main.o:
	$(DEFAULT_OBJECT_LINE) main.cpp
Parser.o:
	$(DEFAULT_OBJECT_LINE) Parser.cpp
Runner.o:
	$(DEFAULT_OBJECT_LINE) Runner.cpp
Stack.o:
	$(DEFAULT_OBJECT_LINE) Stack.cpp
PredefinedFunctions.o:
	$(DEFAULT_OBJECT_LINE) PredefinedFunctions.cpp
FunctionAnalyzer.o:
	$(DEFAULT_OBJECT_LINE) FunctionAnalyzer.cpp
Prelude.charm.o:
	g++ -c -Wall -O3 --std=c++17 Prelude.charm.cpp

clean:
	rm $(OBJECT_FILES)
reload-prelude:
	rm Prelude.charm.o
	make
