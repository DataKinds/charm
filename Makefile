FILES = main.cpp Parser.cpp Runner.cpp Stack.cpp PredefinedFunctions.cpp FunctionAnalyzer.cpp
LIBS = -lreadline
CFLAGS =

OUT_FILE = charm-release

# Compilation flags
DEBUG ?= false
OPTIMIZE_INLINE ?= true

release:
	g++ -Wall -O3 --std=c++17 -DDEBUGMODE=false -DOPTIMIZE_INLINE=true $(FILES) $(LIBS) -o $(OUT_FILE)

debug:
	g++ -Wall -g --std=c++17 -DDEBUGMODE=$(DEBUG) -DOPTIMIZE_INLINE=$(OPTIMIZE_INLINE) $(CFLAGS) $(FILES) $(LIBS) -o charm-debug
