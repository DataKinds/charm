FILES = main.cpp Parser.cpp Runner.cpp Stack.cpp PredefinedFunctions.cpp
LIBS = -lreadline
CFLAGS =


# Compilation flags
DEBUG ?= false
OPTIMIZE_INLINE ?= true

all:
	g++ -Wall -g --std=c++11 -DDEBUGMODE=$(DEBUG) -DOPTIMIZE_INLINE=$(OPTIMIZE_INLINE) $(CFLAGS) $(FILES) $(LIBS) -o charm

release:
	g++ -Wall -O3 --std=c++11 $(FILES) $(LIBS) -o charm-release
