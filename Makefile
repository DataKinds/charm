FILES = main.cpp Parser.cpp Runner.cpp Stack.cpp PredefinedFunctions.cpp
LIBS = -lreadline
CFLAGS =

DEBUG ?= false

all: create-debug-h
	g++ -Wall -g --std=c++11 $(CFLAGS) $(FILES) $(LIBS) -o charm

release: create-debug-h
	g++ -Wall -O3 --std=c++11 $(FILES) $(LIBS) -o charm-release

.PHONY: create-debug-h
create-debug-h:
	echo "#define ONLYDEBUG if(DEBUGMODE)\n#define DEBUGMODE $(DEBUG)" > Debug.h
