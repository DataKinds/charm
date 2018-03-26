FILES = main.cpp Parser.cpp Runner.cpp PredefinedFunctions.cpp
LIBS = -lreadline

DEBUG ?= false

all: create-debug-h
	g++ -Wall -g --std=c++11 $(FILES) $(LIBS) -o charm

.PHONY: create-debug-h
create-debug-h:
	echo "#define ONLYDEBUG if(DEBUGMODE)\n#define DEBUGMODE $(DEBUG)" > Debug.h
