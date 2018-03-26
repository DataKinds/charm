FILES = main.cpp Parser.cpp Runner.cpp PredefinedFunctions.cpp
LIBS = -lreadline

create-debug-h:
	echo "#define ONLYDEBUG if(DEBUGMODE)" > Debug.h

debug: create-debug-h
	echo "#define DEBUGMODE true" >> Debug.h
	g++ -Wall -g --std=c++11 $(FILES) $(LIBS) -o charm

all: create-debug-h
	echo "#define DEBUGMODE false" >> Debug.h
	g++ -Wall -g --std=c++11 $(FILES) $(LIBS) -o charm
