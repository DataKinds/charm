FILES = main.cpp Parser.cpp Runner.cpp PredefinedFunctions.cpp
LIBS = -lreadline

all:
	g++ -Wall -g --std=c++11 $(FILES) $(LIBS) -o charm.o
