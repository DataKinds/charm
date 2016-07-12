FILES = main.cpp Parser.cpp Runner.cpp
LIBS = -lreadline

all:
	g++ -Wall --std=c++11 $(FILES) $(LIBS) -o charm.o
