# In the base charm directory, make sure you've run `make ffi-lib`.
# Then, inside of charm (from this directory), run " hello " " main.o " " charmFFIHelloWorld " ffi

all:
	g++ -shared -fPIC --std=c++17 -lcharmffi -o testLib.so testLib.cpp
