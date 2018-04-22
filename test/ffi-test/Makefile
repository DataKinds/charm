# In the base charm directory, make sure you've run `make ffi-lib`.
# Then, inside of charm (from this directory), run " hello " " testLib.so " " charmFFIHelloWorld " ffi

all:
	g++ --shared -fPIC --std=c++1z -o testLib.so testLib.cpp -lcharmffi 

clean:
	-rm testLib.so
