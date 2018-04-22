# In the base charm directory, make sure you've run `make ffi-lib`.

dynamic:
	ghc -g -c -dynamic -fPIC doubleLib.hs
	ghc -g -dynamic -fPIC -c -I/usr/lib/ghc/include -optc--std=c++17 hsLib.cpp
	ghc -g -shared -dynamic -fPIC -o hsLib.so doubleLib.o hsLib.o -lstdc++ -lcharmffi -lHSrts-ghc8.0.2

clean:
	-rm *.hi
	-rm *.o
	-rm *.a
	-rm *_stub.h
	-rm *.so
	-rm a.out
