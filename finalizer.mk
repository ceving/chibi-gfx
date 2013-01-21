all: finalizer.so
	chibi-scheme < finalizer.scm

finalizer.so: finalizer.o
	gcc -shared -o $@ $^ -lchibi-scheme -lX11

finalizer.c: finalizer.stub
	chibi-ffi $<

finalizer.o: finalizer.c
	gcc -fPIC -c $<
