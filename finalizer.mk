all: finalizer.so nofinalizer.so
	@echo ";;; Without finalizer"
	chibi-scheme < nofinalizer.scm
	@echo ";;; With finalizer"
	chibi-scheme < finalizer.scm

%.so: %.o
	gcc -shared -o $@ $^ -lchibi-scheme -lX11

%.c: %.stub
	chibi-ffi $<

%.o: %.c
	gcc -fPIC -c $<
