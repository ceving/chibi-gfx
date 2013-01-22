PROJECT=chibi-xlib

CC=gcc
CFLAGS=-fPIC

PREFIX:=$(or $(PREFIX),/usr/local)

all: $(PROJECT).so

clean:
	-rm *~ *.o $(PROJECT).so $(PROJECT).c $(PROJECT).stub

dist:
	mkdir $(PROJECT)-`cat VERSION`
	cp `cat FILES` $(PROJECT)-`cat VERSION`/.
	tar czf $(PROJECT)-`cat VERSION`.tar.gz $(PROJECT)-`cat VERSION`
	rm -rf $(PROJECT)-`cat VERSION`

#install: $(MODULESO)
#	install -D $(MODULESO) $(PREFIX)/lib/chibi/posix/$(MODULESO)
#	install -D syslog.sld $(PREFIX)/share/chibi/posix/syslog.sld

$(PROJECT).so: $(PROJECT).o
	$(CC) -shared -o $@ $^ -lchibi-scheme -lX11

$(PROJECT).stub: Xlib.stub X.stub
	cat $^ > $@

$(PROJECT).c: $(PROJECT).stub
	chibi-ffi $<

$(PROJECT).o: $(PROJECT).c Xlib_helper.c
	$(CC) $(CFLAGS) -c $<

gfx: gfx.c
	gcc -o gfx -lX11 -lm gfx.c

X.stub: /usr/include/X11/X.h
	echo '(c-system-include "X11/Xlib.h")' > $@
	sed -n 's/^#define \([A-Z][a-zA-Z0-9]\+\).*/(define-c-const int (X\1 "\1"))/p' $< >> $@
