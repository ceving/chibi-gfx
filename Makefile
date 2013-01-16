MODULE=Xlib
PROJECT=chibi-xlib

CC=gcc

PREFIX:=$(or $(PREFIX),/usr/local)

MODULEC=$(MODULE).c
MODULESO=$(MODULE).so

all: $(MODULESO)

clean:
	-rm *~ $(MODULEC) $(MODULESO)

dist:
	mkdir $(PROJECT)-`cat VERSION`
	cp `cat FILES` $(PROJECT)-`cat VERSION`/.
	tar czf $(PROJECT)-`cat VERSION`.tar.gz $(PROJECT)-`cat VERSION`
	rm -rf $(PROJECT)-`cat VERSION`

install: $(MODULESO)
	install -D $(MODULESO) $(PREFIX)/lib/chibi/posix/$(MODULESO)
	install -D syslog.sld $(PREFIX)/share/chibi/posix/syslog.sld

%.c: %.stub
	chibi-ffi $<

$(MODULESO): $(MODULEC)
	$(CC) -fPIC -shared -lX11 -o $@ $<

gfx: gfx.c
	gcc -o gfx -lX11 -lm gfx.c
