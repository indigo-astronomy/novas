CC=gcc
CPATH=src
LIBPATH=$(CPATH)/lib
CFLAGS=-Wall -I$(LIBPATH)
LDFLAGS=-lm -s
SOLSYSOBJS=$(LIBPATH)/solsys1.o
#SOLSYSOBJS=$(LIBPATH)/solsys2.o #! used $(LIBPATH)/jplint.f
#SOLSYSOBJS=$(LIBPATH)/solsys3.o
LIBOBJS=$(LIBPATH)/novas.o $(LIBPATH)/novascon.o $(LIBPATH)/nutation.o $(LIBPATH)/eph_manager.o $(LIBPATH)/readeph0.o $(SOLSYSOBJS)
LIBNAMES=libnovas.a libnovas.so.0
PROGNAMES=example checkout-mp checkout-stars checkout-stars-full cio_file

all: lib progs

lib: $(LIBNAMES)

progs: $(PROGNAMES)

libnovas.a: $(LIBOBJS)
	ar rcs $@ $^

libnovas.so.0: $(LIBOBJS)
	$(CC) $(CFLAGS) -shared $^ -o $@ $(LDFLAGS)

example: $(CPATH)/example.c libnovas.a
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

checkout-mp: $(CPATH)/checkout-mp.c libnovas.a
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

checkout-stars: $(CPATH)/checkout-stars.c libnovas.a
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

checkout-stars-full: $(CPATH)/checkout-stars-full.c libnovas.a
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

cio_file: $(CPATH)/cio_file.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(LIBNAMES) $(PROGNAMES) $(LIBOBJS)
