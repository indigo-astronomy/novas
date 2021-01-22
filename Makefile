CC=gcc
CFLAGS=-Wall
LDFLAGS=-lm -s
SOLSYSOBJS=src/solsys1.o
#SOLSYSOBJS=src/solsys2.o #! used src/jplint.f
#SOLSYSOBJS=src/solsys3.o
LIBOBJS=src/novas.o src/novascon.o src/nutation.o src/eph_manager.o src/readeph0.o $(SOLSYSOBJS)
LIBNAMES=libnovas.a libnovas.so.0
PROGNAMES=example checkout-mp checkout-stars checkout-stars-full cio_file

all: lib progs

lib: $(LIBNAMES)

progs: $(PROGNAMES)

libnovas.a: $(LIBOBJS)
	ar rcs $@ $^

libnovas.so.0: $(LIBOBJS)
	$(CC) $(CFLAGS) -shared $^ -o $@ $(LDFLAGS)

example: src/example.c libnovas.a
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

checkout-mp: src/checkout-mp.c libnovas.a
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

checkout-stars: src/checkout-stars.c libnovas.a
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

checkout-stars-full: src/checkout-stars-full.c libnovas.a
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

cio_file: src/cio_file.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(LIBNAMES) $(PROGNAMES) $(LIBOBJS)
