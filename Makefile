FORTRAN=gfortran
FPATH=fortran
CC=gcc
CPATH=src
LIBPATH=$(CPATH)/lib
CFLAGS=-Wall -I$(LIBPATH)
LDFLAGS=-lm -s
SOLSYSOBJS=$(LIBPATH)/solsys1.o
#SOLSYSOBJS=$(LIBPATH)/solsys2.o #! used $(LIBPATH)/jplint.f
#SOLSYSOBJS=$(LIBPATH)/solsys3.o
LIBOBJS=$(LIBPATH)/novas.o $(LIBPATH)/novascon.o $(LIBPATH)/nutation.o $(LIBPATH)/eph_manager.o $(LIBPATH)/readeph0.o $(SOLSYSOBJS)
SOVER=0
VER=3.1
ifeq ($(OS),Windows_NT)
LIBNAMESTATIC=novas.a
LIBNAMESHARED=novas.$(SOVER).$(VER).dll
else
LIBNAMESTATIC=libnovas.a
LIBNAMESHARED=libnovas.so.$(SOVER)
endif
LIBNAMES=$(LIBNAMESTATIC) $(LIBNAMESHARED)
PROGNAMES=example checkout-mp checkout-stars checkout-stars-full cio_file
FPROGNAMES=asc2eph.e testeph1.e

ifneq ($(shell uname -m), i386)
    CFLAGS += -fPIC
endif

all: lib progs

lib: $(LIBNAMES)

progs: $(PROGNAMES)

fortran: $(FPROGNAMES)

$(LIBNAMESTATIC): $(LIBOBJS)
	ar rcs $@ $^

$(LIBNAMESHARED): $(LIBOBJS)
	$(CC) $(CFLAGS) -shared $^ -o $@ $(LDFLAGS)

example: $(CPATH)/example.c $(LIBNAMESTATIC)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

checkout-mp: $(CPATH)/checkout-mp.c $(LIBNAMESTATIC)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

checkout-stars: $(CPATH)/checkout-stars.c $(LIBNAMESTATIC)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

checkout-stars-full: $(CPATH)/checkout-stars-full.c $(LIBNAMESTATIC)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

cio_file: $(CPATH)/cio_file.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

asc2eph.e: $(FPATH)/asc2eph.f
	$(FORTRAN) $^ -o $@

# testeph1.e (below) is newer and smarter, so `testeph.e` is disabled.
# Uncomment testeph.e above if you want it and run `make testeph.e`, but it
# requires additional manual configuration.  See fortran/userguide.txt and
# fortran/example-configurations/*
#testeph.e: $(FPATH)/testeph.f
#	$(FORTRAN) $^ -o $@

testeph1.e: $(FPATH)/testeph1.f
	$(FORTRAN) $^ -o $@

clean:
	rm -f $(LIBNAMES) $(PROGNAMES) $(FPROGNAMES) $(LIBOBJS)
