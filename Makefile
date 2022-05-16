.PHONY: all clean

LYS_TTF=1
LYS_BACKEND?=multicore
CC?=gcc

include lib/github.com/diku-dk/lys/setup_flags.mk

all: main

lys.o: lys.c
	$(CC) -c $^ $(CFLAGS) -w

main.o: main.c lys.c
	$(CC) -c $< $(CFLAGS) -I.

lys.c: lys.fut
	futhark $(LYS_BACKEND) --library $^

main: main.o lys.o lib/github.com/diku-dk/lys/liblys.c lib/github.com/diku-dk/lys/liblys.h lib/github.com/diku-dk/lys/context_setup.c lib/github.com/diku-dk/lys/context_setup.h
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS) -I. -DPROGHEADER='"lys.h"'

clean:
	rm -f main *.o lys.c lys.h
