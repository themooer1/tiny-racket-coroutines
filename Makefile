UNAME := $(shell uname)
.PHONY: test

CFLAGS = -g

ifeq ($(UNAME), Darwin)
  format=macho64
else
  format=elf64
endif

%.run: %.o runtime.o compile.rkt
	gcc runtime.o $< -o $@

runtime.o: main.o char.o coroutine.o io.o
	ld -r main.o char.o coroutine.o io.o -o runtime.o

main.o: main.c types.h runtime.h
	gcc $(CFLAGS) -fPIC -c main.c -o main.o

char.o: char.c types.h
	gcc $(CFLAGS) -fPIC -c char.c -o char.o

coroutine.o: coroutine.c
	gcc $(CFLAGS) -fPIC -c coroutine.c -o coroutine.o

io.o: io.c runtime.h
	gcc $(CFLAGS) -fPIC -c io.c -o io.o

%.o: %.s compile.rkt
	nasm -f $(format) -o $@ $<

%.s: %.rkt compile.rkt
	racket -t compile-file.rkt -m $< > $@

clean:
	rm *.o *.s *.run examples/*.run
