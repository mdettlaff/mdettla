CC=gcc
CFLAGS=-Wall -g

csources = main.c lines.c

programs = $(csources:.c=)

lines : main.o lines.o -lncursesw -lrt

main.o : main.c lines.h

lines.o : lines.c lines.h

all : $(programs)

test:
	./lines

clean:
	@rm -f core \#* *~ *.o *.ps $(programs)
