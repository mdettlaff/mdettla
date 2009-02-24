CC = gcc
CFLAGS = -Wall -lrt

typing : typing.c

all : typing.c

test :
	@echo ">>> TEST: Przepisywanie pliku pliku teksty/test.txt"
	@echo ">>> typing teksty/test.txt"
	@./typing teksty/test.txt

clean:
	@rm -f core \#* *~ *.o
