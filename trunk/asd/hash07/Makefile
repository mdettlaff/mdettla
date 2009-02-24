CC = gcc
CFLAGS = -Wall

hashtable : hashtable.c -lm

hash19 : hash19.c -lm

all : hashtable

test :
	@echo ">>> hashtable data/3700.txt"
	@echo
	@./hashtable data/3700.txt

clean:
	@rm -f core \#* *~ *.o
