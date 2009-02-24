CC = gcc
CFLAGS = -Wall

name=huffman

huffman : $(name).c

all : $(name).c

test :
	./$(name) input
	@echo
	./$(name) t.txt

clean:
	@rm -f core \#* *~ *.o
