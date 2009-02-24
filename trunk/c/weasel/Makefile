CC = gcc
CFLAGS = -Wall -lrt

weasel : weasel.c

all : weasel.c

test :
	@echo ">>> weasel < test.txt"
	@echo
	@./weasel < test.txt

clean:
	@rm -f core \#* *~ *.o
