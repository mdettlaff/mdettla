CC=gcc
CFLAGS=-Wall -g

csources = main.c sort.c test.c

programs = $(csources:.c=)

sort : main.o sort.o test.o -lrt

main.o : main.c sort.h

sort.o : sort.c sort.h

test.o : test.c sort.h

all : $(programs)

test:
	@echo ">>>> sort data/00025.txt data/00250.txt data/02500.txt data/25000.txt data/99999.txt"
	@./sort data/00025.txt data/00250.txt data/02500.txt data/25000.txt data/99999.txt

randomdata:
	@echo "Tworzenie plikow z datami do sortowania"
	@if ! test -d data && test -e dategen/generate; then mkdir data; fi
	@if test -e dategen/generate; then \
	echo "dategen/generate 25 > data/00025.txt"; \
	echo "dategen/generate 250 > data/00250.txt"; \
	echo "dategen/generate 2500 > data/02500.txt"; \
	echo "dategen/generate 25000 > data/25000.txt"; \
	echo "dategen/generate 99999 > data/99999.txt"; \
	dategen/generate 25 > data/00025.txt; \
	dategen/generate 250 > data/00250.txt; \
	dategen/generate 2500 > data/02500.txt; \
	dategen/generate 25000 > data/25000.txt; \
	dategen/generate 99999 > data/99999.txt; \
	else echo "blad: brak programu generujacego dane"; fi

clean:
	@rm -f core \#* *~ *.o *.ps $(programs)
