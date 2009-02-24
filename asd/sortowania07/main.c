/* linkowac z opcja -lrt */

# include <stdio.h>
# include <stdlib.h>
# include <time.h>
# include "sort.h"

int hypothesis; /* zlicza ilosc zwyciestw algorytmow w testach */

int main(int argc, char *argv[]){
	Tdate a[MAXTABSIZE];
	int i, n;
	int ferror=0;

	if (argc < 2){ /* jesli nie podano plikow do testowania */
		printf("Program testujacy algorytmy sortujace dla struktur danych\n");
		printf("Sposob uzycia: sort [plik1] [plik2] ...\n");
		printf("gdzie [plik1], [plik2]... to pliki z datami w formacie dd-mm-yyyy\n");
	}
	else{ /* jesli podano pliki do testowania */
		for (i=1; i < argc; i++){ /* wypisywanie bledow o plikach */
			if ((n=isdatesfile(argv[i])) == -1){
				printf("blad: nie mozna odnalezc pliku %s\n", argv[i]);
				ferror++;
			}
			else if (n == 0){
				printf("blad: plik %s ma nieprawidlowy format danych\n", argv[i]);
				ferror++;
			}
			else if ((n=readdatesfile(a, argv[i])) == -2){
				printf("blad: plik %s ma zbyt wiele danych\n", argv[i]);
				ferror++;
			}
			else if (n < 2){
				printf("blad: brak danych do sortowania w pliku %s\n", argv[i]);
				ferror++;
			}
		}
		if (!(ferror)){ /* jesli wszystkie pliki wczytano poprawnie, wykonaj testy */
			printf("===========================================================================\n\n");
			printf("POROWNANIE WYDAJNOSCI ALGORYTMOW SORTUJACYCH QS I QSW\n");
			printf("QS - Quicksort\n");
			printf("QSW - Splycony Quicksort + sortowanie przez wstawianie (Insertion sort)\n\n");
			printf("Wspolczynnik c reguluje poziom splycenia Quicksort w QSW, tj. minimalna\n");
			printf("dlugosc tablicy sortowanej przez Quicksort\n\n");
			printf("HIPOTEZA: QSW jest wydajniejszy od QS\n\n");

			for (i=1; i < argc; i++)
				testfile(a, argv[i]);

			printf("===========================================================================\n\n");
			printf("WYNIKI TESTOW: hipoteza zostala ");
			if (hypothesis > 0)
				printf("potwierdzona!");
			else
				printf("obalona");
			printf("\n\n");
		}
	}

	return 0;
}
