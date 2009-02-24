/* Michal Dettlaff (mdettla), ASD07 haszowanie, 1 W%OD */

/* implementacja tablicy z haszowaniem modularnym, adresowanie otwarte dwukrotne
 * obslugiwana jest funkcja wstawiania napisow do tablicy
 * bedzie to tablica przechowujaca m napisow
 * napisy brane sa z podanego pliku tekstowego */

# include <stdio.h>
# include <string.h>
# include <math.h>

# define m 1487 /* dlugosc tablicy napisow z haszowaniem */
		/* dobra jest liczba pierwsza daleka od poteg dwojki */
# define MAXSTR 256 /* maksymalna dlugosc napisu */
# define NIL "\0" /* definiujemy element pusty w tablicy */

/* ilosc prob wstawien (porownan) */
int comparsitions=0;

/* obliczanie sredniej ilosci prob wstawien (porownan) n elementow do tablicy T */
int testinsert(char T[][], int n, char file[]);

/* czyta napis z pliku i zwraca jego dlugosc */
int readstrfile(char str[], FILE *f);

/* wpisywanie elementow pustych do tablicy */
void emptytable(char T[][]);

/* umieszcza napis s w tablicy napisow T o rozmiarze m */
int hash_insert(char s[], char T[][]);

/* adresowanie otwarte dwukrotne */
int H(unsigned int k, int i);

/* haszowanie modularne */
int h(unsigned int k);

/* wyliczanie klucza z napisu; powinna generowac klucze wygladajace na losowe,
 * a mala zmiana napisu powinna spowodowac nieprzewidywalna zmiane klucza */
unsigned int key(char s[]);

/* zwraca ilosc napisow wpisanych do tablicy */
int num_inserted(char T[][]);

void showtable(char T[][]);

int main(int argc, char *argv[]){
	char T[m][MAXSTR]; /* tablica m napisow o maks dlugosci MAXSTR */

	if (argc < 2){
		printf("Program testuje operacje wstawiania napisow do tablicy z haszowaniem.\n");
		printf("Uzycie: hashtable [plik_tekstowy]\n");
	}
	else{
		printf("Testowanie operacji wstawiania do tablicy z haszowaniem.\n\n");

		if (testinsert(T, m/2, argv[1]) == -1)
			printf("blad: nie mozna otworzyc pliku %s\n", argv[1]);
		else{
			testinsert(T, m*3/4, argv[1]);
			testinsert(T, m*9/10, argv[1]);
		}
	}

	return 0;
}

int testinsert(char T[][256], int n, char file[]){
	char s[MAXSTR];
	extern int comparsitions;
	FILE *f;
	int i;

	if ((f=fopen(file, "r")) != NULL){
		emptytable(T);
		comparsitions=0;
		printf("wczytywanie do tablicy %d napisow z pliku %s\n", n, file);
		/* wczytywanie napisow do tablicy z haszowaniem az sie skoncza, tablica sie zapelni
		 * lub liczba przeczytanych napisow przekroczy zadana liczbe */
		for (i=0; i < n && readstrfile(s, f) > 0 && hash_insert(s, T) != -1; i++);
		fclose(f);

		printf("poziom wypelnienia tablicy: %d/%d (%.f%%)\n", num_inserted(T), m,
								(double)num_inserted(T)/m*100);
		if (num_inserted(T) > 0)
			printf("srednia ilosc prob wstawienia elementu: %f\n",
						       (double)comparsitions/num_inserted(T));
		putchar('\n');

		return 0;
	}
	else
		return -1;
}

int readstrfile(char s[], FILE *f){
	char c;
	int i;

	for (i=0; (c=fgetc(f)) != '\n' && c != EOF; i++)
		s[i]=c;
	s[i]='\0';

	return i;
}

void emptytable(char T[][256]){
	int i;
	
	for (i=0; i < m; i++)
		strcpy(T[i], NIL);
}

int hash_insert(char s[], char T[][256]){
	extern int comparsitions;

	int j, i=0;

	do{
		j=H(key(s),i);
		comparsitions+=1; /* zliczanie prob wstawien (porownan) */
		if (strcmp(T[j], NIL) == 0){ /* jesli T[j] jest pusty */
			strcpy(T[j],s);
			return j;
		}
		else
			i++;
	}while (i != m);
	return -1; /* tablica przepelniona */
}

int H(unsigned int k, int i){
	return (h(k) + i * (1+(k%(m-2))) ) % m;
}

int h(unsigned int k){
	return k % m;
}

unsigned int key(char s[]){
	int i=0;
	unsigned int k=0;

	for (i=1; s[i] != '\0' && s[i-1] != '\0'; i+=2)
		k=k^(256*s[i-1] + s[i]); /* xor */

	return k;
}

int num_inserted(char T[][256]){
	int i, n=0;
	
	for (i=0; i < m; i++)
		if (strcmp(T[i], NIL))
			n++;
	return n;
}

void showtable(char T[][256]){
	int i;
	
	for (i=0; i < m; i++)
		if (strcmp(T[i], NIL))
			printf("T[%d]=%s\t", i, T[i]);
}
