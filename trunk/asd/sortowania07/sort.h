#ifndef _sort_h
#define _sort_h

/* mozna wczytac maksymalnie MAXTABSIZE-1 dat, bo ostatnie
 * miejsce zarezerwowane jest na element koncowy */
# define MAXTABSIZE 100001

typedef struct date{
	int day;
	int month;
	int year;
} Tdate;

/* funkcje do sortowan */

/* standardowy qsort */
void qsortnormal(Tdate a[], int p, int r);

/* quick-sort + insert sort */
void qsortw(Tdate a[], int p, int r, int c);

/* quick-sort sortujacy tylko tablice dluzsze od c */
void qsortlimit(Tdate a[], int p, int r, int c);

int partition(Tdate a[], int p, int r);

/* sortowanie przez wstawianie */
void insertsort(Tdate a[], int p, int r);

/* zamienia ze soba dwie daty */
void swap(Tdate *a, Tdate *b);

/* wypisz na ekran tablice dat */
void printtable(Tdate a[]);

/* porownuje daty a i b: jesli a>b zwraca 1, a==b zwraca 0, a<b zwraca -1 */
int comparedates(Tdate a, Tdate b);


/* funkcje do testow */

/* przeprowadza testy dla danego pliku */
void testfile(Tdate a[], char filename[]);

/* sortuje tablice qsortem i zwraca czas sortowania */
double timeqsortnormal(Tdate a[], int n);

/* sortuje tablice qsw i zwraca czas sortowania */
double timeqsortw(Tdate a[], int n, int c);

/* sprawdza czy tablica jest posortowana; jesli tak zwraca 1, jesli nie - 0 */
int issorted(Tdate a[]);

/* wczytuje daty z pliku do a[] i zwraca ich ilosc lub -1 jesli brak pliku
 * lub -2 jesli jest zbyt wiele dat w pliku */
int readdatesfile(Tdate a[], char file[]);

/* zwraca dzien, miesiac lub rok odczytany z pliku */
int getymd(FILE *f);

/* jesli plik nie istnieje, zwraca -1, jesli ma zly format daty - 0, jesli OK - 1 */
int isdatesfile(char file[]);

#endif
