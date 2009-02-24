/*
Program mierzy predkosc przepisywania przez uzytkownika tekstu z podanego pliku
Sposob uzycia: typing [plik]
Kompilowac z opcja -lrt
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MLD 1000000000.0
#define DL_WIERSZA 70	/* maksymalna dlugosc wiersza w wynikowym tekscie */
#define MAXWIERSZ 256   /* maksymalna dlugosc wiersza ktora mozna wpisac */

int main (int argc, char *argv[])
{
  struct timespec tp0, tp1;
  char c;
  char tekst[BUFSIZ];
  char wiersz[MAXWIERSZ];
  int i, j, k, n, pocz_wier, koniec_wier, dl_wiersza, ilosc_bledow=0;
  double czas;
  FILE *f;

/* sprawdzenie, czy podano plik do przepisania */
  if (argc < 2){
    printf("Program mierzy czas przepisywania tekstu z pliku\n");
    printf("Skladnia: typing [plik]\n");
  }
  else
  {
  /* wczytanie zawartosci podanego pliku do tablicy tekst[] */
    f=fopen(argv[1], "r");
    if (f == NULL)
      fprintf(stderr, "blad: nie mozna otworzyc pliku %s\n", argv[1]);
    else /* jesli plik istnieje, wykonuj program */
    {
      for (i=0; (c=fgetc(f)) != EOF; i++)
	tekst[i]=c;
      for (i-=1; tekst[i] == '\n'; i--); /* kasowanie '\n' na koncu pliku */
      tekst[i+1]=EOF;
      fclose(f);

      //n=strlen(tekst)-1;
      n=i+1;
      printf("Dlugosc tekstu: %d znakow\n", n);
      printf("Nacisnij ENTER aby rozpoczac pisanie\n");
      while ((c=getchar()) != '\n'); /* czeka na wcisniecie ENTER */

      clock_gettime(CLOCK_REALTIME, &tp0);

    /* przepisywanie tekstu */
      for (i=0; tekst[i] != EOF && i <= BUFSIZ; i++){
	if (tekst[i] == ' ' || tekst[i] == '\t' || tekst[i] == '\n')
	  koniec_wier = i-1;
	if ((i - pocz_wier) == DL_WIERSZA || tekst[i] == '\n'
	     || tekst[i+1] == EOF){ /* do wypisania ostatniego wiersza */
	  /* wypisywanie kolejnego wiersza tekstu zrodlowego */
	  if (tekst[i+1] == EOF) /* do wypisania ostatniego wiersza */
	    koniec_wier=i;
	  printf("\e[40;33;1m"); /* kolor: zolty na czarnym tle */
	  for (j=pocz_wier; j <= koniec_wier; j++)
	    putchar(tekst[j]);
	  printf("\e[0m"); /* wylaczenie kolorow */
	  putchar('\n');
	  /* wpisywanie wiersza przez uzytkownika */
	  for (j=0; (c=getchar()) != '\n' && c != EOF; j++)
	    wiersz[j]=c;
	  dl_wiersza=j-1;
	  /* sprawdzanie poprawnosci wpisanego wiersza */
	  for (j=pocz_wier, k=0; j <= koniec_wier; j++, k++)
	    if (tekst[j] != wiersz[k] || k > dl_wiersza)
	      ilosc_bledow+=1;
	  pocz_wier=koniec_wier+2;
	  koniec_wier=pocz_wier;
	}
      }

      clock_gettime(CLOCK_REALTIME, &tp1);
      czas = tp1.tv_sec + tp1.tv_nsec/MLD - (tp0.tv_sec + tp0.tv_nsec/MLD);

    /* wyswietlanie wynikow */
      printf("\nWyniki:\n");
      printf("czas pisania: %d min %d s\n", (int)czas/60, (int)czas%60);
      printf("ilosc bledow: %d\n", ilosc_bledow);
      printf("predkosc: %.1f znakow/min\n", n/czas*60);
    }
  }
  return 0;
}
