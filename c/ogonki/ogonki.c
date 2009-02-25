/*
Zamienia polskie znaki z ogonkami na litery lacinskie w danym pliku
Sposob uzycia: ogonki [plik]
 */

#include <stdio.h>
#include <stdlib.h>

#define BUFSIZE 32000

int main (int argc, char *argv[])
{
  int i;
  char c;
  char tekst[BUFSIZE];
  FILE *f;

/* sprawdzenie, czy podano plik do przepisania */
  if (argc < 2){
    printf("Program zamienia polskie znaki z ogonkami na litery lacinskie\n");
    printf("Skladnia: ogonki [plik]\n");
  }
  else
  {
  /* wczytanie zawartosci podanego pliku do tablicy tekst[] */
    f=fopen(argv[1], "r+");
    if (f == NULL)
      fprintf(stderr, "blad: nie mozna otworzyc pliku %s\n", argv[1]);
    else /* jesli plik istnieje, wykonuj program */
    {
      for (i=0; (c=fgetc(f)) != EOF; i++)
	tekst[i]=c;
      tekst[i]=EOF;

      for (i=0; tekst[i] != EOF; i++)
	switch(tekst[i]){
	  case '¹': tekst[i]='a'; break;
	  case 'æ': tekst[i]='c'; break;
	  case 'ê': tekst[i]='e'; break;
	  case '³': tekst[i]='l'; break;
	  case 'ñ': tekst[i]='n'; break;
	  case 'ó': tekst[i]='o'; break;
	  case 'œ': tekst[i]='s'; break;
	  case '¿': tekst[i]='z'; break;
	  case 'Ÿ': tekst[i]='z'; break;

	  case '¥': tekst[i]='A'; break;
	  case 'Æ': tekst[i]='C'; break;
	  case 'Ê': tekst[i]='E'; break;
	  case '£': tekst[i]='L'; break;
	  case 'Ñ': tekst[i]='N'; break;
	  case 'Ó': tekst[i]='O'; break;
	  case 'Œ': tekst[i]='S'; break;
	  case '¯': tekst[i]='Z'; break;
	  case '': tekst[i]='Z'; break;
	}

      rewind(f);
      for (i=0; tekst[i] != EOF; i++)
	fputc(tekst[i], f);

      fclose(f);
    }
  }
  return 0;
}
