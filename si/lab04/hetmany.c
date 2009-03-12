/*
 * HETMANY NA SZACHOWNICY
 * 
 * Program rozstawia  N  hetmanow na szachownicy o wymiarach  N x N  w taki
 * sposob, zeby sie nawzajem nie szachowaly.
 */

//======================================================================

#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#define N 8   /* wielkosc szachownicy */

typedef enum  { FALSE = 0, TRUE = 1 }  Boolean;

//======================================================================

typedef  struct {
  int  liczba_hetmanow;  /* aktualnie na szachownicy, miedzy 0 a N wlacznie */
  int  hetman[N];        /* aktualne ustawienia hetmanow w kolejnych wierszach */
  int  ile_wolnych;
  Boolean  wolne[N][N];  /* czy dane pole jest wolne (niebite) */
}  Pozycja;


void  poz_poczatkowa (Pozycja* poz) {
  int  i, j;
  poz->liczba_hetmanow = 0;
  poz->ile_wolnych = N*N;
  for (i=0; i<N; i++)
    for (j=0; j<N; j++)
      poz->wolne[i][j] = TRUE;
}


Boolean  ruch (Pozycja* poz, int k) {
  int  i, j;
  if (poz->wolne [poz->liczba_hetmanow][k]) {
    poz->hetman[poz->liczba_hetmanow] = k;
    for (i=0; i<N; i++) {
      if (poz->wolne [poz->liczba_hetmanow][i]) {
	poz->wolne [poz->liczba_hetmanow][i] = FALSE;  poz->ile_wolnych --;
      }
    }
    for (i=0; i<N; i++) {
      if (poz->wolne [i][k]) {
	poz->wolne [i][k] = FALSE;  poz->ile_wolnych --;
      }
    }
    for (i=0; i<N; i++) {
      j = poz->liczba_hetmanow + k - i;
      if (0<=j && j<N)
	if (poz->wolne [j][i]) {
	  poz->wolne [j][i] = FALSE;  poz->ile_wolnych --;
      }
    }
    for (i=0; i<N; i++) {
      j = poz->liczba_hetmanow - k + i;
      if (0<=j && j<N)
	if (poz->wolne [j][i]) {
	  poz->wolne [j][i] = FALSE;  poz->ile_wolnych --;
      }
    }
    poz->liczba_hetmanow ++;
    return TRUE;
  }
  else  return FALSE;
}


    // Ocena pozycji; im wyzsza tym lepsza:
double  ocena(Pozycja p) {
  return  p.ile_wolnych;
}


void  druk_linia(int n) {
  int i;
  printf("     +");
  for (i=0; i<N; i++)  printf("---+");
  printf("\n");
}

void  druk(Pozycja p) {
  int  i,j;
  printf("\n");
  printf("      ");
  for (i=0; i<N; i++)  printf("%2i  ", i);
  printf("\n");
  druk_linia(N);
  for (i=0; i<N; i++) {
    printf("  %2i |", i);
    for (j=0; j<p.hetman[i]; j++)  printf("   |");
    printf(" X |");
    for (j=p.hetman[i]+1; j<N; j++)  printf("   |");
    printf("\n");
    druk_linia(N);
  }
  printf("\n");
}

//======================================================================
// LISTY Z OPERACJAMI:


    // Struktura list:
struct lista {
  Pozycja  wez;
  struct lista*  dalej;
};
typedef struct lista *  Lista;


    // Tworzenie nowej listy:
Lista  nowa_lista () {
  return (Lista)malloc(sizeof(struct lista));
}


    // Dodawanie wezla na poczatek listy:
void  dodaj_ls(Lista* p, Pozycja w) {
  Lista  p1 = nowa_lista();
  p1->wez = w;  p1->dalej = *p;  *p = p1;
}


void  zdejmij(Lista* ls, Pozycja* p) {
  *p = (*ls)->wez;  *ls = (*ls)->dalej;
}


    // ls = ls \ {p} :
void  odejmij_ls(Lista* ls, Pozycja p) {
  if (*ls != NULL) {
    if (p.ile_wolnych == ((*ls)->wez).ile_wolnych) {
      *ls = (*ls)->dalej;
      odejmij_ls(ls, p);
    }
    else  if (p.ile_wolnych < ((*ls)->wez).ile_wolnych)
      odejmij_ls(&((*ls)->dalej), p);
  }
}


    // ls = ls u {p}  wg liczby wolnych:
void  wstaw_ls_woln(Lista* ls, Pozycja p) {
  if (*ls == NULL) {
    *ls = nowa_lista();
    (*ls)->wez = p; (*ls)->dalej = NULL;
  }
  else  if (ocena(p) > ocena((*ls)->wez)) {
    Lista  lispom = nowa_lista();
    odejmij_ls(ls, p);
    lispom->wez = p;  lispom->dalej = *ls;
    *ls = lispom;
  }
  else  wstaw_ls_woln(&((*ls)->dalej), p);
}


    // Dodawanie  ls = ls u ls1  wg liczby wolnych:
void  dodaj_liste_woln(Lista* ls, Lista ls1) {
  while (ls1 != NULL) {
    wstaw_ls_woln(ls, ls1->wez);
    ls1 = ls1->dalej;
  }
}


    // ls = ls \ ls1 :
void  odejmij_liste(Lista* ls, Lista ls1) {
  while (ls1 != NULL) {
    odejmij_ls(ls, ls1->wez);
    ls1 = ls1->dalej;
  }
}

//======================================================================


    // Lista sasiadow danego wezla:
Lista  NN (Pozycja w) {
  Lista ls;  Pozycja v;  int i;
  ls = NULL;
  for (i=0; i<N; i++) {
    v=w;
    if (ruch(&v, i))
      dodaj_ls(&ls, v);
  }
  return ls;
}

//======================================================================
// ALGORYTM SZUKANIA ROZSTAWIENIA HETMANOW:


Boolean  het(Pozycja* pp) {
  Pozycja p;  Lista  closed = NULL, open = NULL;
  poz_poczatkowa(&p); dodaj_ls(&closed, p);
  while (p.liczba_hetmanow < N) {
    Lista  ls = NN(p);  odejmij_liste(&ls, closed);
    dodaj_liste_woln(&open, ls);
    if (open == NULL)  return FALSE;
    zdejmij(&open, &p);  dodaj_ls(&closed, p);
  }
  *pp = p;
  return TRUE;
}

//======================================================================

int  main () {
  Pozycja pp;

  if (het(&pp))  druk(pp);
  else  printf("\n  NIE MA\n\n");

  return 0;
}
