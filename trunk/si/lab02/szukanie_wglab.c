/*
 * POSZUKIWANIE DROGI W GRAFIE
 *
 * Graf jest zadany w postaci tablicy sasiedztw, z kosztami (liczby
 * rzeczywiste) krawedzi miedzy sasiednimi wierzcholkami.  Brak
 * bezposredniej krawedzi zaznacza sie liczba -1.  Wezly poczatkowy
 * i koncowy to liczby w calkowite nieujemne; nalezy je podac w danych
 * zaraz za grafem.
 */

//======================================================================

#include<stdio.h>
#include<stdlib.h>

#define LICZBA_WEZLOW 6

typedef  enum { FALSE= 0, TRUE = 1 }  Boolean;

typedef  int  Wezel;

struct wez {
  Wezel  w;  /* numer wezla */
  float  k;  /* koszt dojscia do tego wezla */
  struct wez* dalej;
};
typedef  struct wez*  Lista;


float  graf[LICZBA_WEZLOW][LICZBA_WEZLOW];

//======================================================================
// POMOCNICZE:

Boolean  zawiera(Wezel wz, Lista ls) {
    // sprawdza, czy lista  ls  zawiera wezel  wz
  while (ls != NULL)
    if (ls->w == wz)  return TRUE;
    else  ls = ls->dalej;
  return FALSE;
}

Lista  do_listy(Wezel wz, float koszt, Lista ls) {
    // do listy  ls  dolacza (z przodu) wezel  wz  z danym kosztem
  Lista  lls = (Lista)malloc(sizeof(struct wez));
  lls->w = wz; lls->k = koszt; lls->dalej = ls;
  return lls;
}

Lista  wstaw_wg_kosztu(Wezel wz, float k, Lista ls) {
//  if (ls == NULL) return do_listy(wz, k, NULL);
//  else if (ksz <= lista->k) return do_listy(wz, k, ls);
//  else return do_listy(lista->w, lista->k,
//      wstaw_wg_kosztu(wz, ksz, ls->dalej));
  if (ls == NULL) return do_listy(wz, k, NULL);
  else if (k <= ls->k) return do_listy(wz, k, ls);
  else return do_listy(ls->w, ls->k,
      wstaw_wg_kosztu(wz, k, ls->dalej));
}


void  drukuj (Lista sciezka) {
    // drukowanie sciezki
  while (sciezka != NULL) {
    printf("  (%i,%6.2f)", sciezka->w, sciezka->k);
    sciezka = sciezka->dalej;
  }
}

//======================================================================
// PRZESZUKIWANIE:

Lista  otwarte, zamkniete;

Boolean  wgl(Wezel kon, Lista* sciezka) {
    // pomocnicza dla funkcji  wglab
  Lista sciezka1;
  if (otwarte == NULL)  return FALSE;
  else {
    Wezel wz;
    *sciezka = do_listy(otwarte->w, otwarte->k, *sciezka);
    zamkniete = do_listy(otwarte->w, otwarte->k, zamkniete);
    otwarte = otwarte->dalej;
    for (wz=0; wz<LICZBA_WEZLOW; wz++)
      if (graf[(*sciezka)->w][wz] >= 0 && !zawiera(wz, zamkniete))
	otwarte = wstaw_wg_kosztu(
           wz,
           (*sciezka)->k + graf[(*sciezka)->w][wz],
           otwarte
        );
    if ((*sciezka)->w == kon)  return TRUE;
    else
      if (wgl(kon, sciezka))  return TRUE;
      else {
	*sciezka = (*sciezka)->dalej;
	return wgl(kon, sciezka);
      }
  }
}

Boolean  wglab(Wezel pocz, Wezel kon, Lista* sciezka) {
    // przeszukuje graf w glab od wezla  pocz
    // az znajdzie wezel  kon
  *sciezka = NULL;
  zamkniete = NULL;
  otwarte = do_listy(pocz, 0, NULL);
  return wgl(kon, sciezka);    
}

//======================================================================

int main () {
  Wezel  pocz, kon, w1, w2;
  Lista  sciezka;
  int  i, j;

  /* Wczytywanie grafu: */
  for (w1=0; w1<LICZBA_WEZLOW; w1++)
    for (w2=0; w2<LICZBA_WEZLOW; w2++)
      scanf("%f", &graf[w1][w2]);

  /* Wczytanie wierzcholka poczatkowego i wierzcholka koncowego: */
  scanf("%i", &pocz);
  scanf("%i", &kon);

  /* Przeszukiwanie i wydruk: */
  if (wglab(pocz, kon, &sciezka)) {
    printf("\nZnaleziona sciezka:\n");
    drukuj(sciezka);
    printf("\n\n");
  }
  else  printf("\nNie ma sciezki.\n\n");

  return 0;
}
