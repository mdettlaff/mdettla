
//======================================================================

#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#define NIESK 10000
#define DL_NAZWY_WEZLA 50

//======================================================================

typedef enum  { FALSE = 0, TRUE = 1 }  Boolean;


typedef enum {
  Arad = 0,
  Bucuresti = 1,
  Craiova = 2,
  Dobreta = 3,
  Eforie = 4,
  Fagaras = 5,
  Giurgiu = 6,
  Hirsova = 7,
  Iasi = 8,
  Lugoj = 9,
  Mehadia = 10,
  Neamt = 11,
  Oradea = 12,
  Pitesti = 13,
  Rimniscu_Vilcea = 14,
  Sibiu = 15,
  Timisoara = 16,
  Urziceni = 17,
  Vaslui = 18,
  Zerind = 19,
}  Wezel;

void  druk (char dr[], Wezel w) {
  switch (w) {
    case Arad: strcpy(dr, "Arad"); break;
    case Bucuresti: strcpy(dr, "Bucuresti"); break;
    case Craiova: strcpy(dr, "Craiova"); break;
    case Dobreta: strcpy(dr, "Dobreta"); break;
    case Eforie: strcpy(dr, "Eforie"); break;
    case Fagaras: strcpy(dr, "Fagaras"); break;
    case Giurgiu: strcpy(dr, "Giurgiu"); break;
    case Hirsova: strcpy(dr, "Hirsova"); break;
    case Iasi: strcpy(dr, "Iasi"); break;
    case Lugoj: strcpy(dr, "Lugoj"); break;
    case Mehadia: strcpy(dr, "Mehadia"); break;
    case Neamt: strcpy(dr, "Neamt"); break;
    case Oradea: strcpy(dr, "Oradea"); break;
    case Pitesti: strcpy(dr, "Pitesti"); break;
    case Rimniscu_Vilcea: strcpy(dr, "Rimniscu_Vilcea"); break;
    case Sibiu: strcpy(dr, "Sibiu"); break;
    case Timisoara: strcpy(dr, "Timisoara"); break;
    case Urziceni: strcpy(dr, "Urziceni"); break;
    case Vaslui: strcpy(dr, "Vaslui"); break;
    case Zerind: strcpy(dr, "Zerind"); break;
  }
}



float  odl[20][20];

void init_odl() {
  Wezel w, v;
  for (w=Arad; w<=Zerind; w++)
    odl[w][w] = 0;
  for (w=Arad; w<=Zerind; w++)
    for (v=Arad; v<w; v++) // segfault
      odl[w][v] = NIESK;
  odl[Arad][Sibiu] = 140;
  odl[Arad][Timisoara] = 118;
  odl[Arad][Zerind] = 75;
  odl[Bucuresti][Giurgiu] = 90;
  odl[Bucuresti][Fagaras] = 211;
  odl[Bucuresti][Pitesti] = 101;
  odl[Bucuresti][Urziceni] = 85;
  odl[Craiova][Dobreta] = 120;
  odl[Craiova][Pitesti] = 138;
  odl[Craiova][Rimniscu_Vilcea] = 146;
  odl[Dobreta][Mehadia] = 75;
  odl[Eforie][Hirsova] = 86;
  odl[Fagaras][Sibiu] = 99;
  odl[Hirsova][Urziceni] = 98;
  odl[Iasi][Neamt] = 87;
  odl[Iasi][Vaslui] = 92;
  odl[Lugoj][Mehadia] = 70;
  odl[Lugoj][Timisoara] = 111;
  odl[Oradea][Sibiu] = 151;
  odl[Oradea][Zerind] = 71;
  odl[Pitesti][Rimniscu_Vilcea] = 97;
  odl[Rimniscu_Vilcea][Sibiu] = 80;
  odl[Urziceni][Vaslui] = 142;
  // TODO: odbicie symetryczne zeby dzialalo
  for (v=Arad; v<=Zerind; v++)
    for (w=Arad; w<v; w++) // segfault
      odl[w][v] = odl[v][w];
}

float  h (Wezel w) {  // odleglosci w linii prostej do Bucuresti
  switch (w) {
    case Arad: return 366;
    case Bucuresti: return 0;
    case Craiova: return 160;
    case Dobreta: return 242;
    case Eforie: return 161;
    case Fagaras: return 178;
    case Giurgiu: return 77;
    case Hirsova: return 151;
    case Iasi: return 226;
    case Lugoj: return 244;
    case Mehadia: return 241;
    case Neamt: return 234;
    case Oradea: return 380;
    case Pitesti: return 98;
    case Rimniscu_Vilcea: return 193;
    case Sibiu: return 253;
    case Timisoara: return 329;
    case Urziceni: return 80;
    case Vaslui: return 199;
    case Zerind: return 374;
  }
}

//======================================================================

struct ls {
  Wezel  wez;
  struct ls*  dalej;
};
typedef struct ls *  Lista;

float  kappa(Wezel w, Wezel v) {
  return  odl[w][v];
}

Boolean  koncowy (Wezel w) {
  return  w == Bucuresti;
}

void  dodaj_ls(Lista* p, Wezel w) {
  Lista  p1 = (Lista)malloc(sizeof(Lista*));
  p1->wez = w;  p1->dalej = *p;  *p = p1;
}

Lista  N(Wezel w) {
  Lista ls;  Wezel v;
  ls = NULL;
  for (v=Arad; v<=Zerind; v++)
    if (odl[w][v] != NIESK)
      dodaj_ls(&ls, v);
  return ls;
}

//======================================================================

typedef  struct {
  Wezel  ten, poprzedni;
  float  f, g;  // koszt dojscia od wezla poczatkowego
}  Wezel_plus;

struct lp {
  Wezel_plus  wzp;
  struct lp*  dalej_plus;
};
typedef struct lp *  Lista_plus;


void  min_plus(Lista_plus* lp, Wezel_plus* wp) {
  *wp = (*lp)->wzp;  *lp = (*lp)->dalej_plus;
}

void  usun_plus (Lista_plus* lp, Wezel w) {
  if (*lp != NULL)  {
    if (w == ((*lp)->wzp).ten)  *lp = (*lp)->dalej_plus;
    usun_plus(&((*lp)->dalej_plus), w);
  }
}

// wstawiamy na liste wg kolejnosci wag (lista jest uporzadkowana wg wag);
// jak na liscie jest lepsza wersja wezla, to go zastepujemy
void  wstaw_plus (Lista_plus* lp, Wezel_plus wp) {
  if (*lp == NULL)  {
    Lista_plus  lp1 = (Lista_plus)malloc(sizeof(Lista_plus*));
    lp1->wzp = wp;  lp1->dalej_plus = *lp;  *lp = lp1;
  }  else
    if (wp.f < ((*lp)->wzp).f)  {
      Lista_plus  lp1 = (Lista_plus)malloc(sizeof(Lista_plus*));
      lp1->wzp = wp;  usun_plus(lp, wp.ten);  lp1->dalej_plus = *lp;  *lp = lp1;
    }  else
      if (wp.ten != ((*lp)->wzp).ten)
        wstaw_plus(&((*lp)->dalej_plus), wp);
}

void  rozwin (Lista_plus* lp, Wezel w, float g, Lista ls) {
  while (ls != NULL) {
    Wezel_plus  wp;
    wp.poprzedni = w;  wp.ten = ls->wez;
    wp.g = g+kappa(w, ls->wez);  wp.f = wp.g + h(ls->wez);
    wstaw_plus(lp, wp);
    ls = ls->dalej;
  }
}

void  znajdz_sciezke(Lista* p, Lista_plus W, Wezel w, Wezel wg) {
  Wezel_plus  wp;
  *p = NULL;
  while (W != NULL) {
    min_plus(&W, &wp);
    if (wp.ten == wg) {
      dodaj_ls(p, wg);  wg = wp.poprzedni;
    }
  }
  dodaj_ls(p, wg);
}

//======================================================================


Boolean  A_gwiazdka(Wezel w0, Lista* p) {
  Lista_plus  W = NULL;  Lista_plus  R = NULL;  Wezel_plus  wp;
  wp.ten = w0;  wp.g = 0;  wp.f = h(w0);  wstaw_plus(&W, wp);
  rozwin(&R, w0, 0, N(w0));
  while (TRUE) {
    if (R == NULL)  return FALSE;
    else {
      Wezel_plus  wp;
      min_plus(&R, &wp); wstaw_plus(&W, wp);
      if (koncowy(wp.ten)) {
	znajdz_sciezke(p, W, w0, wp.ten);
	return TRUE;
      }
      else  rozwin(&R, wp.ten, wp.g, N(wp.ten));
    }
  }
}

//======================================================================

int main () {
  Lista  p;  char dr[DL_NAZWY_WEZLA];
  init_odl();
  if (A_gwiazdka(Arad, &p)) {
    printf("  SCIEZKA:\n");
    while (p != NULL) {
      druk(dr, p->wez);
      printf("    %s\n", dr);
      p = p->dalej;
    }
  }
  else
    printf("  NIE MA SCIEZKI\n");
  return 0;
}
