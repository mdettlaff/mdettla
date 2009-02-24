//                                          pmp@math.univ.gda.pl  2006
// budowanie i drukowanie drzew cz-cz z wartownikiem wspolnym
// dla wszystkich wezlow

#include <stdio.h>
#include <stdlib.h>

#define RED 1        /* stala oznaczajaca kolor wezla */
#define BLACK 0      /* stala oznaczajaca kolor wezla */
#define SZER_EKR 80  /* szerokosc ekranu */
#define IL_POZ   5   /* ilosc poziomow drzewa, ktore beda wydrukowane   */
                     /* gwiazdka bedzie sygnalizowac istnienie nizszych */
                     /* poziomow                                        */

/* struktury danych do reprezentowania drzewa */
typedef struct wezel *Wskwezla; /* wskaznik na wezel drzewa  */
typedef struct wezel{
	int klucz;
	Wskwezla left, right, p;
	int kolor;
} Twezla ;  /* typ wezla */

typedef struct tree{
	Twezla *root;
} Tdrzewa;

/* drzewa z wartownikami: wezel wskazywany przez "nil" jest wartownikiem 
   zastepujacym NULL; pole p musi byc ustawione odpowiednio w 
   przy usuwaniu */
Wskwezla nil; 

/* funkcja inicjujaca nil; musi byc wywolana przed budowaniem drzewa */
void nilInit(void);

/* funkcja drukujaca drzewo binarne na ekranie (tutaj tylko deklaracja) */
/* funkcja drukuje drzewo o korzeniu "w"                                */
void drukuj(Wskwezla w);

/* funkcja budujaca pelne drzewo binarne o wysokosci n;       */
/* kluczami sa kolejne liczby  naturalne zaczynajac od 1      */
Wskwezla buduj(int n);


/* czytanie polecen z klawiatury */
int getinput(Tdrzewa *T, char command[]);

Twezla *makenode(int k);


/* funkcje drzew CZERWONO-CZARNYCH */

void height_test(Tdrzewa *T);

void rb_insert_klucz(Tdrzewa *T, int k);

/* usuwa z drzewa wezel o zadanym kluczu */
void rb_delete_klucz(Tdrzewa *T, int k);

void rb_insert(Tdrzewa *T, Twezla *x);

Twezla *rb_delete(Tdrzewa *T, Twezla *z);

void rb_delete_fixup(Tdrzewa *T, Twezla *x);

void left_rotate(Tdrzewa *T, Twezla *x);

void right_rotate(Tdrzewa *T, Twezla *x);

/* wykonac pewna ilosc usuniec losowo wygenerowanych kluczy
 * (bez powtorzen) i zbadac jak zmienila sie wysokosc otrzymanego drzewa:
 * zwykla i uwzgledniajaca tylko czarne wezly */
int rb_black_height(Tdrzewa *T);


/* funkcje zwyklych drzew binarnych */

void tree_insert(Tdrzewa *T, Twezla *z);

/* zwraca wezel z kluczem k; jesli takiego nie ma, zwraca nil */
Twezla *tree_search(Twezla *x, int k);

Twezla *tree_successor(Twezla *x);

void tree_height(Twezla *x, int h, int *height);

void tree_node_count(Twezla *x, int *nodes);

Twezla *tree_minimum(Twezla *x);

Twezla *tree_maximum(Twezla *x);
