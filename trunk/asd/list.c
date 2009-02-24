/* Lista jednokierunkowa cykliczna z wartownikiem - przykladowa implementacja */

#include <stdio.h>
#include <stdlib.h>

typedef struct node{
	int key;
	struct node *next;
}Tnode;

typedef struct list{
	Tnode *sentinel;
}Tlist;

/* procedura tworzaca wezel
 * uzycie: wezel=makenode(liczba); */
Tnode *makenode(int k);

/* inicjuje liste, tworzy wartownika */
void list_initialize(Tlist *L);

void list_insert(Tlist *L, int k);

void list_display(Tlist *L);

/* zwraca znaleziony wezel z kluczem k */
Tnode *list_search(Tlist *L, int k);

/* usuwa wezel node z listy */
void list_delete(Tlist *L, Tnode *node);

void isonlist(Tlist L, int k);

/* oproznij liste */
void list_empty(Tlist *L);

int main() {
	Tlist L;

	list_initialize(&L);

	printf("Lista jednokierunkowa cykliczna z wartownikiem - przykladowa implementacja\n\n");

	list_insert(&L, 8);
	list_insert(&L, 7);
	list_insert(&L, 11);
	list_insert(&L, 3);
	list_insert(&L, 4);

	printf("Zawartosc listy: ");
	list_display(&L);
	putchar('\n');

	isonlist(L, 7);
	isonlist(L, 4);
	isonlist(L, 8);
	isonlist(L, 5);

	printf("Usuwanie wezla z kluczem 11\n");
	list_delete(&L, list_search(&L, 11));
	printf("Usuwanie wezla z kluczem 2\n");
	list_delete(&L, list_search(&L, 2));
	printf("Usuwanie wezla z kluczem 8\n");
	list_delete(&L, list_search(&L, 8));
	putchar('\n');

	printf("Zawartosc listy: ");
	list_display(&L);
	putchar('\n');

	printf("Oproznianie listy...\n");
	list_empty(&L);

	printf("Zawartosc listy: ");
	list_display(&L);

	return 0;
}

void isonlist(Tlist L, int k){
	printf("Szukam %d na liscie - ", k);
	if (list_search(&L, k))
		printf("Znaleziono!\n");
	else printf("Nie znaleziono\n");
}

Tnode *makenode(int k){
	Tnode *node;
	node=(Tnode *)malloc(sizeof(Tnode));
	if (node == NULL){
		printf("makenode: blad pamieci\n");
		return NULL;
	}
	node->key=k;
	return node;
}

void list_initialize(Tlist *L){
	Tnode *sentinel;

	/* tworzymy wartownika */
	sentinel=makenode(0);
	L->sentinel=sentinel; /* poczatek listy wskazuje na wartownika */
	sentinel->next = sentinel; /* poczatkowo wartownik wskazuje sam na siebie */
}

void list_insert(Tlist *L, int k){
	Tnode *node;

	node=makenode(k);
	if (node){ /* jesli nie wystapil blad makenode */
		node->next = L->sentinel->next;
		L->sentinel->next = node;
	}
}

void list_display(Tlist *L){
	Tnode *node;

	for (node=L->sentinel->next; node != L->sentinel; node=node->next)
		printf("%d ", node->key);
	putchar('\n');
}

Tnode *list_search(Tlist *L, int k){
	Tnode *node;

	for (node=L->sentinel->next; node->next != L->sentinel->next; node=node->next)
		if (node->key == k)
			return node;
	return NULL; /* jesli nie znaleziono */
}

void list_delete(Tlist *L, Tnode *node){
	Tnode *nodetmp;
	if (node){
	/* majac dany wezel node mozna skasowac tylko nastepny wezel, wiec ustawiamy
	 * przy pomocy petli zmienna pomocnicza nodetmp na pozycje przed node */
		for (nodetmp=L->sentinel; nodetmp->next != node; nodetmp=nodetmp->next);
		nodetmp->next = node->next;
		free(node);
	}
	else{
		printf("blad: nie mozna usunac wezla\n");
	}
}

void list_empty(Tlist *L){
	Tnode *node;
	for (node=L->sentinel->next; node != L->sentinel; node=node->next)
		free(node);
	L->sentinel->next=L->sentinel; /* wartownik znowu wskazuje na siebie samego */
}
