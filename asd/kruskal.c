/* Rodziny zbiorow rozlacznych (Disjoint-set data structure)
 * Algorytm Kruskala znajdowania minimalnego drzewa spinajacego */

#include <stdio.h>
#include <stdlib.h>

#define NO_V 9 // ilosc wierzcholkow grafu
#define NO_E 14 // ilosc krawedzi grafu

/* jeden wezel z drzewa */
/* wezel jest jednoczesnie wezlem grafu */
typedef struct node {
	char key; // dzialamy na wierzcholkach grafu oznaczonych literami
	struct node *parent;
} t_node;

typedef struct edge {
	int w; // waga krawedzi
	t_node *u, *v;
} t_edge;

typedef struct graph {
	t_node V[NO_V];
	t_edge E[NO_E];
} t_graph;


/* zmienne globalne */
t_graph G;
t_edge A[NO_E+1];


/* operacje na zbiorach */

void make_set(t_node *x) {
	x->parent = x;
}

t_node *find(t_node *x) {
	if (x->parent == x)
		return x;
	else
		return find(x->parent);
}

void Union(t_node *x, t_node *y) {
	t_node *xRoot;
	t_node *yRoot;

	xRoot = find(x);
	yRoot = find(y);
	xRoot->parent = yRoot;
}


void init_tab(t_edge A[]) {
	int i;

	// 'empty' edge
	t_edge e;
	e.w = -1; e.u = NULL; e.v = NULL;

	for (i=0; i < NO_E; i++)
		A[i] = e;
}

void init_graph() {
	// przyklad wziety z Cormena
	// wierzcholki
	G.V[0].key = 'a';
	G.V[1].key = 'b';
	G.V[2].key = 'c';
	G.V[3].key = 'd';
	G.V[4].key = 'e';
	G.V[5].key = 'f';
	G.V[6].key = 'g';
	G.V[7].key = 'h';
	G.V[8].key = 'i';

	// powinno byc w mst_kruskal, ale durne wskazniki nie chca sie
	// uaktulanic, grr...
	int i;
	for (i=0; i < NO_V; i++)
		make_set(&G.V[i]);

	// krawedzie
	G.E[0].u = &G.V[0]; G.E[0].v = &G.V[1]; G.E[0].w = 4;
	G.E[1].u = &G.V[0]; G.E[1].v = &G.V[7]; G.E[1].w = 8;
	G.E[2].u = &G.V[1]; G.E[2].v = &G.V[2]; G.E[2].w = 8;
	G.E[3].u = &G.V[1]; G.E[3].v = &G.V[7]; G.E[3].w = 11;
	G.E[4].u = &G.V[6]; G.E[4].v = &G.V[7]; G.E[4].w = 1;
	G.E[5].u = &G.V[7]; G.E[5].v = &G.V[8]; G.E[5].w = 7;;
	G.E[6].u = &G.V[2]; G.E[6].v = &G.V[8]; G.E[6].w = 2;
	G.E[7].u = &G.V[6]; G.E[7].v = &G.V[8]; G.E[7].w = 6;
	G.E[8].u = &G.V[2]; G.E[8].v = &G.V[3]; G.E[8].w = 7;
	G.E[9].u = &G.V[2]; G.E[9].v = &G.V[5]; G.E[9].w = 4;
	G.E[10].u = &G.V[3]; G.E[10].v = &G.V[5]; G.E[10].w = 14;
	G.E[11].u = &G.V[3]; G.E[11].v = &G.V[4]; G.E[11].w = 9;
	G.E[12].u = &G.V[5]; G.E[12].v = &G.V[6]; G.E[12].w = 2;
	G.E[13].u = &G.V[4]; G.E[13].v = &G.V[5]; G.E[13].w = 10;
}

void display_spanning_tree(t_edge A[]) {
	int i;

	printf("Minimalne drzewo spinaj±ce:\n");
	for (i=0; A[i+1].u != NULL; i++)
		printf("(%c,%c) ", A[i].u->key, A[i].v->key);
	printf("(%c,%c)\n", A[i].u->key, A[i].v->key);
}

void display_graph(t_graph G) {
	int i;

	printf("{ ");
	for (i=0; i < NO_V-1; i++)
		printf("%c, ", G.V[i].key);
	printf("%c", G.V[i].key);
	printf(" }\n");
	for (i=0; i < NO_E; i++)
		printf("%c,%c%d ", G.E[i].u->key, G.E[i].v->key, G.E[i].w);
	printf("\n\n");
}

void tab_add(t_edge A[], t_edge e) {
	int i;
	for (i=0; A[i].u != NULL; i++)
		;
	A[i] = e;
}

void sort_edges(t_graph *G, int p, int r) {
	int j, k;
	t_edge current;

	for (k=p+1; k <= r; k++){
		current = (*G).E[k];
		j = k-1;
		while ((j >= p) && (current.w < (*G).E[j].w)){
			(*G).E[j+1] = (*G).E[j];
			j--;
		}
		(*G).E[j+1] = current;
	}
}

void mst_kruskal(t_graph G) {
	int i;

	init_tab(A);
	//for (i=0; i < NO_V; i++)
	//	make_set(&G.V[i]);
	sort_edges(&G, 0, NO_E-1);
	printf("Graf po posortowaniu wagami rosn±co:\n");
	display_graph(G);

	/*for (i=0; i < NO_V; i++) {
		if (G.E[i].u == G.E[i].u->parent)
			printf("u: rodzic rowny\n");
		else
			printf("u %c: rodzic nierowny\n", G.E[i].u->key);
		if (G.E[i].v == G.E[i].v->parent)
			printf("v: rodzic rowny\n");
		else
			printf("v: rodzic nierowny\n");
	}*/

	for (i=0; i < NO_E; i++)
		if (find(G.E[i].u)->key != find(G.E[i].v)->key) {
			tab_add(A, G.E[i]);
			Union(G.E[i].u, G.E[i].v);
		}
}

int main() {
	printf("Algorytm Kruskala znajduj±cy minimalne drzewo spinaj±ce.\n");
	printf("Graf:\n");
	init_graph();
	display_graph(G);

	mst_kruskal(G);

	display_spanning_tree(A);

	return 0;
}

