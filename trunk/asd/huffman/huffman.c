/* Algorytm Huffmana - kompresja plikow poprzez przypisanie kazdemu znakowi
 * kodu w zaleznosci od czestotliwosci jego wystepowania; im czestszy znak,
 * tym krotszy kod.
 * Implementacja drzewa kodu jako struktury dowiazaniowej, a kolejki
 * ze wskaznikami do wezlow za pomoca listy nieuporzadkowanej przechowywanej
 * w tablicy. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define NIL NULL /* element pusty jest pustym wskaznikiem */
#define ASCII_COUNT 256 /* ilosc wszystkich znakow */
#define MAX_CODE_LEN 32


/* jeden wezel z drzewa */
typedef struct node{
	char c;
	int f; /* czestotliwosc wystepowania znaku c */
	struct node *left;
	struct node *right;
}t_node;

/* wyznacza poczatek drzewa ze wskaznikiem na korzen */
typedef struct tree{
	t_node *root;
}t_tree;


/* procedura tworzaca wezel
 * uzycie: wskaznik_na_wezel=makenode(znak, czestotliwosc); */
t_node *makenode(char ch, int freq);


t_node *makenode(char ch, int freq){
	t_node *node;

	node=(t_node *)malloc(sizeof(t_node));
	if (node == NULL){
		printf("makenode: blad pamieci\n");
		return NULL;
	}
	node->left=NIL;
	node->right=NIL;
	node->c=ch;
	node->f=freq;

	return node;
}

/* zeruje tablice */
void table_init(int table[], int length){
	int i;

	for (i=0; i < length; i++)
		table[i]=0;
}

int read_frequencies(int frequencies[], char fileName[]){
	FILE *f;
	int i;
	char ch;

	f=fopen(fileName, "r");
	if (f == NULL)
	    return -1;
	else {
		for (i=0; (ch=fgetc(f)) != EOF; i++){
			frequencies[ (int)ch ]++;
		}
	}
	fclose(f);
	return 0;
}

void queue_init(t_node *queue[]){
	int i;
	
	for (i=0; i < ASCII_COUNT; i++){
		queue[i]=NULL;
	}
}

int queue_length(t_node *queue[]){
	int i;
	for (i=0; queue[i] != NULL; i++);
	return i;
}

void queue_insert(t_node *queue[], t_node *node){
	queue[queue_length(queue)]=node;
}

void insert_frequencies(t_node *queue[], int frequencies[]){
	int i;
	t_node *w;

	for (i=0; i < ASCII_COUNT; i++)
		if (frequencies[i] != 0) {
			w=makenode((char)i, frequencies[i]);
			queue_insert(queue, w);
		}
}

void display_queue(t_node *queue[]){
	int i;

	for (i=0; queue[i] != NULL; i++){
	    if (queue[i]->c == '\n')
		printf("queue : ('\\n', %d)\n", queue[i]->f);
	    else
		printf("queue : ('%c', %d)\n", queue[i]->c, queue[i]->f);
	}
}

t_node *queue_extract_min(t_node *queue[]){
	int i;
	int min_index=0;
	t_node *min_node=queue[0];

	for (i=1; queue[i] != NULL; i++){
		if (queue[i]->f < min_node->f){
			min_node = queue[i];
			min_index=i;
		}
	}
	/* nadpisujemy najmniejszy element ostatnim w tablicy */
	queue[min_index]=queue[i-1];
	queue[i-1]=NULL;

	return min_node;
}

void tree_init(t_tree *T){
	T->root=NIL; /* zaznaczamy, ze drzewo jest puste (brak korzenia) */
}

t_node *huffman(t_tree *codeTree, t_node *queue[], int frequencies[]){
	int i, n;
	t_node *x, *y, *z;

	insert_frequencies(queue, frequencies);
	n=queue_length(queue);
	for (i=1; i < n; i++){
		z=makenode(-1, -1); /* pusty wezel */
		x=queue_extract_min(queue);
		y=queue_extract_min(queue);
		z->left=x;
		z->right=y;
		z->f = x->f + y->f;
		queue_insert(queue, z);
	}
	return queue_extract_min(queue); /* bierzemy jedyny ktory zostal */
}

void get_codes(t_node *x, char bit, int code_len,
			char code[], char codes[ASCII_COUNT][MAX_CODE_LEN]){
	if (x != NIL){
		code[code_len-1]=bit;
		if (x->left == NIL && x->right == NIL) { /* jesli lisc */
			strncpy(codes[(int)x->c], code, code_len);
		}
		if (x->left != NIL)
			get_codes(x->left, '0', code_len+1, code, codes);
		if (x->left != NIL)
			get_codes(x->right, '1', code_len+1, code, codes);
	}
}

void display_codes(int frequencies[], char codes[ASCII_COUNT][MAX_CODE_LEN]){
	int i;

	for (i=0; i < ASCII_COUNT; i++){
		if (frequencies[i] > 0){
			if ((char)i == '\n')
				printf("'\\n'\t%d\t\t%s\n",
						frequencies[i], codes[i]);
			else
				printf("'%c'\t%d\t\t%s\n", (char)i,
					       	frequencies[i], codes[i]);
		}
	}
}

void display_info(int frequencies[], char codes[ASCII_COUNT][MAX_CODE_LEN]){
	int i;
       	int original_size=0;
       	int packed_size=0;

	for (i=0; i < ASCII_COUNT; i++)
		original_size+=frequencies[i];
	for (i=0; i < ASCII_COUNT; i++)
		packed_size+=strlen(codes[i])*frequencies[i];
	packed_size/=8;

	putchar('\n');
	printf("Oryginalny rozmiar pliku: %d bajtow\n", original_size);
	printf("Po zakodowaniu plik zajalby %d bajtow", packed_size);
	double p=((double)packed_size/original_size)*100;
	printf(" (%.1f%% oryginalu)\n", p);
}


int main(int argc, char **argv) {
	/* drzewo kodu wyznaczone przez wskaznik na korzen */
	t_tree *codeTree;
	/* kolejka jako tablica wskaznikow do wezlow */
	t_node *queue[ASCII_COUNT];
	int frequencies[ASCII_COUNT];
	char code[MAX_CODE_LEN]; /* przechowuje kod znaku */
	char codes[ASCII_COUNT][MAX_CODE_LEN];

	table_init(frequencies, ASCII_COUNT);
	tree_init(codeTree);
	queue_init(queue);

    if (argc > 1 && read_frequencies(frequencies, argv[1]) != -1){
	codeTree->root=huffman(codeTree, queue, frequencies);
	get_codes(codeTree->root, 'X', 0, code, codes);
	printf("znak:\tczestosc:\tkod Huffmana:\n");
	display_codes(frequencies, codes);
	display_info(frequencies, codes);
    }
    else if (!(argc > 1)){
	printf("Uzycie: huffman [plik]\n");
    }
    else{
	fprintf(stderr, "blad: nie mozna otworzyc pliku %s\n", argv[1]);
    }

	return 0;
}

