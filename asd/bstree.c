/* Binary search tree - drzewo poszukiwan binarnych */
/* implementaqcja wstawiania, przechodzenia inorder, szukania i usuwania elementow */

#include <stdio.h>
#include <stdlib.h>

#define NIL NULL /* element pusty jest pustym wskaznikiem */

/* jeden wezel z drzewa */
typedef struct node{
	int key;
	struct node *left;
	struct node *right;
	struct node *parent;
}t_node;

/* wyznacza poczatek drzewa ze wskaznikiem na korzen */
typedef struct tree{
	t_node *root;
}t_tree;

/* procedura tworzaca wezel
 * uzycie: wezel=makenode(liczba); */
t_node *makenode(int k);

/* przygotowanie drzewa do wpisywania nowych elementow */
void tree_initialize(t_tree *T);

void tree_insert_key(t_tree *T, int k);

void tree_insert(t_tree *T, t_node *z);

/* przejscie drzewa metoda inorder - argumentem jest wezel bedacy korzeniem */
void inorder_tree_walk(t_node *x);

/* wypisuje, czy w drzewie jest wezel o danym kluczu */
void is_on_tree(t_tree *T, int k);

/* zwraca wezel z kluczem k; jesli takiego nie ma, zwraca NIL */
t_node *tree_search(t_node *x, int k);

/* usuwa z drzewa wezel o zadanym kluczu */
void tree_delete_key(t_tree *T, int k);

/* usuwa z drzewa zadany wezel */
t_node *tree_delete(t_tree *T, t_node *z);

t_node *tree_successor(t_node *x);

t_node *tree_minimum(t_node *x);

/* oproznia drzewo o korzeniu x */
void tree_empty(t_tree *T, t_node *x);

int main() {
	t_tree T; /* drzewo wyznaczone przez wskaznik na korzen */

	tree_initialize(&T);

	printf("Drzewo poszukiwan binarnych - przykladowa implementacja\n\n");
	printf("wstawiam 2\n");
	tree_insert_key(&T, 2);
	printf("wstawiam 7\n");
	tree_insert_key(&T, 7);
	printf("wstawiam 3\n");
	tree_insert_key(&T, 3);
	printf("wstawiam 5\n");
	tree_insert_key(&T, 5);
	printf("wstawiam 4\n");
	tree_insert_key(&T, 4);
	printf("wstawiam 5\n");
	tree_insert_key(&T, 5);
	putchar('\n');

	printf("przejscie inorder: ");
	inorder_tree_walk((&T)->root);
	putchar('\n');
	putchar('\n');

	printf("szukam 5... ");
	is_on_tree(&T, 5);
	printf("szukam 6... ");
	is_on_tree(&T, 6);
	printf("szukam 2... ");
	is_on_tree(&T, 2);
	printf("szukam 7... ");
	is_on_tree(&T, 7);
	printf("szukam 1... ");
	is_on_tree(&T, 1);
	putchar('\n');

	printf("usuwam 4\n");
	tree_delete_key(&T, 4);
	printf("usuwam 5\n");
	tree_delete_key(&T, 5);
	printf("usuwam 6\n");
	tree_delete_key(&T, 6);
	printf("usuwam 2\n");
	tree_delete_key(&T, 2);
	putchar('\n');

	printf("przejscie inorder: ");
	inorder_tree_walk((&T)->root);
	putchar('\n');
	putchar('\n');

	printf("oprozniam drzewo\n");
	tree_empty(&T, (&T)->root);
	printf("przejscie inorder: ");
	inorder_tree_walk((&T)->root);
	putchar('\n');

	return 0;
}

t_node *makenode(int k){
	t_node *node;

	node=(t_node *)malloc(sizeof(t_node));
	if (node == NULL){
		printf("makenode: blad pamieci\n");
		return NULL;
	}
	node->key=k;

	return node;
}

void tree_initialize(t_tree *T){
	T->root=NIL; /* zaznaczamy, ze drzewo jest puste (brak korzenia) */
}

void tree_insert_key(t_tree *T, int k){
	t_node *z;

	z=makenode(k);
	tree_insert(T, z);
}

void tree_insert(t_tree *T, t_node *z){
	t_node *x, *y;

	y=NIL;
	x=T->root;
	while (x != NIL){
		y=x;
		if (z->key < x->key)
			x=x->left;
		else
			x=x->right;
	}
	z->parent=y;
	if (y == NIL)
		T->root=z;
	else if (z->key < y->key)
		y->left=z;
	else
		y->right=z;
}

void inorder_tree_walk(t_node *x){
	if (x != NIL){
		inorder_tree_walk(x->left);
		printf("%d ", x->key);
		inorder_tree_walk(x->right);
	}

}

void is_on_tree(t_tree *T, int k){
	if (tree_search(T->root, k))
		printf("znaleziono!\n");
	else
		printf("nie znaleziono\n");
}

t_node *tree_search(t_node *x, int k){
	if (x == NIL || k == x->key)
		return x;
	if (k < x->key)
		return tree_search(x->left, k);
	else
		return tree_search(x->right, k);
}

void tree_delete_key(t_tree *T, int k){
	if (tree_search(T->root, k)) /* jesli znaleziono klucz */
		free(tree_delete(T, tree_search(T->root, k)));
	else
		printf("tree_delete_key: klucza %d nie znaleziono\n", k);
}

t_node *tree_delete(t_tree *T, t_node *z){
	t_node *x, *y;

	if (z->left == NIL || z->right == NIL)
		y=z;
		else
			y=tree_successor(z);
	if (y->left != NIL)
		x=y->left;
		else
			x=y->right;
	if (x != NIL)
		x->parent=y->parent;
	if (y->parent == NIL)
		T->root=x;
		else if (y == y->parent->left)
			y->parent->left=x;
			else
				y->parent->right=x;
	if (y != z)
		z->key=y->key;
	
	return y;
}

t_node *tree_successor(t_node *x){
	t_node *y;

	if (x->right != NIL)
		return tree_minimum(x->right);
	y=x->parent;
	while (y != NIL && x == y->right){
		x=y;
		y=y->parent;
	}
	return y;
}

t_node *tree_minimum(t_node *x){
	while (x->left != NIL)
		x=x->left;
	return x;
}

void tree_empty(t_tree *T, t_node *x){
	if (x != NIL){
		tree_empty(T, x->left);
		tree_empty(T, x->right);
		free(tree_delete(T, x));
	}
}
