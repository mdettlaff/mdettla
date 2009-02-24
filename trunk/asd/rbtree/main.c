/* ----------------- program testujacy ----------------------- */
// kompilowac razem z rbtree.o:  gcc main.c rbtree.o

#include <time.h>
#include "rbtree.h"

int main(){
     char command[255];
     nilInit();
     int n;
     Tdrzewa T;
     printf("dostepne komendy:\n");
     printf("l x - obrot w lewo wokol wezla x\n");
     printf("r x - obrot w prawo wokol wezla x\n");
     printf("i x - wstawienie wartosci x\n");
     printf("d x - usuniecie wartosci x\n");
     printf("b - wyswietlenie czarnej i zwyklej wysokosci drzewa");
     printf("t - test porownujacy wysokosc zwykla i wysokosc czarna\n");
     printf("q - wyjscie\n");
     putchar('\n');
     printf("podaj wysokosc drzewa (0..5) ");
     scanf("%d", &n);
     putchar('\n');
     (&T)->root=buduj(n-1);
     drukuj((&T)->root);

     /* czytanie polecen z klawiatury */
     while (getinput(&T, command)){
	printf("================================================================================\n");
	drukuj((&T)->root);
     }

     return 0;
}


int getinput(Tdrzewa *T, char command[]){
	int height;

	scanf("%s", command);
	if (command[0] == 'q')
		return 0;
	else if (command[0] == 'r'){ // right rotate
		scanf("%s", command);
		right_rotate(T, tree_search(T->root, atoi(command)));
	}
	else if (command[0] == 'l'){ // left rotate
		scanf("%s", command);
		left_rotate(T, tree_search(T->root, atoi(command)));
	}
	else if (command[0] == 'i'){ // rb_insert
		scanf("%s", command);
		rb_insert_klucz(T, atoi(command));
	}
	else if (command[0] == 'd'){ // rb_delete
		scanf("%s", command);
		rb_delete_klucz(T, atoi(command));
	}
	else if (command[0] == 't') // test czarnej wysokosci drzewa
		height_test(T);
	else if (command[0] == 'b'){ // pokazanie czarnej i zwyklej wysokosci drzewa
		height=0;
		tree_height(T->root, 1, &height);
		printf("wysokosc drzewa: %d, wysokosc czarna: %d\n", height, rb_black_height(T));
	}
	return 1;
}


/* funkcje dla drzew czerwono-czarnych */

void height_test(Tdrzewa *T){
	int i, k, tmp1, tmp2, maxkey, height, node_count;

	srand(time(NULL));
	maxkey=tree_maximum(T->root)->klucz;
	printf("usuwanie losowo wybranych wezlow:\n");
	printf("ilosc wezlow:\twysokosc:\twysokosc czarna:\n");
	node_count=0;
	tree_node_count(T->root, &node_count);
	height=0;
	tree_height(T->root, 1, &height);
	printf("%d\t\t%d\t\t%d\n", node_count, height, rb_black_height(T)); /* sytuacja na poczatku */
	for (i=0; T->root != nil; i++) /* usuwamy wezly az oproznimy drzewo */
		if (tree_search(T->root, k=rand()%(maxkey+1)) != nil){ /* jesli znaleziono klucz */
			tmp1=height;
			tmp2=rb_black_height(T);
			free(rb_delete(T, tree_search(T->root, k)));
			height=0;
			tree_height(T->root, 1, &height);
			node_count=0;
			tree_node_count(T->root, &node_count);
			if (height != tmp1 || rb_black_height(T) != tmp2)
				printf("%d\t\t%d\t\t%d\n", node_count, height, rb_black_height(T));
	}
}

void rb_insert_klucz(Tdrzewa *T, int k){
	Twezla *z;

	z=makenode(k);
	if (tree_search(T->root, k) != nil) /* jesli znaleziono klucz */
		printf("rb_insert_klucz: klucz %d jest juz w drzewie\n", k);
	else
		rb_insert(T, z);
}

void rb_delete_klucz(Tdrzewa *T, int k){
	if (tree_search(T->root, k) != nil) /* jesli znaleziono klucz */
		free(rb_delete(T, tree_search(T->root, k)));
	else
		printf("rb_delete_klucz: klucza %d nie znaleziono\n", k);
}

void rb_insert(Tdrzewa *T, Twezla *x){
	Twezla *y;

	tree_insert(T, x);

	/* naprawianie struktury drzewa czerwono-czarnego */
	x->kolor=RED;
	while (x != T->root && x->p->kolor == RED){
		if (x->p == x->p->p->left){
			y=x->p->p->right;
			if (y->kolor == RED){
				x->p->kolor=BLACK;
				y->kolor=BLACK;
				x->p->p->kolor=RED;
				x=x->p->p;
			}
			else{
				if (x == x->p->right){
					x=x->p;
					left_rotate(T, x);
				}
				x->p->kolor=BLACK;
				x->p->p->kolor=RED;
				right_rotate(T, x->p->p);
			}
		}
		else{ /* jest jak w if, ale zamienione right i left */
			y=x->p->p->left;
			if (y->kolor == RED){
				x->p->kolor=BLACK;
				y->kolor=BLACK;
				x->p->p->kolor=RED;
				x=x->p->p;
			}
			else{
				if (x == x->p->left){
					x=x->p;
					right_rotate(T, x);
				}
				x->p->kolor=BLACK;
				x->p->p->kolor=RED;
				left_rotate(T, x->p->p);
			}
		}
	}
	T->root->kolor=BLACK;
}

Twezla *rb_delete(Tdrzewa *T, Twezla *z){
	Twezla *x, *y;

	if (z->left == nil || z->right == nil)
		y=z;
	else
		y=tree_successor(z);
	if (y->left != nil)
		x=y->left;
	else
		x=y->right;
	x->p=y->p;
	if (y->p == nil)
		T->root=x;
	else if (y == y->p->left)
		y->p->left=x;
	else
		y->p->right=x;
	if (y != z)
		z->klucz=y->klucz;
	if (y->kolor == BLACK)
		rb_delete_fixup(T, x);
	return y;
}

void rb_delete_fixup(Tdrzewa *T, Twezla *x){
	Twezla *w;

	while (x != T->root && x->kolor == BLACK){
		if (x == x->p->left){
			w=x->p->right;
			if (w->kolor == RED){ /* przypadek 1 */
				w->kolor=BLACK;
				x->p->kolor=RED;
				left_rotate(T, x->p);
				w=x->p->right;
			}
			if (w->left->kolor == BLACK && w->right->kolor == BLACK){ /* przypadek 2 */
				w->kolor=RED;
				x=x->p;
			}
			else{
			       	if (w->right->kolor == BLACK){ /* przypadek 3 */
				w->left->kolor=BLACK;
				w->kolor=RED;
				right_rotate(T, w);
				w=x->p->right;
				}
			/* przypadek 4 */
			w->kolor=x->p->kolor;
			x->p->kolor=BLACK;
			w->right->kolor=BLACK;
			left_rotate(T, x->p);
			x=T->root;
			}
		}
		else{ /* tak jak if, ale z zamienionymi right i left */
			w=x->p->left;
			if (w->kolor == RED){ /* przypadek 1 */
				w->kolor=BLACK;
				x->p->kolor=RED;
				right_rotate(T, x->p);
				w=x->p->left;
			}
			if (w->right->kolor == BLACK && w->left->kolor == BLACK){ /* przypadek 2 */
				w->kolor=RED;
				x=x->p;
			}
			else{
			       	if (w->left->kolor == BLACK){ /* przypadek 3 */
				w->right->kolor=BLACK;
				w->kolor=RED;
				left_rotate(T, w);
				w=x->p->left;
				}
			/* przypadek 4 */
			w->kolor=x->p->kolor;
			x->p->kolor=BLACK;
			w->left->kolor=BLACK;
			right_rotate(T, x->p);
			x=T->root;
			}
		}
	}
	x->kolor=BLACK;
}

void left_rotate(Tdrzewa *T, Twezla *x){
	Twezla *y;

	y=x->right;
	x->right=y->left;
	if (y->left != nil)
		y->left->p=x;
	y->p=x->p;
	if (x->p == nil)
		T->root=y;
	else if (x == x->p->left)
		x->p->left=y;
	else
		x->p->right=y;
	y->left=x;
	x->p=y;
}

void right_rotate(Tdrzewa *T, Twezla *x){
	Twezla *y;

	y=x->left;
	x->left=y->right;
	if (y->right != nil)
		y->right->p=x;
	y->p=x->p;
	if (x->p == nil)
		T->root=y;
	else if (x == x->p->right)
		x->p->right=y;
	else
		x->p->left=y;
	y->right=x;
	x->p=y;
}

int rb_black_height(Tdrzewa *T){
	int i=0;
	Twezla *x;
	x=T->root;
	while (x != nil){
		if (x->kolor == BLACK)
			i++;
		x=x->left; /* idziemy ciagle w lewo, bo na kazdej sciezce od liscia do */
	}		   /* korzenia jest tyle samo czarnych wezlow */
	return i;

}


/* funkcje dzialajace tez dla zwyklych drzew binarnych */

Twezla *makenode(int k){
	Twezla *node;

	node=(Twezla *)malloc(sizeof(Twezla));
	if (node == NULL){
		printf("makenode: blad pamieci\n");
		return NULL;
	}
	node->klucz=k;
	node->p=node->left=node->right=nil;

	return node;
}

void tree_insert(Tdrzewa *T, Twezla *z){
	Twezla *x, *y;

	y=nil;
	x=T->root;
	while (x != nil){
		y=x;
		if (z->klucz < x->klucz)
			x=x->left;
		else
			x=x->right;
	}
	z->p=y;
	if (y == nil)
		T->root=z;
	else if (z->klucz < y->klucz)
		y->left=z;
	else
		y->right=z;
}

Twezla *tree_successor(Twezla *x){
	Twezla *y;

	if (x->right != nil)
		return tree_minimum(x->right);
	y=x->p;
	while (y != nil && x == y->right){
		x=y;
		y=y->p;
	}
	return y;
}

void tree_height(Twezla *x, int h, int *height){
	if (x != nil){
		tree_height(x->left, h+1, height);
		if (h > *height)
			*height=h;
		tree_height(x->right, h+1, height);
	}

}

void tree_node_count(Twezla *x, int *node_count){
	if (x != nil){
		tree_node_count(x->left, node_count);
		*node_count=*node_count+1;
		tree_node_count(x->right, node_count);
	}

}

Twezla *tree_minimum(Twezla *x){
	while (x->left != nil)
		x=x->left;
	return x;
}

Twezla *tree_maximum(Twezla *x){
	while (x->right != nil)
		x=x->right;
	return x;
}

Twezla *tree_search(Twezla *x, int k){
	if (x == nil || k == x->klucz)
		return x;
	if (k < x->klucz)
		return tree_search(x->left, k);
	else
		return tree_search(x->right, k);
}
