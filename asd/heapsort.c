# include <stdio.h>
# define HEAPSIZE 10

int heapsize(int a[]);
void heapify(int a[], int i, int heapsize);
void heapsort(int a[]);
void buildheap(int a[]);

void heap_display(int a[]);

int main(){
	int a[]={1,3,5,7,8,6,7,1,4,3,3};

	printf("Sortowanie HEAPSORT\n\n");
	heap_display(a);
	putchar('\n');

	buildheap(a);
	printf("utworzono kopiec\n");
	heapsort(a);

	putchar('\n');
	heap_display(a);

	return 0;
}

void buildheap(int a[]){
	int i;

	for(i=HEAPSIZE/2; i>=0; i--){
		heapify(a,i,HEAPSIZE);
		heap_display(a);
	}
}

void heapsort(int a[]){
	int i,tmp;
	int heapsize=HEAPSIZE;

	for(i=heapsize; i>=1; i--){
		tmp=a[heapsize];
		a[heapsize]=a[0];
		a[0]=tmp;
		heapsize=heapsize-1;
		heapify(a,0,heapsize);
		heap_display(a);
	}
}

void heapify(int a[], int i, int heapsize){
	int l,r,largest,tmp;

	l=2*i+1;
	r=2*i+2;
	if ((l<=heapsize) && (a[l]>a[i]))
		largest=l;
	else
		largest=i;
	if ((r<=heapsize) && (a[r]>a[largest]))
		largest=r;
	if (largest != i){
		tmp=a[i];
		a[i]=a[largest];
		a[largest]=tmp;
		heapify(a,largest,heapsize);
	}
}

void heap_display(int a[]){
	int i;

	for(i=0; i <= HEAPSIZE; i++)
		printf("%d ", a[i]);
	putchar('\n');
}
