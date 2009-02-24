# include <stdio.h>
# include "sort.h"

void qsortnormal(Tdate a[], int p, int r){
	int q;
	if((p < r)){
		q=partition(a,p,r);
		qsortnormal(a,p,q);
		qsortnormal(a,q+1,r);
	}
}

void qsortw(Tdate a[], int p, int r, int c){
	qsortlimit(a,p,r,c);
	insertsort(a,p,r); /* Uzupelnianie sortowaniem przez wstawianie */
}

void qsortlimit(Tdate a[], int p, int r, int c){
	int q;
	if((p < r) && (r-p+1 >= c)){ /* nie sortuje tablic krotszych niz c */
		q=partition(a,p,r);
		qsortlimit(a,p,q,c);
		qsortlimit(a,q+1,r,c);
	}
}

int partition(Tdate a[],int p,int r){ /* limited partition */
	int i,j;
	Tdate x;

	x=a[r];
	i=p-1;
	for(j=p; j<=r; j++)
	if(comparedates(x, a[j]) >= 0){
		i=i+1;
		swap(&a[i], &a[j]);
	}
	if(i<r)
		return i;
	else
		return (i-1);
}

void insertsort(Tdate a[], int p, int r){
	int j,k;
	Tdate current;

	for (k=p+1; k<=r; k++){
		current=a[k];
		j=k-1;
		while ((j >= p) && (comparedates(current, a[j]) < 0)){ /*current < a[j]*/
			a[j+1]=a[j];
			--j;
		}
		a[j+1]=current;
	}
}

int comparedates(Tdate a, Tdate b){
	if (a.year > b.year)
		return 1;
	else if (a.year < b.year)
		return -1;
	else if (a.month > b.month)
		return 1;
	else if (a.month < b.month)
		return -1;
	else if (a.day > b.day)
		return 1;
	else if (a.day < b.day)
		return -1;
	else
		return 0;
}

void swap(Tdate *a, Tdate *b){
	Tdate tmp;
	tmp=*a;
	*a=*b;
	*b=tmp;
}
