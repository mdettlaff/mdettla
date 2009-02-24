#include <stdio.h>
#include <string.h>

#define LEFT '-'
#define UP '|'
#define UP_LEFT '\\'
#define INFINITY 9999
#define MAX 255

char	b[MAX][MAX]; // tablica ze strzalkami
int	c[MAX][MAX]; // tablica z dlugosciami

void	LCS_REC(char X[], char Y[]);
int	Lookup(char X[], char Y[], int i, int j);
void	Print_LCS(char X[], int i, int j);
void	Display_b(char X[], char Y[]);
void	Display_c(char X[], char Y[]);
void	init_b();

int main(){
	char X[]="ABCBDAB";
	char Y[]="BDCABA";
	int m=strlen(X);
	int n=strlen(Y);
	init_b();

	printf("X: %s\n", X);
	printf("Y: %s\n", Y);

	LCS_REC(X, Y);

	Display_c(X, Y);
	Display_b(X, Y);

	printf("NWP(X,Y): ");
	Print_LCS(X, m, n);
	putchar('\n');

	return 0;
}

void LCS_REC(char X[], char Y[]){
	int i, j, n, m;

	m=strlen(X);
	n=strlen(Y);
	for (i=0; i <= m; i++)
		for (j=0; j <= n; j++)
			c[i][j]=INFINITY;
	for (i=0; i <= m; i++)
		c[i][0]=0;
	for (j=0; j <= n; j++)
		c[0][j]=0;
	Lookup(X, Y, m, n);
}

int Lookup(char X[], char Y[], int i, int j){
	int c1, c2;

	if (c[i][j] < INFINITY)
		return c[i][j];
	if (X[i-1] == Y[j-1]){ // indeksowanie od zera w C!
		c[i][j]=Lookup(X, Y, i-1, j-1)+1;
		b[i][j]=UP_LEFT;
	}
	else{
		c1=Lookup(X, Y, i-1, j);
		c2=Lookup(X, Y, i, j-1);
		if (c1 >= c2){
			c[i][j]=c1;
			b[i][j]=LEFT;
		}
		else{
			c[i][j]=c2;
			b[i][j]=UP;
		}
	}
	return c[i][j];
}

void Print_LCS(char X[], int i, int j){
	if (i == 0 || j == 0)
		return;
	if (b[i][j] == UP_LEFT){
		Print_LCS(X, i-1, j-1);
		putchar(X[i-1]); // indeksowanie od zera w C!
	}
	else if (b[i][j] == UP)
		Print_LCS(X, i-1, j);
	else
		Print_LCS(X, i, j-1);
}

void Display_b(char X[], char Y[]){
	int m=strlen(X);
	int n=strlen(Y);
	int i, j;

	putchar('\n');
	printf("tablica b:\n");
	printf("  ");
	for (i=0; i < n; i++)
		printf("%c ", Y[i]);
	putchar('\n');
	for (i=1; i < m+1; i++){
		printf("%c ", X[i-1]);
		for (j=1; j < n+1; j++)
			printf("%c ", b[i][j]);
		putchar('\n');
	}
	putchar('\n');
}

void Display_c(char X[], char Y[]){
	int m=strlen(X)+1;
	int n=strlen(Y)+1;
	int i, j;

	putchar('\n');
	printf("tablica c:\n");
	printf("\t\t");
	for (i=0; i < n-1; i++)
		printf("%c\t", Y[i]);
	printf("\n\t");
	for (i=0; i < n; i++)
		printf("0\t");
	putchar('\n');
	for (i=1; i < m; i++){ // pierwszy pominelismy, bo to same zera
		printf("%c\t", X[i-1]);
		for (j=0; j < n; j++)
			if (c[i][j] == INFINITY)
				printf("INFINIT\t");
			else
				printf("%d\t", c[i][j]);
		putchar('\n');
	}
	putchar('\n');
}

void init_b(){
	int i, j;

	for (i=0; i < MAX; i++)
		for (j=0; j < MAX; j++)
			b[i][j]=' ';
}

