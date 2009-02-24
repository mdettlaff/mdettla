#include <stdio.h>
#include <string.h>

#define MAX 255

/* w tej wersji nie uzywamy tablicy b zawierajacej strzalki */
int c[MAX][MAX]; // tablica z dlugosciami

void LCS_Length(char X[], char Y[]);
void Print_LCS(char X[], char Y[], int i, int j);
void Display_c(char X[], char Y[]);

int main(){
	char X[]="ABCBDAB";
	char Y[]="BDCABA";
	int m=strlen(X);
	int n=strlen(Y);

	printf("X: %s\n", X);
	printf("Y: %s\n", Y);

	LCS_Length(X, Y);

	Display_c(X, Y);

	printf("NWP(X,Y): ");
	Print_LCS(X, Y, m, n);
	putchar('\n');
	
	return 0;
}

void LCS_Length(char X[], char Y[]){
	int i, j, n, m;

	m=strlen(X);
	n=strlen(Y);
	for (i=1; i <= m; i++)
		c[i][0]=0;
	for (j=0; j <= n; j++)
		c[0][j]=0;
	for (i=1; i <= m; i++)
		for (j=1; j <= n; j++){
			if (X[i-1] == Y[j-1]) // indeksowanie od zera w C!
				c[i][j]=c[i-1][j-1]+1;
			else if (c[i-1][j] >= c[i][j-1])
				c[i][j]=c[i-1][j];
			else
				c[i][j]=c[i][j-1];
		}
}

void Print_LCS(char X[], char Y[], int i, int j){
	if (i == 0 || j == 0)
		return;
	if (X[i-1] == Y[j-1]){ //(b[i][j] == LEFTUP){
		Print_LCS(X, Y, i-1, j-1);
		putchar(X[i-1]); // indeksowanie od zera w C!
	}
	else if (c[i-1][j] >= c[i][j-1])//(b[i][j] == UP)
		Print_LCS(X, Y, i-1, j);
	else
		Print_LCS(X, Y, i, j-1);
}

void Display_c(char X[], char Y[]){
	int m=strlen(X)+1;
	int n=strlen(Y)+1;
	int i, j;

	putchar('\n');
	printf("tablica c:\n");
	printf("    ");
	for (i=0; i < n-1; i++)
		printf("%c ", Y[i]);
	printf("\n  ");
	for (i=0; i < n; i++)
		printf("0 ");
	putchar('\n');
	for (i=1; i < m; i++){ // pierwszy pominelismy, bo to same zera
		printf("%c ", X[i-1]);
		for (j=0; j < n; j++)
			printf("%d ", c[i][j]);
		putchar('\n');
	}
	putchar('\n');
}

