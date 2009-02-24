# include <stdio.h>
# include <stdlib.h>
# include <time.h>
# include "sort.h"

# define MLD 1000000000.0

void testfile(Tdate a[], char filename[]){
	int n, c;
	double timeqs, timeqsw, timediff, percdiff;
	extern int hypothesis;

	/* Testowanie QS dla danego pliku */
	n=readdatesfile(a, filename); /* wczytaj daty do tablicy */
	printf("===========================================================================\n\n");
	printf("Sortowanie dla %d dat\n", n);
	printf("Plik zrodlowy: %s\n\n", filename);
	printf("algorytm\tczas (s)\troznica s / %%\t\ttest poprawnosci\n");

	timeqs=timeqsortnormal(a, n); /* mierzenie czasu dzialania algorytmu */

	printf("QS\t\t%f\t\t\t\t", timeqs);
	if (issorted(a))
		printf("OK");
	else
		printf("Niezaliczony");
	putchar('\n');

	/* Testowanie QSW dla danego pliku dla kolejnych wartosci c */
	for (c=10; c <= 80; c+=10){
		n=readdatesfile(a, filename); /* wczytaj daty do tablicy */
		timeqsw=timeqsortw(a, n, c); /* mierzenie czasu dzialania algorytmu */

		timediff=timeqsw-timeqs;
		if (timediff < 0) /* liczymy ilosc zwyciestw dla algorytmow */
			hypothesis++;
		else
			hypothesis--;
		percdiff=(timeqsw/timeqs-1)*100; /* roznica procentowa czasow */
		if (percdiff < 0)
			percdiff*=(-1);
		if (percdiff < 9.95)
			printf("QSW (c=%02d)\t%f\t%+f /  %.1f\t", c, timeqsw, timediff, percdiff);
		else
			printf("QSW (c=%02d)\t%f\t%+f / %.1f\t", c, timeqsw, timediff, percdiff);
		if (issorted(a))
			printf("OK");
		else
			printf("Niezaliczony");
		putchar('\n');

	}
	putchar('\n');
}

double timeqsortnormal(Tdate a[], int n){
	int p=0;
	int r=n-1;
	struct timespec tp0, tp1;
	double time=0;

	clock_gettime(CLOCK_REALTIME, &tp0);
	qsortnormal(a,p,r); 
	clock_gettime(CLOCK_REALTIME, &tp1);

	time=tp1.tv_sec + tp1.tv_nsec/MLD - (tp0.tv_sec + tp0.tv_nsec/MLD);
	return time;
}

double timeqsortw(Tdate a[], int n, int c){
	int p=0;
	int r=n-1;
	struct timespec tp0, tp1;
	double time=0;

	clock_gettime(CLOCK_REALTIME, &tp0);
	qsortw(a,p,r,c); 
	clock_gettime(CLOCK_REALTIME, &tp1);

	time=tp1.tv_sec + tp1.tv_nsec/MLD - (tp0.tv_sec + tp0.tv_nsec/MLD);
	return time;
}

int issorted(Tdate a[]){
	int i;
	for(i=0; a[i+1].year > 0 && a[i+1].month > 0 && a[i+1].day > 0; i++)
		if (comparedates(a[i], a[i+1]) > 0) /* jesli poprzedni > nastepny */
			return 0;
	return 1;
}

int readdatesfile(Tdate a[], char file[]){
	FILE *f;
	int i=-1;

	if ((f=fopen(file, "r")) != NULL){
		do{
			i++;
			a[i].day=getymd(f);
			a[i].month=getymd(f);
			a[i].year=getymd(f);
		} while(a[i].day > 0 && a[i].month > 0 && a[i].year > 0
		&& i < MAXTABSIZE);
		fclose(f);
		if (i >= MAXTABSIZE)
			return -2;
		else
			return i; /* zworc ilosc wczytanych dat */
	}
	else
		return -1;
}

int getymd(FILE *f){
	char ymd[8];
	int i;
	char c;

	c=fgetc(f);
	if (c == EOF) return -1;
	for(i=0; c != '-' && c != '\n' && c != ' ' && c != EOF; i++){
		ymd[i]=c;
		c=fgetc(f);
	}
	if (c == EOF) return -1;
	ymd[i]='\0';

	return atoi(ymd);
}

int isdatesfile(char file[]){
	FILE *f;
	char c;

	if ((f=fopen(file, "r")) == NULL)
		return -1;
	else
		while ((c=fgetc(f)) != EOF)
			if (c > '9' || c < '0')
				if (c != '-' && c != '\n' && c != ' '){
					fclose(f);
					return 0;
				}
	return 1;
}

void printtable(Tdate a[]){
	int i;
	for(i=0; a[i].year > 0 && a[i].month > 0 && a[i].day > 0; i++)
		printf("%02d-%02d-%d ", a[i].day, a[i].month, a[i].year);
	putchar('\n');
}
