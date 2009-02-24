/* najwazniejsze dane dla programu:
 * mutability - prawdopodobienstwo zmutowania genu wynosi 1/mutability
 * generation - ilosc nowych osobnikow na pokolenie; mozna podac jako parametr programu */

# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <time.h>

# define MAXSTR 256 /* maksymalna dlugosc napisu */
# define SHOWEVERY 5 /* czestotliwosc pokazywania wynikow */
# define MUTATE 0 /* jesli wylosowano ta wartosc, to zmutowac dany gen */

/* mierzy przystosowanie osobnika (czyli podobienstwo do danego wzoru) */
int adaptation(char s[], char target[]);

void lowercase(char s[]);

void removenonletter(char s[]);

/* pobiera napis z klawiatury i zwraca jego dlugosc */
int getstring(char s[]);

/* zwraca losowa litere lub spacje */
char randomletter();

/* losuje napis s o dlugosci n */
void randomstr(char s[], int n);

int main(int argc, char *argv[]){
	int i, j, k, n;
	char target[MAXSTR], firstparent[MAXSTR];
	char neworganism[MAXSTR], bestorganism[MAXSTR], parent[MAXSTR];
	int mutability, generation=200;

	if (argc == 2) /* jesli podano jeden parametr, bedzie on iloscia osobnikow na pokolenie */
		generation=atoi(argv[1]);

	srand(time(NULL));

	printf("Program ilustruje dzialanie selekcji kumulatywnej. W kazdym pokoleniu pojawiaja\n");
	printf("sie mutanty, z ktorych najpodobniejszy do wzoru daje poczatek nastepnym.\n");
	printf("Ilosc osobnikow w pokoleniu mozna podac jako parametr programu (domyslnie 200).\n");
	printf("Pierwowzor programu opisany jest w ksiazce Richarda Dawkinsa \"Slepy Zegarmistrz\"\n\n");
	printf("Podaj napis, ktory bedzie naszym docelowym \"osobnikiem\":\n");
	n=getstring(target); /* n - dlugosc osobnika docelowego */
	lowercase(target);
	removenonletter(target);
	while (target[n-1] == ' ') /* usun space na koncu napisu */
		n--;
	target[n]='\0';

	randomstr(firstparent, n); /* tworzenie poczatkowego przypadkowego osobnika */

	printf("\nWzorcowy osobnik (po sformatowaniu):\n");
	printf("\t\t%s\n\n", target);
	printf("Poczatkowy przypadkowo utworzony osobnik:\n");
	printf("\t\t%s\n\n", firstparent);

	printf("pokolenie\twyglad osobnika\n\n");

	mutability=n; /* optymalna mutabilnosc jest mniej wiecej rowna dlugosci napisu */
	strcpy(parent, firstparent);
	for (i=0; (strcmp(target, parent)); i++){ /* jeden obrot petli to jedno pokolenie */
		if (i%SHOWEVERY == 0) /* co ile pokolen wyswietlac najlepiej przystosowanego osobnika */
			printf("%d\t\t%s\n", i, parent);
		strcpy(bestorganism, parent);
		for (k=0; k < generation; k++){ /* niech w jednym pokoleniu bedzie generation osobnikow */
			strcpy(neworganism, parent); /* powstaje nowy osobnik */
			for (j=0; neworganism[j] != '\0'; j++)
				if ((rand()%mutability) == MUTATE) /* czy zmutowac dany gen (litere) */
					neworganism[j]=randomletter();
			/* jesli osobnik lepiej przystosowany, staje sie nowym najlepszym */
			if (adaptation(neworganism, target) > adaptation(bestorganism, target))
				strcpy(bestorganism, neworganism);
		}
		if (adaptation(bestorganism, target) > adaptation(parent, target))
			strcpy(parent, bestorganism); /* rodzicem zostanie najlepszy mutant */
	}

	printf("\nRezultat koncowy po %d pokoleniach:\n", i);
	printf("\t\t%s\n", parent);
	return 0;
}

int adaptation(char s[], char target[]){
	int i, j=0;

	for (i=0; target[i] != '\0'; i++)
		if (s[i]==target[i])
			j++;
	return j;
}

int getstring(char s[]){
	int i;
	char c;

	for (i=0; (c=getchar()) != '\n' && c != EOF; i++)
		s[i]=c;
	s[i]='\0';
	return i;
}

void lowercase(char s[]){
	int i;

	for (i=0; s[i] != '\0'; i++)
		if (s[i] <= 'Z' && s[i] >= 'A')
			s[i]+='a'-'A';
}

void removenonletter(char s[]){
	int i;

	for (i=0; s[i] != '\0'; i++)
		if (s[i] > 'z' || s[i] < 'a')
			s[i]=' ';
}

void randomstr(char s[], int n){
	int i;

	for (i=0; i < n; i++){
		s[i]=randomletter();
		/* niech pierwsza i ostatnia litera nie beda spacjami */
		if (i == 0 || i == n-1)
			while (s[i] == ' ')
				s[i]=randomletter();
	}
	s[i]='\0';
}

char randomletter(){
	char c;

	c=rand()%27 + 'a';   /* losujemy jedna litere wiecej, zeby */
	if (c == 'z' + 1)  /* miec miejsce na spacje */
		c=' ';
	return c;
}
