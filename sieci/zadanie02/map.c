#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main() {
	int i;
	char c;
	char query[255];
	char liczba1[32];
	char liczba2[32];
	char kolor[32];
	int x, y;
	const HALFPIC=75;

	printf("Content-type: text/html\n\n");
	printf("<HTML>\n");
	printf("<BODY>\n");

	/* pobieramy wspolrzedna x z formularza */
	while ((c=getchar()) != '=');
	for (i=0; (c=getchar()) != '&'; i++) {
		liczba1[i] = c;
	}
	liczba1[i] = '\0';
	x = atoi(liczba1);

	/* pobieramy wspolrzedna y z formularza */
	while ((c=getchar()) != '=');
	for (i=0; (c=getchar()) != EOF; i++) {
		liczba2[i] = c;
	}
	liczba2[i] = '\0';
	y = atoi(liczba2);

	printf("x=%d, y=%d<br>\n", x, y);

	if (x < HALFPIC && y < HALFPIC)
	  strcpy(kolor, "bialy");
	else if (x > HALFPIC && y < HALFPIC)
	  strcpy(kolor, "niebieski");
	else if (x < HALFPIC && y > HALFPIC)
	  strcpy(kolor, "czarny");
	else if (x > HALFPIC && y > HALFPIC)
	  strcpy(kolor, "szary");

	printf("%s\n", kolor);
	printf("<br><br><a href=\"formpic.html\">Powrot</a>");

//	scanf("%100s", query);
//	printf("%s\n", query);
//	printf("Podano napis %s\n", getenv("QUERY_STRING"));

	printf("</BODY>\n");
	printf("</HTML>\n");

	return 0;
}

