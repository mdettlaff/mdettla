#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main() {
	int i;
	char c;
	double real1, real2;
	char liczba1[255];
	char liczba2[255];
	char dzialanie;

	printf("Content-type: text/html\n\n");
	printf("<HTML>\n");
	printf("<BODY>\n");

	/* pobieramy pierwsza liczbe z formularza */
	while ((c=getchar()) != '=');
	for (i=0; (c=getchar()) != '&'; i++) {
		liczba1[i] = c;
	}
	liczba1[i] = '\0';

	/* pobieramy druga liczbe z formularza */
	while ((c=getchar()) != '=');
	for (i=0; (c=getchar()) != '&'; i++) {
		liczba2[i] = c;
	}
	liczba2[i] = '\0';

	/* pobieramy dzialanie z formularza */
	while ((c=getchar()) != '=');
	dzialanie = getchar();

	if (dzialanie == 'z' && is_zero(liczba2)) {
		printf("Nie mozna dzielic przez zero");
	} else if (!is_float(liczba1) && !is_float(liczba2)) {
		printf("Zadna z liczb nie jest poprawna");
	} else if (!is_float(liczba1)) {
		printf("Pierwsza liczba nie jest poprawna");
	} else if (!is_float(liczba2)) {
		printf("Druga liczba nie jest poprawna");
	} else {
		printf("%s ", liczba1);
		switch (dzialanie) {
			case 's': printf("+"); break;
			case 'r': printf("-"); break;
			case 'i': printf("*"); break;
			case 'z': printf("/"); break;
		}
		printf(" %s = ", liczba2);
		real1 = atof(liczba1);
		real2 = atof(liczba2);
		switch (dzialanie) {
			case 's': printf("%.2f", real1+real2); break;
			case 'r': printf("%.2f", real1-real2); break;
			case 'i': printf("%.2f", real1*real2); break;
			case 'z': printf("%.2f", real1/real2); break;
		}
	}
	printf("<br><br><a href=\"formcalc.html\">Powrot</a>");
	
//	printf("Podano napis %s\n", getenv("QUERY_STRING"));
	printf("</BODY>\n");
	printf("</HTML>\n");

	return 0;
}

int is_float(char str[]) {
	int i;

	if (atof(str) == 0 && !is_zero(str))
		return 0;
	for (i=0; i < strlen(str); i++) {
		if (!isdigit(str[i]) && !(str[i] == '.'))
			return 0;
	}
	return 1;
}

int is_zero(char str[]) {
	int i;

	for (i=0; i < strlen(str); i++) {
		if (str[i] != '0' && str[i] != '.')
			return 0;
	}
	return 1;
}
