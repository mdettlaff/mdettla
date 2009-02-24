#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main() {
	int i;
	char query[255];

	printf("Content-type: text/html\n\n");
	printf("<HTML>\n");
	printf("<BODY>\n");

	scanf("%100s", query);
	for (i=0; i < 90; i++) {
		query[i]=query[i+7];
	}
	if (atof(query) == 0) {
		printf("To nie jest liczba rzeczywista!");
	} else {
		printf("sin(%s) = %f", query, sin(atof(query)));
	}
//	printf("Podano napis %s\n", getenv("QUERY_STRING"));

	printf("</BODY>\n");
	printf("</HTML>\n");

	return 0;
}

