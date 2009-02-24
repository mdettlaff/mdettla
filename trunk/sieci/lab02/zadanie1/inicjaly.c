#include <stdio.h>

int main() {
	int i;
	char c;
	char query[255];
	printf("Content-type: text/html\n\n");
	printf("<HTML>\n");
	printf("<BODY>\n");
	printf("Twoje inicja³y to: ");
	for (i=0; i < 2; i++) {
		while ((c=getchar()) != '=') {
			c=getchar();
		}
		c=getchar();
		putchar(c);
		putchar('.');
	}
//	scanf("%100s", query);
//	printf("Podano napis %s\n", getenv("QUERY_STRING"));
	printf("</BODY>\n");
	printf("</HTML>\n");

	return 0;
}

