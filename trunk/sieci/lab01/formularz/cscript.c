#include <stdio.h>

int main() {
	printf("Content-type: text/html\n\n");
	printf("<HTML>\n");
	printf("<BODY>\n");
//	scanf("%100s", str);
	printf("Podano napis %s\n", getenv("QUERY_STRING"));
	printf("</BODY>\n");
	printf("</HTML>\n");

	return 0;
}

