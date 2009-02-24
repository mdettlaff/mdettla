#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

int d;

int main() {
  char tekst[BUFSIZ];
  FILE *f;
  char c;
  int i;

  printf("Content-type: text/html; charset=iso-8859-2\n\n");
  printf("<HTML>\n");
  printf("<HEAD>\n");
  printf("<META HTTP-EQUIV=\"Content-type\" CONTENT=\"text/html; charset=iso-8859-2\">");
  printf("</HEAD>\n");
  printf("<BODY>\n");

  f=fopen("guestbook.txt", "r");
  for (i=0; (c=fgetc(f)) != EOF; i++) {
    tekst[i]=c;
  }
  tekst[i]='\0';
  printf("%s\n", tekst);
  fclose(f);

  printf("<br><a href=\"index.html\">Powrót do strony głównej</a>");
  printf("</BODY>\n");
  printf("</HTML>\n");

  return 0;
}
