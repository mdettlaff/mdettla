#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

#define BUFSIZE 32

int d;

void lock() {
  while ((d=open("lockfile", O_CREAT | O_EXCL, 0)) < 0) {
    printf("Plik zajêty, proszê czekaæ...\n");
    sleep(1);
  }
}

void unlock() {
  close(d);
  unlink("lockfile");
}

int main() {
  char tekst[BUFSIZE];
  FILE *f;
  char c;
  int i;

  printf("Content-type: text/html\n\n");
  printf("<HTML>\n");
  printf("<BODY>\n");

  lock();
  f=fopen("licznik.txt", "r+");
  for (i=0; (c=fgetc(f)) != EOF; i++) {
    tekst[i]=c;
  }
  tekst[i]='\0';
  sprintf(tekst, "%d", atoi(tekst)+1);
  rewind(f);
  for (i=0; tekst[i] != '\0'; i++) {
    fputc(tekst[i], f);
  }
  printf("%s\n", tekst);
  fclose(f);
  unlock();

  printf("</BODY>\n");
  printf("</HTML>\n");

  return 0;
}
