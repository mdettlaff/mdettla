#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <fcntl.h>
#include <unistd.h>

char *str_replace(const char *search, const char *replace, const char *subject);

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
  char tekst[BUFSIZ];
  char komentarz[255];
  char *komentarz_v2; // komentarz po konwersji
  char imie[32];
  FILE *f;
  char c;
  int i, j;
  struct tm *local;
  time_t t;

  printf("Content-type: text/html\n\n");
  printf("<HTML>\n");
  printf("<BODY>\n");

  while ((c=getchar()) != '=');
  for (i=0; (c=getchar()) != '&'; i++) {
	  komentarz[i] = c;
  }
  komentarz[i] = '\0';

  /* konwersja */
  komentarz_v2 = str_replace("\%0D\%0A", "<br>\n", komentarz);
  komentarz_v2 = str_replace("+", " ", komentarz_v2);
  komentarz_v2 = str_replace("\%3C", "&lt;", komentarz_v2);
  komentarz_v2 = str_replace("\%3E", "&gt;", komentarz_v2);
  komentarz_v2 = str_replace("\%2F", "/", komentarz_v2);

  char unesc[4];
  char esc[2];
  for (i=1; i < 16; i++) {
    for (j=0; j < 16; j++) {
      unesc[0]='%';
      if (i < 10) unesc[1]=48+i;
      else unesc[1]=55+i;
      if (j < 10) unesc[2]=48+j;
      else unesc[2]=55+j;
      unesc[3]='\0';
      esc[0]=i*16+j*1; esc[1]='\0';
      komentarz_v2 = str_replace(unesc, esc, komentarz_v2);
    }
  }
  /* koniec konwersji */

  while ((c=getchar()) != '=');
  for (i=0; (c=getchar()) != EOF; i++) {
	  imie[i] = c;
  }
  imie[i] = '\0';

  lock();
  f=fopen("guestbook.txt", "a");

  t = time(NULL);
  local = localtime(&t);
  sprintf(tekst, "Czas: %s<br>\nIP: %s<br>\nImie: %s<br>\n%s<br>\n<hr>\n\n",
      asctime(local), getenv("REMOTE_ADDR"), imie, komentarz_v2);

  for (i=0; tekst[i] != '\0'; i++) {
    fputc(tekst[i], f);
  }
  fclose(f);
  unlock();

  printf("Dziêkujemy za dokonanie wpisu<br><br>");
  printf("<a href=\"index.html\">Powrót do strony g³ównej");
  printf("</BODY>\n");
  printf("</HTML>\n");

  return 0;
}


char *str_replace(const char *search, const char *replace, const char *subject)
{
  if (search == NULL || replace == NULL || subject == NULL) return NULL;
  if (strlen(search) == 0 || strlen(replace) == 0 || strlen(subject) == 0) return NULL;

  char *replaced = (char*)calloc(1, 1), *temp = NULL;
  char *p = subject, *p3 = subject, *p2;
  int  found = 0;

  while ( (p = strstr(p, search)) != NULL) {
    found = 1;
    temp = realloc(replaced, strlen(replaced) + (p - p3) + strlen(replace));
    if (temp == NULL) {
      free(replaced);
      return NULL;
    }
    replaced = temp;
    strncat(replaced, p - (p - p3), p - p3);
    strcat(replaced, replace);
    p3 = p + strlen(search);
    p += strlen(search);
    p2 = p;
  }

  if (found == 1) {
    if (strlen(p2) > 0) {
      temp = realloc(replaced, strlen(replaced) + strlen(p2) + 1);
      if (temp == NULL) {
	free(replaced);
	return NULL;
      }
      replaced = temp;
      strcat(replaced, p2);
    }
  } else {
    temp = realloc(replaced, strlen(subject) + 1);
    if (temp != NULL) {
      replaced = temp;
      strcpy(replaced, subject);
    }
  }
  return replaced;
}
