#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char *str_replace(const char *search, const char *replace, const char *subject);

int main() {
  char *search = "find";
  char *replace = "replace";
  char *subject = "I am going to try to find some strings in this text and hope i find them all ok.  this will test if my find code works as planned.  now go find them!";
  char *rep;

  printf("%s\n", subject);
  rep = str_replace(search, replace, subject);
  printf("%s\n", rep);

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
