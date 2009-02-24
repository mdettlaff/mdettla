#include <cstdio>
#include <fcntl.h>
#include <unistd.h>
#include <iostream>
#include <fstream>

int d;

void otworz() {
  while ((d=open("lockfile", O_CREAT | O_EXCL, 0)) < 0) { // sekcja wej¶ciowa
    printf("Plik zajêty, proszê czekaæ...\n");
    sleep(1);
  }
}

void zamknij() {
  close(d);
  unlink("lockfile");
}

int main() {
  FILE *f;
  int c;
  std :: string str;

  printf("MENU\n0 - odczyt z pliku\n1 - zapis do pliku\n2 - koniec\n");
  while ((c=getchar()) != '2') {
    if (c == '1') {
      otworz();
      f=fopen("plik", "w");
      std :: cin>>str;
      fwrite(str.c_str(), str.length(), 1, f);
      fclose(f);
      zamknij();
    } else if (c == '0') {
      otworz();
      std :: ifstream plik("plik");
      while (!plik.eof()) {
	std :: cout << plik;
      }
      zamknij();
    }
  }
  return 0;
}
