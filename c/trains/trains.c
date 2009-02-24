/*
 Micha³ Dettlaff

 Przyk³ad zastosowania IPC.
 Animacja z poci±gami, których wej¶cie do "sekcji krytycznej" na ¶rodku
 kontroluj± semafory, w celu unikniêcia zderzenia.
 Najlepiej uruchamiaæ na terminalu o wymiarach 80x24

 Proces macierzysty i trzy procesy potomne, pamiêæ wspó³dzielona i semafory.
 W pamiêci wspó³dzielonej jest zmienna, modyfikowana przez trzy procesy
 potomne i wy¶wietlana regularnie przez proces macierzysty.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/shm.h>
#include <sys/sem.h>

#include "semun.h"
#include "shm_board.h"


#define REFRESH 100000
#define TRAIN1_SPEED 150000
#define TRAIN2_SPEED 200000
#define TRAIN3_SPEED 250000
#define TRAIN1_LEN 5
#define TRAIN2_LEN 7
#define TRAIN3_LEN 9
#define TRAIN1_DIR "forward"
#define TRAIN2_DIR "backward"
#define TRAIN3_DIR "forward"
#define TRAIN1_CHAR '1'
#define TRAIN2_CHAR '2'
#define TRAIN3_CHAR '3'

#define WIDTH 80
#define HEIGHT 24
#define MAX_ROUTE 128
#define RAIL '#'
#define CRITICAL '@'
#define WAGON 'W'
#define ROUTE 'R'
#define EMPTY 'E'
#define ever ;;

#define SHM_KEY 1235 /* klucz pamieci wspoldzielonej */
#define SEM_SYNC_KEY 1111 /* semafor do synchronizacji pamieci wspoldziel. */
#define SEM_CRIT_KEY 1122 /* semafor do blokowania sekcji krytycznej torow  */


typedef struct coords {
  int x;
  int y;
  char wagon;
} t_coords;


/* plansza bez pociagow (plansza do wyswietlenia jest w pamieci wspoldziel.) */
char clean_board[WIDTH][HEIGHT];


/* semafory */
int sem_sync_id; // ID semafora do pamieci wspoldzielonej
int sem_critical_id; // ID semafora do sekcji krytycznej torow

/* funkcje stanowiace uproszczony interfejs semaforow */

/* The function set_semvalue initializes the semaphore using the SETVAL command
 * in a semctl call. You need to do this before you can use the semaphore. */
static int set_semvalue(int sem_id) {
  union semun sem_union;
  sem_union.val = 1;
  if (semctl(sem_id, 0, SETVAL, sem_union) == -1) return(0);
  return(1);
}

/* The del_semvalue function has almost the same form, except that the call
 * to semctl uses the command IPC_RMID to remove the semaphore's ID. */
static void del_semvalue(int sem_id) {
  union semun sem_union;
  if (semctl(sem_id, 0, IPC_RMID, sem_union) == -1)
    fprintf(stderr, "Failed to delete semaphore\n");
}

/* semaphore_p changes the semaphore by "1" This is the "wait" operation. */
static int semaphore_p(int sem_id) {
  struct sembuf sem_b;
  sem_b.sem_num = 0;
  sem_b.sem_op = -1; /* P() */
  sem_b.sem_flg = SEM_UNDO;
  if (semop(sem_id, &sem_b, 1) == -1) {
    fprintf(stderr, "semaphore_p failed\n");
    return(0);
  }
  return(1);
}

/* semaphore_v is similar except for setting the sem_op part of the sembuf
 * structure to 1. This is the "release" operation, so that the semaphore
 * becomes available. */
static int semaphore_v(int sem_id) {
  struct sembuf sem_b;
  sem_b.sem_num = 0;
  sem_b.sem_op = 1; /* V() */
  sem_b.sem_flg = SEM_UNDO;
  if (semop(sem_id, &sem_b, 1) == -1) {
    fprintf(stderr, "semaphore_v failed\n");
    return(0);
  }
  return(1);
}


/* funkcje do obslugi pociagow */

/* laduje czysta plansze z pliku */
void init_board(char board[][HEIGHT]) {
  FILE *f;
  int i, j;
  char c;

  f=fopen("board.txt", "r");
  if (f == NULL) {
    fprintf(stderr, "b³±d: nie mo¿na otworzyæ pliku z plansz±\n");
  } else { /* jesli plik istnieje, wykonuj program */
    for (j=0; j < HEIGHT; j++) {
      for (i=0; i < WIDTH; i++) {
	if ((c = fgetc(f)) == EOF) {
	  fprintf(stderr, "b³±d: nieoczekiwany koniec pliku z plansz±\n");
	}
	clean_board[i][j] = c;
      }
    }
  }
  for (j=0; j < HEIGHT; j++) {
    for (i=0; i < WIDTH; i++) {
      board[i][j] = clean_board[i][j];
    }
  }
}

void draw_board(char board[][HEIGHT]) {
  int i, j, ind = 0;
  char s[2222];

  for (j=0; j < HEIGHT; j++) {
    for (i=0; i < WIDTH; i++) {
      s[ind] = board[i][j];
      ind++;
    }
  }
  s[ind] = '\0';
  printf("%s", s);
}

/* przesuwa pociag o jedno pole do przodu lub do tylu */
void move_train_on_board(char board[][HEIGHT], t_coords train[], char c,
    char direction[]) {
  int i, j, wrap_around = 0;

  if (strcmp(direction, "forward") == 0) {
  /* przesuwamy wagony na trasie o jedno miejsce do przodu */
    for (i=MAX_ROUTE-1; train[i].wagon == EMPTY; i--) // ustawia index
      ;
    if (train[i].wagon == WAGON)
      wrap_around = 1;
    for (; i > 0; i--) {
      train[i].wagon = train[i-1].wagon;
    }
    if (wrap_around)
      train[0].wagon = WAGON;
    else
      train[0].wagon = ROUTE;
    // wylaczamy semafor przy wychodzeniu z sekcji krytycznej
    for (j=2; j < MAX_ROUTE-1 && train[j].wagon != WAGON; j++);
    if (clean_board[train[j-2].x][train[j-2].y] == CRITICAL
	&& clean_board[train[j-1].x][train[j-1].y] != CRITICAL)
      if (!semaphore_v(sem_critical_id)) exit(EXIT_FAILURE);
    // wlaczamy semafor przy wchodzeniu do sekcji krytycznej
    for (i=MAX_ROUTE-1; train[i].wagon == EMPTY; i--); i-=3;
    for (j=i; j > 1 && train[j].wagon != WAGON; j--);
    if (clean_board[train[j+1].x][train[j+1].y] == CRITICAL
	&& clean_board[train[j].x][train[j].y] != CRITICAL)
      if (!semaphore_p(sem_critical_id)) exit(EXIT_FAILURE);
    // sprzatamy ostatni wagon, zeby nie zalegal na planszy
    for (i=MAX_ROUTE-1; train[i].wagon == EMPTY; i--) // ustawia index
      ;
    if (train[i].wagon != WAGON && train[0].wagon == WAGON)
      board[train[i].x][train[i].y] = clean_board[train[i].x][train[i].y];
    else {
      for (j=i; j > 0 && !(train[j].wagon != WAGON
	    && train[j+1].wagon == WAGON); j--);
      board[train[j].x][train[j].y] = clean_board[train[j].x][train[j].y];
    }
  } else {
  /* przesuwamy wagony na trasie o jedno miejsce do tylu */
    if (train[0].wagon == WAGON)
      wrap_around = 1;
    for (i=1; train[i].wagon != EMPTY; i++) {
      train[i-1].wagon = train[i].wagon;
    }
    for (i=MAX_ROUTE-1; train[i].wagon == EMPTY; i--) // ustawia index
      ;
    if (wrap_around)
      train[i].wagon = WAGON;
    else
      train[i].wagon = ROUTE;
    // wlaczamy semafor przy wchodzeniu do sekcji krytycznej
    for (j=2; j < MAX_ROUTE-1 && train[j].wagon != WAGON; j++);
    if (clean_board[train[j-2].x][train[j-2].y] == CRITICAL
	&& clean_board[train[j-1].x][train[j-1].y] != CRITICAL)
      if (!semaphore_p(sem_critical_id)) exit(EXIT_FAILURE);
    // wylaczamy semafor przy wychodzeniu z sekcji krytycznej
    for (i=MAX_ROUTE-1; train[i].wagon == EMPTY; i--); i-=3;
    for (j=i; j > 1 && train[j].wagon != WAGON; j--);
    if (clean_board[train[j+1].x][train[j+1].y] == CRITICAL
	&& clean_board[train[j].x][train[j].y] != CRITICAL)
      if (!semaphore_v(sem_critical_id)) exit(EXIT_FAILURE);
    // sprzatamy ostatni wagon, zeby nie zalegal na planszy
    for (i=MAX_ROUTE-1; train[i].wagon == EMPTY; i--) // ustawia index
      ;
    if (train[i].wagon == WAGON && train[0].wagon != WAGON)
      board[train[0].x][train[0].y] = clean_board[train[0].x][train[0].y];
    else {
      for (j=0; j<i && !(train[j].wagon == WAGON && train[j+1].wagon != WAGON);
	  j++);
      board[train[j+1].x][train[j+1].y]=clean_board[train[j+1].x][train[j+1].y];
    }
  }
  /* zapisujemy zmieniona pozycje na planszy */
  for (i=0; train[i].wagon != EMPTY; i++) {
    if (train[i].wagon == WAGON)
      board[train[i].x][train[i].y] = c;
  }
}


void init_train(t_coords train[], int route_id, int length) {
  int i;
  for (i=0; i < MAX_ROUTE; i++) {
    train[i].wagon = EMPTY; // puste miejsce
  }
  if (route_id == 1) {
  /* trasa pierwsza: okrag z lewej strony na gorze */
    for (i=0; i < 70; i++) { // 70 - dlugosc tej trasy
      train[i].wagon = ROUTE; // trasa
    }
    train[0].x = 15; train[0].y = 7;
    train[1].x = 15; train[1].y = 8;
    train[2].x = 16; train[2].y = 8;
    train[3].x = 16; train[3].y = 9;
    train[4].x = 17; train[4].y = 9;
    train[5].x = 17; train[5].y = 10;
    train[6].x = 18; train[6].y = 10;
    train[7].x = 19; train[7].y = 10;
    train[8].x = 20; train[8].y = 10;
    train[9].x = 20; train[9].y = 11;
    train[10].x = 21; train[10].y = 11;
    train[11].x = 22; train[11].y = 11;
    train[12].x = 23; train[12].y = 11;
    train[13].x = 23; train[13].y = 12;
    train[14].x = 24; train[14].y = 12;
    train[15].x = 25; train[15].y = 12;
    train[16].x = 26; train[16].y = 12;
    train[17].x = 27; train[17].y = 12;
    train[18].x = 28; train[18].y = 12;
    train[19].x = 28; train[19].y = 13;
    train[20].x = 29; train[20].y = 13;
    train[21].x = 30; train[21].y = 13;
    train[22].x = 31; train[22].y = 13;
    train[23].x = 32; train[23].y = 13;
    train[24].x = 33; train[24].y = 13;
    train[25].x = 34; train[25].y = 13;
    train[26].x = 35; train[26].y = 13;
    train[27].x = 36; train[27].y = 13;
    train[28].x = 37; train[28].y = 13;
    train[29].x = 38; train[29].y = 13;
    train[30].x = 39; train[30].y = 13;
    train[31].x = 39; train[31].y = 12;
    train[32].x = 39; train[32].y = 11;
    train[33].x = 39; train[33].y = 10;
    train[34].x = 39; train[34].y = 9;
    train[35].x = 39; train[35].y = 8;
    train[36].x = 39; train[36].y = 7;
    train[37].x = 39; train[37].y = 6;
    train[38].x = 38; train[38].y = 6;
    train[39].x = 38; train[39].y = 5;
    train[40].x = 37; train[40].y = 5;
    train[41].x = 37; train[41].y = 4;
    train[42].x = 36; train[42].y = 4;
    train[43].x = 35; train[43].y = 4;
    train[44].x = 34; train[44].y = 4;
    train[45].x = 34; train[45].y = 3;
    train[46].x = 33; train[46].y = 3;
    train[47].x = 32; train[47].y = 3;
    train[48].x = 31; train[48].y = 3;
    train[49].x = 31; train[49].y = 2;
    train[50].x = 30; train[50].y = 2;
    train[51].x = 29; train[51].y = 2;
    train[52].x = 28; train[52].y = 2;
    train[53].x = 27; train[53].y = 2;
    train[54].x = 26; train[54].y = 2;
    train[55].x = 25; train[55].y = 2;
    train[56].x = 24; train[56].y = 2;
    train[57].x = 23; train[57].y = 2;
    train[58].x = 23; train[58].y = 3;
    train[59].x = 22; train[59].y = 3;
    train[60].x = 21; train[60].y = 3;
    train[61].x = 20; train[61].y = 3;
    train[62].x = 20; train[62].y = 4;
    train[63].x = 19; train[63].y = 4;
    train[64].x = 18; train[64].y = 4;
    train[65].x = 17; train[65].y = 4;
    train[66].x = 17; train[66].y = 5;
    train[67].x = 16; train[67].y = 5;
    train[68].x = 16; train[68].y = 6;
    train[69].x = 15; train[69].y = 6;
  } else if (route_id == 2) {
  /* trasa druga: okrag z prawej strony na gorze */
    for (i=0; i < 70; i++) { // 70 - dlugosc tej trasy
      train[i].wagon = ROUTE; // trasa
    }
    train[0].x = 63; train[0].y = 7;
    train[1].x = 63; train[1].y = 8;
    train[2].x = 62; train[2].y = 8;
    train[3].x = 62; train[3].y = 9;
    train[4].x = 61; train[4].y = 9;
    train[5].x = 61; train[5].y = 10;
    train[6].x = 60; train[6].y = 10;
    train[7].x = 59; train[7].y = 10;
    train[8].x = 58; train[8].y = 10;
    train[9].x = 58; train[9].y = 11;
    train[10].x = 57; train[10].y = 11;
    train[11].x = 56; train[11].y = 11;
    train[12].x = 55; train[12].y = 11;
    train[13].x = 55; train[13].y = 12;
    train[14].x = 54; train[14].y = 12;
    train[15].x = 53; train[15].y = 12;
    train[16].x = 52; train[16].y = 12;
    train[17].x = 51; train[17].y = 12;
    train[18].x = 50; train[18].y = 12;
    train[19].x = 50; train[19].y = 13;
    train[20].x = 49; train[20].y = 13;
    train[21].x = 48; train[21].y = 13;
    train[22].x = 47; train[22].y = 13;
    train[23].x = 46; train[23].y = 13;
    train[24].x = 45; train[24].y = 13;
    train[25].x = 44; train[25].y = 13;
    train[26].x = 43; train[26].y = 13;
    train[27].x = 42; train[27].y = 13;
    train[28].x = 41; train[28].y = 13;
    train[29].x = 40; train[29].y = 13;
    train[30].x = 39; train[30].y = 13;
    train[31].x = 39; train[31].y = 12;
    train[32].x = 39; train[32].y = 11;
    train[33].x = 39; train[33].y = 10;
    train[34].x = 39; train[34].y = 9;
    train[35].x = 39; train[35].y = 8;
    train[36].x = 39; train[36].y = 7;
    train[37].x = 39; train[37].y = 6;
    train[38].x = 40; train[38].y = 6;
    train[39].x = 40; train[39].y = 5;
    train[40].x = 41; train[40].y = 5;
    train[41].x = 41; train[41].y = 4;
    train[42].x = 42; train[42].y = 4;
    train[43].x = 43; train[43].y = 4;
    train[44].x = 44; train[44].y = 4;
    train[45].x = 44; train[45].y = 3;
    train[46].x = 45; train[46].y = 3;
    train[47].x = 46; train[47].y = 3;
    train[48].x = 47; train[48].y = 3;
    train[49].x = 47; train[49].y = 2;
    train[50].x = 48; train[50].y = 2;
    train[51].x = 49; train[51].y = 2;
    train[52].x = 50; train[52].y = 2;
    train[53].x = 51; train[53].y = 2;
    train[54].x = 52; train[54].y = 2;
    train[55].x = 53; train[55].y = 2;
    train[56].x = 54; train[56].y = 2;
    train[57].x = 55; train[57].y = 2;
    train[58].x = 55; train[58].y = 3;
    train[59].x = 56; train[59].y = 3;
    train[60].x = 57; train[60].y = 3;
    train[61].x = 58; train[61].y = 3;
    train[62].x = 58; train[62].y = 4;
    train[63].x = 59; train[63].y = 4;
    train[64].x = 60; train[64].y = 4;
    train[65].x = 61; train[65].y = 4;
    train[66].x = 61; train[66].y = 5;
    train[67].x = 62; train[67].y = 5;
    train[68].x = 62; train[68].y = 6;
    train[69].x = 63; train[69].y = 6;
  } else if (route_id == 3) {
  /* trasa trzecia: okrag z na dole */
    for (i=0; i < 90; i++) { // 90 - dlugosc tej trasy
      train[i].wagon = ROUTE; // trasa
    }
    train[0].x = 21; train[0].y = 17;
    train[1].x = 21; train[1].y = 18;
    train[2].x = 22; train[2].y = 18;
    train[3].x = 22; train[3].y = 19;
    train[4].x = 23; train[4].y = 19;
    train[5].x = 23; train[5].y = 20;
    train[6].x = 24; train[6].y = 20;
    train[7].x = 25; train[7].y = 20;
    train[8].x = 26; train[8].y = 20;
    train[9].x = 26; train[9].y = 21;
    train[10].x = 27; train[10].y = 21;
    train[11].x = 28; train[11].y = 21;
    train[12].x = 29; train[12].y = 21;
    train[13].x = 29; train[13].y = 22;
    train[14].x = 30; train[14].y = 22;
    train[15].x = 31; train[15].y = 22;
    train[16].x = 32; train[16].y = 22;
    train[17].x = 33; train[17].y = 22;
    train[18].x = 34; train[18].y = 22;
    train[19].x = 35; train[19].y = 22;
    train[20].x = 36; train[20].y = 22;
    train[21].x = 37; train[21].y = 22;
    train[22].x = 38; train[22].y = 22;
    train[23].x = 39; train[23].y = 22;
    train[24].x = 40; train[24].y = 22;
    train[25].x = 41; train[25].y = 22;
    train[26].x = 42; train[26].y = 22;
    train[27].x = 43; train[27].y = 22;
    train[28].x = 44; train[28].y = 22;
    train[29].x = 45; train[29].y = 22;
    train[30].x = 46; train[30].y = 22;
    train[31].x = 47; train[31].y = 22;
    train[32].x = 48; train[32].y = 22;
    train[33].x = 49; train[33].y = 22;
    train[34].x = 49; train[34].y = 21;
    train[35].x = 50; train[35].y = 21;
    train[36].x = 51; train[36].y = 21;
    train[37].x = 52; train[37].y = 21;
    train[38].x = 52; train[38].y = 20;
    train[39].x = 53; train[39].y = 20;
    train[40].x = 54; train[40].y = 20;
    train[41].x = 55; train[41].y = 20;
    train[42].x = 55; train[42].y = 19;
    train[43].x = 56; train[43].y = 19;
    train[44].x = 56; train[44].y = 18;
    train[45].x = 57; train[45].y = 18;
    train[46].x = 57; train[46].y = 17;
    train[47].x = 57; train[47].y = 16;
    train[48].x = 56; train[48].y = 16;
    train[49].x = 56; train[49].y = 15;
    train[50].x = 55; train[50].y = 15;
    train[51].x = 55; train[51].y = 14;
    train[52].x = 54; train[52].y = 14;
    train[53].x = 53; train[53].y = 14;
    train[54].x = 52; train[54].y = 14;
    train[55].x = 51; train[55].y = 14;
    train[56].x = 50; train[56].y = 14;
    train[57].x = 50; train[57].y = 13;
    train[58].x = 49; train[58].y = 13;
    train[59].x = 48; train[59].y = 13;
    train[60].x = 47; train[60].y = 13;
    train[61].x = 46; train[61].y = 13;
    train[62].x = 45; train[62].y = 13;
    train[63].x = 44; train[63].y = 13;
    train[64].x = 43; train[64].y = 13;
    train[65].x = 42; train[65].y = 13;
    train[66].x = 41; train[66].y = 13;
    train[67].x = 40; train[67].y = 13;
    train[68].x = 39; train[68].y = 13;
    train[69].x = 38; train[69].y = 13;
    train[70].x = 37; train[70].y = 13;
    train[71].x = 36; train[71].y = 13;
    train[72].x = 35; train[72].y = 13;
    train[73].x = 34; train[73].y = 13;
    train[74].x = 33; train[74].y = 13;
    train[75].x = 32; train[75].y = 13;
    train[76].x = 31; train[76].y = 13;
    train[77].x = 30; train[77].y = 13;
    train[78].x = 29; train[78].y = 13;
    train[79].x = 28; train[79].y = 13;
    train[80].x = 28; train[80].y = 14;
    train[81].x = 27; train[81].y = 14;
    train[82].x = 26; train[82].y = 14;
    train[83].x = 25; train[83].y = 14;
    train[84].x = 24; train[84].y = 14;
    train[85].x = 23; train[85].y = 14;
    train[86].x = 23; train[86].y = 15;
    train[87].x = 22; train[87].y = 15;
    train[88].x = 22; train[88].y = 16;
    train[89].x = 21; train[89].y = 16;
  }
  for (i=0; i < length && i < MAX_ROUTE; i++) {
    train[i].wagon = WAGON; // wagon
  }
}


int main() {
  pid_t pid;
  /* trasy i polozenie wagonow pociagow */
  t_coords train1[MAX_ROUTE];
  t_coords train2[MAX_ROUTE];
  t_coords train3[MAX_ROUTE];

  /* inicjalizacja semafora do synchronizacji pamieci wspoldzielonej */
  /*sem_sync_id = semget((key_t)SEM_SYNC_KEY, 1, 0666 | IPC_CREAT);
  if (!set_semvalue(sem_sync_id)) {
    fprintf(stderr, "Failed to initialize semaphore\n");
    exit(EXIT_FAILURE);
  }*/

  /* inicjalizacja semafora do sekcji krytycznej torow */
  sem_critical_id = semget((key_t)SEM_CRIT_KEY, 1, 0666 | IPC_CREAT);
  if (!set_semvalue(sem_critical_id)) {
    fprintf(stderr, "Failed to initialize semaphore\n");
    exit(EXIT_FAILURE);
  }

  /* pamiec wspoldzielona, inicjalizacja */
  void *shared_memory = (void *)0;
  struct shared_use_st *shared_stuff;
  int shmid;
  shmid = shmget((key_t)SHM_KEY, sizeof(struct shared_use_st),
      0666 | IPC_CREAT);
  if (shmid == -1) {
    fprintf(stderr, "shmget failed\n");
    exit(EXIT_FAILURE);
  }

  // dolaczamy pamiec wspoldzielona juz tutaj, zeby zainicjalizowac plansze
  shared_memory = shmat(shmid, (void *)0, 0);
  if (shared_memory == (void *)-1) {
    fprintf(stderr, "shmat failed\n");
    exit(EXIT_FAILURE);
  }
  printf("Memory attached at %X\n", (int)shared_memory);
  shared_stuff = (struct shared_use_st *)shared_memory;
  //if (!semaphore_p(sem_sync_id)) exit(EXIT_FAILURE);
  init_board(shared_stuff->board);
  //if (!semaphore_v(sem_sync_id)) exit(EXIT_FAILURE);

  switch(pid = fork()) {
    case -1: // b³±d
      perror("Wystapil blad przy tworzeniu procesu");
      exit(1);
    case 0: /* potomek 1 */
      init_train(train1, 1, TRAIN1_LEN);

      shared_memory = shmat(shmid, (void *)0, 0);
      if (shared_memory == (void *)-1) {
	fprintf(stderr, "shmat failed\n");
	exit(EXIT_FAILURE);
      }
      printf("Memory attached at %X\n", (int)shared_memory);
      shared_stuff = (struct shared_use_st *)shared_memory;

      /* wlasciwa petla potomka 1 */
      for(ever) {
	usleep(TRAIN1_SPEED);
	//if (!semaphore_p(sem_sync_id)) exit(EXIT_FAILURE);
	move_train_on_board(shared_stuff->board, train1, TRAIN1_CHAR,
	    TRAIN1_DIR);
	//if (!semaphore_v(sem_sync_id)) exit(EXIT_FAILURE);
      }
    default: /* rodzic, tworzy kolejnego potomka */
      switch(pid = fork()) {
	case -1: // b³±d
	  perror("Wystapil blad przy tworzeniu procesu");
	  exit(1);
	case 0: /* potomek 2 */
	  init_train(train2, 2, TRAIN2_LEN);

	  shared_memory = shmat(shmid, (void *)0, 0);
	  if (shared_memory == (void *)-1) {
	    fprintf(stderr, "shmat failed\n");
	    exit(EXIT_FAILURE);
	  }
	  printf("Memory attached at %X\n", (int)shared_memory);
	  shared_stuff = (struct shared_use_st *)shared_memory;

	  /* wlasciwa petla potomka 2 */
	  for(ever) {
	    usleep(TRAIN2_SPEED);
	    //if (!semaphore_p(sem_sync_id)) exit(EXIT_FAILURE);
	    move_train_on_board(shared_stuff->board, train2, TRAIN2_CHAR,
	       	TRAIN2_DIR);
	    //if (!semaphore_v(sem_sync_id)) exit(EXIT_FAILURE);
	  }
	default: /* rodzic, tworzy kolejnego potomka */
	  switch(pid = fork()) {
	    case -1: // b³±d
	      perror("Wystapil blad przy tworzeniu procesu");
	      exit(1);
	    case 0: /* potomek 3 */
	      init_train(train3, 3, TRAIN3_LEN);

	      shared_memory = shmat(shmid, (void *)0, 0);
	      if (shared_memory == (void *)-1) {
		fprintf(stderr, "shmat failed\n");
		exit(EXIT_FAILURE);
	      }
	      printf("Memory attached at %X\n", (int)shared_memory);
	      shared_stuff = (struct shared_use_st *)shared_memory;

	      /* wlasciwa petla potomka 3 */
	      for(ever) {
		usleep(TRAIN3_SPEED);
		//if (!semaphore_p(sem_sync_id)) exit(EXIT_FAILURE);
		move_train_on_board(shared_stuff->board, train3, TRAIN3_CHAR,
		    TRAIN3_DIR);
		//if (!semaphore_v(sem_sync_id)) exit(EXIT_FAILURE);
	      }
	    default: /* rodzic */
	      shared_memory = shmat(shmid, (void *)0, 0);
	      if (shared_memory == (void *)-1) {
		fprintf(stderr, "shmat failed\n");
		exit(EXIT_FAILURE);
	      }
	      printf("Memory attached at %X\n", (int)shared_memory);
	      shared_stuff = (struct shared_use_st *)shared_memory;

	      /* wlasciwa petla rodzica */
	      for(ever) {
		usleep(REFRESH);
		draw_board(shared_stuff->board);
	      }
	  }
      }
  }

  /* rodzic pozbywa sie niepotrzebnej pamieci wspoldzielonej */
  printf("Od³±czam pamiêæ wspó³dzielon±... ");
  if (shmdt(shared_memory) == -1) {
    fprintf(stderr, "shmdt failed\n");
    exit(EXIT_FAILURE);
  } else {
    printf("gotowe\n");
  }
  if (shmctl(shmid, IPC_RMID, 0) == -1) {
    fprintf(stderr, "shmctl(IPC_RMID) failed\n");
    exit(EXIT_FAILURE);
  }
  /* pozbywamy sie semafow */
  del_semvalue(sem_sync_id);
  del_semvalue(sem_critical_id);
  exit(EXIT_SUCCESS);
}

