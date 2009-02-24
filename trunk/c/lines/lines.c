/* linkowac z opcjami -lncursesw -lrt */

#include <ncursesw/ncurses.h>  
#include <stdlib.h>  
#include <time.h>  
#include "lines.h"

typedef struct square{ /* informacje o polu na planszy */
  int y; /* pozycja y wzgledem okna planszy */
  int x; /* pozycja x wzgledem okna planszy */
  int marble; /* zawartosc pola (czy jest kulka i jaki ma kolor) */
  int highlighted; /* czy pole jest podswietlone */
} t_square;

t_square board_data[BOARD_SIZE][BOARD_SIZE]; /* tablica przechowujaca
						stan planszy */
static WINDOW *board;       /* okno z plansza do gry */  
static WINDOW *next3window; /* okno z nastepnymi 3 kulkami w kolejce */
int next3marbles[3]; /* tablica z nastepnymi 3 kulkami w kolejce */
 

void init_screen(void){
  initscr();
  noecho();                 /* wylacz echo klawiatury */  
  raw();                    /* znaki dostepne zaraz po wpisaniu */  
  /*nodelay(stdscr, TRUE);*/    /* wlacz nieblokowane wywolania getch() */  
  curs_set(0);              /* niewidoczny kursor */  
  cbreak(); 		    /* szybki dostep do wciskanych klawiszy */
  keypad(stdscr, TRUE);	    /* aktywacja klawiszy specjalnych, np. strzalek */
  start_color();
  refresh();
}
 
void end_screen(void){
  endwin();
}

void init_board(void){
  board=newwin(BOARD_HEIGHT, BOARD_WIDTH, BOARD_POS_Y, BOARD_POS_X);
}

void init_next3window(void){
  next3window=newwin(NEXT3_HEIGHT, NEXT3_WIDTH, NEXT3_POS_Y, NEXT3_POS_X);
}
 
void init_color_pairs(void){
  init_pair(BLANK, 0, 0); 		/* czarny/przezroczysty */
  init_pair(1, 0, COLOR_RED);		/* marble color #1 */
  init_pair(2, 0, COLOR_GREEN); 	/* marble color #2 */
  init_pair(3, 0, COLOR_YELLOW); 	/* marble color #3 */
  init_pair(4, 0, COLOR_BLUE); 		/* marble color #4 */
  init_pair(5, 0, COLOR_MAGENTA); 	/* marble color #5 */
  init_pair(6, 0, COLOR_CYAN); 		/* marble color #6 */
  init_pair(7, 0, COLOR_WHITE); 	/* marble color #7 */

  init_pair(8, COLOR_GREEN, 0);		  /* kolor kursora */
  init_pair(9, COLOR_WHITE, COLOR_BLACK); /* kolor tekstu */
}

void display_welcome_screen(void){
  attron(COLOR_PAIR(9));
  mvwprintw(stdscr, LINES/2-6, COLS/2-3, "LINES");
  mvwprintw(stdscr, LINES/2-4, COLS/2-35,
    "Uzywaj klawiszy strzalek i spacji, aby przemieszczac pionki po planszy");
  mvwprintw(stdscr, LINES/2-2, COLS/2-35,
    "Jesli ulozysz co najmniej 5 pionkow tego samego koloru w linie");
  mvwprintw(stdscr, LINES/2-1, COLS/2-35,
    "pozioma, pionowa lub ukosna, znikna one a za kazdy otrzymasz 2 punkty");
  mvwprintw(stdscr, LINES/2, COLS/2-35,
    "Gra konczy sie, kiedy cala plansza sie zapelni");
  mvwprintw(stdscr, LINES/2+2, COLS/2-20,\
    "Nacisnij dowolny klawisz, aby rozpoczac...");
  attroff(COLOR_PAIR(9));
  refresh();
  getch(); /* czeka na dowolny klawisz */
  clear();
}

void display_game_over_screen(void){
  extern int score;

  clear();
  attron(COLOR_PAIR(9));
  mvwprintw(stdscr, LINES/2-3, COLS/2-6, "Koniec gry");
  mvwprintw(stdscr, LINES/2-1, COLS/2-12, "Twoj wynik: %d punktow", score);
  mvwprintw(stdscr, LINES/2+1, COLS/2-21,
    "Nacisnij dowolny klawisz, aby zakonczyc...");
  attroff(COLOR_PAIR(9));
  refresh();
  getch();
}

void prepare_game(void){
  init_board();
  set_square_positions_on_board();
  remove_marbles_from_board();
  init_next3window();
  get_new_next_3_marbles(); /* losuje kolejne 3 kulki w kolejce */
  insert_next_3_marbles_on_board();
  get_new_next_3_marbles(); /* losuje kolejne 3 kulki w kolejce */
  mvprintw(NEXT3_POS_Y-1, NEXT3_POS_X, "nastepne:");
  mvprintw(SCORE_POS_Y-1, SCORE_POS_X, "punkty:");
  mvprintw(BOARD_POS_Y+17, SCORE_POS_X, "q - zakoncz");
  refresh();
}

void set_square_positions_on_board(void){  
  int i, j;
  for (j=0; j < BOARD_SIZE; j++)
    for (i=0; i < BOARD_SIZE; i++){
      board_data[j][i].y=1+j*SQ_HEIGHT;
      board_data[j][i].x=1+i*SQ_WIDTH;
    }
}

void update_board(void){  
  box(board, ACS_VLINE, ACS_HLINE);
  draw_lines_on_board();
  draw_marbles_on_board();
  wrefresh(board);
}  

void draw_lines_on_board(void){
  int i, j;

  for (i=1; i < BOARD_WIDTH-1; i++)
    if (i%SQ_WIDTH == 0)
      mvwaddch(board, 0, i, ACS_TTEE);

  for (j=2; j < BOARD_HEIGHT-2; j+=2){
    for (i=1; i < BOARD_WIDTH; i++)
      if (i%SQ_WIDTH == 0)
	mvwaddch(board, j-1, i, ACS_VLINE);
    mvwaddch(board, j, 0, ACS_LTEE);
    for (i=1; i < BOARD_WIDTH-1; i++){
      if (i%SQ_WIDTH == 0)
	mvwaddch(board, j, i, ACS_PLUS);
      else
	mvwaddch(board, j, i, ACS_HLINE);
    }
    mvwaddch(board, j, BOARD_WIDTH-1, ACS_RTEE);
  }
  for (i=1; i < BOARD_WIDTH; i++)
    if (i%SQ_WIDTH == 0)
      mvwaddch(board, j-1, i, ACS_VLINE);

  for (i=1; i < BOARD_WIDTH-1; i++)
    if (i%SQ_WIDTH == 0)
      mvwaddch(board, BOARD_HEIGHT-1, i, ACS_BTEE);
}  

void draw_marbles_on_board(void){  
  int i, j;
  for (j=0; j < BOARD_SIZE; j++)
    for (i=0; i < BOARD_SIZE; i++){
      wdraw_marble(board, board_data[j][i].y, board_data[j][i].x,
		   board_data[j][i].marble, board_data[j][i].highlighted);
    }
}

void wdraw_marble(WINDOW *window, int y, int x, int color, int highlighted){
  chtype ch;

  wmove(window, y, x);
  wattron(window, COLOR_PAIR(color));
  if (highlighted)
    ch=HL_CHAR; /* podswietlona */
  else
    ch=' '; 	/* normalna */
  waddch(window, ch); /* kulka sklada sie z */
  waddch(window, ch); /*   dwoch znakow     */
  wattroff(window, COLOR_PAIR(color));
}

void update_next3window(void){
  int i;
  /* rysowanie linii */
  box(next3window, ACS_VLINE, ACS_HLINE);
  for (i=1; i < NEXT3_WIDTH-1; i++)
    if (i%SQ_WIDTH == 0)
      mvwaddch(next3window, 0, i, ACS_TTEE);
  for (i=1; i < NEXT3_WIDTH; i++)
    if (i%SQ_WIDTH == 0)
      mvwaddch(next3window, 1, i, ACS_VLINE);
  for (i=1; i < NEXT3_WIDTH-1; i++)
    if (i%SQ_WIDTH == 0)
      mvwaddch(next3window, NEXT3_HEIGHT-1, i, ACS_BTEE);

  /* rysowanie kulek */
  wdraw_marble(next3window, 1, 1, next3marbles[0], FALSE);
  wdraw_marble(next3window, 1, 5, next3marbles[1], FALSE);
  wdraw_marble(next3window, 1, 9, next3marbles[2], FALSE);
  wrefresh(next3window);
} 

void unhighlight_all_marbles_on_board(void){  
  int i, j;
  for (j=0; j < BOARD_SIZE; j++)
    for (i=0; i < BOARD_SIZE; i++)
      board_data[j][i].highlighted=FALSE;
}

void remove_marbles_from_board(void){  
  int i, j;
  for (j=0; j < BOARD_SIZE; j++)
    for (i=0; i < BOARD_SIZE; i++){
      board_data[j][i].marble=BLANK;
      board_data[j][i].highlighted=FALSE;
    }
}

int number_of_marbles_on_board(void){  
  int i, j, n=0;
  for (j=0; j < BOARD_SIZE; j++)
    for (i=0; i < BOARD_SIZE; i++)
      if (!is_square_free(j, i))
	n++;
  return n;
}

void draw_cursor(int y, int x){
  wmove(board, board_data[y][x].y, board_data[y][x].x+2);
  wattron(board, COLOR_PAIR(8));
  waddch(board, CURSOR_CHAR);  
  wattroff(board, COLOR_PAIR(8));
}

void remove_old_cursor(int y, int x){
  wattron(board, COLOR_PAIR(BLANK));
  /* czysci pola wokol aktualnego kursora */
  mvwaddch(board, board_data[y+1][x].y, board_data[y][x].x+2, ' ');  
  mvwaddch(board, board_data[y-1][x].y, board_data[y][x].x+2, ' ');  
  mvwaddch(board, board_data[y][x].y, board_data[y][x+1].x+2, ' ');  
  mvwaddch(board, board_data[y][x].y, board_data[y][x-1].x+2, ' ');  
  wattroff(board, COLOR_PAIR(BLANK));
}

/* jesli wokol punktu y, x utworzyl sie wzor z kulek, usun go
 * i zwroc liczbe punktow do przyznania */
int remove_pattern_if_exists(int y, int x){
  /* tablica zapamietujaca polozenia kulek do skasowania */
  int pattern[BOARD_SIZE][BOARD_SIZE]={{0}};
  int i, j, k;
  int points=0; /* punkty do dodania */
  /* kolor do dopasowania */
  int color_match=board_data[y][x].marble;

  /* szukamy kulek do skasowania i zapisujemy je w tablicy pattern */
  /* sprawdzamy kierunek zachod - wschod */
  explore_pattern(pattern, color_match, y, x, 0, 1);
  /* sprawdzamy kierunek polnoc - poludnie */
  explore_pattern(pattern, color_match, y, x, 1, 0);
  /* sprawdzamy kierunek polnocny zachod - poludniowy wschod */
  explore_pattern(pattern, color_match, y, x, 1, 1);
  /* sprawdzamy kierunek poludniowy zachod - polnocny wschod */
  explore_pattern(pattern, color_match, y, x, -1, 1);

  /* jesli nie znaleziono wzoru do skasowania, nie przyznawaj punktow */
  if ((pattern[y][x]) == BLANK)
    return 0;

  /* mruganie kulek przed zniknieciem */
  for (k=0; k < BLINKS; k++){
    for (j=0; j < BOARD_SIZE; j++)
      for (i=0; i < BOARD_SIZE; i++)
	if (pattern[j][i] != BLANK)
	  board_data[j][i].marble=BLANK;
    update_board();
    wait(BLINK_DELAY);
    for (j=0; j < BOARD_SIZE; j++)
      for (i=0; i < BOARD_SIZE; i++)
	if (pattern[j][i] != BLANK)
	  board_data[j][i].marble=color_match;
    update_board();
    wait(BLINK_DELAY);
  }

  /* kasujemy kulki ulozone we wzor i obliczamy zdobyte punkty */
  for (j=0; j < BOARD_SIZE; j++)
    for (i=0; i < BOARD_SIZE; i++)
      if (pattern[j][i] != BLANK){
	board_data[j][i].marble=BLANK;
	points+=POINTS4MARBLE;
      }

  return points;
}

/* funkcja uzywana przez remove_pattern_if_exists() */
void explore_pattern(int pattern[][BOARD_SIZE], int color_match,
		     int y, int x, int dir_y, int dir_x){
  int i, matches=0;

  for (i=1; square_color_matches(y-i*dir_y, x-i*dir_x, color_match); i++)
    matches++;
  for (i=1; square_color_matches(y+i*dir_y, x+i*dir_x,  color_match); i++)
    matches++;
  if (matches >= PATTERN_SIZE-1){
    pattern[y][x]=TRUE; /* dosunieta kulka tez sie liczy */
    for (i=1; square_color_matches(y-i*dir_y, x-i*dir_x, color_match); i++)
      pattern[y-i*dir_y][x-i*dir_x]=TRUE;
    for (i=1; square_color_matches(y+i*dir_y, x+i*dir_x, color_match); i++)
      pattern[y+i*dir_y][x+i*dir_x]=TRUE;
  }
}

int is_square_free(int y, int x){
  if (y < 0 || x < 0 || y > BOARD_SIZE-1 || x > BOARD_SIZE-1)
    return 0;
  if (board_data[y][x].marble != BLANK) /* czy jest kulka */
    return 0;
  return 1;
}

/* czy w danym polu jest kulka danego koloru? */
int square_color_matches(int y, int x, int color_match){
  if (y < 0 || x < 0 || y > BOARD_SIZE-1 || x > BOARD_SIZE-1)
    return 0;
  if (board_data[y][x].marble == color_match)
    return 1;
  return 0;
}

/* jesli zaznaczono kulke, przesuwa ja na pozycje target_y, target_x,
 * o ile sciezka do tej pozycji nie jest zablokowana */ 
int move_marble(int target_y, int target_x){
  extern int score;
  int i, points;
  int src_y=NOT_FOUND;
  int src_x=NOT_FOUND;
  /* tablica do algorytmu Dijkstry, przechowujaca odleglosci
   * od pola poczatkowego do danego pola na planszy,
   * ktora wykorzystamy do znalezienia najkrotszej sciezki */
  int D[BOARD_SIZE][BOARD_SIZE];
  /* tablica przechowujaca najkrotsza sciezke */
  yxcoords path[INFINITY];
  for (i=0; i < INFINITY; i++)
    path[i].y=path[i].x=NOT_FOUND;

  /* jesli nie ma czego przesuwac, zakoncz funkcje */
  if (find_highlighted_marble(&src_y, &src_x) == NOT_FOUND)
    return NOT_FOUND;

  /* stosujemy algorytm Dijkstry do wypelnienia tablicy D */ 
  dijkstra(D, src_y, src_x);

  /* jesli sciezka zablokowana, zakoncz funkcje */
  if (D[target_y][target_x] == INFINITY)
    return NOT_FOUND;

  /* znajdujemy najkrotsza sciezke przy uzyciu tablicy D */
  find_shortest_path(path, D, target_y, target_x, src_y, src_x);

  move_marble_along_path(path);

  /* jesli kulki ulozyly sie we wzor, skasuj je i przyznaj punkty */
  /* jesli nie ulozyly sie, dodaj nastepne trzy kulki na plansze */
  if ((points=remove_pattern_if_exists(target_y, target_x)) > 0)
    score+=points;
  else{
    insert_next_3_marbles_on_board();
    get_new_next_3_marbles();
  }

  return 0;
}

int find_highlighted_marble(int *src_y, int *src_x){
  int i, j;

  for (j=0; j < BOARD_SIZE; j++)
    for (i=0; i < BOARD_SIZE; i++)
      if (board_data[j][i].highlighted == TRUE){
       	/* odznacz kulke, ktora bedzie przesunieta */
	board_data[j][i].highlighted=FALSE;
	*src_y=j;
	*src_x=i;
	return 1;
      }
  return -1;
}

void dijkstra(int D[][BOARD_SIZE], int src_y, int src_x){
  int i, j, k;

  /* najpierw przypisujemy kazdemu polu "nieskonczona" odleglosc */
  for (j=0; j < BOARD_SIZE; j++)
    for (i=0; i < BOARD_SIZE; i++)
      D[j][i]=INFINITY;
  /* odleglosc od pola poczatkowego do samego siebie wynosi zero */
  D[src_y][src_x]=0;
  /* odleglosc od pola poczatkowego do sasiednich pol */
  if (is_square_free(src_y-1, src_x))
    D[src_y-1][src_x]=1;
  if (is_square_free(src_y+1, src_x))
    D[src_y+1][src_x]=1;
  if (is_square_free(src_y, src_x-1))
    D[src_y][src_x-1]=1;
  if (is_square_free(src_y, src_x+1))
    D[src_y][src_x+1]=1;

  /* wlasciwy algorytm Dijkstry, nieco uproszczony */
  for (k=0; k < BOARD_SIZE*BOARD_SIZE; k++)
    for (j=0; j < BOARD_SIZE; j++)
      for (i=0; i < BOARD_SIZE; i++){
	/* sprawdz, czy nie lepsza jest sciezka przez sasiada */
	if (is_square_free(j, i) && is_square_free(j-1, i))
	  D[j][i]=min(D[j][i], D[j-1][i]+1);
	if (is_square_free(j, i) && is_square_free(j+1, i))
	  D[j][i]=min(D[j][i], D[j+1][i]+1);
	if (is_square_free(j, i) && is_square_free(j, i-1))
	  D[j][i]=min(D[j][i], D[j][i-1]+1);
	if (is_square_free(j, i) && is_square_free(j, i+1))
	  D[j][i]=min(D[j][i], D[j][i+1]+1);
      }
}

void find_shortest_path(yxcoords path[INFINITY], int D[][BOARD_SIZE],
			int target_y, int target_x, int src_y, int src_x){
  int i, path_length;
  /* zaczynamy wyznaczac sciezke od konca, czyli od pola docelowego */
  path_length=D[target_y][target_x];
  path[path_length].y=target_y;
  path[path_length].x=target_x;
  for (i=path_length; i > 0; i--){
    int y=path[i].y;
    int x=path[i].x;
    look_in_direction(D, path, i-1, y, x, -1, 0);/* polnoc */
    look_in_direction(D, path, i-1, y, x, 1, 0); /* poludnie */
    look_in_direction(D, path, i-1, y, x, 0, -1); /* zachod */
    look_in_direction(D, path, i-1, y, x, 0, 1); /* wschod */
  }
  path[0].y=src_y;
  path[0].x=src_x;
}

void look_in_direction(int D[][BOARD_SIZE], yxcoords path[INFINITY], int i,
		       int y, int x, int dy, int dx){
  if (is_square_free(y+dy, x+dx))
    if (D[y+dy][x+dx]+1 == D[y][x]){
      path[i].y=y+dy;
      path[i].x=x+dx;
    }
}

void move_marble_along_path(yxcoords path[]){
  int i, marble_color;

  marble_color=board_data[path[0].y][path[0].x].marble;
  for (i=0; path[i].y != NOT_FOUND; i++){
    board_data[path[i].y][path[i].x].marble=marble_color;
    update_board();
    wait(MOVE_DELAY);
    board_data[path[i].y][path[i].x].marble=BLANK;
  }
  board_data[path[i-1].y][path[i-1].x].marble=marble_color;
}

int min(int a, int b){
  if (a < b)
    return a;
  return b;
}

void insert_next_3_marbles_on_board(void){
  extern int is_board_full;
  int i=0;
  int y, x;
  while (i < 3 && is_board_full == FALSE){
    if (is_square_free(y=rand()%BOARD_SIZE, x=rand()%BOARD_SIZE)){
      board_data[y][x].marble=next3marbles[i];
      remove_pattern_if_exists(y, x);
      i++;
      if (number_of_marbles_on_board() >= BOARD_SIZE*BOARD_SIZE)
	is_board_full=TRUE;
    }
  }
}

void get_new_next_3_marbles(void){
  /* losuje liczbe od 1 do N_OF_COLORS */
  next3marbles[0]=rand()%N_OF_COLORS+1;
  next3marbles[1]=rand()%N_OF_COLORS+1;
  next3marbles[2]=rand()%N_OF_COLORS+1;
}

chtype read_keyboard_input(int *cursor_y, int *cursor_x){
    chtype ch;
    switch(ch=getch()){ /* reagowanie na klawisze */
      case 'j':
      case KEY_DOWN:{
	if (*cursor_y < BOARD_SIZE-1) /* czy nie wyjdziemy poza plansze */
	  (*cursor_y)++;
	break;
      }
      case 'k':
      case KEY_UP:{
	if (*cursor_y > 0) /* czy nie wyjdziemy poza plansze */
	  (*cursor_y)--;
	break;
      }
      case 'l':
      case KEY_RIGHT:{
	if (*cursor_x < BOARD_SIZE-1) /* czy nie wyjdziemy poza plansze */
	  (*cursor_x)++;
	break;
      }
      case 'h':
      case KEY_LEFT:{
	if (*cursor_x > 0) /* czy nie wyjdziemy poza plansze */
	  (*cursor_x)--;
	break;
      }
      /* spacja zaznaczamy kulke do przesuniecia lub jej cel */
      case ' ':{
	if (!is_square_free(*cursor_y, *cursor_x)){
	  unhighlight_all_marbles_on_board();
	  board_data[*cursor_y][*cursor_x].highlighted=TRUE;
	}
	else
	  move_marble(*cursor_y, *cursor_x);
	break;
      }
    }
    return ch;
}

void wait(double interval){
  struct timespec time;
  time.tv_sec=(int)interval;
  time.tv_nsec=(int)(BLN-1)*(interval-time.tv_sec);
  while (nanosleep(&time, &time) == -1)
    continue;
}

