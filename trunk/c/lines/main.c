/* linkowac z opcjami -lncursesw -lrt */

#include <ncursesw/ncurses.h>
#include <stdlib.h>
#include <time.h>
#include "lines.h"

/* zmienne globalne */
int score=0;
int is_board_full=FALSE;

int main(){
  /* wspolrzedne kursora na planszy BOARD_SIZExBOARD_SIZE */
  int cursor_y=BOARD_SIZE/2;
  int cursor_x=BOARD_SIZE/2;
  chtype ch; /* wciskany klawisz */

  srand(time(NULL)); 	    /* inicjacja generatora liczb pseudolosowych */
  init_screen();  
  init_color_pairs();	    /* definicje kolorow */
  display_welcome_screen();

  prepare_game();
  /* petla glowna gry */
  while (ch != 'q' && !is_board_full){
    draw_cursor(cursor_y, cursor_x); /* narysuj kursor */
    update_board(); /* narysuj plansze i wyswietl na ekranie */
    update_next3window(); /* narysuj nastepne 3 kulki w kolejce */
    mvprintw(SCORE_POS_Y, SCORE_POS_X, "%d", score); /* narysuj wynik */
    ch=read_keyboard_input(&cursor_y, &cursor_x);
    remove_old_cursor(cursor_y, cursor_x);
  }

  if (is_board_full)
    display_game_over_screen();
  end_screen();  
  return 0;  
}
