/* LINES */

/* Gra polega na ukladaniu pionkow (ktore bedziemy nazywac kulkami, choc
 * w tej implementacji to kwadraciki) w linie na planszy o wymiarach 9x9
 * Jesli ulozysz co najmniej 5 kulek tego samego koloru w linie 
 * pozioma, pionowa lub ukosna, znikna one a za kazda otrzymasz 2 punkty
 * Gra konczy sie, kiedy cala plansza sie zapelni
 * Sterowanie odbywa sie za pomoca kursorow lub klawiszy hjkl i spacji
 * Gra odbywa sie w trybie tekstowym z wykorzystaniem biblioteki ncurses */


/* deklaracje i opisy stalych */

#define BOARD_SIZE 9 	    /* szerokosc i wysokosc planszy w polach (9x9) */
#define N_OF_COLORS 7 	    /* ilosc kolorow kulek (domyslnie 7) */
#define PATTERN_SIZE 5	    /* ile kulek trzeba ulozyc w linii zeby znikly */
#define POINTS4MARBLE 2	    /* ilosc punktow za znikniecie jednej kulki */
#define CURSOR_CHAR 'X'     /* moze tez byc np. ACS_CKBOARD */
#define HL_CHAR ACS_CKBOARD /* HIGHLIGHT_CHAR - wyglad podswietlonej kulki */
#define MOVE_DELAY 0.1	    /* predkosc poruszania sie kulek */
#define BLINKS 3	    /* ile razy kulka ma mrugnac przed zniknieciem */
#define BLINK_DELAY 0.2	    /* szybkosc mrugania kulki */
#define SQ_WIDTH 4	    /* SQUARE_WIDTH - szerokosc pola */
#define SQ_HEIGHT 2	    /* SQUARE_HEIGHT - wysokosc pola */
#define BOARD_WIDTH 37	    /* BOARD_SIZE*SQ_WIDTH+1 - rzeczywista szerokosc */
#define BOARD_HEIGHT 19	    /* BOARD_SIZE*SQ_HEIGHT+1 - rzeczywista wysokosc */
#define BOARD_POS_Y 4
#define BOARD_POS_X 32
#define NEXT3_WIDTH 13	    /* NEXT3 - parametry dla okna next3window */
#define NEXT3_HEIGHT 3
#define NEXT3_POS_Y 6
#define NEXT3_POS_X 8
#define SCORE_POS_Y 12
#define SCORE_POS_X 8
#define TRUE 1
#define FALSE 0
#define BLANK 0		    /* oznacza puste pole */
#define NOT_FOUND -1
#define INFINITY 81	    /* gorne oszacowanie na dlugosc sciezki kulki */
#define BLN 1000000000	    /* billion = miliard = 10^9 */


/* deklaracje typow */

typedef struct coords{
  int y; /* pozycja y na planszy */
  int x; /* pozycja x na planszy */
} yxcoords;


/* deklaracje i opisy funkcji */

void init_screen(void);

void end_screen(void);

void init_color_pairs(void);

void display_welcome_screen(void);

/* przygotowanie do rozpoczecia gry */
void prepare_game(void);

void draw_cursor(int y, int x);

/* narysuj plansze w pamieci i wyswietl ja na ekranie */
void update_board(void);

/* narysuj okno z nastepnymi kulkami w kolejce */
void update_next3window(void);

/* zwraca wcisniety klawisz i reaguje odpowiednio na niego */
chtype read_keyboard_input(int *cursor_y, int *cursor_x);

void remove_old_cursor(int y, int x);

void display_game_over_screen(void);


void init_board(void);

void init_next3window(void);
 
/* ustalamy pozycje pol planszy wzgledem okna board */
void set_square_positions_on_board(void);  

/* rysuje linie siatki na planszy */
void draw_lines_on_board(void);

void draw_marbles_on_board(void);  

/* rysuje w oknie window kulke o kolorze color w pozycji y, x,
 * podswietlona jesli highlighted */
void wdraw_marble(WINDOW *window, int y, int x, int color, int highlighted);

void unhighlight_all_marbles_on_board(void);  

void remove_marbles_from_board(void);  

int number_of_marbles_on_board(void);  


/* jesli wokol punktu y, x utworzyl sie wzor z kulek, usun go
 * i zwroc liczbe punktow do przyznania */
int remove_pattern_if_exists(int y, int x);

/* funkcja uzywana przez remove_pattern_if_exists()
 * sprawdz czy w danym kierunku kulki utworzyly wzor do usuniecia */
void explore_pattern(int pattern[][BOARD_SIZE], int color_match,
		     int y, int x, int dir_y, int dir_x);

int is_square_free(int y, int x);

/* czy w danym polu jest kulka danego koloru? */
int square_color_matches(int y, int x, int color_match);


/* jesli zaznaczono kulke, przesuwa ja na pozycje target_y, target_x,
 * o ile sciezka do tej pozycji nie jest zablokowana */ 
int move_marble(int target_y, int target_x);

/* szuka na planszy zaznaczonej kulki do przesuniecia
 * jesli znaleziono odznacza ja i zwraca 1, jesli nie znaleziono zwraca -1 */
int find_highlighted_marble(int *src_y, int *src_x);

/* stosujemy algorytm Dijkstry do wypelnienia tablicy D
 * odleglosciami od src_y, src_x do danego pola */
void dijkstra(int D[][BOARD_SIZE], int scr_y, int src_x);

/* algorytm znajdowania najkrotszej sciezki przy odleglosciach
 * z tablicy D znalezionych przez algorytm Dijkstry */
void find_shortest_path(yxcoords path[INFINITY], int D[][BOARD_SIZE],
			int target_y, int target_x, int src_y, int src_x);

/* funkcja uzywana przez find_shortest_path() */
void look_in_direction(int D[][BOARD_SIZE], yxcoords path[INFINITY], int i,
		       int y, int x, int dy, int dx);

void move_marble_along_path(yxcoords path[]);

int min(int a, int b);


void insert_next_3_marbles_on_board(void);

void get_new_next_3_marbles(void);

/* czeka zadana ilosc sekund */
void wait(double interval);

