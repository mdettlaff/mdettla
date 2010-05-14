/*
 * Gra w kółko i krzyżyk dla dwóch graczy, na dużej planszy.
 * Kto pierwszy zaznaczy 5 pól swojego koloru w jednej linii (poziomo,
 * pionowo lub na skos), wygrywa.
 *
 * Każdy gracz uruchamia swoją własną instancję programu. Kliknięcie na polu
 * planszy powoduje jego zaznaczenie kolorem gracza.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <X11/Xlib.h>

#include "binary_sem.h"

#define TRUE 1
#define FALSE 0
#define BOARD_SIZE 10 // plansza ma wymiary BOARD_SIZE x BOARD_SIZE pól
#define CELL_SIZE 50 // pole planszy ma wymiary CELL_SIZE x CELL_SIZE pikseli
#define LINE_LEN 5 // ile pól trzeba zaznaczyć w jednej linii aby wygrać
#define EMPTY_CELL ' '
#define SHM_KEY 1236 // klucz pamięci współdzielonej
#define SEM_KEY 1116 // klucz semafora


typedef int bool_t; // typ boolean

// struktura przechowująca zmienne znajdujące się w pamięci współdzielonej
struct shared_use_st {
    char board[BOARD_SIZE][BOARD_SIZE]; // plansza na której odbywa się gra
    bool_t is_player_quit; // czy gracz wyszedł z gry
};


// zmienne globalne
int sem_id;
int shm_id;
void *shared_memory;
int player_id; // numer gracza, może być równy 1 lub 2

// zmienne globalne Xlib
Display *display;
Window window;
XSetWindowAttributes window_attributes;
XGCValues gcvalues;
GC gc;
Visual *visual;
int depth;
int screen;
Colormap colormap;
XColor foreground, color1, color2, dummy;
XEvent event;


/*===========================================================================*/
/* Funkcje związane z inicjalizacją i sprzątaniem po semaforach oraz pamięci */
/* współdzielonej.                                                           */
/*===========================================================================*/

void clean_up() {
    // pozbywamy się semafów
    printf("usuwam semafor\n");
    del_semvalue(sem_id);
    // pozbywamy się pamięci współdzielonej
    printf("odłączam pamięć współdzieloną... ");
    if (shmdt(shared_memory) == -1) {
        fprintf(stderr, "shmdt failed\n");
        exit(EXIT_FAILURE);
    } else {
        printf("OK\n");
    }
    if (shmctl(shm_id, IPC_RMID, 0) == -1) {
        fprintf(stderr, "shmctl(IPC_RMID) failed\n");
        exit(EXIT_FAILURE);
    }
}

void quit_game(int sig) {
    struct shared_use_st *shared_variables;
    shared_variables = (struct shared_use_st *)shared_memory;
    printf("koniec gry\n");
    shared_variables->is_player_quit = TRUE;
    if (!semaphore_v(sem_id, 1 - (player_id - 1))) exit(EXIT_FAILURE);
    clean_up();
    exit(EXIT_SUCCESS);
}

void init_shared_memory() {
    shm_id = shmget((key_t)SHM_KEY, sizeof(struct shared_use_st),
            0666 | IPC_CREAT);
    if (shm_id == -1) {
        fprintf(stderr, "shmget failed\n");
        exit(EXIT_FAILURE);
    }
    shared_memory = shmat(shm_id, (void *)0, 0);
    if (shared_memory == (void *)-1) {
        fprintf(stderr, "shmat failed\n");
        exit(EXIT_FAILURE);
    }
    struct shared_use_st *shared_variables;
    shared_variables = (struct shared_use_st *)shared_memory;
    int i, j;
    for (i = 0; i < BOARD_SIZE; i++) {
        for (j = 0; j < BOARD_SIZE; j++) {
            (shared_variables->board)[i][j] = EMPTY_CELL;
        }
    }
    shared_variables->is_player_quit = FALSE;
}

int init_semaphores() {
    int player_id; // graczem 1 zostaje ten, kto pierwszy utworzył semafory
    if ((sem_id = semget((key_t)SEM_KEY, 2, 0666 | IPC_CREAT | IPC_EXCL))
            != -1) {
        player_id = 1;
    } else {
        sem_id = semget((key_t)SEM_KEY, 2, 0666 | IPC_CREAT);
        player_id = 2;
    }
    if (!set_semvalue(sem_id, 0, 0)) {
        fprintf(stderr, "failed to initialize semaphore\n");
        exit(EXIT_FAILURE);
    }
    if (!set_semvalue(sem_id, 1, 1)) {
        fprintf(stderr, "failed to initialize semaphore\n");
        exit(EXIT_FAILURE);
    }
    return player_id;
}


/*===========================================================================*/
/* Funkcje związane z logiką gry.                                            */
/*===========================================================================*/

bool_t is_board_full(char board[BOARD_SIZE][BOARD_SIZE]) {
    int i, j;
    for (i = 0; i < BOARD_SIZE; i++) {
        for (j = 0; j < BOARD_SIZE; j++) {
            if (board[i][j] == EMPTY_CELL) {
                return FALSE;
            }
        }
    }
    return TRUE;
}

/*
 * Czy na planszy jest ułożona linia ze znaków mark pionowo, poziomo lub
 * na skos.
 */
bool_t is_line_complete(char mark, char board[BOARD_SIZE][BOARD_SIZE]) {
    int i, j, k;
    bool_t is_line_complete = FALSE;
    // poziomo
    for (i = 0; i < BOARD_SIZE; i++) {
        for (j = 0; j < BOARD_SIZE - LINE_LEN; j++) {
            is_line_complete = TRUE;
            for (k = 0; k < LINE_LEN; k++) {
                if (board[i][j + k] != mark) {
                    is_line_complete = FALSE;
                }
            }
            if (is_line_complete) {
                return TRUE;
            }
        }
    }
    // pionowo
    for (i = 0; i < BOARD_SIZE - LINE_LEN; i++) {
        for (j = 0; j < BOARD_SIZE; j++) {
            is_line_complete = TRUE;
            for (k = 0; k < LINE_LEN; k++) {
                if (board[i + k][j] != mark) {
                    is_line_complete = FALSE;
                }
            }
            if (is_line_complete) {
                return TRUE;
            }
        }
    }
    // na skos
    is_line_complete = TRUE;
    for (i = 0; i < BOARD_SIZE - LINE_LEN + 1; i++) {
        for (j = 0; j < BOARD_SIZE - LINE_LEN + 1; j++) {
            // na skos NW-SE
            is_line_complete = TRUE;
            for (k = 0; k < LINE_LEN; k++) {
                if (board[i + k][j + k] != mark) {
                    is_line_complete = FALSE;
                }
            }
            if (is_line_complete) {
                return TRUE;
            }
            // na skos NE-SW
            is_line_complete = TRUE;
            for (k = 0; k < LINE_LEN; k++) {
                if (board[i + k][j + LINE_LEN - 1 - k] != mark) {
                    is_line_complete = FALSE;
                }
            }
            if (is_line_complete) {
                return TRUE;
            }
        }
    }
    return FALSE;
}

char get_player_mark(int player_id) {
    return player_id == 1 ? 'X' : 'O';
}

/*
 * Sprawdza, czy należy już zakończyć grę (z powodu wygranej, remisu lub
 * wyłączenia gry przez któregoś z graczy).
 */
void check_winner(struct shared_use_st *shared_variables, int player_id) {
    int winner;
    if (is_line_complete(get_player_mark(1), shared_variables->board)) {
        winner = 1; // wygrał gracz 1
    } else if (is_line_complete(get_player_mark(2), shared_variables->board)) {
        winner = 2; // wygrał gracz 2
    } else if (is_board_full(shared_variables->board)) {
        winner = 0; // remis
    } else {
        winner = -1; // gra toczy się dalej
    }
    if (winner > 0) {
        if (player_id == winner) {
            printf("wygrałeś!\n");
            // zwalnia przegranego, żeby mógł posprzątać
            if (!semaphore_v(sem_id, 1 - (player_id - 1))) exit(EXIT_FAILURE);
        } else {
            printf("przegrałeś\n");
            clean_up(); // przegrany musi posprzątać
        }
        exit(EXIT_SUCCESS);
    } else if (winner == 0) {
        printf("remis\n");
        if (player_id == 2) {
            // zwalnia gracza 1, żeby mógł posprzątać
            if (!semaphore_v(sem_id, 1 - (player_id - 1))) exit(EXIT_FAILURE);
        } else {
            clean_up(); // w razie remisu gracz 1 sprząta
        }
        exit(EXIT_SUCCESS);
    } else if (shared_variables->is_player_quit) {
        printf("przeciwnik zrezygnował z dalszej gry\n");
        exit(EXIT_SUCCESS);
    }
}

void write_move(int row, int column, char board[BOARD_SIZE][BOARD_SIZE],
        int player_id) {
    board[row][column] = get_player_mark(player_id);
}

bool_t is_legal_move(int row, int column, char board[BOARD_SIZE][BOARD_SIZE]) {
    return row >= 0 && row < BOARD_SIZE && column >= 0 && column < BOARD_SIZE
        && board[row][column] == EMPTY_CELL;
}


/*===========================================================================*/
/* Funkcje związane z biblioteką Xlib.                                       */
/*===========================================================================*/

void init_display() {
    display = XOpenDisplay("");
    screen = DefaultScreen(display);
    visual = DefaultVisual(display, screen);
    depth = DefaultDepth(display, screen);
    window_attributes.background_pixel = XWhitePixel(display, screen);
    window_attributes.override_redirect = False;

    int window_size = BOARD_SIZE * CELL_SIZE;
    window = XCreateWindow(display, XRootWindow(display, screen),
            100, 100, window_size, window_size + 50, 0, depth, InputOutput,
            visual, CWBackPixel|CWOverrideRedirect,
            &window_attributes);
    XSelectInput(display, window, ExposureMask|KeyPressMask|ButtonPressMask);

    colormap = DefaultColormap(display, screen);
    XAllocNamedColor(display, colormap, "black", &foreground, &dummy);
    XAllocNamedColor(display, colormap, "red", &color1, &dummy);
    XAllocNamedColor(display, colormap, "blue", &color2, &dummy);

    XMapWindow(display, window);
    gc = DefaultGC(display, screen);

    XFontStruct* font_info;
    char* font_name = "*-helvetica-medium-r-*-24-*";
    font_info = XLoadQueryFont(display, font_name);
    if (!font_info) {
        fprintf(stderr, "XLoadQueryFont: failed loading font %s\n", font_name);
    }
    XSetFont(display, gc, font_info->fid);
}

void draw_board(char board[BOARD_SIZE][BOARD_SIZE]) {
    int i, j;
    XSetForeground(display, gc, foreground.pixel);
    // rysuj linie poziome
    for (i = 0; i < BOARD_SIZE + 1; i++) {
        XDrawLine(display, window, gc,
                0, i * CELL_SIZE,
                BOARD_SIZE * CELL_SIZE, i * CELL_SIZE);
    }
    // rysuj linie pionowe
    for (i = 0; i < BOARD_SIZE + 1; i++) {
        XDrawLine(display, window, gc,
                i * CELL_SIZE, 0,
                i * CELL_SIZE, BOARD_SIZE * CELL_SIZE);
    }
    // rysuje pola oznaczone przez graczy
    for (i = 0; i < BOARD_SIZE; i++) {
        for (j = 0; j < BOARD_SIZE; j++) {
            if (board[i][j] == get_player_mark(1)) {
                XSetForeground(display, gc, color1.pixel);
            } else {
                XSetForeground(display, gc, color2.pixel);
            }
            if (board[i][j] != EMPTY_CELL) {
                XFillArc(display, window, gc,
                        j * CELL_SIZE, i * CELL_SIZE,
                        CELL_SIZE, CELL_SIZE,
                        0, 360 * 64);
            }
        }
    }
    XFlush(display);
}

/*
 * Rysuje informacje o grze wyświetlane pod planszą.
 */
void draw_info(char *message) {
    XClearArea(display, window, 0, BOARD_SIZE * CELL_SIZE + 1,
            BOARD_SIZE * CELL_SIZE, 50, False);
    XSetForeground(display, gc, foreground.pixel);
    XDrawString(display, window, gc, 25, BOARD_SIZE * CELL_SIZE + 32,
            message, strlen(message));
    XFlush(display);
}

void set_window_title() {
    char title[128];
    sprintf(title, "Gracz %d", player_id);
    XStoreName(display, window, title);
}

void init_window(char board[BOARD_SIZE][BOARD_SIZE],
        int player_id, int other_player_id) {
    XNextEvent(display, &event); // czekamy na pierwsze zdarzenie Expose
    set_window_title();
    draw_board(board);
    draw_info("oczekiwanie na drugiego gracza...");
}

/*
 * Czeka, aż użytkownik wykona prawidłowy ruch poprzez kliknięcie na wolne
 * pole na planszy.
 */
void read_legal_move(int *row, int *column,
        char board[BOARD_SIZE][BOARD_SIZE]) {
    bool_t is_legal_move_read = FALSE;
    int i, j;
    while (XPending(display) > 0) {
        // ignoruje ruchy wykonane w czasie oczekiwania na ruch przeciwnika
        XNextEvent(display, &event);
    }
    while (!is_legal_move_read) {
        XNextEvent(display, &event);
        switch (event.type) {
            case ButtonPress:
                i = event.xbutton.y / CELL_SIZE;
                j = event.xbutton.x / CELL_SIZE;
                if (is_legal_move(i, j, board)) {
                    *row = i;
                    *column = j;
                    is_legal_move_read = TRUE;
                }
                break;
            case KeyPress:
                XCloseDisplay(display);
                quit_game(0);
                break;
        }
    }
}


/*
 * Pętla główna gry.
 */
void play_tic_tac_toe(int player_id) {
    struct shared_use_st *shared_variables;
    shared_variables = (struct shared_use_st *)shared_memory;
    int other_player_id = 3 - player_id;
    init_window(shared_variables->board, player_id, other_player_id);
    while (TRUE) {
        if (!semaphore_p(sem_id, player_id - 1)) exit(EXIT_FAILURE);
        draw_board(shared_variables->board);
        draw_info("twoja kolej");
        check_winner(shared_variables, player_id);
        int row, column; // indeksowane od zera
        read_legal_move(&row, &column, shared_variables->board);
        write_move(row, column, shared_variables->board, player_id);
        draw_board(shared_variables->board);
        draw_info("oczekiwanie na ruch przeciwnika...");
        check_winner(shared_variables, player_id);
        if (!semaphore_v(sem_id, 1 - (player_id - 1))) exit(EXIT_FAILURE);
    }
}

int main() {
    signal(SIGINT, quit_game);

    init_display();
    init_shared_memory();
    // graczem 1 zostaje ten, kto pierwszy utworzył semafory
    player_id = init_semaphores();

    play_tic_tac_toe(player_id);

    return EXIT_SUCCESS;
}
