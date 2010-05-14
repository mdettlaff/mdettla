#include <stdio.h>
#include <stdlib.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <signal.h>
#include <X11/Xlib.h>
#include <X11/X.h>

#include "binary_sem.h"

#define TRUE 1
#define FALSE 0
#define BOARD_SIZE 10 // plansza ma wymiary BOARD_SIZE x BOARD_SIZE
#define CELL_SIZE 50 // pole na planszy ma wymiary CELL_SIZE x CELL_SIZE
#define LINE_LEN 5 // ile pól trzeba zaznaczyć w jednej linii aby wygrać
#define EMPTY_CELL ' '
#define SHM_KEY 1236 // klucz pamięci współdzielonej
#define SEM_KEY 1116 // klucz semafora


typedef int bool_t; // typ boolean

// struktura przechowująca zmienne znajdujące się w pamięci współdzielonej
struct shared_use_st {
    char board[BOARD_SIZE][BOARD_SIZE];
    bool_t is_player_quit;
};


// zmienne globalne
int sem_id;
int shm_id;
void *shared_memory;
int player_id;

// zmienne globalne Xlib
Display *mydisplay;
Window mywindow;
XSetWindowAttributes mywindowattributes;
XGCValues mygcvalues;
GC mygc;
Visual *myvisual;
int mydepth;
int myscreen;
Colormap mycolormap;
XColor foreground, mycolor1, mycolor2, dummy;
XEvent myevent;


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
        fprintf(stderr, "Failed to initialize semaphore\n");
        exit(EXIT_FAILURE);
    }
    if (!set_semvalue(sem_id, 1, 1)) {
        fprintf(stderr, "Failed to initialize semaphore\n");
        exit(EXIT_FAILURE);
    }
    return player_id;
}

char get_player_mark(int player_id) {
    return player_id == 1 ? 'X' : 'O';
}

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
 * na ukos.
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
    // na ukos
    is_line_complete = TRUE;
    for (i = 0; i < BOARD_SIZE - LINE_LEN; i++) {
        for (j = 0; j < BOARD_SIZE - LINE_LEN; j++) {
            // na ukos NW-SE
            is_line_complete = TRUE;
            for (k = 0; k < LINE_LEN; k++) {
                if (board[i + k][j + k] != mark) {
                    is_line_complete = FALSE;
                }
            }
            if (is_line_complete) {
                return TRUE;
            }
            // na ukos NE-SW
            is_line_complete = TRUE;
            for (k = 0; k < LINE_LEN; k++) {
                if (board[i + k][LINE_LEN + j - k] != mark) {
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
            // zwalniamy przegranego, żeby mógł posprzątać
            if (!semaphore_v(sem_id, 1 - (player_id - 1))) exit(EXIT_FAILURE);
        } else {
            printf("przegrałeś\n");
            clean_up(); // przegrany musi posprzątać
        }
        exit(EXIT_SUCCESS);
    } else if (winner == 0) {
        printf("remis\n");
        if (player_id == 2) {
            // zwalniamy gracza 1, żeby mógł posprzątać
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
    return row >= 1 && row <= BOARD_SIZE && column >= 1 && column <= BOARD_SIZE
        && board[row][column] == EMPTY_CELL;
}

/*
 * Czeka, aż użytkownik wykona prawidłowy ruch poprzez kliknięcie na wolne
 * pole na planszy.
 */
void read_legal_move(int *row, int *column,
        char board[BOARD_SIZE][BOARD_SIZE]) {
    bool_t is_legal_move_read = FALSE;
    int i, j;
    while (!is_legal_move_read) {
        XNextEvent(mydisplay, &myevent);
        switch (myevent.type) {
            case Expose:
                break;
            case ButtonPress:
                i = myevent.xbutton.y / CELL_SIZE;
                j = myevent.xbutton.x / CELL_SIZE;
                if (is_legal_move(i, j, board)) {
                    *row = i;
                    *column = j;
                    is_legal_move_read = TRUE;
                }
                break;
            case KeyPress:
                XCloseDisplay(mydisplay);
                quit_game(0);
                break;
        }
        printf("event processed\n");
    }
}

void print_board(char board[BOARD_SIZE][BOARD_SIZE]) {
    int i, j;
    for (i = 0; i < BOARD_SIZE; i++) {
        if (i != 0) {
            for (j = 0; j < BOARD_SIZE; j++) {
                if (j != 0) {
                    printf("+");
                }
                printf("-");
            }
            printf("\n");
        }
        for (j = 0; j < BOARD_SIZE; j++) {
            if (j != 0) {
                printf("|");
            }
            printf("%c", board[i][j]);
        }
        printf("\n");
    }
    XSetForeground(mydisplay, mygc, foreground.pixel);
    // rysuj linie poziome
    for (i = 0; i < BOARD_SIZE; i++) {
        XDrawLine(mydisplay, mywindow, mygc,
                0, i * CELL_SIZE,
                BOARD_SIZE * CELL_SIZE, i * CELL_SIZE);
    }
    // rysuj linie pionowe
    for (i = 0; i < BOARD_SIZE; i++) {
        XDrawLine(mydisplay, mywindow, mygc,
                i * CELL_SIZE, 0,
                i * CELL_SIZE, BOARD_SIZE * CELL_SIZE);
    }
    // rysujemy pola oznaczone przez graczy
    for (i = 0; i < BOARD_SIZE; i++) {
        for (j = 0; j < BOARD_SIZE; j++) {
            if (board[i][j] == get_player_mark(1)) {
                XSetForeground(mydisplay, mygc, mycolor1.pixel);
            } else {
                XSetForeground(mydisplay, mygc, mycolor2.pixel);
            }
            if (board[i][j] != EMPTY_CELL) {
                XFillArc(mydisplay, mywindow, mygc,
                        j * CELL_SIZE, i * CELL_SIZE,
                        CELL_SIZE, CELL_SIZE,
                        0, 360 * 64);
            }
        }
    }
    XFlush(mydisplay);
}

void play_tic_tac_toe(int player_id) {
    struct shared_use_st *shared_variables;
    shared_variables = (struct shared_use_st *)shared_memory;
    int other_player_id = 3 - player_id;
    if (player_id == 1) {
        print_board(shared_variables->board);
        printf("oczekiwanie na ruch gracza %d...\n", other_player_id);
    }
    while (TRUE) {
        if (!semaphore_p(sem_id, player_id - 1)) exit(EXIT_FAILURE);
        print_board(shared_variables->board);
        check_winner(shared_variables, player_id);
        printf("gracz %d: ", player_id);
        printf("podaj pozycję rząd, kolumna: ");
        int row, column;
        read_legal_move(&row, &column, shared_variables->board);
        write_move(row, column, shared_variables->board, player_id);
        print_board(shared_variables->board);
        check_winner(shared_variables, player_id);
        printf("oczekiwanie na ruch gracza %d...\n", other_player_id);
        if (!semaphore_v(sem_id, 1 - (player_id - 1))) exit(EXIT_FAILURE);
    }
}

void init_display() {
    mydisplay = XOpenDisplay("");
    myscreen = DefaultScreen(mydisplay);
    myvisual = DefaultVisual(mydisplay, myscreen);
    mydepth = DefaultDepth(mydisplay, myscreen);
    mywindowattributes.background_pixel = XWhitePixel(mydisplay, myscreen);
    mywindowattributes.override_redirect = False;

    int window_size = BOARD_SIZE * CELL_SIZE;
    mywindow = XCreateWindow(mydisplay, XRootWindow(mydisplay, myscreen),
            100, 100, window_size, window_size, 10, mydepth, InputOutput,
            myvisual, CWBackPixel|CWOverrideRedirect,
            &mywindowattributes);

    XSelectInput(mydisplay, mywindow,
            ExposureMask|KeyPressMask|ButtonPressMask);

    mycolormap = DefaultColormap(mydisplay, myscreen);

    XAllocNamedColor(mydisplay, mycolormap, "black", &foreground, &dummy);
    XAllocNamedColor(mydisplay, mycolormap, "blue", &mycolor1, &dummy);
    XAllocNamedColor(mydisplay, mycolormap, "red", &mycolor2, &dummy);

    XMapWindow(mydisplay, mywindow);
    mygc = DefaultGC(mydisplay, myscreen);
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
