/*
 * Gra w kółko i krzyżyk dla dwóch graczy, na dużej planszy.
 * Kto pierwszy zaznaczy 5 pól swojego koloru w jednej linii (poziomo,
 * pionowo lub na skos), ten wygrywa.
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
#define SHM_KEY 1236 // klucz pamięci współdzielonej
#define SEM_KEY 1116 // klucz semafora

#define BOARD_SIZE 12 // plansza ma wymiary BOARD_SIZE x BOARD_SIZE pól
#define LINE_LEN 5 // ile pól trzeba zaznaczyć w jednej linii aby wygrać
#define CELL_SIZE 50 // pole planszy ma wymiary CELL_SIZE x CELL_SIZE pikseli
#define LEFT_MARGIN 25 // odległość tekstu od lewego brzegu okna
#define TOP_MARGIN 50 // odległość między planszą a górnym brzegiem okna
#define BOTTOM_MARGIN 50 // odległość między planszą a dolnym brzegiem okna
#define EMPTY_CELL 0 // wartość wolnego pola na planszy


// typ boolowski
typedef int bool_t;
// struktura przechowująca zmienne znajdujące się w pamięci współdzielonej
typedef struct shared_st {
    int board[BOARD_SIZE][BOARD_SIZE]; // plansza na której odbywa się gra
    bool_t is_only_one_player;
} shared_t;
// współrzędne pola na planszy, indeksowane od zera
typedef struct coords_st {
    int row;
    int column;
} coords_t;


// zmienne globalne
int sem_id;
int shm_id;
void *shared_memory;
shared_t *shared_variables;
int player_id; // numer gracza, może być równy 1 lub 2

// zmienne globalne Xlib
Display *display;
Window window;
GC gc;
XColor foreground, color1, color2, dummy;
XEvent event;
Atom wmDeleteMessage;


/*===========================================================================*/
/* Funkcje związane z inicjalizacją i sprzątaniem po semaforach oraz pamięci */
/* współdzielonej.                                                           */
/*===========================================================================*/

void init_shared_memory() {
    shm_id = shmget((key_t)SHM_KEY, sizeof(shared_t),
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
    shared_variables = (shared_t *)shared_memory;
    int i, j;
    for (i = 0; i < BOARD_SIZE; i++) {
        for (j = 0; j < BOARD_SIZE; j++) {
            (shared_variables->board)[i][j] = EMPTY_CELL;
        }
    }
    shared_variables->is_only_one_player = TRUE;
}

/*
 * Tworzy semafory i zwraca numer gracza. Graczem 1 zostaje ten, kto pierwszy
 * utworzył semafory.
 */
int init_semaphores() {
    int player_id;
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

void clean_up() {
    printf("usuwam semafory\n");
    del_semvalue(sem_id);
    printf("usuwam pamięć współdzieloną\n");
    if (shmdt(shared_memory) == -1) {
        fprintf(stderr, "shmdt failed\n");
        exit(EXIT_FAILURE);
    }
    if (shmctl(shm_id, IPC_RMID, 0) == -1) {
        fprintf(stderr, "shmctl(IPC_RMID) failed\n");
        exit(EXIT_FAILURE);
    }
}

void quit_game(int sig) {
    XCloseDisplay(display);
    if (!shared_variables->is_only_one_player) {
        shared_variables->is_only_one_player = TRUE;
        // kto wyłącza grę pierwszy, ten zwalnia przeciwnika
        if (!semaphore_v(sem_id, 1 - (player_id - 1))) exit(EXIT_FAILURE);
    } else {
        // kto wyłącza grę ostatni, ten sprząta
        clean_up();
    }
    exit(EXIT_SUCCESS);
}


/*===========================================================================*/
/* Funkcje Graficznego Interfejsu Użytkownika (w Xlib).                      */
/*===========================================================================*/

void init_display() {
    display = XOpenDisplay("");
    if (display == NULL) {
        fprintf(stderr, "cannot open display\n");
        exit(EXIT_FAILURE);
    }
    int screen = DefaultScreen(display);
    Visual *visual = DefaultVisual(display, screen);
    int depth = DefaultDepth(display, screen);
    XSetWindowAttributes window_attributes;
    window_attributes.background_pixel = XWhitePixel(display, screen);
    window_attributes.override_redirect = False;

    int window_size = BOARD_SIZE * CELL_SIZE;
    window = XCreateWindow(display, XRootWindow(display, screen),
            100, 100, window_size, window_size + TOP_MARGIN + BOTTOM_MARGIN,
            0, depth, InputOutput, visual, CWBackPixel|CWOverrideRedirect,
            &window_attributes);
    XSelectInput(display, window, ExposureMask|ButtonPressMask);

    Colormap colormap = DefaultColormap(display, screen);
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

    wmDeleteMessage = XInternAtom(display, "WM_DELETE_WINDOW", False);
    XSetWMProtocols(display, window, &wmDeleteMessage, 1);
}

/*
 * Wyświetla imię gracza nad planszą.
 */
void draw_player_name(int player_id) {
    char text[128];
    sprintf(text, "Gracz %d", player_id);
    XColor player_color;
    if (player_id == 1) {
        player_color = color1;
    } else {
        player_color = color2;
    }
    XSetForeground(display, gc, player_color.pixel);
    XDrawString(display, window, gc, LEFT_MARGIN, 32, text, strlen(text));
    XFlush(display);
}

/*
 * Rysuje planszę.
 */
void draw_board(int board[BOARD_SIZE][BOARD_SIZE]) {
    int i, j;
    XSetForeground(display, gc, foreground.pixel);
    // rysuje linie poziome
    for (i = 0; i < BOARD_SIZE + 1; i++) {
        XDrawLine(display, window, gc,
                0, BOTTOM_MARGIN + i * CELL_SIZE,
                BOARD_SIZE * CELL_SIZE, BOTTOM_MARGIN + i * CELL_SIZE);
    }
    // rysuje linie pionowe
    for (i = 0; i < BOARD_SIZE + 1; i++) {
        XDrawLine(display, window, gc,
                i * CELL_SIZE, BOTTOM_MARGIN,
                i * CELL_SIZE, BOTTOM_MARGIN + BOARD_SIZE * CELL_SIZE);
    }
    // rysuje pola zaznaczone przez graczy ich kolorami
    for (i = 0; i < BOARD_SIZE; i++) {
        for (j = 0; j < BOARD_SIZE; j++) {
            XColor player_color;
            if (board[i][j] == 1) {
                player_color = color1;
            } else {
                player_color = color2;
            }
            XSetForeground(display, gc, player_color.pixel);
            if (board[i][j] != EMPTY_CELL) {
                XFillArc(display, window, gc,
                        j * CELL_SIZE, BOTTOM_MARGIN + i * CELL_SIZE,
                        CELL_SIZE, CELL_SIZE,
                        0, 360 * 64);
            }
        }
    }
    XFlush(display);
}

/*
 * Wyświetla napis game_info pod planszą. Jeśli parametr game_info jest
 * równy NULL, wyświetla ostatnio podany napis, który nie był równy NULL.
 */
void draw_info(char *game_info) {
    static char *info;
    if (game_info != NULL) {
        info = game_info;
    }
    XClearArea(display, window, 0, BOTTOM_MARGIN + BOARD_SIZE * CELL_SIZE + 1,
            BOARD_SIZE * CELL_SIZE, BOTTOM_MARGIN, False);
    XSetForeground(display, gc, foreground.pixel);
    XDrawString(display, window, gc,
            LEFT_MARGIN, BOTTOM_MARGIN + BOARD_SIZE * CELL_SIZE + 32,
            info, strlen(info));
    XFlush(display);
}

/*
 * Rysuje od nowa zawartość okna.
 */
void draw_everything(int board[BOARD_SIZE][BOARD_SIZE], int player_id) {
    draw_player_name(player_id);
    draw_board(board);
    draw_info(NULL);
}

void init_window(int board[BOARD_SIZE][BOARD_SIZE], int player_id) {
    XNextEvent(display, &event); // czekamy na pierwsze zdarzenie Expose
    XStoreName(display, window, "Tic Tac Toe");
    draw_info("oczekiwanie na drugiego gracza...");
    draw_everything(board, player_id);
}

void wait_until_close_window() {
    while (TRUE) {
        XNextEvent(display, &event);
        switch (event.type) {
            case ClientMessage:
                if (event.xclient.data.l[0] == wmDeleteMessage) {
                    quit_game(0);
                }
            case Expose:
                draw_everything(shared_variables->board, player_id);
                break;
        }
    }
}

bool_t is_legal_move(coords_t *coords, int board[BOARD_SIZE][BOARD_SIZE]) {
    return coords->row >= 0 && coords->row < BOARD_SIZE
        && coords->column >= 0 && coords->column < BOARD_SIZE
        && board[coords->row][coords->column] == EMPTY_CELL;
}

/*
 * Czeka, aż użytkownik wykona prawidłowy ruch poprzez kliknięcie na wolne
 * pole na planszy, po czym zwraca współrzędne tego pola.
 */
coords_t *read_legal_move(int board[BOARD_SIZE][BOARD_SIZE]) {
    coords_t *coords = malloc(sizeof(*coords));
    while (XPending(display) > 0) {
        // ignorujemy ruchy wykonane w czasie oczekiwania na ruch przeciwnika
        XNextEvent(display, &event);
    }
    bool_t is_legal_move_read = FALSE;
    while (!is_legal_move_read) {
        XNextEvent(display, &event);
        switch (event.type) {
            case ButtonPress:
                coords->row = (event.xbutton.y - BOTTOM_MARGIN) / CELL_SIZE;
                coords->column = event.xbutton.x / CELL_SIZE;
                if (is_legal_move(coords, board)) {
                    is_legal_move_read = TRUE;
                }
                break;
            case ClientMessage:
                if (event.xclient.data.l[0] == wmDeleteMessage) {
                    quit_game(0);
                }
                break;
            case Expose:
                draw_everything(board, player_id);
                break;
        }
    }
    return coords;
}


/*===========================================================================*/
/* Funkcje związane z logiką gry.                                            */
/*===========================================================================*/

bool_t is_board_full(int board[BOARD_SIZE][BOARD_SIZE]) {
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
 * Czy na planszy jest ułożona linia pionowo, poziomo lub na skos przez
 * danego gracza.
 */
bool_t is_line_complete(int board[BOARD_SIZE][BOARD_SIZE], int player_id) {
    int i, j, k;
    bool_t is_line_complete = FALSE;
    // poziomo
    for (i = 0; i < BOARD_SIZE; i++) {
        for (j = 0; j < BOARD_SIZE - LINE_LEN + 1; j++) {
            is_line_complete = TRUE;
            for (k = 0; k < LINE_LEN; k++) {
                if (board[i][j + k] != player_id) {
                    is_line_complete = FALSE;
                }
            }
            if (is_line_complete) {
                return TRUE;
            }
        }
    }
    // pionowo
    for (i = 0; i < BOARD_SIZE - LINE_LEN + 1; i++) {
        for (j = 0; j < BOARD_SIZE; j++) {
            is_line_complete = TRUE;
            for (k = 0; k < LINE_LEN; k++) {
                if (board[i + k][j] != player_id) {
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
                if (board[i + k][j + k] != player_id) {
                    is_line_complete = FALSE;
                }
            }
            if (is_line_complete) {
                return TRUE;
            }
            // na skos NE-SW
            is_line_complete = TRUE;
            for (k = 0; k < LINE_LEN; k++) {
                if (board[i + k][j + LINE_LEN - 1 - k] != player_id) {
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

/*
 * Zaznacza pole o podanych współrzędnych dla podanego gracza.
 */
void make_move(coords_t *coords,
        int board[BOARD_SIZE][BOARD_SIZE], int player_id) {
    board[coords->row][coords->column] = player_id;
}

/*
 * Sprawdza, czy należy już skończyć grę (z powodu wygranej, remisu lub
 * wyłączenia gry przez któregoś z graczy).
 */
void check_winner(int board[BOARD_SIZE][BOARD_SIZE], int player_id) {
    int other_player_id = 3 - player_id;
    if (is_line_complete(board, player_id)) {
        draw_info("wygrana!");
    } else if (is_line_complete(board, other_player_id)) {
        draw_info("przegrana");
    } else if (is_board_full(board)) {
        draw_info("remis");
    } else if (shared_variables->is_only_one_player) {
        draw_info("wygrana walkowerem");
    } else {
        return; // gramy dalej
    }
    if (!semaphore_v(sem_id, 1 - (player_id - 1))) exit(EXIT_FAILURE);
    wait_until_close_window();
}

/*
 * Pętla główna gry.
 */
void play_tic_tac_toe(int board[BOARD_SIZE][BOARD_SIZE], int player_id) {
    if (player_id != 1) {
        shared_variables->is_only_one_player = FALSE;
    }
    while (TRUE) {
        if (!semaphore_p(sem_id, player_id - 1)) exit(EXIT_FAILURE);
        draw_everything(board, player_id);
        draw_info("twoja kolej");
        check_winner(board, player_id);
        make_move(read_legal_move(board), board, player_id);
        draw_everything(board, player_id);
        draw_info("oczekiwanie na ruch przeciwnika...");
        check_winner(board, player_id);
        if (!semaphore_v(sem_id, 1 - (player_id - 1))) exit(EXIT_FAILURE);
    }
}


int main() {
    signal(SIGINT, quit_game);

    init_display();
    init_shared_memory();
    player_id = init_semaphores();
    init_window(shared_variables->board, player_id);

    play_tic_tac_toe(shared_variables->board, player_id);

    return EXIT_SUCCESS;
}
