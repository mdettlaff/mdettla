/*
 * Gra w kółko i krzyżyk dla dwóch graczy, na dużej planszy.
 * Kto pierwszy zaznaczy 5 pól swojego koloru w jednej linii (poziomo,
 * pionowo lub na skos), ten wygrywa.
 *
 * Program można skompilować używając polecenia:
 * gcc -Wall -lX11 -lpthread binary_sem.h binary_sem.c tic_tac_toe.c -o tic_tac_toe
 * Każdy gracz uruchamia swoją własną instancję programu. Kliknięcie na polu
 * planszy powoduje jego zaznaczenie kolorem gracza.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <pthread.h>
#include <X11/Xlib.h>

#include "binary_sem.h"

#define TRUE 1
#define FALSE 0
#define SHM_KEY 1236 // klucz pamięci współdzielonej
#define SEM_KEY 1116 // klucz semafora

#define BOARD_SIZE 12 // plansza ma wymiary BOARD_SIZE x BOARD_SIZE pól
#define LINE_LENGTH 5 // ile pól trzeba zaznaczyć w jednej linii aby wygrać
#define CELL_SIZE_PX 50 // rozmiar pola planszy w pikselach
#define BOARD_X 0 // odległość między planszą a lewym brzegiem okna
#define BOARD_Y 50 // odległość między planszą a górnym brzegiem okna
#define TEXT_LEFT_MARGIN 25 // odległość tekstu od lewego brzegu okna
#define BOTTOM_MARGIN 50 // odległość między planszą a dolnym brzegiem okna
#define BOARD_SIZE_PX BOARD_SIZE * CELL_SIZE_PX // rozmiar planszy w pikselach
#define EMPTY_CELL 0 // wartość wolnego pola na planszy


// typ boolowski
typedef int bool_t;
// struktura przechowująca zmienne znajdujące się w pamięci współdzielonej
typedef struct shared_st {
    int board[BOARD_SIZE][BOARD_SIZE]; // plansza na której odbywa się gra
    int players_count; // ilu graczy aktualnie bierze udział w grze
    bool_t is_game_over;
} shared_t;
// współrzędne pola na planszy, indeksowane od zera
typedef struct coords_st {
    int row;
    int column;
} coords_t;


// zmienne globalne
int sem_id;
int shm_id;
shared_t *shared; // pamięć współdzielona
int player_id; // numer gracza, może być równy 1 lub 2
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t user_input = PTHREAD_COND_INITIALIZER;
coords_t *coords; // współrzędne pola na planszy na które kliknął gracz

// zmienne globalne Xlib
Display *display;
Window window;
GC gc;
XColor foreground, color1, color2, dummy;
XEvent event;
Atom wm_delete_message;


/*===========================================================================*/
/* Funkcje związane z inicjalizacją i sprzątaniem po semaforach oraz pamięci */
/* współdzielonej.                                                           */
/*===========================================================================*/

/*
 * Tworzy pamięć współdzieloną i zwraca numer gracza. Graczem 1 zostaje ten,
 * kto pierwszy utworzy pamięć współdzieloną.
 */
int init_shared_memory() {
    int player_id;
    if ((shm_id = shmget((key_t)SHM_KEY, sizeof (shared_t),
                    0666 | IPC_CREAT | IPC_EXCL)) != -1) {
        player_id = 1;
    } else if ((shm_id = shmget((key_t)SHM_KEY, sizeof (shared_t),
                    0666 | IPC_CREAT)) != -1) {
        player_id = 2;
    } else {
        fprintf(stderr, "shmget failed\n");
        exit(EXIT_FAILURE);
    }
    void *shared_memory = shmat(shm_id, (void *)0, 0);
    if (shared_memory == (void *)-1) {
        fprintf(stderr, "shmat failed\n");
        exit(EXIT_FAILURE);
    }
    shared = (shared_t *)shared_memory;
    if (player_id == 1) {
        int i, j;
        for (i = 0; i < BOARD_SIZE; i++) {
            for (j = 0; j < BOARD_SIZE; j++) {
                (shared->board)[i][j] = EMPTY_CELL;
            }
        }
        shared->players_count = 0;
        shared->is_game_over = FALSE;
    }
    if (shared->players_count >= 2 || shared->is_game_over) {
        fprintf(stderr, "w grze może brać udział co najwyżej dwóch graczy\n");
        exit(EXIT_FAILURE);
    }
    return player_id;
}

void init_semaphores() {
    if ((sem_id = semget((key_t)SEM_KEY, 2, 0666 | IPC_CREAT)) == -1) {
        fprintf(stderr, "semget failed\n");
        exit(EXIT_FAILURE);
    }
    if (!set_semvalue(sem_id, 0, 0)) {
        fprintf(stderr, "set_semvalue failed\n");
        exit(EXIT_FAILURE);
    }
    if (!set_semvalue(sem_id, 1, 1)) {
        fprintf(stderr, "set_semvalue failed\n");
        exit(EXIT_FAILURE);
    }
}

void clean_up() {
    printf("usuwam semafory\n");
    if (!del_semvalue(sem_id)) {
        fprintf(stderr, "del_semvalue failed\n");
        exit(EXIT_FAILURE);
    }
    printf("usuwam pamięć współdzieloną\n");
    if (shmdt(shared) == -1) {
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
    if (shared->players_count > 1) {
        shared->players_count--;
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
    XInitThreads();
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

    window = XCreateWindow(display, XRootWindow(display, screen),
            100, 100, BOARD_X + BOARD_SIZE_PX,
            BOARD_Y + BOARD_SIZE_PX + BOTTOM_MARGIN,
            0, depth, InputOutput, visual, CWBackPixel|CWOverrideRedirect,
            &window_attributes);
    XSelectInput(display, window, ExposureMask|ButtonPressMask);

    Colormap colormap = DefaultColormap(display, screen);
    XAllocNamedColor(display, colormap, "black", &foreground, &dummy);
    XAllocNamedColor(display, colormap, "red", &color1, &dummy);
    XAllocNamedColor(display, colormap, "blue", &color2, &dummy);

    gc = DefaultGC(display, screen);

    XFontStruct* font_info;
    char* font_name = "*-helvetica-medium-r-*-24-*";
    font_info = XLoadQueryFont(display, font_name);
    if (!font_info) {
        fprintf(stderr, "XLoadQueryFont: failed loading font %s\n", font_name);
    }
    XSetFont(display, gc, font_info->fid);

    wm_delete_message = XInternAtom(display, "WM_DELETE_WINDOW", False);
    XSetWMProtocols(display, window, &wm_delete_message, 1);
}

/*
 * Wyświetla imię gracza nad planszą, w kolorze gracza.
 */
void draw_player_name(int player_id) {
    char text[32];
    sprintf(text, "Gracz %d", player_id);
    XColor player_color;
    if (player_id == 1) {
        player_color = color1;
    } else {
        player_color = color2;
    }
    XSetForeground(display, gc, player_color.pixel);
    XDrawString(display, window, gc, TEXT_LEFT_MARGIN, 36, text, strlen(text));
    XFlush(display);
}

/*
 * Wyświetla planszę.
 */
void draw_board(int board[BOARD_SIZE][BOARD_SIZE]) {
    int i, j;
    XSetForeground(display, gc, foreground.pixel);
    // rysuje linie poziome
    for (i = 0; i < BOARD_SIZE + 1; i++) {
        XDrawLine(display, window, gc,
                BOARD_X, BOARD_Y + i * CELL_SIZE_PX,
                BOARD_X + BOARD_SIZE_PX, BOARD_Y + i * CELL_SIZE_PX);
    }
    // rysuje linie pionowe
    for (i = 0; i < BOARD_SIZE + 1; i++) {
        XDrawLine(display, window, gc,
                BOARD_X + i * CELL_SIZE_PX, BOARD_Y,
                BOARD_X + i * CELL_SIZE_PX, BOARD_Y + BOARD_SIZE_PX);
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
                        BOARD_X + j * CELL_SIZE_PX, BOARD_Y + i * CELL_SIZE_PX,
                        CELL_SIZE_PX, CELL_SIZE_PX,
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
    XClearArea(display, window, BOARD_X, BOARD_Y + BOARD_SIZE_PX + 1,
            BOARD_SIZE_PX, BOTTOM_MARGIN, False);
    XSetForeground(display, gc, foreground.pixel);
    XDrawString(display, window, gc,
            TEXT_LEFT_MARGIN, BOARD_Y + BOARD_SIZE_PX + 34,
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
    XMapWindow(display, window);
    XNextEvent(display, &event); // czekamy na pierwsze zdarzenie Expose
    XStoreName(display, window, "Tic Tac Toe");
    draw_info("oczekiwanie na drugiego gracza...");
    draw_everything(board, player_id);
}

/*
 * Wątek do obsługi zdarzeń XEvent.
 */
void *event_thread(void *arg) {
    coords = malloc(sizeof (*coords));
    while (TRUE) {
        XNextEvent(display, &event);
        switch (event.type) {
            case ButtonPress:
                pthread_mutex_lock(&mutex);
                coords->row = (event.xbutton.y - BOARD_Y) / CELL_SIZE_PX;
                coords->column = (event.xbutton.x - BOARD_X) / CELL_SIZE_PX;
                if (event.xbutton.y >= BOARD_Y && event.xbutton.x >= BOARD_X) {
                    pthread_cond_signal(&user_input);
                }
                pthread_mutex_unlock(&mutex);
                break;
            case Expose:
                draw_everything(shared->board, player_id);
                break;
            case ClientMessage:
                if (event.xclient.data.l[0] == wm_delete_message) {
                    quit_game(0);
                }
                break;
        }
    }
    pthread_exit(NULL);
}

void create_event_thread() {
    pthread_t thread;
    if (pthread_create(&thread, NULL, event_thread, NULL) != 0) {
        fprintf(stderr, "thread creation failed\n");
        exit(EXIT_FAILURE);
    }
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
        for (j = 0; j < BOARD_SIZE - LINE_LENGTH + 1; j++) {
            is_line_complete = TRUE;
            for (k = 0; k < LINE_LENGTH; k++) {
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
    for (i = 0; i < BOARD_SIZE - LINE_LENGTH + 1; i++) {
        for (j = 0; j < BOARD_SIZE; j++) {
            is_line_complete = TRUE;
            for (k = 0; k < LINE_LENGTH; k++) {
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
    for (i = 0; i < BOARD_SIZE - LINE_LENGTH + 1; i++) {
        for (j = 0; j < BOARD_SIZE - LINE_LENGTH + 1; j++) {
            // na skos NW-SE
            is_line_complete = TRUE;
            for (k = 0; k < LINE_LENGTH; k++) {
                if (board[i + k][j + k] != player_id) {
                    is_line_complete = FALSE;
                }
            }
            if (is_line_complete) {
                return TRUE;
            }
            // na skos NE-SW
            is_line_complete = TRUE;
            for (k = 0; k < LINE_LENGTH; k++) {
                if (board[i + k][j + LINE_LENGTH - 1 - k] != player_id) {
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
    pthread_mutex_lock(&mutex);
    do {
        // czekamy, aż wątek event_thread wpisze do zmiennej globalnej coords
        // współrzędne prawidłowego ruchu
        pthread_cond_wait(&user_input, &mutex);
    } while (!is_legal_move(coords, shared->board) || shared->is_game_over);
    pthread_mutex_unlock(&mutex);
    return coords;
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
    } else if (shared->players_count == 1) {
        draw_info("wygrana walkowerem");
    } else {
        return; // gramy dalej
    }
    shared->is_game_over = TRUE;
    if (!semaphore_v(sem_id, 1 - (player_id - 1))) exit(EXIT_FAILURE);
}

/*
 * Pętla główna gry.
 */
void play_tic_tac_toe(int board[BOARD_SIZE][BOARD_SIZE], int player_id) {
    shared->players_count++;
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
    player_id = init_shared_memory();
    init_semaphores();
    init_window(shared->board, player_id);

    create_event_thread();
    play_tic_tac_toe(shared->board, player_id);

    return EXIT_SUCCESS;
}
