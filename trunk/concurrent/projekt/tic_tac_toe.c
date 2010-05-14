#include <stdio.h>
#include <stdlib.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <signal.h>

#include "binary_sem.h"

#define TRUE 1
#define FALSE 0
#define BOARD_SIZE 10 // plansza ma wymiary BOARD_SIZE x BOARD_SIZE
#define LINE_LEN 5 // ile pól trzeba zaznaczyć w jednej linii aby wygrać
#define EMPTY_CELL ' '
#define SHM_KEY 1236 // klucz pamięci współdzielonej
#define SEM_KEY 1116 // klucz semafora


typedef int bool_t;

struct shared_use_st {
    char board[BOARD_SIZE][BOARD_SIZE];
    bool_t is_player_quit;
};


// zmienne globalne
int sem_id;
int shm_id;
void *shared_memory;
int player_id;


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
    shared_variables->is_player_quit = 0;
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

int is_board_full(char board[BOARD_SIZE][BOARD_SIZE]) {
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

int is_any_line_complete(char mark, char board[BOARD_SIZE][BOARD_SIZE]) {
    int i, j;
    int is_line_complete = FALSE;
    // poziomo
    for (i = 0; i < BOARD_SIZE; i++) {
        is_line_complete = TRUE;
        for (j = 0; j < BOARD_SIZE; j++) {
            if (board[i][j] != mark) {
                is_line_complete = FALSE;
            }
        }
        if (is_line_complete) {
            return TRUE;
        }
    }
    // pionowo
    for (i = 0; i < BOARD_SIZE; i++) {
        is_line_complete = TRUE;
        for (j = 0; j < BOARD_SIZE; j++) {
            if (board[j][i] != mark) {
                is_line_complete = FALSE;
            }
        }
        if (is_line_complete) {
            return TRUE;
        }
    }
    // na ukos NW-SE
    is_line_complete = TRUE;
    for (i = 0; i < BOARD_SIZE; i++) {
        if (board[i][i] != mark) {
            is_line_complete = FALSE;
        }
    }
    if (is_line_complete) {
        return TRUE;
    }
    // na ukos NE-SW
    is_line_complete = TRUE;
    for (i = 0; i < BOARD_SIZE; i++) {
        if (board[i][BOARD_SIZE - 1 - i] != mark) {
            is_line_complete = FALSE;
        }
    }
    if (is_line_complete) {
        return TRUE;
    }
    return FALSE;
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
}

void check_winner(struct shared_use_st *shared_variables, int player_id) {
    int winner;
    if (is_any_line_complete(get_player_mark(1), shared_variables->board)) {
        winner = 1; // wygrał gracz 1
    } else if (is_any_line_complete(get_player_mark(2),
                shared_variables->board)) {
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
    board[row - 1][column - 1] = get_player_mark(player_id);
}

int is_legal_move(int row, int column, char board[BOARD_SIZE][BOARD_SIZE]) {
    return row >= 1 && row <= BOARD_SIZE && column >= 1 && column <= BOARD_SIZE
        && board[row - 1][column - 1] == EMPTY_CELL;
}

void scan_legal_move(int *row, int *column,
        char board[BOARD_SIZE][BOARD_SIZE]) {
    int is_first_iteration = TRUE;
    do {
        if (!is_first_iteration) {
            printf("nieprawidłowy ruch! podaj inną pozycję: ");
        }
        scanf("%d %d", row, column);
        is_first_iteration = FALSE;
    } while (!is_legal_move(*row, *column, board));
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
        scan_legal_move(&row, &column, shared_variables->board);
        write_move(row, column, shared_variables->board, player_id);
        print_board(shared_variables->board);
        check_winner(shared_variables, player_id);
        printf("oczekiwanie na ruch gracza %d...\n", other_player_id);
        if (!semaphore_v(sem_id, 1 - (player_id - 1))) exit(EXIT_FAILURE);
    }
}

int main() {
    signal(SIGINT, quit_game);

    init_shared_memory();
    // graczem 1 zostaje ten, kto pierwszy utworzył semafory
    player_id = init_semaphores();

    play_tic_tac_toe(player_id);

    return EXIT_SUCCESS;
}
