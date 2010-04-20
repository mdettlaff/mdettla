#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>

#define TRUE 1
#define FALSE 0
#define BOARD_SIZE 9
#define EMPTY_CELL '.'
#define SHM_KEY 1235 /* klucz pamięci współdzielonej */
#define SEM_KEY 1116 /* klucz semafora */


struct shared_use_st {
    char board[BOARD_SIZE];
};

union semun {
    int val;
    struct semid_ds *buf;
    unsigned short *array;
    struct seminfo *__buf;
};


// zmienne globalne
int sem_id;
int shm_id;
void *shared_memory;


/*===========================================================================*/
/* Funkcje stanowiące uproszczony interfejs semaforów (binarnych).           */
/*===========================================================================*/

/*
 * The function set_semvalue initializes the semaphore using the SETVAL command
 * in a semctl call. You need to do this before you can use the semaphore.
 */
static int set_semvalue(int sem_id) {
    union semun sem_union;
    sem_union.val = 1;
    if (semctl(sem_id, 0, SETVAL, sem_union) == -1) return 0;
    return 1;
}

/*
 * The del_semvalue function has almost the same form, except that the call
 * to semctl uses the command IPC_RMID to remove the semaphore's ID.
 */
static void del_semvalue(int sem_id) {
    union semun sem_union;
    if (semctl(sem_id, 0, IPC_RMID, sem_union) == -1)
        fprintf(stderr, "Failed to delete semaphore\n");
}

/*
 * semaphore_p changes the semaphore by -1. This is the "wait" operation.
 */
static int semaphore_p(int sem_id) {
    struct sembuf sem_b;
    sem_b.sem_num = 0;
    sem_b.sem_op = -1; /* P() */
    sem_b.sem_flg = SEM_UNDO;
    if (semop(sem_id, &sem_b, 1) == -1) {
        fprintf(stderr, "semaphore_p failed\n");
        return 0;
    }
    return 1;
}

/*
 * semaphore_v is similar except for setting the sem_op part of the sembuf
 * structure to 1. This is the "release" operation, so that the semaphore
 * becomes available.
 */
static int semaphore_v(int sem_id) {
    struct sembuf sem_b;
    sem_b.sem_num = 0;
    sem_b.sem_op = 1; /* V() */
    sem_b.sem_flg = SEM_UNDO;
    if (semop(sem_id, &sem_b, 1) == -1) {
        fprintf(stderr, "semaphore_v failed\n");
        return 0;
    }
    return 1;
}

/*===========================================================================*/


void init_shared_memory() {
    int i;
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
    //printf("memory attached at %X\n", (int)shared_memory);
    struct shared_use_st *shared_variables;
    shared_variables = (struct shared_use_st *)shared_memory;
    char* board = shared_variables->board;
    for (i = 0; i < BOARD_SIZE; i++) {
        board[i] = EMPTY_CELL;
    }
}

int init_semaphore() {
    int player_id; // graczem 1 zostaje ten, kto pierwszy utworzył semafor
    if ((sem_id = semget((key_t)SEM_KEY, 1, 0666 | IPC_CREAT | IPC_EXCL))
            != -1) {
        if (!set_semvalue(sem_id)) {
            fprintf(stderr, "Failed to initialize semaphore\n");
            exit(EXIT_FAILURE);
        }
        player_id = 1;
    } else {
        sem_id = semget((key_t)SEM_KEY, 1, 0666 | IPC_CREAT);
        player_id = 2;
    }
    return player_id;
}

void clean_up() {
    /* pozbywamy się pamięci współdzielonej */
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
    /* pozbywamy się semafów */
    printf("usuwam semafor\n");
    del_semvalue(sem_id);
}

char get_player_mark(int player_id) {
    return player_id == 1 ? 'X' : 'O';
}

int is_board_full(char* board) {
    return FALSE; // TODO
}

int are_three_marks_in_a_row(char mark, char* board) {
    return board[0] == mark; // TODO
}

void check_winner(char* board, int player_id) {
    int winner;
    if (are_three_marks_in_a_row(get_player_mark(1), board)) {
        winner = 1; // wygrał gracz 1
    } else if (are_three_marks_in_a_row(get_player_mark(2), board)) {
        winner = 2; // wygrał gracz 2
    } else if (is_board_full(board)) {
        winner = 0; // remis
    } else  {
        winner = -1; // gra toczy się dalej
    }
    if (winner > 0) {
        printf("wygrał gracz %d\n", winner);
        if (player_id != winner) {
            clean_up(); // przegrany musi posprzątać
        }
        exit(EXIT_SUCCESS);
    }
}

void print_board(char* board) {
    int i;
    for (i = 0; i < BOARD_SIZE; i++) {
        if (i % 3 == 0) {
            printf("\n");
        }
        printf(" %c", board[i]);
    }
    printf("\n");
}

void write_move(int position, char* board, int player_id) {
    board[position] = get_player_mark(player_id);
}

void play_tic_tac_toe(int player_id) {
    int other_player_id = 3 - player_id;
    struct shared_use_st* shared_variables;
    shared_variables = (struct shared_use_st*)shared_memory;
    if (player_id != 1) {
        print_board(shared_variables->board);
    }
    while (TRUE) {
        if (!semaphore_p(sem_id)) exit(EXIT_FAILURE);
        check_winner(shared_variables->board, player_id);
        print_board(shared_variables->board);
        printf("gracz %d: ", player_id);
        printf("podaj pozycję: ");
        int position;
        scanf("%d", &position);
        write_move(position, shared_variables->board, player_id);
        print_board(shared_variables->board);
        check_winner(shared_variables->board, player_id);
        printf("oczekiwanie na ruch gracza %d...\n", other_player_id);
        if (!semaphore_v(sem_id)) exit(EXIT_FAILURE);
    }
}

int main() {
    int player_id;

    init_shared_memory();
    // graczem 1 zostaje ten, kto pierwszy utworzył semafor
    player_id = init_semaphore();

    play_tic_tac_toe(player_id);

    return EXIT_SUCCESS;
}
