#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>

#define SHM_KEY 1235 /* klucz pamięci współdzielonej */
#define SEM_KEY 1111 /* klucz semafora */


struct shared_use_st {
    char buffer[BUFSIZ];
};

union semun {
    int val;
    struct semid_ds *buf;
    unsigned short *array;
    struct seminfo *__buf;
};


// zmienne globalne
int sem_id;
int shmid;
void *shared_memory;


/*=================================================================*/
/* Funkcje stanowiące uproszczony interfejs semaforów (binarnych). */
/*=================================================================*/

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

/* Semaphore_p changes the semaphore by "1" This is the "wait" operation. */
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
 * Semaphore_v is similar except for setting the sem_op part of the sembuf
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

/*=================================================================*/


void clean_up() {
    /* pozbywamy się pamięci współdzielonej */
    printf("Odłączam pamięć współdzieloną... ");
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
    /* pozbywamy się semafów */
    printf("Usuwam semafor\n");
    del_semvalue(sem_id);
}

void print_shared_memory(char* buffer) {
    printf("On powiedział: %s\n", buffer);
    if (strcmp(buffer, "koniec") == 0) {
        clean_up();
        exit(EXIT_SUCCESS);
    }
}

void write_shared_memory(char* buffer) {
    printf("Ja mówię: ");
    scanf("%s", buffer);
    if (strcmp(buffer, "koniec") == 0) {
        clean_up();
        exit(EXIT_SUCCESS);
    }
}

void concurrent_block(char* who) {
    shared_memory = shmat(shmid, (void *)0, 0);
    if (shared_memory == (void *)-1) {
        fprintf(stderr, "shmat failed\n");
        exit(EXIT_FAILURE);
    }
    printf("Memory attached at %X\n", (int)shared_memory);
    struct shared_use_st *shared_stuff;
    shared_stuff = (struct shared_use_st *)shared_memory;
    while (1) {
        if (!semaphore_p(sem_id)) {
            exit(EXIT_FAILURE);
        }
        printf("%s: ", who);
        print_shared_memory(shared_stuff->buffer);
        printf("%s: ", who);
        write_shared_memory(shared_stuff->buffer);
        if (!semaphore_v(sem_id)) {
            exit(EXIT_FAILURE);
        }
    }
}

int main() {
    /* inicjalizacja semafora */
    sem_id = semget((key_t)SEM_KEY, 1, 0666 | IPC_CREAT);
    if (!set_semvalue(sem_id)) {
        fprintf(stderr, "Failed to initialize semaphore\n");
        exit(EXIT_FAILURE);
    }

    /* inicjalizacja pamięci współdzielonej */
    shared_memory = (void *)0;
    shmid = shmget((key_t)SHM_KEY, sizeof(struct shared_use_st),
            0666 | IPC_CREAT);
    if (shmid == -1) {
        fprintf(stderr, "shmget failed\n");
        exit(EXIT_FAILURE);
    }

    switch (fork()) {
        case -1: /* błąd */
            perror("Wystapil błąd przy tworzeniu procesu");
            exit(1);
            break;
        case 0: /* proces potomny */
            concurrent_block("Proces potomny");
            break;
        default: /* rodzic */
            concurrent_block("Proces rodzicielski");
            break;
    }
    return EXIT_SUCCESS;
}
