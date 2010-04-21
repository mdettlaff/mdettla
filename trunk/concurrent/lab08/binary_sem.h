#ifndef _binary_sem_h
#define _binary_sem_h


/*===========================================================================*/
/* Funkcje stanowiące uproszczony interfejs semaforów (binarnych).           */
/*===========================================================================*/

/*
 * The function set_semvalue initializes the semaphore using the SETVAL command
 * in a semctl call. You need to do this before you can use the semaphore.
 */
int set_semvalue(int sem_id);

/*
 * The del_semvalue function has almost the same form, except that the call
 * to semctl uses the command IPC_RMID to remove the semaphore's ID.
 */
void del_semvalue(int sem_id);

/*
 * semaphore_p changes the semaphore by -1. This is the "wait" operation.
 */
int semaphore_p(int sem_id);

/*
 * semaphore_v is similar except for setting the sem_op part of the sembuf
 * structure to 1. This is the "release" operation, so that the semaphore
 * becomes available.
 */
int semaphore_v(int sem_id);


#endif
