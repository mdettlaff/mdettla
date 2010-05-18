#ifndef _queue_h
#define _queue_h


typedef struct node_st {
    void *value;
    struct node_st *next;
} node_t;
typedef struct queue_st {
    node_t *first;
    node_t *last;
} queue_t;


/*===========================================================================*/
/* Operacje na kolejkach.                                                    */
/*===========================================================================*/

queue_t *queue_create();

int queue_is_empty(queue_t *queue);

void enqueue(void *value, queue_t *queue);

void *dequeue(queue_t *queue);


#endif
