#include <stdlib.h>

#include "queue.h"


node_t *node_create(void *value) {
    node_t *node = malloc(sizeof (*node));
    node->value = value;
    node->next = NULL;
    return node;
}

queue_t *queue_create() {
    return malloc(sizeof (queue_t));
}

int queue_is_empty(queue_t *queue) {
    return queue->first == NULL;
}

void enqueue(void *value, queue_t *queue) {
    node_t *node = node_create(value);
    if (queue_is_empty(queue)) {
        queue->first = node;
    } else {
        queue->last->next = node;
    }
    queue->last = node;
}

void *dequeue(queue_t *queue) {
    void *value = queue->first->value;
    queue->first = queue->first->next;
    return value;
}
