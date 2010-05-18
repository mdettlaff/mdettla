#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>

#include "queue.h"

#define TRUE 1
#define FALSE 0


pthread_mutex_t region_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t data_available = PTHREAD_COND_INITIALIZER;
queue_t *resources;


void queue_print(queue_t *queue) {
    printf("queue: ");
    if (!queue_is_empty(queue)) {
        node_t *node = queue->first;
        while (node != NULL) {
            printf("%d ", *((int *)node->value));
            node = node->next;
        }
    } else {
        printf("empty");
    }
    printf("\n");
}

void *producer_thread(void *arg) {
    int i;
    sleep(1);
    for (i = 0; i < 5; i++) {
        int *resource = malloc(sizeof (*resource));
        *resource = i;
        pthread_mutex_lock(&region_mutex);
        enqueue(resource, resources);
        printf("producer: resource %d produced\n", i);
        queue_print(resources);
        pthread_cond_signal(&data_available);
        pthread_mutex_unlock(&region_mutex);
        sleep(2);
    }
    pthread_exit(NULL);
}

void consumer() {
    while (TRUE) {
        pthread_mutex_lock(&region_mutex);
        if (queue_is_empty(resources)) {
            pthread_cond_wait(&data_available, &region_mutex);
        }
        int resource = *((int *)dequeue(resources));
        printf("consumer: resource %d consumed\n", resource);
        pthread_mutex_unlock(&region_mutex);
        printf("consumer: doing something with resource...\n");
        sleep(5);
        printf("consumer: done\n");
    }
}

int main() {
    resources = queue_create();
    pthread_t thread;
    if (pthread_create(&thread, NULL, producer_thread, NULL) != 0) {
        fprintf(stderr, "thread creation failed\n");
        exit(EXIT_FAILURE);
    }
    consumer();
    exit(EXIT_SUCCESS);
}
