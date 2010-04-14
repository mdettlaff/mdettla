#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/msg.h>
#define MAX_TEXT 512

struct msg_st {
    long int msg_type;
    char word[MAX_TEXT];
};

int main() {
    struct msg_st request_msg;
    struct msg_st response_msg;
    long int msg_to_receive = 0;
    int request_msgid;
    int response_msgid;
    char buffer[BUFSIZ];
    request_msgid = msgget((key_t)1234, 0666 | IPC_CREAT);
    if (request_msgid == -1) {
        fprintf(stderr, "msgget failed with error: %d\n", errno);
        exit(EXIT_FAILURE);
    }
    response_msgid = msgget((key_t)1235, 0666 | IPC_CREAT);
    if (response_msgid == -1) {
        fprintf(stderr, "msgget failed with error: %d\n", errno);
        exit(EXIT_FAILURE);
    }
    printf("podaj słowo w języku polskim do przetłumaczenia:\n");
    fgets(buffer, BUFSIZ, stdin);
    request_msg.msg_type = getpid();
    strcpy(request_msg.word, buffer);
    if (msgsnd(request_msgid, (void *)&request_msg, MAX_TEXT, 0) == -1) {
        fprintf(stderr, "msgsnd failed\n");
        exit(EXIT_FAILURE);
    }
    printf("słowo wysłane, oczekuję na odpowiedź serwera...\n");
    if (msgrcv(response_msgid, (void *)&response_msg, BUFSIZ,
                msg_to_receive, 0) == -1) {
        fprintf(stderr, "msgrcv failed with error: %d\n", errno);
        exit(EXIT_FAILURE);
    }
    printf("%s\n", response_msg.word);
    exit(EXIT_SUCCESS);
}
