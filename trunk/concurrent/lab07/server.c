#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/msg.h>
#include <signal.h>
#define MAX_TEXT 512

void exit_program(int sig);

struct msg_st {
    long int msg_type;
    char word[MAX_TEXT];
};

int request_msgid;
int response_msgid;

int main() {
    int running = 1;
    struct msg_st request_msg;
    struct msg_st response_msg;
    long int msg_to_receive = 0;

    (void) signal(SIGINT, exit_program);

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
    printf("serwer gotowy, czekam na zgłoszenia klientów...\n");
    while (running) {
        if (msgrcv(request_msgid, (void *)&request_msg, BUFSIZ,
                    msg_to_receive, 0) == -1) {
            fprintf(stderr, "msgrcv failed with error: %d\n", errno);
            exit(EXIT_FAILURE);
        }
        printf("proces o PID=%ld żąda tłumaczenia słowa: %s",
                request_msg.msg_type, request_msg.word);
        response_msg.msg_type = getpid();
        if (strcmp("rower\n", request_msg.word) == 0) {
            strcpy(response_msg.word, "bicycle");
        } else if (strcmp("klawiatura\n", request_msg.word) == 0) {
            strcpy(response_msg.word, "keyboard");
        } else if (strcmp("kot\n", request_msg.word) == 0) {
            strcpy(response_msg.word, "cat");
        } else {
            strcpy(response_msg.word, "nie znam takiego słowa");
        }
        if (msgsnd(response_msgid, (void *)&response_msg, MAX_TEXT, 0) == -1) {
            fprintf(stderr, "msgsnd failed\n");
            exit(EXIT_FAILURE);
        }
    }
    exit(EXIT_SUCCESS);
}

void exit_program(int sig) {
    printf("usuwam kolejki komunikatów...");
    if (msgctl(request_msgid, IPC_RMID, 0) == -1) {
        fprintf(stderr, "msgctl(IPC_RMID) failed\n");
        exit(EXIT_FAILURE);
    }
    if (msgctl(response_msgid, IPC_RMID, 0) == -1) {
        fprintf(stderr, "msgctl(IPC_RMID) failed\n");
        exit(EXIT_FAILURE);
    }
    printf(" OK\n");
    exit(EXIT_SUCCESS);
}
