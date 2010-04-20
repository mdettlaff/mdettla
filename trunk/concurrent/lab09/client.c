#include <arpa/inet.h>
#include <netinet/in.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <stdlib.h>

#define BUFLEN 512
#define PORT 5000
#define SERVER_IP "153.19.1.240"

void on_error(char *s) {
    perror(s);
    exit(1);
}

int bytes_to_int(char* bytes) {
    return (((int)bytes[0]) << 24) | (((int)bytes[1]) << 16)
        | (((int)bytes[2]) << 8) | ((int)bytes[3]);
}

void int_to_bytes(int n, char* bytes) {
    bytes[0] = (0xFF000000 & n) >> 24;
    bytes[1] = (0x00FF0000 & n) >> 16;
    bytes[2] = (0x0000FF00 & n) >> 8;
    bytes[3] = (0x000000FF & n);
    bytes[4] = '\0';
}

int main(int argc, char* argv[]) {
    struct sockaddr_in si_other;
    int s;
    int slen = sizeof(si_other);
    char buf[BUFLEN];

    if (argc < 2) {
        printf("Użycie: %s LICZBA\n", argv[0]);
        exit(2);
    }
    int input_number = atoi(argv[1]);

    if ((s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1)
        on_error("socket");
    //memset((char *) &si_other, 0, sizeof(si_other));
    si_other.sin_family = AF_INET;
    si_other.sin_port = htons(PORT);
    if (inet_aton(SERVER_IP, &si_other.sin_addr) == 0) {
        fprintf(stderr, "inet_aton() failed\n");
        exit(1);
    }

    printf("wysyłam liczbę %d\n", input_number);
    int_to_bytes(input_number, buf);
    if (sendto(s, buf, BUFLEN, 0, &si_other, slen) == -1)
        on_error("sendto()");

    int received_bytes_count = recvfrom(s, buf, BUFLEN, 0, &si_other, &slen);
    if (received_bytes_count == -1)
        on_error("recvfrom()");
    printf("odebrano %d bajtów z %s:%d\n", received_bytes_count,
            inet_ntoa(si_other.sin_addr), ntohs(si_other.sin_port));
    printf("otrzymana liczba:\n%d\n", bytes_to_int(buf));

    close(s);
    return 0;
}
