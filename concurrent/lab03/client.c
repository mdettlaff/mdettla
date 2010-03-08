#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

int main(int argc, char *argv[]) {
    const char REQUEST_BUFFER[] = "dane";
    const char RESPONSE_BUFFER[] = "wyniki";

    char username[128] = "";
    char buffer[1024];
    char response_from_server[1024];
    int buf_len = 0;
    int fd; // file descriptor
    int c;
    int i;

    // read username from arguments
    if (argc < 2) {
        printf("Użycie: ./producer nazwa_konta\n");
        return 2;
    }
    strcat(username, argv[1]);
    strcat(username, "\n");
    // read message from keyboard
    do {
        c = getchar();
        buffer[buf_len++] = (char)c;
    } while (c != 27);
    buffer[buf_len] = '\0';
    // write message to buffer
    fd = open(REQUEST_BUFFER,
            O_WRONLY | O_CREAT | O_EXCL | O_APPEND, S_IRWXU);
    if (fd == -1) {
        perror("błąd: nie można otworzyć pliku");
    }
    write(fd, username, strlen(username));
    write(fd, buffer, buf_len);
    close(fd);

    // wait for response
    printf("Czekam na odpowiedź z serwera...\n");
    while ((fd = open(RESPONSE_BUFFER, O_RDONLY, S_IRWXU)) == -1) {}
    // read response from buffer
    read(fd, response_from_server, 1024);
    for (i = 0; (c = response_from_server[i]) != 27; i++) {}
    response_from_server[i] = '\0';
    // display response
    printf("Serwer odpowiedział:\n%s\n", response_from_server);
    close(fd);
    // flush buffer
    unlink(RESPONSE_BUFFER);
    return 0;
}
