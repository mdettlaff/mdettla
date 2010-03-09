#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#define ever ;;

int main(int argc, char *argv[]) {
    const char REQUEST_BUFFER[] = "/home/studinf/mdettla/tmp/dane";
    const char RESPONSE_BUFFER[] = "/home/studinf/mdettla/tmp/wyniki";

    char message_from_user[1024];
    char response_to_user[1024];
    int fd; // file descriptor
    int c;
    int i;

    printf("Serwer gotowy.\nCzekam na zgłoszenia klientów...\n");
    for (ever) {
        // read and display message from request buffer
        while ((fd = open(REQUEST_BUFFER, O_RDONLY, S_IRWXU)) == -1) {}
        read(fd, message_from_user, 1024);
        printf("\nUżytkownik ");
        i = 0;
        for (; (c = message_from_user[i]) != '\n'; i++) {
            putchar(c);
        }
        printf(" przesyła wiadomość:");
        for (; (c = message_from_user[i]) != 27; i++) {
            putchar(c);
        }
        close(fd);
        // flush buffer
        unlink(REQUEST_BUFFER);

        // read response from keyboard
        printf("\nWpisz odpowiedź:\n");
        i = 0;
        do {
            c = getchar();
            response_to_user[i++] = (char)c;
        } while (c != 27);
        response_to_user[i] = '\0';
        // write response to response buffer
        fd = open(RESPONSE_BUFFER,
                O_WRONLY | O_CREAT | O_EXCL | O_TRUNC, S_IRWXU);
        if (fd == -1) {
            perror("błąd: nie można otworzyć pliku");
        }
        write(fd, response_to_user, strlen(response_to_user));
        close(fd);
        // release lock
        unlink("lockfile");
    }
    return 0;
}
