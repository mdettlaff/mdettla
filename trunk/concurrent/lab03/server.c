#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#define ever ;;

int main(int argc, char *argv[]) {
    const char REQUEST_BUFFER[] = "dane";
    const char RESPONSE_BUFFER[] = "wyniki";

    char message_from_user[1024];
    char response_to_user[1024];
    int response_len;
    int fd; // file descriptor
    int c;
    int i;

    for (ever) {
        // read message from request buffer
        while ((fd = open(REQUEST_BUFFER, O_RDONLY, S_IRWXU)) == -1) {}
        read(fd, message_from_user, 1024);
        for (i = 0; (c = message_from_user[i]) != 27; i++) {}
        message_from_user[i] = '\0';
        // display request
        printf("Użytkownik przesyła wiadomość:\n%s\n", message_from_user);
        close(fd);
        // flush buffer
        unlink(REQUEST_BUFFER);
        /*fd = open(REQUEST_BUFFER, O_WRONLY | O_TRUNC, S_IRWXU);
        close(fd);*/

        // read response from keyboard
        printf("Wpisz odpowiedź:\n");
        response_len = 0;
        do {
            c = getchar();
            response_to_user[response_len++] = (char)c;
        } while (c != 27);
        response_to_user[response_len] = '\0';
        // write response to response buffer
        fd = open(RESPONSE_BUFFER,
                O_WRONLY | O_CREAT | O_EXCL | O_TRUNC, S_IRWXU);
        if (fd == -1) {
            perror("błąd: nie można otworzyć pliku");
        }
        write(fd, response_to_user, response_len);
        close(fd);
    }
    return 0;
}
