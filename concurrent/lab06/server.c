#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#define ever ;;

int main(int argc, char *argv[]) {
    const char REQUEST_BUFFER[] = "/home/studinf/mdettla/tmp/serwerfifo";
    const char RESPONSE_BUFFER[] = "/home/studinf/mdettla/tmp/klientfifo";

    char message_from_user[1024];
    char response_to_user[1024];
    char* name_found;
    int fd; // file descriptor
    int c;
    int i;
    int id;
    int msg_len;

    printf("Serwer gotowy.\nCzekam na zgłoszenia klientów...\n");
    for (ever) {
        // read and display message from request buffer
        fd = open(REQUEST_BUFFER, O_RDONLY, S_IRWXU);
        read(fd, message_from_user, 2);
        id = message_from_user[0];
        msg_len = message_from_user[1];
        read(fd, message_from_user, msg_len);
        close(fd);

        printf("użytkownik %s prosi o rekord z ID = %d\n",
                message_from_user, id);
        if (id == 1) {
            name_found = "Dettlaff";
        } else if (id == 2) {
            name_found = "Kowalski";
        } else {
            name_found = "Nie ma";
        }

        response_to_user[0] = strlen(name_found);
        response_to_user[1] = '\0';
        strcat(response_to_user, name_found);

        // write response to response buffer
        fd = open(RESPONSE_BUFFER, O_WRONLY, 0);
        if (fd == -1) {
            perror("błąd: nie można otworzyć pliku");
        }
        write(fd, response_to_user, strlen(response_to_user));
        close(fd);
    }
    return 0;
}
