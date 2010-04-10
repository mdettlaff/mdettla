#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#define ever ;;
#define DB_SIZE 5

typedef struct {
    int ID;
    char* nazwisko;
} record;

int main(int argc, char *argv[]) {
    record database[DB_SIZE];
    const char REQUEST_BUFFER[] = "/home/studinf/mdettla/tmp/serwerfifo";
    char response_buffer[1024] = "";

    char message_from_user[1024];
    char response_to_user[1024];
    char* name_found;
    int fd; // file descriptor
    int id;
    int msg_len;
    int i;
    int znaleziono;

    database[0].ID = 1;
    database[0].nazwisko = "Dettlaff";
    database[1].ID = 2;
    database[1].nazwisko = "Kowalski";
    database[2].ID = 5;
    database[2].nazwisko = "Malinowski";
    database[3].ID = 6;
    database[3].nazwisko = "Karwowski";
    database[4].ID = 7;
    database[4].nazwisko = "Janowski";

    printf("Serwer gotowy.\nCzekam na zgłoszenia klientów...\n");
    for (ever) {
        // read and display message from request buffer
        fd = open(REQUEST_BUFFER, O_RDONLY, S_IRWXU);
        read(fd, message_from_user, 2);
        msg_len = message_from_user[0];
        id = message_from_user[1];
        read(fd, message_from_user, msg_len - 1);
        close(fd);

        printf("użytkownik %s prosi o rekord z ID = %d\n",
                message_from_user, id);
        znaleziono = 0;
        for (i = 0; i < DB_SIZE; i++) {
            if (database[i].ID == id) {
                name_found = database[i].nazwisko;
                znaleziono = 1;
            }
        }
        if (!znaleziono) {
            name_found = "Nie ma";
        }

        response_to_user[0] = strlen(name_found);
        response_to_user[1] = '\0';
        strcat(response_to_user, name_found);

        strcat(response_buffer, "/home/studinf/");
        strcat(response_buffer, message_from_user); // append username
        strcat(response_buffer, "/tmp/klientfifo");
        // write response to response buffer
        fd = open(response_buffer, O_WRONLY, 0);
        if (fd == -1) {
            perror("błąd: nie można otworzyć pliku");
        }
        write(fd, response_to_user, strlen(response_to_user));
        close(fd);

        response_buffer[0] = '\0';
    }
    return 0;
}
