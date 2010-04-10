#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

int main(int argc, char *argv[]) {
    char home_dir[128] = "/home/studinf/";
    char request_buffer_filename[128] = "";
    char response_buffer_filename[128] = "";
    char username[128] = "";
    char message[1024] = "";
    char response_from_server[1024];
    char name_found[1024];
    int fd; // file descriptor
    int id;
    int response_len;

    // read username from arguments
    if (argc < 2) {
        printf("Użycie: ./client nazwa_konta\n");
        return 2;
    }
    strcat(username, argv[1]);
    // find server path
    strcat(request_buffer_filename, home_dir);
    strcat(request_buffer_filename, username);
    strcat(request_buffer_filename, "/tmp/serwerfifo");
    strcat(response_buffer_filename, home_dir);
    strcat(response_buffer_filename, username);
    strcat(response_buffer_filename, "/tmp/klientfifo");
    // read message from keyboard
    printf("Podaj ID rekordu:\n");
    scanf("%d", &id);
    message[strlen(message)] = strlen(username) + 1;
    message[strlen(message) + 1] = '\0';
    message[strlen(message)] = id;
    message[strlen(message) + 1] = '\0';
    strcat(message, username);
    // write message to buffer
    fd = open(request_buffer_filename, O_WRONLY, 0);
    if (fd == -1) {
        perror("błąd: nie można otworzyć pliku");
    }
    write(fd, message, strlen(message));
    close(fd);

    // wait for server response
    printf("Czekam na odpowiedź z serwera...\n");
    fd = open(response_buffer_filename, O_RDONLY, S_IRWXU);
    // read response from buffer
    read(fd, response_from_server, 1);
    response_len = response_from_server[0];
    read(fd, name_found, response_len);
    // display response
    printf("Imię odesłane przez serwer:\n%s\n", name_found);
    close(fd);

    return 0;
}
