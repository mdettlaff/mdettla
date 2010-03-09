#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

int main(int argc, char *argv[]) {
    char home_dir[128] = "/home/studinf/";
    char request_buffer_filename[128] = "";
    char response_buffer_filename[128] = "";
    char lockfile_name[128] = "";
    char username[128] = "";
    char message[1024];
    char response_from_server[1024];
    int buf_len = 0;
    int fd; // file descriptor
    int c;
    int i;

    // read username from arguments
    if (argc < 2) {
        printf("Użycie: ./client nazwa_konta\n");
        return 2;
    }
    strcat(username, argv[1]);
    // find server path
    strcat(request_buffer_filename, home_dir);
    strcat(request_buffer_filename, username);
    strcat(request_buffer_filename, "/tmp/dane");
    strcat(response_buffer_filename, home_dir);
    strcat(response_buffer_filename, username);
    strcat(response_buffer_filename, "/tmp/wyniki");
    strcat(lockfile_name, home_dir);
    strcat(lockfile_name, username);
    strcat(lockfile_name, "/tmp/lockfile");
    // wait while server busy
    while (open(lockfile_name, O_CREAT | O_EXCL, 0) == -1) {
        printf("Serwer zajęty, proszę czekać...\n");
        sleep(2);
    }
    // read message from keyboard
    printf("Wpisz wiadomość:\n");
    do {
        c = getchar();
        message[buf_len++] = (char)c;
    } while (c != 27);
    message[buf_len] = '\0';
    // write message to buffer
    fd = open(request_buffer_filename,
            O_WRONLY | O_CREAT | O_EXCL | O_APPEND, S_IRWXU);
    if (fd == -1) {
        perror("błąd: nie można otworzyć pliku");
    }
    write(fd, username, strlen(username));
    write(fd, "\n", 1);
    write(fd, message, buf_len);
    close(fd);

    // wait for server response
    printf("Czekam na odpowiedź z serwera...\n");
    while ((fd = open(response_buffer_filename, O_RDONLY, S_IRWXU)) == -1) {}
    // read response from buffer
    read(fd, response_from_server, 1024);
    for (i = 0; (c = response_from_server[i]) != 27; i++) {}
    response_from_server[i] = '\0';
    // display response
    printf("Serwer odpowiedział:\n%s\n", response_from_server);
    close(fd);
    // flush buffer
    unlink(response_buffer_filename);

    return 0;
}
