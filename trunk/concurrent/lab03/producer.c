#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

int main(int argc, char *argv[]) {
    const char FILENAME[] = "wyniki";

    char username[128] = "";
    char buffer[1024];
    int buf_len = 0;
    int c;
    int file;

    if (argc < 2) {
        printf("Użycie: ./producer nazwa_konta\n");
        return 2;
    }
    strcat(username, argv[1]);
    strcat(username, "\n");
    do {
        c = getchar();
        buffer[buf_len++] = (char)c;
    } while (c != 27);
    buffer[buf_len] = '\0';

    file = open(FILENAME, O_WRONLY | O_APPEND, S_IRWXU);
    if (file == -1) {
        perror("błąd: nie można otworzyć pliku");
    }
    write(file, username, strlen(username));
    write(file, buffer, buf_len);
    close(file);
    return 0;
}
