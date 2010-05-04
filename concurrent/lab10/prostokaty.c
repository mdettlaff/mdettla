#include <X11/Xlib.h>
#include <X11/X.h>
#include <unistd.h>
#include <time.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>

#define MLD 1000000000.0

Display* mydisplay;
Window mywindow;
XSetWindowAttributes mywindowattributes;
XGCValues mygcvalues;
GC mygc;
Visual* myvisual;
int mydepth;
int myscreen;
Colormap mycolormap;
XColor mycolor, mycolor1, dummy;
XEvent myevent;
struct timespec tp0, tp1;

int display_at(char* address) {
    printf("wyświetlam prostokąt na komputerze: %s\n", address);
    mydisplay = XOpenDisplay(address);
    myscreen = DefaultScreen(mydisplay);
    myvisual = DefaultVisual(mydisplay, myscreen);
    mydepth = DefaultDepth(mydisplay, myscreen);
    mywindowattributes.background_pixel = XWhitePixel(mydisplay, myscreen);
    mywindowattributes.override_redirect = False;

    mywindow = XCreateWindow(mydisplay,XRootWindow(mydisplay, myscreen),
            100, 100, 500, 500, 10, mydepth, InputOutput,
            myvisual, CWBackPixel|CWOverrideRedirect,
            &mywindowattributes);

    XSelectInput(mydisplay, mywindow, ExposureMask|KeyPressMask);
    mycolormap = DefaultColormap(mydisplay, myscreen);
    XAllocNamedColor(mydisplay, mycolormap,"cyan", &mycolor, &dummy);
    XAllocNamedColor(mydisplay, mycolormap,"red", &mycolor1, &dummy);
    XMapWindow(mydisplay, mywindow);
    mygc = DefaultGC(mydisplay, myscreen);

    while (1) {
        XNextEvent(mydisplay, &myevent);
        switch (myevent.type) {
            case Expose:
                XSetForeground(mydisplay, mygc, mycolor.pixel);
                XFillRectangle(mydisplay, mywindow, mygc, 100, 100, 300, 300);
                XSetForeground(mydisplay, mygc, mycolor1.pixel);
                XFillRectangle(mydisplay, mywindow, mygc, 150, 150, 200, 200);
                XFlush(mydisplay);
                clock_gettime(CLOCK_REALTIME, &tp0);
                break;
            case KeyPress:
                XCloseDisplay(mydisplay);
                clock_gettime(CLOCK_REALTIME, &tp1);
                double czas = tp1.tv_sec + tp1.tv_nsec/MLD
                    - (tp0.tv_sec + tp0.tv_nsec/MLD);
                printf("Wyłączono ekran %s po %.1f sekundach.\n", address, czas);
                exit(0);
        }
    }
}

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Użycie: prostokąty ADRESY_UŻYTKOWNIKÓW...\n");
        exit(2);
    }
    int i;
    for (i = 0; i < argc - 1; i++) {
        if (fork() == 0) {
            display_at(argv[i + 1]);
        }
    }
    for (i = 0; i < argc - 1; i++) {
        if (wait(NULL) == -1) {
            perror("błąd wait");
        }
    }
    return 0;
}
