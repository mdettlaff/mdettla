#include <X11/Xlib.h>
#include <X11/X.h>
#include <stdio.h>

Display *mydisplay;
Window mywindow;
XSetWindowAttributes mywindowattributes;
XGCValues mygcvalues;
GC mygc;
Visual *myvisual;
int mydepth;
int myscreen;
Colormap mycolormap;
XColor mycolor,mycolor1,dummy;
int i;

main()

{
   mydisplay = XOpenDisplay("");
   myscreen = DefaultScreen(mydisplay);
   myvisual = DefaultVisual(mydisplay,myscreen);
   mydepth = DefaultDepth(mydisplay,myscreen);
   mywindowattributes.background_pixel = XWhitePixel(mydisplay,myscreen);
   mywindowattributes.override_redirect = True;
   
   mywindow = XCreateWindow(mydisplay,XRootWindow(mydisplay,myscreen),
                            0,0,500,500,10,mydepth,InputOutput,
                            myvisual,CWBackPixel|CWOverrideRedirect,
                            &mywindowattributes);
                    
   mycolormap = DefaultColormap(mydisplay,myscreen);                 
                            
   XAllocNamedColor(mydisplay,mycolormap,"cyan",&mycolor,&dummy);
                    
   XAllocNamedColor(mydisplay,mycolormap,"red",&mycolor1,&dummy);                 
                    
   XMapWindow(mydisplay,mywindow);
   
   mygc = DefaultGC(mydisplay,myscreen);
   
   XSetForeground(mydisplay,mygc,mycolor.pixel);
   
   XFillRectangle(mydisplay,mywindow,mygc,100,100,300,300);
   
   XSetForeground(mydisplay,mygc,mycolor1.pixel);
   
   XSetFunction(mydisplay,mygc,GXcopy);
 
   XSetLineAttributes(mydisplay,mygc,10,LineSolid,CapProjecting,JoinMiter);
   
   XDrawLine(mydisplay,mywindow,mygc,100,100,400,400);

   XDrawLine(mydisplay,mywindow,mygc,100,400,400,100);
   
   XFlush(mydisplay);
   
   sleep(10);
   
   XCloseDisplay(mydisplay);
   
   exit(0);
}
                                             