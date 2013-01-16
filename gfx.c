#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <math.h>
#include <stdint.h>
#include <X11/Xutil.h>
 
Atom wmDeleteMessage;

typedef struct bgr32_s {
    uint8_t blue;
    uint8_t green;
    uint8_t red;
    uint8_t pad; 
} bgr32_t;

typedef union pixel_s {
    struct { uint8_t b, g, r, p; } bgrp;
    unsigned long ul;
} pixel_t;

void draw_image (XImage *image)
{
    static int green = 1;
    int height = image->height;
    int width = image->width;
    bgr32_t* p = (bgr32_t*)image->data;
    int x, y;

    printf ("green %s\n", green ? "on" : "off");

    for(y=0; y < height; y++)
    {
        for(x=0; x < width; x++)
        {
	    double xd = (double)x / width;
	    double yd = (double)y / height;

	    double r = sqrt(xd) - pow(1-xd,2)*pow(yd,4);
	    if (r < 0) r = 0;
	    if (r > 1) r = 1;
	    p->red = r * 255;

	    double g = green ? yd : 0;
	    if (g < 0) g = 0;
	    if (g > 1) g = 1;
	    p->green = g * 255;

	    double b = (1 - sqrt(xd)) - pow(xd,2)*pow(yd,4);
	    if (b < 0) b = 0;
	    if (b > 1) b = 1;
	    p->blue = b * 255;

	    p->pad = 0;
	    p++;
        }
    }
    
    pixel_t blue;
    blue.bgrp.b = 255;
    blue.bgrp.g = 0;
    blue.bgrp.r = 0;
    blue.bgrp.p = 0;
    
    for (x = 100; x < 110; x++)
	for (y = 100; y < 110; y++)
	    XPutPixel (image, x, y, blue.ul);

    green = !green;
}

XImage *CreateTrueColorImage(Display *display, Visual *visual, int width, int height)
{
    XImage *image = XCreateImage(display, visual, 24, ZPixmap, 0, NULL, width, height, 32, 0);
    image->data = (char*) malloc (width*height*4);
    draw_image (image);
    return image;
}

void processEvent(Display *display, Window window, XImage *ximage, int width, int height)
{
    XEvent ev;
    XNextEvent(display, &ev);
    switch(ev.type)
    {
    case Expose:
        XPutImage(display, window, DefaultGC(display, 0), ximage, 0, 0, 0, 0, width, height);
        break;
    case ButtonPress:
	draw_image (ximage);
        XPutImage(display, window, DefaultGC(display, 0), ximage, 0, 0, 0, 0, width, height);
	break;
    case ClientMessage:
	if (ev.xclient.data.l[0] == wmDeleteMessage) exit (0);
    }
}

int main(int argc, char **argv)
{
    XImage *ximage;
    int width=800, height=600;
    Display *display=XOpenDisplay(NULL);
    Visual *visual=DefaultVisual(display, 0);
    Window window=XCreateSimpleWindow(display, RootWindow(display, 0), 0, 0, width, height, 1, 0, 0);
    if(visual->class!=TrueColor)
    {
        fprintf(stderr, "Cannot handle non true color visual ...\n");
        exit(1);
    }

    ximage=CreateTrueColorImage(display, visual, width, height);
    XSelectInput(display, window, ButtonPressMask|ExposureMask);
    XMapWindow(display, window);

    wmDeleteMessage = XInternAtom (display, "WM_DELETE_WINDOW", False);
    XSetWMProtocols(display, window, &wmDeleteMessage, 1);

    while(1)
    {
        processEvent(display, window, ximage, width, height);
    }
}
