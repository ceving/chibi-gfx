// Unification for types lacking the X prefix.
typedef Atom XAtom;
typedef Colormap XColormap;
typedef Drawable XDrawable;
typedef Font XFont;
typedef GContext XGContext;
typedef KeySym XKeySym;
typedef Mask XMask;
typedef Pixmap XPixmap;
typedef Status XStatus;
typedef Time XTime;
typedef VisualID XVisualID;
typedef Window XWindow;
typedef Depth XDepth;
typedef GC XGC;
typedef Screen XScreen;
typedef ScreenFormat XScreenFormat;
typedef Visual XVisual;

// XDisplay
typedef struct XDisplay_s {
    Display *display;
} XDisplay;

// Wrapper for XOpenDisplay
XDisplay *XOpenDisplay_w(char* display_name)
{
    XDisplay *xdisplay = malloc (sizeof(XDisplay));
    if (xdisplay)
        xdisplay->display = XOpenDisplay(display_name);
    return xdisplay;
}
#define XDisplayFinalizer(X) XOpenDisplay_w(X)

// Wrapper for XCloseDisplay
void XCloseDisplay_w(XDisplay *xdisplay)
{
    if (xdisplay && xdisplay->display) {
        XCloseDisplay(xdisplay->display);
        xdisplay->display = NULL;
    }
}

// Unbox wrapper for Xlib function taking a display argument
#define XBlackPixel_w(X, ...) BlackPixel((X)->display, ##__VA_ARGS__)
#define XWhitePixel_w(X, ...) WhitePixel((X)->display, ##__VA_ARGS__)
#define XConnectionNumber_w(X, ...) ConnectionNumber((X)->display, ##__VA_ARGS__)
#define XDefaultColormap_w(X, ...) DefaultColormap((X)->display, ##__VA_ARGS__)
#define XDefaultDepth_w(X, ...) DefaultDepth((X)->display, ##__VA_ARGS__)
#define XDefaultGC_w(X, ...) DefaultGC((X)->display, ##__VA_ARGS__)
#define XDefaultRootWindow_w(X, ...) DefaultRootWindow((X)->display, ##__VA_ARGS__)
#define XDefaultScreenOfDisplay_w(X, ...) DefaultScreenOfDisplay((X)->display, ##__VA_ARGS__)
#define XDefaultScreen_w(X, ...) DefaultScreen((X)->display, ##__VA_ARGS__)
#define XDefaultVisual_w(X, ...) DefaultVisual((X)->display, ##__VA_ARGS__)
#define XDisplayCells_w(X, ...) DisplayCells((X)->display, ##__VA_ARGS__)
#define XDisplayPlanes_w(X, ...) DisplayPlanes((X)->display, ##__VA_ARGS__)
#define XDisplayString_w(X, ...) DisplayString((X)->display, ##__VA_ARGS__)
#define XMaxRequestSize_w(X, ...) XMaxRequestSize((X)->display, ##__VA_ARGS__)
#define XNextRequest_w(X, ...) NextRequest((X)->display, ##__VA_ARGS__)
#define XProtocolVersion_w(X, ...) ProtocolVersion((X)->display, ##__VA_ARGS__)
#define XProtocolRevision_w(X, ...) ProtocolRevision((X)->display, ##__VA_ARGS__)
#define XQLength_w(X, ...) QLength((X)->display, ##__VA_ARGS__)
#define XRootWindow_w(X, ...) RootWindow((X)->display, ##__VA_ARGS__)
#define XScreenCount_w(X, ...) ScreenCount((X)->display, ##__VA_ARGS__)
#define XScreenOfDisplay_w(X, ...) ScreenOfDisplay((X)->display, ##__VA_ARGS__)
#define XServerVendor_w(X, ...) ServerVendor((X)->display, ##__VA_ARGS__)
#define XVendorRelease_w(X, ...) VendorRelease((X)->display, ##__VA_ARGS__)
#define XCreateSimpleWindow_w(X, ...) XCreateSimpleWindow((X)->display, ##__VA_ARGS__)
#define XSelectInput_w(X, ...) XSelectInput((X)->display, ##__VA_ARGS__)
#define XMapWindow_w(X, ...) XMapWindow((X)->display, ##__VA_ARGS__)
#define XMapRaised_w(X, ...) XMapRaised((X)->display, ##__VA_ARGS__)
#define XMapSubwindows_w(X, ...) XMapSubwindows((X)->display, ##__VA_ARGS__)
#define XInternAtom_w(X, ...) XInternAtom((X)->display, ##__VA_ARGS__)
#define XInternAtoms_w(X, ...) XInternAtoms((X)->display, ##__VA_ARGS__)
#define XGetAtomName_w(X, ...) XGetAtomName((X)->display, ##__VA_ARGS__)
#define XGetAtomNames_w(X, ...) XGetAtomNames((X)->display, ##__VA_ARGS__)
#define XSetWMProtocols_w(X, ...) XSetWMProtocols((X)->display, ##__VA_ARGS__)
#define XNextEvent_w(X, ...) XNextEvent((X)->display, ##__VA_ARGS__)
#define XPeekEvent_w(X, ...) XPeekEvent((X)->display, ##__VA_ARGS__)
#define XWindowEvent_w(X, ...) XWindowEvent((X)->display, ##__VA_ARGS__)
#define XCheckWindowEvent_w(X, ...) XCheckWindowEvent((X)->display, ##__VA_ARGS__)
#define XMaskEvent_w(X, ...) XMaskEvent((X)->display, ##__VA_ARGS__)
#define XCheckMaskEvent_w(X, ...) XCheckMaskEvent((X)->display, ##__VA_ARGS__)
#define XCheckTypedEvent_w(X, ...) XCheckTypedEvent((X)->display, ##__VA_ARGS__)
#define XCheckTypedWindowEvent_w(X, ...) XCheckTypedWindowEvent((X)->display, ##__VA_ARGS__)
#define XCreateImage_w(X, ...) XCreateImage((X)->display, ##__VA_ARGS__)
#define XPutImage_w(X, ...) XPutImage((X)->display, ##__VA_ARGS__)
#define XGetImage_w(X, ...) XGetImage((X)->display, ##__VA_ARGS__)
#define XGetSubImage_w(X, ...) XGetSubImage((X)->display, ##__VA_ARGS__)
#define XUnmapWindow_w(X, ...) XUnmapWindow((X)->display, ##__VA_ARGS__)
#define XUnmapSubwindows_w(X, ...) XUnmapSubwindows((X)->display, ##__VA_ARGS__)



// XEvent (XAnyEvent)
#define XEventType(event)      ((event)->type)
#define XEventSerial(event)    ((event)->xany.serial)
#define XEventSendEvent(event) ((event)->xany.send_event)
#define XEventDisplay(event)   ((event)->xany.display)
#define XEventWindow(event)    ((event)->xany.window)

// XButtonEvent
#define XIsButtonPressedEvent(event)  (XEventType(event) == ButtonPress)
#define XIsButtonReleasedEvent(event) (XEventType(event) == ButtonRelease)
#define XIsButtonEvent(event)         (XIsButtonPressedEvent(event) || XIsButtonReleasedEvent(event))
#define XButtonEventRoot(event)       ((event)->xbutton.root)
#define XButtonEventSubwindow(event)  ((event)->xbutton.subwindow)
#define XButtonEventTime(event)       ((event)->xbutton.time)
#define XButtonEventX(event)          ((event)->xbutton.x)
#define XButtonEventY(event)          ((event)->xbutton.y)
#define XButtonEventXRoot(event)      ((event)->xbutton.x_root)
#define XButtonEventYRoot(event)      ((event)->xbutton.y_root)
#define XButtonEventState(event)      ((event)->xbutton.state)
#define XButtonEventButton(event)     ((event)->xbutton.button)
#define XButtonEventSameScreen(event) ((event)->xbutton.same_screen)

// XExposeEvent
#define XIsExposeEvent(event)     (XEventType(event) == Expose)
#define XExposeEventX(event)      ((event)->xexpose.x)
#define XExposeEventY(event)      ((event)->xexpose.y)
#define XExposeEventWidth(event)  ((event)->xexpose.width)
#define XExposeEventHeight(event) ((event)->xexpose.height)
#define XExposeEventCount(event)  ((event)->xexpose.count)

// XClientMessageEvent
#define XIsClientMessageEvent(event)        (XEventType(event) == ClientMessage)
#define XClientMessageEventType(event)      ((event)->xclient.message_type)
#define XClientMessageEventFormat(event)    ((event)->xclient.format)
#define XClientMessageEventDataAtom0(event) ((event)->xclient.data.l[0])

// C macros for other struct fields
#define XVisualClass(visual) ((visual)->class)
