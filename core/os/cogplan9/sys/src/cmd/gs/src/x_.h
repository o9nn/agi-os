#ifndef x__INCLUDED
#  define x__INCLUDED
#undef private
#ifndef VMS
#  define have_Xdebug
#endif
#ifdef VMS
#  ifdef __GNUC__
#    define XAllocColor			xalloccolor
#    define XAllocNamedColor		xallocnamedcolor
#    define XCloseDisplay		xclosedisplay
#    define XCopyArea			xcopyarea
#    define XCreateGC			xcreategc
#    define XCreatePixmap		xcreatepixmap
#    define XCreateWindow		xcreatewindow
#    define XDestroyImage		xdestroyimage
#    define XDisplayString		xdisplaystring
#    define XDrawLine			xdrawline
#    define XDrawPoint			xdrawpoint
#    define XDrawString			xdrawstring
#    define XFillPolygon		xfillpolygon
#    define XFillRectangle		xfillrectangle
#    define XFillRectangles		xfillrectangles
#    define XFlush			xflush
#    define XFree			xfree
#    define XFreeColors			xfreecolors
#    define XFreeFont			xfreefont
#    define XFreeFontNames		xfreefontnames
#    define XFreeGC			xfreegc
#    define XFreePixmap			xfreepixmap
#    define XGetDefault			xgetdefault
#    define XGetGCValues		xgetgcvalues
#    define XGetGeometry		xgetgeometry
#    define XGetImage			xgetimage
#    define XGetRGBColormaps		xgetrgbcolormaps
#    define XGetVisualInfo		xgetvisualinfo
#    define XGetWindowAttributes	xgetwindowattributes
#    define XGetWindowProperty		xgetwindowproperty
#    define XInitImage			xinitimage
#    define XInternAtom			xinternatom
#    define XListFonts			xlistfonts
#    define XLoadQueryFont		xloadqueryfont
#    define XMapWindow			xmapwindow
#    define XNextEvent			xnextevent
#    define XOpenDisplay		xopendisplay
#    define XPutImage			xputimage
#    define XQueryColor			xquerycolor
#    define XResizeWindow		xresizewindow
#    define XSendEvent			xsendevent
#    define XSetBackground		xsetbackground
#    define XSetClipMask		xsetclipmask
#    define XSetClipOrigin		xsetcliporigin
#    define XSetErrorHandler		xseterrorhandler
#    define XSetFillStyle		xsetfillstyle
#    define XSetFont			xsetfont
#    define XSetForeground		xsetforeground
#    define XSetFunction		xsetfunction
#    define XSetLineAttributes		xsetlineattributes
#    define XSetTile			xsettile
#    define XSetWindowBackgroundPixmap	xsetwindowbackgroundpixmap
#    define XSetWMHints			xsetwmhints
#    define XSetWMNormalHints		xsetwmnormalhints
#    define XStoreName			xstorename
#    define XSync			xsync
#    define XVisualIDFromVisual		xvisualidfromvisual
#    define XWMGeometry			xwmgeometry
#    define XtAppCreateShell		xtappcreateshell
#    define XtCloseDisplay		xtclosedisplay
#    define XtCreateApplicationContext	xtcreateapplicationcontext
#    define XtDestroyApplicationContext	xtdestroyapplicationcontext
#    define XtDestroyWidget		xtdestroywidget
#    define XtAppSetFallbackResources	xtappsetfallbackresources
#    define XtGetApplicationResources	xtgetapplicationresources
#    define XtOpenDisplay		xtopendisplay
#    define XtToolkitInitialize		xttoolkitinitialize
#    define CADDR_T
#else
#include <vms_x_fix.h>
#   endif
#  include <decw$include/Xlib.h>
#  include <decw$include/Xproto.h>
#  include <decw$include/Xatom.h>
#  include <decw$include/Xutil.h>
#  include <decw$include/Intrinsic.h>
#  include <decw$include/StringDefs.h>
#  include <decw$include/Shell.h>
#else
#  include <X11/Xlib.h>
#  include <X11/Xproto.h>
#  include <X11/Xatom.h>
#  include <X11/Xutil.h>
#  include <X11/Intrinsic.h>
#  include <X11/StringDefs.h>
#  include <X11/Shell.h>
#endif
#ifndef XtOffsetOf
#  ifdef offsetof
#    define XtOffsetOf(s_type,field) offsetof(s_type,field)
#  else
#    define XtOffsetOf(s_type,field) XtOffset(s_type*,field)
#  endif
#endif
#  if defined(XtSpecificationRelease) && (XtSpecificationRelease >= 4)
#    define HaveStdCMap 1
#  else
#    define HaveStdCMap 0
#    undef XVisualIDFromVisual
#    define XVisualIDFromVisual(vis) ((vis)->visualid)
#  endif
#  if !(defined(XtSpecificationRelease) && (XtSpecificationRelease >= 6))
#    undef XInitImage
#    define XInitImage(im) 1
#  endif
#define private private_
#endif