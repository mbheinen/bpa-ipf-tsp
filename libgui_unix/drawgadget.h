#ifndef _XmDrawG_h
#define _XmDrawG_h

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

/*  Widget class and record definitions  */
#define XiNdrawCallback     "xiDrawCallback"
#define XiNbuttonCallback     "xiButtonCallback"
#define XiNshowLabel     "xiShowLabel"

#define XiCDrawCallback "XiDrawCallback"
#define XiCButtonCallback "XiButtonCallback"
#define XiCShowLabel "XiShowLabel"

#define XiDefaultDrawCallback   NULL
#define XiDefaultButtonCallback   NULL
#define XiDefaultShowLabel False

typedef struct
{
    int     reason;
    XEvent  *event;
    Window  window;
    int     click_count;
    Region  region;
    Position x,y;
    Dimension width,height;
} XidrawgadgetCallbackStruct;

externalref WidgetClass xmDrawGadgetClass;

typedef struct _XmDrawGadgetClassRec * XmDrawGadgetClass;
typedef struct _XmDrawGadgetRec      * XmDrawGadget;

/*fast subclass define */
#ifndef XmIsDrawGadget
#define XmIsDrawGadget(w)     XtIsSubclass(w, xmDrawGadgetClass)
#endif /* XmIsDrawGadget */

/********    Public Function Declarations    ********/
extern Widget XmCreateDrawGadget( 
                        Widget parent,
                        char *name,
                        Arg *arglist,
                        Cardinal argCount) ;

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmDrawG_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
