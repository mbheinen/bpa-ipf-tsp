#ifndef _XmDrawGP_h
#define _XmDrawGP_h

#include "drawgadget.h"
#include <Xm/GadgetP.h>
#include <Xm/ExtObjectP.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************/
/*  The Draw Widget Class and instance records  */
/************************************************/

typedef struct _XmDrawGadgetClassPart     /* draw class record */
{
	XtPointer			extension; /* Pointer to extension record */
} XmDrawGadgetClassPart;

typedef struct _XmDrawGadgetClassRec
{
	RectObjClassPart       rect_class;
	XmGadgetClassPart      gadget_class;
	XmDrawGadgetClassPart draw_class;
} XmDrawGadgetClassRec;

externalref XmDrawGadgetClassRec xmDrawGadgetClassRec;

typedef struct _XmDrawGadgetPart
{
    XtCallbackList   draw_callback;
    XtCallbackList   button_callback;
	XmString	draw;  /* String sent to this widget */
	XmFontList	font;

	/* PRIVATE members -- values computed by DrawWidgetClass methods */
	GC		normal_GC;   /* GC for text */	
	GC		insensitive_GC;
	XRectangle	TextRect;    /* The bounding box of the text, or clip
					rectangle of the window; whichever is smaller */
	Boolean show_label;
	unsigned char draw_type;

} XmDrawGadgetPart;


typedef struct _XmDrawGadgetRec
{
	ObjectPart        object;
	RectObjPart       rectangle;
	XmGadgetPart      gadget;
	XmDrawGadgetPart draw;
} XmDrawGadgetRec;

/* Inherited  Functions exported by draw */

#define XmInheritSetOverrideCallback ((XtWidgetProc) _XtInherit)
#define XmInheritResize  ((XtWidgetProc) _XtInherit)

/* Padding between draw text and accelerator text */

#define LABELG_ACC_PAD		15

/* MACROS */
#define LabG_DrawType(w)			(((XmDrawGadget)(w)) -> \
					   draw.draw_type)

#define LabG__draw(w)			(((XmDrawGadget)(w)) -> \
					   draw.draw)
#define LabG_Font(w)			(((XmDrawGadget)(w)) -> \
					   draw.font)
#define LabG_Pixmap(w)			(((XmDrawGadget)(w)) -> \
                                           draw.pixmap)
#define LabG_PixmapInsensitive(w)	(((XmDrawGadget)(w)) -> \
                                           draw.pixmap_insen)
#define LabG_NormalGC(w)		(((XmDrawGadget)(w)) -> \
                                           draw.normal_GC)
#define LabG_InsensitiveGC(w)		(((XmDrawGadget)(w)) -> \
                                           draw.insensitive_GC)
#define LabG_TextRect(w)		(((XmDrawGadget)(w)) -> \
                                           draw.TextRect)
#define LabG_TextRect_x(w)		(LabG_TextRect(w).x)
                                           
#define LabG_TextRect_y(w)		(LabG_TextRect(w).y)
                                           
#define LabG_TextRect_width(w)		(LabG_TextRect(w).width)
                                           
#define LabG_TextRect_height(w)		(LabG_TextRect(w).height)
                                           
#define LabG_IsText(w)			(LabG_DrawType(w) == XmSTRING)

#define LabG_IsPixmap(w)		(LabG_DrawType(w) == XmPIXMAP)

#define LabG_Shadow(w)                   (((XmDrawGadget)(w))->gadget.shadow_thickness)
#define LabG_Highlight(w)                (((XmDrawGadget)(w))->gadget.highlight_thickness)
#define LabG_Baseline(w)                 (_XmStringBaseline ( \
                                         ((XmDrawGadget)(w))->draw.font,\
                                         ((XmDrawGadget)(w))->draw.draw))

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmDrawGP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
