static char SCCSID_edgec[] = "@(#)EdgeG.c	20.4 5/10/94";

/***************************************************************************
*
* module: EdgeG.c
* purpose: define the Edge Gadget to draw lines on a drawing area
* author: Dan Clark  Sep 09, 1991
* Major revision May 07, 1994 for OSF 1.2/OSF 2.0 compatability
* This object was written in an attempt to maintain maximum portability
* between versions of XWindows Systems and MOTIF software releases
* starting with X/11 R4 and Motif V1.0.
* The object was most recently revised on X11/R5 and MOTIF V2.0
* but the functionality for standards like drag-and-drop have been ommitted
* for backwards compatability with previous releases of MOTIF and X11.
* The class hierarchy for this object is:
* Object->RectObj->XmGadget->EdgeG
*
* background:
* For a better understanding of this module:
* - see X Toolkit Intrinsics Porgramming Manual by Adrian Nye & Tim O'Reilly
* especially chapter 5, "Inside a Widget"
* - see MOTIF V1.1 (or later versions) source code for the push button gadget, 
* label button gadget and gadget objects.
* files: PushBG.c, PushBGP.h, PushBG.h
*        LabelBG.c, LabelBGP.h, LabelBG.h
*        Gadget.c, GadgetP.h, Gadget.h 
*
* known problems:
* The application is responsible for making sure the bounding rectangle defined
* by X,Y, Width & Height is large enough to cover the line length and width,
* the arrow symbol, the transformer symbol and the solution data text.
* If this condition is not met traces of the offending data will be left
* on the drawing area of the screen after movement or partial exposure.
*
* Future versions can reduce dependance on MOTIF by only utilizing
* the single drawing area callback and maintaining all the drawing and
* text handling functions within the application instead of inside
* the Redisplay logic of this module.
* Furthermore if a MOTIF release contains a drawn button gadget this module 
* could be replaced by the use of the standard gadget.  Widgets cannot
* be used due poor performance for large numbers of screen objects.
*
****************************************************************************
*/

#include <Xm/XmP.h>
/* #include <Xm/ManagerP.h> can be used after OSF 2.0 */
#include "EdgeGP.h"   /** only the widget definition code has private header **/
#include <math.h>
#include <stdio.h>	/*** for debugging only ***/
#include <stdlib.h>	

#define max(A,B) ((A) > (B) ? (A) : (B))
#define RADIAN  57.2957795

/*** define resource list 	***/
#define res_offset(field)    XtOffset(EdgeGadget,edge.field)
#define goffset(field)	     XtOffset(EdgeGadget,rectangle.field)
#define gadget_offset(field) XtOffset(EdgeGadget,gadget.field)
 
static XtResource resources[] = {
	{ XiNx1, XiCX1, XmRDimension, sizeof(Dimension),
	res_offset(x1), XmRImmediate, (XtPointer) XiDefaultX1 },
	{ XiNy1, XiCY1, XmRImmediate, sizeof(Dimension),
	res_offset(y1), XmRString, (XtPointer) XiDefaultY1 },
	{ XiNx2, XiCX2, XmRImmediate, sizeof(Dimension),
	res_offset(x2), XmRString, (XtPointer) XiDefaultX2 },
	{ XiNy2, XiCY2, XmRImmediate, sizeof(Dimension),
	res_offset(y2), XmRString, (XtPointer) XiDefaultY2 },
	{ XiNdashOn, XiCDashOn, XmRBoolean, sizeof(Boolean),
	res_offset(dashOn), XmRImmediate, (XtPointer) FALSE},
	{ XiNboxOn, XiCBoxOn, XmRBoolean, sizeof(Boolean),
	res_offset(boxOn), XmRImmediate, (XtPointer) TRUE},
	{ XiNarrowDirection, XiCArrowDirection, XmRBoolean, sizeof(Boolean),
	res_offset(arrowDirection), XmRImmediate, (XtPointer) FALSE},
	{ XiNarrowOn, XiCArrowOn, XmRBoolean, sizeof(Boolean),
	res_offset(arrowOn), XmRImmediate, (XtPointer) FALSE},
	{ XiNtransformerOn, XiCTransformerOn, XmRBoolean, sizeof(Boolean),
	res_offset(transformerOn), XmRImmediate, (XtPointer) FALSE},
	{ XiNlineThickness, XiCLineThickness, XmRHorizontalDimension, 
	sizeof(Dimension), res_offset(lineThickness), XmRImmediate, 
	(XtPointer) XiDefaultLineThickness},

	{ XiNinputCallback,XiCInputCallback,XmRCallback, sizeof(XtCallbackList),
	res_offset(inputCallback),XmRPointer,(XtPointer)XiDefaultInputCallback},
	{ XiNdrawCallback, XiCDrawCallback, XmRCallback, sizeof(XtCallbackList),
	res_offset(drawCallback), XmRPointer, (XtPointer)XiDefaultDrawCallback},
	{ XiNdrawPixel, XiCDrawPixel, XmRPixel, sizeof(Pixel),
	res_offset(drawPixel), XmRImmediate, (XtPointer) XiDefaultDrawPixel},
        /* XmUNSPECIFIED_PIXMAP */ 
        { XiNdrawPixmap, XiCDrawPixmap, XmRGadgetPixmap, sizeof(Pixmap),
	res_offset(drawPixmap), XmRImmediate, (XtPointer) XiDefaultDrawPixmap},
	{ XiNlabelOn, XiCLabelOn, XmRBoolean, sizeof(Boolean),
	res_offset(labelOn), XmRImmediate, (XtPointer) FALSE},
        { XiNlabelString, XmCXmString, XmRXmString, sizeof(_XmString),
	res_offset(label), XmRImmediate, (XtPointer) XiDefaultLabelString},
/*** try using standard definition for fonts to pass through resources
        { XiNlabelFontList,XmCLabelFontList,XmRFontList,sizeof(XmFontList),
	res_offset(labelFontList), XmRImmediate, (XtPointer) XiDefaultLabelFontList},
***/
        { XmNfontList,XmCFontList,XmRFontList,sizeof(XmFontList),
	res_offset(labelFontList), XmRImmediate, (XtPointer) NULL}

}; /* end resources */

#undef res_offset
#undef goffset
#undef pushb_offset

/*** action function declarations 	***/
/*** NO ACTIONS for Gadgets		***/

/*** the actions table 			***/
/*** NO ACTIONS for Gadgets		***/

/*** default translation table		***/
/*** NO TRANSLATIONS for Gadgets	***/

/*** method function declarations	***/
#ifdef _NO_PROTO
static void ClassInitialize();
static void ClassPartInitialize();
static void Initialize();
static void InitializeHook();
static void Realize();
static void Redisplay();		/* expose procedure	*/
static void Resize();		
static Boolean SetValues();
static Boolean SetValuesHook();
static void SetValuesAlmost();
static void Destroy();
static void setDash();
static void getAngleDistancePosition ();
static int roundoff();

/*** function declarations for XmGadget part ***/
static void Arm ();
static void Activate ();
static void arm_and_activate();
static void border_highlight();
static void border_unhighlight();
static Boolean visual_change();
static void input_dispatch();

/*** following functions are private to Edge	***/
static void PrintEdgeRec();
static void PrintEdgeClassRec();
static int drawArrow();
static int drawText();
static int drawTransformer();
static XtGeometryResult QueryGeometry();

#else /* use prototypes */

static void ClassInitialize( void ) ;
static void ClassPartInitialize( WidgetClass widget_class) ;
static void InitializeHook( Widget req, Widget new_w, ArgList args,
                        Cardinal *num_args) ;
static void border_highlight(Widget wid);
static void border_unhighlight(Widget wid);
static void Arm (Widget pb, XEvent *event);
static void Activate (Widget pb, XEvent *event);
static void arm_and_activate();
static void PrintEdgeRec(Widget gw);
static void PrintEdgeClassRec(void);
static Boolean SetValuesHook (Widget gadget, ArgList args, Cardinal *num_args);
static Boolean visual_change(Widget child, Widget current, Widget new);
static void input_dispatch(Widget gw, XEvent *event, Mask call_data);
static Boolean SetValuesPrehook( Widget oldParent, Widget refParent,
                        Widget newParent, ArgList args, Cardinal *num_args) ;
static void GetValuesPrehook( Widget newParent, ArgList args,
                        Cardinal *num_args) ;
static void GetValuesPosthook( Widget new_w, ArgList args, Cardinal *num_args);
static Boolean SetValuesPosthook( Widget current, Widget req, Widget new_w, 
                        ArgList args, Cardinal *num_args) ;
static void Resize( Widget wid) ;
static void Initialize( Widget req, Widget new_w,
                        ArgList args, Cardinal *num_args) ;
static XtGeometryResult QueryGeometry( Widget wid, XtWidgetGeometry *intended,
                        XtWidgetGeometry *reply) ;
static void Destroy( Widget w) ;
static void Redisplay( Widget wid, XEvent *event, Region region) ;
static Boolean SetValues( Widget cw, Widget rw, Widget nw,
                        ArgList args, Cardinal *num_args) ;
static void SetValuesAlmost( Widget cw, Widget nw,
                        XtWidgetGeometry *request, XtWidgetGeometry *reply) ;
static int drawArrow(Widget gw);
static int drawText(Widget gw);
static int drawTransformer(Widget gw);

#endif /* _NO_PROTO */

/*** class record initialization (core, edge)	***/
/*
*    there are three basic fields in this structure: 
* -> class data
* -> 2 chained methods  => adds to (called after superclass) method
*	class_part_init, initialize
*	set_values, get_values_hook, set_values_hook, initialize_hook
*	>resources (downward chained)
*	destroy (only upward chained)
*	>actions (upward chained)
*    pointers to methods 'inherited' from superclass with chaining
*	ClassInitialize(), ClassPartInitialize(),
*	Initialize(), InitializeHook()
*	SetValues(), SetValuesHook(), GetValuesHook() 
* -> 1 self-contained methods => replace methods from superclass with same name
*	class_initialize, realize, resize, expose, 
*	accept_focus, set_values_almost, query_geometry
*	>translations
*    pointers to methods that use symbols to add methods to the class
*	XtInheritTranslations, XtInheritRealize, XtInheritResize
*	XtInheritExpose, XtInheritSetValuesAlmost, XtInheritAcceptFocus(?),
*	XtInheritQueryGeometry
* note: hook fields are for use in widgets that have subparts 
*       that are not widgets
*/


externaldef (edgeclassrec) 
  EdgeClassRec edgeClassRec = {
  /* rectobj class part fields */
    {
    /* (WidgetClass) &rectObjClassRec,  pointer to rect superclass ClassRec */
    (WidgetClass)&xmGadgetClassRec, /* superclass pointer to gadget ClassRec */
    (String)"Edge",		/* class_name widget resource */
    (Cardinal)sizeof(EdgeRec),	/* widget_size in bytes of widget record   */
    (XtProc)ClassInitialize,	/* 1 class_initialize proc	*/
    (XtWidgetClassProc)ClassPartInitialize, /* 2 class_part_initilize dynamic */
    (Boolean)FALSE,	/* class_inited has class been initialized?      */
    (XtInitProc)Initialize, 	/* 2 initialize instance part fields       */
    (XtArgsProc)InitializeHook,	/* 2 NULL, initialize_hook, notify initialize called */
    (XtProc)NULL,	/* w 1 realize, XCreateWindow widget only */
    (XtPointer)NULL,	/* w actions, widget only semantics name to proc map */
    (Cardinal)0,	/* w num_actions */
    (XtResourceList)resources,	/* resources for subclass fields */
    (Cardinal)XtNumber(resources), /* num_resources */
    (XrmClass)NULLQUARK,	/* xrm_class resource class quarkified	    */
    (Boolean)True,	/* w compress_motion MotionNotify events */
    (Boolean)XtExposeCompressMaximal,	/* w compress_exposure events */
    (Boolean)True,	/* w compress enter/leave events  */
    (Boolean)False,	/* w visibility_interest for VisibilityNotify      */
    (XtWidgetProc)Destroy,	/* 2 destroy data for subclass pointers  */
    (XtWidgetProc)Resize,	/* w 1 resize, geom manager changed size */
    (XtExposeProc)Redisplay, 	/* 1 expose XtInheritExpose,  routine to redisplay window */
    (XtSetValuesFunc)SetValues,	/* 2 set_values of subclass resource */
    (XtArgsFunc)SetValuesHook,	/* 2 set values hook notify set_values call */
    (XtAlmostProc)SetValuesAlmost, /* 1` set values almost got "Almost" geo reply */
    (XtArgsProc)NULL,	/* 2 get alues hook notify that get_values called    */
    (XtProc)NULL,	/* w 1 accept_focus, assign input focus to widget     */
    (XtVersionType)XtVersion,	/* version of intrinsics used	    */
    (XtPointer)NULL,	/* list of callback offsets */
    (String)NULL, /* ? default trans tm_table state machine  */
    (XtGeometryHandler)QueryGeometry, /* 1` query_geometry rtn prefer gmtry */
    (XtProc)NULL,	/* w display accfelerator */
    (XtPointer)NULL,	/* extension record pointer */
    } , /* end rectobj class part fields */
  /* start typedef struct _XmGadgetClassPart */
    {
    (XtWidgetProc)border_highlight, /* XmInheritBorderHighlight,  border_highlight draw rect */
    (XtWidgetProc)border_unhighlight, /* XmInheritBorderUnhighlight, border_unhighlight */
    (XtActionProc)arm_and_activate, /* arm_and_activate */
    /* (XmInputDisProc) for motif v1.1  */
    /* (XmWidgetDispatchProc) for motif V1.2 */ 
    input_dispatch,	/* input_dispatch */
    (XmVisualChangeProc)visual_change,	/* visual_change */
    (XmSyntheticResource *)NULL,	/* structure get_resources */
    (int)0,	/* num_syn_resources */
    (XmCacheClassPartPtr)NULL,	/* cache part */
    (XtPointer)NULL,	/* extension; */
    },  /* end  XmGadgetClassPart; */ 
    /* start edge class part */
    { 
    (int)0,	/* dummy field */
    } /* end edge part class fields */
 }; /* end edge class */

/*** section of shared data local to edge widget ***/
/*** temporary shared variables ***/

static int	edge_gadget_cnt = 0;
static Boolean	debug_flag = False;


/*** 
*** this procedure is only included to ensure that some
*** linkers (such as the VAX VMS LINK ) pull in this
*** object from the object library 
*** should become obsolete with the use of externaldef and externalref
***/
int
#ifdef _NO_PROTO
EdgeGCheckClass()
#else
EdgeGCheckClass( void )
#endif /* _NO_PROTO */
{
  printf("EdgeG> WARNING: EdgeGCheckClass should not be called\n");
  if(edgeGadgetClass != (WidgetClass) &edgeClassRec) {
    printf("EdgeG> set edgeGadgetClass\n");
    edgeGadgetClass = (WidgetClass) &edgeClassRec;
  }
}
/* --------------------- Gadget Object methods	--------------------------
* XtProc takes no arguments
* called once - first time an instance of a class is created by appl
* type converters
* see sec 9.4 X toolkit intrinsics V4
* -------------------------------------------------------------------------
*/

/*************************************************************************
* ClassInitialize
* initialize the fields for the class data structure 
*
**************************************************************************
*/
static void
#ifdef _NO_PROTO
ClassInitialize()
#else
ClassInitialize( void )
#endif /* _NO_PROTO */
{

  if(debug_flag == True) {
    printf("Edge> ClassInitialize gadget\n");
    printf("Edge> ClassInit Size of EdgeClassRec = %d\n", 
						sizeof(EdgeClassRec));
    printf("Edge> ClassInit Size of EdgeRec = %d\n", sizeof(EdgeRec));
    printf("Edge> (XtVersionType) rect_class.version %d\n",
         edgeClassRec.rect_class.version);
    PrintEdgeClassRec();
  }

    edge_gadget_cnt = 0;

} /* end ClassInitialize */

static void 
#ifdef _NO_PROTO
ClassPartInitialize(widget_class)
  WidgetClass widget_class;
#else
ClassPartInitialize(
  WidgetClass widget_class)
#endif /* _NO_PROTO */
/******************************************************************************\
* ClassPartInitialize
* XtWidgetClassProc
* called once - first time an instance of a class is created by appl
* same as ClassInitialize but downward chained.
* resolves inheritance of self sufficient methods from the immediate superclass
* Needed only in classes that define their own methods in their class part
* see ch 13 X toolkit intrinsics V4
*******************************************************************************/
{
#ifdef notyet
   EdgeGadgetClass pwc = (EdgeGadgetClass) widget_class;
   XmGadgetClass psc = (XmGadgetClass)widget_class->rect_class.superclass;

   if (pwc->rect_class.resize == XmInheritResize)
      pwc->rect_class.resize = psc->rect_class.resize;
#endif
    if(debug_flag == TRUE) {
       printf("Edge> ClassPartInitialize gadget\n");
    }
} /* end ClassPartInitialize */

/***
* defineDrawGC
* define the graphics context for drawing lines
* derived from MOTIF source code Version 1.1
***/
static void
#ifdef _NO_PROTO
defineDrawGC( peg )
  EdgeGadget peg;
#else
defineDrawGC(
  EdgeGadget peg )
#endif /* _NO_PROTO */
{
  XGCValues drawGCValues;
  XtGCMask drawGCMask; 
  /* XmManagerWidget pmw; */
  Widget ppw; /* pointer to the parent widget */
  XFontStruct     *fs = (XFontStruct *) NULL;


    /* pmw = (XmManagerWidget) XtParent((Widget)peg); */
    ppw = (Widget)XtParent((Widget)peg);

    if(peg->edge.drawPixel != NULL) {
      drawGCMask = GCForeground | GCBackground | GCLineWidth | GCFont; 
      drawGCValues.foreground = peg->edge.drawPixel;
      drawGCValues.background = peg->edge.drawPixel;
    } else {
      drawGCMask = GCForeground | GCBackground | GCLineWidth | GCFont |
                   GCGraphicsExposures | GCClipMask; 

/***
      drawGCValues.foreground = pmw->manager.foreground;
      drawGCValues.background = pmw->core.background_pixel;
***/
      drawGCValues.foreground = ppw->core.border_pixel;
      drawGCValues.background = ppw->core.background_pixel;
      drawGCValues.graphics_exposures = FALSE;
      drawGCValues.clip_mask = None;
    }
    drawGCValues.line_width = peg->edge.lineThickness;

    _XmFontListGetDefaultFont(peg->edge.labelFontList, &fs);
    if (fs==NULL)
      drawGCMask &= ~GCFont;
    else
      drawGCValues.font       = fs->fid;

    peg->edge.drawGC = XtGetGC((Widget)peg, drawGCMask, &drawGCValues);

    if(debug_flag == TRUE) {
      printf("Edge: defineDrawGC: drawPixmap %d drawPixel %d\n",
         peg->edge.drawPixmap, 
         peg->edge.drawPixel );
    };

} /* end defineDrawGC */
/**************************************************************
*
*  setup the XmString
****************************************************************
*/
InitializeText(Widget wid)
{
  EdgeGadget gw = (EdgeGadget) wid;
  Dimension w, h;
#ifdef OSF_2_0
  XmFontListEntry entry1, entry2;
  XmFontList defaultFont;
#endif

#ifdef OSF_2_0
    if (gw->edge.labelFontList == NULL) {
      if(debug_flag == TRUE) 
         printf("labelFontList NULL so create a label font\n");

      entry1 = XmFontListEntryLoad(XtDisplay(XtParent(wid)), 
               "courier12", XmFONT_IS_FONT, "TAG1");
      entry2 = XmFontListEntryLoad(XtDisplay(XtParent(wid)), 
               "courier14", XmFONT_IS_FONT, "TAG2");
      defaultFont = XmFontListAppendEntry(NULL, entry1);
      defaultFont = XmFontListAppendEntry(defaultFont, entry2);
      gw->edge.labelFontList = XmFontListCopy (defaultFont);
    } else {
     /* Make a local copy of the font list */
      if(debug_flag == TRUE) 
        printf(" initialize make a copy of the font list\n");
     gw->edge.labelFontList = XmFontListCopy( gw->edge.labelFontList );
    }
#else
    if (gw->edge.labelFontList == NULL) {
      XFontStruct font;
      XmFontList defaultFont;

      if(debug_flag == TRUE)  {
         printf("labelFontList NULL so create a label font\n");
      }
      defaultFont = _XmGetDefaultFontList( (Widget) gw, XmLABEL_FONTLIST);
      gw->edge.labelFontList = defaultFont;
/***
      gw->edge.labelFontList = XmFontListAdd(gw->edge.labelFontList, 
         &font, );
***/
    }
#endif

    if (gw->edge.label == NULL) {
      if(debug_flag == TRUE) 
        printf("create a label from the name of the widget \n");
#ifdef OSF_2_0
      gw->edge.label =  XmStringCreate(XrmQuarkToString(gw->object.xrm_name), "TAG1");
#else
      gw->edge.label =  XmStringCreate(XrmQuarkToString(gw->object.xrm_name), 
        XmSTRING_DEFAULT_CHARSET);
#endif
    } else { 
      if(debug_flag == TRUE) 
        printf(" label is xm string so copy it over \n");
      gw->edge.label = XmStringCopy( (XmString)gw->edge.label );
    }
}


/******************************************************************************\
* XtInitProc Initialize 
* sets initial values for all the fields in the instance part structure
* data checking on public access fields
* see ch. 6 X toolkit intrinsics V4
*******************************************************************************/
static void 
#ifdef _NO_PROTO
Initialize( request, new, args, num_args) 
  Widget request;
  Widget new;
  ArgList args;
  Cardinal *num_args; 
#else
Initialize(
  Widget request,
  Widget new,
  ArgList args,
  Cardinal *num_args) 
#endif /* _NO_PROTO */
{ 
  EdgeGadget edgeg = (EdgeGadget)new;

    if(debug_flag == TRUE) {
      printf("Edge> Initialize widget\n");
      PrintEdgeClassRec();
      PrintEdgeRec(new);
    }

    edge_gadget_cnt++;

    /* set the input types that this gadget should respond to */
    edgeg->gadget.event_mask = XmARM_EVENT | XmACTIVATE_EVENT | XmHELP_EVENT |
        XmFOCUS_IN_EVENT | XmFOCUS_OUT_EVENT |  
        XmMULTI_ARM_EVENT |  XmMULTI_ACTIVATE_EVENT;
/* | XmBDRAG_EVENT */

    if(debug_flag == TRUE) {
      printf("Edge> (Mask) gadget.event_mask %d\n",
        ((EdgeGadget) new)->gadget.event_mask);
    }

    InitializeText(new);

    /* setup the graphics context for the gadget */
    defineDrawGC(edgeg);

    /*** make sure the gadget has a minimum size for display */
    Resize((Widget)new); 

#ifdef DEBUG
    printf("Edge> Initialize req widget id = %d\n", request);
    printf("Edge> Initialize req window id = %d\n", XtWindow(request));
    printf("Edge> Initialize req Display id = %d\n", XtDisplay(request));
    printf("Edge> Initialize req parent window id = %d\n", XtWindow(XtParent(request)));
    printf("Edge> Initialize req parent display id = %d\n", XtDisplay(XtParent(request)));

    printf("Edge> Initialize new widget id = %d\n", new);
    printf("Edge> Initialize new window id = %d\n", XtWindow(new));
    printf("Edge> Initialize new Display id = %d\n", XtDisplay(new));
    printf("Edge> Initialize new parent window id = %d\n", XtWindow(XtParent(new)));
    printf("Edge> Initialize new parent display id = %d\n", XtDisplay(XtParent(new)));
#endif

} /* end Initialize */


/******************************************************************************\
* XtArgsProc InitializeHook
* called after an initialization
*******************************************************************************/
static void 
#ifdef _NO_PROTO
InitializeHook(req, new_w, args, num_args)
  Widget req;
  Widget new_w;
  ArgList args;
  Cardinal *num_args;
#else
InitializeHook(
  Widget req,
  Widget new_w,
  ArgList args,
  Cardinal *num_args)
#endif
{
    if(debug_flag == TRUE) {
       printf("Edge> InitializeHook gadget proc \n");
    }
}

static void 
#ifdef _NO_PROTO
Destroy (w)
  Widget w;
#else
Destroy (
  Widget w)
#endif
/********************************************************************
* XtWidgetProc
* deallocates local and server memeory allocated by this widget.
* This is called when an application destroys a gadget but remains running
* see X Toolkit Intrinsic Programming Manual V4 ch 6
*
* note: free GC even if some gadgets left since the free
* does not really remove the GC from the server do to cacheing of
* similar GCs. (see X Toolkit Intrinsic Progamming Manual pg. 183)
**********************************************************************/
{
  EdgeGadget peg = (EdgeGadget) w;

    if(debug_flag == TRUE) {
      printf("Edge> Destroy widget\n");
    }
    XtReleaseGC(XtParent(peg), peg->edge.drawGC); 
}

static void
#ifdef _NO_PROTO
Resize( wid )
  Widget wid;
#else
Resize(
  Widget wid )
#endif /* _NO_PROTO */
/********************************************************************
* Resize
* Sets width and height based on contents of gadget
*********************************************************************
*/
{
  EdgeGadget edgeg = (EdgeGadget)wid;
  Dimension width, height;

  if(True == debug_flag) {
    printf("Edge> Resize widget\n");
  }
#ifdef OLDWAY
    /*** set the gadget x,y,width,height and orientation ***/
    /*** based on the x1,y1 and x2,y2 set for the gadget ***/
    if(edgeg->edge.x1 < edgeg->edge.x2) {
        RX(new) = edgeg->edge.x1 + (int)(edgeg->edge.xsize/2);
    } else {
        RX(new) = edgeg->edge.x2 + (int)(edgeg->edge.xsize/2);
    };

    if(edgeg->edge.y1 < edgeg->edge.y2) {
        RY(new) = edgeg->edge.y1 + (int)(edgeg->edge.ysize/2);
    } else {
        RY(new) = edgeg->edge.y2 + (int)(edgeg->edge.ysize/2);
    };
    if(((edgeg->edge.x1 < edgeg->edge.x2)&&
        (edgeg->edge.y1 < edgeg->edge.y2)) ||
        ((edgeg->edge.x1 >= edgeg->edge.x2)&&
        (edgeg->edge.y1 >= edgeg->edge.y2)) )
    {
        edgeg->edge.orientation = 0;
    } else {
        edgeg->edge.orientation = 1;
    }
    /* printf("Bustest edge_shape = %d\n", edge_shape); */
    RWidth(new) = abs(edgeg->edge.x1 - edgeg->edge.x2);
    RHeight(new) = abs(edgeg->edge.y1 - edgeg->edge.y2);
#endif
   /** now make the box is big enough to fit the label **/
/***
                  edgeg->rectangle.x, 
                  edgeg->rectangle.y + (Dimension)(edgeg->rectangle.height/2),
                  edgeg->rectangle.width,
***/
   XmStringExtent(edgeg->edge.labelFontList, edgeg->edge.label,
                  &width, &height);
   /** box must be big enough to contain text **/
   if(edgeg->rectangle.width < width) {
     if(True == debug_flag) {
       printf("Resize: increase width from %d to %d\n", 
         edgeg->rectangle.width, width);
     }
     edgeg->rectangle.width = width;
   }
   if(edgeg->rectangle.height < height) {
     if(True == debug_flag) {
       printf("Resize: increase height from %d to %d\n", 
         edgeg->rectangle.height, height);
     }
     edgeg->rectangle.height = height;
   }
   
   if (edgeg->rectangle.width == 0)    /* set core width and height to a */
       edgeg->rectangle.width = 1;     /* default value so that it doesn't */
   if (edgeg->rectangle.height == 0)   /* a Toolkit Error */
       edgeg->rectangle.height = 1;
}
static void drawBox(Widget wid) 
{
  EdgeGadget edgeg = (EdgeGadget)wid;

  if( TRUE == edgeg->edge.boxOn ) {
     int tmpx, tmpy;
     int tmpwidth, tmpheight;
     tmpwidth = max(edgeg->rectangle.width - 2, 0);
     tmpheight = max(edgeg->rectangle.height - 2, 0);
     tmpx = edgeg->rectangle.x + 1;
     tmpy = edgeg->rectangle.y + 1;

     XDrawRectangle( XtDisplay(wid), XtWindow(wid), 
	  edgeg->edge.drawGC, 
          tmpx, tmpy,
          tmpwidth, tmpheight);
  } /* end boxOn */
}


/******************************************************************************\
* Redisplay or expose
* XtExposeProc (see /usr/include/X11/IntrinsicP.h for param list )
* redraws a widget whenever an Expose event arrives from the server
* makes Xlib calls to draw on the parents window 
* uses instance variables to guide in the redrawing of the object
* Called by:  Any expose event
*******************************************************************************/
static void
#ifdef _NO_PROTO
Redisplay( wid, event, region )
  Widget wid ;
  XEvent *event ;
  Region region ;
#else
Redisplay(
  Widget wid,
  XEvent *event,
  Region region )
#endif /* _NO_PROTO */
{
  EdgeGadget edgeg = (EdgeGadget) wid;
  XtCallbackStatus xtstat;
  XiEdgeCallbackStruct call_value;

    if( debug_flag == True) { 
      printf("Edge> Redisplay debug\n");
      PrintEdgeClassRec();
      PrintEdgeRec(wid); 
    };

    if( debug_flag == True) { 
      printf("Edge> redisplay callback edge.drawCallback %d\n",
        ((EdgeGadget) wid)->edge.drawCallback->callback);
    };
     if ( edgeg->edge.drawCallback ) {
       call_value.reason = event->type;
       call_value.event = event;
       call_value.window = XtWindow(wid);
       call_value.click_count = 0;
       call_value.region = region;
       XtCallCallbackList(wid, edgeg->edge.drawCallback, &call_value);
     };
 
/** set line attributes with dash or nodash ***/
   /* setDash(wid); */

  XSetLineAttributes( XtDisplay(wid),
		      edgeg->edge.drawGC, 
		      edgeg->edge.lineThickness, 
		      LineSolid , CapProjecting, JoinMiter);
  XDrawLine	    ( XtDisplay(wid), XtWindow(wid), 
		      edgeg->edge.drawGC, 
		      edgeg->edge.x1, edgeg->edge.y1,
		      edgeg->edge.x2, edgeg->edge.y2);
/***
  XSetLineAttributes( XtDisplay(wid),
		      edgeg->edge.drawGC, 
		      XiDefaultLineThickness,
		      LineSolid , CapProjecting, JoinMiter);
***/

  if( TRUE == edgeg->edge.arrowOn	)  drawArrow(wid);
  if( TRUE == edgeg->edge.transformerOn	)  drawTransformer(wid);

  if( True == edgeg->edge.labelOn )  drawText(wid);

  /*** draw box surrounding the gadget area - for debug **/
  if( TRUE == edgeg->edge.boxOn ) drawBox(wid);

  if (edgeg->gadget.highlighted)
  {
    (*((EdgeGadgetClass) XtClass( edgeg))->gadget_class.border_highlight)(
                                                                 (Widget) wid) ;
  }

#ifdef DEBUG
    printf("Edge> Redisplay Edge gadget\n");
    printf("Edge> Redisplay Edge gadget Event.type = %d\n", event->type);
    printf("Edge> Redisplay widget id = %d\n", wid);
    printf("Edge> Redisplay window id = %d\n", XtWindow(wid));
    printf("Edge> Redisplay Display id = %d\n", XtDisplay(wid));
    printf("Edge> Redisplay parent window id = %d\n", XtWindow(XtParent(wid)));
    printf("Edge> Redisplay parent display id = %d\n", XtDisplay(XtParent(wid)));

    /* printf("EdgeGadget->shape = %d\n", ((EdgeGadget) wid)->edge.shape); */
    printf("Edge: (%dx%d) (%dx%d) dx (%dx%d)",
             edgeg->edge.x1, edgeg->edge.y1,
             edgeg->edge.x2, edgeg->edge.y2,
             edgeg->edge.x1 - edgeg->edge.x2,
             edgeg->edge.y1 - edgeg->edge.y2);
    printf("(%dx%d) (%dx%d)\n",
             edgeg->rectangle.x, edgeg->rectangle.y,
             edgeg->rectangle.width, edgeg->rectangle.height);
    printf("dashOn = %d, boxOn = %d, lineThickness = %d\n", 
       edgeg->edge.dashOn, edgeg->edge.boxOn, edgeg->edge.lineThickness);
#endif

} /* end Redisplay */


static int roundoff ( double fnum )
/******************************************************************************\
* Purpose:    Round off a floating point num to nearest integer
*
\******************************************************************************/
{
     if ( fnum>= 0 ) return ((int)(fnum+.5));

     return ((int)(fnum-.5));

}


static void 
getAngleDistancePosition ( int x, int y, int deg, int dist, int *x2, int *y2 )
/******************************************************************************\
*       Function: GET ANGLE-DISTANCE POSITION
*                 Returns the x2, y2 value of position after moving an
*                     angular distance from starting position x, y.
*
*       Params:  x: start x-coord
*                y:   "   y
*              deg: direction to move  (right=0, up=90, left=180, down=270)
*             dist: distance to travel to new position.
*              *x2: new x-position
*              *y2: new y_position
*
*       Author: Bill Rogers     July 23, 1991
*
*       Calls:  roundoff
*
\******************************************************************************/
{

#define RADIAN  57.2957795

#ifdef WHY
  while ( deg > 360 ) deg-=360;
  while ( deg < 0   ) deg+=360;
#endif

  *y2 = ( y - roundoff( sin (( double )( deg/RADIAN )) * dist ) );
  *x2 = ( x + roundoff( cos (( double )( deg/RADIAN )) * dist ) );

}



static void setDash(Widget gw)
/***************************************************************************\
\***************************************************************************/
{
  EdgeGadget edgeg = (EdgeGadget) gw;
    if(edgeg->edge.dashOn == TRUE) {
      static int dash_offset = 0;
      static char dash_list[] = { 5, 5 };
      static int dash_list_length = (int)sizeof(dash_list)/sizeof(dash_list[0]);
      XSetDashes (XtDisplay(gw),
		      edgeg->edge.drawGC, 
         dash_offset, dash_list, dash_list_length);
      XSetLineAttributes( XtDisplay(gw),
		      edgeg->edge.drawGC, 
         edgeg->edge.lineThickness, 
         LineOnOffDash, CapNotLast, JoinBevel);
    } else {
      XSetLineAttributes( XtDisplay(gw),
		      edgeg->edge.drawGC, 
         edgeg->edge.lineThickness, 
         LineSolid , CapRound, /* CapNotLast, */ JoinBevel);
    }
} /* end setDash */


static int drawArrow(Widget gw)
/****************************************************************************\
\****************************************************************************/
{
  EdgeGadget edgeg = (EdgeGadget) gw;
     int xmid, ymid;
     int xout, yout, xpt, ypt;
     int dy,dx;
     double theta;
     int alen;
     int itheta;
     /* struct { short x,y; } point[4]; */
     XPoint point[4];
     double pct = 0.3;
     
     XSetLineAttributes( XtDisplay(gw),
         /* edgeg->pushbutton.cache->fill_gc,  */
		      edgeg->edge.drawGC, 
	 XiDefaultLineThickness,
         LineSolid , CapProjecting, JoinMiter);

     xmid = (int)edgeg->edge.x1 + 
            (int)(((int)edgeg->edge.x2 - (int)edgeg->edge.x1)*pct);
     if(xmid < 0) {
       printf("programmer error: EdgeG: xmid = %d reset to 0\n", xmid);
       xmid = 0;
     }
     ymid = (int)edgeg->edge.y1 + 
            (int)(((int)edgeg->edge.y2 - (int)edgeg->edge.y1)*pct);
     if(ymid < 0) {
       printf("programmer error: EdgeG: ymid = %d reset to 0\n", ymid);
       ymid = 0;
     }
     dx = (int)(edgeg->edge.x2 - edgeg->edge.x1);
     dy = (int)(edgeg->edge.y2 - edgeg->edge.y1);
     if(0 == dy) dy = 1;
#ifdef TEST
     point[0].x = (short)(xmid + 5);
     point[0].y = (short)(ymid + 5);
     point[1].x = (short)(xmid);
     point[1].y = (short)(ymid);
     point[2].x = (short)(xmid - 5);
     point[2].y = (short)(ymid - 5);
     XDrawLines( XtDisplay(gw), XtWindow(gw), 
		      edgeg->edge.drawGC, 
            point, 3, 0);
#endif

#define LEN 10
#define ANGLE 30
     if(TRUE == edgeg->edge.arrowDirection ) {
       alen = -LEN;
     } else {
       alen = LEN;
     }
     theta = (double)( atan2((double)-dy, (double)dx) * RADIAN);
     itheta = (int)(theta + 0.5);

     getAngleDistancePosition(xmid, ymid, itheta, (int)alen/2, &xpt, &ypt);
     point[1].x = (short)(xpt);
     point[1].y = (short)(ypt);
     getAngleDistancePosition(xpt,ypt,itheta +180 + ANGLE, alen, &xout, &yout);
     point[0].x = (short)(xout);
     point[0].y = (short)(yout);
     getAngleDistancePosition(xpt,ypt,itheta +180 - ANGLE, alen, &xout, &yout);
     point[2].x = (short)(xout);
     point[2].y = (short)(yout);
     point[3].x = (short)0;
     point[3].y = (short)0;
     XDrawLines( XtDisplay(gw), XtWindow(gw), 
		      edgeg->edge.drawGC, 
                 point, 3, 0);

   /** now adjust the bounding box for the arrow **/
     if(edgeg->rectangle.x+2 > point[0].x) {
       edgeg->rectangle.width += abs(edgeg->rectangle.x - point[0].x) + 4;
       edgeg->rectangle.x = point[0].x - 2;
     } 
     if(edgeg->rectangle.x+2 > point[2].x) {
       edgeg->rectangle.width += abs(edgeg->rectangle.x - point[2].x) + 4;
       edgeg->rectangle.x = point[2].x - 2;
     } 
     if(edgeg->rectangle.x + edgeg->rectangle.width < point[0].x+2) {
       edgeg->rectangle.width += abs(edgeg->rectangle.x - point[0].x) + 2;
     }
     if(edgeg->rectangle.x + edgeg->rectangle.width < point[2].x+2) {
       edgeg->rectangle.width += abs(edgeg->rectangle.x - point[2].x) + 2;
     }
     if(edgeg->rectangle.y+2 > point[0].y) {
       edgeg->rectangle.height += abs(edgeg->rectangle.y - point[0].y) + 4;
       edgeg->rectangle.y = point[0].y - 2;
     } 
     if(edgeg->rectangle.y+2 > point[2].y) {
       edgeg->rectangle.height += abs(edgeg->rectangle.y - point[2].y) + 4;
       edgeg->rectangle.y = point[2].y - 2;
     } 
     if(edgeg->rectangle.y + edgeg->rectangle.height < point[0].y+2) {
       edgeg->rectangle.height += abs(edgeg->rectangle.y - point[0].y) + 2;
     }
     if(edgeg->rectangle.y + edgeg->rectangle.height < point[2].y+2) {
       edgeg->rectangle.height += abs(edgeg->rectangle.y - point[2].y) + 2;
     }
    return;
} /* draw arrow */


static int drawTransformer(Widget gw)
/**************************************************************************\
\**************************************************************************/
{
  EdgeGadget edgeg = (EdgeGadget) gw;
     int xmid, ymid;
     int xout, yout, xpt, ypt;
     int dy,dx;
     double theta;
     int itheta;
     /* struct { short x,y; } point[6]; */
     XPoint point[6];
     double pct = 0.3;
     int itlen;
     
     XSetLineAttributes( XtDisplay(gw),
		      edgeg->edge.drawGC, 
	 XiDefaultLineThickness,
         LineSolid , CapProjecting, JoinMiter);

     xmid = (int)edgeg->edge.x1 + 
            (int)(((int)edgeg->edge.x2 - (int)edgeg->edge.x1)*pct);
     if(xmid < 0) {
       printf("programmer error: EdgeG: xmid = %d reset to 0\n", xmid);
       xmid = 0;
     }
     ymid = (int)edgeg->edge.y1 + 
            (int)(((int)edgeg->edge.y2 - (int)edgeg->edge.y1)*pct);
     if(ymid < 0) {
       printf("programmer error: EdgeG: ymid = %d reset to 0\n", ymid);
       ymid = 0;
     }
     dx = (int)(edgeg->edge.x2 - edgeg->edge.x1);
     dy = (int)(edgeg->edge.y2 - edgeg->edge.y1);
     if(0 == dy) dy = 1;

#define TLEN 6
#define TANGLE 45
     if(TRUE == edgeg->edge.arrowDirection ) {
       itlen = -TLEN;
      } else {
       itlen = TLEN;
      }
     theta = (double)( atan2((double)-dy, (double)dx) * RADIAN);
     itheta = (int)(theta + 0.5);

     getAngleDistancePosition(xmid, ymid, itheta, (int)itlen, &xpt, &ypt);
     point[2].x = (short)(xpt);
     point[2].y = (short)(ypt);
     getAngleDistancePosition(xpt,ypt,itheta + TANGLE, itlen, &xout, &yout);
     point[1].x = (short)(xout);
     point[1].y = (short)(yout);
     getAngleDistancePosition(xout,yout, itheta+180-TANGLE, itlen, &xout, &yout);
     point[0].x = (short)(xout);
     point[0].y = (short)(yout);
     getAngleDistancePosition(xpt,ypt, itheta-TANGLE, itlen, &xout, &yout);
     point[3].x = (short)(xout);
     point[3].y = (short)(yout);
     getAngleDistancePosition(xout,yout, itheta+180+TANGLE, itlen, &xout, &yout);
     point[4].x = (short)(xout);
     point[4].y = (short)(yout);
     point[5].x = (short)(0);
     point[5].y = (short)(0);
     XDrawLines( XtDisplay(gw), XtWindow(gw), 
		      edgeg->edge.drawGC, 
                 point, 5, 0);

   /** now adjust the bounding box for the arrow **/
     if(edgeg->rectangle.x+2 > point[0].x) {
       edgeg->rectangle.width += abs(edgeg->rectangle.x - point[0].x) + 4;
       edgeg->rectangle.x = point[0].x - 2;
     } 
     if(edgeg->rectangle.x+2 > point[4].x) {
       edgeg->rectangle.width += abs(edgeg->rectangle.x - point[4].x) + 4;
       edgeg->rectangle.x = point[4].x - 2;
     } 
     if(edgeg->rectangle.x + edgeg->rectangle.width < point[0].x+2) {
       edgeg->rectangle.width += abs(edgeg->rectangle.x - point[0].x) + 2;
     }
     if(edgeg->rectangle.x + edgeg->rectangle.width < point[4].x+2) {
       edgeg->rectangle.width += abs(edgeg->rectangle.x - point[4].x) + 2;
     }
     if(edgeg->rectangle.y+2 > point[0].y) {
       edgeg->rectangle.height += abs(edgeg->rectangle.y - point[0].y) + 4;
       edgeg->rectangle.y = point[0].y - 2;
     } 
     if(edgeg->rectangle.y+2 > point[4].y) {
       edgeg->rectangle.height += abs(edgeg->rectangle.y - point[4].y) + 4;
       edgeg->rectangle.y = point[4].y - 2;
     } 
     if(edgeg->rectangle.y + edgeg->rectangle.height < point[0].y+2) {
       edgeg->rectangle.height += abs(edgeg->rectangle.y - point[0].y) + 2;
     }
     if(edgeg->rectangle.y + edgeg->rectangle.height < point[4].y+2) {
       edgeg->rectangle.height += abs(edgeg->rectangle.y - point[4].y) + 2;
     }
    return;
} /* drawTransformer */


/***************************************************************************
* drawTExt
*
**************************************************************************
*/
static int drawText(Widget wid)
{
  EdgeGadget edgeg = (EdgeGadget) wid;
  XRectangle clip;

    if(debug_flag == TRUE) 
      printf("drawText: try to draw the text \n");

/*** sorry clipping not working
    clip.x = edgeg->rectangle.x;
    clip.x = edgeg->rectangle.y;
    clip.width = edgeg->rectangle.width;
    clip.height = edgeg->rectangle.height;
    XSetClipRectangles(XtDisplay(XtParent(wid)), edgeg->edge.drawGC,
      0, 0, &clip, 1, Unsorted);
***/

    XmStringDraw (XtDisplay(XtParent(wid)), XtWindow(XtParent(wid)),
                  edgeg->edge.labelFontList, edgeg->edge.label, 
                  edgeg->edge.drawGC,
                  edgeg->rectangle.x, 
                  edgeg->rectangle.y + (Dimension)(edgeg->rectangle.height/2),
                  edgeg->rectangle.width,
                  XmALIGNMENT_CENTER, 
                  XmSTRING_DIRECTION_L_TO_R, NULL);

/***
    XSetClipMask (XtDisplay (wid), edgeg->edge.drawGC, None);
***/

} /* end drawText */




static Boolean 
#ifdef _NO_PROTO
SetValues( gadget, request, new, args, num_args) 
  Widget gadget;
  Widget request;
  Widget new;
  ArgList args;
  Cardinal *num_args; 
#else
SetValues(
  Widget gadget,
  Widget request,
  Widget new,
  ArgList args,
  Cardinal *num_args) 
#endif /* _NO_PROTO */
/******************************************************************************\
* XtSetValuesFunc
* returns TRUE if redisplay needed
* called whenever application call XtSetValues to set gadget resources
* method recalculates private instance variables based on the
* new public instance variable values.
* similar to initialize but called at different and multiple times
* see X Toolkit Intrinsics Programming Manual V4 ch. 6
*******************************************************************************/
{
    EdgeGadget edgeg = (EdgeGadget)new;
    EdgeGadget cur_edgeg = (EdgeGadget)gadget;

    /*** set the gadget x,y,width,height and orientation ***/
    /*** based on the x1,y1 and x2,y2 set for the gadget ***/
    /*** RX(gw), RY(gw) + RHeight(gw), RWidth(gw) ***/
#ifdef OLDWAY 
    /* the user is now responsible for setting x,y, width & height */
    if(edgeg->edge.x1 < edgeg->edge.x2) {
        RX(new) = edgeg->edge.x1 + (int)(edgeg->edge.xsize/2);
    } else {
        RX(new) = edgeg->edge.x2 + (int)(edgeg->edge.xsize/2);
    };

    if(edgeg->edge.y1 < edgeg->edge.y2) {
        RY(new) = edgeg->edge.y1 + (int)(edgeg->edge.ysize/2);
    } else {
        RY(new) = edgeg->edge.y2 + (int)(edgeg->edge.ysize/2);
    };
    if(((edgeg->edge.x1 < edgeg->edge.x2)&&
        (edgeg->edge.y1 < edgeg->edge.y2)) ||
        ((edgeg->edge.x1 >= edgeg->edge.x2)&&
        (edgeg->edge.y1 >= edgeg->edge.y2)) )
    {
        edgeg->edge.orientation = 0;
    } else {
        edgeg->edge.orientation = 1;
    }
    /* printf("Bustest edge_shape = %d\n", edge_shape); */
    RWidth(new) = abs(edgeg->edge.x1 - edgeg->edge.x2);
    RHeight(new) = abs(edgeg->edge.y1 - edgeg->edge.y2);
#endif

  /** check for a change in the label */
  if( cur_edgeg->edge.label != edgeg->edge.label) {
    InitializeText(new);
  }
  
  Resize((Widget)new);

  return(TRUE);
} /* end SetValues */


static Boolean SetValuesHook (Widget gadget, ArgList args, Cardinal *num_args)
/******************************************************************************\
* XtArgsFunc
* Set resource values in subparts.
* This method is used only in widgets that have subparts (see sec 9.4)
* downward chained
* see X Toolkit Intrinsics Programming Manual V4 ch. 9
*******************************************************************************/
{
/***
  printf("Edge> SetValuesHook gadget routine\n");
  Redisplay(gadget, 0, 0);
***/
} /* end SetValuesHook */



static void SetValuesAlmost (Widget gadget, Widget new_gadget,
                     XtWidgetGeometry *request, XtWidgetGeometry *reply)
/******************************************************************************\
* SetValuesAlmost
* XtAlmostProc
*******************************************************************************/
{
  EdgeGadget pnew = (EdgeGadget) new_gadget ;

  if(True == debug_flag) {
    printf("Edge> SetValuesAlmost gadget routine\n");
    printf(" Geometry pos x,y (%d,%d) width x height (%d x %d)\n",
      request->x, request->y, request->width, request->height);
    printf(" Geometry mask %d sibling %d stack mode %d\n",
      request->request_mode, request->sibling, request->stack_mode);
    printf(" stack mode: above,below,topif,bottomif,opposite,dontchange\n");
  }

  Resize((Widget)new_gadget);

  *request = *reply;
  
} /* end SetValuesHook */


static Boolean GetValuesHook (Widget gadget, ArgList args, Cardinal *num_args)
/********************************************************************
* XtArgsProc
* Return resources of subparts
* downward chained
* see X Toolkit Intrinsics Programming Manual V4 ch. 9
**********************************************************************/
{
/***
  printf("Edge> GetValuesHook gadget routine\n");
  Redisplay(gadget, 0, 0);
***/
} /* end GetValuesHook */


static XtGeometryResult QueryGeometry(Widget gadget, 
                     XtWidgetGeometry *request, XtWidgetGeometry *reply)
/******************************************************************************\
* QueryGeometry
* XtGeometryHandler
*******************************************************************************/
{
/***
  printf("Edge> QueryGeometry gadget routine\n");
  Redisplay(gadget, 0, 0);
***/
} /* end SetValuesHook */



/*** Gadget Class methods	***/
static void border_highlight(Widget gw)
/********************************************************************
* XtWidgetProc
**********************************************************************/
{
  /* printf("Edge> border_highlight gadget \n"); */

} /* end border_highlight */



static void border_unhighlight(Widget gw)
/********************************************************************
* XtWidgetProc
**********************************************************************/
{
  printf("Edge> border_unhighlight gadget disabled \n");
} /* end border_unhighlight */

static void arm_and_activate()
/********************************************************************
* arm_and_activate 
* XtProc no parameters
**********************************************************************/
{
  printf("Edge> arm_and_activate gadget routine disabled\n");
} /* end arm_and_activate */

/********************************************************************
*   purpose: invoke the drawInput callback procedures
*
**********************************************************************/
static void 
call_input_procs(Widget gw, XEvent *event, XtPointer call_data )
{
  XtCallbackStatus xtstat;
  EdgeGadget edgeg = (EdgeGadget)gw;
  XiEdgeCallbackStruct call_value;

  if( debug_flag == True) { 
    printf("Edge> call_input_procs gadget # %d\n", gw); 
    printf("Edge> redisplay callback edge.inputCallback %d\n",
        ((EdgeGadget) gw)->edge.inputCallback);

    xtstat = XtHasCallbacks(gw, XiNinputCallback);
    if(XtCallbackNoList == xtstat) {
      printf("  XtCallbackNoList for XiN %s\n", XiNinputCallback);
    }
    if(XtCallbackHasNone == xtstat) {
      printf("  XtCallbackHasNone for XiN %s\n", XiNinputCallback);
    }
    if(XtCallbackHasSome == xtstat) {
      printf("  XtCallbackHasSome for XiN %s\n", XiNinputCallback);
     
    }
  }

    if ( edgeg->edge.inputCallback ) {
      XFlush(XtDisplay(gw));
      call_value.reason = event->type;
      call_value.event = event;
      XtCallCallbackList(gw, edgeg->edge.inputCallback, &call_value);
    }
/***
*   if ( edgeg->edge.inputCallback ) {
*     XtCallbackProc fptr; 
*
*     XFlush(XtDisplay(gw));
*     call_value.reason = event->type;
*     call_value.event = event;
*     fptr = edgeg->edge.inputCallback->callback; 
*    (fptr)((Widget)gw, (XtPointer)edgeg->gadget.user_data, &call_value);
*   }
***/
}

static void 
input_dispatch(Widget gw, XEvent *event, Mask call_data)
/******************************************************************************\
* XtWidgetProc
* NOTE - listed as an XtWidgetProc but this is probably wrong
* since it returns events it is probably an XtExposeProc
* this means that the third parameter is Region data
* method for handling keyboard and mouse input events
* author: Dan Clark  Sep 09, 1991
*******************************************************************************/
{
  EdgeGadget edgeg = (EdgeGadget)gw;
  Mask event_mask = (Mask)call_data;
  XiEdgeCallbackStruct call_value;

   if(True == debug_flag) {
     printf("Edge> input_dispatch gadget # %d\n", gw); 
   }
    
    switch(event->type)  
    {
      case ButtonPress:
        if(True == debug_flag) {
          printf("Edge gadget ButtonPress call inputCallback \n");  
        }
        call_input_procs(gw, event, (XtPointer)call_data ); 
        /*   Arm(gw, event);  */
      break;

      case ButtonRelease:
        if(True == debug_flag) {
          printf("Edge gadget ButtonRelease call inputCallback \n");  
        }
        call_input_procs(gw, event, (XtPointer)call_data );  
        /* Activate(gw, event);  */
      break;

      case MotionNotify:
        /* printf("Edge gadget MotionNotify \n"); */
      break;

      case EnterNotify:
        /* printf("Edge gadget EnterNotify \n"); */
      break;

      case LeaveNotify:
        /* printf("Edge gadget LeaveNotify \n"); */
      break;

      case FocusIn:
        /* printf("Edge gadget FocusIn \n"); */
      break;

      case FocusOut:
        /* printf("Edge gadget FocusOut \n"); */
      break;

      default:
        printf("Warning Edge gadget does not handle Event = %d\n", event->type);
      break;
    };

/***
  printf("Edge> input_dispatch window id = %d\n", XtWindow(gw));
  printf("Edge> input_dispatch parent window id = %d\n", XtWindow(XtParent(gw)));
***/
} /* end input_dispatch */

static void Arm (pb, event)
Widget pb;
XEvent *event;
{
  printf("EdgeG> Arm disabled\n");
}


static void Activate (pb, event)
Widget pb;
XEvent * event;
{
  printf("EdgeG> Activate disabled\n");
}


static Boolean 
visual_change(Widget child, Widget current, Widget new)
/*****************************************************************************\
* XmVisualChangeProc
******************************************************************************/
{
  XmGadget pc = (XmGadget)child;
  XmManagerWidget pmwc = (XmManagerWidget) current;
  XmManagerWidget pmwn = (XmManagerWidget) new;
  printf("Edge> visual_change gadget \n");
}

static void set_override()
/********************************************************************
* XtProc no parameters
**********************************************************************/
{
  printf("Edge> set_override gadget\n");
}

static void set_which_button(Widget pb)
/********************************************************************
* XtProc no parameters
**********************************************************************/
{
  printf("Edge> set_which_button gadget \n");
} /* end set_which_button */


/******************************************************************************\
* PrintEdgeRec
* print many but not all of the fields in the edge record.
* to determine all the fields must reference the header files for 
* the particular object.
* ObjectPart	- /usr/include/X11/ObjectP.h
* RectObjPart	- /usr/include/X11/RectObjP.h
* XmGadgetPart	- /usr/include/Xm/GadgetP.h or /usr/include/Xm/XmP.h  
*
* Known Bugs:
* since the callback structures are linked lists 
* only the pointer to the first callback structure is printed 
*******************************************************************************/
static void PrintEdgeRec(Widget gw)
{
  printf("Edge> ObjectPart data => (EdgeGadget) gw)->object.field \n");
  printf("Edge> ptr to itself (Widget) (EdgeGadget) gw->object.self %d\n",
        ((EdgeGadget) gw)->object.self);
  printf("Edge> ptr to class (WidgetClass) object.widget_class %d\n",
        ((EdgeGadget) gw)->object.widget_class);
  printf("Edge> parent (Widget) object.parent %d\n",
        ((EdgeGadget) gw)->object.parent);
  printf("Edge> resource name quark (XrmName) object.xrm_name %d\n",
        ((EdgeGadget) gw)->object.xrm_name);
  if(NULL != XrmQuarkToString(((EdgeGadget)gw)->object.xrm_name) ) {
    printf("Edge> resource name quark string (XrmName) object.xrm_name %s\n",
      XrmQuarkToString(((EdgeGadget)gw)->object.xrm_name));
  }  
  printf("Edge> marked for destroy (Boolean) object.being_destroyed %d\n",
        ((EdgeGadget) gw)->object.being_destroyed);
  printf("Edge> destry callbacks (XtCallbackList) object.destroy_callbacks %d\n",
        ((EdgeGadget) gw)->object.destroy_callbacks);

  printf("Edge> constraint record (caddr_t) object.constraints %d\n",
        ((EdgeGadget) gw)->object.constraints);

  printf("Edge> RectObjectPart data => (EdgeGadget) gw)->rectangle.field \n");
  printf("Edge> rectangle position (Position) rectangle.x, y  %d,%d\n",
        ((EdgeGadget) gw)->rectangle.x, ((EdgeGadget) gw)->rectangle.y);
  printf("Edge> x1,y1 x x2,y2 position (Position) %d,%d x %d,%d\n",
        ((EdgeGadget) gw)->edge.x1, ((EdgeGadget) gw)->edge.y1,
        ((EdgeGadget) gw)->edge.x2, ((EdgeGadget) gw)->edge.y2);

  printf("Edge> XmGadgetPart (XmP.h) => (EdgeGadget) gw)->gadget.field \n");
  printf("Edge> (short) gadget.shadow_thickness  %d\n",
        ((EdgeGadget) gw)->gadget.shadow_thickness);
  printf("Edge> (short) gadget.highlight_thickness %d\n",
        ((EdgeGadget) gw)->gadget.highlight_thickness);
  printf("Edge> (Boolean) gadget.traversal_on %d\n",
        ((EdgeGadget) gw)->gadget.traversal_on);
  printf("Edge> (Boolean) gadget.highlight_on_enter %d\n",
        ((EdgeGadget) gw)->gadget.highlight_on_enter);
  printf("Edge> (Boolean) gadget.have_traversal %d\n",
        ((EdgeGadget) gw)->gadget.have_traversal);
  printf("Edge> (Mask) gadget.event_mask %d\n",
        ((EdgeGadget) gw)->gadget.event_mask);

} 
/* end PrintEdgeRec */


/******************************************************************************\
* PrintEdgeClassRec
* print some of the class field records.
* the class data structure is initialized in the Edge.c file for the
* edge gadget.  The field names are also derived from the header
* files for each object type (see the function PrintEdgeRec).
* Only the RectObjClassPart is specified since the
* core class, object class, and rectangle object class record all
* share the same area of storage and data layout.
*******************************************************************************/
static void PrintEdgeClassRec()
{
  printf("Edge> RectObjClassPart => edgeClassRec.rect_class.field\n");
  printf("Edge> (String) rect_class.class_name %s\n",
         edgeClassRec.rect_class.class_name);
  printf("Edge> (Cardinal) rect_class.widget_size %d\n",
         edgeClassRec.rect_class.widget_size);
  printf("Edge> (XtVersionType) rect_class.version %d\n",
         edgeClassRec.rect_class.version);

  printf("Edge> XmGadgetClassPart => edgeClassRec.gadget_class.field \n");
  printf("Edge> (XtProc) gadget_class.arm_and_activate %d\n",
         edgeClassRec.gadget_class.arm_and_activate);

} 
/* end PrintEdgeClassRec */

/*** NO ACTIONS in Gadget	***/
/* end of EdgeG.c */
