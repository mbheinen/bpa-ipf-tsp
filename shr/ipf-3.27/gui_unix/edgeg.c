static char SCCSID_edgec[] = "@(#)edgeg.c	20.48 4/30/98";
/*
*           IPF V2.x: Interactive PowerFlow Version 2.x
*              Bonneville Power Administration (BPA)
*         Authors:  D. L. Clark, J. G. Coleman, W. L. Powell, 
*                B. E. Rogers, K. E. Rowell, J. L. Rutis, D. M. Stefonik, 
*                D. M. Syzmanski 
*
*                              NOTICE
*
* Interactive Powerflow (IPF) was developed by BPA and its contractors
* with about 20% of the cost supported by the Electric Power Research
* Institute (EPRI).  By mutual agreement, as described in EPRI 
* Agreement RP2746-03 entitled Graphical User Interface for 
* Powerflow, March, 1992, all results of this project--including 
* the computer program and its documentation--are to be in the 
* public domain.  In a separate Memorandum of Understanding with the 
* Western Systems Coordinating Council (WSCC), BPA agreed in March, 1992, 
* to keep WSCC informed of progress, to make its best effort to develop 
* the program according to the Guidelines adopted by the WSCC Computer 
* Program Management Subcommittee, and to make the final results 
* available for possible further development by WSCC. 
*
* This notice must appear in all copies of this software and documentation.
*/

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
* but the functi nality for standards like drag-and-drop have been ommitted
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
#ifndef IPF_MOTIF_1_1
#include <Xm/ManagerP.h>  /* can be used with MOTIF 1.2 and later */
#endif
#include "EdgeGP.h"   /** only the widget definition code has private header **/
#include <math.h>
#include <stdio.h>	/*** for debugging only ***/
#include <stdlib.h>	
#include <string.h>	
#ifdef __STDC__
#define IPFPROTO(x) x
#else
#define IPFPROTO(x) ()
#endif

#define max(A,B) ((A) > (B) ? (A) : (B))
#define min(A,B) ((A) < (B) ? (A) : (B))
#define RADIAN  57.2957795
#define LINE_WIDTH_DEFAULT 2
#define LTR_SIZE	0.8
#define DRAW_BORDER 15	     /* NOTE: must match SCRN_BORDER in graph_data.h */
#define LEN 10		/* length of arrow symbol */


/*** define resource list 	***/
#define res_offset(field)    XtOffset(EdgeGadget,edge.field)
#define goffset(field)	     XtOffset(EdgeGadget,rectangle.field)
#define gadget_offset(field) XtOffset(EdgeGadget,gadget.field)
 
   /* from (toolbox.c) */
/* static double zoom_factor = (double)1.0; */  

static XtResource resources[] = {
	{ XiNx1, XiCX1, XmRDimension, sizeof(Dimension),
	res_offset(x1), XmRImmediate, (XtPointer) XiDefaultX1 },
	{ XiNy1, XiCY1, XmRImmediate, sizeof(Dimension),
	res_offset(y1), XmRString, (XtPointer) XiDefaultY1 },
	{ XiNx2, XiCX2, XmRImmediate, sizeof(Dimension),
	res_offset(x2), XmRString, (XtPointer) XiDefaultX2 },
	{ XiNy2, XiCY2, XmRImmediate, sizeof(Dimension),
	res_offset(y2), XmRString, (XtPointer) XiDefaultY2 },
	{ XiNzoomFactor, XiCZoomFactor, XmRImmediate, sizeof(Dimension),
	res_offset(zoom), XmRString, (XtPointer) XiDefaultZoomFactor },
	{ XiNdashOn, XiCDashOn, XmRBoolean, sizeof(Boolean),
	res_offset(dashOn), XmRImmediate, (XtPointer) FALSE},
	{ XiNboxOn, XiCBoxOn, XmRBoolean, sizeof(Boolean),
	res_offset(boxOn), XmRImmediate, (XtPointer) FALSE},
	{ XiNsymbolSegment, XiCSymbolSegment, XmRBoolean, sizeof(Boolean),
	res_offset(symbolSegment), XmRImmediate, (XtPointer) FALSE},
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
	{ XiNlabelType, XiCLabelType, XmRBoolean, sizeof(Boolean),
	res_offset(labelType), XmRImmediate, (XtPointer) FALSE},
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
static int edge_motif2cstring IPFPROTO((XmString xmstr, char ** cstr ));
static void ClassInitialize IPFPROTO(( void ));
static void ClassPartInitialize IPFPROTO(( WidgetClass widget_class));
static void InitializeHook IPFPROTO(( Widget req, Widget new_w, ArgList args,
                        Cardinal *num_args));
static void border_highlight IPFPROTO((Widget wid));
static void border_unhighlight IPFPROTO((Widget wid));

/*** function declarations for XmGadget part ***/
static void Arm  IPFPROTO((Widget pb, XEvent *event));
static void Activate  IPFPROTO((Widget pb, XEvent *event));
static void setDash IPFPROTO((Widget wid));
static void arm_and_activate IPFPROTO((void));
static void PrintEdgeRec IPFPROTO((Widget gw));
static void PrintEdgeClassRec IPFPROTO((void));
static Boolean SetValuesHook  IPFPROTO((Widget gadget, ArgList args, Cardinal *num_args));
static Boolean visual_change IPFPROTO((Widget child, Widget current, Widget new));
static void input_dispatch IPFPROTO((Widget gw, XEvent *event, Mask call_data));
static Boolean SetValuesPrehook IPFPROTO(( Widget oldParent, Widget refParent,
                        Widget newParent, ArgList args, Cardinal *num_args)) ;
static void GetValuesPrehook IPFPROTO(( Widget newParent, ArgList args,
                        Cardinal *num_args)) ;
static void GetValuesPosthook IPFPROTO(( Widget new_w, ArgList args, Cardinal *num_args));
static Boolean SetValuesPosthook IPFPROTO(( Widget current, Widget req, Widget new_w, 
                        ArgList args, Cardinal *num_args)) ;
static void Resize IPFPROTO(( Widget wid)) ;
static void Initialize IPFPROTO(( Widget req, Widget new_w,
                        ArgList args, Cardinal *num_args)) ;
static XtGeometryResult QueryGeometry IPFPROTO(( Widget wid, XtWidgetGeometry *intended,
                        XtWidgetGeometry *reply)) ;
static void Destroy IPFPROTO(( Widget w)) ;
static void Redisplay IPFPROTO(( Widget wid, XEvent *event, Region region)) ;
static Boolean SetValues IPFPROTO(( Widget cw, Widget rw, Widget nw,
                        ArgList args, Cardinal *num_args)) ;
static void SetValuesAlmost IPFPROTO(( Widget cw, Widget nw,
                        XtWidgetGeometry *request, XtWidgetGeometry *reply)) ;
/*** following functions are private to Edge	***/
static int drawArrow IPFPROTO	((Widget gw, int alen ));
static int drawText IPFPROTO	((Widget gw));
static int drawTransformer IPFPROTO((Widget gw, int alen ));
static void get_edge_attributtes( Widget gw, int *xmid, int *ymid, double *ang);
static void adj_bounding_box	( Widget wid, int x, int y );

static void drawChars		( Widget wid, char *string, int x, int y,
		                   double size );
int longest_segment		( char *str );
static void drawCharsCenteredAboveLine( Widget wid, char *string,
		                 int x1, int y1, int x2, int y2, double size );
static void setStringBoundary( Widget wid, char *string, double size );
static int findCharID		( char c );
static void drawletter		( Widget w, double x, double y,
				  char c, double sixe );
void preposition_str		( Widget wid, char *str, double size );
static void ck_four_corners	( Widget wid, int x, int y, int len );




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
    (XtSetValuesFunc)SetValues, /* 2 set_values of subclass resource */
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

externaldef(edgegadgetclass) WidgetClass edgeGadgetClass = (WidgetClass) &edgeClassRec; 

/*** section of shared data local to edge widget ***/
/*** temporary shared variables ***/

static int     	edge_gadget_cnt = 0;
static Boolean 	debug_flag = False;
static double  	xs, ys, scr, ssr;


/*** 
*** this procedure is only included to ensure that some
*** linkers (such as the VAX VMS LINK ) pull in this
*** object from the object library 
*** should become obsolete with the use of externaldef and externalref
***/
int
#ifdef _NO_PROTO
EdgeGCheckClass ( )
#else
EdgeGCheckClass ( void )
#endif
{
  printf("EdgeG> WARNING: EdgeGCheckClass should not be called\n");
  if(edgeGadgetClass != (WidgetClass) &edgeClassRec) {
    printf("EdgeG> set edgeGadgetClass\n");
    edgeGadgetClass = (WidgetClass) &edgeClassRec;
  }
  return(0);
}
/* --------------------- Gadget Object methods	--------------------------
* XtProc takes no arguments
* called once - first time an instance of a class is created by appl
* type converters
* see sec 9.4 X toolkit intrinsics V4
* -------------------------------------------------------------------------
*/




static void
#ifdef _NO_PROTO
ClassInitialize()
#else
ClassInitialize( void )
#endif /* _NO_PROTO */
/*************************************************************************
* initialize the fields for the class data structure 
*
\*************************************************************************/
{

  if(True == debug_flag )
  {
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
* XtWidgetClassProc
* called once - first time an instance of a class is created by appl
* same as ClassInitialize but downward chained.
* resolves inheritance of self sufficient methods from the immediate superclass
* Needed only in classes that define their own methods in their class part
* see ch 13 X toolkit intrinsics V4
*******************************************************************************/
{
    if(True == debug_flag ) {
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
  Widget ppw; /* pointer to the parent widget */
  XFontStruct     *fs = (XFontStruct *) NULL;
  XmFontList fontlist;
  Boolean rtn;
  XmFontContext fcontext;
  XmStringCharSet fcharset;

#ifdef IPF_MANAGER_FG
  XmManagerWidget pmw; 
    pmw = (XmManagerWidget) XtParent((Widget)peg);
#endif
    ppw = (Widget)XtParent((Widget)peg);

    if(peg->edge.drawPixel != (Pixel)0) {
      drawGCMask = GCForeground | GCBackground | GCLineWidth; 
      drawGCValues.foreground = peg->edge.drawPixel;
      drawGCValues.background = peg->edge.drawPixel;
    } else {
      drawGCMask = GCForeground | GCBackground | GCLineWidth |
                   GCGraphicsExposures | GCClipMask; 

#ifdef IPF_MANAGER_FG
      drawGCValues.foreground = pmw->manager.foreground;
      drawGCValues.background = pmw->core.background_pixel;
#else
      drawGCValues.foreground = ppw->core.border_pixel;
      drawGCValues.background = ppw->core.background_pixel;
#endif
      drawGCValues.graphics_exposures = FALSE;
      drawGCValues.clip_mask = None;
    }
    drawGCValues.line_width = peg->edge.lineThickness;

    peg->edge.drawGC = XtGetGC((Widget)peg, drawGCMask, &drawGCValues);

    if(True == debug_flag ) {
      printf("Edge: defineDrawGC: drawPixmap %d drawPixel %d\n",
         peg->edge.drawPixmap, 
         peg->edge.drawPixel );
    };

} /* end defineDrawGC */



static void
#ifdef _NO_PROTO
defineTextGC( peg )
  EdgeGadget peg;
#else
defineTextGC(
  EdgeGadget peg )
#endif /* _NO_PROTO */
/**************************************************************************\
* define the graphics context for drawing lines
* derived from MOTIF source code Version 1.1
\**************************************************************************/
{
  XGCValues textGCValues;
  XtGCMask textGCMask; 
  Widget ppw; /* pointer to the parent widget */
  XFontStruct     *fs = (XFontStruct *) NULL;
  XmFontList fontlist;
  Boolean rtn;
  XmFontContext fcontext;
  XmStringCharSet fcharset;

#ifdef IPF_MANAGER_FG
  XmManagerWidget pmw; 
    pmw = (XmManagerWidget) XtParent((Widget)peg);
#endif
    ppw = (Widget)XtParent((Widget)peg);

      textGCMask = GCForeground | GCBackground | GCLineWidth |
                   GCGraphicsExposures | GCClipMask; 

#ifdef IPF_MANAGER_FG
      textGCValues.foreground = pmw->manager.foreground;
      textGCValues.background = pmw->core.background_pixel;
#else
      textGCValues.foreground = ppw->core.border_pixel;
      textGCValues.background = ppw->core.background_pixel;
#endif
      textGCValues.graphics_exposures = FALSE;
      textGCValues.clip_mask = None;
      if( (int)( (double)peg->edge.zoom/(double)100.0 ) < 2 )
      {
        textGCValues.line_width = 1;
      }
      else
      {
        textGCValues.line_width = 2;
      }
/*      textGCValues.line_width = LINE_WIDTH_DEFAULT;*/

#ifndef NO_DEFAULTFONT
#ifdef USE_XIDEFAULTFONT
    fs = XLoadQueryFont(XtDisplay((Widget)peg),  XiDefaultLabelFontString);
    if(NULL == fs) {
      printf("EdgeG> defineDrawGC Error XLoadQueryFont returned NULL\n");
    }
#else
    if(peg->edge.labelFontList == NULL) {
    fs = XLoadQueryFont(XtDisplay((Widget)peg),  XiDefaultLabelFontString);
    if(NULL == fs) {
      printf("EdgeG> defineDrawGC Error XLoadQueryFont returned NULL\n");
    }
    } else {
    rtn = XmFontListInitFontContext( &fcontext, peg->edge.labelFontList);
    if(rtn != TRUE) {
      printf("EdgeG> InitializeText Error XmFontListInitFontContext failed \n");
    }
    rtn = XmFontListGetNextFont(fcontext, &fcharset, &fs);
    if(rtn != TRUE) {
      printf("EdgeG> InitializeText Error XmFontListGetNextFont failed \n");
    }
    XmFontListFreeFontContext(fcontext);
    }
#endif
#else /* not NO_DEFAULTFONT */
      _XmFontListGetDefaultFont(peg->edge.labelFontList, &fs); 
    if(NULL == fs) {
      printf("EdgeG> InitializeText Error _XmFontListGetDefaultFont returned NULL\n");
    } 
#endif /* not NO_DEFAULTFONT */
    /* XmFontListFree(fontlist); */
    if (fs==NULL) {
      printf("EdgeG> ERROR - no font for graphics context\n");
      textGCMask &= ~GCFont;
    } else {
      fontlist = XmFontListCreate(fs, XmSTRING_DEFAULT_CHARSET);
      textGCMask |= GCFont;
      textGCValues.font       = fs->fid;
    }

    peg->edge.textGC = XtGetGC((Widget)peg, textGCMask, &textGCValues);

} /* end defineTextGC */


InitializeText(Widget wid)
/********************************************************************\
*
*  setup the XmString
*********************************************************************/
{
  EdgeGadget gw = (EdgeGadget) wid;
  Dimension w, h;
  XmFontList defaultFont;

  defineTextGC(gw);
  if (gw->edge.labelFontList == NULL)
  {
      if(True == debug_flag ) {
         printf("labelFontList NULL so create a label font\n");
      }
      defaultFont = _XmGetDefaultFontList( (Widget) gw, XmLABEL_FONTLIST);
      gw->edge.labelFontList = XmFontListCopy (defaultFont);
    } else {
     /* Make a local copy of the font list */
      if(True == debug_flag ) {
        printf(" initialize make a copy of the font list\n");
      }
      gw->edge.labelFontList = XmFontListCopy( gw->edge.labelFontList );
  }

  if (gw->edge.label == NULL)
  {
    if(True == debug_flag ) 
      printf("create a label from the name of the widget \n");
	gw->edge.label =  XmStringCreate(XrmQuarkToString(gw->object.xrm_name), 
        XmSTRING_DEFAULT_CHARSET);
  }
  else
  { 
    if(True == debug_flag ) {
      printf(" label is xm string so copy it over \n");
    }
    gw->edge.label = XmStringCopy( (XmString)gw->edge.label );
  }
  gw->edge.minx = (Dimension)1999;
  gw->edge.miny = (Dimension)1999;
  gw->edge.maxx = (Dimension)0;
  gw->edge.maxy = (Dimension)0;
}

#ifdef IPF_TEXT_FONT_OPTION
/**************************************************************
*
*  setup the XmString
****************************************************************
*/
InitializeText_old(Widget wid)
{
  EdgeGadget gw = (EdgeGadget) wid;
  Dimension w, h;
#ifndef IPF_MOTIF_1_1
  XmFontListEntry entry1, entry2;
  XmFontList defaultFont;
#endif

#ifndef IPF_MOTIF_1_1
    if (gw->edge.labelFontList == NULL) {
      if(True == debug_flag ) {
         printf("labelFontList NULL so create a label font\n");
      }
#ifdef FONT_LOAD
      entry1 = XmFontListEntryLoad(XtDisplay(XtParent(wid)), 
                XiDefaultLabelFontString, XmFONT_IS_FONT, "TAG1");
      entry2 = XmFontListEntryLoad(XtDisplay(XtParent(wid)), 
               "courier14", XmFONT_IS_FONT, "TAG2");
      defaultFont = XmFontListAppendEntry(NULL, entry1);
      defaultFont = XmFontListAppendEntry(defaultFont, entry2);
#endif /* FONT_LOAD */
      defaultFont = _XmGetDefaultFontList( (Widget) gw, XmLABEL_FONTLIST);
      gw->edge.labelFontList = XmFontListCopy (defaultFont);
    } else {
     /* Make a local copy of the font list */
      if(True == debug_flag ) {
        printf(" initialize make a copy of the font list\n");
      }
      gw->edge.labelFontList = XmFontListCopy( gw->edge.labelFontList );
    }
#else /* IPF_MOTIF_1_1 */
    if (gw->edge.labelFontList == NULL) {
      XFontStruct font;
      XFontStruct *font1;
      XmFontList defaultFont;

      if(True == debug_flag )  {
         printf("labelFontList NULL so create a label font\n");
      }
      defaultFont = _XmGetDefaultFontList( (Widget) gw, XmLABEL_FONTLIST); 
      gw->edge.labelFontList = XmFontListCopy(defaultFont);

      font1 = XLoadQueryFont(XtDisplay((Widget)gw),  XiDefaultLabelFontString);
    if(NULL == font1) {
      printf("EdgeG> InitializeText Error XLoadQueryFont returned NULL\n");
    }
      _XmFontListGetDefaultFont(gw->edge.labelFontList, &font1); 
    if(NULL == font1) {
      printf("EdgeG> InitializeText Error XmFontListGetDefaultFont returned NULL\n");
    }
      defaultFont = XmFontListCreate( font1, XmSTRING_DEFAULT_CHARSET); 
      gw->edge.labelFontList = defaultFont;
/***
      gw->edge.labelFontList = XmFontListAdd(gw->edge.labelFontList, 
         &font, XmSTRING_DEFAULT_CHARSET);
***/
    }
#endif /* IPF_MOTIF_1_1 */

    if (gw->edge.label == NULL) {
      if(True == debug_flag ) 
        printf("create a label from the name of the widget \n");
#ifndef IPF_MOTIF_1_1
      gw->edge.label =  XmStringCreate(XrmQuarkToString(gw->object.xrm_name), "TAG1");
#else
      gw->edge.label =  XmStringCreate(XrmQuarkToString(gw->object.xrm_name), 
        XmSTRING_DEFAULT_CHARSET);
#endif
    } else { 
      if(True == debug_flag ) 
        printf(" label is xm string so copy it over \n");
      gw->edge.label = XmStringCopy( (XmString)gw->edge.label );
    }
}
#endif


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

    if(edgeg->edge.zoom >= 1 && edgeg->edge.zoom <= 1000) {
      /* zoom_factor = (double)edgeg->edge.zoom/100; */
    } else {
      /* zoom_factor = (double)1.0; */
      edgeg->edge.zoom = 100;
    }
    if(True == debug_flag ) {
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

    if(True == debug_flag ) {
      printf("Edge> (Mask) gadget.event_mask %d\n",
        ((EdgeGadget) new)->gadget.event_mask);
    }

    InitializeText(new);

    /* setup the graphics context for the gadget  */
    defineDrawGC(edgeg); /* must come after InitializeText */

    /*** make sure the gadget has a minimum size for display */
    Resize((Widget)new); 

    if(True == debug_flag ) {
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
    }

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
    if(True == debug_flag ) {
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

    if(True == debug_flag ) {
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
/*********************************************************************\
* Purpose: Checks size of a label gagdet, computes required size of
*	   gadget for compound string to fit inside.
*	   Results stored in edgeGadget wid.
* Author: Dan Clark 
\*********************************************************************/
{
  EdgeGadget edgeg = (EdgeGadget)wid;
  Dimension width = 0;
  Dimension height = 0;
  Dimension newx = 0;
  Dimension newy = 0;
  char *text;
  int lcnt;

  if( True == edgeg->edge.labelOn )
  {
   if(edgeg->edge.labelType == False)
   {

    lcnt = edge_motif2cstring(edgeg->edge.label, &text);
    setStringBoundary( wid, text, LTR_SIZE );

    newx  =edgeg->edge.minx - LINE_WIDTH_DEFAULT;
    newy  =edgeg->edge.miny - LINE_WIDTH_DEFAULT;
    width =edgeg->edge.maxx - edgeg->edge.minx + LINE_WIDTH_DEFAULT;
    height=edgeg->edge.maxy - edgeg->edge.miny + LINE_WIDTH_DEFAULT;

     /* set the x, y, width and height of gadget */
    if(edgeg->rectangle.x > (Position)newx) {
  	   edgeg->rectangle.width += abs(newx - (Dimension)edgeg->rectangle.x); 
  	   edgeg->rectangle.x = (Position)newx; }

    if(edgeg->rectangle.y > (Position)newy) {
  	edgeg->rectangle.height += abs(newy - (Dimension)edgeg->rectangle.y); 
     	edgeg->rectangle.y = (Position)newy; }

    if((Dimension)(edgeg->rectangle.x + edgeg->rectangle.width) < 
        (Dimension)(edgeg->edge.maxx+2))
  	 edgeg->rectangle.width = edgeg->edge.maxx+2 - (Dimension)edgeg->rectangle.x;

    if((Dimension)(edgeg->rectangle.y + edgeg->rectangle.height) < 
        (Dimension)(edgeg->edge.maxy+2))
  	 edgeg->rectangle.height = edgeg->edge.maxy+2 - (Dimension)edgeg->rectangle.y;
    free(text);
   }
   else
   {
       /** now make the box is big enough to fit the label **/
     XmStringExtent(edgeg->edge.labelFontList, edgeg->edge.label,
                  &width, &height);

        /** box must be big enough to contain text **/
        /* text is later written below line by half (see drawText) */
        /* so box height must be doubled */

     height *= 2;

     if(edgeg->rectangle.width < width)
   	edgeg->rectangle.width = width;
   
     if(edgeg->rectangle.height < height)
   	edgeg->rectangle.height = height;
   }
  } /* some type of string drawn above */
   
  if (edgeg->rectangle.width <= 0) /* set core width and height to a */
	edgeg->rectangle.width = 1;  /* default value so that it doesn't */

  if (edgeg->rectangle.height <= 0) /* a Toolkit Error */
	edgeg->rectangle.height = 1;
}


static void drawBox(Widget wid) 
{
  EdgeGadget edgeg = (EdgeGadget)wid;

  if( TRUE == edgeg->edge.boxOn ) {
     Position tmpx, tmpy;
     Dimension tmpwidth, tmpheight;
     tmpwidth = (Dimension)max((int)edgeg->rectangle.width - 2, 0);
     tmpheight = (Dimension)max((int)edgeg->rectangle.height - 2, 0);
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
  double angle;
  EdgeGadget edgeg = (EdgeGadget) wid;
  XtCallbackStatus xtstat;
  XiEdgeCallbackStruct call_value;
  char *text;
  XRectangle clip;
  int xlen, ylen, mlen;
  int lcnt;

  if( debug_flag == True)
  { 
    printf("Edge> Redisplay debug\n");
    PrintEdgeClassRec();
    PrintEdgeRec(wid); 
    printf("Edge> redisplay callback edge.drawCallback %d\n",
        ((EdgeGadget) wid)->edge.drawCallback->callback);
  };

  if ( edgeg->edge.drawCallback )
  {
    call_value.reason = event->type;
    call_value.event = event;
    call_value.window = XtWindow(wid);
    call_value.click_count = 0;
    call_value.region = region;
    XtCallCallbackList(wid, edgeg->edge.drawCallback, &call_value);
  }
  else
  {
 
      /** set line attributes with dash or nodash ***/

/***
* XSetLineAttributes( XtDisplay(wid),
*		      edgeg->edge.drawGC, 
*		      edgeg->edge.lineThickness, 
*		      LineSolid , CapProjecting, JoinBevel);
***/

/**** do use (5/20/96) w.rogers - line thickness is set by setkvcolor ****
    if( (int)( (double)edgeg->edge.zoom/(double)100.0 ) < 2 )
    {
      edgeg->edge.lineThickness = 1;
    }
    else
    {
      edgeg->edge.lineThickness = 2;
    }
*/
    setDash(wid);

    XDrawLine  ( XtDisplay(wid), XtWindow(wid), 
		 edgeg->edge.drawGC, 
		 edgeg->edge.x1, edgeg->edge.y1,
		 edgeg->edge.x2, edgeg->edge.y2);

    xlen = (int)abs( edgeg->edge.x2 - edgeg->edge.x1 );
    ylen = (int)abs( edgeg->edge.y2 - edgeg->edge.y1 );
    xlen = (int)(max( xlen, ylen ))/2;		 /* maximum size allowable */
						 /* normal size		   */
    mlen = (int)((double)LEN * (double)edgeg->edge.zoom/(double)100.0);

    mlen = (int)min( xlen, mlen );

    if( TRUE == edgeg->edge.arrowOn &&
	NULL != edgeg->edge.label   )  drawArrow      (wid, mlen);
    mlen = (int)(mlen/2);
    if( TRUE == edgeg->edge.transformerOn &&
        TRUE == edgeg->edge.symbolSegment  )  drawTransformer(wid, mlen);


    if(edgeg->edge.labelType == False)
    {
      if( True == edgeg->edge.labelOn )
      {
        if (( NULL != edgeg->edge.label ) )
	{
          lcnt = edge_motif2cstring(edgeg->edge.label, &text);

          drawCharsCenteredAboveLine( wid, text,
		  (int)edgeg->edge.x1, (int)edgeg->edge.y1,
		  (int)edgeg->edge.x2, (int)edgeg->edge.y2,
		  LTR_SIZE );

          free(text);

        }
      }
    }
    else
    {
      if( True == edgeg->edge.labelOn )
      {
        if ( ( NULL != edgeg->edge.labelFontList) && 
               ( NULL != edgeg->edge.label ) )
	{
          drawText(wid);
        }
      }
    }


    XSetLineAttributes( XtDisplay(wid),
		      edgeg->edge.drawGC, 
		      XiDefaultLineThickness,
		      LineSolid , CapProjecting, JoinBevel);

 } /* end of default drawing routines */

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
getAngleDistancePosition( int x, int y, double deg, int dist, int *x2, int *y2 )
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
         LineOnOffDash, CapProjecting, JoinBevel);
    } else {
      XSetLineAttributes( XtDisplay(gw),
		      edgeg->edge.drawGC, 
         edgeg->edge.lineThickness, 
         LineSolid , CapProjecting, /* CapNotLast, */ JoinBevel);
    }
} /* end setDash */


/* here you do something similar but do not use a new variable */
static int drawArrow(Widget gw, int alen)
/****************************************************************************\
* Purpose: draw the arrow symbol on the edge widget
*
\****************************************************************************/
{
#define ANGLE 15

  EdgeGadget edgeg = (EdgeGadget) gw;
  int xmid, ymid, xout, yout, xpt, ypt;
  double theta;
  XPoint point[4];
     
  XSetLineAttributes( XtDisplay(gw), edgeg->edge.drawGC, 
	 XiDefaultLineThickness, LineSolid , CapProjecting, JoinBevel);

  get_edge_attributtes( gw, &xmid, &ymid, &theta );

  if(TRUE == edgeg->edge.arrowDirection )
    alen = -alen;
 
  getAngleDistancePosition( xmid, ymid, theta, (int)alen/2, &xpt, &ypt);
  point[1].x = (short)(xpt);
  point[1].y = (short)(ypt);
  getAngleDistancePosition( xpt,  ypt,  theta+180+ANGLE, alen, &xout, &yout);
  point[0].x = (short)(xout);
  point[0].y = (short)(yout);
  getAngleDistancePosition( xpt,  ypt,  theta+180-ANGLE, alen, &xout, &yout);
  point[2].x = (short)(xout);
  point[2].y = (short)(yout);
  point[3].x = (short)0;
  point[3].y = (short)0;

  XDrawLines( XtDisplay(gw), XtWindow(gw), edgeg->edge.drawGC, point, 3, 0);

/* adjust the bounding box for the arrow 
*  The "bounding" box is an imaginary box on screen display which 
*  will trigger the expose function. 
*/

  adj_bounding_box( gw, (int)point[0].x, (int)point[0].y );
  adj_bounding_box( gw, (int)point[2].x, (int)point[2].y );

  return;
} /* draw arrow */


static int drawTransformer(Widget gw, int alen )
/**************************************************************************\
* Purpose: draw the transformer symbol on the edge widget
*
\**************************************************************************/
{
#define TANGLE 30

  EdgeGadget edgeg = (EdgeGadget) gw;
  int xmid, ymid, xout, yout, xpt, ypt;
  double theta;
  XPoint point[6];
     
  XSetLineAttributes( XtDisplay(gw), edgeg->edge.drawGC, 
	 XiDefaultLineThickness, LineSolid , CapProjecting, JoinBevel);

  get_edge_attributtes( gw, &xmid, &ymid, &theta );

  alen *= .8;
  if(TRUE == edgeg->edge.arrowDirection )
    alen = -alen;

  getAngleDistancePosition( xmid, ymid, theta,            alen, &xpt,  &ypt);
  point[2].x = (short)(xpt);
  point[2].x = max(point[2].x, DRAW_BORDER);
  point[2].y = (short)(ypt);
  point[2].y = max(point[2].y, DRAW_BORDER);
  getAngleDistancePosition( xpt,  ypt,  theta+TANGLE,     alen, &xout, &yout);
  point[1].x = (short)(xout);
  point[1].x = max(point[1].x, DRAW_BORDER);
  point[1].y = (short)(yout);
  point[1].y = max(point[1].y, DRAW_BORDER);
  getAngleDistancePosition( xout, yout, theta+180-TANGLE, alen, &xout, &yout);
  point[0].x = (short)(xout);
  point[0].x = max(point[0].x, DRAW_BORDER);
  point[0].y = (short)(yout);
  point[0].y = max(point[0].y, DRAW_BORDER);
  getAngleDistancePosition( xpt,  ypt,  theta-TANGLE,     alen, &xout, &yout);
  point[3].x = (short)(xout);
  point[3].x = max(point[3].x, DRAW_BORDER);
  point[3].y = (short)(yout);
  point[3].y = max(point[3].y, DRAW_BORDER);
  getAngleDistancePosition( xout, yout, theta+180+TANGLE, alen, &xout, &yout);
  point[4].x = (short)(xout);
  point[4].x = max(point[4].x, DRAW_BORDER);
  point[4].y = (short)(yout);
  point[4].y = max(point[4].y, DRAW_BORDER);
  point[5].x = (short)(0);
  point[5].y = (short)(0);

  XDrawLines( XtDisplay(gw), XtWindow(gw), edgeg->edge.drawGC, point, 5, 0);

/** now adjust the bounding box for the transformer symbol **/

  adj_bounding_box( gw, (int)point[0].x, (int)point[0].y );
  adj_bounding_box( gw, (int)point[1].x, (int)point[1].y );
  adj_bounding_box( gw, (int)point[3].x, (int)point[3].y );
  adj_bounding_box( gw, (int)point[4].x, (int)point[4].y );

  return;

} /* drawTransformer */


void  get_edge_attributtes( Widget gw, int *xmid, int *ymid, double *theta )
/***************************************************************************\
* Purpose: calculate & return edge angle and mid point 
*
\***************************************************************************/
{
  EdgeGadget edgeg = (EdgeGadget) gw;

  int dx, dy;
  double pct=0.5;

  dx = (int)(edgeg->edge.x2 - edgeg->edge.x1);
  dy = (int)(edgeg->edge.y2 - edgeg->edge.y1);

  *xmid = (int)edgeg->edge.x1 + (int)(dx*pct);
  *ymid = (int)edgeg->edge.y1 + (int)(dy*pct);

  *theta =  atan2((double)-dy, (double)dx) * RADIAN;

}



void adj_bounding_box( Widget wid, int x, int y )
/***************************************************************************\
* Purpose:  checks x, y.  If outside of bounding box, then increase
*	    bounding box size.
*
\***************************************************************************/
{
  EdgeGadget edgeg = (EdgeGadget)wid;

  if((int)edgeg->rectangle.x+2 > x )
  {
    edgeg->rectangle.width += abs(edgeg->rectangle.x - x ) + 4;
    edgeg->rectangle.x      = x-2;
  } 

  if((int)edgeg->rectangle.x + (int)edgeg->rectangle.width < x+2)
  {
    edgeg->rectangle.width += 
     abs(edgeg->rectangle.x + edgeg->rectangle.width - x) + 2;
  }




  if((int)edgeg->rectangle.y+2 > y )
  {
    edgeg->rectangle.height += abs((int)edgeg->rectangle.y - y) + 4;
    edgeg->rectangle.y       = y-2;
  } 

  if((int)edgeg->rectangle.y + (int)edgeg->rectangle.height < (int)y+2)
  {
    edgeg->rectangle.height += 
     abs(edgeg->rectangle.y + edgeg->rectangle.height - y) + 2;
  }

}


/***************************************************************************
* drawTExt
*
**************************************************************************
*/
static int drawText(Widget wid)
{
  EdgeGadget edgeg = (EdgeGadget) wid;
  XRectangle clip;
  Dimension width = 0;
  Dimension height = 0;
  Dimension yoffset = 0;

    if(debug_flag == True) 
      printf("drawText: try to draw the text \n");

    clip.x = edgeg->rectangle.x;
    clip.y = edgeg->rectangle.y;
    clip.width = edgeg->rectangle.width;
    clip.height = edgeg->rectangle.height;
/*** clip any text outside the bounding box ***/
/*
*    XSetClipRectangles(XtDisplay(XtParent(wid)), edgeg->edge.drawGC,
*      0, 0, &clip, 1, Unsorted);
*/

    XmStringExtent(edgeg->edge.labelFontList, edgeg->edge.label,
                  &width, &height);
    yoffset = (Dimension)(edgeg->rectangle.height/2) - (Dimension)(height/2);
    XmStringDraw (XtDisplay(XtParent(wid)), XtWindow(XtParent(wid)),
                  edgeg->edge.labelFontList, edgeg->edge.label, 
                  edgeg->edge.textGC,
                  edgeg->rectangle.x, 
                  edgeg->rectangle.y + yoffset, 
                  edgeg->rectangle.width,
                  XmALIGNMENT_CENTER, 
                  XmSTRING_DIRECTION_L_TO_R, &clip);
/*
*    XSetClipMask (XtDisplay (wid), edgeg->edge.drawGC, None);
*/

  return(0);

} /* end drawText */



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

  return(False);
} /* end SetValuesHook */


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

#ifdef REMOVE
  if( (cur_edgeg->edge.zoom != edgeg->edge.zoom) ) {
    if(edgeg->edge.zoom >= 1 && edgeg->edge.zoom <= 1000) {
      /* zoom_factor = (double)edgeg->edge.zoom/100; */
    } else {
      /* reset values to old values since change is out of valid range */
      edgeg->edge.zoom = cur_edgeg->edge.zoom;
    }
  }
#endif

  /** check for a change in the label */
  if( cur_edgeg->edge.label != edgeg->edge.label ||
      cur_edgeg->edge.lineThickness != edgeg->edge.lineThickness) {
    InitializeText(new);
  }
  if( (cur_edgeg->edge.drawPixel != edgeg->edge.drawPixel) ||
      (cur_edgeg->edge.drawPixmap != edgeg->edge.drawPixmap) ) {
    defineDrawGC(edgeg);
  }
  Resize((Widget)new);

  return(TRUE);
} /* end SetValues */


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

  return(False);
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

  return(False);
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
/*  printf("Edge> visual_change gadget \n"); */

  return(False);
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


void setStringBoundary( Widget wid, char *string, double size )
/***************************************************************************\
* Purpose: Calculates the "box" which the string should fit in.
*
\***************************************************************************/
{
  int l, i, cnt, maxi, num_lin;
  double x, y;
  EdgeGadget edgeg = (EdgeGadget)wid;

  preposition_str( wid, string, size );
  x = xs;
  y = ys;

  edgeg->edge.minx = min( edgeg->edge.x1, edgeg->edge.x2 );
  edgeg->edge.miny = min( edgeg->edge.y1, edgeg->edge.y2 );
  edgeg->edge.maxx = max( edgeg->edge.x2, edgeg->edge.x1 );
  edgeg->edge.maxy = max( edgeg->edge.y2, edgeg->edge.y1 );

/*  printf("before: minx %d miny %d maxx %d maxy %d\n", edgeg->edge.minx, edgeg->edge.miny, edgeg->edge.maxx, edgeg->edge.maxy);*/

/* now check the four corners of each string */
  maxi = strlen(string);
  cnt = 0;
  num_lin = 0;
  for( i = 0; string[i] != '\0' && i <= maxi; i++ )
  {
    if( string[i] == '\n' )
    {
      ck_four_corners( wid, x, y, cnt );
      switch( ++num_lin )
      {
        case 1:
          x += ssr*(CHAR_HT+4*H_CHR_SP);
	  y += scr*(CHAR_HT+4*V_CHR_SP); /* 2nd line jumps clear of line */
	break;

	default:
          x += ssr*(CHAR_HT+H_CHR_SP-1);
	  y += scr*(CHAR_HT+V_CHR_SP-1);
      }
      cnt = 0;
    }
    cnt++;
  }

  ck_four_corners( wid, x, y, cnt );

/*  printf(" after: minx %d miny %d maxx %d maxy %d\n", edgeg->edge.minx, edgeg->edge.miny, edgeg->edge.maxx, edgeg->edge.maxy);*/

}


int max_len_str( char *string )
/**************************************************************************\
* Purpose:  Find longest string between linefeeds (and eol)
*
\**************************************************************************/
{
  int i, maxlen, maxi, cnt;

  cnt = 0;			/* count of chars between line feed	*/
  maxlen = 0;			/* total chars in full string */
  maxi = strlen(string);

  for( i = 0; string[i] != '\0' && i <= maxi; i++ )
  {
    if( string[i] == '\n' )	/* test for line feed */
    {
      if( cnt > maxlen ) maxlen=cnt;
      cnt = 0;
    }
    cnt++;
  }
  if( cnt > maxlen ) maxlen=cnt;

  return maxlen;
}

void preposition_str( Widget wid, char *string, double size )
/*************************************************************************\
* Purpose: Give end points of line, calculate position to start drawing
*	   the string of characters (including ssr, scr)
*
*	Note: ssr = SizeSinRadians
*	      scr = SizeCosRadians
\*************************************************************************/
{
  int x, y;
  double l,angle;
  EdgeGadget edgeg = (EdgeGadget)wid;

		/* don't care about upsidedown angles	*/
		/* just get angle +90 to -90 degrees	*/

  get_edge_attributtes( wid, &x, &y, &angle );
  xs = (double)x;
  ys = (double)y;

  if( angle < 90. ) angle += 180.;	/* set it right side up for text */
  if( angle > 90. ) angle -= 180.;

  scr = (double)size*(double)cos((double)((double)angle/(double)RADIAN))*(double)edgeg->edge.zoom/(double)100.0;
  ssr = (double)size*(double)sin((double)((double)angle/(double)RADIAN))*(double)edgeg->edge.zoom/(double)100.0;

  l = (max_len_str( string )+.5)*(CHAR_WD+H_CHR_SP)/2;	/* calc. 1/2 length */

  xs -= l*scr;   /* shift back */
  ys += l*ssr;

  xs = (xs - 8.0*ssr);      /* shift 8 pixels above line */
  ys = (ys - 8.0*scr);

}


void ck_four_corners( Widget wid, int x, int y, int len )
/**************************************************************************\
* Purpose: check the four corners of a string 
*	   to find min and max x, y
\**************************************************************************/
{
    int xc, yc;
    EdgeGadget edgeg = (EdgeGadget)wid;

    edgeg->edge.maxx = max( (Dimension)x, edgeg->edge.maxx );	/* SW corner */
    edgeg->edge.minx = min( (Dimension)x, edgeg->edge.minx );
    edgeg->edge.minx = max( DRAW_BORDER , edgeg->edge.minx );
    edgeg->edge.maxy = max( (Dimension)y, edgeg->edge.maxy );
    edgeg->edge.miny = min( (Dimension)y, edgeg->edge.miny );

    xc  = x + scr*len*(CHAR_WD+H_CHR_SP);	/* SE corner */
    edgeg->edge.maxx = max( (Dimension)xc, edgeg->edge.maxx );
    edgeg->edge.minx = min( (Dimension)xc, edgeg->edge.minx );

    yc  = y - ssr*len*(CHAR_WD+V_CHR_SP);
    edgeg->edge.maxy = max( (Dimension)yc, edgeg->edge.maxy );
    edgeg->edge.miny = min( (Dimension)yc, edgeg->edge.miny );

    xc  = x + scr*len*(CHAR_WD+H_CHR_SP) - ssr*CHAR_HT;	/* NE corner */
    edgeg->edge.maxx = max( (Dimension)xc, edgeg->edge.maxx );
    edgeg->edge.minx = min( (Dimension)xc, edgeg->edge.minx );

    yc  = y - ssr*len*(CHAR_WD+V_CHR_SP) - scr*CHAR_HT;
    yc  = max( yc, DRAW_BORDER );
    edgeg->edge.maxy = max( (Dimension)yc, edgeg->edge.maxy );
    edgeg->edge.miny = min( (Dimension)yc, edgeg->edge.miny );

    xc  = x - ssr*CHAR_HT;				/* NW corner */
    xc  = max( xc, 1 );
    edgeg->edge.maxx = max( (Dimension)xc, edgeg->edge.maxx );
    edgeg->edge.minx = min( (Dimension)xc, edgeg->edge.minx );
    edgeg->edge.minx = max( DRAW_BORDER , edgeg->edge.minx );

    yc  = y - scr*CHAR_HT;
    yc  = max( yc, DRAW_BORDER );
    edgeg->edge.maxy = max( (Dimension)yc, edgeg->edge.maxy );
    edgeg->edge.miny = min( (Dimension)yc, edgeg->edge.miny );

/*    edgeg->edge.miny = max( 2, edgeg->edge.miny );	 don't let it go negative 
*    edgeg->edge.minx = max( 2, edgeg->edge.minx );
*/

}

void drawCharsCenteredAboveLine( Widget wid, char *string,
                 int x1, int y1, int x2, int y2, double size )
/*****************************************************************************\
*
\*****************************************************************************/
{
  int xi = 0;
  int yi = 0;

  preposition_str( wid, string, size );

  xi = (int)xs;
  yi = (int)ys;

  drawChars( wid, string, xi, yi, size );

}


void drawChars( Widget wid, char *string, int x, int y,
                         double size )
/*****************************************************************************\
*       Purpose: draw string of characters - any size, any angle
*
*       Author: Wm E. Rogers    Jan 1995
*
*       Called by:
\******************************************************************************/
{
  int len;
  int i, ic;
  int num_lin;
  int seg_len, longest_str;
  double xp, yp, org_x, org_y;

  xp = (double)(x);
  yp = (double)(y);
  org_x = xp;
  org_y = yp;

  len = strlen( string );
  num_lin = 0;
  longest_str = longest_segment( string );

  /* find length of this string (for possible centering) */
  seg_len = 0;
  for ( ic=0; ic<=len; ic++ )
  {
    if( string[ic]== '\n' || string[ic]=='\0' ) 
    {
      seg_len = ic;
      break;
    }
  }

  if( seg_len < longest_str )
  {
    xp += ((longest_str-seg_len)/2)*(CHAR_WD+H_CHR_SP)*scr;
    yp -= ((longest_str-seg_len)/2)*(CHAR_WD+V_CHR_SP)*ssr;
  }

  for ( i=0; i<len; i++ )
  {
    if( string[i]=='\n' )       /* start new line */
    {
      switch( ++num_lin )
      {
        case 1:
	  org_y += (CHAR_HT+4*V_CHR_SP)*scr; /* 2nd line jumps clear of line */
          org_x += (CHAR_HT+4*H_CHR_SP)*ssr;
	break;

	default:
	  org_y += (CHAR_HT+V_CHR_SP-1)*scr;
          org_x += (CHAR_HT+H_CHR_SP  )*ssr;
      }
      xp = org_x;
      yp = org_y;

      seg_len = 0;
      for ( ic++; ic<len; ic++ )
      {
        if( string[ic]== '\n' || string[ic]=='\0' )
        {
	  break;
	}
	else
	seg_len++;
      }

      if( seg_len < longest_str )
      {
        xp += ((longest_str-seg_len)/2)*(CHAR_WD+H_CHR_SP)*scr;
        yp -= ((longest_str-seg_len)/2)*(CHAR_WD+V_CHR_SP)*ssr;
      }

    }
    else
    {
      drawletter( wid, xp, yp, string[i], size );
      xp += (CHAR_WD+H_CHR_SP)*scr;
      yp -= (CHAR_WD+V_CHR_SP)*ssr;
    }
  }

}


int longest_segment( char *str )
/**************************************************************************\
* Purpose: Scan character string & find longest segment (between line feeds)
*
\**************************************************************************/
{
  int longest_str;	/* maximum # of chars on any segment */
  int cnt;
  char *a;

  longest_str = 0;
  cnt = 0;
  a = &str[0];

  while ( *a != '\0' )
  {
    if( *a == '\n' )
    {
      longest_str = max( longest_str, cnt );
      cnt = 0;
    }
    else
    {
      cnt++;
    }
    a++;
  }
  return ( max( longest_str, cnt ));
}


void drawletter( Widget wid, double xf, double yf, char c, double size )
/*****************************************************************************\
*
\*****************************************************************************/
{

  int i=0;
  double xt, yt;
  XPoint p[20];
  int npts;
  int xti,yti;
  int id;
  EdgeGadget pedge = (EdgeGadget)wid;

  if( (id = findCharID( c )) <0 ) return;

  i    = 0;
  npts = 0;
  xt = roundoff(xf);	/* seems to make the letters more consistant */
  yt = roundoff(yf);

  while ( mypts[id].x[i] != 99 )
  {
    if ( i >= MAXPT )
    {  id++;			/* next character */
       i = 0;			/* init data ptr  */
    }

    if ( abs( mypts[id].x[i]) < 100 )	/* if valid draw move */
    {
      if( npts == 0 && i == 0 )
      {
	p[npts  ].x = max(roundoff(xf), DRAW_BORDER);
	p[npts++].y = max(roundoff(yf), DRAW_BORDER);
      }
				/*  to here  */
      xt = xt + (double)(mypts[id].x[i]*scr) + (double)(mypts[id].y[i]*ssr);
      yt = yt + (double)(mypts[id].y[i]*scr) - (double)(mypts[id].x[i]*ssr);

      p[npts  ].x = max(roundoff(xt), DRAW_BORDER);
      p[npts++].y = max(roundoff(yt), DRAW_BORDER);

    }
    else			/* advance relative - (no draw) */
    {
      if( npts )		/* draw old lines ( and clear )  */
      {
        XDrawLines( XtDisplay(wid), XtWindow(wid), pedge->edge.textGC, 
           p, npts, CoordModeOrigin );
      }
      if( mypts[id].x[i] < 0 )	/* move in negative direction */
      {		/* (Remove that '100' from the data - leaving sign intact) */
        xt = xt + (mypts[id].x[i]+100)*scr +  mypts[id].y[i]     *ssr;
        yt = yt +  mypts[id].y[i]     *scr - (mypts[id].x[i]+100)*ssr;
      }
      else			/* move in positive direction */
      {
        xt = xt + (mypts[id].x[i]-100)*scr +  mypts[id].y[i]     *ssr;
        yt = yt +  mypts[id].y[i]     *scr - (mypts[id].x[i]-100)*ssr;
      }
      npts = 0;
      p[npts  ].x = roundoff(xt);	/* start of next set of lines */
      p[npts++].y = roundoff(yt);
    }
    i++;			/* inc to next data waypoint */
  }

  /* Clear out the data */
  XDrawLines( XtDisplay(wid), XtWindow(wid), pedge->edge.textGC, 
           p, npts, CoordModeOrigin );

}

int findCharID( char c )
/*************************************************************************\
\*************************************************************************/
{
  int i;

  for ( i = 0; i<C_LMT; i++ )
  {
    if ( c == mypts[i].symbol[0] ) return i;
  }

  return -1;
}

int edge_motif2cstring(XmString xmstr, char ** cstr )
{
  int cnt;
  char newstr[1024];
  XmStringContext context = NULL;
  char         *text;
  XmStringCharSet  charset;
  XmStringDirection    dir;
  Boolean      separator = False;
  char *eol = "\n";
  int i;

  XmStringInitContext(&context, xmstr);
  newstr[0] = '\0';
  cnt = XmStringLineCount(xmstr);
  for(i=0; i<cnt; i++) {
    XmStringGetNextSegment(context, &text, &charset, &dir, &separator);
    if(i > 0) {
      strcat(newstr, eol);
    }
    strcat(newstr, text);
  }
  XmStringFreeContext(context);

  /* printf("edgeg: cnt %d text = (%s)\n", cnt, newstr); */
  *cstr = (char *)malloc((1+strlen(newstr))*sizeof(char));
  strcpy(*cstr, newstr);
  return cnt;
}


/* end of EdgeG.c */
