/* 
 * draw gadget based on Motif Release 1.2.4
*/ 

#include <Xm/Xm.h>
#include <Xm/XmP.h>
#include <Xm/ManagerP.h>
#include <Xm/Screen.h>
#include "drawgadgetp.h"
#include <string.h>
#include <stdio.h>
#include <ctype.h>

/* #define DEBUG 1 */
#define max(A,B) ((A) > (B) ? (A) : (B))

/********    Static Function Declarations    ********/
#ifdef __STDC__
#define PROTO(x)        x
#else
#define PROTO(x)        ()
#endif

static void ClassInitialize	PROTO (( void )) ;
static void ClassPartInitialize	PROTO (( WidgetClass cl));
static void InitializePosthook	PROTO (( Widget req, Widget new_w,
					ArgList args, Cardinal *num_args)) ;
static Boolean SetValuesPrehook PROTO (( Widget oldParent, Widget refParent,
					Widget newParent, ArgList args,
					 Cardinal *num_args)) ;
static void GetValuesPrehook	PROTO (( Widget newParent, ArgList args,
					Cardinal *num_args)) ;
static void GetValuesPosthook	PROTO (( Widget new_w, ArgList args,
					Cardinal *num_args)) ;
static Boolean SetValuesPosthook PROTO (( Widget current, Widget req,
					  Widget new_w, ArgList args,
					  Cardinal *num_args)) ;
static void SetNormalGC		PROTO	(( XmDrawGadget lw)) ;
static void SetSize		PROTO	(( Widget wid)) ;
static void Initialize		PROTO	(( Widget req, Widget new_w,
				   ArgList args, Cardinal *num_args)) ;
static XtGeometryResult QueryGeometry PROTO (( Widget wid,
					XtWidgetGeometry *intended,
					XtWidgetGeometry *reply)) ;
static void Destroy		PROTO (( Widget w)) ;
static void Redisplay		PROTO (( Widget wid, XEvent *event,
					   Region region)) ;
static Boolean SetValues	PROTO (( Widget cw, Widget rw, Widget nw,
					   ArgList args, Cardinal *num_args));
static Boolean VisualChange	PROTO (( Widget wid, Widget cmw, Widget nmw));
static void InputDispatch	PROTO (( Widget wid, XEvent *event,
					 Mask event_mask));
static void Help		PROTO (( Widget w, XEvent *event)) ;
static void GetDrawString	PROTO (( Widget wid, int offset, XtArgVal *value)) ;
static XmStringCharSet _XmStringCharsetCreate PROTO (( 
					XmStringCharSet stringcharset)) ;
static Cardinal GetDrawBGClassSecResData PROTO (( WidgetClass w_class,
					 XmSecondaryResourceData **data_rtn)) ;
static XtPointer GetDrawClassResBase PROTO (( Widget widget,
						XtPointer client_data)) ;
static Boolean XmDrawGadgetGetDisplayRect PROTO (( Widget w,
						XRectangle *displayrect)) ;
static Boolean Convert		PROTO (( Widget w, Atom *selection,
				 Atom *target, Atom *type, XtPointer *value,
				 unsigned long *length, int *format)) ;
static _XmProcessButtonEvent( Widget w, XEvent *event, String *params,
				Cardinal *num_params );


/****************************\
* resources for Gadget
\****************************/

static XtResource resources[] = 
{
   {
     XiNbuttonCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     XtOffsetOf(struct _XmDrawGadgetRec ,draw.button_callback), 
     XtRCallback, (XtPointer)NULL
   },
   {
     XiNdrawCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     XtOffsetOf(struct _XmDrawGadgetRec ,draw.draw_callback), 
     XtRCallback, (XtPointer)NULL
   },
   {
     XmNlabelType,
     XmCLabelType,
     XmRLabelType,
     sizeof(unsigned char),
     XtOffsetOf( struct _XmDrawGadgetRec, draw.draw_type), XmRImmediate,
		(XtPointer) XmSTRING
   },
   { XiNshowLabel,
     XiCShowLabel,
     XmRBoolean,
     sizeof(Boolean),
     XtOffsetOf( struct _XmDrawGadgetRec, draw.show_label), XmRImmediate,
     (XtPointer) True
   },
   { XmNlabelString,
     XmCXmString,
     XmRXmString,
     sizeof(XmString),
     XtOffsetOf( struct _XmDrawGadgetRec, draw.draw),
     XmRImmediate,
     (XtPointer) NULL
   },
   { XmNfontList,
     XmCFontList,
     XmRFontList,
     sizeof(XmFontList),
     XtOffsetOf( struct _XmDrawGadgetRec, draw.font),
     XmRImmediate,
     (XtPointer) NULL
   },
   {
     XmNhighlightThickness, 
     XmCHighlightThickness, 
     XmRHorizontalDimension,
     sizeof (Dimension), 
     XtOffsetOf( struct _XmGadgetRec, gadget.highlight_thickness),
     XmRImmediate, (XtPointer) 0
   },

};


/* This is the class record that gets set at compile/link time  */
/* this is what is passed to the widget create routine as the   */
/* the class.  All fields must be inited at compile time.       */

externaldef ( xmdrawgadgetclassrec) 
		XmDrawGadgetClassRec xmDrawGadgetClassRec = {
{
    /* superclass	  */	(WidgetClass) &xmGadgetClassRec,
    /* class_name	  */	"XmDrawGadget",
    /* widget_size	  */	sizeof(XmDrawGadgetRec),
    /* class_initialize   */    ClassInitialize,
    /* chained class init */	ClassPartInitialize,
    /* class_inited       */	False,
    /* initialize	  */	Initialize,
    /* initialize hook    */    NULL,
    /* realize		  */	NULL,
    /* actions		  */	NULL,
    /* num_actions	  */	0,
    /* resources	  */	resources,
    /* num_resources	  */	XtNumber(resources),
    /* xrm_class	  */	NULLQUARK,
    /* compress_motion	  */	True,
    /* compress_exposure  */	XtExposeCompressMaximal,
    /* compress enter/exit*/    True,
    /* visible_interest	  */	False,
    /* destroy		  */	Destroy,
    /* resize		  */	SetSize,
    /* expose		  */	Redisplay,
    /* set_values	  */	SetValues,
    /* set values hook    */    NULL,
    /* set values almost  */    NULL,
    /* get values hook    */    NULL,
    /* accept_focus	  */	NULL,
    /* version            */    XtVersion,
    /* callback offsetlst */    NULL,
    /* default trans      */    NULL,
    /* query geo proc	  */	NULL,
    /* display accelerator*/	NULL,
    /* extension record   */    NULL,
  },
  {					/* XmGadget           */
    XmInheritBorderHighlight,		/* border_highlight   */
    XmInheritBorderUnhighlight,		/* border_unhighlight */
    NULL,				/* arm_and_activate   */
    InputDispatch,			/* input dispatch     */
    VisualChange,			/* visual_change      */
    NULL,
    0,
    NULL,
    NULL
  },
  {					/* Draw */
    NULL,				/* extension record         */
  }
};

externaldef(xmdrawgadgetclass) WidgetClass xmDrawGadgetClass =  (WidgetClass) &xmDrawGadgetClassRec;


static void ClassInitialize()
/************************************************************************\
\************************************************************************/
{
	return;
}



static void ClassPartInitialize( cl )
/************************************************************************
 *
 *	Processes the class fields which need to be inherited.
 *
 ************************************************************************/
        WidgetClass cl ;
{ }



static void SetNormalGC( lw )
/************************************************************************
 *
 *	Create the normal and insensitive GC's for the gadget.
 *
 ************************************************************************/
	XmDrawGadget lw ;
{
   XGCValues       values;
   XtGCMask        valueMask;
   XmManagerWidget mw;
   XFontStruct     *fs = (XFontStruct *) NULL;
   XColor          xcolorSlot;  /* used to extract pixel from colorname */
   Arg		   scratchArg;
   XmString        scratchString[513]; /* used to hold returned colorname */


   mw = (XmManagerWidget) XtParent(lw);

   valueMask = GCForeground | GCBackground | GCFont |
               GCGraphicsExposures | GCClipMask;

    _XmFontListGetDefaultFont(LabG_Font(lw), &fs);
   /*
      begin hack.  get current value of "foreground" resource of Manager,
      lookup pixel value assigned to that color, store said value in 
      values.foreground.  directly accessing widget resource structures
      is forbidden in X.

      the below uses whatever foreground and background the manager widget
      is using -- probably black and white.
   */
   XtSetArg(scratchArg, XmNforeground, &scratchString);
   XtGetValues((Widget) mw, &scratchArg, 1);
   XLookupColor(XtDisplay(mw), DefaultColormap(XtDisplay(mw), 0),
	 (char *) scratchString, &xcolorSlot, (XColor *) NULL);
   values.foreground = xcolorSlot.pixel;	/* ber - no effect */
   /* */

   XtSetArg(scratchArg, XmNbackground, &scratchString);
   XtGetValues((Widget) mw, &scratchArg, 1);
   XLookupColor(XtDisplay(mw), DefaultColormap(XtDisplay(mw), 0),
	 (char *) scratchString, &xcolorSlot, (XColor *) NULL);
   values.background = xcolorSlot.pixel;
   
   /*  to hard-code black or white -- if the above doesn't work
   
       values.foreground = BlackPixel(Display, 0);
       values.background = WhitePixel(Display, 0);
   */
   /* original code here 
   values.foreground = mw->manager.foreground;
   values.background = mw->core.background_pixel;
   */
   values.graphics_exposures = FALSE;
   values.clip_mask = None;

   if (fs==NULL)
     valueMask &= ~GCFont;
   else
     values.font       = fs->fid;

   LabG_NormalGC(lw) = XtGetGC( (Widget) mw, valueMask, &values);

   valueMask |= GCFillStyle | GCTile;
   values.fill_style = FillTiled;
   /* "50_foreground" is in the installed set and should always be found;
   ** omit check for XmUNSPECIFIED_PIXMAP
   */
   values.tile = XmGetPixmapByDepth (XtScreen((Widget)(lw)), "50_foreground",
				mw->manager.foreground,
				mw->core.background_pixel,
				mw->core.depth);
   LabG_InsensitiveGC(lw) = XtGetGC( (Widget) mw, valueMask, &values);

}


void _XmCalcDrawGDimensions( wid )
/************************************************************************
 *
 *   Calculates the dimensionsof the draw text and pixmap, and updates
 *   the TextRect fields appropriately. Called at Initialize and SetValues.
 *   This is also called by the subclasses to recalculate draw dimensions.
 *
 *************************************************************************/
	Widget wid ;
{
  XmDrawGadget newlw = (XmDrawGadget) wid ;
  Dimension w, h;

/* initialize TextRect width and height to 0, reset if needed */
  LabG_TextRect(newlw).width = 0;
  LabG_TextRect(newlw).height = 0;

/* If we have a string then size it.  */
  if (!XmStringEmpty (LabG__draw(newlw)))
  {
	XmStringExtent (LabG_Font(newlw), LabG__draw(newlw), &w, &h);
	LabG_TextRect(newlw).width = (unsigned short)w;
	LabG_TextRect(newlw).height = (unsigned short)h;
  }
}    





static void SetSize( wid )
/************************************************************************
 *
 *  SetSize
 *	Sets new width, new height, and new draw.TextRect 
 *	appropriately. It is called by Initialize and SetValues.
 *
 ************************************************************************/
	Widget wid ;
{  
  XmDrawGadget newlw = (XmDrawGadget) wid ;
  Dimension width, height;

  if(newlw->draw.show_label == True)
  {
    XmStringExtent (LabG_Font(newlw), LabG__draw(newlw), &width, &height);
    newlw->rectangle.width = width;
    newlw->rectangle.height = height;
  }

  if (newlw->rectangle.width == 0)    /* set core width and height to a */
	newlw->rectangle.width = 1;     /* default value so that it doesn't */
  if (newlw->rectangle.height == 0)   /* a Toolkit Error */
	newlw->rectangle.height = 1;
}





#ifdef DEBUG
static int motif2cstring(XmString xmstr, char ** cstr )
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
    if(text != NULL)
    strcat(newstr, text);
  }
  XmStringFreeContext(context);

  /* printf("edgeg: cnt %d text = (%s)\n", cnt, newstr); */
  *cstr = (char *)malloc((1+strlen(newstr))*sizeof(char));
  strcpy(*cstr, newstr);
  return cnt;
}
#endif


static void Initialize( req, new_w, args, num_args )
/************************************************************************
 *
 *	This is the widget's instance initialize routine.  It is
 *	called once for each widget.
 *  Changes: Treat draw, pixmap, drawtype, mnemoniccharset as independedntly
 *	settable resource.
 ************************************************************************/
        Widget req ;
        Widget new_w ;
        ArgList args ;
        Cardinal *num_args ;
{
    unsigned char  stringDirection;
    Arg           myargs[1];
    int n;
    char *str;

    XmDrawGadget lw = (XmDrawGadget) new_w;
    XmDrawGadget rw = (XmDrawGadget) req;
#ifdef DEBUG
    motif2cstring(LabG__draw(req), &str);
	printf("initialize: request string %s\n", str);
    motif2cstring(LabG__draw(new_w), &str);
	printf("initialize: new widget string %s\n", str);
#endif
    if (LabG_Font(new_w) == NULL)
    {
	XmFontList defaultFont;
	defaultFont = _XmGetDefaultFontList( (Widget) lw, XmLABEL_FONTLIST);
	lw->draw.font = XmFontListCopy (defaultFont);
    }
    else
    {
	/* Make a local copy of the font list */
	LabG_Font(new_w) = XmFontListCopy( LabG_Font(new_w));
    }

    /*  Handle the draw string :
     *   If no draw string is given accept widget's name as default.
     *     convert the widgets name to an XmString before storing;
     *   else
     *     save a copy of the given string.
     *     If the given string is not an XmString issue an warning.
     */

    if (LabG__draw(new_w) == NULL)
    {
	XmString string;
	XmStringCharSet cset = (XmStringCharSet) XmFONTLIST_DEFAULT_TAG;

	string   =  XmStringLtoRCreate(XrmQuarkToString(lw->object.xrm_name),
			cset);
	LabG__draw(new_w) =  XmStringCopy(string);
	XmStringFree (string);
    }
    else
    { 
      if (_XmStringIsXmString( (XmString) LabG__draw(new_w)))
		LabG__draw(new_w) = XmStringCopy( (XmString) LabG__draw(new_w));
      else
      {  
	XmString string;
	XmStringCharSet cset = (XmStringCharSet) XmFONTLIST_DEFAULT_TAG;
	string   =  XmStringLtoRCreate(XrmQuarkToString(lw->object.xrm_name),
			cset);
	LabG__draw(new_w) =  XmStringCopy(string);
	XmStringFree (string);
      }
    }

/*  If zero width and height was requested by the application,  */
/*  reset new's width and height to zero to allow SetSize()     */
/*  to operate properly.                                        */

    if (rw->rectangle.width == 0) 
		lw->rectangle.width = 0;
    
    if (rw->rectangle.height == 0) 
		lw->rectangle.height = 0;
    
#ifdef REMOVE
    _XmCalcDrawGDimensions(new_w);
    (* (((XmDrawGadgetClassRec *)(lw->object.widget_class))->
		rect_class.resize)) ((Widget) lw); 
#endif

    SetNormalGC(lw);

    SetSize((Widget)lw);

/*  Initialize the interesting input types.  */
    lw->gadget.event_mask = XmHELP_EVENT;
    lw->gadget.event_mask = XmHELP_EVENT | XmFOCUS_IN_EVENT | XmFOCUS_OUT_EVENT
		          | XmENTER_EVENT | XmLEAVE_EVENT | XmBDRAG_EVENT 
			  | XmARM_EVENT | XmMULTI_ARM_EVENT ;
}





static XtGeometryResult QueryGeometry( wid, intended, reply )
/************************************************************************
 *
 *  QueryGeometry
 *
 ************************************************************************/
        Widget wid ;
        XtWidgetGeometry *intended ;
        XtWidgetGeometry *reply ;
{
   XmDrawGadget lg = (XmDrawGadget) wid ;
   reply->request_mode = 0;		/* set up fields I care about */

     /* Don't really know what to do with queries about x,y,border,stacking.
     * Since we are interpreting unset bits as a request for information
     * (asking about neither height or width does the old 0-0 request)
     * a caller asking about x,y should not get back width and height,
     * especially since it is an expensive operation.  So x, y, border, stack
     * all return No, this indicates we'd prefer to remain as is.  Parent
     * is free to change it anyway...
     *
     */

     /* pre-load the reply with input values */

   reply->request_mode = (CWWidth | CWHeight);

   if (reply->width == 0)
          reply->width = 1;

   if (reply->height == 0)
          reply->height = 1;

   return (XtGeometryAlmost);

}


static void Destroy( w )
/************************************************************************
 *
 *	Free up the draw gadget allocated space.  This includes
 *	the draw, and GC's.
 *
 ************************************************************************/
        Widget w ;
{

    XtReleaseGC (XtParent(w), LabG_NormalGC(w));
    XtReleaseGC (XtParent(w), LabG_InsensitiveGC(w));

}


static void Redisplay( wid, event, region )
/************************************************************************
 *
 *
 ************************************************************************/
        Widget wid ;
        XEvent *event ;
        Region region ;
{
   XmDrawGadget lw = (XmDrawGadget) wid;
   GC gc;
   GC clipgc = NULL;
   XRectangle clip_rect;
   Dimension availW, availH, marginal_width, marginal_height, max_text_height;
   Boolean clip_set = False;
   char *str;
   Dimension width, height;
   XidrawgadgetCallbackStruct call_value;

    if(lw->draw.show_label == True) {
#ifdef DEBUG
    motif2cstring(LabG__draw(lw), &str);
    printf("redisplay: new widget string %s\n", str);
#endif
    XmStringExtent (LabG_Font(lw), LabG__draw(lw), &width, &height);
    XmStringDraw (XtDisplay(lw), XtWindow(lw), 
			LabG_Font(lw), LabG__draw(lw),
	(XtIsSensitive(wid) ? LabG_NormalGC(lw) : LabG_InsensitiveGC(lw)),
	lw->rectangle.x, lw->rectangle.y, width,
	XmALIGNMENT_BEGINNING, XmSTRING_DIRECTION_L_TO_R, NULL);
    }
    else
    {
      if ( lw->draw.draw_callback )
      {
	call_value.reason = event->type;
	call_value.event = event;
	call_value.window = XtWindow(wid);
	call_value.click_count = 0;
	call_value.region = region;
	call_value.x = lw->rectangle.x;
	call_value.y = lw->rectangle.y;
	call_value.width = lw->rectangle.width;
	call_value.height = lw->rectangle.height;
	XtCallCallbackList(wid, lw->draw.draw_callback, &call_value);
      } 
    }
}


static Boolean SetValues( cw, rw, nw, args, num_args )
/************************************************************************
 *
 *	This routine will take care of any changes that have been made
 *
 ************************************************************************/
        Widget cw ;
        Widget rw ;
        Widget nw ;
        ArgList args ;
        Cardinal *num_args ;
{
   XmDrawGadget current = (XmDrawGadget) cw ;
   XmDrawGadget req = (XmDrawGadget) rw ;
   XmDrawGadget new_w = (XmDrawGadget) nw ;
   Boolean flag = False;
   Boolean newstring = False;
   Boolean ProcessFlag = FALSE;
   Boolean CleanupFontFlag = FALSE;
   Boolean Call_SetSize = False;

   /*  If the text has changed, make a copy of the new text,  */
   /*  and free the old text.                                  */ 

   if (LabG__draw(new_w)!= LabG__draw(current))
   { newstring = True;
     if (LabG__draw(new_w) == NULL)
     {
          XmString string;
          XmStringCharSet cset = (XmStringCharSet) XmFONTLIST_DEFAULT_TAG;

          string   =  XmStringLtoRCreate(XrmQuarkToString(current->
						object.xrm_name), cset);
          LabG__draw(new_w) =  XmStringCopy(string);
          XmStringFree(string);
     }
     else
     { if (_XmStringIsXmString( (XmString) LabG__draw(new_w))) {
           LabG__draw(new_w) = XmStringCopy( (XmString) LabG__draw(new_w));
       }
       else
       { XmString string;
         XmStringCharSet cset = (XmStringCharSet) XmFONTLIST_DEFAULT_TAG;

         string = XmStringLtoRCreate(XrmQuarkToString(new_w->object.xrm_name),
                                        cset);
         LabG__draw(new_w) =  XmStringCopy(string);
         XmStringFree(string);
       }
     }

     XmStringFree(LabG__draw(current));
     LabG__draw(current)= NULL;
     LabG__draw(req)= NULL;
   }

   if (LabG_Font(new_w) != LabG_Font(current))
   {
     CleanupFontFlag = True;
     if (LabG_Font(new_w) == NULL)
     {
       if (XtClass(new_w) == xmDrawGadgetClass)
         LabG_Font(new_w) = _XmGetDefaultFontList( (Widget) new_w, XmLABEL_FONTLIST);
       else
         LabG_Font(new_w) = _XmGetDefaultFontList( (Widget) new_w, XmBUTTON_FONTLIST); 
     }
     LabG_Font(new_w) = XmFontListCopy (LabG_Font(new_w));

   }


   /*  ReInitialize the interesting input types.  */

   new_w->gadget.event_mask = XmHELP_EVENT;

   new_w->gadget.event_mask |= 
        XmFOCUS_IN_EVENT | XmFOCUS_OUT_EVENT | XmENTER_EVENT | XmLEAVE_EVENT |
        XmBDRAG_EVENT | XmARM_EVENT | XmMULTI_ARM_EVENT ;
     
   /*  Recreate the GC's if the font has been changed  */
   if (LabG_Font(new_w) != LabG_Font(current))
   {
      XtReleaseGC (XtParent (current), LabG_NormalGC(current));
      XtReleaseGC (XtParent (current), LabG_InsensitiveGC(current));
      SetNormalGC(new_w);
      flag = True;
   }

   SetSize((Widget)new_w);

   return (flag);
}



static Boolean VisualChange( wid, cmw, nmw )
/************************************************************************
 *
 *	This function is called from XmManagerClass set values when
 *	the managers visuals have changed.  The gadget regenerates any
 *	GC based on the visual changes and returns True indicating a
 *	redraw is needed.  Otherwize, False is returned.
 *
 ************************************************************************/
        Widget wid ;
        Widget cmw ;
        Widget nmw ;
{
   XmGadget gw = (XmGadget) wid ;
   XmManagerWidget curmw = (XmManagerWidget) cmw ;
   XmManagerWidget newmw = (XmManagerWidget) nmw ;

   return (False);
}

static void InputDispatch( wid, event, event_mask )
/************************************************************************
 *
 *     This function catches input sent by a manager and dispatches it
 *     to the individual routines.
 *
 ************************************************************************/
        Widget wid ;
        XEvent *event ;
        Mask event_mask ;
{
   XmDrawGadget lg = (XmDrawGadget) wid ;
   if (event_mask & XmHELP_EVENT)
     Help ((Widget) lg, event);

   else if (event_mask & XmENTER_EVENT)
     _XmEnterGadget ((Widget) lg, event, NULL, NULL);

   else if (event_mask & XmLEAVE_EVENT) 
     _XmLeaveGadget ((Widget) lg, event, NULL, NULL);

   else if (event_mask & XmFOCUS_IN_EVENT)
     _XmFocusInGadget ((Widget) lg, event, NULL, NULL);

   else if (event_mask & XmFOCUS_OUT_EVENT)
     _XmFocusOutGadget ((Widget) lg, event, NULL, NULL);

   else if (event_mask & XmACTIVATE_EVENT)
     _XmProcessButtonEvent ((Widget) lg, event, NULL, NULL);

   else if (event_mask & XmARM_EVENT)
     _XmProcessButtonEvent ((Widget) lg, event, NULL, NULL);

   else if (event_mask & XmMULTI_ARM_EVENT)
     _XmProcessButtonEvent ((Widget) lg, event, NULL, NULL);

   else if (event_mask & XmMULTI_ACTIVATE_EVENT)
     _XmProcessButtonEvent ((Widget) lg, event, NULL, NULL);
}


static void Help( w, event )
/************************************************************************
 *
 *	This routine is called if the user made a help selection 
 *      on the widget.
 *
 ************************************************************************/
        Widget w ;
        XEvent *event ;
{
    XmDrawGadget lg = (XmDrawGadget) w;
    XmAnyCallbackStruct temp;

    temp.reason = XmCR_HELP;
    temp.event  = event;

}


static void GetDrawString( wid, offset, value )
/************************************************************************
 *
 *     This is a get values hook function that returns the external
 *     form of the draw string from the internal form.
 *
 ***********************************************************************/
/* ARGSUSED */
        Widget wid ;
        int offset ;
        XtArgVal *value ;
{
        XmDrawGadget lw = (XmDrawGadget) wid ;
  XmString string;

#ifdef REMOVE
  string = _XmStringCreateExternal (LabG_Font(lw), LabG__draw(lw));
#endif

  *value = (XtArgVal) string;

}


static XmStringCharSet _XmStringCharsetCreate( stringcharset )
/************************************************************************
 *
 *
 ************************************************************************/
        XmStringCharSet stringcharset ;
{
    char    *cset;
    char    *string;
    int size;

    string = (char *) (stringcharset);
    size = strlen ( string);
    cset = XtMalloc (size +1);
    if (cset != NULL)
    strcpy( cset, string);
    return ( (XmStringCharSet) cset);
}

_XmProcessButtonEvent( w, event, params, num_params )
        Widget w ;
        XEvent *event ;
        String *params ;
        Cardinal *num_params ;
{
    XmDrawGadget lw = (XmDrawGadget) w;
    XidrawgadgetCallbackStruct call_value;

    if ( lw->draw.button_callback )
    {
	call_value.reason = event->type;
	call_value.event = event;
	call_value.window = XtWindow(w);
	call_value.click_count = 0;
	call_value.region = NULL;
	call_value.x = lw->rectangle.x;
	call_value.y = lw->rectangle.y;
	call_value.width = lw->rectangle.width;
	call_value.height = lw->rectangle.height;
	XtCallCallbackList(w, lw->draw.button_callback, &call_value);
	return;
    } 
}

Widget XmCreateDrawGadget( parent, name, arglist, argCount )
/************************************************************************
 *
 *	Externally accessable function for creating a draw gadget.
 *
 ************************************************************************/
        Widget parent ;
        char *name ;
        Arg *arglist ;
        Cardinal argCount ;
{
   return (XtCreateWidget(name, xmDrawGadgetClass, parent, arglist, argCount));
}
