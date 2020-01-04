
static char SCCSID_branchc[] = "@(#)branch.c	20.1 5/2/94";

/****************************************************************************\
*
* module: Branch.c
* purpose: define the Branch Gadget
*
* known problems:
* - if the new bitmap read in is of a different size than the bitmap
*   saved then the line does not center correctly.
* - currently using some global static variables - these should be
*   moved to either gadget instance variables or class variables.
* - not using very many existing variables available from the existing
*   data structures.
*   (using fill_GC only)
* - gadget is large for its function - could make line a direct
*   descendant of rectangular object.
* - color and highlight are not yet implemented.
*
*****************************************************************************/


#include <Xm/XmP.h>
#include <Xm/LabelGP.h>
#include <Xm/PushBGP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/CascadeB.h>

/*** only the widget definition code contains the private header ***/
#include "BranchP.h"

/*** for debugging only ***/
#include <stdio.h>

/*** resource list 	***/
/*** define the resources	***/
#define res_offset(field) XtOffset(BranchGadget,branch.field)
#define goffset(field) XtOffset(BranchGadget,rectangle.field)
#define pushb_offset(field) XtOffset(BranchGadget,pushbutton.field)
#define gadget_offset(field) XtOffset(BranchGadget,gadget.field)
 
static XtResource resources[] = {
	{ XiNx1, XiCX1, XmRDimension, sizeof(Dimension),
	res_offset(x1), XmRString, (caddr_t) XiDefaultX1 },
	{ XiNy1, XiCY1, XmRDimension, sizeof(Dimension),
	res_offset(y1), XmRString, (caddr_t) XiDefaultY1 },
	{ XiNx2, XiCX2, XmRDimension, sizeof(Dimension),
	res_offset(x2), XmRString, (caddr_t) XiDefaultX2 },
	{ XiNy2, XiCY2, XmRDimension, sizeof(Dimension),
	res_offset(y2), XmRString, (caddr_t) XiDefaultY2 },
}; /* end resources */
/***
***/
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
static void ClassInitialize();
static void ClassPartInitialize();	/* unused	*/
static void Initialize();
static void Realize();
static void Redisplay();		/* expose procedure	*/
static void Resize();			/* unused	*/
static Boolean SetValues();
static Boolean SetValuesHook();
static void Destroy();

/*** function declarations for XmGadget part ***/
static void Arm ();
static void arm_and_activate();
static void border_highlight();
static void border_unhighlight();
static Boolean visual_change();
static void input_dispatch();

/*** function declarations for XmLabelGadget part ***/
static void set_override();
static void set_which_button();

/*** following functions are private to Branch	***/
/* static void DrawPixmaps(); */
void PrintBranchRec(Widget);
void PrintBranchClassRec();

/* static XmGetValueResource *get_resources; */

/*** class record initialization (core, branch)	***/
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

BranchClassRec branchClassRec = {
  /* rectobj class part fields */
    {
    (WidgetClass) &xmPushButtonGadgetClassRec, /* pointer superclass ClassRec */
    /* (WidgetClass) &rectObjClassRec,  pointer to rect superclass ClassRec */
    /* (WidgetClass) &widgetClassRec, pointer to core superclass ClassRec */
    (String)"Branch",		/* widget resource class name       */
    (Cardinal)sizeof(BranchRec),	/* size in bytes of widget record   */
    (XtProc)ClassInitialize,	/* 1 ClassInitialize, class initialization proc	    */
    (XtWidgetClassProc)ClassPartInitialize, 	/* 2 ClassPartInitialize, dynamic initialization */
    (Boolean)FALSE,	/* has class been initialized?      */
    (XtInitProc)Initialize, 	/* 2 Initialize, initial instance part fields       */
    (XtArgsProc)NULL,	/* 2 InitializeHook, initialize subpart fields   */
    (XtProc)NULL,	/* w 1 XtInheritRealize, XCreateWindow for widget only	*/
    (XtProc)NULL,	/* w actions, widget semantics name to proc map */
    (Cardinal)0,		/* w XtNumber(actions),  number of entries in actions     */
    (XtResourceList)resources, /* ,NULL	resources for subclass fields    */
    (Cardinal)XtNumber(resources),/* NULL, number of entries in resources */
    (XrmClass)NULLQUARK,	/* resource class quarkified	    */
    (Boolean)TRUE,	/* w compress MotionNotify for widget only */
    (Boolean)XtExposeCompressMaximal,	/* w compress Expose events for widget*/
    (Boolean)TRUE,	/* w compress enter and leave events  */
    (Boolean)FALSE,	/* w select for VisibilityNotify      */
    (XtWidgetProc)Destroy,	/* 2 free data for subclass pointers  */
    (XtWidgetProc)_XtInherit,	/* w 1 XtInheritResize, geom manager changed widget size */
    Redisplay, 	/* 1 (XtExposeProc)XtInheritExpose,  expose routine to redisplay window */
    (XtSetValuesFunc)SetValues,	/* 2 set subclass resource values     */
    (XtArgsFunc)SetValuesHook,	/* 2 SetValuesHook notify that set_values called    */
    (XtAlmostProc)XtInheritSetValuesAlmost,	/* 1` set_values got "Almost" geo reply */
    (XtArgsProc)NULL,	/* 2 GetValuesHook, notify that get_values called    */
    (XtProc)NULL,	/* w 1 XtInheritAcceptFocus, assign input focus to widget     */
    (XtVersionType)XtVersion,	/* version of intrinsics used	    */
    (struct _XtOffsetRec *)NULL,	/* list of callback offsets callback_private       */
    (String)NULL, /* ? defaultTranslations, tm_table state machine  */
    (XtGeometryHandler)XtInheritQueryGeometry,	/* 1` return preferred geometry	*/
    (XtProc)NULL,	/* w XtInheritDisplayAccelerator, display your accelerator */
    (caddr_t)NULL,	/* pointer to extension record      */
    }, /* end rectobj class part fields */
  /* start typedef struct _XmGadgetClassPart */
    {
    (XtWidgetProc)_XtInherit, /* border_highlight, */	/* border_highlight */
    (XtWidgetProc)_XtInherit, /* border_unhighlight, */	/* border_unhighlight */
    (XtProc)arm_and_activate,	/* arm_and_activate */
    (XtWidgetProc)input_dispatch,	/* ??XtExposeProc?? input_dispatch */
    (XmVisualChangeProc)visual_change,	/* visual_change */
    0, /* (XmGetValueResource *) &get_resources,	/* structure get_resources */
    (int)0,	/* num_get_resources */
    (caddr_t)NULL,	/* caddr_t            extension; */
    }, /* end  XmGadgetClassPart; */
  /* start class part  _XmLabelGadgetClassPart  */
    {
    _XtInherit, /* (XtProc) set_override, XmInheritSetOverrideCallback, XtProc setOverrideCallback */
    _XtInherit, /* (XtProc)set_which_button, XmInheritSetWhichButton,  XtProc setWhichButton */
    (caddr_t)NULL, /* caddr_t extension */
    }, /* end xmgadget class part */
  /* typedef struct _XmPushButtonGadgetClassPart */
    {
    (int)NULL, /* int foo */
    }, /*  end XmPushButtonGadgetClassPart */
  /* branch class part */
    { 
    (int)NULL,	/* dummy field */
    }, /* end branch part class fields */
 }; /* end branch class */

WidgetClass branchGadgetClass = (WidgetClass) &branchClassRec; 

/*** section of shared data local to branch widget ***/
/*** temporary shared variables ***/
static	int	branch_gadget_cnt;

static    int	class_gc_init;
static    char * bitmapfilename = "Branch.bits";
static int debug_flag = FALSE;

/*** Gadget Object methods	***/
/******************************************************************************\
* ClassInitialize 
* XtProc takes no arguments
* called once - first time an instance of a class is created by appl
* type converters
* see sec 9.4 X toolkit intrinsics V4
*******************************************************************************/
static void ClassInitialize()
{
    extern int	class_gc_init;
    extern int	branch_gadget_cnt;

  if(debug_flag == TRUE) {
    PrintBranchClassRec();
    printf("Branch> ClassInitialize gadget\n");
    printf("Branch> ClassInit Size of BranchClassRec = %d\n", 
						sizeof(BranchClassRec));
    printf("Branch> ClassInit Size of BranchRec = %d\n", sizeof(BranchRec));
  }

    branch_gadget_cnt = 0;
    class_gc_init = FALSE;

} /* end ClassInitialize*/

/******************************************************************************\
* ClassPartInitialize
* XtWidgetClassProc
* called once - first time an instance of a class is created by appl
* same as ClassInitialize but downward chained.
* resolves inheritance of self sufficient methods from the immediate superclass
* Needed only in classes that define their own methods in their class part
* see ch 13 X toolkit intrinsics V4
*******************************************************************************/
static void 
ClassPartInitialize(WidgetClass widget_class)
{
    if(debug_flag == TRUE) {
       printf("Branch> ClassPartInitialize gadget\n");
    }
} /* end ClassPartInitialize*/


/******************************************************************************\
* Initialize
* XtInitProc
* sets initial values for all the fields in the instance part structure
* data checking on public access fields
* see ch. 6 X toolkit intrinsics V4
*******************************************************************************/
static void Initialize (Widget request, Widget new)
{ 
    extern int	branch_gadget_cnt;
    BranchGadget branchg = (BranchGadget)new;

    if(debug_flag == TRUE) {
        printf("Branch> Initialize widget\n");
        PrintBranchClassRec();
        PrintBranchRec(new);
    }
    branch_gadget_cnt++;

    /*** set the gadget x,y,width,height and orientation ***/
    /*** based on the x1,y1 and x2,y2 set for the gadget ***/
#ifdef OLDWAY
    if(branchg->branch.x1 < branchg->branch.x2) {
        RX(new) = branchg->branch.x1 + (int)(branchg->branch.xsize/2);
    } else {
        RX(new) = branchg->branch.x2 + (int)(branchg->branch.xsize/2);
    };

    if(branchg->branch.y1 < branchg->branch.y2) {
        RY(new) = branchg->branch.y1 + (int)(branchg->branch.ysize/2);
    } else {
        RY(new) = branchg->branch.y2 + (int)(branchg->branch.ysize/2);
    };
    if(((branchg->branch.x1 < branchg->branch.x2)&&
        (branchg->branch.y1 < branchg->branch.y2)) ||
        ((branchg->branch.x1 >= branchg->branch.x2)&&
        (branchg->branch.y1 >= branchg->branch.y2)) )
    {
        branchg->branch.orientation = 0;
    } else {
        branchg->branch.orientation = 1;
    }
    /* printf("Bustest branch_shape = %d\n", branch_shape); */
    RWidth(new) = abs(branchg->branch.x1 - branchg->branch.x2);
    RHeight(new) = abs(branchg->branch.y1 - branchg->branch.y2);
#endif

#ifdef DEBUG
    printf("Branch> Initialize req widget id = %d\n", request);
    printf("Branch> Initialize req window id = %d\n", XtWindow(request));
    printf("Branch> Initialize req Display id = %d\n", XtDisplay(request));
    printf("Branch> Initialize req parent window id = %d\n", XtWindow(XtParent(request)));
    printf("Branch> Initialize req parent display id = %d\n", XtDisplay(XtParent(request)));

    printf("Branch> Initialize new widget id = %d\n", new);
    printf("Branch> Initialize new window id = %d\n", XtWindow(new));
    printf("Branch> Initialize new Display id = %d\n", XtDisplay(new));
    printf("Branch> Initialize new parent window id = %d\n", XtWindow(XtParent(new)));
    printf("Branch> Initialize new parent display id = %d\n", XtDisplay(XtParent(new)));
#endif

} /* end Initialize */


/******************************************************************************\
* InitializeHook
* XtArgsFunc
*******************************************************************************/
static void 
InitializeHook(Widget widget, ArgList args, Cardinal *num_args)
{
    if(debug_flag == TRUE) {
       printf("Branch> InitializeHook gadget proc \n");
    }
} /* end InitializeHook */





/********************************************************************
* Realize 
* NOT USED in Gadget
*********************************************************************
*/
static
void Realize (Widget gw, XtValueMask valueMask, XSetWindowAttributes *attrs)
{
    printf("Branch> Realize widget\n");
} /* end Realize */


/********************************************************************
* Destroy 
* XtWidgetProc
* deallocates local and server memeory allocated by this widget.
* This is called when an application destroys a gadget but remains running
* see X Toolkit Intrinsic Programming Manual V4 ch 6
*
* note: free GC even if some gadgets left since the free
* does not really remove the GC from the server do to cacheing of
* similar GCs. (see X Toolkit Intrinsic Progamming Manual pg. 183)
*********************************************************************
*/
static void Destroy (Widget w)
{
    BranchGadget branchg = (BranchGadget) w;

    if(debug_flag == TRUE) {
      printf("Branch> Destroy widget\n");
    }

} /* end Destroy */  

/********************************************************************
* Resize 
* NOT USED in Gadget
*********************************************************************
*/
static void Resize (Widget gw)
{
  printf("Branch> Resize widget\n");
} /* end Resize */


/******************************************************************************\
* Redisplay or expose
* XtExposeProc (see /usr/include/X11/IntrinsicP.h for param list )
* redraws a widget whenever an Expose event arrives from the server
* makes Xlib calls to draw on the parents window 
* uses instance variables to guide in the redrawing of the object
* author: Dan Clark  Sep 09, 1991
*******************************************************************************/
static void Redisplay(Widget gw, XEvent *event, Region region)
{
    BranchGadget branchg = (BranchGadget) gw;
    XGCValues drawGCValues;
    /* XtGCMask drawGCMask = GCForeground | GCBackGround | GCLineStyle; */
    XtGCMask drawGCMask = 0;
    extern int	class_gc_init;
    static GC holdGC;
    static int width = 3;
    static int dash_offset = 0;
    static char dash_list[] = { 3, 4, 3, 1 };
    static int dash_list_length = (int)sizeof(dash_list)/sizeof(dash_list[0]);
    int tmpwidth, tmpheight;

    if(debug_flag == TRUE) {
        PrintBranchClassRec();
        PrintBranchRec(gw);
    };
/***
    printf("Branch> Redisplay Branch gadget\n");
    printf("Branch> Redisplay Branch gadget Event.type = %d\n", event->type);
    printf("Branch> Redisplay widget id = %d\n", gw);
    printf("Branch> Redisplay window id = %d\n", XtWindow(gw));
    printf("Branch> Redisplay Display id = %d\n", XtDisplay(gw));
    printf("Branch> Redisplay parent window id = %d\n", XtWindow(XtParent(gw)));
    printf("Branch> Redisplay parent display id = %d\n", XtDisplay(XtParent(gw)));
***/
/** shouldn't create a new gc for every redisplay of gadget **/
    if(class_gc_init != TRUE )
    {
        branchg->branch.drawGC = XCreateGC(XtDisplay(gw), XtWindow(gw), 
           drawGCMask, &drawGCValues);

        holdGC = branchg->branch.drawGC;
        class_gc_init = TRUE;
    } else {
       branchg->branch.drawGC = holdGC;
    };

    /* printf("BranchGadget->shape = %d\n", ((BranchGadget) gw)->branch.shape); */
#ifdef OLDMETHOD
    if(((BranchGadget) gw)->branch.orientation == 0) 
    {
      XDrawLine( XtDisplay(gw), XtWindow(gw), 
          branchg->label.normal_GC,
          /* branchg->branch.drawGC, */
          RX(gw), RY(gw), 
          RX(gw) + RWidth(gw), RY(gw) + RHeight(gw) );
    } else {
      XDrawLine( XtDisplay(gw), XtWindow(gw),
          branchg->label.normal_GC,
          /* branchg->branch.drawGC, */
          RX(gw), RY(gw) + RHeight(gw), 
          RX(gw) + RWidth(gw), RY(gw) );
    }
#endif
#ifdef DEBUG
    printf("Branch: (%dx%d) (%dx%d) dx (%dx%d)",
             branchg->branch.x1, branchg->branch.y1,
             branchg->branch.x2, branchg->branch.y2,
             branchg->branch.x1 - branchg->branch.x2,
             branchg->branch.y1 - branchg->branch.y2);
    printf("(%dx%d) (%dx%d)\n",
             branchg->rectangle.x, branchg->rectangle.y,
             branchg->rectangle.width, branchg->rectangle.height);
#endif
#ifdef DASH
   XSetDashes (XtDisplay(gw),
         branchg->pushbutton.cache->fill_gc, 
         dash_offset, dash_list, dash_list_length);
   XSetLineAttributes( XtDisplay(gw),
         branchg->pushbutton.cache->fill_gc, 
         width, 
         LineOnOffDash /* LineSolid */, CapNotLast, JoinBevel);
#else
   XSetLineAttributes( XtDisplay(gw),
         branchg->pushbutton.cache->fill_gc, 
         width, 
         LineSolid , CapNotLast, JoinBevel);
#endif
   XDrawLine( XtDisplay(gw), XtWindow(gw), 
          branchg->pushbutton.cache->fill_gc,
          branchg->branch.x1, branchg->branch.y1,
          branchg->branch.x2, branchg->branch.y2);

#ifdef RECTANGLE
#define max(A,B) ((A) > (B) ? (A) : (B))
          tmpwidth = max(branchg->rectangle.width - 1, 0);
          tmpheight = max(branchg->rectangle.height - 1, 0);

   XDrawRectangle( XtDisplay(gw), XtWindow(gw), 
          branchg->label.insensitive_GC,
          /* branchg->pushbutton.cache->background_gc, */
          branchg->rectangle.x, branchg->rectangle.y,
          tmpwidth, tmpheight);
#endif
/***
   XDrawRectangle( XtDisplay(gw), XtWindow(gw), 
          branchg->label.normal_GC,
          branchg->rectangle.x, branchg->rectangle.y,
          branchg->rectangle.width, branchg->rectangle.height);
***/

} /* end Redisplay */


/******************************************************************************\
* SetValues
* XtSetValuesFunc
* returns TRUE if redisplay needed
* called whenever application call XtSetValues to set gadget resources
* method recalculates private instance variables based on the
* new public instance variable values.
* similar to initialize but called at different and multiple times
* see X Toolkit Intrinsics Programming Manual V4 ch. 6
*******************************************************************************/
static Boolean SetValues (Widget gadget, Widget request, Widget new)
{
    BranchGadget branchg = (BranchGadget)new;

/***
    printf("Branch> SetValues gadget routine\n");
***/
    /*** set the gadget x,y,width,height and orientation ***/
    /*** based on the x1,y1 and x2,y2 set for the gadget ***/
    /*** RX(gw), RY(gw) + RHeight(gw), RWidth(gw) ***/
#ifdef OLDWAY
    if(branchg->branch.x1 < branchg->branch.x2) {
        RX(new) = branchg->branch.x1 + (int)(branchg->branch.xsize/2);
    } else {
        RX(new) = branchg->branch.x2 + (int)(branchg->branch.xsize/2);
    };

    if(branchg->branch.y1 < branchg->branch.y2) {
        RY(new) = branchg->branch.y1 + (int)(branchg->branch.ysize/2);
    } else {
        RY(new) = branchg->branch.y2 + (int)(branchg->branch.ysize/2);
    };
    if(((branchg->branch.x1 < branchg->branch.x2)&&
        (branchg->branch.y1 < branchg->branch.y2)) ||
        ((branchg->branch.x1 >= branchg->branch.x2)&&
        (branchg->branch.y1 >= branchg->branch.y2)) )
    {
        branchg->branch.orientation = 0;
    } else {
        branchg->branch.orientation = 1;
    }
    /* printf("Bustest branch_shape = %d\n", branch_shape); */
    RWidth(new) = abs(branchg->branch.x1 - branchg->branch.x2);
    RHeight(new) = abs(branchg->branch.y1 - branchg->branch.y2);
#endif

/***
  (*branchClassRec.rect_class.expose)(gadget, 0, 0);
   Redisplay(gadget, 0, 0);
***/
  return(TRUE);
} /* end SetValues */


/******************************************************************************\
* SetValuesHook
* XtArgsFunc
* Set resource values in subparts.
* This method is used only in widgets that have subparts (see sec 9.4)
* downward chained
* see X Toolkit Intrinsics Programming Manual V4 ch. 9
*******************************************************************************/
static Boolean SetValuesHook (Widget gadget, ArgList args, Cardinal *num_args)
{
/***
  printf("Branch> SetValuesHook gadget routine\n");
  Redisplay(gadget, 0, 0);
***/
} /* end SetValuesHook */




/******************************************************************************\
* SetValuesAlmost
* XtAlmostProc
*******************************************************************************/
static void SetValuesAlmost (Widget gadget, Widget new_gadget,
                     XtWidgetGeometry *request, XtWidgetGeometry *reply)
{
/***
  printf("Branch> SetValuesHook gadget routine\n");
  Redisplay(gadget, 0, 0);
***/
} /* end SetValuesHook */




/********************************************************************
* GetValuesHook
* XtArgsProc
* Return resources of subparts
* downward chained
* see X Toolkit Intrinsics Programming Manual V4 ch. 9
*********************************************************************
*/
static Boolean GetValuesHook (Widget gadget, ArgList args, Cardinal *num_args)
{
/***
  printf("Branch> GetValuesHook gadget routine\n");
  Redisplay(gadget, 0, 0);
***/
} /* end GetValuesHook */


/******************************************************************************\
* QueryGeometry
* XtGeometryHandler
*******************************************************************************/
static XtGeometryResult QueryGeometry(Widget gadget, 
                     XtWidgetGeometry *request, XtWidgetGeometry *reply)
{
/***
  printf("Branch> QueryGeometry gadget routine\n");
  Redisplay(gadget, 0, 0);
***/
} /* end SetValuesHook */





/*** Gadget Class methods	***/
/********************************************************************
* border_highlight
* XtWidgetProc
*********************************************************************
*/
static void border_highlight(Widget gw)
{
  /* printf("Branch> border_highlight gadget \n"); */

} /* end border_highlight */





/********************************************************************
* border_unhighlight
* XtWidgetProc
*********************************************************************
*/
static void border_unhighlight(Widget gw)
{
  printf("Branch> border_unhighlight gadget \n");
} /* end border_unhighlight */

/********************************************************************
* arm_and_activate 
* XtProc no parameters
*********************************************************************
*/
static void arm_and_activate()
{
  printf("Branch> arm_and_activate gadget routine\n");
} /* end arm_and_activate */


/******************************************************************************\
* input_dispatch 
* XtWidgetProc
* NOTE - listed as an XtWidgetProc but this is probably wrong
* since it returns events it is probably an XtExposeProc
* this means that the third parameter is Region data
* method for handling keyboard and mouse input events
* author: Dan Clark  Sep 09, 1991
*******************************************************************************/
static void 
input_dispatch(Widget gw, XEvent *event, caddr_t call_data)
{
    /* printf("Branch> input_dispatch proc gadget # %d\n", gw); */
    switch(event->type)  
    {
      case ButtonPress:
          /* printf("Branch gadget ButtonPress call callbacks implemented\n"); */
    /*       XtCallCallbacks(gw, XmNarmCallback, event); */
          Arm(gw, event); 

/*** working call of arm_and_activate class action routine ***/
/***
          printf("Branch> branchClassRec.gadget_class.arm_and_activate %d\n",
                 branchClassRec.gadget_class.arm_and_activate);
          (*branchClassRec.gadget_class.arm_and_activate)();
***/
      break;

      case ButtonRelease:
        /* printf("Branch gadget ButtonRelease not yet implemented\n"); */
          Arm(gw, event); 
        /* XtCallCallbacks(gw, XmNdisarmCallback, event); */
/***
       Redisplay(gw, 0, 0);
      (*branchClassRec.rect_class.expose)(gw, 0, 0);
***/
/***
        PrintBranchClassRec();
        PrintBranchRec(gw);
***/
      break;

      case MotionNotify:
        /* printf("Branch gadget MotionNotify \n"); */
      break;

      case EnterNotify:
        /* printf("Branch gadget EnterNotify \n"); */
      break;

      case LeaveNotify:
        /* printf("Branch gadget LeaveNotify \n"); */
      break;

      case FocusIn:
        /* printf("Branch gadget FocusIn \n"); */
      break;

      case FocusOut:
        /* printf("Branch gadget FocusOut \n"); */
      break;

      default:
        printf("Warning Branch gadget does not handle Event = %d\n", event->type);
      break;
    };

/***
  printf("Branch> input_dispatch window id = %d\n", XtWindow(gw));
  printf("Branch> input_dispatch parent window id = %d\n", XtWindow(XtParent(gw)));
***/
} /* end input_dispatch */

static void Arm (pb, event)
XmPushButtonGadget pb;
XEvent *event;

{
   XmPushButtonCallbackStruct call_value;

   PBG_Armed(pb) = TRUE;

   Redisplay(pb, event, FALSE);

   if (PBG_ArmCallback(pb))
   {
      XFlush(XtDisplay (pb));

      call_value.reason = XmCR_ARM;
      call_value.event = event;
      XtCallCallbackList ((Widget) pb, PBG_ArmCallback(pb), &call_value);
   }
}


/******************************************************************************\
* visual_change 
* XmVisualChangeProc
*******************************************************************************/
static Boolean 
visual_change(XmGadget child, XmManagerWidget current, XmManagerWidget new)
{
  printf("Branch> visual_change gadget \n");
} /* end visual_change */

/********************************************************************
* set_override
* XtProc no parameters
*********************************************************************
*/
static void set_override()
{
  printf("Branch> set_override gadget\n");
} /* end set_override */

/********************************************************************
* set_which_button
* XtProc no parameters
*********************************************************************
*/
static void set_which_button()
{
  printf("Branch> set_which_button gadget \n");
} /* end set_which_button */


/******************************************************************************\
* PrintBranchRec
* print many but not all of the fields in the branch record.
* to determine all the fields must reference the header files for 
* the particular object.
* ObjectPart	- /usr/include/X11/ObjectP.h
* RectObjPart	- /usr/include/X11/RectObjP.h
* XmGadgetPart	- /usr/lib/DXM/Xm/XmP.h
* XmLabelGadgetPart	- /usr/lib/DXM/Xm/LabelGP.h
* XmPushButtonGadgetPart- /usr/lib/DXM/Xm/PushBGP.h
*
* Known Bugs:
* - complex structures are not broken down into components
*  need to further specify data fields such as XtCallbackList 
*  and _XmLabel
* - The callback list is a list structure which would need
*  to be walked to truly print out the fields. 
*******************************************************************************/
void PrintBranchRec(Widget gw)
{
  printf("Branch> ObjectPart data => (BranchGadget) gw)->object.field \n");
  printf("Branch> ptr to itself (Widget) (BranchGadget) gw->object.self %d\n",
        ((BranchGadget) gw)->object.self);
  printf("Branch> ptr to class (WidgetClass) object.widget_class %d\n",
        ((BranchGadget) gw)->object.widget_class);
  printf("Branch> parent (Widget) object.parent %d\n",
        ((BranchGadget) gw)->object.parent);
  printf("Branch> resource name quark (XrmName) object.xrm_name %d\n",
        ((BranchGadget) gw)->object.xrm_name);
/***
  printf("Branch> marked for destroy (Boolean) object.being_destroyed %d\n",
        ((BranchGadget) gw)->object.being_destroyed);
  printf("Branch> destry callbacks (XtCallbackList) object.destroy_callbacks %d\n",
        ((BranchGadget) gw)->object.destroy_callbacks);
***/
  printf("Branch> constraint record (caddr_t) object.constraints %d\n",
        ((BranchGadget) gw)->object.constraints);

  printf("Branch> RectObjectPart data => (BranchGadget) gw)->rectangle.field \n");
  printf("Branch> rectangle position (Position) rectangle.x, y  %d,%d\n",
        ((BranchGadget) gw)->rectangle.x, ((BranchGadget) gw)->rectangle.y);

  printf("Branch> XmGadgetPart (XmP.h) => (BranchGadget) gw)->gadget.field \n");
  printf("Branch> (short) gadget.shadow_thickness  %d\n",
        ((BranchGadget) gw)->gadget.shadow_thickness);
  printf("Branch> (short) gadget.highlight_thickness %d\n",
        ((BranchGadget) gw)->gadget.highlight_thickness);
  printf("Branch> (Boolean) gadget.traversal_on %d\n",
        ((BranchGadget) gw)->gadget.traversal_on);
  printf("Branch> (Boolean) gadget.highlight_on_enter %d\n",
        ((BranchGadget) gw)->gadget.highlight_on_enter);
  printf("Branch> (Boolean) gadget.have_traversal %d\n",
        ((BranchGadget) gw)->gadget.have_traversal);
  printf("Branch> (Mask) gadget.event_mask %d\n",
        ((BranchGadget) gw)->gadget.event_mask);

  printf("Branch> XmLabelGadgetPart => (BranchGadget) gw)->label.field \n");
  printf("Branch> string sent to widget (_XmString) label._label %s\n",
        ((BranchGadget) gw)->label._label);
  printf("Branch> (Pixmap) label.pixmap %d\n",
        ((BranchGadget) gw)->label.pixmap);
  printf("Branch> (Pixmap) label.pixmap_insen %d\n",
        ((BranchGadget) gw)->label.pixmap_insen);

  printf("Branch> XmPushButtonGadgetPart => (BranchGadget) gw)->pushbutton.field \n");

/***
  printf("Branch> (short) pushbutton.show_as_default %d\n",
        ((BranchGadget) gw)->pushbutton.show_as_default);
  printf("Branch> (Boolean) pushbutton.fill_on_arm %d\n",
        ((BranchGadget) gw)->pushbutton.fill_on_arm);
  printf("Branch> (Pixel) pushbutton.arm_color %d\n",
        ((BranchGadget) gw)->pushbutton.arm_color);
  printf("Branch> (Pixmap) pushbutton.arm_pixmap %d\n",
        ((BranchGadget) gw)->pushbutton.arm_pixmap);
***/
/*** core
  printf("Branch> (XtCallbackList) pushbutton.arm_callback->callback %d\n",
        ((BranchGadget) gw)->pushbutton.arm_callback->callback);
  printf("Branch> *(XtCallbackList) pushbutton.arm_callback->callback %d\n",
        *((BranchGadget) gw)->pushbutton.arm_callback->callback);
  printf("Branch> (XtCallbackList) pushbutton.arm_callback->closure %d\n",
        ((BranchGadget) gw)->pushbutton.arm_callback->closure);
  printf("Branch> (Boolean) pushbutton.armed %d\n",
        ((BranchGadget) gw)->pushbutton.armed);
  printf("Branch> (Pixmap) pushbutton.unarm_pixmap %d\n",
        ((BranchGadget) gw)->pushbutton.unarm_pixmap);
  printf("Branch> (GC) pushbutton.fill_gc %d\n",
        ((BranchGadget) gw)->pushbutton.fill_gc);
  printf("Branch> (GC) pushbutton.background_gc %d\n",
        ((BranchGadget) gw)->pushbutton.background_gc);
  printf("Branch> (int) pushbutton.timer %d\n",
        ((BranchGadget) gw)->pushbutton.timer);
***/

} 
/* end PrintBranchRec */


/******************************************************************************\
* PrintBranchClassRec
* print some of the class field records.
* the class data structure is initialized in the Branch.c file for the
* branch gadget.  The field names are also derived from the header
* files for each object type (see the function PrintBranchRec).
* Only the RectObjClassPart is specified since the
* core class, object class, and rectangle object class record all
* share the same area of storage and data layout.
* there is only one dummy field for the XmPushButtonGadgetClassPart
*******************************************************************************/
void PrintBranchClassRec()
{
  printf("Branch> RectObjClassPart => branchClassRec.rect_class.field\n");
  printf("Branch> (String) rect_class.class_name %s\n",
         branchClassRec.rect_class.class_name);
  printf("Branch> (Cardinal) rect_class.widget_size %d\n",
         branchClassRec.rect_class.widget_size);
  printf("Branch> (XtVersionType) rect_class.version %d\n",
         branchClassRec.rect_class.version);

  printf("Branch> XmGadgetClassPart => branchClassRec.gadget_class.field \n");
  printf("Branch> (XtProc) gadget_class.arm_and_activate %d\n",
         branchClassRec.gadget_class.arm_and_activate);

  printf("Branch> XmLabelGadgetClassPart => branchClassRec.label_class.field \n");
  printf("Branch> (XtProc) label_class.setOverrideCallback %d\n",
         branchClassRec.label_class.setOverrideCallback);

} 
/* end PrintBranchClassRec */

/*** NO ACTIONS in Gadget	***/
/***
static void DrawCell()
{
  printf("Branch> DrawCell widget\n");
}

static void UndrawCell()
{
  printf("Branch> UndrawCell widget\n");
}

static void ToggleCell()
{
  printf("Branch> ToggleCell widget\n");
}
***/

/* end Branch.c */
