static char sccsid[] = "@(#)vertex.c	20.49 5/19/98";
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
/*****************************************************************************
* file: vertex.c
* author: Dan Clark
* purpose: handle most functions related to graph vertices
*
******************************************************************************
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <Xm/Xm.h>
#include <X11/Xlib.h>
#include <Xm/PushBG.h>
#include <Xm/TextF.h>
#include <Xm/Text.h>

#include "dmgr.h"
#include "graph_data.h"
#include "ipfdebug.h"
#include "drawgadget.h"

extern double zoom_factor;      /* from (toolbox.c) */
extern int icon_bus_width;
extern int icon_bus_height;
extern int bus_icon_key;
extern int show_solution_icon;
extern char *BusIcon2Widgets[];
extern char *BusIconWidgets[];

static int private_color_limit = 41;

extern Widget lookup_kv_wid	( double basekv);
extern Widget widget_id		( char * widget_name);
extern int gdgt_ge_x_offset	( GraphElement *ge );
extern int gdgt_ge_y_offset	( GraphElement *ge );
extern int motif2cstring(XmString xstr, char **str);
extern void get_box_size_of_string ( char *str, int *width, int *height );
extern roundoff			( double fnum );
extern void fetch_widget	( char *wname );
extern char *busNameStr         ( GraphElement *ge );

void drawCharsX			( Widget wid, char *string, int x, int y,
				  double size );
void drawletter1		( Widget wid, double xf, double yf,
				  char c, double size );
void   drawAreaButtonCB		( Widget w, GraphElement *client_data,
				  XmPushButtonCallbackStruct *call_data);
Pixmap createGadgetPixmap	( Widget draw_window_widget_id);
Widget createBusGadget 		( char *name, GraphElement *pnode);
Widget createStringGadget	( GraphElement *pgraphvertex, char *str );
Widget createPtGadget		( GraphElement *pgraphvertex);
Widget createPt1Gadget		( GraphElement *pgraphvertex);
int    manageVertex		( GraphElement *pgraphvertexnxt);
void position_gadget_wrt	( GraphElement *vertex );
void get_str_box_pixel_size	( char *str, int *wd, int *ht );
void change_bus_icon_bg		( unsigned long bkgrd, unsigned long frgrd );
void drawTextCB			( Widget wid, XtPointer tag, XtPointer cb);


#define DRAW_BORDER 15       /* NOTE: must match SCRN_BORDER in graph_data.h */
#define max(A,B) ((A) > (B) ? (A) : (B))





void createVertexGadgets (Widget wid, XtPointer tag, XmAnyCallbackStruct *cbs)
/*********************************************************************\
* Purpose: Create all bus gadgets.  Done only once for a particular
*	   coordinate file.  Before creating a new set all bus gadgets
*	   from previous coordinate file should destroyed. 
*		 (it is possible to write
*	logic to re-use the gadgets from the previous coordinate file if
*	performance is to slow with the destroy method).
*
*	Author: Dan Clark	Last change: 	Sep 27, 1991
*	modby: Dan Clark	Jan 29
*		adapt to graphics system based on dmgr
*       modby: Dan Clark	Nov 13, 1991
*        added logic for creating bitmap for gadget 
* 
* Called by:  buildMainDisplay	(graphdata.c)  (from VIEW/MERGE/ cascade pdn)
*
\*********************************************************************/
{
  Widget draw_window_widget_id;
  GraphElement *pgraphvertexnxt;
  DB_STAT stat;

  if(( draw_window_widget_id = widget_id( "drawwindow" )) == FALSE) return;

/*
* Loop through all buses and create gadgets for the buses that 
* meet the drawing criteria which is:
*   a) the bus is ok to draw (not deleted and merged with base data)
*   b) the display flag is set to '1'
*/

  stat = firstGE( &pgraphvertexnxt, GraphTypeVertex );

  if (stat == D_EOF) {
    return;		 /* EOF no valid records in graph_db */
  }

  if (stat != D_FOUND) {
         stat = D_FOUND; }	 /* db_search set at first base records */

  while(stat == D_FOUND )
  {
    if( pgraphvertexnxt->type    != GraphTypeVertex  ||
        pgraphvertexnxt->display != GraphDisplayRequestOn )
    {
      break;	/*  force end of vertex list */
    }

    manageVertex(pgraphvertexnxt);
printGraphElement( pgraphvertexnxt );
    stat = nextGE(&pgraphvertexnxt, GRAPH_DISPLAY_TYPE); /* next vertex */

  }
}


int manageVertex(GraphElement *pgraphvertexnxt)
/***************************************************************************\
* purpose:   Create a vertex widget and make it appear on screen.
*		(call correct routine depending on Class)
*
* author: Dan Clark 
*
* Called by: turnOnSubedge,   process_pf_connections(2)      (graphdata.c)
*	pscreateStringVertex, create_and_manage_coord_vertex (graphpscor.c)
*	explodeBus (pf_cb.c)
*	group_all_objects, addGraphBusXY, addGraphBranch, breakEdge (toolbox.c)
*	createVertexGadgets (this module)
\***************************************************************************/
{
  Widget gadget_id;
  Widget draw_window_widget_id;
  int wd,ht;  /* ber: WAS Dimension */

  GraphElement graphvertex;
  GraphElement *pgraphvertex;

  DB_STAT stat_disp;

  char	str[13];

  if( widget_id( "drawwindow" ) == FALSE) { return -1; }

  switch(pgraphvertexnxt->class)
  {
      case GraphClassVertexBus:
      case GraphClassVertexGenerator:
      case GraphClassVertexReactor:
      case GraphClassVertexCapacitor:
        sprintf(str, "%12.12s", pgraphvertexnxt->vertex_id); 
        gadget_id = (Widget)createBusGadget( str, pgraphvertexnxt ); 
      break;

      case GraphClassVertexName:
	strcpy( str, busNameStr( pgraphvertexnxt ) );
        gadget_id = (Widget)createStringGadget( pgraphvertexnxt, str );
      break;

      case GraphClassVertexGroup:
        sprintf(str, "%12.12s", pgraphvertexnxt->vertex_id); 
        gadget_id = (Widget)createStringGadget( pgraphvertexnxt, str );
      break;

      case GraphClassVertexBendPoint:
	gadget_id = createPtGadget( pgraphvertexnxt );
      break;

      case GraphClassVertexDrawPoint:
      case GraphClassVertexBorder:
      case GraphClassPaperVertex:
      case GraphClassVertexLegendCorner:
      case GraphClassVertexLabelCorner:
	gadget_id = createPt1Gadget( pgraphvertexnxt );
      break;

      case GraphClassVertexComment:
      case GraphClassVertexSubcomment:
      case GraphClassVertexOrgComment:
      case GraphClassVertexFont:
      case GraphClassVertexLegendText:
      case GraphClassVertexLabelText:
        sprintf( str, "Temp_Str");
	gadget_id = (Widget)createStringGadget ( pgraphvertexnxt, str );
      break;

      default:
        printf("Info: From manageVertex (vertex.c)\n   Graph element Vertex class %d not yet implemented\n", pgraphvertexnxt->class);
        gadget_id = 0;
      break;
  }

    /** update coord record with display flag turned on **/
  pgraphvertex	  = &graphvertex;
  memcpy(pgraphvertex,pgraphvertexnxt, sizeof(GraphElement));
  pgraphvertex->wid	= gadget_id;
  pgraphvertex->display = GraphDisplayOn;
  db_update(&graph_db, &pgraphvertexnxt, &pgraphvertex);

  position_gadget_wrt( pgraphvertex );	/* remember - do this after
					   updating pgraphvertex->wid */
}


void position_gadget_wrt(GraphElement *vertex)
/***************************************************************************\
* author: John Rutis
* purpose: snap the graphics to correspond to the graph element data
* sturcture 
*
*	Called by: rescale_graph (graphpscor.c)
*		   
****************************************************************************/
{
  Position x, y;

  if(GraphDisplayOn != vertex->display) return;

  if(NULL == vertex->wid)
  {
    printf("vertex.c: Error: Display is on but widget id is NULL!\n");
    return;
  }

  x = (Position)( vertex->x - gdgt_ge_x_offset( vertex ));
  y = (Position)( vertex->y - gdgt_ge_y_offset( vertex ));

  x = max( x, DRAW_BORDER );	/* don't go off the edge of the screen! */
  y = max( y, DRAW_BORDER );

  XtVaSetValues(vertex->wid, XmNx, x, XmNy, y, NULL);
}


Widget createBusGadget ( char *name, GraphElement *pgraphvertex)
/*****************************************************************************\
* Purpose:    Makes a bus, bus label, gen or reac that's a push-button widget
* Author: John Rutis	
*    Modified by:	Bill Rogers 		Oct 18, 1991
*	rewritten: Dan Clark	Oct 19, 1993
* Called by:   manageVertex
******************************************************************************/
{
  Widget toolbox_wid = 0;
  Pixmap toolpix;

  Widget  Push1_widget, draw_window_widget_id;
  Pixel   color;

  static char gadget_name[256];
  static int gadget_cnt = 0;
  int x_radius, y_radius;

  draw_window_widget_id = widget_id( "drawwindow" );

  switch( pgraphvertex->class ) /* find correct pixmap for bus icon */
  {
    case GraphClassVertexBus:

     if( pgraphvertex->hide == HIDE )
       toolbox_wid = widget_id("undisplayed_bus_icon" );

     else
       toolbox_wid = widget_id( "bus_legend_bus_pixmap" );
       x_radius = icon_bus_width;
       y_radius = icon_bus_height;
    break;

    case GraphClassVertexGenerator:
      toolbox_wid = widget_id( "bus_legend_gen_pixmap" );
      x_radius = GEN_RADIUS;
      y_radius = GEN_RADIUS;
    break;

    case GraphClassVertexCapacitor:
      toolbox_wid = widget_id( "bus_legend_cap_pixmap" );
      x_radius = 5;
      y_radius = 5;
    break;

    case GraphClassVertexReactor:
      toolbox_wid = widget_id( "bus_legend_cap_pixmap" );
      x_radius = 4;
      y_radius = 12;
    break;

    default:
      printf("ERROR in createBusGadget (vertex.c) not bus, gen or reac\n");
      toolbox_wid = 0;
  }

  if ( toolbox_wid  == FALSE)
  { 
      XmString x_str;

      printf("ERROR in createBusGadget (vertex.c) can't find icon, labeling vertex with text\n");

/*      sprintf(gadget_name, "s%d", gadget_cnt++);*/
      x_str =  XmStringCreate(name, XmSTRING_DEFAULT_CHARSET);

      Push1_widget = XtVaCreateManagedWidget( gadget_name,
	        xmPushButtonGadgetClass, draw_window_widget_id,
		XmNx,			pgraphvertex->x -x_radius,
		XmNy,			pgraphvertex->y -y_radius,
		XmNhighlightThickness,	0,
		XmNshadowThickness,	0,
		XmNmarginHeight,	0,
		XmNmarginWidth,		0,
		XmNfillOnArm,		False,
		XmNlabelType,		XmSTRING,
		XmNlabelString,		x_str,
                XmNuserData,		(GraphElement *)pgraphvertex,
		NULL);

    XtAddCallback(Push1_widget, XmNarmCallback,
                  (XtCallbackProc)drawAreaButtonCB, pgraphvertex);

    return Push1_widget;
  }
    /*** use pixmap from toolbox ***/
  XtVaGetValues(toolbox_wid, XmNlabelPixmap, &toolpix, NULL); 

/*  sprintf(gadget_name, "b%d", gadget_cnt++);	"b" for bus icon */

  Push1_widget = XtVaCreateManagedWidget( gadget_name,
		xmPushButtonGadgetClass, draw_window_widget_id,
		XmNx,			pgraphvertex->x -icon_bus_width,
		XmNy,			pgraphvertex->y -icon_bus_height,
		XmNhighlightThickness,	0,
		XmNshadowThickness,	0,
		XmNmarginHeight,	0,
		XmNmarginWidth,		0,
		XmNfillOnArm,		False,
		XmNlabelType,		XmPIXMAP,
		XmNlabelPixmap,		toolpix,
                XmNuserData,		(GraphElement *)pgraphvertex,
		NULL);

  XtAddCallback(Push1_widget, XmNarmCallback,
                  (XtCallbackProc)drawAreaButtonCB, pgraphvertex);

  return Push1_widget;
} 



static GC gc_local = NULL;	/* this is the BUS lettering color */
void getforegroundGC( Widget wid )
/**************************************************************************\
* Purpose: Find the correct value for black and white.
*	   (On unix color #0 is white, #1 is black )
*	   (On Vms  color #0 is black, #1 is white )
*
* Called by: drawTextCB
\**************************************************************************/
{
  XGCValues values;
  XtGCMask valueMask;
  Widget  mw;
  XColor  color;   /*used to extract pixel from colorname */
  Colormap wmap;
  Display *dpy;
  int     depth; 
  Visual *visual;
  int     share_limit = 140;

  valueMask = GCForeground | GCBackground;
  XtGetGC( wid, valueMask, &values);

/*dpy   = XtDisplay(XtParent(wid));
  depth = DefaultDepth( dpy, 0 );

  printf("                DEPTH is %d ( %d colors)\n", depth, 1<<depth );
  printf("Your terminal supports %d colors.\n", 1<<depth ); */

  mw = (Widget) XtParent(wid);

  gc_local = XtGetGC( mw, valueMask, &values);	/* get copy of graphic contnt */

  XtVaGetValues( widget_id("drawwindow"), XmNforeground, &color.pixel, NULL );

  /* My scheme to prevent private colors - so it can be edited later   */
  /* Bump the RED value up or down to create a new, non-private color */
  /* must be UN-SHARED color i.e. bus letters MUST have color of their own */

  if( color.pixel <= share_limit )	/* Need method to get share_limit */
  {
    int adj = 256;
    wmap = DefaultColormapOfScreen( XtScreen( mw )); /* get ptr to colormap */
    XQueryColor( XtDisplay( mw ), wmap, &color );    /* get RGB values	    */
    while( color.pixel <= share_limit )
    {
      if( color.red > 65000 )  adj = -adj;	/* try decreasing search */
      if( color.red < 0 )
      {
        printf("** WARNING ** BUSNAME color is uneditable!\n" );
        break;
      }
      color.red += adj;	 		/* try another value for red */
      XAllocColor( XtDisplay( mw ), wmap, &color );
    }
    XtVaSetValues( widget_id("drawwindow"), XmNforeground, color.pixel, NULL);
  }

  /* NOTE: this is where BUS NAME lettering color is SET ! */
  values.foreground = color.pixel;

  XChangeGC( XtDisplay(wid), gc_local, GCForeground, &values );

  /* Ensure BG/FG legend pushbutton is set to correct colors */
  /*** User may want to edit the colors - and they ought to be set correct! ***/
  XtVaSetValues( widget_id("kv_dc_label"), XmNforeground,  color.pixel, NULL );
  XtVaGetValues( widget_id("drawwindow" ), XmNbackground, &color.pixel, NULL );
  XtVaSetValues( widget_id("kv_dc_label"), XmNbackground,  color.pixel, NULL );
}


void change_bus_icon_bg( unsigned long bkgrd, unsigned long frgrd )
/**************************************************************************\
* Purpose: change bus widget background color
*		DOES NOT WORK - YET!
*
* Called by: adjust_color (color_edit.c)
\**************************************************************************/
{
  XGCValues values;
  XtGCMask valueMask;

  valueMask = GCForeground | GCBackground;

  values.background = bkgrd;
  values.foreground = frgrd;

  XChangeGC( XtDisplay(widget_id("kv_dc_label")), gc_local,
		 valueMask, &values );

}

void drawTextCB( Widget wid, XtPointer tag, XtPointer cb)
/****************************************************************************\
* Purpose: to redraw any string type widget
*
* Called by: Expose of any string type widget
\****************************************************************************/
{
  XidrawgadgetCallbackStruct *bcb = (XidrawgadgetCallbackStruct*)cb;
  XExposeEvent *exev;
  XmString xstr;
  char *str;
  int width, height;
			
  if(gc_local == NULL)  	/* done only once */
    getforegroundGC(XtParent(wid));

  XtVaGetValues(wid, XmNlabelString, &xstr, NULL);
  motif2cstring(xstr, &str);
/*  printf("drawTextCB wid str(%s) zoom(%lf)  ", str, zoom_factor ); */
/*   printf("drawTextCB wid str(%s)\n", str ); */
  drawCharsX( wid, str, bcb->x, bcb->y, zoom_factor ); 

/*  printf(" x(%d) x(%d) wd(%d) ht(%d)\n", 
           	bcb->x, bcb->y, bcb->width-1, bcb->height-1); */
  if(bcb->event->type == Expose)
  {
    exev = (XExposeEvent *)bcb->event;

    get_str_box_pixel_size( str, &width, &height );
    bcb->width = width;
    bcb->height= height;

    XtVaSetValues( wid,
                   XmNwidth , bcb->width,
                   XmNheight, bcb->height,
                   NULL );

/* For Debugging:
    XDrawRectangle(exev->display, exev->window, gc_local,
           bcb->x, bcb->y, bcb->width-1, bcb->height-1); */
  }
}




Widget createStringGadget ( GraphElement *pgraphvertex, char *str )
/***************************************************************************\
* Purpose: create the "expose" label gadget from the name, solution, label or
*	   comment graphelment
*	   Note that the expose causes call to drawTextCB which draws
*		in the main window. (It does not draw in the expose gadget!).
*
* Called by: manageVertex
\***************************************************************************/
{
  Widget draw_window_widget_id, Push1_widget;
  XmString x_str;
  double basekv;
  Pixel color;
  Pixmap color_pixmap;
  static char gadget_name[256];
  static int gadget_cnt = 0;

  int width;
  int height;
  
  draw_window_widget_id = widget_id( "drawwindow" );

  get_str_box_pixel_size( str, &width, &height );

  sprintf(gadget_name, "s%d", gadget_cnt++);	/* "s" for string */
  x_str =  XmStringCreate( str, XmSTRING_DEFAULT_CHARSET);

	/* create the "expose" (or pushbutton) gadget */
  Push1_widget = XtVaCreateManagedWidget( gadget_name,
		xmDrawGadgetClass,	 draw_window_widget_id,
		XmNx,		 (Position)pgraphvertex->x,
		XmNy,		 (Position)pgraphvertex->y,
		XmNwidth,	 (Dimension)width,
		XmNheight,	 (Dimension)height,
		XmNlabelString,	 (XmString)x_str,
		XmNlabelType,	 XmSTRING, 
		XmNuserData,	 (GraphElement *)pgraphvertex,
		NULL);

  XtAddCallback(Push1_widget, XiNbuttonCallback,
                  (XtCallbackProc)drawAreaButtonCB, pgraphvertex);

  XtAddCallback(Push1_widget, XiNdrawCallback,
                  (XtCallbackProc)drawTextCB, pgraphvertex);

  return Push1_widget;

}


void get_str_box_pixel_size( char *str, int *wd, int *ht )
/**************************************************************************\
* Purpose: return the PIXEL size of a character string
*
* Author: Bill Rogers	1994
*
* Called by: createStringGadget
\**************************************************************************/
{
#define CHAR_HT 7	/* these are in EdgeGp.h - but its defined too late */
#define CHAR_WD 5
#define H_CHR_SP 1
#define V_CHR_SP 2
#define MRGN 1          /* must match value in pscordat.c! */

  int char_wd;
  int char_hi;

  get_box_size_of_string( str, &char_wd, &char_hi );

  *wd = zoom_factor * ((CHAR_WD +H_CHR_SP) * char_wd - H_CHR_SP + 2*MRGN );
  *ht = zoom_factor * ((CHAR_HT +V_CHR_SP) * char_hi - V_CHR_SP + 2*MRGN );

}





Widget createPtGadget ( GraphElement *pgraphvertex)
/***************************************************************************\
* Purpose:
* create a small gadget for the bend, draw or paper corner data
*
* Called by: manageVertex
\***************************************************************************/
{
  XmString x_str;

  Widget	Push1_widget, draw_window_widget_id;
  Pixmap	pt_pix;
  static char	gadget_name[256];

  draw_window_widget_id = widget_id( "drawwindow" );

    /*** use pixmap from vuit ***/
  XtVaGetValues(widget_id("pt_pb"), XmNarmPixmap, &pt_pix, NULL);
 
  Push1_widget = XtVaCreateManagedWidget( gadget_name,
			xmPushButtonGadgetClass, draw_window_widget_id,
			XmNx,		 pgraphvertex->x-PT_RADIUS,
			XmNy,		 pgraphvertex->y-PT_RADIUS,
			XmNwidth,	(int)(5),
			XmNheight,	(int)(5),

			XmNhighlightThickness, 0,
			XmNshadowThickness, 0,
                        XmNmarginWidth, 0 ,
			XmNborderWidth, 0,

			XmNlabelType,	XmPIXMAP,
			XmNlabelPixmap, pt_pix,

                        XmNuserData, (GraphElement *)pgraphvertex,
			NULL);

  XtAddCallback(Push1_widget, XmNarmCallback,
                  (XtCallbackProc)drawAreaButtonCB, pgraphvertex);

  return Push1_widget;

}


Widget createPt1Gadget ( GraphElement *pgraphvertex)
/***************************************************************************\
* Purpose: (NOTE the "1" in the name) Similiar to createPtGadget except
*	create a smaller gadget for paper corner data
*
* Called by: manageVertex
\***************************************************************************/
{
  XmString x_str;

  Widget	Push1_widget, draw_window_widget_id;
  Pixmap	pt_pix;
  static char	gadget_name[256];

  draw_window_widget_id = widget_id( "drawwindow" );

    /*** use pixmap from vuit ***/
  XtVaGetValues(widget_id("pt1_pb"), XmNarmPixmap, &pt_pix, NULL);
 
  Push1_widget = XtVaCreateManagedWidget( gadget_name,
			xmPushButtonGadgetClass, draw_window_widget_id,
			XmNx, pgraphvertex->x-PT1_RADIUS,
			XmNy, pgraphvertex->y-PT1_RADIUS,
			XmNwidth, (int)(3),
			XmNheight,(int)(3),

			XmNhighlightThickness, 0,
			XmNshadowThickness, 0,
                        XmNmarginWidth, 0 ,
			XmNborderWidth, 0,

			XmNlabelType, XmPIXMAP,
			XmNlabelPixmap, pt_pix,

                        XmNuserData, (GraphElement *)pgraphvertex,
			NULL);

  XtAddCallback(Push1_widget, XmNarmCallback,
                  (XtCallbackProc)drawAreaButtonCB, pgraphvertex);

  return Push1_widget;

}


#include "EdgeGP.h"
int findCharIDX(char c);



void drawCharsX( Widget wid, char *string, int x, int y,
                         double size )
/*****************************************************************************\
*       Purpose: draw string of characters - any size
*
*       Author: Wm E. Rogers    Jan 1995
*
*       Called by:
\******************************************************************************/
{
  int len;
  int i;
  double xp, yp, org_x, org_y;

  x += size * MRGN;
  y += size * (CHAR_HT-1+MRGN);		/* shift from gadget's top lf corner */

  xp = (double)(x + 2*size);		/* shift to adjust for char centering */
  yp = (double)(y - 3*size);
  org_x = xp;
  org_y = yp;

  len = strlen( string );
  /* get_box_size_of_string( string, &longest_str, &junk ); */

  for ( i=0; i<len; i++ )
  {
    if( string[i]=='\n' )       /* start new line */
    {
      yp += (V_CHR_SP+CHAR_HT)*size;
      xp = org_x;
    }
    else
    {
      drawletter1( wid, xp, yp, string[i], size );
      xp += (CHAR_WD+H_CHR_SP)*size;
    }
  }

}



int findCharIDX(char c)
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


void drawletter1( Widget wid, double xf, double yf, char c, double size )
/*****************************************************************************\
* Purpose: draw a single letter - plot data from EdgeGP.h 
*
\*****************************************************************************/
{
  int i=0;
  double xt, yt;
  int xti,yti,xfi,yfi;
  int id;

  xt = xf;
  yt = yf;
  i = 0;

  if( (id = findCharIDX( c )) <0 ) return;

  while ( mypts[id].x[i] != 99 )
  {
    if ( i >= MAXPT )
    {  id++;
       i = 0;
    }
    if ( abs( mypts[id].x[i]) < 100 )
    {
      xf = xt;                  /* from here */
      yf = yt;
      xt = xt + (double)(mypts[id].x[i])*size;
      yt = yt + (double)(mypts[id].y[i])*size;
      xti = roundoff(xt);
      xti = max(xti, DRAW_BORDER);
      yti = roundoff(yt);
      yti = max(yti, DRAW_BORDER);
      xfi = roundoff(xf);
      xfi = max(xfi, DRAW_BORDER);
      yfi = roundoff(yf);
      yfi = max(yfi, DRAW_BORDER);
      XDrawLine( XtDisplay(wid), XtWindow(wid), gc_local,
          xfi, yfi, xti, yti );

    }
    else
    {                           /* advance relative - (no draw) */
      if( mypts[id].x[i] < 0 )
      {
        xt = xt + (mypts[id].x[i]+100)*size;

        yt = yt +  mypts[id].y[i]     *size;
      }
      else
      {
        xt = xt + (mypts[id].x[i]-100)*size;
        yt = yt +  mypts[id].y[i]     *size;
      }
    }
    i++;
  }

}

