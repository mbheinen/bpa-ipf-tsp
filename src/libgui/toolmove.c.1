static char sccsid[]="@(#)toolmove.c	1.11 2/22/96";
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
* file: toolmove.c
* author: Dan Clark
* purpose: functions to manipulate graph data
*
****************************************************************************
*/
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>
#include <Xm/Xm.h>

#include "EdgeG.h"
#include "dmgr.h"
#include "graph_data.h"
#include "line2rect.h"
#include "ipfdebug.h"
#include "chgdata.h"

extern Widget widget_id		( char *widget_name);
extern updateEdge		( GraphElement *pedge);
extern void circlePerimeter	( double radians, int size, int *x, int *y );
extern double radiansBetweenPts	( int x, int y, int x2, int y2 );
extern void position_gadget_wrt	( GraphElement *vertex );
extern int chg_push		( CHANGES *pchgnew );
extern void display_coord_data  ( GraphElement *vertex );
extern int gdgt_ge_x_offset     ( GraphElement *ge );
extern int gdgt_ge_y_offset     ( GraphElement *ge );


void setobject		( GraphElement *ge );
GraphElement *getobject	( );
Boolean no_object_set	( );
void start_rubberband	( XEvent *pevent );
void drag_rubberband	( Widget w, XtPointer data, XEvent *pevent);
void stop_rubberband	( Widget w, XtPointer data, XEvent *pevent );
void draw_outline	( GraphElement *object );
void background_object	( GraphElement *object );
void update_location_of_ge_and_attachments( GraphElement *object,
			 int xdelta, int ydelta);
void draw_grid		( int xorg, int yorg, int w, int h, int size );
void center_vtx_if_needed( int x, int y, Widget wid, GraphClass class );

static GC xGC;		/* graphics context for drag image */
static GC xorGC;	/* graphics context to draw/undraw drag image */
static Widget draw_id;	/* id of draw area widget */
static Display *dpy;
static Window w;

#define MISS_DIST 4

static int x_cursor;
static int y_cursor;
static GraphElement *object = NULL;

int xorg   = 0;
int yorg   = 0;
int xdelta, ydelta;


void setobject( GraphElement *ge )
/*********************************************************************\
* Purpose:  to set the current object which has been selected
*	      there is only one current object.  
* author: Dan Clark
*
* Called by: drawAreaButtonCB (13 calls) (toolbox.c)
*	     addGraphBranch  (1)
*	     addDrawLine     (1)
*	     breakEdge       (1)
\**********************************************************************/
{
  object = ge;
}




GraphElement *getobject()
/**************************************************************************\
* purpose: return the object set by setobject
\**************************************************************************/
{
  return object;
}




Boolean no_object_set()
/*************************************************************************\
* Purpose: standardized check if object (or gadget) was picked
\*************************************************************************/
{
  if(object == NULL)
    return True;

  if(object->wid == NULL) {
     printf("Programmer Error - cannot drag current object is not managed\n");
     return True;
  }
  return False;
}


void toolbox_move_toggle(Widget widget,XtPointer tag,XtPointer callback_data)
/**************************************************************************\
* function: toolbox_move_toggle
* purpose: to enable and disable moving of any gadget
*
\**************************************************************************/
{
#include <X11/cursorfont.h>
  static int move_status = False;	/* toggle flag */
  int fg, bg;	/* active foreground and background of drawing area */
  XGCValues xgcv;	/* mask of values desired from window graphic cntx */
  Cursor drag_cursor;	/* cursor to mark drag function active */
  Widget main_window;		/* id of drawing area widget */

  /* printf("START TOGGLE\n"); */

  /*** check for a valid db **/
  if(db_valid_db(&graph_db) != D_OKAY) {
    printf("ERROR: toolbox_move_toggle - no graph table\n");
    return;
  }

  if((main_window = widget_id("gui_main")) == FALSE) return;

  if((draw_id   = widget_id("drawwindow")) == FALSE) return;

  dpy = XtDisplay( draw_id );
  w   = XtWindow ( draw_id );


  if(move_status == False) {
    /** setup the routines to handle the mouse movements ***/
    move_status = True;

    /* create graphics context */
    XtVaGetValues( draw_id,
      XmNforeground, &fg,
      XmNbackground, &bg,
      NULL);

    xgcv.foreground = fg;
    xgcv.background = bg;
    xGC = XtGetGC(draw_id,
        GCForeground | GCBackground,
        &xgcv);

    xgcv.foreground = fg ^ bg;
    xgcv.background = bg;
    xgcv.function = GXxor;
    xorGC = XtGetGC(draw_id,
        GCForeground | GCBackground | GCFunction,
        &xgcv);

    /* change cursor */
    drag_cursor = XCreateFontCursor(XtDisplay(draw_id),
                  XC_fleur);

    XGrabButton(XtDisplay(draw_id), AnyButton, AnyModifier, 
       XtWindow(draw_id), True, 
       PointerMotionHintMask |  
         ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
       GrabModeAsync, GrabModeAsync, 
       XtWindow(draw_id), drag_cursor);
/**
    XGrabButton(XtDisplay(main_window), AnyButton, AnyModifier, 
       XtWindow(main_window), True, 
       ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
       GrabModeAsync, GrabModeAsync, 
       XtWindow(main_window), drag_cursor);
**/

  }
  else
  {
    /** remove the handler **/
    move_status = False;
    XUngrabButton(XtDisplay(draw_id), AnyButton, AnyModifier, 
       XtWindow(draw_id) );
  }

}



void start_rubberband( XEvent *pevent)
/**************************************************************************\
* Purpose: get & set values at start
*	   saves the original cursor starting location.  (from cursor event)
*	   draws outline around "object" (usually the starting bus)
*	   set motion mask
*
* Called by: drawAreaButtonCB (toolbox.c)
*	     tbMove, tbAddBranch, tbNewBend
\**************************************************************************/
{
  Widget draw_id;

  /* printf("START RUBBER BAND\n"); */
  if((draw_id = widget_id("drawwindow")) == FALSE)
  {
    return;
  }

  if( no_object_set() ) return;

  /*draw_grid( 0, 0, 20, 20, 20 );*/

  x_cursor = pevent->xbutton.x;
  y_cursor = pevent->xbutton.y;
  xorg = x_cursor;	/* for stop_rubberband to compute final delta */
  yorg = y_cursor;
  xdelta = 0;		/* set up for 1st undraw */
  ydelta = 0;

  draw_outline( object );

  XtAddEventHandler(draw_id, ButtonMotionMask, False, 
    (XtEventHandler)drag_rubberband, NULL);

}




void drag_rubberband(  Widget w, XtPointer data, XEvent *pevent)
/**************************************************************************\
* Purpose: UNDRAW and DRAW
*
* Called by: MOTIF DRAG event (setup in start_rubberband above)
\**************************************************************************/
{

  if( draw_id == FALSE ) return; 
  if( no_object_set()  ) return;

/* printf("  UNDRAW:\n" );*/
  draw_outline( object );		/* UNDRAW at old location */

  xdelta = pevent->xbutton.x - xorg;		/* new offset */
  ydelta = pevent->xbutton.y - yorg;
/*printf("  ** Event drag distance: %d, %d\n", xdelta, ydelta );*/
/* printf("  DRAW:\n" );*/
  draw_outline( object );		/* DRAW at new location */
}


void stop_rubberband( Widget w, XtPointer data, XEvent *pevent )
/**************************************************************************\
*
* Called by: addGraphBranch (toolbox.c)
*	     addDrawLine    (toolbox.c)
*	     buttonRelease on any moveable gagdet.
\**************************************************************************/
{
  Widget wid;

  if( draw_id == FALSE) return;
   if( no_object_set() ) return;

  if( object->class == GraphClassVertexBus ||		/* these are */
      object->class == GraphClassVertexGenerator ||	/* stackable */
      object->class == GraphClassVertexName ||
      object->class == GraphClassVertexDrawPoint )
    center_vtx_if_needed( object->x+xdelta, object->y+ydelta, 
			  object->wid, object->class );

  draw_outline( object ); 	/* UNDRAW bus and lines */

  /*****  Turn off the drag function ******************/
  XtRemoveEventHandler(draw_id, ButtonMotionMask, False, 
       (XtEventHandler)drag_rubberband, NULL);
  XtRemoveEventHandler(draw_id, ButtonReleaseMask, False, 
       (XtEventHandler)stop_rubberband, NULL);

  update_location_of_ge_and_attachments(object, xdelta, ydelta );

  object = NULL;		/* ensure no more drag action */

  display_coord_data( NULL );
}


int nearend(GraphElement *vertex, GraphElement *edge)
/**************************************************************************\
* Purpose: check to see if the first lnk is the same as the vertex 
*
\**************************************************************************/
{
  GraphLink *lnk;			/* ptr to graph link node */
  DB_STAT db_stat;

  db_stat = ll_first(&graph_db, &edge, &lnk, GRAPH_VERTEX_LINK); 

  if(db_stat != D_FOUND)
  {
    printf("ERROR: nearend: no vertex link found for object\n");
    return(TRUE);
  } 

  if (lnk->graphnode->wid == vertex->wid)
    return(TRUE);    /* printf("Note: nearend: corresponding widget ids\n"); */
  else
    return(FALSE);
     
}



void draw_outline( GraphElement *object )
/**************************************************************************\
* Purpose: draw bus symbol outline and all lines (and name vertex) associated
*		with it.
*
* Called by: start_rubberband, drag_rubberband, stop_rubberband
\**************************************************************************/
{
  GraphLink *lnk;			/* ptr to graph link node */
  Dimension height, width;
  Position x, y;
  Position x2, y2;
  int xi,yi,xc,yc,x2i,y2i,x2c,y2c;
  DB_STAT db_stat;
  double radians;
  double radius;
  GraphElement *pv1, *pv2;

  if( no_object_set() ) return;

  if(object->display == GraphDisplayOn)
  {

/****************************************************************************\
** NOTE: widget location NEVER changes during the drag - the widget id does  *
*	 we are just computing a new location for the each rectangle outline
\****************************************************************************/
    XtVaGetValues( object->wid,	/* get current vertex data */
        XmNx, &x,
        XmNy, &y,
        XmNheight, &height,
        XmNwidth, &width,
        NULL);

/***************************************************************************\
*     NOTE:  (x_cursor - xorg) is move distance of primary widget
*			since start of drag   
*	     x_org, y_org is initial location of this particular widget
\*************** x_cursor changes between undraw and draw ******************/

    xi = (int)(x)+xdelta;
    if(x < SCRN_BORDER) x = SCRN_BORDER;
    yi = (int)(y)+ydelta;
    if(y < SCRN_BORDER) y = SCRN_BORDER;
    
    XDrawRectangle( dpy, w, xorGC, xi, yi, width, height); 

/* now find the CENTER of the widget */
     xc = xi + gdgt_ge_x_offset( object );
     yc = yi + gdgt_ge_y_offset( object );
  }

/*********** Loop through all vertex links and draw outline if displayed **/ 

  db_stat = ll_first(&graph_db, &object, (LNKPTR *)&lnk, GRAPH_VERTEX_LINK); 

  while (db_stat == D_FOUND)
  {
    if(lnk->graphnode->display == GraphDisplayOn)
    {
      if(lnk->graphnode->wid == NULL)
      {
        printf("draw_outline: ERROR: bad vertex link object wid\n");
        printGraphElement(lnk->graphnode);
      }
      else
      {
#ifdef REMOVE 
         done recursiveley
          /* xdraw here */
#endif
      }
    }
    /** recursively draw vertex objects ***/
    if(lnk->graphnode != object)
    {
      draw_outline( lnk->graphnode);
    }
    else
    {
      /* halt recursion from looping back on itself for
      double linked lists */
    }
    db_stat = ll_next(&lnk, &lnk); 
  }

/********************* Now draw all the connections **********************/
/*********** go through all related edges and draw connections ***********/

  db_stat = ll_first(&graph_db, &object, &lnk, GRAPH_EDGE_LINK); 
  if(db_stat != D_FOUND)
  {
    /* printf("draw_outline: Note: no edge links for object\n"); */
    return;
  } 


  /** loop through all vertex links **/ 
  while (db_stat == D_FOUND)
  {
    if(lnk->graphnode->display != GraphDisplayOn)
    {
      /* printf("draw_outline Note: edge found display flag off %12.12s \n", */
      /*     &lnk->graphnode->key);  */
    }
    else
    {
      if(lnk->graphnode->wid == NULL)
      {
        /* printf("draw_outline: ERROR: bad object wid\n"); */
        /* printf(" edge %s\n", &lnk->graphnode->key); */
      }
      else
      {
        Position x2, y2; 
	db_stat = findEdgeVertexes(lnk->graphnode, &pv1, &pv2);
	if( D_FOUND == db_stat )
	{
          if( object->wid == pv1->wid )		/* get opposite widget */
	      pv1 = pv2;

          XtVaGetValues( pv1->wid, XmNx, &x2, XmNy, &y2, NULL);

	  x2i = (int)(x2);			/* ensure Dimension to integer*/
	  y2i = (int)(y2);

    	  radius = gdgt_ge_x_offset( pv1 );	/* NOTE x, y offsets are    */
						/* same for Bus and Bends   */
						/* so only one call is made */
	  x2c = x2i + radius;
	  y2c = y2i + radius;

          radians = radiansBetweenPts( x2c, y2c, xc, yc );
          circlePerimeter( radians, radius, &x2c, &y2c );
          XDrawLine( dpy, w, xorGC, xc, yc, x2c, y2c);
        }
      }
    }
    db_stat = ll_next(&lnk, &lnk); 
  } /* elihw */
} /* end display */


/**************************************************************************\
*
\**************************************************************************/
void background_object(GraphElement *object)
{
  GraphLink *lnk;			/* ptr to graph link node */
  DB_STAT db_stat;

  if( no_object_set() ) return;

  if(object->display == GraphDisplayOn) {
     /*** DO NOT UNMANAGE --- IT IS VERY SLOW ***/
     /** may want to turn a different color or highlight vertices ***/
      /* XtUnmanageChild(object->wid); */
  }

  /** go through all related vertices and unmanage **/
  db_stat = ll_first(&graph_db, &object, (LNKPTR *)&lnk, GRAPH_VERTEX_LINK); 
  if(db_stat != D_FOUND) {
    /* printf("background_object: Note: no vertex links for object\n"); */
    /* printf(" object %25.25s\n", &object->key); */
  } 
  /** loop through all vertex links and unmanage if displayed **/ 
  while (db_stat == D_FOUND) {
    if(lnk->graphnode->display == GraphDisplayOn) {
      if(lnk->graphnode->wid == NULL) {
        printf("background_object: ERROR: bad vertex link object wid\n");
        printGraphElement(lnk->graphnode);
      } else {
#ifdef REMOVE 
         done recursiveley
        /*** DO NOT UNMANAGE --- IT IS VERY SLOW ***/
        /* XtUnmanageChild(lnk->graphnode->wid); */
#endif
      }
    }
    /** recursively background vertex objects ***/
    background_object(lnk->graphnode);
    db_stat = ll_next(&lnk, &lnk); 
  }
  /** now go through all related edges and unmanage **/
  db_stat = ll_first(&graph_db, &object, &lnk, GRAPH_EDGE_LINK); 
  if(db_stat != D_FOUND) {
    /* printf("background_object: Note: no edge links for object\n"); */
  } 
  /** loop through all vertex links and unmanage if displayed **/ 
  while (db_stat == D_FOUND) {
    if(lnk->graphnode->display == GraphDisplayOn) {
      if(lnk->graphnode->wid == NULL) {
        printf("background_object: ERROR: bad edge link object wid\n");
        printGraphElement(lnk->graphnode);
      } else {
        /*** DO NOT UNMANAGE --- IT IS VERY SLOW ***/
        /* XtUnmanageChild(lnk->graphnode->wid); */
      }
    } else {
#ifdef DEBUG
      printf("background_object: Note: edge found display flag off %25.25s\n", 
         &lnk->graphnode->key);
#endif
    }
    db_stat = ll_next(&lnk, &lnk); 
  }
  return;

} /* end background */


void update_location_of_ge_and_attachments(GraphElement *object, int xdelta, int ydelta )
/***********************************************************************
* author: Dan Clark
* purpose: to move any graph object to a new screen location
*
* enhancements:
* - just because an object is not managed (has no wid)
*  does not mean that it should not be moved.
*  the coordinate file data should be updated with the new
*  positions even for objects which do not appear on the graph
***/
{
  int newx, newy;
  Position curx, cury;
  GraphElement graphnode;		/* update node for graph */
  GraphElement *pgraphnode = &graphnode;/* pointer to new graph node data */
  GraphLink *lnk;			/* ptr to graph link node */
  DB_STAT db_stat;

  if( no_object_set() ) return;

  if(object->display == GraphDisplayOn)	  /* get original widget position */
  {
    XtVaGetValues( object->wid,
        XmNx, &curx,
        XmNy, &cury,
        NULL);

    memcpy(pgraphnode, object, sizeof(GraphElement));

    pgraphnode->x += (int)xdelta; 
    pgraphnode->y += (int)ydelta; 

    if(pgraphnode->x < SCRN_BORDER) pgraphnode->x = SCRN_BORDER;
    if(pgraphnode->y < SCRN_BORDER) pgraphnode->y = SCRN_BORDER;

    db_update(&graph_db, &object, &pgraphnode);

    position_gadget_wrt( pgraphnode );	 /* update gadget location */
 
    /* savePositionChange(object, curx, cury, pgraphnode->x, pgraphnode->y); */
  }

  /** now go through all related vertices and move **/
  db_stat = ll_first(&graph_db, &object, (LNKPTR *)&lnk, GRAPH_VERTEX_LINK); 

  /** loop through all vertex links and remanage if displayed **/ 
  while (db_stat == D_FOUND)
  {
    if(lnk->graphnode->display == GraphDisplayOn)
    {
      if(lnk->graphnode->wid == NULL)
      {
        printf("update_location_of_ge_and_attachments: ERROR: bad object wid cannot remanage\n");
        printGraphElement(lnk->graphnode);
      }
    }

    /*** recursively move vertex objects ***/
    if(lnk->graphnode != object)
      update_location_of_ge_and_attachments(lnk->graphnode, xdelta, ydelta );
  
    db_stat = ll_next(&lnk, &lnk); 
  }

  /** now go through all related edges and move **/
  db_stat = ll_first(&graph_db, &object, &lnk, GRAPH_EDGE_LINK); 

  /** loop through all vertex links and move if displayed **/ 
  while (db_stat == D_FOUND)
  {
    if(lnk->graphnode->display == GraphDisplayOn)
    {
      if(lnk->graphnode->wid == NULL)
      {
        printf("update_location_of_ge_and_attachments: ERROR: bad object wid\n");
        printGraphElement(lnk->graphnode);
      }
      else
      {
        updateEdge(lnk->graphnode);

        /* oct30 XtManageChild(lnk->graphnode->wid); */
        /* update_location_of_ge_and_attachments: might create chg rec for coord edge */
      }
    }
    db_stat = ll_next(&lnk, &lnk); 
  }
}

/**************************************************************************\
* author: Dan Clark
* purpose:
* create a change data record for a change in position of
* any object.  This change record is subsequently used to
* create a new coord file by merging the changes with the old
* coordinate file.
*
\**************************************************************************/
int savePositionChange(GraphElement *object, int oldx, int oldy,
					      int newx, int newy)

{
   GraphLink *lnk;                      /* ptr to graph link node */
   CHGXY chg;
   CHGXY *pchg;

   GraphElement *newobject;

   /* create a coordinate change record for the new position */
   /* printf("savePosition: find coord record for non-coord vertex\n"); */
     sprintf(chg.key, "%24.24s", &lnk->graphnode->vertex_id);
     if(lnk->graphnode->class == GraphClassVertexBus ) {
       chg.cde = CHGCDE_SYMBOL_XY;
     } else if (lnk->graphnode->class == GraphClassVertexName) {
       chg.cde = CHGCDE_NAME_XY;
     } else {
/*     printf("WARNING: chg of xy for this lnkobject class not implemented \n");
*/
       chg.cde = CHGCDE_NONE;
     }

     chg.oldx = oldx;
     chg.oldy = oldy;
     chg.newx = newx;
     chg.newy = newy;

#ifdef DEBUG
   printf("savePosition: CHANGE RECORD: ");
   printf("key %s ", chg.key);
   printf("cde %d ", chg.cde);
   printf("%d %d %d %d", chg.oldx, chg.oldy, chg.newx, chg.newy);
   printf("\n");
#endif

   chg_push((CHANGES *)&chg);

#ifdef DEBUG
   chg_peek(&pchg);
   printf("savePosition peek: CHANGE RECORD: ");
   printf("key %s ", pchg->key);
   printf("cde %d ", pchg->cde);
   printf("%d %d %d %d", pchg->oldx, pchg->oldy, pchg->newx, pchg->newy);
   printf("\n");
#endif

  return 0;
}



void center_vtx_if_needed( int x, int y, Widget wid, GraphClass class )
/**************************************************************************\
* Purpose:  test to see if dragged object is being dropped on top of another
*
* Author: Bill E. Rogers		July 1996
*
* Called by: stop_rubberband
\**************************************************************************/
{
  GraphElement *pgraphnodenxt;
  DB_STAT stat;   

  stat = firstGE( &pgraphnodenxt, ANY_TYPE );

  if (stat == D_EOF)
  {
    printf("ERROR db_search did not find any records \n");
    return;
  }
  if (stat == D_NOTFOUND) {
    stat = D_FOUND;
  }


  while(stat == D_FOUND)
  {
    /** now look for all vertices **/
    if( ( pgraphnodenxt->class == class &&  pgraphnodenxt->wid != wid ) ||
	( pgraphnodenxt->class == GraphClassVertexBus && 
	  object->class == GraphClassVertexGenerator ) )
    {
      if( abs( pgraphnodenxt->x - x ) < MISS_DIST &&
	  abs( pgraphnodenxt->y - y ) < MISS_DIST )
      {
	draw_outline( object );
	xdelta += (pgraphnodenxt->x - x);
	ydelta += (pgraphnodenxt->y - y);
	draw_outline( object );
      }
    }

    stat = nextGE( &pgraphnodenxt, GRAPH_DISPLAY ); /** next GraphElement **/
  }



}



#ifdef TEST
void draw_grid( int xorg, int yorg, int wd, int h, int size )
{
  int x, y;
  int xend, yend;

  xend = xorg+wd*size-1;
  yend = yorg+wd*size-1;

  for ( x=xorg; x<=xend; x+=size )
  {
    XDrawLine( dpy, w, xorGC, x, yorg, x, yend);
  }

  for ( y=yorg; y<=yend; y+=size )
  {
    XDrawLine( dpy, w, xorGC, xorg, y, xend, y );
  }
}
#endif
