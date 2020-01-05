
static char sccsid[] = "@(#)edge_research.c	20.1 5/1/94";

/****************************************************************************\
* file: edge.c
* author: Dan Clark
* purpose: handle all edge functions
*
\****************************************************************************/

#include <Xm.h>

#ifdef TEST
#include <DrawnB.h>
#include "EdgeBG.h"
#endif
#include "EdgeG.h"
#include "dmgr.h"
#include "graph_data.h"
#include "base_data.h"
#include "coord_data.h"
#include "line2rect.h"

extern Widget widget_id ( char * );
Widget drawlinebranch   ( int , int , int , int , int , int , GraphElement *);
Widget drawline   ( int x1 , int y1 , int x2, int y2, double kv);
int    drawGraphEdges( Widget ,char *, char *);
void printbranch( );
double setGraphKv(GraphElement *pgraphedgenxt);
Widget lookup_kv_wid(double kv);


/***************************************************************************\
*
\***************************************************************************/
void findbranch( w, data, cbs)
Widget          w;
XtPointer data;
XmDrawingAreaCallbackStruct *cbs;
{
  int x,y;
  XEvent *event = cbs->event;

  if(cbs->reason == XmCR_INPUT) {
    if(event->xany.type == ButtonPress) {
      x = event->xbutton.x;
      y = event->xbutton.y;
      printf("x,y = (%d,%d)\n", x, y);
      findgraphedge(x,y);
    }
  }
}

/*****************************************************************************\
*	Function: drawgraphedges
*
*	Author: Dan Clark	Jan 30
*
\******************************************************************************/

#define NODESIZE 21
int drawGraphEdges(widget, tag, callback_data)
Widget          widget;
char            *tag;
char		*callback_data;
{

  GraphElement graphedge;
  GraphElement *pgraphedgeupdate;		/* ptr to edge */
  GraphElement *pgraphedge;		/* ptr to edge */
  GraphElement *pgraphedgenxt;		/* ptr to next edge */
  GraphElement *pgraphnode1;		/* ptr to node 1    */
  GraphElement *pgraphnode2;		/* ptr to node 2    */
  GraphLink    *pgraphlink;

  DB_STAT stat_disp;
  DB_STAT stat_link;
  DB_STAT stat;
  int	edge_cnt;
  Widget gadget_id;
  Widget draw_id;
  double basekv;
  extern double atof_cnt_zero();

#ifdef DEBUG
  printf("\n\nstart display graph edges \n");
#endif

#ifdef REMOVE
  draw_id = widget_id( "drawwindow" );
  /* XtAddCallback(draw_id, XmNinputCallback, findbranch, NULL); */
#endif

  /** set the key fields and position before first record **/
  pgraphedge = &graphedge;
  pgraphedge->display = GraphDisplayRequestOn;
  pgraphedge->type = GraphTypeEdge;
  stat_disp = db_search(&graph_db, &pgraphedge, &pgraphedgenxt,
                        GRAPH_DISPLAY_TYPE);
  if (stat_disp == D_EOF) {
    printf("ERROR: drawGraphEdges - EOF no valid records in graph_db\n");
    return;
  }
  if (stat_disp != D_FOUND)
  {
#ifdef DEBUG
    printf("NOTE: db_search set at first base record \n");
#endif
    stat_disp = D_FOUND;
  }

  edge_cnt = 0;
  while(stat_disp == D_FOUND ) {
#ifdef DEBUG
    printGraphElement(pgraphedgenxt);
#endif

    edge_cnt++;

    /** follow vertex links to find end points **/
    stat_link = ll_first(&graph_db, &pgraphedgenxt, &pgraphlink,
                GRAPH_VERTEX_LINK);
    if (stat_link != D_FOUND) {
      printf("Note: no vertex links\n");
    }
    while(stat_link == D_FOUND) {
      /* if key values are out of range stop loop */
      if( pgraphedgenxt->type != GraphTypeEdge ||
          pgraphedgenxt->display != GraphDisplayRequestOn) {
#ifdef DEBUG
         printf("NOTE: forceing end to displayed vertex list\n");
         printGraphElement(pgraphedgenxt);
#endif

         stat_disp = D_NOTFOUND;
         break;
      }

      pgraphnode1 = (GraphElement *)pgraphlink->graphnode;
      stat_link = ll_next(&pgraphlink, &pgraphlink);
      if(stat_link != D_FOUND) {
         break;
      }
      
      pgraphnode2 = (GraphElement *)pgraphlink->graphnode;

      /** draw line on screen **/

#ifdef remove
      gadget_id = (Widget) drawlinebranch(pgraphnode1->x, pgraphnode1->y, 
			            pgraphnode2->x, pgraphnode2->y,
			            NODESIZE, NODESIZE, pgraphedgenxt);
#endif

      basekv = setGraphKv(pgraphedgenxt);

      gadget_id = (Widget) drawline(
          pgraphnode1->x, pgraphnode1->y,
          pgraphnode2->x, pgraphnode2->y,
	  basekv);

#ifdef DEBUG
      XtAddCallback(gadget_id, XmNarmCallback,
                  printbranch, pgraphedgenxt);
#endif
      
      /* add gadget id ptr to record */
      pgraphedgeupdate = &graphedge;
      memcpy(pgraphedgeupdate,pgraphedgenxt, sizeof(GraphElement));
      pgraphedgeupdate->wid = gadget_id;
      pgraphedgeupdate->display = GraphDisplayOn;
      db_update(&graph_db, &pgraphedgenxt, &pgraphedgeupdate);

#ifdef DEBUG
      printf("from: %32.32s\n", pgraphnode1->key.pf.type);
      printf("to: %32.32s\n", pgraphnode2->key.pf.type); 
      printf("drawgraphedges: ");
      printf("  (x1,y1) (%d, %d) ", pgraphnode1->x, pgraphnode1->y);
      printf("  (x2,y2) (%d, %d) ", pgraphnode2->x, pgraphnode2->y);
      printf("  cnt %d", edge_cnt);
      printf("\n");
#endif
    }

    /** get the next displayed edge **/
    pgraphedge = pgraphedgenxt;
    stat_disp = db_next(&graph_db, &pgraphedge, &pgraphedgenxt,
           GRAPH_DISPLAY_TYPE);

  } /* end loop through all displayed edges */

} /* end */



/****************************************************************************\
*     Function: printbranch.c 
*		
*
*	Called by: callback from MOTIF when branch is pushed
\****************************************************************************/

void printbranch( w, client_data, reason)
Widget          w;
GraphElement	*client_data;
XmPushButtonCallbackStruct	*reason;

{
  GraphElement graphnode;
  GraphElement *pgraphnode = &graphnode;
  GraphElement *pgraphnodenxt;

  GraphElement *linkgraphrec;
  GraphLink *llretrec;

  DB_STAT stat;         /* status returned for general db access */
  DB_STAT stat_link;            /* status returned for ll dmgr access */
  XEvent *event = reason->event;
  int x,y;

    x = event->xbutton.x;
    y = event->xbutton.y;
#ifdef DEBUG
#endif
    printf("printbranch: x,y = (%d,%d)\n", x, y);

    findgraphedge(x,y);

#ifdef DEBUG
#endif
    printGraphElement(client_data);

#ifdef DEBUG_DETAIL
#endif
    pgraphnodenxt = client_data;
    /** now follow all vertex links **/
    stat_link = ll_first(&graph_db, &pgraphnodenxt, &llretrec,
                GRAPH_EDGE_LINK);
    if (stat_link != D_FOUND) {
      printf("Note: no edge links\n");
    }
    while(stat_link == D_FOUND) {
      linkgraphrec = (GraphElement *)llretrec->graphnode;
      printf("VL: ");
      printGraphElement(linkgraphrec);
      stat_link = ll_next( &llretrec, &llretrec );
    }

}


/****************************************************************************\
*	Function: drawlinebranch 
*		
*
*	Called by: drawGraphEdges (above)
\****************************************************************************/
Widget olddrawlinebranch( int x1, int y1, int x2, int y2,
			  int xsize, int ysize,
              GraphElement *pgraphedge)
{

  Widget	draw_id;
  Arg           arglist[20];

  int           i = 0;

  Widget        line_gadgetid;
  char		*name = "LINE";

  draw_id = widget_id("drawwindow" );

#define MAX_SCALE 3000
    if(x1 > MAX_SCALE || x1 > MAX_SCALE ||
       y1 > MAX_SCALE || y2 > MAX_SCALE ) {
       printf("x or y value out of range and reset to %d\n", MAX_SCALE);
       printf(" name %16.16s\n", pgraphedge->key.pfbranch.type);
      
      if(x1 > MAX_SCALE)
        x1 = MAX_SCALE;
      if(x2 > MAX_SCALE)
        x2 = MAX_SCALE;
      if(y1 > MAX_SCALE)
        y1 = MAX_SCALE;
      if(y2 > MAX_SCALE)
        y2 = MAX_SCALE;
    }

    line_gadgetid = XtVaCreateManagedWidget ( name,
		edgeGadgetClass, draw_id,
		XiNx1, x1,
		XiNy1, y1,
		XiNx2, x2,
		XiNy2, y2,
		XmNborderWidth, (Dimension)2,
		XmNhighlightThickness, (Dimension)2,
		NULL);

#ifdef DEBUG
    XtAddCallback(line_gadgetid, XmNarmCallback,
                  printbranch, pgraphedge);
#endif
/***
    XtAddCallback(Push1_widget, XmNactivateCallback,
                  activategadget, bus+busNumber );
***/

    return line_gadgetid;

} /* end */


#ifdef dlcTEST
static Pixmap tstpixmap;
static GC tstgc;
void EdgeRedraw( w, pixmap, cbs)
Widget          w;
Pixmap pixmap;
XmEdgeButtonCallbackStruct	*cbs;
{
  
  Widget drawid;
  int srcx, srcy, destx, desty, pix_w, pix_h;
  int drawsize, border;
  Dimension brd_w, w_width, w_height;
  short hlthick, shthick;
  Window root;
 /* printf("draw line in gadget here\n"); */
  Widget draw_id;

 draw_id = widget_id("drawwindow");
 XGetGeometry(XtDisplay(draw_id), pixmap, 
   &root, &srcx, &srcy, &pix_w, &pix_h, &srcx, &srcy);
 XtVaGetValues(w, 
   XmNwidth, &w_width,
   XmNheight, &w_height,
   XmNborderWidth, &brd_w,
   XmNhighlightThickness, &hlthick,
   XmNshadowThickness, &shthick,
   NULL);
  
 /* calc avail draw area */
 border = brd_w + hlthick + shthick;
 /* if window is bigger than pixmap, ctr it; els clip pixmap */
 drawsize = w_width - 2*border;
 if(drawsize > pix_w) {
   srcx = 0;
   destx = (drawsize - pix_w)/2 + border;
 } else {
   srcx = (pix_w - drawsize) / 2;
   pix_w = drawsize;
   destx = border;
 }
 /* now height */
 drawsize = w_height - 2*border;
 if(drawsize > pix_h) {
   srcy = 0;
   desty = (drawsize - pix_h)/2 + border;
 } else {
   srcy = (pix_h - drawsize)/2;
   pix_h = drawsize;
   desty = border;
 }

 XCopyArea(XtDisplay(draw_id), pixmap, XtWindow(draw_id), tstgc, 
    srcx, srcy, pix_w, pix_h, destx, desty);
  
}


/****************************************************************************\
*	Function: drawline.c 
*		
*
*	Called by: drawGraphEdges (above)
\****************************************************************************/
Widget drawlinebranch( int x1, int y1, int x2, int y2, int xsize, int ysize,
              GraphElement *pgraphedge)
{

  Pixel fg, bg;
  Widget	draw_id;
  Arg           arglist[20];

  int           i = 0;

  Widget        line_gadgetid;
  char		*name = "LINE";
  XtCallbackRec edge_callback_list[]= {
    {EdgeRedraw, 0} ,
    {(XtCallbackProc) NULL, (caddr_t)NULL}
  };

  draw_id = widget_id("drawwindow");

#define MAX_SCALE 3000
    if(x1 > MAX_SCALE || x1 > MAX_SCALE ||
       y1 > MAX_SCALE || y2 > MAX_SCALE ) {
       printf("x or y value out of range and reset to %d\n", MAX_SCALE);
       printf(" name %16.16s\n", pgraphedge->key.pfbranch.type);
      
      if(x1 > MAX_SCALE)
        x1 = MAX_SCALE;
      if(x2 > MAX_SCALE)
        x2 = MAX_SCALE;
      if(y1 > MAX_SCALE)
        y1 = MAX_SCALE;
      if(y2 > MAX_SCALE)
        y2 = MAX_SCALE;
    }
    XtVaGetValues(draw_id,
           XmNforeground, &fg,
           XmNbackground, &bg,
           NULL);

    tstgc = XCreateGC(XtDisplay(draw_id), 
            RootWindowOfScreen(XtScreen(draw_id)), NULL, 0);

    tstpixmap = XmGetPixmap(XtScreen(draw_id), "escherknot", fg, bg);
    if(tstpixmap == XmUNSPECIFIED_PIXMAP) {
      printf("ERROR: XmGetPixmap failed\n");
    }

    /* Create the line argument list */

    line_gadgetid = XtVaCreateManagedWidget ( name, 
		xmEdgeButtonGadgetClass, draw_id, 
		/* xmDrawnButtonWidgetClass, draw_id,  */
		XmNx, (Position)x1,
		XmNy, (Position)y1,
		XmNwidth, (Dimension)abs(x1-x2),
		XmNheight, (Dimension)abs(y1-y2),
/*		XmNexposeCallback, edge_callback_list, */
		XmNlabelType, XmPIXMAP,
		XmNlabelPixmap, tstpixmap,
		XiNx1, x1,
		XiNy1, y1,
		XiNx2, x2,
		XiNy2, y2,
		XmNborderWidth, (Dimension)2,
		XmNhighlightThickness, (Dimension)2,
		NULL);

    XtAddCallback(line_gadgetid, XmNarmCallback,
                  printbranch, pgraphedge);
    XtAddCallback(line_gadgetid, XmNexposeCallback,
                  EdgeRedraw, tstpixmap);
/***
    XtAddCallback(Push1_widget, XmNactivateCallback,
                  activategadget, bus+busNumber );
***/

    return line_gadgetid;

} /* end */


/****************************************************************************\
*
\****************************************************************************/
Widget drawlineEdgeBG( int x1, int y1, int x2, int y2)
{
  EdgePoint *pedgept;
  EdgeBox edgebox;
  EdgeBox *pedgebox = &edgebox;
  char * name = "Edge";
  Widget draw_id;
  XtCallbackRec edge_callback_list[]= {
    {EdgeRedisplay, 0} ,
    {(XtCallbackProc) NULL, (caddr_t)NULL}
  };

  draw_id = widget_id("drawwindow");

  pedgept = (EdgePoint *)malloc(sizeof(EdgePoint));
  pedgept->x1 = x1;
  pedgept->y1 = y1;
  pedgept->x2 = x2;
  pedgept->y2 = y2;
  pedgept->gc =  XCreateGC(XtDisplay(draw_id), 
            RootWindowOfScreen(XtScreen(draw_id)), NULL, 0);

  line2rect( pedgept, pedgebox );

  XtVaCreateManagedWidget(name,
	xmEdgeButtonGadgetClass, draw_id, 
	XmNx, (Position)pedgebox->x,
	XmNy, (Position)pedgebox->y,
	XmNwidth, (Dimension)pedgebox->width,
	XmNheight, (Dimension)pedgebox->height,
		XiNx1, (Dimension)x1,
		XiNy1, (Dimension)y1,
		XiNx2, (Dimension)x2,
		XiNy2, (Dimension)y2,
	XmNuserData, (EdgePoint *)pedgept,
	XmNexposeCallback, edge_callback_list,
	NULL); 

}


/****************************************************************************\
*
\****************************************************************************/
void EdgeRedisplay( eg, pdata, cbs)
XmEdgeButtonGadget eg;
XtPointer pdata;
XmEdgeButtonCallbackStruct	*cbs;
{
  Widget draw_id;
  int border;
  Position eg_x, eg_y;
  Dimension eg_width, eg_height, eg_brdwidth, eg_hlthick, eg_shthick;
  EdgePoint *pedgept;

  draw_id = widget_id("drawwindow");
  XtVaGetValues(eg,
    XmNwidth, &eg_width,
    XmNheight, &eg_height,
    XmNx, &eg_x,
    XmNy, &eg_y,
    XmNborderWidth, &eg_brdwidth,
    XmNhighlightThickness, &eg_hlthick,
    XmNshadowThickness, &eg_shthick,
    XmNuserData, &pedgept,
    NULL);

#ifdef DOCHECK
  /** NOTE: may want to check so that we do not draw over the borders **/
  border = eg_brdwidth + eg_hlthick + eg_shthick;
  if(width < border || height < border) {
    printf("NOTE: line is overwriting a border\n");
  }
#endif
#ifdef PROBLEMS
    XDrawLine( XtDisplay(draw_id), XtWindow(draw_id),
      pedgept->gc,
      pedgept->x1, pedgept->y1,
      pedgept->x2, pedgept->y2 );
#endif

}
#endif /* dlcTEST */


/***************************************************************************\
*
\***************************************************************************/
Widget drawline( int x1, int y1, int x2, int y2, double base_kv)
{
  EdgePoint edgept;
  EdgePoint *pedgept = &edgept;
  EdgeBox edgebox;
  EdgeBox *pedgebox = &edgebox;
  Dimension zero = 0;
  char * name = "Edge";
  Widget draw_id;
  Widget legend_id;
  Pixel color;
  Pixmap color_pixmap;

  draw_id = widget_id("drawwindow");

  pedgept->x1 = x1;
  pedgept->y1 = y1;
  pedgept->x2 = x2;
  pedgept->y2 = y2;

  line2rect( pedgept, pedgebox );

  legend_id = lookup_kv_wid(base_kv);
  
  XtVaGetValues(legend_id,
                XmNbackground, &color,
                XmNbackgroundPixmap, &color_pixmap,
                NULL);

  XtVaCreateManagedWidget(name,
                        edgeGadgetClass, draw_id,
		        XmNx, (Position)pedgebox->x,
		        XmNy, (Position)pedgebox->y,
			XmNwidth,  (Dimension)pedgebox->width,
			XmNheight, (Dimension)pedgebox->height,
			XiNx1, (Dimension)x1,
			XiNy1, (Dimension)y1,
			XiNx2, (Dimension)x2,
			XiNy2, (Dimension)y2,
			XmNarmColor,  (Pixel)color,
			XmNarmPixmap, (Pixmap)color_pixmap,
			XmNborderWidth, (Dimension)zero,
			XmNhighlightThickness, (Dimension)zero, 
			XmNshadowThickness, (Dimension)zero,
			XmNrecomputeSize, (Boolean)False,
			/* XmNuserData, (EdgePoint *)pedgept,  */
			NULL);

}

toggleEdgeDash(Widget wid)
{
  Boolean curval;
  if(wid == NULL) {
    printf("ERROR: toggleEdgeDash: bad wid\n");
  } else {
    XtVaGetValues(wid,
                XiNdashOn, &curval,
                NULL);
    XtVaSetValues(wid,
                XiNdashOn, (Boolean)!curval,
                NULL);
  }
  return;
}

/****
* function: lookup_kv_wid
* author: Dan Clark
* purpose: determine the wid of the label
* which defines the color for the line gadgets
* a table is setup for the color graduations
****
*/
#define MAXNAME 64
typedef struct {
  double kv;
  char wname[MAXNAME];
}KV_Table;
  
Widget lookup_kv_wid(double base_kv)
{
  Widget legend_id;
  int idx;
  KV_Table kv_tbl[] = {
    { 500.0, "kv_500_label"},
    { 345.0, "kv_345_label"},
    { 300.0, "kv_300_label"},
    { 230.0, "kv_230_label"},
    { 161.0, "kv_161_label"},
    { 138.0, "kv_138_label"},
    { 115.0, "kv_115_label"},
    { 69.0, "kv_69_label"},
    { 46.0, "kv_46_label"},
    { 14.0, "kv_14_label"},
    { 7.0, "kv_7_label"},
  };
  int kv_tbl_size = (int)sizeof(kv_tbl)/sizeof(kv_tbl[0]);


  idx = kv_tbl_size - 1;
  while(idx > 0 && kv_tbl[idx].kv < base_kv ) {
    idx--;
  }

#ifdef GREATERTHAN
  idx = 0;
  while(idx < kv_tbl_size && kv_tbl[idx].kv > base_kv ) {
    idx++;
  }
  if(idx >= kv_tbl_size) {
    idx = kv_tbl_size - 1;
  }
#endif

#ifdef DEBUG
  printf("basekv = %.2f tbl kv %.2f tbl nme %s idx = %d\n",
    base_kv, kv_tbl[idx].kv, kv_tbl[idx].wname, idx);
#endif

  legend_id = widget_id(kv_tbl[idx].wname);
 
#ifdef REMOVE
  legend_id = widget_id("kv_115_label");

  if ( base_kv >= 138.0 )
      legend_id = widget_id("kv_138_label");
  
  if ( base_kv >= 161.0 )
      legend_id = widget_id("kv_161_label");
  
  if ( base_kv >= 230.0 )
      legend_id = widget_id("kv_230_label");
  
  if ( base_kv >= 300.0 )
      legend_id = widget_id("kv_300_label");
  
  if ( base_kv >= 345.0 )
      legend_id = widget_id("kv_345_label");

  if ( base_kv >= 500.0 )
      legend_id = widget_id("kv_500_label");
  
  if ( base_kv <= 115.0 )
      legend_id = widget_id("kv_69_label");

  if ( base_kv <= 70.0 )
      legend_id = widget_id("kv_46_label");
#endif

  return legend_id;
  
}

/**********
* function: setGraphKv
* author: Dan Clark
* purpose: determine the kv of an edge drawn on the graph
* note:
* the kv color of a transformer is set to the
* lowest kv of the line
* if bad data is passed to this function it returns
* a kv of 0.0
*********
*/

double setGraphKv(GraphElement *pgraphedgenxt)
{
  double basekv;
  double basekv2;

      if(pgraphedgenxt->source == GraphSourceBase) {
        if(pgraphedgenxt->key.pfbranch.type[0] == 'T' || 
           pgraphedgenxt->key.pfbranch.type[0] == 'R') {
        basekv = atof_cnt_zero(
          pgraphedgenxt->key.pfbranch.base1, 
          sizeof(pgraphedgenxt->key.pfbranch.base1));
        basekv2 = atof_cnt_zero(
          pgraphedgenxt->key.pfbranch.base2, 
          sizeof(pgraphedgenxt->key.pfbranch.base2));
          basekv = (basekv < basekv2 ? basekv : basekv2); /* min < max > */
        } else if(pgraphedgenxt->key.pfbranch.type[0] == 'L') {
        basekv = atof_cnt_zero(
          pgraphedgenxt->key.pfbranch.base1, 
          sizeof(pgraphedgenxt->key.pfbranch.base1));
        } else if(pgraphedgenxt->key.pfbranch.type[0] == 'B') {
        basekv = atof_cnt_zero(
          pgraphedgenxt->key.pfbus.base1, 
          sizeof(pgraphedgenxt->key.pfbus.base1));
        } else {
          printf("WARNING: bad edge in base file %s\n", 
               pgraphedgenxt->key.pfbranch.type);
          basekv = 0.0;
        }
      } else if (pgraphedgenxt->source == GraphSourceCoord) {
        if(pgraphedgenxt->key.coord.id[0] == 'L') {
          /** check kv at both ends since user may have put L instead of T **/
          basekv = atof_cnt_zero(
            pgraphedgenxt->key.coord.base1, 
            sizeof(pgraphedgenxt->key.coord.base1));
          basekv2 = atof_cnt_zero(
            pgraphedgenxt->key.coord.base2, 
            sizeof(pgraphedgenxt->key.coord.base1));
          basekv = (basekv < basekv2 ? basekv : basekv2); /* min < max > */
          if(basekv2 != basekv) {
            printf("WARNING: coord edge card has different kvs\n");
            printf(" %s\n", pgraphedgenxt->key.coord.id);
          }
        } else if (pgraphedgenxt->key.coord.id[0] == 'T') {
          basekv = atof_cnt_zero(
            pgraphedgenxt->key.coord.base1, 
            sizeof(pgraphedgenxt->key.coord.base1));
          basekv2 = atof_cnt_zero(
            pgraphedgenxt->key.coord.base2, 
            sizeof(pgraphedgenxt->key.coord.base1));
          basekv = (basekv < basekv2 ? basekv : basekv2); /* min < max > */
        } else if(pgraphedgenxt->key.coord.id[0] == 'B') {
          basekv = atof_cnt_zero(
            pgraphedgenxt->key.coord.base1, 
            sizeof(pgraphedgenxt->key.coord.base1));
        } else {
          printf("WARNING: bad edge in coord file %s\n", 
               pgraphedgenxt->key.coord.id);
          basekv = 0.0;
        }
      } else {
        basekv = 0.0;
        printf("ERROR - programmer set unknown edge source %d set kv to %lf", 
           pgraphedgenxt->source, basekv);
      }
#ifdef DEBUG
        printf("setGraphKv: set kv to %lf for edge %s\n", 
           basekv, pgraphedgenxt->key.coord.id);
#endif  
  return basekv;
}

