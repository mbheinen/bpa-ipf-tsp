static char sccsid[]="@(#)edge.c	20.44 5/19/98";
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
/****************************************************************************\
* file: edge.c
* author: Dan Clark
* purpose: handle all edge functions
*
\****************************************************************************/

#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>

#include "EdgeG.h"
#include "dmgr.h"
#include "graph_data.h"
#include "base_data.h"
#include "coord_data.h"
#include "line2rect.h"
#include "ipfdebug.h"

#define RadtoDeg 57.29578

extern Cardinal insertByType	( Widget wid);
extern int  istransformer	( GraphElement *);
extern void add_blanks_to_string( char *string, int col );
extern int reformat		( char *, char *, char * );
extern int EdgeGCheckClass	();
extern Widget widget_id		( char * );
extern void drawAreaButtonCB	( Widget w, GraphElement *client_data,
		                  XmPushButtonCallbackStruct *call_data);
extern void lookup_and_get_field( char *fld_name, char *ret_string, int size );

Widget drawline			( int x1 , int y1 , int x2, int y2, 
		                  double kv, GraphElement *pedge);
int findgraphedge		( int x, int y, GraphElement **pfound);
double get_double_from_text	( char *text_box_name, char *fmt );
double setGraphKv		( GraphElement *pgraphedgenxt);
Widget lookup_kv_wid		( double kv);
int manageEdge			( GraphElement *pgraphedgenxt);
int pointNearSegment		( int x, int y, int x1, int y1, int x2, int y2);
double slopeOfTwoPts		( int x, int y, int x2, int y2 );
double yInterceptOfSlope	( double slope, int x, int y );
void searchGraphEdge		( Widget w, GraphElement *client_data,
				  XmPushButtonCallbackStruct *reason);
void getPerimeterPts		( GraphElement *pgraphelement1,
				  GraphElement *pgraphelement2,
				  int *x, int *y, int *x2, int *y2 );
extern double radiansBetweenPts	( int x, int y, int x2, int y2 );
int roundoff			( double fnum );
void squarePerimeter		( double radians, int size, int *x, int *y );
void circlePerimeter		( double radians, int size, int *x, int *y );
void rectPerimeter		( double r, int wd, int ht, int *x, int *y );
void updateEdge			( GraphElement *pedge);
extern double zoom_factor;
extern int icon_bus_width;
extern int icon_bus_height;
extern int icon_bus_shape;  



void createEdgeGadgets(Widget wid,XtPointer tag, XmAnyCallbackStruct *cbs)
/*****************************************************************************\
*	Function: drawgraphedges
*
*	Author: Dan Clark	Jan 30
*
*	Called by: filedlgrtn.c, graphdata.c, toolbox.c, gui.c, procs.u
*
\******************************************************************************/
{
  GraphElement *pgraphedgenxt;		/* ptr to next edge */

  DB_STAT stat;

  stat = firstGE( &pgraphedgenxt, GraphTypeEdge );

  if(ipfdebug & (DB_TraceMask | DB_Edge)) {
	if (stat == D_EOF) {
		fprintf(ipfdbio,"WARNING: EOF, no valid records in graph_db\n");
	}
  }

  if (stat == D_EOF) {
    return;
  }

  if (stat != D_FOUND) {
    stat = D_FOUND; }		/* NOTE: db_search set at first base record */

  while(stat == D_FOUND )
  {
    manageEdge(pgraphedgenxt);
    stat = nextGE( &pgraphedgenxt, GRAPH_DISPLAY_TYPE ); /* next edge */
  }
}


int manageEdge(GraphElement *pgraphedgenxt)
/****************************************************************************\
* Purpose:  get vertices v1, v2, of edge
* set the x1, y1, x2, y2 of the line gadget based on the vertex x's & y's
* create a widget for this line at that position
* and update the graph data structure with the new wid
* note: that this loop may need to be removed to make it more consistant
* with the rest of the application which makes an assumption that there
* are only two vertex links to an edge.
*
* author: Dan Clark		modified: Nov 13, 1992
*
* Called by: createEdgeGadgets		 (this module)
*	     turnOnSubedge		 (graphdata.c)
*	     process_pf_connections 		"
*	     create_and_manage_coord_edge ( graphpsccor.c)
*	     pf_checkConnect (2)	  ( pf_cb.c)
*	     add_graph_line (2)		  ( toolbox.c)
*	     addGraphBranch			"
*	     breakEdge				"
*	     
*****************************************************************************/
{
  GraphElement graphedge;
  GraphElement *pgraphedgeupdate;	/* ptr to edge */
  GraphElement *pgraphnode1;		/* ptr to node 1    */
  GraphElement *pgraphnode2;		/* ptr to node 2    */

  DB_STAT stat;
  Widget gadget_id;
  double basekv;
  int x, y, x2, y2;


    if( GraphTypeEdge != pgraphedgenxt->type ) {
       printf("Programmer Error: must set edge type = GraphTypeEdge val %d\n",
         pgraphedgenxt->type);
    }

    stat = findEdgeVertexes( pgraphedgenxt, &pgraphnode1, &pgraphnode2 );
    assert ( stat == D_FOUND );

    basekv = setGraphKv(pgraphedgenxt);

#ifdef BER_EXPERIMENTAL
/*  EXPERIMENTAL ber */
    if( pgraphnode1->class==GraphClassVertexBus ||
	      pgraphnode2->class==GraphClassVertexBus )
    {
      getPerimeterPts	( pgraphnode1, pgraphnode2,
				  &x, &y, &x2, &y2 ); 
      gadget_id = (Widget) drawline( x, y, x2, y2, basekv, pgraphedgenxt );
    }
    else
    {
      gadget_id = (Widget) drawline( pgraphnode1->x, pgraphnode1->y,
			             pgraphnode2->x, pgraphnode2->y,
                                     basekv, pgraphedgenxt);
    }
#else
      getPerimeterPts	( pgraphnode1, pgraphnode2, &x, &y, &x2, &y2 ); 
      gadget_id = (Widget) drawline( x, y, x2, y2, basekv, pgraphedgenxt );
#endif

    pgraphedgeupdate = &graphedge;
    memcpy(pgraphedgeupdate, pgraphedgenxt, sizeof(GraphElement));
    pgraphedgeupdate->wid = gadget_id;		/* put in gadget id */
    pgraphedgeupdate->display = GraphDisplayOn;

    stat = db_update(&graph_db, &pgraphedgenxt, &pgraphedgeupdate);
    assert(D_OKAY == stat);

  return(0);
}


void updateEdge(GraphElement *pedge)
/*************************************************************************\
* author: Dan Clark
* purpose: update the screen to correspond to the graph data structure
\*************************************************************************/
{
  Position curx1, cury1, curx2, cury2;
  EdgePoint edgept;
  EdgePoint *pedgept = &edgept;
  EdgeBox edgebox;
  EdgeBox *pedgebox = &edgebox;
  DB_STAT stat_link;
  DB_STAT stat_disp;
  GraphElement *pgraphnode1;
  GraphElement *pgraphnode2;
  GraphLink *pgraphlink;
  Dimension zoom;

  int x, y, x2, y2;

    if(GraphDisplayOn != pedge->display ) {
      printf("ERROR: this edge is not currently displayed\n");
      return;
    }
    if(NULL == pedge->wid) {
      printf("ERROR: this edge has bad wid\n");
      return;
    }
    /** follow vertex links to find end points **/
    stat_link = ll_first(&graph_db, &pedge, &pgraphlink,
                GRAPH_VERTEX_LINK);
    if (stat_link != D_FOUND) {
      printf("ERROR: Note: no vertex links\n");
      return;
    }
    pgraphnode1 = (GraphElement *)pgraphlink->graphnode;
    stat_link = ll_next(&pgraphlink, &pgraphlink);
    if(stat_link != D_FOUND) {
      printf("ERROR: Note: only one vertex link\n");
      return;
    }
    pgraphnode2 = (GraphElement *)pgraphlink->graphnode;

    getPerimeterPts ( pgraphnode1, pgraphnode2, &x, &y, &x2, &y2 );

    pedgept->x1 = x;
    pedgept->y1 = y;
    pedgept->x2 = x2;
    pedgept->y2 = y2;

    line2rect( pedgept, pedgebox );
  zoom = (Dimension)(zoom_factor * (double)100.0);

    XtVaSetValues( pedge->wid,
      XmNx,      (Position) pedgebox->x,
      XmNy,      (Position) pedgebox->y,
      XmNwidth,  (Dimension)pedgebox->width,
      XmNheight, (Dimension)pedgebox->height,
      XiNx1,     (Position) pedgept->x1,
      XiNy1,     (Position) pedgept->y1,
      XiNx2,     (Position) pedgept->x2,
      XiNy2,     (Position) pedgept->y2,
      XiNzoomFactor,     (Dimension) zoom,
      NULL);

}


void searchGraphEdge( Widget w, GraphElement *client_data,
					 XmPushButtonCallbackStruct *reason)
/****************************************************************************\
*		
*
*	Called by: callback from MOTIF when branch is pushed
\****************************************************************************/
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
  GraphElement *pedge;

    x = event->xbutton.x;
    y = event->xbutton.y;

    findgraphedge(x,y, &pedge);
#ifdef EDGE_FORCE_FIND
    if(pedge==NULL) {
      pedge=client_data;
    }
#endif

    if(ipfdebug & (DB_TraceMask | DB_Edge)) {
	fprintf(ipfdbio, "searchGraphEdge: x,y = (%d,%d)\n",x,y);
	fprintf(ipfdbio, "searchGraphEdge: printGraphElement(client_data)\n");
	printGraphElement(client_data);
        if (pedge != NULL) {
	   fprintf(ipfdbio, "searchGraphEdge: printGraphElement(pedge)\n");
	   printGraphElement(pedge);
	}
    }

    drawAreaButtonCB(w, pedge, reason);

}


void printbranch( Widget w, GraphElement *client_data,
				 XmPushButtonCallbackStruct *reason)
/****************************************************************************\
*     Purpose:
*
*	Called by: callback from MOTIF when branch is pushed
\****************************************************************************/
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
  GraphElement *pedge;

    x = event->xbutton.x;
    y = event->xbutton.y;

    findgraphedge(x,y, &pedge);
    printGraphElement(pedge);

    if(ipfdebug & (DB_TraceMask | DB_Edge)) {
	fprintf(ipfdbio, "printbranch: x,y = (%d,%d)\n",x,y);
	fprintf(ipfdbio, "printbranch: printGraphElement(client_data)\n");
	printGraphElement(client_data);
	fprintf(ipfdbio, "printbranch: printGraphElement(pedge)\n");
	printGraphElement(pedge);
    }

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

Widget drawline( int x1, int y1, int x2, int y2, 
				double base_kv, GraphElement *pedge)
/***************************************************************************\
* author: Dan Clark
* purpose: 
* WARNING: this routine must be used with care since it will create
* lines which do not have a corresponding graphelement data structure
* once a line has been created with no graph element structure it is
* will persist on the screen and some other data structure must be used
* to track that element.
* I recommend that all graphics on the draw area have a corresponding
* graph element structure so that the standard management routines can
* be used to handle the callback data!
* 
\***************************************************************************/
{
  EdgePoint edgept;
  EdgePoint *pedgept = &edgept;
  EdgeBox edgebox;
  EdgeBox *pedgebox = &edgebox;
  Dimension zero = 0;
  static Widget draw_id = FALSE;
  Widget legend_id;
  Widget newwid;
  Pixel color;
  Pixmap color_pixmap;
  Boolean transformer_status;
  static char gadget_name[256];
  static int gadget_cnt = 0;
  XtCallbackRec cblist[32];
  static XmFontList defaultFontList = NULL;
  XmString s2;
  Dimension zoom;
  static Widget chk_font_wid = FALSE;
  char xgui_fontname[256];
  Dimension xgui_size = 15;
  XmString x_str;

  if(edgeGadgetClass == 0) {
    /** note: this call to the edgeg.c module is required ***/
    /** for the vax linker to pull in the EdgeG module   ***/
    printf("ERROR - gadget not registered - attempt recovery\n");
    printf("   value of edgeGadgetClass = %d\n", edgeGadgetClass);
    EdgeGCheckClass();
    printf("  new value of edgeGadgetClass = %d\n", edgeGadgetClass);
  }

  if(draw_id == FALSE ) {
    if((draw_id = widget_id( "drawwindow" )) == FALSE) {
      printf("Warning - no draw window widget id found \n");
      return;
    }
  }
/*  create a single label and pull the default font from it  */
  if(defaultFontList == NULL) {
    sprintf(xgui_fontname, "xgui_font_name");
    chk_font_wid = NULL;
    x_str =  XmStringCreate(xgui_fontname, XmSTRING_DEFAULT_CHARSET);
    chk_font_wid = XtVaCreateManagedWidget( xgui_fontname,
      xmPushButtonGadgetClass, draw_id,
      XmNx, (Position)xgui_size,
      XmNy, (Position)xgui_size,
      XmNheight, (Dimension)xgui_size,
      XmNwidth, (Dimension)xgui_size,
      XmNlabelType, XmSTRING,
      XmNlabelString, x_str,
      XmNuserData, (GraphElement *)pedge, 
      NULL);

    if(chk_font_wid != FALSE) {
      XtVaGetValues(chk_font_wid,
        XmNfontList, &defaultFontList,
        NULL, NULL);

      XtUnmanageChild(chk_font_wid);
    } else {
     printf("edge: Warning - failed to create valid font\n"); 
   }
 /*   XtDestroyWidget(chk_font_wid); */
  }
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

  if(True == istransformer(pedge)) {
    transformer_status = (Boolean)TRUE;
  } else {
    transformer_status = (Boolean)FALSE;
  }

  sprintf(gadget_name, "line_%d", gadget_cnt);
  gadget_cnt++;

/*** this method for adding callbacks is unreliable on OSF machine
* cblist[0].callback = (XtCallbackProc)searchGraphEdge;
* cblist[0].closure = NULL;
* cblist[1].callback = NULL;
* cblist[1].closure = NULL;
*                       XiNinputCallback, (XtCallbackList)&cblist, 
*/
/*** create a specific font list for the solution values **/
/*** this should only be used if other methods fail for a particular O/S
*    test the custom line font list on a vanilla gadget
* if(NULL == defaultFontList) {
*   Widget chk_font_wid;
*   XmFontListEntry entry2, entry3;
*   entry2 = XmFontListEntryLoad (XtDisplay(draw_id), 
*   "courier12", XmFONT_IS_FONT, "TAG2");
*   entry3 = XmFontListEntryLoad (XtDisplay(draw_id), 
*   "courier14", XmFONT_IS_FONT, "TAG3");
*   defaultFontList = XmFontListAppendEntry(NULL, entry2);
*   defaultFontList = XmFontListAppendEntry(defaultFontList, entry3);
*   s2 = XmStringCreate(gadget_name, "TAG2");
*   chk_font_wid = XtVaCreateManagedWidget("test_font_list", 
*     xmLabelGadgetClass, draw_id,
*     XmNx, (Position)pedgebox->x,
*     XmNy, (Position)pedgebox->y,
*     XmNfontList, defaultFontList,
*     XmNlabelString, s2,
*     NULL);
*   XtUnmanageChild(chk_font_wid);
*   XtDestroyWidget(chk_font_wid);
* }
* add this resource to creation of line gadget
*   XmNfontList, (XmFontList)defaultFontList,  
***/  
  zoom = (Dimension)(zoom_factor * (double)100.0);

  newwid = XtVaCreateManagedWidget(gadget_name,
			edgeGadgetClass, draw_id,
			XmNx, (Position)pedgebox->x,
			XmNy, (Position)pedgebox->y,
			XmNwidth,  (Dimension)pedgebox->width,
			XmNheight, (Dimension)pedgebox->height,
			XiNx1, (Dimension)x1,
			XiNy1, (Dimension)y1,
			XiNx2, (Dimension)x2,
			XiNy2, (Dimension)y2,
			XiNboxOn, (Boolean)FALSE,
			XiNzoomFactor, (Dimension)zoom,
			XiNarrowOn, (Boolean)FALSE,
			XiNtransformerOn, (Boolean)transformer_status,
			XiNsymbolSegment, (Boolean)pedge->dply_seg,
			XiNdrawPixel,  (Pixel)color,
			XiNdrawPixmap, (Pixmap)color_pixmap,
			XiNlabelOn, (Boolean)FALSE,
			XiNlineThickness, (Dimension)2,
  			XmNfontList, (XmFontList)defaultFontList,  
			XmNborderWidth, (Dimension)zero,
			XmNhighlightThickness, (Dimension)zero, 
			XmNshadowThickness, (Dimension)zero,
			XmNrecomputeSize, (Boolean)False,
			XmNuserData, (GraphElement *)pedge, 
			NULL);

  XtAddCallback(newwid, XiNinputCallback,(XtCallbackProc)searchGraphEdge,pedge);

  return newwid;
}

toggleEdgeDash(Widget wid)
/************************************************************************\
* author: Dan Clark
* purpose: 
*  toggle the edge from solid to dashed
*  may want to use this to signify an outaged line
\************************************************************************/
{
  Boolean curval;
  if(wid == NULL) {
    printf("ERROR: toggleEdgeDash: bad wid\n");
  }
  else
  {
    XtVaGetValues(wid, XiNdashOn, &curval, NULL);
    XtVaSetValues(wid, XiNdashOn, (Boolean)!curval, NULL);
  }
  return;
}


int setEdgeDash(GraphElement *pedge, Boolean dashval)
/***************************************************************************\
* author: Dan Clark
* purpose: 
*  toggle the edge from solid to dashed
*  may want to use this to signify an outaged line
*  need to loop through all segments for line with bends
\***************************************************************************/
{
  Widget wid;
  Boolean curval;

  wid = pedge->wid;
  if(wid == NULL) {
    printf("ERROR: toggleEdgeDash: bad wid\n");
  } else {
    XtVaGetValues(wid,
                XiNdashOn, &curval,
                NULL);
    XtVaSetValues(wid,
                XiNdashOn, (Boolean)dashval,
                NULL);
  }
  return;
}


/*************************************************************************\
* function lookup_pct_wid
* purpose: to look through a list of percentages and
*     find the widget id corresponding to the threshold range
*     the loop starts at the highest value and checks to see if
*     the percentage passed in is over the value
\*************************************************************************/
double pct_tbl[] = { 0.0, 80.0, 100.0, 120.0 };

char *wid_tbl[] =
{
    "overload_none_label",
    "overload_low_label",
    "overload_moderate_label",
    "overload_high_label"
};


read_overload_boxes( Widget wid, XtPointer tag, XtPointer reason )
/*************************************************************************\
* Purpose:
*	Load the overload threshold values from text boxes.
*
*	Routine called when threshold values change
*	Called on startup to read the values 
* that have been specified in the XGUI file
*
* Author: Dan Clark/Bill Rogers		Aug 13, 1993
*
* Called by:
\*************************************************************************/
{
  /* first lookup and get the string from the screen */
 
  /* then set the values in the pct table array (do not use constants) */
  pct_tbl[3] = get_double_from_text("overload_extreme_text" , "f3.0" );
  pct_tbl[2] = get_double_from_text("overload_moderate_text", "f3.0" );
  pct_tbl[1] = get_double_from_text("overload_mild_text"    , "f3.0" );
  pct_tbl[0] = (double)  0.0;	/* last item always 0.0 */

  if(ipfdebug & (DB_TraceMask | DB_Edge)) {
     fprintf(ipfdbio, "read_overload_boxes: thresholds: %f %f %f %f\n",
	 pct_tbl[3], pct_tbl[2], pct_tbl[1],pct_tbl[0] );
  }

}

double get_double_from_text( char *text_box_name, char *fmt )
{
  char string[20];
  double value;
  int len=3;

  lookup_and_get_field( text_box_name, string, sizeof(string) );

  add_blanks_to_string (  string, len+1 );
  reformat (string, string, fmt );

  sscanf( string, "%lf", &value );

  return ( value );

}

Widget lookup_pct_wid(double pct)
{
 Widget legend_id;
 int idx;
 int tbl_size = (int)sizeof(wid_tbl)/sizeof(wid_tbl[0]);

  idx = tbl_size - 1;
  while(idx > 0 && pct_tbl[idx] >  pct) {
    idx--;
  }

/***
  idx = 0;
  while(idx < tbl_size && pct_tbl[idx] > pct) {
    idx++;
  }
  if(idx >= tbl_size) {
    idx = tbl_size - 1;
  }
***/

  legend_id = widget_id(wid_tbl[idx]);
 
  return legend_id;

}


double basekv_tbl[] = { 500.0, 345.0, 300.0, 230.0, 161.0,
			138.0, 115.0,  69.0,  46.0,  14.0, 7.0 };
char *basekv_wid_tbl[] = { "kv_500_label", "kv_345_label",
			   "kv_300_label", "kv_230_label",
			   "kv_161_label", "kv_138_label",
			   "kv_115_label", "kv_69_label",
			   "kv_46_label" , "kv_14_label",
			   "kv_7_label", };

Widget lookup_kv_wid(double base_kv)
/***************************************************************************\
* Purpose:
* author: Dan Clark
* purpose: determine the wid of the label
* which defines the color for the line gadgets
* a table is setup for the color graduations
\***************************************************************************/
{
  Widget legend_id;
  int idx;
  int kv_tbl_size = (int)sizeof(basekv_wid_tbl)/sizeof(basekv_wid_tbl[0]);

  idx = kv_tbl_size - 1;
  while(idx > 0 && basekv_tbl[idx] < base_kv ) {
    idx--;
  }

  legend_id = widget_id(basekv_wid_tbl[idx]);
 
  return legend_id;
  
}


double setGraphKv(GraphElement *pgraphedgenxt)
/*************************************************************************\
* purpose: determine the kv of an edge drawn on the graph note:
*	   KV color of transformer is set to low kv of line
*	   If bad data is passed in, return kv=0.0
*
* author: Dan Clark
\************************************************************************/
{
  double basekv;
  double basekv2;
  extern double atof_cnt_zero();

    basekv = (double)0.0;
    basekv2 = (double)0.0;
    switch (pgraphedgenxt->class) 
    {
      case GraphClassEdgeSection:
      case GraphClassEdgeSubSection:
      case GraphClassEdgeComplexSection:
        basekv = atof_cnt_zero( &pgraphedgenxt->vertex_id[8], 4);
        basekv2 = atof_cnt_zero( &pgraphedgenxt->farvertex_id[8], 4);
        basekv = (basekv < basekv2 ? basekv : basekv2); /* min < max > */
      break;
      case GraphClassEdgeLegend:
      case GraphClassPaperVertex:
      case GraphClassVertexBorder:
      case GraphClassVertexDrawPoint:
      case GraphClassVertexLabelCorner:
      case GraphClassVertexLabelText:
      case GraphClassVertexLegendCorner:
      case GraphClassVertexLegendText:
      case GraphClassVertexComment:
      case GraphClassVertexSubcomment:
      case GraphClassVertexOrgComment:
      case GraphClassVertexFont:
      case GraphClassVertexGroup:
      case GraphClassEdgeBusLink:
        basekv = (double)230.0;
      break;

      case GraphClassPaperEdge:
        basekv = (double)138.0;
      break;

      case GraphClassEdgeDraw :
      case GraphClassEdgeBorder :
      case GraphClassEdgeLabel:
        basekv = (double)115.0;
      break;

      case GraphClassVertexBus:
      case GraphClassVertexName:
      case GraphClassVertexBendPoint:
      case GraphClassVertexGenerator:
      case GraphClassVertexReactor:
        basekv = atof_cnt_zero( &pgraphedgenxt->vertex_id[8], 4);
      break;

      default:
	printf("** INFO ** from setGraphKv (edge.c)\n" );
	printf("     unknown vertex/edge class (%d)\n", pgraphedgenxt->class );
        basekv = (double)0.0;
      break;
    }

  return basekv;
}


int findgraphedge(int x, int y, GraphElement **pfound)
/*********************************************************************\
*       Purpose: find x,y
*
*  Do a brute force search of all the lines currently displayed in
*  an attempt to locate the line that is currently being pointed at.
*  this routine can be optimized greatly.
*  currently each edge by definition only has 2 vertex links
*  but it is possible to allow for the use of the vertex links
*  to acheive a nicer implementation of bending points.
*  furthermore, it is surely possible to utilize the X and Xt library
*  routines to locate all the gadgets which lie under the current point.
*  this would be a much more effective method of locating a line.
*  since the search of the lines would be limited to only those lines
*  that have a bounding rectangle which covers the current location.
*
*       Author: Dan Clark       Dec, 1991       Modified: Dec 17, 1991
*
* Called by: searchGraphEdge  (this module)
*	     printBranch	  "
\*****************************************************************************/

{
  GraphElement *pgraphnodenxt;
  GraphElement *linkgraphrec;
  GraphLink *llretrec;

  DB_STAT stat;		        /* status returned for general db access */
  DB_STAT stat_link;            /* status returned for ll dmgr access */
  int cnt = 0;
  struct {
      int x;
      int y;
      char vertex_id[12];
    } point[10];
  int i;

  if(ipfdebug & (DB_TraceMask | DB_Edge)) {
     fprintf(ipfdbio, "findgraphedge: start. x,y (%d,%d)\n", x, y);
  }

  stat = firstGE( &pgraphnodenxt, ANY_TYPE );

  if (stat == D_EOF)
  {
    printf("ERROR db_search did not find any records \n");
    return(1);
  }
  if (stat == D_NOTFOUND) {
    stat = D_FOUND;
  }
  while(stat == D_FOUND)
  {
    if(pgraphnodenxt->display != GraphDisplayOn) {
      break;
    };

    if(pgraphnodenxt->type == GraphTypeEdge)
    {

      /** now follow all vertex links **/
      stat_link = ll_first(&graph_db, &pgraphnodenxt, &llretrec,
                GRAPH_VERTEX_LINK);
      if (stat_link != D_FOUND) {
        printf("  ERROR: no vertex links\n");
      }

      cnt = 0;
      while(stat_link == D_FOUND) {
        linkgraphrec = llretrec->graphnode;
        point[cnt].x = linkgraphrec->x;
        point[cnt].y = linkgraphrec->y;
        strncpy(point[cnt].vertex_id, linkgraphrec->vertex_id,
              sizeof(linkgraphrec->vertex_id));
        stat_link = ll_next( &llretrec, &llretrec );
        cnt++;
      }
      for(i = 0; i < floor(cnt/2); i++) {
        if( pointNearSegment ( x, y, point[i*2  ].x, point[i*2  ].y,
				   point[i*2+1].x, point[i*2+1].y) )
        {
          *pfound = pgraphnodenxt;
          return 1;
        }
      }

    }; /* end of edge logic */

    stat = nextGE( &pgraphnodenxt, GRAPH_DISPLAY );
  }
   *pfound = NULL;
   return 0;
}

int pointNearSegment ( int x, int y, int x1, int y1, int x2, int y2 )
/*****************************************************************************\
* Purpose: Check if point x,y is on (near) line defined by x1, y1, x2, y2 
*
*  To reduce time-consuming slope and intercept calculations, a simple
*  elimination process is performed, such that if the point is not
*  inside the "rectangle" formed by the line's end points, then the line
*  is skipped.
*
*  the "REACH" is the allowable "miss" distance.  e.g. REACH=3 indicates
*	that the user must click mouse at a spot 3 pixels or less from
*	the line in order to select the line.
*
*       Author: Bill Rogers     June 5, 1991
*
*       Called by: findgraphedge
*
\*****************************************************************************/
#define REACH           3
#define INFINITY        99999
{
  int   x_lo, x_hi, y_lo, y_hi;
  double x_gap, y_gap, slope, y_int;

  if(ipfdebug & (DB_TraceMask | DB_Edge)) {
     fprintf(ipfdbio, "pointNearSegment: start. x,y (%d,%d) ", x, y);
     fprintf(ipfdbio, "x1,y1 (%d,%d) x2,y2 (%d,%d)\n", x1, y1, x2, y2);
  }

     if ( x1<= x2 )
     {
       x_lo = x1 - REACH;
       x_hi = x2 + REACH;
     }
     else
     {
       x_lo = x2 - REACH;
       x_hi = x1 + REACH;
     }

     if ( y1<= y2 )
     {
       y_lo = y1 - REACH;
       y_hi = y2 + REACH;
     }
     else
     {
       y_lo = y2 - REACH;
       y_hi = y1 + REACH;
     }

     /* check if pointer is in the rectangle of this line segment */
     if ( x>=x_lo && x<=x_hi && y>=y_lo && y<=y_hi )
     {

       slope = slopeOfTwoPts ( x1, y1, x2, y2 );
       y_int = yInterceptOfSlope ( slope, x1,y1 );

       y_gap = ( y - y_int + slope * x );
       if ( y_gap < 0 ) y_gap = -y_gap;

       if ( slope ==0 )
         x_gap=INFINITY;             /* level line */
       else if ( slope ==INFINITY )
         x_gap = x - x1;     /* vertical line */
       else
         x_gap =  ( y_int - y )/slope - x;

       if ( x_gap < 0 ) x_gap = -x_gap;

       if ( x_gap <= REACH || y_gap <= REACH ) return ( 1 );

     }

      return ( 0 );   /* point is not near this segment */

}

double slopeOfTwoPts ( int x, int y, int x2, int y2 )
/******************************************************************************\
*       Function: SLOPE OF TWO POINTS
*                 give the x,y coordinates of two points,
*                 return the slope from point 1 to point 2
*
*       Author: Bill Rogers     June 5, 1991
*
\******************************************************************************/
{
    if ( x2 == x )
    {
       return ( INFINITY );
    }
    else
    {
       return ( (double)((y-y2))/(double)((x2-x)) );

        /* NOTE: this is inversed Y formula
                 So it returns a slope value as seen
                 by the viewer of an Xwindow system   */
    }
}

double yInterceptOfSlope ( double slope, int x, int y )
/******************************************************************************\
*       Purpose: Y-INTERCEPT OF SLOPE
*                 Returns the y-intercept of a line with a
*                 given slope and x, y point.
*
*       Author: Bill Rogers     June 11, 1991
*
\******************************************************************************/
{

    if ( slope==INFINITY )
    {
       return ( INFINITY );
    }
    else
    {
       return ( y + slope*x );
        /* NOTE: this is inversed Y formula */
    }

}


void getPerimeterPts( GraphElement *pgraphnode1, GraphElement *pgraphnode2,
				  int *x, int *y, int *x2, int *y2 )
/***************************************************************************\
* Purpose: Given two GraphElement vertexes, computer 'endpts' of line
*	   connecting these vertexes.
*
* 
\***************************************************************************/
{
  double radians;
  int radius;

  *x = pgraphnode1->x;
  *y = pgraphnode1->y;
  *x2= pgraphnode2->x;
  *y2= pgraphnode2->y;

  radians = radiansBetweenPts( *x, *y, *x2, *y2 );

  switch ( pgraphnode1->class )
  {
    case GraphClassVertexBus:
      if( pgraphnode1->hide == NO_HIDE )
        radius = icon_bus_width;
      else
	radius = HIDDEN_BUS_RADIUS;

      if( icon_bus_shape == CIRCLE )
        circlePerimeter( radians, radius, x, y );
      else
	rectPerimeter( radians, icon_bus_width, icon_bus_height, x, y );

    break;

    case GraphClassVertexGenerator:
      circlePerimeter( radians, GEN_RADIUS, x, y );
    break;

    case GraphClassVertexCapacitor:
      if( radians < 3.14159 )
	*y += 11;
    break;

    case GraphClassVertexReactor:
      if( radians < 3.14159 )
	*y += 24;
    break;

    case GraphClassVertexBendPoint:
      circlePerimeter( radians, PT_RADIUS, x, y );
    break;

    case GraphClassVertexDrawPoint:
    case GraphClassVertexLabelCorner:
    case GraphClassVertexLegendCorner:
    case GraphClassVertexBorder:
    case GraphClassPaperVertex:
      circlePerimeter( radians, PT1_RADIUS, x, y );
    break;


    default:
      printf("** ERROR ** from getPerimeterPts (edge.c)\n" );
      printf("       Cannot handle class type (%d)\n", pgraphnode1->class );

  }

  radians += 3.1415927;
  if ( radians > 6.2831853 ) radians-=6.2831853;

  switch ( pgraphnode2->class )
  {
    case GraphClassVertexBus:
      if( pgraphnode2->hide == NO_HIDE )
        radius = icon_bus_width;
      else
	radius = HIDDEN_BUS_RADIUS;

    if( icon_bus_shape == CIRCLE )
      circlePerimeter( radians, radius, x2, y2 );
    else
      rectPerimeter( radians, icon_bus_width, icon_bus_height, x2, y2 );

    break;

    case GraphClassVertexGenerator:
      circlePerimeter( radians, GEN_RADIUS, x2, y2 );
    break;

    case GraphClassVertexCapacitor:
      if( radians < 3.14159 )
	*y += 11;
    break;

    case GraphClassVertexReactor:
      if( radians < 3.14159 )
	*y += 24;
    break;

    case GraphClassVertexBendPoint:
    case GraphClassVertexDrawPoint:
    case GraphClassVertexLabelCorner:
    case GraphClassVertexLegendCorner:
    case GraphClassVertexBorder:
    case GraphClassPaperVertex:
      circlePerimeter( radians, PT_RADIUS, x2, y2 );
    break;

    default:
      printf("** ERROR ** from getPerimeterPts (edge.c)\n" );
      printf("       Cannot handle class type (%d)\n", pgraphnode2->class );

  }

}




int roundoff( double fnum )
/*******************************************************************\
* 	Round off a floating point num to nearest integer
\*******************************************************************/
{
  if (fnum>=0 ) return ((int)(fnum+.5));

  return ((int)(fnum-.5));
}

void squarePerimeter( double radians, int size, int *x, int *y )
{
  int angle;
  angle   = roundoff( radians*RadtoDeg ); 

  if( angle>45 && angle < 135 )
  {
    *x+=(size + cos(radians)*size);
    *y+=(size+size+1);
  }
  else if( angle > 225 && angle < 315 )
  {
    *x+=(size + cos(radians)*size);
    *y-=1;
  }
  else if( angle >= 135 && angle <= 225 )
  {
    *x-=1;
    *y+=(size + sin(radians)*size );
  }
  else
  {
    *x+=(size+size+1);
    *y = *y + size + roundoff(sin(radians)*size);
  }

}

void circlePerimeter( double radians, int size, int *x, int *y )
{
  *x = *x + roundoff( cos(radians)*(size+2) );
  *y = *y + roundoff( sin(radians)*(size+2) );
}

#define PI2	1.5707963
#define PI	3.1415927
void rectPerimeter( double radians, int bus_wd, int bus_ht, int *x, int *y )
{

  double r1,r2,r3,r4;
/*  int x_wd, y_ht; */

  r1 = atan2( bus_ht, bus_wd );
  r2 = PI - r1;
  r3 = r1 + PI;
  r4 = 2*PI - r1;

/*  x_wd = bus_wd/2+1;
  y_ht = bus_ht/2+1;*/

  if( radians < r1 )
  {
    *x = *x + bus_wd;
    *y = *y + roundoff( bus_wd*tan(radians) );
  }
  else if ( radians > r1 && radians < r2 )
  {
    *x = *x + roundoff( bus_ht *tan(PI2-radians));
    *y = *y + bus_ht;
  }
  else if ( radians > r2 && radians < r3 )
  {
    *x = *x - bus_wd;
    *y = *y - roundoff( bus_wd*tan(radians) );
  }
  else if ( radians > r3 && radians < r4 )
  {
    *x = *x + roundoff( bus_ht*tan(radians-PI2) );
    *y = *y - bus_ht;
  }

}
