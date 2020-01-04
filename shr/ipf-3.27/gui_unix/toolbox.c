static char sccsid[] = "@(#)toolbox.c	20.62 7/6/98";
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
/**************
* file: toolbox.c
* author: Dan clark
* purpose: consolidate toolbox action functions
*
* modified: WDRogers 30 August 1994
* added debug flags, removed some old #ifdef code
*
***************
*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <Xm/Xm.h>	
#include <Xm/Text.h>
#include "dmgr.h"
#include "graph_data.h"	
#include "coord_data.h"	
#include "EdgeG.h"
#include "ipfdebug.h"
#include "em.h"	
em_init	

typedef enum toolboxButton {
 tbInput,
 tbOutput,
 tbIO,
 tbStar,
 tbDelete,
 tbOutage,
 tbUndelete,
 tbUnOutage,
 tbNewBend,
 tbMove,
 tbAddBus,
 tbAddBranch,
 tbAddDrawLine,
 tbAddText,
 tbGroupSelect,
 tbCoordEdit,
 tbNull,
 tbLimit
} ToolboxButton;

ToolboxButton findbutton	();
extern void setobject		( GraphElement *client_data );
extern GraphElement *getobject	();
extern double atof_cnt_zero	( char *basekv, int length);
extern int pfReport		(char *instr);
extern void display_bus_selection ( char * bus_string, DBID junkbus_br_db );
extern int check_powerflow	( char *namebase);
extern int setDashAllBusEdges	( GraphElement *pparentedge, Boolean flag);
extern int ipc_synch_rw		( char *rtntext, char *text);
extern Widget  widget_id	();
extern int  pfchanges_cb 	( Widget, XtPointer,
				  XmFileSelectionBoxCallbackStruct *);
extern void explodeBus		( GraphElement *pelement);
extern int  setEdgeDash		( GraphElement *pedge, Boolean dashval );
extern int  manageVertex	( GraphElement *pvertex );
extern int  manageEdge		( GraphElement *pedge );
extern void explodeBranches	( GraphElement * );
extern int  updateEdge		( GraphElement * );
extern void resetCurBusDefaultName( Widget w, XtPointer tag, XtPointer reason );
extern void pscreateGraphCorEdge( GraphElement *vertx_1, GraphElement *vertx_2,
                                  char *key,
                                  GraphClass class,
				  int seg,
                                  GraphElement **new_rec );
extern int getCurBus		( char ** );
extern int  Vuit_MU		( char *, char * );
extern void disable_pushbutton	( char * );
extern void enable_pushbutton	( char * );
extern void lookup_and_fill_field ( char *, char *, int, int );
extern void set_rec_type	( char rec, char stype, int info, int mode );
extern void lookup_and_get_field( char *, char *, int );
extern void fill_bus_dialog2	( char * );
extern void manage_not_implemented();
extern void strcat_field	( char *, char *, int, char *fmt );
extern void fill_branch_jckt	( char * );
extern void start_rubberband	( XEvent *pevent );
extern void stop_rubberband	( Widget w, XtPointer data, XEvent *pevent );
extern void create_bus_rec	();
extern void create_line_rec_2bus( char * );
extern int setCurBus            (char *namebase);
extern int reformat		( char *, char *, char * );
extern void fetch_widget	( char *wname );
extern int pf_get_lines		( GraphElement *pge, XEvent *pevent);
extern int rescale_graph	( double xftr, double yftr, int flag );
extern void display_coord_data	( GraphElement *vertex );
extern void activate_coord_update ( Widget w, XtPointer tag,
                                 XmAnyCallbackStruct *cbs );
extern void deactivate_coord_update ( Widget w, XtPointer tag,
                                 XmAnyCallbackStruct *cbs );


extern void display_graph_coord_data	( GraphElement *pvertex );
extern void psUserCreateDrawVertex( int x, int y, GraphElement **ge );
extern void create_and_manage_scrn_vertex( char *, int x, int y,
                 GraphElement **ppnew_vertex, GraphClass class );
extern Boolean bus_not_created	( );
extern void cascade_pd_label_set( char *, char * );
extern void draw_outline	( GraphElement *object );
extern GraphElement *createGraphBusXYXY( char *name, int x, int y,
                         int xn, int yn, char *gen_a, char *cap_a,
			 int m_flg, int x_flg, int pf_flg,
			 int g_flg, int r_flg );
extern int pscoord_to_MOTIF_x   ( char *x, int col, char *bus_name );
extern int pscoord_to_MOTIF_y   ( char *y, int col, char *bus_name );
extern DB_STAT findGraphEdgebyName2( char *namebase1, char *namebase2,
                                      GraphElement **pedge );
 

void sendOutageCard		( GraphElement *pedge, int flag);
void addGraphBus		( XmPushButtonCallbackStruct * );
void breakEdge			( GraphElement *pedge,int x, int y);
void deleteBendVertex		( GraphElement * );
void addGraphBranch		( GraphElement *,XmPushButtonCallbackStruct *);
void line_drag_switches		( Boolean p1 );
void set_button_sensivity       ( char *widget_name, Boolean v1 );
void addDrawLine
         ( GraphElement *client_data, XmPushButtonCallbackStruct *call_data );
void manage_front_box		( GraphElement *start_bus,
				  GraphElement *end_bus );
void userCreateComment		( XmPushButtonCallbackStruct * );
void remove_possible_group_links( GraphElement *client_data );


#define UNKNOWN_INFO            0
#define BUS_INFO                1       /* flag to set when editing bus data */
#define CONT_INFO               2       /*      "       "       "  +bus data */
#define BRANCH_INFO             3       /*      "       "        branch data */
#define TRANSF_INFO             4       /*      "       "    transformer data */
#define AREA_INFO               5       /*      "       "    area data        */
#define INTERTIE_INFO           6       /*      "       "    interchange data */
#define PQ_CURVE_INFO           7
#define REAC_INFO               8
#define EQUIV_INFO              9
#define REGXFMR_INFO            10

#define BUS_PULLDOWN    0
#define LINENAMEONLY    1
#define ONE_NAME_BASE   2
#define TWO_NAME_BASE   3

#define NODRAG -1
#define DRAG 1

#define ABS             0 	/* not used */
#define REL             1

double zoom_factor = 1.0;
static GraphElement *drag_vtx_id;
static int dragLineStatus = NODRAG;
static int stickyOn;





/***************
* author: Dan Clark
* date: Sept 93
* purpose: create a bounding box to deliniate the contents of a group.
*****************
*/
typedef struct {
 int x1;
 int y1;
 int x2;
 int y2;
} BoundingBox;


drawbox(BoundingBox *pbox)
/*************************************************************************\
* draw the box defined by the coordinates passed in 
* first time through create the XOR drawing context 
* which will be used for all subsequent draws
\*************************************************************************/
{
  static Widget drawing_area = NULL;
  int x, y, width, height;
  int fg, bg;		/* active foreground and background of drawing area */
  XGCValues xgcv;       /* mask of values desired from window graphic cntx */
  static GC xorGC;      /* graphics context to draw/undraw drag image */

  if( drawing_area == NULL) {
    if((drawing_area = widget_id("drawwindow")) == FALSE) {
      return;
    }
    /* create graphics context */
    XtVaGetValues( drawing_area,
      XmNforeground, &fg,
      XmNbackground, &bg,
      NULL);

    xgcv.foreground = fg ^ bg;
    xgcv.background = bg;
    xgcv.function = GXxor;
    xorGC = XtGetGC(drawing_area, GCForeground | GCBackground | GCFunction,
		        &xgcv);
  }

  if(pbox->x2 < pbox->x1)
    x = pbox->x2;
  else
    x = pbox->x1;
 
  if(pbox->y2 < pbox->y1)
    y = pbox->y2;
  else
    y = pbox->y1;
 
  width = abs(pbox->x2 - pbox->x1);
  height = abs(pbox->y2 - pbox->y1);
  XDrawRectangle(XtDisplay(drawing_area), XtWindow(drawing_area), 
                 xorGC, x, y, width, height);

}


void drag_box_flex( Widget w, XtPointer data, XEvent *pevent)
/**************************************************************************\ 
* as the cursor moves this callback will be called automatically
*  by the X windows event manager.  For each call, xor out the
*  previous box and draw the outline of a box at the new cursor
*  position.
\**************************************************************************/
{
  int xfig, yfig;
  BoundingBox *pbox = (BoundingBox *)data;

  drawbox(pbox);
  pbox->x2 = pevent->xbutton.x;
  pbox->y2 = pevent->xbutton.y;
  drawbox(pbox);

}



/**
* when the push button is released this callback will be invoked
* to remove the calls for flexing the box around on the screen
* and the last box which appears will be erased
**/



void drag_box_end( Widget w, XtPointer data, XEvent *pevent)
{
  int xfig, yfig;
  BoundingBox *pbox = (BoundingBox *)data;
  Widget drawing_area;

  if((drawing_area = widget_id("drawwindow")) == FALSE) {
    return;
  }

  drawbox(pbox);
  XtRemoveEventHandler(drawing_area, ButtonMotionMask, False,
       (XtEventHandler)drag_box_flex, pbox);
  XtRemoveEventHandler(drawing_area, ButtonReleaseMask, False,
       (XtEventHandler)drag_box_end, pbox);
  free(pbox);
}

/*
* this routine sets the flexing box in motion
* . define the data structure of the initial box.
* . draw a box
* . setup the callbacks to continue to size the box
*   based on the current cursor position, until the
*   button is released.
*/

drag_box_start(int x, int y)
{
  Widget drawing_area;
  BoundingBox *pbox;

  if((drawing_area = widget_id("drawwindow")) == FALSE) {
    return;
  }
  pbox = (BoundingBox *)malloc(sizeof(BoundingBox));
  if(pbox == NULL) {
    printf("ERROR during malloc of memory\n");
    return;
  }
  pbox->x1 = x;
  pbox->y1 = y;
  pbox->x2 = x+5;
  pbox->y2 = y+5;

  drawbox(pbox);
  XtAddEventHandler(drawing_area, ButtonMotionMask, False,
    (XtEventHandler)drag_box_flex, pbox);
  XtAddEventHandler(drawing_area, ButtonReleaseMask, False,
    (XtEventHandler)drag_box_end, pbox);

}


void group_all_objects(int x1, int y1, int x2, int y2)
/**************************************************************************\
* the routine used to locate all the bus objects which are
* located within a rectangle bound by the coordinates
* given.  This routine builds a linked list of all the
* busses and puts them on a newly created group object
* labeled with the group name.  Moving the group object on
* the screen, moves all vertex neighbors with that object.
***************************************************************************/
{
  GraphElement graphnode;
  GraphElement *pgraphnode = &graphnode;
  GraphElement *pgraphnodenxt;
  GraphElement *pgraphgroup;

  GraphElement *linkgraphrec;
  GraphLink *llretrec;
  DB_STAT stat;         /* status returned for general db access */
  DB_STAT stat_link;            /* status returned for ll dmgr access */
  static int cnt = 0;
  char name[128];
  int buscnt;

  stat = firstGE( &pgraphnodenxt, ANY_TYPE );

  if (stat == D_EOF)
  {
    printf("ERROR db_search did not find any records \n");
  }
  if (stat == D_NOTFOUND) {
    stat = D_FOUND;
  }

  sprintf(name, "GROUP   %4.4d", ++cnt);
  createGraphVertex( GraphClassVertexGroup,
                name,
                x1,
                y1,
		' ',
                &pgraphgroup );

  manageVertex( pgraphgroup );

  if(x2 < x1) {
    int tmp;
    tmp = x1;
    x1 = x2;
    x2 = tmp;
  }

  if(y2 < y1) {
    int tmp;
    tmp = y1;
    y1 = y2;
    y2 = tmp;
  }
  buscnt = 0;
  while(stat == D_FOUND)
  {
    /** now look for all vertices in bounds and move **/
    if(pgraphnodenxt->type == GraphTypeVertex) {
      int curx, cury;
      curx = (int)pgraphnodenxt->x;
      cury = (int)pgraphnodenxt->y;
      if( ( curx >= x1) && (curx <= x2) && ( cury >= y1) && (cury <= y2) )
      {
        if(pgraphnodenxt->class == GraphClassVertexBus ||
           pgraphnodenxt->class == GraphClassVertexBendPoint ||
	   pgraphnodenxt->class == GraphClassVertexComment ||
	   pgraphnodenxt->class == GraphClassVertexSubcomment )
        {
           buscnt++;
           addGraphLink( pgraphgroup, pgraphnodenxt );	/* link to group     */
        }
      }
    }

    stat = nextGE( &pgraphnodenxt, GRAPH_DISPLAY ); /** next GraphElement **/

  }

  if(buscnt <= 0) {
    printf("Warning - no busses put into %s\n", name);
  }

}


void drawAreaButtonCB( Widget w, GraphElement *client_data, 
			XmPushButtonCallbackStruct *call_data)
/****************************************************************************\
*     Purpose:
*     callback routines invoked by pushing gadget buttons
*     used to invoke dialog and list popups from graph
*****************************************************************************/
{

  XButtonEvent *pbuttonevent;
  ToolboxButton tb_item;
  Widget drawing_area;
  GraphElement *pparent;
  GraphElement *pbus;
  PSCoordFileRecord *nextrec;
  int rtncde;
  char buskey[33];
  DBID junkdb;

  static int x1,y1,x2,y2;

  memset(buskey, '\0', sizeof(buskey));

  if (ipfdebug & (DB_TraceMask | DB_Toolbox)) {
	fprintf(ipfdbio, "drawAreaButtonCB: start\n");
  }

  if((drawing_area = widget_id("drawwindow")) == FALSE) return;

  pbuttonevent = (XButtonEvent *)call_data->event;
  switch(pbuttonevent->type)
  {
    case ButtonPress:
      /* printf("OK: button press\n");  */
    break;
    case ButtonRelease:
      /* printf("OK: button release\n"); */
    break;
    default:
      printf("Warning: only handle button press and release\n");
    break;
  }

  tb_item = findbutton();
  if( tb_item >= tbLimit ) tb_item = tbInput;		/* ensure button set */

  if(client_data == NULL )
  { 
    if ( tb_item != tbAddBus &&			/* these items don't need */
	 tb_item != tbAddText &&		/* to start on an existing*/
	 tb_item != tbAddDrawLine &&		/* vertex		  */
         tb_item != tbGroupSelect)
    {
       switch(pbuttonevent->type)
       {
          case ButtonPress:
	  /* no bus item selected */
 	  break;
       }
       return;
    }
  }
  else
  {
    if(client_data->type == GraphTypeVertex) {	/* vertex */
      setobject( client_data );
      if( client_data->class == GraphClassVertexBus )
      {
        sprintf(buskey, "%12.12s", client_data->vertex_id);
        setCurBus( buskey );
      }
      else if( client_data->class == GraphClassVertexName )
      {
        rtncde = findBusfromNameVertex(&pbus, client_data);
        if(0 == rtncde)
	{
          sprintf(buskey, "%12.12s", pbus->vertex_id);
          setCurBus( buskey );
        }
	else
	{
          /* bus not found reset pbus*/
          printf("Warning - no bus found for name\n");
        }
      }
    }
    else
    {
      setobject( NULL );		/* edge   */
    }
      /* printGraphElement(client_data); */
  }

  switch( tb_item )
  {

    case tbInput:
    {
      char value_str[133];
      if( client_data->class == GraphClassVertexBus ||
          client_data->class == GraphClassVertexName )
      {
          Vuit_MU ( "M", "bus_front_box");
          Vuit_MU ( "U", "bus_front_line_name" );
          sprintf( value_str,  "B     %12.12s", buskey);
          display_bus_selection( &value_str[0], junkdb);
      }
    } /* esac */
    break;

    case tbCoordEdit:
      activate_coord_update( 0, 0, 0 );
      display_coord_data( client_data );
    break;

    case tbOutput:
      if( client_data->class == GraphClassVertexBus ||
          client_data->class == GraphClassVertexName )
      {
          pfReport(buskey);
      }
    break;

    case tbIO:
    {
      char value_str[133];
      if( client_data->class == GraphClassVertexBus ||
          client_data->class == GraphClassVertexName )
      {
          Vuit_MU ( "M", "bus_front_box");
          Vuit_MU ( "U", "bus_front_line_name" );
          sprintf( value_str,  "B     %12.12s", buskey);
          display_bus_selection( &value_str[0], junkdb);
          pfReport(buskey);
      }
    } /* esac */
    break;
  
    case tbStar:		/* explode */
      if( client_data->class == GraphClassVertexBus ||
          client_data->class == GraphClassVertexName ) {
          explodeBus(client_data);
      }
    break;

    case tbDelete:

      switch( client_data->class )
      {
	case GraphClassVertexBendPoint:
	  deleteBendVertex( client_data );
	break; 

	case GraphClassVertexBus:
          deleteBusVertex( client_data );
	break;
 
        case GraphClassVertexName:
          rtncde = findBusfromNameVertex(&pbus, client_data);
          if(0 == rtncde)
	  {
            deleteBusVertex( pbus );
	    client_data = pbus;
	  }
	break;

        case GraphClassVertexComment:
        case GraphClassVertexSubcomment:
        case GraphClassVertexGroup:
	  deleteGraphElement( client_data );
	break;

        case GraphClassEdgeSection:
        case GraphClassEdgeComplexSection:
          if( pbuttonevent->type == ButtonRelease )
	    printf("Must use 'wire cutter' toolbox symbol to outage line.\n");
	break;

	default:
	  printf("Can't delete this class of items\n" );
      }

      remove_possible_group_links( client_data );
 
      setobject( NULL );
    break;

    case tbGroupSelect:
    {
      XEvent *event = call_data->event;

      if(event->xany.type == ButtonPress) {
        x1 = event->xbutton.x;
        y1 = event->xbutton.y;
        drag_box_start(x1,y1);
      }
      else if (event->xany.type == ButtonRelease)
      {
        x2 = event->xbutton.x;
        y2 = event->xbutton.y;
        /* drag_box_stop(x2,y2); */
        group_all_objects( x1,y1,x2,y2 );
      }
    }
    break;

    case tbMove:
        if(client_data->class == GraphClassVertexBus		||
           client_data->class == GraphClassVertexName		||
           client_data->class == GraphClassVertexGenerator	||
           client_data->class == GraphClassVertexCapacitor	||
           client_data->class == GraphClassVertexReactor	||
           client_data->class == GraphClassVertexGroup		||
           client_data->class == GraphClassVertexComment	||
           client_data->class == GraphClassVertexDrawPoint	||
/*           client_data->class == GraphClassVertexSubcomment	||
*/
           client_data->class == GraphClassVertexBendPoint )
        {
        XtAddEventHandler(drawing_area, ButtonReleaseMask, False,
                          (XtEventHandler)stop_rubberband, NULL);
        start_rubberband( (XEvent *)pbuttonevent );
      }
    break;

    case tbOutage:
      if(call_data->event->xany.type == ButtonPress) {
        if( client_data->class == GraphClassEdgeSection ) {
          findComplexEdge(&pparent, client_data);
          pf_get_lines(pparent, (XEvent *)pbuttonevent);
          /* setDashAllSubEdges(pparent, True); */
          /* setEdgeDash(client_data, True); */
        }
        else if( client_data->class == GraphClassVertexBus )
        {
          /** loop through all edges for bus & set edge dashed on */
          setDashAllBusEdges(client_data, True);
          sendOutageCard(client_data, 1);
        }
      }
    break;

    case tbUnOutage:
      if( client_data->class == GraphClassEdgeSection ) {
        findComplexEdge(&pparent, client_data);
        setDashAllSubEdges(pparent, False);
        /* setEdgeDash(client_data, False); */
      }
      else if( client_data->class == GraphClassVertexBus )
      {
        /** loop through all edges for bus & set edge dashed on */
        setDashAllBusEdges(client_data, False);
      }
      sendOutageCard(client_data, 0);
    break;

    case tbAddBus:
      if( pbuttonevent->type == ButtonPress ){
        /* printf("ADDBUS button logic \n"); */
        /*if( bus_not_created() == FALSE )
	{*/
	  addGraphBus( call_data );
	  Vuit_MU( "U", "bus_help_dialog" );
	/*}*/
      }
    break;

    case tbAddBranch:
    {
      addGraphBranch( client_data, call_data );
    }
    break;

    case tbNewBend:		
      if(call_data->event->xany.type == ButtonPress) {
        XtAddEventHandler(drawing_area, ButtonReleaseMask, False,
                        (XtEventHandler)stop_rubberband, NULL);
        if( client_data->class == GraphClassEdgeSection ) {
          XEvent *event = call_data->event;
          breakEdge( client_data, event->xbutton.x, event->xbutton.y);
	}
        start_rubberband( (XEvent *)pbuttonevent );
      }
    break;

    case tbAddText:
      if( pbuttonevent->type == ButtonPress )
      {
	userCreateComment( call_data );
      }
    break;

    case tbAddDrawLine:
    {
      addDrawLine( client_data, call_data );
    }
    break;

    case tbUndelete:
      /* printf("Not Yet implemented\n"); */
      manage_not_implemented(0,0,0);
    break;

    case tbNull:
      printf("No toolbox button has been previously set.\n" );
    break;

    default:
      printf("*** Warning from drawAreaButtonCB (toolbox.c)\n");
      printf("  No code written to handle toolbox button %d\n", tb_item);
      printf("  At least one toolbox button must be pushed.\n" );
    break;

  }

  if (ipfdebug & (DB_TraceMask | DB_Toolbox))
  {
    fprintf(ipfdbio, "drawAreaButtonCB: call_data->  reason %d, ",
		call_data->reason);
    fprintf(ipfdbio, "event->type %d, ", call_data->event->type);
    fprintf(ipfdbio, "click_count=%d\n", call_data->click_count);
  }

}


void draw_area_input(Widget wid,XtPointer data,XmDrawingAreaCallbackStruct *cbs)
/***************************************************************************\
*       purpose: To process & route user buttonPress in draw area.
*
*       author: Dan Clark       Nov 1992
*
*       called by: ButtonPush & ButtonRelease in draw area
\***************************************************************************/
{
  int x,y;
  XEvent *event = cbs->event;
  GraphElement *pedge = NULL;

  if (ipfdebug & (DB_TraceMask | DB_Toolbox)) {
	fprintf(ipfdbio, "draw_area_input: start\n");
  }

  if(cbs->reason == XmCR_INPUT)
  {
    if(event->xany.type == ButtonPress)
    {
      /* filter down to the same set of callbacks that an arm produces */
      drawAreaButtonCB(wid, pedge, (XmPushButtonCallbackStruct *)cbs);
    } else if(event->xany.type == ButtonRelease) {
      /* printf("OK look at button releases too\n"); */
      drawAreaButtonCB(wid, pedge, (XmPushButtonCallbackStruct *)cbs);
    }
  }
  else
  { printf("ERROR - drawAreaInput: bad callback reason\n"); }
    /*  possible reasons: XmCR_EXPOSE, XmCR_RESIZE */

  if (ipfdebug & (DB_TraceMask | DB_Toolbox)) {
	fprintf(ipfdbio, "draw_area_input: reason %d, event %d, x=%d, y=%d\n",
		cbs->reason,event->xany.type,event->xbutton.x,event->xbutton.y);
  }

}

void addBus2Base( Widget wid, XtPointer tag, XtPointer reason)
/***********************************************************************\
* snap up the input dialog for adding a bus to the system.
* if the user desires after each add bus from the dialog the
* bus can be positioned on the screen. 
*
* Called by: ???
\************************************************************************/
{
  char *namebase;

  if (ipfdebug & (DB_TraceMask | DB_Toolbox)) {
	fprintf(ipfdbio, "addBus2Base: start\n");
  }

  getCurBus(&namebase);
  if(FALSE == check_powerflow(namebase)) {
   /** bring up dialog box with name newbus 500 ***/
   create_bus_rec();
  }
}


void addGraphBusXY ( int x, int y)
/*****************************************************************************\
*
* purpose:	adds a bus symbol and a name/base entry to graph_db
*		
*	Author: Bill Rogers / Dan Clark	Nov, 1992
*
* conditions to beware of
* 1) add a bus with a name that already exists in the coordinate file
* and is displayed - popup error box.
* 2) should have a easy reference list for all the busses that
* are in the coord file with a flag for which ones are not displayed.
* sort to the top the busses that are not displayed
* this allows us to easily add these busses to the base case without
* typing anything AND helps us just put up an error for condition 3
* must have popup bus dialog to fill in base data to send to powerflow
* 3) add a bus with a name in coord file that is not displayed (and
* therefore is not in the base case.)
* need to popup dialog to allow bus to be added to the base and
* put bus symbol at location already designated in coord file.
* and putup a message that tells the user that bus was located
* and turns on the bending points and connections.
* 4) add a bus which is neither in base or coord file
*  what do we do now...
* ? place the bus at the location
*   default name to New Name 500
*   popup add bus dialog and force them to fill in 
*   if user cancels from this dialog...remove the bus from the diagram.
*
\*****************************************************************************/
{

  GraphElement *pgraphbusrec;

  char *curkey;
  char key[128];
  char namebase[128];
  char str[16];
  int g_flg, r_flg;
  double value;

  if(-1 != getCurBus(&curkey)) {
    sprintf(key, "B1%12.12s%8.8s3", curkey, curkey);
    sprintf(namebase, "%12.12s", curkey);
  }

  g_flg = 0;
  r_flg = 0;

  lookup_and_get_field( "bus_p_gen", str, 4 );
  value = atof_cnt_zero( str, 5 );
  if( value > 0.0 )
    g_flg = 1;
  else
  {
    lookup_and_get_field( "bus_p_max", str, 4 );
    value = atof_cnt_zero( str, 5 );
    if( value > 0.0 )
      g_flg = 1;
    else
    {
      lookup_and_get_field( "bus_q_min", str, 4 );
      value = atof_cnt_zero( str, 5 );
      if( value < 0.0 )
        g_flg = 1;
      else
      {
        lookup_and_get_field( "bus_q_sched", str, 4 );
        value = atof_cnt_zero( str, 5 );
        if( value < 0.0 )
          g_flg = 1;
      }
    }
  }

  lookup_and_get_field( "bus_q_shunt", str, 4 );
  value = atof_cnt_zero( str, 5 );
  if( value > 0.0 )
    r_flg = 1;

  pgraphbusrec = createGraphBusXYXY( namebase, x+1, y+1, x+1, y+31, "30", "60",
     1,		/* manage */
     1,		/* explode branches */
     1,		/* needs to exist in pf */
     g_flg,	/* gen symbol  flg	*/
     r_flg );	/* reac symbol flg	*/

 
}

void addGraphBus ( XmPushButtonCallbackStruct *call_data)
{
  XEvent *event = call_data->event;

  if (ipfdebug & (DB_TraceMask | DB_Toolbox)) {
      fprintf(ipfdbio, "addGraphBus: x = %d, y = %d\n",
              (int)event->xbutton.x, (int)event->xbutton.y);
  }
  /* printf("Add new bus at (%d,%d)\n", event->xbutton.x, event->xbutton.y); */

  addGraphBusXY ( (int)event->xbutton.x, (int)event->xbutton.y);

}


/***
* purpose: 
*  add a new line to the graph
*/
int add_graph_line(char *namebase12)
{
  DB_STAT stat;
  GraphElement *pgraphbus_1;
  GraphElement *pgraphbus_2;
  GraphElement *pnewedge;
  GraphElement *pcomplex_edge;
  GraphElement updatedata;
  GraphElement *pupdatedata = &updatedata;
  char namebase[14];
  PSBranchRecord linekey; 

  if (ipfdebug & (DB_TraceMask | DB_Toolbox)) {
	fprintf(ipfdbio, "add_graph_line: start\n");
  }

    sprintf(namebase, "%12.12s", &namebase12[0]);
    reformat(&namebase[8], &namebase[8], "%f4.0");
    stat = findBusGraphVertexRec(namebase,&pgraphbus_1);
    if(D_FOUND != stat) {
      printf("Warning: bus 1 not found for line %25.25s\n", namebase12);
      return -1;
    }
    sprintf(namebase, "%12.12s", &namebase12[13]);
    reformat(&namebase[8], &namebase[8], "%f4.0");
    stat = findBusGraphVertexRec(namebase,&pgraphbus_2);
    if(D_FOUND != stat) {
      printf("Warning: bus 2 not found for line %25.25s\n", namebase12);
      return -1;
    }
    stat = findGraphEdgebyName2(&pgraphbus_1->vertex_id[0], 
                           &pgraphbus_2->vertex_id[0],
                           &pnewedge );
#ifdef BAD_WID_DELETE
    /* this code needs work since it adds extra lines */
    /* but should never need to use the BAD_WID_DELETE flag */
    if(D_FOUND == stat) {
      if(pnewedge->display != GraphDisplayOn) {
        /* must do db_update to change record */
        printf("Warning - turning on a found edge\n");
        pupdatedata      = &updatedata;
        memcpy( pupdatedata, pnewedge, sizeof( GraphElement ) );
        pupdatedata->display = GraphDisplayRequestOn;
        stat = db_update( &graph_db, &pnewedge, &pupdatedata );
        assert(D_OKAY == stat);
        manageEdge( pnewedge );
        return 0;
      } else {
        if(pnewedge->wid != NULL) {
          if(XtIsManaged(pnewedge->wid)) {
            printf("Warning: edge already on graph - do not add dup line\n"); 
            printGraphElement(pnewedge);
            return -1;
          } else {
            printf("Warning: edge re-managed\n");
            XtManageChild(pnewedge->wid);
          }
        } else {
          printf("Warning - display flag on but wid not managed:\n");
          printGraphElement(pnewedge);
          return -1;
        }
      }
    }
#else
    if(D_FOUND == stat) {
          printf("Warning - no edge added - BAD_WID_DELETE may be set :\n");
          printGraphElement(pnewedge);
          return -1;
    }
#endif

    /** ok - found both vertices and edge not already on screen...so add **/
    memset(&linekey, ' ', sizeof(linekey));
    if(atof_cnt_zero(&pgraphbus_1->vertex_id[8], 4) !=
       atof_cnt_zero(&pgraphbus_2->vertex_id[8], 4) ) {
      strncpy(&linekey.key[0], "T", sizeof(linekey.key));
    } else {
      strncpy(&linekey.key[0], "L", sizeof(linekey.key));
    }
    strncpy(&linekey.name1[0], &pgraphbus_1->vertex_id[0], 
                               sizeof(linekey.name1) + sizeof(linekey.base1));
    strncpy(&linekey.name2[0], &pgraphbus_2->vertex_id[0], 
                               sizeof(linekey.name2) + sizeof(linekey.base2));
    linekey.eol[0] = '\0';
    if (ipfdebug & DB_Toolbox) {
      printf("add_graph_line: linekey = %s\n", &linekey); 
    }

    pscreateGraphCorEdge(pgraphbus_1, pgraphbus_2, &linekey.key[0],
                           GraphClassEdgeComplexSection, 1, &pcomplex_edge);

    pscreateGraphCorEdge(pgraphbus_1, pgraphbus_2, &linekey.key[0],
                           GraphClassEdgeSection, 0, &pnewedge);

    addGraphLink(pnewedge, pcomplex_edge);   /* link parent - */
    addGraphLink(pcomplex_edge, pnewedge);   /* child edges */
    manageEdge(pnewedge);
    return 0;
}

void addGraphBranch ( GraphElement *bus,
				XmPushButtonCallbackStruct *call_data)
/****************************************************************************\
*  Purpose: Handles the button (push & release) for creating branches
*		 Creates a drag (bend) vertex.
*		 Creates an edge to drag.
*
*	Author: Bill Rogers	Nov 13, 1992
*
* Called by: tbAddBranch (drawAreaButtonCB above)
\****************************************************************************/
{
  Widget junk;
  static GraphElement *start_bus;
  static GraphElement *pedge;
  static GraphElement *pdrag_vertex;
  GraphElement *pbus;
  GraphElement *end_bus;

  int rtncde;
  PSBranchRecord line;

  XEvent *event = call_data->event;

  if( bus->class == GraphClassVertexBus )
  { /* O.K. */ }
  else if( bus->class == GraphClassVertexName )
  {
    rtncde = findBusfromNameVertex(&pbus, bus );
    if( rtncde == 0 ) {
      bus = pbus; 		/* bus vertex found */
    } 
  }
  else		 /* wrong vertex class - cancel the line add function */
  {
    if(NODRAG == dragLineStatus)
    {
      return;	 /* printf("WRONG VERTEX START\n"); */
    }
  }

  if( dragLineStatus == NODRAG )
  {
    start_bus = bus;			/* needed later */
    create_and_manage_scrn_vertex( start_bus->vertex_id, 
				   event->xbutton.x,
				   event->xbutton.y,
				   &pdrag_vertex,
				   GraphClassVertexBendPoint );

    memset(&line.key[0], ' ', sizeof(line) - sizeof(line.idx));
    line.idx = 0;
    sprintf(line.name1, start_bus->vertex_id, 24 );

    pscreateGraphCorEdge( start_bus,
			  pdrag_vertex,
			  line.key,
			  GraphClassEdgeSection,
			  0, 
			  &pedge );

    manageEdge( pedge );

    dragLineStatus = DRAG;

    drag_vtx_id = pdrag_vertex;		/* for dragging part below */
    setobject( pdrag_vertex ); 		/* widget to drag */
    start_rubberband( (XEvent *)event );

    line_drag_switches( False );

  }
  else		/* To End Dragging process... */
  {
    if( bus->class == GraphClassVertexBus &&
	bus->wid   != start_bus->wid ) 	    /* don't end on itself! */
    {
      end_bus = bus;

      manage_front_box( start_bus, end_bus );
    }
    else
    { /* printf("WRONG VERTEX STOP\n"); */ }

	/*NOTE: do this before deleting the GraphLinks */
    setobject( drag_vtx_id );
    stop_rubberband( junk,(XtPointer) start_bus,(XEvent *)call_data );
    setobject( NULL );

    deleteGraphLink   ( start_bus, pedge );	 /* end the drag process */
    deleteGraphLink   ( pedge, pdrag_vertex );
    deleteGraphElement( pdrag_vertex );
    deleteGraphElement( pedge );
    line_drag_switches( True );
    dragLineStatus = NODRAG;

  }
}


void manage_front_box( GraphElement *start_bus, GraphElement *end_bus )
/**************************************************************************\
* Purpose: Handles all the setting up of the "front box"
*
*
* Moved from AddGraphBranch mar/96
\**************************************************************************/
{
  char newkey[132];

      fetch_widget("bus_front_box");
      if( 0 == strncmp( start_bus->vertex_id+8, end_bus->vertex_id+8, 4))
      {
        sprintf(newkey, "L %12.12s%12.12s", start_bus->vertex_id, 
				            end_bus  ->vertex_id);
	set_button_sensivity( "line_pb"       , True );
	set_button_sensivity( "equivline_pb"  , True );
        cascade_pd_label_set( "line_type_pd", "L " );
      }
      else
      {
        sprintf(newkey, "T %12.12s%12.12s", start_bus->vertex_id, 
				            end_bus  ->vertex_id);
	set_button_sensivity( "line_pb"       , False );
	set_button_sensivity( "equivline_pb"  , False );
        cascade_pd_label_set( "line_type_pd", "T " );
      }
      set_button_sensivity( "xfmr_pb"       , True  );
      set_button_sensivity( "phase_shift_pb", True  );
      set_button_sensivity( "reg_xfmr_pb"   , True  );
      set_button_sensivity( "dc_line_pb"    , False );
      set_button_sensivity( "lm_line_pb"    , False );
 
     /** add the new line if it is not already displayed **/
      create_line_rec_2bus( newkey );	/* popup the branch edit dialog */

      enable_pushbutton ( "add_pushbutton"    );
      disable_pushbutton( "send_pushbutton"   );
      disable_pushbutton( "delete_pushbutton" );
}


void addDrawLine ( GraphElement *pdrawv,
				XmPushButtonCallbackStruct *call_data )
/****************************************************************************\
*	purpose: finds draw vertex clicked on.
*		 Creates a drag (bend) vertex.
*		 Creates an edge to drag.
*
*	Author: Bill Rogers	Feb 1996
*
\****************************************************************************/
{
  static GraphElement *pedge;
  static GraphElement *pdrag_vertex;
  static GraphElement *startv;
  static GraphElement *pdrawv2;
  GraphLink *lnk;
  DB_STAT db_stat;
  static int vertex_create = 0;
  static int vertex_start = 0;
  static int vertex_end = 0;
  Widget junk;
  static int x1,y1;
  int nlink;
  static drag_status = FALSE;

  XEvent *event = call_data->event;

  /* NOTE: buttonPRESS and buttonRELEASE both create draw vertex - if needed */
  printf("drawline event %d on widget %d \n",
	 call_data->event->xany.type, pdrawv );
  if( pdrawv != NULL ) printf("     class %d\n", pdrawv->class );

	/* if drag is in process - we're only interested in a button release */
  if( drag_status && call_data->event->xany.type == ButtonPress )
	return;

  		/* start or stop on a draw point */
		/* can only do so - IF draw point has ONE edge or less */
  nlink = 0;
  if( pdrawv != NULL && pdrawv->class == GraphClassVertexDrawPoint )
  {
    db_stat = ll_first( &graph_db, &pdrawv, &lnk, GRAPH_EDGE_LINK );
    while( db_stat == D_FOUND )
    {
      nlink++;
      db_stat = ll_next( &lnk, &lnk );
    }
    printf(" %d links to this drawpoint \n", nlink );
  }

  if( drag_status == FALSE && 
      ( pdrawv == NULL || pdrawv->class != GraphClassVertexDrawPoint ||
      nlink > 1 ) )
  {
    x1 = event->xbutton.x;
    y1 = event->xbutton.y;
    psUserCreateDrawVertex( x1, y1, &pdrawv );	/* start or end vertex */
    /* XtRemoveEventHandler(pdrawv->wid, ButtonReleaseMask, False,
       (XtEventHandler)drawAreaButtonCB, NULL); */
    printf("Created!\n");
    vertex_create = 1;
  }
  else
  {
    vertex_create = 0;		/* use last draw point */
  }

	/* all button presses - start a DRAG line */
  if( call_data->event->xany.type == ButtonPress )
  {
    drag_status = TRUE;

    if( vertex_create ) vertex_start = 1;
    startv = pdrawv;
    psUserCreateDrawVertex( x1, y1, &pdrag_vertex );	/* drag vertex */
 
    pscreateGraphCorEdge( startv,
			  pdrag_vertex,
			  "  drawlinedrawline",
			  GraphClassEdgeDraw,
			  0,
			  &pedge );

    manageEdge( pedge );
    setobject ( pdrag_vertex );
    start_rubberband( (XEvent *)event );

    line_drag_switches( False );

  }
  else if( call_data->event->xany.type == ButtonRelease )
  {
    drag_status = FALSE;
    setobject( pdrag_vertex );
    stop_rubberband( junk,(XtPointer) pdrawv,(XEvent *)call_data );
    setobject( NULL );

    if( pdrawv != NULL && pdrawv != startv ) 	    /* don't end on itself! */
    {
      /** remove the drag edge and drag vertex **/
      deleteGraphLink   ( pdrag_vertex, pedge );
      deleteGraphLink   ( pedge, pdrag_vertex );
      deleteGraphElement( pedge );
      deleteGraphElement( pdrag_vertex );
	 /* remove if created at start, otherwise leave intact */
      if( vertex_start == 0 ) deleteGraphElement( startv );

    }
    else	/* ending on a draw vertex - either existing or new */
    {
      if( pdrawv == NULL )	/* new - leave everything as is */
      {
	draw_outline( pdrawv );
      }
      else		/* old - remove drag vertex & re-connect */
      {
        deleteGraphLink   ( pdrag_vertex, pedge );
        deleteGraphLink   ( pedge, pdrag_vertex );
        deleteGraphElement( pdrag_vertex );

        addGraphLink   ( pdrawv, pedge );
        addGraphLink   ( pedge, pdrawv );

      }

    }
    line_drag_switches( True );
    vertex_create = 0;
  }
}


void deleteBendVertex(	GraphElement *client_data )
/*****************************************************************************\
*
* purpose:	deletes a vend vertex, deletes one edge, joins edges together
*		
*	Author: Bill Rogers	Nov, 1992
*
\*****************************************************************************/
{
  GraphElement *pedge_1;
  GraphElement *pedge_2;
  GraphElement *farvertex;
  GraphElement *vertex_d;
  GraphElement *pedge_tmp;
  GraphElement *complex_edge;
  GraphLink    *llretrec;
  GraphElement *bends[PSMAXBENDS + 2];
  GraphElement *pupdate_rec;
  GraphElement update_rec;

  Boolean arrow_on;
  DB_STAT stat;
  int bendcnt,i,seg;


  if (ipfdebug & (DB_TraceMask | DB_Toolbox)) {
	fprintf(ipfdbio, "deleteBendVertex: start\n");
  }

  vertex_d = client_data;
  stat = ll_first( &graph_db,
                   &vertex_d,
                   &llretrec,           /* ptr to 1st edge rec */
                   GRAPH_EDGE_LINK );
  if(D_FOUND != stat) {
    printf("PROGRAMMER ERROR - bend vertex %12.12s has no edge links\n",
    vertex_d->vertex_id);
    printf("  delete bend vertex anyway\n");
    deleteGraphElement( vertex_d );
    return;
  }
  pedge_1 = llretrec->graphnode;        /* will be modified */

  stat = ll_next( &llretrec, &llretrec );
  if(D_FOUND != stat) {
    printf("PROGRAMMER ERROR - bend vertex %12.12s missing second edge link\n",
    vertex_d->vertex_id);
    printf("  delete bend vertex and single line anyway\n");
    deleteGraphElement( pedge_1 );
    deleteGraphElement( vertex_d );
    return;
  }
  pedge_2 = llretrec->graphnode;        /* will be deleted */

  if(pedge_1->wid != NULL) {
    XtVaGetValues(pedge_1->wid,  
                XiNarrowOn, &arrow_on,
                NULL);
    if(arrow_on == (Boolean)TRUE) {
      /* switch edges to delete */
      pedge_tmp = pedge_1;
      pedge_1 = pedge_2;
      pedge_2 = pedge_tmp;
    }
  } else {
    printf("Warning - arbitrarily picking edge to delete \n");
  }

  stat = farendGraphVertex( vertex_d, pedge_1, &farvertex );
  if(D_FOUND != stat) {
     printf("PROGRAMMER ERROR: Edge has missing end vertex.\n");
     printf("  delete bend vertex and edges anyway\n");
     deleteGraphElement( pedge_2 );
     deleteGraphElement( pedge_1 );
     deleteGraphElement( vertex_d );
     return;
  }

  deleteGraphElement( pedge_1 );
  switchEdge	    ( pedge_2, vertex_d, farvertex );
  deleteGraphElement( vertex_d );
  resetCurBusDefaultName( 0, 0, 0);

  /** housekeeping - reset dply_seg on complex edge */
  findComplexEdge( &complex_edge, pedge_2 );
  bendcnt = createBendArray( bends, complex_edge, GraphTypeEdge );
  for( i = 0; i<bendcnt; i++ )
  {
    if( bends[i]->dply_seg == 1 )  seg = i+1;
  }

  pupdate_rec = &update_rec;
  memcpy( pupdate_rec, complex_edge, sizeof(GraphElement));
  pupdate_rec->dply_seg = seg;
  stat = db_update(&graph_db, &complex_edge, &pupdate_rec);
  assert(D_OKAY == stat);

}



void remove_possible_group_links( GraphElement *client_data )
/*****************************************************************************\
* Purpose:   Loops to find and remove any bus-group links.
*
* Author: Bill Rogers	1/15/97
\*****************************************************************************/
{
  GraphElement graphvertex;
  GraphElement *pgraphvertex;
  GraphElement *pgraphvertexnxt;
  DB_STAT stat, statlnk, statdel;
  GraphLink *llretrec, *llnxtrec;

  if(db_valid_db(&graph_db) != D_OKAY) { return; }

  stat = db_first( &graph_db, &pgraphvertexnxt, GRAPH_TYPE );

  if (stat == D_EOF) { return; }

  while(stat == D_FOUND)
  {
    if(pgraphvertexnxt->class == GraphClassVertexGroup)
    {
      statlnk = ll_first( &graph_db, &pgraphvertexnxt, &llnxtrec,
                                                 GRAPH_VERTEX_LINK );
      while(D_FOUND == statlnk) {
        if(llnxtrec->graphnode == client_data )
        {
          statdel = ll_delete(&llnxtrec);
          assert(statdel == D_OKAY);
          /* printf("Deleted from group.....\n"); */
          break;
        }
        llretrec = llnxtrec;
        statlnk = ll_next( &llretrec, &llnxtrec );
      }
    }

    pgraphvertex = pgraphvertexnxt;
    stat = db_next( &graph_db, &pgraphvertex, &pgraphvertexnxt,
						GRAPH_TYPE );
  }
} 



void breakEdge(	GraphElement *pedge, int x, int y)
/*****************************************************************************\
*
* purpose:	breaks single edge into two edges.
*		
*	Author: Bill Rogers	Nov, 1992
*
\*****************************************************************************/
{

  GraphElement *pvertex_1;
  GraphElement *pvertex_2;
  GraphElement *vertex_bend;
  GraphElement *pcomplex_edge;
  GraphElement *new_edge;
  GraphLink    *llretrec;
  GraphElement *bends[PSMAXBENDS + 2]; 
  int bendcnt;
  PSBranchRecord line;
 
  DB_STAT stat;

  if (ipfdebug & (DB_TraceMask | DB_Toolbox)) {
	fprintf(ipfdbio, "breakEdge: x = %d, y = %d\n", x, y);
        printGraphElement( pedge ); 
  }

/******* locate the parent edge *****************************************/
  stat = findComplexEdge(&pcomplex_edge, pedge);
  if( stat != D_FOUND ) {
    printf("*** WARNING *** from BREAK EDGE - edge has no parent cancel!\n" );
    return;
  }
/******* determine the number of bends for this line ***/
  bendcnt = createBendArray(bends, pcomplex_edge, GraphTypeVertex );
  if (bendcnt >= PSMAXBENDS + 1) {
    sprintf(errmsg,"Warning - cannot create more than %d bends\n", PSMAXBENDS);
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "breakEdge";
    err.type = WARNING;
    err.ident = sccsid;
    em_show( &err );
    return;
  }

/******* create the line-bend ******************************************/
  createGraphVertex( GraphClassVertexBendPoint,
			&pcomplex_edge->vertex_id[0],
			x,
			y,
			' ',
			&vertex_bend );

/* useful only to set color of bend vertex later */
/*     memcpy( vertex_bend->vertex_id,
	     pcomplex_edge->vertex_id,
	     sizeof(vertex_bend->vertex_id));
  memcpy( vertex_bend->farvertex_id,
	     pcomplex_edge->farvertex_id,
	     sizeof(vertex_bend->vertex_id));*/

  manageVertex( vertex_bend );

  stat = findEdgeVertexes( pedge, &pvertex_1, &pvertex_2 );
  assert( stat == D_FOUND );

  deleteGraphLink( pvertex_1, pedge );

  addGraphLink ( pedge, vertex_bend );
  addGraphLink ( vertex_bend, pedge );
  updateEdge   ( pedge );

/******* create new edge ********************************************/
  memset(&line.key[0], ' ', sizeof(line) - sizeof(line.idx));
  line.idx = 0;
  memcpy(line.name1,pcomplex_edge->vertex_id,sizeof(pcomplex_edge->vertex_id));
  memcpy(line.name2,pcomplex_edge->farvertex_id,sizeof(pcomplex_edge->vertex_id));
  pscreateGraphCorEdge( vertex_bend,	 pvertex_1,
                        &line.key[0],	 GraphClassEdgeSection, 0, &new_edge );

   /* Note that pscreateGraphCorEdge did all the required links */

/******* link complex edge to child edge ****************************/
  addGraphLink( pcomplex_edge, new_edge );
  addGraphLink( new_edge, pcomplex_edge );

  setobject( vertex_bend );       /* makes changes visible */
  manageEdge( new_edge );

  if (ipfdebug & (DB_TraceMask | DB_Toolbox)) {
      printGraphElement( new_edge );
      printGraphElement( vertex_bend );
  }

  resetCurBusDefaultName( 0, 0, 0);
}


void tools_zoom_cb(Widget widget, int *tag, XtPointer callback_data)
/*****************************************************************************\
* Purpose: change display size when + or - zoom button is pushed.
*		
* Author: John Rutis	Nov, 1992
* Modifications: Wm E. Rogers 1994, JAN/95
*
* Called by: zoom_in, zoom_out pushbuttons
*
\*****************************************************************************/
#define UPPER_THRESHOLD 2.0	/* point which zoom button stops */
#define LOWER_THRESHOLD 0.5	/* point which zoom button stops */
#define ZOOM_IN_INCR	2.0
#define ZOOM_OUT_INCR	0.5
#define ZOOM_MIN	0.1
#define ZOOM_MAX	10.
{
  Widget       wid;
  int  ret_value;
  char str[4];
  double mult_by;
  static double new_zoom, old_zoom = 1.0;

  if (ipfdebug & (DB_TraceMask | DB_Toolbox)) {
	fprintf(ipfdbio, "tools_zoom_cb: *tag = %d\n", *tag);
  }

  if( 0 == *tag)	/* zoom in (enlargen display) */
  {
    if( zoom_factor >= UPPER_THRESHOLD ) return;
    mult_by = ZOOM_IN_INCR;
  }
  else if( 1 == *tag)	/* zoom out (shrink display) */
  {
    if( zoom_factor <= LOWER_THRESHOLD ) return;
    mult_by = ZOOM_OUT_INCR;
  }
  else if( 9 == *tag)		/* continuous zoom */
  {
    lookup_and_get_field( "tools_zoom_factor_text", str, 4 );
    new_zoom = atof_cnt_zero( str, 3 );
    if( new_zoom < ZOOM_MIN ) new_zoom = ZOOM_MIN;
    if( new_zoom > ZOOM_MAX ) new_zoom = ZOOM_MAX;
    mult_by = new_zoom/old_zoom;
  }
  else
  {
    if( zoom_factor == 1.0 ) return;
    lookup_and_fill_field( "tools_zoom_factor_text", "1.0", 0, 2 );
    mult_by = 1.0/old_zoom;
  }

  zoom_factor *= mult_by;
  rescale_graph( mult_by, mult_by, FULL_RESCALE );

  sprintf( str, "%3.1f", zoom_factor );
  lookup_and_fill_field( "tools_zoom_factor_text", str, 0, 2 );

  old_zoom = zoom_factor;	/* save for next zoom adjustment */
}



/***************************************************************************\
* Purpose: determine which toolbox radio button is in the ON position.
*
*
\***************************************************************************/
ToolboxButton findbutton()
{
  Widget rbox_id, button_id;
  XmString label_cs;
  char *label_str;
  int toolbox_item;
  XmStringContext context;
  XmStringCharSet       charset;
  XmStringDirection     dir;
  Boolean       separator;


  rbox_id = widget_id("tools_action_opt_rbox"); 
  XtVaGetValues(rbox_id, XmNmenuHistory, &button_id, NULL);

  if(button_id == NULL) return;

  XtVaGetValues(button_id, XmNlabelString, &label_cs, NULL);
  XmStringInitContext(&context, label_cs);
  XmStringGetNextSegment(context, &label_str, &charset, &dir, &separator);
  toolbox_item = atoi(label_str);
  XmStringFree(label_cs);
  XmStringFreeContext(context);

  if (ipfdebug & (DB_TraceMask | DB_Toolbox)) {
	fprintf(ipfdbio, "ToolboxButton: toolbox_item = %d\n", toolbox_item);
  }

  return (ToolboxButton)toolbox_item;

}


void line_drag_switches( Boolean p1 )
/************************************************************************\
*
*
*
\************************************************************************/
{
  if (ipfdebug & (DB_TraceMask | DB_Toolbox)) {
	fprintf(ipfdbio, "line_drag_switches: p1 = %d\n", (int)p1);
  }
  set_button_sensivity( "tool_input_rb"    , p1 );
  set_button_sensivity( "tools_output_rb"  , p1 );
  set_button_sensivity( "tools_input_output_rb", p1 );
  set_button_sensivity( "tools_star_rb"    , p1 );
  set_button_sensivity( "tools_delete_rb"  , p1 );
  set_button_sensivity( "tools_outage_rb"  , p1 );
  set_button_sensivity( "tools_undelete_rb", p1 );
  set_button_sensivity( "tools_unoutage_rb", p1 );

  set_button_sensivity( "tools_new_bend_rb", p1 );
  set_button_sensivity( "tools_move_rb"    , p1 );
  set_button_sensivity( "tools_newbus_rb"  , p1 );
  set_button_sensivity( "tools_newline_rb" , p1 );
  /* set_button_sensivity( "tools_draw_rb"    , p1 ); */
  /* set_button_sensivity( "tools_alpha_rb"   , p1 ); */

  set_button_sensivity( "tools_zoomin_rb" , p1 );
  set_button_sensivity( "tools_zoomout_rb", p1 );

/*  set_button_sensivity( "tools_coord_select_rb", p1 );  These won't work right
  set_button_sensivity( "tools_merge_select_rb", p1 );
  set_button_sensivity( "tools_empty_select_rb", p1 );*/

  set_button_sensivity( "tools_select_group_rb", p1 );
  set_button_sensivity( "tool_coord_rb", p1 );

}
