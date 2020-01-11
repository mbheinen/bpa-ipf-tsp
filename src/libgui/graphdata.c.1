static char sccsid[]="@(#)graphdata.c	20.75 8/18/98";
/***************************************************************************
* purpose: functions to manipulate graph data
*
* file: graph_data.c
* author: Dan Clark
*
****************************************************************************
*/
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>
#include <Xm/Xm.h>
#include <Xm/ToggleBG.h>

#include "dmgr.h"	/* make sure this one comes before coord_data.h! */
#include "coord_data.h"
#include "graph_data.h"
#include "EdgeG.h"
#include "line2rect.h"
#include "ipf_ipc.h"
#include "base_data.h"
#include "ipfdebug.h"
#include "math.h"

#include "em.h"
em_init
extern int graphorder;

long graph_idx = (long)0;

#define max(A,B) ((A) > (B) ? (A) : (B))

#define POSFORMAT	"%6.2f"
#define SYMBOL_RADIUS	0
#define DISPLAY_FLAG 	" "
#define SYMBOL_SHAPE 	"  "
#define V_FLAG 		"3"

#define PixelsPerCm	39.37
#define DegtoRad	.0174532

static int border = 10;

int  setEdgeDash		( GraphElement *pedge, Boolean dashval);
int  istransformer              ( GraphElement *  );
void process_pf_connections	( char *pfdataret );
Cardinal insertByType		( Widget wid );
long cmp_display_type		( char *a, char *b, long len);
long cmp_type_vertex_id		( char *a, char *b, long len);
long cmp_type_vertex_farvertex	( char *a, char *b, long len);
long cmp_class_idx		( char *a, char *b, long len);
void deleteGraphElementByClass	( GraphClass class ) ;
void buildMainDisplay		( int mergeFlag );
void printAllBusesAndBranches	();
DB_STAT findGraphEdgebyName2	( char *namebase1, char *namebase2, 
	                              GraphElement **pedge );
DB_STAT findGraphEdgebyName	( char *namebase1, char *namebase2, 
	                              GraphElement **pedge );
int get_alpha_order_of_cmplx	( GraphElement *pcmplx, GraphElement **lo_alpha,
			    GraphElement **hi_alpha, GraphElement **sub_edge );
void findVertxLinkedToVertx	(GraphElement **pvtxname, GraphElement *pvtx,
					GraphClass class );
void createGraphGenReac( GraphElement **vtx, GraphElement *busrec, char *gen_a,
			 GraphClass type, int dist, char *namstr, int m_flg );
GraphElement *createGraphBusXYXY( char *namebase, int x, int y, int xn, int yn,
		 char *gen_a, char *cap_a, int m_flg, int x_flg, int pf_flg,
		 int g_flg, int r_flg );


extern double	zoom_factor;
extern int atoi_cnt		(char *str, int length);
extern double atof_cnt_zero	(char *basekv, int length);
extern int ipc_synch_rw 	( char *, char *);
extern manageVertex             ( GraphElement *);
extern int forEachipfLine	();
extern int pfcheck_cb		(char *instr);
extern void createVertexGadgets ( Widget wid, XtPointer tag, 
                                  XmAnyCallbackStruct *cbs );
extern void createEdgeGadgets	( Widget wid, XtPointer tag,
				  XmAnyCallbackStruct *cbs );
extern void pscreateGraphCorEdge( GraphElement *vertx_1,
	                           GraphElement *vertx_2,
	                           char *key,
	                           GraphClass class,
				   int seg,
	                           GraphElement **new_rec );
extern int buildGraphBase       ( Widget, XtPointer,
                                  XmAnyCallbackStruct *);

extern int manageEdge		( GraphElement *pgraphedgenxt);
extern Widget widget_id		( char *widget_name);
extern double MOTIF_to_ps_x     ( int x, char *, char *, char * );
extern double MOTIF_to_ps_y     ( int y, char *, char *, char * );

extern int  checkLines		( char *line, char *cpcon);
extern int  checkLinesBuses	( char *line, char *cpcon);
extern int  ipfstate_get		( int *ostates );
extern void psDrawOptions	();
extern void refresh_comments	();
extern void enable_pushbutton   ( char * );
extern int check_powerflow	( char *namebase);
extern void explodeBranches	( GraphElement *pge );
extern void createGraphBranch          ( GraphElement *v1, GraphElement *v2);
extern void ipf_itoa		( int n, char s[]);
extern double radiansBetweenPts	( int x, int y, int x2, int y2 );


/*##########################################################################
*
*  the following data structures and functions define the dmgr schema
*  for the graph table 
*
\##########################################################################*/
DBID graph_db;
DB_STAT db_stat;
static GraphElement graph;
static GraphElement * pgraph;

#define GRAPH_PARTITIONS 0
#define LINK_OFFSET	 0
#define LINK_DATA_SIZE	 sizeof(long) /* 64 bit compatable sizeof(GraphLink) */
#define GRAPH_KEY_CNT	 (int)(sizeof(graph_schema)/sizeof(graph_schema[0]))

static SCHEMA graph_schema[] =
{
  { 'n', 'c', sizeof(GraphElement), 0 },	   /* size of record stored */
  { 'd', 'i', sizeof(graph.display),
        offsetof(GraphElement, display)}, /* GRAPH_DISPLAY	 */
  { 'd', 'i', sizeof(graph.type),
        offsetof(GraphElement, type)},    /* GRAPH_TYPE	 */
  { 'd', 'c', sizeof(graph.vertex_id),
        offsetof(GraphElement, vertex_id)}, /* GRAPH_VERTEX_ID */
  { 'd', 'u',(long)sizeof(GraphElement), 0, 0,cmp_type_vertex_id	 }, 
  { 'd', 'u',(long)sizeof(GraphElement), 0, 0,cmp_display_type		 }, 
  { 'd', 'u',(long)sizeof(GraphElement), 0, 0,cmp_type_vertex_farvertex	 }, 
  { 'd', 'u',(long)sizeof(GraphElement), 0, 0,cmp_class_idx	 }, 
  { 'n', 'l', LINK_DATA_SIZE, LINK_OFFSET }, /* GRAPH_VERTEX_LINK */
  { 'n', 'l', LINK_DATA_SIZE, LINK_OFFSET }  /* GRAPH_EDGE_LINK */
};



int creategraphtbl( Widget w, XtPointer tag, XmAnyCallbackStruct *cbd)
/*****************************************************************************\
* Purpose: 
*  create graph table AND chg_create table for VERTEX and EDGE data
*
* Author: Dan Clark	Dec, 1991	Modified: Dec 17, 1991
*
* Called by: filedlgrtn,
*
\*****************************************************************************/
{
  DB_STAT stat;		/* status returned from data manager calls */
  Widget draw_id;

  /*** create the table for graphics data ***/
  stat = db_create((SCHEMA *)graph_schema, GRAPH_KEY_CNT,
			GRAPH_PARTITIONS, &graph_db);

  if(stat != D_OKAY) {
      sprintf( errmsg, "error creating graph db\n");
      err.line = EM_LINE;
      err.msg  = errmsg;
      err.link = "creategraphtbl";
      err.type = WARNING;
      err.ident = sccsid;
      em_show( &err );
  }
  assert(stat == D_OKAY);

  if(( draw_id = widget_id( "drawwindow" )) == FALSE) {
    printf("Warning: creategraphtbl: cannot find drawwindow\n");
    return;
  }
  XtVaSetValues(draw_id,
            XmNinsertPosition, insertByType,
            NULL);

  /* added 2/96 - if coord_db is created - then it is now possible to   *
  *    edit, plot and save a file					*/

  enable_pushbutton( "file_menu_save_button"  	    );
  enable_pushbutton( "file_menu_saveas_button"	    );
  enable_pushbutton( "file_menu_print_plot_button"  );
  enable_pushbutton( "file_menu_plot_options_button");

  enable_pushbutton( "tools_action_opt_rbox"	    );
  enable_pushbutton( "tools_zoom_form"		    );

  enable_pushbutton( "edit_menu_cascade"	    );
  enable_pushbutton( "view_menu_cascade"	    );
}


void createGraphVertex( GraphClass class, char *namebase, 
			int x, int y, char hide, GraphElement **pgraphrec )
/*****************************************************************************\
*
* Note:  This routine must ALWAYS create a GraphElement - no exceptions!
*
*	FUNCTION: Standardized add a new vertex to the graph_db.
*		  Fills in some missing data, set sort key.
*
*	PARAMS:   class:	e.g. BUS, NAME, BEND, etc.
*		  namebase:	vertex id name.
*		  x, y:		Position (in pixels) from NW corner.
*		  source:	e.g. Coord, pf-base, user-created, etc.
*		  pgraphrec:	Returned ptr to graph record
*
*	Author: Bill Rogers	Oct-15-92
*
*	Called by: graphcor.c 	( obsolete - same as graphpscor.c     )
*		   graphpscor.c (when data is transfered from coord db)
*		   pf_cb.c	(when explode function is activated)
*		   toolbox.c	(when creating new bus or branch )
*
\*****************************************************************************/
{
  GraphElement  graphrecord;	/* define structure & allocate enough 	*/
				/* space for data graphrecord		*/

  GraphElement *pgraphrecord = &graphrecord; /* allocate memory for ptr	*/

  GraphElement *pnewrec;
  DB_STAT stat;

  pgraphrecord = &graphrecord;		/* set the pointer value         */

					/* start initial filling of data */
  graphrecord.class = class;		/* i.e. bus, name or bend */

  memset( pgraphrecord->farvertex_id, ' ', sizeof(pgraphrecord->farvertex_id));

  switch(class)
  {
    case GraphClassVertexBus: 
    case GraphClassVertexGenerator: 
    case GraphClassVertexCapacitor: 
    case GraphClassVertexReactor: 
    case GraphClassVertexGroup: 
      strncpy( pgraphrecord->vertex_id,  namebase,
		sizeof(pgraphrecord->vertex_id) );
    break;

    case GraphClassVertexName: 
      /* need to update name with alias from coord file */
      strncpy( pgraphrecord->farvertex_id,  namebase,
		sizeof(pgraphrecord->vertex_id) );
      strncpy( pgraphrecord->vertex_id,  namebase,
		sizeof(pgraphrecord->vertex_id) );
      pgraphrecord->dply_seg = 3;	/* default voltage below name for now */
    break;

    case GraphClassVertexDrawPoint: 
      strncpy( pgraphrecord->vertex_id,  namebase,
		sizeof(pgraphrecord->vertex_id) );
      strncpy( pgraphrecord->farvertex_id,  namebase,
		sizeof(pgraphrecord->farvertex_id) );
    break;

    case GraphClassPaperVertex: 
    case GraphClassVertexBorder: 
    case GraphClassVertexSubcomment: 
    case GraphClassVertexLegendText: 
    case GraphClassVertexLegendCorner: 
    case GraphClassVertexBendPoint: 
      strncpy( pgraphrecord->vertex_id,  namebase,
                sizeof(pgraphrecord->vertex_id) );
    break;

    case GraphClassVertexFont: 
    case GraphClassVertexComment: 
    case GraphClassVertexOrgComment: 
    case GraphClassVertexLabelText: 
    case GraphClassVertexLabelCorner: 
      strncpy( pgraphrecord->farvertex_id, namebase,      12 );
    break;

    default:
      printf("PROGRAMMER ERROR - in call to createGraphVertex (graphdata.c)\n");
      printf("        Unknown Class for vertex (%d)\n", class );
    break;
  }

  /* by forcing the x and y onto the MOTIF draw window prevents hard bugs */
  if( x<0 || y<0 ) {
    printf("WARNING - from createGraphVertex\n" );
    printf(" Attempt to use NEGATIVE value(s) for MOTIF plot data.\n");
 printf(" Original data: %12.12s (org: x=%6.2f, y=%6.2f) (MOTIF: x=%d, y=%d)\n",
                pgraphrecord->vertex_id,
                MOTIF_to_ps_x( x, "type_unkn", pgraphrecord->vertex_id, ""),
                MOTIF_to_ps_y( y, "type_unkn", pgraphrecord->vertex_id, ""),
                        x, y );
      if( x<0 ) x = DrawBorder;
      if( y<0 ) y = DrawBorder;
      printf(" (x=%d, y=%d)\n", x, y );
      printf(" Adjusted data: (x=%d, y=%d)\n", x, y );
  }

  graphrecord.display	= GraphDisplayOff;
  graphrecord.hide	= hide;
  graphrecord.type	= GraphTypeVertex;
  graphrecord.x		= x;
  graphrecord.y		= y;
  graphrecord.wid	= NULL;
  graphrecord.idx	= graph_idx++;

  if( GraphClassVertexBus == graphrecord.class )	/*** BUS SORT ***/
    stat = db_insert_opt ( &graph_db, &pgraphrecord, &pnewrec, optsv);
 
  else							/** ALL OTHERS **/
    stat = db_insert_opt ( &graph_db, &pgraphrecord, &pnewrec, optsn);

  if(stat != D_OKAY) { 
    printf("ERROR failed to insert record in graph_db , stat=%d\n", stat );
    printf("      Apparent out-of-memory error.\n" );
  }
  assert(stat == D_OKAY);

  *pgraphrec = pnewrec;		/* Returned ptr to data location */

  return;
}


GraphElement *createGraphBusXYXY( char *namebase, int x, int y, int xn, int yn,
		 char *gen_a, char *cap_a, int m_flg, int x_flg, int pf_flg,
		 int g_flg, int r_flg )
/****************************************************************************\
* Purpose: Standardized routine to create a BUS vertex and its NAME.
*
* 	   x,y = screen location of bus symbol
*	   xn,yn = screen location of name
*	   m_flg = manage Flag
*	   x_flg = explode Flag
*	   pf_flg = don't create if bus not powerflow
*
* Author: Bill E. Rogers		June 24, 1996
*
* Called by: psbuildGraphCoord	( graphpscor.c )
*	     explodeBus	 (2)	( pf_cb.c )
*	     addGraphBusXY	( toolbox.c )
\***************************************************************************/
{
  GraphElement *pgraphbusrec;
  GraphElement *pgraphnamerec;
  GraphElement *pvtx;
  GraphElement *pgraphbasebus;
  DB_STAT stat_search ;

  stat_search = findBusGraphVertexRec( namebase, &pgraphbasebus );

  if( D_FOUND == stat_search)
  {
    if( GraphDisplayOn == pgraphbasebus->display)
    {
      sprintf(errmsg, "** Info ** BUS (%s) is already displayed at (%d,%d)\n",
                namebase, pgraphbasebus->x, pgraphbasebus->y );
    }
    else
    {
      sprintf(errmsg, "%s\n%s\n",
        "Error - bus is already in data structure, not displayed",
        "  Bus may have been deleted from powerflow and removed from diagram");
    }
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "addGraphBus";
    err.type = WARNING;
    err.ident = sccsid;
    em_show( &err );
    return;
  }

  if( pf_flg && FALSE == check_powerflow(namebase))
  {
    /*** only add a bus to the screen if there exists a powerflow bus */
    sprintf(errmsg, "Warning Bus %s NOT ADDED TO POWERFLOW\n", namebase );
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "addGraphBus";
    err.type = WARNING;
    err.ident = sccsid;
    em_show( &err );
    return;
  }

  createGraphVertex(		        /*** create bus SYMBOL vertex ***/
                GraphClassVertexBus,
                namebase,
                x, y, ' ',
                &pgraphbusrec );

  createGraphVertex(                    /*** create bus NAME vertex ***/
                GraphClassVertexName,
                namebase,
                xn, yn, ' ',
                &pgraphnamerec );

  addGraphLink( pgraphbusrec, pgraphnamerec );

  if( m_flg )		/* if bus must be drawn on screen now */
  {
    manageVertex( pgraphbusrec );
    manageVertex( pgraphnamerec );
  }

	/* NOTE: gen, reac, always created, but not always managed */
	/* routine handles problem of gen angles being blank or zero */
  createGraphGenReac( &pvtx, pgraphbusrec, gen_a,
		 GraphClassVertexGenerator, G_DIST, "ZZGenerator ", g_flg );
 
  createGraphGenReac( &pvtx, pgraphbusrec, cap_a, GraphClassVertexCapacitor,
			R_DIST, SHUNT_N, r_flg );

  if( x_flg )		/* if all connecting branches must be drawn now */
  {
    explodeBranches(pgraphbusrec);
  }

  return pgraphbusrec;
}

void createGraphGenReac( GraphElement **pgraphgen,
			 GraphElement *pgraphbusrec, char *angl,
			 GraphClass class, int dist, char *namstr, int m_flg )
/**************************************************************************\
* Purpose: to create a GENERATOR OR REACTOR vertex graphelement with
*	   all the proper attachments.
*
* Called by:
*
* Author:  Bill Rogers 3/11/98
\**************************************************************************/
{
  GraphElement *pgraphedge;
  GraphElement graphnode;
  GraphElement *pgraphnode = &graphnode;
  DB_STAT stat;
  double ang;
  int xa, ya;
  char name_str[25];
  char hide_flg;

  ang = atof_cnt_zero( angl, 3 );	/* convert to float */

  xa = pgraphbusrec->x+(int)(cos(ang*DegtoRad)*dist*zoom_factor);
  ya = pgraphbusrec->y-(int)(sin(ang*DegtoRad)*dist*zoom_factor);

  if( ang >= 1 )
    hide_flg = NO_HIDE;
  else
    hide_flg = HIDE;

  createGraphVertex(                    /*** create bus GEN/REAC vertex ***/
                class,
  		pgraphbusrec->vertex_id,
                xa, ya, hide_flg,
                pgraphgen );

  addGraphLink( pgraphbusrec, *pgraphgen );

  sprintf( name_str, "L %12.12s%12.12s", pgraphbusrec->vertex_id, namstr );
  pscreateGraphCorEdge( pgraphbusrec, *pgraphgen,
  	name_str,
        GraphClassEdgeComplexSection,
        0,
        &pgraphedge );

  if ( m_flg && ang>=1 )
  {
    manageVertex( *pgraphgen );
    manageEdge  ( pgraphedge );
  }

}




long cmp_class_idx(char *a, char *b, long len)
/*******************************************************\
*
* purpose: display & type comparison routine.
* called by: dmgr search routines
\*****************************************************************************/
{
  GraphElement *keyrec;
  GraphElement *checkrec;
  int rtncmp1, rtncmp2;

  keyrec   = (GraphElement *)a;
  checkrec = (GraphElement *)b;
  rtncmp1 = (keyrec->class - checkrec->class);
  rtncmp2 = (keyrec->idx -  checkrec->idx);


  if(rtncmp1 > 0)
     return(1);
  else
     if(rtncmp1 < 0)
        return(-1);
  else
     if(rtncmp2 > 0)
        return(1);
  else
     if(rtncmp2 < 0)
        return(-1);
  else
     return(0);

}

long cmp_display_type(char *a, char *b, long len)
/*******************************************************\
*
* purpose: display & type comparison routine.
* called by: dmgr search routines
\*****************************************************************************/
{
  GraphElement *keyrec;
  GraphElement *checkrec;
  int rtncmp1, rtncmp2;

  keyrec   = (GraphElement *)a;
  checkrec = (GraphElement *)b;
  /* rtncmp2 = strncmp(keyrec->type, checkrec->type, 1); */
  rtncmp1 = (keyrec->display -  checkrec->display);
  rtncmp2 = (keyrec->type - checkrec->type);


  if(rtncmp1 > 0)
     return(1);
  else
     if(rtncmp1 < 0)
        return(-1);
  else
     if(rtncmp2 > 0)
        return(1);
  else
     if(rtncmp2 < 0)
        return(-1);
  else
     return(0);

}


long cmp_type_vertex_id(char *a, char *b, long len)
/*********************************************\
*
* purpose: type, source & vertex_id comparison routine.
* called by: dmgr search routines
\**************************************************************************/
{
  GraphElement *keyrec;
  GraphElement *checkrec;
  int rtncmp1, rtncmp2;

  keyrec   = (GraphElement *)a;
  checkrec = (GraphElement *)b;
  /* rtncmp2 = strncmp(keyrec->type, checkrec->type, 1); */

  rtncmp1 = (keyrec->type - checkrec->type); 
  rtncmp2 = strncmp(keyrec->vertex_id, checkrec->vertex_id,
            sizeof(checkrec->vertex_id));
  if(rtncmp1 > 0)
     return(1);
  else
     if(rtncmp1 < 0)
        return(-1);
  else
     if(rtncmp2 > 0)
        return(1);
  else
     if(rtncmp2 < 0)
        return(-1);
  else
     return(0);
}


long cmp_type_vertex_farvertex(char *a, char *b, long len)
/*********************************************\
*
* purpose: multiple key comparison routine.
* called by: dmgr search routines
*
\**************************************************************************/
{
  GraphElement *keyrec;
  GraphElement *checkrec;
  int rtncmp1, rtncmp2, rtncmp3;

  keyrec   = (GraphElement *)a;
  checkrec = (GraphElement *)b;
  /* rtncmp2 = strncmp(keyrec->type, checkrec->type, 1); */

  rtncmp1 = (keyrec->type - checkrec->type); 
  rtncmp2 = strncmp(keyrec->vertex_id, checkrec->vertex_id, 
            sizeof(checkrec->vertex_id));
  rtncmp3 = strncmp(keyrec->farvertex_id, checkrec->farvertex_id, 
            sizeof(checkrec->farvertex_id));

  if(rtncmp1 > 0)
     return(1);
  else
     if(rtncmp1 < 0)
        return(-1);
  else
     if(rtncmp2 > 0)
        return(1);
  else
     if(rtncmp2 < 0)
        return(-1);
  else
     if(rtncmp3 > 0)
        return(1);
  else
     if(rtncmp3 < 0)
        return(-1);
  else
     return(0);


}

#ifdef NOTUSED

void checkCoord(GraphElement *pgraphedgenxt, GraphElement *pparent, 
            GraphLink *plink ,char * user_data)
/*****************************************************************\
*
* author: Dan Clark
* purpose:
* check if there is a coordinate file edge which corresponds to
* the edge which is in the file.
* if there is not one - add a new edge and label with a
* source of merge
*
* called by:
****************************************************************************/
{
  GraphElement graphnode;		/* update node for graph */
  GraphElement *pgraphnode = &graphnode;/* ptr to new graph node data */
  GraphElement *pgraphnodenxt;		/* ptr to new graph node data */
  GraphLink    *pgraphlink;		/* ptr to graph link node */
  GraphElement *pgraphnode1;		/* ptr to graph link */
  GraphElement *pgraphnode2;		/* ptr to graph link */

  DB_STAT stat_link;
  DB_STAT stat;
  
    /** first get the vertex id's of the endpoints of this edge **/
    /** follow vertex links to find end points **/
    stat_link = ll_first(&graph_db, &pgraphedgenxt, &pgraphlink,
                GRAPH_VERTEX_LINK);
    if (stat_link != D_FOUND) {
      printf("Warning: checkCoord: no vertex links\n");
    }

    while(stat_link == D_FOUND) {
      pgraphnode1 = (GraphElement *)pgraphlink->graphnode;
      stat_link = ll_next(&pgraphlink, &pgraphlink);
      if(stat_link != D_FOUND) {
         break;
      }
      pgraphnode2 = (GraphElement *)pgraphlink->graphnode;
      /** we know both ends of edge so check if it has a graph coord edge **/

      /** first check if farend bus is in coord file **/
      pgraphnode = &graphnode;
      memset(pgraphnode, ' ', sizeof(graphnode));
      pgraphnode->type = GraphTypeVertex;
      strncpy(pgraphnode->vertex_id, pgraphedgenxt->farvertex_id, 
              sizeof(pgraphnode->vertex_id) ); 
      stat = db_search(&graph_db, &pgraphnode, &pgraphnodenxt,
                       GRAPH_TYPE_VERTEX_ID);
      if(D_FOUND != stat) {
         printf("No bus on graph for farend bus stat %d \n", stat);
         printGraphElement(pgraphnode); 
         return;
      }

      /* now look for the edge in the coord graph data */

      stat = findGraphEdgebyName2(pgraphedgenxt->vertex_id, 
            pgraphedgenxt->farvertex_id, &pgraphnodenxt);

      if(D_FOUND == stat) {
        /* no need to create edge has coord data */
      } else {
        /* need to create a new merge edge */
        char newkey[132];
        GraphElement *vtx1, *vtx2, *pnewedge, *pcomplex_edge;
        stat = findBusGraphVertexRec( pgraphedgenxt->vertex_id, &vtx1 );
        assert(D_FOUND == stat);
        stat = findBusGraphVertexRec( pgraphedgenxt->farvertex_id, &vtx2 );
        assert(D_FOUND == stat);

        sprintf(newkey, "%1.1s1%12.12s%12.12s", "L",
                &pgraphedgenxt->vertex_id[0],
                &pgraphedgenxt->farvertex_id[0]);
        if(newkey[0] == 'R') {
          /* coord file records only have types T or L */
          newkey[0] = 'T';
        }
        /* sprintf( newkey, &pgraphedgenxt->key.coord.id[0]); */

        /** Create the parent ( complex) edge **/
        pscreateGraphCorEdge ( vtx1, vtx2,   newkey,
                         GraphClassEdgeComplexSection, 1, &pcomplex_edge);

        pscreateGraphCorEdge(vtx1, vtx2, newkey,
		         GraphClassEdgeSection, 0, &pnewedge);

        addGraphLink( pnewedge, pcomplex_edge );   /* link parent - */
        addGraphLink( pcomplex_edge, pnewedge);   /* child edges */

      }

  } /* end loop through edge links */

} /* end checkCoord */
#endif


int turnOnSubedge(GraphElement *pcmplxedge)
/*************************************************************************\
* Purpose:  Manages (makes visible on screen) all subedges (line segments)
*	    of a line (complex edge).
*
\*************************************************************************/
{
  DB_STAT stat;
  GraphLink *psubedge;
  GraphElement *pedge;
  Widget seg_2;		/* section to put transformer symbol on */
  DB_STAT stat_bend;
  GraphLink *pbendlink;
  GraphElement *pbend;
  int cnt = 0;

  stat = ll_first(&graph_db, &pcmplxedge, &psubedge, GRAPH_EDGE_LINK);

  if(D_FOUND != stat) {
    printf("Programmer error - no subedges for complex edge\n");
    return stat;
  }

  while (D_FOUND == stat)
  {
    pedge = psubedge->graphnode; 

    /* turn on if not already on */
    if(GraphDisplayOn != pedge->display)
    {
      manageEdge(pedge);
      if( ++cnt <= 2 ) seg_2 = pedge->wid;

      /* now check if both bending points are on for this edge */
      stat_bend = ll_first(&graph_db, &pedge, &pbendlink, GRAPH_VERTEX_LINK);
      if(D_FOUND != stat) {
        printf("Programmer warning - no bend vertices for edge\n");
      }
      while(D_FOUND == stat_bend) {
        pbend = pbendlink->graphnode;
        if(GraphDisplayOn != pbend->display ) {
          manageVertex(pbend);
        }
        stat_bend = ll_next(&pbendlink, &pbendlink);
      }
    }
      
    stat = ll_next(&psubedge, &psubedge);
  }

  /*if(True == istransformer(pcmplxedge))     THIS causes a bomb on explode 
    XtVaSetValues( seg_2, XiNtransformerOn, TRUE, NULL ); */
  return D_OKAY;
}


int setDashAllSubEdges(GraphElement *pparentedge, Boolean flag)
/************************************************************************\
* Purpose: Line outage
*
\************************************************************************/
{
  DB_STAT stat;
  GraphLink *psubedge;
  GraphElement *pedge;

  stat = ll_first(&graph_db, &pparentedge, &psubedge, GRAPH_EDGE_LINK);

  if(D_FOUND != stat) {
    printf("Programmer error - no subedges found\n");
    return stat;
  }
  while (D_FOUND == stat)
  {
    pedge = psubedge->graphnode; 

    if(GraphClassEdgeSection == pedge->class) {
      setEdgeDash(pedge, flag);
    } 
    stat = ll_next(&psubedge, &psubedge);
  }

  return stat;
 
}


int setDashAllBusEdges(GraphElement *pparentedge, Boolean flag)
/************************************************************************\
* Purpose: Bus outage
*
\************************************************************************/
{
  DB_STAT stat;
  GraphLink *psubedge;
  GraphElement *pedge;

  stat = ll_first(&graph_db, &pparentedge, &psubedge, GRAPH_EDGE_LINK);

  if(D_FOUND != stat) {
    printf("Programmer error - no subedges found\n");
    return stat;
  }
  while (D_FOUND == stat)
  {
    pedge = psubedge->graphnode; 

    if(GraphClassEdgeComplexSection == pedge->class) {
      setDashAllSubEdges(pedge, flag);
    } 
    stat = ll_next(&psubedge, &psubedge);
  }

  return stat;
 
}

DB_STAT findComplexEdge(GraphElement **ppcmplxedge, GraphElement *pcuredge)
/*************************************************************************\
* purpose: find the complex edge which is the parent to the current
* subedge
*
\*************************************************************************/
{
  DB_STAT stat;
  GraphLink *psubedge;
  GraphElement *pedge;

  if(GraphClassEdgeComplexSection == pcuredge->class) {
      *ppcmplxedge = pcuredge; 
      return D_FOUND;
  }

  if(GraphClassEdgeSection != pcuredge->class )
  {
    printf("findComplexEdge: Programmer Warning, not an edge \n");
    printGraphElement(pcuredge);
    return D_NOTFOUND;
  }

  stat = ll_first(&graph_db, &pcuredge, &psubedge, GRAPH_EDGE_LINK);
  if(D_FOUND != stat)
  {
    printf("findComplexEdge: Programmer warning, no complex edge found for:\n");
    printGraphElement(pcuredge);
    return stat;
  }

  while (D_FOUND == stat)
  {
    pedge = psubedge->graphnode; 

    if(GraphClassEdgeComplexSection == pedge->class) {
      *ppcmplxedge = pedge; 
      break;
    } 
    stat = ll_next(&psubedge, &psubedge);
  }

  return stat;
}

DB_STAT findGraphEdgebyName2( char *namebase1, char *namebase2, 
                              GraphElement **pedge )
/*************************************************************************\
* Purpose: find edge in the graph data 	(reversing names if needed)
*
* Called by: checkCoord, checkOn(2),  deleteEdgebyName, turnoffEdgebyName
*		process_pf_connections,
*		explodeBranches(2) (pf_cb.c)
*		add_graph_line (toolbox.c)
\*************************************************************************/
{
  GraphElement *pgraphnodenxt;
  DB_STAT stat;
  DB_STAT rtn;

  stat = findGraphEdgebyName( namebase1, namebase2, &pgraphnodenxt );

  if(D_FOUND != stat)
    stat = findGraphEdgebyName( namebase2, namebase1, &pgraphnodenxt );


  if(D_FOUND == stat)
  {
    rtn = findComplexEdge(&pgraphnodenxt, pgraphnodenxt);
    assert(D_FOUND == rtn);
  }

  *pedge = (GraphElement *)pgraphnodenxt;
  return stat;
}


DB_STAT findGraphEdgebyName( char *namebase1, char *namebase2, 
                              GraphElement **pedge )
/*************************************************************************\
* Purpose: find edge in the graph data 
*
* Called by: findGraphEdgebyName2
\*************************************************************************/
{
  GraphElement graphnode;
  GraphElement *pgraphnode = &graphnode;
  GraphElement *pgraphnodenxt;
  DB_STAT stat;


  memset(pgraphnode, ' ', sizeof(graphnode));
  pgraphnode->type   = GraphTypeEdge;
  strncpy(pgraphnode->vertex_id,    namebase1, sizeof(pgraphnode->vertex_id) ); 
  strncpy(pgraphnode->farvertex_id, namebase2, sizeof(pgraphnode->vertex_id) ); 

  stat = db_search(&graph_db, &pgraphnode, &pgraphnodenxt,
                       GRAPH_TYPE_VERTEX_FARVERTEX);

  *pedge = (GraphElement *)pgraphnodenxt;
  return stat;
}



int findnextedge(GraphElement *pvcur, GraphElement *pecur, GraphElement **penxt)
/**************************************************************************\
* Purpose: find the next edge connected to this edge
*
*	   i.e.    -----*-- ? --
*		     e  v   e
*
* Author: Dan Clark	 Dec 24, 1992
\**************************************************************************/
{
  DB_STAT stat;
  GraphLink *llretrec;
  GraphElement *pe;
  GraphElement *pv1, *pv2;
  GraphElement *pvnxt;

    stat = ll_first ( &graph_db, &pvcur, &llretrec, GRAPH_EDGE_LINK);
    if(D_FOUND != stat) {
      printf("  ERROR: no edge links - should be link to connected edges \n");
      return stat;
    }
    while(D_FOUND == stat) {
      pe = llretrec->graphnode; 
      if(pe != pecur) {
        if( GraphTypeEdge == pe->type &&
            GraphClassEdgeSection == pe->class ) { 
          *penxt = pe;
          return stat;
         } else {
           printf("bad edge found\n");
           printGraphElement(pe);
         }
      }
      stat = ll_next( &llretrec, &llretrec);
    }
    return stat;
}

DB_STAT findEdgeVertexes( GraphElement *pedge, 
                          GraphElement **vertex_1, GraphElement **vertex_2 )
/***************************************************************************\
*	purpose: Standardized call (with checks) to get the vertexes id's
*		 of the endpoints of an edge.  From graph_db.
*
*	Author: Bill Rogers	November 13, 1992
*
\***************************************************************************/
{
  GraphLink *plink;		/* ptr to graph link node */
  DB_STAT stat_link;

  stat_link = ll_first(&graph_db, &pedge, &plink, GRAPH_VERTEX_LINK);

  if (stat_link != D_FOUND) {
    printf("Warning: findEdgeVertexes: no vertex links\n");
    return( D_NOTFOUND );
  }

  *vertex_1 = (GraphElement *)plink->graphnode;
  stat_link   = ll_next(&plink, &plink);

  if(stat_link != D_FOUND) {
    printf("Warning: findEdgeVertexes: only 1 vertex link available.\n");
    return( D_NOTFOUND );
  }

  *vertex_2 = (GraphElement *)plink->graphnode;

  return( D_FOUND );		/* denotes 2 vertexes found */

}


DB_STAT findBusGraphVertexRec( char *namebase, GraphElement **rec )
/***************************************************************************\
*	Purpose: finds a graph_db bus vertex by name and base
*
*	Called by: createGraphCorEdges
*	 createGraphBusXYXY, checkCoord, findBusfromNameVertex
*		 process_pf_connections(3), merge_wp
*		 deleteVertexbyName, changeGraphBusVertex,
*		 turnoffVertexbyName, 
*
*	Author: D.Clark/B.Rogers
\***************************************************************************/
{
  GraphElement graphrecord;                     /* define   */
  GraphElement *pgraphrecord = &graphrecord;    /* allocate */
  GraphElement *pfoundrec;

  DB_STAT stat;

  if(db_valid_db(&graph_db) != D_OKAY) {	 /*** check for a valid db **/
    return D_EOF;
  }

  memset(pgraphrecord, ' ', sizeof(graphrecord));
  graphrecord.type   = GraphTypeVertex;		/* must be vertex */

  strncpy ( pgraphrecord->vertex_id,
            namebase,
            sizeof(pgraphrecord->vertex_id));	/* 12 characters long */


  stat = db_search ( &graph_db,
                     &pgraphrecord,		/* key */
                     &pfoundrec,		/* pointer to found record */
                     GRAPH_TYPE_VERTEX_ID );

/*  if(stat != D_FOUND)  then pf bus was not in coord file */

  *rec = pfoundrec;

  return( stat );
}

int findBusfromNameVertex(GraphElement **pbus, GraphElement *pvtx)
/****************************************************************************\
* Purpose: find bus vertex (given the NAME vertex)
*
* Called by: drawAreaButtonCB(2), addGraphBranch, (toolbox.c)
\****************************************************************************/
{
  GraphElement *pbusvtx;
  DB_STAT  stat;
  char namebase[32];

  sprintf(namebase, "%12.12s", pvtx->vertex_id);
  stat = findBusGraphVertexRec( namebase, &pbusvtx );

  if(D_FOUND == stat) {
    *pbus = pbusvtx;
    return 0;
  }
  return -1;
}


int findnextvertex(GraphElement *pecur, GraphElement *pvcur, GraphElement **pvnxt)
/****************************************************************************\
* purpose: find the "far end" of an edge.
*
*	i.e.  * ----- ?
*	      v   e   v
*
* author: Dan Clark		Dec 24, 1992
\****************************************************************************/
{
  DB_STAT stat;
  GraphElement *pv1, *pv2;

    stat = findEdgeVertexes(pecur, &pv1, &pv2); 
    if(D_FOUND == stat) {
      if(pv1 == pvcur){
        *pvnxt = pv2;
      } else {
        *pvnxt = pv1;
      }
    } else {
        *pvnxt = NULL;
    }
    return stat;
}


void findVertxLinkedToVertx(GraphElement **pvtxname, GraphElement *pvtx,
			GraphClass class )
/**************************************************************************\
*
\**************************************************************************/
{
  DB_STAT stat_link;
  GraphLink *plnk;
  GraphElement *ptmp;
    
    *pvtxname = NULL;
    stat_link = ll_first(&graph_db, &pvtx, &plnk, GRAPH_VERTEX_LINK);
    if(D_FOUND != stat_link) {
      printf("ERROR: cannot find a name vertex for bus %12.12s\n", 
        pvtx->vertex_id);
      return;
    }

    while(D_FOUND == stat_link) {
      ptmp = plnk->graphnode;
      if( class == ptmp->class) { 
        *pvtxname = ptmp;
        return;
      }
      stat_link = ll_next(&plnk, &plnk);
    }
}


int createBendArray( GraphElement *bends[], GraphElement *pcmplx,
			 GraphType type )
/***********************************************************************\
* input complex edge
* output ordered array of all bending points
*
*  Called by: printPSBranchRecord, deleteGraphComplexEdge
*		breakEdge (toolbox.c)
\***********************************************************************/
{
  int cnt, bendcnt;
  GraphElement *pecur, *pvcur;
  GraphElement *penxt, *pvnxt;
  GraphElement *stop_bend;
  GraphElement *pedge;
  DB_STAT stat;
  int ok;

  if( GraphClassEdgeComplexSection != pcmplx->class )
  {
    printf("*** WARNING *** from createBendArray\n");
    printf("    Edge param is not complex edge\n");
    findComplexEdge( &pedge, pcmplx );  /* ascertain its complx */
    pcmplx = pedge;
  }

  for(cnt = 0; cnt < PSMAXBENDS + 2; bends[cnt++] = NULL );

  bendcnt = 0;

  ok = get_alpha_order_of_cmplx( pcmplx, &pvcur, &stop_bend, &pecur );
  if ( ok != 1 ) return;
      
  if ( GraphTypeVertex == type )
    bends[bendcnt++] = pvcur;
  else if ( GraphTypeEdge == type )
    bends[bendcnt++] = pecur;
  else
  {
    printf("Bad param in call to createBendArray (graphdata.c)\n");
    return 0;
  }

  /*** find each subedge in order starting with the first **/
  penxt = NULL;
  pvnxt = NULL;

  while (bendcnt <= PSMAXBENDS + 2)
  {
    stat = findnextvertex(pecur, pvcur, &pvnxt);
    if(D_FOUND != stat) {
      printf("findnextvertex failed\n");
      break;
    } 
    if(pvnxt == stop_bend)
    { 		/* completed finding all bend points */
      if ( GraphTypeVertex == type )		/* Note that there is no */
        bends[bendcnt++] = pvnxt;		/* last edge to record   */
      return bendcnt;
    }
    stat = findnextedge(pvnxt, pecur, &penxt);
    if(D_FOUND != stat) {
      printf("findnextedge failed\n");
      break;
    } 
    if (bendcnt >= PSMAXBENDS + 2) {
      break;
    }
    if ( GraphTypeEdge == type )
      bends[bendcnt++] = penxt;		/* record edge & continue */
    else
      bends[bendcnt++] = pvnxt;		/* record vertex & continue */

    pvcur = pvnxt;			/* advance to next seg */
    pecur = penxt;
  }

  printf("WARNING - too many bends for edge, truncated after %d\n",
       PSMAXBENDS);
  printGraphElement(pcmplx);

}

int get_alpha_order_of_cmplx( GraphElement *pcmplx, GraphElement **lo_alpha,
		    GraphElement **hi_alpha, GraphElement **sub_edge )
/***************************************************************************\
* Purpose: Returns 3 values, enabling other routines
*	   to "walk the line"
*	1) Start vertex
*	2) Stop vertex
*	3) 1st edge (from low-alphabetical bus vertex)
*
* Called by: createBendArray (this module)
*
* Author: Wm E. Rogers		June 1996
\***************************************************************************/
{
  GraphElement *v1;
  GraphElement *v2;
  GraphElement *edge;
  GraphLink *llretrec;
  DB_STAT	stat;

  stat = ll_first ( &graph_db, &pcmplx, &llretrec, GRAPH_EDGE_LINK);

  if(stat != D_FOUND)
  {
/*    printf("  WARNING: From get_alpha_order_of_cmplx (graphdata.c)\n" );
    printf("       no edge links - should be link to subedges \n");
*/
    return 0;
  }

  *lo_alpha = NULL;
  *hi_alpha = NULL;

  stat = findEdgeVertexes(pcmplx, &v1, &v2); 
  if(D_FOUND != stat)
  {
    printf("ERROR bad vertexes found for edge\n");
    printGraphElement(pcmplx);
  } 

  if(0 == strncmp(v1->vertex_id,pcmplx->vertex_id, sizeof(pcmplx->vertex_id)))
  {
    *hi_alpha = v1;
    *lo_alpha = v2;
  }
  else if(0 == strncmp(v2->vertex_id, pcmplx->vertex_id,sizeof(v1->vertex_id)))
  {
    *lo_alpha = v2;
    *hi_alpha = v1;
  }
  else
  {
    printf("ERROR  get_alpha_ordr_of_cmpx (graphdata.c)\n");
    printf("   could not determine alphabetical order of vertexes\n");
    printGraphElement(pcmplx);
  }

    /** find the first subedge for the complex edge 
        by checking all the edge links of the complex edge **/
  while(stat == D_FOUND )
  {
    DB_STAT stat;
    GraphElement *pv1, *pv2;

    edge = llretrec->graphnode;
    stat = findEdgeVertexes( edge, &pv1, &pv2); 

    if( GraphClassEdgeSection == edge->class &&
	( pv1 == *lo_alpha || pv2 == *lo_alpha) )
    {
	*sub_edge = edge;
	return 1;
    }
      
    stat = ll_next( &llretrec, &llretrec );
  }

  printf("WARNING - from get_alpha_order (graphdata.c)\n");
  printf("WARNING: routine failed to find subedge connected to low-alpha vertex\n");
  printGraphElement( edge);

  return 0;
}



void destroyAllGadgets(Widget w, XtPointer tag, XmAnyCallbackStruct *cbd)
/*****************************************************************
* Purpose: remove ALL gadgets with a display flag set on
*	 and update the data manager to reflect the change
*	 in status.  This routine should be called to clean off the 
*	 graph display piece by piece.  Cleaning the graph will
*	 should be done when a new coordinate file has been selected
*	 for viewing or a new base case is merged.
* NOTE:
*	 A more effecient means of destroying
*	 all the displayed graphics is to destroy the draw window
*	 and recreate it (not yet tested).  Even if the draw window
*	 is destroyed to remove all the gadgets, all the data
*	 structures need to be updated to turn off the display
*	 flag.
*
* Called by: tools_set_view_mode_cb (graphdata.c)
*	     create_from_scratch (filedlgrtn.c)
*	     apply_files   (filedlgrtn.c) 
\*******************************************************************/
{
  GraphElement graphvertex;
  GraphElement *pgraphvertex;
  GraphElement *pgraphvertexnxt;
  GraphElement *pgraphvertexprev;
  DB_STAT stat, stat_disp;


  /*** check for a valid db **/
  if(db_valid_db(&graph_db) != D_OKAY) {
    return;		 /* no graph table so there can't be any edges */
  }

  stat_disp = db_first( &graph_db, &pgraphvertexnxt, GRAPH_TYPE );

  if (stat_disp == D_EOF) {
    /* no vertexes/edges found, just return */
    return;
  }

  while(stat_disp == D_FOUND)
  {
    if(pgraphvertexnxt->wid != NULL)		 /** destroy the gadget **/
    {
      if(XtIsManaged(pgraphvertexnxt->wid))
        XtUnmanageChild(pgraphvertexnxt->wid);
  
      XtDestroyWidget(pgraphvertexnxt->wid);
    }

    /** delay update until after next performed **/
    pgraphvertexprev = pgraphvertexnxt;

    pgraphvertex = pgraphvertexnxt;
    stat_disp = db_next( &graph_db, &pgraphvertex, &pgraphvertexnxt,
							 GRAPH_TYPE );

    /** update display flag to off for previous vertex **/
    pgraphvertex = &graphvertex;
    memcpy(pgraphvertex,pgraphvertexprev, sizeof(GraphElement));
    pgraphvertex->display = GraphDisplayOff;
    pgraphvertex->wid = NULL;
    stat = db_update(&graph_db, &pgraphvertexprev, &pgraphvertex);

    if(stat != D_OKAY) {
        printf("ERROR: destroyAllGadgets: update failed stat = %d\n", stat);
    }

  } /* end loop through all displayed vertices */

}

#define MAXRECS 500

typedef struct newLine
{
  char *lines[MAXRECS];
  int cnt;
} NewLine;

typedef struct connectData { char type[1];
                             char blank1[1];
                             char name1[8];
                             char base1[4];
                             char blank2[1];
                             char name2[8];
                             char base2[4];
                             char blank3[1];
                             char cktcnt[1];
} ConnectData;



void process_pf_connections(char *pfdataret)
/**************************************************************************\
* Purpose: part of the MERGE process - reads powerflow feedback and
*	   creates all buses and branches mentioned in the feedback.
*
* Called by:	merge_wp
\**************************************************************************/
{
  NewLine con;
  ConnectData *pcon;
  int rtncde;
  int nvtx;
  int idx;
  DB_STAT stat;
  GraphElement *pgraphname;
  GraphElement *pgraphcoordvertexnxt = NULL;

  GraphElement *pgraphbus1;
  GraphElement *pgraphbus2;
  GraphElement *pgraphnamerec;
  GraphElement *pnewedge;
  GraphElement *pcomplex_edge;
  GraphElement *vtx1, *vtx2;
  
  char key[128];
  char namebase[128];

    /* yes there is a cooresponding powerflow node */
    /* 1) display the bus */
    /* 2) add all the lines to already added buses */

  con.cnt = 0;		/* initialize the count (used in checkLinesBuses) */
  forEachipfLine(pfdataret, checkLinesBuses, (char *)&con);


    /* now index through all lines */
  nvtx = 0;
		/* con.cnt = number of lines returned by forEachipfLine */

  for(idx = 0; idx < con.cnt; idx++)
  {
    pcon = (ConnectData *)con.lines[idx];

    /********************** process a PF bus record ************************/
    if(strncmp((pcon->type), "B", sizeof(pcon->type)) == 0) 
    {
      sprintf(namebase, "%12.12s", &pcon->name1[0]);
      stat = findBusGraphVertexRec( namebase, &pgraphcoordvertexnxt );
      if(stat != D_FOUND)
      {
        continue;
      }

      manageVertex(pgraphcoordvertexnxt);	/* bus */

      findVertxLinkedToVertx(&pgraphname, pgraphcoordvertexnxt,
				GraphClassVertexName );
      if( pgraphname )
          manageVertex(pgraphname);		/* name */

      if( pgraphcoordvertexnxt->hide == NO_HIDE )
      {

	if( XmToggleButtonGadgetGetState( widget_id( 
			"view_menu_bus_combine_tb")) == 0 )
	{
	  findVertxLinkedToVertx(&pgraphname, pgraphcoordvertexnxt,
				 GraphClassVertexGenerator);
	  if( pgraphname && pgraphname->hide == NO_HIDE )
	  {
	    manageVertex(pgraphname);		/* Gen */
	    findGraphEdgebyName2( pgraphcoordvertexnxt->vertex_id,
				   "ZZGenerator ", &pnewedge );
	    if( pnewedge )
	      manageEdge( pnewedge );
	  }

	  findVertxLinkedToVertx(&pgraphname, pgraphcoordvertexnxt,
				GraphClassVertexCapacitor );
	  if( pgraphname && pgraphname->hide == NO_HIDE )
	  {
	    manageVertex(pgraphname);		/* Reac */
	    findGraphEdgebyName2( pgraphcoordvertexnxt->vertex_id,
				   SHUNT_N, &pnewedge );
	    if( pnewedge )
	      manageEdge( pnewedge );
	  }
	}
      }
    }
    else
    { /* now manage all branches for bus GraphElement *pgraphbus; */
  
      pcon = (ConnectData *)con.lines[idx];	/* list of lines connected */

      /* take the farend bus name from the line and check for a bus */

      sprintf(namebase, "%12.12s", &pcon->name1[0]);

      stat = findBusGraphVertexRec(namebase, &pgraphbus1 );

      if(D_FOUND != stat) {
        continue;		 /* near end bus not in coord file */
      }

      if(pgraphbus1->display != GraphDisplayOn) {
	continue;		 /* near end bus not displayed */
      }

      sprintf(namebase, "%12.12s", &pcon->name2[0]);
      stat = findBusGraphVertexRec(namebase, &pgraphbus2 );

      if(D_FOUND != stat) {
        continue;		 /* far end bus not in coord file */
      }

		/* far end bus not displayed */
		/* also... don't create branch between two hidden buses */
      if(pgraphbus2->display != GraphDisplayOn ||
         ( pgraphbus1->hide == HIDE && pgraphbus2->hide == HIDE )) {
        continue;		 /* far end bus not displayed */
      }

      /* now manage line between two busses */
      /* take all the line data and create a branch */
      /* may want to make all branches from low to high */
  
      stat = findGraphEdgebyName2(pcon->name1, pcon->name2, &pnewedge );

		/* create the missing branches..... */

      if(D_FOUND != stat)
      {
	createGraphBranch( pgraphbus1, pgraphbus2);	/* cmplx edge */
      }
      else						/* turn on */
      {
        if(GraphDisplayOn != pnewedge->display)
	{
          if(GraphClassEdgeComplexSection == pnewedge->class) {
             /* printf("need to turn on all sub edges\n"); */
            turnOnSubedge(pnewedge);
	  }
	  else
	  {
	    printGraphElement(pnewedge);
          printf("PROGRAMMER ERROR mergeCoordBase should never find subedge\n");
	  }
	}
	else
	{
              /* printf("already turned on so leave it alone\n"); */
        }
      }
  
    } /* end of processing a line */
  } /* end loop through powerflow records */
} /* end do this bus */


#define NOT_DONE	0
#define DONE 		1



int merge_wp(int *merge)
/**************************************************************************\
*  Purpose: 1) Read all coord bus records
*	    2) Confirm existance in powerflow
*	    3) merges results
*
*  Called by: mergeCoordBase (this module)
\**************************************************************************/
{
  GraphElement graphcoordvertex;	/* vertex of base data (storage) */ 
  GraphElement *pgraphcoordvertex = &graphcoordvertex;	/* ptr to vertex */
  
  static GraphElement *pgraphcoordvertexnxt;	/* next graph vertex */

  static DB_STAT stat = D_NOTFOUND;	/* status from coord db access */

  char newkey[256];
  char  outbuf_array[BUFSIZE];
  char  inbuf_array [BUFSIZE];
  char *pfcmdstr  = outbuf_array;
  char *pfdataret = inbuf_array;

  Boolean rtn_stat;
  int bus_cnt;
  int rtncde;

  sprintf(pfcmdstr, "/GET_DATA, TYPE = CONNECTION\n");

  /* retrieve the graph vertices derived from coord data	*/
  /* set the key fields and set ptr before first record	*/

  bus_cnt = 0;

  do
  {
    if(D_FOUND != stat)		/* Note this is executed only at start	  */
    {				/* Use blank to position at 1st bus in db */
      stat = findBusGraphVertexRec( "            ", &pgraphcoordvertexnxt );

      if (stat == D_EOF)
      {
        printf("WARNING: merge - EOF no bus coord record(s) in coord file\n");
        *merge = DONE;			/* routine shall be callable again */
        return ((Boolean) TRUE);	/* merge is done */
      }
      else if ( stat == D_FOUND )
      {
	printf("*** PROGRAMMER 	ERROR in merge_wp (graphdata.c)\n");
	printf("*** Blank bus name record encountered.\n");
      }

      stat = D_FOUND;	   /* NOTE: "blank" bus record returned D_NOTFOUND */
    }

    assert(D_FOUND == stat);

    /** add bus vertex to string **/
    sprintf(newkey, "B     %12.12s   \n", pgraphcoordvertexnxt->vertex_id);
    strcat(pfcmdstr, newkey);

    pgraphcoordvertex = pgraphcoordvertexnxt;		 /** get next **/
    stat = db_next(&graph_db, &pgraphcoordvertex, &pgraphcoordvertexnxt, 
							GRAPH_TYPE_VERTEX_ID);
    /* if key values are out of range, stop loop */
    if(D_FOUND == stat)			/* next condition of next item */
    {
      rtn_stat = (Boolean) FALSE;	/* continue work process */
    }
    else
    {
      *merge = DONE;			/* frees wp to be callable again */
      rtn_stat = (Boolean) TRUE;	/* merge is done */
    } 
    bus_cnt++;

  } while (bus_cnt < 10 && stat == D_FOUND);

  strcat(pfcmdstr, ENDOFMSG);
  strcat(pfcmdstr, "\n");

  rtncde = ipc_synch_rw(pfdataret, pfcmdstr);
  process_pf_connections(pfdataret);

  return((Boolean) rtn_stat);
}




void mergeCoordBase()
/**************************************************************************\
* Purpose: starts the merge work process (loops thur 10 coord buses
*	   at a time)
*
*	   Supposenly, this work process allows MOTIF to execute
*	     while processing powerflow stuff, but doesn't seem to
*	     be working correctly.
*
* Called by: build*MainDisplay (above)
*
* Author: Dan Clark    (Cleaned up by Bill Rogers 8/13/98)
\**************************************************************************/
{
  extern XtAppContext app_context;
  XtWorkProcId merge_wpid;
  static int merge = DONE;   /* tells program that previous merge is done */

  if( DONE == merge)
  {
    merge = NOT_DONE;

    merge_wpid = XtAppAddWorkProc( app_context, (XtWorkProc)merge_wp, &merge);
	/* Note: merge_wp continues to call itself until merge_wpid = TRUE */
  }
  else
  {
    printf("*** INFO *** From mergeCoordBase (graphdata.c)\n");
    printf("User Warning - must wait for previous request to complete\n");
  }
}


void updateGraphDisplay( GraphElement *pge, GraphDisplay graphdisplay )
/************************************************************************\
* Purpose: Standard method of changing GraphDisplay status of any
*	   graphelement.
*
* Author: Bill E. Rogers		June, 1996
\************************************************************************/
{
  GraphElement graphnode;               /* update node for graph */
  GraphElement *pgraphnode = &graphnode;/* pointer to new graph node data */
  DB_STAT stat_update;

  if( (long)graphdisplay == pge->display )
  {
    printf( "* WARNING * from updateGraphDisplay\n" );
    printf( "		graphelement %24.24s is already set\n",
             &pge->vertex_id[0] );
  }
 
  memcpy(pgraphnode, pge, sizeof(GraphElement));
  pgraphnode->display = graphdisplay;
  stat_update = db_update(&graph_db, &pge, &pgraphnode);

  assert( stat_update == D_OKAY );
}


int checkOn (GraphElement *pelement, GraphElement *pparent, GraphLink *plink,
							char *userdata)
/************************************************************************\
* 
\************************************************************************/
{
  int cnt;
  DB_STAT stat;
  GraphElement *pedge;
  GraphElement *porig = (GraphElement *)userdata;
  
  if(pelement->class == GraphClassEdgeSection ) {
     /** skip display of base edge **/
  }
  else if(pelement->class == GraphClassEdgeComplexSection)
  {
    /** skip print and get all child edges **/
    /** if both ends of the edge are in powerflow **/
    stat = findGraphEdgebyName2(pelement->vertex_id, 
            pelement->farvertex_id, &pedge);
    if(D_FOUND == stat) {
        cnt = forEachLink(graph_db, pelement, GRAPH_EDGE_LINK, 
            checkOn, (char *)userdata);
    }
  }
  else if(pelement->class == GraphClassEdgeSection)
  {
    /** check if both endpoints are in powerflow here **/
    stat = findGraphEdgebyName2(pelement->vertex_id, 
            pelement->farvertex_id, &pedge );
    if(D_FOUND == stat) {
        cnt = forEachLink(graph_db, pelement, GRAPH_VERTEX_LINK, 
            checkOn, (char *)userdata);
       updateGraphDisplay( pelement, GraphDisplayRequestOn );
    }
  }
  else if(pelement->type == GraphTypeVertex)
  {
     if(pelement->class == GraphClassVertexBendPoint) {
       updateGraphDisplay( pelement, GraphDisplayRequestOn );
     }
  }
  else
  {
    updateGraphDisplay( pelement, GraphDisplayRequestOn );
  }
  return(0);
}



void graphCorOn(Widget w, XtPointer tag, XmAnyCallbackStruct *callback_data)
/*****************************************************************************\
* Purpose: loop through all elements (in graph data structure) that were
*	 derived from the coordinate file and turn on the display flag.
*	 Once the display flag is on all of these objects will be manifested
*	 on the screen according to the type of object (vertex or edge)
*	 and the class (vertex: bend or bus, edge: transformer,
*	 line (500 or 230 etc)
* see vertex.c and edge.c for logic which creates the gadgets
*	 to show these objects.
*
* Author: Dan Clark	Dec, 1991	Modified: Dec 17, 1991
*
* Called by: buildMainDisplay
* this routine should be called by a button on the graphics toolbox.
*
\*****************************************************************************/
{
  GraphElement graphnode;
  GraphElement *pgraphnode = &graphnode;
  GraphElement *pgraphnodenxt;

  DB_STAT stat;		/* status returned for general db access */

  /*** check for a valid db **/
  if(db_valid_db(&graph_db) != D_OKAY) {
    return;
  }

  printAllBusesAndBranches();

  /** DEBUG  DEBUG Get the first record **/
  pgraphnode = &graphnode;
  stat = db_first ( &graph_db, &pgraphnodenxt, GRAPH_TYPE_VERTEX_ID);


printf("\nBEGIN TURNING ON ALL GRAPHELEMENTS......(graphCorOn graphdata.c)\n" );
  /** find 1st record dervied from coord file **/
  pgraphnode = &graphnode;

  stat = db_first(&graph_db, &pgraphnodenxt, GRAPH_TYPE_VERTEX_ID );
  if (stat == D_EOF)
  {
    printf("ERROR db_search did not find any records \n");
    return;
  }
  /** set to start of records **/
  if (stat == D_NOTFOUND)
  {
     stat = D_FOUND;
  }

  while(stat == D_FOUND)
  {
    if (pgraphnodenxt->class == GraphClassEdgeComplexSection ||
	pgraphnodenxt->class == GraphClassVertexOrgComment   ||
	pgraphnodenxt->class == GraphClassVertexFont ) {
        /* skip turning on these edges */
    }
    else
    {
      updateGraphDisplay( pgraphnodenxt, GraphDisplayRequestOn );
      printGraphElement( pgraphnodenxt );
    }

    pgraphnode = pgraphnodenxt;
    stat = db_next(&graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_TYPE );
  }
}


int printAllGraphData(Widget widget, XtPointer tag, XmAnyCallbackStruct *cbs)
/*****************************************************************************\
* Purpose:
*   this routine was designed primarily for debugging of the graphics
* data structure.  It is a good example of how to loop through the
* dmgr table of graphics data on a particular key.  It can serve as
* the basis for a routine to write out a file to save the graphics
* information that is more represented of the graph which is drawn.
*       Called by:
*  typically hooked up to a menu pulldown for debugging or
*  an icon in the toolbox.
*
*  Author: Dan Clark	Dec, 1991	Modified: Dec 17, 1991
\*****************************************************************************/
{
  GraphElement graphnode;
  GraphElement *pgraphnode = &graphnode;
  GraphElement *pgraphnodenxt;

  GraphElement *linkgraphrec;
  GraphLink *llretrec;

  DB_STAT stat;         /* status returned for general db access */
  DB_STAT stat_link;            /* status returned for ll dmgr access */

  /*** check for a valid db **/
  if(db_valid_db(&graph_db) != D_OKAY) {
    return;
  }
  /** retrieve all the records (in key order) and write to output file **/

  /** Get the first record **/
  pgraphnode = &graphnode;
  stat = db_first ( &graph_db, &pgraphnodenxt, GRAPH_TYPE_VERTEX_ID);

  if(stat != D_FOUND)
  { 
    printf("ERROR db_search did not find any records \n");
    return(1);
  }

  printf("\n\nPrint Graph Data\n");

  /** Now loop through all the records **/
  while(stat == D_FOUND)
  {
    printf("\n");
    printGraphElement(pgraphnodenxt);

    /** now follow all vertex links of the current record **/
    stat_link = ll_first
                (
                  &graph_db,
                  &pgraphnodenxt,
                  &llretrec,
                  GRAPH_VERTEX_LINK
                );

    if(stat_link != D_FOUND)
    {
      printf("  Note: no vertex links\n");
    }

    while(stat_link == D_FOUND)
    {
      linkgraphrec = llretrec->graphnode;

      /** Print the vertex record **/
      printf("  VL ~");

      printGraphElement(linkgraphrec);

      stat_link = ll_next( &llretrec, &llretrec );
    }

    /** now follow all edge links of the current record **/
    stat_link = ll_first
                (
                  &graph_db,
                  &pgraphnodenxt,
                  &llretrec,
                  GRAPH_EDGE_LINK
                );

    if(stat_link != D_FOUND)
    {
      printf("  Note: no edge links\n");
    }

    while(stat_link == D_FOUND)
    {
      linkgraphrec = llretrec->graphnode;

      /** Print the edge record **/
      printf("  EL ~");

      printGraphElement(linkgraphrec);

      stat_link = ll_next( &llretrec, &llretrec );
    }

    /** Get next record **/
    pgraphnode = pgraphnodenxt;
    stat = db_next
           (
             &graph_db,
             &pgraphnode,
             &pgraphnodenxt,
             GRAPH_TYPE_VERTEX_ID
           );
  }
  printf("\nEnd Print Graph ALL Data\n\n");
}



void printGraphData( Widget w, XtPointer tag, XmAnyCallbackStruct *cbd)
/*****************************************************************************\
*  Purpose:  write graph data ( in coord file format )
*
* currently specified as printGraphData to utilize pre-existing
*  button for testing.  fixes file name to cor.out
*		
*	Author: Dan Clark	Dec 16, 1992
*
* Called by: NONE   - used for debugging when needed.
\*****************************************************************************/

{
  GraphElement graphnode;
  GraphElement *pgraphnode = &graphnode;
  GraphElement *pgraphnodenxt;

  GraphElement *linkgraphrec;
  GraphLink *llretrec;

  DB_STAT stat;         /* status returned for general db access */
  DB_STAT stat_link;            /* status returned for ll dmgr access */

  char *corfilename = "cor.out";
  FILE *pcorfile = NULL;	/* makes it come out on screen */

  pcorfile = NULL;

  if(NULL == pcorfile) {
     pcorfile = fopen(corfilename, "w");
     if(NULL == pcorfile) {
        printf("ERROR - cannot open coord file\n");
        return;
     }
  }

  /*** check for a valid db **/
  if(db_valid_db(&graph_db) != D_OKAY) {
    printf("ERROR: printGraphData - no graph table\n");
    return;
  }
  /** retrieve all the records (in key order) and write to output file **/

  /** Get the first record **/
  pgraphnode = &graphnode;
  stat = db_first ( &graph_db, &pgraphnodenxt, GRAPH_TYPE_VERTEX_ID);

  if(stat != D_FOUND) { 
    printf("ERROR db_search did not find any records \n");
    return;
  }

  fprintf(pcorfile, "%s\n", PSIDRECORD);

  /** first loop through all bus records **/
  while(stat == D_FOUND)
  {
    if( GraphTypeVertex == pgraphnodenxt->type &&
        GraphClassVertexBus == pgraphnodenxt->class )
    { 
      printPSBusRecord(pcorfile, pgraphnodenxt);
    }

    /** Get next record **/
    pgraphnode = pgraphnodenxt;
    stat = db_next ( &graph_db, &pgraphnode, &pgraphnodenxt,
             GRAPH_TYPE_VERTEX_ID);
  }

  /** Get the first branch record **/
  pgraphnode = &graphnode;
  stat = db_first ( &graph_db, &pgraphnodenxt, 
                    GRAPH_TYPE_VERTEX_FARVERTEX);
  if(stat != D_FOUND) { 
    printf("ERROR db_search did not find any branch records\n");
    return;
  }
  while(stat == D_FOUND)
  {
    if( GraphTypeEdge == pgraphnodenxt->type &&
        GraphClassEdgeComplexSection == pgraphnodenxt->class ) { 
      printPSBranchRecord(pcorfile, pgraphnodenxt);
    }
    /** Get next record **/
    pgraphnode = pgraphnodenxt;
    stat = db_next ( &graph_db, &pgraphnode, &pgraphnodenxt,
             GRAPH_TYPE_VERTEX_ID);
  }

  fprintf(pcorfile, "%s\n", PSENDRECORD);	/* prints (*EOR) */

  fclose(pcorfile);
  pcorfile = NULL;
  return;
}


void printPSBusRecord(FILE *pcorfile, GraphElement *pv)
/************************************************************************\
* Purpose:  print bus records in POSTSCRIPT form.
*
* Called by: printGraphData, pscordat.c
\************************************************************************/
{
  PSBusRecord bus;
  PSCoordFileRecord *coord_rec_p;		/* by B.Rogers */

  DB_STAT stat_link;
  GraphLink *llretrec;
  GraphElement *linkgraphrec;
  GraphElement *pgraphname;
  GraphElement *pgraphgen;
  char abuf[16];
  char aformat[64];
  int reclen;
  double d_ang;
  char c_ang[5];

  /* temporary patch to dig up some missing data from original coord file */
  coord_rec_p = coord_db_bus_search( &pv->vertex_id[0] );
  findVertxLinkedToVertx( &pgraphname, pv, GraphClassVertexName );

  memset( &bus.key[0],  ' ',       sizeof(bus) - sizeof(bus.idx));
  memcpy( &bus.key[0],  PSKEY_BUS, sizeof(bus.key) );
  bus.display[0] = pv->hide;

  memcpy( &bus.name[0],    pv->vertex_id, sizeof(bus.name) + sizeof(bus.base) );
  memcpy( &bus.abbr[0],   pgraphname->farvertex_id, sizeof(bus.abbr) ); 

  ipf_itoa(pgraphname->dply_seg,  abuf );
  memcpy( &bus.v_flag[0],  abuf, sizeof(bus.v_flag) ); 

  sprintf( abuf, POSFORMAT, MOTIF_to_ps_x(pv->x, "BUS", bus.name, " " ) );
  memcpy( &bus.symbol.x[0], abuf, sizeof(bus.symbol.x) );

  sprintf( abuf, POSFORMAT, MOTIF_to_ps_y(pv->y, "BUS", bus.name, " " ) );
  memcpy( &bus.symbol.y[0], abuf, sizeof(bus.symbol.y) );

	/*********** GENERATOR *************/
  findVertxLinkedToVertx( &pgraphgen, pv, GraphClassVertexGenerator );
  if( pgraphgen )
  {
    d_ang = radiansBetweenPts( pv->x, pgraphgen->y, pgraphgen->x, pv->y );
			/* NOTE: y is reverse due to MOTIF reversed coord */
    d_ang /= DegtoRad;
    if (d_ang == 0 && pgraphgen->hide == NO_HIDE )
      d_ang = 1.;

    sprintf( c_ang, "%3.0f", d_ang );
    memcpy( &bus.generator_ang[0], c_ang, sizeof(bus.generator_ang) );
  }
  else if( coord_rec_p != NULL )
      memcpy( &bus.generator_ang[0], coord_rec_p->bus.generator_ang,
				   sizeof(bus.generator_ang) );
  else
      memset( &bus.generator_ang[0], ' ', sizeof(bus.generator_ang) );

	/*********** REACTOR *************/
  findVertxLinkedToVertx( &pgraphgen, pv, GraphClassVertexCapacitor );
  if( pgraphgen )
  {
    d_ang = radiansBetweenPts( pv->x, pgraphgen->y, pgraphgen->x, pv->y );
			/* NOTE: y is reverse due to MOTIF reversed coord */
    d_ang /= DegtoRad;
    if (d_ang == 0 && pgraphgen->hide == NO_HIDE )
      d_ang = 1.;

    sprintf( c_ang, "%3.0f", d_ang );
    memcpy( &bus.reactor_ang[0], c_ang, sizeof(bus.reactor_ang) );
  }
  else if( coord_rec_p != NULL )
    memcpy( &bus.reactor_ang[0], coord_rec_p->bus.reactor_ang,
				 sizeof(bus.reactor_ang) );
   else
    memset( &bus.reactor_ang[0], ' ', sizeof(bus.reactor_ang) );

  memcpy( &bus.symbol_shape[0], SYMBOL_SHAPE, sizeof(bus.symbol_shape) );
  sprintf( abuf, "%4.2f", ( SYMBOL_RADIUS )/PixelsPerCm );

  memcpy( &bus.symbol_radius[0], abuf, sizeof(bus.symbol_radius) );

  /** now follow all vertex links of the current record **/
  stat_link = ll_first ( &graph_db, &pv, &llretrec, GRAPH_VERTEX_LINK);

  if(stat_link != D_FOUND) {
      printf("PROGRAM ERROR noticed by printPSBusRecord (graphdata.c)\n" );
      printf("  Missing vertex link of bus-to-name. \n");
      printf("  BUS RECORD:%s\n", pv->vertex_id );
  }

  while(stat_link == D_FOUND) {
    linkgraphrec = llretrec->graphnode;

    /** format name info into bus record **/
    if( GraphClassVertexName == linkgraphrec->class &&
        GraphTypeVertex == linkgraphrec->type ) {

      sprintf( abuf, POSFORMAT,
		 MOTIF_to_ps_x(linkgraphrec->x, "LBL", bus.name, " ") );
      memcpy( &bus.namepos.x[0], abuf, sizeof(bus.namepos.x) );

      sprintf( abuf, POSFORMAT, 
		 MOTIF_to_ps_y(linkgraphrec->y, "LBL", bus.name, " ") );
      memcpy( &bus.namepos.y[0], abuf, sizeof(bus.namepos.y) );

    }

    stat_link = ll_next( &llretrec, &llretrec );
  }

  reclen = sizeof(bus) - sizeof(bus.idx);
  /* bus.eol[0] = 0; */
  sprintf(aformat, "%s%d.%d%s", "%", reclen, reclen, "s\n");
  fprintf(pcorfile, aformat, &bus.key[0] );
    
  /* fprintf(pcorfile, "%s#%32.32s\n", &bus.type[0], pv->key.pfbus.type); */
}


#define PSCIRCUIT_ID " "
#define PSSECTION_ID " "

void printPSBranchRecord(FILE *pcorfile, GraphElement *pcomplx)
/*************************************************************************\
* Purpose: Write complex edge to COORD file
*
*  check for bend pts and include all bendpts for edge
*
* 1) format the coordinate file line record header info
* 2) find the first edge
* 3) chase the links from the current edge to the next
*    until the farend vertex is found
* 4) if the edge has at least one bending point
*    print the edge to the output file
*
* Author: Dan Clark	 Dec 15, 1992
***********************************************************************/
{
#define ALWAYS_SAV 0		/* Causes ALL branches to write out */
#define SAVE_CONDT 1		/* Causes write only if bends occur */

  GraphElement		*bends[PSMAXBENDS + 2];
  PSBranchRecord	line;
  char abuf[16];
  char aformat[64];
  int cnt, bendcnt;
  int reclen;
  int save_fg;
  int save_sw = SAVE_CONDT;	/* hardwire to save only if bends exists */

  save_fg = 0;

  /** format the line record from the complex edge data **/
  memset(&line.key[0], ' ', sizeof(line) - sizeof(line.idx) );

  if( istransformer( pcomplx ) )
    memcpy( &line.key[0], PSKEY_TRANSFORMER, sizeof(line.key) ); /* trnsfmr */
  else
    memcpy( &line.key[0], PSKEY_LINE, sizeof(line.key) );  /* its a line */

  memcpy( &line.name1[0], pcomplx->vertex_id, 
             sizeof(line.name1) + sizeof(line.base1) );
  memcpy( &line.name2[0], pcomplx->farvertex_id, 
             sizeof(line.name2) + sizeof(line.base2) );

  memcpy( &line.circuit_id[0], PSCIRCUIT_ID, sizeof(line.circuit_id) );
  memcpy( &line.section_id[0], PSSECTION_ID, sizeof(line.section_id) );

  sprintf( abuf, "%2d", pcomplx->dply_seg );
  memcpy( &line.segment[0], abuf, sizeof(line.segment) );
  if( pcomplx->dply_seg != 1 ) save_fg = 1;

    /** get vertexes of bendpoints complex edge ***/
  bendcnt = createBendArray(bends, pcomplx, GraphTypeVertex );
  if( bendcnt > 2 ) save_fg = 1;

    /** format bending point data into record **/
  for(cnt = 0; cnt < bendcnt-2; cnt++)
  {
    sprintf( abuf, POSFORMAT,
		MOTIF_to_ps_x(bends[cnt+1]->x, "BEND", line.name1, line.name2));
    memcpy( line.bend[cnt].x, abuf, sizeof(line.bend[cnt].x) );
    sprintf( abuf, POSFORMAT,
		MOTIF_to_ps_y(bends[cnt+1]->y, "BEND", line.name1, line.name2));
    memcpy( line.bend[cnt].y, abuf, sizeof(line.bend[cnt].y) );
  }

  if( save_sw && save_fg )
  {
    reclen = sizeof(line) - sizeof(line.idx);
    sprintf(aformat, "%s%d.%d%s", "%", reclen, reclen, "s\n");
    fprintf(pcorfile, aformat, &line.key[0] );
  }

}


void printPSDrawRecord	( FILE *pcorfile, GraphElement *pv )
/*********************************************
* purpose: Print out all the draw records.
* 
* author: Bill Rogers
***********************************************/
{
  printf(" printPSDrawRecord routine yet to be written!\n" );

  return;
}
 


int sprintfGraphElement(char *outstr, GraphElement *pgraphnodenxt)
/*********************************************
* author: Dan Clark/ John Rutis
* purpose: pretty print an element in the graph database
**********************************************
*/
{
  char temp[512];
    sprintf(outstr, "");
    if(pgraphnodenxt == NULL) {
       printf("Programmer Error: printGraphElement: null value passed\n");
       return 1;
    }
    /** Print the current record **/
    sprintf(temp,"%c~", pgraphnodenxt->type?'E':'V');
    strcat(outstr,temp);

    /* if(!pgraphnodenxt->type) */
      sprintf(temp,"%12.12s~", pgraphnodenxt->vertex_id);
      strcat(outstr,temp);
      sprintf(temp,"%12.12s~", pgraphnodenxt->farvertex_id);
      strcat(outstr,temp);
    switch(pgraphnodenxt->display)
    {
      case GraphDisplayOff:	strcat(outstr,"OFF  ~"); break;
      case GraphDisplayOn:	strcat(outstr,"ON   ~"); break;
      case GraphDisplayRequestOn: strcat(outstr,"ReqON~"); break;
    }

    switch(pgraphnodenxt->class)
    {
      case GraphClassVertexBus:		strcat(outstr,"Bus    ~"); break;
      case GraphClassVertexName:	strcat(outstr,"Name   ~"); break;
      case GraphClassVertexBendPoint:	strcat(outstr,"Bend   ~"); break;
      case GraphClassVertexGenerator:	strcat(outstr,"Gen    ~"); break;
      case GraphClassVertexCapacitor:	strcat(outstr,"Reac   ~"); break;
      case GraphClassVertexReactor:	strcat(outstr,"Reac   ~"); break;
      case GraphClassEdgeSection:	strcat(outstr,"Sect   ~"); break;
      case GraphClassEdgeComplexSection:strcat(outstr,"CpxSect~"); break;
      case GraphClassEdgeSubSection:	strcat(outstr,"SubSect~"); break;
    }

    if(!pgraphnodenxt->type)
    {
      sprintf(temp,"X%4d~", pgraphnodenxt->x);
      strcat(outstr,temp);
      sprintf(temp,"Y%4d~", pgraphnodenxt->y);
      strcat(outstr,temp);
    }

    sprintf(temp,"\n");
    strcat(outstr,temp);
    return 0;
}
int printGraphElement(GraphElement *pgraphnodenxt)
{
  int rtncde;
    char outstr[512];
    memset(outstr, ' ', sizeof(outstr));
    rtncde = sprintfGraphElement(outstr, pgraphnodenxt);
    if(0 == rtncde) {
      printf("%s", outstr);
    }
    return(rtncde);
}
/**
* function: insertByType
* purpose: make the order of the children inserted on
* the graph correspond to the type of graph element
* If the graph list is walked top down then the vertices
* must be in the list first for the mouse pointer to grab
* them on top of lines,  if the graph list is walked the
* other direction then the vertices must be listed last
*
*/
Cardinal insertByType(Widget wid)
{
  static Widget draw_id = FALSE;
  Cardinal numberChildren;
  GraphElement *pge;
  XtPointer pudata;

  if(FALSE == draw_id) {
    if((draw_id = widget_id("drawwindow")) == FALSE)
    {
       printf("ERROR:cannot find drawwindow\n");
       return;
    }
  }

  XtVaGetValues( wid,
        XmNuserData, &pudata,
        NULL);
  if(graphorder == 0) {
  if(NULL != pudata) {
      pge = (GraphElement *) pudata;
      if(pge->type != GraphTypeVertex) {
        numberChildren = 0;
      } else {
        XtVaGetValues( draw_id,
          XmNnumChildren, &numberChildren,
          NULL);
      }
   }
 } else { /* BOTTOM_UP server searches child list from bottom up (OSF MACHINE) */
  if(NULL != pudata) {
      pge = (GraphElement *) pudata;
      if(pge->type != GraphTypeEdge) {
        numberChildren = 0;
      } else {
        XtVaGetValues( draw_id,
          XmNnumChildren, &numberChildren,
          NULL);
      }
   }
 } /* graph order */
  return numberChildren;
}
/* 
* function: printGraphCompositeChildren
* purpose:
* print the ordered list of children displayed on the graph
* this is a useful debugging tool if the graph does not
* seem to respond to the mouse clicks for busses located under
* lines
* usually hooked up to a button on the toolbox.
*/
void printGraphCompositeChildren()
{
  Widget draw_id;
  Cardinal numberChildren;
  WidgetList pchildren;

  if((draw_id= widget_id("drawwindow")) == FALSE)
  {
     printf("ERROR:cannot find drawwindow\n");
  }
  XtVaGetValues( draw_id,
        XmNnumChildren, &numberChildren,
        XmNchildren, &pchildren,
        NULL);
  while(numberChildren > 0 ) {
    GraphElement *pge;
    XtPointer pudata;
    Widget wid;
    wid = (Widget)pchildren[numberChildren-1];
    XtVaGetValues( wid,
        XmNuserData, &pudata,
        NULL);
    pge = (GraphElement *) pudata;
    printGraphElement(pge);
    numberChildren--;
  }
  return;
}


void switchEdge(  GraphElement *pedge,
		  GraphElement *poldvertex,
		  GraphElement *pnewvertex )
/***************************************************************************\
*	purpose:  to transfer the end pt of an edge from one vertex
*		  to a new vertex.
*
*	Author:	 Dan Clark/Bill Rogers		Nov 1992
*
\***************************************************************************/
{
  Position curx1, cury1, curx2, cury2;
  EdgePoint edgept;
  EdgePoint *pedgept = &edgept;
  EdgeBox edgebox;
  EdgeBox *pedgebox = &edgebox;
  DB_STAT stat_link;
  GraphElement *pgraphnode1;
  GraphElement *pgraphnode2;
  Dimension zoom = (Dimension)(zoom_factor * (double)100.0);

  if(GraphDisplayOn != pedge->display ) {
    printf("ERROR: this edge is not currently displayed\n");
    return;
  }
  if(NULL == pedge->wid) {
    printf("ERROR: this edge has bad wid\n");
    return;
  }

#ifdef NOWAY
  deleteGraphLink   ( pedge, poldvertex);
  addGraphLink      ( pedge, pnewvertex );
  addGraphLink      ( pnewvertex, pedge );
#endif
  replaceGraphLink (pedge, poldvertex, pnewvertex);

  stat_link = findEdgeVertexes( pedge, &pgraphnode1, &pgraphnode2 );
  if ( stat_link == D_NOTFOUND ) {
     printf("*** ERROR *** from switchEdge\n" );
     printf("    Did not find two valid vertexes\n" );
     return;
  }

  pedgept->x1 = (int)pgraphnode1->x;
  pedgept->y1 = (int)pgraphnode1->y;
  pedgept->x2 = (int)pgraphnode2->x;
  pedgept->y2 = (int)pgraphnode2->y;

  line2rect( pedgept, pedgebox );

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


DB_STAT farendGraphVertex ( GraphElement *vertex, 
			    GraphElement *edge,
			    GraphElement **farvertex )
/**************************************************************************\
*	purpose: Given a vertex and an edge from this vertex,
*		 find the vertex on opposite end of edge.
*
*	Author: Bill Rogers/Dan Clark 	Nov 5, 1992
*
*
\**************************************************************************/
{
  GraphElement *pvertex;
  GraphLink    *rec;

  DB_STAT stat;
  /* ptr to vertex rec */ 
  stat = ll_first( &graph_db, &edge, &rec, GRAPH_VERTEX_LINK );
  if(D_FOUND != stat) {
    printf("ERROR: farendgraphvertex\n");
    *farvertex = NULL;
    return;
  }

  while( stat == D_FOUND )
  {
    if( rec->graphnode != vertex )
    {
      *farvertex = rec->graphnode;
      break;			/* found it, exit routine */
    }
    stat = ll_next( &rec, &rec );    /* get other vertex */
  }
  return( stat );
}


int changeVertexName(GraphElement *pvtxname, GraphElement *pvtx)
/**************************************************************************\
*
\**************************************************************************/
{
  GraphElement graphnode;
  GraphElement *pgraphnode;
  DB_STAT stat;
  char out[50];
  char *pout = &out[0];
  XmString x_str;


#ifdef LATER
    if( (GraphTypeVertex != pvtxname->type) ||
      (GraphClassVertexName != pvtxname->class) ||
      (GraphDisplayOn != pvtxname->display) ) {
      printf("ERROR: graph element is not a name vertex\n");
      return -1;
    }
#endif
    pgraphnode = &graphnode;
    memcpy(pgraphnode, pvtxname, sizeof(GraphElement));
    stat = db_update(&graph_db, &pvtxname, &pgraphnode);
    if(D_OKAY != stat) {
      printf("ERROR: graph db not update with new name node (%s)\n", 
              pvtx->vertex_id);
      return -1;
    }

    /** found a name vertex & the name is on the screen */
    sprintf(pout, "%12.12s", &pvtx->vertex_id[0]);
    x_str =  XmStringCreateLtoR(pout, XmSTRING_DEFAULT_CHARSET);
    XtVaSetValues( pvtxname->wid, XmNlabelString, x_str, NULL);
    XmStringFree(x_str);
    return 0;
}


GraphElement *prmelement;
int deleteGraphElement(GraphElement *pdelelement) 
/*******************************\
*
* author: Dan Clark
* this removes the element from the data base checking to see if
* there are any links to other elements, and removes these links.
* warning: this will remove the element
* but if any one way links are pointing to this element 
* they will become invalid.
* it is imperative that the programmer remove all links before calling
* this routine.
* NOTE: 
* this routine should SAVE information about what has been deleted
* so that when the coord file is written out the data is removed
* from the file!
******************************************************************************/
{
  DB_STAT stat;
  DB_STAT statdel;
  int cnt;
  
  /*** check for a valid db **/
  if(db_valid_db(&graph_db) != D_OKAY) {
    printf("ERROR: deleteGraphElement - no graph table\n");
    return;
  }
  prmelement = pdelelement;

  /*** check for links BACK to this element and remove ***/
  cnt = forEachLink(graph_db, prmelement, GRAPH_EDGE_LINK, 
                    rmBackLink, (char *)prmelement);
  cnt = forEachLink(graph_db, prmelement, GRAPH_VERTEX_LINK, 
                    rmBackLink, (char *)prmelement);

  /* finally remove the element itself from the db */

  if(pdelelement->display == GraphDisplayOn)
  {
    if(XtIsManaged(pdelelement->wid))
    {
      XtUnmanageChild(pdelelement->wid);
      XtDestroyWidget(pdelelement->wid);
    }
    else
    {
      printf("Warning - deleteGraphElement (graphdata.c)\n");
      printf("	        Following graphElement widget not managed:\n");
      printGraphElement(pdelelement);
    }
  }
  statdel = db_delete(&graph_db, &pdelelement);
  assert(statdel == D_OKAY);
  return statdel;
}


void deleteGraphElementByClass( GraphClass class ) 
/****************************************************************************\
* Purpose:  Delete by CLASS (like all edge or border vertexes)
*
* author: Bill Rogers
\***************************************************************************/
{
  GraphElement ge;
  GraphElement *pge = &ge;
  GraphElement *pnext;

  DB_STAT stat;

  stat = db_first( &graph_db, &pnext, GRAPH_CLASS_IDX );

  while ( stat != D_EOF )
  {
    if( class == pnext->class )
    {
      deleteGraphElement( pnext ); 
    }

    pge = pnext;
    stat = db_next( &graph_db, &pge, &pnext, GRAPH_CLASS_IDX );
  }

}


int deleteGraphComplexEdge(GraphElement *prmedge)
/**************************************************************************\
* NOTE:
*  this routine is inefficient.
*  it should walk the links in order and delete the
*  sections and bends as it goes!
*
\**************************************************************************/
{
  DB_STAT stat;
  int rtncde;
  GraphElement *psubedge;
  GraphLink    *llenxt;
  GraphLink    *lleret;
  DB_STAT  estat;
  GraphElement *bends[PSMAXBENDS + 2];
  int bendcnt;
  int cnt;

    if( GraphClassEdgeComplexSection != prmedge->class ) {
      printf("Warning: complex edge not passed to delete edge function\n");
      return -1;
    }

    /*** find all the bending points ***/
    bendcnt =  createBendArray(bends, prmedge, GraphTypeVertex );

    /*** process this edge and all subedges ***/
    estat = ll_first( &graph_db,
                   &prmedge,
                   &lleret,           /* ptr to 1st edge rec */
                   GRAPH_EDGE_LINK );
    while(D_FOUND == estat) {
      estat = ll_next( &lleret, &llenxt);
      psubedge = lleret->graphnode;
      deleteGraphElement(psubedge);
      lleret = llenxt;
    }
    deleteGraphElement(prmedge);

    /** now delete the bends ***/
    if(bendcnt-2 > 0) {
      /** only save lines if they are routed **/
      for(cnt = 0; cnt < bendcnt-2; cnt++) {
       deleteGraphElement(bends[cnt+1]);
      }
    }

}


int deleteEdgebyName( char *type, char *name1base1, char *name2base2)
/**************************************************************************\
*
\**************************************************************************/
{
  PFBRANCH branch;
  DB_STAT stat;
  DB_STAT stat2;
  int rtncde;
  char checkbuf[128];
  GraphElement *pedge;
  GraphElement *pcomplexedge;

  memset(branch.type, ' ', sizeof(PFBRANCH));
  strncpy(branch.type, type, sizeof(branch.type));
  strncpy(branch.name1, name1base1, sizeof(branch.name1)+sizeof(branch.base1) );
  strncpy(branch.name2, name2base2, sizeof(branch.name2)+sizeof(branch.base2) );
  strncpy(branch.circuit, "*", sizeof(branch.circuit) );
  
  rtncde = pfcheck_cb( branch.type );
 
  if(TRUE != rtncde ) {
    /* powerflow has no more edges by this name...remove from graph */
    stat = findGraphEdgebyName2( name1base1, name2base2, &pedge );

    if( stat == D_FOUND ) {
      stat2 = findComplexEdge(&pcomplexedge, pedge);
      if(D_FOUND == stat2) {
        deleteGraphComplexEdge( pcomplexedge );
      } else {
        printf("deleteEdgebyName: Programmer Warning - add complex edge!\n");
      }
    }
  }

  return rtncde;
 
}


int deleteBusVertex(GraphElement *pvtx)
/*************************************************************************\
* purpose: delete a bus vertex, all subvertices and subedges
*
* author: Dan Clark
\*************************************************************************/
{
  GraphElement *psubnode;
  GraphLink    *llretrec;
  GraphLink    *llnxtrec;
  DB_STAT  stat;
  DB_STAT  statdel;

  /* ptr to 1st sub edge rec */
  stat = ll_first( &graph_db, &pvtx, &llretrec, GRAPH_EDGE_LINK ); 

  while(D_FOUND == stat)	/* loop to delete all branches */
  {
    stat = ll_next( &llretrec, &llnxtrec );
    psubnode = llretrec->graphnode;

    if(GraphClassEdgeComplexSection == psubnode->class ) {
       deleteGraphComplexEdge(psubnode);
    }
      /*** deleting the complex edge gets rid of it and all subedges 
      ** but the link should still need to be deleted
      ** causes a crash on OSF V2.0 so it is removed 
      *   statdel = ll_delete(&llretrec);
      *   assert(statdel == D_OKAY);
      ***/
    llretrec = llnxtrec;
  }

  /* ptr to 1st sub vertex rec */
  stat = ll_first( &graph_db, &pvtx, &llretrec, GRAPH_VERTEX_LINK );

  while(D_FOUND == stat)	/* loop to delete name vertx */
  {
    stat = ll_next( &llretrec, &llnxtrec );
    psubnode = llretrec->graphnode;
    deleteGraphElement(psubnode);
    statdel = ll_delete(&llretrec);
    assert(statdel == D_OKAY);
    llretrec = llnxtrec;
  }

  deleteGraphElement(pvtx);	/* finally, delete bus vertex itself */

  return(0);
}

int deleteVertexbyName(char *namebase)
/**************************************************************************\
*
* Called by: sect_ok (bussect.c)
*	     linetap (linesect.c) (2)
*	     send_del_data_to_powerflow (selection.c) (presently inactive)
\**************************************************************************/
{
  DB_STAT stat;
  GraphElement *pvtx;
  int rtncde;

    stat = findBusGraphVertexRec( namebase, &pvtx );
    if(D_FOUND != stat) {
      printf("WARNING: cannot delete graph vertex bus (%s)\n", namebase);
      return -1;
    }
    rtncde = deleteBusVertex(pvtx);
  return rtncde;
}

int deleteGraphLink(GraphElement *pe1, GraphElement *pe2) 
/*****************************************************************************\
*
* author: Dan Clark
* purpose search for a links between to vertices and remove them
*
******************************************************************************/
{
  int cnt;

  /*** check for a valid db **/
  if(db_valid_db(&graph_db) != D_OKAY) {
    printf("ERROR: deleteGraphLink - no graph table\n");
    return;
  }

  /** search and remove links from 1 to 2 ***/
  prmelement = (GraphElement *)pe2;
  if(pe2->type == GraphTypeVertex) 
    cnt = forEachLink(graph_db, pe1, GRAPH_VERTEX_LINK, rmLink, (char *)pe1);
  else
    cnt = forEachLink(graph_db, pe1, GRAPH_EDGE_LINK, rmLink, (char *)pe1);

  /** search and remove links from 2 to 1 ***/
  prmelement = pe1;
  if(pe1->type == GraphTypeVertex)
    cnt = forEachLink(graph_db, pe2, GRAPH_VERTEX_LINK, rmLink, (char *)pe2);
  else
    cnt = forEachLink(graph_db, pe2, GRAPH_EDGE_LINK, rmLink, (char *)pe2);
}

int replaceGraphLink(GraphElement *pe, GraphElement *pold, GraphElement *pnew) 
/*****************************************************************************\
*
* author: Dan Clark
* look for the links between pe and pold
* remove both links from pe to pold and pold to pe.
* replace with links from pe to pnew
*
******************************************************************************/
{
  int cnt;
  DB_STAT stat_link;
  long key;
  GraphElement *pgraphrec;
  GraphElement *linkgraphrec;
  GraphLink    *llretrec;
  GraphLink    *llnxtrec;
  GraphLink    *llretrechold;

  /*** check for a valid db **/
  if(db_valid_db(&graph_db) != D_OKAY) {
    printf("ERROR: replaceGraphLink - no graph table\n");
    return;
  }
  if(pold->type == GraphTypeVertex) 
    key = GRAPH_VERTEX_LINK;
  else
    key = GRAPH_EDGE_LINK;

  /** search for links from pe to pold ***/
  stat_link = ll_first(&graph_db, &pe, &llretrec, key);
  if (stat_link != D_FOUND) {
      /* printf("NOTE: forEachLink: no links\n"); */
  } 
  while(stat_link == D_FOUND) {
    linkgraphrec = llretrec->graphnode;
    /** fetch the next record in case the function deletes the link **/
    if(pold == linkgraphrec) {
      llretrec->graphnode = pnew;
    }
    stat_link = ll_next(&llretrec, &llretrechold);
    llretrec = llretrechold;
  }

  /** add links from pnew back to pe **/
  addGraphLink (pnew, pe); 

  /** search and remove links from pold back to pe ***/
  prmelement = pe;
  if(pe->type == GraphTypeVertex)
   cnt = forEachLink(graph_db, pold, GRAPH_VERTEX_LINK, rmLink, (char *)pold);
  else
   cnt = forEachLink(graph_db, pold, GRAPH_EDGE_LINK, rmLink, (char *)pold);

} /* end replaceGraphLink */



int addGraphLink(GraphElement *pe1, GraphElement *pe2) 
/****************************************************************************\
* note:
* no error checking for an existing link between these two elements
* this sould write out an ADD record into the change database so that
* the coord file can be updated
*
* Author: Dan Clark
*****************************************************************************/
{
  DB_STAT stat_link;
  GraphLink *pgraphlink;

  /*** check for a valid db **/
  if(db_valid_db(&graph_db) != D_OKAY) {
    printf("ERROR: addLink - no graph table\n");
    return;
  }

      if(pe2->type == GraphTypeVertex)
      {
        stat_link = ll_insert(&graph_db, &pe1,  &pgraphlink,
                 GRAPH_VERTEX_LINK, LL_ORDER_FIRST);
        assert(stat_link == D_OKAY);
        pgraphlink->graphnode = pe2;
      }

      else if(pe2->type == GraphTypeEdge )
      {
        stat_link = ll_insert(&graph_db, &pe1,  &pgraphlink,
                 GRAPH_EDGE_LINK, LL_ORDER_FIRST);
        assert(stat_link == D_OKAY);
        pgraphlink->graphnode = pe2;
      }
      
      else
      {
         printf("*** WARNING *** from addGraphLink\n" );
	 printf("          2nd graph element must be an edge or vertex\n" );
	 printf("          Attempt to link between\n");
         printGraphElement(pe1);
         printGraphElement(pe2);
      }
  
} /* end addGraphLink */

#ifdef PROTOTYPE_PROBLEM
int forEachLink(IPFCONST DBID db, GraphElement *pgraphrec, IPFCONST long key,
  int (*do_fnct)(), char *userdata)
#else
int forEachLink(IPFCONST DBID db, GraphElement *pgraphrec, IPFCONST long key,
  int (*do_fnct)( GraphElement *pchild, GraphElement *pparent, GraphLink *plink,  char *do_userdata),
  char *userdata)
#endif
/************************************************************************\
* 
\************************************************************************/
{
  GraphLink *llretrec;			/* ptr to graph link node */
  GraphLink *llretrechold;		/* ptr to graph link node */
  GraphElement *linkgraphrec;		/* ptr to vertex link record */
  int retcde;
  int cnt;
  DB_STAT stat_link;
      
   stat_link = ll_first(&db, &pgraphrec, &llretrec, key);

  if (stat_link != D_FOUND) {
      /* printf("NOTE: forEachLink: no links\n"); */
  } 

  while(stat_link == D_FOUND) {
    linkgraphrec = llretrec->graphnode;
    /** fetch the next record in case the function deletes the link **/
    stat_link = ll_next(&llretrec, &llretrechold);
    retcde = (*do_fnct)(linkgraphrec, pgraphrec, llretrec, userdata);
    cnt++;
#ifdef LATER
      if(retcde != 0) {
        break;
      }
#endif
    llretrec = llretrechold;
  }
  return cnt;
}


int rmLink(GraphElement *pchild, GraphElement *pparent, GraphLink *plink,
 char *userdata)
/***************************************************************************\
* author: Dan Clark
\***************************************************************************/
{
  DB_STAT statdel;

  if(pchild == prmelement) {
    statdel = ll_delete(&plink);
    assert(statdel == D_OKAY);
  }

  return(0);
}


int rmBackLink(GraphElement *pchild, GraphElement *pparent, GraphLink *plink,
char *userd)
/*****************************************************************************\
*
\*****************************************************************************/
{
  DB_STAT statdel;
  int cnt;

  /*** chase down the backward link ***/
  if(prmelement->type == GraphTypeVertex) {
    cnt = forEachLink(graph_db, pchild, GRAPH_VERTEX_LINK, 
                        rmLink, (char *)pchild);
  }
  else
  {
    cnt = forEachLink(graph_db, pchild, GRAPH_EDGE_LINK,
			rmLink, (char *)pchild);
  }
  /*** now remove the forward link ***/
  statdel = ll_delete(&plink);
  assert(statdel == D_OKAY);

  return(0);
} 

int changeGraphBusVertex(char *oldnamebase, char *namebase)
/**************************************************************************\
*
\**************************************************************************/
{
  DB_STAT stat;
  GraphElement *pvtx;
  GraphElement *pvtxname;
  GraphElement graphnode;
  GraphElement *pgraphnode;
  int rtncde;
    /* first fine the graph vertex for the bus */
    stat = findBusGraphVertexRec( oldnamebase, &pvtx );
    if(D_FOUND != stat) {
      printf("WARNING: cannot find graph vertex for bus (%s)\n", namebase);
      return -1;
    }
    /* now change the name of the graph vertex in the data base */
    pgraphnode = &graphnode;
    memcpy(pgraphnode, pvtx, sizeof(GraphElement));
    strncpy(pgraphnode->vertex_id, namebase, sizeof(pgraphnode->vertex_id) );
    stat = db_update(&graph_db, &pvtx, &pgraphnode);
    if(D_OKAY != stat) {
      printf("ERROR: graph database not update with new name (%s)\n", namebase);
      return -1;
    }
    /* find name vertex and change the screen */
    findVertxLinkedToVertx(&pvtxname, pvtx, GraphClassVertexName);
    
    if ( pvtxname )
    {
      changeVertexName( pvtxname, pvtx );
    }
    else
    {
      printf("ERROR: no name found\n"); 
      return -1;
    }

}



int turnoffGraphElement(GraphElement *pelement)
/**************************************************************************
* author: Dan Clark
*
\**************************************************************************/
{
  GraphElement graphelement;
  GraphElement *pgraphelement = &graphelement;
  DB_STAT stat;

      if(pelement->display != GraphDisplayOn) {
         printf("Warning: trying to turn off element which is not on\n");
         return;
      }
      /** destroy the gadget **/
      if(pelement->wid != NULL) {
        if(XtIsManaged(pelement->wid)) {
          XtUnmanageChild(pelement->wid);
          XtDestroyWidget(pelement->wid);
        } else {
          printf("Warning - turnoffGraphElement display flag on but wid not managed:\n");
          printGraphElement(pelement);
        }
      } else {
        printf("Warning - turnoffGraphElement display flag on bad wid id:\n");
        printGraphElement(pelement);
      }

      /** update display flag to off for previous vertex **/
      pgraphelement = &graphelement;
      memcpy(pgraphelement,pelement, sizeof(GraphElement));
      pgraphelement->display = GraphDisplayOff;
      pgraphelement->wid = NULL;
      stat = db_update(&graph_db, &pelement, &pgraphelement);
      if(stat != D_OKAY) {
        printf("ERROR: turnoffGraphElement: update failed stat = %d\n", stat);
      }
}


int turnoffGraphComplexEdge(GraphElement *poffedge)
/**************************************************************************\
*
\**************************************************************************/
{
  DB_STAT stat;
  int rtncde;
  GraphElement *psubedge;
  GraphLink    *llenxt;
  GraphLink    *lleret;
  DB_STAT  estat;

    if( GraphClassEdgeComplexSection != poffedge->class ) {
      printf("Warning: complex edge not passed to delete edge function\n");
      return -1;
    }

    /*** process this edge and all subedges ***/
    estat = ll_first( &graph_db,
                   &poffedge,
                   &lleret,           /* ptr to 1st edge rec */
                   GRAPH_EDGE_LINK );
    while(D_FOUND == estat) {
      estat = ll_next( &lleret, &llenxt);
      psubedge = lleret->graphnode;
      turnoffGraphElement(psubedge);
      lleret = llenxt;
    }
    turnoffGraphElement(poffedge);

}


int turnoffGraphVertex(GraphElement *pvtx)
/**************************************************************************\
*
\**************************************************************************/
{
  GraphElement *pedge;
  GraphElement *psubedge;
  GraphLink    *llretrec;
  GraphLink    *llnxtrec;
  GraphLink    *llenxt;
  GraphLink    *lleret;
  DB_STAT  stat;
  DB_STAT  estat;

  stat = ll_first( &graph_db,
                   &pvtx,
                   &llretrec,           /* ptr to 1st edge rec */
                   GRAPH_EDGE_LINK );
  while(D_FOUND == stat) {
    stat = ll_next( &llretrec, &llnxtrec );
    pedge = llretrec->graphnode;

    if(GraphClassEdgeComplexSection == pedge->class ) {
       turnoffGraphComplexEdge(pedge);
    }
    llretrec = llnxtrec;
  }
  stat = ll_first( &graph_db,
                   &pvtx,
                   &llretrec,           /* ptr to 1st edge rec */
                   GRAPH_VERTEX_LINK );
  while(D_FOUND == stat) {
    stat = ll_next( &llretrec, &llnxtrec );
    pedge = llretrec->graphnode;
    turnoffGraphElement(pedge);
    llretrec = llnxtrec;
  }
  turnoffGraphElement(pvtx);
  return 0;
}


int turnoffEdgebyName( char *type, char *name1base1, char *name2base2)
/**************************************************************************\
*
\**************************************************************************/
{
  PFBRANCH branch;
  DB_STAT stat;
  DB_STAT stat2;
  int rtncde;
  char checkbuf[128];
  GraphElement *pedge;
  GraphElement *pcomplexedge;

  memset(branch.type, ' ', sizeof(PFBRANCH));
  strncpy(branch.type, type, sizeof(branch.type));
  strncpy(branch.name1, name1base1, sizeof(branch.name1)+sizeof(branch.base1) );
  strncpy(branch.name2, name2base2, sizeof(branch.name2)+sizeof(branch.base2) );
  strncpy(branch.circuit, "*", sizeof(branch.circuit) );
  
  rtncde = pfcheck_cb( branch.type );
 
  if(TRUE != rtncde ) {
    /* powerflow has no more edges by this name...remove from graph */
    stat = findGraphEdgebyName( name1base1, name2base2, &pedge );

    if( stat == D_FOUND ) {
      stat2 = findComplexEdge(&pcomplexedge, pedge);
      if(D_FOUND == stat2) {
        turnoffGraphComplexEdge( pcomplexedge );
      } else {
        printf("deleteEdgebyName: Programmer Warning - add complex edge!\n");
      }
    }
  }

  return rtncde;
 
}


int turnoffVertexbyName(char *namebase)
/**************************************************************************\
*
\**************************************************************************/
{
  DB_STAT stat;
  GraphElement *pvtx;
  int rtncde;

    stat = findBusGraphVertexRec( namebase, &pvtx );
    if(D_FOUND != stat) {
      printf("WARNING: cannot find graph vertex for bus (%s)\n", namebase);
      return -1;
    }
    rtncde = turnoffGraphVertex(pvtx);
    return 0;
}



int checkLineOn(GraphElement *pgraphrec)
/************************************************************************\
* check if a line is on for this parent edge
* must pass in a parent edge
\************************************************************************/
{
  GraphLink *llretrec;			/* ptr to graph link node */
  GraphLink *llretrechold;		/* ptr to graph link node */
  GraphElement *linkgraphrec;		/* ptr to vertex link record */
  int retcde;
  int cnt;
  DB_STAT stat_link;
      
  cnt = 0;
  stat_link = ll_first(&graph_db, &pgraphrec, &llretrec, GRAPH_EDGE_LINK);
  while(stat_link == D_FOUND) {
    linkgraphrec = llretrec->graphnode;
    /** fetch the next record in case the function deletes the link **/
    stat_link = ll_next(&llretrec, &llretrechold);
    if(GraphDisplayOn == linkgraphrec->display) {
      cnt++;
    }

    llretrec = llretrechold;
  }
  return cnt;
}


void tools_set_view_mode_cb(Widget w, int *tag, XmAnyCallbackStruct *reason)
/*************************************************************************\
* Purpose:  Clears the main window and rebuilds either CoordOnly or
*		CoordAndBase merge
*
*  Author: John Rutis         5 Oct 92
* modified: Dan Clark		(Bill Rogers 4/13/95)
*
* Called by: VIEW/MERGE SCHEME cascade pulldown on main menu bar.
\*************************************************************************/
{
  Widget draw_id;
  int loctag;

  loctag = (int)*tag;

  if((draw_id = widget_id("drawwindow")) == FALSE)
  {
     printf("tools_set_view_mode_cb:cannot optimize draw/redraw\n");
  } else {
     /* XtUnmanageChild(draw_area); remove for optimization */
  }
 
  destroyAllGadgets ( 0, 0, 0 );	/* but not GraphElements */

  buildMainDisplay( loctag );

}


void buildMainDisplay( int mergeFlag )
/***************************************************************************\
* Purpose: Checks mergeFlg, calls proper routines for Coord or Merge display
* 	   Builds the main display of buses, lines, comments, (EVERYTHING!)
*
* Author: William E. Rogers 
*
* Called by: apply_files(1)	    (filedlgrtn.c)
*	     tools_set_view_mode_cb (above)     
\***************************************************************************/
{
  Widget wid;
  int view_solve_on;
  int ostates;

  if( mergeFlag != CoordOnly && mergeFlag != CoordAndBase )
  {
    printf("** WARNING ** from buildMainDisplay (filedlgrtn.c)\n" );
    printf("      Incorrect param, must be CoordAndBase -or- CoordOnly\n" );
  }

  /*** check for a valid db **/
  if(db_valid_db(&graph_db) != D_OKAY) {
    printf("ERROR: buildMainDisplay (filedlgrtn.c) - no graph table\n");
    return;

  }

  wid = widget_id("view_menu_solution_pd");
  if(wid != NULL)
    view_solve_on = XmToggleButtonGadgetGetState(wid);
  else
    view_solve_on = True;

  switch ( mergeFlag )
  {
    case CoordOnly:      /* just display the coord file i.e. no base case */
      graphCorOn          (0,0,0); /* everything in cor file must be made */
				   /* visible  (graphdata.c) */
      createVertexGadgets (0,0,0); /* (vertex.c )   */
      createEdgeGadgets	  (0,0,0); /* (edge.c   )   */
      psDrawOptions ();

            /* TODO should toggle the button so it says coord mode */
    break;

    case CoordAndBase:
    default:
      mergeCoordBase();
      psDrawOptions ();

      ipfstate_get(&ostates);	/* from ipf_cb.c */
				/* get 'state' of pf solution */

      if((ostates >= 5) && view_solve_on == True)
                   refresh_comments();
  }

}


DB_STAT firstGE( GraphElement **ptr, GraphType gr_type )
/***************************************************************************\
* Purpose: find the first VERTEX or EDGE graphelement
*	   ( used to start a search loop thru the graphelements )
*
*
*
\***************************************************************************/
{
  GraphElement ge;
  GraphElement *pKey;
  DB_STAT stat;

  int type;

  pKey = &ge;
  /* memset( pKey, ' ', sizeof( pKey ) ); */
  pKey->display = GraphDisplayOn;

  switch ( gr_type )
  {
    case GraphTypeVertex:
    case GraphTypeEdge:
	type       = GRAPH_DISPLAY_TYPE;
	pKey->type = gr_type;
    break;

    default:
	type	   = GRAPH_DISPLAY;
 
  }

  stat = db_search(&graph_db, &pKey, ptr, type );
 
  return stat;

}

DB_STAT nextGE( GraphElement **ptr, int type )
/***************************************************************************\
* Purpose: find the next VERTEX or EDGE GraphElement
*	   ( used to continue a search loop thru the graphelements )
*
*
* Called by:
*    destroyAllGadgets	   [loop to find ge to destroy]		 (this module)
*    createEdgeGadgets,	   [loop to find edge to create],	 (edge.c)
*    findgraphedge	   [loop to find graph edge]		 (edge.c)
*    group_all_objects	   [loop to find inbounds vertexes]	 (toolbox.c)
*    change_bus_name_label [loop to set bus name/voltage style ] (graphpscor.c)
*    clear_solution_data,  [loop to clear voltage from bus name]  (pf_cb.c)
*    solve_wp,		   [loop to add voltage to bus name]	  (pf_cb.c)
*    createVertexGadgets   [loop to create all coord bus gadgets] (vertex.c)
*
\***************************************************************************/
{
  GraphElement *pge;
  DB_STAT stat;

  pge = *ptr;

  stat = db_next(&graph_db, &pge, ptr, type );

  return stat;
}


void printAllBusesAndBranches()
{
  GraphElement graphnode;
  GraphElement *pgraphnode = &graphnode;
  GraphElement *pgraphnodenxt;
  GraphElement *linkgraphrec;
  GraphLink *llretrec;

  DB_STAT stat, stat_link;

  printf("\nLIST OF BUS/LINES GRAPHELEMENTS....(printAllBusesAndBranches)\n" );

  pgraphnode = &graphnode;
  stat = db_first ( &graph_db, &pgraphnodenxt, GRAPH_TYPE_VERTEX_ID);

  /** Now loop through all the records **/
  while(stat == D_FOUND)
  {
    printf("\n");
    printGraphElement(pgraphnodenxt);

    /** now follow all vertex links of the current record **/
    stat_link = ll_first ( &graph_db, &pgraphnodenxt,
		           &llretrec, GRAPH_VERTEX_LINK);

    if(stat_link != D_FOUND) printf("  Note: no vertex links\n");

    while(stat_link == D_FOUND)
    {
      linkgraphrec = llretrec->graphnode;

      /** Print the vertex record **/
      printf("  VL ~");

      printGraphElement(linkgraphrec);

      stat_link = ll_next( &llretrec, &llretrec );
    }

    /** now follow all edge links of the current record **/
    stat_link = ll_first ( &graph_db, &pgraphnodenxt, &llretrec,
			 GRAPH_EDGE_LINK );

    if(stat_link != D_FOUND) { printf("  Note: no edge links\n"); }

    while(stat_link == D_FOUND)
    {
      linkgraphrec = llretrec->graphnode;

      /** Print the edge record **/
      printf("  EL ~");

      printGraphElement(linkgraphrec);

      stat_link = ll_next( &llretrec, &llretrec );
    }

    /** Get next record **/
    pgraphnode = pgraphnodenxt;
    stat = db_next ( &graph_db, &pgraphnode, &pgraphnodenxt,
			 GRAPH_TYPE_VERTEX_ID );
  }
  printf("\nEnd List Graph Data BUS and LINE data \n\n");

}
