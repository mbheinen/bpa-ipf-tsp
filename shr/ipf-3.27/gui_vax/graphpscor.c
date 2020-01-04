static char sccsid[]="@(#)graphpscor.c	20.108 7/6/98";
/**********************************************************************\
*		
* Author: Dan Clark/Bill Rogers     Dec, 1991    Modified:
*
\**********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <Xm/Xm.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/Text.h>
#include "dmgr.h"
#include "coord_data.h"
#include "graph_data.h"
#include "ipfdebug.h"	/* use DB_GraphPSCorMask */
#include "ipf_ipc.h"
#include "define.h"
#include "EdgeGP.h"

#include "em.h"
em_init

#define max(A,B) ((A) > (B) ? (A) : (B))
#define min(A,B) ((A) > (B) ? (B) : (A))
#define MAX_BENDS 5
#define PixelsPerCm 39.37	/* assuming 100 pixels per screen inch */
#define PixelsPerInch 100.
#define CM_2_PIXEL( n )         ( int  ) n*PixelsPerCm

#define PAPER_EDGE	0
#define DRAW_EDGE	1
#define LABEL_BOX	2
#define BORDER		3
#define LEGEND_BOX	4
#define COMMENT		5

#define STANDARD_BORDER	.5
#define BOTTOM_BORDER	.7
#define LEGEND_WIDTH	2.2
#define LEGEND_HEIGHT	4.5
#define VERT_GAP	0.0

#define ABS		0
#define REL		1

#define SOLID		False
#define DASH		True
#define BUS_RAD	0.127

static double pixelsPerUnit = PixelsPerCm; /* hardwired for now */
static int    plot_pixels_ht;		/* max height (in pixels) */
static int    plot_pixels_wd;		/* max width  (in pixels) */
static double x_wd;
static double y_ht;
static double xscale;
static double yscale;
static double x_cm_offset;
static double y_cm_offset;
static double paper_x_offset;
static double paper_y_offset;
static int paperOffsetY = 0;
static int paperOffsetX = 0;
static double min_x;
static double max_y;
static double min_y;
static double adj = 0.2;		/* error adjustment value */
static char units[8]= "Cm";
static int expose_occured = False;
/* following is being disabled for now */
static Boolean force_vertexes_inside_paper_border = False;
static Boolean shift_paper_as_needed = True;
static char   origin	= 'B';
static Boolean scaling_flg = True;
static int    paper_border   = 0;	/* (optional) value to keep plot  *
					 * points away from edge of paper */
static double 	old_xscale, old_yscale;
static double 	old_x_cm_offset,  old_y_cm_offset;
static int	old_paperOffsetX, old_paperOffsetY;
static double 	old_paper_ht;

static double 	new_xscale, new_yscale;
static double 	new_x_cm_offset, new_y_cm_offset;
static int	new_paperOffsetX, new_paperOffsetY;
static double 	new_paper_ht;

static char     outbuf_array[BUFSIZE];
static char     inbuf_array [BUFSIZE];

extern double zoom_factor;	/* from (toolbox.c) */
extern long graph_idx;

extern Widget widget_id		( char * );
extern double atof_cnt		( char *, int );
extern double atof_cnt_zero	( char *, int );
extern void   breakEdge		( GraphElement *pedge,int x, int y );
extern void   get_paper_size	( double *paper_wd, double *paper_ht,
				  double *xscale,  double *yscale,
				  double *x_cm_offset, double *y_cm_offset );
extern char *add_decimal	( char *field, int dec_pt, int max_len, char *outstr );
extern int   manageVertex	( GraphElement *pgraphvertexnxt );
extern int   manageEdge         ( GraphElement *pgraphedgenxt);
extern int   setEdgeDash	( GraphElement *pedge, Boolean dashval);
extern void lookup_and_get_field( char *, char *, int );
extern void clip_trailing_blanks( char *string );
extern int psbendCount		(PSBranchRecord *rec );
extern void clip_trailing_blanks( char * );
extern int ipc_synch_rw		( char *, char *);
extern void display_bus_coord_data  ( char *busname );
extern void display_coord_data	( GraphElement *vertex );
extern void position_gadget_wrt ( GraphElement *vertex );
extern void updateEdge		( GraphElement *pedge );
void drawChars			( Display *dpy, Window wid, char *string,
				 int x, int y, double size, double angle );
extern char *cstring_to_string  ( XmString motif_string );
extern int add_define_cards_to_str (char *pf_send_rec);
extern void clip_outside_blanks	( char * );
extern void  testdraw		();
extern void create_from_scratch	( Widget, XtPointer, XmAnyCallbackStruct * );
extern void set_button_state	( char *, Boolean );
extern void change_cursor_to	( Widget, int *, XtPointer );
extern void create_new_ipf_bus	( Widget, XtPointer, XtPointer );
extern void send_add_data_to_powerflow(Widget,XtPointer, XmAnyCallbackStruct *);
extern void addGraphBusXY	( int, int );
extern void set_bus_type	( Widget, XtPointer, XmAnyCallbackStruct * );
extern void file_save_cb	( Widget, XtPointer, XmAnyCallbackStruct * );
extern void apply_files		( Widget, XtPointer, XmAnyCallbackStruct * );
extern void name_change_file_status_set(Widget widget, int *tag,
                                        XmAnyCallbackStruct *callback_data);
extern uscanc                   ( char *,  char *wrd[], int *n,
                                  char *lmt1, char *lmt2);
extern Widget VUIT_Manage	( char * );
extern void VUIT_Unmanage	( char *);
extern void lookup_and_fill_field( char *, char *, int, int );
extern void file_check_and_save_cb( Widget widget, int * tag,
                                     XmAnyCallbackStruct *callback_data);
extern int HashLookup		( char *name, Widget *id );
extern void fetch_widget	( char *wname );
extern GraphElement *createGraphBusXYXY	( char *name, int x, int y,
				int xn, int yn, char *gen_ang, char *cap_ang,
				int m_flg, int x_flg, int pf_flg,
				int g_flg, int r_flg );
extern int atoi_cnt			( char *str, int size );
extern void get_str_box_pixel_size	( char *str, int *wd, int *ht );
extern void set_standard_paper_size	();
extern void disable_pushbutton  ( char * );
extern void enable_pushbutton   ( char * );
extern void deleteGraphElementByClass  ( GraphClass class );



DB_STAT findBusGraphVertexRec 	( char *namebase, GraphElement **pe );
int    psgetHtWdScale		();
int    pscoord_to_MOTIF_x 	( char *x, int col, char *bus_name );
int    pscoord_to_MOTIF_x_flt	( double x,  char *bus_name );
int    pscoord_to_MOTIF_y 	( char *y, int col, char *bus_name );
int    pscoord_to_MOTIF_y_flt	( double y, char *bus_name );
int    pscoord_to_MOTIF_x_abs 	( char *x, int col );
int    pscoord_to_MOTIF_y_abs 	( char *y, int col );
double MOTIF_to_ps_x		( int x, char *, char *, char * );
double MOTIF_to_ps_y		( int y, char *, char *, char * );
void   pscreateGraphCorEdges 	( GraphElement *pgraphbusrec, char *namebase, 
				   PSBusRecord *coordnextbusrec,
				   PSBranchRecord *coordnextbranchrec );
void   pscreateGraphCorEdge  	( GraphElement *vertx_1, GraphElement *vertx_2, 
				   char *key,
				   GraphClass class,
				   int seg,  
				   GraphElement **new_rec );
void   psreverseCoordCard 	( PSBranchRecord *pcoordrev,
				  PSBranchRecord *pcoordorg );
void   psDrawOptions		();
int    processDrawCards		();
void   pscreatePaperEdges	();
void   pscreateBorderEdges	();
int    pscreateLabelBox		();
/*void   pscreateLegend		();*/
void   pscreate_plot_form 	();
void   pscreateCommentVertexes	();
void   make_vertex_invisible	( GraphElement *ge);
void   send_comments_and_defines_to_pf();

Boolean coord_db_not_valid	( char *caller, int flag );
void create_and_manage_coord_vertex( double x, double y,
			 GraphElement **ppnew_vertex, GraphClass, int );
void create_and_manage_scrn_vertex( char *name, int x, int y,
                 GraphElement **ppnew_vertex, GraphClass class );
void create_and_manage_coord_edge( GraphElement *vertex_1,
				   GraphElement *vertex_2,
				   GraphClass class,
				   Boolean dash_flg );
void   create_and_draw_box	( double, double, double, double, int,
				  GraphClass, GraphClass, Boolean);
GraphElement *pscreateStringVertex( double, double, char *, int );
double text_vertex_ht		( Widget wid );
void shift_paper		( int deltaX, int deltaY );
void psfindGraphMax		( );
void min_x_value_char		( char *cm, int adj );
void max_y_value_char		( char *cm, int adj );
void findGraphMax		( );
void change_bus_name_label	( Widget, XtPointer, XmAnyCallbackStruct *);
void set_graph_unit_and_origin_cb( Widget w, XtPointer tag,
                                 XmAnyCallbackStruct *cbs );
int rescale_graph		( double xftr, double yftr, int flag );

void test_graphdata		( Widget, XtPointer, XmAnyCallbackStruct * );
void test_pscoord_to_MOTIF_x	( );
void test_pscoord_to_MOTIF_x_1	( char *, double, double, int, double, int );
void test_pscoord_to_MOTIF_y	( );
void test_pscoord_to_MOTIF_y_1	( char *, double, double, int, double, int );
void get_box_size_of_string	( char *str, int *width, int *height );
void build_label_header		( char * );
void set_units_to_cm		();
void set_origin_to_bottom	();
void psUserCreateDrawVertex	( int x, int y, GraphElement ** );
void psUserEndDrawEdge		( int x, int y );
void userCreateComment		( XmPushButtonCallbackStruct * );
void set_expose_occured		();
void jiggle_all_graphelements	();
void jiggle_graphelement	( GraphElement *ge );
void set_new_values		();
void set_old_values		();
int x_flt_2_pxl			( double x );
int y_flt_2_pxl			( double y );


int psbuildGraphCoord ( Widget widget, caddr_t *tag, caddr_t *callback_data)
/****************************************************************************\
*
*	FUNCTION: Transfers pscor_db BUS data to graph_db.
*		  Creates all the bus and name vertexes.
*		  Creates all the branch edges.
*		  Establishes all the links.
*		  ( Begins the process of building the graph_db. )
*
*	Called by: apply_files (filedlgrtn.c)
*
\****************************************************************************/

{
  PSCoordFileRecord coordkeyvalrecord;	/* storage for db_search key values */

  PSBranchRecord *coordbranchvalrec;	/* ptr to key for branch search	    */
  PSBranchRecord *coordnextbranchrec;	/* ptr returned for next branch rec */
  PSBranchRecord *coordtmpbranchrec;	/* ptr returned for next branch rec */

  PSBusRecord	*coordbusvalrec;	/* ptr to key for bus search	    */
  PSBusRecord	*coordnextbusrec;	/* ptr returned for next bus rec    */

  GraphElement   graphrecord;		/* define structure graphrecord	    */
  GraphElement  *pgraphrecord = &graphrecord;	/* allocate memory	    */
  GraphElement  *pgraphbusrec;
  GraphElement   graphupdaterec;
  GraphElement  *pgraphupdaterec = &graphupdaterec;
  GraphElement	*pgraphnamerec;

  GraphLink *pgraphlink;

  DB_STAT statcoordbus;	/* status returned for bus queries of coord db	*/
  DB_STAT statbranch;	/* status returned for branch queries of db	*/
  DB_STAT stat;		/* status returned from graph db operations	*/

  int rtncmp;

  if( coord_db_not_valid( "psbuildGraphCoord", 1 ) ) return;
  graph_idx = 0;	/* initialize the index for new graph elements */

  set_units_to_cm(); 		/* hardwire to use only cm data */
  set_origin_to_bottom();

  /* ensure solution data toggle button is OFF (and prevent confusion) */
  set_button_state( "view_menu_solution_pd", False );

  /** set pointer to first Bus record of coord file **/
  statcoordbus = psfindfirstcoordbusrec ( &coordnextbusrec );

  if(statcoordbus == D_EOF) return;	/* ABORT - no buses in coord db_base */

  psfindGraphMax();

  while(statcoordbus == D_FOUND)	/* LOOP THUR BUSES */
  {

    pgraphbusrec = createGraphBusXYXY( coordnextbusrec->name,
	pscoord_to_MOTIF_x( coordnextbusrec->symbol.x, 6, coordnextbusrec->key),
	pscoord_to_MOTIF_y( coordnextbusrec->symbol.y, 6, coordnextbusrec->key),
    	pscoord_to_MOTIF_x(coordnextbusrec->namepos.x, 6, coordnextbusrec->key),
    	pscoord_to_MOTIF_y(coordnextbusrec->namepos.y, 6, coordnextbusrec->key),
	coordnextbusrec->generator_ang,
	coordnextbusrec->reactor_ang,
	0, 	/* don't manage */
	0, 	/* don't explode branches */
	0,	/* does not need to exist in pf */
	0,	/* no gen */
	0 );	/* no reac */
    assert(pgraphbusrec != NULL);

	/** set hide and abbr for bus vertex **/
    memcpy(pgraphupdaterec, pgraphbusrec, sizeof( GraphElement ) );
    memcpy(pgraphupdaterec->farvertex_id, coordnextbusrec->abbr,
					sizeof(coordnextbusrec->abbr));
    pgraphupdaterec->hide = coordnextbusrec->display[0]; 	/* added 2/96 */
    stat = db_update(&graph_db, &pgraphbusrec, &pgraphupdaterec);
    assert( D_OKAY == stat );
			
	/** set v_flag for name vertex **/
    findVertxLinkedToVertx( &pgraphnamerec, pgraphbusrec, GraphClassVertexName);
    memcpy( pgraphupdaterec, pgraphnamerec, sizeof( GraphElement ) );
    memcpy(pgraphupdaterec->farvertex_id, coordnextbusrec->abbr,
					sizeof(coordnextbusrec->abbr));
    pgraphupdaterec->dply_seg = atoi_cnt( coordnextbusrec->v_flag,1 );
    stat = db_update(&graph_db, &pgraphnamerec, &pgraphupdaterec);
    assert( D_OKAY == stat );

/************* NOW FIND ALL BRANCHES LINKED TO THIS BUS (Using 2nd name) ******/
    coordbranchvalrec = (PSBranchRecord *)&coordkeyvalrecord; /* define and   */
							      /* allocate ptr */

    strncpy( coordbranchvalrec->key,   " ",                    1 ); /* setup  */
    strncpy( coordbranchvalrec->name1, coordnextbusrec->name, 12 ); /* search */
    strncpy( coordbranchvalrec->name2, "            ",        12 ); /* key    */

    /** retrieve the 1st BRANCH record for this bus      **********************/
    statbranch = db_search ( &pscor_db,			/* search coord_db    */
			     &coordbranchvalrec,	/* for all branches   */
			     &coordnextbranchrec,	/* connected to this  */
			     COORD_NAME1_BASE1 );	/* bus. NOTE: coord_cb*/
							/* is fully loaded    */

    while( statbranch == D_FOUND)
    {
      if(coordnextbranchrec->key[0] != coordnextbusrec->key[0])
      {
		/* process branch ONLY IF far bus name	    *\
		\* comes alphabetically before near bus name*/
        rtncmp = strncmp( coordnextbranchrec->name2, 
                          coordnextbusrec->name, 12);
        if(rtncmp < 0)
        {
        /** may require transposing bend points **/
          coordtmpbranchrec = (PSBranchRecord *)&coordkeyvalrecord;

          psreverseCoordCard(  coordtmpbranchrec, coordnextbranchrec); 
          pscreateGraphCorEdges(  pgraphbusrec, 
				  coordnextbranchrec->name2, 
				  coordnextbusrec, 
				  coordtmpbranchrec );
        }
      } 

      coordbranchvalrec = coordnextbranchrec;	 /* move the "start" ptr up */

      statbranch = db_keynext(  &pscor_db,		/* get next branch **/
				&coordbranchvalrec, 
				&coordnextbranchrec,
				COORD_NAME1_BASE1 );
    } /* end branch loop */

/** NOW FIND ALL BRANCHES LINKED TO THIS BUS (USING BRANCH 1st NAME) ******/
/** ( This is a near-duplicate of the above code ) ************************/
    coordbranchvalrec = (PSBranchRecord *)&coordkeyvalrecord;

    strncpy(coordbranchvalrec->key, " ", 1);
    strncpy(coordbranchvalrec->name1, "            " , 12);
    strncpy(coordbranchvalrec->name2, coordnextbusrec->name , 12);

    /** retrieve the 1st BRANCH record for this bus      **********************/
    statbranch = db_search ( &pscor_db,
		             &coordbranchvalrec,
		             &coordnextbranchrec,
		             COORD_NAME2_BASE2 );

    /** loop through each branch record for bus **/
    while(statbranch == D_FOUND)
    {
      if(coordnextbranchrec->key[0] != coordnextbusrec->key[0])
      {
        rtncmp = strncmp ( coordnextbranchrec->name1, 
                   coordnextbusrec->name, 12 );

        if(rtncmp < 0)
        {
        /** this trick may require that the bending points be transposed **/
          coordtmpbranchrec = (PSBranchRecord *)&coordkeyvalrecord;
          /* psreverseCoordCard(coordtmpbranchrec, coordnextbranchrec); */ 
          pscreateGraphCorEdges( pgraphbusrec, 
				 coordnextbranchrec->name1, 
				 coordnextbusrec, 
				 coordnextbranchrec);
        }
      } /* End of processing of coord file branch record */

      coordbranchvalrec = coordnextbranchrec;	 /* get next branch **/
      statbranch = db_keynext ( &pscor_db,
		                &coordbranchvalrec,
		                &coordnextbranchrec, 
		                COORD_NAME2_BASE2 );
    } /* end branch loop */

    /** get the next coord bus **/
    coordbusvalrec = coordnextbusrec;
    statcoordbus = db_next ( &pscor_db,
	                     &coordbusvalrec,
	                     &coordnextbusrec,
	                     COORD_KEY_NAME1_BASE1 );

    if(coordnextbusrec->key[0] != 'B')
    {
       statcoordbus = D_NOTFOUND;
    }
  } /* end bus loop */

/*  New code to process the draw records */

  return(0);
}



void psreverseCoordCard 
	( PSBranchRecord *pcoordrev,	/* ptr returned for next branch rec */
	  PSBranchRecord *pcoordorg ) 	/* ptr returned for next branch rec */
/***************************************************************************\
*
* author: Dan Clark
* I want to get rid of this function by reversing the data when
* it is read in but i cannot get it to work today.
*
*	Called by: above
\***************************************************************************/
{
  int bendcnt;
  int i,seg;

    /* printf("NOTE: reverseCoordCard Called\n"); */
    memcpy(pcoordrev, pcoordorg,sizeof(PSBranchRecord));
    memcpy(pcoordrev->name1,pcoordorg->name2, 12);
    memcpy(pcoordrev->name2,pcoordorg->name1, 12);

    bendcnt = psbendCount( pcoordrev );

    for(i = 0; i < bendcnt; i++)
    {
      assert((bendcnt-i-1) >= 0);
      memcpy(pcoordrev->bend[i].x,pcoordorg->bend[bendcnt-i-1].x, 6);
      memcpy(pcoordrev->bend[i].y,pcoordorg->bend[bendcnt-i-1].y, 6);
    }

    seg = atoi ( pcoordrev->segment );

    if( bendcnt < seg )
    {
      printf("WARNING - from psreversCoordCard\n");
      printf("      flow segment incorrect - not enough bends.\n" );
    }

    if( bendcnt <= 0 )
      pcoordrev->segment[0] = '0';
    else
      strncpy( pcoordrev->segment, (char *)( bendcnt + 2 - seg ), 1 );
}




void pscreateGraphCorEdges ( GraphElement *pstartbusrec,
	char	       *far_vertex_name_base,
        PSBusRecord    *coordnextbusrec,  /* ptr returned for next bus rec    */
        PSBranchRecord *coordnextbranchrec)/* ptr returned for next branch rec*/
/****************************************************************************\
*
*	Function: Processes coord line card, creating all vertexes and
*		  edges.    Including line bends.
*
*	Notice: (to new programers) "Complex" edge is the straight line
*		without any bends, between two bus vertexes.
*		The bends in the line are called the "edges".
*
\****************************************************************************/
{
  PSBranchRecord	 *coordlinerec;         /* ptr to coord line record */

  GraphElement graphrecord;			/* define   */
  GraphElement *pgraphrecord = &graphrecord;	/* allocate */
  GraphElement *pgraphbendrec;
  GraphElement *pedge_last_created;
  GraphElement *pvertex_last_created;
  GraphElement *pcomplex_edge;
  GraphElement *pgraphfarendbusrec;
  GraphElement *pstart_edge;
  GraphElement *v1;
  GraphElement *v2;

  GraphLink *pgraphlink;

  DB_STAT statgraph;		/* status returned from graph db operations */
  DB_STAT stat;			 /* status returned for general db access */

  int bendcnt;		/* total bends on cmplx edge */
  int cnt;		/* bend # being created */
  int seg;		/* bend section that flow data should appear on */
  int flow_data_flg;	/* set TRUE if flow data goes on this bend segment */
  static int toggle = TRUE;

  stat = findBusGraphVertexRec( far_vertex_name_base, &pgraphfarendbusrec);
  if(stat != D_FOUND)
  {
    sprintf( errmsg, 
	     "ERROR line: %12.12s %12.12s bus 2 not specified in coord file\n",
             coordnextbusrec->name,
             far_vertex_name_base );
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "createGraphCorEdges";
    err.type = WARNING;
    err.ident = sccsid;
    em_show( &err );
    return;
  } 

  if(coordnextbranchrec->key[0] == 'L' || 	/* It's a line */
     coordnextbranchrec->key[0] == 'T' )	/* It's a transformer */
  {
    coordlinerec = (PSBranchRecord *)coordnextbranchrec;

    bendcnt = psbendCount( coordlinerec ); 	/* # of bend vertexes */

   /* now make v1 correspond to key[1] and v2 to key[2] */
   if(strncmp(pstartbusrec->vertex_id, coordnextbranchrec->name1, 12) == 0) {
      v1 = pstartbusrec;
      v2 = pgraphfarendbusrec;
   }
   else
   {
      v1 = pgraphfarendbusrec;
      v2 = pstartbusrec;
   }

   seg = atoi( coordnextbranchrec->segment );
   if( seg > bendcnt+1 )
   {
     printf("WARNING from pscreateGraphCorEdges (graphpscor.c)\n" );
  printf("  Bad value (%d) for segment - only %d segment(s)! (Default to %d)\n",
                                         seg, bendcnt+1, bendcnt+1 );
     seg = bendcnt+1;
   }

   /** Create the parent ( complex) edge **/
   pscreateGraphCorEdge ( v1,			/* vertex 1 */
			 v2, 		/* vertex 2 */
			 coordnextbranchrec->key,	/* name data */
			 GraphClassEdgeComplexSection,	/* class */ 
			 seg,
			 &pedge_last_created );

   pcomplex_edge = pedge_last_created;		/* save ptr to complex edge */

   if( bendcnt == 0 )
     flow_data_flg = 1;
   else
     flow_data_flg = 0;

   pscreateGraphCorEdge ( v1,		/* vertex 1 */
			 v2, 		/* vertex 2 */
			 coordnextbranchrec->key,	/* name data */
			 GraphClassEdgeSection,		/* class */ 
			 flow_data_flg,
			 &pedge_last_created );

   addGraphLink( pedge_last_created, pcomplex_edge );	 /* link parent - */
   addGraphLink( pcomplex_edge, pedge_last_created );	 /* child edges   */
   pstart_edge = pedge_last_created;		/* save ptr to complex edge */
   pvertex_last_created = pstartbusrec;

#ifdef NOWAY 
    /** this is a nice idea but the breakEdge routine manages to much **/
    /** Now create bend points followed by edges **/
    while(bendcnt > 0)
    {
      breakEdge(pedge_last_created, 
                pscoord_to_MOTIF_x (coordlinerec->bend[bendcnt - 1].x,
                                    6, coordnextbusrec->key),
                pscoord_to_MOTIF_y (coordlinerec->bend[bendcnt - 1].y,
                                    6, coordnextbusrec->key)  );
      --bendcnt;
    }   /* end bend processing */
#endif
   /** Now create bend points followed by edges **/
   cnt = 1;
   while(cnt <= bendcnt)
   {
      GraphElement *pedge, *pvertex_1, *pvertex_2;
      /** Create bend vertex in graph_db **/
      createGraphVertex( GraphClassVertexBendPoint,
			 coordnextbusrec->name,	
			 pscoord_to_MOTIF_x (coordlinerec->bend[cnt - 1].x,
                         6, coordnextbusrec->key),
			 pscoord_to_MOTIF_y (coordlinerec->bend[cnt - 1].y,
                         6, coordnextbusrec->key),
			 ' ',
			 &pgraphbendrec );
      assert(pgraphbendrec != NULL);

      pedge = pedge_last_created;
      stat = findEdgeVertexes( pedge, &pvertex_1, &pvertex_2 );
      if(pvertex_last_created == pvertex_1)
      {
        GraphElement *ptmp;
        ptmp = pvertex_1;
        pvertex_1 = pvertex_2;
        pvertex_2 = ptmp;
      }

      pvertex_last_created = pvertex_2;

      deleteGraphLink( pvertex_2, pedge );

      addGraphLink ( pedge, pgraphbendrec);
      addGraphLink ( pgraphbendrec, pedge );

      /** Now create child edge in graph_db **/
      if(  (cnt+1) == seg )
        flow_data_flg = 1;
      else
        flow_data_flg = 0;

      pscreateGraphCorEdge ( pgraphbendrec, 		/* vertex 1 */
			     pvertex_2, 		/* vertex 2 */
			     coordnextbranchrec->key,	/* name data */
			     GraphClassEdgeSection,	/* class */ 
			     flow_data_flg,		/* flag  */
			     &pedge_last_created );
 
      addGraphLink( pcomplex_edge, pedge_last_created );
      addGraphLink( pedge_last_created, pcomplex_edge ); /* cross link edges */
      cnt++;
    }   /* end bend processing */

  }
  else
  {
    printf("*** WARNING *** following branch card is not a 'L' or 'T' card\n" );
    printf("(%78.78s)\n", coordnextbranchrec );
  }

}



void change_bus_name_label( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
/***************************************************************************\
* Purpose: Answers the radio button, setting correct bus name style
*	   (Name only, Name and Kv or Abbreviated Name)
*
* Author:  Wm E. Rogers		Nov 8, 1994
*
* Called by: Any of three Bus Detail (Bus Name) radio buttons.
\***************************************************************************/
{
  char *pctag;
  char *org_str;
  char new_str[256];
  XmString x_str;

  XmStringContext context;       /* x context of compound string	*/
  XmStringCharSet charset;	 /* x return char set (unused)		*/
  char	          *text;         /* text pointer to file name		*/
  XmStringDirection     dir;     /* x return directory of file (unused) */
  Boolean	  separator;     /* x separator for file string (unused)*/

  GraphElement *pgraphrecord;
  
  DB_STAT stat;

  pctag = (char *)tag;
  /*  printf(" change_bus_name_label called......(state %d)\n",
		(int)XmToggleButtonGadgetGetState(w) );  */

  if( (int)XmToggleButtonGadgetGetState(w) == 0 ) return;

  if( db_valid_db(&graph_db ) != D_OKAY ) return;
  
  stat = firstGE( &pgraphrecord, GraphTypeVertex );

  while( D_FOUND == stat )
  {
    if( pgraphrecord->class == GraphClassVertexName )
    {
      switch (pctag[0])
      {
        case 'A':
	  if( strncmp( pgraphrecord->farvertex_id, "        ", 8 ) != 0 ) {
  	    strncpy( new_str, pgraphrecord->farvertex_id, 
              sizeof(pgraphrecord->farvertex_id));
	  } else {
  	    strncpy( new_str, pgraphrecord->farvertex_id, 8 );
          }
	  new_str[8] = '\0';
        break;
 
        case 'N':
	  strncpy( new_str, pgraphrecord->vertex_id,  8 );
	  new_str[8] = '\0';
        break;

        case 'V':
	  strncpy( new_str, pgraphrecord->vertex_id, 12 );
	  new_str[12] = '\0';
        break;

	default:
	printf("** PROGRAMMER ERROR in change_bus_name_lbl (graphpscor.c)\n");
	printf("** Radio pushbutton tag (%s) not valid.\n", pctag[0] );
	printf("   Valid values are: A, N or V\n" );
      }

     if(pgraphrecord->wid != NULL) {
      XtVaGetValues( pgraphrecord->wid, XmNlabelString, &x_str, NULL);
      XmStringInitContext(&context, x_str);
      XmStringGetNextSegment( context, &text,		/* discard 1st */
			 &charset, &dir, &separator );

      while( XmStringGetNextSegment( context, &text,
			 &charset, &dir, &separator ))
      {
        strcat( new_str, "\n" );
        strcat( new_str, text );
      }
      x_str =  XmStringCreateLtoR(new_str, XmSTRING_DEFAULT_CHARSET);
      XtVaSetValues( pgraphrecord->wid, XmNlabelString, x_str, NULL);
      XmStringFree(x_str);
     }
    }

    stat = nextGE( &pgraphrecord, GRAPH_DISPLAY_TYPE );
  }
}


void pscreateGraphCorEdge ( GraphElement *vertx_1, GraphElement *vertx_2, 
			    char *bdata, GraphClass class, int seg,
			    GraphElement **new_rec ) 
/****************************************************************************\
* Purpose: Creates edge record for graph_db, inserts, creates all links
*
*	Author: Bill Rogers 	Oct 21, 1992
*
*	Called by: createGraphCorEdges
*		create_and_manage_coord_edge
*		process_pf_connections(2)(graphdata.c)
*		explodeBranches(2) (pf_cb.c)
*		explodeBus(2)	   (pf_cb.c)
*		add_graph_line(2)  (toolbox.c)
*		addGraphBranch	   (toolbox.c)
*		addDrawLine	   (toolbox.c)
*		breakEdge	   (toolbox.c)
\****************************************************************************/
{
  GraphElement graphrecord;			/* define   */
  GraphElement *pgraphrecord = &graphrecord;	/* allocate */
  GraphElement *pgraphbranchrec;
  PSBranchRecord branch;
  PSBranchRecord *pbranch = &branch;
  GraphElement *vtxhi;
  GraphElement *vtxlo;

  DB_STAT stat;			 /* status returned for general db access */

  pgraphrecord = &graphrecord;

  graphrecord.display = GraphDisplayOff;
  graphrecord.type    = GraphTypeEdge;
  graphrecord.class   = class;
  graphrecord.dply_seg= seg;
  graphrecord.x       = 0;
  graphrecord.y       = 0;
  graphrecord.wid     = NULL;

  /* ensure all branches are specified low to high and vtx corresponds */

  memset(&pbranch->key[0], ' ', sizeof(branch) - sizeof(branch.idx));

  pbranch->idx = graph_idx++;

  memcpy(&pbranch->key[0], bdata, sizeof(branch) - sizeof(branch.idx));

  if(strncmp(pbranch->name1, pbranch->name2, sizeof(graphrecord.vertex_id)) > 0) {
    strncpy(graphrecord.farvertex_id, pbranch->name1,
					 sizeof(graphrecord.vertex_id));
    strncpy(graphrecord.vertex_id, pbranch->name2,
					 sizeof(graphrecord.farvertex_id));
    vtxlo = vertx_2;
    vtxhi = vertx_1;
  }
  else
  {
    strncpy(graphrecord.vertex_id, pbranch->name1,
					 sizeof(graphrecord.vertex_id));
    strncpy(graphrecord.farvertex_id, pbranch->name2,
					 sizeof(graphrecord.farvertex_id));
    vtxlo = vertx_1;
    vtxhi = vertx_2;
  }
  stat = db_insert_opt(&graph_db, &pgraphrecord, &pgraphbranchrec, optse);	

  if(stat != D_OKAY)
  {
    printf("ERROR insert graph table stat %d\n",stat);
    printf("  record failed is %s\n", pgraphrecord);
  }
  assert(stat == D_OKAY);

  addGraphLink( vtxlo, pgraphbranchrec );
  addGraphLink( pgraphbranchrec, vtxlo );

  addGraphLink( vtxhi, pgraphbranchrec );
  addGraphLink( pgraphbranchrec, vtxhi );

  *new_rec = pgraphbranchrec;

}


int psgetHtWdScale()
/****************************************************************************\
* Purpose: Get & set 8 values from the Plot Option Dialog
*          1) x_wd, y_ht                (inches, cm or pixels)
*          2) xscale, yscale            (fraction )
*          3) x offset, y offset        (in cm    )
*          4) paper width, paper height (in pixels)
*
*	Called by: startup		(autostart.c)
*		   read_print_options	(printopts.c)
*		   pswriteCoordFile	(pscordat.c)
*		   buildGraphCoord	(graphcor.c - obsolete )
\****************************************************************************/
{

  get_paper_size( &x_wd,	&y_ht,
		  &xscale,      &yscale,
		  &x_cm_offset, &y_cm_offset );

  plot_pixels_wd  =  (int)(pixelsPerUnit * x_wd + .5);
  plot_pixels_ht  =  (int)(pixelsPerUnit * y_ht + .5);


  return(0);
}


void psshow_graph_db(Widget w, caddr_t *tag, caddr_t *reason)
/*****************************************************************************\
* Purpose: Debugging tool to list all Coord db cards.
*
* ***** (PRESENTLY INACTIVE) ******
\*****************************************************************************/
{
  PSBusRecord		*coordbusvalrec;
  PSBusRecord		*coordnextbusrec;
  PSCoordFileRecord	*coordkeyvalrecord;
  DB_STAT		statcoordbus;

  coordbusvalrec = (PSBusRecord *)&coordkeyvalrecord;
  statcoordbus = db_search ( &pscor_db,
		             &coordbusvalrec,
		             &coordnextbusrec,
		             COORD_KEY_NAME1_BASE1 );

  if(statcoordbus == D_EOF)
  {
    printf("ERROR: no bus data found in coord data structure\n");
    return;
  }

  if(statcoordbus != D_FOUND)
  {
     statcoordbus = D_FOUND; /* positioned before first bus rec in coord file */
  }

  while(statcoordbus == D_FOUND)
  {
    printf("XXX\n");

    statcoordbus = db_next ( &pscor_db,
	                     &coordbusvalrec,
	                     &coordnextbusrec,
	                     COORD_KEY_NAME1_BASE1);
  }

}


void shift_paper( int deltaX, int deltaY )
{
  printf("Paper shift......");
  paperOffsetX += deltaX;
  paperOffsetY += deltaY;

}

void set_graph_unit_and_origin_cb (Widget w, XtPointer tag,
                                 XmAnyCallbackStruct *cbs )
/************************************************************************\
* Purpose: Change the x,y coord values depending on which button is set.
*
* Author: W.E. Rogers		Dec 1994
*
* Called by: Any of 7 push buttons in bus_modify_coord_form (buscoord.u)
\************************************************************************/
{
  char o_flag;
  char *pctag;
  char busname[13];
  char str[9];
  Boolean button_state;

  pctag = (char *)tag;
  o_flag = pctag[0];

  button_state = True;

  switch( o_flag )
  {
    case 'C':
      pixelsPerUnit = PixelsPerCm;
      adj	    = 0.02;
      button_state = XmToggleButtonGetState( widget_id( "graph_units_cm_rb") );
			/* Above line prevents those "double calls" */
      strcpy( units, "Cm" );
    break;
 
    case 'I':
      pixelsPerUnit = PixelsPerInch;
      adj	    = 0.01;
      button_state = XmToggleButtonGetState( widget_id( "graph_units_inch_rb"));
      strcpy( units, "Inches" );
    break;

    case 'P':
      pixelsPerUnit = 1.0;
      adj	    = 1.0;
      button_state = XmToggleButtonGetState( widget_id("graph_units_pixel_rb"));
      strcpy( units, "Pixels" );
    break;

    case 'T':			/* top		*/
    case 'B':			/* bottom	*/
      origin = o_flag;
      enable_pushbutton ( "graph_units_inch_rb" );
      enable_pushbutton ( "graph_units_cm_rb" );
    break;

    case 'M':			/* absolute (MOTIF) */
      origin = o_flag;
      set_button_state( "graph_units_pixel_rb", True );
      disable_pushbutton ( "graph_units_inch_rb" );
      disable_pushbutton ( "graph_units_cm_rb" );
    break;

    case 'S':
      scaling_flg = False;	/* forces coord_display to scale to */
    				/* compute original or "to save" values  */
    break;

    case 'A':
      scaling_flg = True;	/* do not scale anything i.e. scale = 1.0 */
    break;			/* i.e. original, Unscaled data 	  */

    case 'E':			/* show GraphElement x, y value 	*/
    case 'G':			/* show Gadget x, y value 		*/
    break;

    default:
      printf("**WARNING** bad param in call to set_graph_unit_and_origin_cb\n");
      printf("    Valid characters are: C, I, P, T, B, M, S, A, E,  or 'G'\n" );
   }

  /* following IF  to call these routines only when the button is set TRUE*/
  /* (This prevents an unneeded double call )			*/

   if( button_state )
   {
     display_coord_data( NULL );
   }
}


int pscoord_to_MOTIF_x( char *x, int col, char *bus_name )
/****************************************************************************\
* Purpose:  Same as pscoord_to_MOTIF_x_flt, except this call allows
*		CHARACTER input in place of INTEGER value.
\****************************************************************************/
{
  return( pscoord_to_MOTIF_x_flt( atof_cnt_zero(x,col), bus_name ) );
}






int pscoord_to_MOTIF_x_flt( double x, char *bus_name )
/****************************************************************************\
* Purpose:  Standard routine to convert coord file coordinates from
*	    centimeter values to MOTIF pixel values.
*
* Note:     Routine MOTIF_to_ps_x is the direct opposite of this routine.
*	    DO NOT CHANGE THE CONVERSION METHOD WITHOUT CHANGING MOTIF_to_ps_x!
*
* Author: William E. Rogers
*
\****************************************************************************/
{
  int x_pixels;

  x_pixels = x_flt_2_pxl( x );				/*(1)(2)(3)*/

    if( force_vertexes_inside_paper_border &&
       ( x_pixels > (plot_pixels_wd-paper_border) || x_pixels < paper_border ) )
    {
      sprintf(errmsg, 
             "WARNING: X coordinate %d off paper %d for %18.18s\n", 
             x_pixels, plot_pixels_wd, bus_name );
      err.line = EM_LINE;
      err.msg  = errmsg;
      err.link = "pscoord_to_MOTIF_x";
      err.type = WARNING;
      err.ident = sccsid;
      em_show( &err );

      if( x_pixels< paper_border )
	  x_pixels = paper_border;		/* move right to left edge */
      	else
	  x_pixels = plot_pixels_wd -paper_border; /* move left to right edge */
    }

    x_pixels += paperOffsetX;		/* (5) adj for scrn display	*/
    x_pixels *= zoom_factor;		/* (6) value set by toolbox.c	*/
    x_pixels += SCRN_BORDER;		/* (7)				*/

    return ( x_pixels );
}



int pscoord_to_MOTIF_x_abs( char *x, int col )
/****************************************************************************\
* SPECIAL case when NO OFFSETS and NO SCALING is desired. 
* 	e.g. plot may be scaled to fit paper but paper size is NEVER scaled!
*
* Note: for now this routine is used ONLY for paper edges.
\****************************************************************************/
{
  int x_pixels;

  x_pixels = (int)(pixelsPerUnit * ( atof_cnt_zero(x,col) ) +.5 );  /*(2)*/

  x_pixels += paperOffsetX;					    /*(5)*/
  x_pixels *= zoom_factor;		/* value set by toolbox.c     (6)*/
  x_pixels += SCRN_BORDER;					    /*(7)*/

  if( x_pixels <1 ) x_pixels = 1;

  return ( x_pixels );

}


int pscoord_to_MOTIF_y( char *y, int col, char *bus_name )
/****************************************************************************\
* Purpose:  Same as pscoord_to_MOTIF_y_flt, except this call allows
*		CHARACTER input in place of INTEGER value.
\****************************************************************************/
{
  return( pscoord_to_MOTIF_y_flt( atof_cnt_zero(y,col), bus_name ) );
}

int pscoord_to_MOTIF_y_flt( double y, char *bus_name ) /*****************\
* Purpose:  Standard routine to convert coord file coordinates from
*	    centimeter values to MOTIF pixel values.
*
* Note:     Routine MOTIF_to_ps_y is the direct opposite of this routine.
*	    DO NOT CHANGE THE CONVERSION METHOD WITHOUT CHANGING MOTIF_to_ps_y!
*
* Author: William E. Rogers
*
\****************************************************************************/
{
  int y_pixels;

  y_pixels = y_flt_2_pxl( y ); 				/*(1)(2)(3)*/

    if( force_vertexes_inside_paper_border &&
            ( y_pixels > (plot_pixels_ht - paper_border) ||
	    ( y_pixels + SCRN_BORDER) < paper_border   ) )
    {
      sprintf(errmsg, 
             "WARNING: Y coordinate %d out of range %d for %18.18s\n", 
             y_pixels, plot_pixels_ht, bus_name );
      err.line = EM_LINE;
      err.msg  = errmsg;
      err.link = "pscoord_to_MOTIF_y";
      err.type = WARNING;
      err.ident = sccsid;
      em_show( &err );

      if( y_pixels <= paper_border )
	y_pixels = paper_border;		 /* move up to bottom border */
      else
	y_pixels = plot_pixels_ht +paper_border; /* move down to top border*/
    }

    if( force_vertexes_inside_paper_border &&
            ( y_pixels > (plot_pixels_ht - paper_border) ||
	    ( y_pixels + SCRN_BORDER) < paper_border   ) )
    {
      sprintf(errmsg, 
             "WARNING: Y coordinate %d out of range %d for %18.18s\n", 
             y_pixels, plot_pixels_ht, bus_name );
      err.line = EM_LINE;
      err.msg  = errmsg;
      err.link = "pscoord_to_MOTIF_y";
      err.type = WARNING;
      err.ident = sccsid;
      em_show( &err );

      if( y_pixels <= paper_border )
	y_pixels = paper_border;		/* move up to bottom border */
      else
	y_pixels = plot_pixels_ht +paper_border; /* move down to top border */
    }

    if( origin == 'B' )
    {
      y_pixels = plot_pixels_ht - y_pixels;  /* convert to MOTIF coords (4)*/
					     /* ( Y-coord only )	   */
    }

    y_pixels += paperOffsetY;		/* adj for scrn display		(5)*/
    y_pixels *= zoom_factor;		/* value set by toolbox.c	(6)*/
    y_pixels += SCRN_BORDER;					      /*(7)*/

    return ( y_pixels );
}

/*************************************************************************\
*	DOCUMENTATION OF OPERATIONS:
*
*	(1) x_cm_offset, y_cm_offset
*	(2) pixelsPerUnit
*	(3) * xscale, yscale	(IF scaling flag is set)
*	(4) plot_pixels_ht	(y coords only - IF origin is bottom left)
*	(5) paperOffsetY, paperOffsetX
*	(6) zoom_factor
*	(7) SCRN_BORDER
*
\*************************************************************************/

int pscoord_to_MOTIF_y_abs( char *y, int col )
/****************************************************************************\
* SPECIAL case when NO SCALING and NO OFFSET is desired. 
* 	e.g. plot may be scaled to fit paper but paper size is NEVER scaled!
*
* Note: for now this routine is used only for paper edges.
\****************************************************************************/
{
  int y_pixels;

  y_pixels = (int)(pixelsPerUnit*( atof_cnt_zero(y,col) ) +.5 );   /*(2)*/

  if( origin == 'B' )		/* for normal coord data with		*/
  {				/* origin at bottom left paper corner	*/
    y_pixels = plot_pixels_ht - y_pixels;			   /*(4)*/
  }

  y_pixels += paperOffsetY;					   /*(5)*/
  y_pixels *= zoom_factor;		/* value set by toolbox.c    (6)*/
  y_pixels += SCRN_BORDER;					   /*(7)*/

  if( y_pixels <1 ) y_pixels = 1;

  return ( y_pixels );

}


#define MAXLOOPCNT 5

double MOTIF_to_ps_x( int x_pixels, char *type, char *name1, char *name2 )
/****************************************************************************\
*
* Called by: printPSBusRecord, printPSBranchRecord ( graphdata.c )
\****************************************************************************/
{
  char x_cm_test[7];
  double x_pos;
  int cnt;
  int new_x;
  int x_int;
  int left_edge;
  int right_edge;

  x_pos = x_pixels -SCRN_BORDER;				/*(7)*/
  x_pos /= zoom_factor;						/*(6)*/
  x_pos -= paperOffsetX;					/*(5)*/

  if(0.0 == xscale) xscale = 1.0;
  if( scaling_flg ) x_pos /= xscale;   /* change back to original (3)*/

  x_pos /= pixelsPerUnit;					/*(2)*/

  if( pixelsPerUnit != 1.0 ) /* in fractions of cm or inch */
  {
    x_pos = (double)(x_pos - x_cm_offset + .0049 );		/*(1)*/
    sprintf( x_cm_test, "%6.2lf", x_pos );
  }
  else /* in whole pixels */
  {
    x_int = (int)(x_pos - x_cm_offset + .499 );			/*(1)*/
    x_pos = (double)(x_int);
    sprintf( x_cm_test, "%d", x_int );
  }

/*************  border check and warnings ***********************************/
/**   give a minor warning if this value is off the edge of the paper **/
/**   ( check in pixels - pixelPerUnit conversion for error msg ) */

  right_edge = (paperOffsetX + plot_pixels_wd)*zoom_factor + SCRN_BORDER;
  left_edge  = paperOffsetX*zoom_factor +SCRN_BORDER;

  if( x_pixels > right_edge || x_pixels < left_edge )
  {
    printf("INFO (%4.4s %12.12s %12.12s)", type, name1, name2 );
    if( x_pixels < left_edge )
      printf(" %5.1f %s left of paper edge.\n",	 
	  (double)((left_edge-x_pixels)/zoom_factor/pixelsPerUnit ), units );
    else
      printf(" %5.1f %s right of paper edge.\n", 
	  (double)((x_pixels-right_edge)/zoom_factor/pixelsPerUnit), units );
  }
 
/*************  creep check *************************************************/
/* "creep" check proceedures to ensure coordinates don't creep on graph
     by successive saves.   */

  new_x = pscoord_to_MOTIF_x( x_cm_test, 6, "creep_ck" );

  cnt = 0;

  while( new_x != x_pixels && cnt <= MAXLOOPCNT )
  {
    cnt++;
    if(ipfdebug & DB_GraphPSCorMask) {
      printf("  X creepage! org(%6.2f %4d) <> new(%4d)", x_pos, x_pixels,new_x);
    }

    if( new_x < x_pixels )
      x_pos += adj;
    else
      x_pos -= adj;

    sprintf( x_cm_test, "%6.2lf", x_pos );
    new_x = pscoord_to_MOTIF_x( x_cm_test, 6, "creep_ck" );
    if(ipfdebug & DB_GraphPSCorMask) {
      printf("  Adjusted to %s\n", x_cm_test );
    }
  }

  if( cnt >= MAXLOOPCNT )
    printf("*** WARNING *** X-coord Creepage error (%s %12.12s %12.12s)\n",
						 type, name1, name2 );
  else if ( cnt > 0 )
    if(ipfdebug & DB_GraphPSCorMask) {
      printf("INFO *** X-coord Creepage corrected (%s %12.12s %12.12s)\n",
						 type, name1, name2 );
    }
  return ( x_pos );
}



double MOTIF_to_ps_y( int y_pixels, char *type, char *name1, char *name2 )
/****************************************************************************\
*
* Called by: printPSBusRecord, printPSBranchRecord ( graphdata.c )
\****************************************************************************/
{
  char y_cm_test[7];
  double y_pos;
  int cnt;
  int new_y;
  int y_int;
  int top_edge;
  int bottom_edge;

  y_pos = SCRN_BORDER - y_pixels;				/*(7)*/
  y_pos /= zoom_factor;						/*(6)*/
  y_pos += paperOffsetY;					/*(5)*/

  if( origin == 'B' )
  {
    y_pos += plot_pixels_ht;					/*(4)*/
  }

  if(0.0 == yscale) yscale = 1.0;
  if( scaling_flg ) y_pos /= yscale;   /* change back to original (3)*/

  y_pos /= pixelsPerUnit;					/*(2)*/

  if( pixelsPerUnit != 1.0 ) /* in fractions of cm or inch */
  {
    y_pos = (double)( y_pos + y_cm_offset + .0049 );		/*(1)*/
    sprintf( y_cm_test, "%6.2lf", y_pos );
  }
  else /* in whole pixels */
  {
    y_int = (int)(y_pos + y_cm_offset + .499 );			/*(1)*/
    y_pos = (double)(y_int);
    sprintf( y_cm_test, "%d", y_int );
  }

/*************  border check *******************************************/
/**   give a minor warning if this value is off the edge of the paper **/
/**   ( checks in pixels - pixelPerUnit conversion with error statment ) */
  
  top_edge    = paperOffsetY*zoom_factor + SCRN_BORDER;
  bottom_edge = (plot_pixels_ht + paperOffsetY)*zoom_factor + SCRN_BORDER;

  if( y_pixels > bottom_edge )
  {
    printf("INFO (%4.4s %12.12s %12.12s)", type, name1, name2 );
    printf(" %5.1f %s below paper edge.\n",
	(double)((y_pixels-bottom_edge)/zoom_factor/pixelsPerUnit), units );
  }
  else if ( y_pixels < top_edge )
  {
    printf("INFO (%4.4s %12.12s %12.12s)", type, name1, name2 );
    printf(" %5.1f %s above paper edge.\n", 
	(double)((top_edge -y_pixels)/zoom_factor/pixelsPerUnit), units  );
  }
 
/*************  creep check *************************************************/

  new_y = pscoord_to_MOTIF_y( y_cm_test, 6, "creep_ck" );

  cnt = 0;
  while( new_y != y_pixels && cnt <= MAXLOOPCNT )
  {
    cnt++;
    if(ipfdebug & DB_GraphPSCorMask) {
      printf("  Y creepage! org(%6.2f %4d) <> new(%4d)", y_pos,y_pixels, new_y);
    }

    if( new_y < y_pixels && origin=='B' || new_y > y_pixels && origin!='B' )
      y_pos -= adj;
    else
      y_pos += adj;

    sprintf( y_cm_test, "%6.2lf", y_pos );
    new_y = pscoord_to_MOTIF_y( y_cm_test, 6, "creep_ck" );
    if(ipfdebug & DB_GraphPSCorMask) {
      printf("  Adjusted to %s\n", y_cm_test );
    }
  }
 
  if( cnt >= MAXLOOPCNT )
    printf("*** WARNING *** Y-coord Creepage (%s %12.12s %12.12s)\n",
						 type, name1, name2 );
  else if ( cnt > 0 )
    if(ipfdebug & DB_GraphPSCorMask) {
      printf("INFO *** Y-coord Creepage corrected (%s %12.12s %12.12s)\n",
						 type, name1, name2 );
    }
  return ( y_pos );
}




void set_units_to_cm()
/*************************************************************************\
* Purpose: for outside callers to ensure UNITS is set to CENTIMETERS
*		(like before READING or SAVING coordinate data)
*
* Called by: psbuildGraphCoord
*	     test_graphdata
*	     pswriteCoordFile (pscordat.c)
\*************************************************************************/
{

  fetch_widget("graph_units_cm_rb");   /* Can't do this - screws up 
					sensivity changes later! */

  if( expose_occured )	/* theres a possibility that user changed button
			/*  settings... and must ensure its set back to CM */
    set_button_state( "graph_units_cm_rb",       True );

  pixelsPerUnit = PixelsPerCm;

}

void set_expose_occured()
{
  expose_occured = True;
}

void set_origin_to_bottom()
/*************************************************************************\
* Purpose: for outside callers to ensure ORIGIN is set to BOTTOM of paper
*		(like before READING or SAVING coordinate data)
*
\*************************************************************************/
{
  fetch_widget("graph_origin_bottom_rb");

  set_button_state( "graph_origin_bottom_rb",       True );
  origin = 'B';
}




int rescale_graph( double xfactor, double yfactor, int scale_flag )
/*****************************************************************************\
* Purpose: Adjust location of all graphelements and their vertexes
*
*	scale_flag = FULL, means to rescale EVERYTHING (like for zoom)
*		   = PARTIAL - rescale only the network grid
*
*       Author: John Rutis      Nov, 1992
*
* Called by: redraw_graph_with_new_dply_opts (printopts.c)
*	     tools_zoom_cb	(toolbox.c)
\*****************************************************************************/
{
  GraphElement graphnode;
  GraphElement *pgraphnode = &graphnode;
  GraphElement *pgraphnodenxt;
  DB_STAT stat;         /* status returned for general db access */

  Dimension wd,ht;

  /*** check for a valid db **/
  if(db_valid_db(&graph_db) != D_OKAY)
  {
    printf("ERROR: printGraphData - no graph table\n");
    return;
  }

/***** retrieve all the vertex records and update the x and y values *****/

  /** Get the first vertex record **/
  pgraphnode = &graphnode;
  pgraphnode->type = GraphTypeVertex;
  stat = db_search(&graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_TYPE);

  if(stat != D_FOUND)
  {
    printf("ERROR db_search did not find any records \n");
    return(1);
  }

  findGraphMax();	/* shift paper if needed */

  /** Now loop through all the records **/
  while(stat == D_FOUND)
  {
    pgraphnode = &graphnode;
    memcpy(pgraphnode, pgraphnodenxt, sizeof(GraphElement));

	/*  paper edges and border are rescaled only on ZOOM */
    if( scale_flag == FULL_RESCALE ||
	( pgraphnode->class != GraphClassPaperVertex &&
	  pgraphnode->class != GraphClassVertexBorder &&
	  pgraphnode->class != GraphClassVertexLabelCorner &&
	  pgraphnode->class != GraphClassVertexDrawPoint &&
	  pgraphnode->class != GraphClassVertexLabelText ) )
    {
      pgraphnode->x = (pgraphnode->x - SCRN_BORDER)*xfactor + SCRN_BORDER;
      if( scale_flag == FULL_RESCALE )
      {
        pgraphnode->y = (pgraphnode->y - SCRN_BORDER)*yfactor + SCRN_BORDER;
      }
      else if ( scale_flag == PARTIAL_RESCALE )
      {
        pgraphnode->y = plot_pixels_ht*zoom_factor - 
	   (plot_pixels_ht*zoom_factor -pgraphnode->y + SCRN_BORDER)*yfactor +
			 SCRN_BORDER;
      }
      else
      {
	printf("*** Programmer error - rescale_graph (graphpscor.c)\n" );
	printf("    Incorrect param for flag - should be FULL_RESCALE or PARTIAL_RESCALE\n");
      }

      /* now change size of "expose" widget (busname and comments only)*/
      if( pgraphnode->class == GraphClassVertexComment ||
	  pgraphnode->class == GraphClassVertexLabelText ||
	  pgraphnode->class == GraphClassVertexName )
      {
        if( pgraphnode->wid != 0 )	/* ensure widget has been managed */
        {
          XtVaGetValues( pgraphnode->wid, XmNwidth, &wd, XmNheight, &ht, NULL );
          wd = (Dimension)(wd*xfactor);
          ht = (Dimension)(ht*xfactor);
          XtVaSetValues( pgraphnode->wid, XmNwidth, wd, XmNheight, ht, NULL );
        }
      }

      stat = db_update(&graph_db, &pgraphnodenxt, &pgraphnode);

      position_gadget_wrt(pgraphnodenxt);
    }

    /** Get next record **/
    pgraphnode = pgraphnodenxt;
    stat = db_keynext(&graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_TYPE);
  }

/**************retrieve and update all the edge widgets ******************/

  /** Get the first edge record **/
  pgraphnode = &graphnode;
  pgraphnode->type = GraphTypeEdge;
  stat = db_search(&graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_TYPE);

  if(stat != D_FOUND)
  {
    printf("ERROR db_search did not find any records \n");
    return(1);
  }

  /** Now loop through all the records **/
  while(stat == D_FOUND)
  {
    if (ipfdebug & (DB_TraceMask | DB_Toolbox)) {
        fprintf(ipfdbio, "rescale_graph: printGraphElement(pgraphnodenxt)\n");
        printGraphElement(pgraphnodenxt);
    }
    if(GraphDisplayOn == pgraphnodenxt->display)
    {
      if(NULL == pgraphnodenxt->wid)
        {
          printf("Display is on but widget id is NULL!\n");
        }
        else
        {
	  /*  paper edges and border are rescaled only on ZOOM */
          if( scale_flag == FULL_RESCALE ||
	     ( pgraphnodenxt->class != GraphClassPaperEdge &&
	       pgraphnodenxt->class != GraphClassEdgeBorder ) )
	  {
            updateEdge(pgraphnodenxt);
	  }
        }
    }
 
    /** Get next record **/
    pgraphnode = pgraphnodenxt;
    stat = db_keynext(&graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_TYPE);
  }
}


void redraw_graph_with_new_dply_opts( )
/***************************************************************************\
* Purpose:
*
* Author: Bill Rogers   April 12, 1993
*
* Called by: OK (APPLY) pushbutton on page_option dialog.
\***************************************************************************/
{
  double xfactor, yfactor;
  double new_plot_ht;

  old_xscale = xscale;    /* save old values */
  old_yscale = yscale;
  old_paperOffsetX = paperOffsetX;
  old_paperOffsetY = paperOffsetY;
  old_x_cm_offset = x_cm_offset;
  old_y_cm_offset = y_cm_offset;
  old_paper_ht = plot_pixels_ht;

  /* possible new PAPER HT, PAPER OFFSETS, GRID OFFSETS or SCALING */
  get_paper_size( &x_wd,        &new_plot_ht,	/* from text windows */
                  &new_xscale,  &new_yscale,
                  &new_x_cm_offset, &new_y_cm_offset );


  new_paper_ht = (int)(new_plot_ht * pixelsPerUnit + 0.5 );
  new_y_cm_offset = -new_y_cm_offset;

/* remember - everything here is done without zoomfactor and screen border */
  new_paperOffsetX = 0;
  new_paperOffsetY = 0;
  jiggle_all_graphelements( );		/* 1st pass - gets min/max values */

  rescale_graph( 1.0, 1.0, FULL_RESCALE ); 	/* has routine to set */
						/* gadget locations   */

/***************************** 2nd part - redo border ********************/
/****************  Added 3/10/95 *****************************************/

  if(db_valid_db(&graph_db) == D_OKAY)
  {
  deleteGraphElementByClass   ( GraphClassPaperVertex ) ;
  deleteGraphElementByClass   ( GraphClassPaperEdge ) ;
  deleteGraphElementByClass   ( GraphClassEdgeBorder ) ;
  deleteGraphElementByClass   ( GraphClassVertexBorder ) ;
  deleteGraphElementByClass   ( GraphClassVertexLabelCorner ) ;
  deleteGraphElementByClass   ( GraphClassVertexLabelText ) ;
  deleteGraphElementByClass   ( GraphClassEdgeLabel ) ;
  }

/*  deleteGraphElementByClass   ( GraphClassVertexLegendCorner ) ;
*  deleteGraphElementByClass   ( GraphClassEdgeLegend ) ;		
*  deleteGraphElementByClass   ( GraphClassVertexLegendText ) ;*/

  psgetHtWdScale();	/* set y_ht, plot_pixels_ht	*/
			/* so following will work right */


  pscreate_plot_form ();      /* create paper, border and label */
}



void jiggle_all_graphelements( )
/**************************************************************************\
\**************************************************************************/
{
  GraphElement graphvertex;
  GraphElement *pgraphvertex;
  GraphElement *pgraphvertexnxt;
  DB_STAT stat;

  if(db_valid_db(&graph_db) == D_OKAY) 
  {

  stat = db_first( &graph_db, &pgraphvertexnxt, GRAPH_TYPE );

  while(stat == D_FOUND)	/* LOOP THROUGH ALL GRAPHELEMENTS */
  {
    switch ( pgraphvertexnxt->class )
    {
      case GraphClassVertexBus:
      case GraphClassVertexName:
      case GraphClassVertexBendPoint:
      case GraphClassVertexGenerator:
      case GraphClassVertexDrawPoint:
	jiggle_graphelement( pgraphvertexnxt );
      break;

    }
    pgraphvertex = pgraphvertexnxt;
    stat = db_next( &graph_db, &pgraphvertex, &pgraphvertexnxt, GRAPH_TYPE );

  } /* end loop */
  }
}

void jiggle_graphelement( GraphElement *ge )
/**************************************************************************\
\**************************************************************************/
{
  double x,y;
  DB_STAT stat;
  GraphElement updatedata;
  GraphElement *pupdatedata      = &updatedata;

  memcpy( pupdatedata, ge, sizeof( GraphElement ) );

  x = MOTIF_to_ps_x( ge->x, "", "", "" );	/*convert back to cm */
  y = MOTIF_to_ps_y( ge->y, "", "", "" );

  set_new_values();
  pupdatedata->x = pscoord_to_MOTIF_x_flt( x, "" );	/*find new location */
  pupdatedata->y = pscoord_to_MOTIF_y_flt( y, "" );

  stat = db_update( &graph_db, &ge, &pupdatedata );

  set_old_values();

}

void  set_new_values()
/**************************************************************************\
\**************************************************************************/
{
  xscale	= new_xscale;
  yscale	= new_yscale;
  paperOffsetX	= new_paperOffsetX;
  paperOffsetY	= new_paperOffsetY;
  x_cm_offset	= new_x_cm_offset;
  y_cm_offset	= new_y_cm_offset;
  plot_pixels_ht= new_paper_ht;

}
void  set_old_values()
/**************************************************************************\
\**************************************************************************/
{
  xscale	= old_xscale;    /* save old values */
  yscale	= old_yscale;
  paperOffsetX	= old_paperOffsetX;
  paperOffsetY	= old_paperOffsetY;
  x_cm_offset	= old_x_cm_offset;
  y_cm_offset	= old_y_cm_offset;
  plot_pixels_ht= old_paper_ht;

}

void snap_graphelements_in_place()
/**************************************************************************\
\**************************************************************************/
{


}

void psDrawOptions()
/****************************************************************************\
* Purpose:  Central routine which gets all the options routines.
*	    (So we don't have to keep changing filedlgrtn all the time!)
*
* Author: William E. Rogers	9/22/94
*
* Called by: buildMainDisplay (graphdata.c)
*
\****************************************************************************/
{

  pscreate_plot_form 	();      /* create paper, border and label */

  processDrawCards();

  pscreateCommentVertexes();		/* see line 2490 */

  send_comments_and_defines_to_pf();	/* see line 2250 */

/*  printf("calling testdraw\n");
  testdraw();
*/

}



#ifdef JUNK

turn_on_graph_element(GraphElement **ppel)
{
  GraphElement *pel = *ppel;
  GraphElement *pupdatedata;
  GraphElement updatedata;

  DB_STAT stat;			 /* status returned for general db access */

      /* must do db_update to change record */
      pupdatedata      = &updatedata;
      memcpy( pupdatedata, pel, sizeof( GraphElement ) );
      pupdatedata->display = GraphDisplayRequestOn;
      stat = db_update( &graph_db, &pel, &pupdatedata );
      assert(D_OKAY == stat);
      manageVertex ( pel);
  *ppel = pel;
}
#endif


int   processDrawCards	()
/**************************************************************************\
*  Purpose: scan the coord database, find all the 'D' cards and
*	    create on screen.
*
*  Author:	Bill E. Rogers Aug/94
*
*  Called by: psDrawOptions
*
\**************************************************************************/
{
  PSCoordFileRecord keyvalrecord;  /* storage for db_search key values    */
  PSCoordFileRecord *drawvalrec;   /* ptr to key for bus search		  */
  PSCoordFileRecord *nextrec;	   /* ptr assigned to address of next rec */
  PSCoordFileRecord *drawrec;	   /* ptr assigned to address of next rec */

  GraphElement	    *pnew_vertex = NULL;
  GraphElement	    *plast_vertex = NULL;
  DB_STAT stat;

  char cx[256];
  char cy[256];

  int i;

  if( coord_db_not_valid( "DRAW", 1 ) ) return (1);

  drawvalrec = &keyvalrecord;    /* set the pointer to data field */
                                /* (this is also the search key) */

  strncpy(drawvalrec->draw.key, PSKEY_DRAW, 1 );
  stat = db_search( &pscor_db, &drawvalrec, &nextrec, COORD_KEY );

  while(stat == D_FOUND)
  {
    for( i=0; i<PSMAXPENS; i++ )	/* PSMAXPENS defined in coord_data.h */
    {
      if( nextrec->draw.pt[i].up_dn == ' ' ||	/* blank entry    */
	  nextrec->draw.pt[i].up_dn == '0' )	/* ignore this pt */
      {
	/* ignore */
      }
      else if( nextrec->draw.pt[i].up_dn == '1' ||	/* move w/ pen down */
	       nextrec->draw.pt[i].up_dn == '2' )	/* move w/ pen up   */
      {
        add_decimal( &nextrec->draw.pt[i].x[0], 2, 6,cx );
        add_decimal( &nextrec->draw.pt[i].y[0], 2, 6,cy );

	create_and_manage_coord_vertex( atof_cnt_zero(cx,6),
					atof_cnt_zero(cy,6),
					&pnew_vertex,
					GraphClassVertexDrawPoint,
					REL );

	/*   DRAW WITH PEN DOWN CREATES A LINE SEGMENT	*/
        if( nextrec->draw.pt[i].up_dn=='1' )		/* pen down */
	{						/* (drawing line) */
          if( plast_vertex != NULL )
	    create_and_manage_coord_edge( plast_vertex, pnew_vertex,
						 GraphClassEdgeDraw, 0 );
	}

	plast_vertex = pnew_vertex;
      }
      else	/* bad draw card format */
      {
        char err_tmplt[PSMAXPENS*sizeof(PSPen_pts)+3];

	strcpy( err_tmplt, "D XXXXxxYYYYyy XXXXxxYYYYyy XXXXxxYYYYyy XXXXxxYYYYyy XXXXxxYYYYyy XXXXxxYYYYyy " );
	err_tmplt[(i+1)*sizeof(PSPen_pts)+1] = '*';
        printf("** INFO ** DRAW card < %d characters? Or bad pen up/down character at asterisk.\n%s\n%s\n",
		PSMAXPENS*sizeof(PSPen_pts)+2, err_tmplt, nextrec->draw.key);
      }
    }

    drawrec = nextrec;
    stat = db_next( &pscor_db, &drawrec, &nextrec, COORD_KEY );
    if(stat != D_FOUND) {
      break;
    }

    if( nextrec->draw.key[0] != 'D' )
    {
      stat = D_NOTFOUND;
    }
  } 
}


void psUserCreateDrawVertex( int x, int y, GraphElement **ge )
/**************************************************************************\
*  Purpose: Answers tbAddDrawLine (toolbox.c)
*	    Creates a DRAW vertex from mouse pushbutton event
*
*  Author:	Bill E. Rogers Feb/96
*
*  Called by: addDrawLine (toolbox.c)
*
\**************************************************************************/
{
  double xf,yf;
  GraphElement *pdrawv;

  PSCoordFileRecord keyvalrecord;  /* storage for db_search key values    */
  PSCoordFileRecord *drawvalrec;   /* ptr to key for bus search		  */
  PSCoordFileRecord *nextrec;	   /* ptr assigned to address of next rec */
  PSCoordFileRecord *drawrec;	   /* ptr assigned to address of next rec */

  DB_STAT stat;

  xf = (x - SCRN_BORDER-paperOffsetX/zoom_factor)/PixelsPerCm;
  yf = (plot_pixels_ht - y + SCRN_BORDER + paperOffsetY/zoom_factor )
		/PixelsPerCm;

  create_and_manage_coord_vertex( xf, yf, &pdrawv,
                                  GraphClassVertexDrawPoint,
                                  ABS );
 
  *ge = pdrawv;
}

void psUserEndDrawEdge( int x, int y )
/**************************************************************************\
*  Purpose: Answers tbAddDrawLine (toolbox.c)
*	    Creates a DRAW line from mouse pushbutton event
*
*  Author:	Bill E. Rogers Feb/96
*
*  Called by: drawAreaButtonCB
*
\**************************************************************************/
{
  double xf,yf;
  GraphElement *pdrawv;

  PSCoordFileRecord keyvalrecord;  /* storage for db_search key values    */
  PSCoordFileRecord *drawvalrec;   /* ptr to key for bus search		  */
  PSCoordFileRecord *nextrec;	   /* ptr assigned to address of next rec */
  PSCoordFileRecord *drawrec;	   /* ptr assigned to address of next rec */

  DB_STAT stat;

}


void pscreate_plot_form ()
/**************************************************************************\
*  Purpose: standard method to create a new form - 3 parts
*
*  Author: Bill Rogers - Jan 7, 1997
\**************************************************************************/
{
  pscreatePaperEdges    ();
  pscreateBorderEdges   ();
  pscreateLabelBox	();
/*  pscreateLegend(); */

}



void   pscreatePaperEdges	()
/**************************************************************************\
*  Purpose: get the PAPER SIZE and draw outline on screen.
*
*  Author:	Bill E. Rogers Sept/94
*
*  Called by: apply_files (filedlgrtn.c)
*
\**************************************************************************/
{

 /* test for now */
  paper_x_offset = 0.0;	/* too be used later when drag paper is functional */
  paper_y_offset = 0.0;
 
  create_and_draw_box( paper_x_offset,		/* NW corner */
	     paper_y_offset,
	     x_wd + paper_x_offset,		/* SE corner */
	     y_ht + paper_y_offset,
	     ABS,				/* no scaling */
	     GraphClassPaperVertex,
	     GraphClassPaperEdge,
	     DASH );

}


void   pscreateBorderEdges	()
/**************************************************************************\
*  Purpose: scan the coord database, find all the BORDER card and
*	    create on screen.
*
*  Author:	Bill E. Rogers Sept/94
*
*  Called by: apply_files (filedlgrtn.c)
*
\**************************************************************************/
{
  double x1,y1;
  char str[32];

  /* ALWAYS put a border around the paper's edge */

  create_and_draw_box( STANDARD_BORDER,
			y_ht - STANDARD_BORDER,
			x_wd - STANDARD_BORDER,
			BOTTOM_BORDER,
			ABS,
			GraphClassVertexBorder,
			GraphClassEdgeBorder,
			SOLID );

  /* additional border - if requested (that's the way Dave Stefonek does it!) */
  lookup_and_get_field( "print_border_xpos_text", str, 6 );
  x1 = atof_cnt( str, 6 );
  
  if( x1>0.0 )
  {
    lookup_and_get_field( "print_border_ypos_text", str, 6 );
    y1 = atof_cnt( str, 6 );

    create_and_draw_box( STANDARD_BORDER,
                         BOTTOM_BORDER,
                         x1*xscale-STANDARD_BORDER,
                         y1*yscale-BOTTOM_BORDER,
                         ABS,
                         GraphClassVertexBorder,
                         GraphClassEdgeBorder,
                         SOLID );
  }
}


int    pscreateLabelBox()
/**************************************************************************\
*  Purpose:
*	  
*
*  Author:	Bill E. Rogers Sept/94
*
*  Called by: apply_files (filedlgrtn.c)
*
\**************************************************************************/
{
  Widget id;
  char x1_str[32], y1_str[32], x2_str[32], y2_str[32];
  double x1,y1,x2,y2;
  double text_ht=0.3;
  Boolean header_not_processed;
  GraphElement *pvertex;

  char outbuf[BUFSIZE];    /* contains messages to powerflow (8192 chars)*/
  char inbuf [BUFSIZE];    /* contains messages from powerflow */
  char *psubstr;           /* ptr to sub-string - used by strtok to cut
                                   up the inbuf string */
  char *pcmmtstr;
  char id_str[1000];
  int n;


  if( coord_db_not_valid( "BOX", 1 ) ) return (1);

  lookup_and_get_field( "label_box_x", x1_str, 6 );
  x1 = atof_cnt( x1_str, 6 );

  lookup_and_get_field( "label_box_y", y1_str, 6 );
  y1 = atof_cnt( y1_str, 6 );

  lookup_and_get_field( "label_box_se_x", x2_str, 6 );
  if( strlen( x2_str ) )
    x2 = atof_cnt( x2_str, 6 );
  else
    x2 = (x_wd - STANDARD_BORDER)/xscale;  /* this side counterscaled */

  sprintf(x2_str, "%6.2lf", x2 );

  lookup_and_get_field( "label_box_se_y", y2_str, 6 );
  if( strlen( y2_str ) )
    y2 = atof_cnt( y2_str, 6 );
  else
    y2 = BOTTOM_BORDER/yscale;  /* this side counterscaled */;

/*  printf("Label box:(%s) (%s) (%s) (%s)  \n", x1_str,y1_str,x2_str,y2_str );*/

  create_and_draw_box( x1, y1, x2, y2, REL, GraphClassVertexLabelCorner,
				  GraphClassEdgeLabel, SOLID );

/*****************************************************************************/
  x1 += .2/xscale;
  y1 -= .3/yscale;  /* move up - must space down 1 line for each new line */

  sprintf( outbuf, "/get_data,type=COMMENTS\n%s\n", ENDOFMSG );

  ipc_synch_rw( inbuf, outbuf);         /* transmit inbuf to powerflow, */

  psubstr = strtok( inbuf, SEPCHARS );
  id_str[0] = '\0';
  header_not_processed = TRUE;

  while( psubstr != NULL )		/* now get all the lines of data */
  {
      /* check for the logical end of the message */
    if( !strncmp( psubstr, ENDOFMSG, sizeof(ENDOFMSG)) )
        break;

    if ( !strncmp ("CASE_ID", psubstr, 7 ) )
    {
      /* strcat( id_str, psubstr );					   */
      /* pscreateStringVertex( x1, y1, id_str, GraphClassVertexLabelText); */
    }

    else if( !strncmp ("CASE_DS", psubstr, 7 ) )
    {  /* strcat( id_str, psubstr );	*/ }

    else if( !strncmp ("/", psubstr, 1 ) )
    { /* get rid of command and status returned  */ }

    else if( !strncmp ("H", psubstr, 1 ) )
    {
      if( header_not_processed )
      {
        strcpy( id_str, psubstr );
	build_label_header( id_str );

	/* y1 -= (text_ht+VERT_GAP)/yscale; */
	pscreateStringVertex( x1 - x_cm_offset,
			      y1 + y_cm_offset,
			      id_str, GraphClassVertexLabelText);
	header_not_processed = FALSE;
      }
    }

    psubstr = strtok( NULL, SEPCHARS );
  }

  n = 0;

  if(( HashLookup( "pf_comments_text", &id )) != FALSE)
  {
    char *c;

    y1 -= (text_ht+VERT_GAP)/yscale;
    pcmmtstr = XmTextGetString(id);
    pscreateStringVertex( x1 - x_cm_offset,
			  y1 + y_cm_offset,
			  pcmmtstr, GraphClassVertexLabelText);

    for( c=pcmmtstr; *c!='\0' ; c++ )	/* find # of cr's to adjust next line */
    {
      if( *c == '\n' ) n++; 
    }
  }

  if(( HashLookup( "user_comments_text", &id )) != FALSE)
  {
    y1 -= n*(text_ht+VERT_GAP)/yscale;
    pcmmtstr = XmTextGetString(id);
    pscreateStringVertex( x1 - x_cm_offset,
			  y1 + y_cm_offset,
			  pcmmtstr, GraphClassVertexLabelText);
  }

/************************************************************************/
/*    Diagram line at very bottom					*/

    strcpy( id_str, "Diagram Type:              Plotted:          Time:            Version:            Powerflow run at:         " );
    pscreateStringVertex( .5/xscale-x_cm_offset, .4/yscale+y_cm_offset,
				 id_str, GraphClassVertexLabelText);
}


void build_label_header( char *id_str )
/****************************************************************************\
* Purpose: Digest 1st header string returned from powerflow,
*	   Find the 4 key words (CaseID, Date, Powerflow, Case)
*	   Extract the correct words in between (some start with offsets).
*	   Build diagram label string (occording to BPA users)
*
*	It's crazy programming - but it works for now.
*
* Author: Bill Rogers - Aug 31, 1995 (Happy 47th birthday)
\****************************************************************************/
{
  char *word[20],
       *limit1 = "#",
       *limit2 = ":,= \t\n";
  int nwrd;
  char *s_str[4] ={ "CaseID", "Date", "Powerflow", "Case"};
  int  s_str_l[4] = { 6, 4, 9, 4 };
  int  wrdx[4] = { 1, 1, 2, 1 };
  int  wrd1[4], wrd2[4];
  int i,j;

  uscanc( id_str, word, &nwrd, limit1, limit2 );
  strcpy( id_str, "      " );

  for( i=0; i<4; i++ )
  {
    wrd1[i]=-1;		/* default to "not found" */
    for( j=0; j<nwrd; j++ )
    {
      if( strncmp( word[j], s_str[i], s_str_l[i] ) == 0 &&
			 strlen( word[j] ) == s_str_l[i] )
      {
	wrd1[i] = j;
	break;
      }
    }
  }

  wrd2[0] = wrd1[3]-1;	/* stop before Case Description */
  wrd2[1] = nwrd-1;	/* stop at last word */
  wrd2[2] = wrd1[0]-1;	/* stop before CaseID */

  for( i=0; i<3; i++ )	/* strcat CaseId, Date & Version */
  {
    if( wrd1[i]>=0 && wrd1[i]+wrdx[i]<=wrd2[i])	/* safety check just in case */
    {
      for( j=wrd1[i] +wrdx[i]; j<=wrd2[i]; j++ )
      {
	strcat( id_str, word[j] );
	if( i != 1 ) strncat( id_str, " ", 2 );
      }
      strncat( id_str, "   ", 4 );
    }
    else
    {
     if( wrd1[i]+wrdx[i]>wrd2[i]+1)
     {
      printf("*** WARNING *** from build_label_header (graphpscor.c)\n" );
      printf("  Powerflow label data not consistant with GUI expectations.\n");
      printf("  (Parts of screen label data will be missing.)\n" );
     }
    }
  }

}

GraphElement *pscreateStringVertex( double x, double y, char *str,
					 int vertex_class )
/****************************************************************************\
* Purpose: Create string GraphElement AND MOTIF vertex.
*
* Called by: pscreateCommentVertex
\****************************************************************************/
{
  GraphElement updatedata;
  GraphElement *pupdatedata = &updatedata;
  GraphElement *pnew_vertex = NULL;
  XmString strx;

  int width, height;

  createGraphVertex( vertex_class,
		     str,
		     pscoord_to_MOTIF_x_flt( x, "label_data" ),
		     pscoord_to_MOTIF_y_flt( y, "label_data" ),
		     ' ',
		     &pnew_vertex );

  assert(pnew_vertex != NULL);

  manageVertex ( pnew_vertex);

  strx = XmStringCreateLtoR( str, XmSTRING_DEFAULT_CHARSET );
  if(pnew_vertex->wid != NULL)
  {
    get_str_box_pixel_size( str, &width, &height );

    XtVaSetValues( pnew_vertex->wid, 
		   XmNwidth, width,
		   XmNheight, height,
	           XmNlabelString, strx, 
	           NULL);
  }

  return pnew_vertex;
}
 

void get_box_size_of_string( char *str, int *n_wide, int *n_high )
/***************************************************************************\
* Purpose: search character string
*
\***************************************************************************/
{
  int cnt;
  char *a;

  *n_wide = 1;
  *n_high = 1;
  cnt    = 0;
  a = &str[0];

  while ( *a != '\0' )
  {
    if( *a == '\n' )
    {
      *n_wide = max( *n_wide, cnt );
      (*n_high)++;
      cnt = 0;
    }
    else
    {
       cnt++;
    }
    a++;
  }

  *n_wide = max( *n_wide, cnt );
}


#ifdef LEGEND
void   pscreateLegend		()
/*************************************************************************\
*  Purpose:	Dave's Stefonik's idea .... not generally accepted by 
*				users.
*
*  Author:	Bill E. Rogers Sept/94
*
*  Called by:
*
\**************************************************************************/
{
Widget draw_id;
Window wid;
Display *dpy;

  char x1_str[7], y1_str[7], x2_str[7], y2_str[7];
  double x1,y1;

  lookup_and_get_field( "legend_x_text", x1_str, 6 );
  if( strlen(x1_str) )
  {
    x1 = atof_cnt( x1_str, 6 );

    lookup_and_get_field( "legend_y_text", y1_str, 6 );
    y1 = atof_cnt( y1_str, 6 );
 
    create_and_draw_box( x1, y1,
			 x1 + LEGEND_WIDTH,
			 y1 - LEGEND_HEIGHT,
			 REL,
			 GraphClassVertexLegendCorner,
		 	 GraphClassEdgeLegend, SOLID );

  if(( draw_id = widget_id( "drawwindow" )) == FALSE) {
    printf("Warning: creategraphtbl: cannot find drawwindow\n");
    return;
  }
  dpy = XtDisplay( draw_id );
  wid = XtWindow ( draw_id );

    x1 += .7;
    y1 -= .6;
/*printf("DRAWing LEGEND at %d,%d\n", (int)x1, (int)y1 );
    drawChars( dpy, wid,  "LEGEND", 25, 200, 1.5, 0.0 );*/
    pscreateStringVertex( x1, y1, "LEGEND", GraphClassVertexLegendText );

    x1 -= .6;
    y1 -= 0.5;
    pscreateStringVertex( x1, y1, "  0-<100 kv", GraphClassVertexLegendText );

    y1 -= 0.8;
    pscreateStringVertex( x1, y1, "100-<200 kv", GraphClassVertexLegendText );

    y1 -= 0.8;
    pscreateStringVertex( x1, y1, "200-<231 kv", GraphClassVertexLegendText );

    y1 -= 0.8;
    pscreateStringVertex( x1, y1, "231-<500 kv", GraphClassVertexLegendText );

    y1 -= 0.8;
    pscreateStringVertex( x1, y1, "500kv and up",  GraphClassVertexLegendText );

  }

}
#endif


void   pscreateCommentVertexes	()
/**************************************************************************\
*  Purpose: scan the coord database, find all the 'C' cards and
*	    create on screen.
*
*  Author:	Bill E. Rogers Sep/94  Modifications feb/95
*
*  Called by: psDrawOptions
*
\**************************************************************************/
{
  PSCoordFileRecord cmmt_rec;	  /* storage for db_search key values	 */
  PSCoordFileRecord *pcmmt_rec;	  /* ptr to key for bus search		 */
  PSCoordFileRecord *nextrec;	  /* ptr assigned to address of next rec */
  PSCoordFileRecord *drawrec;	  /* ptr assigned to address of next rec */
  PSCoordFileRecord *fontrec;	  /* ptr assigned to address of next rec */

  GraphElement	    *pnew_vertex = NULL;
  GraphElement	    *font_vertex = NULL;
  GraphElement	    *org_comment_vtx = NULL;

  DB_STAT stat;

  double tht;
  char coord_x[256];
  char coord_y[256];
  char comment_str[256];
  Boolean font_flg = FALSE;
  double last_x, last_y;
  int num_com;
  XmString xstr;

/*  psprintcoord_db( 'A' ); debug stuff*/

  if( coord_db_not_valid( "COMMENT_VERTEXES", 1 ) ) return;

  num_com = 0;
  tht = 0.0;
  last_x = (double)999.0;	/* all set if new coord file is loaded */
  last_y = (double)999.0;	/* all set if new coord file is loaded */

  pcmmt_rec = &cmmt_rec;    /* set the pointer to data field */
                                /* (this is also the search key) */

  strncpy(pcmmt_rec->comment.key, PSKEY_COMMENTS, 1 );
  pcmmt_rec->comment.idx = 0;
  stat = db_search( &pscor_db, &pcmmt_rec, &nextrec, COORD_KEY_IDX );
  if(stat == D_EOF) {
     /* no records for this type found so return */
     return;
  }
  if(stat == D_NOTFOUND) {
    /* ok so no record found with key == comments AND idx == 0 */
    /* so now see if some record was returned anyway */
    if(strncmp(nextrec->comment.key, PSKEY_COMMENTS,
				     sizeof(nextrec->comment.key)) == 0)
    {
       stat = D_FOUND;
    }
  }

  while(stat == D_FOUND)
  {
    if( nextrec->comment.subkey[0] == 'P' )	/* Font record */
    {
      font_flg = TRUE;
      font_vertex = pscreateStringVertex( (int)15, (int)15,
		 nextrec->comment.key, GraphClassVertexFont ); 

      make_vertex_invisible( font_vertex );
    }
    else
    {
      /*** if missing comment coordinates - then compute it from last one ***/
      if( strncmp( &nextrec->comment.x[0], "      ", 6 ) == 0 )
      {
        if( last_x != (double)999.0 )
        {
          sprintf( coord_x, "%6.2lf", last_x );	  /* change float to char */
          sprintf( coord_y, "%6.2lf", last_y - (tht+VERT_GAP)/yscale );
          strncpy( &nextrec->comment.x[0], coord_x, 6 );
          strncpy( &nextrec->comment.y[0], coord_y, 6 );
        }
        else
        {
	printf("*** WARNING *** from pscreateCommentVertexes (graphpscor.c)\n");
	printf("    Missing x,y coord from comment card should be preceeded\n");
	printf("    by a COMMENT card with non-blank x,y coordinates.\n");
	printf("(%s)\n", &nextrec->comment.key[0] );
        }
      }

      add_decimal( &nextrec->comment.x[0], 2, 6,coord_x ); /* char to char */
      add_decimal( &nextrec->comment.y[0], 2, 6,coord_y );
 
      last_x = atof_cnt_zero ( coord_x, 6 );		 /* char to float */
      last_y = atof_cnt_zero ( coord_y, 6 );

      memset(comment_str, '\0', sizeof(comment_str));
      strncpy( comment_str, nextrec->comment.text, sizeof(nextrec->comment.text));

      clip_trailing_blanks( comment_str );

      pnew_vertex = pscreateStringVertex( last_x, last_y, comment_str,
			GraphClassVertexComment ); 

      tht = text_vertex_ht( pnew_vertex->wid );

      /******************  FONT & ORGINAL DATA LINKAGE ******************/
      if( font_flg )
      {
        addGraphLink( pnew_vertex, font_vertex );
        font_flg = FALSE;
      }

/*    Create original (editable) comment string */
      org_comment_vtx = pscreateStringVertex( last_x, last_y, comment_str,
					GraphClassVertexOrgComment ); 
      make_vertex_invisible( org_comment_vtx );
      addGraphLink( pnew_vertex, org_comment_vtx );
    }

    drawrec = nextrec;
    stat = db_next( &pscor_db, &drawrec, &nextrec, COORD_KEY_IDX );
    if(stat != D_FOUND)
    {
      break;		/* end loop if no more comment cards */
    }

    if( strncmp( nextrec->comment.key, PSKEY_COMMENTS, 1 ) )
    {
      stat = D_NOTFOUND;
    }

  }
}


void   userCreateComment( XmPushButtonCallbackStruct *call_data )
/**************************************************************************\
*  Purpose: creates a comment from a button click
*	   
*
*  Author:	Bill E. Rogers Apr/96
*
*  Called by: drawAreaButtonCB (toolbox.c)
*
\*************************************************************************/
{
  GraphElement	    *pnew_vertex = NULL;
  GraphElement	    *porg_vertex = NULL;
  XEvent *event = call_data->event;

  pnew_vertex = pscreateStringVertex( MOTIF_to_ps_x( event->xbutton.x,"","",""),
				      MOTIF_to_ps_y( event->xbutton.y,"","",""),
				      "New User Comment",
				      GraphClassVertexComment ); 

  porg_vertex = pscreateStringVertex( MOTIF_to_ps_x( event->xbutton.x,"","",""),
				      MOTIF_to_ps_y( event->xbutton.y,"","",""),
				      "New User Comment",
				      GraphClassVertexOrgComment ); 

  make_vertex_invisible( porg_vertex );
  addGraphLink( pnew_vertex, porg_vertex );
}


void make_vertex_invisible( GraphElement *ge)
/***************************************************************************\
* Purpose: Unmanages vertex - without destroying it.   It's still there
*	   but does not appear on screen display.
*
* Author: Bill Rogers/Dan Clark		Feb 1995
\***************************************************************************/
{

  XtUnmanageChild( ge->wid );

  updateGraphDisplay( ge, GraphDisplayOff );

}


double text_vertex_ht( Widget wid )
/**************************************************************************\
*  Purpose: returns the height of a text widget in CM
*
\**************************************************************************/
{
  Dimension text_ht, border_wd, shadow_wd, hilite_wd;

  if(!wid) {
    printf("Warning - cannot set attributes of widget if not managed\n");
    return((double)1.0);
  }
  XtVaGetValues( wid, XmNheight, &text_ht,
		      XmNborderWidth, &border_wd,
		      XmNshadowThickness, &shadow_wd,
		      XmNhighlightThickness, &hilite_wd,
		      NULL );

  return ((double)((double)text_ht + (double)border_wd +
		 (double)shadow_wd + (double)hilite_wd )/(double)pixelsPerUnit);

}




Boolean coord_db_not_valid( char *caller, int flag )
/**************************************************************************\
*  Purpose: Standardized check if coord db is available.
*
*  Author:	Bill E. Rogers Sept/94
*
*  Called by: pscreateBorderEdges (above)
*	      pscreatePaperEdges  (above)
*	      processDrawCards	  (above)
*
\**************************************************************************/
{
  if(db_valid_db(&pscor_db) != D_OKAY)
  {
    if ( flag )
    { 
      printf(" *** WARNING *** pscor_db is empty\n" );
      printf("       Called by: %s.\n", caller );
    }
    return( True );
  }
  else
    return( False );
}


void create_and_manage_coord_vertex( double x, double y,
		 GraphElement **ppnew_vertex, GraphClass class, int abs_rel )
/**************************************************************************\
*  Purpose: 
*
*  Author:	Bill E. Rogers Sept/94
*
*  Called by: create_and_draw_box
*		processDrawCards
*
\**************************************************************************/
{
  GraphElement *pnew_vertex;
  char cx[256],cy[256];
  int xi, yi;

  DB_STAT stat;			 /* status returned for general db access */

  if( class == GraphClassVertexLabelCorner )
  {
     x-=x_cm_offset;	/* counteract the effects of pscoord_to_MOTIF stuff */
     y+=y_cm_offset;	/* offsets should not affect label box */
  }

  if( abs_rel == ABS )		 /* i.e. don't SCALE this! */
  {
    sprintf(cx, "%6.2lf", x );
    sprintf(cy, "%6.2lf", y );

    xi = pscoord_to_MOTIF_x_abs( cx, 6 );
    yi = pscoord_to_MOTIF_y_abs( cy, 6 );
  }
  else 
  {
    xi = pscoord_to_MOTIF_x_flt( x, "option_pt" );
    yi = pscoord_to_MOTIF_y_flt( y, "option_pt" );
  }

  create_and_manage_scrn_vertex( "boxcornr230.", xi, yi, &pnew_vertex, class );

  *ppnew_vertex = pnew_vertex;
}

void create_and_manage_scrn_vertex( char *name, int x, int y,
                 GraphElement **ppnew_vertex, GraphClass class )
/*****************************************************************************\
* Purpose: Very similiar to create_and_manage_coord_vertex
*          EXCEPT: x, y values are in absolute PIXELS  (not cm)
*
*
*
\*****************************************************************************/
{
  GraphElement updatedata;
  GraphElement *pupdatedata = &updatedata;
  GraphElement *pnew_vertex;

  DB_STAT stat;                  /* status returned for general db access */

  createGraphVertex( class, name, x, y, ' ', &pnew_vertex );

  assert(pnew_vertex != NULL);

/* NOTE: this is a very meaningless piece of code if - manageVertex
* 	 is called afterwards.  manageVertex sets display to
*	 GraphDisplayON !!!!!
*  pupdatedata      = &updatedata;
*  memcpy( pupdatedata, pnew_vertex, sizeof( GraphElement ) );
*  pupdatedata->display = GraphDisplayRequestOn;
*  stat = db_update( &graph_db, &pnew_vertex, &pupdatedata );
*  assert(D_OKAY == stat);
*/
  manageVertex ( pnew_vertex );

  *ppnew_vertex = pnew_vertex;

}


void create_and_manage_coord_edge( GraphElement *vertex_1,
				   GraphElement *vertex_2,
				   GraphClass class,
				   Boolean dash_flg )
/**************************************************************************\
*  Purpose: 
*
*  Author:	Bill E. Rogers Sept/94
*
*  Called by: create_and_draw_box
*		processDrawCards
*
\**************************************************************************/
{
  GraphElement graphedge;
  GraphElement *pgraphedge  = &graphedge;
  DB_STAT stat;			 /* status returned for general db access */

  pscreateGraphCorEdge( vertex_1, vertex_2,
        " ",
	class,
	0,
	&pgraphedge );

  manageEdge( pgraphedge );

  setEdgeDash( pgraphedge, dash_flg );
}


void create_and_draw_box( double x1, double y1, double x2, double y2,
		int type, GraphClass v_class, GraphClass e_class, Boolean dash )
/****************************************************************************\
* Purpose: standard box drawing routine.
*
* Author: William E. Rogers	9/22/94
*
\****************************************************************************/
{
  PSCoordFileRecord nextrec;			/* allocate... */
  PSCoordFileRecord *pnextrec = &nextrec;	/* ptr to address of nextrec*/

  GraphElement sw_vertex;
  GraphElement nw_vertex;
  GraphElement ne_vertex;
  GraphElement se_vertex;
  GraphElement *psw_vertex = &sw_vertex;
  GraphElement *pnw_vertex = &nw_vertex;
  GraphElement *pne_vertex = &ne_vertex;
  GraphElement *pse_vertex = &se_vertex;

  strncpy( pnextrec->cor.key, "D", 1 );
 
  create_and_manage_coord_vertex( x1, y1, &pnw_vertex, v_class, type);
  create_and_manage_coord_vertex( x2, y1, &pne_vertex, v_class, type);
  create_and_manage_coord_vertex( x2, y2, &pse_vertex, v_class, type);
  create_and_manage_coord_vertex( x1, y2, &psw_vertex, v_class, type);

  if( psw_vertex->x>=paper_border &&
      pnw_vertex->x>=paper_border &&
      psw_vertex->y>=paper_border)
    create_and_manage_coord_edge( psw_vertex, pnw_vertex, e_class, dash );

  if( pnw_vertex->y>=paper_border &&
      pne_vertex->y>=paper_border &&
      pne_vertex->x>=paper_border)
    create_and_manage_coord_edge( pnw_vertex, pne_vertex, e_class, dash );

  if( pne_vertex->x>=paper_border &&
      pse_vertex->x>=paper_border && 
      pse_vertex->y>=paper_border )
    create_and_manage_coord_edge( pne_vertex, pse_vertex, e_class, dash );

  if( pse_vertex->y>=paper_border &&
      psw_vertex->y>=paper_border &&
      pse_vertex->x>=paper_border)
    create_and_manage_coord_edge( pse_vertex, psw_vertex, e_class, dash );

}


void psfindGraphMax ( )
/****************************************************************************\
* FUNCTION: Looks through coord file data and determines
*		  maximum y-value and minimum x-value (in cm).
*
*	Called by: psbuildGraphCoord
*
\****************************************************************************/
{
  PSBranchRecord   *coordbranchrec;	/* ptr to key for branch search	    */
  PSBusRecord	   *coordbusrec;	/* ptr to key for bus search	    */
  PSCommentRecord  *coordcomrec;	/* ptr to key for bus search	    */
  PSDrawRecord	   *drawrec;		/* ptr to key for bus search	    */
  PSCoordFileRecord *coordnextrec;	/* ptr returned for next bus rec    */
  PSCoordFileRecord *coordprevrec;	/* ptr returned for next bus rec    */

  DB_STAT stat;		/* status returned for bus queries of coord db	*/

  int max_y_pxl, min_x_pxl;
  double paper_wd, paper_ht;
  int i, nbend;

  min_x = 99999.0;
  max_y = 0.0;

  /* assume the calling routine has already done all the preliminary checks */
  /* for correct, and filled coord_db 					    */

  stat = db_first ( &pscor_db, &coordnextrec, COORD_KEY );

  while(stat == D_FOUND)	/* LOOP THROUGH ALL COORD RECORDS */
  {
    switch ( coordnextrec->cor.key[0] )
    {
      case 'B':
	coordbusrec = (PSBusRecord *)coordnextrec;

	min_x_value_char( coordbusrec->symbol.x, -BUS_RAD ); /* set min_x */
	max_y_value_char( coordbusrec->symbol.y,  BUS_RAD ); /* set max_y */
			
	min_x_value_char( coordbusrec->namepos.x, 0         );
	max_y_value_char( coordbusrec->namepos.y, 2*BUS_RAD );
      break;

      case 'L':
      case 'T':
        coordbranchrec = (PSBranchRecord *)coordnextrec;
	nbend = psbendCount( coordbranchrec );
        for (i = 0; i<nbend; i++ )
        {
	  min_x_value_char( coordbranchrec->bend[i].x, 0 );
	  max_y_value_char( coordbranchrec->bend[i].y, 0 );
	}
      break;

      case 'C':
        coordcomrec = (PSCommentRecord *)coordnextrec;
	min_x_value_char( coordcomrec->x, 0 );
	max_y_value_char( coordcomrec->y, 2*BUS_RAD );
      break;

      case 'D':
        drawrec = (PSDrawRecord *)coordnextrec;
	i = 0;
	while( strlen ( drawrec->pt[i].x ) >=6  &&
	       strncmp( drawrec->pt[i].x, "      ", 6 ) &&
		i<PSMAXPENS)
	{
	  min_x_value_char( drawrec->pt[i].x, 0 );
	  max_y_value_char( drawrec->pt[i].y, 0 );
	  i++;
	}
      break;

    }

    coordprevrec = coordnextrec;
    stat = db_next ( &pscor_db, &coordprevrec, &coordnextrec, COORD_KEY);

  } /* end coord file loop */

  /* compute maximum pixel values */
  		/* assuming B,L,D,C card values are adjusted */
  min_x_pxl = x_flt_2_pxl( min_x );
  max_y_pxl = y_flt_2_pxl( max_y );

/*  printf("Max y coord:%6.2f  Min x coord: %6.2f\n", max_y, min_x ); */

  get_paper_size( &paper_wd,    &paper_ht,
		  &xscale,      &yscale,
		  &x_cm_offset, &y_cm_offset );

  paper_ht *= pixelsPerUnit;

  if( max_y_pxl > paper_ht && shift_paper_as_needed )
  {
    paperOffsetY = max_y_pxl - paper_ht;
    /* printf("** INFO ** Paper shifted down because some values are off the paper.\n" );
    printf( "Paper Y offset by %d pixels.\n", paperOffsetY ); */
  }
  else
    paperOffsetY = 0;

  if( min_x_pxl < 0 && shift_paper_as_needed )
  {
    paperOffsetX = -min_x_pxl;
  /*    printf("** INFO ** Paper shifted right because some values are off the paper.\n" );
    printf( "Paper X offset by %d pixels.\n", paperOffsetX ); */
  }
  else
    paperOffsetX = 0;
}

void min_x_value_char( char *cm, int adj )
/**************************************************************************\
* Purpose compare character pointer against a previous minimum value
*	  and sets min_x whichever is lower
*
*	cm  = address of six-char value to compare.
*	adj = special case adjustments.
*
* Note: Global value min_x must be set externally before the calls begin!
\**************************************************************************/
{

  if( atof_cnt_zero( cm, 6 ) + adj < min_x )

    min_x = atof_cnt_zero( cm, 6 ) + adj;

}

void max_y_value_char( char *cm, int adj )
/**************************************************************************\
* Purpose: Same as above except, find max Y.
* Note: Global value max_y must be set externally before the calls begin!
\**************************************************************************/
{

  if( atof_cnt_zero( cm, 6 ) + adj > max_y )

    max_y = atof_cnt_zero( cm, 6 ) + adj;

}

int x_flt_2_pxl( double x )
/**************************************************************************\
* Purpose: Standard (cm)-to-pixel conversion of a plot point
*
\**************************************************************************/
{
  double x_pxl_ft;
  int x_pixels;

  x_pxl_ft = x + x_cm_offset;					/*(1)*/
  x_pxl_ft *= pixelsPerUnit;					/*(2)*/

  if( scaling_flg )
    x_pxl_ft *= xscale;						/*(3)*/

  if( x_pxl_ft >= 0 )
    x_pixels = (int)(x_pxl_ft +.5 );		/* pos direction */
  else
    x_pixels = (int)(x_pxl_ft -.5 );		/* neg direction */

  return( x_pixels );
}

int y_flt_2_pxl( double y )
/**************************************************************************\
\**************************************************************************/
{
  double y_pxl_ft;
  int y_pixels;

  y_pxl_ft = (y - y_cm_offset)*pixelsPerUnit;			/*(1)(2)*/

  if( scaling_flg )
      y_pxl_ft *= yscale;					/*(3)*/

  if( y_pxl_ft >= 0 )					/* round off */
    y_pixels = (int)( y_pxl_ft +.5 );
  else
    y_pixels = (int)( y_pxl_ft -.5 );

  return (y_pixels);

}


void findGraphMax ( )
/****************************************************************************\
* FUNCTION: Same as psfindGraphMax, except use GraphElements instead of
*		 Coord-file data.
*
*	Called by: rescale_graph
*
\****************************************************************************/
{
  GraphElement graphvertex;
  GraphElement *pgraphvertex;
  GraphElement *pgraphvertexnxt;
  GraphElement *pgraphvertexprev;

  DB_STAT stat;		/* status returned for bus queries of coord db	*/

  double paper_wd, paper_ht;

  min_x = 9999.0;
  max_y = 0.0;

  /* assume the calling routine has already done all the preliminary checks */
  /* for correct, and filled coord_db 					    */

  /*** check for a valid db **/
  if(db_valid_db(&graph_db) != D_OKAY) {
    return;              /* no graph table so there can't be any edges */
  }

  stat = db_first( &graph_db, &pgraphvertexnxt, GRAPH_TYPE );

  if (stat == D_EOF) {
    /* no vertexes/edges found just return */
    return;
  }

  while(stat == D_FOUND)	/* LOOP THROUGH ALL GRAPHELEMENTS */
  {
    switch ( pgraphvertexnxt->type )
    {
      case GraphTypeVertex:
        min_x = min( pgraphvertexnxt->x, min_x );
        max_y = max( pgraphvertexnxt->y, max_y );
      break;

    }
    pgraphvertex = pgraphvertexnxt;
    stat = db_next( &graph_db, &pgraphvertex, &pgraphvertexnxt, GRAPH_TYPE );

  } /* end loop */

  /* convert graphelement pixel values to cm */
  /*min_x = MOTIF_to_ps_x( min_x, &junk, &junk, &junk );*/
  max_y = (max_y - y_cm_offset + BUS_RAD - plot_pixels_ht)/PixelsPerCm;
  min_x = (min_x + y_cm_offset + BUS_RAD)/PixelsPerCm;

  get_paper_size( &paper_wd,     &paper_ht,
                  &xscale,      &yscale,
                  &x_cm_offset, &y_cm_offset );

  if( paper_ht < 0 && shift_paper_as_needed )
    paperOffsetY = (int)((max_y*yscale) * pixelsPerUnit);
  else
    paperOffsetY = 0;

  if( min_x < 0 && shift_paper_as_needed )
    paperOffsetX = (int)(-min_x * xscale * pixelsPerUnit);
  else
    paperOffsetX = 0;

}


void send_comments_and_defines_to_pf( )
/*********************************************************************** **\
* Purpose: Initializes and sends the DEFINE/COMMENTS to powerflow.
*
*
* Called by: psDrawOptions
\*************************************************************************/
{
  GraphElement graphnode;	  /* storage for db_search key values	 */
  GraphElement *pgraphnode;	  /* ptr to key for bus search		 */
  GraphElement *pgraphnodenxt;	  /* ptr assigned to address of next rec */
  char *pcmmt;
  XmString xstr;
  DB_STAT stat;
  int rtncde;
  int num_a;
  char  *pf_send_rec = outbuf_array;
  char  *str_rtn = inbuf_array;
  XmAnyCallbackStruct *cbx;
 
  pgraphnode = &graphnode;
  stat = db_valid_db(&graph_db); /*** check for a valid db **/
  if(stat == D_INVALID_DB)
  {
    printf("send_comments_and_defines..: info - no graph data\n");
		return;
  }

  sprintf(pf_send_rec, "/GET_DATA, TYPE = INIT_DEFINE\n%s\n", ENDOFMSG);
  rtncde = ipc_synch_rw(str_rtn, pf_send_rec);

  sprintf(pf_send_rec, "/GET_DATA, TYPE = LOAD_DEFINE\n");

  num_a = add_define_cards_to_str( pf_send_rec );

/*  printAllGraphData( 0, 0, 0 ); */

  /**** Get first ORGINAL COMMENT record ************/
  pgraphnode = &graphnode;
  pgraphnode->type = GraphTypeVertex;
  pgraphnode->class = GraphClassVertexOrgComment;
  pgraphnode->idx = (long)0;

 	/* ber note: most likely will get 1st index=0 and D_NOTFOUND */
  stat = db_search( &graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_CLASS_IDX);

  if(stat == D_EOF )
    printf("send_comments...: Info - no comments to send\n");
  else
  {
    if(pgraphnodenxt->class == GraphClassVertexOrgComment) {
	stat = D_FOUND;
    }
    /***** loop through all COMMENT graphelements **********************/
    while(stat == D_FOUND)
    {
      if( pgraphnodenxt->wid )
      {
        XtVaGetValues(pgraphnodenxt->wid, XmNlabelString, &xstr, NULL);
        pcmmt = cstring_to_string(xstr);

      /* Herein lies the source of a two week delay!!!!!   	    */
      /* XmStringFree(xstr);  Don't do this!  Kills the gadget str! */

        strcat(pf_send_rec, "C             ");

        if( pcmmt != NULL )
        {
  	  clip_outside_blanks( pcmmt );
	  strcat( pf_send_rec, pcmmt );
	  strcat(pf_send_rec, "\n");
	  num_a++;
        }
        else
        {
 printf("PROGRAMMER ERROR - send_comments_and_defines_to_pf (graphpscor.c)\n");
 printf("   Comment string is NULL!  No valid address for comments string.\n");
 printf("   This comment ignored!\n" );
        }
      }
      else
      {
	printf("*** WARNING **** from send_comments_and_define_to_pf (graphpscor.c)\n" );
	printf("         graphElement comment (%s) widget id is NULL!\n",
				 pgraphnodenxt->vertex_id );
      }
      /** Get next record **/
      pgraphnode = pgraphnodenxt;
      stat = db_next ( &graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_CLASS_IDX);
      if(pgraphnodenxt->class != GraphClassVertexOrgComment) {
			stat = D_NOTFOUND;
	}
    }
  }
  strcat( pf_send_rec, ENDOFMSG);
  strcat(pf_send_rec, "\n");
  if( num_a )
    rtncde = ipc_synch_rw(str_rtn, pf_send_rec); /* send DEFINES & COMMENTS */

}


void test_graphdata( Widget w, XtPointer tag,
                                 XmAnyCallbackStruct *cbs )
/*************************************************************************\
* Purpose: self-test package to test the routines in this module
*
* Author W.E. Rogers		Dec-1994
*
* Called by: Self test pulldown button (under "Process" of main menu)
\*************************************************************************/
{
  Widget junk_wid;
  XtPointer junktag;
  XtPointer junkreason;

  int input_0   = 0;
  int newbus_10 = 10;
  int net_file  = 3;
  int cor_file  = 4;

  printf("####### SELF TESTING graphdata.c module #######\n" );
  printf("###(Absense of warnings indicates test O.K.)###\n" );

  set_button_state( "print_portrait_rb", True );
  set_standard_paper_size();
  psgetHtWdScale();

  set_units_to_cm(); 		/* hardwire to use only cm data */
  set_origin_to_bottom();
  test_pscoord_to_MOTIF_x( );
  test_pscoord_to_MOTIF_y( );

  /* NEW FROM SCRATCH */
  VUIT_Manage("file_new_message_dia");
  create_from_scratch( 0, 0, 0 );
  VUIT_Unmanage("file_new_message_dia");
  set_button_state( "tools_newbus_rb", True );
  change_cursor_to( 0, &newbus_10, 0 );

  create_new_ipf_bus( w, junktag, junkreason ); /* manages bus input dialog */
  lookup_and_fill_field( "bus_front_name", "TESTLOAD", 0, 12 );
  lookup_and_fill_field( "bus_p_load", "10.0", 0, 4 );
  send_add_data_to_powerflow ( 0, 0, 0 );
  addGraphBusXY ( 100, 100 );

  create_new_ipf_bus( w, junktag, junkreason ); /* manages bus input dialog */
  lookup_and_fill_field( "bus_front_name", "ACEGIKMO", 0, 12 );
  send_add_data_to_powerflow ( 0, 0, 0 );
  addGraphBusXY ( 200, 100 );

  create_new_ipf_bus( w, junktag, junkreason ); /* manages bus input dialog */
  send_add_data_to_powerflow ( 0, 0, 0 );
  addGraphBusXY ( 200, 200 );

  create_new_ipf_bus( w, junktag, junkreason ); /* manages bus input dialog */
  lookup_and_fill_field( "bus_front_name", "TEST GEN", 0, 12 );
  lookup_and_fill_field( "bus_front_volts", "13.8", 0, 4);
  set_bus_type( 0, "S", 0 );
  send_add_data_to_powerflow ( 0, 0, 0 );
  addGraphBusXY ( 200, 400 );

  VUIT_Unmanage("bus_front_box");


 
/*  TEST BY SAVING AND RELOADING */
  VUIT_Manage("save_file_dialog");
  lookup_and_fill_field( "open_dia_coord_dir_text",
					 "system_test_save.cor", 0, 20 );
  lookup_and_fill_field( "save_coord_text",
					 "system_test_save.cor", 0, 20 );

  lookup_and_fill_field( "open_dia_network_dir_text",
					 "system_test_save.net", 0, 20 );
  lookup_and_fill_field( "save_net_text",
					 "system_test_save.net", 0, 20 );
  file_check_and_save_cb( 0, &cor_file, 0 );
  file_check_and_save_cb( 0, &net_file, 0 );
  VUIT_Unmanage("save_file_dialog");

  VUIT_Manage("file_new_message_dia");
  create_from_scratch( 0, 0, 0 );
  VUIT_Unmanage("file_new_message_dia");
  set_button_state( "tool_input_rb", True );
  change_cursor_to( 0, &input_0, 0 );

  VUIT_Manage("open_file_dialog");
  lookup_and_fill_field( "file_select_dia_coord_text",
					 "system_test_save.cor", 0, 20 );
  lookup_and_fill_field( "file_select_dia_network_text",
					 "system_test_save.net", 0, 20 );

  name_change_file_status_set ( 0, &cor_file, 0 );	/* reset coord file */
  name_change_file_status_set ( 0, &net_file, 0 );	/* reset netwk file */

  apply_files( 0, 0, 0 );

}




void test_pscoord_to_MOTIF_x( )
/*************************************************************************\
* Purpose: self-test package to test the routines in this module
*
* Author W.E. Rogers		Dec-1994
\*************************************************************************/
{
  char value_1[16]="     0";	/* test data - in character format */
  char value_2[16]=" 21.59";
  int ans;

  printf("### SELF TESTING pscoord_to_MOTIF_x routine ###\n" );

/*				     x_cm    x    paper  zoom    correct */
/*				     offset scale offset factor  answer  */
  test_pscoord_to_MOTIF_x_1( value_1, 0.0,  1.0,   0,     1.0,    15  );
  test_pscoord_to_MOTIF_x_1( value_2, 0.0,  1.0,   0,     1.0,    865 );

/*  zoom_factor = 2.0 */
  test_pscoord_to_MOTIF_x_1( value_1, 0.0, 1.0,   0, 2.0, 15 );
  test_pscoord_to_MOTIF_x_1( value_2, 0.0, 1.0,   0, 2.0, 1715 );

/*  zoom_factor = 0.5 */
  test_pscoord_to_MOTIF_x_1( value_1, 0.0, 1.0,   0, 0.5, 15 );
  test_pscoord_to_MOTIF_x_1( value_2, 0.0, 1.0,   0, 0.5, 440 );

/*  x_scale = 2.0 */
  test_pscoord_to_MOTIF_x_1( value_1, 0.0, 2.0,   0, 1.0, 15 );
  test_pscoord_to_MOTIF_x_1( value_2, 0.0, 2.0,   0, 1.0, 1715 );

/*  x_scale = 0.5 */
  test_pscoord_to_MOTIF_x_1( value_1, 0.0, 0.5,   0, 1.0, 15 );
  test_pscoord_to_MOTIF_x_1( value_2, 0.0, 0.5,   0, 1.0, 440 );

/*  the works! */
  test_pscoord_to_MOTIF_x_1( value_1, 4.3, 1.5,   7, 2.1, 563  );
  test_pscoord_to_MOTIF_x_1( value_2, 4.3, 1.5,   7, 2.1, 3240 );

}

void test_pscoord_to_MOTIF_x_1( char *value, double xco, double xsc,
				 int po, double zf, int correct )
/**************************************************************************\
* Purpose: test one x value
*
\**************************************************************************/
{
  double xco_sav,xsc_sav,zf_sav;
  int ans, po_sav;

  xco_sav = x_cm_offset;
  xsc_sav = xscale;
  zf_sav  = zoom_factor;
  po_sav  = paperOffsetX;

  x_cm_offset = xco;
  xscale      = xsc;
  paperOffsetX= po;
  zoom_factor = zf;

  if( (ans = pscoord_to_MOTIF_x( value, 6, "self_test" )) != correct )
  {
    printf("*** ERROR *** pscoord_to_MOTIF_x failed self-test with following values:\n" );
    printf("x             '%s'\n", value );
    printf("x_cm_offset    %f\n", x_cm_offset );
    printf("x scale        %f\n", xscale );
    printf("paperOffsetX   %d\n", paperOffsetX );
    printf("zoom_factor    %f\n", zoom_factor  );
    printf("scrn_border    %d\n", SCRN_BORDER  );
    printf("plot_pixels_wd %d\n\n", plot_pixels_wd  );
    printf("  Routine returned value of %d when correct value should have been %d\n\n", ans, correct );
  }

  x_cm_offset = xco_sav; /* restore */
  xscale      = xsc_sav;
  zoom_factor = zf_sav;
  paperOffsetX = po_sav;

}  

void test_pscoord_to_MOTIF_y( )
/*************************************************************************\
* Purpose: self-test package to test the routines in this module
*
* Author W.E. Rogers		Dec-1994
\*************************************************************************/
{
  char value_1[16]="     0";
  char value_2[16]=" 27.94";
  int ans;

  printf("### SELF TESTING pscoord_to_MOTIF_y routine ###\n" );

/*				      co   ys    po  zf   cor */
  test_pscoord_to_MOTIF_y_1( value_1, 0.0, 1.0,   0, 1.0, 1115 );
  test_pscoord_to_MOTIF_y_1( value_2, 0.0, 1.0,   0, 1.0, 15 );

/*  zoom_factor = 2.0 */
  test_pscoord_to_MOTIF_y_1( value_1, 0.0, 1.0,   0, 2.0, 2215 );
  test_pscoord_to_MOTIF_y_1( value_2, 0.0, 1.0,   0, 2.0, 15 );

/*  zoom_factor = 0.5 */
  test_pscoord_to_MOTIF_y_1( value_1, 0.0, 1.0,   0, 0.5, 565 );
  test_pscoord_to_MOTIF_y_1( value_2, 0.0, 1.0,   0, 0.5, 15 );

/*  y_scale = 2.0 */
  test_pscoord_to_MOTIF_y_1( value_1, 0.0, 2.0,   0, 1.0, 1115 );
  test_pscoord_to_MOTIF_y_1( value_2, 0.0, 2.0,   0, 1.0, -1085 );
   /* NOTE:  negative values OK - other routines normally adjust paper offset
	     to compenstate for this */

/*  y_scale = 0.5 */
  test_pscoord_to_MOTIF_y_1( value_1, 0.0, 0.5,   0, 1.0, 1115 );
  test_pscoord_to_MOTIF_y_1( value_2, 0.0, 0.5,   0, 1.0, 565 );

/*  the works! */
  test_pscoord_to_MOTIF_y_1( value_1, 4.3, 1.5,   7, 2.1, 2873 );
  test_pscoord_to_MOTIF_y_1( value_2, 4.3, 1.5,   7, 2.1, -591 );

}

void test_pscoord_to_MOTIF_y_1( char *value, double co, double sc,
				 int po, double zf, int correct )
/**************************************************************************\
* Purpose: test one set of values
*
\**************************************************************************/
{
  double yco_sav,ysc_sav,zf_sav;
  int ans, po_sav;

  yco_sav = y_cm_offset;
  ysc_sav = yscale;
  zf_sav  = zoom_factor;
  po_sav  = paperOffsetY;

  y_cm_offset = co;
  yscale      = sc;
  paperOffsetY= po;
  zoom_factor = zf;

  if( (ans = pscoord_to_MOTIF_y( value, 6, "self_test" )) != correct )
  {
    printf("*** ERROR *** pscoord_to_MOTIF_y failed self-test with following values:\n");
    printf("y             '%s'\n", value );
    printf("y_cm_offset    %f\n", y_cm_offset );
    printf("y scale        %f\n", yscale );
    printf("paperOffsetY   %d\n", paperOffsetY );
    printf("zoom_factor    %f\n", zoom_factor  );
    printf("scrn_border    %d\n", SCRN_BORDER  );
    printf("plot_pixels_ht %d\n\n", plot_pixels_ht  );
    printf("  Routine returned value of %d when correct value should have been %d\n\n", ans, correct );
  }

  y_cm_offset = yco_sav; /* restore */
  yscale      = ysc_sav;
  zoom_factor = zf_sav;
  paperOffsetY = po_sav;
}
