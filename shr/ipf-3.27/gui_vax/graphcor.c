
static char sccsid[]="@(#)graphcor.c	20.2 5/16/94";
/**********************************************************************\
*		
*	Author: Dan Clark	Dec, 1991	Modified: Sept 30, 1992
*
\**********************************************************************/
#include <stdio.h>
#include <assert.h>
#include <Xm/Xm.h>
#include "dmgr.h"
/* include "base_data.h" */
#include "coord_data.h"
#include "graph_data.h"

#include "em.h"
em_init

#define max(A,B) ((A) > (B) ? (A) : (B))
#define MAX_BENDS 4

static int    height;
static int    width;
static int    border = 10;
static double xscale;
static double yscale;

extern double atof_cnt_zero( char *, int );
extern int    atoi_cnt_zero( char *, int );
extern void   createGraphVertex ( char *key, GraphType type, char *name,
	                             int x, int y, int source, 
                                     GraphElement **pgraphelement);
extern void addGraphLink (GraphElement *, GraphElement *);
extern DB_STAT findBusGraphVertexRec(char *, GraphElement **, GraphSource );
extern int deleteGraphLink(GraphElement *pe1,GraphElement *pe2);

int  bendCount		 ( LINERECORD *rec );
int  coord_to_MOTIF_x	 ( char *x, int col, char *bus_name );
int  coord_to_MOTIF_y	 ( char *y, int col, char *bus_name );
void getHtWdScale	 ();
int findfirstcoordbusrec ( BUSRECORD *nextbusrec );
void show_graph_db	 ();
void createGraphCorEdges ( GraphElement *pgraphbusrec, char *name_base,
			   BUSRECORD *coordnextbusrec,
			   BRANCHRECORD *coordnextbranchrec );
void createGraphCorEdge  ( GraphElement *vertx_1,
			   GraphElement *vertx_2, 
			   char *key,
			   int level, 
			   GraphClass class, 
			   GraphElement **new_rec );
void reverseCoordCard 	 ( LINERECORD *pcoordrev, LINERECORD *pcoordorg );


int getHeight()
{
  return height;
}

graph2cor(int *cx, int *cy, int gx, int gy)	/* called by coord_data.c */
{
   *cx = gx;
   *cy = height - gy;
   if(*cy < 0)
   {
      printf("Warning: setting y to zero to avoid negative coord\n");
      *cy = 0;
   };
}


int 
buildGraphCoord /************************************************************\
*
*	FUNCTION: Transfers coord_db BUS data to graph_db.
*
*	Called by: View - Graphic/Character pulldown menu
*
\****************************************************************************/
( Widget widget, caddr_t *tag, caddr_t *callback_data)

{
  COORDFILERECORD coordkeyvalrecord;	/* storage for db_search key values */

  LINERECORD	 *coordbranchvalrec;	/* ptr to key for branch search	    */
  LINERECORD	 *coordnextbranchrec;	/* ptr returned for next branch rec */
  LINERECORD	 *coordtmpbranchrec;	/* ptr returned for next branch rec */

  BUSRECORD	 *coordbusvalrec;	/* ptr to key for bus search	    */
  BUSRECORD	 *coordnextbusrec;	/* ptr returned for next bus rec    */

  GraphElement  graphrecord;		/* define structure graphrecord	    */
  GraphElement *pgraphrecord = &graphrecord;	/* allocate memory	    */
  GraphElement *pgraphbusrec;
  GraphElement *pgraphnamerec;

  GraphLink *pgraphlink;

  DB_STAT stat;		/* status returned for general db access	*/
  DB_STAT statcoordbus;	/* status returned for bus queries of coord db	*/
  DB_STAT statbranch;	/* status returned for branch queries of db	*/
  DB_STAT statgraph;	/* status returned from graph db operations	*/

  int rtncmp;
  int bendcnt;
  int i;

  /** make sure the coordinate file has been loaded **/
  if(coord_db.nkeys == (long)0)
  {
    printf("INFO: coord_db empty - Abort transfer of coord_db to graph_db\n");
    return;
  }

  getHtWdScale();

  printf("*******************************\n");
  printf("* Moving coord_db to graph_db *\n");
  printf("*******************************\n");

  /** set pointer to first Bus record of coord file **/
  statcoordbus = findfirstcoordbusrec(coordnextbusrec);

  if(statcoordbus == D_EOF) return;	/* abort if no buses in record	*/

  while(statcoordbus == D_FOUND)	/* LOOP THUR THE BUSES */
  {
    createGraphVertex( coordnextbusrec->key,	/*** create symbol vertex ***/
	GraphClassVertexBus,
	coordnextbusrec->name,
    	coord_to_MOTIF_x( coordnextbusrec->symbol_x, 4, coordnextbusrec->key ),
    	coord_to_MOTIF_y( coordnextbusrec->symbol_y, 4, coordnextbusrec->key ),
	GraphSourceCoord,
	&pgraphbusrec );
			
    createGraphVertex( coordnextbusrec->key,  /*** create bus name vertex ***/
	GraphClassVertexName,	
	coordnextbusrec->name,
    	coord_to_MOTIF_x( coordnextbusrec->name_x, 4, coordnextbusrec->key ),
    	coord_to_MOTIF_y( coordnextbusrec->name_y, 4, coordnextbusrec->key ),
	GraphSourceCoord,
	&pgraphnamerec );

    addGraphLink( pgraphbusrec, pgraphnamerec );
			
    /** retrieve the first branch Record for this bus **/
    coordbranchvalrec = (LINERECORD *)&coordkeyvalrecord;
    strncpy(coordbranchvalrec->key, " ", 1);
    strncpy(coordbranchvalrec->name1, coordnextbusrec->name , 12);
    strncpy(coordbranchvalrec->name2, "            " , 12);

    statbranch = db_search ( &coord_db,
			     &coordbranchvalrec,
			     &coordnextbranchrec,
			     COORD_NAME1_BASE1 );

    while( statbranch == D_FOUND)
    {
      if(coordnextbranchrec->key[0] != coordnextbusrec->key[0])
      {
        rtncmp = strncmp( coordnextbranchrec->name2, 
                          coordnextbusrec->name, 12);
        if(rtncmp < 0)
        {
        /** this trick may require that the bending points be transposed **/
          coordtmpbranchrec = &coordkeyvalrecord.line;
          reverseCoordCard(coordtmpbranchrec, coordnextbranchrec);
          createGraphCorEdges(pgraphbusrec, 
				coordnextbranchrec->name2, 
				coordnextbusrec, 
				(BRANCHRECORD *)coordtmpbranchrec);
        }
      } 

      coordbranchvalrec = coordnextbranchrec;	 /* get next branch **/

      statbranch = db_keynext(  &coord_db,
				&coordbranchvalrec, 
				&coordnextbranchrec,
				COORD_NAME1_BASE1 );
    } /* end branch loop */

    /** retrieve the first farend branch Record for this bus **/
    coordbranchvalrec = (LINERECORD *)&coordkeyvalrecord;
    strncpy(coordbranchvalrec->key, " ", 1);
    strncpy(coordbranchvalrec->name1, "            " , 12);
    strncpy(coordbranchvalrec->name2, coordnextbusrec->name , 12);

    statbranch = db_search ( &coord_db,
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
        coordtmpbranchrec = (LINERECORD *)&coordkeyvalrecord;

        /* reverseCoordCard(coordtmpbranchrec, coordnextbranchrec); */
        createGraphCorEdges( pgraphbusrec, 
				coordnextbranchrec->name1, 
				coordnextbusrec, 
				(BRANCHRECORD *)coordnextbranchrec);
        }
      } /* End of processing of coord file branch record */

      coordbranchvalrec = coordnextbranchrec;	 /* get next branch **/
      statbranch = db_keynext ( &coord_db,
	                      &coordbranchvalrec,
	                      &coordnextbranchrec, 
	                      COORD_NAME2_BASE2 );
    } /* end branch loop */

    /** get the next coord bus **/
    coordbusvalrec = coordnextbusrec;
    statcoordbus = db_next ( &coord_db,
	                     &coordbusvalrec,
	                     &coordnextbusrec,
	                     COORD_KEY_NAME1_BASE1 );

    if(coordnextbusrec->key[0] != 'B')
    {
#ifdef DEBUG
       printf("Note: forcing end - gone past base bus records\n");
       printf("last: %s\n", coordnextbusrec);
#endif
       statcoordbus = D_NOTFOUND;
    }
  } /* end bus loop */
  return(0);
}



void reverseCoordCard 
( LINERECORD *pcoordrev,	/* ptr returned for next branch rec */
  LINERECORD *pcoordorg ) 	/* ptr returned for next branch rec */
/*****************************************************\
*
* author: Dan Clark
* I want to get rid of this function by reversing the data when
* it is read in but i cannot get it to work today.
*
*	Called by: above
\***************************************************************************/
{
  int bendcnt;
  int i;

    /* printf("NOTE: reverseCoordCard Called\n"); */
    memcpy(pcoordrev, pcoordorg,sizeof(LINERECORD));
    memcpy(pcoordrev->name1,pcoordorg->name2, 12);
    memcpy(pcoordrev->name2,pcoordorg->name1, 12);

    bendcnt = bendCount( pcoordrev );

    for(i = 0; i < bendcnt; i++) {
#ifdef DEBUG
printf("Bend %d X = %4.4s\n", bendcnt, pcoordorg->bend[bendcnt-1].x);
printf("Bend %d Y = %4.4s\n", bendcnt, pcoordorg->bend[bendcnt-1].y);
#endif
      assert((bendcnt-i-1) >= 0);
      memcpy(pcoordrev->bend[i].x,pcoordorg->bend[bendcnt-i-1].x, 4);
      memcpy(pcoordrev->bend[i].y,pcoordorg->bend[bendcnt-i-1].y, 4);
      
    }

}




void createGraphCorEdges ( GraphElement *pstartbusrec,
        char         *far_vertex_name_base,
        BUSRECORD    *coordnextbusrec,	  /* ptr returned for next bus rec    */
        BRANCHRECORD *coordnextbranchrec) /* ptr returned for next branch rec */
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
  LINERECORD	 *coordlinerec;         /* ptr to coord line record */

  GraphElement graphrecord;			/* define   */
  GraphElement *pgraphrecord = &graphrecord;	/* allocate */
  GraphElement *pgraphbendrec;
  GraphElement *pedge_last_created;
  GraphElement *pcomplex_edge;
  GraphElement *pgraphfarendbusrec;

  GraphLink *pgraphlink;

  DB_STAT stat;		/* status returned for general db access */
  DB_STAT statgraph;	/* status returned from graph db operations */

  int bendcnt;

  stat = findBusGraphVertexRec( far_vertex_name_base,
				&pgraphfarendbusrec,
				GraphSourceCoord );
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
    coordlinerec = (LINERECORD *)coordnextbranchrec;

    bendcnt = bendCount( coordlinerec );

    /** Create the parent ( complex) edge **/
    createGraphCorEdge ( pstartbusrec,			/* vertex 1 */
			 pgraphfarendbusrec, 		/* vertex 2 */
			 coordnextbranchrec->key,	/* name data */
			 0, 				/* level */
			 GraphClassEdgeComplexSection,	/* class */ 
			 &pedge_last_created );

    pcomplex_edge = pedge_last_created;		/* save ptr to complex edge */

    createGraphCorEdge ( pstartbusrec,			/* vertex 1 */
			 pgraphfarendbusrec, 		/* vertex 2 */
			 coordnextbranchrec->key,	/* name data */
			 1, 				/* level */
			 GraphClassEdgeSection,		/* class */ 
			 &pedge_last_created );

    addGraphLink( pedge_last_created, pcomplex_edge );	 /* link parent - */
    addGraphLink( pcomplex_edge, pedge_last_created );	 /* child edges   */

    /** Now create bend points followed by edges **/
    while(bendcnt > 0)
    {
      /** Create bend vertex in graph_db **/
      createGraphVertex( coordnextbusrec->key,
			 GraphClassVertexBendPoint,
			 coordnextbusrec->name,		/* not used anyway */
			 coord_to_MOTIF_x (coordlinerec->
				bend[bendcnt - 1].x, 4, coordnextbusrec->key),
			 coord_to_MOTIF_y (coordlinerec->
				bend[bendcnt - 1].y, 4, coordnextbusrec->key),
			 GraphSourceCoord,
			 &pgraphbendrec );

      /** destroy old pointers of edge to far bus **/
      deleteGraphLink( pedge_last_created, pgraphfarendbusrec );

      addGraphLink   ( pedge_last_created, pgraphbendrec );
      addGraphLink   ( pgraphbendrec, pedge_last_created );

      /** Now create child edge in graph_db **/
      createGraphCorEdge ( pgraphbendrec,		/* vertex 1 */
			   pgraphfarendbusrec, 		/* vertex 2 */
			   coordnextbranchrec->key,	/* name data */
			   1, 				/* level */
			   GraphClassEdgeSection,	/* class */ 
			   &pedge_last_created );
 
      addGraphLink( pedge_last_created, pcomplex_edge ); /* cross link edges */
      addGraphLink( pcomplex_edge, pedge_last_created );

      --bendcnt;
    }   /* end bend processing */

  }
  else
  {
    printf("*** WARNING *** following branch card is not a 'L' or 'T' card\n" );
    printf("(%78.78s)\n", coordnextbranchrec );
  }

}


void createGraphCorEdge 
( GraphElement *vertx_1,
  GraphElement *vertx_2, 
  char *key,
  int level, 
  GraphClass class, 
  GraphElement **new_rec ) 
/****************************************************\
*
*	Purpose: Creates an edge for the graph_db
*
*	Author: Bill Rogers 	Oct 21, 1992
*
*	Called by: createGraphCorEdges
*
\****************************************************************************/
{

  GraphElement graphrecord;			/* define   */
  GraphElement *pgraphrecord = &graphrecord;	/* allocate */
  GraphElement *pedge_created;
  GraphElement *pgraphbranchrec;

  DB_STAT	stat;

  pgraphrecord = &graphrecord;
  graphrecord.display = GraphDisplayOff;
  graphrecord.level   = level;
  graphrecord.type    = GraphTypeEdge;
  graphrecord.class   = class;
  graphrecord.x       = 0;
  graphrecord.y       = 0;
  graphrecord.wid     = NULL;
  graphrecord.source  = GraphSourceCoord;
#ifdef REPLACE
  memset(graphrecord.vertex_id,    ' ', sizeof(graphrecord.vertex_id));
  memset(graphrecord.farvertex_id, ' ', sizeof(graphrecord.farvertex_id));
#endif
  strncpy ( pgraphrecord->key.coord.id, key, sizeof(CoordKey)  ); 
  strncpy(graphrecord.vertex_id, &pgraphrecord->key.coord.name1[0], 
          sizeof(graphrecord.vertex_id));
  strncpy(graphrecord.farvertex_id, &pgraphrecord->key.coord.name2[0], 
          sizeof(graphrecord.farvertex_id));

  stat = db_insert_opt(&graph_db, &pgraphrecord, &pgraphbranchrec, optse);	

  if(stat != D_OKAY)
  {
    printf("ERROR insert graph table stat %d\n",stat);
    printf("  record failed is %s\n", pgraphrecord);
  }
  assert(stat == D_OKAY);

  addGraphLink( vertx_1, pgraphbranchrec );
  addGraphLink( pgraphbranchrec, vertx_1 );

  addGraphLink( vertx_2, pgraphbranchrec );
  addGraphLink( pgraphbranchrec, vertx_2 );

  *new_rec = pgraphbranchrec;

}


int bendCount (LINERECORD *rec )
/**************************************************************\
*
*	Function: counts number of bend x-coords on an 'L' or 'T' record.
*
\****************************************************************************/

{
#define MAXTRANSBEND 2
  int bendcnt = 0;
  int i;


  if( rec->key[0] == 'L' || rec->key[0] == 'T' )
  {
    for(i = 0; i < MAX_BENDS; ++i) /* count bends */
    {
      bendcnt += 0 < strncmp(rec->bend[i].x, "    ", 4);
    }
  }
  else
  {
    printf("*** PROGRAMMER ERROR *** in bendCount ( graphcor.c )\n");
    printf("          pointer does not point to a valid line record.\n");
    printf("          %60.60s\n", rec );
  }

  if(rec->key[0] == 'T') {
    /** bends on transformers max out at 2 */
    if(bendcnt > MAXTRANSBEND) {
      bendcnt = MAXTRANSBEND;
    }
  }

  return ( bendcnt );

}


void getHtWdScale()
/****************************************************************************\
*	Function: getHtWdScale
*		  ( get Height Width X & Y Scale )
*
*	Called by: buildGraphCoord
\****************************************************************************/
{
  COORDRECORD *coordvalrec;		/* ptr to generic coord record */
  COORDFILERECORD coordkeyvalrecord;	/* storage for db_search key values */
  COORDRECORD *coordrec;		/* ptr to generic coord record */
  DB_STAT statcoordbus;

  /** retrieve the COORD card for coord file **/
  coordvalrec = (COORDRECORD *)&coordkeyvalrecord;
  strncpy(coordvalrec->key, "C", 1);
  strncpy(coordvalrec->key_ext, "OORD    ", 8);
  statcoordbus = db_search
                 (
                   &coord_db,
                   &coordvalrec,
                   &coordrec,
                   COORD_KEY_NAME1
                 );

  if(statcoordbus != D_FOUND)
  {
    printf("Warning: COORD card not in coordinate file\n");
    width  = 850;
    height = 1100;
    xscale = 1.0;
    yscale = 1.0;
    printf("WARNING: BAD COORD card %40s\n", coordrec->key);
    printf(" reset width x height (%d x %d) xscale x yscale (%4.2lf x %4.2lf)",
      width, height, xscale, yscale);
  }
  else
  {
    width  = atoi_cnt_zero(coordrec->width, 4);
    height = atoi_cnt_zero(coordrec->height, 4);
    xscale = atof_cnt_zero(coordrec->xscale, 4);
    xscale = xscale / 100.0;
    yscale = atof_cnt_zero(coordrec->yscale, 4);
    yscale = yscale / 100.0;
    if( width  < 850) width = 850;
    if( height < 850) height = 850;
    if( xscale < 0.10 || xscale > 100) xscale = 1.0;
    if( yscale < 0.10 || yscale > 100) yscale = 1.0;
    printf("COORD card %40s\n", coordrec->key);
    printf("width x height (%d x %d) xscale x yscale (%4.2lf x %4.2lf)\n",
      width, height, xscale, yscale);
  }
}


/*****************************************************************************\
*
\*****************************************************************************/
void show_graph_db(Widget w, caddr_t *tag, caddr_t *reason)
{
  BUSRECORD		*coordbusvalrec;
  BUSRECORD		*coordnextbusrec;
  COORDFILERECORD	*coordkeyvalrecord;
  DB_STAT		statcoordbus;

  coordbusvalrec = (BUSRECORD *)&coordkeyvalrecord;
  statcoordbus = db_search
                 (
                   &coord_db,
                   &coordbusvalrec,
                   &coordnextbusrec,
                   COORD_KEY_NAME1_BASE1
                 );

  if(statcoordbus == D_EOF)
  {
    printf("ERROR: no bus data found in coord data structure\n");
    return;
  }
  if(statcoordbus != D_FOUND)
  {
     /* positioned before first bus rec in coord file */
     statcoordbus = D_FOUND;
  }

  while(statcoordbus == D_FOUND)
  {
    printf("XXX\n");

    statcoordbus = db_next ( &coord_db,
	                     &coordbusvalrec,
	                     &coordnextbusrec,
	                     COORD_KEY_NAME1_BASE1);
  }

}


/****************************************************************************\
*
\****************************************************************************/
int coord_to_MOTIF_x( char *x, int col, char *bus_name )
{
  int x_value;

    x_value = atoi_cnt_zero( x, 4);

#ifdef SCALE
    x_value = (int)(x * xscale);
#endif

    if( x_value > width || x_value < 0 )
    {
#ifdef DEBUG
      printf("WARNING: X coordinate %d out of range %d for %s\n", 
             x_value, width, bus_name );
#endif
      sprintf(errmsg, 
             "WARNING: X coordinate %d out of range %d for %s\n", 
             x_value, width, bus_name );
      err.line = EM_LINE;
      err.msg  = errmsg;
      err.link = "coord_to_MOTIF_x";
      err.type = WARNING;
      err.ident = sccsid;
      em_show( &err );

      x_value = max(width - border, 0);
    }

    return ( x_value );
}


int coord_to_MOTIF_y( char *y, int col, char *bus_name )
{
  int y_value;

    y_value = atoi_cnt_zero( y, 4);

#ifdef SCALE
    y_value = (int)(y * yscale);
#endif

    if( y_value > height || y_value < 0 )
    {
#ifdef DEBUG
      printf("WARNING: Y coordinate %d out of range %d for %s\n", 
             y_value, height, bus_name );
#endif
      sprintf(errmsg, 
             "WARNING: Y coordinate %d out of range %d for %s\n", 
             y_value, height, bus_name );
      err.line = EM_LINE;
      err.msg  = errmsg;
      err.link = "coord_to_MOTIF_y";
      err.type = WARNING;
      err.ident = sccsid;
      em_show( &err );

      y_value = max(height - border, border);
    }

    y_value = height - y_value;

    return ( y_value );
}
