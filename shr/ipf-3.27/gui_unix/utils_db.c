/**************************************************************************\
*
* Purpose:  Utilities to support dbase debugger
*
*
\**************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/List.h>
#include <Xm/SelectioB.h>
#include <Xm/ToggleB.h>
#include <ctype.h>
#include <string.h>
#include "dmgr.h"
#include "graph_data.h"
#include "coord_data.h"

void set_dbase_source	( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs );
void set_search_flg	( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs );
void get_db_first	( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs );
void print_lk		( );
void fill_graphelement_fields	( GraphElement * );
int dbase_box_getwid	( );
void add_comment_string_to_list_box( GraphElement * );
void print_str		( GraphElement *pge );
void add_list_item	( char *ge_str, GraphElement * );
void establish_nxt_link	( GraphElement *plast_ge );

extern void disable_pushbutton	( char *name );
extern void enable_pushbutton	( char *name );
extern lookup_and_fill_field	( char *, char *, int, int );
extern void lookup_and_get_field( char *, char *, int );
extern Widget widget_id 	( char *name);
extern char *cstring_to_string  ( XmString motif_string );

static char db_flg = '1';
static Widget id, list_id, text_id;
static Widget sbid, sblstid, sbtxtid;
Boolean eof_flg = False;

static DB_STAT stat;  /* status returned for general db access */
static GraphElement	 graphnode;
static GraphElement	*pgraphnode = &graphnode;
static GraphElement	*pgraphnodenxt;
static GraphElement	*pnextlink;
static GraphElement	*pKey;
static GraphElement	key;
static GraphElement	*ptmp;
static GraphLink	*pgraphlink;
XmString rec[1000];
static long db_search_idx = GRAPH_CLASS_IDX;
static int class_flg	  = GraphClassVertexBus;
static int display_flg	  = GraphDisplayOn;
static int type_flg	  = GraphTypeVertex;
static int nrec =0;
static char ge_str[100];
static int get_status;

#define	VERTEX_LINK	0
#define EDGE_LINK	1
#define STOP		2

void set_dbase_source( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
/************************************************************************\
*  Purpose:
*
\************************************************************************/
{
  char *pctag;

  pctag = (char *)tag;
  db_flg=pctag[0];

  printf("set_dbase_source set with %c\n", db_flg );

  disable_pushbutton ( "dbase_next_pb" );
  disable_pushbutton ( "dbase_link_pb" );
  disable_pushbutton ( "dbase_nextlink_pb" );
  disable_pushbutton ( "dbase_prev_pb" );


}
 

void set_search_flg( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
/************************************************************************\
*  Purpose:
*
\************************************************************************/
{

  if ( (int)XmToggleButtonGetState(widget_id("graphelement_vbus_tb")))
  {
    class_flg  = GraphClassVertexBus;
  }
  else if ( (int)XmToggleButtonGetState(widget_id("graphelement_vcomment_tb")))
  {
    class_flg  = GraphClassVertexComment;
  }
  else if((int)XmToggleButtonGetState(widget_id("graphelement_class_label_bn")))
  {
    class_flg  = GraphClassVertexLabelText;
    printf("Set to: LABEL\n" );
  }
  else if ((int)XmToggleButtonGetState(widget_id("graphelement_lbl_crnr_pb")))
  {
    class_flg  = GraphClassVertexLabelCorner;
    printf("Set to: LABEL CORNER\n" );
  }

  else if((int)XmToggleButtonGetState(widget_id("graphelement_comlx_edge_pb")))
  {
    class_flg  = GraphClassEdgeComplexSection;
    printf("Set to: COMPLEX EDGE\n" );
  }
  else if ((int)XmToggleButtonGetState(widget_id("graphelement_edge_lbl_pb")))
  {
    class_flg  = GraphClassEdgeLabel;
    printf("Set to: EDGE LABEL\n" );
  }
  else if ((int)XmToggleButtonGetState(widget_id("graphelement_lengnd_txt_pb")))
  {
    class_flg  = GraphClassVertexLegendText;
    printf("Set to: LEGEND TEXT\n" );
  }
  else if((int)XmToggleButtonGetState(widget_id("graphelement_class_group_bn")))
  {
    class_flg  = GraphClassVertexGroup;
    printf("Set to: GROUP\n" );
  }

  else if ((int)XmToggleButtonGetState(widget_id("graphelement_vertx_brdr_pb")))
  {
    class_flg  = GraphClassVertexBorder;
  }
  else if ((int)XmToggleButtonGetState(widget_id("graphelement_edge_brdr_pb")))
  {
    class_flg  = GraphClassEdgeBorder;
  }
  else if ((int)XmToggleButtonGetState(widget_id("graphelement_papr_vertx_pb")))
  {
    class_flg  = GraphClassPaperVertex;
  }
  else if ((int)XmToggleButtonGetState(widget_id("graphelement_paper_edge_pb")))
  {
    class_flg  = GraphClassPaperEdge;
  }
}


void get_db_first( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
/************************************************************************\
*  Purpose:
*
\************************************************************************/
{
  char vtx_name[20];

  switch( db_flg )
  {
    case '0':
      if(db_valid_db( &pscor_db ) != D_OKAY) {
        printf("ERROR: printCoordData - no coord table\n");
        return;
      }
      printf(" DEBUGGER DOES NOT YET SUPPORT COORD FILE LIST\n" );
    break;

    case '1':
      if(db_valid_db(&graph_db) != D_OKAY) {
        printf("ERROR: printGraphData - no graph table\n");
        return;
      }

      if( nrec )
      {
	XmListDeleteAllItems( sblstid );
	nrec = 0;
      }

      /** Setup the first record for search **/
      pgraphnode = &graphnode;
      stat = db_first ( &graph_db, &pgraphnodenxt, db_search_idx ); 

      pKey = &key;
      pKey->type    = type_flg;
      pKey->display = display_flg;
      pKey->class   = class_flg;

      lookup_and_get_field( "vertext_text_fld", vtx_name, 13);
      if( &vtx_name[0] != NULL )
      {
        strncpy( pKey->vertex_id, vtx_name, 13 );
	printf("Looking for...[%s]\n", vtx_name );
	printf("Looking for...[%s]\n", pKey->vertex_id );
      }

	/* Now get the first class of records desired .... */
      stat = db_search( &graph_db, &pKey, &pgraphnodenxt, db_search_idx );
  
	/* NOTE:  since idx=0, record will never be found, but will
	*      be positioned on 1st available record and ready to go...
	\***************************************************************/

      if( pgraphnodenxt->class == class_flg )
      {
        stat = D_FOUND;
	print_str( pgraphnodenxt );

        if( dbase_box_getwid() == False)
	   break;

	add_list_item( ge_str, pgraphnodenxt );

	enable_pushbutton ( "dbase_next_pb" );

	establish_nxt_link( pgraphnodenxt );

	disable_pushbutton ( "dbase_nextlink_pb" );
	disable_pushbutton ( "dbase_prev_pb" );
      }
      else
      {
	printf(" NO RECORDS \n" );
	disable_pushbutton ( "dbase_next_pb" );
	disable_pushbutton ( "dbase_link_pb" );
	disable_pushbutton ( "dbase_nextlink_pb" );
	disable_pushbutton ( "dbase_prev_pb" );
      }
    break;

    default:
      printf("ERROR - no db_flg set \n" );
    
  }
}

void establish_nxt_link( GraphElement *plast_ge )
/************************************************************************\
* Purpose:  Look-ahead to find any links from this graph element.
*
\************************************************************************/
{

  stat = ll_first(&graph_db, &plast_ge, &pnextlink, GRAPH_VERTEX_LINK);
  if(D_FOUND == stat)
  {
    enable_pushbutton ( "dbase_link_pb" );
    printf("There's a vertex link...\n");
    get_status = VERTEX_LINK;
  }
  else
  {
    stat=ll_first(&graph_db,&pgraphnodenxt,&pnextlink, GRAPH_EDGE_LINK);
    if(D_FOUND == stat)
    {
      enable_pushbutton ( "dbase_link_pb" );
      printf("There's a edge link...\n");
      get_status = EDGE_LINK;
    }
    else
    {
      disable_pushbutton( "dbase_link_pb" );
      printf("There's no more links...\n");
      get_status = STOP;
    }
  }

}



void get_db_next( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
/************************************************************************\
*  Purpose:
*
\************************************************************************/
{
  char string[100];

  pgraphnode = pgraphnodenxt;

  stat = db_next( &graph_db, &pgraphnode, &pgraphnodenxt, db_search_idx);

  if( stat == D_EOF )
  {
    rec[nrec] = XmStringCreate( "EOF", XmSTRING_DEFAULT_CHARSET );
    stat = D_NOTFOUND;
    eof_flg = TRUE;
    disable_pushbutton ( "dbase_next_pb" );

    rec[nrec++] = XmStringCreate( "EOL", XmSTRING_DEFAULT_CHARSET );
    XmListAddItem( sblstid, rec[nrec-1], nrec );

  }

  if( stat == D_FOUND )
  {
    print_str( pgraphnodenxt );

    add_list_item( ge_str, pgraphnodenxt );

    disable_pushbutton ( "dbase_nextlink_pb" );
    disable_pushbutton ( "dbase_prev_pb" );
  }
}



void get_db_1st_link( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
/************************************************************************\
*  Purpose:
*
\************************************************************************/
{
  char strng2[100];

  if(D_FOUND == stat)
  {
    print_lk( );
    sprintf( strng2, "LINK:%s", ge_str );

    add_list_item( strng2, ptmp );

    establish_nxt_link( pnextlink );

    enable_pushbutton ( "dbase_nextlink_pb" );
    enable_pushbutton ( "dbase_prev_pb" );

  }
}


void get_db_nxt_link( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
/************************************************************************\
*  Purpose:
*
\************************************************************************/
{
  char strng2[100];

  if( get_status == STOP ) return;
  stat = ll_next(&pgraphlink, &pgraphlink);
  if(D_FOUND == stat)
  {
    print_lk( );
    sprintf( strng2, "LINK:%s", ge_str );

    add_list_item( strng2, ptmp );
    return;
  }

  if( get_status == VERTEX_LINK )
  {
printf("look for edge link....\n");
    stat = ll_first(&graph_db, &pgraphnodenxt, &pgraphlink, GRAPH_EDGE_LINK);
    if(D_FOUND == stat)
    {
    get_status = EDGE_LINK;
    print_lk( );
    sprintf( strng2, "LINK:%s", ge_str );

    add_list_item( strng2, ptmp );
    }
    else
    {
      printf("stop link search\n");
      disable_pushbutton ( "dbase_link_pb" );
      disable_pushbutton ( "dbase_nextlink_pb" );
      get_status = STOP;
    }
  } 

}



void print_lk( )
/************************************************************************\
*  Purpose:  Calls print_str - with pre-step to extract ptr
*
\************************************************************************/
{
  ptmp = pgraphlink->graphnode;		/* use link to get prt */

  print_str( ptmp );

}


void get_db_link_to_link( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
/************************************************************************\
*  Purpose:
*
\************************************************************************/
{
}


void print_str( GraphElement *pge )
/************************************************************************\
*  Purpose:
*
\************************************************************************/
{

  sprintf( ge_str, "%3d %1d %1d %1d %6d %6d (%d) %24.24s",
	pge->idx,
	pge->display,
	pge->type,
	pge->class,
	pge->x,
	pge->y,
	pge->wid,
	pge->vertex_id );
 
  printf( "%s\n", ge_str );
}


int dbase_box_getwid( )
/**************************************************************************\
* Purpose:  Get the wid_id of the selection box XmDIALOG_LIST and
*               XmDIALOG_TEXT
*
* Called by:
\**************************************************************************/
{

  if(( sbid = widget_id("dbase_list_box")) == FALSE)
        return(False);

  sblstid = (Widget)XmSelectionBoxGetChild( sbid, XmDIALOG_LIST);
  if( sblstid == NULL)
  {
    printf("PROGRAMMER ERROR no XmDIALOG_LIST for %s\n", "dbase_list_box");
                return(False);
  }

  sbtxtid = (Widget)XmSelectionBoxGetChild( sbid, XmDIALOG_TEXT);
  if( sbtxtid == NULL)
  {
    printf("PROGRAMMER ERROR no XmDIALOG_TEXT for %s\n", "dbase_list_box");
    return(False);
  }
  return(True);
}



void add_list_item( char *ge_str, GraphElement *pge )
{

  rec[nrec++] = XmStringCreate( ge_str, XmSTRING_DEFAULT_CHARSET );
  XmListAddItem( sblstid, rec[nrec-1], nrec );

  fill_graphelement_fields( pge );

  if( pge->class == GraphClassVertexComment ||
	pge->class == GraphClassVertexOrgComment )
      add_comment_string_to_list_box( pge );

}



void fill_graphelement_fields( GraphElement *pge )
/************************************************************************\
*  Purpose:
*
\************************************************************************/
{
  char c_idx[10];
  char class[40];
  char type[20];

  sprintf( c_idx, "%d", pge->idx );
  lookup_and_fill_field( "graphelement_index_text_fld", c_idx, 0, 3 );

  switch( pge->class )
  {
    case GraphClassVertexBus:
      strcpy( class, "0) GraphClassVertexBus" );
    break;

    case GraphClassVertexName:
      strcpy( class, "1) GraphClassVertexName" );
    break;

    case GraphClassVertexBendPoint:
      strcpy( class, "2) GraphClassVertexBendPoint" );
    break;

    case GraphClassVertexGenerator:
      strcpy( class, "3) GraphClassVertexGenerator" );
    break;

    case GraphClassEdgeSection:
      strcpy( class, "4) GraphClassEdgeSection" );
    break;

    case GraphClassEdgeComplexSection:
      strcpy( class, "5) GraphClassEdgeComplexSection" );
    break;

    case GraphClassEdgeSubSection:
      strcpy( class, "6) GraphClassEdgeSubSection" );
    break;

    case GraphClassVertexGroup:
      strcpy( class, "7) GraphClassVertexGroup" );
    break;

    case GraphClassVertexDrawPoint:
      strcpy( class, "8) GraphClassVertexDrawPoint" );
    break;

    case GraphClassEdgeDraw:
      strcpy( class, "9) GraphClassEdgeDraw" );
    break;

    case GraphClassPaperVertex:
      strcpy( class, "10) GraphClassPaperVertex" );
    break;

    case GraphClassPaperEdge:
      strcpy( class, "11) GraphClassPaperEdge" );
    break;

    case GraphClassVertexComment:
      strcpy( class, "12) GraphClassVertexComment" );
    break;

    case GraphClassVertexSubcomment:
      strcpy( class, "13) GraphClassVertexSubcomment" );
    break;

    case GraphClassVertexOrgComment:
      strcpy( class, "14) GraphClassVertexOrgComment" );
    break;

    case GraphClassVertexLegendCorner:
      strcpy( class, "15) GraphClassVertexLegendCorner" );
    break;

    case GraphClassVertexLegendText:
      strcpy( class, "16) GraphClassVertexLegendText" );
    break;

    case GraphClassEdgeLegend:
      strcpy( class, "17) GraphClassEdgeLegend" );
    break;

    case GraphClassVertexLabelCorner:
      strcpy( class, "18) GraphClassVertexLabelCorner" );
    break;

    case GraphClassVertexLabelText:
      strcpy( class, "19) GraphClassVertexLabelText" );
    break;

    case GraphClassEdgeLabel:
      strcpy( class, "20) GraphClassEdgeLabel" );
    break;

    case GraphClassVertexBorder:
      strcpy( class, "21) GraphClassVertexBorder" );
    break;

    case GraphClassEdgeBorder:
      strcpy( class, "22) GraphClassEdgeBorder" );
    break;

    case GraphClassVertexFont:
      strcpy( class, "23) GraphClassVertexFont" );
    break;

    default:
      sprintf( class, "Class type %d", pge->class );

  }
  lookup_and_fill_field( "graphelement_class_text_fld", class, 0, 39 );

  switch( pge->display )
  {
    case GraphDisplayOff:
      strcpy( type, "GraphDisplayOff");
    break;

    case GraphDisplayOn:
      strcpy( type, "GraphDisplayOn");
    break;
  }
  lookup_and_fill_field( "graphelement_display_text_fld", type, 0, 19 );

  switch( pge->type )
  {
    case GraphTypeVertex:
      strcpy( type, "GraphTypeVertex" );
    break;

    case GraphTypeEdge:
      strcpy( type, "GraphTypeEdge" );
    break;
  }
  lookup_and_fill_field( "graphelement_type_text_fld", type, 0, 19 );

}


void add_comment_string_to_list_box( GraphElement *pge )
/************************************************************************\
*  Purpose:
*
\************************************************************************/
{
  char *pcmt_str;
  char str[100];
  XmString xstr;

  XtVaGetValues(pge->wid, XmNlabelString, &xstr, NULL);

  pcmt_str = cstring_to_string(xstr);
  if( pcmt_str == NULL )
      sprintf(str, "ERROR - NULL PTR to WIDGET!" );
  else
      sprintf(str, "  (%d)(%s)", pge->wid, pcmt_str );

  rec[nrec++] = XmStringCreate( str, XmSTRING_DEFAULT_CHARSET );
  XmListAddItem( sblstid, rec[nrec-1], nrec );
}
