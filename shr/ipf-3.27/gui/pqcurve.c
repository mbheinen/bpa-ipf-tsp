static char sccsid[]="@(#)pqcurve.c	20.6 2/15/95";
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
/********** PQCURVE.C *****************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleBG.h>
#include <Xm/Text.h>
#include <math.h>
#include "dmgr.h"
#include "base_data.h"
#include "ipf_ipc.h" /* define BUFSIZE ... do not use BUFSIZ ***/

#include "em.h"
em_init              /* Error structure macro - must be included in */
                     /* every file.                                 */
#define MAXPQC 10
#define PQ_CURVE_INFO           7

extern DBID   bus_br_db;
extern Widget widget_id	( char * widget_name );
extern char   rec_type;
extern int    send_type;
static int    modify;            /* modify flag: 0 = no, 1 = yes     */

Widget w_p_gen,		/* p_gen list widet id */
       w_q_max,		/* q_max list widet id */
       w_q_min;		/* q_min list widet id */

int  num_pts, row;
char active, unit, old_active, old_unit;
static char mod_rec[120];
char xpgen[MAXPQC][10], xqmax [MAXPQC][10], xqmin [MAXPQC][10];

static char out_buffer[BUFSIZE];

extern int  manage2parent(char *action, char *wname, char *wparent);
extern void disable_pushbutton	( char * );
extern void enable_pushbutton	( char * );
extern void stringPart		( char *, char *, int, int );
extern void strcat_field	( char *, char *, int, char *fmt );
extern void reformat		( char *, char *, char * );
extern int  pfchanges_cb	( Widget,  XtPointer,
				  XmFileSelectionBoxCallbackStruct *);
extern set_jacket               ( int stype, char type, char subtype );
extern void lookup_and_fill_field( char *, char *, int, int );


int  line_pq_edit_insert	( Widget, int *, unsigned long *);
void line_pq_edit_clear		();
int  line_pq_edit_delete	( Widget, int *, unsigned long *);
int  line_pq_edit_replace	( Widget, int *, unsigned long *);
void delete_pq_list_row		( int row );
void transf_pq_list_row		( int row );
void read_pq_from_db		();
void lookup_and_fill_list	( char *fld_name, char string[][10], int size );
void line_pq_list_cb		( Widget w, XtPointer *client_data, 
	                            XmListCallbackStruct *list_data);
void list_to_edit		( char *edit_fld, char *list_fld,
				   int row, int length );
void highlight_pq_list_row		( int irow );
void edit_to_list		( char *edit_fld, Widget list_id, int irow );
void pq_hi_lite			( char *widget_name, int irow );
void process_pq_radio_buttons	( Widget w, char *tag,	
				  XtCallbackList *list_data );
void add_pq_curve_rec		( char *rec );


void 
load_pq_list_widget_id() /*****************************************************\
*
*	First routine called - - find the widget id's.
*  
*	Author Bill Rogers	Dec 1992
*
*	Called by: fill_pq_jacket (selection.c)
*
\*****************************************************************************/

{

  if(( w_p_gen = widget_id( "pq_curve_p_gen_list"    )) == FALSE) return; 
  if(( w_q_max = widget_id( "pq_curve_q_max_list"    )) == FALSE) return;
  if(( w_q_min = widget_id( "pq_curve_q_min_list"    )) == FALSE) return;

  unit = 'M';		/* M = MVA, P = PU (per unit )	*/
  active = 'A';		/* A = active, * = inactive */

/* set up for possible error message */

   err.line  = EM_LINE;
   err.msg   = out_buffer;
   err.type  = FATAL;
   err.link  = "PQ Curve";
   err.ident = sccsid;
}


void 
read_pq_from_db( char *bus_namevolt ) /***************************************\
*	
*	Purpose: Get the Q cards associated with this bus.
*  
*	Author Bill Rogers	Dec 1992
*
*	Called by: 
*
\*****************************************************************************/

{
  PFBASERECORD keyvalrecord;    /* storage for db_search key values */
                                /* used for any search */
  static PFBASERECORD *busvalrec;       /* ptr to key for bus search */
  static PFBASERECORD *nextbusrec;      /* ptr returned for next bus rec */
  DB_STAT statbus;              /* status returned for bus queries of db */
 
  int i;
  char pq_str[80];

  disable_pushbutton( "reset_pushbutton"  );
  disable_pushbutton ( "add_pushbutton"    );
  enable_pushbutton ( "delete_pushbutton" );
  enable_pushbutton ( "send_pushbutton"   );

  send_type = PQ_CURVE_INFO;

  for (i = 0; i < MAXPQC; i++) {
      strcpy(xpgen[i],"      ");
      strcpy(xqmax[i],"      ");
      strcpy(xqmin[i],"      ");
  }

  busvalrec = &keyvalrecord;
  strncpy( busvalrec->bus.name, bus_namevolt, 12 );

  statbus = db_search( &bus_br_db, &busvalrec, &nextbusrec, BSE_NAME1_BASE1 );

  while(statbus == D_FOUND)
  {
 
    if( (strncmp( nextbusrec->bus.name, busvalrec->bus.name, 12 ) == 0) )
    {
      strncpy( pq_str, (char *)nextbusrec, 80 );

      add_pq_curve_rec( pq_str );

      statbus = db_keynext( &bus_br_db,
                            &nextbusrec,
                            &nextbusrec,
                            BSE_NAME1_BASE1 );

    }
    else     /** ran past cards with the right bus name **/
    {
       break;
    }
  }
 

  XmListDeleteAllItems ( w_p_gen  );
  XmListDeleteAllItems ( w_q_max );
  XmListDeleteAllItems ( w_q_min );

  err.line = EM_LINE;
  lookup_and_fill_list("pq_curve_p_gen_list", xpgen, num_pts );
  lookup_and_fill_list("pq_curve_q_max_list", xqmax, num_pts );
  lookup_and_fill_list("pq_curve_q_min_list", xqmin, num_pts );

  highlight_pq_list_row( 1 );	/* make 1st row inverse video */

/*************************************************************************\
***  control now reverts back to Motif, which awaits a user response 
***  such as a pushbutton or a selection of one of the above items.
*** 
***  The next routine called will be the line_pq_list_cb 
***  which is activated when user clicks on an item from any of the 3
***  list boxes.
\*************************************************************************/

}


void add_pq_curve_rec( char *rec )
/*************************************************************************\
*
*
*
*
\*************************************************************************/
{
  Widget id, id2;
  int len, col;

  len = strlen( rec );
  rec_type = rec[0];

  manage2parent("M", "pq_curve_jacket", "bus_front_box");

  switch ( rec_type )
  {
  case 'Q':
    num_pts = 0;
    col = 18;
    switch( rec[1] )
    {
      case 'P':
	id = widget_id( "line_pq_active_rb" );
	id2= widget_id( "line_pq_inactive_rb" );
	active = rec[5];
	old_active = active;
	switch( active )
	{
	case '*':
	  XtVaSetValues( id , XmNset, 0, NULL );
	  XtVaSetValues( id2, XmNset, 1, NULL );
	break;

	case ' ':
	case 'A':
	  XtVaSetValues( id , XmNset, 1, NULL );
	  XtVaSetValues( id2, XmNset, 0, NULL );
	break;
	}

	id = widget_id( "line_pq_MVA_rb" );
	id2= widget_id( "line_pq_perunit_rb" );

	switch( rec[3] )
	{
	  case ' ':
	  case 'M':
	    XtVaSetValues( id , XmNset, 1, NULL );
	    XtVaSetValues( id2, XmNset, 0, NULL );
	    unit = 'M';
	  break;

	  case 'P':
	    XtVaSetValues( id , XmNset, 0, NULL );
	    XtVaSetValues( id2, XmNset, 1, NULL );
	    unit = 'P';
	  break;
	}

	old_unit = unit;

	while( strncmp( &rec[col], "      ", 6 ) && col<len )
        {
  	  stringPart( rec, xpgen[num_pts++], col, col+5 );
	  col+= 6;
        }
      break;

      case 'X':
	while( strncmp( &rec[col], "      ", 6 ) && col<len )
	{
	  stringPart( rec, xqmax[num_pts++], col, col+5 );
	  col+= 6;
        }
      break;

      case 'N':
	while( strncmp( &rec[col], "      ", 6 ) && col<len )
	{
	  stringPart( rec, xqmin[num_pts++], col, col+5 );
	  col+= 6;
      	}
      break;
    }

    break;
  }
}


void line_pq_list_cb ( w, client_data, list_data)
/*****************************************************************************\
*
*	Purpose: transfers data from all list widgets to edit dialog.
*		
*	Author: Bill Rogers	Dec 14, 1992
*
*	Called by the following list widget callbacks:
*	pq_curve_p_gen_list, pq_curve_p_max_list, pq_curve_p_min_list
*
\*****************************************************************************/
Widget w;
XtPointer *client_data; 
XmListCallbackStruct *list_data;
{
 
   row = list_data->item_position;
   highlight_pq_list_row( row );

   err.line = EM_LINE;

   list_to_edit( "pq_curve_p_gen_text", "pq_curve_p_gen_list" , row, 6 );
   list_to_edit( "pq_curve_q_max_text" ,"pq_curve_q_max_list" , row, 6 );
   list_to_edit( "pq_curve_q_min_text" ,"pq_curve_q_min_list" , row, 6 );

}


int line_pq_edit_replace (w, tag, list_data)
/*****************************************************************************\
*
*	Purpose:
*
*       2. If n <= num_pts overwrite (modify) scrollable list data
*          and set modify flag = TRUE.
*       3. Update scrollable list display.
*		
*	Author: Bill Rogers 	Dec 14, 1992
*
*	Called by: "Replace" push button callback.
*
\*****************************************************************************/
Widget		w;
int		*tag;
unsigned long *list_data;

{

   delete_pq_list_row ( row );    /* do it this way because book example */
   transf_pq_list_row ( row );    /* on list replace just won't work!    */
   line_pq_edit_clear();
   return ( FALSE );

}


int line_pq_edit_insert (w, tag, list_data)
/*****************************************************************************\
*
*	Purpose:
*
*       3. If n > num_pts write new line of scrollable list data
*          and set modify flag = TRUE.
*       4. Update scrollable list display.
*		
*	Author: Bill Rogers		Dec 14, 1992
*
*	Called by: "Insert" push button callback.
*
\*****************************************************************************/
Widget		w;
int		*tag;
unsigned long   *list_data;

{

/** Transfer data from fields into character arrays */
   XtVaGetValues( w_p_gen, XmNitemCount, &num_pts, NULL);
  
   if( num_pts >= MAXPQC )
   {
      sprintf(out_buffer,"*** INFO *** Cannot add PQ data.\n\
             Limit of %d P-Q points allowed\n", MAXPQC );
      err.line = EM_LINE;
      err.type = INFO;
      em_show(&err);
      return( TRUE );
   }


   transf_pq_list_row( row+1 );
   line_pq_edit_clear();
   return( FALSE );

}


int line_pq_edit_delete (w, tag, list_data)
/*****************************************************************************\
*
*	Purpose: This function deletes a row of list data.
*
*	Author: Bill Rogers 	Dec 14, 1992
*
*	Called by: "Delete" push button callback.
*
\*****************************************************************************/
Widget		w;
int		*tag;
unsigned long   *list_data;

{

   delete_pq_list_row( row );
   line_pq_edit_clear();
   return( FALSE );

}


void delete_pq_list_row ( int irow )
/*****************************************************************************\
*
*	Purpose:  deletes a horizontal row of list items.		
*
*	Author: Bill Rogers	Dec 14, 1992
*
*	Called by: 
\*****************************************************************************/
{

  if ( irow < 0 || irow> MAXPQC) 
  {
     err.line = EM_LINE;
     sprintf( out_buffer, "*** WARNING *** Program error -\
 bad value for row (%d).\n                System crash averted.", irow );
     err.type = WARNING;
     em_show( &err );
     return;
  }

  XmListDeletePos ( w_p_gen, irow );
  XmListDeletePos ( w_q_max, irow );
  XmListDeletePos ( w_q_min, irow );

}


void transf_pq_list_row ( int irow )
/*****************************************************************************\
*
*	Purpose:  transfer all edit data to list boxes
*
*	Author: Bill Rogers	Dec 14, 1992
*
*	Called by: line_pq_edit_replace, line_pq_edit_insert
\*****************************************************************************/
{
  /* Note: edit_to_list (in linezcalc.c) creates a new list element */

  edit_to_list( "pq_curve_p_gen_text", w_p_gen , irow );
  edit_to_list( "pq_curve_q_max_text", w_q_max , irow );
  edit_to_list( "pq_curve_q_min_text", w_q_min , irow );

  highlight_pq_list_row( irow );

}



void highlight_pq_list_row ( int irow )
/*****************************************************************************\
*
*	Purpose:  make entire row highlighted 
*
*	Author: Bill Rogers	Dec 14, 1992
*
*	Called by: clicking one of 3 list columns. 
\*****************************************************************************/
{

  pq_hi_lite( "pq_curve_p_gen_list", irow ); 
  pq_hi_lite( "pq_curve_q_max_list", irow ); 
  pq_hi_lite( "pq_curve_q_min_list", irow ); 

}

void pq_hi_lite( char *widget_name, int irow )
{
  Boolean notify;
  XtRemoveAllCallbacks( widget_id( widget_name ), XmNsingleSelectionCallback ); 
  XmListSelectPos( widget_id( widget_name ), irow, notify ); 
  XtAddCallback( widget_id( widget_name ),  
    XmNsingleSelectionCallback, (XtCallbackProc)line_pq_list_cb, 0 ); 

}



void line_pq_edit_clear ()
/*****************************************************************************\
*
*	Purpose: Copies blanks into to the widget edit list boxes
*
*	Author: Bill Rogers	Dec 14, 1992
*
*	Called by: line_pq_edit_replace, line_pq_edit_insert
*			line_pq_edit_delete
\*****************************************************************************/

{
   
  lookup_and_fill_field( "pq_curve_p_gen_text", " ", 0, 0 );
  lookup_and_fill_field( "pq_curve_q_max_text", " ", 0, 0 );
  lookup_and_fill_field( "pq_curve_q_min_text", " ", 0, 0 );

}


/* DUMMY ROUTINES TO HOLD VUIT UNTIL THEY ARE READY */

void process_pq_radio_buttons ( w, tag, list_data)
Widget w;
char *tag; 
XtCallbackList *list_data;
{

printf("BINGO!\n");
  active = 'A';
  unit   = 'M';

  if( tag[0] == '*' )  active = '*';
  if( tag[0] == 'P' )  unit   = 'P';

}



void build_pq_curve_add_rec( )
/***************************************************************************\
*
*	Purpose:
*
*	Author: Bill Rogers	Dec 16, 1992
*
*	Called by: send_add_data_to_powerflow
*
\**************************************************************************/
{

  Widget id;
  XmString *strlist;
  char *text;
  int i, num_items;

  memset( mod_rec, ' ', sizeof( mod_rec ) );
  mod_rec[0] = 'Q';
  mod_rec[1] = 'P';
  mod_rec[2] = ' ';
  mod_rec[3] = 0;

  if( unit == 'P' )
    strncat( mod_rec, "PU", 2 );
  else if ( unit == 'M' )
    strncat( mod_rec, "MV", 2 );

  mod_rec[5] = active;
  mod_rec[6] = 0;

  strcat_field( mod_rec, "bus_front_name" , 8, "a8"   );
  strcat_field( mod_rec, "bus_front_volts", 4, "f4.0" );

  id = widget_id( "pq_curve_p_gen_list" );
  XtVaGetValues  (id, XmNitemCount, &num_items, XmNitems, &strlist, NULL );

  for ( i=0; i<num_items; i++ )
  {
    if( XmStringGetLtoR ( strlist[i], XmSTRING_DEFAULT_CHARSET, &text ) )
    {
      reformat( text, text, "f6.0" );
      strncat( mod_rec, text, 6 );
    }
  }
  pfchanges_cb( 0, mod_rec, 0 ); 

  mod_rec[1] = 'X';
  mod_rec[18] = 0;
    
  id = widget_id( "pq_curve_q_max_list" );
  XtVaGetValues  (id, XmNitemCount, &num_items, XmNitems, &strlist, NULL );

  for ( i=0; i<num_items; i++ )
  {
    if( XmStringGetLtoR ( strlist[i], XmSTRING_DEFAULT_CHARSET, &text ) )
    {
      reformat( text, text, "f6.0" );
      strncat( mod_rec, text, 6 );
    }
  }
  pfchanges_cb( 0, mod_rec, 0 ); 

  mod_rec[1] = 'N';
  mod_rec[18] = 0;

  id = widget_id( "pq_curve_q_min_list" );
  XtVaGetValues  (id, XmNitemCount, &num_items, XmNitems, &strlist, NULL );

  for ( i=0; i<num_items; i++ )
  {
    if( XmStringGetLtoR ( strlist[i], XmSTRING_DEFAULT_CHARSET, &text ) )
    {
      reformat( text, text, "f6.0" );
      strncat( mod_rec, text, 6 );
    }
  }
  pfchanges_cb( 0, mod_rec, 0 ); 
    
  XtFree( text );
}


void build_pq_curve_del_rec( )
/***************************************************************************\
*
*	Purpose:
*
*	Author: Bill Rogers	Dec 16, 1992
*
*	Called by: send_del_data_to_powerflow
*
\**************************************************************************/
{

  memset( mod_rec, ' ', sizeof( mod_rec ) );
  mod_rec[0] = 'Q';
  mod_rec[1] = 'P';
  mod_rec[2] = 'D';
  mod_rec[3] = ' ';
  mod_rec[4] = ' ';
  mod_rec[5] = ' ';
  mod_rec[6] = 0;

  strcat_field( mod_rec, "bus_front_name" , 8, "a8"   );
  strcat_field( mod_rec, "bus_front_volts", 4, "f4.0" );

/*  printf("New DEL pq curve records are:\n" );
  printf("%s\n", mod_rec );*/
  pfchanges_cb( 0, mod_rec, 0 ); 

  mod_rec[1] = 'X';
/*  printf("%s\n", mod_rec );*/
  pfchanges_cb( 0, mod_rec, 0 ); 

  mod_rec[1] = 'N';
/*  printf("%s\n", mod_rec );*/
  pfchanges_cb( 0, mod_rec, 0 ); 


}


void build_pq_curve_mod_rec( )
/***************************************************************************\
*
*	Purpose:
*
*	Author: Bill Rogers	Dec 16, 1992
*
*	Called by: send_mod_data_to_powerflow (module selection.c)
*
*****   This routine is no longer used.  The "build_pq_curve_add_rec"
*****   routine is used instead for "modify" requests.
*
\**************************************************************************/
{
  int i, flag, num_items;
  XmString *strlist;
  char *text;
  Widget id;

/*  printf("New MOD pq curve records are:\n" );*/

  memset( mod_rec, ' ', sizeof( mod_rec ) );
  mod_rec[0] = 'Q';
  mod_rec[1] = 'P';
  mod_rec[2] = 'M';
  mod_rec[3] = 0;

  if( unit == 'P' )
    strncat( mod_rec, "PU", 2 );
  else if ( unit == 'M' )
    strncat( mod_rec, "MV", 2 );

  mod_rec[5] = active;
  mod_rec[6] = 0;

  strcat_field( mod_rec, "bus_front_name" , 8, "a8"   );
  strcat_field( mod_rec, "bus_front_volts", 4, "f4.0" );

  id = widget_id( "pq_curve_p_gen_list" );
  XtVaGetValues  (id, XmNitemCount, &num_items, XmNitems, &strlist, NULL );
  flag = 0;
  if ( old_unit != unit || old_active != active ) flag = 1;

  for ( i=0; i<num_items; i++ )
  {
    if( XmStringGetLtoR ( strlist[i], XmSTRING_DEFAULT_CHARSET, &text ) )
    {
      reformat( text, text, "f6.0" );
      if ( strncmp( xpgen[i], text, 6 ) )
      {
        strncat( mod_rec, text, 6 );
        flag = 1;
      }
      else
        strncat( mod_rec, "      ", 6 );

      XtFree( text );
    }
    else
      break;	/* end of string data - interrupt the for loop */
  }

  if( flag )
  {
     pfchanges_cb( 0, mod_rec, 0 ); 
/*     printf("%s\n", mod_rec );*/
  }

  mod_rec[1] = 'X';
  mod_rec[18] = 0;

  id = widget_id( "pq_curve_q_max_list" );
  XtVaGetValues  (id, XmNitems, &strlist, NULL );
  flag = 0;
  for ( i=0; i<num_items; i++ )
  {
    if( XmStringGetLtoR ( strlist[i], XmSTRING_DEFAULT_CHARSET, &text ) )
    {
      reformat( text, text, "f6.0" );
      if ( strncmp( xqmax[i], text, 6 ) )
      {
        strncat( mod_rec, text, 6 );
        flag = 1;
      }
      else
        strncat( mod_rec, "      ", 6 );

      XtFree( text );

    }
    else
      break;	/* end of string data - interrupt the for loop */
  }

  if( flag )
  {
     pfchanges_cb( 0, mod_rec, 0 ); 
/*     printf("%s\n", mod_rec );*/
  }

  mod_rec[1] = 'N';
  mod_rec[18] = 0;

  id = widget_id( "pq_curve_q_min_list" );
  XtVaGetValues  (id, XmNitems, &strlist, NULL );
  flag = 0;
  for ( i=0; i<num_items; i++ )
  {
    if ( XmStringGetLtoR ( strlist[i], XmSTRING_DEFAULT_CHARSET, &text ) )
    {
      reformat( text, text, "f6.0" );
      if ( strncmp( xqmin[i], text, 6 ) )
      {
        strncat( mod_rec, text, 6 );
        flag = 1;
      }
      else
        strncat( mod_rec, "      ", 6 );

      XtFree( text );
    }

    else
      break;	/* end of string data - interrupt the for loop */
  }

  if( flag )
  {
     pfchanges_cb( 0, mod_rec, 0 ); 
/*     printf("%s\n", mod_rec );*/
  }

}


void create_pq_record( )
/***************************************************************************\
*
*	Purpose:
*
*	Author: Bill Rogers	Dec 22, 1992
*
*	Called by: add PQ curve pull down menu (bus front jacket)
*
\**************************************************************************/
{
  char str[120];

  enable_pushbutton ( "reset_pushbutton"  );
  enable_pushbutton ( "add_pushbutton"    );
  disable_pushbutton( "delete_pushbutton" );
  disable_pushbutton( "send_pushbutton"   );

  num_pts = 0;

  strcpy(xpgen[0],"      ");
  strcpy(xqmax[0],"      ");
  strcpy(xqmin[0],"      ");

  strcpy( str, "QP   " );
  strcat_field( str, "bus_front_name" , 8, "a8"   );
  strcat_field( str, "bus_front_volts", 4, "f4.0" );

  strncat( str, "   0.0      ", 12 );

  add_pq_curve_rec( str );

  str[1] = 'X';
  add_pq_curve_rec( str );
  str[1] = 'N';
  add_pq_curve_rec( str );

  load_pq_list_widget_id ();	/* ensure w_p_gen, etc. is defined */

  set_jacket( PQ_CURVE_INFO, 'Q', ' ' );

  XmListDeleteAllItems ( w_p_gen  );
  XmListDeleteAllItems ( w_q_max );
  XmListDeleteAllItems ( w_q_min );

  err.line = EM_LINE;
  lookup_and_fill_list("pq_curve_p_gen_list", xpgen, 1 );
  lookup_and_fill_list("pq_curve_q_max_list", xqmax, 1 );
  lookup_and_fill_list("pq_curve_q_min_list", xqmin, 1 );

/*  highlight_pq_list_row( 1 );	 make 1st row inverse video */
		/* removed 4/4/94 on Merilyn's request */
}
