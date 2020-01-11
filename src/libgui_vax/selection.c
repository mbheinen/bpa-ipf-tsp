static char sccsid[]="@(#)selection.c	20.45 6/4/98";
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
/*****************************************************************************\
*	Function: 
*		
*
\*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/List.h>
#include <Xm/SelectioB.h>
#include <ctype.h>
#include <string.h>
#include "dmgr.h"
#include "base_data.h"
#include "ipf_ipc.h" /* define BUFSIZE - do not use BUFSIZ */
#include "em.h"

#define min(A,B) ((A)<(B) ? (A):(B))
em_init			/* special initialization for error message box */
			/* must be in every file using this.		*/

static char out_buffer[BUFSIZE];
extern DBID bus_br_db;
#define NEW_BUS_NAME  "NEWBUSNM500."

#define UNKNOWN_INFO		0
#define	BUS_INFO		1	/* flag to set when editing bus data */
#define	CONT_INFO		2	/*	"	"	"  +bus data */
#define	BRANCH_INFO		3	/*	"	"	 branch data */
#define	TRANSF_INFO		4	/*	"	"    transformer data */
#define	AREA_INFO		5	/*	"	"    area data        */
#define	INTERTIE_INFO		6	/*	"	"    interchange data */
#define	PQ_CURVE_INFO		7
#define	REAC_INFO		8
#define	EQUIV_INFO		9
#define	REGXFMR_INFO		10

extern Widget VUIT_Manage(char *widget_name);
extern int manage2parent( char *type, char *wname, char *wpname);
extern int add_graph_line	( char *namebase12 );
extern int atoi_cnt		( char *str, int size );
extern int getCurBus		(char **);
extern int reformat		(char *, char *, char *);
extern Widget widget_id		( char * );
extern Vuit_MU			( char *, char * );
extern char *month_to_three_char( char month );
extern char three_char_to_month	( char * );
extern DB_STAT findfirstbusrec	( PFBASERECORD **nextbusrec);
extern int loadBusBranch	( Widget, XtPointer, XmAnyCallbackStruct *);
extern int pfchanges_cb		( Widget, XtPointer, XmAnyCallbackStruct * );
extern void stringPart		( char *, char *, int, int );
extern int setCurBus		( char * );
extern int setCurLine		( char * );
extern int loadArea2		( );
extern void load_pq_list_widget_id();
extern void read_pq_from_db	( char * );
extern void build_pq_curve_add_rec();
extern void build_pq_curve_del_rec();
/*
* not used, use "add" instead
* extern void build_pq_curve_mod_rec();
*/
extern int  changeGraphBusVertex( char *oldnamebase, char *namebase );
extern int  deleteVertexbyName	( char *bus_namevolt );
extern int  turnoffVertexbyName	( char *bus_namevolt );
extern int  deleteEdgebyName	( char *type, char *namebase1, char *namebase2);
extern int  turnoffEdgebyName	( char *type, char *namebase1, char *namebase2);
extern void disable_pushbutton	( char * );
extern void enable_pushbutton	( char * );
extern void set_button_sensivity( char *wid_name, Boolean v1 );
extern void lookup_and_reduce_text_field ( char *text_name, int size );
extern void clip_outside_blanks	( char * );
extern void clip_trailing_blanks( char * );
extern int  HashLookup		( char *name, Widget *id );
extern int  askUser             ( Widget, char *, char *, char * );

/* bus data routines */
  void display_bus_selection	( char *bus_string, DBID junkbus_br_db );
  void add_bus_cascade_pb	( Widget pd_id,
				  char *label, 
				  void (*cb_data)(Widget wid, XtPointer tag,
					 XmAnyCallbackStruct *cb_routine), 
					 XtPointer cbs );
  void fill_bus_dialog2		( char *bus_string );
  void fill_bus_dialog_cb	( Widget, XtPointer, XmAnyCallbackStruct * );
  int  fill_bus_selection_box	( Widget, XtPointer, XmAnyCallbackStruct * );
  void clear_all_bus_jkts_text	();
  void clear_text_widget	( char *name, int size );
  void fill_bus_out_dialog	();
  void fill_bus_out_texts	( char * );
  void get_bus_selection	( Widget, XtPointer,
					  XmSelectionBoxCallbackStruct *cbs );
  void set_bus_type		( Widget, XtPointer, XmAnyCallbackStruct * );
  int  manage_bus_jackets	( char *, char *, int, int, int, int, int );

/* branch data routines */
  void get_branch_selection	( Widget w, XtPointer client_data,
				  XmSelectionBoxCallbackStruct *cbs );
  void fill_branch_jacket_cb_sb	( Widget, XtPointer client_data,
					  XmSelectionBoxCallbackStruct *cbs );
  void fill_branch_jacket_cb_pd	( Widget, XtPointer, XmAnyCallbackStruct * );
  void fill_branch_jckt		( char *);

/* branch data routines */
  int  fill_branch_selection_box();
  void manage_bus_pix		( char );

  void fill_transf_jckt		( char * );

  void fill_regxfmr_jckt	( char * );
  void process_regxfmr_rb(Widget w, XtPointer tag, XmAnyCallbackStruct *cbs );


/* continuation bus data routines */
  void set_cont_type		( Widget, XtPointer, XmAnyCallbackStruct * );
  void fill_cont_jacket		( Widget, XtPointer, XmAnyCallbackStruct * );
  void create_cont_record	( Widget, XtPointer, XmAnyCallbackStruct * );

/* switched reactance data routines */
  void fill_reac_jacket		( Widget, XtPointer, XmAnyCallbackStruct * );
  void create_reac_rec		( Widget, XtPointer, XmAnyCallbackStruct * );
  void create_line_rec_2bus     ( char * );
                                           
  void create_line_rec		( Widget, XtPointer, XmAnyCallbackStruct * );
  void create_xfmr_rec		( Widget, XtPointer, XmAnyCallbackStruct * );
  void create_xfmr_shift_rec    ( Widget, XtPointer, XmAnyCallbackStruct * );
  void create_equiv_rec		( Widget, XtPointer, XmAnyCallbackStruct * );
  void create_regxfmr_rec	( Widget, XtPointer, XmAnyCallbackStruct * );
  void create_dc_2_term_rec	( Widget, XtPointer, XmAnyCallbackStruct * );
  void create_dc_multi_term_rec	( Widget, XtPointer, XmAnyCallbackStruct * );
  void create_bus_component	( char, char, int );
  Boolean bus_not_created	( );
  Boolean check_for_zero_impedance();
  Boolean either_is_non_zero	( char *fld_1, char *fld_2 );

/* area data routines */
  void get_area_selection( Widget w, XtPointer client_data,
                                 XmSelectionBoxCallbackStruct *cbs );

  void fill_area_interchange	();
  void set_area_cont_edit	( Widget, XtPointer, XmAnyCallbackStruct * );

  void fill_pq_jacket		( Widget, XtPointer, XmAnyCallbackStruct * );

/* misc support routines */
  void set_jacket		  ( int stype, char type, char subtype );
  char *add_decimal               ( char *, int, int, char * );
  void change_field               ( char *, char *, char *, int, int, char *);
  int  char_pos			  ( char *, char );
  int  left_digits		  ( char * );
  void lookup_and_fill_field      ( char *, char *, int, int );
  void lookup_and_fill_field_w_dec( char *, char *, int, int, int );
  void lookup_and_get_field       ( char *, char *, int );
  int  ck_col_params		  ( int, int, char *, char *);
  void add_blanks_if_short	  ( char *, int, char * );
  void add_blanks_to_string	  ( char *, int );
  void reset_data                 ();
  void send_add_data_to_powerflow ( Widget, XtPointer, XmAnyCallbackStruct * );
  void send_del_data_to_powerflow ( Widget, XtPointer, XmAnyCallbackStruct * );
  void send_mod_data_to_powerflow ( Widget, XtPointer, XmAnyCallbackStruct * );
  int check_sub_type		  ( );
  void strcat_field		  ( char *, char *, int, char *fmt );
  int  reformat			  ( char *, char *, char * );
  void field_spec_init		  ();
  void field_spec		  ( char *, int, int, char *, char *, int );
  int  find_first_field_spec	  ( char *type );
  void record_to_text_boxes	  ( char * );
  void build_add_rec		  ( char * );
  void build_mod_rec		  ( char * );
  void set_title_mode		  ( int new_mode );
  void set_rec_type		  ( char rec, char stype, int info, int mode );
  void set_regxfmr_jckts	  ( );
  void set_regxfmr_jckts_cb	  ( Widget w, XtPointer tag,
					 XmAnyCallbackStruct *cbs);
  void cascade_pd_label_set	  ( char *, char * );
void set_title_mode_2namebase	( Widget w, XtPointer tag,
                                         XmAnyCallbackStruct *cbs);
void set_title_mode_linename	( Widget w, XtPointer tag,
                                         XmAnyCallbackStruct *cbs);
int area_control_cb		( Widget w, XtPointer tag,
					 XmAnyCallbackStruct *cbs);
int intertie_cb			( Widget w, XtPointer tag,
					 XmAnyCallbackStruct *cbs);
void set_area_card_mode		( Boolean flg, char  *m );

   


/*****************************************************************************/


extern int area_box_fill_flag;
extern int branch_box_fill_flag;
extern int bus_box_fill_flag;
Boolean bus_not_created_flg;
Boolean area_cont_card;

int send_type;		/* pushbutton action i.e. BUS, BRANCH, AREA etc.*/
			/* warning - used by toolbox.c */
char rec_type;
char sub_type;
int  title_mode;
#define BUS_PULLDOWN	0
#define LINENAMEONLY	1
#define ONE_NAME_BASE	2
#define TWO_NAME_BASE	3
Boolean any_regxfmr_type = False;

char active_gen_jacket[30]; 	/* holds name of one of 3 types of gen jackets*/

char bus_namevolt  [20];

static char mod_rec    [120];		/* mod data to send to powerflow */
char org_bus    [120];
char org_data	[120];
char org_area   [120];
char br_type;		 /* warning - used by toolbox.c */
char var_tap;

#define	MAXFLDS		550
char fld_crd_type[MAXFLDS][2];
int  fld_col_one [MAXFLDS];
int  fld_col_end [MAXFLDS];
char fld_format  [MAXFLDS][6];
char fld_text_box[MAXFLDS][40];
int  fld_instr   [MAXFLDS];
static int not_initialized=1;
static num_flds=0;
 
static Widget cscd_id[50];   /* list of cascaded pushbutton widgets created */
static int n_opts = 0;       /* number of pushbutton widgets created */

static Boolean bus_help_not_disabled = False;

Widget list_widget;
XmListCallbackStruct *cbs;


int fill_bus_selection_box ( Widget w, XtPointer tag,
				XmAnyCallbackStruct *cbs)
/*****************************************************************************\
*	Purpose:  First routine called.....  extracts all buses from dmgr
*		  system and loads data into area for later acess by MOTIF.
*		
*	Author: Dan Clark/Bill Rogers		Modified: Oct 06, 1992
*
*	Called by: "Bus/Branch" push button callback (In "Edit" pull-down)
*
\*****************************************************************************/
{

  Widget  id, list_id;
  int i=0;			/* bus count				*/
  XmString bus_record[10000];	/* future holding array of all buses	*/

  PFBASERECORD *busvalrec;	/* ptr to key for bus search		*/
  PFBASERECORD *nextbusrec;	/* ptr returned for next bus rec	*/
				/* used by db_search & db_next		*/

  DB_STAT stat;			/* status returned for general db access */

  /** if no base table exists just return (no data ) for list **/
  if( db_valid_db(&base_db) != D_OKAY) {
    bus_box_fill_flag = FALSE;
    return(0);
  }

  if ( bus_box_fill_flag == FALSE )	/* if table not loaded */
  {

    if(( id = widget_id( "bus_selection_dialog" )) == FALSE) return;

    list_id = (Widget)XmSelectionBoxGetChild( id, XmDIALOG_LIST);

    XtSetSensitive ( (Widget)XmSelectionBoxGetChild( id, XmDIALOG_HELP_BUTTON),
                      False );

    XtSetSensitive ( XmSelectionBoxGetChild( id, XmDIALOG_APPLY_BUTTON),
		     False );
 
/****************************************************************************\
\**** retrieve all the records (in key order) and write to output file ******/

    if( ( stat = findfirstbusrec( &nextbusrec ) ) == D_NOTFOUND )
      return( 1 );


    /******** PHASE 3 - LOOP TO GET ALL BUSES *****/ 
    while(stat == D_FOUND)
    {
      if( nextbusrec->type[0] == 'B' ) {	 /* add data to bus_record */
        bus_record[i++] = XmStringCreate( (char *)nextbusrec,
					   XmSTRING_DEFAULT_CHARSET);
      }

      stat=db_next( &base_db, &nextbusrec, &nextbusrec, BSE_TYPE_NAME1_BASE1);

    }

    /******** PHASE 4 - BULLDOZE BUSES INTO MOTIF BASE *****/ 
    XtVaSetValues( id, XmNlistItems, bus_record, XmNlistItemCount, i, NULL);

    printf("%d items added into BUS selection box\n", i );

    bus_box_fill_flag = TRUE;	/* set flag to show bus data is loaded	   */
				/* assures next call won't reload the base */
  }

  return(0);	/* return control to MOTIF until user makes bus selection */

}


void get_bus_selection( Widget list_widget, XtPointer client_data, 
					XmSelectionBoxCallbackStruct *cbs )
/*****************************************************************************\
*	Purpose: After bus_selection popup dialog is shown, this routine
*		  returns the highlight user selection.
*  OBSOLETE dlc may 25, 1993 see get_bus_alpha_selection
*
*	Author: B.Rogers (from example pp 170, Motif Programming Manual
*		Jan 29, 1991		Modified: Jul 22, 92
*
*	Called by: Callback on "Edit Bus" button in 'SelectionBox' window.
*
\*****************************************************************************/
{
  char value_str[133];
  char *value = value_str;
     
    XmStringGetLtoR( cbs->value, XmSTRING_DEFAULT_CHARSET, &value );

    if ( (int)strlen( value ) > 0 )	/* prevents bomb if nothing selected */
    {

      Vuit_MU ( "M", "bus_front_box" );	/* Manage the main box 		     */
      Vuit_MU ( "U", "bus_front_line_name" );
      Vuit_MU ( "U", "line_type_pd" );

      display_bus_selection( value, bus_br_db );/* Manage the required boxes */

    } 
}

void get_bus_alpha_select( Widget list_widget, XtPointer client_data, 
					XmSelectionBoxCallbackStruct *cbs )
/*****************************************************************************\
*	Purpose: After bus_selection popup dialog is shown, this routine
*		  returns the highlight user selection.
*
*	Author: B.Rogers (from example pp 170, Motif Programming Manual
*		Jan 29, 1991		Modified: Jul 22, 92
*
*	Called by: Callback on "Edit Bus" button in 'SelectionBox' window.
*
\*****************************************************************************/
{
  char *newnamebase;
  char value_str[133];

  getCurBus(&newnamebase);

  sprintf( value_str,  "B     %12.12s", newnamebase);

     
    if ( (int)strlen( newnamebase) > 0 ) /* prevents bomb if nothing selected */
    {

      Vuit_MU ( "M", "bus_front_box" );	/* Manage the main box 		     */
      Vuit_MU ( "U", "bus_front_line_name" );
      Vuit_MU ( "U", "line_type_pd" );

      display_bus_selection( value_str, bus_br_db );/* Manage the required boxes */

    } 
}


void display_bus_selection( char *bus_string, DBID junkbus_br_db )
/*****************************************************************************\
*	Purpose: Given bus name, base,  this routine searches dmgr for
*			+buses and branches associated with bus.
*			Creates cascade pushbuttons for bus_front_box window.
*			Makes a 32-character label for each pushbutton.
*		
*	Author: Bill Rogers	May 1992	Modified:
*
*	Called by: get_bus_selection
*		   send_mod_data_to_powerflow
*		   popup_input_dia  (vertex.c)
*
\*****************************************************************************/
{
#define CASCADE_LENGTH 32

  Widget pd_id;   
  Widget opt_menu_id;
  Widget opt_label_id;
  XmString bus_title;
  int isQrecord;
  int i;

  char pf_rec[80];
  char pd_title[80];

  PFBASERECORD keyvalrecord;    /* storage for db_search key values */
                                /* used for any search */
  static PFBASERECORD *busvalrec;       /* ptr to key for bus search */
  static PFBASERECORD *nextbusrec;      /* ptr returned for next bus rec */
  DB_STAT statbus;      /* status returned for bus queries of db */
 
  if ( (int)strlen( bus_string ) < 10 )
  {  printf("*** WARNING *** from routine display_bus_selection\n");
     printf("                parmeter 'bus_string' appears empty\n");
     printf("bus_string:(%s)\n\n", bus_string );
  }

  if( bus_string[0] != 'B' && bus_string[2] != ' ' )
  {
    printf(" *** PROGRAMMER WARNING ****\n" );
    printf(" Call to display_bus_selection should have data in\n");
    printf(" bus record format.  Name/base should be in columns 8-24.\n");
    printf("   (Data looks suspicious)\n\n");
  }

  /*** dlc create a new bus_br_db **/
  loadBusBranch((Widget)NULL, (XtPointer )bus_string, 
                (XmAnyCallbackStruct *)NULL);

  while(n_opts > 1)
    XtUnmanageChild(cscd_id[ --n_opts]); /* Was XtDestroyWidget JLR 18 Feb */
  n_opts = 1;

  pd_id = widget_id("bus_pulldown");

  isQrecord = 0;	/* presume no pq curves at this time */
  set_button_sensivity( "bus_jacket_new_option_menu", True ); /*printopts.c*/
  enable_pushbutton("bus_jacket_pq_curve_pb");
  enable_pushbutton("bus_jacket_swr_reac_pb");
  Vuit_MU( "U", "bus_bd_button" );	/* ensure no DC bus change */
  Vuit_MU( "U", "bus_bm_button" );

  bus_title = XmStringCreate((char *)"Bus", XmSTRING_DEFAULT_CHARSET);
  opt_menu_id = widget_id("bus_item_pulldown");

  XtVaSetValues(opt_menu_id, 
		XmNbuttonSet, (int)0, 
		XmNpostFromButton, (int)0,  
		NULL);
  opt_label_id = XmOptionButtonGadget(opt_menu_id);
  XtVaSetValues( opt_label_id, XmNlabelString, bus_title, NULL);

  stringPart(bus_string, bus_namevolt, 6, 17);
  memset(&keyvalrecord, ' ', sizeof(keyvalrecord));
  busvalrec = &keyvalrecord;
  strncpy(busvalrec->bus.name, bus_namevolt, 12);
  reformat(&busvalrec->bus.name[8], &busvalrec->bus.name[8], "%f4.0");
  busvalrec->bus.eol[0] = '\0';
  statbus = db_search( &bus_br_db, &busvalrec, &nextbusrec, BSE_BRANCH );
  if ( D_EOF == statbus )
  {
    printf("*** FATAL ERROR AVERTED *** in display_bus_selection\n" );
    printf("     db_search did not find bus record (%s)\n", bus_namevolt );
    printf("     Cascade pulldown not fully created.\n" );
    return;
  } else {
    statbus = D_FOUND;
  } 

  while(statbus == D_FOUND)
  {
    if( (strncmp( nextbusrec->bus.name, busvalrec->bus.name, 12 ) == 0) )
    {
      strncpy( pf_rec, (char *)nextbusrec, CASCADE_LENGTH );
      switch ( pf_rec[0] )
      {

	case 'B':
	  disable_pushbutton( "add_pushbutton"    );/* gray-out this button */
	  enable_pushbutton ( "delete_pushbutton" );
	  enable_pushbutton ( "close_pushbutton"  );
	  enable_pushbutton ( "send_pushbutton"  );
	  enable_pushbutton ( "reset_pushbutton"  );

	  strncpy ( org_bus, (char *)nextbusrec , sizeof(org_bus) );
	  sub_type = org_bus[1];
 
	  clear_all_bus_jkts_text();

	  fill_bus_dialog2( org_bus );

          if ( cscd_id[0] != NULL ) 
             XtRemoveAllCallbacks( cscd_id[0], XmNactivateCallback );

	  cscd_id[0] = widget_id( "first_button" );
	  XtAddCallback ( cscd_id[0],
	  		  XmNactivateCallback,
	  		  (XtCallbackProc)fill_bus_dialog_cb,
	  		  nextbusrec );		/* value passed when called */
        break;

	case '+':
	  pf_rec[ 6 ] = 0;

          add_bus_cascade_pb( pd_id, pf_rec, fill_cont_jacket,
			       (XtPointer )nextbusrec );
	break;


	case 'X':
	  pf_rec[ 6 ] = 0;

          add_bus_cascade_pb( pd_id, pf_rec, fill_reac_jacket,
			       (XtPointer )nextbusrec );
          disable_pushbutton ( "bus_jacket_swr_reac_pb" );
	break;

	case 'L':
	case 'T':
	case 'R':	/* regulating transf  */
	case 'E':	/* equiv trans line   */

			/* build the pushbutton label.. */
	  pd_title[0] = pf_rec[0];
	  pd_title[1] = pf_rec[1];
	  pd_title[2] = 0;
	  strcat ( pd_title, " " );
	  strncat ( pd_title,
		    nextbusrec->branch.name2 , 
                    sizeof(nextbusrec->branch.name2) );
	  strncat ( pd_title, 
		    nextbusrec->branch.base2, 
                    sizeof(nextbusrec->branch.base2) );
	  strcat ( pd_title, " " );
	  strncat ( pd_title, 
		    nextbusrec->branch.circuit, 
                    sizeof(nextbusrec->branch.circuit));
	  strcat ( pd_title, " " );
	  strncat ( pd_title, 
		    nextbusrec->branch.section, 
                    sizeof(nextbusrec->branch.section));
	  pd_title[19] = 0;

          add_bus_cascade_pb( pd_id, pd_title, fill_branch_jacket_cb_pd,
			       (XtPointer )nextbusrec );
	break;

	case 'Q':
           /** if there is a  Q type card make a menu item to get to it **/
           /** but only make one menu item even if there are 3 q cards **/
           if ( 0 == isQrecord){

           add_bus_cascade_pb( pd_id, "PQ_CURVES", fill_pq_jacket,
			       (XtPointer )nextbusrec );
           isQrecord = 1;

		/* don't let user add any addition pq cards! */
           disable_pushbutton( "bus_jacket_pq_curve_pb");
          }
        break;

        default:
          printf("*** WARNING *** from display_bus_selection.\n" );
	  printf("  Unable to process following record:\n(%s)\n\n", nextbusrec);
      }

    }
#ifdef REMOVE
    else     /** ran past cards with the right bus name **/
    {
       break;
    }
#endif
      statbus = db_keynext( &bus_br_db,
			    &nextbusrec,
	                    &nextbusrec,
			    BSE_BRANCH );
   }

#ifdef REMOVE
  /** now get the far end line records **/
  busvalrec = &keyvalrecord;
  strncpy ( busvalrec->branch.name2, bus_namevolt,   12 );
  strncpy ( busvalrec->branch.name1, "            ", 12 );

  statbus = db_search( &bus_br_db,
		       &busvalrec,
                       &nextbusrec,
		       BSE_NAME2_BASE2);

  while(statbus == D_FOUND)
  {
    if( (strncmp( nextbusrec->branch.name2, busvalrec->branch.name2, 12 ) ==0))
    {
      pd_title[0] = 0;
      strncat ( pd_title, (char *)nextbusrec, 1 );
      strcat  ( pd_title, " " );
      strncat ( pd_title, 
		nextbusrec->branch.name1 , 
                sizeof(nextbusrec->branch.name1) );
      strncat ( pd_title, 
		nextbusrec->branch.base1 , 
                sizeof(nextbusrec->branch.base1) );
      pd_title[14] = 0;

      add_bus_cascade_pb( pd_id, pd_title, fill_branch_jacket_cb_pd,
			       (XtPointer )nextbusrec );

      statbus = db_keynext( &bus_br_db,
			    &nextbusrec,
                            &nextbusrec,
			    BSE_NAME2_BASE2 );

    }
    else
    {
        /** ran past cards with the right bus name **/
      break;
    }
  }
#endif

/* JLR 18 Feb
for(i = 0; i <= n_opts; ++i)
{
  fprintf(stderr, "cscd_id[%d] = %d\n", i, (int)cscd_id[i]);
}
*/

  XtManageChildren(cscd_id, n_opts);

}



void clear_all_bus_jkts_text()
/***************************************************************************\
* Purpose: Clear possible residual data from any of the three mini-jackets.
*	   and all optional boxes
* Author:  W.E. Rogers 3/17/95
\***************************************************************************/
{
  clear_text_widget( "bus_p_max"      , 4);
  clear_text_widget( "bus_p_gen"      , 5 );
  clear_text_widget( "bus_q_sched"    , 5 );
  clear_text_widget( "bus_q_min"      , 5 );

  clear_text_widget( "bus_p_max_2"    , 4 );
  clear_text_widget( "bus_p_gen_2"    , 5 );
  clear_text_widget( "bus_q_max"      , 5 );
  clear_text_widget( "bus_q_min_2"    , 5 );

  clear_text_widget( "bus_p_max_3"    , 4 );
  clear_text_widget( "bus_phase"      , 4 );
  clear_text_widget( "bus_q_sched_3"  , 5 );
  clear_text_widget( "bus_q_min_3"    , 5 );

  clear_text_widget( "bus_v_max"      , 4 );
  clear_text_widget( "bus_v_hold"     , 4 );
  clear_text_widget( "bus_phase_xx"   , 4 );
  clear_text_widget( "bus_v_min"      , 4 );
  clear_text_widget( "bus_remote_name", 7 );
  clear_text_widget( "bus_remote_base", 3 );
  clear_text_widget( "bus_remote_pcs" , 3 );
}


void clear_text_widget( char *name, int size )
{
  Widget id;

  if( size >12 )  {
    printf("*** WARNING *** from clear_text_widget (selection.c)\n" );
    printf("     Does not clear texts greater than 12 characters\n" );
  }

  if(HashLookup(name, &id) == False) return;

  lookup_and_fill_field( name, "            "   , 0, size );

}


void add_bus_cascade_pb( Widget pd_id,
			 char *label, 
void (*cb_routine)(Widget wid, XtPointer tag, XmAnyCallbackStruct *cb_routine), 
  XtPointer cb_data )
/****************************************************************************\
*
*	Purpose: Standardized add - to create a new pushbutton to the
*		 bus pulldown menu.
*
*	Author: Bill rogers	Jan 11, 93
*
\****************************************************************************/
{
  Widget id;

  if ( n_opts < 50 )
  {
    id = XmCreatePushButton( pd_id, label, NULL, 0 );

    XtAddCallback ( id,
		    XmNactivateCallback,
		    (XtCallbackProc)cb_routine, 
		    cb_data );

    cscd_id[ n_opts++ ] = id;
  }
}


void fill_bus_dialog_cb( Widget w, XtPointer tag,
				 XmAnyCallbackStruct *cbs)
/***************************************************************************\
*
* Called by: "Edit Bus/Branch" button on cascade pulldown
*		"Bus" cascade pd on bus front dialog
*
\***************************************************************************/
{

  disable_pushbutton( "add_pushbutton"    );	/* grey out this button */
  disable_pushbutton( "delete_pushbutton" );
  enable_pushbutton ( "send_pushbutton"   );	/* re-activate this button */
  enable_pushbutton ( "close_pushbutton"  );	/* ensure its active */
	  enable_pushbutton ( "reset_pushbutton"  );

  XtVaSetValues( (widget_id("bus_front_name")) , XmNeditable, FALSE, NULL );
  XtVaSetValues( (widget_id("bus_front_volts")), XmNeditable, FALSE, NULL );

  Vuit_MU( "U", "bus_bd_button"    );
  Vuit_MU( "U", "bus_bm_button"    );

  sub_type = org_bus[1];
  fill_bus_dialog2( org_bus );
}
/*****************************************************************************\
* purpose: just manage the widgets for the bus type passed in
*
*
\*****************************************************************************/
manage_bus_dialog( char *bus_str) 
{
  Widget id, id2;   
  Widget option_button_id;

  if ( bus_str[0] != 'B' )
  {
    printf("** WARNING ** from manage_bus_dialog (selection.c) - bad input string\n");
    printf("   Following string should have type/subtype/mod/owner/name/volts format.\n");
    printf("   And begin with 'B'\n" );
    printf("(%s)\n", bus_str );
    return;
  }

  /* NOTE: bus sub-type should not be set here!*/
  set_title_mode( BUS_PULLDOWN );
  any_regxfmr_type = False;

  Vuit_MU( "M", "bus_front_name"     );
  Vuit_MU( "M", "bus_front_volts"    );
  Vuit_MU( "U", "bus_front_line_name");
  Vuit_MU( "U", "line_type_pd"       );
  Vuit_MU( "M", "bus_item_pulldown"  );

  set_jacket( BUS_INFO, 'B', sub_type );
  enable_pushbutton( "sectionalize_pushbutton" );
  enable_pushbutton( "bus_item_pulldown" );

  manage_bus_pix ( sub_type ); 		/* display pix-map picture */

  switch(sub_type)
  {
    case ' ':
      if( manage_bus_jackets("bus_blank_button", "B ", 
                        FALSE, TRUE, TRUE, FALSE, FALSE ) == FALSE )  return; 
		     /* Vhold  V-max V-min Remote Phase */
      strcpy( active_gen_jacket, "bus_gen_sched" );
    break;

    case 'C':
      if( manage_bus_jackets("bus_bc_button", "BC", 
                        TRUE, FALSE, FALSE, FALSE, FALSE )== FALSE  )  return; 
        strcpy( active_gen_jacket, "bus_gen_sched" );
    break;

    case 'E':
      if( manage_bus_jackets("bus_be_button", "BE", 
                        TRUE, FALSE, FALSE, FALSE, FALSE )== FALSE  )  return; 
      strcpy( active_gen_jacket, "bus_gen_max" );
    break;

    case 'F':
      if( manage_bus_jackets("bus_be_button", "BF", 
                        TRUE, FALSE, FALSE, FALSE, FALSE )== FALSE  )  return; 
      strcpy( active_gen_jacket, "bus_gen_max" );
    break;

    case 'G':
      if( manage_bus_jackets("bus_bg_button", "BG", 
                        FALSE, TRUE, TRUE, TRUE, FALSE )== FALSE  )  return; 
      strcpy( active_gen_jacket, "bus_gen_max" );
    break;

    case 'Q':
      if( manage_bus_jackets("bus_bq_button", "BQ", 
                        TRUE, FALSE, FALSE, FALSE, FALSE )== FALSE  )  return; 
      strcpy( active_gen_jacket, "bus_gen_max" );
    break;

    case 'S':
      if( manage_bus_jackets("bus_bs_button", "BS", 
                        TRUE, FALSE, FALSE, FALSE, TRUE )== FALSE  )  return; 
      strcpy( active_gen_jacket, "bus_gen_sched" );
    break;

    case 'T':
      if( manage_bus_jackets("bus_bt_button", "BT", 
                        TRUE, FALSE, FALSE, FALSE, FALSE )== FALSE  )  return; 
      strcpy( active_gen_jacket, "bus_gen_sched" );
    break;

    case 'V':
      if( manage_bus_jackets("bus_bv_button", "BV", 
                        FALSE, TRUE, TRUE, FALSE, FALSE )== FALSE  )  return; 
      strcpy( active_gen_jacket, "bus_gen_sched" );
    break;

    case 'X':
      if( manage_bus_jackets("bus_bx_button", "BX", 
                        FALSE, TRUE, TRUE, TRUE, FALSE )== FALSE  )  return; 
      strcpy( active_gen_jacket, "bus_gen_max" );
    break;

    case 'M':
    case 'D':
      active_gen_jacket[0] = 0;
    break;

    default:
      printf("Unknown bus type %c\n", sub_type);
      return;
  }

  manage2parent( "U", "bus_gen_max"   , "bus_jacket" );
  manage2parent( "U", "bus_gen_phase" , "bus_jacket");
  manage2parent( "U", "bus_gen_sched" , "bus_jacket");
  if( active_gen_jacket[0] != 0 ) 
    manage2parent( "M", active_gen_jacket, "bus_jacket" );

}


void fill_bus_dialog2 ( char *bus_str )
/*****************************************************************************\
*	Purpose:  Fills in the bus jacket.
*
*	Author: B.Rogers
*		Feb 20, 1991		Modified:	Jul 09, 1992
*
*	Called by: display_bus_selection
*		   fill_bus_dialog (Edit Bus/Branch pd)
*		   create_bus_record
*		   set_bus_type
*		   reset_data
*
\*****************************************************************************/

{
  Widget id, id2;   
  Widget option_button_id;

  int v_hold, v_min, v_max, remote;

  if ( bus_str[0] != 'B' )
  {
    printf("** WARNING ** from fill_bus_dialog2 - bad input string\n");
    printf("   Following string should have type/subtype/mod/owner/name/volts format.\n");
    printf("(%s)\n", bus_str );
    return;
  }
  
  manage_bus_dialog(bus_str);

/* establish reset data for reset button */
  strncpy ( org_data, bus_str, sizeof(org_data) ); 

  lookup_and_fill_field( "bus_front_name",  org_data,  6, 13 );
  lookup_and_fill_field( "bus_front_volts", org_data, 14, 17 );

  record_to_text_boxes( bus_str );

}


int manage_bus_jackets ( char *button_name, char *box_title,
				int vhold, int vmin, int vmax,
				int remote,
				int phase )
/*****************************************************************************\
*	Purpose:
*		
*	Author: Bill Rogers	Jul 92		Modified: Jun 22, 93
*
*	Called by:  fill_bus_dialog2
*
\*****************************************************************************/
{
  Widget button_id;

  if( ( button_id = widget_id( button_name ) ) == FALSE ) return(0);

  cascade_pd_label_set( "bus_type_menu", box_title );

  Vuit_MU( "U", "bus_v_hold_label");	/* see util.c for this routine */
  Vuit_MU( "U", "bus_v_hold"      );
  Vuit_MU( "U", "bus_v_max_label" );
  Vuit_MU( "U", "bus_v_max"       );
  Vuit_MU( "U", "bus_v_min_label" );
  Vuit_MU( "U", "bus_v_min"       );
  Vuit_MU( "U", "bus_phase_label_x");
  Vuit_MU( "U", "bus_phase_xx"    );

  Vuit_MU( "U", "bus_remote_label");
  Vuit_MU( "U", "bus_remote_name" );
  Vuit_MU( "U", "bus_remote_base" );
  Vuit_MU( "U", "bus_pcs_label"   );
  Vuit_MU( "U", "bus_remote_pcs"  );

  if ( vhold )
  {
    Vuit_MU( "M", "bus_v_hold_label");
    Vuit_MU( "M", "bus_v_hold"      );
  }

  if ( vmax )
  {
    Vuit_MU( "M", "bus_v_max_label" );
    Vuit_MU( "M", "bus_v_max"       );
  }

  if ( vmin )
  {
    Vuit_MU( "M", "bus_v_min_label" );
    Vuit_MU( "M", "bus_v_min"       );
  }

  if ( remote )
  {
    Vuit_MU( "M", "bus_remote_label");
    Vuit_MU( "M", "bus_remote_name" );
    Vuit_MU( "M", "bus_remote_base" );
    Vuit_MU( "M", "bus_pcs_label"   );
    Vuit_MU( "M", "bus_remote_pcs"  );
  }

  if ( phase )
  {
    Vuit_MU( "M", "bus_phase_label_x" );
    Vuit_MU( "M", "bus_phase_xx"      );
  }

  return(1);
}



void manage_bus_pix( char sub_type )
/*****************************************************************************\
*	Purpose:
*		
*	Author: Bill Rogers		Modified: May 05, 1992
*
*	Called by:  fill_bus_dialog2
*
\*****************************************************************************/
{
  static char last_pix[30];

  if ( last_pix[0] != '\0') Vuit_MU( "U", last_pix );

  switch (sub_type)
  {
    case ' ':
      strcpy( last_pix, "B_picture" );
    break;

    case 'C':
      strcpy( last_pix, "BC_picture" );
    break;

    case 'E':
    case 'F':
      strcpy( last_pix, "BE_picture" );
    break;

    case 'G':
      strcpy( last_pix, "BG_picture" );
    break;

    case 'Q':
      strcpy( last_pix, "BQ_picture" );
    break;

    case 'S':
      strcpy( last_pix, "BS_picture" );
    break;

    case 'T':
      strcpy( last_pix, "BT_picture" );
    break;

    case 'V':
      strcpy( last_pix, "BV_picture" );
    break;

    case 'X':
      strcpy( last_pix, "BX_picture" );
    break;

    case 'M':
    case 'D':
	/* not ready */
    break;

    default:
      printf("*** WARNING *** in call to manage_bus_pix\n");
      printf("                No such bus type '%c'\n");
      last_pix[0] = '\0';
      return;
  }

  if ( last_pix[0] != '\0') 
    manage2parent( "M", last_pix, "bus_jacket" );

}


void create_bus_rec ( )
/*****************************************************************************\
*	Purpose: Give a name to NEW_BUS_NAME
*		
*	Author: Bill Rogers		Jan 15, 1993
*
*	Called by: addBus2Base (toolbox.c)
*		   create_new_ipf_bus ( curbus.c )
*
\*****************************************************************************/
{
  char *newnamebase;

  getCurBus(&newnamebase);

  Vuit_MU ( "M", "bus_front_box" );		/* Manage the main box 	*/
  manage2parent("M", "bus_jacket", "bus_front_box");
  if( bus_help_not_disabled )
  {
#ifdef notdef
    Vuit_MU ( "M", "bus_help_dialog" );
#endif
    Vuit_MU ( "M", "bus_help_1" );
    Vuit_MU ( "M", "bus_help_2" );
    Vuit_MU ( "U", "bus_help_3" );
    Vuit_MU ( "U", "bus_help_4" );
  }
  sprintf( org_data,  "B     %12.12s", newnamebase);
  add_blanks_to_string( org_data, 80 );
  sprintf( org_bus, "B      %12.12s", newnamebase);

  disable_pushbutton( "delete_pushbutton" );
  disable_pushbutton( "send_pushbutton" );
  enable_pushbutton ( "add_pushbutton" );
  enable_pushbutton ( "close_pushbutton"  );	/* ensure its active */
  enable_pushbutton ( "reset_pushbutton"  );

  Vuit_MU( "M", "bus_bd_button"); /*  temporary enable in case user */
  Vuit_MU( "M", "bus_bm_button"); /*  wants a DC bus */

  set_button_sensivity( "bus_jacket_new_option_menu", False ); /*printopts.c*/

  bus_not_created_flg = TRUE;

  clear_all_bus_jkts_text();

  sub_type = ' ';
  fill_bus_dialog2( org_data );
  
  disable_pushbutton( "sectionalize_pushbutton" );
  disable_pushbutton( "bus_item_pulldown" );

  /* this is the only time users may change bus name and volts */
  XtVaSetValues( (widget_id("bus_front_name")) , XmNeditable, TRUE, NULL );
  XtVaSetValues( (widget_id("bus_front_volts")), XmNeditable, TRUE, NULL );


}

void disable_bus_help ()
{
  bus_help_not_disabled = False;
#ifdef notdef
  Vuit_MU ( "U", "bus_help_dialog" );
#endif
}

Boolean bus_not_created( )
/*****************************************************************************\
*	Purpose: Returns key whether or not user completed the creation of bus
*	Author: Bill Rogers		Feb 12, 1993
*	Called by: toolbox.c (hopefully)
\*****************************************************************************/
{
   return ( bus_not_created_flg );  
}


void set_bus_type ( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs)
/*****************************************************************************\
*	Purpose: An interface for the bus TYPE cascase pd
*		
*	Author: Bill Rogers		May 21, 1992
*
*	Called by: Bus TYPE cascade menu.
*
\*****************************************************************************/
{
  char *pctag;
  char tmp_bus[512];

  pctag = (char *)tag;
  sub_type = pctag[0];

  build_add_rec( tmp_bus );	/* gather possible changed data from */
				/* text boxes */
  org_data[1] = sub_type;	/* insert new bus type */

  fill_bus_dialog2( tmp_bus );	/* redo the bus jacket with new sub_type */

}



void fill_cont_jacket ( Widget w, XtPointer tag,
			 XmAnyCallbackStruct *cbs )
/*****************************************************************************\
*	Purpose: (from MOTIF)
*		
*	Author: Bill Rogers	May, 1992	Modified: May 14, 1992
*
*	Called by: "+" bus pulldown
*
\*****************************************************************************/
{

/* for recovery later	*/
  strncpy ( org_data, (char *)tag, sizeof(org_data));

  set_jacket( CONT_INFO, '+', org_data[1] );
  Vuit_MU ( "U", "cont_type_warning_form" );	/* ensure it's off */

  XtSetSensitive( widget_id("cont_type_label"), FALSE );
  XtSetSensitive( widget_id("cont_type_optionmenu"), FALSE );
  XtSetSensitive( widget_id("cont_owner_text"), FALSE );
  XtSetSensitive( widget_id("cont_owner_label"), FALSE );
  XtSetSensitive( widget_id("code_year_label"), FALSE );
  XtVaSetValues( (widget_id("code_year_text" )), XmNeditable, FALSE, NULL );

  disable_pushbutton( "add_pushbutton"    );
  enable_pushbutton ( "delete_pushbutton" );
  enable_pushbutton ( "send_pushbutton"   );
	  enable_pushbutton ( "close_pushbutton"  );
	  enable_pushbutton ( "reset_pushbutton"  );


  record_to_text_boxes( org_data );

}



void create_cont_record ( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
/*****************************************************************************\
*	Purpose: Manages appropiate jackets, defines a blank cont record.
*		
*	Author: Bill Rogers	Dec 23, 1992	Modified: Jan 6, 1993
*
*	Called by: create Continuation Bus pulldown menu on Bus jacket.
*
\*****************************************************************************/
{

  strncpy( org_data, "+?    ", 6 );
  strcat_field( org_data, "bus_front_name" , 8, "a8" );
  strcat_field( org_data, "bus_front_volts", 4, "f4.0" );
  org_data[18] = 0;
  add_blanks_to_string( org_data, 80 );

  set_jacket( CONT_INFO, '+', '?' );

  cascade_pd_label_set( "cont_type_optionmenu", "+?" ); /* clear residual */

  Vuit_MU ( "M", "cont_type_warning_form" );

/*  Vuit_MU( "M", "cont_type_optionmenu" );*/
  XtSetSensitive( widget_id("cont_type_label"), TRUE );
  XtSetSensitive( widget_id("cont_type_optionmenu"), TRUE );
  XtSetSensitive( widget_id("cont_owner_text"), TRUE );
  XtSetSensitive( widget_id("cont_owner_label"), TRUE );
  XtSetSensitive( widget_id("code_year_label"), TRUE );
  XtVaSetValues( (widget_id("code_year_text" )), XmNeditable, TRUE, NULL );

  enable_pushbutton  ( "add_pushbutton"    );
  disable_pushbutton ( "delete_pushbutton" );
  disable_pushbutton ( "send_pushbutton"   );
	  enable_pushbutton ( "close_pushbutton"  );
	  enable_pushbutton ( "reset_pushbutton"  );


  record_to_text_boxes( org_data );
  set_title_mode( ONE_NAME_BASE );

}


void set_cont_type ( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs)
/*****************************************************************************\
*	Purpose:
*		
*	Author: Bill Rogers		Dec 08, 1992
*
*	Called by: Cont TYPE cascade menu.
*
\*****************************************************************************/
{
  char *pctag;

  pctag = (char *)tag;
  sub_type = pctag[0];

/*  printf("CONT bus type set to: (%c)\n", sub_type );*/
  Vuit_MU( "U", "cont_type_warning_form" );

}



void fill_reac_jacket		( Widget w, XtPointer tag,
				  XmAnyCallbackStruct *cbs )
/*****************************************************************************\
*	Purpose:
*		
*	Author: Bill Rogers	Jan, 1992	Modified:
*
*	Called by: "X" bus pulldown
*
\*****************************************************************************/
{



/* for recovery later	*/
  strncpy ( org_data, (char *)tag, sizeof(org_data));
  disable_pushbutton( "reset_pushbutton" );
	  disable_pushbutton( "add_pushbutton"    );/* gray-out this button */
	  enable_pushbutton ( "delete_pushbutton" );
	  enable_pushbutton ( "close_pushbutton"  );
	  enable_pushbutton ( "send_pushbutton"  );


  set_jacket( REAC_INFO, 'X', ' ' );

  record_to_text_boxes( org_data );

}



void create_reac_rec( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs)
/****************************************************************************\
*	Purpose:
*
*	Author: Bill Rogers
*		Jan 11, 1993		Modified:
*
*	Called by: create bus comp pd REAC pb
*
\*****************************************************************************/
{

  strncpy( org_data, "X     ", 6 );
  strcat_field( org_data, "bus_front_name" , 8, "a8" );
  strcat_field( org_data, "bus_front_volts", 4, "f4.0" );
  org_data[18] = 0;
  add_blanks_to_string( org_data, 80 );

  set_jacket( REAC_INFO, 'X', ' ' );

  enable_pushbutton ( "add_pushbutton"    );
  enable_pushbutton ( "close_pushbutton"  );	/* ensure its active */
  disable_pushbutton( "delete_pushbutton" );
  disable_pushbutton( "send_pushbutton"   );
	  enable_pushbutton ( "reset_pushbutton"  );


}


int fill_branch_selection_box ( Widget w, XtPointer tag,
				 XmAnyCallbackStruct *cbs)
/*****************************************************************************\
*	Purpose:
*		
*	Author: Dan Clark/Bill Rogers	Dec, 1991	Modified: Feb 19, 1992
*
*	Called by: "Branch" push button callback (edit_menu_branch_button)
*
\*****************************************************************************/
{

  Widget		id, list_id;
  int i=0;
  XmString branch_record[10000];

  PFBASERECORD keyvalrecord;	/* storage for db_search key values */
				/* used for any search */
  PFBASERECORD *nextrec;	/* ptr assigned to address of next rec */
				/* used by db_search & db_next */
  PFBASERECORD *rec;		/* ptr to key for branch search */

  DB_STAT stat;			/* status returned for general db access */

  /** if no base table exists just return (no data ) for list **/
  if(db_valid_db( &base_db ) != D_OKAY) {
    branch_box_fill_flag = FALSE;
    return(0);
  }

  if ( branch_box_fill_flag == FALSE )
  {

    id = widget_id( "branch_selection_dialog"  );

    list_id = (Widget)XmSelectionBoxGetChild( id, XmDIALOG_LIST);
 
    XtSetSensitive ( (Widget)XmSelectionBoxGetChild( id, XmDIALOG_HELP_BUTTON),
                      False );

    XtSetSensitive ( XmSelectionBoxGetChild( id, XmDIALOG_APPLY_BUTTON),
		     False );

    XmListDeleteAllItems( list_id );

    /** retrieve all the records (in key order) and write to output file **/
    stat = db_first(&base_db, &nextrec, BSE_TYPE);
    if (stat != D_FOUND)
    {
      printf("ERROR db_search did not find any records \n");
      return(1);
    }

    rec = &keyvalrecord;
    memset(&keyvalrecord, ' ', sizeof(keyvalrecord));
    rec->branch.eol[0] = '\0';
    stat = db_search( &base_db, &rec, &nextrec, BSE_BRANCH);

    if ( stat != D_FOUND )
    {
       if(stat == D_EOF) {
         printf("Warning: move warning to error box End of table reached\n");
       } else {
         stat = D_FOUND;
       }
    }
   	
    while(stat == D_FOUND)
    {
      if( nextrec->type[0] == 'L' || nextrec->type[0] == 'T' ||
	  nextrec->type[0] == 'E' || nextrec->type[0] == 'R' ) {
        /* only put line records on line selectoin screen */

        branch_record[i++] = XmStringCreate( (char *)nextrec,
					 XmSTRING_DEFAULT_CHARSET);

      }

      stat=db_next( &base_db, &nextrec, &nextrec, BSE_BRANCH);

    }

    XtVaSetValues( id, XmNlistItems, branch_record, XmNlistItemCount, i, NULL);

    printf("%d items added into BRANCH selection box\n", i );
    branch_box_fill_flag = TRUE;

  }

  return(0);

}




void fill_branch_jacket_cb_sb ( Widget w, XtPointer client_data,
				 XmSelectionBoxCallbackStruct *cbs )
/*****************************************************************************\
*	Purpose: Handles any motif data which passes back line or transf
*		 data from selection box.
*		
*	Author: Bill Rogers		Modified: May 14, 1992
*
*	Called by: 
*
\*****************************************************************************/
{
  char value_str[133];
  char *value = value_str;
     
  XmStringGetLtoR( cbs->value, XmSTRING_DEFAULT_CHARSET,
		   &value );

  if( (int)strlen( value ) > 0 )/* prevents bomb if nothing selected */
  {
    Vuit_MU( "M", "bus_front_box"   );

    enable_pushbutton ( "add_pushbutton"    );
    enable_pushbutton ( "delete_pushbutton" );
    enable_pushbutton ( "close_pushbutton"  );	/* ensure its active */
	  disable_pushbutton ( "send_pushbutton"  );
	  enable_pushbutton ( "reset_pushbutton"  );


    set_title_mode( LINENAMEONLY );
    fill_branch_jckt( value );
  }

}




void fill_branch_jacket_cb_pd ( Widget w, XtPointer tag,
				 XmAnyCallbackStruct *cbs )
/*****************************************************************************\
*	Purpose: Handles any motif data which passes back line or transf
*		 data from bus pulldown display.
*		
*	Author: Bill Rogers		Modified: May 14, 1992
*
*	Called by: 
*
\*****************************************************************************/
{

   disable_pushbutton( "add_pushbutton"    );
   enable_pushbutton ( "delete_pushbutton" );
   enable_pushbutton ( "send_pushbutton"   );
   enable_pushbutton ( "close_pushbutton"  );	/* ensure its active */
	  enable_pushbutton ( "reset_pushbutton"  );

   strncpy( org_data, (char *)tag, 119 );
   org_data[119] = 0;

   if( strlen( org_data ) == 0 )
      printf("*** ERROR *** fill_branch_jacket_cb_pd has no data!\n");

   set_title_mode( BUS_PULLDOWN );
   fill_branch_jckt( org_data );

}


void set_title_mode ( int new_mode )
/*****************************************************************************\
*	Purpose: Sets new title mode and manages correct text boxes.
*		
*	Author: Bill Rogers	Feb 2, 1993	Modified:
*
*	Called by:
*
\*****************************************************************************/
{
  title_mode = new_mode;

  switch ( title_mode )
  {
  case BUS_PULLDOWN:
    Vuit_MU( "U", "bus_front_line_name" );
    Vuit_MU( "U", "line_type_pd"        );
    Vuit_MU( "M", "bus_front_name"      );
    Vuit_MU( "M", "bus_front_volts"     );
    Vuit_MU( "U", "bus_front_name_2"    );
    Vuit_MU( "U", "bus_front_volts_2"   );
    Vuit_MU( "M", "bus_item_pulldown"   );
  break;

  case LINENAMEONLY:
    Vuit_MU( "M", "bus_front_line_name" );
    Vuit_MU( "M", "line_type_pd"        );
    Vuit_MU( "U", "bus_front_name"      );
    Vuit_MU( "U", "bus_front_volts"     );
    Vuit_MU( "U", "bus_front_name_2"    );
    Vuit_MU( "U", "bus_front_volts_2"   );
    Vuit_MU( "U", "bus_item_pulldown"   );
  break;

  case TWO_NAME_BASE:
    Vuit_MU( "U", "bus_front_line_name" );
    Vuit_MU( "U", "line_type_pd"        );
    Vuit_MU( "M", "bus_front_name"      );
    Vuit_MU( "M", "bus_front_volts"     );
    Vuit_MU( "M", "bus_front_name_2"    );
    Vuit_MU( "M", "bus_front_volts_2"   );
    Vuit_MU( "U", "bus_item_pulldown"   );
  break;

  case ONE_NAME_BASE:
    Vuit_MU( "U", "bus_front_line_name" );
    Vuit_MU( "U", "line_type_pd"        );
    Vuit_MU( "M", "bus_front_name"      );
    Vuit_MU( "M", "bus_front_volts"     );
    Vuit_MU( "U", "bus_front_name_2"    );
    Vuit_MU( "U", "bus_front_volts_2"   );
    Vuit_MU( "U", "bus_item_pulldown"   );
  break;
  }
}


void set_line_type ( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs)
/*****************************************************************************\
*	Purpose: An interface for the line TYPE cascase pd
*		
*	Author: Bill Rogers		Oct 05, 1993
*
*	Called by: Line TYPE cascade menu. (When line is graphically created)
*
\*****************************************************************************/
{
  char *pctag;
  char tmp_bus[133];

  pctag = (char *)tag;
  sub_type = pctag[0];

  printf("set_line_type routine called\n" );
/*  build_add_rec( tmp_bus );*/	/* gather possible changed data from */
				/* text boxes */
  org_data[1] = sub_type;	/* insert new bus type */
 
/*  fill_bus_dialog2( tmp_bus );*//* redo the bus jacket with new sub_type */

}


void fill_branch_jckt ( char *branch_data )
/*****************************************************************************\
*	Purpose: Routes to next routine depend if branch is a
*		 transmission line or transformer.
*		
*	Author: Bill Rogers	Dec 2, 1992	Modified:
*
*	Called by: fill_branch_jacket_cb_sb
*		   create_line_rec_2bus
*		   create_line_rec_bus
*		   create_xfmr_rec
*		   create_xfmr_shift_rec
*		   create_equiv_rec
*		   create_regxfmr_rec
*		   create_dc_2_term
*		   create_dc_multi_term_rec
*
\*****************************************************************************/
{

  set_jacket( UNKNOWN_INFO, branch_data[0], branch_data[1] );

  lookup_and_fill_field( "bus_front_line_name", branch_data, 6, 31 );
  enable_pushbutton ( "bus_jacket_new_option_menu" );
  enable_pushbutton ( "bus_item_pulldown" );

  setCurLine( branch_data );	/* used in line tapping */

  switch ( branch_data[0] )
  {
  case 'L':
    switch ( sub_type )
    {
     case ' ':
       /* take no action */
     break;

     case 'D':
       Vuit_MU ( "M", "dc_line_LD_form" );
       Vuit_MU ( "U", "dc_line_LM_form" );
       Vuit_MU ( "M", "dc_type_D_label" );
       Vuit_MU ( "U", "dc_type_M_label" );
     break;

     case 'M':
       Vuit_MU ( "U", "dc_line_LD_form" );
       Vuit_MU ( "M", "dc_line_LM_form" );
       Vuit_MU ( "U", "dc_type_D_label" );
       Vuit_MU ( "M", "dc_type_M_label" );
     break;

    }
    record_to_text_boxes( branch_data );

    send_type = BRANCH_INFO;	/* sets flag so when "SEND" button is pushed
				/* routine will build a BRANCH card           */
  break;

  case 'T':
    switch ( sub_type )
    {
    case ' ':
      Vuit_MU( "U", "xfmr_dialog_phase_text"    );
      Vuit_MU( "U", "xfmr_dialog_phase_label"   );
      Vuit_MU( "M", "xfmr_dialog_tap1_kv_text"  );
      Vuit_MU( "M", "xfmr_dialog_tap1_label" );
/*    Vuit_MU( "M", "xfmr_dialog_tap2_kv_text"  );	Walt changed his mind!
      Vuit_MU( "M", "xfmr_dialog_tap2_label" );*/
    break;

    case 'P':
      Vuit_MU( "M", "xfmr_dialog_phase_text"    );
      Vuit_MU( "M", "xfmr_dialog_phase_label"   );
      Vuit_MU( "U", "xfmr_dialog_tap1_kv_text"  );
      Vuit_MU( "U", "xfmr_dialog_tap1_label" );
/*    Vuit_MU( "U", "xfmr_dialog_tap2_kv_text"  );
      Vuit_MU( "U", "xfmr_dialog_tap2_label" );*/
    break;
    }
    record_to_text_boxes( branch_data );
    send_type = TRANSF_INFO;	/* sets flag so when "SEND" button is pushed
				/* routine will build a Transformer record   */
  break;

  case 'E':
    record_to_text_boxes( branch_data );
    send_type = EQUIV_INFO;
  break;

  case 'R':
    fill_regxfmr_jckt ( branch_data );
    record_to_text_boxes( branch_data );
    send_type = REGXFMR_INFO;
  break;

  default:
    printf("WARNING - from fill_branch_jckt\n" );
    printf(" Invalid Branch type (%s) in column 2 \n", branch_data );
  }

}





void fill_regxfmr_jckt ( char *line_data )
/*****************************************************************************\
*	Purpose: Cuts up "R" record and fills appropiate boxes of
*		 the bus_front line jacket.
*		
*	Author: Bill Rogers	Jan 12, 1993	Modified:
*
*	Called by: fill_branch_jckt (above)
*
\*****************************************************************************/
{
  Widget pb_1_id, pb_2_id, pb_3_id;

/* for recovery later	*/
  strncpy ( org_data, (char *)line_data, sizeof(line_data));

  var_tap = org_data[18];
  pb_1_id = widget_id( "regxfmr_fixed_rb" );
  pb_2_id = widget_id( "regxfmr_bus1_tap_rb" );
  pb_3_id = widget_id( "regxfmr_bus2_tap_rb" );

  switch (var_tap)
  {
    case '0':
      XtVaSetValues( pb_1_id, XmNset, 1, NULL ); /* set "fixed" radio botton */
      XtVaSetValues( pb_2_id, XmNset, 0, NULL );
      XtVaSetValues( pb_3_id, XmNset, 0, NULL );
    break;

    case '1':
      XtVaSetValues( pb_1_id, XmNset, 0, NULL );
      XtVaSetValues( pb_2_id, XmNset, 1, NULL ); /* set bus 1 variable rb */
      XtVaSetValues( pb_3_id, XmNset, 0, NULL );
    break;

    case '2':
      XtVaSetValues( pb_1_id, XmNset, 0, NULL );
      XtVaSetValues( pb_2_id, XmNset, 0, NULL );
      XtVaSetValues( pb_3_id, XmNset, 1, NULL ); /* set bus 2 variable rb */
    break;


  }

  set_regxfmr_jckts( );
}



void set_regxfmr_jckts_cb ( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs)
/***************************************************************************\
*
* Called by: releasing reg_xfmr cascade pulldown button
\***************************************************************************/
{
  char *pctag;

  pctag = (char *)tag;
  sub_type = pctag[0];
  org_data[1] = sub_type;
  set_regxfmr_jckts( );
}


void set_regxfmr_jckts( )
/***************************************************************************\
* Purpose: manages all the correct texts according to regulating
*	   transformer type
*
* Author: Bill Rogers	June 22, 93
* Called by: fill_regxfmr_jckt, set_regxfmr_jckts_cb
\***************************************************************************/
{
  char regxfmr_type[] = "R ";

  regxfmr_type[1] = sub_type;
  cascade_pd_label_set( "regxfmr_option_menu", regxfmr_type );

  switch( sub_type )	/* two basic types - bVQN and PM  */
  {
  case ' ':
  case 'V':
  case 'Q':
  case 'N':
    Vuit_MU( "M", "regxfmr_VQN_form"       );
    Vuit_MU( "U", "regxfmr_PM_form"        );

    Vuit_MU( "U", "regxfmr_minMVAR_text"   );
    Vuit_MU( "U", "regxfmr_maxMVAR_text"   );
    Vuit_MU( "U", "regxfmr_minMVAR_label"  );
    Vuit_MU( "U", "regxfmr_maxMVAR_label"  );
    Vuit_MU( "U", "regxfmr_schedMVAR_text" );
    Vuit_MU( "U", "regxfmr_schedMVAR_label");

/*    Vuit_MU( "U", "regxfmr_in_month_text" );
*    Vuit_MU( "U", "regxfmr_in_year_text"  );
*    Vuit_MU( "U", "regxfmr_out_month_text");
*    Vuit_MU( "U", "regxfmr_out_year_text" );
*    Vuit_MU( "U", "regxfmr_datein_label"  );
*    Vuit_MU( "U", "regxfmr_dateout_label" );*/

    disable_pushbutton( "regxfmr_RM_pb"  );
    disable_pushbutton( "regxfmr_RP_pb"  );
    enable_pushbutton ( "regxfmr_R_pb"   );
    enable_pushbutton ( "regxfmr_RN_pb"  );
    enable_pushbutton ( "regxfmr_RQ_pb"  );
    enable_pushbutton ( "regxfmr_RV_pb"  );
  break;

  case 'P':
  case 'M':
    Vuit_MU( "M", "regxfmr_PM_form"        );
    Vuit_MU( "U", "regxfmr_VQN_form"       );

    Vuit_MU( "U", "regxfmr_schedflow_text" );
    Vuit_MU( "U", "regxfmr_schedflow_label");
    Vuit_MU( "U", "regxfmr_minflow_text"  );
    Vuit_MU( "U", "regxfmr_maxflow_text"  );
    Vuit_MU( "U", "regxfmr_minflow_label" );
    Vuit_MU( "U", "regxfmr_maxflow_label" );

/*    Vuit_MU( "M", "regxfmr_in_month_text" );
*    Vuit_MU( "M", "regxfmr_in_year_text"  );
*    Vuit_MU( "M", "regxfmr_out_month_text");
*    Vuit_MU( "M", "regxfmr_out_year_text" );
*    Vuit_MU( "M", "regxfmr_datein_label"  );
*    Vuit_MU( "M", "regxfmr_dateout_label" );*/

    enable_pushbutton ( "regxfmr_RM_pb"  );
    enable_pushbutton ( "regxfmr_RP_pb"  );
    disable_pushbutton( "regxfmr_R_pb"   );
    disable_pushbutton( "regxfmr_RN_pb"  );
    disable_pushbutton( "regxfmr_RQ_pb"  );
    disable_pushbutton( "regxfmr_RV_pb"  );
  break;

  default:
    printf("*** WARNING **** from set_regxfmr_jckt \n");
    printf("            Invalid regulating xfmr subtype (%c)\n", &sub_type);
  }

  if( any_regxfmr_type )
  {
    enable_pushbutton ( "regxfmr_RM_pb" );
    enable_pushbutton ( "regxfmr_RP_pb" );
    enable_pushbutton ( "regxfmr_R_pb"  );
    enable_pushbutton ( "regxfmr_RN_pb" );
    enable_pushbutton ( "regxfmr_RQ_pb" );
    enable_pushbutton ( "regxfmr_RV_pb" );
  }

  switch( sub_type )
  {
  case 'Q':
    Vuit_MU( "M", "regxfmr_schedMVAR_text" );
    Vuit_MU( "M", "regxfmr_schedMVAR_label");
  break;

  case 'N':
    Vuit_MU( "M", "regxfmr_minMVAR_text"   );
    Vuit_MU( "M", "regxfmr_maxMVAR_text"   );
    Vuit_MU( "M", "regxfmr_minMVAR_label"  );
  break;

  case 'P':
    Vuit_MU( "M", "regxfmr_schedflow_text" );
    Vuit_MU( "M", "regxfmr_schedflow_label");
  break;

  case 'M':
    Vuit_MU( "M", "regxfmr_minflow_text"  );
    Vuit_MU( "M", "regxfmr_maxflow_text"  );
    Vuit_MU( "M", "regxfmr_minflow_label" );
    Vuit_MU( "M", "regxfmr_maxflow_label" );
  break;

  }
}



void process_regxfmr_rb(Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
/***************************************************************************\
*	Purpose:
*
*	Author: Bill Rogers
*		Jan 13, 1993		Modified:
*
*
\*****************************************************************************/
{
  char *pctag;
  pctag = (char *)tag;
  if ( pctag[0] == '0' ) var_tap = '0';
  if ( pctag[0] == '1' ) var_tap = '1';
  if ( pctag[0] == '2' ) var_tap = '2';

  printf("var_tap set to:%c\n", var_tap );
}


/**************************************************************************\
* Purpose: to finish a graphically created line.
*
* Author: Bill Rogers		Feb 25, 1993
*
* Called by: (toolbox.c)
\**************************************************************************/
void create_line_rec_2bus( char *new_br )
{
  char new_data[120];

  memset(new_data, ' ', sizeof(org_data));
  strncpy(&new_data[0], &new_br[0], 1);
  strncpy(&new_data[6], &new_br[2], 12);
  strncpy(&new_data[19],&new_br[14], 12);
  new_data[119] = 0;
#ifdef OLDWAY
  strncpy( new_data, "L     ", 6 );
  new_data[6] = 0;
  strncat( new_data, &new_br[2], 12 );
  strncat( new_data, " ", 1 );
  strncat( new_data, &new_br[14], 12 );
  strcat(new_data,"                                                                  " );

  if(  strncmp( new_br+14, new_br+27, 4)) new_data[0] = 'T';

#endif
  org_data[0] = 0;	/* clues send_add_data_ not to pop back to org bus */

  strcpy( org_data, new_data );

  Vuit_MU ( "M", "bus_front_box" );	/* Manage the main box 		     */
  enable_pushbutton  ( "add_pushbutton"    );
  disable_pushbutton ( "delete_pushbutton" );
  disable_pushbutton ( "send_pushbutton"   );
  disable_pushbutton ( "close_pushbutton"  );
	  enable_pushbutton ( "reset_pushbutton"  );

  memset ( org_bus, ' ', sizeof(org_data)); /* these 3 lines of code to */
  strncpy( org_bus, "B     ", 6 );
  strncat( org_bus, &new_br[14], 12 );

  set_title_mode( LINENAMEONLY );
  fill_branch_jckt( new_data );

  disable_pushbutton ( "bus_jacket_new_option_menu" );
  disable_pushbutton ( "front_dia_tap_line_pb" );
}


/***************************************************************************\
*	Purpose: Create various types of components.
*
*	Author: Bill Rogers
*		Feb 01, 1993		Modified:
*
*	Called by: Any of 7 pushbuttons on "create new component" pulldown.
\*****************************************************************************/
void create_line_rec(Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
{
  create_bus_component( 'L', ' ', BRANCH_INFO );
  fill_branch_jckt( org_data );
  disable_pushbutton ( "bus_jacket_new_option_menu" );
  disable_pushbutton ( "front_dia_tap_line_pb" );
  disable_pushbutton ( "bus_item_pulldown" );
}

void create_xfmr_rec(Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
{
  create_bus_component( 'T', ' ', TRANSF_INFO );
  fill_branch_jckt( org_data );
  disable_pushbutton ( "bus_jacket_new_option_menu" );
  disable_pushbutton ( "front_dia_tap_line_pb" );
  disable_pushbutton ( "bus_item_pulldown" );
}

void create_xfmr_shift_rec(Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
{
  create_bus_component( 'T', 'P', TRANSF_INFO );
  fill_branch_jckt( org_data );
  disable_pushbutton ( "bus_jacket_new_option_menu" );
  disable_pushbutton ( "front_dia_tap_line_pb" );
  disable_pushbutton ( "bus_item_pulldown" );
}

void create_equiv_rec(Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
{
  create_bus_component( 'E', ' ', EQUIV_INFO );
  fill_branch_jckt( org_data );
  disable_pushbutton ( "bus_jacket_new_option_menu" );
  disable_pushbutton ( "front_dia_tap_line_pb" );
  disable_pushbutton ( "bus_item_pulldown" );
}

void create_regxfmr_rec(Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
{
  any_regxfmr_type = True;
  create_bus_component( 'R', ' ', REGXFMR_INFO );
  fill_branch_jckt( org_data );
  disable_pushbutton ( "bus_jacket_new_option_menu" );
  disable_pushbutton ( "front_dia_tap_line_pb" );
  disable_pushbutton ( "bus_item_pulldown" );
}

void create_dc_2_term_rec(Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
{
  create_bus_component( 'L', 'M', BRANCH_INFO );
  fill_branch_jckt( org_data );
  disable_pushbutton ( "bus_jacket_new_option_menu" );
  disable_pushbutton ( "front_dia_tap_line_pb" );
  disable_pushbutton ( "bus_item_pulldown" );
}

void create_dc_multi_term_rec(Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
{
  create_bus_component( 'L', 'D', BRANCH_INFO );
  fill_branch_jckt( org_data );
  disable_pushbutton ( "bus_jacket_new_option_menu" );
  disable_pushbutton ( "front_dia_tap_line_pb" );
  disable_pushbutton ( "bus_item_pulldown" );
}


void set_title_mode_2namebase( Widget w, XtPointer tag,
                                         XmAnyCallbackStruct *cbs)
/**************************************************************************\
*	Purpose: for outside callers to set title_mode.
*	Author:  Bill Rogers	10/12/93
\**************************************************************************/
{
  set_title_mode(TWO_NAME_BASE );

  lookup_and_reduce_text_field( "bus_front_name_2",  0 );
  lookup_and_reduce_text_field( "bus_front_volts_2", 0 );

}




void set_title_mode_linename	( Widget w, XtPointer tag,
                                         XmAnyCallbackStruct *cbs)
/**************************************************************************\
*	Purpose: for outside callers to set title_mode.
*	Author:  Bill Rogers	10/12/93
\**************************************************************************/
{
  set_title_mode(LINENAMEONLY );
}


void create_bus_component( char type, char stype, int info_type )
/***************************************************************************\
*	Purpose: Set org_bus value correctly.
*		 Set push buttons correctly.
*		 Set send_type.
*
*	Author: Bill Rogers
*		Feb 01, 1993		Modified:	Oct 12, 1993
*
*	Called by: above 8 routines.
\*****************************************************************************/
{
  org_data[0] = type;
  org_data[1] = stype;
  strncat( org_data, "    ", 4 );

  switch ( type )
  {
    case 'B':
      strcat_field( org_data, "bus_front_name" , 8, "a8" );
      strcat_field( org_data, "bus_front_volts", 4, "f4.0" );
      org_data[18] = 0;
    break;

    case 'L':
    case 'T':
    case 'R':
    case 'E':
      switch ( title_mode )
      {
        case LINENAMEONLY:
	  strcat_field( org_data, "bus_front_line_name" , 14, "a14" );
	  org_data[33] = 0;
        break;

        case TWO_NAME_BASE:
	  strcat_field( org_data, "bus_front_name"   , 8, "a8"   );
	  strcat_field( org_data, "bus_front_volts"  , 4, "f4.0" );
	  strcat_field( org_data, "bus_front_name_2" , 8, "a8"   );
	  strcat_field( org_data, "bus_front_volts_2", 4, "f4.0" );
	  org_data[33] = 0;
	break;
      }
    break;

    default:
      printf("*** INFO *** from create_bus_component\n" );
      printf("       Possible incorrect org_data data\n");
      printf("       For component type (%c)\n", type );

  }


  add_blanks_to_string( org_data, 80 );

  send_type = info_type;

  enable_pushbutton  ( "add_pushbutton"    );
  disable_pushbutton ( "delete_pushbutton" );
  disable_pushbutton ( "send_pushbutton"   );
  enable_pushbutton  ( "close_pushbutton"  );
  enable_pushbutton  ( "reset_pushbutton"  );


}


void fill_area_selection_box2( Widget w, XtPointer tag,XmAnyCallbackStruct *cbs)
/*****************************************************************************\
* Purpose: (Superceeds "fill_area_selection_box")
*	Calls loadArea2, setting value area_box_fill_flag
*
*
\*****************************************************************************/
{

  area_box_fill_flag = loadArea2 ();

}



void get_area_selection( Widget w, XtPointer client_data,
				 XmSelectionBoxCallbackStruct *cbs )
/*****************************************************************************\
*	Purpose: 
*
*		  After bus_selection popup dialog is shown, this routine
*		  returns the highlight user selection.
*
*	Author: B.Rogers (from example pp 170, Motif Programming Manual
*		an 29, 1991		Modified:
*
\*****************************************************************************/
{
  Widget id;
  char msg[512], *prompt, *value, ret_string[80];
  int dialog_type;
  int len;
    
    switch ( cbs->reason )
    {
	case XmCR_OK:
 	    prompt = "Selection:\n";
	    dialog_type = XmDIALOG_MESSAGE;
	    break;
	case XmCR_NO_MATCH:
	    prompt = "Not a valid selection:\n";
	    dialog_type = XmDIALOG_ERROR;
	    break;
	default:
	    prompt = "Unknown selection:\n";
	    dialog_type = XmDIALOG_ERROR;
    }

    XmStringGetLtoR( cbs->value, XmSTRING_DEFAULT_CHARSET, &value );
    sprintf(msg, "%s%s", prompt, value );
    /* printf( "%s%s\n", prompt, value ); */

	/* for mod card and reset pb */
    memset(org_area, '\0', sizeof(org_area));
    len = min(strlen(value) + 1, sizeof(org_area) - 1);
    strncpy ( org_area, value, len );
    org_area[len] = '\0';

    disable_pushbutton("area_add_pushbutton");
    enable_pushbutton ("area_send_pushbutton");
    enable_pushbutton ("area_delete_pushbutton");

    fill_area_interchange( org_area );

    /* XtFree( value ); */
 }


void fill_area_interchange( char *ai_string )
/*****************************************************************************\
*	Purpose: 
*
*	Author: B.Rogers
*		Jul 21, 1991		Modified:	Jul 21, 1992
*
*	Called by: "edit" pushbutton on area/interchange list box
*
\*****************************************************************************/
{
  rec_type = ai_string[0];
  sub_type = ai_string[1];

  strncpy( org_data, ai_string, 119 );
  org_data[119] = 0;

  set_area_card_mode( False, "M" );  /* So build_add_rec card will be altered */

  switch( rec_type )	/* by card type */
  {
    case 'A':
      send_type = AREA_INFO;	/* set flag so when "SEND" button is pushed,
				   routine will build an AC (or A) card	   */
      Vuit_MU( "M", "area_control_jacket"  );
      Vuit_MU( "U", "area_intertie_jacket" );
    break;

    case 'I':
      send_type = INTERTIE_INFO;	/* flag to make "SEND button build
					/* an "I" card			  */
      Vuit_MU( "U", "area_control_jacket"  );
      Vuit_MU( "M", "area_intertie_jacket" );
    break;

    default:
      printf("*** WARNING *** from fill_area_interchange routine.\n" );
      printf("                Param is not an 'A' or 'I' card.\n" );
      printf("     You must select a valid AREA or INTERTIE record before editing.\n" );
      return;
  }

  record_to_text_boxes( ai_string );
}


void set_area_cont_edit( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
/*****************************************************************************\
*	Purpose: 
*
*	Author: B.Rogers 	Jan 10, 1994	
*
*	Called by: Create Area Continuation Card pushbutton
*
\*****************************************************************************/
{

  disable_pushbutton	( "area_send_pushbutton" );
  disable_pushbutton	( "area_delete_pushbutton" );
  disable_pushbutton	( "area_reset_pushbutton" );
  enable_pushbutton	( "area_add_pushbutton");

  set_area_card_mode( True, "U" );   /* So build_add_rec card will be altered */

  lookup_and_fill_field( "area_zone_1_text"          , "  "      , 0, 1 );
  lookup_and_fill_field( "area_zone_2_text"          , "  "      , 0, 1 );
  lookup_and_fill_field( "area_zone_3_text"          , "  "      , 0, 1 );
  lookup_and_fill_field( "area_zone_4_text"          , "  "      , 0, 1 );
  lookup_and_fill_field( "area_zone_5_text"          , "  "      , 0, 1 );
  lookup_and_fill_field( "area_zone_6_text"          , "  "      , 0, 1 );
  lookup_and_fill_field( "area_zone_7_text"          , "  "      , 0, 1 );
  lookup_and_fill_field( "area_zone_8_text"          , "  "      , 0, 1 );
  lookup_and_fill_field( "area_zone_9_text"          , "  "      , 0, 1 );
  lookup_and_fill_field( "area_zone_10_text"         , "  "      , 0, 1 );

}



void set_area_card_mode( Boolean flg, char  *m )
/*****************************************************************************\
*	Purpose: manage/unmanaged group of widgets.
*		
*	Author: B Rogers	Jan 12, 1994
*
*	Called by: set_area_cont_edit, fill_area_interchange
*
\*****************************************************************************/
{

  area_cont_card = flg;	/* So build_add_rec card will be altered */

  Vuit_MU( "M", "area_slack_bus_name_text"  );
  Vuit_MU( "M", "area_base_kv_text"         );
  Vuit_MU( "M", "area_export_label"         );
  Vuit_MU( "M", "area_scheduled_export_text");
  Vuit_MU( "M", "area_max_volt_label"       );
  Vuit_MU( "M", "area_max_volt_text"        );
  Vuit_MU( "M", "area_min_volt_label"       );
  Vuit_MU( "M", "area_min_volt_text"        );

  Vuit_MU( "M", "create_area_cont_pb"       );

}




int area_control_cb( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs)
/*****************************************************************************\
*	Purpose: Add an area control or intertie card.
*		
*	Author: B Rogers	Modified: Dec 01, 1993
*
*	Called by: area control Add pushbutton
*
\*****************************************************************************/
{

  enable_pushbutton ("area_add_pushbutton");
  disable_pushbutton("area_send_pushbutton");
  disable_pushbutton("area_delete_pushbutton");

  fill_area_interchange( "AC                                                                                                                      " );
  return(0);
}


int intertie_cb( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs)
{
  enable_pushbutton ("area_add_pushbutton");
  disable_pushbutton("area_send_pushbutton");
  disable_pushbutton("area_delete_pushbutton");

  fill_area_interchange( "I                                                                                                                       " );
  return(0);
}


void fill_pq_jacket ( Widget w, XtPointer tag,
				 XmAnyCallbackStruct *cbs)
/****************************************************************************\
*
*
*	Called by: PQ curve Bus cascade pulldown.
\****************************************************************************/
{

  set_jacket( PQ_CURVE_INFO, 'Q', ' ' );

  load_pq_list_widget_id();

  stringPart(org_data, bus_namevolt, 6, 17);
  read_pq_from_db( bus_namevolt );



/*  loadBusBranch(0, bus_string, 0);	 /*** dlc create a new bus_br_db **/
/*  need code here to pull out all the "Q" cards associated with this bus */
/** hey bill - all the cards are in the bus_br_db data table,
copy the code you have in the routine to loop the bus_br_db and only
check for the Q cards - start the search with type = Q and name, kv set
to blanks - then loop untill no more Q cards
if you want the name & kv to search for they are in the callback_data
structure. but you really do not need them to do the search
***/

}


void set_jacket( int stype, char type, char subtype )
/*****************************************************************************\
*	Purpose: manages the correct bus_front jacket 
*
*	Author: Bill Rogers	Dec 8, 1992	Modified: May 20, 1993
*
*	Called by: 8 callers known as of 5/20/93
*
\*****************************************************************************/
{
  Widget myid;
  Widget pid;
  send_type = stype;
  rec_type  = type;
  sub_type  = subtype;

  /* Unmanage everything */
  manage2parent( "U", "bus_jacket"     , "bus_front_box");	
  manage2parent( "U", "cont_jacket"    , "bus_front_box");
  manage2parent( "U", "branch_jacket"  , "bus_front_box");
  manage2parent( "U", "xfmr_jacket"    , "bus_front_box");
  manage2parent( "U", "dc_bus_jacket"  , "bus_front_box");
  manage2parent( "U", "dc_line_jacket" , "bus_front_box");
  manage2parent( "U", "pq_curve_jacket", "bus_front_box");
  manage2parent( "U", "reg_xfmr_jacket", "bus_front_box"); 
  manage2parent( "U", "equiv_jacket"   , "bus_front_box"); 
  manage2parent( "U", "pq_curve_jacket", "bus_front_box");
  manage2parent( "U", "swr_reac_jacket", "bus_front_box");

  switch ( rec_type )
  {
  case 'B':
    disable_pushbutton("bus_jacket_dc_mutli_term_pb");
    disable_pushbutton("bus_jacket_dc_2_term_pb"    );
    enable_pushbutton ("bus_jacket_cont_bus_pb"     );
    switch ( sub_type )
    {
    case 'X':     
      manage2parent( "M", "bus_jacket"   , "bus_front_box");	/* manage the bus jacket */
      disable_pushbutton("bus_jacket_dc_2_term_pb" );
    break;

    case ' ':     
    case 'C':     
    case 'T':     
    case 'V':     
      disable_pushbutton( "bus_jacket_swr_reac_pb" );
      disable_pushbutton( "bus_jacket_pq_curve_pb" );
      manage2parent( "M", "bus_jacket"  ,"bus_front_box" );	/* manage the bus jacket */
    break;

    case 'E':     
    case 'F':     
    case 'G':     
    case 'Q':     
    case 'S':     
      disable_pushbutton( "bus_jacket_swr_reac_pb" );
      manage2parent( "M", "bus_jacket" , "bus_front_box"  );	/* manage the bus jacket */
    break;

    case 'M':     
      manage2parent( "M", "dc_bus_jacket", "bus_front_box");
      Vuit_MU( "M", "bus_BM_label");
      Vuit_MU( "U", "bus_BD_label");
      manage2parent( "M", "dc_bus_BM_form", "bus_front_box");
      enable_pushbutton ( "bus_jacket_dc_mutli_term_pb");
      disable_pushbutton( "bus_jacket_cont_bus_pb" );
      disable_pushbutton( "bus_jacket_swr_reac_pb" );
      disable_pushbutton( "bus_jacket_pq_curve_pb" );
    break;

    case 'D':     
      manage2parent( "M", "dc_bus_jacket", "bus_front_box");
      Vuit_MU( "U", "bus_BM_label" );
      Vuit_MU( "M", "bus_BD_label" );
      manage2parent( "U", "dc_bus_BM_form", "bus_front_box");
      enable_pushbutton ( "bus_jacket_dc_2_term_pb");
      disable_pushbutton( "bus_jacket_cont_bus_pb" );
      disable_pushbutton( "bus_jacket_swr_reac_pb" );
      disable_pushbutton( "bus_jacket_pq_curve_pb" );
    break;

    default:
      printf("*** PROGRAM ERROR *** from set_jacket\n" );
      printf("   bus sub_type(%c) is not a valid value.\n", sub_type );
    }
  break;

  case '+':
       manage2parent( "M", "cont_jacket", "bus_front_box"  );     /* manage the +bus jacket*/
  break;

  case 'L':
    switch ( sub_type )
    {
    case ' ':
  	manage2parent( "M", "branch_jacket", "bus_front_box");
        enable_pushbutton ( "front_dia_tap_line_pb" );
    break;


    case 'M':
    case 'D':
  	manage2parent( "M", "dc_line_jacket", "bus_front_box");
    break;

    default:
      printf("*** PROGRAM ERROR *** from set_jacket\n" );
      printf("   line sub_type(%c) is not a valid value.\n", sub_type );
    }
  break;

  case 'T':
    manage2parent( "M", "xfmr_jacket", "bus_front_box"  );
  break;

  case 'E':
    manage2parent( "M", "equiv_jacket", "bus_front_box");
  break;

  case 'R':
    manage2parent( "M", "reg_xfmr_jacket", "bus_front_box");
  break;

  case 'Q':
    manage2parent( "M", "pq_curve_jacket", "bus_front_box");
  break;

  case 'X':
    manage2parent( "M", "swr_reac_jacket", "bus_front_box");
  break;
  }
}



void cancel_bus_settings()
/*************************************************************************\
*
*	Purpose:  Clears all data flags - so program will be properly
*		  clued if any flags are not set.
*
*	Author: B. Rogers 	Jan 12, 93
*
*	Called by: "Cancel" pushbutton in bus_front_jacket.
*
\*************************************************************************/
{

  rec_type = '*';
  sub_type = '*';
  send_type= UNKNOWN_INFO;
  br_type  = '*';

  bus_not_created_flg = TRUE;

}


void move_field( char *chg_field, char *fld_name,
					   int col_1,  int col_2, char *frmt )
/*****************************************************************************\
*       Purpose:  Move a field - unconditionally
*
*       Author: B Rogers      Date: Mar 25, 1992
*
*       Called by: 
*
\*****************************************************************************/
{
   int len;
   char new_value[20];
   
   len = col_2 - col_1+1;

   ck_col_params( col_1, col_2, chg_field, "change_field" );	/* error ck */

   lookup_and_get_field(  fld_name, new_value, sizeof(new_value) );
   add_blanks_to_string (  new_value, len+1 );
   reformat (new_value, new_value, frmt);

   memcpy ( &chg_field[col_1] , new_value, len );

}
void change_field( char *old_fld, char *chg_field, char *fld_name,
					   int col_1,  int col_2, char *frmt )
/*****************************************************************************\
*       Purpose:  examine a field and if change, replace entire field
*		  with new data.  If no change, then blank out the field.
*
*       Author: B Rogers      Modified: Mar 25, 1992
*
*       Called by: 
*
\*****************************************************************************/
{
   char old_value[20], new_value[20], c20[20];
   int i, j, len, status;
   int non_blank= 0;
   
   len = col_2 - col_1+1;

   ck_col_params( col_1, col_2, chg_field, "change_field" );	/* error ck */

   lookup_and_get_field(  fld_name, new_value, sizeof(new_value) );
   add_blanks_to_string (  new_value, len+1 );
   reformat (new_value, new_value, frmt);

   stringPart ( old_fld, old_value, col_1, col_2 );
   add_blanks_to_string ( old_value, len+1 );
   reformat (old_value, old_value, frmt);

   if( strcmp( old_value, new_value ) )
   {
     for (i=0 ;i<(int)strlen(new_value) ; i++ )
     {
	if ( new_value[i] != ' ' )
	{
	   non_blank = 1;
	   break;
	}
     }

     if ( non_blank == 0 ) {
	len = strlen( new_value );
	if ( len == 1 )
	  new_value[0] = '0';
        else if ( len == 2 )
 	  strcpy ( new_value, ".0" );
	else if ( len >= 2 )
	  strcpy ( new_value, "0.0" );
     }
     memcpy ( &chg_field[col_1] , new_value, len );
     chg_field[col_1+len] = '\0';
   }

   else
      add_blanks_to_string( chg_field, col_2+1 );

}



void lookup_and_fill_field_w_dec( char *fld_name, char *string, 
					int col_1, int col_2, int dec_pt )
/*****************************************************************************\
*       Purpose: Inserts a portion of "string" into text box "fld_name"
*		 col_1 = starting column of string.
*		 col_2 = ending     "    "    "
*		 dec_pt = n, place decimal n spaces from right
*	 		< 0, no decimal
*
*       Author: B Rogers	Mar 17, 1992	Modified: Dec 31, 1992
*							  May 26, 1992
*
*       Called by: Too many to name!
*
\*****************************************************************************/
#define MAX_FILL_LEN	132
{

  Widget id;
  char field[MAX_FILL_LEN];
  char tmp_str [MAX_FILL_LEN];
  char new_str[MAX_FILL_LEN];

  if( ( id = widget_id( fld_name )) == FALSE) return;

  if( col_2 >= MAX_FILL_LEN )
  {
    printf("*** WARNING *** PROGRAMMER ERROR in lookup_and_fill_field_w_dec\n");
    printf("                Text field too long (%d) to handle.\n", col_2);
    printf("                End column changed to %d.\n", MAX_FILL_LEN-1 );
    col_2 = MAX_FILL_LEN-2;
  }

  strncpy( tmp_str, string, MAX_FILL_LEN );	/* ensure calling routine's   */
						/* input literal isn't altered*/

  add_blanks_if_short( tmp_str, col_2, fld_name ); /* ensure stringPart works */
						   /* if string is too short  */
  if ( ck_col_params( col_1, col_2, tmp_str, 
				"lookup_and_fill_field_w_dec" )) return;

  stringPart( tmp_str, field, col_1, col_2 );

  if ( dec_pt>=0 && char_pos( field, '.' )==0 ) /* chk if decimal pt required */
  {
    /* add in a mandatory decimal point   */
    add_decimal( field, dec_pt, col_2-col_1+1, tmp_str);
    if( atof( tmp_str ) == 0.0 )
    {
      tmp_str[0] = '\0';		/* zeros are blank-filled for clarity */
      add_blanks_to_string( tmp_str, col_2-col_1+1 );
    }
  } 
  else
    strncpy( tmp_str, field, sizeof(tmp_str) );
  
    /** decimal fields should have leading blanks removed **/
  if( dec_pt >= 0 )
    clip_outside_blanks( tmp_str );
  else
    clip_trailing_blanks( tmp_str );

 /* the following if logic clears the text box to circumvent
  * ridiculous MOTIF short-comings of not excepting 0 length strings
  * set text to single space, select it, and remove it from the text box
  */
  if(strlen(tmp_str) == 0) {
    sprintf(tmp_str,"\0");
    /* sprintf(tmp_str," ");  Doesn't work, loads nothing in 1-char field! */
    /* sprintf(tmp_str,"");   Doesn't work, causes X Toolkit Warning */
  } 
  XmTextFieldSetString( id, tmp_str ); /* Compound string to text box */
}








void lookup_and_fill_field( char *fld_name, char *string, int col_1, int col_2 )
/*****************************************************************************\
*       Purpose: Standard (and safe?) method to move a string of characters
*		 to a MOTIF text box widget.
*
*       Author: B Rogers    Mar 17, 1992	Modified: Apr 28, 1993
*
*	Altered May 26, 1994 	To call lookup_and_fill_field_w_dec so that
*	blocks of code would not be duplicated.
*
*       Called by: Too many to name!
*
\*****************************************************************************/
{

  lookup_and_fill_field_w_dec ( fld_name, string, col_1, col_2, -1 );

}



void lookup_and_get_field( char *fld_name, char *ret_string, int size )
/*****************************************************************************\
*       Purpose:  Given a MOTIF text field name, (or text box)
*		  Return the character string from that box.
*		  Also ensure string is null terminated  (2-17-94)
*
*	* Programers BEWARE! * Param ret_string must be allocated!
*			       i.e. the calling module must have:
*			          char *ret_string[MAX]; 
*			       then use:
*				  lookup_and_get_field( "a_field:, *ret_string )
*
*       Author: B Rogers	Mar 18, 1992      Modified: Jul 28, 1992
*
*       Called by: 	edge.c 		filedlgrtn.c
*			linezcalc.c	pf_cb.c
*			pf_descrip.c	printopts.c
*			pscordat.c	reports.c
*			selection.c	
*
\*****************************************************************************/
{
  /** this is quick fix to existing routine **/
  Widget id;
  char *pnew_txt_widget_str;

  id = widget_id( fld_name );
  /* fields managed this way will be managed to the front dia screen... */
  if(id == NULL) {
    printf("Note: field %s Not managed now in error_message_dialog\n",fld_name);
    manage2parent("M", fld_name, "error_message_dialog");
    id = widget_id( fld_name );
    if(id == NULL) {
      printf("Warning - field %s not found...returning\n", fld_name);
      return;
    } else {
      printf("Warning...managing unfound text field %s to error_message_dialog\n", 
        fld_name);
    }
  }
  if( ( id = widget_id( fld_name ) ) )
  {
    pnew_txt_widget_str = XmTextFieldGetString(id);

   /* could overwrite memory if shortage of space in calling program */
    strncpy(ret_string, pnew_txt_widget_str, size );
    XtFree(pnew_txt_widget_str);
  }
  else
  {
    memset ( ret_string, '*', sizeof(ret_string) );
  }

  ret_string[size-1] = '\0';	/* assure a null-terminated string	*/
				/* -1 added in 3/2/94			*/
}


int check_sub_type( )
/*****************************************************************************\
*
*	Purpose: Check to confirm that card type is consistant with sub_type
*
*	Author: Bill Rogers	Jan 14, 1993
*
*
\*****************************************************************************/
{
  switch ( rec_type )
  {
    case '+':
      switch ( sub_type )
      {
	case 'A':
	case 'C':
	case 'F':
	case 'I':
	case 'N':
	case 'P':
	case 'S':
	case ' ':
		/* every thing O.K. */
	break;
   
      default:	
	printf("*** WARNING *** in call to routine [check_sub_type]\n");
	printf("*   Card subtype %c is not a character for\n",sub_type);
	printf("*   a continuation bus.\n");
	printf("*   Valid characters are: A, C, F, I, N, P, S or blank.\n");
    }
    break;

    case 'A':
    break;

    case 'B':
      switch (sub_type)
      {
	case ' ':
	case 'T':
	case 'C':
	case 'V':
	case 'E':
	case 'F':
	case 'Q':
	case 'G':
	case 'X':
	case 'S':
	case 'D':
	case 'M':
	     /* everything O.K. */
        break;

      default:
        printf("*** WARNING *** in call to routine [check_sub_type]\n");
	printf("*   Card subtype %c is not a valid character for\n",sub_type);
	printf("*   a bus card.\n");
	printf("*   Valid characters are: T, C, V, E, F, Q, G, X, S, D, M, or blank.\n");
      }
    break;

    case 'E':
    case 'X':
      if ( sub_type!=' ' )
      {
	printf("*** WARNING *** in call to routine [check_sub_type]\n");
	printf("*   Card subtype %c is not a valid character for\n",sub_type);
	printf("*   an %c card.\n");
	printf("*   Subtype replaced with a blank.\n");
	sub_type = ' ';
      }
    break;

    case 'I':
    break;

    case 'L':
      switch (sub_type)
      {
        case ' ':
	case 'D':
	case 'M':
           /* everything O.K. */
        break;

        default:
	printf("*** WARNING *** in call to routine [check_sub_type]\n");
	printf("*   Line subtype %c is not a valid character for\n",sub_type);
	printf("*   a line card.\n");
	printf("*   Valid characters are: D, M, or blank.\n");
      }
    break;

    case 'T':
      switch (sub_type)
      {
      case ' ':
      case 'P':
        /* everything O.K. */
      break;

      default:
	printf("*** WARNING *** in call to routine [check_sub_type]\n");
	printf("*   Card subtype %c is not a valid character for\n",sub_type);
	printf("*   a transformer card.\n");
	printf("*   Valid characters are: P, or blank.\n");
      }
    break;

    case 'R':
      if ( sub_type!=' ' && sub_type!='V' && sub_type!='Q' &&
	   sub_type!='P' && sub_type!='N' && sub_type!='M')
      {
        printf("*** WARNING *** in call to routine [check_sub_type]\n");
        printf("*   Card subtype %c is not a valid character for\n",sub_type);
        printf("*   a regulating transformer card.\n");
        printf("*   Valid characters are: V, Q, P, N, M, or blank.\n");
      }
    break;

    default:
      printf("*** ERROR *** in call to routine [check_sub_type]\n");
      printf("*       Card type '%c' is not a valid character\n", rec_type);
      printf("*	Valid characters are: B, L, T, +, E, R, X.\n");
      printf("*	Card build process - aborted.\n\n");
      return(1);

  }
  return( 0 );
}


void strcat_field( char *out_rec, char *fld_name, int fix_len, char *fmt )
/*****************************************************************************\
*
*	Purpose: Get text box data and add to "out_rec"
*
*	Author: Bill Rogers
*		jul 27, 1992		Modified: Jan 6, 1993
*
*	Called by: Lots of routines.
*
\*****************************************************************************/
{
   char string[MAX_FILL_LEN];
   
   if ( fix_len>MAX_FILL_LEN)
   {
      printf("*** WARNING *** from strcat_field (module selection.c)\n");
      printf("                fix_len (%d) too large!  Adjusted to %d\n",
			 fix_len, MAX_FILL_LEN-2 );
      fix_len =MAX_FILL_LEN-2;
   }

   lookup_and_get_field( fld_name, string, sizeof(string) );

   reformat( string, string, fmt );
   add_blanks_to_string ( string, fix_len );

   strcat( out_rec, string );
}



void send_add_data_to_powerflow ( Widget w, XtPointer tag,
					 XmAnyCallbackStruct *cbs )
/*****************************************************************************\
*       Purpose:   Answers the "SEND" button, decides what type of card
*		  is to be built.
*
*       Author: B Rogers 	May 14, 1992		Modified:
*
*       Called by: SEND pushbutton in bus_front_box
*
\*****************************************************************************/
{
  DBID junkbus_br_db;
  int rtncde;
  char *newnamebase;
  char value_str[133];
  int input_cursor = 0;

  switch ( send_type )
  {
    case BUS_INFO:
      lookup_and_get_field( "bus_front_volts", value_str, 4 ); 
      if( atof( value_str ) == 0.0 ) {
        sprintf( errmsg, "Bus Voltage has zero value. Bus not added to PF.\n" );
        err.line = EM_LINE;
        err.msg  = errmsg;
        err.link = "selection";
        err.type = WARNING;
        err.ident = sccsid;
        em_show( &err );
        printf( "BAD bus voltage!\n" );
        return;
      }

      build_add_rec( mod_rec );
      setCurBus( &mod_rec[6] );

      rtncde = pfchanges_cb( 0, mod_rec, 0 );	/* send changes to pf */
      /** add check to make sure change went through to powerflow **/
      /** and reject bus if powerflow didn't take the change  **/
      if(rtncde == TRUE) {
        sprintf( errmsg, "Powerflow rejected changes (%s)\n", mod_rec);
        err.line = EM_LINE;
        err.msg  = errmsg;
        err.link = "selection";
        err.type = WARNING;
        err.ident = sccsid;
        em_show( &err );
        printf("Powerflow Error: reject change and remove bus\n");
        /* deleteVertexbyName(NEW_BUS_NAME); */
        return;
      }

      bus_not_created_flg = FALSE;

      lookup_and_get_field( "bus_front_name", bus_namevolt, 9 );
      add_blanks_if_short( bus_namevolt, 8, "bus_namevolt" );
      bus_namevolt[8] = 0;
      strcat_field( bus_namevolt, "bus_front_volts", 4, "f4.0" );

      /* changeGraphBusVertex( NEW_BUS_NAME, bus_namevolt );  */
      setCurBus( bus_namevolt );
	  disable_pushbutton( "add_pushbutton"    );/* gray-out this button */
	  enable_pushbutton ( "delete_pushbutton" );
	  enable_pushbutton ( "close_pushbutton"  );
	  enable_pushbutton ( "send_pushbutton"  );
	  enable_pushbutton ( "reset_pushbutton"  );

      display_bus_selection( mod_rec, junkbus_br_db );	/* update bus box */
      XtVaSetValues( (widget_id("bus_front_name")) , XmNeditable, FALSE, NULL );
      XtVaSetValues( (widget_id("bus_front_volts")), XmNeditable, FALSE, NULL );
      Vuit_MU( "M", "bus_jacket_new_option_menu" );

      if( bus_help_not_disabled )
      {
        Vuit_MU( "U", "bus_help_1" );
        Vuit_MU( "U", "bus_help_2" );
        Vuit_MU( "M", "bus_help_3" );
        Vuit_MU( "M", "bus_help_4" );
      }
    break;

    case CONT_INFO:
      if( sub_type != '?' )
      {
        build_add_rec( mod_rec );
        rtncde = pfchanges_cb(0,mod_rec,0);	
        if(TRUE != rtncde) {
           printf("Programmer Error: change rejected (%s)\n", mod_rec);
        }
        display_bus_selection( org_bus, junkbus_br_db );   /* update bus box */
      }
      else
      {
	Vuit_MU ( "M", "cont_type_warning_form" );
      }
    break;

    case REAC_INFO:
      disable_pushbutton ( "bus_jacket_swr_reac_pb" );
    case TRANSF_INFO:
    case EQUIV_INFO:
    case REGXFMR_INFO:
    case BRANCH_INFO:
      if( check_for_zero_impedance() == 0 );
      {
	sprintf( errmsg, "Zero impedances will prevent powerflow solution.");
	err.line = EM_LINE;
        err.msg  = errmsg;
        err.link = "selection";
        err.type = WARNING;
	err.ident = sccsid;
	em_show( &err );
      }

      build_add_rec( mod_rec );
      rtncde = pfchanges_cb(0,mod_rec,0);	
      if(TRUE != rtncde) {
         printf("Programmer Error: change rejected (%s)\n", mod_rec);
      } else {
        add_graph_line(&mod_rec[6]);
      }

  getCurBus(&newnamebase);	/* just in case */
  sprintf( org_bus,  "B     %12.12s", newnamebase);

      if( strlen( org_bus ) )
      {
        display_bus_selection( org_bus, junkbus_br_db );   /* update bus box */
      }
      else	/* assume no originating bus */
      {
        Vuit_MU( "U", "bus_front_box" );
      }
	  disable_pushbutton( "add_pushbutton"    );/* gray-out this button */
	  enable_pushbutton ( "delete_pushbutton" );
	  enable_pushbutton ( "close_pushbutton"  );
	  enable_pushbutton ( "send_pushbutton"  );
	  enable_pushbutton ( "reset_pushbutton"  );

    break;

    case AREA_INFO:
    case INTERTIE_INFO:
      Vuit_MU( "U", "area_interchange_box" );
      build_add_rec( mod_rec );
      if( area_cont_card ) mod_rec[1] = '4';
      rtncde = pfchanges_cb(0,mod_rec,0);	
      if(TRUE != rtncde) {
         printf("Programmer Error: change rejected (%s)\n", mod_rec);
      }
      area_box_fill_flag = FALSE;
      fill_area_selection_box2( 0, 0, 0);
    break;

    case PQ_CURVE_INFO:
      build_pq_curve_add_rec();
      rtncde = pfchanges_cb(0,mod_rec,0);	
      if(TRUE != rtncde) {
         printf("Programmer Error: change rejected (%s)\n", mod_rec);
      }
      display_bus_selection( org_bus, junkbus_br_db );   /* update bus box */
    break;


    default:
      printf("* WARNING * from send_add_data_to_powerflow\n");
      printf("            attempt to send - send_type wasn't set right!\n");
    break;
  }

}


void send_del_data_to_powerflow ( Widget w, XtPointer tag,
					 XmAnyCallbackStruct *cbs )
/*****************************************************************************\
*       Purpose:  Answers the "DELETE" button, decides what type of data
*		  is to be restored.
*
*       Author: B Rogers 	May 21, 1992		Modified:
*
*       Called by: DELETE pushbutton in bus_front_box
*
\*****************************************************************************/
{
  DBID junkbus_br_db;
  DB_STAT stat;
  int rtncde;
  char nobus[64];

  switch ( send_type )
  {
    case BUS_INFO:
      strncpy( mod_rec, org_data, 18 );
      mod_rec[2] = 'D';
      mod_rec[18] = '\0';
      rtncde = pfchanges_cb(0,mod_rec,0);
      if(TRUE != rtncde) {
         printf("Programmer Error: change rejected (%s)\n", mod_rec);
      } 

      if(TRUE == rtncde) {
        /** success in deleting bus from powerflow **/
        stringPart( bus_namevolt, mod_rec, 6, 18 );
#ifdef BAD_WID_DELETE
        rtncde = turnoffVertexbyName( bus_namevolt );
#else
        rtncde = deleteVertexbyName( bus_namevolt );
#endif
        sprintf(nobus, "            ");
        setCurBus(nobus);
        if( rtncde != 0 ) {
          printf("Warning - bus not deleted\n");
        }
      }
    break;

    case CONT_INFO:
      strncpy( mod_rec, org_data, 18 );
      mod_rec[2] = 'D';
      mod_rec[33] = 0;
      rtncde = pfchanges_cb(0,mod_rec,0);
      if(TRUE != rtncde) {
         printf("Programmer Error: change rejected (%s)\n", mod_rec);
      }
      display_bus_selection( org_bus, junkbus_br_db );   /* update bus box */
    break;

    case AREA_INFO:
      Vuit_MU( "U", "area_interchange_box" );
      strncpy( mod_rec, org_data, 13 );
      mod_rec[2] = 'D';
      mod_rec[33] = 0;
      rtncde = pfchanges_cb(0,mod_rec,0);
      if(TRUE != rtncde) {
         printf("Programmer Error: change rejected (%s)\n", mod_rec);
      }
      area_box_fill_flag = FALSE;
      fill_area_selection_box2( 0, 0, 0);
    break;

    case INTERTIE_INFO:
      Vuit_MU( "U", "area_interchange_box" );
      strncpy( mod_rec, org_data, 24 );
      mod_rec[2] = 'D';
      mod_rec[33] = 0;
      rtncde = pfchanges_cb(0,mod_rec,0);
      if(TRUE != rtncde) {
         printf("Programmer Error: change rejected (%s)\n", mod_rec);
      }
      area_box_fill_flag = FALSE;
      fill_area_selection_box2( 0, 0, 0);
    break;

    case PQ_CURVE_INFO:
      build_pq_curve_del_rec();
      display_bus_selection( org_bus, junkbus_br_db );   /* update bus box */
    break;

    case REAC_INFO:
      strncpy( mod_rec, org_data, 18 );
      mod_rec[2] = 'D';
      mod_rec[33] = 0;
      rtncde = pfchanges_cb(0,mod_rec,0);
      if(TRUE != rtncde) {
         printf("Programmer Error: change rejected (%s)\n", mod_rec);
      }
      display_bus_selection( org_bus, junkbus_br_db );   /* update bus box */
      enable_pushbutton ( "bus_jacket_swr_reac_pb" );
    break;

    case BRANCH_INFO:
    case TRANSF_INFO:
    case EQUIV_INFO:
    case REGXFMR_INFO:
      strncpy( mod_rec, org_data, 33 );
      mod_rec[2] = 'D';
      mod_rec[33] = 0;

      rtncde = pfchanges_cb(0,mod_rec,0);
      if(TRUE != rtncde) {
         printf("Programmer Error: change rejected (%s)\n", mod_rec);
      }
      display_bus_selection( org_bus, junkbus_br_db );   /* update bus box */

      if(TRUE == rtncde) {
        /* powerflow says the branch was successfully deleted */
        /* so attempt to remove it from the screen */
#ifdef BAD_WID_DELETE
        rtncde = turnoffEdgebyName( &mod_rec[0], &mod_rec[6], &mod_rec[19]); 
#else
        rtncde = deleteEdgebyName( &mod_rec[0], &mod_rec[6], &mod_rec[19]); 
#endif
        if( rtncde != 0 ) {
          printf("Warning - edge not deleted\n");
        }
      }

    break;

    default:
      printf("* WARNING * from send_del_data_to_powerflow\n");
      printf("            attempt to send - send_type wasn't set right!\n");
    break;
  }
}


void send_mod_data_to_powerflow ( Widget w, XtPointer tag, 
					XmAnyCallbackStruct *cbs )
/*****************************************************************************\
*       Purpose:  Answers the "SEND" button, decides what type of card
*		  is to be built.
*
*       Author: B Rogers	 May 14, 1992	Modified: Jan 7, 1993
*
*       Called by: SEND pushbutton in bus_front_box
*
\*****************************************************************************/
{
  DBID junkbus_br_db;
  XmString item_str;
  Widget wid_id;
  int postn;
  char str[20];
  char busname[32];
  int rtncde;

  switch ( send_type )
  {
    case BUS_INFO:

	/* Check for possible bus name change */
      lookup_and_get_field( "bus_front_name", busname, 8 );
      add_blanks_to_string( busname, 8 );
      lookup_and_get_field( "bus_front_volts", str, 4 );
      add_blanks_to_string( str, 4 );
      sprintf( busname, "%8.8s%4.4s", busname, str );

      if( strncmp( &org_bus[6], busname, 12 ) )
      {
        if( askUser(w,"Yes","No","Change bus name in Powerflow?")==1)
	{
	  sprintf( mod_rec, "ZB    %12.12s  %12.12s", &org_bus[6], busname );
          rtncde = pfchanges_cb(0,mod_rec,0);

	  if( TRUE != rtncde )
          {
	    printf("Programmer Error: change rejected (%s)\n", mod_rec);
	  }
	  else
	  {
	    /* make changes to GraphElement */
	    changeGraphBusVertex( &org_bus[6], busname);
	  }
	}
      }

      build_mod_rec( mod_rec );

      if( mod_rec[1]==' ' && org_bus[1]!=' ' )	/* send 0 as to not confuse */
        mod_rec[1] = '0';			/* the powerflow	    */

      rtncde = pfchanges_cb(0,mod_rec,0);
      if(TRUE != rtncde) {
         printf("Programmer Error: change rejected (%s)\n", mod_rec);
      }
      display_bus_selection( mod_rec, junkbus_br_db );   /* update bus box */

      /* experimental code! */
      if( bus_box_fill_flag)
      {
	wid_id = widget_id( "bus_selection_dialog" );
	strncpy( str, mod_rec, 18 );
	str[18] = 0;
	item_str = XmStringCreate( (char *)str, XmSTRING_DEFAULT_CHARSET);
	postn = XmListItemPos( wid_id, item_str );
      }
    break;

    case CONT_INFO:
      build_mod_rec( mod_rec );
      rtncde = pfchanges_cb(0,mod_rec,0);	
      if(TRUE != rtncde) {
         printf("Programmer Error: change rejected (%s)\n", mod_rec);
      }
      display_bus_selection( org_bus, junkbus_br_db );   /* update bus box */
    break;

    case BRANCH_INFO:
    case TRANSF_INFO:
    case REAC_INFO:
    case EQUIV_INFO:
    case REGXFMR_INFO:
      build_mod_rec( mod_rec );
      rtncde = pfchanges_cb(0,mod_rec,0);
      if(TRUE != rtncde) {
         printf("Programmer Error: change rejected (%s)\n", mod_rec);
      }
      display_bus_selection( org_bus, junkbus_br_db );   /* update bus box */
    break;

    case AREA_INFO:
    case INTERTIE_INFO:
      Vuit_MU( "U", "area_interchange_box" );
      build_add_rec( mod_rec );
      mod_rec[2] = 'M';
      rtncde = pfchanges_cb(0,mod_rec,0);	
      if(TRUE != rtncde) {
         printf("Programmer Error: change rejected (%s)\n", mod_rec);
      }
      area_box_fill_flag = FALSE;
      fill_area_selection_box2( 0, 0, 0);
    break;

    case PQ_CURVE_INFO:
      /* build_pq_curve_mod_rec();  ** use "add" instead **  */
      build_pq_curve_add_rec();
      display_bus_selection( org_bus, junkbus_br_db );   /* update bus box */
    break;

    default:
      printf("* WARNING * from send_mod_data_to_powerflow\n");
      printf("            attempt to send - send_type wasn't set right!\n");
    break;
  }

}


Boolean check_for_zero_impedance()
/*************************************************************************\
* Purpose: Gives a friendly warning if uses sends line with
*	   no impedance to powerflow.
*
* Author: B.Rogers	April 16, 1993
*
* Called by:
\*************************************************************************/
{

  switch( send_type )
  {
     case TRANSF_INFO:
       if( either_is_non_zero( "xfmr_dialog_R_text", "xfmr_dialog_X_text" ) )
		 return (False);
     break;

     case EQUIV_INFO:
       if( either_is_non_zero( "equiv_G1_text", "equiv_B1_text" ) )
		 return (False);
     break;

     case BRANCH_INFO:
       if( either_is_non_zero( "line_R", "line_X" ) )
		 return (False);
     break;

     default:
	/* don't check any other types */
       return( False );
     break;
  
  }
  return (True);
}

Boolean either_is_non_zero( char *fld_1, char *fld_2 )
{
  double value;
  char str[10];

  lookup_and_get_field( fld_1, str, 6 );
  sscanf( str, "%lf", &value );
  if( value != 0.0 ) return( True );

  lookup_and_get_field( fld_2, str, 6 );
  sscanf( str, "%lf", &value );
  if( value != 0.0 ) return( True );

  return (False);
}


void reset_data ( )
/*****************************************************************************\
*       Purpose:  Answers the "RESET" button, decides what type of data
*		  is to be restored.
*
*       Author: B Rogers 	May 19, 1992		Modified:
*
*       Called by: RESET pushbutton in bus_front_box
*
\*****************************************************************************/
{

  switch ( send_type )
  {
    case BUS_INFO:
/*      fill_bus_dialog2( org_data ); */
      record_to_text_boxes( org_data );
    break;


    case AREA_INFO:
    case INTERTIE_INFO:
      fill_area_interchange( org_area );
    break;

    case CONT_INFO:
    case BRANCH_INFO:
    case TRANSF_INFO:
    case PQ_CURVE_INFO:
    case REAC_INFO:
    case EQUIV_INFO:
    case REGXFMR_INFO:
      record_to_text_boxes( org_data );
    break;
  }
}



char *add_decimal ( char *field, int dec_pt, int max_len, char *outstr )
/*****************************************************************************\
*       Purpose:   Put a decimal point in a character string
*		  Insert "dec_pt" spaces to left of EOL
*		  Skip - if decimal point is all ready in the string.
*
*       Author: B Rogers 	Jun 08, 1992	Modified: Aug 10, 1992
*
*       Called by: change_field
*		   processDrawCards	(graphpscor.c)
*
\*****************************************************************************/
{
  int rt_str;			/* right_string - # of chars to right of dp */
  int lf_str;			/*  left_string - # of chars to left  of dp */
  int len, i, j;
  char string[256];

  len = strlen( field );
  if( dec_pt < 0 || dec_pt > 50 )
  {
    printf("** WARNING ** from add_decimal (selection.c)\n" );
    printf("   Bad parameter for decimal point (%d)\n", dec_pt );
    printf("   If less than 0 or greater than 50\n" );
  }

  if( len==0 )
  {
    printf("** INFO ** Call to add_decimal (selection.c) has null field.\n" );
  }
  else if( max_len > len )
  {
    printf("** WARNING ** from add_decimal (selection.c)\n" );
    printf("   Decimal field length param (%d) appears too large for string (%s).\n", max_len, field );
  }

  /* check for existing decimal - don't mess it up! */
  if ( char_pos ( field, '.' ) ) {
    strncpy( outstr, field, len );
    return( field );
  }

  rt_str = dec_pt;
  lf_str = max_len - dec_pt;

  for ( i=0; i<lf_str; i++ )		/* 1) copy over left-chars 	*/
  {
     if ( field[ i ] == ' ' )
        string[ i ] = '0';		/*    blanks are considered zero */
     else
        string[ i ] = field[ i ];
  }
 
  string[ i ] = '.';			/* 2) add the decimal point 	*/

  for( i=lf_str; i<max_len; i++ )	/* 3) copy over remainder	*/
  { 
     if ( field[ i ] == ' ' )
        string[ i+1 ] = '0';		/*   blanks are considered zero */
     else
        string[ i+1 ] = field[ i ];
  }

  string[i+1] = 0;			/* 4) Put EOL mark in		*/

  if ( lf_str>=2 )		/* cosmetic looks - remove leading zeros */
  {
    for ( i=0; i<lf_str-1; i++ )
    {
      if ( string[ i ] == '0' )
         string[ i ] = ' ';
      else
         break;			/* stop as soon as non-zero is encountered */
    }
  }

  strcpy( outstr, string );
  return ( string );

}


int left_digits ( char *field )
/*****************************************************************************\
*       Purpose: Returns # of digits to left of decimal point.
*
*       Author: B Rogers 	Jun 09, 1992		Modified:
*
\*****************************************************************************/
{
  int len, i, dloc, ld;

  len = strlen( field );

  dloc = char_pos ( field, '.' );	/* get abs decimal location */
  
  if ( dloc==0 )	/* skip spaces <-- (leftward) until non-blank space */
  {
    for ( i=len-1; i<=0; i-- )
    {
      if ( field[ i ] != ' ' )
      {
         dloc= i+1;	/* presummed decimal point (direct) location */
         break;
      }
    }
  }

/*  count digits from assumed decimal point place until blank is found */
  for ( i=dloc-2, ld=0; i>=0; i--, ld++ )
  {
    if ( field[ i ] == ' ' ) break;
  } 

  return ( ld );	/* return # of numbers left of decimal */
}


int char_pos ( char *field, char key )
/*****************************************************************************\
*       Purpose: 
*		 Returns position of 1st occurance of a character.
*		 0 = character not found before EOL mark
*		 1 = character found in first column.
*		 n = character found in nth column.
*
*		 Note:  This is 'direct' position as opposed to standard
*		 	C code location where the 1st column is 0.
*
*       Author: B Rogers 	Jun 10, 1992		Modified:
*
\*****************************************************************************/
{
  int i, len;
  len = strlen( field );

  for ( i=0; i<len; i++ )	/* loop to find decimal */
  { 
    if ( field[i]==key ) return( i+1); 
  }

  return( 0 );			/* not found */

}


int ck_col_params ( int col_1, int col_2, char *data, char *routine_name )
/*****************************************************************************\
*  Purpose:  Check that params for beginning and end columns are ok.
*
*   Author: B Rogers 	Jul 28, 1992	Modified: Jul 29, 1992
*		Add strlen check 5/23/94
\*****************************************************************************/
{

   if( col_1<0 || col_1>500 )
   {
     printf("*** ERROR *** in call to %s\n", routine_name );
     printf("              bad start column number %d \n" ,col_1 );
     return(1);
   }

   if( col_2<0 || col_2>500 )
   {
     printf("*** ERROR *** in call to %s\n", routine_name );
     printf("              bad end column number %d \n" ,col_1 );
     return(1);
   }

   if( col_2<col_1 )
   {
     printf("*** ERROR *** in call to %s\n", routine_name );
     printf("              column 1 should be <= column 2 %d,%d \n" ,col_1, col_2 );
     return(1);
   }

   if( (int)strlen( data )<col_1 )
   {
     printf("*** ERROR *** in call to %s\n", routine_name );
     printf("    Data strlen too short - must be at least %d columns long.\n" ,col_1 );
     return(0);
   }

   return(0);
}


void add_blanks_if_short( char *string, int length, char *fld_name )
/****************************************************************************\
*
*	Purpose: Assure character string has minimun length
*		 Add blanks to extend field if needed.
*
*		 string = string to be checked (and extended)
*		 length = min length of string.
*		 fld_name = text field name (for warning message only )
*
*	Author: Bill Rogers	Dec 31, 1992
*
*	Called by: lookup_and_fill_field, lookup_and_fill_field_w_dec
*
\****************************************************************************/
{
  if ( (int)strlen( string ) < length )
  {
    add_blanks_to_string ( string, length );
  }
}




void add_blanks_to_string( char *string, int col )
/****************************************************************************\
*
*	Purpose: Assure character string has minimun length
*		 Add blanks to extend field.
*
*		 string = string to be checked (and extended)
*		 col = min length of string.
*
*	Author: Bill Rogers	jan 04, 1993
*
*	Called by: add_blanks_if_short
*
\****************************************************************************/
{
  int i=0;

   if(col > MAXFLDS) {
     printf("ERROR - add_blanks_to_string - col %d reset to %d\n",
       col, MAXFLDS);
     col = MAXFLDS;
   }

  while ( string[i] && i< col ) i++;	/* check for EOL before end */
  while ( i< col ) string[i++] = 32; 	/* add blank */ 
  string[i] = 0;			/* EOL mark */

}


void field_spec_init()
/**************************************************************************\
*
*	Purpose: initalize the field-specifications
*
*	Author: Bill Rogers	Jan 20, 1993
*
*
\**************************************************************************/
{
  if ( not_initialized )
  {
    strcpy( fld_format[0], "NULL" );	/* for possible error msg */

    field_spec("B ",  3,  5, "a3"  , "bus_owner"        , 0 );
    field_spec("B ",  6, 13, "a8"  , "bus_front_name"   , 1 );
    field_spec("B ", 14, 17, "f4.0", "bus_front_volts"  , 1 );
    field_spec("B ", 18, 19, "a2"  , "bus_zone"         , 0 );
    field_spec("B ", 20, 24, "f5.0", "bus_p_load"       , 0 );
    field_spec("B ", 25, 29, "f5.0", "bus_q_load"       , 0 );
    field_spec("B ", 30, 33, "f4.0", "bus_p_shunt"      , 0 );
    field_spec("B ", 34, 37, "f4.0", "bus_q_shunt"      , 0 );
    field_spec("B ", 38, 41, "f4.0", "bus_p_max"        , 0 );
    field_spec("B ", 42, 46, "f5.0", "bus_p_gen"        , 0 );	/* 10 */
    field_spec("B ", 47, 51, "f5.0", "bus_q_sched"      , 0 );
    field_spec("B ", 52, 56, "f5.0", "bus_q_min"        , 0 );
    field_spec("B ", 57, 60, "f4.3", "bus_v_max"	, 0 );
    field_spec("B ", 61, 64, "f4.3", "bus_v_min"        , 0 );

    field_spec("BC",  3,  5, "a3"  , "bus_owner"        , 0 );
    field_spec("BC",  6, 13, "a8"  , "bus_front_name"   , 1 );
    field_spec("BC", 14, 17, "f4.0", "bus_front_volts"  , 1 );
    field_spec("BC", 18, 19, "a2"  , "bus_zone"         , 0 );
    field_spec("BC", 20, 24, "f5.0", "bus_p_load"       , 0 );
    field_spec("BC", 25, 29, "f5.0", "bus_q_load"       , 0 );	/* 20 */
    field_spec("BC", 30, 33, "f4.0", "bus_p_shunt"      , 0 );
    field_spec("BC", 34, 37, "f4.0", "bus_q_shunt"      , 0 );
    field_spec("BC", 38, 41, "f4.0", "bus_p_max"        , 0 );
    field_spec("BC", 42, 46, "f5.0", "bus_p_gen"        , 0 );
    field_spec("BC", 47, 51, "f5.0", "bus_q_sched"      , 0 );
    field_spec("BC", 52, 56, "f5.0", "bus_q_min"        , 0 );
    field_spec("BC", 57, 60, "f4.3", "bus_v_hold"       , 0 );
    field_spec("BC", 61, 64, "f4.3", "bus_v_min"        , 0 );
 
    field_spec("BE",  3,  5, "a3"  , "bus_owner"        , 0 );
    field_spec("BE",  6, 13, "a8"  , "bus_front_name"   , 1 );
    field_spec("BE", 14, 17, "f4.0", "bus_front_volts"  , 1 );
    field_spec("BE", 18, 19, "a2"  , "bus_zone"         , 0 );
    field_spec("BE", 20, 24, "f5.0", "bus_p_load"       , 0 );
    field_spec("BE", 25, 29, "f5.0", "bus_q_load"       , 0 );
    field_spec("BE", 30, 33, "f4.0", "bus_p_shunt"      , 0 );
    field_spec("BE", 34, 37, "f4.0", "bus_q_shunt"      , 0 );
    field_spec("BE", 38, 41, "f4.0", "bus_p_max_2"      , 0 );
    field_spec("BE", 42, 46, "f5.0", "bus_p_gen_2"      , 0 );
    field_spec("BE", 47, 51, "f5.0", "bus_q_max"        , 0 );
    field_spec("BE", 52, 56, "f5.0", "bus_q_min_2"      , 0 );
    field_spec("BE", 57, 60, "f4.3", "bus_v_hold"       , 0 );
    field_spec("BE", 61, 64, "f4.3", "bus_v_min"        , 0 );

    field_spec("BF",  3,  5, "a3"  , "bus_owner"        , 0 );
    field_spec("BF",  6, 13, "a8"  , "bus_front_name"   , 1 );
    field_spec("BF", 14, 17, "f4.0", "bus_front_volts"  , 1 );
    field_spec("BF", 18, 19, "a2"  , "bus_zone"         , 0 );
    field_spec("BF", 20, 24, "f5.0", "bus_p_load"       , 0 );
    field_spec("BF", 25, 29, "f5.0", "bus_q_load"       , 0 );
    field_spec("BF", 30, 33, "f4.0", "bus_p_shunt"      , 0 );
    field_spec("BF", 34, 37, "f4.0", "bus_q_shunt"      , 0 );
    field_spec("BF", 38, 41, "f4.0", "bus_p_max_2"      , 0 );
    field_spec("BF", 42, 46, "f5.0", "bus_p_gen_2"      , 0 );
    field_spec("BF", 47, 51, "f5.0", "bus_q_max"        , 0 );
    field_spec("BF", 52, 56, "f5.0", "bus_q_min_2"      , 0 );
    field_spec("BF", 57, 60, "f4.3", "bus_v_hold"       , 0 );
    field_spec("BF", 61, 64, "f4.3", "bus_v_min"        , 0 );

    field_spec("BG",  3,  5, "a3"  , "bus_owner"        , 0 );
    field_spec("BG",  6, 13, "a8"  , "bus_front_name"   , 1 );
    field_spec("BG", 14, 17, "f4.0", "bus_front_volts"  , 1 );
    field_spec("BG", 18, 19, "a2"  , "bus_zone"         , 0 );
    field_spec("BG", 20, 24, "f5.0", "bus_p_load"       , 0 );
    field_spec("BG", 25, 29, "f5.0", "bus_q_load"       , 0 );
    field_spec("BG", 30, 33, "f4.0", "bus_p_shunt"      , 0 );
    field_spec("BG", 34, 37, "f4.0", "bus_q_shunt"      , 0 );
    field_spec("BG", 38, 41, "f4.0", "bus_p_max_2"      , 0 );
    field_spec("BG", 42, 46, "f5.0", "bus_p_gen_2"      , 0 );
    field_spec("BG", 47, 51, "f5.0", "bus_q_max"        , 0 );
    field_spec("BG", 52, 56, "f5.0", "bus_q_min_2"      , 0 );
    field_spec("BG", 57, 60, "f4.3", "bus_v_max"        , 0 );
    field_spec("BG", 61, 64, "f4.3", "bus_v_min"        , 0 );
    field_spec("BG", 65, 72, "a8"  , "bus_remote_name"  , 1 );
    field_spec("BG", 73, 76, "f4.0", "bus_remote_base"  , 1 );
    field_spec("BG", 77, 79, "f3.0", "bus_remote_pcs"   , 0 );

    field_spec("BQ",  3,  5, "a3"  , "bus_owner"        , 0 );
    field_spec("BQ",  6, 13, "a8"  , "bus_front_name"   , 1 );
    field_spec("BQ", 14, 17, "f4.0", "bus_front_volts"  , 1 );
    field_spec("BQ", 18, 19, "a2"  , "bus_zone"         , 0 );
    field_spec("BQ", 20, 24, "f5.0", "bus_p_load"       , 0 );
    field_spec("BQ", 25, 29, "f5.0", "bus_q_load"       , 0 );
    field_spec("BQ", 30, 33, "f4.0", "bus_p_shunt"      , 0 );
    field_spec("BQ", 34, 37, "f4.0", "bus_q_shunt"      , 0 );
    field_spec("BQ", 38, 41, "f4.0", "bus_p_max_2"      , 0 );
    field_spec("BQ", 42, 46, "f5.0", "bus_p_gen_2"      , 0 );
    field_spec("BQ", 47, 51, "f5.0", "bus_q_max"        , 0 );
    field_spec("BQ", 52, 56, "f5.0", "bus_q_min_2"      , 0 );
    field_spec("BQ", 57, 60, "f4.3", "bus_v_hold"       , 0 );

    field_spec("BS",  3,  5, "a3"  , "bus_owner"        , 0 );
    field_spec("BS",  6, 13, "a8"  , "bus_front_name"   , 1 );
    field_spec("BS", 14, 17, "f4.0", "bus_front_volts"  , 1 );
    field_spec("BS", 18, 19, "a2"  , "bus_zone"         , 0 );
    field_spec("BS", 20, 24, "f5.0", "bus_p_load"       , 0 );
    field_spec("BS", 25, 29, "f5.0", "bus_q_load"       , 0 );
    field_spec("BS", 30, 33, "f4.0", "bus_p_shunt"      , 0 );
    field_spec("BS", 34, 37, "f4.0", "bus_q_shunt"      , 0 );
    field_spec("BS", 38, 41, "f4.0", "bus_p_max"        , 0 );
    field_spec("BS", 42, 46, "f5.0", "bus_p_gen"        , 0 );
    field_spec("BS", 47, 51, "f5.0", "bus_q_sched"      , 0 );
    field_spec("BS", 52, 56, "f5.0", "bus_q_min"        , 0 );
    field_spec("BS", 57, 60, "f4.3", "bus_v_hold"       , 0 );
    field_spec("BS", 61, 64, "f4.1", "bus_phase_xx"     , 0 );

    field_spec("BT",  3,  5, "a3"  , "bus_owner"        , 0 );
    field_spec("BT",  6, 13, "a8"  , "bus_front_name"   , 1 );
    field_spec("BT", 14, 17, "f4.0", "bus_front_volts"  , 1 );
    field_spec("BT", 18, 19, "a2"  , "bus_zone"         , 0 );
    field_spec("BT", 20, 24, "f5.0", "bus_p_load"       , 0 );
    field_spec("BT", 25, 29, "f5.0", "bus_q_load"       , 0 );
    field_spec("BT", 30, 33, "f4.0", "bus_p_shunt"      , 0 );
    field_spec("BT", 34, 37, "f4.0", "bus_q_shunt"      , 0 );
    field_spec("BT", 38, 41, "f4.0", "bus_p_max"        , 0 );
    field_spec("BT", 42, 46, "f5.0", "bus_p_gen"        , 0 );
    field_spec("BT", 47, 51, "f5.0", "bus_q_sched"      , 0 );
    field_spec("BT", 52, 56, "f5.0", "bus_q_min"        , 0 );
    field_spec("BT", 57, 60, "f4.3", "bus_v_hold"       , 0 );

    field_spec("BV",  3,  5, "a3"  , "bus_owner"        , 0 );
    field_spec("BV",  6, 13, "a8"  , "bus_front_name"   , 1 );
    field_spec("BV", 14, 17, "f4.0", "bus_front_volts"  , 1 );
    field_spec("BV", 18, 19, "a2"  , "bus_zone"         , 0 );
    field_spec("BV", 20, 24, "f5.0", "bus_p_load"       , 0 );
    field_spec("BV", 25, 29, "f5.0", "bus_q_load"       , 0 );
    field_spec("BV", 30, 33, "f4.0", "bus_p_shunt"      , 0 );
    field_spec("BV", 34, 37, "f4.0", "bus_q_shunt"      , 0 );
    field_spec("BV", 38, 41, "f4.0", "bus_p_max"        , 0 );
    field_spec("BV", 42, 46, "f5.0", "bus_p_gen"        , 0 );
    field_spec("BV", 47, 51, "f5.0", "bus_q_sched"      , 0 );
    field_spec("BV", 52, 56, "f5.0", "bus_q_min"        , 0 );
    field_spec("BV", 57, 60, "f4.3", "bus_v_max"        , 0 );
    field_spec("BV", 61, 64, "f4.3", "bus_v_min"        , 0 );

    field_spec("BX",  3,  5, "a3"  , "bus_owner"        , 0 );
    field_spec("BX",  6, 13, "a8"  , "bus_front_name"   , 1 );
    field_spec("BX", 14, 17, "f4.0", "bus_front_volts"  , 1 );
    field_spec("BX", 18, 19, "a2"  , "bus_zone"         , 0 );
    field_spec("BX", 20, 24, "f5.0", "bus_p_load"       , 0 );
    field_spec("BX", 25, 29, "f5.0", "bus_q_load"       , 0 );
    field_spec("BX", 30, 33, "f4.0", "bus_p_shunt"      , 0 );
    field_spec("BX", 34, 37, "f4.0", "bus_q_shunt"      , 0 );
    field_spec("BX", 38, 41, "f4.0", "bus_p_max_2"      , 0 );
    field_spec("BX", 42, 46, "f5.0", "bus_p_gen_2"      , 0 );
    field_spec("BX", 47, 51, "f5.0", "bus_q_max"        , 0 );
    field_spec("BX", 52, 56, "f5.0", "bus_q_min_2"      , 0 );
    field_spec("BX", 57, 60, "f4.3", "bus_v_max"        , 0 );
    field_spec("BX", 61, 64, "f4.3", "bus_v_min"        , 0 );
    field_spec("BX", 65, 72, "a8"  , "bus_remote_name"  , 1 );
    field_spec("BX", 73, 76, "f4.0", "bus_remote_base"  , 1 );
    field_spec("BX", 77, 79, "f3.0", "bus_remote_pcs"   , 0 );

    field_spec("BD",  3,  5, "a3"  , "dc_bus_owner_text"      , 0 );
    field_spec("BD",  6, 13, "a8"  , "bus_front_name"         , 1 );
    field_spec("BD", 14, 17, "f4.0", "bus_front_volts"        , 1 );
    field_spec("BD", 18, 19, "a2"  , "dc_bus_zone_text"       , 0 );
    field_spec("BD", 23, 24, "i2"  , "dc_bus_bridges_text"    , 0 );
    field_spec("BD", 25, 29, "f5.1", "dc_bus_smooth_reac_text", 0 );
    field_spec("BD", 30, 34, "f5.1", "dc_bus_min_angle_text"  , 0 );
    field_spec("BD", 35, 39, "f5.1", "dc_bus_max_angle_text"  , 0 );
    field_spec("BD", 40, 44, "f5.1", "dc_bus_value_drop_text" , 0 );
    field_spec("BD", 45, 49, "f5.1", "dc_bus_bridge_rate_text", 0 );
    field_spec("BD", 50, 57, "a8"  , "dc_bus_commutate_text"  , 1 );
    field_spec("BD", 58, 61, "f4.0", "dc_volts_commutate_text", 1 );

    field_spec("BM",  3,  5, "a3"  , "dc_bus_owner_text"      , 0 );
    field_spec("BM",  6, 13, "a8"  , "bus_front_name"         , 1 );
    field_spec("BM", 14, 17, "f4.0", "bus_front_volts"        , 1 );
    field_spec("BM", 18, 19, "a2"  , "dc_bus_zone_text"       , 0 );
    field_spec("BM", 23, 24, "i2"  , "dc_bus_bridges_text"    , 0 );
    field_spec("BM", 25, 29, "f5.1", "dc_bus_smooth_reac_text", 0 );
    field_spec("BM", 30, 34, "f5.1", "dc_bus_min_angle_text"  , 0 );
    field_spec("BM", 35, 39, "f5.1", "dc_bus_max_angle_text"  , 0 );
    field_spec("BM", 40, 44, "f5.1", "dc_bus_value_drop_text" , 0 );
    field_spec("BM", 45, 49, "f5.1", "dc_bus_bridge_rate_text", 0 );
    field_spec("BM", 50, 57, "a8"  , "dc_bus_commutate_text"  , 1 );
    field_spec("BM", 58, 61, "f4.0", "dc_volts_commutate_text", 1 );
    field_spec("BM", 62, 62, "a1"  , "dc_bus_conv_type_text"  , 0 );
    field_spec("BM", 63, 65, "f3.1", "dc_bus_ign_del_text"    , 0 );
    field_spec("BM", 66, 68, "f3.1", "dc_bus_min_ext_text"    , 0 );
    field_spec("BM", 69, 74, "f6.1", "dc_bus_power_text"      , 0 );
    field_spec("BM", 75, 79, "f5.1", "dc_bus_voltage_text"    , 0 );

/*    field_spec("+ ",  2,  2, "a1"  , "cont_type_optionmenu"  , 3 );*/
    field_spec("+ ",  3,  5, "a3"  , "cont_owner_text"       , 0 );
    field_spec("+ ",  6, 13, "a8"  , "bus_front_name"        , 1 );
    field_spec("+ ", 14, 17, "f4.0", "bus_front_volts"       , 1 );
    field_spec("+ ", 18, 19, "i2"  , "code_year_text"        , 0 );
    field_spec("+ ", 20, 24, "f5.0", "cont_load_p"           , 0 );
    field_spec("+ ", 25, 29, "f5.0", "cont_load_q"           , 0 );
    field_spec("+ ", 30, 33, "f4.0", "cont_shunt_p"          , 0 );
    field_spec("+ ", 34, 37, "f4.0", "cont_shunt_q"          , 0 );
    field_spec("+ ", 42, 46, "f5.0", "cont_gen_p"            , 0 );
    field_spec("+ ", 47, 51, "f5.0", "cont_gen_qmax"         , 0 );
    field_spec("+ ", 52, 56, "f5.0", "cont_gen_qmin"         , 0 );
/*    field_spec("+ ", 74, 74, "i1"  , "cont_datein_month"     , 2 );
*    field_spec("+ ", 75, 76, "i2"  , "cont_datein_year"      , 0 );*/

    field_spec("L ",  3,  5, "a3"  , "line_owner"            , 0 );
    field_spec("L ",  6, 13, "a8"  , "bus_front_name"        , 1 );
    field_spec("L ", 14, 17, "f4.0", "bus_front_volts"       , 1 );
    field_spec("L ", 18, 18, "i1"  , "line_meter"            , 0 );
    field_spec("L ", 19, 26, "a8"  , "bus_front_name_2"      , 1 );
    field_spec("L ", 27, 30, "f4.0", "bus_front_volts_2"     , 1 );
    field_spec("L ", 31, 31, "a1"  , "line_circuit_id"       , 1 );
    field_spec("L ", 32, 32, "i1"  , "line_section"          , 1 );
    field_spec("L ", 33, 36, "f4.0", "line_rating_emergency" , 0 );
    field_spec("L ", 37, 37, "i1"  , "line_no_parallels"     , 0 );
    field_spec("L ", 38, 43, "f6.5", "line_R"                , 0 );
    field_spec("L ", 44, 49, "f6.5", "line_X"                , 0 );
    field_spec("L ", 50, 55, "f6.5", "line_G"                , 0 );
    field_spec("L ", 56, 61, "f6.5", "line_B"                , 0 );
    field_spec("L ", 62, 65, "f4.1", "line_miles"            , 0 );
/*    field_spec("L ", 74, 74, "a1"  , "line_datein_month"        , 2 );
*    field_spec("L ", 75, 76, "i2"  , "line_datein_year"         , 0 );
*    field_spec("L ", 77, 77, "a1"  , "line_dateout_month"       , 2 );
*    field_spec("L ", 78, 79, "i2"  , "line_dateout_year"        , 0 );*/
    field_spec("L ", 80, 83, "f4.0", "line_rating_thermal"      , 0 );
    field_spec("L ", 84, 87, "f4.0", "line_rating_bottleneck_text", 0 );

    field_spec("E ",  3,  5, "a3"  , "equiv_owner_text"         , 0 );
    field_spec("E ",  6, 13, "a8"  , "bus_front_name"           , 1 );
    field_spec("E ", 14, 17, "f4.0", "bus_front_volts"          , 1 );
    field_spec("E ", 18, 18, "i1"  , "equiv_meter_text"         , 0 );
    field_spec("E ", 19, 26, "a8"  , "bus_front_name_2"         , 1 );
    field_spec("E ", 27, 30, "f4.0", "bus_front_volts_2"        , 1 );
    field_spec("E ", 31, 31, "a1"  , "equiv_circuit_id_text"    , 1 );
    field_spec("E ", 32, 32, "i1"  , "equiv_section_text"       , 1 );
    field_spec("E ", 33, 36, "f4.0", "equiv_rating_nominal_text", 0 );
    field_spec("E ", 37, 37, "i1"  , "equiv_parallels_text"     , 0 );
    field_spec("E ", 38, 43, "f6.5", "equiv_R_text"             , 0 );
    field_spec("E ", 44, 49, "f6.5", "equiv_X_text"             , 0 );
    field_spec("E ", 50, 55, "f6.5", "equiv_G1_text"            , 0 );
    field_spec("E ", 56, 61, "f6.5", "equiv_B1_text"            , 0 );
    field_spec("E ", 62, 67, "f6.5", "equiv_G2_text"            , 0 );
    field_spec("E ", 68, 73, "f6.5", "equiv_B2_text"            , 0 );
/*    field_spec("E ", 74, 74, "a1"  , "equiv_datein_month_text"  , 2 );
*    field_spec("E ", 75, 76, "i2"  , "equiv_datein_year_text"   , 0 );
*    field_spec("E ", 77, 77, "a1"  , "equiv_dateout_month_text" , 2 );
*    field_spec("E ", 78, 79, "i2"  , "equiv_dateout_year_text"  , 0 );*/
    field_spec("E ", 80, 83, "f4.0", "equiv_rating_thermal_text", 0 );
    field_spec("E ", 84, 87, "f4.0", "equiv_rating_bottleneck_text", 0 );

    field_spec("LD",  3,  5, "a3"  , "dc_line_owner_text"       , 0 );
    field_spec("LD",  6, 13, "a8"  , "bus_front_name"           , 1 );
    field_spec("LD", 14, 17, "f4.0", "bus_front_volts"          , 1 );
    field_spec("LD", 19, 26, "a8"  , "bus_front_name_2"         , 1 );
    field_spec("LD", 27, 30, "f4.0", "bus_front_volts_2"        , 1 );

    field_spec("LD", 33, 36, "f4.0", "dc_line_nominal_text"     , 0 );
    field_spec("LD", 37, 42, "f6.2", "dc_line_resistance_text"  , 0 );
    field_spec("LD", 43, 48, "f6.2", "dc_line_inductance_text"  , 0 );
    field_spec("LD", 49, 54, "f6.2", "dc_line_capacitance_text" , 0 );
    field_spec("LD", 55, 55, "a1"  , "dc_line_controls_text"    , 0 );
    field_spec("LD", 56, 60, "f5.1", "dc_line_power_text"       , 0 );
    field_spec("LD", 61, 65, "f5.1", "dc_line_voltage_text"     , 0 );
    field_spec("LD", 66, 69, "f4.1", "dc_line_ign_text"         , 0 );
    field_spec("LD", 70, 73, "f4.1", "dc_line_ext_text"         , 0 );
    field_spec("LD", 74, 77, "f4.0", "dc_line_length_text"      , 0 );
    field_spec("LD", 80, 83, "f4.0", "dc_line_thermal_text"     , 0 );
    field_spec("LD", 84, 87, "f4.0", "dc_line_bottleneck_text"  , 0 );

    field_spec("LM",  3,  5, "a3"  , "dc_line_owner_text"       , 0 );
    field_spec("LM",  6, 13, "a8"  , "bus_front_name"           , 1 );
    field_spec("LM", 14, 17, "f4.0", "bus_front_volts"          , 1 );
    field_spec("LM", 18, 18, "a1"  , "dc_line_metering_text"    , 0 );
    field_spec("LM", 19, 26, "a8"  , "bus_front_name_2"         , 0 );
    field_spec("LM", 27, 30, "f4.0", "bus_front_volts_2"        , 0 );

    field_spec("LM", 33, 36, "f4.0", "dc_line_nominal_text"     , 0 );
    field_spec("LM", 37, 42, "f6.2", "dc_line_resistance_text"  , 0 );
    field_spec("LM", 43, 48, "f6.2", "dc_line_inductance_text"  , 0 );
    field_spec("LM", 49, 54, "f6.2", "dc_line_capacitance_text" , 0 );
    field_spec("LM", 70, 73, "f4.0", "dc_line_length_text"      , 0 );
/*    field_spec("LM", 74, 74, "a1"  , "dc_line_in_month_text"    , 2 );
*    field_spec("LM", 75, 76, "i2"  , "dc_line_in_year_text"     , 0 );
*    field_spec("LM", 77, 77, "a1"  , "dc_line_out_month_text"   , 2 );
*    field_spec("LM", 78, 79, "i2"  , "dc_line_out_year_text"    , 0 );*/
    field_spec("LM", 80, 83, "f4.0", "dc_line_thermal_text"     , 0 );
    field_spec("LM", 84, 87, "f4.0", "dc_line_bottleneck_text"  , 0 );

    field_spec("R ",  3,  5, "a3"  , "regxfmr_owner_text"       , 0 );
    field_spec("R ",  6, 13, "a8"  , "bus_front_name"           , 1 );
    field_spec("R ", 14, 17, "f4.0", "bus_front_volts"          , 1 );
    field_spec("R ", 19, 26, "a8"  , "bus_front_name_2"         , 1 );
    field_spec("R ", 27, 30, "f4.0", "bus_front_volts_2"        , 1 );

    field_spec("R ", 33, 40, "a8",   "regxfmr_remote_name_text" , 0 );
    field_spec("R ", 41, 44, "f4.0", "regxfmr_remote_kv_text"   , 0 );
    field_spec("R ", 45, 49, "f5.2", "regxfmr_min_tap_text"     , 0 );
    field_spec("R ", 50, 54, "f5.2", "regxfmr_max_tap_text"     , 0 );
    field_spec("R ", 55, 56, "i2"  , "regxfmr_taps_text"        , 0 );

    field_spec("RV",  3,  5, "a3"  , "regxfmr_owner_text"       , 0 );
    field_spec("RV",  6, 13, "a8"  , "bus_front_name"           , 1 );
    field_spec("RV", 14, 17, "f4.0", "bus_front_volts"          , 1 );
    field_spec("RV", 19, 26, "a8"  , "bus_front_name_2"         , 1 );
    field_spec("RV", 27, 30, "f4.0", "bus_front_volts_2"        , 1 );

    field_spec("RV", 33, 40, "a8",   "regxfmr_remote_name_text" , 0 );
    field_spec("RV", 41, 44, "f4.0", "regxfmr_remote_kv_text"   , 0 );
    field_spec("RV", 45, 49, "f5.2", "regxfmr_min_tap_text"     , 0 );
    field_spec("RV", 50, 54, "f5.2", "regxfmr_max_tap_text"     , 0 );
    field_spec("RV", 55, 56, "i2"  , "regxfmr_taps_text"        , 0 );

    field_spec("RQ",  3,  5, "a3"  , "regxfmr_owner_text"       , 0 );
    field_spec("RQ",  6, 13, "a8"  , "bus_front_name"           , 1 );
    field_spec("RQ", 14, 17, "f4.0", "bus_front_volts"          , 1 );
    field_spec("RQ", 19, 26, "a8"  , "bus_front_name_2"         , 1 );
    field_spec("RQ", 27, 30, "f4.0", "bus_front_volts_2"        , 1 );
    field_spec("RQ", 33, 40, "a8",   "regxfmr_remote_name_text" , 0 );
    field_spec("RQ", 41, 44, "f4.0", "regxfmr_remote_kv_text"   , 0 );
    field_spec("RQ", 45, 49, "f5.2", "regxfmr_min_tap_text"     , 0 );
    field_spec("RQ", 50, 54, "f5.2", "regxfmr_max_tap_text"     , 0 );
    field_spec("RQ", 55, 56, "i2"  , "regxfmr_taps_text"        , 0 );
    field_spec("RQ", 57, 66, "f5.0", "regxfmr_schedMVAR_text"   , 0 );
    
    field_spec("RN",  3,  5, "a3"  , "regxfmr_owner_text"       , 0 );
    field_spec("RN",  6, 13, "a8"  , "bus_front_name"           , 1 );
    field_spec("RN", 14, 17, "f4.0", "bus_front_volts"          , 1 );
    field_spec("RN", 19, 26, "a8"  , "bus_front_name_2"         , 1 );
    field_spec("RN", 27, 30, "f4.0", "bus_front_volts_2"        , 1 );
    field_spec("RN", 33, 40, "a8",   "regxfmr_remote_name_text" , 0 );
    field_spec("RN", 41, 44, "f4.0", "regxfmr_remote_kv_text"   , 0 );
    field_spec("RN", 45, 49, "f5.2", "regxfmr_min_tap_text"     , 0 );
    field_spec("RN", 50, 54, "f5.2", "regxfmr_max_tap_text"     , 0 );
    field_spec("RN", 55, 56, "i2"  , "regxfmr_taps_text"        , 0 );
    field_spec("RN", 57, 61, "f5.0", "regxfmr_minMVAR_text"     , 0 );
    field_spec("RN", 62, 66, "f5.0", "regxfmr_maxMVAR_text"     , 0 );

    field_spec("RP",  3,  5, "a3"  , "regxfmr_owner_text"       , 0 );
    field_spec("RP",  6, 13, "a8"  , "bus_front_name"           , 1 );
    field_spec("RP", 14, 17, "f4.0", "bus_front_volts"          , 1 );
    field_spec("RP", 19, 26, "a8"  , "bus_front_name_2"         , 1 );
    field_spec("RP", 27, 30, "f4.0", "bus_front_volts_2"        , 1 );
    field_spec("RP", 33, 40, "a8",   "regxfmr_remote_name_text" , 0 );
    field_spec("RP", 41, 44, "f4.0", "regxfmr_remote_kv_text"   , 0 );
    field_spec("RP", 45, 49, "f5.2", "regxfmr_min_shift_text"   , 0 );
    field_spec("RP", 50, 54, "f5.2", "regxfmr_max_shift_text"   , 0 );
    field_spec("RP", 55, 56, "i2"  , "regxfmr_shifts_text"      , 0 );
    field_spec("RP", 57, 61, "f5.0", "regxfmr_schedflow_text"   , 0 );
    field_spec("RP", 62, 66, "f5.0", "regxfmr_maxMVAR_text"     , 0 );
/*    field_spec("RP", 74, 74, "a1"  , "regxfmr_in_month_text"    , 0 );
*    field_spec("RP", 75, 76, "i2"  , "regxfmr_in_year_text"     , 0 );
*    field_spec("RP", 77, 77, "a1"  , "regxfmr_out_month_text"   , 0 );
*    field_spec("RP", 78, 79, "i2"  , "regxfmr_out_year_text"    , 0 );*/

    field_spec("RM",  3,  5, "a3"  , "regxfmr_owner_text"       , 0 );
    field_spec("RM",  6, 13, "a8"  , "bus_front_name"           , 1 );
    field_spec("RM", 14, 17, "f4.0", "bus_front_volts"          , 1 );
    field_spec("RM", 19, 26, "a8"  , "bus_front_name_2"         , 1 );
    field_spec("RM", 27, 30, "f4.0", "bus_front_volts_2"        , 1 );
    field_spec("RM", 33, 40, "a8",   "regxfmr_remote_name_text" , 0 );
    field_spec("RM", 41, 44, "f4.0", "regxfmr_remote_kv_text"   , 0 );
    field_spec("RM", 45, 49, "f5.2", "regxfmr_max_shift_text"   , 0 );
    field_spec("RM", 50, 54, "f5.2", "regxfmr_min_shift_text"   , 0 );
    field_spec("RM", 55, 56, "i2"  , "regxfmr_shifts_text"      , 0 );
    field_spec("RM", 57, 61, "f5.0", "regxfmr_maxflow_text"     , 0 );
    field_spec("RM", 62, 66, "f5.0", "regxfmr_minflow_text"     , 0 );
/*    field_spec("RM", 74, 74, "a1"  , "regxfmr_in_month_text"    , 0 );
*    field_spec("RM", 75, 76, "i2"  , "regxfmr_in_year_text"     , 0 );
*    field_spec("RM", 77, 77, "a1"  , "regxfmr_out_month_text"   , 0 );
*    field_spec("RM", 78, 79, "i2"  , "regxfmr_out_year_text"    , 0 );*/

    field_spec("T ",  3,  5, "a3"  , "xfmr_dialog_owner_text"    , 0 );
    field_spec("T ",  6, 13, "a8"  , "bus_front_name"            , 1 );
    field_spec("T ", 14, 17, "f4.0", "bus_front_volts"           , 1 );
    field_spec("T ", 18, 18, "i1"  , "xfmr_dialog_metering_text" , 0 );
    field_spec("T ", 19, 26, "a8"  , "bus_front_name_2"          , 1 );
    field_spec("T ", 27, 30, "f4.0", "bus_front_volts_2"         , 1 );
    field_spec("T ", 31, 31, "a1"  , "xfmr_dialog_id_text"       , 1 );
    field_spec("T ", 32, 32, "i1"  , "xfmr_dialog_section_text"  , 1 );
    field_spec("T ", 33, 36, "f4.0", "xfmr_dialog_nominal_text"  , 0 );
    field_spec("T ", 37, 37, "i1"  , "xfmr_dialog_parallels_text",0 );
    field_spec("T ", 38, 43, "f6.5", "xfmr_dialog_R_text"        , 0 );
    field_spec("T ", 44, 49, "f6.5", "xfmr_dialog_X_text"        , 0 );
    field_spec("T ", 50, 55, "f6.5", "xfmr_dialog_G_text"        , 0 );
    field_spec("T ", 56, 61, "f6.5", "xfmr_dialog_B_text"        , 0 );
    field_spec("T ", 62, 66, "f5.2", "xfmr_dialog_tap1_kv_text"  , 0 );
    field_spec("T ", 67, 71, "f5.2", "xfmr_dialog_tap2_kv_text"  , 0 );
/*    field_spec("T ", 74, 74, "a1"  , "xfmr_dialog_month_in_text" , 2 );
*    field_spec("T ", 75, 76, "i2"  , "xfmr_dialog_year_in_text"  , 0 );
*    field_spec("T ", 77, 77, "a1"  , "xfmr_dialog_month_out_text", 2 );
*    field_spec("T ", 78, 79, "i2"  , "xfmr_dialog_year_out_text" , 0 );*/
    field_spec("T ", 80, 83, "f4.0", "xfmr_dialog_thermal_text"  , 0 );
    field_spec("T ", 84, 87, "f4.0", "xfmr_dialog_emerg_text"    , 0 );
    field_spec("T ", 88, 91, "f4.0", "xfmr_dialog_bottleneck_text", 0 );

    field_spec("TP",  3,  5, "a3"  , "xfmr_dialog_owner_text"    , 0 );
    field_spec("TP",  6, 13, "a8"  , "bus_front_name"            , 1 );
    field_spec("TP", 14, 17, "f4.0", "bus_front_volts"           , 1 );
    field_spec("TP", 18, 18, "i1"  , "xfmr_dialog_metering_text" , 0 );
    field_spec("TP", 19, 26, "a8"  , "bus_front_name_2"          , 1 );
    field_spec("TP", 27, 30, "f4.0", "bus_front_volts_2"         , 1 );
    field_spec("TP", 31, 31, "a1"  , "xfmr_dialog_id_text"       , 1 );
    field_spec("TP", 32, 32, "i1"  , "xfmr_dialog_section_text"  , 1 );
    field_spec("TP", 33, 36, "f4.0", "xfmr_dialog_nominal_text"  , 0 );
    field_spec("TP", 37, 37, "i1"  , "xfmr_dialog_parallels_text", 0 );
    field_spec("TP", 38, 43, "f6.5", "xfmr_dialog_R_text"        , 0 );
    field_spec("TP", 44, 49, "f6.5", "xfmr_dialog_X_text"        , 0 );
    field_spec("TP", 50, 55, "f6.5", "xfmr_dialog_G_text"        , 0 );
    field_spec("TP", 56, 61, "f6.5", "xfmr_dialog_B_text"        , 0 );
    field_spec("TP", 62, 66, "f5.2", "xfmr_dialog_phase_text"    , 0 );
    field_spec("TP", 67, 71, "f5.2", "xfmr_dialog_tap2_kv_text"  , 0 );
/*    field_spec("TP", 74, 74, "a1"  , "xfmr_dialog_month_in_text" , 2 );
*    field_spec("TP", 75, 76, "i2"  , "xfmr_dialog_year_in_text"  , 0 );
*    field_spec("TP", 77, 77, "a1"  , "xfmr_dialog_month_out_text", 2 );
*    field_spec("TP", 78, 79, "i2"  , "xfmr_dialog_year_out_text" , 0 );*/
    field_spec("TP", 80, 83, "f4.0", "xfmr_dialog_thermal_text"  , 0 );
    field_spec("TP", 84, 87, "f4.0", "xfmr_dialog_emerg_text"    , 0 );
    field_spec("TP", 88, 91, "f4.0", "xfmr_dialog_bottleneck_text", 0 );

    field_spec("X ",  3,  5, "a3"  , "swr_reac_owner_text"       , 0 );
    field_spec("X ",  6, 13, "a8"  , "bus_front_name"            , 1 );
    field_spec("X ", 14, 17, "f4.0", "bus_front_volts"           , 1 );
    field_spec("X ", 20, 27, "a8"  , "swr_reac_remote_name_text" , 0 );
    field_spec("X ", 28, 31, "f4.0", "swr_reac_remote_kv_text"   , 0 );
    field_spec("X ", 32, 32, "i1"  , "swr_reac_steps_1_text"     , 0 );
    field_spec("X ", 33, 37, "f5.0", "swr_reac_mvar_1_text"      , 0 );
    field_spec("X ", 38, 38, "i1"  , "swr_reac_steps_2_text"     , 0 );
    field_spec("X ", 39, 43, "f5.0", "swr_reac_mvar_2_text"      , 0 );
    field_spec("X ", 44, 44, "i1"  , "swr_reac_steps_3_text"     , 0 );
    field_spec("X ", 45, 49, "f5.0", "swr_reac_mvar_3_text"      , 0 );
    field_spec("X ", 50, 50, "i1"  , "swr_reac_steps_4_text"     , 0 );
    field_spec("X ", 51, 55, "f5.0", "swr_reac_mvar_4_text"      , 0 );
    field_spec("X ", 56, 56, "i1"  , "swr_reac_steps_5_text"     , 0 );
    field_spec("X ", 57, 61, "f5.0", "swr_reac_mvar_5_text"      , 0 );
    field_spec("X ", 62, 62, "i1"  , "swr_reac_steps_6_text"     , 0 );
    field_spec("X ", 63, 67, "f5.0", "swr_reac_mvar_6_text"      , 0 );
    field_spec("X ", 68, 68, "i1"  , "swr_reac_steps_7_text"     , 0 );
    field_spec("X ", 69, 73, "f5.0", "swr_reac_mvar_7_text"      , 0 );
    field_spec("X ", 74, 74, "i1"  , "swr_reac_steps_8_text"     , 0 );
    field_spec("X ", 75, 79, "f5.0", "swr_reac_mvar_8_text"      , 0 );
 
    field_spec("A ",  3, 12, "a10"  , "area_name_text"           , 1 );
    field_spec("A ", 13, 20, "a8"   , "area_slack_bus_name_text" , 1 );
    field_spec("A ", 21, 24, "f4.0" , "area_base_kv_text"        , 1 );
    field_spec("A ", 26, 33, "f8.0" , "area_scheduled_export_text" , 0 );
    field_spec("A ", 35, 36, "a2"   , "area_zone_1_text"         , 0 );
    field_spec("A ", 38, 39, "a2"   , "area_zone_2_text"         , 0 );
    field_spec("A ", 41, 42, "a2"   , "area_zone_3_text"         , 0 );
    field_spec("A ", 44, 45, "a2"   , "area_zone_4_text"         , 0 );
    field_spec("A ", 47, 48, "a2"   , "area_zone_5_text"         , 0 );
    field_spec("A ", 50, 51, "a2"   , "area_zone_6_text"         , 0 );
    field_spec("A ", 53, 54, "a2"   , "area_zone_7_text"         , 0 );
    field_spec("A ", 56, 57, "a2"   , "area_zone_8_text"         , 0 );
    field_spec("A ", 59, 60, "a2"   , "area_zone_9_text"         , 0 );
    field_spec("A ", 62, 63, "a2"   , "area_zone_10_text"        , 0 );
    field_spec("A ", 72, 75, "f4.3" , "area_max_volt_text"       , 0 );
    field_spec("A ", 76, 79, "f4.3" , "area_min_volt_text"       , 0 );

    field_spec("AC",  3, 12, "a10"  , "area_name_text"           , 1 );
    field_spec("AC", 13, 20, "a8"   , "area_slack_bus_name_text" , 1 );
    field_spec("AC", 21, 24, "f4.0" , "area_base_kv_text"        , 1 );
    field_spec("AC", 26, 33, "f8.0" , "area_scheduled_export_text" , 0 );
    field_spec("AC", 35, 36, "a2"   , "area_zone_1_text"         , 0 );
    field_spec("AC", 38, 39, "a2"   , "area_zone_2_text"         , 0 );
    field_spec("AC", 41, 42, "a2"   , "area_zone_3_text"         , 0 );
    field_spec("AC", 44, 45, "a2"   , "area_zone_4_text"         , 0 );
    field_spec("AC", 47, 48, "a2"   , "area_zone_5_text"         , 0 );
    field_spec("AC", 50, 51, "a2"   , "area_zone_6_text"         , 0 );
    field_spec("AC", 53, 54, "a2"   , "area_zone_7_text"         , 0 );
    field_spec("AC", 56, 57, "a2"   , "area_zone_8_text"         , 0 );
    field_spec("AC", 59, 60, "a2"   , "area_zone_9_text"         , 0 );
    field_spec("AC", 62, 63, "a2"   , "area_zone_10_text"        , 0 );
    field_spec("AC", 72, 75, "f4.3" , "area_max_volt_text"       , 0 );
    field_spec("AC", 76, 79, "f4.3" , "area_min_volt_text"       , 0 );

    field_spec("A1",  3, 12, "a10"  , "area_name_text"           , 1 );
    field_spec("A1", 13, 20, "a8"   , "area_slack_bus_name_text" , 1 );
    field_spec("A1", 21, 24, "f4.0" , "area_base_kv_text"        , 1 );
    field_spec("A1", 26, 33, "f8.0" , "area_scheduled_export_text" , 0 );
    field_spec("A1", 35, 36, "a2"   , "area_zone_1_text"         , 0 );
    field_spec("A1", 38, 39, "a2"   , "area_zone_2_text"         , 0 );
    field_spec("A1", 41, 42, "a2"   , "area_zone_3_text"         , 0 );
    field_spec("A1", 44, 45, "a2"   , "area_zone_4_text"         , 0 );
    field_spec("A1", 47, 48, "a2"   , "area_zone_5_text"         , 0 );
    field_spec("A1", 50, 51, "a2"   , "area_zone_6_text"         , 0 );
    field_spec("A1", 53, 54, "a2"   , "area_zone_7_text"         , 0 );
    field_spec("A1", 56, 57, "a2"   , "area_zone_8_text"         , 0 );
    field_spec("A1", 59, 60, "a2"   , "area_zone_9_text"         , 0 );
    field_spec("A1", 62, 63, "a2"   , "area_zone_10_text"        , 0 );
    field_spec("A1", 72, 75, "f4.3" , "area_max_volt_text"       , 0 );
    field_spec("A1", 76, 79, "f4.3" , "area_min_volt_text"       , 0 );

    field_spec("A2",  3, 12, "a10"  , "area_name_text"           , 1 );
    field_spec("A2", 13, 20, "a8"   , "area_slack_bus_name_text" , 1 );
    field_spec("A2", 21, 24, "f4.0" , "area_base_kv_text"        , 1 );
    field_spec("A2", 26, 33, "f8.0" , "area_scheduled_export_text" , 0 );
    field_spec("A2", 35, 36, "a2"   , "area_zone_1_text"         , 0 );
    field_spec("A2", 38, 39, "a2"   , "area_zone_2_text"         , 0 );
    field_spec("A2", 41, 42, "a2"   , "area_zone_3_text"         , 0 );
    field_spec("A2", 44, 45, "a2"   , "area_zone_4_text"         , 0 );
    field_spec("A2", 47, 48, "a2"   , "area_zone_5_text"         , 0 );
    field_spec("A2", 50, 51, "a2"   , "area_zone_6_text"         , 0 );
    field_spec("A2", 53, 54, "a2"   , "area_zone_7_text"         , 0 );
    field_spec("A2", 56, 57, "a2"   , "area_zone_8_text"         , 0 );
    field_spec("A2", 59, 60, "a2"   , "area_zone_9_text"         , 0 );
    field_spec("A2", 62, 63, "a2"   , "area_zone_10_text"        , 0 );
    field_spec("A2", 72, 75, "f4.3" , "area_max_volt_text"       , 0 );
    field_spec("A2", 76, 79, "f4.3" , "area_min_volt_text"       , 0 );

    field_spec("A3",  3, 12, "a10"  , "area_name_text"           , 1 );
    field_spec("A3", 13, 20, "a8"   , "area_slack_bus_name_text" , 1 );
    field_spec("A3", 21, 24, "f4.0" , "area_base_kv_text"        , 1 );
    field_spec("A3", 26, 33, "f8.0" , "area_scheduled_export_text" , 0 );
    field_spec("A3", 35, 36, "a2"   , "area_zone_1_text"         , 0 );
    field_spec("A3", 38, 39, "a2"   , "area_zone_2_text"         , 0 );
    field_spec("A3", 41, 42, "a2"   , "area_zone_3_text"         , 0 );
    field_spec("A3", 44, 45, "a2"   , "area_zone_4_text"         , 0 );
    field_spec("A3", 47, 48, "a2"   , "area_zone_5_text"         , 0 );
    field_spec("A3", 50, 51, "a2"   , "area_zone_6_text"         , 0 );
    field_spec("A3", 53, 54, "a2"   , "area_zone_7_text"         , 0 );
    field_spec("A3", 56, 57, "a2"   , "area_zone_8_text"         , 0 );
    field_spec("A3", 59, 60, "a2"   , "area_zone_9_text"         , 0 );
    field_spec("A3", 62, 63, "a2"   , "area_zone_10_text"        , 0 );
    field_spec("A3", 72, 75, "f4.3" , "area_max_volt_text"       , 0 );
    field_spec("A3", 76, 79, "f4.3" , "area_min_volt_text"       , 0 );

    field_spec("A4",  3, 12, "a10"  , "area_name_text"           , 1 );
    field_spec("A4", 13, 20, "a8"   , "area_slack_bus_name_text" , 1 );
    field_spec("A4", 21, 24, "f4.0" , "area_base_kv_text"        , 1 );
    field_spec("A4", 26, 33, "f8.0" , "area_scheduled_export_text" , 0 );
    field_spec("A4", 35, 36, "a2"   , "area_zone_1_text"         , 0 );
    field_spec("A4", 38, 39, "a2"   , "area_zone_2_text"         , 0 );
    field_spec("A4", 41, 42, "a2"   , "area_zone_3_text"         , 0 );
    field_spec("A4", 44, 45, "a2"   , "area_zone_4_text"         , 0 );
    field_spec("A4", 47, 48, "a2"   , "area_zone_5_text"         , 0 );
    field_spec("A4", 50, 51, "a2"   , "area_zone_6_text"         , 0 );
    field_spec("A4", 53, 54, "a2"   , "area_zone_7_text"         , 0 );
    field_spec("A4", 56, 57, "a2"   , "area_zone_8_text"         , 0 );
    field_spec("A4", 59, 60, "a2"   , "area_zone_9_text"         , 0 );
    field_spec("A4", 62, 63, "a2"   , "area_zone_10_text"        , 0 );
    field_spec("A4", 72, 75, "f4.3" , "area_max_volt_text"       , 0 );
    field_spec("A4", 76, 79, "f4.3" , "area_min_volt_text"       , 0 );

    field_spec("I ",  3, 12, "a10"  , "interchange_area_1_text"  , 1 );
    field_spec("I ", 14, 23, "a10"  , "interchange_area_2_text"  , 1 );
    field_spec("I ", 26, 33, "f8.0" , "interchange_export_text"  , 0 );

    not_initialized = 0;
/*    printf(" %d fields initialized\n", num_flds );*/
  }
}



void field_spec( char *crd_type,
		 int col_1,
		 int col_2,
		 char *frmt,
		 char *text_box,
		 int  instr )
/**************************************************************************\
*
*	Purpose: fills in field specs - helps make above routine clean.
*
*	Author: Bill Rogers 	Jan 20, 1993
*
\**************************************************************************/
{

  if ( MAXFLDS < ++num_flds )
  {
    printf("PROGRAM ERROR - limited reached on MAXFLDS!!");
  }
  else
  {
    strncpy( fld_crd_type[ num_flds ], crd_type, 2 );
    fld_col_one[ num_flds ] = col_1;
    fld_col_end[ num_flds ] = col_2;
    strcpy( fld_format  [ num_flds ], frmt );
    strcpy( fld_text_box[ num_flds ], text_box );
    fld_instr  [ num_flds ] = instr;
  }

}


void record_to_text_boxes( char *data )
/***************************************************************************\
*
*	Purpose: cuts apart a pf record according to type and
*		 fills data into respective MOTIF text boxes.
*
*	Author: Bill Rogers Jan 20, 1993
*
\***************************************************************************/
{
  int i,l,dec_pt, nchar;
  char ftyp[3], c[2];
  char *date_mon;

  ftyp[2] = 0;
  c[1] = '\0';

  i = find_first_field_spec( ftyp );

  if( i==0 )
  {
    printf("WARNING - from record_to_text_boxes (selection.c )\n" );
    printf("  Unknown card type (%s)\n", ftyp );
    return;
  }

  if( ftyp[0]=='+' )
    nchar = 1;		/* continuation bus - compare only one char */
  else
    nchar = 2;

  while( strncmp(fld_crd_type[i], ftyp, nchar )== 0 && i<= num_flds )
  {
    switch ( fld_instr[i] )
    {
    case 0:
    case 1:
      switch ( fld_format[i][0] )
      {
        case 'a': 
        case 'i': 
          lookup_and_fill_field( fld_text_box[i],
	  		       data,
	  		       fld_col_one[i],
 			       fld_col_end[i]);
        break;
    
        case 'f': 
          l = strlen( fld_format[i] );
          c[0] = fld_format[i][l-1];
          dec_pt = atoi_cnt( &c[0], 1 );
          lookup_and_fill_field_w_dec( fld_text_box[i],
				     data,
				     fld_col_one[i],
 				     fld_col_end[i],
				     dec_pt );
        break;
 
        default:
          printf("PROGRAM ERROR - from record_to_text_boxes (selection.c)\n");
	  printf("  Invalid array element in fld_format[%d]=(%s).\n",
		 i, fld_format[i] );
          printf("  Format spec must begin with (a)lpha, (i)nteger or (f)loat.\n" );
      }
      break;

    case 2:	/* special instructions to convert mon to 3-char */
      XmTextFieldSetString( widget_id( fld_text_box[i] ), 
      			month_to_three_char( data[ fld_col_one[i] ] ) );
    break;

    }
    i++	;
  }

}

int find_first_field_spec( char *type )
{
  int i, nchar;

  field_spec_init();

  type[0] = rec_type;
  type[1] = sub_type;
  type[2] = 0;

  if( rec_type=='+' )
    nchar = 1;		/* continuation bus - compare only one char */
  else
    nchar = 2;

  if( num_flds == 0 ) printf("Field specs not loaded!\n" );
  if( strlen(type) < 2 ) printf("rec_type or sub_type is NULL!\n" );

  for ( i=1; i<=num_flds && strncmp(fld_crd_type[i], type, nchar ); i++ );
/*  printf("field_spec data begins at %d\n", i ); */
  if( i> num_flds )
  {
    printf("PROGRAM ERROR - cannot process %s card\n", type );
    return(0);
  }
  return(i);

}


void build_add_rec( char *add_rec )
/***************************************************************************\
* Purpose: assembles data from text box to build ANY type of record.
*	   ALSO: used to build FULL area/interchange record.
*
*	Author: Bill Rogers Jan 21, 1993
*
* Called by: set_bus_type
*	     set_line_type
*	     fill_area_interchange
*	     set_area_cont_edit
*	     send_add_data_to_powerflow (5)
*	     send_mod_data_to_powerflow
\***************************************************************************/
{

  int i,next_col, nchar;
  char ftyp[3], mon_str[4];

  check_sub_type( );

  add_rec[0] = rec_type;
  add_rec[1] = sub_type;
  add_rec[2] = ' ';
  add_rec[3] = 0;
  next_col = 3;

  /** dlc manage the destination jacket...only need to manage/not fill **/
  if( rec_type == 'B' ) manage_bus_dialog(add_rec);

  i = find_first_field_spec( ftyp );

  if( ftyp[0]=='+' )
    nchar = 1;
  else
    nchar = 2;

  while( strncmp(fld_crd_type[i], ftyp, nchar )== 0 && i<= num_flds )
  {
    if( next_col < fld_col_one[i] )
      add_blanks_to_string( add_rec, fld_col_one[i] );

    switch ( fld_instr[i] )
    {
    case 2:		/* convert 3-char mon to 1-9,O,N,D */
      lookup_and_get_field( fld_text_box[i], mon_str, 3 );
      add_rec[ fld_col_one[i]   ] = three_char_to_month( mon_str );
      add_rec[ fld_col_one[i]+1 ] = 0;
    break;
 
    case 0:
    case 1:
      strcat_field( add_rec,
		  fld_text_box[i],
 		  fld_col_end[i] - fld_col_one[i] + 1,
		  fld_format[i] );
    } 
    
    next_col =fld_col_end[i++] + 1;
  }


}


void build_mod_rec( char *new_rec )
/***************************************************************************\
*
*	Purpose: assembles data from text box to build ANY type of
*		 record.
*
*	Author: Bill Rogers Jan 21, 1993
*
\***************************************************************************/
{

  int i,next_col, nchar;
  char ftyp[3], c_mon, mon_str[4];

/*  check_sub_type(); */

  strncpy( new_rec, org_data, 119 );
  new_rec[2] = 'M';
  next_col  = 3;

  i = find_first_field_spec( ftyp );

  if( ftyp[0]=='+' )
    nchar = 1;
  else
    nchar = 2;

  while( strncmp(fld_crd_type[i], ftyp, nchar )== 0 && i<= num_flds )
  {
    if( next_col < fld_col_one[i] )
      add_blanks_to_string( new_rec, fld_col_one[i] );

    switch ( fld_instr[i] )
    {
    case 2:		/* convert 3-char mon to 1-9,O,N,D */
      lookup_and_get_field( fld_text_box[i], mon_str, 3 );
      c_mon = three_char_to_month( mon_str );

      if( c_mon != org_data[ fld_col_one[i] ] )
        new_rec[ fld_col_one[i] ] = c_mon;
      else
        new_rec[ fld_col_one[i] ] = ' ';

      new_rec[ fld_col_one[i] +1 ] = 0;

    break;

    case 0:		/* only if change occured */
      change_field( org_data, new_rec,
		  fld_text_box[i],
		  fld_col_one[i], 
 		  fld_col_end[i], 
		  fld_format[i] );
    break;

    case 1:		/* always copy this field */
      move_field( new_rec,
		  fld_text_box[i],
		  fld_col_one[i], 
 		  fld_col_end[i], 
		  fld_format[i] );
    break;
    
    }

    next_col =fld_col_end[i++] + 1;
  }


}


void set_rec_type( char rec, char stype, int info, int mode )
/***************************************************************************\
*
*	Purpose: Allow 'outside' routines to set internal record types.
*
*	Author: Bill Rogers Feb 03, 1993
*
*	Called by: toolbox.c
*
\***************************************************************************/
{

  rec_type = rec;
  sub_type = stype;
  send_type= info;
  title_mode= mode;

}

void cascade_pd_label_set( char *pd_name, char *str )
/***********************************************************************\
* Copies new label string into a pulldown menu - with checks to
*	prevent bombs.
*
* Author: Bill Rogers	June 22, 1993
* Called by: set_regxfmr_jckts
\***********************************************************************/
{

  Widget menu_id;

  if ( (menu_id = widget_id ( pd_name ) ) )
  {
    XtVaSetValues( (Widget)XmOptionButtonGadget(menu_id), 
                   XmNlabelString,
                   XmStringCreate( str, XmSTRING_DEFAULT_CHARSET),
                   NULL);
  }
}
