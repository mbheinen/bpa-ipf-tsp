static char sccsid[]="@(#)printopts.c	20.33 1/20/99";
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
* file: printopts.c
* author: John Rutis
* date: Jan 8, 1993
* purpose: functions to handle print plot options
*
\*****************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>
#include <Xm/Xm.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/Text.h>
#include "dmgr.h"
#include "coord_data.h"
#include "graph_data.h"
#include "ipf_ipc.h"

#include "em.h"
em_init

#define ON		True
#define OFF		False
#define TOGGLE_BUTTON	1
#define RADIO_BUTTON	2
#define TEXT_BOX	3
#define PRT_FALSE	0
#define PRT_TRUE	1
#define PRT_NEVER	2
#define PRT_TEXT	3

double  paper_wd, paper_len;
double  offset_x, offset_y;
static double  scale_x_old, scale_y_old;
#define MINDIV 1.0e-06
Boolean portrait_mode = True;
static  char out_buffer[BUFSIZE];

#define MAXOPTS		100
char optw_1   [MAXOPTS][5];
char optw_2   [MAXOPTS][5];
char optw_3   [MAXOPTS][7];
char optw_4   [MAXOPTS][5];
char opt_long2[MAXOPTS][20];
char opt_long3[MAXOPTS][20];
char opt_long4[MAXOPTS][20];
int  opt_ty   [MAXOPTS];
Boolean state [MAXOPTS];
Boolean deflt [MAXOPTS];
char opt_wid_name[MAXOPTS][30];
int  prt_flg  [MAXOPTS];
Boolean show_flg = False;
Boolean dbg_flg = False;

static int num_opts = 0;
static int opt_not_init = 1;


void init_prtopt_dialog		();
void process_optrecord		( char *opt_record );
int  set_diagram_flow_deflts	( char dia_type );
void change_flow_buttons	( Widget w, char *dia_type,	
				  XmAnyCallbackStruct *reason);
void set_double_paper_size	();
void set_standard_paper_size	();
void get_paper_size		( double *x_wd,    double *y_ht,
				  double *sc_x, double *sc_y,
				  double *os_x, double *os_y );
void set_flow_lst		( Boolean v1, Boolean v2, Boolean v3,
				  Boolean v4, Boolean v5, Boolean v6,
				  Boolean v7, Boolean v8 );
void set_button_sensivity	( char *widget_name, Boolean v1 );
void set_button_state 		( char *name, Boolean state );
void set_text			( char *widget_name, char *word );
int  option_setting		( char *widget_name);
void read_print_options		( Widget w,char *tag,XtPointer *callback_data);

void option_spec_init		();
void rb				( char *op_2, char *op_3,
				  char *op_4, int prt, char *wid_name,
				  char *long2, char *long3, char *long4);
void tb				( char *op_2,  char *op_3,
			 	  Boolean state,  int prt,  char *wid_name,
				  char *long2, char *long3 );
void tx				( char *op_2, char * wid_name,
				  char *long2 );
void option_spec		( char *op_1, char *op_2, char *op_3,
				  char *opt_4, int type, Boolean state,
				  int prt, char *op_bn_name,
				  char *long2, char *long3, char *long4 );
int  get_option_index		( char *w1, char *w2, char *w3, char *w4 );
void tell_state			( char *wid_name);
void remove_carriage_return	( char *str );
void write_out_option_cards	( FILE  *fptr );
void write_opt_card             ( FILE *fptr, char *opt_name, char *value1,
				  char *value2, char *deflt1, char *deflt2 );


extern void fetch_widget	(char *widget_name);
extern int gotime		(int now, char *msg);
extern uscanc			( char *,  char *wrd[], int *n,
				  char *lmt1, char *lmt2);
extern Widget widget_id		( char * );
extern void lookup_and_get_field( char *, char *, int );
extern void disable_pushbutton	( char * );
extern void enable_pushbutton	( char * );
extern int  psgetHtWdScale	();
extern void rescale_graph	( double xftr, double yftr, int flag );
extern void lookup_and_fill_field( char *nam, char *str, int col1, int col2);
extern Boolean get_widget_text	( char *fld_name, char *ret_string, int size );




void fetch_print_windows ()
{
  static Boolean windows_fetched = False;
  int now;

  /* set plot defaults */
  if(windows_fetched == False) {
   now = gotime(now, "");
   fetch_widget("print_opt_page_dialog");
   now = gotime(now, "fetch print_opt_page ");
   fetch_widget("print_opt_display_dialog");
   now = gotime(now, "fetch print_opt_display ");
   fetch_widget("printer_selection_box");
   now = gotime(now, "fetch printer_selection ");
   fetch_widget("plot_options_dialog");
   now = gotime(now, "fetch plot_options ");
   windows_fetched = True;
  }
}
void init_prtopt_dialog( )
/*****************************************************************************\
*	Purpose: initialize all print_opt_page_dialog values
*		
*	Author: Bill Rogers	Jan, 1993
*       modified 4.29 dc uncommented reset of fields 
*
*	Called by: startup 		(autostart.c)
*		   apply_files		(filedlgrtn.c)
*		   psbuildGraphCoord	(graphpscor.c)
\*****************************************************************************/
{
  Widget wid;

  fetch_print_windows();

  paper_wd = 21.59;		/* (portrait mode) */
  paper_len = 27.94;

  set_button_state( "print_portrait_rb", ON );
  set_button_state( "print_transparent_rb", ON );

  set_text( "print_width_text",  "21.59" );
  set_text( "print_length_text", "27.94" );

  set_text( "print_border_xpos_text", "" );
  set_text( "print_border_ypos_text", "" );

  set_text( "print_case_xpos_text", "" );
  set_text( "print_case_ypos_text", "" );

  scale_x_old = 1.0;	/* important for re-scaling factor later */
  scale_y_old = 1.0;

  set_text( "print_cmnt_xpos_text", "" );
  set_text( "print_cmnt_ypos_text", "" );

  set_text( "print_x_offset_text",  "0.0" ); /* Following lines Added 7/20/94 */
  set_text( "print_y_offset_text",  "0.0" );

  set_text( "print_x_scale_text",   "1.0" );
  set_text( "print_y_scale_text",   "1.0" );

  set_text( "label_box_x",          "12.0" );
  set_text( "label_box_y",          " 3.0" );
  set_text( "label_box_se_x",       "" );
  set_text( "label_box_se_y",       "" );

  set_text( "legend_x_text",        "" );
  set_text( "legend_y_text",        "" );

}

void set_text( char *name, char *word )
{
  Widget wid;

  if((wid = widget_id( name )) )
    XmTextSetString( wid, word );

}


void read_print_options( Widget w,char *tag,XtPointer *callback_data)
/*****************************************************************************
*	Purpose: Get the option records from the database and
*               change the print option dialogs accordingly
*		
*	Author: John Rutis	Jan, 1993
*
*	Called by: apply_files  (in filedlgrtn.c)
*
\*****************************************************************************/
{
  PSCoordFileRecord keyvalrecord;
  PSCoordFileRecord *nextptr;
  PSCoordFileRecord *searchkey = &keyvalrecord;
  DB_STAT stat;

/** if no base table exists just return (no data ) for list **/
  if(db_valid_db(&pscor_db) != D_OKAY)
  {
    printf("*** ERROR *** in read_print_options (printopts.c) - no valid pscor_db records\n");
    return;
  }

  /**************** setup up search key ***********************************/
  searchkey = &keyvalrecord;
  memset(&searchkey->cor.idx, '\0', sizeof(PSCoordFileRecord));
  searchkey->cor.idx = 0;
  strncpy(searchkey->cor.key, PSKEY_BUS, sizeof(searchkey->cor.key));

  stat = db_first(&pscor_db, &nextptr, COORD_IDX);

  if(stat == D_EOF)
  {
    printf("INFORMATION: db_search (in printopts.c) did not find any ps coord file records\n");
    return;
  }


  /****** loop to retreive and process all Option records (in key order) ****/
  while(stat == D_FOUND)
  {
    if(nextptr->cor.key[0] == 'O' || nextptr->cor.key[0] == 'o')
    {
     /*(debug)fprintf(stderr, "%d %s\n", nextptr->cor.idx, nextptr->cor.key);*/
      process_optrecord(nextptr->cor.key);
    }

    stat = db_next(&pscor_db, &nextptr, &nextptr, COORD_IDX);
  }

  psgetHtWdScale();	/* set the just-read params */

}


void process_optrecord( char *opt_record )
/*****************************************************************************\
*	Purpose:
*  		Scans and decipers an option record and sets the appropriate
*               option dialogs
*		
* Author: John Rutis	Jan, 1993	Modified: 3/3/94  by B.Rogers
*
*	Called by: read_print_options (above)
*
\*****************************************************************************/
{
  char *word[20],
       *limit1 = "#",
       *limit2 = ",= \t\n";
  int nwrd, i, op_idx;
  int iopt;
  int err_msg_flg;

  option_spec_init();

  uscanc (opt_record, word, &nwrd, limit1, limit2);
  nwrd--;

  if( strncmp( word[1], "SH", 2 )==0 )
  {
    show_flg = True;
  }
  else if( strncmp( word[1], "DB", 2 )==0 )
  {
    dbg_flg = True;
  }

  for (i = 1; i < nwrd; i++)
  {
    iopt = get_option_index( word[i-1], word[i], word[i+1], word[i+2] );
    if( iopt<0 && err_msg_flg )
    {
      remove_carriage_return( opt_record );
      printf("** GUI (printopts.c) did not understand coord card.\n(%.78s)\n",
		opt_record );
 printf("   Word #%d of (%s)(%s)(%s)(%s) has incorrect usage or spelling.\n",
		 abs(iopt), word[i-1], word[i], word[i+1], word[i+2] );
      err_msg_flg=0;
    }
    else
    {
      /* double check: ensure that all required options match!	*/
      if( strncmp( word[i-1], optw_1[iopt], strlen( optw_1[iopt] ) ) ||
	  strncmp( word[i  ], optw_2[iopt], strlen( optw_2[iopt] ) ) ||
	  (strlen( optw_3[iopt] ) &&
	     strncmp( word[i+1], optw_3[iopt], strlen( optw_3[iopt] ) ) ) ||
	  (strlen( optw_4[iopt] ) &&
	     strncmp( word[i+2], optw_4[iopt], strlen( optw_4[iopt] ) ) ) )
      {
	printf("*** PROGRAM ERROR *** get_option_index (printopts.c)\n" );
	printf("            Returned an incorrect option index!!!!\n");
      }


      else	/* everything's o.k. */
      {
        switch( opt_ty[iopt] )
        {
          case RADIO_BUTTON:
            set_button_state( opt_wid_name[iopt], ON );
            i++;
          break;

	  case TOGGLE_BUTTON:
	    set_button_state( opt_wid_name[iopt], state[iopt] );
	  break;

	  case TEXT_BOX:
	    op_idx = i;		/* ptr to the word after "OP" */
				/* There are 3 conditions where logic	*/
				/* reading new values should stop	*/
	    while( i<nwrd &&
		   strncmp( word[op_idx], optw_2[iopt], 2 )==0 &&
		   strncmp( word[i+1], "OP", 2 ) )
	    {
	      lookup_and_fill_field( opt_wid_name[iopt++], word[++i], 0, 4 );
	    }
	    i++;		/* assure correct position for next option */
	  break;
          default:
           printf("process_opt_record: programmer error bad option\n");
           err_msg_flg = 1;
          break;
        }
      }
    }
  }
}


void  remove_carriage_return( char *str )
{
  int last_char;

  last_char = strlen( str )-1;
  if( str[last_char] == '\n' )  str[last_char] = '\0';

}


void set_button_state( char *name, Boolean state )
/***************************************************************************\
*
\***************************************************************************/
{
  Widget wid;

  if( state != True && state != False ) {
    printf("*** Error in call to routine set_button_state\n");
    printf("          Param must be of value True or False\n\n");
  }

  if((wid = widget_id( name )) )
    XmToggleButtonSetState(wid, state, True);

}




void set_button_sensivity( char *widget_name, Boolean v1 )
{
  Widget wid;

  if( v1 != True && v1 != False ) {
    printf("*** Error in call to routine set_button_sensivity\n");
    printf("          Param must be of value True or False\n\n");
  }

  if((wid = widget_id( widget_name )) )
     XtSetSensitive(wid, v1 );

/*  if( v1 == False ) disable_pushbutton( widget_name ); */

}





void tell_state( char *wid_name)
{
  if( option_setting( wid_name ) )
    printf("%s is ON\n", wid_name );
  else
    printf("%s is OFF\n", wid_name );

}


void set_dia_flow_deflts(Widget w, XtPointer tag, XmAnyCallbackStruct *reason)
/*****************************************************************************\
*  Purpose: Callback to do set_diagram_flow_defaults
*		
*  Author: Bill Rogers	May, 1993
*
*  Called by: plot_diagram_group (radio buttons)
*	      May be PQ, MVAi, Loss, Diff, Coord button pushed.
*
\*****************************************************************************/
{
  char *pctag;
  char dia_type;

  pctag = (char *)tag;
  dia_type = pctag[0];

  switch(dia_type)
  {
    case 'P':		/* PQ */
    case 'D':		/* Diff */
      set_flow_lst( True, True, True, True, True, True, False, False );
    break;

    case 'L':		/* Loss */
      set_flow_lst( True, True, False, False, True, True, False, False );
    break;

    case 'I':		/* Inter */
      set_flow_lst( False, False, False, False, False, False, False, False );
    break;

    case 'M':		/* MVAi */
      set_flow_lst( True, True, False, False, True, True, False, False );
    break;

    case 'C':		/* Coord */
      set_flow_lst( False, False, False, False, False, False, False, False );
    break;

    case '%':		/* percentage */
      set_flow_lst( True, True, False, False, True, True, False, False );
    break;

  }
}


void set_flow_lst( Boolean v1, Boolean v2, Boolean v3, Boolean v4,
		   Boolean v5, Boolean v6, Boolean v7, Boolean v8 )
/*************************************************************************\
* Purpose:
*	   Across-the-board method of setting all the respective
*	   pq buttons.
*
* Author: B.Rogers	March 30, 1993
*
* Called by: above (set_dia_flow_dflts)
*
\*************************************************************************/
{

  set_button_sensivity( "print_p_send_tb",    v1 );
  set_button_sensivity( "print_q_send_tb",    v2 );
  set_button_sensivity( "print_p_receive_tb", v3 );
  set_button_sensivity( "print_q_receive_tb", v4 );

  set_button_state    ( "print_p_send_tb",    v5 );
  set_button_state    ( "print_q_send_tb",    v6 );
  set_button_state    ( "print_p_receive_tb", v7 );
  set_button_state    ( "print_q_receive_tb", v8 );

}



void change_print_plot_opts(Widget w,XtPointer tag,XmAnyCallbackStruct *reason)
/*****************************************************************************\
*	Purpose:
*  		Set the XmNuserData in the print_dialog widget to 1
*               ( This enables print_plot (in pf_cb.c) to go to
*                 PRINT mode instead of VIEW mode. )
*		
*	Author: John Rutis	Jan, 1993
*
*	Called by: OK pushbuttons on PAGE OPTIONS and DIAGRAM OPTIONS
*
\*****************************************************************************/
{
  Widget wid;
  int *number;

/*  if((wid = widget_id("print_dialog")) == False) return;
*  XtVaSetValues(wid, XmNuserData, 1, NULL);
*/
}



int option_setting(char *widget_name)
/****************************************************************************\
* Purpose: return whether the option is True or False.
*
* Called by: get_paper_size
	     write_out_option_cards
	     tell_state
	     write_coord_plot_dlg_to_db (pscordat.c)
*
\****************************************************************************/
{
  Widget wid;
  int set;

  if((wid = widget_id(widget_name)) == False)
  {
   printf("WARNING.. no toggle/radio button by name %s\n", widget_name);
    return(0);
  }

  set =(int)XmToggleButtonGetState(wid);
/*  printf("RADIO BUTTON:%s is set to state(%d)\n", widget_name, set ); */
  return( set );
}


process_prtopt_rb( Widget w, char *tag, XtCallbackList *list_data )
/***************************************************************************\
*
*
*
*
*
\***************************************************************************/
{

  if( strncmp( tag, "Landscape", 9 ) )
  {
    portrait_mode = False;
  }

  else if( strncmp( tag, "Portrait", 9 ) )
  {
    portrait_mode = True;
  }

}


void get_paper_size( double *x_wd, double *y_ht,
		     double *sc_x, double *sc_y,
		     double *os_x, double *os_y )
/***************************************************************************\
* Purpose: get the screen size from MOTIF text boxes.
*
* Called by: psgetHtWdScale	(graphpscor.c)
\***************************************************************************/
{
  char str[6];

  fetch_print_windows();

  lookup_and_get_field( "print_x_scale_text", str, sizeof( str ) );
/*  printf("screen size input x: %s Length:%d\n", str, sizeof(str) ); */
  sscanf( str, "%lf", &scale_x_old );
  if(scale_x_old < (double)MINDIV) {
    printf("Warning - scale_x_old less than min...reset to 1.0\n");
    scale_x_old = (double)1.0;
  }
  *sc_x = scale_x_old;		/* for figuring rescale factor later */

  lookup_and_get_field( "print_y_scale_text", str, sizeof( str ) );
/*  printf("screen size input y: %s Length:%d\n", str, sizeof(str)-1 ); */
  sscanf( str, "%lf", &scale_y_old );
  if(scale_y_old < (double)MINDIV) {
    printf("Warning - scale_y_old less than min...reset to 1.0\n");
    scale_y_old = (double)1.0;
  }
  *sc_y = scale_y_old;		/* for figuring rescale factor later */

/*  printf("screen size: %f %f\n", *sc_x, *sc_y ); */

  lookup_and_get_field( "print_width_text", str, sizeof( str ) );
  sscanf( str, "%lf", &paper_wd );

  lookup_and_get_field( "print_length_text", str, sizeof( str ) );
  sscanf( str, "%lf", &paper_len );

/*  if( portrait_mode ) 
*  tell_state( "print_portrait_rb" ); */
  if( option_setting( "print_portrait_rb" ) )
  {
    *x_wd = paper_wd;
    *y_ht = paper_len;
  }

  else
  {
    *x_wd = paper_len;
    *y_ht = paper_wd;
  }

  lookup_and_get_field( "print_x_offset_text", str, sizeof( str ) );
  sscanf( str, "%lf", os_x );

  lookup_and_get_field( "print_y_offset_text", str, sizeof( str ) );
  sscanf( str, "%lf", os_y );

}


void set_standard_paper_size()
/*************************************************************************\
*
*
\*************************************************************************/
{
  Widget wid;

  if((wid = widget_id("print_width_text")) )
      XmTextSetString(wid, "21.59");

  if((wid = widget_id("print_length_text")) )
      XmTextSetString(wid, "27.94");
  
}

void set_double_paper_size()
{
  Widget wid;

  if((wid = widget_id("print_width_text")) )
      XmTextSetString(wid, "27.74");

  if((wid = widget_id("print_length_text")) )
      XmTextSetString(wid, "43.18");
  
}


void option_spec_init()
/*************************************************************************\
* Purpose: Universal method of specifying all buttons and their
*	   respective I/O structure to the coord file.
*
*	   This is a 3-way thing, establishes HOW the coord card is read.
*				  establishes the activation procedure.
*				  establishes HOW the coord rec is written.
*
* Author: B. Rogers	April 1993
*
* Called by: process_optrecord
*
\*************************************************************************/
{
  if ( opt_not_init )
  {
    /* alphabetical list of TEXT boxes */
    tx( "BO", "print_border_xpos_text", "BOrder"    );
    tx( "BO", "print_border_ypos_text", "BOrder"    );
    tx( "BX", "label_box_x"           , "BX_ident"  );
    tx( "BX", "label_box_y"           , "BX_ident"  );
    tx( "BX", "label_box_se_x"        , "BX_ident"  );
    tx( "BX", "label_box_se_y"	      , "BX_ident"  );
    tx( "CA", "print_case_xpos_text"  , "CAse_name" );
    tx( "CA", "print_case_ypos_text"  , "CAse_name" );
    tx( "CO", "print_cmnt_xpos_text"  , "COmments"  );
    tx( "CO", "print_cmnt_ypos_text"  , "COmments"  );
/*  tx( "CR", "coord_name_x"          , "CR"        );*/
/*  tx( "CR", "coord_name_y"          , "CR"        );*/
    tx( "LG", "legend_x_text"         , "LGend"     );
    tx( "LG", "legend_y_text"         , "LGend"     );
    tx( "OF", "print_x_offset_text"   , "OFfset"    );
    tx( "OF", "print_y_offset_text"   , "OFfset"    );
    tx( "SC", "print_x_scale_text"    , "SCale"     );
    tx( "SC", "print_y_scale_text"    , "SCale"     );
    tx( "SI", "print_width_text"      , "SIze"      );
    tx( "SI", "print_length_text"     , "SIze"      );

/****** list of RADIO buttons 	NOTE: last one is considered default *********\
*
*   PRT_TRUE means, print this option on the coord file if button is IN 
*   PRT_NEVER means,  its a default, don't bother putting in the coord file
*
\*****************************************************************************/
    rb( "BR", "P", "C",   PRT_NEVER, "print_lines_comb_rb",
					"BRanch", "Parallels", "Combined"   );
    rb( "BR", "P", "S",   PRT_TRUE, "print_lines_sep_rb",
					"BRanch", "Parallels", "Separate"    );

    rb( "BU", "B", "A",   PRT_NEVER, "print_busname_abbrv_rb",
					"BUs_detail", "BUs",  "Abbreviation"  );
    rb( "BU", "B", "N",   PRT_TRUE,  "print_busname_full_nokv_rb",
					"BUs_detail", "BUs",  "No_kv_pf_name" );
    rb( "BU", "B", "P",   PRT_TRUE,  "print_busname_full_rb",
				   "BUs_detail",   "BUs",    "Powerflow_name" );

    rb( "BU", "V", "V",   PRT_NEVER, "print_volts_kv_rb", "BUs_detail",
							"Voltage","Volts" );
    rb( "BU", "V", "P",   PRT_TRUE, "print_volts_pu_rb", "BUs_detail",
							"Voltage","Per_unit" );

    rb( "BU", "NO_G","",  PRT_TRUE,  "print_no_gen_rb", "BUs_detail",
							"NO_Gen", "");
    rb( "BU", "G",  "",   PRT_NEVER, "print_gen_rb",    "BUs_detail",
							"Gen",""  );
    rb( "BU", "MO_G","",  PRT_TRUE,  "print_more_gen_rb", "BUs_detail",
							"MO_Gen", "");
    rb( "BU", "AL_G","",  PRT_TRUE,  "print_al_gen_rb", "BUs_detail",
							"AL_Gen", "");

    rb( "BU", "S",  "",   PRT_NEVER, "print_shunt_rb", "BUs_detail",
							"Shunt", "" );
    rb( "BU", "AL_S","",  PRT_TRUE,  "print_al_shunt_rb", "BUs_detail",
							"AL_Shunt", "" );
    rb( "BU", "NO_S","",  PRT_TRUE,  "print_no_shunt_rb", "BUs_detail",
							"NO_Shunt", "" );

    rb( "DI", "P",  "",    PRT_NEVER,"print_pq_flow_rb", "DIagram",
								"Pq_flow", "" );
    rb( "DI", "M",  "",    PRT_TRUE,"print_mvai_rb",  "DIagram",
								"Mva/I", "" );
    rb( "DI", "L",  "",    PRT_TRUE,"print_loss_rb",  "DIagram",
								"Loss", "" );
    rb( "DI", "I",  "",    PRT_TRUE,"print_inter_rb", "DIagram",	
							"Interchange", ""   );
    rb( "DI", "C",  "",    PRT_TRUE,"print_coord_rb", "DIagram",
							"Coordinates", ""   );
    rb( "DI", "%",  "",    PRT_TRUE,"print_load_percent_rb", "DIagram",
							"%_Rating", "" );

    rb( "OR", "P",  "",  PRT_NEVER, "print_portrait_rb",
			 "ORientation", "Portrait", ""     );
    rb( "OR", "L",  "",  PRT_TRUE,  "print_landscape_rb",
			 "ORientation", "Landscape", ""    );

    rb( "TR", "O",  "",  PRT_TRUE,  "print_opaque_rb",
			 "TRansparent", "Opaque", ""       );
    rb( "TR", "T",  "",  PRT_NEVER, "print_transparent_rb",
			 "TRansparent", "Transparent", ""  );

    rb( "VA", "N",  "",  PRT_NEVER,  "print_values_norm_rb",
			 "VAlues", "Normal", ""  );
    rb( "VA", "D",  "",  PRT_TRUE,   "print_values_diff_rb",
			 "VAlues", "Diff", ""  );

/************************** list of TOGGLE buttons ***************************/
			/* NOTE: 2nd of pair True/False sets button position */
			/* 	 Disagreement with True/False flag will	     */
			/*	 cause an Option record to be generated.     */

    tb( "BR","NO_T",True, PRT_NEVER, "print_x_taps_tb", "BRanch", "NO_Taps"   );
    tb( "BR","T",   False,PRT_TRUE,  "print_x_taps_tb", "BRanch", "Trans_taps");

    tb( "BR","NO_C",True, PRT_NEVER, "print_comp_tb",
						 "BRanch", "NO_Compensation" );
    tb( "BR","C",   False,PRT_TRUE,  "print_comp_tb", "BRanch", "Compensation");

    tb( "BR","O",   True, PRT_TRUE,  "print_branch_outage_tb",
							 "BRanch", "Outages" );
    tb( "BR","NO_O",False,PRT_NEVER, "print_branch_outage_tb",
							 "BRanch","NO_Outages");

    tb( "BU","NO_A",False,PRT_FALSE, "print_angle_tb", "BUs_detail","NO_Angle");
    tb( "BU","A",   True, PRT_NEVER, "print_angle_tb", "BUs_detail", "Angle" );

    tb( "BU","NO_L",False,PRT_NEVER, "print_load_tb", "BUs_detail", "NO_Load");
    tb( "BU","L",   False,PRT_TRUE,  "print_load_tb", "BUs_detail", "Load"   );

    tb( "BU","NO_T",False,PRT_NEVER, "print_cut_tb", "BUs_detail", "NO_Total");
    tb( "BU","T",   False,PRT_TRUE,  "print_cut_tb", "BUs_detail", "Total"   );

    tb( "BU","NO_O",True, PRT_NEVER, "print_bus_outage_tb",
						     "BUs_detail", "NO_Outage");
    tb( "BU","O",   False,PRT_TRUE,  "print_bus_outage_tb",
						     "BUs_detail", "Outage" );

    tb( "FL","NO_P_S",False,PRT_FALSE, "print_p_send_tb","FLow",  "NO_P_Send" );
    tb( "FL","P_S",   True, PRT_NEVER, "print_p_send_tb","FLow",  "P_Send" );

    tb( "FL","NO_Q_S",False,PRT_FALSE, "print_q_send_tb", "FLow", "NO_Q_Send" );
    tb( "FL","Q_S",   True, PRT_NEVER, "print_q_send_tb", "FLow", "Q_Send" );

    tb( "FL","NO_P_R",False,PRT_NEVER, "print_p_receive_tb",
						       "FLow", "NO_P_Receive" );
    tb( "FL","P_R",   False,PRT_TRUE,  "print_p_receive_tb",
							 "FLow", "P_Receive" );

    tb( "FL","NO_Q_R",False,PRT_NEVER, "print_q_receive_tb",
						 "FLow", "NO_Q_Receive" );
    tb( "FL","Q_R",   False, PRT_TRUE,  "print_q_receive_tb",
						 "FLow", "Q_Receive" );

    opt_not_init = 0;
    /* printf(" %d options initialized\n", num_opts ); */
  }
}


void rb( char *op_2, char *op_3, char *op_4,
		     int prt, char *wid_name,
		     char *long2, char *long3, char *long4  )
/**************************************************************************\
*
*
\**************************************************************************/
{
  option_spec( "OP", op_2, op_3,  op_4, RADIO_BUTTON, FALSE, prt, wid_name,
			long2, long3, long4 );
}
 


void tb( char *op_2,  char *op_3, Boolean state,   int prt, char *wid_name,
			char *long2, char *long3 )
{
  option_spec( "OP", op_2, op_3,  "", TOGGLE_BUTTON, state, prt, wid_name,
			long2, long3, "" );
  set_button_state( wid_name, state );
}



void tx ( char *op_2, char *wid_name, char *long2 )
{
  option_spec( "OP", op_2, "",  "", TEXT_BOX, FALSE, PRT_TEXT, 
			wid_name, long2, "", ""  );
}


void option_spec( char *op_1, char *op_2, char *op_3,  char *op_4,
		  int type, Boolean stat, int prt, char *op_bn_name,
		  char *long2, char *long3, char *long4 )
/*************************************************************************\
* Purpose: fills in an option specification.
*
* Author: Bill Rogers	Apr 23, 1993
*
* Called by: rb, tb, tx (above)
*
\*************************************************************************/
{
  int j;

  if(MAXOPTS < num_opts )
  {
    printf("*** PROGRAM ERROR *** in printopts.c - limit reached on MAXOPTS!\n" );
  }
  else
  {
    if( type==TOGGLE_BUTTON || type==RADIO_BUTTON || type==TEXT_BOX )
    {
      if( widget_id( op_bn_name ) )
      { 
        strncpy( optw_1[ num_opts ], op_1, sizeof(optw_1[num_opts] ) );
        strncpy( optw_2[ num_opts ], op_2, sizeof(optw_2[num_opts] ) );
        strncpy( optw_3[ num_opts ], op_3, sizeof(optw_3[num_opts] ) );
        strncpy( optw_4[ num_opts ], op_4, sizeof(optw_4[num_opts] ) );
        strncpy( opt_long2[ num_opts ], long2, sizeof(opt_long2[num_opts] ) );
        strncpy( opt_long3[ num_opts ], long3, sizeof(opt_long3[num_opts] ) );
        strncpy( opt_long4[ num_opts ], long4, sizeof(opt_long4[num_opts] ) );
        opt_ty[ num_opts ] = type;
        state [ num_opts ] = stat;
        deflt [ num_opts ] = stat;
	prt_flg[num_opts ] = prt;

        j = strlen(op_bn_name);
        if( strlen(op_bn_name) > sizeof(opt_wid_name) )
        {
          printf("WARNING - from option_spec routine.\n" );
	  printf(" Text, toggle or radio button has execess length name %d.\n",
			strlen(op_bn_name) );
          j = sizeof(op_bn_name);
        }
        strncpy( opt_wid_name[num_opts], op_bn_name, j );
/*      printf("%d) %s %s %s %s %s %d %s %s\n",
*			optw_1[num_opts],
*			optw_2[num_opts],
*			optw_3[num_opts],
*			optw_4[num_opts],
*			opt_ty[num_opts],
*			state [num_opts],
*			opt_wid_name[ num_opts ] );
*/

        num_opts++;
      }
      else
      {
	printf("WARNING - no such widget (%s) registered\n", op_bn_name );
      }
    }
    else
    {
      printf("INVALID option type! Must be RADIO_BUTTON, TOGGLE_BUTTON or TEXT_BOX./\n");
      return;
    }
   
  }
 
}



int get_option_index( char *w1, char *w2, char *w3, char *w4)
/***************************************************************************\
* PURPOSE: search the arrays for a matching options set
*
* Author: Bill Rogers		Date 1993
\***************************************************************************/
{
  int j1,j2,j3,j4, err;

  if( opt_not_init ) option_spec_init();

  err = -1;
  for ( j1=0; j1<num_opts; j1++ )
  {
    if( strncmp( optw_1[j1], w1, strlen( optw_1[j1] ) ) == 0 )
    {
      if( strlen(optw_2[j1]) == 0 ) return(j1);
      err = -2;
      for ( j2=j1; j2<num_opts; j2++ )
      {
        if( strncmp( optw_2[j2], w2, strlen( optw_2[j2] ) )  == 0)
        {
          if( strlen(optw_3[j2]) == 0 ) return(j2);
	  err = -3;
          for ( j3=j2; j3<num_opts; j3++ )
          {
            if( strncmp(optw_3[j3], w3, strlen( optw_3[j3]) ) == 0 &&
                strncmp(optw_2[j3], w2, strlen( optw_2[j3]) ) == 0 )
    	    {
 	     if( strlen(optw_4[j3]) == 0 ) return(j3);
	     err = -4;
 	     for ( j4=j3; j4<num_opts; j4++ ) 
	      if( strncmp(optw_4[j4], w4, strlen(optw_4[j4])) == 0 &&
                  strncmp(optw_3[j4], w3, strlen( optw_3[j4]) ) == 0 &&
                  strncmp(optw_2[j4], w2, strlen( optw_2[j4]) ) == 0 )
								 return(j4);
	    }
	  }
	}
      }
    }
  }

  return(err);

}



void write_out_option_cards( FILE  *fptr )
/****************************************************************************\
* Purpose: Write all NON-DEFAULT options.
*		NOTE: This routine transferred from pscordat.c 3/11/94
* Author: B.Rogers
*
* Called by: pswriteCoordFile
\****************************************************************************/
{

  char str1[20], str[80];
  Boolean rtncde1, to_prt_flg;
  int i,j;

  fetch_print_windows ();
  memset(str1, '\0', sizeof(str1));
  memset(str,  '\0', sizeof(str ));
  to_prt_flg = False;

  if( show_flg ) fprintf(fptr, "OPtion SHow\n" );
  if( dbg_flg  ) fprintf(fptr, "OPtion DBG\n" );

  option_spec_init();	/* just in case no options were on input data */

        /* loop through all know options as defined by option_spec_init   */
        /*      in printopts.c                                            */
  for ( i=0; i<num_opts;  )
  {
    switch( opt_ty[i] )
    {
      case TEXT_BOX:    /* PAPER SIZE, OFFSET, BORDER, CASE NAME, LEGEND, */
        strcpy( str, opt_long2[i] );
        j = i;
                   /* loop to get all x,y values */
        while( i<num_opts && strncmp( optw_2[i], optw_2[j], 2 ) ==0 )
        {
          rtncde1 = get_widget_text( opt_wid_name[i], str1, 6 );
          if((True == rtncde1 && strlen( str1 ) && 
				 strncmp( str1, "          ", strlen(str1))) ||
				 show_flg )
          {
            strcat( str, " " );
            strcat( str, str1 );
	    to_prt_flg = True;	/* flag indicates this option record	*/
				/* should be written			*/
          }
          i++;
        }
      break;

      case TOGGLE_BUTTON:
        strcpy( str, opt_long2[i] );
	if((option_setting( opt_wid_name[i] )==True  && PRT_TRUE==prt_flg[i]) ||
	   (option_setting( opt_wid_name[i] )==False && PRT_FALSE==prt_flg[i]))
        {
	  strcat( str, " " );
	  strcat( str, opt_long3[i] );
	  to_prt_flg = True;
	}
        i++;
      break;

      case RADIO_BUTTON:
        strcpy( str, opt_long2[i] );
	if((option_setting( opt_wid_name[i] )==True && PRT_TRUE==prt_flg[i]) ||
	   (option_setting( opt_wid_name[i] )==False && PRT_FALSE==prt_flg[i]))
        {
	  strcat( str, " " );
	  strcat( str, opt_long3[i] );
	  if( strlen( opt_long4[i]) )
	  {
	    strcat( str, " " );
	    strcat( str, opt_long4[i] );
	  }
	  to_prt_flg = True;
	}
        i++;
      break;

    }
    if( to_prt_flg )
    {
      fprintf(fptr, "OPtion %s\n", str );
      if ( dbg_flg ) printf( "OPtion %s\n", str ); 
    }
    to_prt_flg = False;
  }


}


void write_opt_card( FILE *fptr, char *opt_name, char *value1, char *value2,
                                 char *deflt1, char *deflt2 )
/****************************************************************************\
*
*
*
\****************************************************************************/
{

  fprintf(fptr, "OPtion  %s %s %s\n", opt_name, value1, value2 );
}

/* 
* set the return state based on the toggle buttons set
* using powers of two to allow multiple toggle buttons 
* to be turned on
*/
#define MAXOPT 12
void get_bus_name_solution_opt(int *state)
{
  static char *opt_name[] = {"print_busname_abbrv_rb", 
    "print_busname_full_nokv_rb", "print_busname_full_rb"};
  static Widget opt_wid[MAXOPT];
  static int opt_wid_ok = 0;
  Boolean flag;
  int rstate;
  int i;
  int optcnt;

  optcnt = sizeof(opt_name) / sizeof(opt_name[0]);

  if(opt_wid_ok != 1) {
    for(i = 0; i<optcnt; i++) {
      opt_wid[i] = widget_id(opt_name[i]);
    }
    opt_wid_ok = 1;
  }
  rstate = 0;
  for(i = 0; i<optcnt; i++) {
    if(opt_wid[i] != FALSE) {
      flag = XmToggleButtonGadgetGetState(opt_wid[i]);
      rstate += (1<<i)*flag;
    }
  }
  *state = rstate;
}
void get_bus_solution_opt(int *state)
{
  static char *opt_name[] = {"print_volts_kv_rb", "print_volts_pu_rb"};
  static Widget opt_wid[MAXOPT];
  static int opt_wid_ok = 0;
  Boolean flag;
  int rstate;
  int i;
  int optcnt;

  optcnt = sizeof(opt_name) / sizeof(opt_name[0]);
  if(opt_wid_ok != 1) {
    for(i = 0; i<optcnt; i++) {
      opt_wid[i] = widget_id(opt_name[i]);
    }
    opt_wid_ok = 1;
  }
  rstate = 0;
  for(i = 0; i<optcnt; i++) {
    if(opt_wid[i] != FALSE) {
      flag = XmToggleButtonGadgetGetState(opt_wid[i]);
      rstate += (1<<i)*flag;
    }
  }
  *state = rstate;
}
void get_branch_solution_opt(int *state)
{
  static char *opt_name[] = {"print_pq_flow_rb", "print_mvai_rb",
    "print_loss_rb", "print_inter_rb", "print_coord_rb", 
    "print_load_percent_rb"};
  static Widget opt_wid[MAXOPT];
  static int opt_wid_ok = 0;
  Boolean flag;
  int rstate;
  int i;
  int optcnt;

  optcnt = sizeof(opt_name) / sizeof(opt_name[0]);

  if(opt_wid_ok != 1) {
    for(i = 0; i<optcnt; i++) {
      opt_wid[i] = widget_id(opt_name[i]);
    }
    opt_wid_ok = 1;
  }
  rstate = 0;
  for(i = 0; i<optcnt; i++) {
    if(opt_wid[i] != FALSE) {
      flag = XmToggleButtonGadgetGetState(opt_wid[i]);
      rstate += (1<<i)*flag;
    }
  }
  *state = rstate;
}
void get_branch_detail_opt(int *state)
{
  static char *opt_name[] = {"print_p_send_tb", "print_q_send_tb",
    "print_p_receive_tb", "print_q_receive_tb"};
  static Widget opt_wid[MAXOPT];
  static int opt_wid_ok = 0;
  Boolean flag;
  int rstate;
  int i;
  int optcnt;

  optcnt = sizeof(opt_name) / sizeof(opt_name[0]);
  if(opt_wid_ok != 1) {
    for(i = 0; i<optcnt; i++) {
      opt_wid[i] = widget_id(opt_name[i]);
    }
    opt_wid_ok = 1;
  }
  rstate = 0;
  for(i = 0; i<optcnt; i++) {
    if(opt_wid[i] != FALSE) {
      flag = XmToggleButtonGadgetGetState(opt_wid[i]);
      rstate += (1<<i)*flag;
    }
  }
  *state = rstate;
}
void get_branch_parallel_opt(int *state)
{
  static char *opt_name[] = {"print_lines_comb_rb", "print_lines_sep_rb"};
  static Widget opt_wid[MAXOPT];
  static int opt_wid_ok = 0;
  Boolean flag;
  int rstate;
  int i;
  int optcnt;

  optcnt = sizeof(opt_name) / sizeof(opt_name[0]);
  if(opt_wid_ok != 1) {
    for(i = 0; i<optcnt; i++) {
      opt_wid[i] = widget_id(opt_name[i]);
    }
    opt_wid_ok = 1;
  }
  rstate = 0;
  for(i = 0; i<optcnt; i++) {
    if(opt_wid[i] != FALSE) {
      flag = XmToggleButtonGadgetGetState(opt_wid[i]);
      rstate += (1<<i)*flag;
    }
  }
  *state = rstate;
}
void get_branch_other_opt(int *state)
{
  static char *opt_name[] = {"print_x_taps_tb", "print_comp_tb", 
    "print_branch_outage_tb"};
  static Widget opt_wid[MAXOPT];
  static int opt_wid_ok = 0;
  Boolean flag;
  int rstate;
  int i;
  int optcnt;

  optcnt = sizeof(opt_name) / sizeof(opt_name[0]);
  if(opt_wid_ok != 1) {
    for(i = 0; i<optcnt; i++) {
      opt_wid[i] = widget_id(opt_name[i]);
    }
    opt_wid_ok = 1;
  }
  rstate = 0;
  for(i = 0; i<optcnt; i++) {
    if(opt_wid[i] != FALSE) {
      flag = XmToggleButtonGadgetGetState(opt_wid[i]);
      rstate += (1<<i)*flag;
    }
  }
  *state = rstate;
}
void get_branch_values_opt(int *state)
{
  static char *opt_name[] = {"print_values_norm_rb", "print_values_diff_rb"};
  static Widget opt_wid[MAXOPT];
  static int opt_wid_ok = 0;
  Boolean flag;
  int rstate;
  int i;
  int optcnt;

  optcnt = sizeof(opt_name) / sizeof(opt_name[0]);
  if(opt_wid_ok != 1) {
    for(i = 0; i<optcnt; i++) {
      opt_wid[i] = widget_id(opt_name[i]);
    }
    opt_wid_ok = 1;
  }
  rstate = 0;
  for(i = 0; i<optcnt; i++) {
    if(opt_wid[i] != FALSE) {
      flag = XmToggleButtonGadgetGetState(opt_wid[i]);
      rstate += (1<<i)*flag;
    }
  }
  *state = rstate;
}
void get_bus_generation_opt(int *state)
{
  static char *opt_name[] = {"print_no_gen_rb", "print_gen_rb",
   "print_more_gen_rb", "print_al_gen_rb"};
  static Widget opt_wid[MAXOPT];
  static int opt_wid_ok = 0;
  Boolean flag;
  int rstate;
  int i;
  int optcnt;

  optcnt = sizeof(opt_name) / sizeof(opt_name[0]);
  if(opt_wid_ok != 1) {
    for(i = 0; i<optcnt; i++) {
      opt_wid[i] = widget_id(opt_name[i]);
    }
    opt_wid_ok = 1;
  }
  rstate = 0;
  for(i = 0; i<optcnt; i++) {
    if(opt_wid[i] != FALSE) {
      flag = XmToggleButtonGadgetGetState(opt_wid[i]);
      rstate += (1<<i)*flag;
    }
  }
  *state = rstate;
}
