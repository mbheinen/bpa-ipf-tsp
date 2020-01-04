static char sccsid[] = "@(#)filedlgrtn.c	20.64 4/30/98";
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
*
\****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/List.h>
#include <Xm/FileSB.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/MessageB.h>

#include "ipf_ipc.h"
#include "dmgr.h"
#include "coord_data.h"
#include "ipfdebug.h"
#include "em.h"

em_init		/* Error structure macro - to call the standard error msg */

void apply_files                ( Widget, XtPointer, XmAnyCallbackStruct * );
void move_filter_text		( char *filter_name_text, Widget button_id );
void set_rdy_label		( int, char *, char * );
void copy_fn			( char *wid_name, char *wid_name_to );
void get_file_save_text_wid	( Widget *id, int ftype ); 
void copy_fn2			( char *wid_name, char *wid_name_to );
void change_lbl			( char *, char * );
void name_change_file_status_set(Widget widget, int *tag,
					XmAnyCallbackStruct *callback_data);
void file_check_and_save_cb	( Widget widget, int * tag,
		                     XmAnyCallbackStruct *callback_data);
void file_save_cb		( Widget widget, XtPointer tag,
		                     XmAnyCallbackStruct *callback_data);
void set_all_filenames		( char *fn_stub );
void fetch_file_windows		();
Boolean file_exists		( char *filename);
/* Boolean file_writeable		( char *filename );*/
void get_bg_color		( char *widget_name, Pixel *bg );
void set_filenames_userid	();
int  pf_file_exists		( char *filename );
int  pf_file_status		( char *filename );
void save_xgui			();


extern Widget VUIT_Manage       ( char *widget_name );
extern int gotime		( int now, char *str );
extern void fetch_widget	( char *wname );
extern Widget widget_id		( char * );

extern Boolean send_pf_save_file_name( char *type, char *file_name);
extern Boolean send_pf_file_name( char *, XtPointer );
extern Vuit_MU			( char *, char * );
extern void left_justify_text   ( Widget id);
extern void pfinit_cb		();
extern int  ipc_synch_rw	( char *, char * );
extern void destroyAllGadgets	( Widget, XtPointer,
				  XmAnyCallbackStruct *);
extern int  psbuildGraphCoord	( Widget, XtPointer,
				  XmAnyCallbackStruct *);
extern int  buildGraphCoord	( Widget, XtPointer,
				  XmAnyCallbackStruct *);
extern char *get_file_name_from	( XmFileSelectionBoxCallbackStruct *);
extern void read_print_options	( Widget, XtPointer,
                                  XmAnyCallbackStruct * );
extern void init_prtopt_dialog	();
extern void set_zoom_factor_1   ();

extern int  creategraphtbl      ( Widget , XtPointer,
					XmFileSelectionBoxCallbackStruct *);
extern int  askUser             ( Widget, char *, char *, char * );
extern void fillDefineList	();
extern void buildMainDisplay	( int mergeFlag );
extern void pscreate_plot_form	();
extern char *getValueOfWidget	( Widget );	/* utils.c */
extern int  psgetHtWdScale	();
extern FILE *get_xgui_ptr	( char * );
void lookup_and_get_field	( char *, char *, int );


#define	 fNone		0
#define	 fCmd	1
#define	 fChg	2
#define	 fBas	3
#define	 fNet	4
#define	 fCor	5
#define	 fPti	6

static int  button_number = 1;
       int  default_flag;
static int f_type;
static char *file_string = NULL;

char	outbuf_array[BUFSIZE];
char	inbuf_array[BUFSIZE];

/* refresh if new coord file - reloads any missing base data */
#define MAXFILES	25
#define  fsInactive	0
#define  fsCreate	1
#define  fsActive	2
#define  fsRefresh	3
#define  fsReplace	4

#define  CHG_FILE	1
#define  BAS_FILE	2
#define  NET_FILE	3
#define  COR_FILE	4
#define  STAB_FILE	5
#define  PTI_FILE	6

#define	 WRITEABLE	1
#define	 OK		0
#define	 IPC_OK		1
#define	 IPC_NOT_OK	0

static int fileStat[MAXFILES];

Widget	button_id[7],
	back_color_palette,
	active_color_palette;


char *dir_n_wid[7] ={"unused",		    /* directory text wids */
			"open_dia_command_dir_text",
			"open_dia_change_dir_text",
			"open_dia_base_dir_text",
			"open_dia_network_dir_text",
			"open_dia_coord_dir_text",
			"open_dia_pti_dir_text"};

char *fil_n_wid[7] ={"unused",		    /* file name text wids */
                        "file_select_dia_command_text",
			"file_select_dia_change_text",
			"file_select_dia_base_text",
			"file_select_dia_network_text",
			"file_select_dia_coord_text",
			"file_select_dia_pti_text"};

char *select_wid[7] = {"unsused",		    /* select button widgets */
			"opndia_cmd_pb",
			"opndia_chg_pb",
			"opndia_bas_pb",
			"opndia_net_pb",
			"opndia_cor_pb",
			"opndia_pti_pb"};

char *rdy_wid[7] =	{"unsused",		    /* "ready" label widgets */
			"opndia_cmd_rdy_lbl",
			"opndia_chg_rdy_lbl",
			"opndia_bas_rdy_lbl",
			"opndia_net_rdy_lbl",
			"opndia_cor_rdy_lbl",
			"opndia_pti_rdy_lbl"};

char *load_wid[7] =	{"unsused",		    /* "loaded" label widgets */
			"opndia_cmd_loaded_lbl",
			"opndia_chg_loaded_lbl",
			"opndia_bas_loaded_lbl",
			"opndia_net_loaded_lbl",
			"opndia_cor_loaded_lbl",
			"opndia_pti_loaded_lbl"};

char *no_ld_wid[7] ={"unsused",		    	    /* "no load" lbl widgets  */
			"opndia_cmd_noload_lbl",
			"opndia_chg_noload_lbl",
			"opndia_bas_noload_lbl",
			"opndia_net_noload_lbl",
			"opndia_cor_noload_lbl",
			"opndia_pti_noload_lbl"};

Pixel active_color, selected_color, back_color;



/*##########################################################################*\
#	Collection of routines to LOAD existing files
\*##########################################################################*/

int set_default_files( Widget widget, XtPointer tag, 
                       XmAnyCallbackStruct *callback_data)
/*****************************************************************************\
* Purpose:	Init default file names when open file dialog first opened
*
* Author: John Rutis 8 June		Modified: Aug 19, 1992
*
* Called by: file menu Open... push button
*	     autostart.c
*
\*****************************************************************************/
{
  int f;
  char *string;
  Widget wid;
  Widget cancel_wid;

  if(default_flag == 1)		/* done only once */
    return;

  default_flag = 1;

/********* Remove the CANCEL button from the file open dialog *************/

  if((wid = widget_id("file_selection_box_open")) == False) return;

  cancel_wid = XmFileSelectionBoxGetChild(wid, XmDIALOG_CANCEL_BUTTON);

  if(XtIsManaged(cancel_wid))
  {
    XtUnmanageChild(cancel_wid);
    XtDestroyWidget(cancel_wid);
  }
  else
    printf("Warning (from filedlgrtn.c) - CANCEL wid is not managed: ");

/********* Turn on READY TO LOAD label - if file exists *******************/
  /* NOTE: some of these filename textboxes MAY have been defined 
	   ( or filled ) by the XGUI file. 				*/

  for( f=1; f<=6; f++ )
  { 
    if((wid = widget_id( fil_n_wid[f])) == NULL);
    {
      string = getValueOfWidget( wid );	/* utils.c */

      if(string[0] != '\0' && file_exists( string ) )
      {
        Vuit_MU( "M", rdy_wid[f] );
        fileStat[f] = fsCreate;
      }
    }

    /* find all the pushbutton widget id's */
    button_id[f] = widget_id( select_wid[f] );
  }

/**************************************************************************/
  get_bg_color( "opndia_cor_pb"          , &active_color   );
  get_bg_color( "file_select_done_button", &selected_color );
  get_bg_color( "file_selection_box_open", &back_color     );

}



void get_bg_color( char *widget_name, Pixel *bg )
/**************************************************************************\
*  Called by:  Above
\**************************************************************************/
{
  Widget id;

  if((id = widget_id( widget_name )) != NULL)
    XtVaGetValues( id,  XmNbackground, bg, NULL);
  else
    bg = NULL;

}


void file_default_set ( Widget widget, int *tag,
			XmAnyCallbackStruct *callback_data)
/*****************************************************************************\
* Purpose: Move choosen default directory to the XmFileSelectionBox
*	
*	   Author: John Rutis	Mar 04	modified: Aug 27, 1992
*
* Called by: Any of 6 "SELECT" button callbacks (on open_file_dialog)
*
\*****************************************************************************/
{
  char out_buffer[BUFSIZE];

  err.line = EM_LINE;

  if(ipfdebug & (DB_TraceMask | DB_Filedlgrtn)) {
	fprintf(ipfdbio, "file_default_set: button_number %d\n", button_number);
  }

  if( button_id[button_number] == NULL) return;

/* Turn button off (lt yellow) - so one can be turned on later.... */
  XtVaSetValues(button_id[button_number], XmNbackground,  active_color, NULL);

  button_number = *tag;
  move_filter_text( dir_n_wid[button_number], button_id[button_number] );

  switch(button_number) 
  {
    case fCmd:
      fileStat[fChg] = fsInactive;
      fileStat[fBas] = fsInactive;
      fileStat[fNet] = fsInactive;
      fileStat[fCor] = fsInactive;
      fileStat[fPti] = fsInactive;
    break;

    case fCor:
    case fChg:
    break;

    case fBas:		/* Only Base nor Network file can be active */
      fileStat[fNet] = fsInactive;
      fileStat[fPti] = fsInactive;
    break;

    case fNet:
      fileStat[fBas] = fsInactive;
      fileStat[fPti] = fsInactive;
    break;

    case fPti:
      fileStat[fBas] = fsInactive;
      fileStat[fNet] = fsInactive;
    break;

    default:
      printf("PROGRAMMER ERROR from file_default_set (filedlgrtn.c)\n" );
      printf(" Incorrect tag (%d) from MOTIF (must be 1-6)\n", button_number);
      return;
  }

  if( fileStat[button_number] == fsActive ||
	fileStat[button_number] == fsReplace )
    fileStat[button_number] = fsReplace;

  else
    fileStat[button_number] = fsCreate;
}


void move_filter_text( char *filter_name_text, Widget button_id )
/*****************************************************************************\
* Function:   move_filter_text
*		Does two things, 
*		1) moves contents of selected file filter to main filter box.
*		2) changes yellow "Select" button to a lt blue "Make Selections"
*
*	   Author: Bill Rogers	Aug 27, 1992	Modified: 
*
*	Called by: file_default_set
\*****************************************************************************/
{
  Widget filter_id, id;
  char *filter_string;
  XmString select_str;

  err.line = EM_LINE;
  if((id        = widget_id("file_selection_box_open")) == NULL) return;
  if((filter_id = widget_id( filter_name_text        )) == NULL) return;

/**** move the chg/coord/base/net filter to the main filter box ****/

  filter_string = getValueOfWidget( filter_id );

  XtVaSetValues(id, 
                XmNdirMask,
                XmStringCreate(filter_string ,XmSTRING_DEFAULT_CHARSET), 
                XmNbackground, active_color,
                NULL);

/**** set selected button color to lt blue ****/
  select_str = XmStringCreate( "Make Selection", XmSTRING_DEFAULT_CHARSET);
  XtVaSetValues( button_id,
                XmNbackground, selected_color,
                XmNlabelString, select_str,
                NULL);

  XtFree(filter_string);
  XmStringFree(select_str);
}


int file_name_set( Widget widget, XtPointer tag,
                  XmFileSelectionBoxCallbackStruct *callback_data)
/*****************************************************************************\
* purpose: Move highlighted name to correct filename text
*
*   Author: Dan Clark/Bill Rogers    Jan 27	Modified: Aug 19, 1992
*
* Called by: "APPLY" button on XmFileSelectionBox
\*****************************************************************************/
{
  Widget 	id;
  char		*file_name;
  XmString	select_str;
  int f;
 
  file_name = get_file_name_from(callback_data);

/* Set all select buttons to active_color */
  select_str = XmStringCreate("Select", XmSTRING_DEFAULT_CHARSET);

  /* change "Select" pushbutton back to lt yellow */
  if( button_id[button_number] == NULL ) return;
  XtVaSetValues(button_id[button_number], XmNbackground, active_color,
		                XmNlabelString, select_str, NULL);

  if((id = widget_id("file_selection_box_open")) == False) return;
    XtVaSetValues(id,  XmNbackground, back_color, NULL);

  if((id = widget_id( fil_n_wid[button_number] )) == FALSE) return;
  XmTextSetString(id, file_name);
  left_justify_text(id);

  Vuit_MU( "M", rdy_wid  [button_number] );
  Vuit_MU( "U", load_wid [button_number] );
  Vuit_MU( "U", no_ld_wid[button_number] );

  switch(button_number) 
  {
    case fCmd:
      Vuit_MU( "U", rdy_wid  [fChg] );
      Vuit_MU( "U", load_wid [fChg] );
      Vuit_MU( "U", no_ld_wid[fChg] );
      Vuit_MU( "U", rdy_wid  [fBas] );
      Vuit_MU( "U", load_wid [fBas] );
      Vuit_MU( "U", no_ld_wid[fBas] );
      Vuit_MU( "U", rdy_wid  [fNet] );
      Vuit_MU( "U", load_wid [fNet] );
      Vuit_MU( "U", no_ld_wid[fNet] );
      Vuit_MU( "U", rdy_wid  [fCor] );
      Vuit_MU( "U", load_wid [fCor] );
      Vuit_MU( "U", no_ld_wid[fCor] );
      Vuit_MU( "U", rdy_wid  [fPti] );
      Vuit_MU( "U", load_wid [fPti] );
      Vuit_MU( "U", no_ld_wid[fPti] );
    break;

    case fChg:
      Vuit_MU( "U", rdy_wid  [fCmd] );
      Vuit_MU( "U", load_wid [fCmd] );
      Vuit_MU( "U", no_ld_wid[fCmd] );
    break;

    case fBas:
      Vuit_MU( "U", rdy_wid  [fNet] );
      Vuit_MU( "U", rdy_wid  [fPti] );
      fileStat[fCor] = fsReplace;	/* WER force a re-merge w/ coord file */
					/* GraphElements tend to get destroyed*/
    break;

    case fNet:
      Vuit_MU( "U", rdy_wid  [fBas] );	
      Vuit_MU( "U", rdy_wid  [fPti] );
      fileStat[fCor] = fsReplace;	/* WER force a re-merge w/ coord file */
					/* GraphElements tend to get destroyed*/
    break;

    case fCor:
    break;

    case fPti:
      Vuit_MU( "U", rdy_wid  [fBas] );	
      Vuit_MU( "U", rdy_wid  [fNet] );
      fileStat[fCor] = fsReplace;	/* WER force a re-merge w/ coord file */
    break;
  }
  XtFree(file_name);
  XmStringFree(select_str);
}




void apply_files(Widget widget,XtPointer tag,XmAnyCallbackStruct *callback_data)
/*****************************************************************************\
* purpose:  decide what to do with the files user selected on the screen
*	    uses array setup with above functions
*
* Author: Dan Clark		Mar 25, 1992
*
* Called by: "Load Selections" button at bottom of open_file_dialog popup box
*	     autostart.c
\*****************************************************************************/
{
  Widget id;
  Widget wid;

  char status_text[256];
  char *pfilename;
  char *text_str;

  int curtime = 0;			/* for timing tests */
  int merge_flag = CoordOnly;

  if(ipfdebug & (DB_TraceMask | DB_Filedlgrtn))
  {
	fprintf(ipfdbio, "apply_files: start\n");
        curtime = gotime(curtime, "Start Loading file (appy_files)");
  }

  fetch_file_windows();

/******** check through all files in special order ***************************/
  merge_flag = CoordOnly;
  destroyAllGadgets(0,0,0);
  sprintf(status_text, "Files Loaded: ");


/**************** Process the coordinate file ********************************/
  switch(fileStat[ fCor ])
  {
    case fsCreate:
    case fsReplace:
      /* set_zoom_factor_1(); */
      change_lbl( "status_coord_file_text", " " );
      copy_fn2( fil_n_wid[fCor], "save_coord_text" );
      copy_fn ( fil_n_wid[fCor], "status_coord_file_text" );

      Vuit_MU( "U", rdy_wid[fCor]  );
      Vuit_MU( "U", load_wid[fCor] );
      Vuit_MU( "M", no_ld_wid[fCor]);

      pscreatecoordtbl();	/* Delete old dmgr & create new */

      pfilename = XmTextFieldGetString(widget_id("file_select_dia_coord_text"));

      if( psopenCoordFile( pfilename ) == True )
      {
        if(ipfdebug & DB_Filedlgrtn) {
          printf("***INFO*** Create/replace coordinate file\n");
        }

        psreadCoordData();
        pscloseCoordFile();

        Vuit_MU( "U", no_ld_wid[fCor]);
        Vuit_MU( "M", load_wid[fCor] );

        merge_flag = CoordAndBase;	/* need base case later	*/
        fileStat[ fCor ] = fsActive;	/* file opened and read */
        /* refreshfile(fBas); */
        if(fileStat[fBas] == fsActive) fileStat[ fBas ] = fsRefresh;
	init_prtopt_dialog();
	read_print_options( 0, 0, 0 );
        strcat(status_text, "Coord ");
      }

      XtFree(pfilename);
      creategraphtbl	(0,0,0); /* very short init graph_db (graphdata.c) */
      psbuildGraphCoord	(0,0,0); /* xfer pscor_db to graph_db (graphcor.c) */
      fillDefineList();
    break;

    case fsInactive:
    case fsActive:
    case fsRefresh:
	/* OK: take no action */
    if(ipfdebug & DB_Filedlgrtn) {
      printf("***INFO*** No coord load  *************************\n");
    }
    break;

    default:
    if(ipfdebug & DB_Filedlgrtn) {
      printf("ERROR: fs bad switch %d\n", fileStat[ fCor ]);
    }
    break;
  }

/********************** Process the base file ********************************\
** note: need to add check to ensure either a base file OR a coord file
** is present  (could assume a merge with both selected)
*** now do operations of base file ***/

  switch(fileStat[ fBas ])
  {
    case fsCreate:
    case fsReplace:
      pfinit_cb(0,0,0);
      merge_flag = CoordAndBase;
      change_lbl( "status_base_file_text", " " );
      change_lbl( "status_base_file_label", "Base File" );

      Vuit_MU( "U", no_ld_wid[fPti]);
      Vuit_MU( "U", no_ld_wid[fNet]);
      Vuit_MU( "U", rdy_wid[fBas]  );
      Vuit_MU( "U", load_wid[fBas] );
      Vuit_MU( "M", no_ld_wid[fBas]);

      if( send_pf_file_name( "old_base", XmTextGetString(  
			    widget_id( fil_n_wid[fBas] ) ) ))
      {
      copy_fn2( fil_n_wid[fBas], "save_base_text" );
      copy_fn ( fil_n_wid[fBas], "status_base_file_text" );
        fileStat[ fBas ] = fsActive;
        Vuit_MU( "M", load_wid[fBas] );
        Vuit_MU( "U", load_wid[fNet] );
        Vuit_MU( "U", load_wid[fPti] );
        Vuit_MU( "U", no_ld_wid[fBas]);
        strcat(status_text, "Base ");
      } 

    break;

    case fsRefresh:	 /* cause a reload and merge of pf data */
      fileStat[ fBas ] = fsActive;
      merge_flag = CoordAndBase;
    break;

    case fsActive:
    case fsInactive:
	/* OK: take no action */
    break;

    default:
      printf("ERROR: fs bad switch %d\n", fileStat[ fBas ]);
    break;
  }

/******************** Process the network file ********************************/
  switch(fileStat[ fNet ])
  {
    case fsCreate:
    case fsReplace:
      pfinit_cb(0,0,0);
      merge_flag = CoordAndBase;

      change_lbl( "status_base_file_text", " " );
      change_lbl( "status_base_file_label", "Net File" );

      Vuit_MU( "U", no_ld_wid[fBas]);
      Vuit_MU( "U", no_ld_wid[fPti]);
      Vuit_MU( "U", rdy_wid[fNet] );
      Vuit_MU( "U", load_wid[fNet]);
      Vuit_MU( "M", no_ld_wid[fNet]);

      if( send_pf_file_name( "network_data", XmTextGetString(  
			    widget_id( fil_n_wid[fNet] ) ) ))
      {
      copy_fn2( fil_n_wid[fNet], "save_net_text" );
      copy_fn ( fil_n_wid[fNet], "status_base_file_text" );
        fileStat[ fNet ] = fsActive;
        Vuit_MU( "M", load_wid [fNet] );
        Vuit_MU( "U", load_wid[fBas] );
        Vuit_MU( "U", load_wid[fPti] );
        Vuit_MU( "U", no_ld_wid[fNet] );
        strcat(status_text, "Network ");
      }

    break;

    case fsRefresh:
      merge_flag = CoordAndBase;	/* reload the data from the base */
      fileStat[ fNet ] = fsActive;
    break;

    case fsActive:
    case fsInactive:
	/* OK: take no action */
    break;

    default:
      printf("ERROR: fs bad switch %d\n", fileStat[ fNet ]);
    break;
  }


/******************** Process the PTI file ********************************/
  switch(fileStat[ fPti ])
  {
    case fsCreate:
    case fsReplace:
      pfinit_cb(0,0,0);
      merge_flag = CoordAndBase;

      change_lbl( "status_base_file_text", " " );
      change_lbl( "status_base_file_label", "PTI File" );

      Vuit_MU( "U", no_ld_wid[fBas]);
      Vuit_MU( "U", no_ld_wid[fNet]);
      Vuit_MU( "U", rdy_wid[fPti] );
      Vuit_MU( "U", load_wid[fPti]);
      Vuit_MU( "M", no_ld_wid[fPti]);

      if( send_pf_file_name( "LOAD_PTI,TYPE=PTI,FILE=", XmTextGetString(  
			    widget_id( fil_n_wid[fPti] ) ) ))
      {
      copy_fn2( fil_n_wid[fPti], "save_stab_text" );
      copy_fn ( fil_n_wid[fPti], "status_base_file_text" );
        fileStat[ fPti ] = fsActive;
        Vuit_MU( "M", load_wid [fPti] );
        Vuit_MU( "U", load_wid[fBas] );
        Vuit_MU( "U", load_wid[fNet] );
        Vuit_MU( "U", no_ld_wid[fPti] );
        strcat(status_text, "Pti  ");
      }

    break;

    case fsRefresh:
      merge_flag = CoordAndBase;	/* reload the data from the base */
      fileStat[ fPti ] = fsActive;
    break;

    case fsActive:
    case fsInactive:
	/* OK: take no action */
    break;

    default:
      printf("ERROR: fs bad switch %d\n", fileStat[ fPti ]);
    break;
  }



/******************** Process the change file *********************************/
  switch(fileStat[ fChg ])
  {
    case fsCreate:
    case fsReplace:
      merge_flag = CoordAndBase;
      change_lbl( "status_change_file_text", " " );

      Vuit_MU( "U", rdy_wid[fChg] );
      Vuit_MU( "U", load_wid[fChg] );
      Vuit_MU( "M", no_ld_wid[fChg]);

      if( send_pf_file_name( "changes", XmTextGetString(  
			    widget_id( fil_n_wid[fChg] ) ) ))
      {
      copy_fn2( fil_n_wid[fChg], "save_change_text" );
      copy_fn ( fil_n_wid[fChg], "status_change_file_text" );
        fileStat[ fChg ] = fsActive;	/* file open and ready */
        Vuit_MU( "M", load_wid[fChg] );
        Vuit_MU( "U", no_ld_wid[fChg]);
        strcat(status_text, "Change ");
      }

    break;

    case fsRefresh:
      fileStat[ fChg ] = fsActive;
    break;

    case fsActive:
    case fsInactive:
	/* OK: take no action */
    break;

    default:
      printf("ERROR: fs bad switch %d\n", fileStat[ fChg ]);
    break;
  }

/******************** Process the command file ********************************/
  switch(fileStat[ fCmd ])
  {
    case fsCreate:
    case fsReplace:
      merge_flag = True;
      Vuit_MU( "M", "command_warning_dia");

      Vuit_MU( "U", rdy_wid[fCmd] );
      Vuit_MU( "U", load_wid[fCmd] );
      Vuit_MU( "M", no_ld_wid[fCmd]);

      if( send_pf_file_name( "command", XmTextGetString(  
			    widget_id( fil_n_wid[fCmd] ) ) ))
      {
        fileStat[ fCmd ] = fsActive;
        Vuit_MU( "U", load_wid[fCmd] );
        Vuit_MU( "M", no_ld_wid[fCmd]);
        strcat(status_text, "Command ");
      }

    break;

    case fsRefresh:
    case fsActive:
    case fsInactive:
	/* OK: take no action */
    break;

    default:
      printf("ERROR: fs bad switch %d\n", fileStat[ fCmd ]);
    break;
  }


  buildMainDisplay( merge_flag );

  change_lbl( "status_status_text", status_text );
  if(ipfdebug & (DB_TraceMask | DB_Filedlgrtn)) {
    curtime = gotime(curtime, "End Loading files (appy_files)");
  }
}



void change_lbl( char *widget_name, char *text_str )
/**********************************************************\
* Purpose: insert a new text string into a label widget
\**********************************************************/
{
  Widget id;
  if(( id = widget_id( widget_name )) != False) {
    XtVaSetValues(id, XmNvalue, text_str, NULL);
  }
}





void copy_fn( char *wid_name, char *wid_name_to )
/****************************************************************************\
* Purpose: copys contents of a text widget from one widget to another
*
\****************************************************************************/
{
  Widget id;
  char *text_str;

  if(( id = widget_id( wid_name ) ))
  {
    text_str = XmTextGetString( id );

    if(( id = widget_id( wid_name_to ) ))
    {
      XmTextSetString( id, text_str);
      left_justify_text(id);
    }

    XtFree( text_str );
  }
}


void copy_fn2( char *wid_name, char *wid_name_to )
/****************************************************************************\
* Purpose: copys contents of a text widget from one widget to another
*	   Slightly different from above because it cuts begining directory
*	   data from string.
\****************************************************************************/
{
  Widget id;
  char *text_str;
  char *pstr;
  char file_str[1024];

  if(( id = widget_id( wid_name ) ))
  {
    text_str = XmTextGetString( id );

    if(( id =  widget_id( wid_name_to ) ))
    {
#ifdef REMOVE_SAVE_FILEPATH
      /*** first remove the start path from the file ***/
      if(NULL != strrchr(text_str, ']')) {
        sprintf(file_str, 1+strrchr(text_str, ']'));
      } else if(NULL != strrchr(text_str, ':')) {
        sprintf(file_str, 1+strrchr(text_str, ':'));
      } else if(NULL != strrchr(text_str, '/')) {
        sprintf(file_str, 1+strrchr(text_str, '/'));
      } else {
        sprintf(file_str, text_str);
      }
#endif
       /** this leaves the whole path for the file for the save operation **/
        sprintf(file_str, text_str);
      if(NULL != (pstr = strrchr(file_str, ';'))) {
        *pstr = 0;
      }
      XmTextSetString( id, &file_str[0]);
    }

    XtFree( text_str );
  }
}



void name_change_file_status_set (Widget widget, int *tag,
					XmAnyCallbackStruct *callback_data)
/***********************************************************************\
* Function: change file status if user changes coord file name
*
* Called by: ANY keystroke in the coord file text
\***********************************************************************/
{
  button_number = *tag;
  if(ipfdebug & (DB_TraceMask | DB_Filedlgrtn)) {
	fprintf(ipfdbio, "name_change_file_status_set: tag %d\n",button_number);
  }

  if( fileStat[button_number] == fsActive ||
	fileStat[button_number] == fsReplace )
    fileStat[button_number] = fsReplace;

  else
    fileStat[button_number] = fsCreate;
}







/*##########################################################################*\
#	Collection of routines to CREATE new file
\*##########################################################################*/


void create_from_scratch(Widget widget,XtPointer tag, XmAnyCallbackStruct *cbs)
/**************************************************************************\
* Purpose: reset all the data structures for the gui and the server
*	   so that a new case can be created from scratch.
*
* Called by: "NEW" pushbutton on main menu
*		( and after the "OK" button on file_new_message_dialog )
\**************************************************************************/
{
  destroyAllGadgets	(0,0,0);  /* remove all items from screen */
  creategraphtbl	(0,0,0);  /* zeros graph_db (graphdata.c) */

  init_prtopt_dialog	();	  /* reset print options */
  psgetHtWdScale	();

  pscreatecoordtbl	();	  /* create (or zero) new pscor_db dmgr */
/*  psbuildGraphCoord	(0,0,0);   xfer pscor_db to graph_db (graphcor.c) */

  pscreate_plot_form	();	  /* create paper, border and label */
  
  pfinit_cb		(0,0,0);  /* send initialize to base data */
  set_filenames_userid	();
}





void set_filenames_userid()
/************************************************************************\
* Purpose: set the filenames to userid#.ext in the file save box
*	   clear the filenames in the file open box.
*	   set the filenames in the status box
*
* Called by: create_from_scratch
\************************************************************************/
{
#ifndef L_cuserid
#define L_cuserid 11
#endif
  char sid[L_cuserid];

  cuserid(sid);
  set_all_filenames(sid);
}


void set_all_filenames(char *fn_stub)
/**************************************************************************\
* Purpose: for CREATE-FROM-SCRATCH situlations
*
* Called by: above
\**************************************************************************/
{
#define MAXEXT 6
  char *fn_ext[MAXEXT] = { "sta", "chg", "bse", "net", "cor", "pti"};
  char *fn_save_box[MAXEXT] = {"save_stab_text"  ,
			       "save_change_text",
			       "save_base_text" ,
			       "save_net_text"  ,
			       "save_coord_text",
			       "save_pti_text" };
  static Widget fn_save_wid[MAXEXT];

  char *fn_open_box[MAXEXT] = { "file_select_dia_command_text",
			        "file_select_dia_change_text" ,
				"file_select_dia_base_text"   ,
				"file_select_dia_network_text",
				"file_select_dia_coord_text"  ,
				"file_select_dia_pti_text"   };
  static Widget fn_open_wid[MAXEXT];

  char fn[256];
  static int filetag = 0;
  static Boolean wids = False;
  Boolean fn_used;
  int i;

  fetch_file_windows();

  /* first time get the save text widgetids */
  if(wids == False)
  {
    for(i = MAXEXT-1; i >= 0; i--)   /* 654321 (Pti-Cor-Net-Bas-Chg-Cmd/Stab) */
    {
      fn_save_wid[i] = widget_id(fn_save_box[i]);	/* uses Cmd  */
      fn_open_wid[i] = widget_id(fn_open_box[i]);	/* uses Stab */
    }
    wids = True;				/* never exec this loop again */
  }
  
/*################ PREPARE THE SAVE TEXT ##############################*/
/* find a default SAVE filename that is not used */
  do {
    filetag++;
    fn_used = False; 
    for(i = MAXEXT-1; i >= 0; i--) /* 6-5-4-3-2-1 (Pti-Cor-Net-Bas-Chg-Stab) */
    {
      sprintf(fn, "%s_%d.%s", fn_stub, filetag, fn_ext[i]);
      if(file_exists(fn) == True) {
        fn_used = True;
        break;			/* start over with new filetag value */
      }
    }
  } while (fn_used != False);

/* set the filenames to the text boxes in the save dialog */
  for(i = MAXEXT-1; i >= 0; i--)    /* 6-5-4-3-2-1 (Pti-Cor-Net-Bas-Chg-Stab) */
  {
    sprintf(fn, "%s_%d.%s", fn_stub, filetag, fn_ext[i]);
    if(fn_save_wid[i] != FALSE) {
      XmTextFieldSetString(fn_save_wid[i], fn );
    }
  }

/*################ PREPARE THE OPEN FILE  TEXT ##############################*/
/* set the default filenames in the open dialog (except the command file) */
  for(i = MAXEXT; i >= 2; i--)	/* goes 6-5-4-3-2 (Pti-Cor-Net_Bas-Chg) */
  {
    /* sprintf(fn, "%s_%d.%s", fn_stub, filetag, fn_ext[i]); */
    /* if(fn_open_wid[i] != FALSE) */
    /*   XmTextFieldSetString(fn_open_wid[i], fn ); */

    Vuit_MU( "U", load_wid [ i ] );	/* remove all load labels */
    Vuit_MU( "U", rdy_wid  [ i ] );
    Vuit_MU( "U", no_ld_wid[ i ] );

#ifdef BAD_PF_INITIALIZE
    if(i == 2) { /* reset base data with empty file (initialize problem) */
      send_pf_file_name( "network_data", fn); 
    }
#endif
  }

  /* set the filename in the status fields */
  sprintf(fn, "%s_%d.%s", fn_stub, filetag, fn_ext[3]);
  XmTextFieldSetString( widget_id("status_coord_file_text"), fn );
  sprintf(fn, "%s_%d.%s", fn_stub, filetag, fn_ext[0]);
  XmTextFieldSetString( widget_id("status_change_file_text"), fn );
  sprintf(fn, "%s_%d.%s", fn_stub, filetag, fn_ext[2]);
  XmTextFieldSetString( widget_id("status_base_file_text"), fn );
  
}



void fetch_file_windows()
/**************************************************************************\
* Purpose: make sure that all the windows are managed for 
*	   1) file open and 2) save dialogs before trying to use the text 
*	   widgets that are part of the windows.
*
* Called by: set_all_filenameS
\**************************************************************************/
{
  static Boolean save_box_fetched = False;

  if(save_box_fetched == False)		/* done only once */
  {
    int now = 0;

    now = gotime(now, "");

    fetch_widget("save_file_dialog");
    now = gotime(now, "fetch save_file_dialog ");

    fetch_widget("write_protected_file_msg");
    fetch_widget("open_file_dialog");
    now = gotime(now, "fetch open_file_dialog ");

    save_box_fetched = True;
  }
}


Boolean file_exists(char *filename)
/**************************************************************************\
* Purpose: check to see if file exists
*	   Returns TRUE if file exists
* 
* YES this is the ONLY routine that checks existance w/o keeping the file ptr
*
* Called by: set_all_filenames, file_check_and_save_cb
\**************************************************************************/
{
  FILE *fp;
  Boolean stat;

  if((fp = fopen(filename, "r")) == NULL)
    stat = False;		/* it's a new file name */

  else
      stat = True;		/* old file writable */

  fclose(fp);

  return stat;
}


#ifdef WRT_CK
Boolean file_writeable( char *filename)
/**************************************************************************\
* Purpose: check to see file is writeable
*	   Returns TRUE if file is exists and is writeable
* 
* YES this is the ONLY routine that checks existance w/o keeping the file ptr
*
* Called by: file_check_and_save_cb
\**************************************************************************/
{
  FILE *fp;
  Boolean stat;

  if((fp = fopen(filename, "w")) == NULL)
    stat = IPC_NOT_OK;		/* something wrong - prob write protected */
  else
    stat = IPC_OK;		/* old file writable - everythings OK */


  fclose(fp);

  return stat;
}
#endif



int pf_file_exists(char *filename)
/**************************************************************************\
* Purpose: make powerflow check to see if file exists
*	   Returns TRUE if file exists
*	   Similiar to file_exists
* 
* Called by: file_check_and_save_cb
\**************************************************************************/
{
char *pfdataret = inbuf_array;
char *pfcmdstr  = outbuf_array;
  int rtncde;

  sprintf(pfcmdstr, "/GET_DATA, TYPE = FILE_EXISTS, FILE=%s\n%s\n",
		 filename, ENDOFMSG );

	/* powerflow returns	0 = does NOT exists, 1 = exists  */
	/* ipc_cb returns	1 = exists, 0 = NOT exist 		*/
	/* (Convert to correct value)				*/

  rtncde = ipc_synch_rw(pfdataret, pfcmdstr);

  return ( rtncde );
}

int pf_file_status(char *filename)
/**************************************************************************\
* Purpose: make powerflow check to see if file exists
*	   Returns TRUE if file exists
*	   Similiar to file_exists
* 
* Called by: file_check_and_save_cb
\**************************************************************************/
{
char *pfdataret = inbuf_array;
char *pfcmdstr  = outbuf_array;
  int rtncde;

  sprintf(pfcmdstr, "/GET_DATA, TYPE = FILE_WRT, FILE=%s\n%s\n",
		 filename, ENDOFMSG );

	/* powerflow returns   0 = writeable, 1 = write protected */

  rtncde = ipc_synch_rw(pfdataret, pfcmdstr);

  return ( rtncde );
}


/*##########################################################################*\
#	Collection of routines to SAVE files
\*##########################################################################*/

void file_save_proc(Widget widget,XtPointer tag, XmAnyCallbackStruct *cbs)
/**************************************************************************\
* Purpose: automatically save all the files listed on the file save
*	   dialog without putting up a screen.
*
* Called by: "SAVE" button on main menubar pulldown.
\**************************************************************************/
{
   int i;

   fetch_file_windows();
   for(i = 1; i <= 6; i++) {
     file_check_and_save_cb( 0, &i, 0);
   }
}


static int save_file_type = 0;

void file_save_select_cb( Widget widget, int *tag,
                             XmAnyCallbackStruct *callback_data)
/**************************************************************************\
* Purpose: move filename from file selection menu to save dialog field
*
* Called by: Any of 6 "Select" buttons on file_save dialog
\**************************************************************************/
{
  save_file_type = *tag;
  VUIT_Manage("file_save_select_dia");
}




void file_save_select_ok_cb( Widget widget, XtPointer tag,
                             XmFileSelectionBoxCallbackStruct *cbs)
/*************************************************************************\
* Purpose:  move highlighted selection to the correct text box
*
* Called by: "OK" button on XmFileSelectionDialog (for save)
\*************************************************************************/
{
  Widget id;
  char *text;

  get_file_save_text_wid(&id, save_file_type);
  if( id==FALSE) return;

  /* free the string from the previous use if it is still allocated */
  if(NULL != file_string) {
      XtFree(file_string);
  }
  XmStringGetLtoR( cbs->value, XmSTRING_DEFAULT_CHARSET, &text);
  XmTextFieldSetString(id, text);

}



void file_check_and_save_cb( Widget widget, int * tag,
	                     XmAnyCallbackStruct *callback_data)
/*****************************************************************************\
*   purpose:  Standardized save file routine.
*
*   Author: Bill Rogers    April 5, 1994
*	Called by: Any of five "SAVE" pushbuttons in save_file_dialog
*	file_save_proc (from SAVE pulldown button)
*
\*****************************************************************************/
{
  Widget id;
  char message[256];
  FILE *fp;
  XmString xmnewstring;
  Boolean ok_to_overwrite;
  Widget butt_wid;
  Widget wid;
  int ftag;
  int f_exists;
  int file_prot;
  static int not_destroyed = 1;


  ftag = (int)*tag;
  f_type = ftag;

  if(ipfdebug & (DB_TraceMask | DB_Filedlgrtn)) {
	fprintf(ipfdbio, "file_check_and_save_cb: f_type %d\n", f_type);
  }

  get_file_save_text_wid(&id, f_type);

  if( id==FALSE) return;

  /* free the string from the previous use if it is still allocated */
  if(NULL != file_string) {
      XtFree(file_string);
  }
  file_string = XmTextFieldGetString(id);

  if((int)strlen(file_string) >= 1)
  {
    switch( f_type )
    {
      case CHG_FILE:
      case BAS_FILE:
      case NET_FILE:
      case STAB_FILE:
        f_exists = pf_file_exists(file_string);
      break;

      case COR_FILE:
        f_exists = file_exists(file_string);
      break;
    }

    if( not_destroyed )
    {
      if( (wid = widget_id("write_protected_file_msg")) );
      {
        butt_wid = XmMessageBoxGetChild(wid, XmDIALOG_HELP_BUTTON);
        XtDestroyWidget(butt_wid);

        butt_wid = XmMessageBoxGetChild(wid, XmDIALOG_CANCEL_BUTTON);
        XtDestroyWidget(butt_wid);
        not_destroyed = 0;
      }
    }


    if( f_exists == OK )
    {
      file_save_cb( 0, 0, 0 );  /* save as new file */
    }
    else        /* consider overwrite */
    {
      Widget parent;
      int response;

      parent = widget_id("gui_main");
      sprintf(message, "%s exists, do you want to overwrite it?",
              file_string);
      response = askUser(parent, "OK", "Cancel", message);
      if(response == 1)
      {
	switch( f_type )
	{
	  case CHG_FILE:
	  case BAS_FILE:
	  case NET_FILE:
	  case STAB_FILE:
	    file_prot = pf_file_status(file_string);
	    if( file_prot == WRITEABLE )
	    {
	      file_save_cb( 0, 0, 0 );
	    }
	    else
	    {
	      extern XtAppContext app_context;
	      static int answer;

	      Vuit_MU( "M", "write_protected_file_msg"  );
              /* hold up - until user pushes a pushbutton */
	      while (answer == 0) XtAppProcessEvent(app_context, XtIMAll);
	    }
	  break;

	  case COR_FILE:
		/* checks are done in pswriteCoordFile */
		/* file_prot = file_writeable(file_string); */
            file_save_cb( 0, 0, 0 );
	  break;
	}
      }
    }
  } 
}


 
void get_file_save_text_wid( Widget *id, int ftype ) 
/**************************************************************************\
* Purpose: find the correct widget id - depends on which button was pushed.
*
* Called by: file_save_select_ok_cb
\**************************************************************************/
{ 
  switch( ftype )
  {
    case CHG_FILE:
      *id = widget_id("save_change_text");
    break;

    case BAS_FILE:
      *id = widget_id("save_base_text");
    break;

    case NET_FILE:
      *id = widget_id("save_net_text");
    break;

    case COR_FILE:
      *id = widget_id("save_coord_text");
    break;

    case STAB_FILE:
      *id = widget_id("save_stab_text");
    break;

    case PTI_FILE:
      *id = widget_id("save_pti_text");
    break;

    default:
      printf("Program error! Bad file num (%d) in call to get_file_save_text_wid\n", ftype );
      *id = FALSE;
  }
}


void file_save_cb( Widget widget, XtPointer tag,
	                     XmAnyCallbackStruct *callback_data)
/*****************************************************************************\
*   purpose:  Standardized save file routine.
*
*   Author: Bill Rogers    April 5, 1994
*	Called by: OVERWRITE option in save_file_error_box
*			- or -
*		   file_check_and_save_cb  (above)
*
\*****************************************************************************/
{
  Widget wid;
  int set;
  char type[25];
  char dialect[7];
  char rating[10];
  char size[4];
  static Boolean save_net_box = False;
  static Boolean save_stab_box = False;

char *pfdataret = inbuf_array;
char *pfcmdstr  = outbuf_array;

    switch( f_type )
    {
      case CHG_FILE:
        send_pf_save_file_name( "CHANGES", file_string );
      break;

      case BAS_FILE:
        send_pf_file_name("new_base", file_string );
      break;

      case NET_FILE:
        if(save_net_box == False) {
          int now;
          now = gotime(now, "pre fetch save_network ");
          fetch_widget("save_network_dialog");
          now = gotime(now, "fetch save_network ");
          save_net_box = True;
        }

	if((wid = widget_id("save_net_wscc_rb")) == FALSE) return;
	set = (int)XmToggleButtonGetState(wid);
	if(set) strcpy(dialect, "WSCC");

	if((wid = widget_id("save_net_wscc1_rb")) == FALSE) return;
	set = (int)XmToggleButtonGetState(wid);
	if(set) strcpy(dialect, "WSCC1");

	if((wid = widget_id("save_net_pti_rb")) == FALSE) return;
	set = (int)XmToggleButtonGetState(wid);
	if(set) strcpy(dialect, "PTI");

	if((wid = widget_id("save_net_bpa_rb")) == FALSE) return;
	set = (int)XmToggleButtonGetState(wid);
	if(set) strcpy(dialect, "BPA");

	if((wid = widget_id("save_net_extended_rb")) == FALSE) return;
	set = (int)XmToggleButtonGetState(wid);
	if(set) strcpy(rating, "EXTENDED");

	if((wid = widget_id("save_net_nominal_rb")) == FALSE) return;
	set = (int)XmToggleButtonGetState(wid);
	if(set) strcpy(rating, "NOMINAL");

	if((wid = widget_id("save_net_minimum_rb")) == FALSE) return;
	set = (int)XmToggleButtonGetState(wid);
	if(set) strcpy(rating, "MINIMUM");

	if((wid = widget_id("save_net_80_rb")) == FALSE) return;
	set = (int)XmToggleButtonGetState(wid);
	if(set) strcpy(size, "80");

	if((wid = widget_id("save_net_120_rb")) == FALSE) return;
	set = (int)XmToggleButtonGetState(wid);
	if(set) strcpy(size, "120");

	strcpy(pfcmdstr, "/SAVE_FILE, TYPE = NETWORK_DATA,\n");
	strcat(pfcmdstr, " FILE = ");
	if((wid = widget_id("save_net_text")) == FALSE) return;
	strcat(pfcmdstr, XmTextGetString(wid));
	strcat(pfcmdstr, ",\n DIALECT = ");
	strcat(pfcmdstr, dialect);
	strcat(pfcmdstr, ",\n RATINGS = ");
	strcat(pfcmdstr, rating);
	strcat(pfcmdstr, ",\n SIZE = ");
	strcat(pfcmdstr, size);
	strcat(pfcmdstr, "\n");
	strcat(pfcmdstr, ENDOFMSG);

        if(ipfdebug & DB_Filedlgrtn) {
	  fprintf(ipfdbio, "Save message = \n%s\n", pfcmdstr);
        }
	ipc_synch_rw(pfdataret, pfcmdstr);
      break;
  
      case COR_FILE:
	pswriteCoordFile( file_string );
      break;
  
      case STAB_FILE:
        if(save_stab_box == False) {
          int now;
          now = gotime(now, "pre fetch save_stability form");
          fetch_widget("stability_save_form"); 
          now = gotime(now, "fetched save_stability form");
          save_stab_box = True;
        }
	if((wid = widget_id("save_stability_ascii_rb")) == FALSE) return;
	set = (int)XmToggleButtonGetState(wid);
	if(set) strcpy(type, "WSCC_ASCII_STABILITY");

	if((wid = widget_id("save_stability_binary_rb")) == FALSE) return;
	set = (int)XmToggleButtonGetState(wid);
	if(set) strcpy(type, "WSCC_BINARY_STABILITY");

	strcpy(pfcmdstr, "/SAVE_FILE, TYPE = \n");
	strcat(pfcmdstr, type);
	strcat(pfcmdstr, "\n FILE = ");
	if((wid = widget_id("save_stab_text")) == FALSE) return;
	strcat(pfcmdstr, XmTextGetString(wid));
	strcat(pfcmdstr, "\n");
	strcat(pfcmdstr, ENDOFMSG);

        if(ipfdebug & DB_Filedlgrtn) {
          fprintf(ipfdbio, "Save message = \n%s\n", pfcmdstr);
        }
	ipc_synch_rw(pfdataret, pfcmdstr);

      break;

      case PTI_FILE:
	/* code not written yet */
      break;
    }

}


void save_xgui()
/**********************************************************************\
*
\**********************************************************************/
{
  Widget w, w1;
  XColor colr;
  Colormap wmap;
#define MAX_R 1000
#define LEN 132
  char recs[MAX_R][LEN];

  FILE *xgui_ptr;
  char new[120];
  char str[120];
  char xgui_file_name[120];
  char hex_chars[32];
  char *a, *b;
  long offset;
  int nrec=0;
  int wr=0;
  int len;
  char lbl_name[64];
  int pos2;

  xgui_file_name[0]='\0';

  xgui_ptr = get_xgui_ptr( xgui_file_name );
  if( xgui_ptr == 0 )
    return;

  while ((fgets( &recs[nrec][0], 132, xgui_ptr )) != NULL)
  {
    if( recs[nrec][0] != '!' &&		/* skip comment records */
	recs[nrec][0] != '#' )
    {
      b =  strstr( &recs[nrec][0], "XGUI*open_dia_" );
      if( b == 0 )
	b = strstr( &recs[nrec][0], "XGUI*file_select_dia_" );

      if( b )
      {
	pos2 = 45;			/* position to put file name */
        offset = b - &recs[nrec][0];
        {
          a  =  &recs[nrec][offset+5];
          b  =  strstr( &recs[nrec][offset+5], ".");
          len = b-a;

	  memset( &new[0], ' ', 119 );
          strncpy( new, &recs[nrec][offset+5], len );
          new[len] = '\0';

          lookup_and_get_field( new, str,  119 );

	  if( strlen( str ) )
	  {
            a  =  &recs[nrec][0];
            b  =  strchr( &recs[nrec][0], ':');
	    len = b-a;
	    memset (&recs[nrec][len], ' ', 120-len );
            recs[nrec][len] = ':';

	    if (pos2 < len ) pos2 = len + 2;	/* ensure no possible error */
						/* if name is too long */
	    strcat( str, "\n\0" );
            strcpy( &recs[nrec][pos2], str );
	  }
	  else
	  {
	    printf("%s NAME not saved in XGUI.\n", new );
	  }
        }

        if(nrec >= MAX_R )
        {
          printf("* ERR * from save_xgui (filedlgrtn.c)\n");
          printf("        Resource file XGUI(.DAT) has too many records.\n");
          return;
        }
      }
      else
      {
	b = strstr( &recs[nrec][0], "XGUI*kv_" );
        if( b )
        {
          offset = b - &recs[nrec][0];
	  a = strstr( &recs[nrec][0], "kv_" );
	  b = strstr( &recs[nrec][0], "."   );
          len = b-a;
          strncpy( lbl_name, a, len );
          lbl_name[len] = '\0';

          b = strchr( &recs[nrec][0], ':'   );
          len = b - &recs[nrec][0]+1;

	  memset( &new[0], ' ', 119 );
          strncpy( new, &recs[nrec][0], len );


          if( w = widget_id( lbl_name ) )
          {
            w1 = widget_id("drawwindow");
            wmap = DefaultColormapOfScreen( XtScreen( w1 )); /* ptr to clrmap */

            if( strstr( &recs[nrec][0], ".background" ) )
            {
              XtVaGetValues( w, XmNbackground, &colr.pixel, NULL );
            }
            else if( strstr( &recs[nrec][0], ".foreground" ) )
            {
              XtVaGetValues( w, XmNforeground, &colr.pixel, NULL );
            }

            XQueryColor( XtDisplay( w1 ), wmap, &colr );
            sprintf( hex_chars, "#%4.4x%4.4x%4.4x\n\0",
                     colr.red, colr.green, colr.blue );
            strcpy( &new[50], hex_chars );	/* in column 50 */
            strcpy( &recs[nrec][0], new );
          }
	} 
      }
    }
    nrec++;
  }


#ifdef VMS
  fclose( xgui_ptr );
  xgui_ptr = fopen( xgui_file_name, "w");
#else
  rewind( xgui_ptr );
#endif
  for( wr = 0; wr< nrec; wr++)
  {
    fprintf( xgui_ptr, "%s",  &recs[wr][0] );
  }
  printf("File saved as: (%s)\n", xgui_file_name );
  fclose( xgui_ptr );
}
