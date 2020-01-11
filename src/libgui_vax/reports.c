static char sccsid[]="@(#)reports.c	20.24 3/19/98";
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

/*############################################################################\
# file: reports.c	(Cut from pf_cb.c 12/21/93)
# PURPOSE: callback functions for reports from power flow
#
# author: Dan Clark
\############################################################################*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stddef.h>
#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/FileSB.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>
#include <signal.h>
#include <errno.h>

#ifndef VMS
#include <unistd.h>
#endif

#ifdef VMS
#include <processes.h>
#define fork vfork
#endif


#include "ipf_ipc.h"
#include "dmgr.h"
#include "graph_data.h"
#include "ipfdebug.h"
#include "em.h"

#define max(A,B) ((A) >= (B) ? (A) : (B))
#define MAXRECS 500
typedef struct xRpt {
  XmString xlns[MAXRECS];
  int xcnt;
} XRpt;


/*** NOTE: follow data must be in 1-to-1 corespondense with reports.u file */
#define BUS_INPUT_DATA		1
#define NAMED_BUS_BR_INPT	2
#define NAMED_BUS_BR_OUTP	3
#define AREA_INT_SUMMARY	4
#define TIE_LINE_SUMMARY	5
#define OVERLD_LINE_RPT		6
#define OVERLD_XFMR_RPT		7
#define OVER_UNDER_VOLTS	8
#define PHASE_SHIFT_SUM		9
#define NETWORK_CHANGES		10
#define BUS_VOLT_COMPARE	11
#define LINE_COMPARISION	12
#define USER_ANALYSIS		13
#define CMPR_INSTL_SHUNT	14
#define CMPR_BUS_GEN_MW		15
#define CMPR_BUS_GEN_MVAR	16
#define CMPR_BUS_LD_MW		17
#define CMPR_BUS_LD_MVAR	18
#define CMPR_BR_FLOW_MW		19
#define CMPR_BR_FLOW_MVA	20
#define CMPR_BR_LOSS_MV		21
#define CMPR_BR_LOSS_MVAR	22
#define CMPR_BRANCH_R		23
#define CMPR_BRANCH_X		24
#define CMPR_BRANCH_G		25
#define CMPR_BRANCH_B		26
#define CMPR_XFMR_TAP1		27
#define CMPR_XFMR_TAP2		28
#define PRT_AREA_SUM_RPT	29
#define BONE_PILE		30


/*##########################################################################*/

extern char * cstring_to_string	( XmString);
extern Widget widget_id		( char *);
extern int  ipc_synch_rw	( char *, char *);
extern int  createReportLine	( char *line, char *pcxrpt);
extern int  is_err_msg		( char *str);
extern int  forEachipfLine	( char *, int (*do_fnct)( char *, char *),
                                   char *);
extern void set_button_sensivity( char *widget_name, Boolean v1 );
extern int  setCurBus		( char * );
extern int  Vuit_MU		( char *, char * );
extern int getCurBus		( char ** );
extern Widget VUIT_Manage	( char *widget_name);
extern char *getValueByName	( char * );
extern char *getValueOfWidget	( Widget );

void add_all_selects_to_str	( char *pfcmdstr, int filter );
void addminmax			( char *, char * );
void addonvalues		( char *, char *, char * );
void addonfield			( char *text, char *pfcmd, char *pfcmdstr );
int  GetSelectedListItems	( char *widgetname, char *list[], int maxcnt);
void lookup_and_get_field	( char *, char *, int );
int  pfGetList			( char *list_type, char *text_name );
int  pfGetList_to_file		( char *file_name );
void setup_pfGetList		( char *dialog_title, char *pf_msg );
void set_dialog_title		( char *widget_name, char *title_str );
void strcat_output_file_name	( );
void writeReportFile		( char *file_string);
void rpt_str			( char *, int, int, int, char * );
void add_comparision_file_name	( );
void add_limit_to_str		( int limit );
void get_data_bus_list		( Widget alpha_lst_id );
int XmnewStringNCompare		( XmString s1, XmString s2, int cnt );
void pfGetFilterLists		( Widget w, XtPointer tag,
				  XmAnyCallbackStruct *reason);


em_init			/* special initialization for error message box */
			/* must be in every file using this.		*/

static char	outbuf_array[BUFSIZE];
static char	inbuf_array [BUFSIZE];
static char	out_buffer  [BUFSIZE];
 
char *pfcmdstr  = outbuf_array;
char *pfdataret = inbuf_array;

char viewPrintFlag;
char  addon[8];	/* is this an addon to the where clause */

#define MAXLINELIST 10000
#define MAXLISTSIZE 1000
#define MAXNAMECNT 10

#define RPTNONE	0
#define RPTMAX	1
#define RPTMIN	2
#define N_FLTR	0
#define FILTER	1
#define B_FLTR	2
#define NO_COMP	0
#define COMPAR	1

static int new_alpha_list = 0;


void pf_new_case(Widget w, XtPointer tag, XmAnyCallbackStruct *reason)
{
  new_alpha_list = 1;
}
void pf_update_alpha_list(Widget w, XtPointer tag, XmAnyCallbackStruct *reason)
{
  Widget wid;
    if((wid = widget_id("reports_bus_scroll_list")) != False) {
      get_data_bus_list(wid);
    }
}

void pf_deselect_list_items(char *wname)
{
  Widget wid;
    if( ( wid = widget_id( wname ) ) != NULL)  {
      XmListDeselectAllItems(wid);
    }
}
void pf_deselect_alpha_list(Widget w, XtPointer tag, XmAnyCallbackStruct *reason)
{
    pf_deselect_list_items("reports_bus_scroll_list" );
}
void pf_reset_filter_lists(Widget w, XtPointer tag, XmAnyCallbackStruct *reason)
/****************************************************************************\
* Purpose: resets selections on filter lists
*
*  Called by: select button
\****************************************************************************/
{
  Widget	wid;
  Boolean tb_state;

    pf_deselect_list_items("bus_branch_area_list"   );
  
    pf_deselect_list_items("bus_branch_owner_list"  );
  
    pf_deselect_list_items("bus_branch_zone_list"   );

    pf_deselect_list_items("bus_branch_basekv_list" );

    pf_deselect_list_items("bus_branch_type_list" );
}
void pf_reset_filter_lists_old(Widget w, XtPointer tag, XmAnyCallbackStruct *reason)
/****************************************************************************\
* Purpose: resets selections on filter lists
*
*  Called by: select button
\****************************************************************************/
{
  Widget	wid;
  Boolean tb_state;

    if( ( wid = widget_id( "bus_branch_area_list" ) ) != NULL)  {
      XmListDeselectAllItems(wid);
    }
  
    if( ( wid = widget_id( "bus_branch_owner_list" ) ) != NULL)  {
      XmListDeselectAllItems(wid);
    }
  
    if( ( wid = widget_id( "bus_branch_zone_list" ) ) != NULL)  {
      XmListDeselectAllItems(wid);
    }

    if( ( wid = widget_id( "bus_branch_basekv_list" ) ) != NULL)  {
      XmListDeselectAllItems(wid);
    }

    if( ( wid = widget_id( "bus_branch_type_list" ) ) != NULL)  {
      XmListDeselectAllItems(wid);
    }

    if( ( wid = widget_id( "reports_bus_scroll_list" ) ) != NULL)  {
      XmListDeselectAllItems(wid);
    }

}

void pfGetFilterLists(Widget w, XtPointer tag, XmAnyCallbackStruct *reason)
/****************************************************************************\
* Purpose: fills in the 5 "filter lists" widgets.
*
*  Gets the filter criteria lists from powerflow and places
*  them in the filter list scrolling windows
*
*  Author: John Rutis
*
*  Called by: Pull down menu for REPORTS.
\****************************************************************************/
{
  Widget	wid;
  Boolean tb_state;

    pfGetList("/GET_DATA, TYPE = AREA_LIST",  "bus_branch_area_list"   );
  
    pfGetList("/GET_DATA, TYPE = OWNER_LIST", "bus_branch_owner_list"  );
  
    pfGetList("/GET_DATA, TYPE = ZONE_LIST",  "bus_branch_zone_list"   );

    pfGetList("/GET_DATA, TYPE = BSEKV_LIST", "bus_branch_basekv_list" );

    pfGetList("/GET_DATA, TYPE = RECORD_LIST", "bus_branch_type_list" );

    if((wid = widget_id("reports_bus_list_autoload_tb")) != False) {
      tb_state = XmToggleButtonGetState(wid);
    } else {
      tb_state = True;
    }
    if(new_alpha_list == 1 || tb_state == True) {
      pf_update_alpha_list(w, tag, reason);
      new_alpha_list = 0;
    }
}


/**************************************************************************\
* function: createReportLine
* purpose:
* take as input a character string representing a line of data
* and create a pointer to a Motif String which is stored in a
* xRpt data structure.  The xRpt data structure is an array of
* pointers to motif strings with a line count for the total number
* of lines of data currently stored.
*
\**************************************************************************/
int createReportLine( char *line, char *pcxrpt)
{
  XRpt *xrpt = (XRpt *)pcxrpt;

  xrpt->xlns[xrpt->xcnt] = XmStringCreate((char *)line,
                                          XmSTRING_DEFAULT_CHARSET);
  xrpt->xcnt++;

  return(0);
}

int pfReport(char *instr)
/***********************************************************************\
* function: pfReport
* purpose:
* create the output report for a bus
* params: pass in a pointer to a character string representing
*  the powerflow id of the bus in with the syntax of nnnnnnnnvvvv
*  where n is the bus name and v is the base voltage.
*
\***********************************************************************/
{
  char *pfcmdstr = outbuf_array;
  char *pfdataret = inbuf_array;
  Widget          rpt_lst_id;

  char *pbuskey = 0;
  char buskey[256];
  static XRpt xrpt;
  static XRpt *pxrpt = &xrpt;
  static firsttime = TRUE;
  char *newname = "NewBus**500.";
  int rtncde;
  XmString *prevxlns;
  int prevxcnt;
  int cnt;

  if(getCurBus(&pbuskey) == -1)
  {
    pbuskey = newname;
    printf("ERROR - no current bus set to %s\n", pbuskey);
  }

  strncpy(buskey, &pbuskey[0], 18);
  buskey[19] = '\0';

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "pfReport: buskey=\"%s\"\n", buskey);
  }

  sprintf(pfcmdstr, "%s\n%s\"%s\"\n%s\n",
                    "/REPORTS,SELECT BUS_BR_OUTPUT",
                    " WHERE BUS = ", buskey,
                    ENDOFMSG);
  /* printf("pfcmdstr = %s\n", pfcmdstr); */

  rtncde = ipc_synch_rw(pfdataret, pfcmdstr);
  /* printf("pfdataret = %s\n", pfdataret); */
  if(TRUE != rtncde) {
    sprintf(errmsg, "Failed to get report BUS_BR_OUTPUT rtncde %d\n",
           rtncde);
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "cflow_launch_cb";
    err.type = WARNING;
    err.ident = sccsid;
    em_show(&err);
    return;
  }

  VUIT_Manage("ipf_report_list_dialog");

  if((rpt_lst_id = widget_id("ipf_reports_list")) == False) return;

#ifdef DONOTFREE
  /* remove the string free code for a list since motif
     cannot seem to handle de-allocating the list **/
  XtVaGetValues(rpt_lst_id,
                XmNitems, &prevxlns,
                XmNitemCount, &prevxcnt,
                NULL);
  if(prevxcnt > 0) {
    for(;prevxcnt > 0; prevxcnt--) {
      XmStringFree(prevxlns[prevxcnt-1]);
    }
  }
#endif
#ifdef OTHERWAY
  if(TRUE == firsttime) {
    pxrpt->xcnt = 0;
    firsttime = FALSE;
  }
  /* now clear memory allocated for strings from previous report */
  for(pxrpt->xcnt;pxrpt->xcnt > 0; pxrpt->xcnt--)
  { /* free previous allocated display lines */
    XmStringFree(pxrpt->xlns[pxrpt->xcnt -1]);
  }
#endif
  pxrpt->xcnt = 0;
  cnt = forEachipfLine(pfdataret, createReportLine, (char *)pxrpt);

  XtVaSetValues(rpt_lst_id,
                XmNitems, pxrpt->xlns,
                XmNitemCount, pxrpt->xcnt,
                NULL);
  /* printf("%d items added into reports selection box\n", pxrpt->xcnt ); */

  return(0);
} /* end pfReport */

int pfGetList	(char *list_type, char *text_widget_name )
/****************************************************************************\
*
*  Gets a specified list from powerflow and puts it in widget
*  rpt_lst_id.
*
*  Author: John Rutis
*
\****************************************************************************/
{
  char *pfcmdstr = outbuf_array;
  char *pfdataret = inbuf_array;

  char buskey[256];
  char *pbuskey;
  XRpt xrpt;
  XRpt *pxrpt = &xrpt;
  XmString *prevxlns;
  int prevxcnt;
  Widget rpt_lst_id;

  if( ( rpt_lst_id = widget_id( text_widget_name ) ) == NULL)  return;

#ifdef NOGOOD
  /*** first check if anything is on the list and free storage for it **/
  XtVaGetValues(rpt_lst_id,
                XmNitems, &prevxlns,
                XmNitemCount, &prevxcnt,
                NULL);
  if(prevxcnt > 0) {
    /* printf("Note: freeing previous %d lines from list\n", prevxcnt); */
    for(;prevxcnt > 0; prevxcnt--) {
      XmStringFree(prevxlns[prevxcnt-1]);
    }
  }
#endif

  /** get the data for this list from powerflow **/
  sprintf(pfcmdstr, "%s\n%s", list_type, ENDOFMSG);

  ipc_synch_rw(pfdataret, pfcmdstr);

  pxrpt->xcnt = 0;
  forEachipfLine(pfdataret, createReportLine, (char *)pxrpt);

  XtVaSetValues(rpt_lst_id, 
                XmNitems, pxrpt->xlns,  
                XmNitemCount, pxrpt->xcnt,
                NULL);

}


void get_data_bus_list( Widget alpha_lst_id )
/****************************************************************************\
* purpose:  gets all the busses for the alpha search list
*	    this routine needs to be cleanup up to use the new
*	    routine which goes through all the line data
*
* called by: pfAlphaList, pfGetFilterLists, 
*
* author: Dan Clark
\****************************************************************************/
#define MOREDATAMSG "*[MORE]"
{
  char buskey[256];

  extern int createRecPtrList();	/* in crtLnLst.c */
  char *lines[MAXLINELIST];
  XmString xlns[MAXLINELIST];
  XmString *prevxlns;
  int prevxcnt;
  int num_recs;
  int cnt;
  int xcnt;
  static int totrecs;
  int oldtotrecs;
  char lastbus[64];
  int lastbuscnt;
  int endofdata;
  int rtncde;
  Boolean reading_1st_blk;

  XtVaGetValues(alpha_lst_id, 
                XmNitems, &prevxlns,  
                XmNitemCount, &prevxcnt,
                NULL);
#ifdef NOFREE
  if(prevxcnt > 0) {
    /* printf("Note: freeing previous %d lines from list\n", prevxcnt); */
    /* VUIT_Unmanage("ipf_alpha_bus_list_dialog"); */
    for(;prevxcnt > 0; prevxcnt--) {
      XmStringFree(prevxlns[prevxcnt-1]);
    }
  }
#endif

  xcnt		  = 0;
  totrecs 	  = 0;
  lastbuscnt      = 0;	  /* set to zero in case no busses in case */
  endofdata	  = FALSE;  /* assume end of data UNLESS moredata msg */
  reading_1st_blk = TRUE;

  while(endofdata == FALSE)	 /** now get the rest of the data **/
  {
    endofdata = TRUE;  /* assume end of data UNLESS moredata msg */

    if( reading_1st_blk )
    {
      sprintf(pfcmdstr, "%s\n%s\n",
                    "/GET_DATA, TYPE = BUS_LIST WHERE ALL", ENDOFMSG);
      reading_1st_blk = FALSE;
    }
    else
    {
      sprintf(pfcmdstr, "%s %s%s%s\n%s\n",
                      "/GET_DATA, TYPE = BUS_LIST WHERE", 
                      "AFTER_BUS = \"", lastbus, "\"",
                      ENDOFMSG);
    }

    rtncde = ipc_synch_rw(pfdataret, pfcmdstr);

    if(FALSE == rtncde)
    {
      sprintf(errmsg, "Bad return code from IPF\ncommand: %s\n", pfcmdstr);
      err.line = EM_LINE;
      err.msg  = errmsg;
      err.link = "get_data_bus_list";
      err.type = WARNING;
      err.ident = sccsid;
      em_show(&err);
    }

    num_recs = createRecPtrList(lines, MAXLINELIST, pfdataret);

    oldtotrecs  = totrecs;
    totrecs += num_recs;

    if(num_recs > MAXLINELIST)
    {
      printf("ERROR: pfAlphaList: number of lines %d > MAXLINELIST %d\n",
             num_recs, MAXLINELIST);
      num_recs = MAXLINELIST;
    }

    if(totrecs > MAXLINELIST)
    {
      printf("ERROR: pfAlphaList: number of tot lines %d > MAXLINELIST %d\n",
             totrecs, MAXLINELIST);
      num_recs = totrecs - oldtotrecs;
    }

    if(num_recs > 0)
    {
      for(cnt = 0; cnt < num_recs; cnt++) /* create a db and stuff data in */
      {
        if(!strncmp(lines[cnt], ENDOFMSG, sizeof(ENDOFMSG))) /* ck end of msg*/
        {
          endofdata = TRUE; 
          break;
        }

        if(!strncmp(lines[cnt], MOREDATAMSG, sizeof(MOREDATAMSG)))
        {
          endofdata = FALSE; 
          break;
        }

        if(!strncmp(lines[cnt],"/",1))	 /*  skip control msg */
        {
          continue;
        }

        if(is_err_msg(lines[cnt]))	 /* do something with warning msg */
        {
          printf("alpha skip pf msgs:(%120.120s)\n", lines[cnt]);
          continue;
        }
        /* do something with report data */
        xlns[xcnt] = XmStringCreate( (char *)lines[cnt], 
                     XmSTRING_DEFAULT_CHARSET);
        xcnt++;
	lastbuscnt = cnt;
      }
      strncpy(lastbus, lines[lastbuscnt], 12);
      lastbus[12] = 0;
    }
  }
  XtVaSetValues(alpha_lst_id, 
                XmNitems, xlns,  
                XmNitemCount, xcnt,
                NULL);
}


int special_selection_action_cb( Widget w, XtPointer tag,
                                 XmListCallbackStruct *reason)
/*************************************************************************\
* Purpose: Manages and unmanages widgets depending on report selection.
*
* Called by: selecting a single item in "reports available" list.
\*************************************************************************/
{
  Widget wid;
  char filename[40];

  filename[0] = '\0';
  wid = widget_id("reports_output_file_name_text");
  set_button_sensivity( "reports_view_pb",  True );
  set_button_sensivity( "reports_save_pb",  True );
  set_button_sensivity( "append_report_pb", True );
  Vuit_MU( "U", "reports_compr_filename_label");
  Vuit_MU( "U", "reports_compare_file_name_text");
  Vuit_MU( "U", "rpt_bus_search_form");

  switch((int)reason->item_position)
  {
    case BUS_INPUT_DATA:
      Vuit_MU( "M", "bus_branch_filter_form" );
      Vuit_MU( "U", "reports_limits_form" );

    break;

    case NAMED_BUS_BR_INPT:
    case NAMED_BUS_BR_OUTP:
      Vuit_MU( "M", "rpt_bus_search_form");
      Vuit_MU( "M", "reports_limits_form" );
      Vuit_MU( "U", "bus_branch_filter_form" );
      Vuit_MU( "U", "reports_limits_form" );
    break;

    case OVERLD_LINE_RPT:
    case OVERLD_XFMR_RPT:
      Vuit_MU( "M", "reports_limits_form" );
      Vuit_MU( "M", "reports_loading_max_text" );  /* Note?: always manage */
      Vuit_MU( "M", "reports_limit_field_lbl" );   /* this before this.	  */
      Vuit_MU( "U", "PU_volt_relax_label" );
      Vuit_MU( "U", "reports_loading_min_text" );
      Vuit_MU( "M", "bus_branch_filter_form" );
    break;

    case OVER_UNDER_VOLTS:
      Vuit_MU( "M", "reports_limits_form" );
      Vuit_MU( "U", "reports_limit_field_lbl" );
      Vuit_MU( "M", "PU_volt_relax_label" );
      Vuit_MU( "M", "reports_loading_min_text" );
      Vuit_MU( "U", "reports_loading_max_text" );
      Vuit_MU( "M", "bus_branch_filter_form" );
    break;

    case BUS_VOLT_COMPARE:
      Vuit_MU( "M", "reports_limits_form" );
      Vuit_MU( "U", "reports_limit_field_lbl" );
      Vuit_MU( "M", "PU_volt_relax_label" );
      Vuit_MU( "U", "reports_loading_max_text" );
      Vuit_MU( "M", "reports_loading_min_text" );
      Vuit_MU( "M", "bus_branch_filter_form" );
      Vuit_MU( "M", "reports_file_select_dia");
      Vuit_MU( "M", "reports_compr_filename_label");
      Vuit_MU( "M", "reports_compare_file_name_text");
    break;

    case LINE_COMPARISION:
      Vuit_MU( "M", "reports_limits_form" );
      Vuit_MU( "M", "reports_limit_field_lbl" );
      Vuit_MU( "U", "PU_volt_relax_label" );
      Vuit_MU( "M", "reports_loading_max_text" );
      Vuit_MU( "U", "reports_loading_min_text" );
      Vuit_MU( "M", "bus_branch_filter_form" );
      Vuit_MU( "M", "reports_file_select_dia");
      Vuit_MU( "M", "reports_compr_filename_label");
      Vuit_MU( "M", "reports_compare_file_name_text");
    break;

    case AREA_INT_SUMMARY:
    case TIE_LINE_SUMMARY:
    case PHASE_SHIFT_SUM:
    case NETWORK_CHANGES:
      Vuit_MU( "M", "bus_branch_filter_form" );
      Vuit_MU( "U", "reports_limits_form" );
    break;

    case USER_ANALYSIS:		/* User Analysis */
    break;

    case CMPR_INSTL_SHUNT:	/* all compare reports */
    case CMPR_BUS_GEN_MW:
    case CMPR_BUS_GEN_MVAR:
    case CMPR_BUS_LD_MW:
    case CMPR_BUS_LD_MVAR:
    case CMPR_BR_FLOW_MW:
    case CMPR_BR_FLOW_MVA:
    case CMPR_BR_LOSS_MV:
    case CMPR_BR_LOSS_MVAR:
    case CMPR_BRANCH_R:
    case CMPR_BRANCH_X:
    case CMPR_BRANCH_G:
    case CMPR_BRANCH_B:
    case CMPR_XFMR_TAP1:
    case CMPR_XFMR_TAP2:
      Vuit_MU( "U", "reports_limits_form" );
      Vuit_MU( "U", "reports_limit_field_lbl" );
      Vuit_MU( "U", "PU_volt_relax_label" );
      Vuit_MU( "U", "reports_loading_max_text" );
      Vuit_MU( "U", "reports_loading_min_text" );
      Vuit_MU( "M", "bus_branch_filter_form" );
      Vuit_MU( "M", "reports_file_select_dia");
      Vuit_MU( "M", "reports_compr_filename_label");
      Vuit_MU( "M", "reports_compare_file_name_text");
    break;

    case PRT_AREA_SUM_RPT:
    case BONE_PILE:
      Vuit_MU( "M", "bus_branch_filter_form" );
      Vuit_MU( "U", "reports_limits_form" );
    break;

    default:
      filename[0] = '\0';
    break;
  }

  XtVaSetValues(wid, XmNvalue, filename, NULL);

  return(0);
}


int pfGetReport(Widget w, XtPointer tag, XmAnyCallbackStruct *reason)
/************************************************************************\
* Purpose: take appropiate action depending on selection, generate
*	   required dialog to send to pf.
*
* Called by: VIEW REPORT or APPEND REPORT pushbutton in select report dialog.
\************************************************************************/
{
  Widget	wid;
  char		*list_items[MAXLISTSIZE];
  char		*filename;
  char 		*pctag;
  int		i, cnt;
  int		*position, count;

  pctag = (char *)tag;		/* 'Overwrite' or 'Append" key passed from */
  viewPrintFlag = pctag[0];

  wid = widget_id("reports_select_scroll_list");
  if(NULL == wid ) return;

  if(0 == XmListGetSelectedPos(wid, &position, &count))
  {
    Vuit_MU( "M", "reports_not_selected_dia");
    return;
  }

  Vuit_MU( "M", "pf_report_dialog");

  switch((int)*position)
  {
    case BUS_INPUT_DATA:
     rpt_str("BUS_INPUT",      NO_COMP, FILTER, RPTNONE, "BUS INPUT DATA");
    break;

    case NAMED_BUS_BR_INPT:
     rpt_str("BUS_BR_INPUT",   NO_COMP, B_FLTR, RPTNONE, "BUS/BRANCH INPUT");
    break;

    case NAMED_BUS_BR_OUTP:
     rpt_str("BUS_BR_OUTPUT",  NO_COMP, B_FLTR, RPTNONE, "BUS/BRANCH OUTPUT");
    break;

    case OVERLD_LINE_RPT:
     rpt_str("OVERLOADED_LINES",NO_COMP, FILTER, RPTMAX, "OVERLOADED LINES");
    break;

    case OVERLD_XFMR_RPT:
     rpt_str("OVERLOADED_TXS", NO_COMP, FILTER, RPTMAX,"OVERLOADED TRANSFORMERS" );
    break;

    case OVER_UNDER_VOLTS:
     rpt_str("BUS_UVOV",     NO_COMP, FILTER, RPTMIN, "OVERVOLTAGE/UNDERVOLTAGE" );
    break;

    case BUS_VOLT_COMPARE:
     rpt_str("VOLTAGE_COMPARISION", COMPAR,FILTER, RPTMIN, "VOLTAGE COMPARISION" );
    break;

    case LINE_COMPARISION:
     rpt_str("LINE_COMPARISION",  COMPAR,  FILTER, RPTMAX,"LINE COMPARIONS");
    break;

    case AREA_INT_SUMMARY:
     rpt_str("AI_SUMMARY", NO_COMP, FILTER, RPTNONE,"AREA INTERCHANGE SUMMARY");
    break;

    case TIE_LINE_SUMMARY:
     rpt_str("TIE_LINE_SUMMARY", NO_COMP, FILTER, RPTNONE, "TIE LINE SUMMARY");
    break;

    case PHASE_SHIFT_SUM:
     rpt_str("PHASE_SHIFTER",     NO_COMP, N_FLTR, RPTNONE, "PHASE SHIFTER");
    break;

    case NETWORK_CHANGES:
     rpt_str("NETWORK_CHANGES",   NO_COMP, N_FLTR, RPTNONE, "NETWORK CHANGES");
    break;

    case USER_ANALYSIS:
     rpt_str("USER_ANALYSIS",     NO_COMP, N_FLTR, RPTNONE, "USER ANALYSIS");
    break;

    case CMPR_INSTL_SHUNT:
     rpt_str("CAPACITOR_COMP", COMPAR, FILTER, RPTNONE,"CAPACITOR COMPARISION");
    break;

    case CMPR_BUS_GEN_MW:
     rpt_str("BUS_COMP_PGEN",   COMPAR,  FILTER, RPTNONE, "BUS MW COMPARISION");
    break;

    case CMPR_BUS_GEN_MVAR:
     rpt_str("BUS_COMP_QGEN",   COMPAR,  FILTER, RPTNONE,
						"BUS MVAR COMPARISION");
    break;

    case CMPR_BUS_LD_MW:
     rpt_str("BUS_COMP_PLOAD",  COMPAR,  FILTER, RPTNONE,
						"BUS LOAD MW COMPARISION");
    break;

    case CMPR_BUS_LD_MVAR:
     rpt_str("BUS_COMP_QLOAD", COMPAR, FILTER, RPTNONE,
						"BUS LOAD MVAR COMPARISION");
    break;

    case CMPR_BR_FLOW_MW:
     rpt_str("BR_COMP_FLOW_MW", COMPAR, FILTER, RPTNONE,
						"BRANCH MW FLOW COMPARISION");
    break;

    case CMPR_BR_FLOW_MVA:
     rpt_str("BR_COMP_FLOW_MVAR", COMPAR, FILTER, RPTNONE,
						"BRANCH MVAR FLOW COMPARISION");
    break;

    case CMPR_BR_LOSS_MV:
     rpt_str("BR_COMP_LOSS_MW", COMPAR, FILTER, RPTNONE,
						"BRANCH MW LOSS COMPARIONS");
    break;

    case CMPR_BR_LOSS_MVAR:
     rpt_str("BR_COMP_LOSS_MVAR", COMPAR, FILTER, RPTNONE, 
						"BRANCH MVAR LOSS COMPARIONS");
    break;

    case CMPR_BRANCH_R:
     rpt_str("BR_COMP_R", COMPAR, FILTER, RPTNONE, "COMPARE BRANCH R (p.u.)");
    break;

    case CMPR_BRANCH_X:
     rpt_str("BR_COMP_X", COMPAR, FILTER, RPTNONE, "COMPARE BRANCH X (p.u.)");
    break;

    case CMPR_BRANCH_G:
     rpt_str("BR_COMP_G", COMPAR, FILTER, RPTNONE, "COMPARE BRANCH G (p.u.)");
    break;

    case CMPR_BRANCH_B:
     rpt_str("BR_COMP_B", COMPAR, FILTER, RPTNONE, "COMPARE BRANCH B (p.u.)");
    break;

    case CMPR_XFMR_TAP1:
     rpt_str("BR_COMP_TAP1", COMPAR, FILTER, RPTNONE,
						 "COMPARE TRANSFORMER TAP1");
    break;

    case CMPR_XFMR_TAP2:
     rpt_str("BR_COMP_TAP2", COMPAR, FILTER, RPTNONE,
						 "COMPARE TRANSFORMER TAP2");
    break;

    case PRT_AREA_SUM_RPT:
     rpt_str("AREA_SUMM", NO_COMP, FILTER, RPTNONE, "AREA SUMMARY");
    break;

    case BONE_PILE:
     rpt_str("NETWORK_DELETIONS", NO_COMP, N_FLTR, RPTNONE, "BONEPILE OUTPUT");
    break;

    default:
     printf("*** WARNING *** in pfGetReport (reports.c)\n" );
     printf("     Invalid report type parameter\n" );
     return;
  }

  pfGetList ( pfcmdstr, "pf_report_scroll_list" );

}


void rpt_str ( char *cmd, int compare, int filter, int limit, char *title )
/**************************************************************************\
* Purpose: greatly simplify the pfGetReport routine.
*
* Date:	4/22/94		Bill Rogers
*
* Params: cmd	- key word added to "/REPORTS,SELECT" to tell pf what
*		  reports are requested.
*	  cmp	- Compare file flag.  If set to COMPAR then built add
*		  "FILE = (name from reports_compare_file_name_text)" to string.
*	  fltr	- Filter flag.  If set to FILTER then, search the AREAS,
*		  ZONES, OWNERS, BASEKV and TYPE, and add any list items
*		  to string.  
*	  limit	- limit to include in the filter, such as MAX overload,
*		  MIN PU, etc.
*	  title	- Title to appear at top of the reports dialog.
*
* Called by: above
\**************************************************************************/
{
  char		out_file_name[31];

  sprintf( pfcmdstr, "/REPORTS,SELECT %s\n", cmd );

  if( compare  )   add_comparision_file_name ( );

  strcat_output_file_name();

  if( filter )   add_all_selects_to_str( pfcmdstr, filter );

  if( limit  )   add_limit_to_str( limit );

  set_dialog_title( "pf_report_dialog", title );
 
}


void add_comparision_file_name ( )
/**************************************************************************\
*
*
* Called by: above
\**************************************************************************/
{
  Widget wid;
  char *filename;

  filename = getValueByName( "reports_compare_file_name_text" );
  if(filename[0] != '\0' && filename[0] != ' ')
  {
    strcat(pfcmdstr, " FILE = ");
    strcat(pfcmdstr, filename);
    strcat(pfcmdstr, " \n");
  }
}


void strcat_output_file_name( )
/*************************************************************************\
* Purpose: 
*
* Called by: rpt_str
\*************************************************************************/
{
  char		out_file_name[31];

  if( viewPrintFlag == 'S' || viewPrintFlag == 'A' )
  {
  switch ( viewPrintFlag )
  {
    case 'A':
      strcat( pfcmdstr, "APPEND = " );
    break;

    case 'S':
    case 'V':
    default:
      strcat( pfcmdstr, "OVERWRITE = " );

  }

  lookup_and_get_field(  "reports_output_file_name_text", out_file_name, 30 );
  if( strlen( out_file_name ) )
  {
    strcat( pfcmdstr, out_file_name );
    strcat( pfcmdstr, "\n" );
  }
  else
  {
    strcat( pfcmdstr, " REPORTS.DAT\n" );
  }
  }

}


void add_all_selects_to_str( char *pfcmdstr, int filter )
/*************************************************************************\
*
*
\*************************************************************************/
{

  if( FILTER == filter )
  {
    strcpy( addon, " WHERE " );

    addonvalues("bus_branch_area_list"  , "AREAS" , pfcmdstr );
    addonvalues("bus_branch_zone_list"  , "ZONES" , pfcmdstr );
    addonvalues("bus_branch_owner_list" , "OWNERS", pfcmdstr );
    addonvalues("bus_branch_basekv_list", "BASEKV", pfcmdstr );
    addonvalues("bus_branch_type_list"  , "TYPE"  , pfcmdstr );
  }
  else if ( B_FLTR == filter )
  {
    strcpy( addon, "" );
    addonvalues("reports_bus_scroll_list"  , " WHERE BUS " , pfcmdstr );
  }
}


void addonvalues(char *listname, char *pfcmd, char *pfcmdstr )
/*************************************************************************\
*
\*************************************************************************/
{
  char	*list_items[MAXLISTSIZE];
  int cnt;
  int i;

  if(cnt = GetSelectedListItems(listname, list_items, MAXNAMECNT))
  {
    strcat(pfcmdstr, addon );
    
    strcat(pfcmdstr, pfcmd);
    strcat(pfcmdstr, " = \"");
    strcat(pfcmdstr, list_items[0]);
    if(cnt > 1)
    {
      for(i = 1; i < cnt; i++)
      {
        strcat(pfcmdstr, "\",\"");	/* add quote, comma, quote */
        strcat(pfcmdstr, list_items[i]);
      }
    }
    strcat( pfcmdstr, "\"");
    strcpy( addon, " AND\n");

    /* while(cnt--) XtFree(list_items[cnt]); */
  }
}


int GetSelectedListItems(char *widget_name, char *list[], int maxcnt)
/****************************************************************************\
*
*  Gets the report according to the filter criteria lists
*
*  Author: John Rutis
*
\****************************************************************************/
{
  Widget	wid;
  XmString	*motif_string;
  int		count, i;

  wid = widget_id(widget_name);
  XtVaGetValues(wid,
                XmNselectedItems, &motif_string,
                XmNselectedItemCount, &count,
                NULL);

  if(count > maxcnt) {
    sprintf(errmsg, "Warning: list items selected %d reset to max %d\n",
      count, maxcnt);
    printf("Warning: GetSelectedListItems: %s\n", errmsg);

    count = maxcnt;
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "GetSelectedListItems";
    err.type = WARNING;
    err.ident = sccsid;
    em_show(&err);
   }

  if(count)
  {
    for(i = 0; i < count; ++i)
    {
      list[i] = cstring_to_string((XmString)motif_string[i]);
    }
/**** don't do this 
    for(i = 0; i < count; ++i ) {
      XmStringFree(motif_string[i]);
    }
    XmStringFree(motif_string);
****/
  }
  return(count);
}




void add_limit_to_str( limit )
/**************************************************************************\
*
\**************************************************************************/
{
  if( RPTMAX == limit )
    addonfield("reports_loading_max_text",   "LOADING", pfcmdstr );

  else if( RPTMIN == limit )
    addonfield("reports_loading_min_text",   "LOADING", pfcmdstr );

  else {}
    /* do nothing */

}


void addonfield(char *text, char *pfcmd, char *pfcmdstr )
/****************************************************************************\
*
*  Gets the report according to the filter criteria lists
*
*  Author: John Rutis
*
\****************************************************************************/
{
  char		*cmin;
  double dmin;
  Widget wid;
  char newcmin[32];

  cmin = getValueByName( text );

  dmin = (double)atof(cmin);
  sprintf(newcmin, "%lf", dmin);


/*  if(dmin > 0) 
*  {
*/
    strcat(pfcmdstr, addon );

    strcat(pfcmdstr, pfcmd);
    strcat(pfcmdstr, " = \"");
    strcat(pfcmdstr, newcmin);
    strcat(pfcmdstr, "\"");
    strcat(pfcmdstr, "\n");

    strcpy( addon, " AND ");
/*  }
*/

}

void pfAlphaList_map_cb( Widget widget, XtPointer tag,
                  XmAnyCallbackStruct *callback_data)
{
  Widget wid;
  Widget alpha_lst_id;
  if((wid = widget_id("alpha_auto_reload_tb")) != False) {
      if(XmToggleButtonGetState(wid) != False) {
        if((alpha_lst_id = widget_id("ipf_alpha_bus_list")) == False) return;
        get_data_bus_list(alpha_lst_id);
      }
  }
}


void pfAlphaList( Widget widget, XtPointer tag,
                  XmAnyCallbackStruct *callback_data)
/****************************************************************************\
* purpose: 
* gets all the busses for the alpha search list
* this routine needs to be cleanup up to use the new
* routine which goes through all the line data
*
* author: Dan Clark
\****************************************************************************/
{
  Widget          alpha_lst_id;

  if((alpha_lst_id = widget_id("ipf_alpha_bus_list")) == False) return;

  get_data_bus_list(alpha_lst_id);

}



void ipf_alpha_srch_value_chg	( Widget widget, XtPointer tag,
	                          XmAnyCallbackStruct *cb_data)
/****************************************************************************\
*
* Called by: KEYSTROKE in reports_alpha_entry_text box
\****************************************************************************/
{
  String chgtxt;
  XmString xchgtxt;
  int lstpos;
  Widget alpha_lst_id;
  int *poslst;
  int poscnt;
  Boolean rtnmatch;
  XmStringTable itemlst;
  int itemtot;
  int curitem;
  String *lststr;
  int i;
  char text_widget_name[40];
  
  chgtxt = getValueOfWidget( widget );

#ifdef UN_CASE_SENSATIVE
  for(i = 0; i < strlen(chgtxt); i++)
  {
    chgtxt[i] = toupper(chgtxt[i]);
  }
#endif

  /* printf("alpha srch chg txt %s\n", chgtxt); */

  xchgtxt = XmStringCreate(chgtxt, XmSTRING_DEFAULT_CHARSET);

  if((alpha_lst_id = widget_id((char *)tag)) == False) return;

  XtVaGetValues(alpha_lst_id, 
                XmNitems, &itemlst,  
                XmNitemCount, &itemtot,  
                NULL);

  curitem = 0;

  while(curitem < itemtot)
  {
    if(XmnewStringNCompare(itemlst[curitem], xchgtxt, strlen(chgtxt)) >= 0)
    {
      /* printf("found substring item %d\n", curitem); */
      XmListSetPos(alpha_lst_id, curitem + 1);
      /* XmListSelectPos(alpha_lst_id, curitem + 1, False); */
      break;
    }
    curitem++;
  }
#ifdef REMOVE
  if(curitem >= itemtot)
  {
    XmListSetPos(alpha_lst_id, itemtot);
    XmListDeselectAllItems(alpha_lst_id);
  }
#endif
  XmStringFree(xchgtxt);
#ifdef EXACTMATCH
  rtnmatch = XmListGetMatchPos(alpha_lst_id, xchgtxt, &poslst, &poscnt);

  if(rtnmatch != NULL)
  {
    lstpos = poslst[0];
    /* printf("list pos returned from list item pos %d\n", lstpos); */
    XmListSetPos(alpha_lst_id, lstpos);
    XmListSelectPos(alpha_lst_id, lstpos);
  }
#endif
}





int XmnewStringNCompare(XmString s1, XmString s2, int cnt) 
/****************************************************************************\
*
* Called by: ipf_alpha_srch_value_chg	(above)
\****************************************************************************/
{
  char *ts1;
  char *ts2;
  int rtncde = False;
  
  if(!XmStringGetLtoR(s1, XmSTRING_DEFAULT_CHARSET, &ts1))
  {
    printf("ERROR - no string found for s1\n");
    return -1;
  };
  if(!XmStringGetLtoR(s2, XmSTRING_DEFAULT_CHARSET, &ts2))
  {
    printf("ERROR - no string found for s2\n"); 
    return -1;
  };
   
  /* printf(" cnt %d s1(%s) s2(%s)\n", cnt, ts1, ts2); */
  rtncde = strncmp(ts1, ts2, cnt);

  XtFree(ts1);
  XtFree(ts2);

  return rtncde;
}


void alpha_bus_list_select ( Widget widget, XtPointer tag,
                             XmListCallbackStruct *cb_data)
/****************************************************************************\
*
* Called by:  mouse click (selecting) a name in the alpha bus list of
*		the report_bus_alphasearch_form
\****************************************************************************/
{
  char *ts1;
  char buskey[30];

#ifdef DEBUG
  printf("Selected data: "); 
  printf(" item_position %d ", cb_data->item_position);

  if(!XmStringGetLtoR(cb_data->item, XmSTRING_DEFAULT_CHARSET, &ts1))
  {
    printf("ERROR - no string found for item\n");
  };

  printf(" item_string (%s) ", ts1);
  sprintf(buskey, "%s", ts1);
  printf(" buskey (%s) ", buskey);
  printf("\n");
#endif

  if(!XmStringGetLtoR(cb_data->item, XmSTRING_DEFAULT_CHARSET, &ts1))
  {
    printf("ERROR - no string found for item\n");
    ts1 = "                     ";
  };

  sprintf(buskey,"%s", ts1);
  setCurBus(buskey);
   
  XtFree(ts1);
}



void set_dialog_title	(char *widget_name, char *title_str)
/****************************************************************************\
*
*  Sets the title of the named dialog
*
*  Author: John Rutis
*
\****************************************************************************/
{
  XtVaSetValues( widget_id(widget_name),
                 XmNdialogTitle,
                 XmStringCreateSimple(title_str),
                 NULL );
}



void setup_pfGetList( char *dialog_title, char *pfcmdstr )
/************************************************************************\
* Called by: 	( NOT USED )
\************************************************************************/
{

      set_dialog_title( "pf_report_dialog", dialog_title );
      pfGetList       ( pfcmdstr, "pf_report_scroll_list" );
}
