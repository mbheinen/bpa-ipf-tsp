static char sccsid[]="@(#)pf_cb.c	20.100 7/6/98";
/*****************************************************************************\
* file: pf_cb.c
* author: Dan Clark
* purpose: callback functions for interfacing with power flow
*
\*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stddef.h>
#include <assert.h>
#include <math.h>
#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/FileSB.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <signal.h>
#include <errno.h>

#include "ipf_ipc.h"
#include "dmgr.h"
#include "graph_data.h"
#include "em.h"
#include "base_out.h"
#include "EdgeG.h"
#include "base_data.h"
#include "coord_data.h"
#include "ipfdebug.h"
#include "define.h"

#define max(A,B) ((A) >= (B) ? (A) : (B))
#define MINSOCKET 1024
#define MAXSOCKET 4096
#define MAXRECS 500
#define MAXARG 10

#define SHOW_NAME_ABBR 1L<<0
#define SHOW_NAME_FULL_NOKV 1L<<1
#define SHOW_NAME_FULL 1L<<2

extern double zoom_factor;      /* from (toolbox.c) */

extern int HashLookup(char *name, Widget *id);
extern int new_process(char *servername,
                       int argc, char *server_argv[], int nenv, char *envp[]);
extern double atof_cnt_zero(char * basekv, int length);
extern int atoi_cnt( char *str, int size);
extern int ipfstate_get(int *ostates);
extern void pscreateGraphCorEdge(GraphElement *, GraphElement *, char *,
	                            GraphClass, int, GraphElement **);
extern Widget lookup_pct_wid(double pct);
extern Widget lookup_kv_wid(double pct);
extern double setGraphKv(GraphElement *pedge);
extern double atof_cnt( char *str, int size );
extern XtAppContext app_context;
extern void lookup_and_get_field( char *, char *, int );
extern void lookup_and_fill_field( char *, char *, int, int );
extern char * get_file_name_from( XmFileSelectionBoxCallbackStruct *);
extern char * cstring_to_string	( XmString);
extern int    getCurBus		( char **);
extern Widget widget_id		( char *);
extern int ipc_synch_rw	( char *, char *);
extern char   *ipcgetnodename	( );
extern int    manageEdge	( GraphElement *);
extern int    manageVertex	( GraphElement *);
extern Widget VUIT_Manage	( char *widget_name );
extern void VUIT_Unmanage	( char *widget_name );
extern void read_overload_boxes	( Widget wid, XtPointer tag, XtPointer reason );
extern void set_button_sensivity( char *widget_name, Boolean v1 );
extern void Vuit_MU		( char *, char * );
extern void fetch_print_windows ();
extern void get_branch_solution_opt	(int *state);
extern void get_branch_detail_opt	(int *state);
extern void get_branch_other_opt	(int *state);
extern void get_bus_name_solution_opt	(int *state);
extern void get_bus_solution_opt	(int *state);
extern void get_bus_generation_opt	(int *state);
extern int pswriteCoordFile	( char *coord_output_file_name);
extern int  atoi_cnt_zero	( char *str, int size );
extern void clip_trailing_blanks( char *str );
extern void update_location_of_ge_and_attachments
				( GraphElement *object, int xdelta, int ydelta);
extern char *getValueByName	( char * );
extern void set_button_state	( char *name, Boolean state );
extern int uscanc		( char *,  char *wrd[], int *n,
                                  char *lmt1, char *lmt2);
extern void enable_pushbutton	( char *);
extern void disable_pushbutton	( char *);
extern void position_gadget_wrt ( GraphElement *vertex );
extern GraphElement *createGraphBusXYXY( char *name, int x, int y,
                         int xn, int yn, char *gen_a, char *cap_a,
			 int m_flg, int x_flg, int pf_flg,
			 int g_flg, int r_flg );
extern int pscoord_to_MOTIF_x	( char *x, int col, char *bus_name );
extern int pscoord_to_MOTIF_y	( char *y, int col, char *bus_name );
extern DB_STAT findGraphEdgebyName2( char *namebase1, char *namebase2,
                                      GraphElement **pedge );
extern void get_str_box_pixel_size( char *str, int *wd, int *ht );
extern int update_bus			( char *, char * );



void refresh_solution_data(Widget w, XtPointer adata, XtPointer cdata);
void toggle_button_display(Widget w, XtPointer adata, XtPointer cdata);
int  writeBusLabel		( GraphElement *, GraphElement *,	
				  GraphLink *, char *);
int  updateOutput		( char *, char * );
int  checkSolnEdge		( GraphElement *, pfOutRecord *);
int  forEachipfLine		( char *, int (*do_fnct)( char *, char *),
				   char *);
int  isline			(pfOutRecord *);
int  isbus			(pfOutRecord *);
void addonvalues		(char *, char *, char *, int *);
void refreshBusImage		(pfOutRecord *);
void update_edge		(pfOutRecord *);
int  istransformer		(GraphElement *);
void stringPart			(char longString[], char returnString[],
			           int  start_col, int  end_col);
int setCurBus			(char *);
int GetSelectedListItems	(char *widgetname, char *list[], int maxcnt);

void distribute_rays		( int num, double ray_rad[50], int ray_fix[50]);
double radiansBetweenPts	( int x, int y, int x2, int y2 );
int  print_plot			( Widget w, int *tag,
			XmFileSelectionBoxCallbackStruct *callback_data);
void set_overload_colors	( Widget wid, XtPointer tag, XtPointer release);
void set_kv_colors		( Widget wid, XtPointer tag, XtPointer release);
void set_solution_status	( int status );
void refresh_comments		();
int  read_prcsn_radiobuttons	();
int  read_bus_prcsn_radiobuttons();
void remove_prefix_from_filename( char *fname );
void pfget_solution_params	( Widget wid, XtPointer tag, XtPointer cdata );
void set_toggle_button		( char *wid_name, char *onoff );
void process_update_data	( char *pfdataret );
int  refreshGraph 		( );
int  get_pf_connect_data	( GraphElement *pge, char *data );
void explodeBus			( GraphElement *pge );
void explodeBranches		( GraphElement *pge );
int  checkLines			( char *line, char *cpcon);
void createGraphBranch		( GraphElement *v1, GraphElement *v2 );
char *busNameStr                ( GraphElement *ge );
void set_bus_icon		( Widget w, XtPointer tag, XtPointer rsn );
void manage_gen_and_reac_symbols(GraphElement *pvertex, pfOutRecord *psolnData);
void createGraphGenReac( GraphElement **pvtx, GraphElement *pgraphbusrec,
	 char *gen_a, GraphClass type, int dist, char *namstr, int m_flg );


em_init			/* special initialization for error message box */
			/* must be in every file using this.		*/

#define MAX_LOAD (double)90.0
#define MAX_LOAD_OVER (double)100.0

#define MAX_NONE '0'
#define MAX_NEAREND '1'
#define MAX_FAREND '2'

static char	outbuf_array[BUFSIZE];
static char	inbuf_array [BUFSIZE];
static char	out_buffer  [BUFSIZE];
static int sol_opt;
static int name_opt;

int bus_icon_key = -1;
int icon_bus_shape	= CIRCLE;
int icon_bus_style	= HOLLOW;
int icon_bus_width	= MED_RADIUS;
int icon_bus_height	= MED_RADIUS;



int is_err_msg(char *str)
/***************************************************************************\
*	Purpose: standard check and returns T/F if pf returned an error msg
*
*	Author:Bill Rogers	Sept 22, 1992
*
*	Called by: bussect, ipc_cb, linetap, linezcalc
*		   forEachipfLine, appendOutstr, pfAlphaList
*
\***************************************************************************/
{
    if (strncmp ( str, " *INFORMATIVE" , 13) == 0 ) {
      return 1;
    } else if( strncmp ( str, " *** WARNING"  , 12) == 0 ) {
      return 2;
    } else if( strncmp ( str, " *** ERROR"    , 10) == 0 ) {
      return 4;
    } else if( strncmp ( str, " *** FATAL"    , 10) == 0 ) {
      return 8;
    } else if( strncmp ( str, " *** ABORT"    , 10) == 0 ) {
      return 16;
    } else {
      return FALSE;
    }
}


int forEachipfLine(char *instr, int (*do_fnct)( char *line, char *userdata ),
			     char *userdata)
/*****************************************************************************
* PURPOSE: 
*   provide a utility routine to loop through all the output 
*   returned from powerflow.
*   Calls a subfunction to process the data for each line of actual data
*   ignores any error messages 
*   and ignores command lines returned
*
* AUTHOR: Dan Clark	1992
* 
****************************************************************************/
{
  extern int createRecPtrList();
  char *lines[MAXRECS];
  int cnt;
  int rtn_cde;
  int num_recs;

  num_recs = createRecPtrList(lines, MAXRECS, instr);

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "forEachipfLine: num_recs=%d\n", num_recs);
  }

  if(num_recs > 0) 
  {
    for(cnt = 0; cnt < num_recs; cnt++)
    {
      /* check for the logical end of the message */
      if( !strncmp(lines[cnt], ENDOFMSG, sizeof(ENDOFMSG))) { break; }

      if( !strncmp(lines[cnt],"/",1) ) { continue; } /* Skip control msg */

      if ( is_err_msg ( lines[cnt] ) ) { continue; } /* Skip warning msg */

      /* do something with output data - call designated processing function */
      rtn_cde = (*do_fnct)( lines[cnt], userdata);

    }

  }
  return num_recs;
}


int pfinit_cb(Widget wid, XtPointer tag, XmAnyCallbackStruct *cbs)
/*********************************************************************\
*       Purpose:  send message to ipf (powerflow) to initialize memory.
*       of the server process.
*
*       Author: Dan Clark       Dec, 1991       Modified: Dec 17, 1991
*
*       Called by: "PF Initialize" pb on main menu "Process"
*  input: no input data is used by this routine
*  output: return the status code passed back from powerflow 
*
\*********************************************************************/
{
  char	*outbuf = outbuf_array;
  char  *str_rtn = inbuf_array;
  int rtncde;

  sprintf(outbuf, "/initialize\n%s\n",ENDOFMSG);

  rtncde = ipc_synch_rw(str_rtn, outbuf);

  return rtncde;
}

/* ----------------------------------------------------------
*  purpose: check powerflow for data (bus or branch)
*  return a true/false answer
*  do not return any of the actual data
*  author: Dan Clark
*  ----------------------------------------------------------
*/

int pfcheck_cb(char *instr)
{
  char *pfcmdstr = outbuf_array;
  char *pfdataret = inbuf_array;
  int rtncde;

  char *lines[MAXRECS];
  int num_recs;
  int cnt;

  /* append EODOFMSG to instr and call ipf */
  sprintf(pfcmdstr, "/GET_DATA, TYPE = INPUT\n%s\n%s\n",
     instr, ENDOFMSG);

  rtncde = ipc_synch_rw(pfdataret, pfcmdstr);

  return(rtncde);

}

int pfchanges_cb( Widget widget, XtPointer tag,
			XmFileSelectionBoxCallbackStruct *callback_data )
/*********************************************************************\
* Purpose: 
*	   Send changes to PF
*
*       Author: Dan Clark       Dec, 1991
*
\*********************************************************************/
{
  char  *outbuf = outbuf_array;
  char  *str_rtn = inbuf_array;
  int rtncde;


  sprintf(outbuf, "/changes\n%s\n%s\n", tag, ENDOFMSG);

  rtncde = ipc_synch_rw( str_rtn, outbuf );

  return rtncde;
}

static int xline, yline;
static GraphElement *pline = NULL;
#define PF_SEPCHARS "\n"
#define PF_LINE_TYPE 'L'
#define PF_TRANSFORMER_TYPE 'T'
#define PF_EQUIVALENT_TYPE 'E'
#define PF_REACTOR_TYPE 'R'
#define MAX_LINES_PER_BUS 30
static XmString x_str[MAX_LINES_PER_BUS+3];


pf_line_cb(Widget w, XtPointer cbd, XmAnyCallbackStruct cbs)
{
  char *linedel;
  int btn_num = (int)cbd;

  /* printf("pf_line_cb: button pressed %d\n", btn_num); */
  if(x_str[btn_num+2] != NULL) {
    if(XmStringGetLtoR(x_str[btn_num+2], XmSTRING_DEFAULT_CHARSET, &linedel)) {
      pfchanges_cb(0, linedel, 0);
      if(pline != NULL) {
        setDashAllSubEdges(pline, True);
      }
    };
  };
}


pf_line_selection(char *rtnbuf)
{
  char *psubstr;
  PFBASERECORD *r;
  char *lines[MAX_LINES_PER_BUS];
  int lcnt;
  static Widget wid_menu = NULL;
  static int popcnt = 0;
  int i;
  int ckt_id;
  Widget pwid;
  char allstr[4096];
  char substr[256];
  char name[256];
  XmButtonType x_btype[MAX_LINES_PER_BUS+3];
  int x_bcnt;
  XmString x_title;
  Arg al[64];
  int ac;
  int set_all;

  /* clear the pointers to lines from previous runs */
  for(i = 0; i < MAX_LINES_PER_BUS; i++) {
    lines[i] = NULL;
  }
  /** create an array of strings containing powerflow line data **/
  lcnt = 0;
  psubstr = strtok(rtnbuf,SEPCHARS);
  while(psubstr != NULL) {
      r = (PFBASERECORD *)psubstr;
      if(PF_LINE_TYPE	     == r->type[0] ||
         PF_TRANSFORMER_TYPE == r->type[0] ||
         PF_EQUIVALENT_TYPE  == r->type[0] ||
         PF_REACTOR_TYPE     == r->type[0]) 
      {
         ckt_id = atoi_cnt_zero(&r->branch.circuit[0],1);

         if(ckt_id >=0 && ckt_id <= MAX_LINES_PER_BUS) {
           lines[ckt_id] = psubstr;
           lcnt++;
         }
      }
      psubstr = strtok(NULL,SEPCHARS);
  }

  if(wid_menu != NULL) {
    XtUnmanageChild(wid_menu);
    XtDestroyWidget(wid_menu);
  }
  /* clear the prevous xstrings used for titles */
  for(i = 0; i < MAX_LINES_PER_BUS; i++) {
    if(x_str[i] != NULL) {
      XmStringFree(x_str[i]);
      x_str[i] = NULL;
    }
  }
  /* create a title string  and separator */
  x_str[1] = XmStringCreateLtoR("Separator", XmSTRING_DEFAULT_CHARSET);
  x_btype[1] = XmSEPARATOR;
  x_bcnt = 2;
  set_all = False;
  for(i = 0; i < MAX_LINES_PER_BUS; i++) {
    if(lines[i] != NULL) {
      if(set_all == False) {
        sprintf(substr, "%32.32s ", lines[i]);
        substr[2] = 'D';
        substr[3] = ' ';
        substr[4] = ' ';
        substr[5] = ' ';
        substr[31] = '*';
        substr[32] = '0';
        x_str[x_bcnt] =  XmStringCreateLtoR(substr, XmSTRING_DEFAULT_CHARSET);
        x_btype[x_bcnt] = XmPUSHBUTTON;
        x_bcnt++;
        set_all = True;
      }
      sprintf(substr, "%32.32s ", lines[i]);
      substr[2] = 'D';
      substr[32] = '0';
      x_str[x_bcnt] =  XmStringCreateLtoR(substr, XmSTRING_DEFAULT_CHARSET);
      x_btype[x_bcnt] = XmPUSHBUTTON;
      x_bcnt++;
    }
  }
  if(x_bcnt < 3)  {
    x_str[0] =  XmStringCreateLtoR("All Circuits Outaged/Deleted", 
                                  XmSTRING_DEFAULT_CHARSET);
  } else {
    x_str[0] =  XmStringCreateLtoR("Pick Circuit to Outage/Delete", 
                                  XmSTRING_DEFAULT_CHARSET);
  }
  x_btype[0] = XmTITLE;
  if(x_bcnt == 4)  {
    XmString xtmp;
    xtmp = x_str[2];
    x_str[2] = x_str[3];
    x_str[3] = xtmp;
    x_bcnt = 3;
  }
   /* destroy previous popup menu */
    pwid = widget_id("gui_main");
    assert(pwid != NULL);
    popcnt++;
    sprintf(name, "Line Pop %d", popcnt);
    ac = 0;
    XtSetArg(al[ac], XmNsimpleCallback, (XtCallbackProc)pf_line_cb); ac++;
    XtSetArg(al[ac], XmNx, (Position)xline); ac++;
    XtSetArg(al[ac], XmNy, (Position)yline); ac++;
    XtSetArg(al[ac], XmNbuttonCount, x_bcnt); ac++;
    XtSetArg(al[ac], XmNbuttons, x_str); ac++;
    XtSetArg(al[ac], XmNbuttonType, x_btype); ac++;
    wid_menu = XmCreateSimplePopupMenu(pwid, name, al, ac);

    assert(wid_menu);
    XtManageChild(wid_menu);

}


int pf_get_lines(GraphElement *pge, XEvent *pevent)
{
  char *pfcmdstr = outbuf_array;
  char *pfdataret = inbuf_array;
  char newkey[256];
  int rtncde;
  int xorg, yorg;

  char *lines[MAXRECS];
  int num_recs;
  int cnt;

  xline = pevent->xbutton.x_root;
  yline = pevent->xbutton.y_root;
  pline = pge;

  if( GraphClassEdgeComplexSection == pge->class )
  {
    sprintf(newkey, "L     %12.12s %12.12s*0", 
            pge->vertex_id, pge->farvertex_id);
  } else {
    printf("pf_get_lines: line data not found \n");
    printGraphElement(pge);
    return -1;
  }

  /* append EODOFMSG to instr and call ipf */
  sprintf(pfcmdstr, "/GET_DATA, TYPE = INPUT\n%s\n%s\n",
     newkey, ENDOFMSG);

  rtncde = ipc_synch_rw(pfdataret, pfcmdstr);

  /* now index through all lines */
  pf_line_selection(pfdataret);

  return(rtncde);

}

static int pid_cflow = -1;	/* process id of cflow */

void cflow_launch_cb( Widget widget, XtPointer tag,
		      XmFileSelectionBoxCallbackStruct *callback_data)
/*********************************************************************
*       Author: Dan Clark       Dec, 1991       Modified: Dec 17, 1991
*
* function: cflow_launch_cb
* purpose: read the socket address and file name from the screen
*  and create a child process to execute the cflow program.
*
*********************************************************************/
{
  static char socketid_cmd[] = "-socketid";
  char  *outbuf = outbuf_array;
  char  *str_rtn = inbuf_array;

  char file_name[81];            /* name of input file */

  XmString compound_string;      /* x compound string of file name */
  XmStringContext context;       /* x context of compound string */
  char          *text;           /* text pointer to file name */
  XmStringCharSet       charset; /* x return char set (unused) */
  XmStringDirection     dir;     /* x return directory of file (unused) */
  Boolean       separator;       /* x separator for file string (unused) */


  char *psubstr;
  char *pfilestr;

  char *cflow_argv[MAXARG];
  char **tmp_envp;
  char *ptmp_envp = NULL;
  char *nodename = NULL;
  int argc;
  int nenvp;
#define MAXNAME 128
  char socketid[MAXNAME];
  char *socket_str;
  int socketobj;
  Widget wid;
  unsigned int wait_time = 3;
  int rtncde;
  char cmd_args[256] = " ";

/*
  if((wid = widget_id("cflow_sel_text")) ==False) return;
  nodename = (char *)ipcgetnodename();
  socket_str = XmTextGetString(wid);
  socketobj = atoi(socket_str);
  if(socketobj < MINSOCKET || socketobj > MAXSOCKET)
  {
    sprintf(errmsg, "Socketid %d invalid must be between %d and %d",
            socketobj, MINSOCKET, MAXSOCKET);
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "cflow_launch_cb";
    err.type = WARNING;
    err.ident = sccsid;
    em_show(&err);
    return;
  }

  sprintf(socketid, "%d", socketobj);
*/
  /* Print the string */
  /* printf("Control File Name: %s\n", tag); */

  /*** get the filename from the file selection widget ***/
  /* Get the compound string containing the file name selected */
  if((wid = widget_id("cflow_file_selection_box")) ==FALSE) return;
  XtVaGetValues(wid, XmNdirSpec, &compound_string, NULL); 
  /* Extract the file name from the compound string */
  XmStringInitContext(&context, compound_string);
  XmStringGetNextSegment(context, &text, &charset, &dir, &separator);

  /* Print the string */
  /* printf("Control File Name: %s\n", text); */
  sprintf(file_name, "%s", text);

/*

  argc = 0;
  nenvp = 0;
  cflow_argv[argc++] = file_name;
  cflow_argv[argc++] = socketid_cmd;
  cflow_argv[argc++] = socketid;
  cflow_argv[argc++] = '\0';
  tmp_envp = &ptmp_envp;

  pid_cflow= new_process(file_name, argc, cflow_argv, nenvp, tmp_envp);

  printf("client process continues child pid = %d\n", pid_cflow);

  if(wait_time > 0) {
    sprintf(errmsg, "Waiting %d secs before attempting to connect to cflow application\n", 
          wait_time);
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "cflow_launch_cb";
    err.type = WARNING;
    err.ident = sccsid;
    em_show(&err);
    sleep(wait_time);
*/
    /* printf("%30.30s\n",outbuf); */
/*  }
*/
  sprintf(outbuf, "/cflow, in_window\nprogram= %s\nargs= %s\n%s\n",
          file_name, cmd_args, ENDOFMSG);

  rtncde = ipc_synch_rw(str_rtn, outbuf);
  if(TRUE != rtncde) {
    sprintf(errmsg, "Cflow command sent to server returned code %d\n",
           rtncde);
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "cflow_launch_cb";
    err.type = WARNING;
    err.ident = sccsid;
    em_show(&err);
  }
}


/*********************************************************************\
*       Function: cflow_debug_cb
*
*       Author: Dan Clark       Dec, 1991       Modified: Dec 17, 1991
*
*       Called by: "Run CFLOW" pb on main menu "Process"
*
\*********************************************************************/
void cflow_debug_cb(Widget widget,
                    XtPointer tag,
                    XmAnyCallbackStruct *callback_data)
{
  char	*outbuf = outbuf_array;
  char  *str_rtn = inbuf_array;

  char *nodename;
  char socketid[MAXNAME];
  char *socket_str;
  int socketobj;
  Widget wid;
  int rtncde;

  nodename = (char *)ipcgetnodename();
/*
  socketobj = ipcgetsockobjnum();
  socketobj += 1;
*/
  if((wid = widget_id("cflow_socket_number_text")) == False) return;
  socket_str = XmTextGetString(wid);
  socketobj = atoi(socket_str);
  if(socketobj < MINSOCKET || socketobj > MAXSOCKET)
  {
    sprintf(errmsg, "Socketid %d invalid must be between %d and %d",
            socketobj, MINSOCKET, MAXSOCKET);
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "cflow_debug_cb";
    err.type = WARNING;
    err.ident = sccsid;
    em_show(&err);
    return;
  }

  sprintf(socketid, "%d", socketobj);

  sprintf(outbuf, "/cflow,nodename=%s,socket=%s\n%s\n",
          nodename, socketid, ENDOFMSG);
  /* printf("%30.30s\n",outbuf); */

  rtncde = ipc_synch_rw(str_rtn, outbuf);

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "cflow_debug_cb: rtncde=%d\n",rtncde);
  }

  /* printf("%30.30s\n",str_rtn); */
  if(TRUE != rtncde) {
    sprintf(errmsg, "cflow command sent to ipf server returned code %d\n",
           rtncde);
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "cflow_launch_cb";
    err.type = WARNING;
    err.ident = sccsid;
    em_show(&err);
  }
}


/*********************************************************************\
*       Function: cflow_kill_cb
*
*       Author: Dan Clark       Dec, 1991       Modified: Dec 17, 1991
*
*       Called by: "Debug CFLOW" pb on main menu "Process"
*
\*********************************************************************/
void cflow_kill_cb(Widget widget,
                   XtPointer tag, 
                   XmAnyCallbackStruct *callback_data)
{
  char	*outbuf = outbuf_array;
  char  *str_rtn = inbuf_array;

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "cflow_kill_cb: pid_cflow=%d\n", pid_cflow);
  }

  printf("kill cflow process if it exists\n"); 

  if(pid_cflow != -1)
  {
    printf("cflow kill pid\n"); 
    kill(pid_cflow, 0);
    pid_cflow = -1;
  }
    
}


int solve_reset( Widget widget, XtPointer tag,
		 XmAnyCallbackStruct *callback_data)
/*********************************************************************\
*       Function: solve_reset
*
*       Author: John Rutis  Dec 23, 1991   Modified:
*
*       Called by: "Reset" pushbutton on solve dialog
*
\*********************************************************************/
{
  Widget	wid;

  XmToggleButtonSetState(widget_id("solve_misc_flat_start_tb" ), True,  0);
  XmToggleButtonSetState(widget_id("solve_misc_dclp_tb"    ), True,  0);

  XmToggleButtonSetState(widget_id("solve_area_on_rb"      ), True,  0);
  XmToggleButtonSetState(widget_id("solve_area_mon_rb"     ), False, 0);
  XmToggleButtonSetState(widget_id("solve_area_off_rb"     ), False, 0);

  XmToggleButtonSetState(widget_id("solve_LTC_on_rb"       ), True,  0);
  XmToggleButtonSetState(widget_id("solve_LTC_RPRQ_rb"     ), False, 0);
  XmToggleButtonSetState(widget_id("solve_LTC_RV_rb"       ), False, 0);
  XmToggleButtonSetState(widget_id("solve_LTC_DC_rb"       ), False, 0);
  XmToggleButtonSetState(widget_id("solve_LTC_off_rb"      ), False, 0);

  XmToggleButtonSetState(widget_id("solve_phase_BPA_rb"    ), True,  0);
  XmToggleButtonSetState(widget_id("solve_phase_WSCC_rb"   ), False, 0);

  XmToggleButtonSetState(widget_id("solve_volts_bias_BPA_rb" ), True,  0);
  XmToggleButtonSetState(widget_id("solve_volts_bias_WSCC_rb"), False, 0);
  XmToggleButtonSetState(widget_id("solve_volts_bias_VMAX_rb"), False, 0);

  XmToggleButtonSetState(widget_id("solve_debug_tx_pb"     ), False, 0);
  XmToggleButtonSetState(widget_id("solve_debug_bus_tb"    ), False, 0);
  XmToggleButtonSetState(widget_id("solve_debug_ai_tb"     ), False, 0);
  XmToggleButtonSetState(widget_id("solve_debug_dc_tb"     ), False, 0);

  XmToggleButtonSetState(widget_id("solve_iter_summary_tb" ), False, 0);
  XmToggleButtonSetState(widget_id("solve_base_solution_tb"), False, 0);

  wid = widget_id(  "solve_limits_qres_text");
  XtVaSetValues(wid, XmNvalue, ".10", NULL);
  wid = widget_id(  "solve_limit_pha_text");
  XtVaSetValues(wid, XmNvalue, "45.001", NULL);
  wid = widget_id(  "solve_limits_DA_text");
  XtVaSetValues(wid, XmNvalue, "1.000", NULL);
  wid = widget_id(  "solve_limits_DV_text");
  XtVaSetValues(wid, XmNvalue, ".150", NULL);

  wid = widget_id(  "solve_numvsteps_text");
  XtVaSetValues(wid, XmNvalue, "3", NULL);

  wid = widget_id(  "solve_misc_cont_tstart_text");
  XtVaSetValues(wid, XmNvalue, "0.5", NULL);

  wid = widget_id(  "solve_tol_busv_text");
  XtVaSetValues(wid, XmNvalue, ".005", NULL);
  wid = widget_id(  "solvel_tol_aipower_text");
  XtVaSetValues(wid, XmNvalue, ".001", NULL);
  wid = widget_id(  "solve_tolerance_tx_text");
  XtVaSetValues(wid, XmNvalue, ".001", NULL);
  wid = widget_id(  "solve_tolerance_q_text");
  XtVaSetValues(wid, XmNvalue, ".500", NULL);

  wid = widget_id(  "solve_decoupled_text");
  XtVaSetValues(wid, XmNvalue, "2", NULL);
  wid = widget_id(  "solve_newton_text");
  XtVaSetValues(wid, XmNvalue, "30", NULL);
  return(0);
}


void clear_solution_data(Widget w, XtPointer adata, XtPointer cdata)
/*************************************************************************\
* Purpose: Loops thru all bus name vertexes and edge vertexes and
*	   remove the characters having solution data.
*
* Called by: refreshGraph	   (this module)
*	     refresh_solution_data (this module)
\*************************************************************************/
{
  GraphElement *pgraphnodenxt;
  GraphElement *pe;
  XmString x_str;
  DB_STAT stat;
  Widget kv_color_wid;
  double dbasekv;
  Pixmap color_pixmap;
  Pixel color;

  char out[50];
  char *pout = &out[0];

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "clear_solution: start\n");
  }

  /** loop through all display bus vertices **/
  if(db_valid_db(&graph_db) != D_OKAY)
  {
    /* printf("Warning: no graph table - processing interupted\n");
    *   Most likely creating a case from scatch  */
    return;
  }

  stat = firstGE( &pgraphnodenxt, GraphTypeVertex );

  if(D_FOUND != stat) {
    /* printf("NO busses displayed on graph\n"); */
    return;
  }

  while(D_FOUND == stat)
  {
    if( (GraphDisplayOn       == pgraphnodenxt->display) &&
        (GraphTypeVertex      == pgraphnodenxt->type   ) && 
        (GraphClassVertexName == pgraphnodenxt->class  ) )
    { 
      strcpy( pout, busNameStr( pgraphnodenxt ));

      x_str =  XmStringCreateLtoR(pout, XmSTRING_DEFAULT_CHARSET);
      XtVaSetValues( pgraphnodenxt->wid, XmNlabelString, x_str, NULL);
      position_gadget_wrt( pgraphnodenxt );
      XmStringFree(x_str);
    }
    else if((GraphDisplayOn   == pgraphnodenxt->display) &&
            (GraphTypeEdge    == pgraphnodenxt->type   ) && 
            (GraphClassEdgeSection == pgraphnodenxt->class) )
    {
      /** found an edge remove solution data from screen */
      sprintf(pout, " ");
      x_str =  XmStringCreateLtoR(pout, XmSTRING_DEFAULT_CHARSET);
/*** old way
*     XtVaSetValues( pgraphnodenxt->wid, XmNlabelString, x_str, NULL);
***/
      dbasekv = setGraphKv(pgraphnodenxt); 
      kv_color_wid = lookup_kv_wid( dbasekv );
      XtVaGetValues( kv_color_wid, 
	                XmNbackground, &color,
	                XmNbackgroundPixmap, &color_pixmap,
		        NULL);

     XtVaSetValues( pgraphnodenxt->wid, 
      XiNlabelString, x_str, 
      XiNlabelOn, (Boolean)FALSE, 
      XiNdrawPixel,  (Pixel)color,
      XiNdrawPixmap, (Pixmap)color_pixmap,
      NULL);
      XmStringFree(x_str);

    }

    stat = nextGE( &pgraphnodenxt, GRAPH_DISPLAY_TYPE );
  }

}


int pfsolution_cb( Widget widget, XtPointer tag,
		   XmAnyCallbackStruct *callback_data)
/*********************************************************************\
*       Function: pfsolution_cb
*
*       Author: Dan Clark  Dec, 1991   Modified: Dec 23, 1991 J. Rutis
*						 May 10, 1994 B. Rogers
*
*       Called by: "Solve" pushbutton on solve dialog
*
\*********************************************************************/
{
  char		*value;
  char		*pfcmdstr = outbuf_array;
  char		*str_rtn  = inbuf_array;
  Widget	wid;
  int		rtncde;
  int 		ostates;

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "pfsolution_cb: start\n");
  }

  /* Build SOLUTION command string */
    
  strcpy(pfcmdstr, "/SOLUTION\n>MISC_CNTRL, -\n VFLATSTART = ");

  if(XmToggleButtonGetState(widget_id("solve_misc_flat_start_tb")))
    strcat(pfcmdstr, "ON");
  else
    strcat(pfcmdstr, "OFF");

  strcat(pfcmdstr, ", -\n DCLP = ");
  if(XmToggleButtonGetState(widget_id("solve_misc_dclp_tb")))
    strcat(pfcmdstr, "ON");
  else
    strcat(pfcmdstr, "OFF");

  strcat(pfcmdstr, ", -\n ITER_SUM = ");
  if(XmToggleButtonGetState(widget_id("solve_iter_summary_tb")))
    strcat(pfcmdstr, "ON");
  else
    strcat(pfcmdstr, "OFF");

  strcat(pfcmdstr, ", -\n X_BUS = ");

  if(XmToggleButtonGetState(widget_id("solve_volts_bias_BPA_rb")))
    strcat(pfcmdstr, "BPA");
  else if(XmToggleButtonGetState(widget_id("solve_volts_bias_WSCC_rb")))
    strcat(pfcmdstr, "WSCC");
  else
    strcat(pfcmdstr, "VMAX");

  strcat(pfcmdstr, ", -\n TSTART = ");
  strcat(pfcmdstr, getValueByName( "solve_misc_cont_tstart_text" ) );

  strcat(pfcmdstr, ", -\n NUMVSTEPS = ");
  strcat(pfcmdstr, getValueByName( "solve_numvsteps_text" ));

  strcat(pfcmdstr, ", -\n PHASE_SHIFTER_BIAS = ");

  if(XmToggleButtonGetState(widget_id("solve_phase_BPA_rb")))
    strcat(pfcmdstr, "BPA");
  else
    strcat(pfcmdstr, "WSCC");

  strcat(pfcmdstr, "\n>LIMITS, QRES = ");
  strcat(pfcmdstr, getValueByName( "solve_limits_qres_text" ));

  strcat(pfcmdstr, ", -\n PHA = ");
  strcat(pfcmdstr, getValueByName( "solve_limit_pha_text" ));

  strcat(pfcmdstr, ", -\n DEL_ANG = ");
  strcat(pfcmdstr, getValueByName( "solve_limits_DA_text" ));

  strcat(pfcmdstr, ", -\n DEL_VOLT = ");
  strcat(pfcmdstr, getValueByName( "solve_limits_DV_text" ));

  strcat(pfcmdstr, "\n>TOLERANCE, BUSV = ");
  strcat(pfcmdstr, getValueByName( "solve_tol_busv_text" ));

  strcat(pfcmdstr, ", -\n AIPOWER = ");
  strcat(pfcmdstr, getValueByName( "solvel_tol_aipower_text" ));

  strcat(pfcmdstr, ", -\n TX = ");
  strcat(pfcmdstr, getValueByName( "solve_tolerance_tx_text" ));

  strcat(pfcmdstr, ", -\n Q = ");
  strcat(pfcmdstr, getValueByName( "solve_tolerance_q_text" ));

  strcat(pfcmdstr, "\n>DEBUG, TX = ");
  if(XmToggleButtonGetState(widget_id("solve_debug_tx_pb")))
    strcat(pfcmdstr, "ON");
  else
    strcat(pfcmdstr, "OFF");

  if(XmToggleButtonGetState(widget_id("solve_debug_bus_tb")))
    strcat(pfcmdstr, ", BUS = ON");
  else
    strcat(pfcmdstr, ", BUS = OFF");

  if(XmToggleButtonGetState(widget_id("solve_debug_ai_tb")))
    strcat(pfcmdstr, ", AI = ON");
  else
    strcat(pfcmdstr, ", AI = OFF");

  if(XmToggleButtonGetState(widget_id("solve_debug_dc_tb")))
    strcat(pfcmdstr, ", DCMODEL = ON");
  else
    strcat(pfcmdstr, ", DCMODEL = OFF");

  if(XmToggleButtonGetState(widget_id("solve_base_solution_tb")))
    strcat(pfcmdstr, "\n>BASE_SOLUTION");

  strcat(pfcmdstr, "\n>LTC = ");
  if(XmToggleButtonGetState(widget_id("solve_LTC_on_rb")))
    strcat(pfcmdstr, "ON");
  else if(XmToggleButtonGetState(widget_id("solve_LTC_RPRQ_rb")))
    strcat(pfcmdstr, "ON_NV");
  else if(XmToggleButtonGetState(widget_id("solve_LTC_RV_rb")))
    strcat(pfcmdstr, "ON_NPS");
  else if(XmToggleButtonGetState(widget_id("solve_LTC_DC_rb")))
    strcat(pfcmdstr, "ON_DCONLY");
  else
    strcat(pfcmdstr, "OFF");

  strcat(pfcmdstr, "\n>AI_CONTROL = ");

  if(XmToggleButtonGetState(widget_id("solve_area_on_rb")))
    strcat(pfcmdstr, "CON");
  else if(XmToggleButtonGetState(widget_id("solve_area_mon_rb")))
    strcat(pfcmdstr, "MON");
  else
    strcat(pfcmdstr, "OFF");

  strcat(pfcmdstr, "\n>SOL_ITER, -\n DECOUPLED = ");

  strcat(pfcmdstr, getValueByName( "solve_decoupled_text" ));

  strcat(pfcmdstr, ", NEWTON = ");
  strcat(pfcmdstr, getValueByName( "solve_newton_text" ));

  strcat(pfcmdstr, "\n");
  strcat(pfcmdstr, ENDOFMSG);

  rtncde = ipc_synch_rw(str_rtn, pfcmdstr);

  if(TRUE == rtncde) {
    refresh_solution_data(0, 0, 0);
  }

  return(0);
}



void process_update_data(char *pfdataret)
/***************************************************************************\
* Purpose: process the pfdataret string
*
* Called by: solve_wp
\***************************************************************************/
{
  int cnt;
    cnt = forEachipfLine(pfdataret, updateOutput, NULL);
}


Boolean solve_wp(int *wp_status)
/**************************************************************************\
* purpose: loops thru all GraphElements, gets names and requests 
*	   corresponding data from powerflow.
*
* Called by: refresh_graph  (via work process)
\**************************************************************************/
{
  GraphElement graphnode;
  GraphElement *pgraphnode;
  static GraphElement *ptest;
  static GraphElement *pgraphnodenxt = NULL;

  char *pfcmdstr = outbuf_array;
  char *pfdataret = inbuf_array;

  char buskey[256];
  char newkey[256];
  char *pbuskey = buskey;
  char busname[32];
  static DB_STAT stat = D_NOTFOUND;
  int cnt;
#define MAX_SOLVE_BUS_CNT 2		/* reduce due to increased precesion */
  int bus_cnt;

  if(db_valid_db(&graph_db) != D_OKAY)
  {
    /* printf("Warning: no graph table - processing interupted\n"); */
    return;
  }

  sprintf(pfcmdstr, "/get_data,type=output\n");
  bus_cnt = 0;

  /** loop through all display bus vertices & build "get_data" string **/
  do
  {
    if(D_FOUND != stat) 
    {
      /* get the first displayed bus */
      stat = firstGE( &pgraphnodenxt, GraphTypeVertex );

      if(D_FOUND != stat)
      {
        /* printf("NO busses displayed on graph\n"); */
        *wp_status = 0;
        return((Boolean)True);
      }
    } /* end of search for first bus */

    if( GraphDisplayOn == pgraphnodenxt->display &&
        GraphClassVertexBus == pgraphnodenxt->class )
    { 
      sprintf(newkey, "B     %12.12s   \n", pgraphnodenxt->vertex_id);
      strcat(pfcmdstr, newkey);
      bus_cnt++;
    } /* end of check of valid code */

    stat = nextGE( &pgraphnodenxt, GRAPH_DISPLAY_TYPE );

  } while (bus_cnt < MAX_SOLVE_BUS_CNT && stat == D_FOUND);

  strcat(pfcmdstr, ENDOFMSG);
  if(bus_cnt > 0 )
  {
    ipc_synch_rw(pfdataret, pfcmdstr);
    process_update_data(pfdataret);
  }

  if(stat != D_FOUND)
  {
    *wp_status = 0;
    return((Boolean)True);	/* actual end (close this process) */
  }
  else
  {
    return((Boolean)False);	/* keep executing this process */
  }
}


void refresh_comments()
/*************************************************************************\
*  Purpose: Asks powerflow for all DEFINE/COMMENT strings 
*		and displays them on screen
*
*  Called by: refreshGraph (above)
*
*  Author: Bill E. Rogers		11-4-94
\*************************************************************************/
{
  int rtncde;
  int len;
  char  *pf_send_rec = outbuf_array;
  char  *str_rtn = inbuf_array;
  char *psubstr;
  char *psubstr2;
  char substring[256];

  GraphElement graphnode;     /* storage for db_search key values    */
  GraphElement *pgraphnode;   /* ptr to key for bus search       */
  GraphElement *pgraphnodenxt;    /* ptr assigned to address of next rec */
  DB_STAT stat;

  PSCommentRecord *rec;

  int offset;
  XmString strx;

  stat = db_valid_db(&graph_db); /*** check for a valid db **/
  if(stat == D_INVALID_DB)
  {
    printf("refresh comments: Info - graph db invalid\n");
    return;
  }

  /**** Get first COMMENT record ************/
  pgraphnode = &graphnode;
  pgraphnode->type = GraphTypeVertex;
  pgraphnode->class = GraphClassVertexComment;
  pgraphnode->idx = (long)0;

  stat = db_search( &graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_CLASS_IDX);
  if(stat == D_EOF) {
    /* printf("refresh comments: Info - no coord data available\n"); */
    return;
  }

  if(pgraphnodenxt->class == GraphClassVertexComment) {
    stat = D_FOUND;
  }
  if(stat != D_FOUND) {
    /* printf("refresh comments: Info - no comments on screen\n"); */
    return;
  }

  offset = sizeof(rec->key) + sizeof(rec->subkey) +
  sizeof( rec->x ) + sizeof( rec->y ) - 1;

  sprintf(pf_send_rec, "/GET_DATA, TYPE = SUB_DEFINE\n%s\n", ENDOFMSG);
  rtncde = ipc_synch_rw(str_rtn, pf_send_rec);

  /* set up string ptr (1st string is header which is ignored) */
  psubstr = strtok(str_rtn, SEPCHARS);
  psubstr2= strtok(NULL,SEPCHARS);

  while(psubstr2!= NULL && stat == D_FOUND)
  {
    psubstr = psubstr2;
    psubstr2= strtok(NULL,SEPCHARS);
    if( is_err_msg(psubstr) == FALSE )
    {
      len = psubstr2-psubstr-offset;
      if( len > sizeof( substring ) ) len=sizeof( substring ) -1;
  
      strncpy( substring, psubstr+offset, len );
      clip_trailing_blanks( substring );
	
      strx = XmStringCreateLtoR( substring, XmSTRING_DEFAULT_CHARSET );
      if(pgraphnodenxt->wid != NULL)
	XtVaSetValues( pgraphnodenxt->wid, XmNlabelString, strx, NULL);
      XmStringFree(strx);
    }
	/** Get next record **/
    pgraphnode = pgraphnodenxt;
    stat = db_next(&graph_db,&pgraphnode,&pgraphnodenxt,GRAPH_CLASS_IDX);
    if(pgraphnodenxt->class != GraphClassVertexComment) {
	stat = D_NOTFOUND;
    }
  } /* end while */
}


int refreshGraph()
/**************************************************************************\
* purpose: 
*
* Called by: refresh_solution_data, 
\**************************************************************************/
{
   extern XtAppContext app_context;
   static Widget pwid;
   XtWorkProcId solution_wpid;
   static int status = 0;

  char  *pf_send_rec = outbuf_array;
  char  *str_rtn     = inbuf_array;

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "refreshGraph: start\n");
  }

  /** loop through all display bus vertices **/
  if(db_valid_db(&graph_db) != D_OKAY)
  {
    printf("Warning: no graph table - processing interupted\n");
    return;
  }
  fetch_print_windows();

  /* clear old solution values from the screen */
  clear_solution_data(0, 0, 0);

  read_overload_boxes( 0, 0, 0 );

  pwid = widget_id("drawwindow");
  assert(pwid != NULL);

  get_bus_name_solution_opt(&name_opt);	/* read toggle button - get format */
  get_bus_solution_opt(&sol_opt);

  if( 0 == status)
  {
    status = 1;
    solution_wpid=XtAppAddWorkProc( app_context, (XtWorkProc)solve_wp, &status);
  }
  else
  {
    printf("User Warning - must wait for previous solution req to complete\n");
  }

  refresh_comments();

} /* end refreshGraph */


void refresh_solution_data(Widget w, XtPointer adata, XtPointer cdata)
/**************************************************************************\
*
* Called by: "Solution Data On" toggle button (in View pulldown).
*		pfsolution_cb, setup_screen_display, set_overload_color
*		set_kv_colors
\**************************************************************************/
{
  int ostates;
  Widget wid;
  Boolean view_solve_on;

  ipfstate_get(&ostates);

  view_solve_on = True;
  wid = widget_id("view_menu_solution_pd");
  if(wid != NULL) {
    view_solve_on = XmToggleButtonGadgetGetState(wid);
  }

  /* 5 or higher indicates a successful solution */
  if((ostates >= 5) && view_solve_on == True) {	
    refreshGraph();
  } else {
    clear_solution_data(0, 0, 0);
  }
}


typedef struct newLine
{
  char *lines[MAXRECS];
  int cnt;
} NewLine;




int checkLines(char *line, char *cpcon)
/*************************************************************************\
* Purpose: check data string from powerflow and return
*	   pointers to strings that denote a transmission line
*
* Called by: explodeBus, explodeBranches
\*************************************************************************/
{
  NewLine *pcon = (NewLine *)cpcon;

  if(isline((pfOutRecord *)line))
  {
    pcon->lines[pcon->cnt] = line;
    pcon->cnt++;
  }
  return(0);
}




int checkLinesBuses(char *line, char *cpcon)
/*************************************************************************\
* Purpose: Analyze char string, ignoring everything except buses and lines
*	   returning list of ptrs.
*
* Called by: process_pf_connections (graphdata.c)
\*************************************************************************/
{
  NewLine *pcon = (NewLine *)cpcon;	/* will hold ptrs to buses & lines */

  if(isline((pfOutRecord *)line) || isbus((pfOutRecord *)line ))
  {
    pcon->lines[pcon->cnt] = line;
    pcon->cnt++;
  }

/* else */
/** some output data types ( + bus cards) are not handled **/
/* printf("error - unhandled line type %s\n", line); */

  return (0);
}

 
/**********************
* this is the data layout of information returned from
* the powerflow request 
*  getdata type = connect 
*
************************/
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




void explodeBranches( GraphElement *pge )
/*************************************************************************\
* Purpose: given a (new) bus, add any missing powerflow branches to bus.
*	   And if far bus is NOT on graph... don't add!
*
*		Turn on any subedges if bus is in powerflow.
*
* Called by: addGraphBusXY (graphdata.c)
*	     explodeBus (this module)
\*************************************************************************/
{
  char	inbuf_array [BUFSIZE];
  char *pfdataret = inbuf_array;
  NewLine con;
  ConnectData *pcon;
  DB_STAT stat;
  int cnt;
  int idx;

  get_pf_connect_data( pge, pfdataret );
  
  con.cnt = 0;

  forEachipfLine( pfdataret, checkLines, (char *)&con);

  idx = con.cnt;		 /* now index through all lines */
  for(idx--; idx >= 0; idx--)
  {
    GraphElement *pnewedge;
    GraphElement *pgraphbus;
    char key[128];
    char namebase[128];

    pcon = (ConnectData *)con.lines[idx];

	    /* get farend bus vtx of line */
    sprintf(namebase, "%12.12s", &pcon->name2[0]);
    stat = findBusGraphVertexRec(namebase, &pgraphbus );

    if(D_FOUND != stat)
    {
      continue;	 /* create NO line - far bus not on graph! */
    } 


    /* check and see if branch is already in graph_db (may not be turned on) */
    stat = findGraphEdgebyName2(pcon->name1, pcon->name2, &pnewedge );

    if(D_FOUND != stat )	/* branch does not exist = create it */
    {
      createGraphBranch( pge, pgraphbus );
    }
    else	/* branch already exists - check that its turned on */
    {
      if(GraphDisplayOn != pnewedge->display)
      {
        if(GraphClassEdgeComplexSection == pnewedge->class)
        {
          turnOnSubedge(pnewedge);
        }
        else
        {
          printf("PROGRAMMER ERROR - should never find subedge\n");
          printGraphElement(pnewedge);
        }
      }
      /* else */
        /* printf("already turned on so leave it alone\n"); */
    }
  }
}


void explodeBus(GraphElement *pge)
/**************************************************************************\
* Purpose: performs the "explode" or "star" function
*	   i.e. finds all buses connected to this bus and creates
*	   all appropiate edge and bus vertexes.
*
*	Author: D.Clark		Dec 92
*
*	Called by: toolbox.c (drawAreaButtonCB)
*
\**************************************************************************/
{
  static GraphElement *ray_wid[50];	/* bus widgets (old and new) */
  static double ray_rad[50];		/* direction of each widget */
  static int ray_fix[50];		/* flag whether widget can be */
					/* relocated */
  static int nvtx;

  char	inbuf_array [BUFSIZE];		/* array to hold returned pf data */
  char *pfdataret = inbuf_array;
  NewLine con;				/* array holding pfdatret rec ptrs */

  int idx;
  ConnectData *pcon;
  DB_STAT stat;
  int xdelta, ydelta;

  char	inbuf_array2 [BUFSIZE];		/* array to hold returned pf data */
  char *pfcmdstr = outbuf_array;
  char *pfdataret2 = inbuf_array2;

  if( get_pf_connect_data( pge, pfdataret ) ) 
	return;		/* failed - cancel the explode function */

  con.cnt = 0;
  forEachipfLine(pfdataret, checkLines, (char *)&con);

  nvtx = 0;		/* number of "rays" created     */
  idx = con.cnt;	/* number of lines in pfdataret */
			/* (returned by forEachipfLine) */

  /* now index through all pfdataret lines */
  for(idx--; idx >= 0; idx--)		/* looping backwards */
  {
    GraphElement *pgraphbus;
    GraphElement *pgraphnamerec;
    GraphElement *pnewedge;
    PSCoordFileRecord *coordrec_p;     

    char namebase[128];

    pcon = (ConnectData *)con.lines[idx];

    /* take the farend bus name from the line and create a bus */
    /* set up a standard coordinate file key */
    sprintf(namebase, "%12.12s", &pcon->name2[0]);

    stat = findBusGraphVertexRec(namebase, &pgraphbus);

    if(D_FOUND == stat)
    {
      ray_rad[++nvtx] = radiansBetweenPts( pge->x, pge->y,
					   pgraphbus->x,     pgraphbus->y  );
      ray_fix[nvtx] = 1;	 /* NOTE if bend is on line, above should*/
				 /* really look for location of 1st bend */
    }
    else
    {
      /*char newname[15];  no more*/
      char newkey[125];
	/* should now attempt to find x,y from coord file */
      coordrec_p = coord_db_bus_search( namebase );
      if( coordrec_p != NULL )
      {
	pgraphbus = createGraphBusXYXY( namebase,
          pscoord_to_MOTIF_x( coordrec_p->bus.symbol.x,  6, namebase ),
          pscoord_to_MOTIF_y( coordrec_p->bus.symbol.y,  6, namebase ),
          pscoord_to_MOTIF_x( coordrec_p->bus.namepos.x, 6, namebase ),
          pscoord_to_MOTIF_y( coordrec_p->bus.namepos.y, 6, namebase ),
	  coordrec_p->bus.generator_ang,
	  coordrec_p->bus.reactor_ang,
          1,      /* manage */
          1,      /* explode branches */
          1,    /* needs to exist in pf */
	  1,	/* gen flg */
	  1 );	/* reac flg */

        ray_rad[++nvtx] = radiansBetweenPts( 
		atoi_cnt( coordrec_p->bus.symbol.x, 6 ),
		atoi_cnt( coordrec_p->bus.symbol.y, 6 ),
                pgraphbus->x,     pgraphbus->y  );
        ray_fix[nvtx] = 1;         /* NOTE if bend is on line, above should*/
                                 /* really look for location of 1st bend */
      }
      else
      {
        ray_rad[++nvtx] = 0;
        ray_fix[nvtx] = 0;

        pgraphbus = createGraphBusXYXY( namebase,
	  pge->x, pge->y, pge->x, pge->y+20, "30", "60",
          1,      /* manage */
          1,      /* explode branches */
          0,    /* needs to exist in pf */
	  1,
	  1 );

      }
      if( pgraphbus )
      {
	sprintf(pfcmdstr, "/get_data,type=output\n");
	sprintf(newkey, "B     %12.12s   \n", pgraphbus->vertex_id);
	strcat(pfcmdstr, newkey);
	strcat(pfcmdstr, ENDOFMSG);
	ipc_synch_rw(pfdataret2, pfcmdstr);

	forEachipfLine(pfdataret2, update_bus, NULL);/* fill in name data*/

      }
      else
      {
        printf("*WARNING* unable to create bus %s on graph\n", namebase );
        return;
      }
 
    }
    ray_wid[nvtx] = pgraphbus;

    stat = findGraphEdgebyName2(pcon->name1, pcon->name2, &pnewedge );

    if(D_FOUND != stat)	       /* take all the line data and create a branch */
    {
      createGraphBranch( pge, pgraphbus );
    }
    else
    {
      if(GraphDisplayOn != pnewedge->display)
      {
        if(GraphClassEdgeComplexSection == pnewedge->class)
        {
          /* printf("need to turn on all sub edges\n"); */
          turnOnSubedge(pnewedge);
        }
        else
        {
          printf("PROGRAMMER ERROR - should never fine subedge\n");
          printGraphElement(pnewedge);
        }
      }
      else
      {
        /* printf("already turned on so leave it alone\n"); */
      }
    }
  }
#define PERIMETER 90
#define PIE 3.1415927
#define BORDER 10

  if( nvtx ==0 ) {
    printf("*** WARNING *** no lines found!!!!\n" );
  }
  else
  {
    distribute_rays( nvtx, ray_rad, ray_fix );

    for( idx=1; idx<=nvtx; idx++ )
    {
      double rad;
      int xoffset, yoffset, newx, newy;
      if( ray_fix[idx]==0 )
      {
        rad = ray_rad[idx];
        xoffset = (int)(0.5 + PERIMETER * zoom_factor * cos(rad) );
        yoffset = (int)(0.5 + PERIMETER * zoom_factor * sin(rad) );

     /*   if( rad > PIE )  yoffset = -yoffset; 
        if( rad > PIE/2 && rad < PIE*3/2 ) xoffset = -xoffset; */

        newx = max( BORDER, pge->x + xoffset);
        newy = max( BORDER, pge->y + yoffset);
	xdelta = newx - pge->x;
	ydelta = newy - pge->y;
        update_location_of_ge_and_attachments( ray_wid[idx], xdelta, ydelta );
      }
    }
  }
}


int get_pf_connect_data( GraphElement *pge, char *pfdataret )
/*************************************************************************\
* Purpose: Given a graphelement (vertex or edge), send a GET_DATA
*	   request to powerflow and return with data array filled.
*
* Author: Wm E. Rogers
*
* Called by: explodeBranches, explodeBus (this module)
\**************************************************************************/
{
  GraphElement *pbus;
  char	outbuf_array[BUFSIZE];
  char *pfcmdstr = outbuf_array;
  char newkey[256];
  char buskey[256];
  char *pbuskey = buskey;

  memset(newkey, ' ', sizeof(newkey));		/* clear the search key */

  switch( pge->class )
  {
    case GraphClassVertexName:
      if( findBusfromNameVertex( &pbus, pge ) )
	 return(1);		 /* failed */
      pge = pbus;		/* change from name vtx to symbol vtx */

    case GraphClassVertexBus:
      sprintf(newkey, "B     %12.12s", pge->vertex_id);
    break;

    case GraphClassEdgeSection:
      sprintf(newkey, "L     %12.12s %12.12s", pge->vertex_id,
                                             pge->farvertex_id);
    break;

      printf("explodeBus data only valid for buses and branches\n");
      return(1);	/* failed */
  }
 
  strncpy(pbuskey, newkey, 19);
  pfcmdstr [0] = 0;
  pfdataret[0] = 0;

  sprintf(pfcmdstr, "/GET_DATA, TYPE = CONNECTION\n%s\n%s\n", 
          pbuskey, ENDOFMSG);
  
  ipc_synch_rw( pfdataret, pfcmdstr );

  return (0);	/* success */
}


void createGraphBranch( GraphElement *v1, GraphElement *v2)
/***************************************************************************\
* Purpose: create a no-bend graph element between two bus vertexes
*
*
\***************************************************************************/
{
  GraphElement *vtx1, *vtx2;
  GraphElement *pcomplex_edge;
  GraphElement *pnewedge;
  PSBranchRecord linekey;
  DB_STAT stat;

  memset(&linekey, ' ', sizeof(linekey));

    /* create a branch - low to high */
  if(strncmp( v1->vertex_id, v2->vertex_id, 
     sizeof( v1->vertex_id)) > 0)
  {
    vtx2 = v1;
    vtx1 = v2;
  }
  else
  {
    vtx1 = v1;
    vtx2 = v2;
  }

  if( atof_cnt_zero( &vtx1->vertex_id[8], 4) !=
      atof_cnt_zero( &vtx2->vertex_id[8], 4))
    strncpy(&linekey.key[0], "T", sizeof(linekey.key));
  else
    strncpy(&linekey.key[0], "L", sizeof(linekey.key));

  strncpy(linekey.name1, vtx1->vertex_id, 
    sizeof(linekey.name1) + sizeof(linekey.base1));
  strncpy(linekey.name2, vtx2->vertex_id, 
    sizeof(linekey.name2) + sizeof(linekey.base2));

  pscreateGraphCorEdge ( vtx1, vtx2,   &linekey.key[0],
                     GraphClassEdgeComplexSection, 1, &pcomplex_edge);

  pscreateGraphCorEdge(vtx1, vtx2, &linekey.key[0],
                     GraphClassEdgeSection, 1, &pnewedge);

  addGraphLink( pnewedge, pcomplex_edge );   /* link parent - */
  addGraphLink( pcomplex_edge, pnewedge);   /* child edges */

  manageEdge(pnewedge);

  if(True == istransformer(pnewedge))
  {
    XtVaSetValues( pnewedge->wid, XiNtransformerOn, (Boolean)TRUE, NULL);
  }
 
}


void distribute_rays( int nvtx, double ray_rad[50], int ray_fix[50] )
/**************************************************************************\
*	Purpose: evenly distributes several rays around a point.
*
*	Author: B.Rogers		Feb 18, 1993
*
* Called by: explode_bus
\**************************************************************************/
{
  int i,j;
  int tick_mark[50];
  double maxangle, tm_slce, angle;
  int best_loc, tkmk;

  tm_slce = PIE*2/nvtx;		/* size of each "pie slice" in radians */

  for( i=0; i<=nvtx; tick_mark[i++]=0 ); 	/* mark as "not taken" */

  /* find tick mark closest to existing lines */
  for( i=0; i<=nvtx; i++ )
  { 
    if( ray_fix[i]==1 )
    {
      maxangle = 9999.0;
      for( tkmk=0; tkmk<nvtx; tkmk++ )
      {
        angle = fabs( (double)(tkmk*tm_slce) - ray_rad[i]);
        if( angle > PIE ) angle = PIE*2 - angle;

        if( (tick_mark[tkmk]==0) && angle < maxangle )
        {
          best_loc = tkmk;
	  maxangle = angle;
        }
      }
      tick_mark[best_loc] = 1;	/* eliminate this mark from being assigned */
			/* continue until all previous lines are assigned */
      /* printf("line %d %d %6.2f assigned to tick mark %d of %d\n",
		 i, ray_fix[i], ray_rad[i], best_loc, nvtx ); */
    }
  }

  /* PHASE II assign each new exploded line to an unassigned tick mark */
  for( i=1; i<= nvtx; i++ )
  {
    if( ray_fix[i]==0 )
    {
       for( j=0; j< nvtx; j++ )
       {
         if( tick_mark[j]==0 )
         {
           ray_rad[i] = j*tm_slce;
           tick_mark[j] = 1;
	   break;
         }
       }
    }
  }
}



double radiansBetweenPts( int x, int y, int x2, int y2 )
/*************************************************************************\
* Purpose: Return angle in radians that x2,y2 is from x,y
*  	   i.g. for 90 deg, x2,y2 is directly above x,y
\*************************************************************************/
{
  double radns;

  if( x2-x != 0)
    radns = (double)atan2( (double)(y2-y), (double)(x2-x) );
  else if ( y2 > y )
    radns = (double)PIE/2;	/* 90 degrees */
  else
    radns = (double)PIE*3/2;	/* 270 degrees */

  if( radns < 0 ) radns += (double)PIE*2;

  /* printf("%f radians from %d-%d to %d-%d\n", radns, x, y, x2, y2 ); */

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "radiansBetweenPts: radns=%f\n", radns);
  }
  return ( radns );

}


updateOutput(char *psolnData, char *userdata )
/************************************************************************\
* Purpose: decide if pf solution data is a bus or a branch and 
*	   call appropiate routine.
*
* Called by: refreshGraph
* Note:
*  The + Cards and X cards are not passed on to the update vertex function.
*  Currently the data from these solution cards is not used to update
*  the screen graph with values.  If the solution options displayed on
*  the screen is enhanced it may require these cards to be passed
*  on through this function by updateing isbus and refreshBusImage
*  to handle these types.
*
\************************************************************************/
{
  if(isbus((pfOutRecord *)psolnData))
    refreshBusImage((pfOutRecord *)psolnData);
  
  else if(  isline((pfOutRecord *)psolnData) )
    update_edge((pfOutRecord *)psolnData);
 
}


#define BUSCODE 'B'

/*************************************************************************\
\*************************************************************************/
int isbus(pfOutRecord *psolnData) 
{
 if(BUSCODE == psolnData->code[0]) 
   return True;
 else
   return False;

} /* end isbus */





int isline(pfOutRecord *psolnData) 
/*************************************************************************\
\*************************************************************************/
{
#define LINECODE 'L'
#define TRANSFORMERCODE 'T'
#define EQUIVALENTCODE 'E'
#define REACTORCODE 'R'

 if( LINECODE	     == psolnData->code[0] ||
     TRANSFORMERCODE == psolnData->code[0] ||
     EQUIVALENTCODE  == psolnData->code[0] ||
     REACTORCODE     == psolnData->code[0] ) 
   return True;
 else
   return False;
} /* end isline */






#define VTGENERATOR 1
#define VTCAPACITOR 2
#define VTREACTOR 4

int isDCBus(pfOutRecord *psolnData)
/***************************************************************************\
\***************************************************************************/
{
  int cde = False;
  /* first check to make sure the data is for a bus */
  if(psolnData->bus.code[0] == BUSCODE )
  {
    /* now check if it is one of the DC bus sub types */
    if( psolnData->bus.type[0] == 'D' ||
        psolnData->bus.type[0] == 'M')
    {
       cde = True;
    }
  } 
  return cde;
}


void getVertexType( pfOutRecord *psolnData, int *vtype )
/*************************************************************************\
* Purpose: Determind if bus as any GENERATOR or CAP/REACTANCE
*
* Called by: setVertexIcon
*	     writeBusLabel
\*************************************************************************/
{
#define MINVAL (double)0.1
#define GEN_NONE 1L<<0
#define GEN_NORM 1L<<1
#define GEN_MORE 1L<<2
#define GEN_ALL  1L<<3

  double cor_angle = (double)1.0;
  double dzero = 0.0;
  double dval1 = 0.0;
  double dval2 = 0.0;
  int type;
  int gen_opt;

  type = 0;

  if((False == isDCBus(psolnData)))
  {
    get_bus_generation_opt(&gen_opt);
    /** check if generator data is available and assume generator **/
    switch ( gen_opt )
    {
      case GEN_NORM:
      case GEN_MORE:
      case GEN_ALL:
        if (( cor_angle > MINVAL ) || (gen_opt & GEN_MORE))
        {
	  if((MINVAL < fabs(atof_cnt(&psolnData->acbus.P_gen[0],
                    sizeof(psolnData->acbus.P_gen)))) ||
	     (MINVAL < fabs(atof_cnt(&psolnData->acbus.Q_gen[0],
                    sizeof(psolnData->acbus.Q_gen)))) )
	  {
            type |= VTGENERATOR;
	  }
	}
      break;

      case GEN_NONE:
	    /* do not show any generation if it exists */
      break;

    } 

    /** check if capacitor data is available and assume capicitor **/
      if(((MINVAL < fabs(atof_cnt(&psolnData->acbus.B_shunt_cap_used[0],
                     sizeof(psolnData->acbus.B_shunt_cap_used))) ) ||
         (MINVAL < fabs(atof_cnt(&psolnData->acbus.B_shunt_cap_sched[0],
                     sizeof(psolnData->acbus.B_shunt_cap_sched))) ))){
        type |= VTCAPACITOR;
      }
    /** check if reactor data is available and assume reactor **/
      if(((MINVAL < fabs(atof_cnt(&psolnData->acbus.B_shunt_react_used[0],
                    sizeof(psolnData->acbus.B_shunt_react_used))) ) ||
          (MINVAL < fabs(atof_cnt(&psolnData->acbus.B_shunt_react_sched[0],
                    sizeof(psolnData->acbus.B_shunt_react_sched))) ))){
         type |= VTREACTOR;
       }
  }
    /* printf("type set to %d\n", type); */
    *vtype = type;
}

char *BusIconWidgets[] = {
	"bus_norm_pb",
	"bus_gen_pb",
	"bus_norm_cap_pb",
	"bus_gen_cap_pb",
	"bus_norm_reac_pb",
	"bus_gen_reac_pb",
	"bus_norm_cap_reac_pb",
	"bus_gen_cap_reac_pb",
	"undisplayed_bus_icon"
};

char *BusIcon2Widgets[] = {
	"bus_small_circle",
	"bus_legend_bus_pixmap",
	"bus_large_circle",
	"bus_small_circle_blk",
	"bus_med_circle_blk",
        "bus_large_circle_blk",
	"bus_small_square",
	"bus_med_square",
	"bus_large_square",
        "bus_small_square_blk",
        "bus_med_square_blk",
        "bus_large_square_blk",
	"bus_small_bar",
	"bus_medium_bar",
	"bus_large_bar",
        "bus_small_bar_blk",
        "bus_med_bar_blk",
        "bus_large_bar_blk",
	"bus_rect_round",
	"bus_rect_square"
};


void setVertexIcon(GraphElement *pvertex, pfOutRecord *psolnData)
/*************************************************************************\
* Purpose: determine the type of the data and change bus icon
*
* Called by: refreshBusImage
\*************************************************************************/
{
  int vtype;  /* mask of types */
  Widget wid;
  Pixmap arm_pixmap;

  if(GraphTypeVertex == pvertex->type      &&
     GraphClassVertexBus == pvertex->class &&
     GraphDisplayOn == pvertex->display	   &&
     NULL != pvertex->wid )
  {

    if( pvertex->hide != HIDE )
    {
      if( (int)XmToggleButtonGadgetGetState(
			widget_id("view_menu_bus_combine_tb")))
      {
        getVertexType(psolnData, &vtype);
        wid = widget_id(BusIconWidgets[vtype]);
      }
      else
      {
	if( bus_icon_key >= 0 )
          wid = widget_id(BusIcon2Widgets[bus_icon_key]);
	else
	  wid = widget_id( "bus_legend_bus_pixmap" );
      }
    }
    else
    {
      wid = widget_id("undisplayed_bus_icon");
    }

    if(wid == False) return;

    XtVaGetValues(wid,
                XmNarmPixmap, &arm_pixmap,
                NULL);
    XtVaSetValues(pvertex->wid,
                XmNlabelType,		 XmPIXMAP,
                XmNlabelInsensitivePixmap, (Pixmap)arm_pixmap,
                XmNlabelPixmap,		 (Pixmap)arm_pixmap,
                XmNarmPixmap,		 (Pixmap)arm_pixmap,
                NULL);

    position_gadget_wrt(pvertex); /* in case widget size changes */
	/* also need to change line ends! */
  }
  else
  {
    printf("setVertexIcon: warning: expected vertex class\n");
    printGraphElement(pvertex);
  }
}

void manage_gen_and_reac_symbols(GraphElement *pvertex, pfOutRecord *psolnData)
/*************************************************************************\
* Purpose: determine the type of bus data and set up GEN and REAC gadgets
*
* Called by: refreshBusImage
\*************************************************************************/
{
  Widget toolbox_wid;
  Pixmap toolpix;
  char name_str[25];

  GraphElement *pgraphgen;
  GraphElement *pedge;
  GraphElement vtxupdate;
  GraphElement *pvtxupdate = &vtxupdate;

  int vtype;  /* mask of types */

  if(GraphTypeVertex == pvertex->type      &&
     GraphClassVertexBus == pvertex->class &&
     GraphDisplayOn == pvertex->display	   &&
     NULL != pvertex->wid )
  {

    if( pvertex->hide != HIDE )
    {
      getVertexType(psolnData, &vtype);

      findVertxLinkedToVertx( &pgraphgen, pvertex, GraphClassVertexGenerator );

      if(( vtype & VTGENERATOR &&
         XmToggleButtonGadgetGetState(widget_id("print_gen_rb"))==True) ||
	 XmToggleButtonGadgetGetState(widget_id("print_al_gen_rb"))==True )
      {
        if ( pgraphgen )
        {
	  if (pgraphgen->display == GraphDisplayOff )
	  {
	    manageVertex( pgraphgen );	/*ensure generator is turned on */
	    findGraphEdgebyName2(pvertex->vertex_id, "ZZGenerator ", &pedge );
	    manageEdge( pedge );
	  }
        }
        else 				/* otherwise create it */
        {
	  createGraphGenReac( &pgraphgen, pvertex, "  1",
		 GraphClassVertexGenerator, G_DIST, "ZZGenerator ", 1 );
        }
      }
      else if ( pgraphgen ) /* no gen, turn if off (if off) */
      {
        if( pgraphgen->display )
	{
	  turnoffGraphElement( pgraphgen );
          findGraphEdgebyName2( pvertex->vertex_id, "ZZGenerator ", &pedge );
	  turnoffGraphElement( pedge );
	}
      }

/**************  process CAP/REAC *****************/
      findVertxLinkedToVertx( &pgraphgen, pvertex, GraphClassVertexCapacitor );

      if( pgraphgen == NULL )
        findVertxLinkedToVertx( &pgraphgen, pvertex, GraphClassVertexReactor );

      if((( vtype & VTCAPACITOR || vtype & VTREACTOR ) &&
         XmToggleButtonGadgetGetState(widget_id("print_shunt_rb"))==True)||
         XmToggleButtonGadgetGetState(widget_id("print_al_shunt_rb"))==True )
      {
        if ( pgraphgen )
        {
	  if (pgraphgen->display == GraphDisplayOff )
	  {
	    manageVertex( pgraphgen );	/*ensure reactance is turned on */
	    findGraphEdgebyName2(pvertex->vertex_id, SHUNT_N, &pedge );
	    manageEdge( pedge );
	  }
        }
        else 				/* otherwise create it */
        {
	  createGraphGenReac( &pgraphgen, pvertex, "  1",
		 GraphClassVertexReactor, R_DIST, SHUNT_N, 1 );
        }

	/* set appropiate picture and class */
	pvtxupdate = &vtxupdate;
	memcpy(pvtxupdate, pgraphgen, sizeof(GraphElement));

	if ( vtype & VTREACTOR )
	{
	  toolbox_wid = widget_id( "bus_legend_react_pixmap" );
	  pvtxupdate->class = GraphClassVertexReactor;
        }
	else if ( vtype & VTCAPACITOR )
        {
	  toolbox_wid = widget_id( "bus_legend_cap_pixmap" );
	  pvtxupdate->class = GraphClassVertexCapacitor;
        }

	db_update(&graph_db, &pgraphgen, &pvtxupdate);



	XtVaGetValues(toolbox_wid, XmNarmPixmap, &toolpix, NULL);

	XtVaSetValues( pgraphgen->wid,
                XmNlabelType,		 XmPIXMAP,
                XmNlabelInsensitivePixmap, (Pixmap)toolpix,
                XmNlabelPixmap,		 (Pixmap)toolpix,
                XmNarmPixmap,		 (Pixmap)toolpix,
		NULL );
 
      }
      else if ( pgraphgen ) /* no reac, turn it off (if on) */
      {
        if( pgraphgen->display )
	{
	  turnoffGraphElement( pgraphgen );
          findGraphEdgebyName2( pvertex->vertex_id, SHUNT_N, &pedge );
	  turnoffGraphElement( pedge );
	}
      }
    }
  }

}


void set_bus_icon( Widget wid, XtPointer tag, XtPointer reason )
{
  char *size;
  char *shape;
  char *style;

  size  = (char *)tag;
  shape = size+1;
  style = size+2;
 
  if( (int)XmToggleButtonGadgetGetState(
		widget_id("view_menu_bus_combine_tb")))
  {
     icon_bus_width = 5;
     icon_bus_height= 5;
     return;
  }
  
  switch( *shape )
  {
    case 'C':
      /* printf("circle "); */
      bus_icon_key = 0;
      icon_bus_shape = CIRCLE;
    break;

    case 'S':
      /* printf("square "); */
      bus_icon_key = 6;
      icon_bus_shape = SQUARE;
    break;

    case 'B':
      /* printf("Bar "); */
      bus_icon_key = 12;
      icon_bus_shape = BAR;
    break;

    case 'R':
      /* printf("Rect "); */
      bus_icon_key = 18;
      icon_bus_shape = RECTANGLE;
    break;
  }

  switch( *style )
  {
    case 'N':
      /* printf("Hollow"); */
      icon_bus_style = HOLLOW;
    break;

    case 'B':
      /* printf("Solid "); */
      bus_icon_key += 3;
      icon_bus_style = FILLED;
    break;

    case 'R':
      /* printf("Rounded "); */
      bus_icon_key -= 2;
    break;

    case 'S':
      /* printf("Squared "); */
      bus_icon_key -= 1;
    break;
  }

  switch( *size )
  {
    case 'S':
      /* printf("Small "); */
      icon_bus_width = 3;
      icon_bus_height= 3;
    break;

    case 'M':
      /* printf("Medium "); */
      bus_icon_key += 1;
      icon_bus_width = 5;
      icon_bus_height= 5;
    break;

    case 'L':

      bus_icon_key += 2;
      /* printf("Large "); */
      icon_bus_width = 9;
      icon_bus_height= 9;
    break;

  }

/********  set bus width and height for odd shapes ***************/

  switch( *shape )
  {
    case 'R':		/* Round or Square Rect */
      icon_bus_width = 25;
      icon_bus_height = 9;
    break;

    case 'B':		/* BAR shapes */
      switch( *size )
      {
	case 'S':
	  icon_bus_width = 7;
	  icon_bus_height= 1;
	break;

        case 'M':
          icon_bus_width = 15;
          icon_bus_height= 2;
        break;

        case 'L':
	  icon_bus_width = 20;
	  icon_bus_height = 3;
	break;

      } 
    break; 
  }

/*  printf(" %d %d %d\n", bus_icon_key, icon_bus_width, icon_bus_height ); */

  refreshGraph();
}



void refreshBusImage(pfOutRecord *psolnData)
/***********************************************************************\
* Purpose: Given: busname from ipf data, updates following items on screen:
*	   1) Bus Icon color/shape
*	   2) Generation symbol
*	   3) Cap/Reac symbol
*	   4) Name/Volts label (filled with pf info)
*
*  Called by: updateOutput
*		update_bus (pscordat.c)
\***********************************************************************/
{
  GraphElement *pvtx;
  GraphElement *pedge;
  GraphElement *pgraphnodenxt;
  int cnttmp;
  DB_STAT stat;
  char *pcsolnData;

  if(db_valid_db(&graph_db) != D_OKAY)
  {
    printf("Warning: no graph table - processing interupted\n");
    return;
  }

  stat = findBusGraphVertexRec( psolnData->bus.name, &pgraphnodenxt );

  if(stat == D_FOUND)
  {						 /* found a graph node */
    setVertexIcon(pgraphnodenxt, psolnData);	 /* set the vertex icon */

    if( (int)XmToggleButtonGadgetGetState(
			widget_id("view_menu_bus_combine_tb")))
    {
      findVertxLinkedToVertx( &pvtx, pgraphnodenxt,
					 GraphClassVertexGenerator );
      if( pvtx )
      {
	if( pvtx->display )
	{   
	  turnoffGraphElement( pvtx );
          findGraphEdgebyName2( pvtx->vertex_id, "ZZGenerator ", &pedge );
	  turnoffGraphElement( pedge );
	}
      }

      findVertxLinkedToVertx( &pvtx, pgraphnodenxt,
					 GraphClassVertexCapacitor );
      if( pvtx == NULL )
        findVertxLinkedToVertx( &pvtx, pgraphnodenxt,
					 GraphClassVertexReactor );
      if( pvtx )
      {
	if( pvtx->display )
	{   
	  turnoffGraphElement( pvtx );
          findGraphEdgebyName2( pvtx->vertex_id, SHUNT_N, &pedge );
	  turnoffGraphElement( pedge );
	}
      }
    }
    else
    {
      manage_gen_and_reac_symbols( pgraphnodenxt, psolnData );
    }

    if( (int)XmToggleButtonGadgetGetState(widget_id("view_menu_solution_pd")))
    {
      pcsolnData = (char *)psolnData;		 /* write the vertex data */
      cnttmp = forEachLink(graph_db, pgraphnodenxt,
                         GRAPH_VERTEX_LINK, writeBusLabel, pcsolnData);
    }
  }
  else
  {
    /* hey this bus just isn't on the diagram */
    /* printf("WARNING: refreshGraph: not found stat %d\n", stat); */
    /* printGraphElement(pgraphnode); */
  }
} /* end refreshBusImage */


void update_edge(pfOutRecord *psolnData)
/***********************************************************************\
* Purpose: locate the branch graphelement and draw flow data on screen
*
* Called by: updateOutput
\***********************************************************************/
{
  GraphElement graphnode;
  GraphElement *pgraphnode;
  GraphElement *pgraphnodenxt;
  DB_STAT stat;

  if(db_valid_db(&graph_db) != D_OKAY)
  {
    printf("Warning: no graph table - processing interupted\n");
    return;
  }

  pgraphnode = &graphnode;

  stat = findGraphEdgebyName( psolnData->line.name1, psolnData->line.name2,
                              &pgraphnodenxt );

  while(D_FOUND == stat )
  {
    if( GraphClassEdgeComplexSection == pgraphnodenxt->class )
    {				 /* found complex edge, now get sub edges */
      checkSolnEdge( pgraphnodenxt, psolnData);
    }
 
    pgraphnode = pgraphnodenxt;			 /** Get next edge **/
    stat = db_keynext(&graph_db, &pgraphnode, &pgraphnodenxt, 
           GRAPH_TYPE_VERTEX_FARVERTEX);
  }
} /* end update_edge */


static int show_overload_flag = False;
int showOverload()
{
  return show_overload_flag;
}






void set_overload_colors( Widget wid, XtPointer tag, XtPointer release)
/*************************************************************************\
*
*
* Called by: Main menu pulldown
\*************************************************************************/
{
  VUIT_Manage("overload_color_legend_form");
  VUIT_Unmanage("kv_color_legend_form");
  set_button_sensivity( "kv_color_pb", True );
  set_button_sensivity( "overload_color_pb", False );
  show_overload_flag = True;
  refresh_solution_data(0,0,0);
}




void set_kv_colors( Widget wid, XtPointer tag, XtPointer release)
/*************************************************************************\
\*************************************************************************/
{
  VUIT_Manage("kv_color_legend_form");
  VUIT_Unmanage("overload_color_legend_form");
  set_button_sensivity( "kv_color_pb", False );
  set_button_sensivity( "overload_color_pb", True );
  show_overload_flag = False;
  refresh_solution_data(0,0,0);
}


int isOverloaded(pfOutRecord *psolnData)
/*************************************************************************\
\*************************************************************************/
{
  static int toggle = 0;
  double crfloam, crratam, ttfloam;
  double crflova, crratva, ttflova;
  double olpct, crpctam, crpctva;
  double crflo, crrat;
  char ratcod[2];
  char ratcod1[2];
  char ovldflg[2];

    crfloam = atof_cnt(psolnData->line.critical_line_load, 
        sizeof(psolnData->line.critical_line_load));
    crratam = atof_cnt(psolnData->line.critical_line_rating, 
        sizeof(psolnData->line.critical_line_rating));
    ttfloam = atof_cnt(psolnData->line.total_line_loading, 
        sizeof(psolnData->line.total_line_loading));
    crflova = atof_cnt(psolnData->line.critical_transformer_load, 
        sizeof(psolnData->line.critical_transformer_load));
    crratva = atof_cnt(psolnData->line.critical_transformer_rating, 
        sizeof(psolnData->line.critical_transformer_rating));
    ttflova = atof_cnt(psolnData->line.total_transformer_loading, 
        sizeof(psolnData->line.total_transformer_loading));
    /* now concatenate overload information */
    if(psolnData->line.critical_line_loading_terminal[0] != MAX_NONE 
       && crratam > (double) 0.0) {
      crpctam = crfloam / crratam * (double)100.0;
    } else {
      crpctam = (double)0.0;
    }
    if(psolnData->line.critical_transformer_loading_terminal[0] != MAX_NONE 
       && crratva > (double) 0.0) {
      crpctva = crflova / crratva * (double)100.0;
    } else {
      crpctva = (double)0.0;
    }

    if(psolnData->line.critical_transformer_loading_terminal[0] != MAX_NONE) {
      if(psolnData->line.critical_line_loading_terminal[0] != MAX_NONE) {
        /* mixed L & T sections in circuit */
        ratcod[0] = '*';
        if(crpctva > crpctam) {
          /* transformer more overloaded */
          ratcod1[0] = 'm';
          crflo = crflova;
          crrat = crratva;
          olpct = crpctva;
        } else {
          /* line more overloaded */
          ratcod1[0] = 'a';
          crflo = crfloam;
          crrat = crratam;
          olpct = crpctam;
        }
      } else {
        /* checking for parallel transformer circuits */
        if(ttflova > crflova) {
          ratcod[0] = '*';
        } else {
          ratcod[0] = psolnData->line.critical_transformer_rating_code[0];
        }
        ratcod1[0] = 'm';
        crflo = crflova;
        crrat = crratva;
        olpct = crpctva;
      }
    } else {
        if(ttfloam > crfloam) {
          ratcod[0] = '*';
        } else {
          ratcod[0] = psolnData->line.critical_line_rating_code[0];
        }
        ratcod1[0] = 'a';
        crflo = crfloam;
        crrat = crratam;
        olpct = crpctam;
    }
    if(olpct >= (double)MAX_LOAD_OVER) {
      ovldflg[0] = ratcod[0];
    }
#ifdef REMOVE
    if(olpct >= (double)MAX_LOAD) {
      sprintf(outx, "\n%.*f", prcsn, crflo);
      strcat(pout, outx);
      if(psolnData->line.critical_transformer_loading_terminal[0] != MAX_NONE
         && psolnData->line.critical_line_loading_terminal[0] != MAX_NONE) {
        strcat(pout, "a");
      }
      strcat(pout, ratcod);
      sprintf(outx, "%.*f", prcsn, crrat);
      strcat(pout, outx);
    }
#endif

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "isOverloaded: olpct=%lf\n", olpct);
  }
  if(olpct > (double)MAX_LOAD_OVER ) {
    return True;
  } else {
    return False; 
  }
}


void timer_blink_proc(XtPointer pdata, XtIntervalId *pinterval)
/*************************************************************************\
\*************************************************************************/
{
  Boolean curval;
  GraphElement *pge = (GraphElement *)pdata;
  Widget wid;
  static unsigned long interval_millisec = 1000;

  wid = pge->wid;
  XtVaGetValues(wid,
                XiNdashOn, &curval,
                NULL);
  XtVaSetValues(wid,
                XiNdashOn, (Boolean)!curval,
                NULL);
  XtAppAddTimeOut( app_context, interval_millisec, timer_blink_proc, pge);
  return;
}






setblink(GraphElement *pge)
/*************************************************************************\
\*************************************************************************/
{
  unsigned long interval_millisec = 1000;
  
  XtAppAddTimeOut( app_context, interval_millisec, timer_blink_proc, pge);
  
}


setkvColor(GraphElement *pedge, pfOutRecord *psolnData)
/**************************************************************************\
*
\**************************************************************************/
{
  Widget kv_color_wid;
  Pixmap color_pixmap;
  Pixel color;
  double dbasekv;
  Dimension thick;

    if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
        fprintf(ipfdbio, "setkvColor: start\n");
    }
    dbasekv = setGraphKv(pedge); 
    kv_color_wid = lookup_kv_wid( dbasekv );
    XtVaGetValues( kv_color_wid, 
                XmNbackground, &color,
                XmNbackgroundPixmap, &color_pixmap,
          NULL);

    if(True == isOverloaded(psolnData)) {
      /* setblink(pedge);  */
      thick = (Dimension)5;
    } else {
      thick = (Dimension)2;
    }

    XtVaSetValues( pedge->wid, 
                       XiNarrowOn, (Boolean)FALSE,
                       XiNlineThickness,  (Dimension)thick,
                       XiNdrawPixel,  (Pixel)color,
                       XiNdrawPixmap, (Pixmap)color_pixmap,
                       NULL);
}

setOverloadColor(GraphElement *pedge, pfOutRecord *psolnData)
/**************************************************************************\
*
\**************************************************************************/
{
  Widget overload_color_wid;
  Pixmap color_pixmap;
  Pixel color;
  double dload_pct;

    if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
       fprintf(ipfdbio, "setOverloadColor: start\n");
    }
    dload_pct = atof_cnt(psolnData->line.total_line_load_pct, 
                       sizeof(psolnData->line.total_line_load_pct));

    overload_color_wid = lookup_pct_wid( dload_pct );
    XtVaGetValues( overload_color_wid, 
                   XmNbackground, &color,
                   XmNbackgroundPixmap, &color_pixmap,
                   NULL);

    XtVaSetValues( pedge->wid, 
                   XiNarrowOn, (Boolean)FALSE,
                   XiNlineThickness,  (Dimension)3,
                   XiNdrawPixel,  (Pixel)color,
                   XiNdrawPixmap, (Pixmap)color_pixmap,
                   NULL);
}



setOverloadColorold(GraphElement *pedge)
/*************************************************************************\
\*************************************************************************/
{
        Widget overload_color_wid;
        Pixmap color_pixmap;
        Pixel color;

        overload_color_wid = widget_id("kv_dc_label");
        XtVaGetValues( overload_color_wid, 
                XmNbackground, &color,
                XmNbackgroundPixmap, &color_pixmap,
                NULL);

        XtVaSetValues( pedge->wid, 
                        XiNarrowOn, (Boolean)FALSE,
                        XiNlineThickness,  (Dimension)5,
/***
                        XmNarmColor,  (Pixel)color,
                        XmNarmPixmap, (Pixmap)color_pixmap,
**/
          NULL);
}


void set_solution_status( int status )
/*************************************************************************\
* Purpose: displays powerflow status on main GUI display
*
* Called by: ipc_synch_rw (ipc_cb.c)
\*************************************************************************/
{ 
  static int old_status = -1;

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "set_solution_status: status=%d\n", status);
  }

  if( status == old_status ) return;

  enable_pushbutton( "process_menu_cascade"         );

  switch (status)
  {

    case 0:
    case 1:
      disable_pushbutton( "process_menu_cascade" );
      VUIT_Manage  ( "no_case_loaded_label"      );
      VUIT_Unmanage( "unsolved_case_label"       );
      VUIT_Unmanage( "successful_solution_label" );
      VUIT_Unmanage( "failed_solution_label"     );
    break;

    case 2:
    case 3:
    case 4:
      VUIT_Unmanage( "no_case_loaded_label"      );
      VUIT_Manage  ( "unsolved_case_label"       );
      VUIT_Unmanage( "successful_solution_label" );
      VUIT_Unmanage( "failed_solution_label"     );
    break;

    case 5:
    case 6:
      VUIT_Unmanage( "no_case_loaded_label"      );
      VUIT_Unmanage( "unsolved_case_label"       );
      VUIT_Manage  ( "successful_solution_label" );
      VUIT_Unmanage( "failed_solution_label"     );
    break;

    case 7:
    case 8:
      VUIT_Unmanage( "no_case_loaded_label"      );
      VUIT_Unmanage( "unsolved_case_label"       );
      VUIT_Unmanage( "successful_solution_label" );
      VUIT_Manage  ( "failed_solution_label"     );
    break;

    default:
    printf("***WARNING*** from set_solution_status (pf_cb.c)\n");
    printf("      Integer status param (%d) should be 0-6.\n", status );

  }
  old_status = status;

}



setSolnDataArrow(GraphElement *pedge, pfOutRecord *psolnData)
/**************************************************************************\
* purpose:
* Author: Dan Clark
*
* Called by: checkSolnEdge
\**************************************************************************/
{
    double dP_in;	/* double float of character value from ipf */
    double dQ_in;
    double dP_out;
    double dQ_out;
    double dline_load;
    double dtrans_load;
    double load_pct;
    char csection_cnt[32];
    Boolean arrowOrient = FALSE;	/* direction to point arrow */
    char out[256];			/* output string for edge text */
    char *pout = &out[0];
    char outx[256];
    char outa[256];
    char outb[256];
    XmString x_str;			/* x fromatted str for edge */
    int prt_opt;
    int prt_detail_opt;
    int prt_tapcomp_opt;
    Boolean trans_sym;
    int prcsn = 1;
    double tpin, tqin, tpout, tqout;
    int section_cnt;
    char mvaend;
    double olpct, line_load_pct, tran_load_pct;
    double dP_loss, dQ_loss;
    double crpctam, crfloam, crratam;
    double crpctva, crflova, crratva;
    double ttfloam, ttflova;
    double crflo, crrat;
    char ratcod[2], ratcod1[2], ovldflg[2];
    double pctcomp, tap1kv, tap2kv;
    int line2;
    Widget prcsn_wid;

  memset(ratcod, '\0', sizeof(ratcod));
  memset(ratcod1, '\0', sizeof(ratcod));
  memset(ovldflg, '\0', sizeof(ratcod));

  if(NULL == pedge->wid) {
      printf("Programmer warning - wid null - cannot set soln data\n");
      return;
  }
  dP_in  = atof_cnt(psolnData->line.P_in, sizeof(psolnData->line.P_in));
  dQ_in  = atof_cnt(psolnData->line.Q_in, sizeof(psolnData->line.Q_in));
  dP_out = atof_cnt(psolnData->line.P_out, sizeof(psolnData->line.P_out));
  dQ_out = atof_cnt(psolnData->line.Q_out, sizeof(psolnData->line.Q_out));
  load_pct = atof_cnt(psolnData->line.total_line_load_pct,
		 sizeof(psolnData->line.total_line_load_pct));

  get_branch_solution_opt(&prt_opt);
#define SHOW_PQ_FLOW 1<<0
#define SHOW_MVAI 1<<1
#define SHOW_LOSS 1<<2
#define SHOW_INTER 1<<3
#define SHOW_COORD 1<<4
#define SHOW_LOAD_PERCENT 1<<5

  get_branch_detail_opt(&prt_detail_opt);
#define PRT_P_SEND 1<<0
#define PRT_Q_SEND 1<<1
#define PRT_P_RECEIVE 1<<2
#define PRT_Q_RECEIVE 1<<3

  get_branch_other_opt(&prt_tapcomp_opt);
#define PRT_TAPS 1<<0
#define PRT_COMP 1<<1
#define PRT_OUTAGE 1<<2

  prcsn = read_prcsn_radiobuttons();

  if(dP_in < (double)0.0)
    arrowOrient = FALSE;
  else
    arrowOrient = TRUE;
 
  sprintf(pout,"");
  sprintf(outa,"");
  sprintf(outb,"");
  if( (prt_opt & SHOW_PQ_FLOW) || (prt_opt & SHOW_INTER) ) /* print pq flow */
  {
    if(dP_in < (double)0.0) {
      tpin  = -dP_out;
      tqin  = -dQ_out;
      tpout = -dP_in;
      tqout = -dQ_in;
    }
    else
    {
      tpin  = dP_in;
      tqin  = dQ_in;
      tpout = dP_out;
      tqout = dQ_out;
    }
    if(prt_detail_opt & PRT_P_SEND) {
      sprintf(outx, "%.*f", prcsn, tpin);
      strcat(outa,outx);
    }

    if(prt_detail_opt & PRT_Q_SEND) {
      sprintf(outx, "(%.*f)", prcsn, tqin);
      strcat(outa,outx);
    }

    if(prt_detail_opt & PRT_P_RECEIVE || prt_detail_opt & PRT_Q_RECEIVE) {
        strcat(outb, "[");
    }

    if(prt_detail_opt & PRT_P_RECEIVE) {
      sprintf(outx, "%.*f", prcsn, tpout);
      strcat(outb,outx);
    }

    if(prt_detail_opt & PRT_Q_RECEIVE) {
      sprintf(outx, "(%.*f)", prcsn, tqout);
      strcat(outb,outx);
    }

    if(prt_detail_opt & PRT_P_RECEIVE || prt_detail_opt & PRT_Q_RECEIVE) {
        strcat(outb, "]");
    }

    if(dP_in < (double) 0.0) {
      strcat(pout, outb);
      strcat(pout, outa);
    }
    else
    {
      strcat(pout, outa);
      strcat(pout, outb);
    }
      /* end PQ flow format */
  }
  else if(prt_opt & SHOW_MVAI)
  {
    if(dP_in < (double)0.0)
    {
      if(psolnData->line.critical_line_loading_terminal[0] == MAX_NEAREND)
      {
        psolnData->line.critical_line_loading_terminal[0] = MAX_FAREND;
      }
      else if(psolnData->line.critical_line_loading_terminal[0] == MAX_FAREND)
      {
        psolnData->line.critical_line_loading_terminal[0] = MAX_NEAREND;
      }
      if(psolnData->line.critical_transformer_loading_terminal[0] ==
			 MAX_NEAREND)
      {
        psolnData->line.critical_transformer_loading_terminal[0] = MAX_FAREND;
      }
      else if(psolnData->line.critical_transformer_loading_terminal[0] ==
			 MAX_FAREND)
      {
        psolnData->line.critical_transformer_loading_terminal[0] = MAX_NEAREND;
      }
    } 

    sprintf(pout, "<");
    if(psolnData->line.critical_line_loading_terminal[0] != MAX_NONE)
    {
      dline_load = atof_cnt(psolnData->line.critical_line_load,  
                              sizeof(psolnData->line.critical_line_load));
      sprintf(outx, "%.*lf", prcsn, dline_load);
      strcat(pout, outx);
      if(psolnData->line.critical_transformer_loading_terminal[0] != MAX_NONE)
      {
        strcat(pout, "a");
      } 
    }
    if(psolnData->line.critical_transformer_loading_terminal[0] != MAX_NONE)
    {
      dline_load = atof_cnt(psolnData->line.critical_transformer_load,  
                           sizeof(psolnData->line.critical_transformer_load));
      sprintf(outx, "%.*lf", prcsn, dline_load);
      strcat(pout, outx);
      if(psolnData->line.critical_line_loading_terminal[0] != MAX_NONE) {
        strcat(pout, "m");
      } 
    }
      /* the line component override tx component in determining wether 
      *  max flow is at near or far end of missed L & T sections  */

    if(psolnData->line.critical_line_loading_terminal[0] != MAX_NONE)
        mvaend = psolnData->line.critical_line_loading_terminal[0];
    else
        mvaend = psolnData->line.critical_transformer_loading_terminal[0];
 
    if((dP_in >= (double)0.0 && mvaend == MAX_FAREND) ||
       (dP_in < (double)0.0 && mvaend == MAX_NEAREND)) {
        strcat(pout, "R");
      }
    strcat(pout, ">");
    /* end of mva & i line flows */
  }
  else if(prt_opt & SHOW_LOSS)
  {
      /* print losses */
    dP_loss = atof_cnt(psolnData->line.P_loss, sizeof(psolnData->line.P_loss));
    dQ_loss = atof_cnt(psolnData->line.Q_loss, sizeof(psolnData->line.Q_loss));
    if(prt_detail_opt & PRT_P_SEND) {
      sprintf(outx, "%.*f", 2, dP_loss);
      strcat(pout,outx);
    }
    if(prt_detail_opt & PRT_Q_SEND) {
      sprintf(outx, "(%.*f)", 2, dQ_loss);
      strcat(pout,outx);
    }
  }
  else if(prt_opt & SHOW_COORD)
  {
      /* do not print solution data */
  }
  else if(prt_opt & SHOW_LOAD_PERCENT)
  {
     /* show load percent data */
    tran_load_pct = atof_cnt(psolnData->line.total_transformer_load_pct, 
        sizeof(psolnData->line.total_transformer_load_pct));
    line_load_pct = atof_cnt(psolnData->line.total_line_load_pct, 
        sizeof(psolnData->line.total_line_load_pct));
    if(psolnData->line.critical_transformer_loading_terminal[0] != MAX_NONE)
    {
      olpct = tran_load_pct;
      if(psolnData->line.critical_line_loading_terminal[0] != MAX_NONE &&
           line_load_pct > tran_load_pct )
      {
        olpct = line_load_pct;
        sprintf(outx, "%.*f", prcsn, olpct);
        strcat(pout,outx);
        strcat(pout, "%a");
      }
      else
      {
        sprintf(outx, "%.*f", prcsn, olpct);
        strcat(pout,outx);
        strcat(pout, "%");
      }
    }
    else
    {
      olpct = line_load_pct;
      sprintf(outx, "%.*f", prcsn, olpct);
      strcat(pout,outx);
      strcat(pout, "%");
    }
  }

  /*** need to put taps or %compensation logic here ***/
  line2 = 0;
  if(prt_tapcomp_opt & PRT_COMP)
  {
    if(psolnData->line.code[0] == 'L' || psolnData->line.code[0] == 'E')
    {
      pctcomp = atof_cnt(psolnData->line.tap1kv_pctcomp, 
          sizeof(psolnData->line.tap1kv_pctcomp));
      if(pctcomp > (double)0.1)
      {
        line2 = 1;
        sprintf(outx, "\n%.*f", prcsn, pctcomp);
        strcat(pout, outx);
        strcat(pout, "% ");
      }
    } 
  }
    /* put first half of tap before count of parallel lines */
  if(prt_tapcomp_opt & PRT_TAPS)
  {
    if(psolnData->line.code[0] == 'T' )
    {
      tap1kv = atof_cnt(psolnData->line.tap1kv_pctcomp, 
          sizeof(psolnData->line.tap1kv_pctcomp));
      if(tap1kv > (double)0.0) {
          line2 = 1;
          sprintf(outx, "\n%.*f", prcsn+1, tap1kv);
          strcat(pout, outx);
          strcat(pout, "  ");
      }
    }
  }

  if(1 < atoi_cnt(psolnData->line.section, sizeof(psolnData->line.section))) 
  {
    section_cnt = atoi_cnt(psolnData->line.section, 
       sizeof(psolnData->line.section));
    if(!line2) {
      sprintf(outx, "\n");
      strcat(pout, outx);
      line2 = 1;
    }
    sprintf(csection_cnt, "[%d]", section_cnt);
    strcat(pout, csection_cnt);
  }

  /* put second half of tap after count of parallel lines */
  if(prt_tapcomp_opt & PRT_TAPS) {
    if(psolnData->line.code[0] == 'T' )
    {
      tap2kv = atof_cnt(psolnData->line.tap2kv,sizeof(psolnData->line.tap2kv));
      if(tap2kv > (double)0.0)
      {
        if(!line2)
	{
          sprintf(outx, "\n");
          strcat(pout, outx);
          line2 = 1;
        }
        sprintf(outx, "  %.*f", prcsn+1, tap2kv);
        strcat(pout, outx);
      }
    }
  }

  crfloam = atof_cnt( psolnData->line.critical_line_load, 
	      sizeof( psolnData->line.critical_line_load));
  crratam = atof_cnt( psolnData->line.critical_line_rating, 
	      sizeof( psolnData->line.critical_line_rating));
  ttfloam = atof_cnt( psolnData->line.total_line_loading, 
	      sizeof( psolnData->line.total_line_loading));
  crflova = atof_cnt( psolnData->line.critical_transformer_load, 
	      sizeof( psolnData->line.critical_transformer_load));
  crratva = atof_cnt( psolnData->line.critical_transformer_rating, 
	      sizeof( psolnData->line.critical_transformer_rating));
  ttflova = atof_cnt( psolnData->line.total_transformer_loading, 
	      sizeof( psolnData->line.total_transformer_loading));

    /* now concatenate overload information */
  if(psolnData->line.critical_line_loading_terminal[0] != MAX_NONE &&
        crratam > (double) 0.0)
    crpctam = crfloam / crratam * (double)100.0;
  else
    crpctam = (double)0.0;

  if(psolnData->line.critical_transformer_loading_terminal[0] != MAX_NONE 
		       && crratva > (double) 0.0)
    crpctva = crflova / crratva * (double)100.0;
  else
    crpctva = (double)0.0;

  if(psolnData->line.critical_transformer_loading_terminal[0] != MAX_NONE)
  {
    if(psolnData->line.critical_line_loading_terminal[0] != MAX_NONE)
    {
        /* mixed L & T sections in circuit */
      ratcod[0] = '*';
      if(crpctva > crpctam)
      {
        /* transformer more overloaded */
        ratcod1[0] = 'm';
        crflo = crflova;
        crrat = crratva;
        olpct = crpctva;
      }
      else
      {
          /* line more overloaded */
        ratcod1[0] = 'a';
        crflo = crfloam;
        crrat = crratam;
        olpct = crpctam;
      }
    }
    else
    {
      /* checking for parallel transformer circuits */
      if(ttflova > crflova)
        ratcod[0] = '*';
      else
        ratcod[0] = psolnData->line.critical_transformer_rating_code[0];

      ratcod1[0] = 'm';
      crflo = crflova;
      crrat = crratva;
      olpct = crpctva;
    }
  }
  else
  {
    if(ttfloam > crfloam)
      ratcod[0] = '*';
    else
      ratcod[0] = psolnData->line.critical_line_rating_code[0];

      ratcod1[0] = 'a';
      crflo = crfloam;
      crrat = crratam;
      olpct = crpctam;
  }
  if(olpct >= (double)100.0) {
      ovldflg[0] = ratcod[0];
  }

  if(olpct >= (double)90.0)
  {
    sprintf(outx, "\n%.*f", prcsn, crflo);
    strcat(pout, outx);
    if(psolnData->line.critical_transformer_loading_terminal[0] != MAX_NONE
         && psolnData->line.critical_line_loading_terminal[0] != MAX_NONE) {
      strcat(pout, "a");
    }
    strcat(pout, ratcod);
    sprintf(outx, "%.*f", prcsn, crrat);
    strcat(pout, outx);
  }

#ifdef REMOVE
    /* now concatenate overload information */
    /* if(load_pct >= MAX_LOAD_OVER)	    */
/***
      if(psolnData->line.critical_line_rating_code[0] == '*') {
        dmax_rating = atof_cnt(psolnData->line.critical_line_rating, 
                            sizeof(psolnData->line.critical_line_rating));
        sprintf(outx, "\n%4.0lf%1.1s%4.0lf", load_pct,
		 &psolnData->line.critical_line_rating_code[0], dmax_rating);
        strcat(pout,outx);
      } else 
***/
  if(load_pct >= MAX_LOAD)
  {
    dmax_rating = atof_cnt(psolnData->line.critical_line_rating, 
                    sizeof(psolnData->line.critical_line_rating));
    sprintf(outx, "\n%1.1s%4.0lf", 
          &psolnData->line.critical_line_rating_code[0], dmax_rating);
    strcat(pout,outx);
  } 
    /* show mva or I for overloads */
    /* if(load_pct >= MAX_LOAD_OVER)  */
  if(psolnData->line.critical_line_rating_code[0] == '*')
  {
    dmax_mva_i = atof_cnt(psolnData->line.critical_line_load,  
                              sizeof(psolnData->line.critical_line_load));
    dmax_rating = atof_cnt(psolnData->line.critical_line_rating, 
                            sizeof(psolnData->line.critical_line_rating));
    sprintf(outx, "\n%4.0lf%1.1s%3.0lf", 
      load_pct, &psolnData->line.critical_line_rating_code[0], dmax_rating);
    strcat(pout, outx);
  }
  else if(load_pct >= MAX_LOAD)
  {
    dmax_mva_i = atof_cnt(psolnData->line.critical_line_load,  
                              sizeof(psolnData->line.critical_line_load));
    dmax_rating = atof_cnt(psolnData->line.critical_line_rating, 
                            sizeof(psolnData->line.critical_line_rating));
    sprintf(outx, "\n%4.0lf%1.1s%3.0lf", 
    dmax_mva_i, &psolnData->line.critical_line_rating_code[0], dmax_rating);
    strcat(pout, outx);
  } 
#endif

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask))
  {
    fprintf(ipfdbio, "setSolnDataArrow: soln \"%s\" data=\"%s\", pedge :\n", 
        &psolnData->line.code[0], pout);
    printGraphElement(pedge);
  }

  x_str =  XmStringCreateLtoR(pout, XmSTRING_DEFAULT_CHARSET);

  trans_sym = istransformer(pedge);

  XtVaSetValues( pedge->wid, 
	        XiNarrowOn, (Boolean)TRUE,
        	XiNarrowDirection, (Boolean)arrowOrient,
        	XiNtransformerOn, (Boolean)trans_sym,
        	XiNlabelString, x_str, 
        	XiNlabelOn, (Boolean)TRUE, 
        	NULL);

}


int read_prcsn_radiobuttons()
/**************************************************************************\
* Purpose: Check the radio button positions (if managed) to determine
*	   how many decimal places to draw line data.
*
* Author: Bill Rogers		Feb 16 1995
\**************************************************************************/
{
  Widget prcsn_wid;

	/* NOTE: use HashLookup because widget_id gives unwanted warnings */
  if(HashLookup("display_menu_dec_place_2_rb", &prcsn_wid ) != False)
  {
    if(prcsn_wid != FALSE)
    {
      if (XmToggleButtonGetState( prcsn_wid ) )
        return(2);
    }
    prcsn_wid = widget_id( "display_menu_dec_place_1_rb" );
    if(prcsn_wid != FALSE) 
    {
      if (XmToggleButtonGetState( prcsn_wid ) )
        return(1);
    }
  }
  return(0);
}




int read_bus_prcsn_radiobuttons()
/**************************************************************************\
* Purpose: Check the radio button positions (if managed) to determine
*	   how many decimal places to draw line data.
*
* Author: Bill Rogers		Feb 16 1995
\**************************************************************************/
{
  Widget prcsn_wid;

	/* NOTE: use HashLookup because widget_id gives unwanted warnings */
  if(HashLookup("display_menu_bus_dec_pl_2_rb", &prcsn_wid ) != False)
  {
    if(prcsn_wid != FALSE)
    {
      if (XmToggleButtonGetState( prcsn_wid ) )
        return(2);
    }
    prcsn_wid = widget_id( "display_menu_bus_dec_pl_1_rb" );
    if(prcsn_wid != FALSE) 
    {
      if (XmToggleButtonGetState( prcsn_wid ) )
        return(1);
    }
  }
  return(0);
}


int checkSolnEdge( GraphElement *pcmplx, pfOutRecord *psolnData )
/***************************************************************************\
* Purpose: Find all sub-edges, set line colors each each branch subedge
*	   Set subedge for which to set labelOn (and show flow data)
*
* Called by: update_edge (only caller)
\***************************************************************************/
{
  DB_STAT stat_link;
  GraphLink *pgraphlink; 
  GraphElement *pedge; 
  GraphElement *pedgehold; 
  GraphElement *bends[PSMAXBENDS + 2];
  int bendcnt;

  if( GraphTypeEdge		   == pcmplx->type && 
      GraphClassEdgeComplexSection == pcmplx->class )
  { 
    /** found a complex edge check for first or second child */
    stat_link = ll_first(&graph_db, &pcmplx, &pgraphlink, GRAPH_EDGE_LINK);

    if (stat_link != D_FOUND)
    {
      printf("ERROR: Note: no edge links for cmplex edge\n");
      printGraphElement(pcmplx);
      return;
    }

    while(D_FOUND == stat_link)
    {
      pedge = (GraphElement *)pgraphlink->graphnode;
      if( GraphTypeEdge		== pedge->type    && 
          GraphClassEdgeSection == pedge->class   && 
          GraphDisplayOn	== pedge->display )
      {	
	if( NULL != pedge->wid )
	{
	  if(True == showOverload() )
            setOverloadColor(pedge, psolnData);
	  else
            setkvColor(pedge, psolnData);
	}
  
        /* DO NOT FREE line string XmStringFree(x_str); */
      }
      else
      {
        printf("WARNING: no edge on for this link parent/child \n");
        printGraphElement(pcmplx);
      }
      stat_link = ll_next(&pgraphlink, &pgraphlink);
    } /* end loop  through all bending points */

    /* LOOP to find the edge which flow data should go on (dply_seg) */
 
    if ( pcmplx->dply_seg >= 0 )	/* (if -1 then don't display) */
    {
      bendcnt = createBendArray( bends, pcmplx, GraphTypeEdge );
		/* returns list of bend edges in bends array */
		/* fills array from ZERO to bendcnt-1   */

      if( pcmplx->dply_seg <= bendcnt )
      {
        pedgehold = bends[pcmplx->dply_seg-1];
      }
      else
      {
        printf("WARNING - from checkSolnEdge \n" );
        printf(" Bad value (%d) for edge display - only %d edges! (Default to ",
					 pcmplx->dply_seg, bendcnt );
        if( bendcnt <= 1 )
	{
          pedgehold = bends[0];
	  printf("1)\n");
	}
        else
	{
          pedgehold = bends[1];
	  printf("2)\n");
	}
      }

      if(NULL == pedgehold)
      {
        printf("PROGAMMER ERROR - cannot find sub-edge for solution data \n");
        printGraphElement(pcmplx);
        pedgehold = pedge;		/* use last edge anyway */
      }

      if( (int)XmToggleButtonGadgetGetState(widget_id("view_menu_solution_pd")))
        setSolnDataArrow(pedgehold,psolnData);
    }
  }
  else
  {
    printf("WARNING: not a complex edge link\n");
    printGraphElement(pcmplx);
  } /* end complex section */
}


int istransformer(GraphElement *pel)
/***********************************************************************\
*
\***********************************************************************/
{
#define TRANSFORMER 'T'
  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "istransformer: start\n");
  }
  if(GraphTypeEdge != pel->type)	 return False;

  if(atof_cnt_zero(&pel->vertex_id[8], 4) !=
     atof_cnt_zero(&pel->farvertex_id[8], 4) ) {
    return True;
  } 
  return False;
}


typedef struct outBox {
 char name[8];
 char volt[4];
 char angle[4];
} OutBox;

#define SHOW_KV 1L<<0
#define SHOW_PERUNIT 1L<<1



int writeBusLabel( GraphElement *pchild, GraphElement *pparent,
	       GraphLink *plink, char *pcsolnData)
/***********************************************************************\
* Purpose: Create bus name/volt/gen/cap/react label with solution data.
*
* Called by: refreshBusImage  (above)
\***********************************************************************/
{
  char n_str[32],v_str[32],g_str[32],r_str[32] ;
  char out[128];
  char *pout = &out[0];
  char v_pos;
  int ht,wd;
  int prcsn;
  PSCoordFileRecord  *coordrec_p;       /* allocate 1 space for ptr  */
  GraphElement *pe;
  GraphElement *pnamevtx;

  pfOutRecord *psolnData = (pfOutRecord *)pcsolnData;

#ifdef WORKS 
  XmString label_cs;
  XmString xnew;
  XmString xnew2;
  XmString xnew3;
  XmString xsep;
#endif

  XmString x_str;
  char *pname;
  int vtype;
  double voltage, base, angle;
  double P_gen, Q_gen, B_shunt_used, B_shunt_sched;

  prcsn = read_bus_prcsn_radiobuttons();

  if((GraphTypeVertex == pchild->type)       && 
     (GraphClassVertexName == pchild->class) && 
     (GraphDisplayOn == pchild->display))
  {
    /** found a name vertex & the name is on the screen */

    voltage = atof_cnt(&psolnData->acbus.voltage[0], 
                            sizeof(psolnData->acbus.voltage));
    angle = atof_cnt(&psolnData->acbus.angle[0], 
                            sizeof(psolnData->acbus.angle));

    strcpy( n_str, busNameStr( pparent ) );

    if( pparent->hide == NO_HIDE )
    {
      if( sol_opt == SHOW_PERUNIT )	 /* printf("Set to PerUnit\n"); */
      {
        base = atof_cnt(&psolnData->acbus.base[0], 
                            sizeof(psolnData->acbus.base));
        voltage /= base;
      }

      if( sol_opt == SHOW_PERUNIT )
        sprintf(v_str, "%.3lf %4.1lf", voltage, angle);
      else
        sprintf(v_str, "%.*lf %.*lf", prcsn, voltage, prcsn, angle);
	

      getVertexType(psolnData, &vtype);
	    /** check if generator data is available and add to string **/
      if(vtype & VTGENERATOR &&
	 XmToggleButtonGadgetGetState( widget_id("print_no_gen_rb") ) == False )
      {
        P_gen = atof_cnt(&psolnData->acbus.P_gen[0], 
                           sizeof(psolnData->acbus.P_gen));
        Q_gen = atof_cnt(&psolnData->acbus.Q_gen[0], 
                            sizeof(psolnData->acbus.Q_gen));
        sprintf(g_str, "G {%4.*lf %4.*lf}", prcsn, P_gen, prcsn, Q_gen);
      } 
      else
      {
        g_str[0] = '\0';
      }

      /** check if reactor/capacitor data is available and add to string **/
      r_str[0] = '\0';
      if((FALSE == isDCBus(psolnData)))
      {
        if( vtype & VTREACTOR &&
	 XmToggleButtonGadgetGetState(widget_id("print_no_shunt_rb"))==False )
        {
          B_shunt_used = atof_cnt(&psolnData->acbus.B_shunt_react_used[0], 
                            sizeof(psolnData->acbus.B_shunt_react_used));
	  B_shunt_sched = atof_cnt(&psolnData->acbus.B_shunt_react_sched[0], 
                            sizeof(psolnData->acbus.B_shunt_react_sched));
	  sprintf(r_str, "R <%.*lf %.*lf>", prcsn, B_shunt_sched,							    prcsn, B_shunt_used);
        }

        else if(  vtype & VTCAPACITOR &&
	 XmToggleButtonGadgetGetState(widget_id("print_no_shunt_rb"))==False )
        {
  	  B_shunt_used = atof_cnt(&psolnData->acbus.B_shunt_cap_used[0], 
                            sizeof(psolnData->acbus.B_shunt_cap_used));
  	  B_shunt_sched = atof_cnt(&psolnData->acbus.B_shunt_cap_sched[0], 
                            sizeof(psolnData->acbus.B_shunt_cap_sched));
	  sprintf(r_str, "C <%.*lf %.*lf>", prcsn, B_shunt_sched,
					    prcsn, B_shunt_used );
        } 

      }
	
    }

    coordrec_p = coord_db_bus_search( &psolnData->acbus.name[0] );
		/* locate coord file record of this bus name */

    if( coordrec_p )
    {
      v_pos = *coordrec_p->bus.v_flag;
    }
    else
    {		/* no coord file rec - vertex prob recently created by user */
      v_pos = '0';
    }

    if( pparent->hide == HIDE )
    {
      sprintf( pout, "%s", n_str );
    }
    else
    {
    switch( v_pos )
    {
      case '1':		/* over name */
	sprintf( pout, "%s", v_str );
	if( g_str[0] != '\0' ) 
          sprintf( pout, "%s\n%s", pout, g_str );
	if( r_str[0] != '\0' ) 
          sprintf( pout, "%s\n%s", pout, r_str );
	
        sprintf( pout, "%s\n%s", pout, n_str );
      break;

      case '2':		/* right of name */
	sprintf( pout, "%s %s %s %s", n_str, v_str, g_str, r_str );
      break;

      case '3':		/* below name */
      case '0':
      case ' ':
	sprintf( pout, "%s\n%s", n_str, v_str );
	if( g_str[0] != '\0' ) 
          sprintf( pout, "%s\n%s", pout, g_str );
	if( r_str[0] != '\0' ) 
          sprintf( pout, "%s\n%s", pout, r_str );
      break;

      case '4':		/* left of name */
	sprintf( pout, "%s %s %s %s", v_str, g_str, r_str, n_str );
      break;

      case '5':		/* no voltage */
	sprintf( pout, "%s", n_str );
      break;

      case '6':		/* voltage, no name */
	sprintf( pout, "%s", v_str );
      break;

    }
    }

    x_str =  XmStringCreateLtoR(pout, XmSTRING_DEFAULT_CHARSET);
    get_str_box_pixel_size( pout, &wd, &ht );

    XtVaSetValues( pchild->wid, XmNlabelString, x_str,
				XmNheight, ht,
				XmNwidth,  wd, NULL);

    /*if( v_pos == '1' )*/
      position_gadget_wrt( pchild );  /* in case of volts-over-name adj */

    XmStringFree(x_str);

#ifdef WORKS 
    /** this is an example that will help to create multifont strings **/
    XtVaGetValues(pchild->wid, XmNlabelString, &label_cs, NULL);
    xsep = XmStringSeparatorCreate();
    xnew = XmStringConcat(label_cs, xsep);
    sprintf(pout, "%9.9s", &psolnData->volt[0]);
    xnew2 = XmStringCreate(pout, XmSTRING_DEFAULT_CHARSET);
    xnew3 = XmStringConcat(xnew, xnew2);
    XtVaSetValues( pchild->wid, XmNlabelString, xnew3, NULL);
    XmStringFree(xnew);
    XmStringFree(xnew2);
    /* may not be able to free this one XmStringFree(xnew3); */
#endif

  }
  return(0);
}


char *busNameStr( GraphElement *ge )
/**************************************************************************\
* Purpose: Get the proper name style
*
* Called by:	clear_solution_data
*		writeBusLabel
*		manageVertex (vertex.c)
\**************************************************************************/
{
  GraphElement *pnamevtx;
  char bus_str[20];

  get_bus_name_solution_opt(&name_opt);

  switch( name_opt )
  {
    case SHOW_NAME_ABBR:

      if( GraphClassVertexName == ge->class )
	pnamevtx = ge;
      else if( GraphClassVertexBus == ge->class )
        findVertxLinkedToVertx( &pnamevtx, ge, GraphClassVertexName );
      else
	printf("ERORR in call to busNameStr (pf_cb.c)\n");

      if( strncmp( pnamevtx->farvertex_id, "        ", 8 ))
        sprintf(bus_str, "%8.8s", &pnamevtx->farvertex_id[0] );
      else
        sprintf(bus_str, "%8.8s", &pnamevtx->vertex_id[0] );

    break;

    case SHOW_NAME_FULL_NOKV:
      sprintf(bus_str, "%8.8s", &ge->vertex_id[0] );
    break;

    case SHOW_NAME_FULL:
     sprintf(bus_str, "%12.12s", &ge->vertex_id[0] );
    break;
  }

  return &bus_str[0];
}




Boolean send_pf_file_name( char *str, XtPointer tag )
/*********************************************************************\
* Purpose: General purpose send file name to powerflow.
*	   Cut up from Dan Clarks old pfoldbase_cb
*
*       Author: Bill Rogers       Mar, 1993
*
\*********************************************************************/
{
  char  *outbuf = outbuf_array;
  char  *str_rtn = inbuf_array;
  char base_input_file_name[81];        /* name of input file */

  sprintf(base_input_file_name, "%s", tag);

  sprintf(outbuf, "/%s,file=%s\n%s\n", str, base_input_file_name, ENDOFMSG);

  return( (Boolean)ipc_synch_rw(str_rtn, outbuf) );

}




Boolean send_pf_save_file_name(char *type, char *file_name)
/*********************************************************************\
* Purpose: send save file name to powerflow.
*
*       Author: Dan Clark May, 1993
*
\*********************************************************************/
{
  char  *outbuf = outbuf_array;
  char  *str_rtn = inbuf_array;

  sprintf(outbuf, "/SAVE_FILE, TYPE = %s, FILE = %s\n%s\n", 
     type, file_name, ENDOFMSG);

  return( (Boolean)ipc_synch_rw(str_rtn, outbuf) );

}



/*** these next three functions may be obsolete ***/
/*** setStrtoBus, pfgetbus, pfgetoutbus ***/
#define MAXBUSREC 133 
setStrtoBus(char *line, char *outstr)
{
  if(isbus((pfOutRecord *)line))
  {
    strncpy(outstr, line, MAXBUSREC);
    stringPart(outstr, outstr, 0, 132); 
  }
}


int pfgetbus(char *outstr, char *instr)
/*** this function may be obsolete ***/
{

  char *pfcmdstr = outbuf_array;
  char *pfdataret = inbuf_array;
  char *psubstr;
  char *pnamestr;

  char buskey_array[256];
  char *buskey = buskey_array;
  char busname[32];
  int cnt;

/* should be sizeof wscc bus structure */

  strncpy(buskey, instr, 19);
  buskey[19] = '\0';
  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "pfgetbus: buskey=\"%s\"\n", buskey);
  }
  sprintf(pfcmdstr, "/get_data,type=input\n%s\n%s\n", buskey, ENDOFMSG);
  /* printf("pfcmdstr = %s\n", pfcmdstr); */

  ipc_synch_rw(pfdataret, pfcmdstr);
  /* printf("pfdataret = %s\n", pfdataret); */

  cnt = forEachipfLine(pfdataret, setStrtoBus, outstr);

  return(0);
}


int pfgetoutbus(char *outstr, char *instr)
{
  char *pfcmdstr = outbuf_array;
  char *pfdataret = inbuf_array;
  char *psubstr;
  char *pnamestr;

  char buskey_array[256];
  char *buskey = buskey_array;
  char busname[32];
  int cnt;

/* should be sizeof wscc bus structure */

  strncpy(buskey, instr, 19);
  buskey[19] = '\0';
  /* printf("buskey = %s\n", buskey); */

  sprintf(pfcmdstr, "/get_data,type=output\n%s\n%s\n", buskey, ENDOFMSG);
  /* printf("pfcmdstr = %s\n", pfcmdstr); */

  ipc_synch_rw(pfdataret, pfcmdstr);
  /* printf("pfdataret = %s\n", pfdataret); */

  cnt = forEachipfLine(pfdataret, setStrtoBus, outstr);

  return(0);
}


int getbus(char *bus_string, char *key)
/*************************************************************************\
*  Purpose: get bus string from the wscc data read from gui 
*
\*************************************************************************/
{

  PFBASERECORD busvalrecord;      /* ptr to key for bus search */
  PFBASERECORD *busvalrec;      /* ptr to key for bus search */
  PFBASERECORD *nextbusrec;     /* ptr returned for next bus rec */

  DB_STAT statbus;      /* status returned for bus queries of db */

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "getbus: start\n");
  }
  /** retrieve the first Bus record **/
  busvalrec = &busvalrecord;
  strncpy(busvalrec->type, key, 20);
  statbus = db_search(&base_db,&busvalrec, &nextbusrec, BSE_TYPE_NAME1_BASE1);

  if(statbus != D_FOUND)
  {
     printf("Warning: no busses found in table\n");
     strncpy(bus_string, key, 20);
  }
  else
  {
     strncpy(bus_string, nextbusrec->type, sizeof(PFBASERECORD));
  }
  /* printf("bus_string = %s\n", bus_string); */

  return(0);
}


void pfget_solution_params( Widget w, XtPointer adata, XtPointer r )
/*************************************************************************\
*   Purpose: When solution dialog pops up, ask powerflow for the
* 	     solution params and put them in the dialog box.
*
\*************************************************************************/
{
  extern int createRecPtrList();

  char *word[20],
       *limit1 = "#",
       *limit2 = ",= \t\n";
  char *lines[MAXRECS];
  char *pfcmdstr = outbuf_array;
  char *pfdataret = inbuf_array;
  int rtncde;
  int cnt, numrecs;
  int nwrd;
  int ln;

  sprintf(pfcmdstr, "/GET_DATA, TYPE = SOL_PAR\n%s\n", ENDOFMSG );
  rtncde = ipc_synch_rw(pfdataret, pfcmdstr);

  numrecs = createRecPtrList( lines, MAXRECS, pfdataret );

  if( numrecs > 0 )
  {
    for ( cnt = 0; cnt < numrecs; cnt++ )
    {
      if( !strncmp( lines[cnt], ENDOFMSG, sizeof(ENDOFMSG))) { break; }

      if( !strncmp( lines[cnt], "/", 1 ) ) { continue; }

      if( is_err_msg ( lines[ cnt ] ) ) { continue; }

      /* debug - - printf("%d %s\n", cnt, lines[cnt] ); */

      uscanc( lines[cnt], word, &nwrd, limit1, limit2);
      nwrd--;
 
      ln = strlen( word[1] );

      if (strncmp( word[0], ">LTC", 4 )==0 )
      {
	if( strncmp( word[1], "ON_DCONL", 7 )==0 )
	  set_button_state( "solve_LTC_DC_rb", TRUE );
	else if( strncmp( word[1], "ON_NPS", 6 )==0 )
	  set_button_state( "solve_LTC_RV_rb", TRUE );
	else if( strncmp( word[1], "ON_NV", 5 )==0 )
	  set_button_state( "solve_LTC_RPRQ_rb", TRUE );
	else if( strncmp( word[1], "OFF", 3 )==0 )
	  set_button_state( "solve_LTC_off_rb", TRUE );
	else if( strncmp( word[1], "ON", 2 )==0 )
	  set_button_state( "solve_LTC_on_rb", TRUE );
	else
	  printf("**ERROR** Unknown word (%s) for option %s\n",
				 word[1], word[0] );
      }

      else if( strncmp( word[0], ">AI_CONTROL", 11 )==0 )
      {
	if( strncmp( word[1], "CON", 3 )==0 )
	  set_button_state( "solve_area_on_rb", TRUE );
	else if( strncmp( word[1], "MON", 3 )==0 )
	  set_button_state( "solve_area_mon_rb", TRUE );
	else if( strncmp( word[1], "OFF", 3 )==0 )
	  set_button_state( "solve_area_off_rb", TRUE );
	else
	  printf("**ERROR** Unknown word (%s) for option %s\n",
				 word[1], word[0] );
      }

      else if (strncmp( word[0], ">MISC_PHASE_SHIFT_BIAS", 22 )==0 )
      {
	if( strncmp( word[1], "BPA", 3 )==0 )
	  set_button_state( "solve_phase_BPA_rb", TRUE );
	else if( strncmp( word[1], "WSCC", 4 )==0 )
	  set_button_state( "solve_phase_WSCC_rb", TRUE );
	else
	  printf("**ERROR** Unknown word (%s) for option %s\n",
				 word[1], word[0] );
      }

      else if( strncmp( word[0], ">LIMITS_QRES", 12 )==0 )
	lookup_and_fill_field( "solve_limits_qres_text", word[1], 0, ln );

      else if( strncmp( word[0], ">LIMITS_PHA", 11 )==0 )
	lookup_and_fill_field( "solve_limit_pha_text", word[1], 0, ln );

      else if( strncmp( word[0], ">LIMITS_DA", 10 )==0 )
	lookup_and_fill_field( "solve_limits_DA_text", word[1], 0, ln );

      else if( strncmp( word[0], ">LIMITS_DV", 10 )==0 )
	lookup_and_fill_field( "solve_limits_DV_text", word[1], 0, ln );

      else if( strncmp( word[0], ">TOL_BUSV", 9 )==0 )
	lookup_and_fill_field( "solve_tol_busv_text", word[1], 0, ln );

      else if( strncmp( word[0], ">TOL_AIPOWER", 12 )==0 )
	lookup_and_fill_field( "solvel_tol_aipower_text", word[1], 0, ln );

      else if( strncmp( word[0], ">TOL_TX", 7 )==0 )
	lookup_and_fill_field( "solve_tolerance_tx_text", word[1], 0, ln );

      else if( strncmp( word[0], ">TOL_Q", 6 )==0 )
	lookup_and_fill_field( "solve_tolerance_q_text", word[1], 0, ln );

      else if( strncmp( word[0], ">SOL_ITER_DECOUPLED", 20 )==0 )
	lookup_and_fill_field( "solve_decoupled_text", word[1], 0, ln );

      else if( strncmp( word[0], ">SOL_ITER_NEWTON", 16 )==0 )
	lookup_and_fill_field( "solve_newton_text", word[1], 0, ln );

      else if( strncmp( word[0], ">MISC_ITER_SUM", 14 )==0 )
	set_toggle_button( "solve_iter_summary_tb", word[1] );

      else if( strncmp( word[0], ">BASE_SOLUTION", 14 )==0 )
      {
	set_toggle_button( "solve_base_solution_tb", word[1] );

	if( strncmp( word[1], "NORM", 4 )==0  )	/* temp patch  NORMAL=ON */
	  set_button_state( "solve_base_solution_tb", TRUE );
      }

      else if( strncmp( word[0], ">DEBUG_TX", 9 )==0 )
	set_toggle_button( "solve_debug_tx_pb", word[1] );

      else if( strncmp( word[0], ">DEBUG_BUS", 10 )==0 )
	set_toggle_button( "solve_debug_bus_tb", word[1] );

      else if( strncmp( word[0], ">DEBUG_AI", 9 )==0 )
	set_toggle_button( "solve_debug_ai_tb", word[1] );

      else if( strncmp( word[0], ">DEBUG_DC", 9 )==0 )
	set_toggle_button( "solve_debug_dc_tb", word[1] );

      else if( strncmp( word[0], ">MISC_VFLAT", 11 )==0 )
	set_toggle_button( "solve_misc_flat_start_tb", word[1] );

      else if( strncmp( word[0], ">MISC_DCLP", 10 )==0 )
	set_toggle_button( "solve_misc_dclp_tb", word[1] );

      else if( strncmp( word[0], ">MISC_XBUS", 10 )==0 )
      {
	if( strncmp( word[1], "BPA", 3 )==0 )
	  set_button_state( "solve_volts_bias_BPA_rb", TRUE );
	else if( strncmp( word[1], "VMAX", 4 )==0 )
	  set_button_state( "solve_volts_bias_VMAX_rb", TRUE );
	else if( strncmp( word[1], "WSCC", 4 )==0 )
	  set_button_state( "solve_volts_bias_WSCC_rb", TRUE );
	else
	  printf("**ERROR** Unknown word (%s) for option %s\n",
				 word[1], word[0] );
      }

      else if( strncmp( word[0], ">MISC_TSTART", 12 )==0 )
	lookup_and_fill_field( "solve_misc_cont_tstart_text", word[1], 0, ln );

      else if( strncmp( word[0], ">NUMVSTEPS", 10 )==0 )
	lookup_and_fill_field( "solve_numvsteps_text", word[1], 0, ln );

      else
	printf("***ERROR*** unknown GET_DATA TYPE from powerflow (%s)\n",
		word[0] );
    }

  }
}




void set_toggle_button( char *wid_name, char *onoff )
{
  Widget wid;
  
  wid = widget_id( wid_name );

  if( wid )
  {
    if( strncmp( onoff, "ON", 2 )==0 || strncmp( onoff, "TR", 2 )==0 )
      set_button_state( wid_name, TRUE );
    else if( strncmp( onoff, "OF", 2 )==0 || strncmp( onoff, "FA", 2==0 ) )
      set_button_state( wid_name, FALSE );
  }
  else
    printf("***ERROR*** from set_toggle_button (pf_cb.c)\n");
}


/***************************************************************************\
*	Function: pfgetdata_cb
*
*	Author: Walt Powell	 1 Sep 1992	Modified:
*
*	Called by: linezcalc
*
*       Purpose: Obtain the line impedance record for a given configuration.
*
*       Parameters: 
*
*          *input_buffer - a pointer to the input buffer describing
*                          the line impedance parameters.
*
*               /GET_DATA, TYPE = LINE_IMPEDANCE_CALCULATION,
*                UNITS = < ENGLISH | METRIC >, DISTANCE = < miles | km >
*                BASEKV = <basekv>, BASEMVA = <basemva>, FREQUENCY = <freq>
*                CONDUCTOR = 1 .3636 .05215 1.602 -20.75 50. 50. 0.0 0.0 0
*
*          *output_buffer- a pointer to a line(s) of output describing
*                          the line impedance and admittance. 
*
*               LIC = <r1> <x1> <g1/2> <b1/2> <r0> <x0>
*
\***************************************************************************/
int appendOutstr(char *line, char *outstr)
{
 strcat(outstr, line);
 return(0);
}

int pfgetdata_cb(char *instr, char *outstr)
{
  char *pfcmdstr = outbuf_array;
  char *pfdataret = inbuf_array;
  char *c;
  int rtncde;

  extern int createRecPtrList();
  char *lines[MAXRECS];
  int num_recs;
  int cnt;

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "pfgetdata_cb: start\n");
  }

  /* append EODOFMSG to instr and call ipf */
  sprintf(pfcmdstr, "/GET_DATA, TYPE = LINE_IMPEDANCE_CALCULATION\n");
  strcat(pfcmdstr, instr);
  strcat(pfcmdstr, ENDOFMSG);

  rtncde = ipc_synch_rw(pfdataret, pfcmdstr);

  *outstr = '\0';
  cnt = forEachipfLine(pfdataret, appendOutstr, outstr);

  return(rtncde);
} /* end pfgetdata_cb */


void set_printer_selection( Widget print_wid, XtPointer client_data,
                                        XmSelectionBoxCallbackStruct *cbs )
/*************************************************************************\
\*************************************************************************/
{
  Widget        printer_select_id;
  XmString xstr;
  char *text;

  if((printer_select_id = widget_id("printer_selection_box")) == False) return;

    XtVaGetValues(printer_select_id, XmNtextString, &xstr, NULL);
    XmStringGetLtoR( xstr, XmSTRING_DEFAULT_CHARSET, &text);

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "set_printer_selection: prev text=\"%s\"\n", text);
  }

    XtVaSetValues(printer_select_id, XmNtextString, cbs->value, NULL);

    XtVaGetValues(printer_select_id, XmNtextString, &xstr, NULL);
    XmStringGetLtoR( xstr, XmSTRING_DEFAULT_CHARSET, &text);

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "set_printer_selection: next text=\"%s\"\n", text);
  }

    XtFree(text);
    XmStringFree(xstr);
}



create_outfile(char *infile, char *outfile, char *suffix)
/*************************************************************************\
\*************************************************************************/
{
#define CORTMP "cortmp."
  int lenfile;

  if((lenfile = strlen(infile)) < 1) {
    sprintf(outfile, "%s%s", CORTMP, suffix);
    printf("Warning invalid input file - create %s\n", outfile);
    return;
  }
  sprintf(outfile, "%s", infile);
  while(lenfile > 0) {
    if(outfile[lenfile] == '.') {
      sprintf(&outfile[lenfile+1], suffix);
      break;
    }
    lenfile--;
  }
  if(lenfile < 0) {
    sprintf(outfile, "%s%s", CORTMP, suffix);
    printf("Warning invalid input file - create %s\n", outfile);
  }
  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "create_outfile: infile=\"%s\" outfile=\"%s\"\n",
        infile, outfile);
  }
}

get_printer_name(char *print_str)
/*************************************************************************\
\*************************************************************************/
{
  Widget printer_select_id;

  XmString  xstr = NULL;
  char      *text = NULL;

  /** get the printer name **/
  if((printer_select_id = widget_id("printer_selection_box")) == False) {
    /** window not found - just use unix default **/
    printf("Warning - no printer selection window - set printer to lpr\n");
    sprintf(print_str, "lpr");
  } else {
    XtVaGetValues(printer_select_id, XmNtextString, &xstr, NULL);
    XmStringGetLtoR( xstr, XmSTRING_DEFAULT_CHARSET, &text);
    if(strlen(text) < 1) {
      printf("Warning - no string set in printer selection - set to lpr\n");
      sprintf(print_str, "lpr");
    } else {
      sprintf(print_str, "%s", text);
    }
    XtFree(text);
    XmStringFree(xstr);
  }
  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "get_printer_name: printer string=\"%s\"\n", print_str);
  }
}

int print_plot( Widget w, int *tag,
		XmFileSelectionBoxCallbackStruct *callback_data)
/*********************************************************************\
*       Author: John Rutis       Aug, 1992
*	updated dlc Jul 93
*       updated jgs Feb 11,2001
*       updated jgs Feb 13,2001
*
\*********************************************************************/
{
#define MAXFNME 1024
  int		rtncde;
  Widget        coord_text_id;
  Widget        printer_select_id;
  char          *outbuf = outbuf_array;
  char          *str_rtn = inbuf_array;
  char          file_name[MAXFNME];        /* name of input file */
  char          file_name_tmp[MAXFNME];        /* name of input file */
  char          file_name_ps[MAXFNME];        /* name of input file */
  char          print_str[MAXFNME];        /* printer string */
  char		*pcomments_txt;
  char 		*ptstr;
  Widget	id;

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "print_plot: start\n");
  }

  /* Get the Coord File Name */
  if(HashLookup("save_coord_text", &coord_text_id ) == False)
  {
    /* generate a temporary file name if save file not specified */
    if( (ptstr = tmpnam(file_name)) == NULL)
      sprintf(file_name, "NoName"); 
  }
  else
  {	 /* create the file name from the save file name */
    strcpy(file_name, XmTextGetString(coord_text_id));
  }

  remove_prefix_from_filename( file_name );

  create_outfile   (file_name, file_name_tmp, "tmp");

  pswriteCoordFile (file_name_tmp);

  create_outfile   (file_name, file_name_ps, "ps");

  get_printer_name (print_str);

        /* NOTE: use HashLookup because widget_id gives unwanted warnings */
  if(( HashLookup( "user_comments_text", &id )) != FALSE)
  {
    int len;
    pcomments_txt = XmTextGetString(id);
    len = strlen(file_name_tmp) + strlen(file_name_ps) 
          + strlen(pcomments_txt) + 20;
    if(len > BUFSIZE)
    {
      int cmt_size;
      cmt_size = strlen(pcomments_txt);
      if(cmt_size < len - BUFSIZE)
      {
        fprintf(stderr,"Warning trim comments %d - lack of message space\n");
        pcomments_txt[ cmt_size - (len - BUFSIZE)] = '\0';
      }
      else
      {
        fprintf(stderr,"ERROR message buffer to big\n");
        return;
      }
    }
    sprintf(outbuf, "/PLOT\n%s\n%s\n%s\n%s\n", file_name_tmp, file_name_ps, 
	      pcomments_txt, ENDOFMSG);
    XtFree(pcomments_txt);
  }
  else
  {
    sprintf(outbuf, "/PLOT\n%s\n%s\n%s\n", file_name_tmp, file_name_ps, 
      ENDOFMSG);
  }
  /* ( this is where comments should go ) */

  rtncde = ipc_synch_rw(str_rtn, outbuf);
  if(TRUE != rtncde) {
    sprintf(errmsg, "Failed on call to PLOT rtncde %d\n",
           rtncde);
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "cflow_launch_cb";
    err.type = WARNING;
    err.ident = sccsid;
    em_show(&err);
    return;
  }

  if(*tag == 1)
    sprintf(outbuf, "/SYSCAL\n%s %s\n%s\n", print_str, file_name_ps, ENDOFMSG);
  else
    sprintf(outbuf, "/PLOT\n%s\n%s\n%s\n", file_name_tmp, file_name_ps, ENDOFMSG);
    sprintf(outbuf, "/SYSCAL\ngv %s &\n%s\n", file_name_ps, ENDOFMSG);

  rtncde = ipc_synch_rw(str_rtn, outbuf);
  if(TRUE != rtncde) {
    sprintf(errmsg, "Failed to /SYSCAL VIEW or PRINT rtncde %d\n",
           rtncde);
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "cflow_launch_cb";
    err.type = WARNING;
    err.ident = sccsid;
    em_show(&err);
    return;
  }
}


void remove_prefix_from_filename( char *fname )
/***************************************************************************\
* Purpose:  Removes the preliminary stuff from file name - if any.
*
*
\***************************************************************************/
{
  char *pstr;

  if     (NULL != strrchr(fname, ']'))
    sprintf(fname, 1+strrchr(fname, ']'));

  else if(NULL != strrchr(fname, ':'))
    sprintf(fname, 1+strrchr(fname, ':'));

  else if (NULL != strrchr(fname, '/'))
    sprintf(fname, 1+strrchr(fname, '/'));

  if(NULL != (pstr = strrchr(fname, ';'))) {
    *pstr = 0;
  }
}


void reports_file_ok_cb(Widget w, XtPointer tag,
                        XmFileSelectionBoxCallbackStruct *reason)
/*************************************************************************\
\*************************************************************************/
{
  char *filename;
  Widget wid;

  filename = get_file_name_from(reason);
  wid = widget_id("reports_compare_file_name_text");
  XtVaSetValues(wid, XmNvalue, filename, NULL);
}

int check_powerflow(char *namebase)
/*************************************************************************\
\*************************************************************************/
{
char    outbuf_array[BUFSIZE];
char    inbuf_array[BUFSIZE];
char *pfcmdstr = outbuf_array;
char *pfdataret = inbuf_array;
  int rtncde;

  char key[128];

#ifdef BUS_EXISTS
    sprintf(pfcmdstr, "/get_data,type=bus_exists,bus=\"%s\"\n%s\n", 
            namebase, ENDOFMSG);
#else
    sprintf(key, "B     %12.12s", namebase);
    sprintf(pfcmdstr, "/get_data, type=connection\n%s\n%s\n", key, ENDOFMSG); 
#endif
    /* printf("pfcmdstr = %s\n", pfcmdstr); */

    rtncde = ipc_synch_rw(pfdataret, pfcmdstr);

    return(rtncde);
}


/*************************************************************************\
\*************************************************************************/
void sendOutageCard(GraphElement *pedge, int flag)
{
static char    outbuf_array[BUFSIZE];
static char    inbuf_array[BUFSIZE];
static  char *pfcmdstr = outbuf_array;
static  char *pfdataret = inbuf_array;
char *setChgCde;
 
  PFBRANCH branch;
  PFBRANCH *pbr;
  PFBUS  bus;
  PFBUS  *pbus;

  if (ipfdebug & (DB_TraceMask | DB_Pf_CbMask)) {
    fprintf(ipfdbio, "sendOutageCard: flag=%d\n", flag);
  }

  if( 1 == flag ) {
    setChgCde = "D";
  } else {
    setChgCde = "R";
  }

  memset(outbuf_array, ' ', sizeof(outbuf_array) );
  memset(inbuf_array, ' ', sizeof(inbuf_array) );

  if( pedge->class == GraphClassEdgeSection ) {
    pbr = &branch;
    memset(pbr, ' ', sizeof(PFBRANCH) );
    memcpy(pbr->name1, pedge->vertex_id, 
          sizeof(branch.name1) + sizeof(branch.base1) );   
    memcpy(pbr->name2, pedge->farvertex_id, 
          sizeof(branch.name2) + sizeof(branch.base2) );   
    memcpy(pbr->type, "L", sizeof(branch.type) );
    memcpy(pbr->chgcde, setChgCde, sizeof(branch.chgcde) );
    memcpy(pbr->circuit, "*", sizeof(branch.circuit) );
    memcpy(pbr->section, "0", sizeof(branch.section) );
    pbr->eol[0] = 0;
    /* printf("send outage (%s)\n", &pbr->type[0]); */
    pfchanges_cb(0,&pbr->type[0], 0);
  }
  else if( pedge->class == GraphClassVertexBus )
  {
    pbus = &bus;
    memset(pbus, ' ', sizeof(PFBUS) );
    memcpy(pbus->name, pedge->vertex_id, 
           sizeof(bus.name) + sizeof(bus.base) );   
    memcpy(pbus->type, "B", sizeof(bus.type) );
    memcpy(pbus->chgcde, setChgCde, sizeof(bus.chgcde) );
    pbus->eol[0] = 0;
    /* printf("send outage (%s)\n", &pbus->type[0]); */
    pfchanges_cb(0,&pbus->type[0], 0);
  } else {
    printf("Warning: bad icon selected for outage\n");
  }

}
void setup_screen_display( Widget w, int *tag, XmAnyCallbackStruct *cbd)
/**************************************************************************\
* Purpose: answers the APPLY of the display_menu_dialog
*
* 
*
\**************************************************************************/
{
  /* NOTE: not correct way to do this. */
  refresh_solution_data( 0, 0, 0 );
}


void setup_bus_style( Widget w, int *tag, XmAnyCallbackStruct *cbd)
/**************************************************************************\
* Purpose: 
*
*
* Called by: Combine Bus/Gen Sym toggle button (main pulldown menu)
\**************************************************************************/
{

  if(XmToggleButtonGetState(widget_id("view_menu_bus_combine_tb")))
  {
    VUIT_Unmanage("bus_legend_form");
    VUIT_Manage("main_bus_icon_form");
  }
  else
  {
    VUIT_Manage("bus_legend_form");
    VUIT_Unmanage("main_bus_icon_form");
  }

  refreshGraph();
}
