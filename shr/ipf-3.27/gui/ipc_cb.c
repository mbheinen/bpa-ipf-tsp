static char sccsid[]="@(#)ipc_cb.c	20.28 3/11/98";
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

/*************************************************************
* file: ipc_cb.c
* purpose: ipctest callback routines to process xwindows data
* and convert to c data
* enhancements:
*
**************************************************************
*/
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "ipfdebug.h"   /* use DB_QuikExitMask */

#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include <Xm/Command.h>

#ifndef AXP_VMS_BUG
#ifdef VMS
#include <socket.h> 
#include <time.h>
#include "types_fds.h"
#else
#include <sys/types.h>
#include <sys/socket.h> 
#include <sys/time.h>
#endif
#endif
#ifdef RS6K
#include <sys/select.h>
#endif

#include <errno.h>
#include <X11/cursorfont.h>


#include "ipf_ipc.h"
#include "em.h"
#include "ipf_rsrc.h"


em_init                 /* special initialization for error message box */
                        /* must be in every file using this.            */

#define MAXNAME 256
#define NODEMAXNAME 128

static int  ipfstate; /* local saved variable of state returned from powerflow*/
static char sendtext[BUFSIZE];
static char rtntext [BUFSIZE];
static int  gui_socket;
static int  ipcsockobjnum = 0;
static char ipcnodename[NODEMAXNAME];
static char out_buffer[BUFSIZE];

static XtInputId ipcAsynchCheckid = 0;


void ipc_display_data		( char * );
int checkSocket			( int * );
void ipc_display_err_msgs	( char *pfdataret );
int ipc_synch_rw		( char *rtntext, char *text );
int create_socket1		( int *sock_objnum );
int create_socket2		( int *ipc_socket, int max_sleep );
void set_cursor_watch		( int watch_on );
int ipc_startup			( AppResData *app_resdata );
int ipc_startup1		( int *sockid );
int ipc_startup2		( int sockid, int max_sleep );

extern int HashLookup		( char *name, Widget *id );
extern int atoi_cnt		( char *str, int size );
extern int is_err_msg		( char *str );
extern int pfinit_cb		( Widget wid, XtPointer tag,
				   XmAnyCallbackStruct *cbs );
extern int ipcclientret		( Widget, XtPointer, XtPointer);
extern int ipcGetSendCnt	( );
extern int ipcGetRecvCnt	( );
extern Widget  widget_id	( char *);
extern int ipcsend		( char *inputstr, int ipc_socket );
extern int ipcrecv		( char *outputstr, int ipc_socket );
/* extern XtInputId XtAppAddInput (); */
extern void set_solution_status	( int status );
extern int launch_srv           ( char *servername, int socketid );



/***************************************************************************\
*	Routines - so other routines can extracts values from this module.
\***************************************************************************/

int get_gui_socket()
{
  return gui_socket;
}


int ipcgetsockobjnum()
{
  return ipcsockobjnum;
}


char *ipcgetnodename()
{
  return ipcnodename;
}


XtInputId getAsynchid()
{
  return ipcAsynchCheckid;
}

int setAsynchid(XtInputId asynchid)
{
  ipcAsynchCheckid = asynchid;
  return 0;
}

ipcAsynchCheck(XtPointer client_data,int *fid, XtInputId *id)
{
  int nfound;

  nfound = checkSocket(fid);

  if(nfound > 0){
    printf("ipcAsynchCheck: ASYNCH RETRIEVAL !\n");
    ipcclientret(0,0,0);
  }

}


int checkSocket( int *fid )
/***************************************************************************\
* Purpose:
*
* Called by: above
\***************************************************************************/
{
#ifdef AXP_VMS_BUG
#include <socket.h> 
#include <time.h>
#include "types_fds.h"
#endif
#ifdef VMS_WORKAROUND
  int  readfds, writefds, exceptfds;
/* on some machines may need to use unsigned long */
#else
  fd_set readfds, writefds, exceptfds;
#endif
  int    nfound, nfds;
  int    exceptfound;
  struct timeval timeout;

  memset(&timeout, 0,  sizeof(timeout));
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;

#ifdef VMS_WORKAROUND
  nfds = 32;
  readfds   = 1<<*fid;
  writefds  = 0;
  exceptfds = 0;
#else
  nfds = FD_SETSIZE;	
  FD_ZERO(&readfds);
  FD_SET(*fid, &readfds);
#endif
  nfound = select(nfds, &readfds, &writefds, &exceptfds, &timeout);
  if(nfound < 0 ) {
      perror("ipcAsynchCheck");
      if(ipcAsynchCheckid != 0) {
        XtRemoveInput(ipcAsynchCheckid);
      }
      ipcAsynchCheckid = 0;
      /* exit(1); */
  }

#ifdef VMS_WORKAROUND
  nfds = 32;
  readfds   = 0;
  writefds  = 0;
  exceptfds   = 1<<*fid;
#else
  nfds = FD_SETSIZE;	
  FD_ZERO(&exceptfds);
  FD_SET(*fid, &exceptfds);
#endif
  exceptfound = select(nfds, &readfds, &writefds, &exceptfds, &timeout);
  if(exceptfound > 0) {
  /* lost socket so remove asynch handler */
    printf("checkSocket: ERROR execptions found on fid %d\n", *fid);
    if(ipcAsynchCheckid != 0) {
      XtRemoveInput(ipcAsynchCheckid);
    }
    ipcAsynchCheckid = 0;
  }

#ifdef ERRNO
  if(errno == EINTR) {
    perror("    ipcAsynchCheck EINTR");
  }
#endif
  return nfound;
}



int ipc_startup1( int *sockid )
/***************************************************************************\
* Purpose:
*
* Called by: gui.c
*
\***************************************************************************/
{
  *ipcnodename = '\0';

  if ( create_socket1( sockid ) != 0 )
  {
    err.line = EM_LINE;
    err.msg  = out_buffer;
    err.type = FATAL;
    err.link = "IPC callback";
    err.ident= sccsid;

    sprintf(out_buffer, "*** ERROR *** NO CONNECTION to Powerflow server\n\
   Socketid:%d \n\
 - No socket specified -or-\n\
 - Gui started with -server flag -or-\n\
 - Socket id is different from gui -or-\n\
 - Socket id may be in use by some one else.\n",
       *sockid );
     em_show( &err );
  }

  ipcsockobjnum = *sockid;
  return(0);
}


int ipc_startup2( int sockid, int max_sleep )
/***************************************************************************\
* Purpose:
*
* Called by: gui.c
*
\***************************************************************************/
{
  extern XtAppContext app_context;
  XtPointer condition;
  int sockfd;

  if ( create_socket2( &gui_socket, max_sleep ) != 0 )
  {
    err.line = EM_LINE;
    err.msg  = out_buffer;
    err.type = FATAL;
    err.link = "IPC callback";
    err.ident= sccsid;

    sprintf(out_buffer, "*** ERROR *** NO CONNECTION to Powerflow server\n\
   Socketid:%d  Gui_socket:%d\n\
 - No socket specified -or-\n\
 - Gui started with -server flag -or-\n\
 - Socket id is different from gui -or-\n\
 - Socket id may be in use by some one else.\n",
       sockid, gui_socket );
     em_show( &err );
  }
  else
  {

/* Performance problem with VMS sockets is fixed --
*              no longer using mailboxes !!!
*
* #ifdef VMS  /!!!* fix for VMS which has 2 mailbox fds encoded *!!!/
*      sockfd = gui_socket - ( ( gui_socket / 1000 ) * 1000 );
* #else
*/
   sockfd = gui_socket;
/* #endif
*/
    condition = (XtPointer)XtInputReadMask;
    ipcAsynchCheckid = XtAppAddInput(app_context, sockfd, 
                   condition, (XtInputCallbackProc)ipcAsynchCheck, NULL);
    pfinit_cb(0,0,0);
  }

  return(0);
}



int ipc_startup( AppResData *app_resdata )
/***************************************************************************\
\***************************************************************************/
{
  ipc_startup1(&app_resdata->socketid); /* initialize the ipc socket */
  if(app_resdata->server)
  {
       /* start ipfsrv */
     launch_srv( app_resdata->servername, app_resdata->socketid );
  }

  /* "accept" ipc connection from ipfsrv */
  ipc_startup2( app_resdata->socketid, app_resdata->sleep);

  return 0;
}



void ipc_display_cmd( char *str)
/***************************************************************************\
* Purpose: convert a character string into the command window.
*
* author: Dan Clark
*
* Called by:
\***************************************************************************/
{
  static Widget output_wid = False;
  static Widget window_wid = False;
  XmTextPosition txt_pos1;
  XmTextPosition txt_pos;
  char cnt_str[32];

  if(window_wid == False)
  {
    if( HashLookup("ipc_command_board", &window_wid ) == False ) return;
  }
  if(!XtIsManaged(window_wid)) return;
  if(output_wid == False) {
    if(( output_wid = widget_id( "ipc_text_guitopf" )) == False ) return;
  }

  /*** add one to send command this is displayed prior to send call ***/
  sprintf(cnt_str, "gui->pf cnt: %d\n", ipcGetSendCnt()+1);
  txt_pos1 = XmTextGetLastPosition(output_wid);
  XmTextInsert(output_wid, txt_pos1, cnt_str);

  txt_pos = XmTextGetLastPosition(output_wid);
  XmTextInsert(output_wid, txt_pos, str);

  XmTextSetTopCharacter(output_wid, txt_pos1);

}


void ipc_display_data( char *str)
/****************************************************************************\
* Purpose: convert a character string into the command window.
*
* author: Dan Clark
*
* Called by:
\****************************************************************************/
{
  static Widget output_wid = False;
  static Widget window_wid = False;
  XmTextPosition txt_pos1;
  XmTextPosition txt_pos;
  char cnt_str[32];

  if(window_wid == False) {
    if( HashLookup("ipc_command_board", &window_wid ) == False ) return;
  }
  if(!XtIsManaged(window_wid)) return;
  if(output_wid == False) {
    if(( output_wid = widget_id( "ipc_text_pftogui" )) == False ) return;
  }
    
  sprintf(cnt_str, "pf->gui: %d\n", ipcGetRecvCnt());
  txt_pos1 = XmTextGetLastPosition(output_wid);
  XmTextInsert(output_wid, txt_pos1, cnt_str);

  txt_pos = XmTextGetLastPosition(output_wid);
  XmTextInsert(output_wid, txt_pos, str);

  XmTextSetTopCharacter(output_wid, txt_pos1);

}



void ipc_commandString_xtoc( Widget w, int *tag, unsigned long reason )
/**************************************************************************\
* Purpose: handle commands entered interactively through the command
*	   window interface to powerflow 
*
* Author: Dan Clark
* Revised by: Jay Coleman
* Revised by: Dan Clark May 30 - clear string after each command
*
* Called by:
\**************************************************************************/
{
  static int cmdStart = True;
  XmCommandCallbackStruct *cmd_data;
  XmString              cmd_area_xmstr;
  XmStringContext       context;
  char                  *text;
  XmStringDirection     drctn;
  XmStringCharSet       charset;
  Boolean               rtncde;
  Boolean               sprtr;

  /* rtntext = (char *)malloc(BUFSIZE * sizeof(char)); */

  cmd_data = (XmCommandCallbackStruct *)reason;

  switch(cmd_data->reason)
  {
    case XmCR_COMMAND_CHANGED:
    break;

    case XmCR_COMMAND_ENTERED:
      cmd_area_xmstr = cmd_data->value;
      rtncde = XmStringInitContext(&context, cmd_area_xmstr);
      if(rtncde == False) {
        printf("ipcSendCommand: failed string init context\n");
        return;
      } 

      rtncde = XmStringGetNextSegment(context, &text, &charset, &drctn, &sprtr);
      if(rtncde == False) {
         printf("ipcSendcommand: failed string get next segment\n");
         return;
      }
      XmStringFreeContext(context);

      if(cmdStart == True)
      {
        strcpy(sendtext, text);
        cmdStart = False;
      }
      else
      {
        strcat(sendtext, text);
      }
      strcat( sendtext, "\n" );
      if ( text[0] == '*'  &&  text[1] == '[' )
      {
        ipc_synch_rw( rtntext, sendtext );
        memset (sendtext, '\0', sizeof(sendtext));
        cmdStart = True;
      }
      else
      {
        if ( text[0] == '^'  &&  text[1] == '[' )
        {
          ipc_display_cmd( sendtext );
          ipcsend( sendtext, gui_socket );
          memset(sendtext, '\0', sizeof(sendtext));
          cmdStart = True;
        }
      }
    break;

    default:
      printf("ipcSendCommand: unknown callback reason = %d\n",
                cmd_data->reason);
    break;
  } /* end switch */

}


void set_cursor_watch(int watch_on)
/*************************************************************************\
* Purpose:
*
* Called by: 
\*************************************************************************/
{
  unsigned int theCursorShape;
  static Window *draw_window;
  static Cursor watchCursor;
  static Cursor ptrCursor;
  static Widget draw_area;
  static int first = 1;

  if(1 == first) {
    if((draw_area = widget_id("gui_main")) == FALSE) {
      printf("Warning: cannot change cursor\n");
    }
    watchCursor = XCreateFontCursor(XtDisplay(draw_area), XC_watch);
    ptrCursor = XCreateFontCursor(XtDisplay(draw_area), XC_left_ptr);
    first = 0;
  }

  if(TRUE == watch_on) {
    XDefineCursor(XtDisplay(draw_area), XtWindow(draw_area), watchCursor);
  } else {
/***
    XUndefineCursor(XtDisplay(draw_area), XtWindow(draw_area));
**/
    XDefineCursor(XtDisplay(draw_area), XtWindow(draw_area), ptrCursor);
  }
  XFlush(XtDisplay(draw_area)); 
  
}

int ipfstate_get(int *ostates)
/******************************************************************\
*
\******************************************************************/
{
   *ostates = ipfstate;
   return ipfstate;
}


int ipc_synch_rw(char *rtntext, char *text)
/**************************************************************************\
* Purpose:  read and write a single message over the ipc channel
*	  used from the client program.
*	  Input: text - a string to send over ipc
*	  Returns: pointer to the string returned over channel
*
* author: Dan Clark
\**************************************************************************/
{
  int cnt, i;
  char *str_rtn;
  int ipfstatus = FALSE;

  if(text[0] == '\0') {
    printf("WARNING: ipc_synch_rw received empty string\n");
    return(FALSE);
  };

  ipc_display_cmd( text );

  set_cursor_watch(TRUE);

  ipcsend( text, gui_socket  );

  cnt = 0;
  do
  {
    if(ipcGetRecvCnt()+1 < ipcGetSendCnt()) {
      ipcclientret(0,0,0);	 /* process async message */
    }
    else
    {
      ipcrecv( rtntext, gui_socket  );
      ipc_display_data( rtntext );

      err.line = EM_LINE;
      err.type = INFO;
      err.link = "IPC synch rw";
      err.ident= sccsid;
      ipc_display_err_msgs( rtntext );
    }
    cnt++;
  } while(ipcGetRecvCnt() < ipcGetSendCnt() && cnt < 2);      

  set_cursor_watch(FALSE);

  /* check the return data for the state flag */
  if( ( str_rtn = strstr(rtntext, "IPF state:")) == NULL ){
    printf("*** WARNING *** from ipc_synch_rw\n" );
    printf("       No 'state' text in string returned from powerflow.\n" );
    printf("       Send string was: %s\n", text );
  } else {
    int tmplen;
    tmplen = (int)strlen("IPF state:");
    if(strlen(&str_rtn[tmplen]) >= 3) {
      ipfstate = atoi_cnt(&str_rtn[tmplen], (int)3);
      set_solution_status(ipfstate);
  /* printf("ipc_synch_rw: ipfstate = %d string (%s)\n", ipfstate, str_rtn); */
    }
  }
  /* fix by B.Rogers to ensure no bomb */
  if( ( str_rtn = strstr(rtntext, "return status:")) == NULL ){
    printf("*** ERROR *** from ipc_synch_rw\n" );
    printf("       No 'return status:' text in string returned from powerflow.\n" );
    printf("       Send string was: %s\n", text );
    ipfstatus = FALSE;

    err.line = EM_LINE;
    err.msg  = out_buffer;
    err.type = FATAL;
    err.link = "IPC callback";
    err.ident= sccsid;

    sprintf(out_buffer, "*** FATAL *** POWERFLOW CRASH \n" );
    em_show( &err );
  }
  else
  {
    int tmplen; 
    tmplen = (int)strlen("return status:");
    if(strlen(&str_rtn[tmplen]) >= 3) {
      ipfstatus = atoi_cnt(&str_rtn[tmplen], (int)3);
      if(0 == ipfstatus) {
        ipfstatus = TRUE;
      } else if (1 == ipfstatus) {
        ipfstatus = FALSE;
      } else {
        ipfstatus = FALSE;
      }
    }
  }

  return(ipfstatus);

}



void ipc_display_err_msgs( char *pfdataret )
/*************************************************************************\
* Purpose:  Scans powerflow data text for error messages
*	and places them in error window.
*
*	Author: Bill Rogers
*
\*************************************************************************/
{
#define MAXLINES 500
  int numlines, cnt;
  char *lines[MAXLINES];
  extern int createRecPtrList();
  char tempbuf[BUFSIZE];
  char outline[256];
  int severity;

  strncpy(tempbuf,pfdataret,BUFSIZE);
  numlines = createRecPtrList(lines, MAXLINES, tempbuf);

  if(numlines > 0) /* create a db and stuff data in */
  {
    for(cnt = 0; cnt < numlines; cnt++)
    {
      severity = is_err_msg ( lines[cnt] ); /* routine in pf_cb.c */
      if ( FALSE != severity )		
      {
        err.line = EM_LINE;
        err.link = "IPC display error messages";
        err.ident= sccsid;
	sprintf(outline,"%s\n",lines[cnt]);
        err.msg  = outline;
        if(severity >= 4) {
          err.type = FATAL;
          em_show( &err );
        } else if(severity >= 2) {
          err.type = WARNING;
        } else {
          err.type = INFO;
        }
      }
    }
  }

}



void exit_ipf ( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs)
/*****************************************************************************\
* Purpose: To close the server without getting any error msg.
*
* Author: Bill Rogers             Jun 1, 1994
*
* Called by: OK button on exit_warning_box.
*
\*****************************************************************************/
{

  sprintf(sendtext, "%s\n%s\n",
                    "/EXIT", ENDOFMSG);

  ipcsend( sendtext, get_gui_socket() );
 
  printf("*** EXIT POWERFLOW / END GUI ***\n" );

}



void exit_ipf_quick ( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs)
/*****************************************************************************\
*       Purpose: Same as above, except bypasses confirmation of exit.
*
*       Author: Bill Rogers             Oct 3, 1994
*
*       Called by: EXIT button
*
\*****************************************************************************/
{

  if( ipfdebug & DB_QuikExitMask)	/* AND function */
  { 
     exit_ipf ( 0, 0, 0);

     exit(0);		/* exits the GUI MOTIF application (w/o seg. fault) */
  }
}
