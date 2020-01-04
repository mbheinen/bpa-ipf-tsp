static char sccsid[] = "@(#)ipcclient.c	20.6 2/15/95";
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
/***********************************************************
* file: ipcclient.c
* author: Dan Clark
* date: Apr 20, 1992	Modified:
*
* purpose:  
* a) parse client commands 
* b) invoke callback routine to format data
* c) package ipc buffer and send to powerflow
*------------------------------------------------------------------
* d) receive data back from powerflow
* e) reparse message
* f) invoke callup routine to use data
*
************************************************************
*/

#include <stdio.h>
#include <string.h>
#include <Xm/Xm.h>

#include "ipf_ipc.h"
#include "em.h"

em_init		/* Error structure macro - must be include in every file */

FILE *logfileptr = NULL;
static char  inbuf[BUFSIZE];
static char  outbuf[BUFSIZE];

extern int setAsynchid(XtInputId asynchid);
extern XtInputId getAsynchid();
extern int cmdparse_gui_cb( char *cmd , int (**fptr)() );
extern void ipc_display_cmd( char *str);
extern int ipcsend( char *inputstr, int ipc_socket );
extern int get_gui_socket();
extern int ipcrecv( char *outputstr, int ipc_socket );
extern void ipc_display_data( char *str);
extern int cmdparse_gui_cu( char *cmd , int (**fptr)() );

int ipcclient(widget, tag, callback_data)
Widget          widget;
caddr_t         *tag;
caddr_t         *callback_data;
{
   int (*fptr)(char *, char *);
   int status;
   int cmd;
   char *pinbuf;
   char *poutbuf;

   /** a) get command string from tag **/
   memset(inbuf, 0, BUFSIZE * sizeof(char));
   strcpy(inbuf, (char *)tag);
   printf("ipc client received cmd=%s*\n", inbuf);

   /** b) parse to determine syntax **/
   cmd = cmdparse_gui_cb(inbuf, &fptr);
   if (!cmd) {
     printf("WARNING: callback command not found\n");
     return;
   }

   /** c) invoke callback routine **/
   /* hide command in start of buffer passed to callback routine */
   if ( (pinbuf = strchr(inbuf,'\n' )) == NULL ) {
     printf("WARNING: command should be new line terminated\n");
     strcat(inbuf, "\n");
     pinbuf++;
   }
   if ( (pinbuf = strchr(inbuf,'\0' )) == NULL ) {
     printf("ERROR: command must be null terminated\n");
     return;
   } 
   /* memset ( pinbuf, 0, ( BUFSIZE - ( pinbuf - inbuf) ) * sizeof(char) ); */

   status = (*fptr)(outbuf, pinbuf);
   if(status != I_OK) {
      printf("WARNING: status of callback function = %d\n", status);
   }
   if ( (pinbuf = strchr(inbuf,'\0' )) == NULL ) {
     printf("ERROR: command must be null terminated\n");
     return;
   } 
   printf("inbuff now = %s\n", inbuf);   

   /** c.5) display command to command window ***/
   ipc_display_cmd( inbuf );
   
   /** d) send buffer to powerflow **/
   status = ipcsend(inbuf, get_gui_socket() ); 
   if(status != I_OK) {
     printf("ERROR - send failed\n");
   }

#ifdef SYNCH
   /* ipcclientret(0,0,0); */
#endif

}


/*****( ipcclientret )*******************************************************\
*
*	Called by ipc_cb.c (3 instances)
*
\****************************************************************************/

int ipcclientret(widget, tag, callback_data)
Widget          widget;
caddr_t         *tag;
caddr_t         *callback_data;
{
   int (*fptr)(char *, char *);
   int status;
   int cmd;
   char *pinbuf;
   char *poutbuf;
   extern void ipc_display_err_msgs(char *pfdataret);
   XtInputId asynchid;


   /** e) get data from powerflow **/
   memset (outbuf, 0, BUFSIZE * sizeof(char) );
   status = ipcrecv(outbuf, get_gui_socket() ); 
   if(status != I_OK)
   {
     /* shutdown asynchronous handler */
#ifdef DEBUG
     printf("ipcclientret: ERROR - ipcrecv failed \n");
#endif
    sprintf(errmsg, "\n%s\n%s\n%s\n%s\n", 
    "ipcclientret: ERROR - IPF server failed, Powerflow crash or abort",
    "COMMUNICATION WITH POWERFLOW HAS BEEN INTERRUPTED",
    "Gui is in stand alone mode - no further data available from Powerflow",
    "Recommended action: Save any coord file changes and restart application.");
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "ipcclientret";
    err.type = FATAL;
    err.ident = sccsid;
    em_show( &err );


     /* temporary fix ... need to check for failure of socket
     *   int asynch check
     */
     asynchid = getAsynchid();
     if(asynchid != 0) {
       XtRemoveInput(asynchid); 
       setAsynchid((XtInputId)0);
     }
     return;
   }

   /*** note handle errors returned from powerflow HERE **/
   err.line = EM_LINE;
   err.ident= sccsid;
   err.type = INFO;
   err.link = " ipcclient ";
   ipc_display_err_msgs( outbuf );

   /** e.5) display data back from powerflw **/
   ipc_display_data( outbuf );

   cmd = cmdparse_gui_cu(outbuf, &fptr);
   if (!cmd) {
     printf("WARNING: callup command not found\n");
     return;
   }

   /** f) invoke the callup routine to process the data **/
   /* hide command in start of buffer passed to callback routine */
   if ( (poutbuf = strchr(outbuf,'\n' )) == NULL ) {
     if ( (poutbuf = strchr(outbuf,'\0' )) == NULL ) {
       printf("ERROR: command must be new line terminated\n");
       return;
     } else {
       printf("WARNING: command should be new line terminated\n");
       strcat(outbuf, "\n");
       poutbuf++;
     }
   }
   poutbuf++;
   status = (*fptr)(poutbuf, inbuf);
   if(status != I_OK) {
      printf("WARNING: status of callback function = %d\n", status);
   }

}

