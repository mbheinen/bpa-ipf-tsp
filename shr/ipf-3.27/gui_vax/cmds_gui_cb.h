/*******************************************
* file: ipc_cmdparse.h
* author: Dan Clark
* date: Feb, 1992
* purpose: 
*   define a table of the commands for the parser on both
*   the client and server side.
*   the general flow of commands starts at the client (GUI).
*   a) command buffer is formated and sent to the gui parser
*    and the parser determines the callback routine to invoke
*    to setup data and handle the command.
*    if no additional data accompanies the command a noop function
*    is called.
*   b) the full buffer is then sent to the server (IPF) 
*     assuming an asynchronous protocol
*     NOTE: currently implemented with synchronous protocol.
*   c) The server (IPF) uses the same table to locate the powerflow
*     routine to retrieve the data.  This routine is called with
*     an inbuffer and outbuffer.
*   d) The server signals the GUI that it is ready to send data
*   e) The server (IPF) gets back a full outbuffer and sends back
*     the data to the client (GUI) and blocks on the send untill
*     the client starts picking up the data.
*   f) The client processes the signal, reads in the first buffer
*     of data and invokes the proper callup routine determined from
*     the table below.
*   
* revised:
*  Apr 20, 1992	Dan Clark
*   add column of call up routines for the gui parsing of
*   the return command from powerflow.
*   and a colum of call back routines for the gui.
*******************************************
*/

/***
* the forward declaration of all client callback functions
***/
extern int cb_gtnetdata();
extern int cb_pfinit();
extern int cb_gtbase();
extern int cb_change();
extern int cb_solton();
extern int cb_gtdata();
extern int cb_report();
extern int cb_cdproc();
extern int cb_newbse();
extern int cb_pfexit();

/***
* static declaration of parse table
***/
static CmdTbl ipf_cmdtbl [] = 
{
  { "/INITIALIZE", cb_pfinit}, 
  { "/initialize", cb_pfinit}, 
  { "/NETWORK_DATA", cb_gtnetdata}, 
  { "/network_data", cb_gtnetdata}, 
  { "/OLD_BASE", cb_gtbase}, 
  { "/old_base", cb_gtbase}, 
  { "/oldbase", cb_gtbase}, 
  { "/CHANGES", cb_change}, 
  { "/changes", cb_change}, 
  { "/SOLUTION", cb_solton}, 
  { "/solution", cb_solton}, 
 /* { "/getdataout", cb_gtdata}, */
  { "/GET_DATA", cb_gtdata}, 
  { "/get_data", cb_gtdata}, 
  { "/getdata", cb_gtdata}, 
  { "/REPORTS", cb_report}, 
  { "/reports", cb_report}, 
  { "/PROCESS", cb_cdproc}, 
  { "/process", cb_cdproc}, 
  { "/NEW_BASE", cb_newbse}, 
  { "/new_base", cb_newbse}, 
  { "/newbase", cb_newbse}, 
  { "/QUIT", cb_pfexit}, 
  { "/quit", cb_pfexit}, 
  { "/EXIT", cb_pfexit}, 
  { "/exit", cb_pfexit}, 
};

#define IPF_CMD_TBL_MAX (int)(sizeof(ipf_cmdtbl)/sizeof(ipf_cmdtbl[0]))
