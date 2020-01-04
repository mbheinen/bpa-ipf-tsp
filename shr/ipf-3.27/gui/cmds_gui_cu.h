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
* the forward declaration of all client callup functions
***/
extern int cu_gtnetdata();
extern int cu_pfinit();
extern int cu_gtbase();
extern int cu_change();
extern int cu_solton();
extern int cu_gtdata();
extern int cu_report();
extern int cu_cdproc();
extern int cu_newbse();
extern int cu_pfexit();

/***
* static declaration of parse table
***/
static CmdTbl ipf_cmdtbl [] = 
{
  { "/INITIALIZE", cu_pfinit }, 
  { "/initialize",  cu_pfinit }, 
  { "/NETWORK_DATA", cu_gtnetdata }, 
  { "/network_data", cu_gtnetdata }, 
  { "/OLD_BASE", cu_gtbase }, 
  { "/old_base", cu_gtbase }, 
  { "/oldbase", cu_gtbase }, 
  { "/CHANGES", cu_change }, 
  { "/changes", cu_change }, 
  { "/SOLUTION", cu_solton }, 
  { "/solution", cu_solton }, 
 /* { "/getdataout", cu_gtdata }, */
  { "/GET_DATA", cu_gtdata }, 
  { "/get_data", cu_gtdata }, 
  { "/getdata", cu_gtdata }, 
  { "/REPORTS", cu_report }, 
  { "/reports", cu_report }, 
  { "/PROCESS", cu_cdproc }, 
  { "/process", cu_cdproc }, 
  { "/NEW_BASE", cu_newbse }, 
  { "/new_base", cu_newbse }, 
  { "/newbase", cu_newbse }, 
  { "/QUIT", cu_pfexit }, 
  { "/quit", cu_pfexit }, 
  { "/EXIT", cu_pfexit }, 
  { "/exit", cu_pfexit }, 
};

#define IPF_CMD_TBL_MAX (int)(sizeof(ipf_cmdtbl)/sizeof(ipf_cmdtbl[0]))
