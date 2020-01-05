static char sccsid[]="@(#)ipcsrvstbs.c	20.2 2/15/95";
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
/*******************************************
* file: ipc_client_stubs.c
* author: Dan Clark
* date: Feb, 1992
* purpose: 
*   define a set of stub routines so that the server can link
*   without actually haveing the client callback and callup routines
*   linked in with the program.
*
*******************************************
*/

/***
* the declaration of all server routines
***/
int p_gtnetdata_(char *inbuf, char *outbuf) {prtError(); return(-1);};
int p_pfinit_(char *inbuf, char *outbuf) {prtError(); return(-1);};
int p_gtbase_(char *inbuf, char *outbuf) {prtError(); return(-1);};
int p_change_(char *inbuf, char *outbuf) {prtError(); return(-1);};
int p_solton_(char *inbuf, char *outbuf) {prtError(); return(-1);};
int p_gtdata_(char *inbuf, char *outbuf) {prtError(); return(-1);};
int p_report_(char *inbuf, char *outbuf) {prtError(); return(-1);};
int p_cdproc_(char *inbuf, char *outbuf) {prtError(); return(-1);};
int p_newbse_(char *inbuf, char *outbuf) {prtError(); return(-1);};
int p_pfexit_(char *inbuf, char *outbuf) {prtError(); return(-1);};
int p_cflow_gui(char *inbuf, char *outbuf) {prtError(); return(-1);};

