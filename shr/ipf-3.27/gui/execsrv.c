static char sccsid[] = "@(#)execsrv.c	20.9 9/22/94";
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
/****
* file execsrv.c
* purpose: 
*   fork a child to run as the server
****/
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <signal.h>

#include <Xm/Xm.h>
#include <X11/X.h>

#include "ipf_ipc.h"
#include "ipf_rsrc.h"

extern int new_process		(char *servername, int argc,
				 char *server_argv[], int nenv, char *envp[]);
extern void ipf_itoa		(int n, char s[]);


/************************************************************
* function: excsrv
* author: Dan Clark	Apr 10
* purpose: fork a child process and startup powerflow data server
*
*************************************************************
*/
int execsrv(AppResData *app_resdata, int argc, char *argv[])
{
#define MAXARG 20

  char *server_argv[MAXARG];
  char socketstr[128], run_srv_cmd[128];
  int pid_server;
  char *envp[1];
  int sargc;
  int nenvp;
/*  static char exec_cmd[] = "ipf_exec"; */
  static char sock_cmd[] = "-socketid";

  strcpy (run_srv_cmd, getenv("RUN_IPFSRV"));
  nenvp = 0;
  envp[nenvp] = NULL;

  if(app_resdata->server) {

    /*** fork the server (ipf data server) ***/

    ipf_itoa(app_resdata->socketid, socketstr);
    sargc = 0;
/*
#ifdef VMS
*   server_argv[sargc++] = exec_cmd;
**#endif
*   server_argv[sargc++] = run_srv_cmd;
*/
#ifndef VMS
    server_argv[sargc++] = run_srv_cmd;
#endif
    server_argv[sargc++] = app_resdata->servername;
    server_argv[sargc++] = sock_cmd;
    server_argv[sargc++] = socketstr;
    server_argv[sargc++] = NULL;

    pid_server = new_process(server_argv[0], 
                             sargc, server_argv, nenvp, envp);

/*
*   printf("client process continues child pid = %d\n", pid_server);
*   if(app_resdata->sleep > 0) {
*     printf("GUI waiting for %d seconds for ipfsrv to start\n", 
*             app_resdata->sleep);
*     sleep((unsigned int)app_resdata->sleep);
*   }
*/
  }
  return(0);
}
