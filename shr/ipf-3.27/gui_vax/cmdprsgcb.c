static char sccsid[]="@(#)cmdprsgcb.c	20.1 5/1/94";
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
/******************************************************
* function: cmdparse_gui_cb
* author: Dan Clark
* purpose: 
*   search a table of strings for a command 
*   and when found return a pointer to the corresponding
*   function which will process this command.
*   the table is layed out in the include file ipc_cmdparse
*   The functions pointers typically point to FORTRAN
*   procedures in the powerflow application and pass
*   a pointer to an in buffer and an output buffer.
*******************************************************
*/

#include <stdio.h>
#include <string.h>

#include "ipf_ipc.h"
#include "cmdparse.h"
#include "cmds_gui_cb.h"

/*************************************************
* function: print_cmd_tbl
* purpose: display a list of all commands when a bad command is sent
*
*************************************************
*/
int cbprint_cmd_tbl()
{
  int idx;

    idx = 0;
    printf("Command\tlength\n");
    while ( (idx < IPF_CMD_TBL_MAX) ) {
      printf("%s\t%d\n", ipf_cmdtbl[idx].command,
             strlen(ipf_cmdtbl[idx].command) ); 
      idx++;
    }

  return(0);
}


int cmdparse_gui_cb( char *cmd , int (**fptr)() )
{
   int idx;
   int rtncde;
   int cmd_found;

   /* do a simple search for the command on the table */

   cmd_found = FALSE;
   idx = 0;
   while ( (idx < IPF_CMD_TBL_MAX) && (cmd_found == FALSE) ) {
      rtncde=strncmp(ipf_cmdtbl[idx].command,cmd,
             strlen(ipf_cmdtbl[idx].command)); 
/***
*      printf("command %s length %d\n", ipf_cmdtbl[idx].command,
*             strlen(ipf_cmdtbl[idx].command) ); 
***/
      if (rtncde == 0) {
         /** command found set return pointer to function **/
         cmd_found = TRUE;
         *fptr = ipf_cmdtbl[idx].fptr;
      } else {
#ifdef DEBUG
         printf("current command =  %s\n", ipf_cmdtbl[idx].command);
#endif
         idx++;
      }
   } /* end while */
   if(cmd_found == FALSE) {
     cbprint_cmd_tbl();
   }

   return cmd_found;
}
