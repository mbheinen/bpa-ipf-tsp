static char sccsid[] = "@(#)new_process.c	1.6 3/19/98";
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
* file new_process.c
* purpose: 
*   fork a child to run as the server
****/
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#ifdef VMS
#include <types.h>
#include <processes.h>		/* for fork and exec */
#include <signal.h>		/* for sleep functions */
#define fork vfork
#else
#include <unistd.h>		/* for fork exec and sleep */
#include <sys/types.h>
#include <sys/signal.h>		/* for pid_t definition sleep functions */
#endif

extern void ipf_itoa(int n, char s[]);


/************************************************************
* function: new_process
* author: Dan Clark	Apr 10
* purpose: fork a child process and startup powerflow data server
* note:
*  may want to set the enviroment variables of the child
*  process after the fork
*
*************************************************************
*/
int new_process(char *servername, 
int argc, char *server_argv[], int nenv, char *envp[])
{
  int debug = 0;
#if VMS
  pid_t pid_server;

      if((pid_server = fork()) == 0) {        
        if(servername != NULL) {
          printf("server process %s starts here pid = %d\n", 
		servername, pid_server);
        }
        if(debug==1) {
          printf(" server_argv[0]=%s*\n", server_argv[0]);
          printf(" server_argv[1]=%s*\n", server_argv[1]);
        }
        execv(servername, server_argv);
        printf("ERROR server process Failed pid = %d socketid %s\n",
               pid_server, server_argv[0]);
        perror(server_argv[0]);
        exit(-1);
      } else if (pid_server < 0) {
         printf("FORK FAILED continue without server\n");
         perror("fork failed");
         /* printf("errno = %d\n", errno); */
      }
  return (int)pid_server;
#else /* VMS */
   int i;
   static char blank[] = " ";
   static char cmd_bg[] = " &";
   static char system_cmd[256];

   strcpy( system_cmd, server_argv[0] );
   for ( i=1; i < argc && server_argv[i] != NULL; i++ ) {
      strcat( system_cmd, blank );
      strcat( system_cmd, server_argv[i] );
   }
   strcat( system_cmd, cmd_bg );
   i = system( system_cmd );
   printf("system command = < %s >\nstatus = %d\n", system_cmd, i );
   return i;
#endif /* VMS */
}
