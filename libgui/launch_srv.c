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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef VMS
#include <clidef.h>
#include <libdef.h>
#include <lib$routines.H>
#include <descrip.h>
#endif


int launch_srv( char *prog_name, int sockid )
{
   int i;
   int prog_pid;
   char system_cmd[128];
   char sockid_str[16];

#ifdef VMS
   int cliflag = CLI$M_NOWAIT;
   struct dsc$descriptor_d dsc_system_cmd;

   dsc_system_cmd.dsc$w_length = (unsigned short)sizeof(system_cmd);
   dsc_system_cmd.dsc$b_dtype = (char)DSC$K_DTYPE_T;
   dsc_system_cmd.dsc$b_class = (char)DSC$K_CLASS_D;
   dsc_system_cmd.dsc$a_pointer = (char *)system_cmd;
#endif


   memset( system_cmd, '\0', sizeof( system_cmd ) );
   memset( sockid_str, '\0', sizeof( sockid_str ) );

   sprintf( sockid_str, "%d", sockid );

   strcpy( system_cmd, prog_name ? prog_name : "ipfsrv");
   strcat( system_cmd, " -socketid " );
   strcat( system_cmd, sockid_str );

#ifdef VMS
   i = lib$spawn( &dsc_system_cmd,0,0,&cliflag,0,&prog_pid,0,0,0,0,0,0,0 );
   printf("lib$spawn command = < %s >\nstatus = %d\n", system_cmd, i );
   if ( i == 1 ) i = 0;
#else
   strcat( system_cmd, " &" );
   i = system( system_cmd );
   printf("system command = < %s >\nstatus = %d\n", system_cmd, i );
#endif
   return i;
}
