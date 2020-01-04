
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef VMS
#include <clidef.h>
#include <libdef.h>
#include <lib$routines.H>
#include <descrip.h>
#endif


int launch_cf( char *prog_name, int sockid, int in_window, char *prog_args )
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

   if ( in_window ) strcpy( system_cmd, "run_cflow_win " );
   else             strcpy( system_cmd, "run_cflow_bg " );
   strcat( system_cmd, prog_name );
   strcat( system_cmd, " -connect " );
   strcat( system_cmd, sockid_str );
   strcat( system_cmd, " " );
   strcat( system_cmd, prog_args );

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
