
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#ifdef VMS
#include  clidef
#include  libdef
#include  lib$routines
#include  descrip
#endif /* VMS */

int run_ipfsrv( int socknum )
{
   int i;
   char socknumstr[10];
   char system_cmd[128];
#ifdef VMS
   int prog_pid;
   int cliflag = CLI$M_NOWAIT;
   struct dsc$descriptor_d dsc_system_cmd;

   dsc_system_cmd.dsc$w_length = (unsigned short)sizeof(system_cmd);
   dsc_system_cmd.dsc$b_dtype = (char)DSC$K_DTYPE_T;
   dsc_system_cmd.dsc$b_class = (char)DSC$K_CLASS_D;
   dsc_system_cmd.dsc$a_pointer = (char *)system_cmd;
#endif /* VMS */

   memset( system_cmd, '\0', sizeof( system_cmd ) );
   memset( socknumstr, '\0', sizeof( socknumstr ) );
   sprintf( socknumstr, "%d", socknum );

   strcpy( system_cmd, getenv ("IPFSRV_CF") );
   strcat( system_cmd, " -socket " );
   strcat( system_cmd, socknumstr );
#ifdef VMS
   i = lib$spawn( &dsc_system_cmd,0,0,&cliflag,0,&prog_pid,0,0,0,0,0,0,0 );
   if ( i == 1 ) i = 0; else i = 1;
#else /* VMS */
   strcat( system_cmd, " &" );
   i = system( system_cmd );
#endif /* VMS */
   if ( i ) {
      printf("run_ipfsrv cmd = < %s >\nstatus = %d\n", system_cmd, i );
      perror("run_ipfsrv cmd");
   }
      printf("run_ipfsrv cmd = < %s >\nstatus = %d\n", system_cmd, i );
   return i;
}
