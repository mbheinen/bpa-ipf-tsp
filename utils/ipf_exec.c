#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char blank[] = " ";
static char system_cmd[128];

main ( int argc, char *argv[] )
{
   int i;

   strcpy( system_cmd, argv[1] );
   for ( i=2; i < argc; i++ ) {
      strcat( system_cmd, blank );
      strcat( system_cmd, argv[i] );
   }
   printf("system command = < %s >\n", system_cmd );
   i = system( system_cmd );
   printf("system command = < %s >\nstatus = %d\n", system_cmd, i );
   exit(0);
}
