/******************************************************
* function: srv_cmdprs
* author: Jay Coleman
* purpose:
*   Process the data in "cmd_buf" and return the result "data_buf".
*   Recursive routines are handled here (/COMMAND and /CFLOW).
*   This routine calls the fortran routine "srv_fcmdprs" to handle
*   all the remaining commands.
*******************************************************
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "ipf_ipc.h"

#ifdef WIN32
#pragma aux srv_fcmdprs_cw "^"
#endif

/* prototypes */
int p_cflow_c( char *, char * );
int p_cmdfil_c( char *, char * );
int srv_fcmdprs_cw( char * , char * );

int srv_cmdprs( char *cmd_buf, char *data_buf )
{
   char  *pcmd, *pdata, tmpstr[11];
   char  *p, *p1, *p2, *p3;
   int   status;

#ifdef DEBUG
   printf("server received:cmd_buf =\n%s\n", cmd_buf);
#endif
   *data_buf = '\0';
   if ( pcmd = strchr( cmd_buf, '\n' ) ) {
      *pcmd = '\0';
      strcpy( data_buf, cmd_buf );
      pdata = (char *)data_buf + strlen(data_buf);
      *pcmd = '\n';
      *pdata = '\n'; pdata++;
      *pdata = '\0';
   } else {
      strcat( data_buf, " *** FATAL    NO LINEFEED IN COMMAND BUFFER\n\0");
      strcat( data_buf, "\n*[EOM]\0\0");
      return 1;
   }
   if ( pcmd = strstr( cmd_buf, "\n*[" ) ) {
      strcpy( pcmd, "\n(END)\0\0" );
   } else {
/* this code is a "work around" for GUI problems with no "\n*[EOM]"
*      pcmd = (char *)cmd_buf + strlen( cmd_buf );
*      strcpy( pcmd, "\n(END)\0\0" );
*/
      strcat( data_buf, " *** FATAL    NO  *[EOM]  IN COMMAND BUFFER\n\0");
      strcat( data_buf, "\n*[EOM]\0\0");
      return 1;
   }
   /***
      memset( pdata, '\0', ( BUFSIZE - ( pdata - (char *)data_buf ) )
             * sizeof(char) );
   ***/

   /* check for commands that must be handled here */

   strncpy( tmpstr, cmd_buf, sizeof(tmpstr)-1 );
   tmpstr[sizeof(tmpstr)-1] = '\0';
   p = strtok( tmpstr, ", =\n\0" );
   p++; /*  skip the "/"  */
   for ( p2=p; *p2 != '\0'; p2++ ) *p2 = (char) toupper( (int) *p2 );

   if (        strncmp( p, "COMMAND"   , 7 ) == 0 ) {
      status = p_cmdfil_c( cmd_buf, pdata );
   } else if ( strncmp( p, "CFLOW"     , 5 ) == 0 ) {
      status = p_cflow_c( cmd_buf, pdata );
   } else {
      status = srv_fcmdprs_cw( cmd_buf, pdata );

#ifdef DEBUG
      printf("returned from srv_fcmdprs, status = %d\n", status);
      printf("data_buf = %s\n", data_buf);
#endif
   }
   strcat( data_buf, "\n*[EOM]\0\0");
   return status;
}
