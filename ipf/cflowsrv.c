/* #define DEBUG */
/*****************************************************************************
* file: cflowsrv.c
* author: Jay Coleman
* purpose: This is the set of cflow routines that are in the IPF server
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#ifdef VMS
#include <types.h>
#include <socket.h>
#include <unixio.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>
#endif

#include "ipf_ipc.h"

#ifdef WIN32
#pragma aux apnd_stat_cw "^"
#endif


static  char  cf_cmd_buf[BUFSIZE];
static  char  cf_data_buf[BUFSIZE];
static  int   cflow_socket;


/* prototypes */

int launch_cf( char *, int, int, char * );
int cflow_server( void );
int srv_cmdprs( char *, char * );
int create_socket1( int * );
int create_socket2( int *, int );
int ipcrecv( char *, int );
int ipcsend( char *, int );
void apnd_stat_cw( char *, int, char * );

/*
****************************************************************************
* function:  p_cflow_c
* author:    Jay Coleman
* purpose:   handle the command  "/CFLOW"
****************************************************************************
*/
int  p_cflow_c(char *inbuf, char *outbuf)
{
   static char  rname[16] = "p_cflow_c.c";
   int   sockid, status, max_sleep, in_window;
   char  tmp_buf[512], prog_name[128], cmd_args[256];
   char  *p, *p0, *p1, *p2, *p3;

   sockid = 1024;
   max_sleep = 30;
   in_window = 0;
   memset( prog_name, '\0', sizeof( prog_name ) );
   memset( cmd_args, '\0', sizeof( cmd_args ) );
/*
*  parse:  /cflow, program = <program name>
*        [ args = <command line args> ]
*/
   p0 = tmp_buf;
   strncpy( p0, inbuf, sizeof( tmp_buf ) );
   p = strtok( p0, ", =\n" );
   while ( p != NULL ) {
      for ( p2=p; *p2 != '\0'; p2++ ) *p2 = (char) toupper( (int) *p2 );
      if ( strncmp( p, "PROG", 4 ) == 0 ) {
         p = strtok( 0, ", =\n" );
         if ( p != NULL ) strcpy( prog_name, p );
      } else if (strncmp( p, "WAIT", 4 ) == 0 ) {
         p = strtok( 0, ", =\n" );
         if ( p != NULL ) max_sleep = atoi(p);
      } else if (strncmp( p, "WIN", 3 ) == 0 ) {
         in_window = 1;
      } else if (strncmp( p, "IN_WIN", 6 ) == 0 ) {
         in_window = 1;
      } else if (strncmp( p, "ARGS", 4 ) == 0 ) {
         p = strtok( 0, ", =\n" );
         /* check for no args, as well as "(END)", or "*[EOM]" */
         if ( p != NULL && *p != '\0' && *p != '(' && *p != '*') {
            p1 = inbuf + ( p - p0 );
            p2 = strchr( p1, '\n' );
            if ( p2 != NULL ) {
               *p2 = '\0';
               strcpy( cmd_args, p1 );
            }
            break;
         }
      }
      if ( p != NULL ) p = strtok( 0, ", =\n" );
   }

   status = create_socket1( &sockid );
   if ( status != 0 ) {
      printf("cflow_server: Warning - no IPC connection - \n");
      printf("              create_socket1 failed, status = %d\n", status );
      sprintf( outbuf, " *** ERROR    cflow_server, status = %d\n\0", status );
      apnd_stat_cw( outbuf, status, rname );
      return status;
   }
   status = launch_cf( prog_name, sockid, in_window, cmd_args );
   if ( status != 0 ) {
      printf("cflow_server: Warning - cflow program not launched - \n");
      sprintf( outbuf, " *** ERROR    cflow program not launched \n\0");
      apnd_stat_cw( outbuf, status, rname );
      return status;
   }
   status = create_socket2( &cflow_socket, max_sleep );
   if ( status != 0 ) {
      printf("cflow_server: Warning - no IPC connection - \n");
      printf("              create_socket2 failed, status = %d\n", status );
      sprintf( outbuf, " *** ERROR    cflow_server, status = %d\n\0", status );
      apnd_stat_cw( outbuf, status, rname );
      return status;
   }
   printf("cflow_server: Successful connection using socket  %d\n", sockid);
   status = cflow_server();
   if ( status == 0 ) {
      strcpy(  outbuf, " *INFORMATIVE   CFLOW normal completion\n" );
   }else{
      sprintf( outbuf, " *** ERROR    cflow_server status = %d\n\0", status );
   }
   apnd_stat_cw( outbuf, status, rname );
/* take this out to see if "linger" problem goes away
   shutdown( cflow_socket, 2 );
*/
   close( cflow_socket );
   return status;
}

/*****************************************************************************
* function: cflow_server
* author: Jay Coleman
*
* purpose: set up the cflow socket, then execute a loop similar to the IPF
* server's main loop to process commands from the cflow program
*
*
******************************************************************************/
int cflow_server( void )
{
   int  ipc_status;

   while (1) {
      /***
      memset(cf_data_buf, '\0', BUFSIZE * sizeof(char) );
      ***/
      ipc_status = ipcrecv( cf_cmd_buf, cflow_socket );
      if( ipc_status != 0 ) {
         printf("cflow_server: lost ipc connection\n");
         return ipc_status;
      }
#ifdef DEBUG
      printf("cflow_server: received cmd \n%0.30s\n", cf_cmd_buf);
#endif
      if( strncmp( cf_cmd_buf, "/cflow_done", 11 ) == 0 ) {
         return 0;
      } else {
         /* process this command */
         srv_cmdprs( cf_cmd_buf, cf_data_buf );
      }
      ipc_status = ipcsend( cf_data_buf, cflow_socket);
      if(ipc_status != 0) {
         printf("cflow_server: cannot send data - assume ipc lost\n");
         return ipc_status;
      }
   }
}
