/*****************************************************************************
* file: cflow_lib_ipc.c
* author: Jay Coleman
* purpose: 
*
*   This is the base set of cflow routines that are needed for all of the
*   "low level" <cflow program> to <powerflow> inter-process communication.
*
*   The routines  "create_socket1", "create_socket2", "connect_to_socket",
*   "ipcsend", and "ipcrecv"  are in the "ipc" library.
*
*   "run_ipfsrv" is a separate file, to allow complete replacement,
*   if needed, for system dependent reasons.
*
******************************************************************************/

#ifdef VMS
#include  <stdio>
#include  <stdlib>
#include  <string>
#include  <ctype>
#include  <errno>
#include  <clidef>
#include  <libdef>
#include  <lib$routines>
#include  <descrip>
#else /* VMS */
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  <ctype.h>
#include  <errno.h>
#endif /* VMS */

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define CFLOW_GLOBAL_DATA
#include "cflowlib.h"

int create_socket1( int * );
int create_socket2( int *, int );
int connect_to_socket( char *, int, int * );
int ipcsend( char *, int );
int ipcrecv( char *, int );
int run_ipfsrv( int );

static const char m1[] = "Error creating socket for connection to IPF server";
static const char m2[] = "Error connecting to IPF server socket";
static int pf_connect;
static int pf_server;
static int pf_wait;
static char node_name[128];

/*****************************************************************************
* function: pf_usage_exit
* author: Jay Coleman
******************************************************************************/
void pf_usage_exit(char *program_name)
{
   printf("Warning - improper use of function - usage: \n");
   printf("%s [cfprog args] \n", program_name);
   printf("   --  OR -- \n");
   printf("%s -connect <socknum> [cfprog args] \n", program_name);
   printf("   --  OR for cflowlib debugging -- \n");
   printf("%s [-noserver] [-wait <seconds>] [cfprog args] \n", program_name);
   exit(1);
}

/*****************************************************************************
* function: pf_cflow_init
* author: Jay Coleman
*
* purpose: initialize the cflow client program for socket communication
*          with the IPF server
******************************************************************************/
void pf_cflow_init( int argc, char *argv[] )
{
   int  socknum;
   int  error;
   int  i, j, k;
   char c, *p;

   printf("Now running %s\n", argv[0]);

#ifdef DEBUG
    for(i = 0; i < argc; i++) { 
    printf("argv[%d] = %s \n", i, argv[i]);
   }
#endif

   pf_connect = FALSE;
   pf_server = TRUE;
   pf_wait = 30;
   socknum = 0;
   i = 1;
   while ( i < argc ) {
      if ( argv[i][0] == '-' ) {
         c = (char) toupper( (int) argv[i][1] );
         switch ( c ) {
         case 'C' :
            pf_connect = TRUE;
            socknum = atoi( argv[i+1] );
            for ( j=i,k=i+2; k < argc; j++,k++ ) argv[j] = argv[k];
            argc -= 2;
            break;
         case 'N' :
            pf_server = FALSE;
            for ( j=i,k=i+1; k < argc; j++,k++ ) argv[j] = argv[k];
            argc--;
            break;
         case 'W' :
            pf_wait = atoi( argv[i+1] );
            for ( j=i,k=i+2; k < argc; j++,k++ ) argv[j] = argv[k];
            argc -= 2;
            break;
         default:
            pf_usage_exit(argv[0]);
            break;
         }
      } else {
         i++;
      }
   }
   if ( pf_connect ) {
      if ( socknum < 1024  ||  socknum > 5000 ) {
      printf("socket number not in range 1024-5000\n");
      pf_usage_exit(argv[0]);
      }
      memset( node_name, '\0', sizeof( node_name ) );
      error = connect_to_socket( node_name, socknum, &pf_cflow_socket );
      if( error ) {
         perror( "cflow program " );
         printf("cflow program : %s\n", m2 );
         exit(1);
       }
   } else {
      socknum = 1024;
      error = create_socket1( &socknum );
      if( error ) {
         perror( "cflow program " );
         printf("cflow program : %s\n", m1 );
         exit(1);
      }
      if ( pf_server ) {
         error = run_ipfsrv( socknum );
         if( error ) {
            perror( "cflow program " );
            printf("cflow program : %s\n", m1 );
            exit(1);
         }
      }
      error = create_socket2( &pf_cflow_socket, pf_wait );
      if( error ) {
         perror( "cflow program " );
         printf("cflow program : %s\n", m1 );
         exit(1);
      }
   }
   return;
}

/*****************************************************************************
* function: pf_cflow_exit
* author: Jay Coleman
*
* purpose: called from cflow program at exit to notify the IPF server that
*          a normal exit is occuring.
******************************************************************************/

void pf_cflow_exit()
{
   int  error;

   if ( pf_connect ) {
      strcpy( pf_cflow_outbuf, "/cflow_done\n*[EOM]\n" );
   } else {
      strcpy( pf_cflow_outbuf, "/exit\n*[EOM]\n" );
   }
   error = ipcsend( pf_cflow_outbuf, pf_cflow_socket );
   return;
}

/*****************************************************************************
* function: pf_cflow_ipc
* author: Jay Coleman
*
* purpose: called to do the communication to the IPF server
******************************************************************************/

int pf_cflow_ipc()
{
/***
   int  error;
   error = ipcsend( pf_cflow_outbuf, pf_cflow_socket );
   if( error ) return error;
   error = ipcrecv( pf_cflow_inbuf, pf_cflow_socket );
   return error;
***/
   if( ipcsend( pf_cflow_outbuf, pf_cflow_socket ) ) exit(1);
   if( ipcrecv( pf_cflow_inbuf,  pf_cflow_socket ) ) exit(1);
   return 0 ;
}

