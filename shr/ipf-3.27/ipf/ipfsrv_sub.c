/******************************************************************************
* file: ipfsrv_sub.c
* purpose:  This is the IPF server main program.  It consists of initialization
*    code followed by a "while forever" processing loop to process commands
*    from the Inter-Process Communication (IPC) socket interface.
******************************************************************************/

/* #define DEBUG */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
/* #include <signal.h>   needed only for "sleep" */

#include "ipf_ipc.h"

/* prototypes */
#ifdef WIN32
#pragma aux initlz_cw "^"
#pragma aux ipf_fexit_cw "^"
#pragma aux pfinit_cw "^"
#endif

/* void ftn77init( void ); *** not needed except for Salford Fortran ***/
int connect_to_socket( char *, int, int * );
int ipcrecv( char *, int );
int ipcsend( char *, int );
int srv_cmdprs( char * , char * );
int initlz_cw( void );
void pfinit_cw( void );
void ipf_fexit_cw( void );
/*** debug only
FILE *logfileptr = NULL;
***/

static  char  cmd_buffer[BUFSIZE];
static  char  data_buffer[BUFSIZE];
static  int   done;
static  int   sockid;
static  int   ipf_socket;
static  int   sockin  = 0;
static  int   sockout = 1;
static  int   status;
static  int   ipc_status;
static  char  nodename[128];
/*** static  char  response[10]; ***/

/*** void ipfsrv_sub( int argc, char *argv[], char *envp[] )  ***/
void ipfsrv_sub( int argc, char *argv[] )
{
   int  socknum, i;
   char *p;

   memset( nodename, '\0', sizeof( nodename ) );
   sockid = I_SOCKID;
   socknum = 0;
   if ( argc == 2 ) {
      socknum = atoi( argv[1] );
   } else {
      for ( i=1; i < argc; i++ ) {
         for ( p=argv[i]; *p != '\0'; p++ ) *p = (char) toupper( (int) *p );
         if (       strncmp( argv[i], "-NODE", 5 ) == 0 ) {
            i++;
            strcpy( nodename, argv[i] );
         } else if (strncmp( argv[i], "-SOCK", 5 ) == 0 ) {
            i++;
            socknum = atoi( argv[i] );
         }
      }
   }
   if( socknum == 0 ) {
      printf("Warning - improper use of function\n");
      printf("usage %s -sock <socketid> [-node <nodename>]\n", argv[0]);
      sockid = I_SOCKID;
      printf("setting socket address to default value %d\n", sockid);
   } else {
      sockid = socknum;
   }
   /*** ftn77init(); *** not needed except for Salford Fortran ***/
   printf("SERVER: Initialize connection to socket id %d\n", sockid);
   fflush(stdout);
   /* sleep(3); */
   ipc_status = connect_to_socket( nodename, sockid, &ipf_socket );
   if(ipc_status != 0) {
      printf("Warning - no IPC connection\n");
      /***
      printf(" Continue ? <y|n> n ");
      gets(response);
      if(toupper(response[0]) != 'Y' ){
      ***/
         printf("Exiting...\n");
         exit(0);
      /*** } ***/
   } else {
      sockin  = ipf_socket;
      sockout = ipf_socket;
   }

   status = initlz_cw();  /* init for cpyinpbuf */
   pfinit_cw();

   done = FALSE;

   while (!done ) {
      /***
      memset( (char *)data_buffer, '\0', BUFSIZE * sizeof(char) );
      ***/
      ipc_status = ipcrecv( cmd_buffer, sockin );
      if( ipc_status != 0 ) {
         printf("server lost ipc connection perform orderly shutdown\n");
         done = TRUE;
         break;
      }
      srv_cmdprs( cmd_buffer, data_buffer );
      ipc_status = ipcsend( data_buffer, sockout );
      if(ipc_status != 0) {
         printf("server cannot send data - exiting ...\n");
         done = TRUE;
         break;
      }
   }
   printf("ipfsrv: ipc shutdown - ipc_status %d \n", ipc_status);
   fflush(stdout);
/*
*   strcpy(  (char *)cmd_buffer, "/exit\n*[EOM]\n");
*   srv_cmdprs( cmd_buffer, data_buffer );
*/
   ipf_fexit_cw();
   exit(1); /* should not return from "ipf_fexit_cw"  */
}

