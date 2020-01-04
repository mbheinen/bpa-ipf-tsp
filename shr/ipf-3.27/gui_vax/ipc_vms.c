/*************************************************************
* file: ipc_vms.c
* author: Jay Coleman
* purpose: interprocess communications routines - VMS mailbox version
**************************************************************
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <types.h>
#include <time.h>
#include <perror.h>
#include <climsgdef.h>
#include <unixio.h>
#include <file.h>
#include <starlet.h>
#include <ssdef.h>
#include <descrip.h>
#include <signal.h> 
#include <stat.h>
#include <limits.h>

#include "ipf_ipc.h"

#ifndef FD_SETSIZE
#define FD_SETSIZE 256
#endif

   static int ipcsendCnt = 0;
   static int ipcrecvCnt = 0;

int   ipcGetSendCnt()    { return ipcsendCnt     ; }
int   ipcGetRecvCnt()    { return ipcrecvCnt     ; }

/***********************************************************
* note that the first declaration of the signal handler 
* corresponds to the definition of the online documentation but
* will not pass the ansi compiler check with more than one 
* integer argument.
************************************************************
*/
/* void signal_handler(int sig, int code, struct sigcontext *scp) */
void signal_handler(int sig)
{
    /* printf("Signal %d, code %d\n", sig, code); */
}

/*******************************************************
* function: create_socket
* author: Jay Coleman
* purpose:
*  create the "socket" (mailbox)
*  in order to perform inter-process communication
*
*******************************************************
*/
                                   
int create_socket( int sock_objnum, int *ipc_socket )
{
#ifndef L_cuserid
#define L_cuserid 11
#endif

   static  char userid[L_cuserid];
   static  int socks,sockr, mchan, mc_stat;
   static  char mbox_c[31];
   static  char mbox_d[31];
   static  char tmp_str[31];
   static struct dsc$descriptor_d dsc_data_c;
   static struct dsc$descriptor_d dsc_data_d;

   cuserid( userid );
   strcpy( mbox_c, userid );
   strcpy( mbox_d, userid );
   sprintf( tmp_str, "%d", sock_objnum );
   strcat( mbox_c, tmp_str );
   strcat( mbox_d, tmp_str );
   strcat( mbox_c, "C" );
   strcat( mbox_d, "D" );

   dsc_data_c.dsc$w_length = (unsigned short)strlen( mbox_c );
   dsc_data_c.dsc$b_dtype = (char)DSC$K_DTYPE_T;
   dsc_data_c.dsc$b_class = (char)DSC$K_CLASS_D;
   dsc_data_c.dsc$a_pointer = mbox_c;

   dsc_data_d.dsc$w_length = (unsigned short)strlen( mbox_d );
   dsc_data_d.dsc$b_dtype = (char)DSC$K_DTYPE_T;
   dsc_data_d.dsc$b_class = (char)DSC$K_CLASS_D;
   dsc_data_d.dsc$a_pointer = mbox_d;

/*
   mc_stat = sys$crembx( 0, &mchan, 8192, 16384, 0, 0, &dsc_data_c ,0 );
*/
   mc_stat = sys$crembx( 0, &mchan, 0, 0, 0, 0, &dsc_data_c ,0 );
   if ( mc_stat != SS$_NORMAL ) return mc_stat;
/*
   mc_stat = sys$crembx( 0, &mchan, 8192, 16384, 0, 0, &dsc_data_d ,0 );
*/
   mc_stat = sys$crembx( 0, &mchan, 0, 0, 0, 0, &dsc_data_d ,0 );
   if ( mc_stat != SS$_NORMAL ) return mc_stat;
   socks = open( mbox_c, O_RDWR, 0 );
   sockr = open( mbox_d, O_RDWR, 0 );
   *ipc_socket = ( socks * 1000 ) + sockr;

   return I_OK;
}
/***********************************************************
* function: connect_to_socket
* author: Jay Coleman
* purpose:
*  connect to an existing "socket" (mailbox)
*  in order to perform inter-process communication
************************************************************
*/
int connect_to_socket( char *nodename, int sock_objnum, int *ipc_socket )
{
   static int status, socks, sockr;

   status = create_socket( sock_objnum, ipc_socket );
   /* reverse "send" and "receive" */
   socks = *ipc_socket / 1000;
   sockr = *ipc_socket - ( socks * 1000 );
   *ipc_socket = ( sockr * 1000 ) + socks;
   return status;
}

/***********************************************************
* function: ipcsend
* author: Dan Clark/Jay Coleman
* purpose: to send data across ipc using sockets
*
* enhancements:
* find a more efficient means of checking length of inputstr
************************************************************
*/

#ifdef TESTBUF
int ipcsend( Buffer inputstr, int ipc_socket )
#else
int ipcsend( char *inputstr, int ipc_socket )
#endif
{
   static int length;
   static int tlength;
   static int split;
   static char slength[5];

   ipc_socket = ipc_socket / 1000;
   length = strlen(inputstr);

   /* Only send non-empty lines */
   if( length <= 0 ) {
      fprintf(stderr, "ERROR: ipcsend got a null input string\n");
      perror("ipcsend got null string");
      return I_NO_DATA;
   }
   sprintf(slength,"%4d",length);

   if( (tlength = write(ipc_socket, slength, 4)) <= 0 ) {
      fprintf(stderr, "ERROR: ipcsend couldn't write data over network\n");
      perror("ipcsend couldn't write to socket");
      return I_ERROR;
   }

#ifdef PARTIALMSG /* split lines to simulate pasteing msgs together */
   split = (rand() % length-1) + 1;
   write(ipc_socket, inputstr, split); /* split message up to confuse recv */
   if( write(ipc_socket, &inputstr[split], length-split) < 0 ) {
#else
   if( (tlength = write(ipc_socket, &inputstr[0], length)) <= 0 ) {
#endif /* PARTIALMSG */
      fprintf(stderr, "ERROR: ipcsend couldn't write data over network\n");
      perror("ipcsend couldn't write to socket");
      return I_ERROR;
   }
   ipcsendCnt++;
   return I_OK;
}

/*************************************************
* function: ipcrecv
* author: Dan Clark/Jay Coleman
* purpose: receive a buffer of data over the ipc channel
* this routine is used by both the client and server side
* returns -1 for a failed read
* returns I_LOST_CONNECTION for a zero length read
*  which generally implies a lost server
*
* enhancements:
**************************************************
*/

#ifdef TESTBUF
int ipcrecv( Buffer outputstr, int ipc_socket )
#else
int ipcrecv( char *outputstr, int ipc_socket )
#endif
{
   static int length;
   static int bsize;
   static int curptr;
   static char asize[10];

   ipc_socket %= 1000;

   memset(asize, '\0', sizeof(asize));

   if( (length = read(ipc_socket, asize, 1) ) < 0 ) {
      printf("length=%d\n",length);
      fprintf(stderr, "ERROR: ipcrecv couldn't read line over network\n");
      perror("ipcrecv couldn't read from socket");
      return I_ERROR;
   }

   if(length == 0) {
      fprintf(stderr, "ERROR: ipcrecv - ipc connection lost\n");
      perror("ipcrecv lost socket connection");
      return I_LOST_CONNECTION;
   }

   if ( *asize == '\n' ) {
      curptr = 0;
      bsize = 5;
   } else {
      curptr = 1;
      bsize = 4;
   }

   while (bsize > 0) {
      if( (length = read(ipc_socket, &asize[curptr], bsize) ) < 0 ) {
         printf("length=%d\n",length);
         fprintf(stderr, "ERROR: ipcrecv couldn't read line over network\n");
         perror("ipcrecv couldn't read from socket");
         return I_ERROR;
      }

      if(length == 0) {
         fprintf(stderr, "ERROR: ipcrecv - ipc connection lost\n");
         perror("ipcrecv lost socket connection");
         return I_LOST_CONNECTION;
      }

      bsize -= length;
      curptr += length;

   } /* end while */

   if ( asize[4] == '\n' ) {
      curptr = 0;
      asize[4] = '\0';
      bsize = atol(asize);
   } else {
      curptr = 1;
      outputstr[0] = asize[4];
      asize[4] = '\0';
      bsize = atol(asize) - 1;
   }

   while (bsize > 0) {
/*     if (curptr != 0) printf("bsize=%d, curptr=%d\n",bsize,curptr); */
      if( (length = read(ipc_socket, &outputstr[curptr], bsize) ) < 0 ) {
         printf("length=%d\n",length);
         fprintf(stderr, "ERROR: ipcrecv couldn't read line over network\n");
         perror("ipcrecv couldn't read from socket");
         return I_ERROR;
      }

      if(length == 0) {
         fprintf(stderr, "ERROR: ipcrecv - ipc connection lost\n");
         perror("ipcrecv lost socket connection");
         return I_LOST_CONNECTION;
      }

/*      if (bsize != length) printf("bsize=%d, length=%d\n",bsize,length); */
      bsize -= length;
      curptr += length;

   } /* end while */

   outputstr[curptr] = '\0';

#ifdef DEBUG
   printf("ipcrecv buffer=%s*\n", outputstr);
#endif

   ipcrecvCnt++;
   return I_OK;
}

