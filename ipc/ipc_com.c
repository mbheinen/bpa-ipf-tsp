/*************************************************************
* file: ipc_com.c
* author: Dan Clark/Jay Coleman
* purpose: interprocess communications routines common to all protocols
**************************************************************
*/
#ifdef VMS
#include <stdio>
#include <stdlib>
#include <string>
#include <errno>
#include <types>
#include <socket>
#else   /* VMS */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#endif   /* VMS */

#include "ipf_ipc.h"

static unsigned int sock_rbuf_size;
static unsigned int sock_sbuf_size;
static unsigned int isz = sizeof( int );
static unsigned int one = 1;
static char *onec = (char *)&one;
static char *sbc = (char *)&sock_sbuf_size;
static char *rbc = (char *)&sock_rbuf_size;
static int isz1 = sizeof( int );

static int ipcsendCnt = 0;
static int ipcrecvCnt = 0;

int ipcGetSendCnt() { return ipcsendCnt; }
int ipcGetRecvCnt() { return ipcrecvCnt; }

/***********************************************************
* function: ipc_unix_socket_setup
* author: Jay Coleman
************************************************************
*/
void ipc_unix_socket_setup( int sock )
{
   if ( getsockopt( sock, SOL_SOCKET, SO_SNDBUF, sbc, &isz1 ) != 0 )
     printf("ipc - setsockopt <SNDBUF> failed, errno = %d\n", errno);
   if ( sock_sbuf_size < 1024 || sock_sbuf_size > 4096 ) sock_sbuf_size = 4096;
   return;
}
/***********************************************************
* function: ipc_inet_socket_setup
* author: Jay Coleman
************************************************************
*/
void ipc_inet_socket_setup( int sock )
{
 /*
   if ( setsockopt( sock, SOL_SOCKET, SO_USELOOPBACK, onec, isz ) != 0 )
     printf("ipc - setsockopt <LOOPBACK> failed, errno = %d\n", errno);
  */

   if ( getsockopt( sock, SOL_SOCKET, SO_SNDBUF, sbc, &isz1 ) != 0 )
     printf("ipc - setsockopt <SNDBUF> failed, errno = %d\n", errno);
   if ( sock_sbuf_size < 1024 || sock_sbuf_size > 4096 ) sock_sbuf_size = 4096;
/*
** if ( getsockopt( sock, SOL_SOCKET, SO_RCVBUF, rbc, &isz1 ) != 0 )
**   printf("ipc - setsockopt <RCVBUF> failed, errno = %d\n", errno);
** if ( sock_rbuf_size < 1024 || sock_rbuf_size > 4096 ) sock_rbuf_size = 4096;
*/
   return;
}
/***********************************************************
* function: ipcsend
* author: Dan Clark/Jay Coleman
* purpose: to send data across ipc using sockets
************************************************************
*/
#ifdef TESTBUF
int ipcsend( Buffer ipcsendstr, int ipc_socket )
#else
int ipcsend( char *ipcsendstr, int ipc_socket )
#endif
{
   int length;
   int tlength;
   int curptr;
   char slength[5];
   int flag = 0;
   int sbuf_size;

   sbuf_size = sock_sbuf_size;

#ifdef PARTIALMSG
/* simulate small socket buffers for testing */
   sbuf_size = 1024;
#endif /* PARTIALMSG */

   length = strlen(ipcsendstr);

   /* Only send non-empty lines */
   if ( length <= 0 ) {
      fprintf(stderr, "ERROR: ipcsend got a null input string\n");
      perror("ipcsend got null string");
      return I_NO_DATA;
   }
   sprintf(slength,"%4d",length);

   if ( send(ipc_socket, slength, 4, flag) < 4 ) {
      fprintf(stderr, "ERROR: ipcsend couldn't write data over network\n");
      perror("ipcsend couldn't write to socket");
      return I_ERROR;
   }
   curptr = 0;
   while ( length > 0 ) {
      tlength = length;
      if ( tlength > sbuf_size ) tlength = sbuf_size;
      if ( send(ipc_socket, &ipcsendstr[curptr], tlength, flag) < tlength ) {
         fprintf(stderr, "ERROR: ipcsend couldn't write data over network\n");
         perror("ipcsend couldn't write to socket");
         return I_ERROR;
      }
      length -= tlength;
      curptr += tlength;
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
int ipcrecv( Buffer ipcrecvstr, int ipc_socket )
#else
int ipcrecv( char *ipcrecvstr, int ipc_socket )
#endif
{
int length;
int bsize;
int curptr;
char asize[10];
int flag = 0;

   memset(asize, '\0', sizeof(asize));
   curptr = 0;
   bsize = 4;
   while (bsize > 0) {
      if( (length = recv(ipc_socket, &asize[curptr], bsize, flag) ) < 0 ) {
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

   curptr = 0;
   bsize = atol(asize);
   while (bsize > 0) {
/*     if (curptr != 0) printf("bsize=%d, curptr=%d\n",bsize,curptr); */
      if ((length = recv(ipc_socket, &ipcrecvstr[curptr], bsize, flag)) < 0) {
         printf("length=%d\n",length);
         fprintf(stderr, "ERROR: ipcrecv couldn't read line over network\n");
         perror("ipcrecv couldn't read from socket");
         return I_ERROR;
      }
      if (length == 0) {
         fprintf(stderr, "ERROR: ipcrecv - ipc connection lost\n");
         perror("ipcrecv lost socket connection");
         return I_LOST_CONNECTION;
      }

/*      if (bsize != length) printf("bsize=%d, length=%d\n",bsize,length); */
      bsize -= length;
      curptr += length;
   } /* end while */

   ipcrecvstr[curptr] = '\0';

#ifdef DEBUG
   printf("ipcrecv buffer=%s*\n", ipcrecvstr);
#endif

   ipcrecvCnt++;
   return I_OK;
}
 
