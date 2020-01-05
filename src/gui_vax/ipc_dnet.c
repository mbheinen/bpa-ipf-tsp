/*************************************************************
* file: ipc_dnet.c
* author: Dan Clark/Jay Coleman
* purpose: interprocess communications routines - DECnet version
**************************************************************
*/
#include <stdio.h>
#include <assert.h>
#include  <errno.h>

#ifdef VAXC
#include  <types.h>
#include  <socket.h>
#include  <dn.h>
#include  <dnetdb.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netdnet/dn.h>
#include <netdnet/dnetdb.h>
#endif

/* include <sys/wait.h> */
/* include <signal.h> */

#include "ipf_ipc.h"

#define DEBUG

/* extern int ipcclientret(); */

   static int ipcsendCnt = 0;
   static int ipcrecvCnt = 0;

int   ipcGetSendCnt()    { return ipcsendCnt     ; }
int   ipcGetRecvCnt()    { return ipcrecvCnt     ; }

/***********************************************************
* function: connect_to_socket
* author: Dan Clark/Jay Coleman
* purpose: 
*  connect to an existing socket in order to perform
*  inter-process communication
*
************************************************************
*/

int connect_to_socket( char *nodename, int sock_objnum, int *ipc_socket )
{
   int sock;
   static struct  sockaddr_dn   sockaddr;
          struct  dn_naddr     *node_addr;
          struct  nodeent      *nodep;

#ifdef DEBUG
   char *host_name;
   char host_name_str[31];
   int host_name_len = 30;
#endif

#ifdef DEBUG
   printf("ipc Initialize begin\n");

   host_name = host_name_str;
   gethostname(host_name, host_name_len);
   printf("host name: %s\n", host_name );
#endif

   if ((sock = socket(AF_DECnet, SOCK_SEQPACKET, 0)) < 0) {
      fprintf(stderr, "connect_to_socket: failed on socket call ( AF_DECnet protocol )\n");
      perror("socket");
      return I_ERROR;
   }

   memset(&sockaddr, NULL, sizeof(sockaddr));
   sockaddr.sdn_family = AF_DECnet;
   sockaddr.sdn_objnum = sock_objnum;

   if( ( node_addr = dnet_addr(nodename)) == NULL ) {
      if( ( nodep = getnodebyname(nodename)) == NULL ) {
         fprintf(stderr, "%s: Node unknown\n", nodename);
         fprintf(stderr, "ipc - addr failed sock obj = %d\n", sock_objnum);
         perror( "getnodebyname");
         return I_ERROR;
      } else {
/*         bcopy(nodep->n_addr, sockaddr.sdn_nodeaddr, nodep->n_length); */
         memcpy(nodep->n_addr, sockaddr.sdn_nodeaddr, nodep->n_length);
         sockaddr.sdn_nodeaddrl = nodep->n_length;
      }
   } else {
      sockaddr.sdn_add = *node_addr;
   }

   /* Connect to partner on specified node */
   if ( connect(sock, &sockaddr, sizeof(sockaddr)) < 0 ) {
      fprintf(stderr, "ipc failed on connect\n");
      return I_ERROR;
   }
   *ipc_socket = sock;

#ifdef DEBUG
    printf("client assigned sock %d to socket %d\n", sock, *ipc_socket );
#endif

    /*** initialize asynchronous read of the socket ***/
    /* XtAddInput(sock, XtInputReadMask, ipcclientret, NULL); */

   return I_OK;
}

/*******************************************************
* function: create_socket
* author: Dan Clark/Jay Coleman
* derived from decnet demo program dnetecho1d
* purpose: 
*  create the socket for the "server" (with respect to IPC processing)
*  in order to perform inter-process communication
*
*******************************************************
*/

int create_socket( int sock_objnum, int *ipc_socket )
{
   int s, ns, acclen, rdlen;
   static struct sockaddr_dn sockaddr;
   static struct sockaddr_dn accsockaddr;
   static  char            hostname[256];  /* Name of local host.  */
   int     retval;                         /* helpful for debugging */

#ifdef DEBUG
   char *host_name;
   char host_name_str[31];
   int host_name_len = 30;
#endif

#ifdef DEBUG
   host_name = host_name_str;
   gethostname(host_name, host_name_len);
   printf("host name: %s\n", host_name );
#endif

   /* Create socket in DECnet address family */
   /* of type sequenced packet.              */
   if ( (s = socket(AF_DECnet, SOCK_SEQPACKET, 0)) < 0 ) {
      printf("create_socket: failed on socket call\n");
      perror("ipc socket");
      return I_ERROR;
   }

   retval = gethostname(hostname,sizeof hostname);
   if (retval) {
      perror ("ipc gethostname");
      return I_ERROR;
   }

   /* The socket address indicates the DECnet address */
   /* and object number of                        */
   memset(&sockaddr, NULL, sizeof(struct sockaddr_dn));
   sockaddr.sdn_family = AF_DECnet;
   sockaddr.sdn_objnum = sock_objnum;

   /* Bind the socket to a DECnet socket address and */
   /* listen for a connection.                       */
   if( bind(s, &sockaddr, sizeof(sockaddr)) < 0 ) {
      fprintf(stderr, "ipc - bind failed sock obj = %d\n", sock_objnum);
      perror("ipc bind");
      return I_ERROR;
   }

   if( listen(s, SOMAXCONN) < 0 ) {
      perror("ipc listen");
      return I_ERROR;
   }

   /* Accept an incoming connection */
   acclen = sizeof(accsockaddr);
   ns = accept(s, &accsockaddr, &acclen);
   if(ns == -1) {
      perror("ipc accept");
      if (shutdown(s, 2) == -1) perror("ipc shutdown");
      if (close(s) != 0) perror("ipc close");
      return I_ERROR;
   }

   *ipc_socket = ns;
   close(s);

#ifdef DEBUG
   printf("server assigned sock ns %d to socket %d\n", ns, *ipc_socket );
#endif

   return I_OK;
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
   int length;

   length = strlen(inputstr);

   /* Only send non-empty lines */
   if( length <= 0 ) {
      fprintf(stderr, "ERROR: ipcsend got a null input string");
      perror("ipcsend got null string");
      return I_NO_DATA;
   }
   if( write(ipc_socket, inputstr, length) < 0 ) {
      fprintf(stderr, "ERROR: ipcsend couldn't write data over network");
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
int length;

   if( (length = read(ipc_socket, outputstr, BUFSIZE)) < 0 ) {
    fprintf(stderr, "ERROR: ipcrecv couldn't read line over network");
    perror("ipcrecv couldn't read from socket");
    return I_ERROR;
   }

   /* if(dnet_eof(ipc_socket)) { */
   if(length == 0) {
    fprintf(stderr, "ERROR: ipcrecv - ipc connection lost\n");
    perror("ipcrecv lost socket connection");
    return I_LOST_CONNECTION;
   }

   outputstr[length] = '\0';

#ifdef DEBUG
   printf("ipcrecv buffer=%s*\n", outputstr);
#endif

   ipcrecvCnt++;
   return I_OK;
}
