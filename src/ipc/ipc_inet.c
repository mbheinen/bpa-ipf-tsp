/*************************************************************
* file: ipc_inet.c
* author: Dan Clark/Jay Coleman
* purpose: interprocess communications routines - internet (TCP/IP) version
**************************************************************
*/

#ifdef VMS
#include <stdio>
#include <stdlib>
#include <string>
#include <assert>
#include <errno>
#include <types>
#include <socket>
#include <in>
#include <tcp>
#include <netdb>             /* change hostent to comply with BSD 4.3 */
#include <inet>
#include <unixio>
/* include <time> */
#include <signal> 
#include <setjmp> 
/* #include  <ucx$inetdef.h> */       /* INET symbol definitions */
#else   /* VMS */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/uio.h>
/* include <sys/wait.h> */
/* include <sys/time.h> */
#include <unistd.h>
#include <signal.h> 
#include <setjmp.h> 
#endif   /* VMS */


#include "ipf_ipc.h"

void ipc_inet_socket_setup( int sock );

/***********************************************************
* function: connect_to_socket
* author: Dan Clark/Jay Coleman
* purpose:
*  connect to an existing socket in order to perform
*  inter-process communication
************************************************************
*** NULL nodename means "local node"
*/
int connect_to_socket( char *nodename, int sock_objnum, int *ipc_socket )
{
   int sock, status;
   struct  sockaddr_in  sockaddr;      /* Address struct for socket */
   struct  hostent      hostentstruct; /* Storage for hostent data  */
   struct  hostent     *hostentptr;    /* Pointer to hostent data   */
   char *host_name;
   char host_name_str[128];
   int host_name_len = 127;

   host_name = host_name_str;
   if ( nodename == NULL ) {
      gethostname(host_name, host_name_len);
   } else if ( *nodename == '\0' ) {
      gethostname(host_name, host_name_len);
   } else {
      host_name = nodename;
   }

#ifdef DEBUG
   printf("ipc Initialize begin\n");
   printf("host name: %s\n", host_name );
#endif

   if ((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
      printf("connect_to_socket: failed on socket call (AF_INET)\n");
      perror("socket");
      return I_ERROR;
   }
   if ((hostentptr = gethostbyname (host_name)) == NULL)
   {
      perror( "gethostbyname");
      if (close(sock)) perror("close");
      return I_ERROR;
   }
   memcpy(&hostentstruct, hostentptr, sizeof(hostentstruct));
   memset(&sockaddr, '\0', sizeof(sockaddr));
   sockaddr.sin_family = hostentstruct.h_addrtype;
   sockaddr.sin_port = htons( (unsigned short)sock_objnum );
   sockaddr.sin_addr = *( (struct in_addr *) hostentstruct.h_addr );

   /* Connect to partner on specified node */
   if ( ( status = connect( sock, (struct sockaddr *)&sockaddr,
        sizeof(sockaddr) ) ) < 0 ) {
      printf("ipc failed on connect, status = %d, socket number = %d \n",
              status, (int)ntohs(sockaddr.sin_port) );
      perror("ipc connect");
      if (close(sock)) perror("close");
      return I_ERROR;
   }
   printf("ipc -- successful connect, status = %d \n", status );
   *ipc_socket = sock;

   ipc_inet_socket_setup( sock );

   return I_OK;
}


static int sock, save_socknum;


/*******************************************************
* function: create_socket1
* author: Dan Clark/Jay Coleman
* purpose:
*  create the socket in order to perform inter-process communication
*******************************************************
*/
int create_socket1( int *sock_objnum )
{
   int status;
   struct sockaddr_in sockaddr;
   struct hostent hostentstruct;  /* Storage for hostent data.  */
   struct hostent *hostentptr;    /* Pointer to hostent data.   */
   int retval;                    /* helpful for debugging */
   char *host_name;
   char host_name_str[128];
   int host_name_len = 127;
   int sock_num;
   int sock_addr_len;
   int *sal = &sock_addr_len;

   host_name = host_name_str;
   save_socknum = sock_num = *sock_objnum;

/* Create socket in TCP/IP address family, of type socket stream. */
   if ( (sock = socket(AF_INET, SOCK_STREAM, 0)) == -1 ) {
      printf("create_socket: failed on socket call\n");
      perror("ipc socket");
      return I_ERROR;
   }

   ipc_inet_socket_setup( sock );

   retval = gethostname(host_name,host_name_len);
   if (retval) {
      perror ("ipc gethostname");
      return I_ERROR;
   }

   if ((hostentptr = gethostbyname (host_name)) == NULL) {
      perror("ipc gethostbyname");
      return I_ERROR;
   }

   memcpy(&hostentstruct, hostentptr, sizeof(hostentstruct));
   memset(&sockaddr, '\0', sizeof(sockaddr));
   /* sockaddr.sin_family = AF_INET; */
   sockaddr.sin_family = hostentstruct.h_addrtype;
   sockaddr.sin_addr = * (( struct in_addr *) hostentstruct.h_addr);

   status = -1;
   if ( sock_num < 1024  ||  sock_num > 5000 ) {
      printf("ipc - illegal socket number: %d\n", sock_num);
      printf("attempting to find an available socket number\n");
   } else {
      sockaddr.sin_port = htons( (unsigned short)sock_num );
      if ( ( status = bind(sock, (struct sockaddr *)&sockaddr,
            sizeof(sockaddr) ) ) < 0 ) {
         printf("ipc - socket number  %d  was not available\n", sock_num);
         printf("  <bind> call status = %d \n", status );
         printf("attempting to find an available socket number\n");
      } else {
         save_socknum = sock_num;
         printf("  <bind> call status = %d \n", status );
         printf("IPC -- using socket number  %d\n", save_socknum );
         return (0);
      }
   }
   if ( status < 0 ) {
      sock_num = 0;
      sockaddr.sin_port = htons( (unsigned short)sock_num );
      if ( ( status = bind( sock, (struct sockaddr *)&sockaddr,
           sizeof(sockaddr) ) ) < 0 ) {
         printf("Socket <bind> failed, no socket numbers available.\n");
         printf("Sockets probably not working on this system.\n");
         printf("  <bind> call status = %d \n", status );
         return I_ERROR;
      } else {
         getsockname(sock, (struct sockaddr *)&sockaddr, sal );
         *sock_objnum = save_socknum = (int)ntohs(sockaddr.sin_port);
         printf("  <bind> call status = %d \n", status );
         printf("IPC -- using socket number  %d\n", save_socknum );
         return (0);
      }
   }

/*******************************************************************
*   sock_num = 1023;
*   while ( status < 0  &&  sock_num < 5000 ) {
*      sock_num++;
*      sockaddr.sin_port = htons( (unsigned short)sock_num );
*      status = bind(sock, (struct sockaddr *)&sockaddr, sizeof(sockaddr) );
*   }
*   if ( status < 0 ) {
*      printf("Socket <bind> failed, no socket numbers available.");
*      printf("Sockets probably not working on this system.");
*      printf("  <bind> call status = %d \n", status );
*      return I_ERROR;
*   } else {
*      *sock_objnum = save_socknum = sock_num;
*      printf("  <bind> call status = %d \n", status );
*      printf("IPC -- using socket number  %d\n", save_socknum );
*   }
*******************************************************************/

   return (0);
}


/*
* note that the first declaration of the socket signal handler
* corresponds to the definition of the online documentation but
* will not pass the ansi compiler check with more than one
* integer argument.
*/
/* void signal_pipe(int sig, int code, struct sigcontext *scp) */
void signal_pipe( int sig )
{
   /* printf("Signal %d, code %d\n", sig, code); */
   return;
}

static jmp_buf saved_context;

void signal_alarm( int sig )
{
   longjmp( saved_context, 1 );
}

/*******************************************************
* function: create_socket2
* author: Dan Clark/Jay Coleman
* purpose:
*  create the socket in order to perform inter-process communication
*******************************************************
*/
int create_socket2( int *ipc_socket, int max_sleep )
{
   int nsock, acclen, status;
   struct sockaddr_in accsockaddr;
   int sock_addr_len;
   int *sal = &sock_addr_len;

   signal( SIGPIPE, signal_pipe );
   signal( SIGALRM, signal_alarm );

   if ( max_sleep == 0 ) { /* default */
      max_sleep = 30;
   } else if ( max_sleep < 15  ||  max_sleep > 600 ) {
      printf("<sleep> value not in range 15-600, <sleep> set to 30\n\n");
      max_sleep = 30;
   }

   alarm( max_sleep );

   if ( setjmp( saved_context ) != 0 ) {
      printf("\nIPC -- create_socket2  hung on <accept socket connection>");
      printf("\n\nIPC -- NO CONNECTION\n\n");
      fflush(stdout);
      return I_ERROR;
   }

   /* if ( listen(sock, SOMAXCONN) < 0 ) { */
   if ( listen(sock, 1) < 0 ) {
      perror("ipc listen");
      return I_ERROR;
   }

   /* Accept an incoming connection */
   acclen = sizeof(accsockaddr);
   nsock = accept(sock, (struct sockaddr *)&accsockaddr, (int *)&acclen);
   if (nsock == -1) {
      perror("ipc accept");
      if (shutdown(sock, 2) == -1) perror("ipc shutdown");
      if (close(sock) != 0) perror("ipc close");
      return I_ERROR;
   }

   alarm( 0 );

   *ipc_socket = nsock;
   close(sock);

#ifdef DEBUG
   getsockname(nsock, (struct sockaddr *)&accsockaddr, sal );
   save_socknum = (int)ntohs(accsockaddr.sin_port);
   printf("IPC -- using socket number  %d\n", save_socknum );

   printf("server assigned sockfd  %d  to socketnum  %d\n",
           nsock, save_socknum );
#endif

   /*** initialize asynchronous read of the socket ***/
   /* XtAddInput(sock, XtInputReadMask, ipcclientret, NULL); */

   return I_OK;
}

 
/*******************************************************
* function: create_socket
* author: Dan Clark/Jay Coleman
* purpose:
*  create the socket in order to perform inter-process communication
*
*******************************************************
*/
int create_socket( int sock_objnum, int *ipc_socket )
{
   int socknum, status;
   int max_sleep = 90;

   socknum = sock_objnum;
   if ( ( status = create_socket1( &socknum ) ) != 0 ) return status;
   status = create_socket2( ipc_socket, max_sleep );
   return status;
}
