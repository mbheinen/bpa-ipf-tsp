/*************************************************************
* file: ipc_unix.c
* author: Dan Clark/Jay Coleman/Dave Szymanski  
* purpose: interprocess communications routines - Unix domain (AF_UNIX) version
**************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <netdb.h>
#include <signal.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <linux/uio.h>
#include <sys/time.h>
#include <sys/un.h>
#include <unistd.h>
#include "ipf_ipc.h"

void ipc_unix_socket_setup( int sock );

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
   int sock, status, i;
   char *tmpsock_env;
   struct  sockaddr_un  sockaddr;      /* Address struct for socket */
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

   if ((sock = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
      printf("connect_to_socket: failed on socket call (AF_UNIX)\n");
      perror("socket");
      return I_ERROR;
   }
   memset(&sockaddr, '\0', sizeof(sockaddr));
   sockaddr.sun_family = AF_UNIX;
   tmpsock_env = getenv("IPF_SOCK_PATH");
   if ( tmpsock_env== NULL) {
     sprintf(sockaddr.sun_path,"tmp_sock_%4d\0",sock_objnum);
   } else {
      i = strlen( tmpsock_env ) - 1;
      if ( tmpsock_env[i] == '/' ) tmpsock_env[i] = '\0';
      sprintf(sockaddr.sun_path,"%s/tmp_sock_%4d\0",tmpsock_env,sock_objnum);
   }
   /* Connect to partner on specified node */
   if ( ( status = connect( sock, (struct sockaddr *)&sockaddr,
         strlen(sockaddr.sun_path)+sizeof(sockaddr.sun_family) ) ) < 0 ) {
      printf("ipc failed on connect, status = %d, tmp_sock file = %s \n",
              status, sockaddr.sun_path );
      perror("ipc connect");
      if (close(sock)) perror("close");
      return I_ERROR;
   }
   unlink(sockaddr.sun_path);
   *ipc_socket = sock;

   ipc_unix_socket_setup( sock );

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
   struct sockaddr_un sockaddr;
   int sock_num, i;
   char tmpsock_path[128];
   char *tmpsock_env;

   save_socknum = sock_num = *sock_objnum;

/* Create socket in Unix domain address family, of type socket stream */
   if ( (sock = socket(AF_UNIX, SOCK_STREAM, 0)) == -1 ) {
      printf("create_socket: failed on socket call\n");
      perror("ipc socket");
      return I_ERROR;
   }

   ipc_unix_socket_setup( sock );

   memset(&sockaddr, NULL, sizeof(sockaddr));
   sockaddr.sun_family = AF_UNIX;
   tmpsock_env = getenv("IPF_SOCK_PATH");
   if ( tmpsock_env== NULL) {
     sprintf(tmpsock_path,"tmp_sock_\0");
   } else {
      i = strlen( tmpsock_env ) - 1;
      if ( tmpsock_env[i] == '/' ) tmpsock_env[i] = '\0';
      sprintf(tmpsock_path,"%s/tmp_sock_\0",tmpsock_env);
   }

   status = -1;
   if ( sock_num < 1024  ||  sock_num > 5000 ) {
      printf("ipc - illegal socket number: %d\n", sock_num);
      printf("attempting to find an available socket number\n");
   } else {
      sprintf(sockaddr.sun_path,"%s%4d\0",tmpsock_path,sock_num);
      if ( ( status = bind(sock, (struct sockaddr *)&sockaddr,
       strlen(sockaddr.sun_path) + sizeof(sockaddr.sun_family) ) ) < 0 ) {
         printf("ipc - tmp_sock file  %s  was in use\n", sockaddr.sun_path );
         printf("  <bind> call status = %d \n", status );
         printf("attempting to find an available tmp_sock file\n");
      } else {
         save_socknum = sock_num;
         printf("  <bind> call status = %d \n", status );
         printf("IPC -- using tmp_sock file  %s\n", sockaddr.sun_path );
         printf("IPC -- using socket number  %d\n", save_socknum );
         return (0);
      }
   }
   sock_num = 1023;
   while ( status < 0  &&  sock_num < 5000 ) {
      sock_num++;
      sprintf(sockaddr.sun_path,"%s%4d\0",tmpsock_path,sock_num);
      status = bind( sock, (struct sockaddr *)&sockaddr,
               strlen(sockaddr.sun_path) + sizeof(sockaddr.sun_family) );
   }
   if ( status < 0 ) {
      printf("Socket <bind> failed, no socket numbers available.");
      printf("Sockets probably not working on this system.");
      printf("  <bind> call status = %d \n", status );
      return I_ERROR;
   } else {
      *sock_objnum = save_socknum = sock_num;
      printf("  <bind> call status = %d \n", status );
      printf("IPC -- using tmp_sock file  %s\n", sockaddr.sun_path );
      printf("IPC -- using socket number  %d\n", save_socknum );
   }
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
   struct sockaddr_un accsockaddr;

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
   nsock = accept(sock, (struct sockaddr *)&accsockaddr, &acclen);
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

