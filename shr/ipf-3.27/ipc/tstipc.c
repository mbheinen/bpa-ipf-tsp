/***
* file: tstipc.c
* purpose: simple test of ipc mechanism
*
 cc -c ipc_inet.c
 cc -c -DSERVER tstipc.c -o srv.o
 cc -o srv srv.o ipc_inet.o 
 srv 1024 &
 cc -c -DCLIENT tstipc.c -o clt.o
 cc -o clt clt.o ipc_inet.o 
 clt ds5003 1024
*
***/
#include <stdlib.h>
#include <stdio.h>
#define BUFSIZE 1000
int bufsize=30;
#define MAXLOOP BUFSIZE-31
#define NAMESIZE 256
main(int argc, char *argv[])
{
  int i,c;
  static int ipcsocket;
  static int socknum = 1024;
  char hostname[NAMESIZE];

  char recvbuf[BUFSIZE];
  char sendbuf[BUFSIZE];

#ifdef SERVER
    printf("SERVER start\n");
    if(argc < 1) {
      printf("SERVER: Syntax %s <socket_number>\n", argv[0]);
      printf(" where  1024 <= socketnumber >= 4000\n");
      exit(1);
    } else {
      socknum = atoi(argv[1]);
      printf("socket number set to %d\n", socknum);
    };
    create_socket(socknum, &ipcsocket);
    for (c=0; c<MAXLOOP; ++c) {
      bufsize++;
      for (i=0; i<bufsize; ++i)
        sendbuf[i]=33+(c%95);
      sendbuf[i] = '\0';
      if( ipcsend(sendbuf, ipcsocket) ) exit(1);
    }
    printf("SERVER end\n");
#else
    printf("CLIENT start\n");
    if(argc < 2) {
      printf("CLIENT: Syntax %s <nodename> <socket_number> \n", argv[0]);
      printf(" where nodename is tcp/ip name of the machine of the server\n");
      printf(" and 1024 <= socketnumber >= 4000\n");
      exit(1);
    } else {
      strcpy(hostname, argv[1]);
      socknum = atoi(argv[2]);
      printf("socket number set to %d\n", socknum);
      printf("server machine name set to %s\n", hostname);
    };
    connect_to_socket(hostname, socknum, &ipcsocket);
    for (c=0; c<MAXLOOP; ++c) {
      if( ipcrecv(recvbuf, ipcsocket) ) exit(1); 
      bufsize++;
      for (i=0; i<bufsize; ++i) 
        if (recvbuf[i]!=(33+(c%95))) {
          printf( "bad character read @ loop=%d, byte=%d, should=%c, is=%c\n",
            c,i,33+(c%95),recvbuf[i]);
          exit(1);
       }
    }
    printf("CLIENT end\n");
#endif
}
/***
*
 cc -c ipc_inet.c
 cc -o srv -DSERVER tstipc.c ipc_inet.o
 srv 1040 &
 cc -o clt -DCLIENT tstipc.c ipc_inet.o
 clt `hostname` 1040
*
***/
