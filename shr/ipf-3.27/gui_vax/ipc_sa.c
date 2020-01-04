/*************************************************************
* file: ipc_sa.c
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
#include <time>
#else   /* VMS */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/time.h>
#endif   /* VMS */

#include "ipf_ipc.h"

#ifndef FD_SETSIZE
#define FD_SETSIZE 256
#endif


int ipcSocketAlive(int fid)
{

  int    nfound, nfds, readfds, writefds, exceptfds;
  int    exceptfound;
  struct timeval timeout;

  memset(&timeout, 0,  sizeof(timeout));
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;

  nfds = FD_SETSIZE;
  readfds   = 1<<fid;
  writefds  = 1<<fid;
  exceptfds = 1<<fid;
  exceptfound = select(nfds, &readfds, NULL, &exceptfds, &timeout);
  if (exceptfound > 0) {
  /* lost socket so remove asynch handler */
  }
    printf("ipcSocketAliveRead: ERROR execptions found on fid %d found %d\n", 
       fid, exceptfound);

  if (exceptfound < 0 && errno == EINTR) {
    perror("    ipcAsynchCheck EINTR");
  }
  return exceptfound;
}
 
