static char sccsid[]="@(#)goipf.c	20.2 2/15/95";
/*
*           IPF V2.x: Interactive PowerFlow Version 2.x
*              Bonneville Power Administration (BPA)
*         Authors:  D. L. Clark, J. G. Coleman, W. L. Powell, 
*                B. E. Rogers, K. E. Rowell, J. L. Rutis, D. M. Stefonik, 
*                D. M. Syzmanski 
*
*                              NOTICE
*
* Interactive Powerflow (IPF) was developed by BPA and its contractors
* with about 20% of the cost supported by the Electric Power Research
* Institute (EPRI).  By mutual agreement, as described in EPRI 
* Agreement RP2746-03 entitled Graphical User Interface for 
* Powerflow, March, 1992, all results of this project--including 
* the computer program and its documentation--are to be in the 
* public domain.  In a separate Memorandum of Understanding with the 
* Western Systems Coordinating Council (WSCC), BPA agreed in March, 1992, 
* to keep WSCC informed of progress, to make its best effort to develop 
* the program according to the Guidelines adopted by the WSCC Computer 
* Program Management Subcommittee, and to make the final results 
* available for possible further development by WSCC. 
*
* This notice must appear in all copies of this software and documentation.
*/
/***************************************
* file: goipf.c
* procedure: main
* author: Dan Clark
* purpose:
*  simple program to start off ipf and gui
* 
***************************************
*/
#include <types.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#ifdef VMS
#include <processes.h>          /* for fork and exec */
#include <signal.h>             /* for sleep functions */
typedef int pid_t;
#define fork vfork
#else
#include <wait.h>
#include <unistd.h>             /* for fork exec and sleep */
#endif

#define GUI "gui"
#define IPF "ipfsrv"
#define SOCKID "2001"
#define MAXFILENAME 133
#define MAXARG 10
main(int argc, char *argv[], char *envp[])
{
  char xclient[MAXFILENAME];
  char dataserver[MAXFILENAME];
  char sockid[MAXFILENAME];
  char *client_argv[MAXARG];
  char *server_argv[MAXARG];
  char **tmp_envp;
  int cnt_trys = 0;

  pid_t pid_server;
  pid_t pid_client;
  pid_t pid_wait;
#ifndef VMS
  union wait status_wait;
#endif
  int options_wait = 0;
  struct rusage *rusage_wait;

    if(argc < 4){
      printf("usage %s <gui> <ipf> <sockid>\n", argv[0]);
      printf("NOTE - assuming defaults\n");
      printf("  both programs are assumed to be in current directory\n");
      strncpy(xclient, GUI, MAXFILENAME);
      strncpy(dataserver, IPF, MAXFILENAME);
      strncpy(sockid, SOCKID, MAXFILENAME);
    } else {
      strncpy(xclient, argv[1], MAXFILENAME);
      strncpy(dataserver, argv[2], MAXFILENAME);
      strncpy(sockid, argv[3], MAXFILENAME);
    }
    printf("client: %s server: %s\n", xclient, dataserver);

    if(pid_server != 0) {
       printf("server returned skip start of client\n");
    } else { 
    /*** fork the client (gui interface) ***/
      client_argv[0] = xclient;
      tmp_envp = envp;
      execve(xclient, client_argv, tmp_envp);
      perror(argv[0]);
      printf(" gui process ended kill server & exit\n");
      kill(pid_server, 0);
      exit(-1);
    }

#define MAXTRYS 2
    /*** fork the server (ipf data server) ***/
    if((pid_server = fork()) == 0) {
#ifdef RESTART
      while(cnt_trys < MAXTRYS) {
#endif
        printf("server process starts here pid = %d\n", pid_server);
        server_argv[0] = dataserver;
        tmp_envp = envp;
        execve(dataserver, server_argv, tmp_envp);
        printf("server process Failed pid = %d sockid %d\n", 
               pid_server, sockid);
        perror(argv[0]);
        cnt_trys++;
#ifdef RESTART
      }
#endif
    } else if (pid_server < 0) {
         printf("FORK FAILED continue without server\n");
         perror("fork failed");
    }
#ifdef PRODUCTION
    /*** fork the client (gui interface) ***/
    if((pid_client = fork()) == 0) {
      printf("gui process starts here pid = %d\n", pid_client);
      client_argv[0] = xclient;
      tmp_envp = envp;
      execve(xclient, client_argv, tmp_envp);
      perror(argv[0]);
      printf(" gui process ended kill server & exit\n");
      kill(pid_server, 0);
      exit(-1);
    } else if (pid_client < 0) {
         printf("FORK FAILED continue without client\n");
         perror("fork failed");
         /* printf("errno = %d\n", errno); */
    }
#endif

#ifdef VMS
    pid_wait = wait((int *)0);
#else
    pid_wait = waitpid((union wait *)0, status_wait, options_wait); 
    perror("ipfinit");
    printf("wait returned with id %d\n", pid_wait);
    if(WIFSTOPPED(status_wait) != 0) 
       printf("wait status WIFSTOPPED %d\n", WIFSTOPPED(status_wait));
    if(WIFEXITED(status_wait) != 0) 
       printf("wait status WIFEXITED %d\n", WIFEXITED(status_wait));
    if(WEXITSTATUS(status_wait) != 0) 
       printf("wait status WEXITSTATUS %d\n", WEXITSTATUS(status_wait));
    if(WIFSIGNALED(status_wait) != 0) 
       printf("wait status WIFSIGNALED %d\n", WIFSIGNALED(status_wait));
    if(WTERMSIG(status_wait) != 0) 
       printf("wait status WTERMSIG %d\n", WTERMSIG(status_wait));
    if(WSTOPSIG(status_wait) != 0) 
       printf("wait status STOPSIG %d\n", WSTOPSIG(status_wait));
#endif
}
