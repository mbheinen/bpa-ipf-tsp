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
#include <stdio.h>
#include <stdlib.h>







#ifdef FULL_TIME
#include <sys/time.h>
#include <sys/resource.h>
#else
#include <time.h>
#endif

#include "ipfdebug.h"

#define MILLISEC 1000000

static int time_test_print_flg = 0;


int rtntime()
/************************************************************************\
* Purpose: returns the CPU time (microseconds) since 1st clock call.
*	   (first call always returns zero.)
*
* Called by: gotime (this module)
\************************************************************************/
{
  int rsec;

  rsec = clock( );

  return rsec;
}




#ifndef CLOCKS_PER_SEC
#define CLOCKS_PER_SEC 100
#endif


int gotime(int intime, char *where)
/************************************************************************\
* Purpose: Used to time and test some of the slow modules.
*
* Author: Dan Clark
*
* Called by: fetch_print_windows (printopts.c)
*	     fetch_file_windows  (filedlgrtn.c)
\************************************************************************/
{
  int now = 0;
  if(ipfdebug & DB_TimerMask) {
    if( !time_test_print_flg )
    {  
      printf("TIMING TESTS:\n");
      time_test_print_flg = 1;
    }
    now = rtntime();
    fprintf(ipfdbio, "	%5.3lf secs %s\n",
		(double)(now-intime)/CLOCKS_PER_SEC, where ); 
  }
  return now;
}






/************************ Experimental ************************************/
#ifdef FULL_TIME
checkdiff(struct rusage *r_usage1, struct rusage *r_usage2)
/************************************************************************/
{
  int sec_diff;
  int usec_diff;

  sec_diff = r_usage2->ru_utime.tv_sec -
             r_usage1->ru_utime.tv_sec;
  usec_diff = r_usage2->ru_utime.tv_usec -
             r_usage1->ru_utime.tv_usec;
  sec_diff += r_usage2->ru_stime.tv_sec -
             r_usage1->ru_stime.tv_sec;
  usec_diff += r_usage2->ru_stime.tv_usec -
             r_usage1->ru_stime.tv_usec;

  fprintf(ipfdbio, "Difference %d %d\n", sec_diff, usec_diff);
}

checkprint(struct rusage *pr_usage)
/************************************************************************/
{
  FILE *io;
  struct timeval *time;

  io = stderr;

  time = &pr_usage->ru_utime;
    fprintf(ipfdbio,"user time %ld %ld ", time->tv_sec, time->tv_usec);
  time = &pr_usage->ru_stime;
    fprintf(ipfdbio,"system time %ld %ld \n", time->tv_sec, time->tv_usec);

  fprintf(ipfdbio,"maxrss %ld ixrss %ld idrss %ld isrss %ld\n",
    pr_usage->ru_maxrss, pr_usage->ru_ixrss, 
    pr_usage->ru_idrss, pr_usage->ru_isrss);
  fprintf(ipfdbio,"minflt %ld majflt %ld nswap %ld inblock %ld oublock %ld\n",
    pr_usage->ru_minflt, pr_usage->ru_majflt, 
    pr_usage->ru_nswap, 
    pr_usage->ru_inblock, pr_usage->ru_oublock);
  fprintf(ipfdbio,"msgsnd %ld msgrcv %ld nsignals %ld nvcsw %ld nivcsw %ld\n",
    pr_usage->ru_msgsnd, pr_usage->ru_msgrcv, 
    pr_usage->ru_nsignals, 
    pr_usage->ru_nvcsw, pr_usage->ru_nivcsw);
}

checktime()
/************************************************************************/
{
  int rtn;
  int who;
  static struct rusage r_usage1;
  static struct rusage r_usage2;
  static struct rusage *pr_usage1;
  static struct rusage *pr_usage2;

  if(pr_usage1 == &r_usage1) {
    pr_usage1 = &r_usage2;
    pr_usage2 = &r_usage1;
  } else {
    pr_usage1 = &r_usage1;
    pr_usage2 = &r_usage2;
  }

  who =  RUSAGE_SELF;
  rtn = getrusage ( who, pr_usage2 );

  checkdiff(pr_usage1, pr_usage2);

  checkprint(pr_usage2);
}
#endif





/**************** this portion used to test the gotime routine *************/
#ifdef TESTTIME
#define MAX 5

main(int argc, char *argv)
/*************************************************************************\
\*************************************************************************/
{
  int i;
  int now;
  int ta[MAX];

  for(i = MAX-1; i >= 0; i--) {
    now = rtntime();
    ta[i] = now;
    docompute(i);
  }
  for(i = MAX-1; i > 0; i--) {
    fprintf(ipfdbio, "%d %ld - %ld = %ld sec %lf\n",
      i, ta[i-1], ta[i], ta[i-1] - ta[i], (float)(ta[i-1] - ta[i])/MILLISEC);
  }
}



docompute( int me )
/**********************************************************************\
\**********************************************************************/
{
  double junk;
  long i;
  i = 0;
  junk = (double)0.0;
         for(i=1;i<1<<(me + 20);i++){
           junk = junk*i;
         }
}
#endif

  

