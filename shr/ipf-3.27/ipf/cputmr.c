/*******************************************************************/
/*                                                                 */
/* This is a FORTRAN callable routine  for obtaining the processor */
/* time for the current process.                                   */
/* On the DECstation, the clock runs at 1000000 ticks/second so    */
/* the clock can "wrap-around" in 36 minutes.  On other platforms  */
/* the clocks/sec may be different and is defined in <time.h>      */
/* as CLOCKS_PER_SEC )                                             */
/*                                                                 */
/* This routine is called as:                                      */
/*                                                                 */
/*        FORTRAN                          "C"                     */
/*                               |                                 */
/*      call cputmr(cpusec)      |    cputmr(&cpusec);             */
/*                                                                 */
/* where cpusec is the floating point seconds of processor time    */
/* for the current process. dummy should also be a floating point  */
/* number.                                                         */
/*                                                                 */
/*******************************************************************/
#include <stdio.h>
#include <time.h>

#ifndef CLOCKS_PER_SEC
#if VMS
#define CLOCKS_PER_SEC 100
#endif
#endif

#if defined(WIN32) || defined(__WATCOMC__)
#pragma aux cputmr "^"

void cputmr(float *cpusec)

#elif defined UNDERSCORE
void cputmr_(float *cpusec)

#else
void cputmr(float *cpusec)

#endif
{
   clock_t tme;
   float cps, ticks;

   tme = clock();
   ticks = tme;   /*  convert to float before dividing  */
   cps = CLOCKS_PER_SEC;
   *cpusec = ticks/cps;
}
