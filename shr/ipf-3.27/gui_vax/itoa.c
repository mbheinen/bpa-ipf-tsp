static char sccsid[]="@(#)itoa.c	20.3 2/15/95";
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
/**********************************************************
* function: itoa
* purpose: convert integer to character
* see The C Programming Language by B. Kernighan & D. Ritchie
*  the second edition pg 64
*
* itoa: convert n to character in s 
***********************************************************
*/
#include <string.h>

/* reverse: reverse string s in place */
void ipf_reverse(char s[])
{
  int c, i, j;
    for(i = 0, j = strlen(s) - 1; i < j; i++, j--) {
      c = s[i];
      s[i] = s[j];
      s[j] = c;
    }
}

void ipf_itoa(int n, char s[])
{
  int i, sign;

  if ((sign = n) < 0)	/* record sign */
    n = -n;		/* make n positive */
  i = 0;
  do {			/* generate digits in reverse order */
    s[i++] = n % 10 + '0';	/* get next digit */
  } while ((n /= 10) > 0);	/* delete it */
  if (sign < 0)
    s[i++] = '-';
  s[i] = '\0';
  ipf_reverse(s);
}
    
