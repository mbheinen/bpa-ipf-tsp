static char sccsid[]="@(#)stringpart.c	20.2 2/15/95";
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
#include <string.h>
/*****************************************************************************\
*     Function:	STRING PART
*                                                                             *
*     Returns the part of the string between the given columns
*                                                                             *
*     Called by:   
*                                                                             *
*     Calls:       none                                                       *
\*****************************************************************************/
#define CARDCHARS 512
void stringPart(longString, returnString, start_col, end_col)

char	longString[CARDCHARS + 1];
char	returnString[CARDCHARS + 1];
int	start_col, end_col;

{
  int		i, j, len, abs_len;

  len = end_col - start_col+1;       /* determine length of string to extract */

	/*** check for bad parameters values ***/
  if( len < 1 || len > CARDCHARS || start_col<0) 
  {
   printf("======================================================\n");
   printf("= Bad Column number(s) in call to routine stringPart =\n");
   printf("======================================================\n");
   if( len < 1 ) len = 1;
   if( len > CARDCHARS) len = CARDCHARS;
   if( start_col<0) start_col = 0;
   printf(" reset len = %d and start_col = %d\n", len, start_col);
  }

  j = 0;	/* location to insert 1st character in return string */

  abs_len = strlen(longString) - 1;		   /* length of input string  */
  while ( longString[abs_len] == '\n' ) abs_len--; /* skip the EOL mark if any*/

  for(i = start_col; i <= end_col; ++i)	       /* loop to transfer the string */
  {
    if(i <= abs_len)
        returnString[j++] = longString[i];   /* move the string bit by bit... */
    else
	returnString[j++] = ' ';     /* blank fill if org string is too short */
  }

  returnString[j] = '\0';		     /* put the end-of-line marker in */

}
