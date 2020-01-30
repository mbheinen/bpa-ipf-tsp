static char sccsid[]="@(#)is_float.c	20.2 2/15/95";
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
#include <stdlib.h>
#include <ctype.h>

#define TRUE 1
#define FALSE 0

#ifdef DEBUG

char *progname;
int is_float (char *, int);

main(argc, argv)
int argc;
char *argv[];
{
  char temp[10];
  int len;

  printf (" Enter input string > ");
  while (gets (temp) != NULL) {
     len = strlen(temp);
     if (is_float (&temp[0], len) == 0) {
        fprintf (stderr, "\n Illegal chanracter data in field \n");
     }
     printf ("\n Enter input string > ");
  }
}

#endif

/***********************************************************************
* File: is_float.c
* Purpose: Establish the validity of a floating point character field.
*          This is more restrictive than sscanf or atof since all 
*          characters in the set must be valid.
*
* Author: Walt Powell	November 6, 1992
*
************************************************************************
*/
int is_float (char * record, int width)
{
   char temp[11], *lp, *i;
   double rval;
   int status, matched;

   strncpy (temp, record, width);
   temp[width] = 0;

   /* Align "lp" to the first non-blank character */

   for (lp = &temp[0]; lp < &temp[10] && *lp == ' '; lp++);

   /* In the following steps, progressively align "i" to the last+1 
    * valid floating point character.
    */

   i = lp;
   if (*i == '-' || *i == '+') i++;
   if (isdigit(*i)) {
      while (isdigit(*i)) i++;
      if (*i == '.') {
         matched = TRUE;
         i++;
         while (isdigit(*i)) i++;
      } else {
         matched = FALSE;
      }
   } else if (*i == '.') {
      i++;
      if (isdigit(*i)) {
         matched = TRUE;
         while (isdigit(*i)) i++;
      } else {
         matched = FALSE;
      }
   } else {
      matched = FALSE;
   }
   if (matched) {
      if (*i == 'e' || *i == 'E') {
         i++;
         if (*i == '-' || *i == '+') i++;
         if (isdigit(*i)) {
            while (isdigit(*i)) i++;
         }
      }
   }
   while (*i == ' ') {
      *i = '0';
      i++;
   }

   /* The grand finale is at hand! If the last character processed is NULL,
    * the word is a viable floating point character set. Otherwise the word 
    * contains illegal character information.
    */

   if (*i == 0) {
      status = TRUE;
      rval = atof(lp);

#ifdef DEBUG
      fprintf (stderr, " String %s value %f \n", lp, rval);
#endif

   } else {
      status = FALSE;
      fprintf (stderr, " Illegal data in field %s (%s) \n", temp, i);
      rval = 0.0;
   }
   return (status);
}
