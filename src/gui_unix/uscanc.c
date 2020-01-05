static char sccs_id[] = "@(#)uscanc.c	20.4 2/15/95";
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

#ifdef DEBUG

char *progname;

main(argc, argv)
int argc;
char *argv[];
{
  char in_buf[81],
       *out_buf[20],
       *limit1 = ",=",
       *limit2 = " \t\n";
  
  int  i, nwrd;

  progname = argv[0];
  while (strstr(in_buf, "quit") == 0) {
     printf (" Enter input string > ");
     gets(in_buf);
     printf ("\n");
     nwrd = 0;
     uscanc (in_buf, out_buf, &nwrd, limit1, limit2);
     for (i = 0; i < nwrd; i++) {
        printf (" word[] = %s \n", out_buf[i]);
     }
  }
}
#endif

/*****************************************************************************\
*	Function: uscanc.c 
*
*	This function is a C-equivalent of the FORTRAN uscan.f.
*       uscanc parses the input string into an array of tokens
*       which are delimited by visible delimiters limit1 and
*       invisible delimiters limit2. nwrd is the number of
*       tokens produced.
*
* NOTE: use of this function allocates memory which must be 
*       freed by the application which calls this function.
*       the programmer using this function must carefully analyze
*       each line of this code to determine exactly how to
*       give the memory back to the system.
*		
*	Author: Walt Powell		Modified: August 5, 1992
*
\*****************************************************************************/
int uscanc (char *in, char *out[], int *nwrd, char *limit1, char *limit2)
{
   char *c, *d;
   int num = 0;

   num = 0;

   c = in;
   out[num] = (char *) malloc(strlen(c)+1);         /* allocate new word with
                                                       entire string c */
   strcpy(out[num], c);
   d = out[num];
   for (c = in; *c != 0; c++) {
      if ( strchr(limit1, *c) != NULL) {            /* parse delimiter */
         *d = '\0';                                 /* delimit last word */
         if (strlen(out[num]) > 0) num++;
         out[num] = (char *) malloc(2);             /* allocate 2 bytes for 
                                                       visible delimiter */
         strcpy(out[num], strchr(limit1, *c));      /* copy delimiter */
         num++;
         out[num] = (char *) malloc(strlen(c)+1);   /* allocate new word with 
                                                       remaining string c */
         strcpy(out[num], c);                   
         d = out[num];
      } else if ( strchr(limit2, *c) != NULL) {     /* skip delimiter */
         *d = '\0';                                 /* delimit last word */
         if (strlen(out[num]) > 0) num++;
         out[num] = (char *) malloc(strlen(++c)+1); /* allocate new word with
                                                       remaining string c */
         strcpy(out[num], c--);                    
         d = out[num];
      } else {
         d++;                                       /* character accepted in
                                                       out[num]; 
                                                       advance pointer */
      }
   }
   if (strlen(out[num]) > 0) num++;
   *nwrd = num;

   return(0);
}   
