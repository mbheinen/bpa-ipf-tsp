static char sccsid[]="@(#)reformat.c	20.11 7/15/97";
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
/*
 * Revised 26-Jan-1993 by WLP: Added return (0) for normal exit in 
 *                             invalid_char_ck
 */
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <math.h>

#include "em.h"
em_init                 /* special initialization for error message box */
                        /* must be in every file using this.            */
#define BUFSIZE 133
static char out_buffer[BUFSIZE]; 

extern void shift_string(char *, int, int);
int  reformat		 ( char *, char *, char *);
int  invalid_char_ck	 ( char *, char *, char * );

#ifdef DEBUG

void em_show (ERROR *err)
{ 
  printf(" FILE=%s, LINE=%d, TYPE=%d, LINK=%s, IDENT=%s\n MSG=%s\n",
      err->file, err->line, err->type, err->link, err->ident, err->msg);
}

main (argc, argv)
   int argc;
   char argv[];
{
   char record[21], format[10];
   int  status;

   while (1) {
      printf ("\n Enter 20-column input record > ");
      gets(record);
      printf ("\n Enter format > ");
      gets(format);

      status = reformat (record, record, format);
      if (status)
         printf ("\n Error decoding record - status > %d", status);
      else 
         printf ("\n Modified record > %s", record);
   }
}

#endif


/*****************************************************************************\
*
*	Integer function reformat (char *input, char *output, char *format)
*
*       Purpose: This subroutine emulates the FORTRAN character function CODE.
*                It reformats a floating decimal string using a user-specified 
*                format. The motive is to increase its precision by judicious 
*                postioning or omission of the decimal point.
*
*       Return values:  0 = OK
*                       1 = Bad format, data intact
*                       2 = Format OK, bad data
*                       3 = Format OK, data inaccurate (overflow)
*                       4 = Format OK, no data
*  
*	Author Walt Powell	Oct 1992
*	Extensive corrections: by William E. Rogers - 1997
*
*       Example use: Presume that we wish to reformat a string with the 
*                    format "%4.0f". An example follows: 
*
*                    record = "  15"
*
*                    status = reformat (record, record, "%4.0f")
*
*                    The ensuing output record is
*
*                    record = "15.0"
*
*	WARNING: This routine puts a NULL at end of input data.  i.e.
*		 it will alter your original data!  Use with care!!
*
\*****************************************************************************/

int reformat (char *in_string, char *out_string, char *fmt)
{
   char temp[BUFSIZE], *c, newfmt[9], *ptrfmt;
   float a;
   int  len, id, m, n, free;
   int maxlen;

/* set up for possible error message */

   err.msg   = out_buffer;
   err.type  = INFO;

  ptrfmt= fmt;
  if( *ptrfmt == '%' ) ptrfmt++;

  switch( *ptrfmt )
  {

    case 'a':
     sscanf (++ptrfmt, "%d", &m);
     if (m <= 0 ) {
         sprintf (out_buffer, " Illegal format width (%s) \n", fmt);
         err.type = INFO;
         em_show(&err);
         return (1);
     }
     strncpy (out_string, in_string, m );  
     out_string[m] = 0;
     return (0);		/* all done with 'a' type */
    break;

    case 'i':
     n = 0;
     sscanf (++ptrfmt, "%d.%d", &m, &n);
     if (m <= 0 || m > 10 || m < n || n < 0) {
       sprintf (out_buffer, " Illegal format size or width (%s) \n", fmt);
       em_show(&err);
       strcpy (out_string, in_string);  
       return (1);
     }
     strncpy (out_string, in_string, m );  
     out_string[m] = 0;
     if ( invalid_char_ck( " +-0123456789", out_string, in_string ) ) return(2);
     return (0);		/* all done with 'i' type */

    break;
  
    case 'f':
     sscanf (++ptrfmt, "%d.%d", &m, &n);
     if (m <= 0 || m > 10 || m < n || n < 0) {
        sprintf (out_buffer, " Illegal format width or size (%s) \n", fmt);
        em_show(&err);
        strcpy (out_string, in_string);  
        return (1);
     }


/********************************************************************/
/* Insert explicit decimal point in temporary string if none exists */

     maxlen = (int)strlen(in_string);
     if(maxlen > BUFSIZE) {
       printf("reformat: info - shorted length to %d of string %s\n", 
       BUFSIZE, in_string);
       maxlen = BUFSIZE - 1;
     } 
     strncpy (temp, in_string, maxlen);  
     temp[maxlen] = '\0';	/* eliminate residual in case of NULL str */ 
     if (strchr (temp, '.') == NULL) {
      strncpy(temp, in_string, m-n);
      strncpy(&temp[m-n], ".", 1);
      if (n > 0) strncpy (&temp[m-n+1], &in_string[m-n], n);
      temp[m+1] = 0;
     }   
   
     a = 0.0;
     sscanf (temp, "%f", &a);

     /* Skip under/overflow tests for 0.0    */

     if (a == 0.0) {
      strncpy (out_string, in_string, m );  
      out_string[m] = 0;
      if( invalid_char_ck( " +-.0123456789", out_string, in_string )) return(2);
      return (0);
     }
     /* Encode word with 10 digits on either side of decimal point */

     sprintf (temp, "%21.10f", a);

     /* Left justify encoded word                                  */

     if (temp[0] == ' ') {
      for (c = &temp[0]; *c == ' '; c++);
      shift_string (temp, (c - &temp[0]) / sizeof temp[0], 0);
     }
 
     /* Eliminate superfluous "0" in "-0."                         */

     if (temp[0] == '-' && temp[1] == '0')  shift_string (temp, 2, 1);
 
     /* Eliminate superfluous "0" in "0."                          */

     else if (temp[0] == '0')  shift_string (temp, 1, 0);

     /* Locate decimal point                                       */

     for (c = &temp[0]; *c > 0 && *c != '.'; c++); 
     id = (c - &temp[0]) / sizeof temp[0];

     /* Compute available characters for mantissa                  */

     free = m - n - id;
 /* 
  *   Three conditions exist for encoding "temp". The first condition is
  *   the encoded word fits perfectly into m.n.  Encode the word into m+2.n 
  *   and squeeze out decimal point.
  */
     if (free == 0 || free == 1) {
      if (n == 0 && m >= 10) {
         sprintf (newfmt, "%%%2d.0f", m);
         sprintf (temp, newfmt, a);
      } else if (n == 0 ) {
         sprintf (newfmt, "%%%d.0f", m);
         sprintf (temp, newfmt, a);
      } else if (m >= 10 ) { 
         sprintf (newfmt, "%%%2d.%df", m+2, n);
         sprintf (temp, newfmt, a);
      } else { 
         sprintf (newfmt, "%%%d.%df", m+2, n);
         sprintf (temp, newfmt, a);
      }

    /*   Left justify encoded word one character if leading character
     *   is " "
     */

      if (n > 0) {
         if (temp[0] == ' ') {
            shift_string (temp, 1, 0);
    
            /* Locate decimal point and remove it                         */

            for (c = &temp[0]; *c > 0 && *c != '.'; c++); 
            id = (c - &temp[0]) / sizeof temp[0];
            shift_string (temp, id+1, id);
            if (id == 1 && temp[0] == '0') temp[0] = ' ';

         } else if (temp[0] == '-') {
            if (temp[1] == '0') shift_string (temp, 2, 1);
 
            /* Locate decimal point and remove it                         */

            for (c = &temp[0]; *c > 0 && *c != '.'; c++); 
            id = (c - &temp[0]) / sizeof temp[0];
            shift_string (temp, id+1, id);
         }
      }
 /* 
  *   The second condition is leading blanks appear in encoded word.  
  *   The Accuracy of encoding can be improved by shifting the decimal point
  *   left.
  */
     }
     else if (free > 1)
     {
      if (fabs ((double) a) >= 1.0) { 
         sprintf (newfmt, "%%%d.%df", m+1, m-id-1);
         sprintf (temp, newfmt, a);

    /*   Left justify encoded word one character if leading character
     *   is " "
     */
         if (temp[0] == ' ') shift_string (temp, 1, 0);

     }
     else
     {
         sprintf (newfmt, "%%%d.%df", m+2, m-id-1);
         sprintf (temp, newfmt, a);

    /*   Left justify encoded word one character if leading character
     *   is " "
     */
         if (temp[0] == ' ') shift_string (temp, 1, 0);

         /* Eliminate superfluous "0" in "-0."                         */

         if (temp[0] == '-' && temp[1] == '0') shift_string (temp, 2, 1); 
 
         /* Eliminate superfluous "0" in "0."                          */

         else if (temp[0] == '0') shift_string (temp, 1, 0);
     }

 /* 
  *   The third condition is overflow occurs in the present format.  
  *   Check if shifting the decimal point to the right suffices.
  */
     }
     else if (free < 0)
     {
      if (m < id || (m-id==0 && n!=0) || m-id-1<0 )
      {
        sprintf (out_buffer, " Encoding overflow - value %12.5e format \
%%%d.%d \n", a, m, n);
        em_show(&err);
        strncpy (out_string, in_string, m );  
        out_string[m] = 0;
        return (3);
      }

      if( m-id-2 != 0 )
	sprintf (newfmt, "%%%d.%df", m+1, m-id-2);
      else
	sprintf (newfmt, "%%%df", m+1 );

      sprintf (temp, newfmt, a);


 /*   Left justify encoded word one character if leading character
  *   is " "
  */
      if (temp[0] == ' ' && strchr(temp, '.')) shift_string (temp, 1, 0);
     }
     len = (int)strlen(temp) < m ? (int)strlen(temp) : m;
     strncpy (out_string, temp, m );
     out_string[m] = 0;
     if( invalid_char_ck( " +-.0123456789", out_string, in_string )) return(2);
     return (0);
   break;

    default:
     sprintf (out_buffer, " Unrecognized format (%s) \n", fmt);
     em_show(&err);
     strcpy (out_string, in_string);  
     return (1);
  }
}

int invalid_char_ck( char *list, char *str, char *in_string )
/**************************************************************************\
*       Purpose: compares string of characters (str) against a list of
*                valid characters (list)
*
*        Returns (0 = all valid), (2 = error + warning msg)
*
\**************************************************************************/
{
   char *c;

   for (c = str; *c > 0; c++)
   { 
     if (strchr ( list, *c) == 0 ) {
       sprintf (out_buffer, " Illegal character data in numeric field (%s) \
\n", in_string);
       em_show(&err);
       return (2);
     }
   }
   return (0);
}
