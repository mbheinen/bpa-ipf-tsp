static char sccsid[]="@(#)stdlib_ext.c	20.10 2/26/96";
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
#include <string.h>

#define ATOF_LIMIT 19

static char workarea[ATOF_LIMIT+1];

void chg_str_blanks_to_zeros	( char *str, int size );
void workarea_cpy		( char *str, int size );
void ensure_eol			( int *limit );


/*****************************************************************************\
* Purpose: Package of 4 routines whose task is to extract a number
*	   (floating pt or integer) from a larger string record
*	   which has NO null terminator.
*
*          With care that a missing null terminator does not mess up data.
*          Very important not to alter the original data!
*
*       Author: B. Rogers       1991            Modified: Jun 16, 1994
*	modified dlc for portability do no use a function in a return statment.
*
\*****************************************************************************/
int atoi_cnt_zero ( char *str, int size )
{
  int tmp;
  ensure_eol	( &size );
  chg_str_blanks_to_zeros( str, size );

  tmp = atoi (workarea);
  return ( tmp );
}
  



int atoi_cnt( char *str, int size )
/*****************************************************************************\
* Purpose: Convert integer string
\*****************************************************************************/
{
  int tmp;

  ensure_eol	( &size );
  workarea_cpy	( str, size );

  tmp = atoi(workarea);
  return ( tmp );
} 



double atof_cnt( char *str, int size )
/*****************************************************************************\
*	purpose: Converts floating point string
\*****************************************************************************/
{
  double tmp;

  ensure_eol	( &size );
  workarea_cpy	( str, size );
  
  tmp = atof(workarea);
  return ( tmp );
} 




double atof_cnt_zero( char *str, int size )
/*****************************************************************************\
* Purpose: Converts floating pt string with "blanks are zero" subsitution
*
* Warning: there is NO check for illegal characters!  
*	   Use convert routine if needed.
\*****************************************************************************/
{
  double tmp;

  ensure_eol	( &size );
  chg_str_blanks_to_zeros( str, size );

  tmp = atof(workarea);
  return ( tmp );
} 



void ensure_eol( int *size )
/****************************************************************************\
* Purpose: put a NULL terminator in string
\****************************************************************************/
{
  if ( *size<0 || *size>ATOF_LIMIT )
  {
    printf("*** WARNING *** from atof_cnt_zero (stdlib_ext.c)\n" );
    printf("      size param (%d) should not be greater than 20\n", size );
    *size = ATOF_LIMIT;
  }
}




void workarea_cpy( char *str, int size )
/************************************************************************\
* Purpose: Direct copy until eol OR limit is reached
\************************************************************************/
{
  int i;

  memset(workarea,' ', sizeof(workarea));
  workarea[size] = '\0';

  i = 0;
  while( str[i] != '\0' && i<size ) {
      workarea[i] = str[i];             /* Transfer original	*/
      i++;
  }
}


void chg_str_blanks_to_zeros( char *str, int size )
/**************************************************************************\
*  Purpose: replaces all blanks with zero until NULL terminator
* 	    OR size limit is reached
\**************************************************************************/
{
  int i;

  workarea[size] = '\0';

  i = 0;

  while( str[i] != '\0' && i<size )
  {
    if( str[i] == ' ')
    {				   /* Test for blank       */
      workarea[i] = '0';           /* Replace blank with zero */
    }
    else if( str[i] == '-' || str[i] == '+' )
    {
      workarea[i] = '0';                /* To handle		  */
      workarea[0] = str[i];             /* plus or minus symbol	  */
    }
    else
    {
      workarea[i] = str[i];             /* Transfer original	*/
    }
    i++;
  }
  workarea[i] = '\0';		/* added by BER 12/2/94 - corrects problem */
				/* of residual data */
}
