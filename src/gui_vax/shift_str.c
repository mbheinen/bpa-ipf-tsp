static char sccsid[]="@(#)shift_str.c	20.2 2/15/95";
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
/*****************************************************************************\
*
*	Subroutine: string_shift (*record, from, to)
*
*       Purpose: This subroutine performs a left byte shift of a character 
*                array.
*
*       Example:  string_shift (record, 2, 1);
*
*                 This shifts record[2] into record[1],
*                             record[3] into record[2], etc.
*  
*	Author Walt Powell	Oct 1992
*
\*****************************************************************************/
void shift_string (char *str, int from, int to)
{
   char *c, *d;

   for (c = &str[from], d = &str[to]; *c > 0; c++) {
      *d = *c;
      d++;
   }
   d++;
   *d = 0;
}
