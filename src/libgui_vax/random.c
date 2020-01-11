static char sccsid[]="@(#)random.c	20.2 2/15/95";
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
/****
* generate random number between 0 and 1
****
*/
#define random_b 31415821
#define random_m 100000000
#define random_m1 10000

int mult(int p, int q)
{
  int p1, p0, q1, q0;
  p1 = p / random_m1;
  p0 = p % random_m1;
  q1 = q / random_m1;
  q0 = q % random_m1;
  return ((((p0*q1+p1*q0)%random_m1) *
             random_m1+p0*q0)%random_m);
}

double random( int *random_a)
{
    *random_a = (mult(*random_a, random_b)+1)%random_m;
    return ((double)*random_a/(double)random_m);
}

