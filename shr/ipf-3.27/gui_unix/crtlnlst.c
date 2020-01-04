static char sccsid[]="@(#)crtlnlst.c	20.2 2/15/95";
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

/*** create a list of pointer to the beginings of each line ***/

#include <string.h>
typedef char * RecList;
static char inbuf_array[256];

#define SEPCHARS "\n"


int createRecPtrList( RecList plist[], int maxline, char *inbuff )
/**********************************************************************\
*
*
*	Author: D. Clark
*
*	Called by: forEachipfLine, pfgetdata_cb, pfGetBusBranch,
		   pfAlphaList
*
\**********************************************************************/
{
  int rec_cnt;
  char *psubstr;

  psubstr = inbuff;
  rec_cnt = 0;
  plist[rec_cnt++] = psubstr;			/* start of first rec */

  psubstr = strtok(inbuff,SEPCHARS);

  while(rec_cnt < maxline && psubstr != NULL) {
      psubstr = strtok(NULL,SEPCHARS);
      plist[rec_cnt++] = psubstr;		/* add ptr to list */
  }

  if( rec_cnt < maxline ) rec_cnt--;

  return rec_cnt;
}


/**************************************************************************/
#define MAXLIST 100
#define ENDOFMSG "[EOM]"

#ifdef TESTCLL
main(int argc, char *argv[])
{
  RecList list[MAXLIST];
  char *pfdataret = inbuf_array;
  int numlines;
  int cnt;

  /* hard coded test data */
  sprintf(pfdataret, "/get_data,type=input\n%s\n%s\n",  "data line", ENDOFMSG);
  printf("in str=(%s)\n", pfdataret);

  numlines = createRecPtrList(list,MAXLIST,pfdataret);

  while(cnt < numlines) {
    printf("line %d =(%s)\n", cnt, list[cnt]);
    cnt++;
  }

}
#endif
