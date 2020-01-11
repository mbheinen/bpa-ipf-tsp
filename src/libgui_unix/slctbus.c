static char sccsid[]="@(#)slctbus.c	20.3 2/15/95";
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
#include <Xm/Xm.h>

extern int HashLookup(char *name, Widget *id);

void
fill_selection_box(Widget w, char *file_name, unsigned long *reason)
{
  FILE *fp;

  char	c;
  char *s, *q;

  int	i;

  XmString bus_name[10];

  Widget id;

  if((fp = fopen(file_name, "r")) == NULL)
  {
    printf("\nCan't open input file %s\n", file_name);
    return;
  }
  else
  {
    if((HashLookup("bus_selection_dialog", &id)) == FALSE)
    {
      printf("\nCan't find bus_selection_dialog id\n");
      return;
    }

    i = 0;

    while((q = fgets(s, 10, fp)) != NULL)
    {
      q[strlen(q) - 1] = '\0';
      bus_name[i] = XmStringCreate(q, XmSTRING_DEFAULT_CHARSET);
      ++i;
    }

    XtVaSetValues(id, XmNlistItems, bus_name, XmNlistItemCount, i, NULL);
  }
}
