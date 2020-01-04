static char sccsid[]="@(#)openfiles.c	20.3 2/15/95";
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
* file: openfile.c
* Author: Dan Clark    Jan 27
* purpose: service file open dialog box
*
\*****************************************************************************/
#include <stdio.h>
#include <Xm/Xm.h>

extern int HashLookup(char *name, Widget *id);

int toggle_filter(widget, tag, callback_data)
Widget          widget;
caddr_t         *tag;
caddr_t         *callback_data;
{
  static int toggle = TRUE;
  Widget id;

  char ret_string[80], bus_type;

  if((HashLookup("file_selection_box_open", &id)) == FALSE)
  {
    printf("\nCan't find open dialog box file_selection_box_open id\n");
    return;
  }

  if(toggle == TRUE) {
    toggle = FALSE;
    XtVaSetValues(id, 
      XmNdirMask,XmStringCreate("/all/ipf/dat/*.cor",XmSTRING_DEFAULT_CHARSET), 
      NULL);
  } else {
    toggle = TRUE;
    XtVaSetValues(id, 
      XmNdirMask,XmStringCreate("/all/ipf/dat/*.dat",XmSTRING_DEFAULT_CHARSET), 
      NULL);
   }

}

