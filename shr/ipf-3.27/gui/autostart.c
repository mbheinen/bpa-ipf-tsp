static char sccsid[]="@(#)autostart.c	20.16 3/13/98";
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
/**************************************************************************\
* Purpose: setup the automatic start routine to read the data files
*	   defaulted into the open dialog and display also provide a place
*	   to initialize anything for the graphics system.
\**************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include "ipfdebug.h"

extern int  fetch_file_windows();
extern int  set_default_files	( Widget, XtPointer, XmAnyCallbackStruct * );
extern void apply_files		( Widget, XtPointer, XmAnyCallbackStruct * );
extern void change_cursor_to	( Widget, int *, XtPointer );
extern void set_button_state	( char *name, Boolean state );
extern Widget widget_id		( char *);
extern void set_kv_colors	();   
extern void setup_bus_style	( Widget, XtPointer, XmAnyCallbackStruct * );
extern void fetch_widget        (char *widget_name);

void startup()
/****************************************************************************\
*
*
* Called by: gui.c
\****************************************************************************/
{
    /******** users should not get the self-test button ********/
    /*** Must use -debug 2 option to keep this button active ***/
    if(!(ipfdebug & DB_SelfTest)) {
      fetch_widget( "process_test_self_pb" );
      fetch_widget( "process_dmgr_debug_pb" );
      XtDestroyWidget( widget_id("process_test_self_pb") );
      XtDestroyWidget( widget_id("process_dmgr_debug_pb") );
    }
	set_kv_colors();   
	setup_bus_style( 0, 0, 0 );
}


void autostart()
/****************************************************************************\
* Purpose: if autostart option (app_resdata.autostart) 
*		is set in XGUI, gui.c calls this rtn.
*
* Called by: gui.c
\****************************************************************************/
{
  int zero = 0;
  fetch_file_windows();
  set_default_files(0,0,0);		/* (in filedlgrtn.c) */
  apply_files	   (0,0,0);		/* (in filedlgrtn.c) */

  set_button_state ("tool_input_rb", True );
  change_cursor_to (0,&zero,0);		/* turns on input toolbox cursor */
}
