static char sccsid[]="@(#)systemtest.c	1.4 2/15/95";
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
*       System test routines.
*
\*****************************************************************************/
#include <stdio.h>
#include <Xm/Xm.h>

extern void HashCount		 ();
extern int  Vuit_MU		 ( char *, char * );
extern void lookup_and_fill_field( char *, char *, int, int );
extern void lookup_and_get_field ( char *, char *, int );
extern void apply_files		 ( Widget widget, XtPointer tag,
				   XmAnyCallbackStruct *callback_data );
extern int  file_name_set	 ( Widget widget, XtPointer tag,
                                   XmAnyCallbackStruct *callback_data );


int  system_test		();
int  test_Open			( char *name_cor, char *name_net,
				  char *name_bse );

int system_test()
{

/*  if( Vuit_MU( "X", "non_existant_widget" )==False )
    printf("TESTED OK - Vuit_MU( X )\n");
  if( Vuit_MU( "M", "non_existant_widget_2" )==False )
    printf("TESTED OK - Vuit_MU( non_existant_widget( )\n");*/

  HashCount();

/*  test_Open( "/shrunis/ipf/dat/testdc3.cor",
	     "/shrunis/ipf/dat/testdc3.net",
	     "" );
  test_Open( "/shrunis/ipf/dat/9bus.cor",
	     "/shrunis/ipf/dat/9bus.net",
	     " " );
*/

  return(1);
}


int  test_Open			( char *name_cor, char *name_net,
				  char *name_bse )
/**************************************************************************\
*
*
\**************************************************************************/
{

  Vuit_MU( "M", "open_file_dialog" );
  lookup_and_fill_field( "file_select_dia_coord_text"  , name_cor, 0,
					strlen(name_cor) );
  lookup_and_fill_field( "file_select_dia_network_text", name_net, 0,
					strlen(name_net) );
  lookup_and_fill_field( "file_select_dia_base_text"   , name_bse, 0,
					strlen(name_bse) );
  file_name_set( 0, 0, 0 );
  apply_files  ( 0, 0, 0 );
  Vuit_MU( "U", "open_file_dialog" );

  return(0);
}
