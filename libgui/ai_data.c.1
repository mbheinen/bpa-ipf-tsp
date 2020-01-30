static char sccsid[]="@(#)ai_data.c	20.4 6/9/95";
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
/********************************************************************\
* file: area_data.c
* author: Bill Rogers
* purpose: manipulate area control data and intertie exchange
*
\*********************************************************************/

#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <Xm/Xm.h>
#include <Xm/SelectioB.h>
#include <Xm/List.h>
#include "ai_data.h"		/* defines 3 typedefs, AREA, INTERTIE & AIREC */
#include "ipf_ipc.h"		/* from ipc library - defines BUFSIZE,
						ENFOFMSG and SEPCHARS */

#define min(A,B) ((A)<(B) ? (A):(B))
#define MAXAILEN (int)sizeof(AIREC)
#define MAXAIREC  100

XmString ai_rec[MAXAIREC]; 
int num_rec;

extern void ipc_synch_rw	( char *, char * );
extern Widget widget_id         ( char * );

void load_ai_recs( char *name_str );


int loadArea2()
/*********************************************************************\
* Purpose: Same as old loadArea - except put data directly into MOTIF listbox.
*
* Called by: fill_area_selection_box
*
* Author: WE Rogers 
\*********************************************************************/
{
  Widget  id, list_id;

  if ((id = widget_id( "area_selection_dialog" )) == FALSE ) return(0);
    list_id = (Widget)XmSelectionBoxGetChild( id, XmDIALOG_LIST);

  XmListDeleteAllItems( list_id );    /* assure its empty */
  num_rec = 0;

  XtSetSensitive ( (Widget)XmSelectionBoxGetChild( id, XmDIALOG_APPLY_BUTTON),
                      False );

  load_ai_recs( "A_DATA" );

  load_ai_recs( "I_DATA" );


              /******** BULLDOZE RECORDS INTO MOTIF LIST BOX *****/
  XtVaSetValues( id, XmNlistItems, ai_rec, XmNlistItemCount, num_rec, NULL);

  return(1);
}





void load_ai_recs( char *name_str )
/**************************************************************************\
\**************************************************************************/
{
  char outbuf[BUFSIZE];		/* contains messages to powerflow (8192 chars)*/
  char inbuf [BUFSIZE];		/* contains messages from powerflow */
  char *psubstr;                /* ptr to sub-string - used by strtok to cut
				   up the inbuf string */
  char outstr[256];		/* temporary way point string */
  int len;

  /* build command string to transmit to powerflow to get area data */
  sprintf( outbuf, "/get_data,type=%s\n%s\n", name_str, ENDOFMSG );

  ipc_synch_rw( inbuf, outbuf);		/* transmit inbuf to powerflow,
					 * receive outbuf from powerflow */

                       /* NOTE: strtok (string token pp. 302 C-Refference Man.)
			* 1st Call defines the string (inbuf)
			* each call afterwards has NULL telling strtok to
			* find the next token after the last call.
			* SEPCHAR is defined as "\n" in ipf_ipc.h            */

  psubstr = strtok( inbuf, SEPCHARS );	/* skip command string  */

  psubstr = strtok( NULL, SEPCHARS );	/* now get all the lines of data */

  while( psubstr != NULL )
  {
	/* check for the logical end of the message */
    if( *psubstr=='/'  || !strncmp( psubstr, ENDOFMSG, sizeof(ENDOFMSG)) )
	break;

    if( num_rec >= MAXAIREC )
    {
      printf("WARNING - from load_ai_rec (ai_data.c)\n" );
      printf(" Powerflow sent more than %d area/interchg records\n", MAXAIREC );
      printf(" Only %d records were accepted\n" , MAXAIREC );
	break;
    }

    len = strlen(psubstr) + 1;
    len = min((int)strlen(psubstr) + 1, MAXAILEN - 1);
    if(len > (int)sizeof(outstr) - 1) {
      printf("ERROR - len reset to outstr\n");
      len = (int)sizeof(outstr) - 1;
    };

    strncpy( outstr, psubstr, len );
    outstr[len] = '\0';

    ai_rec[num_rec++] = XmStringCreate( outstr, XmSTRING_DEFAULT_CHARSET);
 
    psubstr = strtok( NULL, SEPCHARS );		/* up pointer to next string */

  }
}
