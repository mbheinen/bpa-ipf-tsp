static char sccsid[]="@(#)curbus.c	20.12 6/2/98";
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
/***************************************************************************\
* file curbus.c
* author: Dan Clark
* function: setCurBus and getCurBus
* data format expected of the current bus is the powerflow bus layout
* type/subtype/chgcode/owner/name/kv/zone
* 123456789012
* name    kv
\***************************************************************************/
#include <stdio.h>
#include <string.h>
#include <Xm/Xm.h>
#include "ipfdebug.h"
#define MAXKEY 64
#define CURBUSERROR -1
#define CURBUSOK 1
static char buskey[MAXKEY];
static int cnt = 0;
static int buskeyset = 0;

extern int check_powerflow	( char *namebase );
extern void create_bus_rec	();
extern int busFoundInPF		( char *card);
extern int reformat		( char *, char *, char *);


/****************************************************************************\
* Purpose: returns the CurBus from this module
*
* Called by: Lots of routines
\****************************************************************************/
int getCurBus(char **namebase)
{
  
  *namebase = (char *)&buskey[0];
  if(buskey[0] == '0') {
    return CURBUSERROR;
  } else {
    return CURBUSOK; 
  }
}


/********************************************************************\
* Purpose: set up a 12-char name/volt string for later referrence
*
* Called by: sect_ok(2) 		(bussect.c)	OK button
*	     tap_ok(3)			(linetap.c)	OK button
*	     alpha_bus_list_select	(reports.c)	List Name
*	     send_add_data_to_powerflow(2),		Add button
*	     send_del_data_to_powerflow (selection.c)	Del button
*	     drawAreaButtonCB(2) 	(toolbox.c)	Any Bus Gadget
\********************************************************************/
int setCurBus(char *namebase)
{
  char base[5];

  if (strlen(namebase) <= 0)
  {
    printf("*** WARNING ** from setCurBus (curbus.c) - bus key string empty\n");
    buskey[0] = '0';
    return CURBUSERROR;
  }

  if (strlen(namebase) > MAXKEY) {
    printf("*** WARNING ** from setCurBus (curbus.c) - bus key string greater than max %d\n", MAXKEY);
    buskey[0] = '0';
    return CURBUSERROR;
  }

  reformat(&namebase[8], base, "%f4.0");
  sprintf(buskey, "%8.8s%4.4s", namebase, base );

  buskeyset = 1;

  return CURBUSOK;
}


void setNewCurBusDefaultName( Widget w, XtPointer tag, XtPointer reason )
/**************************************************************************\
* Purpose: Create a NEW name (increment the counter)
*                Sets the current bus to "NEW00001230.".
*
*       Author: Bill Rogers / Dan Clark
*
* Called by: tap_init (linetap.c)
\**************************************************************************/
{
  static char namebase[32];

  cnt++;
  if(2 > cnt && buskeyset == 0)
    sprintf(namebase, "NEW%5.5d 230", cnt);
  else
    sprintf(namebase, "NEW%5.5d%4.4s", cnt, &buskey[8]);

  setCurBus(namebase);

}





void create_new_ipf_bus( Widget w, XtPointer tag, XtPointer reason )
/**************************************************************************\
* Purpose: create a bus name for the line tap.
*
*       Author: Bill Rogers / Dan Clark
*
* Called by: tap_init (linetap.c)
\**************************************************************************/
{
  char *tmpname;

  setNewCurBusDefaultName( w, tag, reason );
  getCurBus(&tmpname);

  /** now create the add bus dialog box with name newbus 500 ***/
  if( busFoundInPF(tmpname)==0 )
  {
   create_bus_rec();
  }

}




void resetCurBusDefaultName( Widget w, XtPointer tag, XtPointer reason )
/**************************************************************************\
* Purpose: create a bus name (using last counter)
*                Sets the current bus to "NEW00001".
*
*       Author: Dan Clark/Bill Rogers
*
* Called by: deleteBendVertex, breakEdge (both in toolbox.c)
\**************************************************************************/
{
  static char namebase[32];

  if(2 > cnt)
    sprintf(namebase, "NEW%5.5d 230", cnt);
  else
    sprintf(namebase, "NEW%5.5d%4.4s", cnt, &buskey[8]);

  setCurBus(namebase);

}


static char linekey[MAXKEY];

int getCurLine(char **plinekey)
{
  *plinekey = (char *)linekey;
  if(linekey[0] == '0') {
    return CURBUSERROR;
  } else {
    return CURBUSOK;
  }
}

int setCurLine(char *plinekey)
{

  strncpy(linekey, plinekey, MAXKEY);

  if (ipfdebug & DB_TraceMask ) {
    printf("setCurLine: linekey set to %s\n", linekey);
  }

  return CURBUSOK;
}

#ifdef TEST
main(int argc, char *argv[])
{
  char somekey[MAXKEY];
  char *curkey;
  
  sprintf(somekey, "%s", "B     ABCDEFGH1234");

  printf("main: somekey(%s)\n", somekey);
  if (setCurBus(somekey) ) {
    printf("CURBUSERROR: main - bad key data\n");
  }
  curkey = getCurBus();

  printf("main: curkey(%s)\n", curkey);
   
  return;
}
#endif
