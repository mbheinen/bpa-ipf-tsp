static char sccsid[]="@(#)linetap2.c	20.6 6/2/98";
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
*	linetap2.c   
*
\*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include "ipf_ipc.h"
#include "dmgr.h"
#include "graph_data.h"
#include "base_data.h"

#include "em.h"
em_init

static Widget  bus_1_txt, bus_2_txt, tap_txt, sect_1_txt, sect_2_txt;
static Widget  scale_wdgt;


extern int getCurLine		( char **);

extern void addGraphBusXY       ( int x, int y );
extern void chg2PF		( char *card, char op );
extern void chgs2PF		( char *card, char op );
extern int  is_err_msg		( char * );
extern void ipc_synch_rw	( char *, char * );
extern int  busFoundInPF	( char * );
extern int  setCurBus		( char * );
extern int  Vuit_MU		( char *, char * );
extern Widget widget_id 	( char * );

void tap_init2			();
void tap_changed		(Widget w,XtPointer tag,XtPointer list_data);
void tap_send			(Widget w,XtPointer tag,XtPointer list_data);


static char inbuf[BUFSIZE];
static char inbuf1[BUFSIZE];
static char inbuf2[BUFSIZE];
static char inbuforig[BUFSIZE];




void tap_changed(Widget w,XtPointer tag,XtPointer list_data) 
/*************************************************************************\
*
* Called by: Releasing scale slider
\*************************************************************************/
{
   char *tmpname;
   char tapname[BUFSIZE];
   XmString *strlist;
   int cnt=0, tunits=0;

   printf("Called tap_changed routine\n" );

   XtVaGetValues(scale_wdgt,XmNvalue,&tunits,NULL);
   printf("scale set to %lf\n", (double)tunits/10);

   strcpy(inbuf,inbuforig);
   tmpname=XmTextGetString(tap_txt);
   sprintf(tapname,"%-12s",tmpname);
   sprintf(tapname,"B     %-12s",tmpname);

   if (strncmp( tapname,"            ",12) != 0)
   {
     if (busFoundInPF(tapname)) 	/* see: bussect.c */
     {
       err.line = EM_LINE;
       err.ident = sccsid;
       err.msg = "\nThe tapped bus name must be unique.\n";
       err.type = WARNING;
       em_show(&err);
     }
   }
   else
   {
     err.line = EM_LINE;
     err.ident = sccsid;
     err.msg = "\nThe tapped bus name must be non-blank.\n";
     err.type = WARNING;
     em_show(&err);
   }
   XtFree(tmpname);
}


void tap_init2()
/*************************************************************************\
*
*
* on decstation variable width & height are getting overwritten
*
*  Called by: tap_apply button in bus_front box
\*************************************************************************/
{
   char *busname;
   char str[200];
   char *line;

   bus_1_txt   = widget_id( "line_tap_bus_1"     );
   bus_2_txt   = widget_id( "line_tap_bus_2"     );
   sect_1_txt  = widget_id( "line_tap_section_1" );
   sect_2_txt  = widget_id( "line_tap_section_2" );

   scale_wdgt = widget_id( "line_tap_position" );
   tap_txt    = widget_id( "line_tap_name"    );

   if(getCurLine(&busname) == -1) {
     printf("ERROR: linetap.c No current line set\n");
     return;
   }
     
   strncpy( str, &busname[6], 8 );
   str[8] = 0;
   XmTextSetString(bus_1_txt, str );
   strncpy( str, &busname[19], 8 );
   XmTextSetString(bus_2_txt, str );

   busname[32]='0';
   sprintf(str,"/GET_DATA, TYPE=INPUT\n%s\n%s\n", busname, ENDOFMSG );
   ipc_synch_rw(inbuf,str);

   printf("PF DATA:\n%s\n", inbuf );

   strcpy( inbuf1   , inbuf );
   strcpy( inbuf2   , inbuf );
   strcpy( inbuforig, inbuf );

   line = strtok( inbuf2, "\n" );
   while( line != NULL )
   {
     if( line[0]=='L' )
     {
       strcpy( str, line );
       XmTextSetString(sect_1_txt, str );
       XmTextSetString(sect_2_txt, str );
       break;
     }
     line = strtok( NULL, "\n" );
   }
    

}


void tap_send(Widget w,XtPointer tag,XtPointer list_data)
/*************************************************************************\
*
* Called by: OK (SEND) button on LINE TAPPING dialog
\*************************************************************************/
{

  GraphElement *pfoundrec;
  int x,y;
  char *line,*bus1,*bus2;
  char *tmpname;
  char str[8000];


  printf("Called tap_send routine \n");

  /* send changes to powerflow */
  strcpy(inbuf1,inbuforig);
/*
  inbuf2[0]='\0';
  line = strtok(inbuf1,SEPCHARS);
  while(line != NULL)
  {
    if (strchr("/*",line[0]) == NULL  && !is_err_msg(line))
    {
       line[2] = 'D';
       strcat(inbuf2,line);
       strcat(inbuf2,"\n");
    }
    line = strtok(NULL, SEPCHARS);
  }
*/
/* printf("inbuf2={%s}\n",inbuf2); */
/*  chgs2PF(inbuf2,'D');*/   /* delete all original lines */

/*  tmpname=XmTextGetString(tap_txt);
  sprintf(str,"B     %-12s",tmpname);
  chg2PF(str,' '); */      /* create tapped bus */

/*  str[0]='\0';
  chgs2PF(str,' ');

  str[0]='\0';
  chgs2PF(str,' ');

  bus1=XmTextGetString(bus_2_txt);
  findBusGraphVertexRec(bus1,&pfoundrec);
  x = pfoundrec->x;
  y = pfoundrec->y;
  deleteVertexbyName(bus1);
  
  strncpy(str,bus1,12);
  str[12] = '\0';
  setCurBus(str);
  addGraphBusXY(x,y);

  bus2=XmTextGetString(bus_1_txt);
  findBusGraphVertexRec(bus2,&pfoundrec);
  x = pfoundrec->x;
  y = pfoundrec->y;
  deleteVertexbyName(bus2);

  strncpy(str,bus2,12);
  str[12] = '\0';
  setCurBus(str);
  addGraphBusXY(x,y);

  strncpy(str,tmpname,12);
  str[12] = '\0';
  setCurBus(str);
  printf("x=%d, y=%d\n",x,y);
  addGraphBusXY(x+20,y);

  XtFree(tmpname);
  XtFree(bus1);
  XtFree(bus2);

  Vuit_MU( "U", "line_tap_dialog");
  Vuit_MU( "U", "bus_front_box");
*/
  printf("line tapping OK button not implemented\n");
}

