static char sccsid[]="@(#)bussect.c	20.11 6/2/98";
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
*	bussect.c
* 
* Thu Sep 10 16:16:08 PDT 1992 [dbs] delete old lists when doing sect_init
\*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/MessageB.h>
#include "ipf_ipc.h"
#include "dmgr.h"
#include "fm.h"
#include "graph_data.h"
#include "base_data.h"

#include "em.h"
em_init

static Widget w1, w2, id1, id2, t1; /* dbs */

#define SEPCHARS "\n"
#define EOM      "*[EOM]\n"

int reccmp			( char *s1, char *s2);
int busFoundInPF		( char *card);
int askUser (Widget parent, char *labell, char *label2, char *str);

void GetFromList		( Widget w, char *str);
void chg2PF			( char *card, char op);
void chgs2PF			( char *card, char op);

extern int getCurBus		( char **pbus );
extern Widget widget_id		( char *widget_name );
extern int ipc_synch_rw		( char *rtntext, char *intext);
extern int is_err_msg		( char *line );
extern void addGraphBusXY	( int x, int y);
extern void VUIT_Unmanage	( char *);
extern int setCurBus            ( char * );

char savebuf[BUFSIZE];	/* 4096 characters */
static int id2_editable = 1;   /* disallow edits after first change */
#define MAXLINELIST 10000



AddToList(Widget w,char *str,char *ignored,int (*cmp)()) /* dbs */
/***************************************************************************\
*  Add string to MOTIF list box (alphabetical order)
*
*  Called by: sect_bus, sect_init
*
\***************************************************************************/
{
  char *line = strtok(str,SEPCHARS);
  int (*rec_cmp)();

  if (cmp == NULL)
    rec_cmp = reccmp;
  else
    rec_cmp = strcmp;

  while(line != NULL)
  {
    XmString motif_string, *strlist;
    int cnt;
    if (strchr(ignored,line[0]) == NULL  && !is_err_msg(line))
    {
      motif_string = XmStringCreate(line,XmSTRING_DEFAULT_CHARSET);

      XtVaGetValues(w,XmNitemCount,&cnt,XmNitems,&strlist,NULL);
      for (; cnt > 0; --cnt)	/* loop to find alpha location */
      {
        char *text;
        XmStringGetLtoR(strlist[cnt-1],XmSTRING_DEFAULT_CHARSET, &text);
        if (rec_cmp(text, line) <= 0)
	{
          XtFree(text);
          break;	/* breaks out of for loop */
        }
        else
	  XtFree(text);
      }

      XmListAddItemUnselected( w, motif_string, cnt+1 );
      XmStringFree(motif_string);
    }
    line = strtok(NULL, SEPCHARS);
  }
}


void GetFromList(Widget w, char *str)
/***************************************************************************\
* Get data from list w and append to buffer str
*
*  Called by: sect_ok
*
\***************************************************************************/
{
   int cnt;
   XmString *strlist;
   XtVaGetValues(w,XmNitemCount,&cnt,XmNitems,&strlist,NULL);
   for (; cnt > 0; --cnt) {
      char *text;
      XmStringGetLtoR(strlist[cnt-1],XmSTRING_DEFAULT_CHARSET, &text);
      strcat(str,text);
      strcat(str,"\n");
      XtFree(text);
   }
}


static char rtype[]="BX+";

int reccmp(char *s1, char *s2) {
/***************************************************************************\
*  compares two line records
*
*  Called by: AddToList
\***************************************************************************/
   char *r1 = strchr(rtype, s1[0]); 
   char *r2 = strchr(rtype, s2[0]); 

   r1 = (r1)?(r1):rtype+10;
   r2 = (r2)?(r2):rtype+10;
   if (r1 > r2)
     return(1);
   else if (r1 < r2)
     return(-1);
   else if (r1 < (rtype+10))
   {
      if (s1[1] > s2[1]) return(1);
      else if (s1[1] < s2[1]) return(-1);
      else if (memcmp(&s1[18], &s2[18], 2) > 0) return(1);
      else if (memcmp(&s1[18], &s2[18], 2) < 0) return(-1);
      else if (memcmp(&s1[3], &s2[3], 3) > 0) return(1);
      else if (memcmp(&s1[3], &s2[3], 3) < 0) return(-1);
   }
   else  /* r1 > 10 */
   {
      if      (memcmp(&s1[19], &s2[19], 8) > 0) return(1);  /* 2nd bus */
      else if (memcmp(&s1[19], &s2[19], 8) < 0) return(-1);
      else if (memcmp(&s1[27], &s2[27], 4) > 0) return(1);  /* 2nd kv  */
      else if (memcmp(&s1[27], &s2[27], 4) < 0) return(-1);
      else if (memcmp(&s1[31], &s2[31], 2) > 0) return(1);
      else if (memcmp(&s1[31], &s2[31], 2) < 0) return(-1);
      else if (s1[0] > s2[0]) return(1);
      else if (s1[0] < s2[0]) return(-1);
   }
   return(0);
}


void sect_bus(Widget w, int *tag, XmListCallbackStruct *list_data) /* dbs */
/***************************************************************************\
*   Toggle line records between two list widgets on bus sectionalize screen.
*   Only move them if there are two unique busses.  If tag = 0 then move from
*   left window to right. otherwist move from right to left.  Rename branches
*   as they are moved between panes.
*   9/28 modified to properly create line data for sectionalized bus
*
*  Called by: clicking on any item in one of two list widgets
****************************************************************************/
{
  int cnt;
  XmString *strlist;
  char *bufstr[2], *buf, *nbuf;
  char bufstr3[30];

  bufstr[0] = XmTextGetString(id1);	/* addr bus 1 card  */
  nbuf = XmTextGetString(id2);		/* addr bus 2 card  */
  bufstr[1] = bufstr3;
  strcpy(bufstr[1],bufstr[0]);
  memset(bufstr[1]+6,' ', 8);
  memcpy(bufstr[1]+6,nbuf, (strlen(nbuf)>8)?8:strlen(nbuf));

  XtFree(nbuf);
  buf = bufstr[(*tag)^1];		/* selected widget item */

  /* if names not same and not all spaces */
  if( (strncmp(bufstr[0]+6,bufstr[1]+6,12) != 0) &&
      (strncmp(bufstr[1]+6,"        ", 8) != 0)	)
  {
    char *str;

	/* ensure user's new bus name 	    */
	/* doesn't match an existing name ! */
	/* don't want warnings from pf!!!   */
    if (id2_editable) {
      if (busFoundInPF(bufstr[1]+6)) {
        err.line = EM_LINE;
        err.ident = sccsid;
        err.msg =
          "Sectionalized Bus 2 name is already in case.\n Rename Bus 2\n";
        err.type = FATAL;
        em_show(&err);
        return;	
      } 
      XtVaSetValues(id2,XmNeditable,FALSE,NULL);	/* no longer editable */
      id2_editable = 0;
    }

    XmStringGetLtoR(list_data->item, XmSTRING_DEFAULT_CHARSET, &str);

    /* find all sections of line and move them */
    XtVaGetValues(w,XmNitemCount,&cnt,XmNitems,&strlist,NULL);	/* get cnt */
    for (; cnt > 0; --cnt)
    {
      char *text;
      XmStringGetLtoR(strlist[cnt-1],XmSTRING_DEFAULT_CHARSET, &text);
      if(NULL == text) {
        printf("Programmer error - no text found on bussect\n");
        return;
      }
      if (strncmp(&str[6],&text[6],26) == 0)
      {
         int tstlen;
         XmListDeleteItem(w,strlist[cnt-1]);	/* delete from widget */

		/* copy corrected 2st bus name into string */
         tstlen = (strlen(&buf[6])>12)?12:strlen(&buf[6]);
         memcpy(&text[6],&buf[6], tstlen);

         if (*tag)
	   AddToList(w1,text,"/B*",NULL);	/* add to left widget */
         else
	   AddToList(w2,text,"/B*",NULL);	/* add to right widget */

      }
      XtFree(text);
    }
    XtFree(str);
  }
  else
  {
    err.line = EM_LINE;
    err.ident = sccsid;
    err.msg =
      "Cannot move branch to a bus with the same name.\n Rename Bus 2\n";
    err.type = FATAL;
    em_show(&err);
  }
  XtFree(bufstr[0]);
}


sect_init()
/***************************************************************************\
* initialize all widget id's, delete any old info in list widgets, and
*   fill with new data from the most recently selected bus.  The default name
*   for bus 2 is the same as bus 1 but must be unique before any data can
*   be moved between the lists.
*
*  3/1/94 default owner and zone are copied from original bus
*
*  Called by:  "Reset" pushbutton in BUS SECTIONALIZATION DIALOG
* 	       "Sectionalize" pushbutton in BUS FRONT BOX
\***************************************************************************/
{
   char *busname;
   char abusname[50];
   char busname2[50];
   char *shortbusname;
   char str[200];
   char inbuf[BUFSIZE];
   busname = &abusname[0];

   w1  = widget_id("sect_bus_item1_list");
   id1 = widget_id("sect_bus_id1_text");
   w2  = widget_id("sect_bus_item2_list");
   id2 = widget_id("sect_bus_id2_text");
   t1  = widget_id("bus_sect_dia_tie_text");

   if(w1==FALSE || w2==FALSE || id1==FALSE || id2==FALSE || t1==FALSE) {
     printf("error in sect_init, widgets not initialized\n");
     return;
   }
   if(getCurBus(&shortbusname) == -1) {
     printf("ERROR - no bus name set\n");
     return;
   }
   XmTextSetString(t1,"");

   XmListDeleteAllItems(w1);
   XmListDeleteAllItems(w2);
   sprintf(busname, "B     %12.12s", shortbusname);
   sprintf(str,"/GET_DATA, TYPE=INPUT\n%s\n%s",busname,EOM);
   ipc_synch_rw(inbuf,str);

   
   strncpy(busname,strchr(inbuf,'\n')+1,20);
   busname[20]='\0';
   XmTextSetString(id1,busname);
   sprintf(busname2, "%8.8s", shortbusname);     
   XmTextSetString(id2,busname2);
   strcpy(savebuf,inbuf);
   AddToList(w1,inbuf,"/B*",NULL);
   XtVaSetValues(id2,XmNeditable,TRUE,NULL);
   id2_editable = 1;
}


void sect_tie(w, tag, list_data) /* dbs */
/***************************************************************************\
*
*
*  Called by: "Bus Tie" pushbutton in BUS SECTIONALIZE DIALOG
\***************************************************************************/
Widget		w;
int		*tag;
unsigned long *list_data;
{
  char str[BUFSIZE];
  char bufstr3[30];
  char *bufstr[2], *buf;
  bufstr[0] = XmTextGetString(id1);
  bufstr[1] = bufstr3;
  buf = XmTextGetString(id2);
  strcpy(bufstr[1],bufstr[0]);
  memset(bufstr[1]+6,' ', 8);
  memcpy(bufstr[1]+6,buf, (strlen(&buf[0])>8)?8:strlen(&buf[0]));
  XtFree(buf);
  if( (strncmp(&bufstr[1][6], "        ", 8) != 0) &&
      (strncmp(bufstr[0],bufstr[1],20) != 0) ) {
    id2_editable = 0;
    XtVaSetValues(id2,XmNeditable,FALSE,NULL);
    strcpy(str,"L                                           0000100                                    "); /* fill in x */
    memcpy(&str[6],&bufstr[0][6],
      (strlen(&bufstr[0][6])>12)?12:strlen(&bufstr[0][6]));
    memcpy(&str[19],&bufstr[1][6],
      (strlen(&bufstr[1][6])>8)?8:strlen(&bufstr[1][6]));
    memcpy(&str[27],&bufstr[0][14],
      (strlen(&bufstr[0][14])>4)?4:strlen(&bufstr[0][14]));
    XmTextSetString(t1,str);
  } else {
    err.line = EM_LINE;
    err.ident = sccsid;
    err.msg =
      "Cannot tie branch to a bus with the same name.\n Rename Bus 2\n";
    err.type = FATAL;
    em_show(&err);
  }
  XtFree(bufstr[0]);
}


int busFoundInPF(char *name) {
/***************************************************************************\
*
*   9/28 modified to correctly format for new pf call
*   3/1/94 removed conditional code to always use BUS_EXISTS
\***************************************************************************/
  char str[BUFSIZE];
  char inbuf[BUFSIZE];
  char *line;
  int rtncde;
  sprintf(str,"/GET_DATA, TYPE=BUS_EXISTS, BUS = \"%12.12s\"\n%s",name,EOM);
  rtncde = ipc_synch_rw(inbuf,str);
  /* BUS_EXIST return TRUE if bus exists, otherwise FALSE */
  return(rtncde);
}

void chg2PF(char *card, char op) {
/***************************************************************************\
*
*
\***************************************************************************/
   char str[BUFSIZE];
   char inbuf[BUFSIZE];
   card[2] = op;
   sprintf(str,"/CHANGES,FILE=*\n%s\n%s",card,EOM);
   ipc_synch_rw(inbuf,str);
}

void chgs2PF(char *card, char op) {
/***************************************************************************\
*
*
\***************************************************************************/
/* cannot use strtok to parse and send changes individually.
   I think strtok must be used by a lower level routine and is not re-entrant
   so I get the wrong substring back?????
  char *line = strtok(card,SEPCHARS);
  while(line != NULL) {
    chg2PF(line,op);
    line = strtok(NULL, SEPCHARS);
  }
*/

   char str[BUFSIZE];
   char inbuf[BUFSIZE];
   sprintf(str,"/CHANGES,FILE=*\n%s\n%s",card,EOM);
   ipc_synch_rw(inbuf,str);
}


void sect_ok(Widget w, int *tag, unsigned long *list_data) /* dbs */
/***************************************************************************\
*
*
*  Called by: "OK" pushbutton in BUS_SECT_DIALOG
* 
*  3/1/94 send just 2 buffers, one to delete, one to add
\***************************************************************************/
{
   GraphElement *pfoundrec;
   char str[BUFSIZE];
   char nstr[13];
   char *line;
   int  x,y;
   char *bufstr1 = XmTextGetString(id1);	/* orginal bus 1 name	    */
   char *bufstr4 = XmTextGetString(id2);	/* new sectional bus 2 name */
   char *bufstr3 = XmTextGetString(t1);		/* bus tie name 	    */
   char bufstr2[30];
   char *ignored = "B+XQ";
   DB_STAT stat;

   strcpy(bufstr2,bufstr1);
   /* addr bus 2 card  */
   memset(bufstr2+6,' ', 8);
   memcpy(bufstr2+6,bufstr4, (strlen(bufstr4)>8)?8:strlen(bufstr4));
   XtFree(bufstr4);

   if( (strncmp(&bufstr2[6], "        ", 8) == 0) ||
       (strncmp(&bufstr1[6],&bufstr2[6],12) == 0) )
   {
    err.line = EM_LINE;
    err.ident = sccsid;
    err.msg =
      "Cannot add a new bus with the same name or blanks.\n Rename Bus 2\n";
    err.type = FATAL;
    em_show(&err);
    return;
   }

   chg2PF(bufstr1,'D');		/* delete the bus */

   str[0] = '\0';   /* initialize string so GetFromList can append to it */
/* must put back all B+X & Q records from original PF bus list */
   line = strtok(savebuf,SEPCHARS);

   while(line != NULL)
   {
     if (strchr(ignored,line[0]) != NULL) {
       line[2] = ' ';
       strcat(str, line);
       strcat(str, "\n");
     }
     line = strtok(NULL, SEPCHARS);
   }

   bufstr2[2] = ' ';
   bufstr3[2] = ' ';
   strcat(str, bufstr2);
   strcat(str, "\n");

   if (strlen(bufstr3) > 0) {
     strcat(str, bufstr3);
     strcat(str, "\n");
   }

   GetFromList(w1, str); /* Append the data from our 2 scrolled lists */
   GetFromList(w2, str);

   chgs2PF(str,' ');		/* Add a bus */

   stat = findBusGraphVertexRec(&bufstr1[6],&pfoundrec);
   if(D_FOUND == stat)
   {
     x = pfoundrec->x;	/* use the x and y only if the bus is found */
     y = pfoundrec->y;
   }
   else
   {
     x = 10;
     y = 10;
   }

   deleteVertexbyName(&bufstr1[6]);

   strncpy(nstr,&bufstr1[6],12);
   nstr[12] = '\0';
   setCurBus(nstr);
   addGraphBusXY(x,y);

   strncpy(nstr,&bufstr2[6],12);
   nstr[12] = '\0';
   setCurBus(nstr);
   addGraphBusXY(x+20,y);

   XtFree(bufstr1);
   XtFree(bufstr3);
   VUIT_Unmanage("bus_sect_dialog");
   VUIT_Unmanage("bus_front_box");
}


void overstrike(Widget w, int *tag, XmTextVerifyCallbackStruct *cbs)
/***************************************************************************\
*
*
*  Called by: typing char in main edit_bus_id1_text of bus_edit_dialog
*	      or network_edit_text_box of network_edit_dialog
\***************************************************************************/
{
/*    printf("startPos=%d, endPos=%d, length=%d, str=%s!!!\n",
      cbs->startPos,cbs->endPos,cbs->text->length,cbs->text->ptr); */

   if ((cbs->text->length == 1) && (cbs->text->ptr[0] == '\n')) return;
   if ((cbs->startPos+1) < cbs->endPos) {
      char message[200];
      /* never delete a block of text
         prompt user yes or no to delete */
      sprintf(message,"Do you really want to delete the %d characters in your edit window",
         cbs->endPos - cbs->startPos);
      if (askUser(w,"Yes - Delete & Exit","No - Exit only",message)==2) {
         cbs->doit = False;
         return;
      }
   }
   if (cbs->text->ptr && ((cbs->text->ptr[0] == '@') ||
                          (cbs->text->ptr[0] == '\t'))) {
      cbs->text->ptr[0] = ' ';             /* if an @ insert space instead */
   } else {
      int length_from_start=0; /* distance in chars from start of line */
      int length_to_end=0; /* distance from end of line or end of file */
      char *txt = XmTextGetString(w);
      if (txt != NULL) {
         while((cbs->startPos - length_from_start) >= 0) {
            if (txt[cbs->startPos - length_from_start] == '\n') break;
            ++length_from_start;
         }
/*         printf("%d characters from start of line \n",length_from_start);*/
         while(txt[cbs->startPos + length_to_end] != 0) {
            if (txt[cbs->startPos + length_to_end] == '\n') break;
            ++length_to_end;
         }
         /* printf("%d characters to end of line \n",length_to_end); */
         cbs->endPos +=
            (cbs->text->length>length_to_end)?length_to_end:cbs->text->length;
         XtFree(txt);
      }
   }
}

/* modified version of modal dialog from
The X Journal, May-June 1993 */


static void response(Widget w, int *answer, XmAnyCallbackStruct *cbs) {
/***************************************************************************\
*
*
*  Called by: askUser widget
\***************************************************************************/
   switch(cbs->reason) {
      case XmCR_OK:  *answer = 1; break;
      case XmCR_CANCEL: *answer = 2; break;
   }
}


int askUser (Widget parent, char *labell, char *label2, char *str) {
/***************************************************************************\
*  Displays box with two pushbuttons for user's response
*
\***************************************************************************/
   static Widget dialog;
   XmString text, labl, lab2;
   static int answer;
   extern XtAppContext app_context;
   Widget child_pb;
   
   labl = XmStringCreateSimple(labell);
   lab2 = XmStringCreateSimple(label2);
   text = XmStringCreateSimple(str);
   if (!dialog)
   {
      dialog = XmCreateQuestionDialog(parent,"dialog",NULL,0);
      XtVaSetValues( dialog, XmNdialogStyle,
		XmDIALOG_FULL_APPLICATION_MODAL,
		XmNokLabelString, labl,
		XmNcancelLabelString,lab2, NULL);
      XtAddCallback(dialog,XmNokCallback,(XtCallbackProc)response,&answer);
      XtAddCallback(dialog,XmNcancelCallback,(XtCallbackProc)response,&answer);

      /* don't want the HELP button - remove it */
      child_pb = XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON);
      if(XtIsManaged(child_pb))
      {
        XtUnmanageChild(child_pb);
        XtDestroyWidget(child_pb);
      }
 
   }
   answer = 0;
   XtVaSetValues(dialog,XmNmessageString, text,
      XmNokLabelString,labl,XmNcancelLabelString,lab2,NULL);
   XmStringFree(text);
   XmStringFree (labl) ;
   XmStringFree(lab2);
   XtManageChild(dialog);
   XtPopup(XtParent(dialog),XtGrabNone);
   while (answer == 0) XtAppProcessEvent(app_context, XtIMAll);
   XtPopdown(XtParent(dialog));
   return (answer) ;
}
