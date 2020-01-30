static char sccsid[]="@(#)busfilter.c	20.8 2/15/95";
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
*	busfilter.c
* 
* Thu Sep 10 16:16:08 PDT 1992 [dbs] delete old lists when doing sect_init
* enhanced with autoload logic by dc 94aug
\*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/MessageB.h>
#include <Xm/ToggleB.h>
#include "ipf_ipc.h"
#include "dmgr.h"
#include "fm.h"
#include "graph_data.h"
#include "base_data.h"
#include "ipfdebug.h"

#include "em.h"
em_init


extern int reccmp		( char *s1, char *s2 );

extern Widget widget_id		( char *widget_name );
extern void ipc_synch_rw	( char *, char * );
extern int  is_err_msg		( char *line );
extern void VUIT_Unmanage	( char * );
extern int  userCancelStatus	( int *status);
extern int userCancel		( Widget parent, char *labell, char *label2, 
				  char *str, Widget *rtn);
extern int  askUser 		( Widget parent, char *labell, char *label2, 
				  char *str);
extern void lookup_and_fill_field( char *, char *, int, int );

static int apply_filters	();
static void get_filters		();
void AppendToList		(Widget w,char *str,char *ignored,int (*cmp)());


#define MAXLINELIST 10000
#define MAXCARDS 30
#define SEPCHARS "\n"
#define EOM      "*[EOM]\n"

static  Widget eid1, cw;
static  char cards[MAXCARDS][200];

static  struct { Widget form;
		 Widget list;
		 Widget apply;
		 Widget reset;
                 Boolean autoload; } edit_item[7];

static int  work_cnt=0;
static int  max_flt=1;
static int  editor=0;		/* default to Internal editor mode */
static int  edit_remap[]  ={ 0,1,2,3,4,5,6 };
static char *edit_labels[]={ "","AREAS","ZONES","OWNERS","BASES",
                             "TYPES","BUSES"};
static char *edit_names[] ={ "","AREA_LIST" , "ZONE_LIST",  "OWNER_LIST",
				"BSEKV_LIST", "RECORD_LIST","BUS_LIST"};
static char edit_filters[6][1000];




edit_init() {
/***************************************************************************\
*
*
*  Called by: "Network Data Edit" cascade pulldown pushbutton.
\***************************************************************************/
   int cnt;
   char str[1000],inbuf[BUFSIZE];
   Widget wid;
   
   if(ipfdebug & (DB_TraceMask | DB_BusFilterMask)) {
      fprintf(ipfdbio, "edit_init: start\n");
   }   

   cw   = widget_id( "edit_bus_count"   );
   eid1 = widget_id( "edit_bus_id1_text");

   edit_item[0].form  = NULL;
   edit_item[1].form  = widget_id("edit_bus_form1"     );
   edit_item[2].form  = widget_id("edit_bus_form2"     );
   edit_item[3].form  = widget_id("edit_bus_form3"     );
   edit_item[4].form  = widget_id("edit_bus_form4"     );
   edit_item[5].form  = widget_id("edit_bus_form5"     );
   edit_item[6].form  = widget_id("edit_bus_form6"     );
   edit_item[1].list  = widget_id("edit_bus_item1_list");
   edit_item[2].list  = widget_id("edit_bus_item2_list");
   edit_item[3].list  = widget_id("edit_bus_item3_list");
   edit_item[4].list  = widget_id("edit_bus_item4_list");
   edit_item[5].list  = widget_id("edit_bus_item5_list");
   edit_item[6].list  = widget_id("edit_bus_item6_list");
   edit_item[1].apply = widget_id("edit_bus_apply1_pb" );
   edit_item[2].apply = widget_id("edit_bus_apply2_pb" );
   edit_item[3].apply = widget_id("edit_bus_apply3_pb" );
   edit_item[4].apply = widget_id("edit_bus_apply4_pb" );
   edit_item[5].apply = widget_id("edit_bus_apply5_pb" );
   edit_item[6].apply = widget_id("edit_bus_apply6_pb" );
   edit_item[1].reset = widget_id("edit_bus_reset1_pb" );
   edit_item[2].reset = widget_id("edit_bus_reset2_pb" );
   edit_item[3].reset = widget_id("edit_bus_reset3_pb" );
   edit_item[4].reset = widget_id("edit_bus_reset4_pb" );
   edit_item[5].reset = widget_id("edit_bus_reset5_pb" );
   edit_item[6].reset = widget_id("edit_bus_reset6_pb" );
   edit_item[0].autoload = (Boolean)False;
   edit_item[1].autoload = (Boolean)True;
   edit_item[2].autoload = (Boolean)True;
   edit_item[3].autoload = (Boolean)True;
   edit_item[4].autoload = (Boolean)True;
   edit_item[5].autoload = (Boolean)True;
   edit_item[6].autoload = (Boolean)True;
	
   for (cnt=1; cnt <= 6; ++cnt) {
      if (edit_item[cnt].list == False) {
         printf("error in edit_init, widgets not initialized\n");
         return;
      }
      XmListDeleteAllItems(edit_item[cnt].list);
   }
   if((wid = widget_id("bus_edit_autoload_tb")) == False) {
      printf("Warning: bus_edit_autoload_tb widget not found\n");
   } else {
      edit_item[6].autoload = XmToggleButtonGetState(wid);
      /* printf("autoload_tb = %d\n", edit_item[6].autoload); */
   }
   cnt=apply_filters();
   sprintf(str,"Total Records = %6d",cnt);
   XmTextSetString( cw, str );
}


static int apply_filters() {
/***************************************************************************\
* Purpose: Gets all possible keys for each filter list from the PF
*	   and inserts them into the list boxes.
*
*  Called by: edit_init, edit_apply, edit_reset
\***************************************************************************/
   int cnt;
   int n,n1=0;
   char *more,filter[6000],inbuf[BUFSIZE];

   if(ipfdebug & (DB_TraceMask | DB_BusFilterMask)) {
      fprintf(ipfdbio, "apply_filters: start\n");
   }   

   get_filters();

   for(cnt=max_flt; cnt <= 6; ++cnt)
   {
     if(edit_item[edit_remap[cnt]].autoload == True) {
      XmListDeleteAllItems(edit_item[edit_remap[cnt]].list);
      n1=0;
      do {
         sprintf(filter,"/GET_DATA, TYPE=%s%s\n",
                  edit_names[edit_remap[cnt]],(n1==0)?"":", CONTINUE");
         ++n1;
         for(n=1; n < max_flt; ++n) {
            sprintf(filter,"%s %s %s = %s\n",filter,(n==1)?"WHERE":"AND",
                  edit_labels[edit_remap[n]], edit_filters[edit_remap[n]]);
         }
         strcat(filter,EOM);
         ipc_synch_rw(inbuf,filter);
         more = strstr(inbuf,"*[MORE]");
/* printf("more=%d,{%s}\n",more,inbuf); */
         AppendToList(edit_item[edit_remap[cnt]].list,inbuf,"/*",strcmp);
      } while(more != NULL);
    } 
   }

   sprintf(filter,"/GET_DATA, TYPE=%s\n","COUNT");
   for(n=1; n < max_flt; ++n) {
      sprintf(filter,"%s %s %s = %s\n",filter,(n==1)?"WHERE":"AND",
               edit_labels[edit_remap[n]], edit_filters[edit_remap[n]]);
   }
   strcat(filter,EOM);
   ipc_synch_rw(inbuf,filter);
   return(atol(strrchr(inbuf,'=')+1));
}


void AppendToList(Widget w,char *str,char *ignored,int (*cmp)())  /* dbs */
/***************************************************************************\
*
*  Called by: apply_filters
\***************************************************************************/
{
  XmString xlns[MAXLINELIST];
  XmString motif_string, *strlist;
  char *line = strtok(str,SEPCHARS);
  int xcnt;
  int cnt;
  int (*rec_cmp)();

  if(ipfdebug & (DB_TraceMask | DB_BusFilterMask)) {
    fprintf(ipfdbio, "AppendToList: start\n");
  }   

  xcnt = 0;

  if (cmp == NULL)
    rec_cmp=reccmp;
  else
    rec_cmp = strcmp;

  XtVaGetValues(w,XmNitemCount,&cnt,XmNitems,&strlist,NULL);

/*** method crashes on OSF V2.0
* for (xcnt=0; xcnt<cnt; xcnt++) {
*   xlns[xcnt] = strlist[xcnt];
* }
***/

  while(line != NULL)
  {
    if (strchr(ignored,line[0]) == NULL  && !is_err_msg(line))
    {
      xlns[xcnt] = XmStringCreate(line,XmSTRING_DEFAULT_CHARSET);
      xcnt++;
    }
    line = strtok(NULL, SEPCHARS);
  }
  XmListAddItems(w, xlns, xcnt, 0); /* position 0 is end of list */
/*** method crashes on OSF V2.0 replace with listadditems
* XtVaSetValues(w,
*               XmNitems, xlns,
*               XmNitemCount, xcnt,
*               NULL);
***/

}


static void get_filters()
/***************************************************************************\
*
*
*  Called by: apply_filters
\***************************************************************************/
{
   int cnt;

   if(ipfdebug & (DB_TraceMask | DB_BusFilterMask)) {
      fprintf(ipfdbio, "get_filters: start\n");
   }   

   for(cnt=1; cnt <= 6; ++cnt) {
      int selcnt;
      XmString *strings;
      edit_filters[cnt][0]='\0';
      XtVaGetValues( edit_item[cnt].list,XmNselectedItemCount,&selcnt,
		     XmNselectedItems,&strings,NULL);
      for(;(--selcnt) >= 0;) {
         char *text;
         XmStringGetLtoR(strings[selcnt],XmSTRING_DEFAULT_CHARSET,&text);
         sprintf(edit_filters[cnt],"%s \"%s\"\n",edit_filters[cnt],text);
         XtFree(text);
      }
   }
}


static rearrange_filters()
/*************************************************************************\
* Purpose: Set the appearance order of the sort widgets,
*	   from left to right.
*
* Called by: edit_apply, edit_reset
\*************************************************************************/
{
   int n;

   if(ipfdebug & (DB_TraceMask | DB_BusFilterMask)) {
      fprintf(ipfdbio, "rearrange_filters: start\n");
   }

/*  printf("remap=%d, %d, %d, %d, %d, %d, %d\n",
		edit_remap[0], edit_remap[1], edit_remap[2], edit_remap[3],
		edit_remap[4], edit_remap[5], edit_remap[6]);
*/
   XtVaSetValues( edit_item[edit_remap[1]].form,
                     XmNleftAttachment,
		     XmATTACH_FORM,
		     NULL );

   for(n=2; n <= 6; ++n)
   {
      XtVaSetValues( edit_item[edit_remap[n]].form,
                     XmNleftAttachment,
		     XmATTACH_WIDGET,
                     XmNleftWidget,
		     edit_item[edit_remap[n-1]].form,
		     NULL );
    }
}


Boolean fill_dialog_workproc(Widget w)
/************************************************************************\
* Purpose: starts a background process of bringing up all the records -
*	   during idle time on screen.
*
* Called by: (started by) edit_bus
\************************************************************************/
{
  extern XtAppContext app_context;
  static int linecnt;
  static Widget cancel_wid;
  char *more,*line,filter[BUFSIZE],inbuf[BUFSIZE],str[120];
  char tmpbuf[BUFSIZE];
  XmTextPosition cpos;
  int cancel_status;
  int cnt=0;
  int n;
  static FILE *fp = NULL;
  XtWorkProcId editor_wpid;
  XmString text;
  char ctext[128];

  if(ipfdebug & (DB_TraceMask | DB_BusFilterMask)) {
    fprintf(ipfdbio, "fill_dialog_workproc: start\n");
  }

  /* new filter => open output file, clear old data, & manage cancel box */
  if(0 == work_cnt)
  {
    do
    {
      cnt=0;
      if ((fp=fopen("editbus.dat","w")) == NULL) {
          perror("editbus");
          if ((cnt=
                askUser(w,"Retry","Cancel","Unable to open editbus.dat"))==2) {
             return;
          }
      }
    } while(cnt==1);

    /* clear the data from the internal editor window */
    XmTextSetString(eid1,"");
    linecnt = 0;
    /* dlc: now put up the cancel box */
    userCancel(w,"OK","Cancel",
                    "Press OK or Cancel to Interupt Lines = 0", &cancel_wid);
  }

  sprintf(filter,"/GET_DATA, TYPE=%s%s\n",
         "NETWORK_DATA",(work_cnt==0)?"":", CONTINUE");
  ++work_cnt;
  for(n=1; n < max_flt; ++n) {
      sprintf(filter,"%s %s %s = %s\n",filter,(n==1)?"WHERE":"AND",
              edit_labels[edit_remap[n]], edit_filters[edit_remap[n]]);
  }
  strcat(filter,EOM);
  ipc_synch_rw(inbuf,filter);
  more = strstr(inbuf,"*[MORE]");
  line = strtok(inbuf,SEPCHARS);
  sprintf(tmpbuf,"\0");
  while(line != NULL)
  {
    if (strchr("/*",line[0]) == NULL) {
      if (editor == 0) {			/* Internal editor */
        strcat(tmpbuf,line);
        strcat(tmpbuf, "\n");
      }
      fputs(line,fp);
      fputs("\n",fp);
      linecnt++;
    }
    line = strtok(NULL, SEPCHARS);
  }

  cpos=XmTextGetLastPosition(eid1);
  XmTextInsert(eid1,cpos,tmpbuf);

  sprintf(ctext, "Press OK or Cancel to Interupt Lines = %d", linecnt);
  text = XmStringCreateSimple(ctext);
  XtVaSetValues(cancel_wid, XmNmessageString, text, NULL);
  XmStringFree(text);

  userCancelStatus(&cancel_status);
  if((more != NULL) && (cancel_status == 0)) {
      /* more data is available so continue calling workproc */
    return((Boolean) FALSE);
  }
  else
  {
    /* wrapup since there is no more data or a cancel was called */
    fclose(fp);
    XtUnmanageChild(cancel_wid);
    if (editor == 1) {		/* External editor mode */
#ifdef VMS
      system("editbus");	/* executes file *editbus in user's directory */
#else
      system(gfind("editbus","r"));
#endif
    }
/*
*     editor_wpid = XtAppAddWorkProc( app_context, fill_dialog_workproc, w);
*/
      return((Boolean) TRUE);
  }
}


void edit_bus( Widget w, int *tag, XmPushButtonCallbackStruct *cbs)
/***************************************************************************\
*
*
*  Called by: "Internal editor" and "External editor" pushbutton
*		( tag = 0 )		( tag = 1 )
\***************************************************************************/
{
   extern XtAppContext app_context;
   static Widget pwid;

   XtWorkProcId editor_wpid;

   if(ipfdebug & (DB_TraceMask | DB_BusFilterMask)) {
     fprintf(ipfdbio, "edit_bus: tag=%d\n", *tag);
   }

   work_cnt = 0;
   if(NULL != tag)
     editor = *tag;	/* 0 = internal, 1 = external */
   else
     editor = 0;	/* set editor to internal one */
 
   pwid = w;
   editor_wpid = XtAppAddWorkProc( app_context, (XtWorkProc)fill_dialog_workproc, pwid);
}



void edit_bus_close( Widget w, int *tag, XmPushButtonCallbackStruct *cbs)
/***************************************************************************\
*
*
*  Called by: "Close" pushbutton on network editor.
\***************************************************************************/
{
   int x;

   if(ipfdebug & (DB_TraceMask | DB_BusFilterMask)) {
      fprintf(ipfdbio, "edit_bus_close: tag=%d\n", *tag);
   }

   XmTextSetString(eid1,"");
   for(x=1; x <= 6; ++x) {
      XtSetSensitive(edit_item[x].apply,True);
      XtSetSensitive(edit_item[x].reset,False);
   }
   max_flt=1;
   VUIT_Unmanage("bus_edit_dialog");
}


void edit_send_to_pf( Widget w, int *tag, XmPushButtonCallbackStruct *cbs)
/***************************************************************************\
*
*
*  Called by: "Send to PF" pushbutton.
\***************************************************************************/
{
  char str[BUFSIZE];
  char inbuf[BUFSIZE];
  int cnt=0;
  static FILE *fp = NULL;

  if(ipfdebug & (DB_TraceMask | DB_BusFilterMask)) {
     fprintf(ipfdbio, "edit_send_to_pf: tag=%d\n", *tag);
  }

  if (editor == 0)
  {		/* Internal editor */
     char *text;
     do
     {
       cnt=0;
       if ((fp=fopen("editbusn.dat","w")) == NULL)
       {
          perror("editbusn");
          if ((cnt=
               askUser(w,"Retry","Cancel","Unable to open editbusn.dat"))==2) {
               return;
           }
        }
     } while(cnt==1);

     text=XmTextGetString(eid1);
     fputs(text,fp);
     fclose(fp);
     XtFree(text);
   }
#ifdef VMS
   system("ipf_diff");
#else
   system(gfind("ipf_diff","r"));
#endif
   sprintf(str,"/CHANGES, FILE=editbusc.dat\n");
   strcat(str,EOM);
   ipc_synch_rw(inbuf,str);
}


void edit_apply( Widget w, int *tag, XmPushButtonCallbackStruct *cbs )
/***************************************************************************\
*
*
*  Called by: Any of the 6 APPLY pushbuttons.
\***************************************************************************/
{
   int n = edit_remap[max_flt] ;
   int x,count;
   char str[100];

   if(ipfdebug & (DB_TraceMask | DB_BusFilterMask)) {
      fprintf(ipfdbio, "edit_apply: tag=%d\n", *tag);
   }

   XtSetSensitive(edit_item[*tag].apply,False);
   XtSetSensitive(edit_item[*tag].reset,True);

   for (x=1; x <= 6; ++x)
   {
      if (edit_remap[x] == *tag)
      {
         edit_remap[x]=n;
         edit_remap[max_flt++]= *tag;
      }
   }

   rearrange_filters();

   count=apply_filters();

   sprintf(str,"Total records = %6d",count);

   XmTextSetString(cw,str);

}


void edit_reset( Widget w, int *tag, XmPushButtonCallbackStruct *cbs)
/***************************************************************************\
*
*
*  Called by: Any of 6 RESET pushbuttons.
\***************************************************************************/
{
  int n;
  int x,count;
  char str[100];
  XtSetSensitive(edit_item[*tag].apply,True);
  XtSetSensitive(edit_item[*tag].reset,False);
  XmListDeselectAllItems(edit_item[*tag].list);

   if(ipfdebug & (DB_TraceMask | DB_BusFilterMask)) {
      fprintf(ipfdbio, "edit_reset: tag=%d\n", *tag);
   }

   for (x=1; x <= 6; ++x) {  /* find position in remap array */
      if (edit_remap[x] == *tag) break;
   }

   for (; x < max_flt ; ++x) {
      edit_remap[x]=edit_remap[x+1];
   }

   edit_remap[--max_flt]= *tag;
   rearrange_filters();
   count=apply_filters();
   sprintf(str,"Total records = %6d",count);
   XmTextSetString(cw,str);
}


void output_column_num( Widget w, int *tag, XmTextVerifyCallbackStruct *cbs)
/***************************************************************************\
* Purpose: finds cursor position of text window - using motif.
*	   puts results in text box.
*
* Author: Bill Rogers	Jan 5, 1994
*         with help from Dan Clark.
\***************************************************************************/
{
  Position x, y;
  Position x1, y1, x0, y0, dx, dy;
  XmTextPosition pos;
  int col, row;
  char alpha_text[10];

  pos = XmTextGetTopCharacter(w);
  XmTextPosToXY( w, pos, &x0, &y0 );
  XmTextPosToXY( w, pos+1, &x1, &y1 );
  dx = x1-x0;
  if(0 == dx ) {
    printf("Warning dx == 0 reset to 1\n");
    dx = 1;
  }
  dy = y1-y0;
  if(0 == dy ) { 
    /* try to get the row change by jumping ahead to the second row */
    XmTextPosToXY( w, pos+151, &x1, &y1 );
    dy = y1-y0;
    if(0 == dy ) { 
      printf("Warning dy == 0 reset to 1\n");
      dy = 1;
    }
  }
/*  printf("delta x = %d, delta y = %d\n", dx,dy);*/

  pos = cbs->newInsert;
  XmTextPosToXY( w, pos, &x, &y );
/*  printf("Text position is:%d %d %d\n", pos, x, y );*/
  col = (int)(x - x0)/dx + 1;
  row = (int)(y - y0)/dy;

  if(ipfdebug & (DB_TraceMask | DB_BusFilterMask)) {
    fprintf(ipfdbio, "output_column_num: col=%d, dx=%d, dy=%d\n", col, dx, dy);
  }

  sprintf( alpha_text, "%d", col );
  lookup_and_fill_field( "edit_column_num_text", alpha_text, 0, 3 );

}


void bus_edit_autoload_toggle( Widget w, int *tag, XmAnyCallbackStruct *cbs)
{
  /* may want to load the bus list here when this is pressed */
  Widget wid;

  if((wid = widget_id("bus_edit_autoload_tb")) == False) {
    printf("Warning: bus_edit_autoload_toggle: bus_edit_autoload_tb not found\n");
  } else {
      edit_item[6].autoload = XmToggleButtonGetState(wid);
  }

  if(ipfdebug & (DB_TraceMask | DB_BusFilterMask)) {
    fprintf(ipfdbio, "bus_edit_autoload_toggle: tag=%d, autoload_tb=%d\n",
	*tag, edit_item[6].autoload);
  }


}
