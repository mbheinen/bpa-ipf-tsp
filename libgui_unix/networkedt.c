static char sccsid[]="@(#)networkedt.c	1.4 2/15/95";
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
\*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/MessageB.h>
#include "dmgr.h"
#include "fm.h"
#include "ipf_ipc.h"
#include "graph_data.h"
#include "base_data.h"

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
extern void AppendToList	(Widget w,char *str,char *ignored,int (*cmp)());

static int apply_filtersz	();
static void get_filtersz	();


#define MAXLINELIST 10000
#define MAXCARDS 30
#define SEPCHARS "\n"
#define EOM      "*[EOM]\n"

static  Widget eid1, cw;
static  char cards[MAXCARDS][200];

static  struct { Widget form;
		 Widget list;
		 Widget apply;
		 Widget reset; } edit_item[7];

static int  work_cnt=0;
static int  max_flt=1;
static int  editor=0;		/* default to Internal editor mode */
static int  edit_remap[]  ={ 0,1,2,3,4,5,6 };
static char *edit_labels[]={ "","AREAS","ZONES","OWNERS","BASES",
                             "TYPES","BUSES"};
static char *edit_names[] ={ "","AREA_LIST" , "ZONE_LIST",  "OWNER_LIST",
				"BSEKV_LIST", "RECORD_LIST","BUS_LIST"};
static char edit_filters[6][1000];




network_edit_init() {
/***************************************************************************\
*
*
*  Called by: "Network Data Edit" cascade pulldown pushbutton.
\***************************************************************************/
   int cnt;
   char str[1000],inbuf[BUFSIZE];
   
   cw   = widget_id( "network_edit_count"   );
   eid1 = widget_id( "network_edit_text_box");

   edit_item[0].form  = NULL;
   edit_item[1].form  = widget_id("network_form1"     );
   edit_item[2].form  = widget_id("network_form2"     );
   edit_item[3].form  = widget_id("network_form3"     );
   edit_item[4].form  = widget_id("network_form4"     );
   edit_item[5].form  = widget_id("network_form5"     );
   edit_item[6].form  = widget_id("network_form6"     );
   edit_item[1].list  = widget_id("network_list1");
   edit_item[2].list  = widget_id("network_list2");
   edit_item[3].list  = widget_id("network_list3");
   edit_item[4].list  = widget_id("network_list4");
   edit_item[5].list  = widget_id("network_list5");
   edit_item[6].list  = widget_id("network_list6");
   edit_item[1].apply = widget_id("network_apply1_pb" );
   edit_item[2].apply = widget_id("network_apply2_pb" );
   edit_item[3].apply = widget_id("network_apply3_pb" );
   edit_item[4].apply = widget_id("network_apply4_pb" );
   edit_item[5].apply = widget_id("network_apply5_pb" );
   edit_item[6].apply = widget_id("network_apply6_pb" );
   edit_item[1].reset = widget_id("network_reset1_pb" );
   edit_item[2].reset = widget_id("network_reset2_pb" );
   edit_item[3].reset = widget_id("network_reset3_pb" );
   edit_item[4].reset = widget_id("network_reset4_pb" );
   edit_item[5].reset = widget_id("network_reset5_pb" );
   edit_item[6].reset = widget_id("network_reset6_pb" );
   for (cnt=1; cnt <= 6; ++cnt) {
      if (edit_item[cnt].list == False) {
         printf("error in network_edit_init, widgets not initialized\n");
         return;
      }
      XmListDeleteAllItems(edit_item[cnt].list);
   }
   cnt=apply_filtersz();
   sprintf(str,"Total Records = %6d",cnt);
   XmTextSetString( cw, str );
}


static int apply_filtersz() {
/***************************************************************************\
* Purpose: Gets all possible keys for each filter list from the PF
*	   and inserts them into the list boxes.
*
*  Called by: network_edit_init, edit_applyz, edit_resetz
\***************************************************************************/
   int cnt;
   int n,n1=0;
   char *more,filter[6000],inbuf[BUFSIZE];

   get_filtersz();

   for(cnt=max_flt; cnt <= 6; ++cnt)
   {
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

   sprintf(filter,"/GET_DATA, TYPE=%s\n","COUNT");
   for(n=1; n < max_flt; ++n) {
      sprintf(filter,"%s %s %s = %s\n",filter,(n==1)?"WHERE":"AND",
               edit_labels[edit_remap[n]], edit_filters[edit_remap[n]]);
   }
   strcat(filter,EOM);
   ipc_synch_rw(inbuf,filter);
   return(atol(strrchr(inbuf,'=')+1));
}


static void get_filtersz()
/***************************************************************************\
*
*
*  Called by: apply_filtersz
\***************************************************************************/
{
   int cnt;

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


static rearrange_filtersz()
/*************************************************************************\
* Purpose: Set the appearance order of the sort widgets,
*	   from left to right.
*
* Called by: edit_applyz, edit_resetz
\*************************************************************************/
{
   int n;

/*  printf("remap=%d, %d, %d, %d, %d, %d, %d\n",
		edit_remap[0], edit_remap[1], edit_remap[2], edit_remap[3],
		edit_remap[4], edit_remap[5], edit_remap[6]);
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
*/
}


Boolean fill_dialog_workprocz(Widget w)
/************************************************************************\
* Purpose: starts a background process of bringing up all the records -
*	   during idle time on screen.
*
* Called by: (started by) edit_busz
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
*     editor_wpid = XtAppAddWorkProc( app_context, fill_dialog_workprocz, w);
*/
      return((Boolean) TRUE);
  }
}


void edit_busz( Widget w, int *tag, XmPushButtonCallbackStruct *cbs)
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

   work_cnt = 0;
   if(NULL != tag)
     editor = *tag;	/* 0 = internal, 1 = external */
   else
     editor = 0;	/* set editor to internal one */
 
   pwid = w;
   editor_wpid = XtAppAddWorkProc( app_context, (XtWorkProc)fill_dialog_workprocz, pwid);
}



void edit_bus_closez( Widget w, int *tag, XmPushButtonCallbackStruct *cbs)
/***************************************************************************\
*
*
*  Called by: "Close" pushbutton on network editor.
\***************************************************************************/
{
   int x;
   XmTextSetString(eid1,"");
   for(x=1; x <= 6; ++x) {
      XtSetSensitive(edit_item[x].apply,True);
      XtSetSensitive(edit_item[x].reset,False);
   }
   max_flt=1;
   VUIT_Unmanage("network_edit_dialog");
}


void edit_send_to_pfz( Widget w, int *tag, XmPushButtonCallbackStruct *cbs)
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


void edit_applyz( Widget w, int *tag, XmPushButtonCallbackStruct *cbs )
/***************************************************************************\
*
*
*  Called by: Any of the 6 APPLY pushbuttons.
\***************************************************************************/
{
   int n = edit_remap[max_flt] ;
   int x,count;
   char str[100];

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

   rearrange_filtersz();

   count=apply_filtersz();

   sprintf(str,"Total records = %6d",count);

   XmTextSetString(cw,str);

}


void edit_resetz( Widget w, int *tag, XmPushButtonCallbackStruct *cbs)
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

   for (x=1; x <= 6; ++x) {  /* find position in remap array */
      if (edit_remap[x] == *tag) break;
   }

   for (; x < max_flt ; ++x) {
      edit_remap[x]=edit_remap[x+1];
   }

   edit_remap[--max_flt]= *tag;
   rearrange_filtersz();
   count=apply_filtersz();
   sprintf(str,"Total records = %6d",count);
   XmTextSetString(cw,str);
}


void output_column_numz( Widget w, int *tag, XmTextVerifyCallbackStruct *cbs)
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

  sprintf( alpha_text, "%d", col );
  lookup_and_fill_field( "network_column_num_text", alpha_text, 0, 3 );

}


