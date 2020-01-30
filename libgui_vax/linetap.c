static char sccsid[]="@(#)linetap.c	20.16 6/2/98";
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
*	linetap.c   
* modified dlc may 19 
* default a new bus name to tap bus name
* search for the correct bus type for deleting old graph bus icons
* set the x,y of the new bus to be 1/2 way between two old busses
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
#include "ipfdebug.h"

#include "em.h"
em_init
#define MINDIV 1.0e-06

static Pixmap pix;
static Widget pct_rb_wid, kilo_rb_wid, mile_rb_wid, sect_rb_wid, t1, w1, w2,
                       bus_name1_wid, bus_name2_wid, s1, l1, rb_wid; /* dbs */
static Window window;
static GC tapGC;

char tap_zone[5], tap_owner[5];

extern Display *display;
extern int getCurLine(char **);
enum { PCT,MILE,KILO,SECT};

extern int is_err_msg	(char *);
extern void ipc_synch_rw(char *, char *);
extern AddToList	(Widget, char *, char *, void *);
extern Widget widget_id (char *);
extern int  Vuit_MU	( char *, char * );
extern int  busFoundInPF	( char * );
extern void GetFromList		( Widget w, char *str);
extern void addGraphBusXY       ( int x, int y);
extern void chg2PF		(char *card, char op);
extern void chgs2PF		(char *card, char op);
extern int setCurBus            ( char * );
extern int getCurBus            ( char ** );
extern double implied_dec_rw 	( char *str, int a, int b);
extern void setNewCurBusDefaultName	( Widget w, XtPointer tag,
					   XtPointer reason );
extern void lookup_and_get_field( char *, char *, int );

void draw_bus_mark	( int x );
void draw_tick_mark	( int x );
void tapline		( char *, char *, double);
void tap_apply		( Widget w, XtPointer tag,  XtPointer list_data);
void makelist1		( char *, char *, double);
void makelist2		( char *, char *, double);

#define MAXMI 20
static int mi_array[MAXMI];
static double react_array[MAXMI];
static double tmiles=(double)0.0;
static int  tlines=0;
static double treact=(double)0.0;


void totals(char *str) 
/**********************************************************************\
* Purpose: Get total length of all sections combined.
*
\**********************************************************************/
{ 
  char *line;
  int cnt=0;
  double miles=(double)0.0;
  long react=(long)0;

  tmiles=(double)0.0;
  tlines=0;
  treact=(double)0.0;

  line = strtok(str,SEPCHARS);

  while(line != NULL)
  {
    XmString motif_string, *strlist;
    int cnt;
    if (strchr("*/B",line[0]) != NULL  || is_err_msg(line))
    { }
    else
    {
      if (strncmp(line,"L ",2) == 0)
      {
	miles = implied_dec_rw( &line[62], 4, 1 );
        tmiles += miles;
	mi_array[tlines] = (int)tmiles;
        sscanf(&line[44],"%6ld",&react);
        if (react > 0) 
           treact+= (double)react/100000;
      }
      else if ((strncmp(line,"E ",2) == 0) || (strncmp(line,"T ",2) == 0))
      {
	mi_array[tlines] = (int)tmiles;
        sscanf(&line[44],"%6ld",&react);
        if (react > 0)
           treact+= (double)react/100000;
      }


      react_array[tlines++] = treact;
/* printf("(%5.5s ) (%4f) (%d) (%f) (%f)\n", &line[62],
*		 miles, mi_array[tlines], react, react_array[tlines-1] );
*/
    }
    line = strtok(NULL, SEPCHARS);
  }
  cnt = tlines;
  while(cnt < MAXMI) {
     mi_array[cnt] = 0;
     react_array[cnt++] = 0;
  }
}


#define LAB_HEIGHT 30
static char inbuf[BUFSIZE];
static char inbuf1[BUFSIZE];
static char inbuf2[BUFSIZE];
static char inbuforig[BUFSIZE];
static int utype;




tap_init()
/***********************************************************************\
*
*
* Called by: When line_tap_dialog is MAPPED (appears on screen)
\***********************************************************************/
{ /* on decstation variable width &| height are getting overwritten*/

   long tunits, width, height;
   Widget menuhist;
   int x,xpos;
   char *busname;
   char str[200];
   char *newbusname;
   char busnamebuf[32];

   w1		= widget_id("original_data_scroll_list");
   w2		= widget_id("tapped_data_scroll_list");
   bus_name1_wid = widget_id("line_tap_bus_1_text");
   bus_name2_wid = widget_id("line_tap_bus_2_text");
   pct_rb_wid	= widget_id("line_tap_button_pct");
   mile_rb_wid	= widget_id("line_tap_button_mile");
   kilo_rb_wid	= widget_id("line_tap_button_kilo");
   sect_rb_wid	= widget_id("line_tap_button_sect");
   l1		= widget_id("tap_scale_label");
   s1		= widget_id("line_tap_scale");
   rb_wid	= widget_id("line_tap_radio_box");
   t1		= widget_id("line_tap_name_text");

   XmListDeleteAllItems(w1);
   XmListDeleteAllItems(w2);

   if(getCurLine(&busname) == -1) {
     printf("ERROR: linetap.c No current line set\n");
     return;
   }
     
   memset(busnamebuf, '\0', sizeof(busnamebuf));

   sprintf(busnamebuf, "%12.12s", &busname[6]);		/* fill bus1 name */
   XmTextSetString(bus_name1_wid,&busnamebuf[0]);

   sprintf(busnamebuf, "%12.12s", &busname[19]);	/* fill bus2 name */
   XmTextSetString(bus_name2_wid,&busnamebuf[0]);

   busname[32]='\0';
   sprintf(str,"/GET_DATA, TYPE=INPUT\n%s\n%s\n",busname, ENDOFMSG);
   ipc_synch_rw(inbuf,str);

   strcpy(inbuf1,inbuf);
   strcpy(inbuforig,inbuf);
   strcpy(inbuf2,inbuf);
   AddToList(w1,inbuf2,"*/B",NULL);

   /** add code to put in a new bus default name ***/
   setNewCurBusDefaultName(0,0,0);
   getCurBus(&newbusname);
   XmTextSetString(t1, newbusname);
   XtVaGetValues(s1,XmNheight,&height,XmNwidth,&width,NULL);
   width -= 10; /* dbs make it smaller than the window, but I don't know
                       how much smaller,  I really need the size of the
                       scale widget? */

#ifdef REMOVE
   window = XtWindow(s1);
   /* printf("display=%d, depth=%d, width=%d, height=%d\n",display,
      DefaultDepth(display,DefaultScreen(display)),width,height); */
   if (pix) {
      XFreePixmap(display,pix);
      pix = 0;
   }
   pix = XCreatePixmap(display,window,width,LAB_HEIGHT,
      DefaultDepth(display,DefaultScreen(display)));
   if (!tapGC) tapGC=XCreateGC(display, window, 0L, (XGCValues *) NULL);
   XSetForeground(display,tapGC,WhitePixel(display,0));
   XFillRectangle(display,pix,tapGC,0,0,width,LAB_HEIGHT);
   XSetForeground(display,tapGC,BlackPixel(display,0));

   XDrawLine(display,pix,tapGC,7,LAB_HEIGHT/4,width-6,LAB_HEIGHT/4);
#endif

   totals(inbuf1);

   XtVaGetValues(rb_wid,XmNmenuHistory,&menuhist,NULL);

   if( menuhist == mile_rb_wid )	/* last button pushed */
   {
      utype = MILE;
      tunits = (long)tmiles;
      if(tunits <= 0) { /* avoid divide by zero */
        tunits = 1; 
      }
      draw_bus_mark( 4 );
      draw_bus_mark( width-4 );
      for (x=1; x < MAXMI; ++x) {
	 xpos = (int)(mi_array[x]*((double)width/tunits));
         draw_tick_mark( xpos );
      }
   }
   else if ( menuhist == kilo_rb_wid )
   {
      utype = KILO;
      tunits = (int)(tmiles * 1.60936);
      if(tunits <= 0) 
          tunits = 1;

      draw_bus_mark( 4 );
      draw_bus_mark( width-4 );
      for (x=1; x < MAXMI; ++x)
      {
	 xpos = (int)(mi_array[x]*1.60936*((double)width/tunits));
         draw_tick_mark( xpos );
      }
   }
   else if ( menuhist == sect_rb_wid )
   {
      utype = SECT;
      tunits = tlines*10;
      if(tunits <= 0)
        tunits = 1;
 
      draw_bus_mark( 4 );
      draw_bus_mark( width-4 );
      for (x=1; x <= tlines; ++x)
      {
	 xpos = (int)(x*10*((double)width/tunits));
         draw_tick_mark( xpos );
      }
   }
   else 			/* pct_rb_wid */
   {
      utype = PCT;		/* type of unit */
      tunits = 1000;
      draw_bus_mark( 4 );
      draw_bus_mark( width-4 );
      for (x=1; x < MAXMI; ++x)
      {
         double divisor;
         divisor = treact*react_array[x]*10*((double)width/tunits);
         if(divisor > MINDIV)
           xpos = (int)(100/divisor);
         else
           xpos = (int)1;

         draw_tick_mark( xpos );
      }
   }

#ifdef REMOVE
   XtVaSetValues(l1,XmNlabelType,XmPIXMAP,XmNlabelPixmap,pix,NULL);
#endif
   XtVaSetValues(s1,XmNmaximum,tunits,NULL);

   tap_apply( 0, 0, 0 ); 

}


void draw_tick_mark( int x )
{
  int N,W,S,E;
  N = LAB_HEIGHT/4-3;
  S = LAB_HEIGHT/4+3;
  W = x-1;
  E = x+1;
#ifdef REMOVE
   XDrawLine(display,pix,tapGC, W, N, W, S);
   XDrawLine(display,pix,tapGC, x, N, x, S);
   XDrawLine(display,pix,tapGC, E, N, E, S);
#endif
}

void draw_bus_mark( int x )
{
  int y;
  int s;	/* size of bus square */

  y = (int)LAB_HEIGHT/4;
  s = 3;
  
#ifdef REMOVE
   XDrawLine(display,pix,tapGC, x-s, y-s, x-s, y+s );
   XDrawLine(display,pix,tapGC, x-s, y+s, x+s, y+s );
   XDrawLine(display,pix,tapGC, x+s, y+s, x+s, y-s );
   XDrawLine(display,pix,tapGC, x+s, y-s, x-s, y-s );
#endif
}



void tap_apply(Widget w,XtPointer tag,XtPointer list_data) 
/************************************************************************\
* Purpose: Answers the tap slider movement, calculates impedence on
*	   each half of the tapped line.
*
*
\************************************************************************/
{
  char *tmpname;
  char tapname[BUFSIZE];
  int tunits=0;

  XtVaGetValues(s1,XmNvalue,&tunits,NULL);
  if(tunits <= 0) {
     tunits = 1;
  }

  tmpname=XmTextGetString(t1);		/* get name from list box */
  sprintf(tapname,"%-12s",tmpname);

  if (strncmp(tapname,"            ",12) == 0)
  {
    err.line = EM_LINE;
    err.ident = sccsid;
    err.msg = "\nThe tapped bus name must be non-blank.\n";
    err.type = WARNING;
    em_show(&err);
    return;
  }

  if (busFoundInPF(tapname))
  {
    err.line = EM_LINE;
    err.ident = sccsid;
    err.msg = "\nThe new bus tap name already exists in powerflow.\n";
    err.type = WARNING;
    em_show(&err);
    return;
  }

  XtFree(tmpname);

  strcpy ( inbuf, inbuforig );
  tapline( tapname, inbuf, (double)tunits/10 );

}


void tap_ok(Widget w,XtPointer tag,XtPointer list_data)
/*****************************************************************\
* Purpose: creates new tap by sending data to powerflow
*
\*****************************************************************/
{
  GraphElement *pfoundrec;
  int x,y;
  int x1,y1,x2,y2;
  char *line,*bus1,*bus2;
  char *tmpname;
  char str[8000];
 
  lookup_and_get_field( "bus_zone", tap_zone, 3 );
  printf("Zone is: %s\n", tap_zone );
  lookup_and_get_field( "bus_owner", tap_owner, 4 );
  printf("Owner is: %s\n", tap_owner );

  tap_apply(w, tag, list_data);

  /* send changes to powerflow */
  strcpy(inbuf1,inbuforig);
  inbuf2[0]='\0';
  line = strtok(inbuf1,SEPCHARS);
  while(line != NULL) {
    if (strchr("/*",line[0]) == NULL  && !is_err_msg(line)) {
       line[2] = 'D';
       strcat(inbuf2,line);
       strcat(inbuf2,"\n");
    }
    line = strtok(NULL, SEPCHARS);
  }

  chgs2PF(inbuf2,'D');   /* delete all original lines */

  tmpname=XmTextGetString(t1);
  sprintf(str,"B  %3.3s%-12s%s", tap_owner, tmpname, tap_zone );
  chg2PF(str,' ');       /* create tapped bus */

  str[0]='\0';
  GetFromList(w1, str); /* Append the data from our 2 scrolled lists */
  chgs2PF(str,' ');

  str[0]='\0';
  GetFromList(w2, str);
  chgs2PF(str,' ');

  bus1=XmTextGetString(bus_name1_wid);
  bus2=XmTextGetString(bus_name2_wid);
  findBusGraphVertexRec(bus1,&pfoundrec);

  x1 = pfoundrec->x;
  y1 = pfoundrec->y;
  deleteVertexbyName(bus1);

/*****************************************************************/
  strncpy(str,bus1,12);
  str[12] = '\0';
  setCurBus(str);

  addGraphBusXY(x1,y1);

  findBusGraphVertexRec(bus2,&pfoundrec);

  x2 = pfoundrec->x;
  y2 = pfoundrec->y;
  deleteVertexbyName(bus2);

/*****************************************************************/
  strncpy(str,bus2,12);
  str[12] = '\0';

  setCurBus(str);
  addGraphBusXY(x2,y2);

/*****************************************************************/
  strncpy(str,tmpname,12);
  str[12] = '\0';
  setCurBus(str);

  x = (int)(x1+x2)/2;
  y = (int)(y1+y2)/2;

  addGraphBusXY(x,y);

  XtFree(tmpname);
  XtFree(bus1);
  XtFree(bus2);

  Vuit_MU( "U", "line_tap_dialog");
  Vuit_MU( "U", "bus_front_box");

}


void tapline(char *tapname,char *inbuf,double tapvalue)
/***************************************************************\
* Purpose:
*
* Called by: tap_apply (above)
\***************************************************************/
{
  char *line;
  int state=0;
  int lineno = 0;
  int x;
  int tappoint;
  double tappct;
  double react;
  double divisor;

  XmListDeleteAllItems(w1);
  XmListDeleteAllItems(w2);
  inbuf1[0]='\0';
  inbuf2[0]='\0';

  switch(utype)
  {
    case KILO:
    case MILE:
      for(x=0; x<MAXMI; ++x)
      {
        if (mi_array[x] >= (tapvalue*10)) break;
      }
      tappoint = x;
      divisor = (mi_array[x]-mi_array[x-1]);

      if(divisor > MINDIV)
        tappct = ((tapvalue*10)-mi_array[x-1]) / divisor;
      else
      {
        printf("Warning - avoiding divide by zero set tappct 0\n");
        tappct = 0.0;
      }
    break;

    case SECT:
      tappoint = (int) tapvalue;
      tappct = tapvalue - (int)tapvalue;
      if ((react_array[tappoint] == 0) || 
            (tappoint && (react_array[tappoint] == react_array[tappoint-1])))
      {
        if (tappct > .5)
	{
           tappoint++;
           tappct = 1;
        }
      }
    break;

    case PCT:
      for(x=0; x<MAXMI; ++x)
      {
        if (react_array[x] > (tapvalue * treact/100))
	  break;
        else if (react_array[x] == (tapvalue * treact/100))
	{
          ++x;
	  break;
        }
      }
      tappoint = x;
      tappct = ((treact * tapvalue/100)-react_array[x-1]) / 
                   (react_array[x]-react_array[x-1]);
    break;

    default: printf("utype not defined\n");
  }

  line = strtok(inbuf,SEPCHARS);
  while(line != NULL)
  {
    if (strchr("*/B",line[0]) == NULL  && !is_err_msg(line))
    {
      if(lineno < tappoint)
        makelist1(line,tapname,(double)100);
      else if (lineno > tappoint)
	makelist2(line,tapname,(double)100);
      else
      {
        if (strncmp(line,"L ",2)==0)
	{
          sscanf(&line[44],"%6lf",&react);

          if (react==0)
	    printf("invalid line record X=0\n");
	  else if (react < 0)
            state = 2;
	  else
            state = 1;
        }

	else if (line[0]=='E')
          state = 3;
	else if (line[0]=='T')
          state = 2;

        switch(state)
	{
          case 1: /* L records with X > 0 */
          {
            char tmpbuf[BUFSIZE];
            strcpy(tmpbuf,line);
            makelist1(line,  tapname,tappct);
            makelist2(tmpbuf,tapname,1-tappct);
          }
          break;

          case 2: /* L records with X < 0 */
            makelist1(line,tapname,(double)1);
          break;

          case 3: /* E records            */
          {
            char tmpbuf[BUFSIZE];
            char tmpstr[BUFSIZE];
            strcpy(tmpbuf,line);
            sprintf(tmpstr,"%6ld",0);
            for(x=62;x < 74; x+=6)
	    {
	      strncpy(&line[x],tmpstr,6);
            }

            makelist1(line,tapname,(double)1);
            for(x=50;x < 62; x+=6)
	    {
              strncpy(&tmpbuf[x],tmpstr,6);
            }
            makelist2(tmpbuf,tapname,(double)1);
          }
         break;

         }
       }
       ++lineno;
     }
     line = strtok(NULL, SEPCHARS);
   }
   AddToList(w1,inbuf1,"",NULL);
   AddToList(w2,inbuf2,"",NULL);
}


void makelist1(char *line, char *tapname, double tappct)
/*****************************************************************\
* Purpose: Build modify card with pct R,X,G,B,miles values
*
*
\*****************************************************************/
{
  int x;
  char tmpstr[7];

  tmpstr[6]='\0';

  if (tappct == 0) return;

  if (tappct != 100)
  {
    long num=0;
    for(x=38;x < 62; x+=6)	/* adjust R,X,G,B values */
    {
      strncpy(tmpstr,&line[x],6);
      num=atol(tmpstr);
      sprintf(tmpstr,"%6ld",(long)(num*tappct));
      strncpy(&line[x],tmpstr,6);
    }

    tmpstr[4]='\0';		/* adjust miles value */
    strncpy(tmpstr,&line[62],4);
    num=atol(tmpstr);
    sprintf(tmpstr,"%4ld",(long)(num*tappct));
    strncpy(&line[62],tmpstr,4);
  }

  strncpy(&line[19],tapname,12);	/* tap name as 2nd bus name */
  strcat(inbuf1,line);
  strcat(inbuf1,"\n");
}




void makelist2(char *line, char *tapname, double tappct)
/*****************************************************************\
* Purpose: Build modify card with pct R,X,G,B,miles values
*
*
\*****************************************************************/
{
  int x;
  char tmpstr[7];

  tmpstr[6]='\0';
  if (tappct == 0) return;

  if (tappct != 100)
  {
    long num=0;
    for(x=38;x < 62; x+=6)
    {
      strncpy(tmpstr,&line[x],6);
      num=atol(tmpstr);
      sprintf(tmpstr,"%6ld",(long)(num*tappct));
      strncpy(&line[x],tmpstr,6);
    }
    tmpstr[4]='\0';
    strncpy(tmpstr,&line[62],4);
    num=atol(tmpstr);
    sprintf(tmpstr,"%4ld",(long)(num*tappct));
    strncpy(&line[62],tmpstr,4);
  }
  strncpy(&line[6],tapname,12);		/* tap name as 1st bus name */
  strcat(inbuf2,line);
  strcat(inbuf2,"\n");
}


#ifdef REFERENCE
   bufstr1=XmTextGetString(bus_name1_wid);
   bufstr1[2] = 'D';
   bufstr2=XmTextGetString(bus_name2_wid);
   bufstr2[2] = 'D';
   sprintf(str,"/CHANGES,FILE=*\n%s\n%s\n%s\n",bufstr1,bufstr2,ENDOFMSG);
   ipc_synch_rw(inbuf,str);
   bufstr1[2] = ' ';
   bufstr2[2] = ' ';
   sprintf(str,"/CHANGES,FILE=*\n%s\n%s\n",bufstr1,bufstr2);
   XtFree(bufstr1);
   XtFree(bufstr2);
   XtVaGetValues(w1,XmNitemCount,&cnt,XmNitems,&strlist,NULL);
   for (; cnt > 0; --cnt) {
      char *text;
      XmStringGetLtoR(strlist[cnt-1],XmSTRING_DEFAULT_CHARSET, &text);
      strcat(str,text);
      strcat(str,"\n");
      XtFree(text);
   }
   XtVaGetValues(w2,XmNitemCount,&cnt,XmNitems,&strlist,NULL);
   for (; cnt > 0; --cnt) {
      char *text;
      XmStringGetLtoR(strlist[cnt-1],XmSTRING_DEFAULT_CHARSET, &text);
      strcat(str,text);
      strcat(str,"\n");
      XtFree(text);
   }
   strcat(str,"*[EOM]\n");
   ipc_synch_rw(inbuf,str);
#endif
