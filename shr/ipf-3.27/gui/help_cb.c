static char sccsid[]="@(#)help_cb.c	20.3 2/15/95";
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
/************************************************************************
*
*      help_cb.c
*
* Fri Sep 11 17:29:48 PDT 1992  [dbs] modified new_text to add text to
*				      selection dialogs.
*
*************************************************************************/
#include  <X11/IntrinsicP.h>
#include  <X11/cursorfont.h>
#include  <Xm/Xm.h>
#include  <Xm/Text.h>
#include  <Xm/List.h>
#include  <Xm/SelectioB.h>
#include  <stdio.h>
#include  "em.h"
#include  "fmtpg.h"
#include  "em_p.h"

em_init

extern Widget toplevel_widget;        /* Root widget ID of application */
extern Condition c[CONDMAX];
extern MifFont f[FONTMAX];
extern TRECT tr[TRECMAX];
extern Page pg[PAGEMAX];
extern Pgf p[PGFMAX];
extern Marker m[MKRMAX];
extern Ucm ucm_dir[UCM_DIR_MAX];
extern FILE *ucm_fp;

extern initwin();
extern find_link(char *link);
extern help_resize(double x, double y);
extern int load_mif(char *file,int cpage);
extern setColor(GC theColor, int colornumber);
extern Widget widget_id(char *widget_name);
extern Widget VUIT_Manage(Widget widget_name);
extern void VUIT_Unmanage(Widget widget_name);

extern Display *display;
GC      drawing_gc;
unsigned int pixwidth, pixheight;
Pixmap  pixmap;
int pixmap_valid = 0;
Widget  drawing_widget;
Colormap theColormap;

static long cpage=4;
static long spage=1;

extern  int crntmkr;
extern  long crnttr;
extern int paraline;
extern int maxpg;
extern int marker;

#define PGWIDTH 7500
#define PGHEIGHT 10000

char *filename = "ipf_gui_help.mif";

void help_expose_callback( widget, client_data, draw_struct )
  Widget                          widget;
  caddr_t                         client_data;
  XmDrawingAreaCallbackStruct     *draw_struct;
{
  if (!pixmap_valid)
  {
    initwin();
/*    XSetForeground(display,drawing_gc, WhitePixel(display,0));
    XFillRectangle(display,pixmap,drawing_gc,0,0,
		    pixwidth,pixheight); */
  }
  XCopyArea(display,pixmap,XtWindow(drawing_widget),drawing_gc,
    draw_struct->event->xexpose.x,
    draw_struct->event->xexpose.y,
    draw_struct->event->xexpose.width,
    draw_struct->event->xexpose.height,
    draw_struct->event->xexpose.x,
    draw_struct->event->xexpose.y);
  XFlush( display );
}

void help_input_callback( widget, client_data, draw_struct )
  Widget                          widget;
  caddr_t                         client_data;
  XmDrawingAreaCallbackStruct     *draw_struct;
{
  switch( draw_struct->event->type )
  {
    case ButtonPress:
    {
      int x = draw_struct->event->xbutton.x;
      int y = draw_struct->event->xbutton.y;
      int z;
      /* printf( "Button Press   at (%d,%d)\n", x,y ); */
      for(z=0; z <= crntmkr; ++z)
      {
        if(x >= m[z].MRect.i1)
	  if(x <= m[z].MRect.i3)
	    if(y >= m[z].MRect.i4)
	      if(y <= m[z].MRect.i2)
              {
/*	        printf("found marker %s at (%d, %d) marker = %d, type = %d\n",
		  m[z].MText,x,y,z,m[z].MType); */
		if(m[z].MType == 8)
                {
                  XSetForeground(display,drawing_gc, WhitePixel(display,0));
                  XFillRectangle(display,pixmap,drawing_gc,0,0,
		    pixwidth,pixheight);
		  cpage = find_link(m[z].MText);
                  XCopyArea(display,pixmap, XtWindow(drawing_widget),drawing_gc,
                    0,0,PGWIDTH,PGHEIGHT,0,0);
                  help_resize(pg[maxpg].PageSize.i1, pg[maxpg].PageSize.i2);
                  XFlush(display);
		}
	      }
      }
      break;
    }
    case ButtonRelease:
      break;
    default:
      printf("Other event %d\n", draw_struct->event->type);
  }
}


void help_dialog_page_up(widget, client_data, cbs)
  Widget                          widget;
  caddr_t                         client_data;
  XmAnyCallbackStruct             *cbs;
{
  int stat;
  XSetForeground(display,drawing_gc, WhitePixel(display,0));
  XFillRectangle(display,pixmap,drawing_gc, 0,0,pixwidth,pixheight);

  if(--cpage < 5)
    cpage=5;

  if(spage)
  {
    stat = load_mif(filename,cpage);
    if (stat == 0) spage=0;
  }
  else
  {
    stat = load_mif(NULL,cpage);
  }

  XCopyArea(display,pixmap, XtWindow(drawing_widget),drawing_gc,
    0,0,PGWIDTH,PGHEIGHT,0,0);
  if (stat == 0) help_resize(pg[maxpg].PageSize.i1, pg[maxpg].PageSize.i2);
  XFlush(display);
}


void help_dialog_page_down(widget, client_data, cbs)
  Widget                          widget;
  caddr_t                         client_data;
  XmAnyCallbackStruct             *cbs;
{
  int stat;
  XSetForeground(display,drawing_gc, WhitePixel(display,0));
  XFillRectangle(display,pixmap,drawing_gc, 0,0,pixwidth,pixheight);

  if(++cpage > maxpg)
    cpage=maxpg;

  if(cpage < 5)
    cpage=5;

  if(spage)
  {
    stat = load_mif(filename,cpage);
    if (stat == 0) spage=0;
  }
  else
  {
    stat = load_mif(NULL,cpage);
  }

  XCopyArea(display,pixmap, XtWindow(drawing_widget),drawing_gc,
    0,0,PGWIDTH,PGHEIGHT,0,0);
  if (stat == 0) help_resize(pg[maxpg].PageSize.i1, pg[maxpg].PageSize.i2);
  XFlush( display );
}


static Widget help_widget;

void help_key_cb(widget, client_data, cbs)
  Widget                          widget;
  caddr_t                         client_data;
  XmAnyCallbackStruct             *cbs;
{
  CoreWidget core;
  core = widget;
  printf("found widget = %d,name =%s\n",help_widget, core->core.name);
}


long inchtopixx(double d)
{
  return(XmConvertUnits(drawing_widget,XmHORIZONTAL,Xm1000TH_INCHES,
    (long)(d * 1000), XmPIXELS));
}

long inchtopixy(double d)
{
  return(XmConvertUnits(drawing_widget,XmVERTICAL,Xm1000TH_INCHES,
    (long)(d * 1000), XmPIXELS));
}
typedef struct
{
  char fname[120];
  XFontStruct *fstruct;
  Font fid;
} fontlist;

static fontlist fonts[20];
static int maxfonts= -1;

ld_fnt(int fontnum)
{
  if(f[fontnum].fontid == 0)
  {
    char *weight;
    char slant;
    char fnt[100];
    int x=0;

    switch(f[fontnum].FWeight[0])
    {
      case 'B': weight= "bold"; break;
      case 'R': weight= "medium"; break;
    }

    switch(f[fontnum].FAngle[0])
    {
      case 'I': slant='i'; break;
      case 'R': slant='r'; break;
      case 'O': slant='o'; break;
    }

    sprintf(fnt,"-*-%s-%s-%c-*-*-*-%d-*-*-*-*-*-*",
      f[fontnum].FFamily,weight,slant,(long)(f[fontnum].FSize*720+.5));

    for(x=0; x < maxfonts; ++x)
    {
      if (strcmp(fnt,fonts[x].fname)==0)
      {
	f[fontnum].fontid = fonts[x].fid;
	f[fontnum].fontstruct = fonts[x].fstruct;
	return(0);
      }
    }

    strcpy(fonts[++maxfonts].fname,fnt);
    /* printf("loading font %d, %s\n",fontnum,fnt); */
    fonts[maxfonts].fstruct =f[fontnum].fontstruct =XLoadQueryFont(display,fnt);

    if(f[fontnum].fontstruct == NULL)
    {
      char str1[200];
      static int first=1;
      static XFontStruct *fstruct;
      sprintf(str1,"font=%s, not loaded substituting variable\n",fnt);

      if(first)
      {
	first=0;
        fstruct =XLoadQueryFont(display,"variable");
        err.line=EM_LINE;
        err.msg=str1;
        err.type=INFO;
        err.link="loading_fonts";
        err.ident=sccsid;
        em_show(&err);
      }
      fonts[maxfonts].fstruct = f[fontnum].fontstruct = fstruct;
    }
    fonts[maxfonts].fid = f[fontnum].fontid = (f[fontnum].fontstruct)->fid;
  }
}

long wdth,pgx,pgy;
long prevx=0, prevy=0;
showtext(char *str)
{
  int direction,ascent,descent;
  XCharStruct overall;
  ld_fnt(p[0].PgfFont);

  if(prevx == 0)
  {
    /* don't allocate for margins */
    if (paraline==0)
    {
      pgx = inchtopixx(tr[crnttr].BRect.i1 + p[0].PgfFIndent - .5);
    }
    else
    {
      pgx = inchtopixx(tr[crnttr].BRect.i1 + p[0].PgfLIndent - .5);
    }
    prevx=pgx;
  }

  if(prevy == 0)
  {
    /* don't allocate for margins */
    pgy= inchtopixy(tr[crnttr].BRect.i2 + f[p[0].PgfFont].FSize - .5);
    prevy=pgy;
  }

  if(f[p[0].PgfFont].fontid != 0)
    XSetFont(display,drawing_gc,f[p[0].PgfFont].fontid);

  if(marker==1)
  {
    setColor(drawing_gc,6);
  }
  else
  {
    XSetForeground(display,drawing_gc,BlackPixel(display,0));
  }

  wdth = XTextWidth(f[p[0].PgfFont].fontstruct, str, strlen(str));

  if(p[0].PgfAlignment == CENTER)
  {
    /* prevx is the left margin, pgx is the width  */
    pgx = inchtopixx(tr[crnttr].BRect.i3 - p[0].PgfRIndent);
    prevx += (pgx - wdth)/2;
  }

  XDrawString(display,pixmap, drawing_gc, prevx, prevy, str,strlen(str));
  prevx += wdth;

  if((marker==1) && (f[p[0].PgfFont].FAngle[0] == 'I'))
  {
    marker=0;
    m[crntmkr].MRect.i3 = prevx;
    m[crntmkr].MRect.i4 = prevy - inchtopixy(f[p[0].PgfFont].FSize);
  }
}


/* store txt in the text widget, and, if not NULL, in the selection dialog
						   list box. */
new_text(char *widget_name, char *selection_box_name, char *txt)
{
  Widget w,s,l;    /* text widget, selection dialog, list widget */

  if((w = widget_id( widget_name )) == FALSE )  return; /* exit(1); */

  XmTextSetString(w,txt);
  if (selection_box_name)
  {
    if(( s = widget_id( selection_box_name )) == FALSE ) return; /* exit(1); */
    VUIT_Manage(s);

    l = XmSelectionBoxGetChild(s,XmDIALOG_LIST);

    XmListAddItem(l,XmStringCreateLtoR(txt,XmSTRING_DEFAULT_CHARSET),1);
  }
}

void help_annotate_remove(w, tag, reason)
Widget          w;
int             *tag;
unsigned long   *reason;
{
  Widget wid;
  ucm_dir[cpage].start=0;
  ucm_dir[cpage].len  =0;
  fseek(ucm_fp,cpage * sizeof(Ucm),SEEK_SET);
  fwrite(&ucm_dir[cpage],sizeof(Ucm),1,ucm_fp);
  fflush(ucm_fp);
  if((wid = widget_id("paper_clip")) == FALSE ) return;
  VUIT_Unmanage(wid);
}
void help_annotate_save(w, tag, reason)
Widget          w;
int             *tag;
unsigned long   *reason;
{
  char *text;
  Widget widget = widget_id("help_annotate_text");
  Widget wid;
  fseek(ucm_fp,0L,SEEK_END);
  printf("sizeof(ucm_dir)=%d, ftell(ucm_fp)=%d, sizeof(Ucm)=%d\n",
    sizeof(ucm_dir),ftell(ucm_fp),sizeof(Ucm));
  ucm_dir[cpage].start = (ftell(ucm_fp) < sizeof(ucm_dir)) ?
    sizeof(ucm_dir) : ftell(ucm_fp);
  fseek(ucm_fp,ucm_dir[cpage].start,SEEK_SET);
  text = XmTextGetString(widget);
  ucm_dir[cpage].len = strlen(text);
  fwrite(text,ucm_dir[cpage].len,1,ucm_fp);
  fseek(ucm_fp,cpage * sizeof(Ucm),SEEK_SET);
  fwrite(&ucm_dir[cpage],sizeof(Ucm),1,ucm_fp);
  fflush(ucm_fp);
  XtFree(text);
  if((wid = widget_id("paper_clip")) == FALSE ) return;
  VUIT_Manage(wid);
}
void help_annotate_get(w, tag, reason)
Widget          w;
int             *tag;
unsigned long   *reason;
{
  char *text =XtMalloc(ucm_dir[cpage].len+2);
  Widget widget = widget_id("help_annotate_text");
  fseek(ucm_fp,ucm_dir[cpage].start,SEEK_SET);
  fread(text,ucm_dir[cpage].len,1,ucm_fp);
  text[ucm_dir[cpage].len] = '\0';
  XmTextSetString(widget,text);
  XtFree(text);
}

