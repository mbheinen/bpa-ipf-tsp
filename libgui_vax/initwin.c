static char sccsid[]="@(#)initwin.c	20.3 2/15/95";
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

#include  <stdio.h>
#include  <Xm/Xm.h>
#include  "fmtpg.h"
#include  "em_p.h"

extern Display *display;
extern GC       drawing_gc;
extern Pixmap pixmap;
extern int pixmap_valid;
extern Colormap theColormap;
extern Widget drawing_widget;
extern unsigned int pixwidth, pixheight;
extern int HashLookup(char *, Widget *);

#define PGWIDTH 7500
#define PGHEIGHT 10000
static char *filename = "ipf_gui_help.mif";

extern void help_expose_callback( Widget, caddr_t, XmDrawingAreaCallbackStruct);
extern void help_input_callback ( Widget, caddr_t, XmDrawingAreaCallbackStruct);
extern void initDefaultColors	( );
extern int load_mif		( char *file, int cpage );
extern void exit		( );

static int first=1;

initwin()
{
  int screen;
  Window window;

  if (!first) return;
  first=0;
  if(HashLookup("help_dialog_draw_area", &drawing_widget) == 0)
  {
    printf("drawing widget not found = %d\n", drawing_widget);
    exit(1);
  }
/* XtAddCallback( drawing_widget, XmNinputCallback, help_input_callback, NULL );
  XtAddCallback( drawing_widget, XmNexposeCallback,help_expose_callback,NULL );
 */

  display    = XtDisplay( drawing_widget );
  window     = XtWindow(  drawing_widget );
  screen     = DefaultScreen(  display );
  theColormap   = DefaultColormap( display, screen );

  pixwidth = XmConvertUnits(drawing_widget,XmHORIZONTAL,Xm1000TH_INCHES,
    PGWIDTH,XmPIXELS);
  pixheight= XmConvertUnits(drawing_widget,XmVERTICAL,Xm1000TH_INCHES,
    PGHEIGHT,XmPIXELS);
  /* printf("pixmap size = %d, %d, @ %d\n",pixwidth,pixheight,
    DefaultDepth(display,screen)); */
  pixmap = XCreatePixmap(display,window, pixwidth,pixheight,
    DefaultDepth(display,screen));

  drawing_gc = XCreateGC( display, window, 0L, (XGCValues *) NULL );
  initDefaultColors();
  pixmap_valid = 1;

  XSetForeground(display,drawing_gc, WhitePixel(display,0));
  XFillRectangle(display,pixmap,drawing_gc,0,0,
    pixwidth,pixheight);
  load_mif(filename,5);

  XCopyArea(display,pixmap, XtWindow(drawing_widget),drawing_gc,
    0,0,PGWIDTH,PGHEIGHT,0,0);
  XFlush(display);
}

help_resize(double x,double y) {
  XtVaSetValues(drawing_widget, XmNwidth, (long)(x * 1000),
				XmNheight,(long)(y * 1000),
				NULL);
}
