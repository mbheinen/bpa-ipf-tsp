static char sccsid[]="@(#)drawchar.c	1.10 4/6/98";
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
/****************************************************************************\
* author: BER
* purpose: draw those characters at any size and any angle
*
\****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <Xm/Xm.h>
#include <Xm/LabelG.h>

#include "EdgeGP.h"
#include "dmgr.h"
#include "graph_data.h"
#include "ipfdebug.h"

#define RadtoDeg 57.29578

static GC xorGC; 
static GC xGC;  

static min_max_flg;
static int maxx, minx, maxy, miny;
Boolean drawchar_not_initialized = TRUE;
double scr, ssr;

extern double zoom_factor;      /* from (toolbox.c) */

extern void drawAreaButtonCB	( Widget w, GraphElement *client_data,
		                  XmPushButtonCallbackStruct *call_data);
extern void lookup_and_get_field( char *fld_name, char *ret_string, int size );
extern int roundoff		( double fnum );
extern Widget widget_id		( char * );

void drawChars	( Display *dpy, Window wid, char *string, int x, int y,
			 double size, double angle );
void drawCharsCenteredAboveLine( Display *dpy, Window wid, char *string,
		 int x1, int y1, int x2, int y2, double size );
int findCharID		( char c );
void drawletter		( Display *, Window w, double x, double y, char c,
				double sixe );
void testdraw		();
void set_min_max	( int x1, int y1, int x2, int y2 );
void setup_chars	();


void drawCharsCenteredAboveLine( Display *dpy, Window wid, char *string,
		 int x1, int y1, int x2, int y2, double size )
/*****************************************************************************\
*
\*****************************************************************************/
{
  double l = (double)0.0;
  double x = (double)0.0;
  double y = (double)0.0;
  double angle = (double)0.0;
  int xi = 0;
  int yi = 0;

  if( drawchar_not_initialized )  setup_chars();


  if( x1 <= x2 )
  {
    angle = atan2( y2 - y1,
                   x2 - x1 );
  }
  else
  {
    angle = atan2( y1 - y2,
                   x1 - x2 );
  }
 
  angle *= -RadtoDeg;
 
  x = (double)(x1+x2)/2.0;	/* find mid-point */
  y = (double)(y1+y2)/2.0;
  
  l = strlen( string )*6/2;

  scr = size*cos(angle/RadtoDeg)*zoom_factor;
  ssr = size*sin(angle/RadtoDeg)*zoom_factor;
  x -= l*scr;	/* shift back */
  y += l*ssr;

  x = (x - 4*ssr);	/* shift above line */
  y = (y - 4*scr);

  xi = (int)x;
  yi = (int)y;

  maxx=0;
  minx=0;
  maxy=0;
  miny=0;

/*   printf(" ptf (%lf,%lf) pti (%d,%d) angle %lf\n", x,y, xi,yi,angle); */
  drawChars( dpy, wid, string, xi, yi, size, angle );

}


void drawChars( Display *dpy, Window wid, char *string, int x, int y,
			 double size, double angle )
/*****************************************************************************\
*	Purpose: draw string of characters - any size, any angle
*
*	Author: Wm E. Rogers	Jan 1995
*
*	Called by: 
\******************************************************************************/
{
  int len;
  int i;
  double xp, yp, org_x, org_y;
  int xti, yti;

  if( drawchar_not_initialized )  setup_chars();

  xp = (double)(x);
  yp = (double)(y);
  org_x = xp;
  org_y = yp;

  scr = size*cos(angle/RadtoDeg)*zoom_factor;
  ssr = size*sin(angle/RadtoDeg)*zoom_factor;

  len = strlen( string );
  for ( i=0; i<len; i++ )
  {
    if( i==0 || i==len-1 || string[i+1]=='\n' )
    {
      min_max_flg = 1;
    }
    else
      min_max_flg = 0;
 
    if( string[i]=='\n' )	/* start new line */
    {
      min_max_flg = 1;
      org_x += 8*ssr;
      org_y += 8*scr;
      xp = org_x;
      yp = org_y;
    }

    drawletter( dpy, wid, xp, yp, string[i], size );
    xp += 6*scr;
    yp -= 6*ssr;
  }

}


void drawletter( Display *dpy, Window w,
			 double xf, double yf, char c, double size )
/*****************************************************************************\
*
\*****************************************************************************/
{
  int i=0;
  double xt, yt;
  int xti,yti,xfi,yfi;
  int id;
  int nt, ofst;

  xt = xf;
  yt = yf;
  i = 0;

  if( (id = findCharID( c )) <0 ) return;

  while ( mypts[id].x[i] != 99 )
  {
    if ( i >= MAXPT )
    {  id++;
       i = 0;
    }

    if ( abs( mypts[id].x[i]) < 100 )
    {
      xf = xt;			/* from here */
      yf = yt;
      xt = xt + (double)(mypts[id].x[i]*scr) + (double)(mypts[id].y[i]*ssr); /* to here */
      yt = yt + (double)(mypts[id].y[i]*scr) - (double)(mypts[id].x[i]*ssr);
      xti = roundoff(xt);
      yti = roundoff(yt);
      xfi = roundoff(xf);
      yfi = roundoff(yf);
      XDrawLine( dpy, w, xGC, xfi, yfi, xti, yti );

      if( min_max_flg ) set_min_max( xfi, yfi, xti, yti );

      nt = (int)(size*zoom_factor*2.4);
    
      for( ofst = 1; ofst< nt; ofst++ )
      {
        xti = roundoff(xt+ofst*scr/nt);
        yti = roundoff(yt-ofst*ssr/nt);
        xfi = roundoff(xf+ofst*scr/nt);
        yfi = roundoff(yf-ofst*ssr/nt);
        XDrawLine( dpy, w, xGC, xfi, yfi, xti, yti );

        if( min_max_flg ) set_min_max( xfi, yfi, xti, yti );
      }
    }
    else
    {				/* advance relative - (no draw) */
      if( mypts[id].x[i] < 0 )
      {
        xt = xt + (mypts[id].x[i]+100)*scr + mypts[id].y[i]*ssr;
        yt = yt + mypts[id].y[i]*scr - (mypts[id].x[i]+100)*ssr;
      }
      else
      {
        xt = xt + (mypts[id].x[i]-100)*scr + mypts[id].y[i]*ssr;
        yt = yt + mypts[id].y[i]*scr - (mypts[id].x[i]-100)*ssr;
      }
    }
    i++;
  }

}

int findCharID( char c )
{
  int i;

  for ( i = 0; i<C_LMT; i++ )
  {
    if ( c == mypts[i].symbol[0] ) return i;
  }

  return -1;
}

void set_min_max( int x1, int y1, int x2, int y2 )
{
   if( x1 < minx ) minx = x1;
   if( x2 < minx ) minx = x2;
   if( x1 > maxx ) maxx = x1;
   if( x2 > maxx ) maxx = x2;

   if( y1 < miny ) miny = y1;
   if( y2 < miny ) miny = y2;
   if( y1 > maxy ) maxy = y1;
   if( y2 > maxy ) maxy = y2;
}


void testdraw()
/*****************************************************************************\
*
\*****************************************************************************/
{
Widget draw_id;
Window wid;
Display *dpy;
 
  draw_id   = widget_id("drawwindow");
  dpy = XtDisplay( draw_id );
  wid = XtWindow ( draw_id );

 drawChars( dpy, wid, " 1234567890.()[]/{}\nABCDEFGHIJKLMNOPQRSTUVWXYZ!",
	 60, 60, 3.0,   0.0 );
 drawChars( dpy, wid, " 1234567890\n.()[]/", 60, 160, 2.0, -20.0 );
/* drawChars( dpy, wid, " 1234567890.()[]/", 60, 60, 2.0,  20.0 );
 drawChars( dpy, wid, " 1234567890.()[]/", 60, 60, 1.5, -45.0 );
 drawChars( dpy, wid, " 1234567890.()[]/", 60, 60, 1.8,  45.0 );
 drawChars( dpy, wid, " 123456\n7890.()[]/", 60, 60, 5.0, -90.0 );
 drawChars( dpy, wid, " 1234567890.()[]/", 60, 60, 3.0,  90.0 );
 drawChars( dpy, wid, " 1234567890.()[]/", 60, 60, 1.0, -135.0 );
 drawChars( dpy, wid, " 1234567890.()[]/", 60, 60, 1.0,  135.0 );*/
}


void setup_chars()
{

Widget draw_id;
XGCValues xgcv;
int fg, bg;

  draw_id   = widget_id("drawwindow");

  XtVaGetValues( draw_id,
      XmNforeground, &fg,
      XmNbackground, &bg,
      NULL);

  xgcv.foreground = fg;
  xgcv.background = bg;
  xGC = XtGetGC(draw_id, GCForeground | GCBackground, &xgcv);

  xgcv.foreground = fg ^ bg;
  xgcv.background = bg;
  xgcv.function = GXxor;

  xorGC = XtGetGC(draw_id, GCForeground | GCBackground | GCFunction, &xgcv);

  drawchar_not_initialized = FALSE;

/*  testdraw(); */
}

