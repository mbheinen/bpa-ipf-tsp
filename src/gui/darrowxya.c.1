static char sccsid[]="@(#)darrowxya.c	20.2 2/15/95";
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
*	Function: DRAW ARROW XYA
*	 	  Draw a line from x,y at angle on "drawing area"
*		  Remember, trick is to draw arrow without xor-ing out
*	          the point or the line its drawn on.
*
*	Passed Params:	x:  x-coord on drawing area
*			y:  y   "
*                   angle:  direction arrow points in degrees (right=0, up=90)
*
*	Author: Bill Rogers	July 22, 1991	Modified: Oct 28, 1991
*
*	Called by: drawLineArrow, dropSymbol, move_line_symbol,
*		   processButtonPress, removeLineSymbols, 
*		   removeSegSymbolsIfHere
*
*	Calls:	getAngleDistancePosition
*
\*****************************************************************************/
#include "coord.h"
#include "symbols_specs.h"

void 
drawArrowXYA ( int x, int y, int angle )

{
  int x2, y2, len, w, ang, gb;
  struct { short x, y; } point[4];

  if ( x!=NONE && x!=ERASED && xyCheck( x, y )!=1 ) 
  {
    gb = checkGrabBox ( x, y );

    if ( gb==NONE OR GBOX_TYPE( gb )!=BUS )
    {
      len = (int)( ARROW_LENGTH*scale );
      x   = screenX( x );
      y   = screenY( y );
      w   = (int)(scale);
      if ( w<1 ) w = 1;

      XSetLineAttributes ( dpy, xorGC, scaledLineWidth(),
                           LineSolid, CapProjecting, JoinMiter );

/*    DRAW LEFT SIDE OF ARROW  ( when arrow points up ) **********************/

/*    Shift to the side of the line first.                                   */
      getAngleDistancePosition ( x, y, angle+90, w, &x2, &y2 );  /* calc pos */

/*    Move down three pixel so point will be in correct place                */
      getAngleDistancePosition ( x2, y2, angle, w*3, &x2, &y2 );
      point[1].x = (short)(x2);
      point[1].y = (short)(y2);

/*    Define the "stub"  which causes the JoinMiter to draw a sharp point    */
      getAngleDistancePosition ( x2, y2, angle, w*3, &x2, &y2);
      point[0].x = (short)(x2);
      point[0].y = (short)(y2);

/*    Define the long outside edge of the arrow                              */
      ang = angle + ARROW_H_WEDGE_ANGLE;
      getAngleDistancePosition ( point[1].x, point[1].y, ang, len, &x2, &y2 );
      point[2].x = (short)(x2);
      point[2].y = (short)(y2);
 
      XDrawLines ( dpy, window, xorGC, point, 3, 0 );
 
/*******  REPEAT ABOVE CODE TO DRAW OTHER SIDE OF ARROW  ********************/

      getAngleDistancePosition ( x,  y,  angle-90, w,   &x2, &y2 );
      getAngleDistancePosition ( x2, y2, angle,    w*3, &x2, &y2 );
      point[1].x = (short)(x2);
      point[1].y = (short)(y2);

      getAngleDistancePosition ( x2, y2, angle, w*3, &x2, &y2);
      point[0].x = (short)(x2);
      point[0].y = (short)(y2);

      ang = angle - ARROW_H_WEDGE_ANGLE;
      getAngleDistancePosition ( point[1].x, point[1].y, ang, len, &x2, &y2 );
      point[2].x = (short)(x2);
      point[2].y = (short)(y2);

      XDrawLines ( dpy, window, xorGC, point, 3, 0 );


/*****Reset to orginal line join style to prevent problems in other routines */

      XSetLineAttributes ( dpy, xorGC, scaledLineWidth(),
                           LineSolid, CapNotLast, JoinBevel );
    } 
  }
}
