static char sccsid[]="@(#)line2rect.c	20.4 2/15/95";
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
* purpose: 
* -> given the coordinates for the corners of a line
* (x1,y1) (x2,y2) 
* <- determine the gadget parameters for defining the bounding
* box for the drawing area for the line.
* x, y, width, height, slope 
* 
* also do some error checking on the data passed 
* 1) end points of the line cannot be inside the border of the
* drawing area. 
* (coord >= border) where coord is element of {x1,y1,x2,y2}
* 2) the corners of the bounding box cannot be above or to the 
* left of the drawing area
* (position > 0) where position is element of {x, y, x1, y1, x2, y2}
* 3) width and height must be above the minimum window size
* (dimension > mindimension) where dimension is element of {width, height}
*
* Author: Dan Clark	Modified by B.Rogers 11/16/93
*
* Called by: drawline  , updateEdge (edge.c)
*	     edgeupdate (toolmove.c)
*	     switchEdge (graphdata.c)
\**************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "line2rect.h"
#define max(A,B) ((A) > (B) ? (A) : (B))

void border_ck( int *i, char *var_nam );

line2rect(EdgePoint *pedgept, EdgeBox *pedgebox)
{
  int xoffset = 0;
  int yoffset = 0;

  /* assure pts are not near zero edge (left and top) of screen */
  border_ck( &pedgept->x1, "x1" );
  border_ck( &pedgept->y1, "y1" );
  border_ck( &pedgept->x2, "x2" );
  border_ck( &pedgept->y2, "y2" );

  if(((pedgept->x1 < pedgept->x2) && (pedgept->y1 < pedgept->y2)) ||
        ((pedgept->x1 >= pedgept->x2) && (pedgept->y1 >= pedgept->y2)) )
    pedgebox->slope = SlopeNeg;
  else
    pedgebox->slope = SlopePos;
      
/*************************************************************************\
*	 now make a rough setting for the edge of the box that dictates
\*       the edges of the refreshed drawing area **/

  if( pedgept->x1 < pedgept->x2 )
    pedgebox->x = (Position)pedgept->x1 - xoffset;
  else
    pedgebox->x = (Position)pedgept->x2 - xoffset;

  if( pedgept->y1 < pedgept->y2 )
    pedgebox->y = (Position)pedgept->y1 - yoffset;
  else
    pedgebox->y = (Position)pedgept->y2 - yoffset;

  /* tune up the setting of x and y with a buffer area */
  pedgebox->x = max(pedgebox->x - MAXLINETHICKNESS, 0);
  pedgebox->y = max(pedgebox->y - MAXLINETHICKNESS, 0);

/*************** set the width of height of bounding box ***************/
  pedgebox->width = (Dimension)abs(pedgept->x1 - pedgept->x2) + 2 * xoffset;
  pedgebox->width = pedgebox->width + (Dimension)(MAXLINETHICKNESS * 2);

  if(pedgebox->width < (Dimension)MINDIMENSION)
  {
    pedgebox->width = (Dimension)MINDIMENSION;
    if(pedgebox->x > (Position)MINDIMENSION/2) {
          pedgebox->x = pedgebox->x - (Position)MINDIMENSION/2;
    }
  }

  pedgebox->height = (Dimension)abs(pedgept->y1 - pedgept->y2) + 2 * yoffset;
  pedgebox->height = pedgebox->height + (Dimension)(MAXLINETHICKNESS * 2);

  if(pedgebox->height < (Dimension)MINDIMENSION)
  {
    pedgebox->height = (Dimension)MINDIMENSION;
    if(pedgebox->y > (Position)MINDIMENSION/2) {
        pedgebox->y = pedgebox->y - (Position)MINDIMENSION/2;
    }
  }

#ifdef DEBUG
    printf("line2rect: (%dx%d) (%dx%d) dx (%dx%d)", 
             pedgept->x1, pedgept->y1,
             pedgept->x2, pedgept->y2,
             pedgept->x1 - pedgept->x2, pedgept->y1 - pedgept->y2);
    printf("(%dx%d) (%dx%d)\n", 
             pedgebox->x, pedgebox->y,
             pedgebox->width, pedgebox->height);
#endif
    return;
}

void border_ck( int *i, char *var_nam )
/************************************************************************/
{
  if( *i < DrawBorder)
  {
    printf("Programmer error - from line2rect.c\n" );
  printf("  %s %d pt < %d pixels from zero (left & top) edge of MOTIF screen\n",
		var_nam, *i, DrawBorder );
    printf(" Calling program should check values before calling line2rect!\n" );
      *i = DrawBorder; 
    printf(" Value changed to %d\n", *i );
  }
}
