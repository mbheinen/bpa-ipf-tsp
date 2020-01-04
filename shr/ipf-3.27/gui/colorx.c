static char sccsid[]="@(#)colorx.c	20.4 2/15/95";
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
#include   <Xm/Xm.h>

extern Display  *display;
extern Colormap theColormap;

#define maxPixels 7

unsigned long thePixels[ maxPixels ];
char *theColorNames[ maxPixels ] = {
   "aquamarine",      /* -- 0   */
   "black",           /* -- 1   */
   "blue",            /* -- 2   */
   "green",           /* -- 3   */
   "Navy",           /* -- 4   */
   "orange",         /* -- 5   */
   "red"             /* -- 6   */   
};
initDefaultColors() {
   unsigned long theBlackPixel;
   XColor theRGBColor, theHardwareColor;
   int theStatus;
   int i;

   theBlackPixel = 0L;
   for(i = 0; i < maxPixels; i++) {
      theStatus = XLookupColor(display,theColormap,theColorNames[i],   
            &theRGBColor, &theHardwareColor );
      if ( theStatus != 0 ) {
         theStatus=XAllocColor(display,theColormap,&theHardwareColor);
         if ( theStatus != 0 ) thePixels[i] = theHardwareColor.pixel;
         else thePixels[i] =  theBlackPixel;
      }
   }
}

setColor(theGC, colorNumber) GC theGC; int colorNumber; {
   if ((colorNumber < maxPixels) && (colorNumber >= 0)) {
      XSetForeground(display, theGC, thePixels[colorNumber]);
   }
}
