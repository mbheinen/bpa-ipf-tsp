static char SCCSIDedgeh[] = "@(#)EdgeG.h	5.2 8/12/93";
/*****************************************************************************
*
* module: EdgeG.h
* purpose: define the Edge Gadget public header 
*
* Name             Class            Data Type       Default         Modify
* ---------------  ---------------  --------------- --------------- ----------
* XiNsymbolDisplay XiCsymbolDisplay int             1               OK
*
* note: The resouces which can not be modified are only specified during
*  the startup/configuration operations.
*
*****************************************************************************
*/
#ifndef EDGEG_H
#define EDGEG_H

/** Gadgets public header file includes the superclass's public header **/
#include <Xm/Xm.h>
/* #include <X11/Object.h> */
/* #include <X11/RectObj.h> */

/*** Define the resource identifiers for this widget (Edge vs Xi) ***/
#define XiNx1			"x1"
#define XiNy1			"y1"
#define XiNx2			"x2"
#define XiNy2			"y2"
#define XiNzoomFactor		"zoomFactor"
#define XiNdashOn		"dashOn"
#define XiNboxOn		"boxOn"
#define XiNsymbolSegment	"symbolSegment"
#define XiNarrowOn		"arrowOn"
#define XiNarrowDirection	"arrowDirection"
#define XiNtransformerOn	"transformerOn"
#define XiNlineThickness	"lineThickness"
#define XiNdrawPixel		"drawPixel"
#define XiNdrawPixmap		"drawPixmap"
#define XiNdrawCallback 	"drawCallback"
#define XiNinputCallback 	"inputCallback"
#define XiNlabelType		"labelType"
#define XiNlabelOn		"labelOn"
#define XiNlabelString		"labelString"
#define XiNlabelFontList	"labelFontList"

#define XiCX1			"X1"
#define XiCY1			"Y1"
#define XiCX2			"X2"
#define XiCY2			"Y2"
#define XiCZoomFactor		"ZoomFactor"
#define XiCDashOn		"DashOn"
#define XiCBoxOn		"BoxOn"
#define XiCSymbolSegment	"SymbolSegment"
#define XiCArrowOn		"ArrowOn"
#define XiCArrowDirection	"ArrowDirection"
#define XiCTransformerOn	"TransformerOn"
#define XiCLineThickness	"LineThickness"
#define XiCDrawPixel		"DrawPixel"
#define XiCDrawPixmap		"DrawPixmap"
#define XiCDrawCallback		"DrawCallback"
#define XiCInputCallback	"InputCallback"
#define XiCLabelOn		"LabelOn"
#define XiCLabelType		"LabelType"
#define XiCLabelString		"LabelString"
#define XiCLabelFontList	"LabelFontList"

#define XiDefaultX1		0
#define XiDefaultY1		0
#define XiDefaultX2		0
#define XiDefaultY2		0
#define XiDefaultZoomFactor		100
#define XiDefaultLineThickness	2
#define XiDefaultDrawPixel	0
#define XiDefaultDrawPixmap	0
#define XiDefaultDrawCallback	NULL
#define XiDefaultInputCallback	NULL
#define XiDefaultLabelString	NULL
#define XiDefaultLabelFontList	NULL
#define XiDefaultLabelFontString	"-*-Menu-Medium-R-Normal--*-120-*-*-P-*-ISO8859-1"

/*** Define data types for (pointers to) the class and instance records ***/

typedef struct _EdgeRec *EdgeGadget; 

typedef struct _EdgeClassRec *EdgeGadgetClass;

/*** Define the widget class identifier (pointer) for calls to create ***/
externalref WidgetClass edgeGadgetClass; 
/*** extern XmGadgetClass edgeGadgetClass; ***/
/* extern WidgetClass edgeGadgetClass; */

/*** List of function declarations that define the public interface ***/
/***  each followed by a the function parameters as comments	***/
/* extern void EdgePopup(); */

typedef struct
{
    int     reason;
    XEvent  *event;
    Window  window;
    int     click_count;
    Region  region;
} XiEdgeCallbackStruct;

/* DON'T ADD STUFF AFTER THIS #endif */
#endif 
