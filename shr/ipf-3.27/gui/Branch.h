static char SCCSIDbranchh[] = "@(#)Branch.h	20.1 5/2/94";
/*****************************************************************************
*
* module: Branch.h
* purpose: define the Branch Gadget public header 
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
#ifndef BRANCH_H
#define BRANCH_H

/** Gadgets public header file includes the superclass's public header **/
#include <Xm/Xm.h>
#include <X11/Object.h>
#include <X11/RectObj.h>
#include <Xm/LabelG.h> 
#include <Xm/PushBG.h> 

/*** Define the resource identifiers for this widget (Branch vs Xi) ***/
#define XiNx1	"x1"
#define XiNy1	"y1"
#define XiNx2	"x2"
#define XiNy2	"y2"

#define XiCX1	"X1"
#define XiCY1	"Y1"
#define XiCX2	"X2"
#define XiCY2	"Y2"

#define XiDefaultX1		"0"
#define XiDefaultY1		"0"
#define XiDefaultX2		"0"
#define XiDefaultY2		"0"

/*** Define data types for (pointers to) the class and instance records ***/

typedef struct _BranchRec *BranchGadget; 

typedef struct _BranchClassRec *BranchGadgetClass;

/*** Define the widget class identifier (pointer) for calls to create ***/
/* externalref WidgetClass branchGadgetClass; */
/***
extern XmGadgetClass branchGadgetClass;
***/
extern WidgetClass branchGadgetClass;

/*** List of function declarations that define the public interface ***/
/***  each followed by a the function parameters as comments	***/
/* extern void BranchPopup(); */

/*** never used ***/
/*   BranchGadget local_branchwidget_ptr; */
/*   BranchGadget branchwidget; */

/* DON'T ADD STUFF AFTER THIS #endif */
#endif 
