/* static char SCCSIDbranchph[] = "@(#)BranchP.h	20.1 5/2/94"; */
/*********************************************************************
*
* module: BranchP.h
* purpose: define the Branch Gadget private header 
*
*
**********************************************************************
*/
#ifndef BRANCHP_H
#define BRANCHP_H

#include "Branch.h"  
/** use pushbutton as parent class for new object **/
#include <Xm/XmP.h>
#include <X11/ObjectP.h>
#include <X11/RectObjP.h> 
#include <Xm/LabelGP.h> 
#include <Xm/PushBGP.h> 
#include <Xm/ExtObjectP.h>


/*** define constants for gadget	***/
/* no constants */

/****
 *
 * Additional instance fields for widgets of (sub)class 'Branch' 
 * define the fields (layout) of the instance record.
 *
 ****/

typedef struct _BranchPart { 
  /* resources */
  XSegment seg;
  int	x1;
  int	y1;
  int	x2;
  int	y2;
/***
***/
  
  /* private state */
  int	orientation;
  GC	drawGC;

} BranchPart;

/*** 
 * define the template for an instance record 
 * which XtCreateWidget() allocates dynamically
 **/
typedef struct _BranchRec {
   ObjectPart              object;
   RectObjPart             rectangle;
   XmGadgetPart            gadget;
   XmLabelGadgetPart       label;
   XmPushButtonGadgetPart	pushbutton;
   BranchPart 	branch;
} BranchRec;

/*****
 *
 *  Additional class fields for widgets of (sub)class 'Branch'
 *  most widget classes provide no new fields in class part
 *  since the Core class already provides all the class fields
 *  that Xt knows what to do with.
 *  (see X Toolkit Intrinsics Programming Manual pg. 138) 
 *
 *****/

typedef struct _BranchClassPart { 
  int Branchdummyclasspart;
  } BranchClassPart,*BranchPartPtr; 

/*****
 *  Define the class hierarchy ( the inheritance path) in the class
 *  record for the widget.
 ****/
typedef struct _BranchClassRec {
   RectObjClassPart             rect_class;
   XmGadgetClassPart            gadget_class;
   XmLabelGadgetClassPart       label_class;
   XmPushButtonGadgetClassPart      pushbutton_class;
   BranchClassPart 	branch_class;
   } BranchClassRec;

/*** 
 * Provide a public declaration for the class record
 * note that this declaration corresponds to the class record 
 * definition that appears in the implemenation file.  Each
 * widget class must provide this public declaration so that subclasses
 * can reference the superclass record.
 * (see Object Oriented Programming with the X Window System Toolkits
 * by Jerry Smith - pg 196)
 ***/
/* externalref BranchClassRec branchClassRec; */
/* WidgetClass branchWidgetClass = (WidgetClass) &branchClassRec; */
/* extern BranchClassRec branchClassRec;  */

/* DON'T ADD STUFF AFTER THIS #endif */
#endif /* BRANCHP_H */
