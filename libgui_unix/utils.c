static char sccsid[]="@(#)utils.c	20.16 3/15/96";
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
*	Utility routines.
*		get_file_name_from
*		widget_id
*
\*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/MessageB.h>
#ifdef NT
#include "ipfcursors_nt.h"
#else
#include "ipfcursors.h"
#endif

#define ERRMSGSIZE 4096
#include "em.h"
em_init                 /* special initialization for error message box */
                        /* must be in every file using this.            */

extern Widget reparent(char *widget_name, Widget parent);
extern int HashLookup	  ( char *name, Widget *id);
extern void HashErrorMsg  ( char *widget_name );
extern Widget VUIT_Manage ( char *widget_name );
extern void VUIT_Unmanage ( char *widget_name );


Widget widgit_id		( char *widget_name );
void lookup_and_reduce_text_field( char *text_name, int size );
void reduce_text_field		( Widget, int );
void clip_outside_blanks	( char * );
void clip_trailing_blanks	( char * );
char *getValueByName		( char * );
char *getValueOfWidget		( Widget );

/*****************************************************************************\
*       Function: widget_id
*                 calls Hashlookup with error check and message if
*                 id is not found.
*
*       Author: B Rogers        Jul 08, 1992    Modified: Jul 09, 1992
*
\*****************************************************************************/

Widget widget_id(char *widget_name)
{
  Widget id;
  char out_buffer[ERRMSGSIZE];


  if((HashLookup(widget_name, &id)) == False)
  {
      sprintf(out_buffer,"*** Programmer error ***\n\
  Widget [%s]\n\
  Not in hash lookup tables.\n\
  Check spelling or check VUIT register_name call.\n\
  Check that old .UID file was deleted before a make.\n\
  Check that widget was MANAGED before this call.\n\
  If widget is ATTACHED to another widget, check that other widget has been MANAGED.\n", widget_name);
      err.msg = out_buffer;
      err.type = WARNING;
      em_show(&err);
      printf("%s", out_buffer);
      HashErrorMsg( widget_name );
      return(False);
   }
  return(id);
}


/***
* manage2parent
* author: Dan Clark
* purpose: optimize performance of GUI by simplifying the heirarchy
* of windows in the UIL definition of the program and only
* managing each sub-window into a form when it is needed.
* input:
*   action : "M" to manage a widget 
*            "U" to unmanage a widget
*   wname :  text name of the widget as defined in UIL
*   wparent : text name of parent widget that the widget should
*             be managed under (does not need to be the same as
*             the parent defined in the UIL hierarchy.
*****/
int
manage2parent(char *action, char *wname, char *wparent)
{
  Widget wid;
  Widget wpid;

  /** check for an unmanage but only unmanage widgets put up before **/
  if(action[0] == 'U' ) {
    if((HashLookup(wname, &wid)) != False) {
      XtUnmanageChild( wid );
    }
    return;
  }

  /* otherwise process a manage request */

  /* allow X to manage previously defined windows */
  if((HashLookup(wname, &wid)) != False) {
    XtManageChild( wid );
    return;
  }

  /*  check to make sure the foreground form is around */
    if((HashLookup(wparent, &wpid)) == False) {
      wpid = VUIT_Manage(wparent);
      if(wpid == FALSE ) {
        printf("Failed to manage %s\n", wparent );
        return;
      }
    }
  /* get the requested widget and manage (to the background form) */
  reparent( wname, wpid);
  /* locate the widget background form and reparent widget */
  wid = widget_id( wname );
  if( wid == NULL) {
    printf("Can't find widget name %s\n", wname);
    return;
  }
}




/*****************************************************************************\
*
*  Vuit_MU
*
*       Purpose: Unmanage/Manage widget with proper checks
*		 A way to ensure a more meaningful message than that
*		 old "XToolkit Warning: null child passed to XtManageChildren
*
*       Author: B Rogers        Oct 23, 1992    Modified: 
*       mod: d clark do not report errors for a non-existant widget on unmanage
*
\*****************************************************************************/

int Vuit_MU(char *action, char *widget_name)
{
  Widget id;
  char out_buffer[ERRMSGSIZE];

  switch (action[0]) 
  {
    case 'M':
      id =VUIT_Manage(widget_name);
    break;

    case 'U':
      /* silently ignore any widget not yet instantiated when unmanaging it */
      if(HashLookup(widget_name, &id) == True) {
        VUIT_Unmanage(widget_name);
      } else {
        return(False);
      }
    break;

    default:
      printf("**** WARNING **** bad parameter in call to Vuit_MU\n" );
      printf("     first character must be 'M' or 'U'\n" );
      printf("     Param was (%s) on (%s)\n", action, widget_name );
      return(False);
   }

   return(True);
}


/****************************************************************************\
*
*
\****************************************************************************/

char *cstring_to_string(XmString motif_string)
{
   XmStringContext	context;
   char			*text;
   XmStringCharSet	charset;
   XmStringDirection	dir;
   Boolean		separator;

  XmStringInitContext(&context, motif_string);
  XmStringGetNextSegment(context, &text, &charset, &dir, &separator);
  XmStringFreeContext(context);
  return(text);
}


/****************************************************************************\
*
*
\****************************************************************************/

char *get_file_name_from(XmFileSelectionBoxCallbackStruct *callback_struct)
{
   XmString		motif_string;
   char			*text;

  motif_string = callback_struct->value;
  text = cstring_to_string(motif_string);
/*
  XmStringFree(motif_string);
*/
  return(text);
}


/*****************************************************************************\
*     Function:	Shortens code to change_cursor_to routine.
*
*     Called by:   change_cursor_to (below)
\*****************************************************************************/
void change_cursor(char *cursor_fg,
                   char *cursor_bg,
                   char *fg_color_name,
                   char *bg_color_name,
                   int hotx,
                   int hoty)
{
  Pixmap	background_bitmap;
  Pixmap	foreground_bitmap;
  Colormap	color_map;
  XColor 	forecolor, backcolor;
  XColor 	xforecolor, xbackcolor;
  Cursor	the_cursor;
  Widget	w;

  w = widget_id("drawwindow");

  color_map = XDefaultColormapOfScreen(XtScreen(w));

  XLookupColor
  (
    XtDisplay(w),
    color_map,
    fg_color_name,
    &xforecolor,
    &forecolor
  );

  XLookupColor
  (
    XtDisplay(w),
    color_map,
    bg_color_name,
    &xbackcolor,
    &backcolor
  );

  background_bitmap = XCreateBitmapFromData
  (
    XtDisplay(w),
    XtWindow(w),
    cursor_bg,
    16,
    16
  );

  foreground_bitmap = XCreateBitmapFromData
  (
    XtDisplay(w),
    XtWindow(w),
    cursor_fg,
    16,
    16
  );

  the_cursor = XCreatePixmapCursor
  (
    XtDisplay(w),
    foreground_bitmap,
    background_bitmap,
    &forecolor,
    &backcolor,
    hotx, hoty
  );

  XDefineCursor
  (
    XtDisplay(w),
    XtWindow(w),
    the_cursor
  );
}

void change_cursor_to(Widget w, int *tag, XtPointer reason)
/*****************************************************************************\
* Purpose: Answers toolbox buttons,
*
* Called by: toolbox pushbuttons
\*****************************************************************************/
{
  switch(*tag)
  {
    case 0:  /* Input */
      change_cursor
      (
        input_cursor_foreground,
        input_cursor_background,
        "black", "white",
        9, 7  /* hot spot */
      );
    break;

    case 1:  /* Output */
      change_cursor
      (
        output_cursor_foreground,
        output_cursor_background,
        "black", "white",
        14, 7  /* hot spot */
      );
    break;

    case 2:  /* Input/output */
      change_cursor
      (
        io_cursor_foreground,
        io_cursor_background,
        "black", "white",
        7, 7  /* hot spot */
      );
    break;

    case 3:  /* Star */
      change_cursor
      (
        star_cursor_foreground,
        star_cursor_background,
        "black", "white",
        7, 7  /* hot spot */
      );
    break;

    case 4:  /* Delete */
      change_cursor
      (
        del_cursor_foreground,
        del_cursor_background,
        "red", "white",
        8, 8  /* hot spot */
      );
    break;

    case 5:  /* Outage */
      change_cursor
      (
        del_cursor_foreground,
        del_cursor_background,
        "blue", "white",
        8, 8  /* hot spot */
      );
    break;

    case 6:  /* Undelete */
      change_cursor
      (
        undel_cursor_foreground,
        undel_cursor_background,
        "red", "white",
        7, 7  /* hot spot */
      );
    break;

    case 7:  /* Restore outage */
      change_cursor
      (
        undel_cursor_foreground,
        undel_cursor_background,
        "blue", "white",
        7, 7  /* hot spot */
      );
    break;

    case 8:  /* New Bend */
      change_cursor
      (
        move_cursor_foreground,
        move_cursor_background,
        "blue", "white",
        1, 1  /* hot spot */
      );
    break;

    case 9:  /* Move bus */
      change_cursor
      (
        move_cursor_foreground,
        move_cursor_background,
        "black", "white",
        1, 1  /* hot spot */
      );
    break;

    case 10:  /* New bus */
      change_cursor
      (
        bus_cursor_foreground,
        bus_cursor_background,
        "black", "white",
        8, 8  /* hot spot */
      );
    break;

    case 11:  /* New line */
      change_cursor
      (
        line_cursor_foreground,
        line_cursor_background,
        "blue", "white",
        8, 8  /* hot spot */
      );
    break;

    case 12:  /* Draw */
      change_cursor
      (
        draw_cursor_foreground,
        draw_cursor_background,
        "black", "white",
        8, 8  /* hot spot */
      );
    break;

    case 13:  /* Alpha */
      change_cursor
      (
        alph_cursor_foreground,
        alph_cursor_background,
        "black", "white",
        8, 8  /* hot spot */
      );
    break;

    case 14:  /* Group */
      change_cursor
      (
        grp_cursor_foreground,
        grp_cursor_background,
        "black", "white",
        8, 8  /* hot spot */
      );
    break;

    case 15:  /* Plot Dialog */
      change_cursor
      (
        plot_cursor_foreground,
        plot_cursor_background,
        "black", "white",
        8, 8  /* hot spot */
      );
    break;

    default:
	printf("** WARNING ** from change_cursor (utils.c)\n" );
	printf("   Incorrect param tag for cursor type %c\n", tag );
  }

}


char *month_to_three_char(char month)
/**************************************************************************\
\**************************************************************************/
{
  switch(month)
  {
    case '1': return("JAN");
    case '2': return("FEB");
    case '3': return("MAR");
    case '4': return("APR");
    case '5': return("MAY");
    case '6': return("JUN");
    case '7': return("JUL");
    case '8': return("AUG");
    case '9': return("SEP");
    case 'o':
    case 'O': return("OCT");
    case 'n':
    case 'N': return("NOV");
    case 'd':
    case 'D': return("DEC");
    default : return("   ");
  }
}

char three_char_to_month(char *abbrev)
{
  switch(abbrev[2])
  {
    case 'n':
    case 'N':
      if(abbrev[1] == 'A' || abbrev[1] == 'a') return('1');
      else return('6');
    case 'b':
    case 'B': return('2');
    case 'r':
    case 'R':
      if(abbrev[1] == 'A' || abbrev[1] == 'a') return('3');
      else return('4');
    case 'y':
    case 'Y': return('5');
    case 'l':
    case 'L': return('7');
    case 'g':
    case 'G': return('8');
    case 'p':
    case 'P': return('9');
    case 't':
    case 'T': return('O');
    case 'v':
    case 'V': return('N');
    case 'c':
    case 'C': return('D');
    default : return(' ');
  }
}


/***************************************************************************\
*
*  left_justify_text
*
*  left justify a text widget
*
*  Author: John Rutis 30 Dec 1992
*
\***************************************************************************/
void left_justify_text(Widget id)
{
  XmTextShowPosition(id, XmTextGetLastPosition(id));
}


void disable_pushbutton ( char *name )
/*****************************************************************************\
*       Purpose:
*                 turns off pushbutton so user cannot activate it.
*
*       Author: B Rogers        Jun 12, 1992    Modified: Jul 09, 1992
* silently return if the button is not managed for an attempt to diable it!
*
\*****************************************************************************/
{
  Widget id;

  if(HashLookup(name, &id) == False) return;
  /* if(( id = widget_id( name )) == FALSE) return; */

  XtSetSensitive( (Widget)id, False );

}


 void enable_pushbutton ( char *name )
/*****************************************************************************\
*       Purpose:
*                 turns on pushbutton so user can activate it.
*
*       Author: B Rogers        Jun 12, 1992    Modified: Jul 09, 1992
*
\*****************************************************************************/
{
  Widget id;

  if(( id = widget_id( name )) == FALSE) return;

  XtSetSensitive( (Widget)id, True );

}
 
Boolean get_widget_text( char *fld_name, char *ret_string, int size )
/*****************************************************************************\
*       Purpose:  Given a MOTIF text field name, (or text box)
*		  Return the character string from that box.
*
*	* Programers BEWARE! * Param ret_string must be allocated!
*			       i.e. the calling module must have:
*			          char *ret_string[MAX]; 
*			       then use:
				  get_widget_text( "a_field:, *ret_string )
*
*       Author: B Rogers	Mar 18, 1992      Modified: Jul 28, 1992
*	D. Clark - add error return code Apr 26, 1993
*
*       Called by:
*
\*****************************************************************************/
{
  /** this is quick fix to existing routine **/
  Widget id;
  char *pnew_txt_widget_str;

  if( ( id = widget_id( fld_name ) ) )
  {
    pnew_txt_widget_str = XmTextGetString(id);

   /* could overwrite memory if shortage of space in calling program */
    strncpy(ret_string, pnew_txt_widget_str, size );
    XtFree(pnew_txt_widget_str);
    return True;
  }
  else
  {
    /* memset ( ret_string, '*', sizeof(ret_string) ); */
    memset ( ret_string, '*', size );
    return False;
  }
}

/*****************************************************************************\
*       Function: decode_x.c
*
*       This function implicitly decodes a string given a fortran format
*       specification.
*
\*****************************************************************************/

double implied_dec_rw (char *str, int a, int b)
{
   char *c, *d, str1[21], str2[21];

   int i = 0;  /* flag 0/1 = decimal point not found/found        */

   if (a < b || a == 0 || a > 20) return (0.0);
   for (c = str, d = str1; *c > 0 && c < &str[a]; c++) {
      *d = *c;
      d++;
      if (*c == '.') i = 1;
   }
   str1[a] = '\0';
   for (; d < &str1[a]; d++) *d = '0'; /* zero fill field   */
   if (i == 0) {
      for (c = str1, d = str2; c < &str1[a-b]; c++) {
         *d = *c;
         d++;
      }
      *d = '.';                                    /* add decimal point */
      d++;
      for (; c <= &str1[a]; c++) {
         *d = *c;
         d++;
      }
      *d = '\0';
      return (atof (str2));
   } else {
      return (atof (str1));
   }
}

static int answer;
int userCancelStatus(int *status)
{
  *status = answer;
  return(answer);
}
static void user_cancel_cb(Widget w, int *answer, XmAnyCallbackStruct *cbs) {
/***************************************************************************\
*
*
*  Called by: user cancel widget
\***************************************************************************/
   switch(cbs->reason) {
      case XmCR_OK:  *answer = 1; break;
      case XmCR_CANCEL: *answer = 2; break;
   }
}

/***************************************************************************\
*  Displays a standard message box for user's response
\***************************************************************************/
int userCancel(Widget parent, char *labell, char *label2, char *str, Widget *rtn) 
{
   static Widget dialog;
   XmString text, labl, lab2;

   labl = XmStringCreateSimple(labell);
   lab2 = XmStringCreateSimple(label2);
   text = XmStringCreateSimple(str);

   if (!dialog) {
      dialog = XmCreateQuestionDialog(parent,"dialog",NULL,0);
      XtVaSetValues(dialog,
        XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL,
        XmNdefaultButtonType,XmDIALOG_CANCEL_BUTTON,
        XmNdialogType,XmDIALOG_WORKING,
        XmNokLabelString,labl,XmNcancelLabelString,lab2,NULL);
      XtSetSensitive(XmMessageBoxGetChild(dialog,XmDIALOG_HELP_BUTTON),False);
      XtAddCallback(dialog,XmNokCallback,(XtCallbackProc)user_cancel_cb,&answer);
      XtAddCallback(dialog,XmNcancelCallback,(XtCallbackProc)user_cancel_cb,&answer);
   }
   answer = 0;
   XtVaSetValues(dialog,XmNmessageString, text,
      XmNokLabelString,labl,
      XmNcancelLabelString,lab2,NULL);
   XmStringFree(text);
   XmStringFree (labl) ;
   XmStringFree(lab2);
   XtManageChild(dialog);

   *rtn = dialog;
   return (answer);
}


void lookup_and_reduce_text_field( char *fld_name, int size )
/**************************************************************************\
* Purpose: removes trailing or leading blanks from contents of
*	   a text dialog box - so that users can add more data
*	   without having to remove the blanks.
*
* Author: Bill Rogers 4/21/94
\**************************************************************************/
{

  Widget id;

  if ( id = widget_id( fld_name ) )
		 reduce_text_field( id, size );

}


void reduce_text_field( Widget id, int size )
{
#define MAXSTR 120

  char *txt_widget_str;
  char string[MAXSTR];

  if( size > MAXSTR ) 
  {
    printf("*** WARNING *** from reduce_text_field (utils.c)\n");
    printf("     String size (%d) too large for routine to handle\n", size );
  }

  txt_widget_str = XmTextGetString(id);
  strncpy( string, txt_widget_str, size );
  XtFree( txt_widget_str );

  string[size] = '\0';

  clip_outside_blanks( string );

  XmTextSetString( id, string );

}

void clip_outside_blanks  ( char *string )
/**************************************************************************\
*
*       Purpose: Removes leading and trailing blanks in data
*                Normally used before filling a text field so
*                that users won't have to delete spaces in order
*                to add characters.
*
*       Author: Walt Powell
*
\**************************************************************************/
{
  char *c, *d;
  int len;

  if( string == NULL )
  {
    printf("WARNING - call to clip_outside_blanks (utils.c)\n");
    printf("  NULL passed as string address (segmentation fault adverted)\n" );
    return;
  }

  /* search end of string for blanks, replace with NULL */
  clip_trailing_blanks  ( string );

  /* search beginning of string for blanks, left justify */
  for (d = string; *d > 0 && *d == ' '; d++);
  if (d > string) {
    for (c = string; *d > 0; c++, d++) *c = *d;
    *c = 0;
  }
}


void clip_trailing_blanks  ( char *string )
/**************************************************************************\
*       Purpose: Removes trailing blanks in data
*
*       Author: Walt Powell (cut from clip_outside_blanks by B.Rogers)
\**************************************************************************/
{
  char *c;
  int len;

  if( string == NULL )
  {
    printf("WARNING - call to clip_trailing_blanks (utils.c)\n");
    printf("  NULL passed as string address (segmentation fault adverted)\n" );
    return;
  }

  /* search end of string for blanks, replace with NULL */
  len = strlen(string);
  for (c = &string[len-1]; c >= string && (*c == ' ' || *c == '\n'); c--) {
    *c = 0;
  }
}

int motif2cstring(XmString xmstr, char ** cstr )
{
  int cnt;
  char newstr[1024];
  XmStringContext context = NULL;
  char         *text;
  XmStringCharSet  charset;
  XmStringDirection    dir;
  Boolean      separator = False;
  char *eol = "\n";
  int i;

  XmStringInitContext(&context, xmstr);
  newstr[0] = '\0';
  cnt = XmStringLineCount(xmstr);
  for(i=0; i<cnt; i++) {
    XmStringGetNextSegment(context, &text, &charset, &dir, &separator);
    if(i > 0) {
      strcat(newstr, eol);
    }
	if(text != NULL)
		strcat(newstr, text);
  }
  XmStringFreeContext(context);
  /* printf("edgeg: cnt %d text = (%s)\n", cnt, newstr); */
  *cstr = (char *)malloc((1+strlen(newstr))*sizeof(char));
  strcpy(*cstr, newstr);
  return cnt;
}


char *getValueByName( char *name )
/*************************************************************************\
* Purpose: Get compound string from named widget
*
*
*
\*************************************************************************/
{
  Widget wid;

  wid = widget_id( name );
 
  return( getValueOfWidget( wid ) );

}

char *getValueOfWidget( Widget wid )
/*************************************************************************\
* Purpose: Get compound string from widget
*
*
\*************************************************************************/
{
  char *str;
  XmString xstr;

  if( wid == NULL )
  {
    printf("*** ERROR *** from getValue\n");
    return ( "NULL" );
  }
  else
  {
    XtVaGetValues( wid, XmNvalue, &str, NULL );   /* gets compound str */
    /* str = cstring_to_string(xstr);       change compound string to ascii */
  }
  return str;
}
