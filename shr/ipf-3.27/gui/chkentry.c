static char sccsid[]="@(#)chkentry.c	20.4 2/15/95";
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>

extern Widget VUIT_Manage(char *widget_name);

void next_limit();

/****************************************************************************\
*
*   digit_check  callback procedure to allow only decimal digits
*                to be entered in text boxes
*
*   called by XmNmodifyVerifyCallback of any text box
*
*   Written by John Rutis
*
\****************************************************************************/

void digit_check(w, tag, reason)
Widget w;
XtPointer tag;
XmTextVerifyCallbackStruct *reason;
{
  if(reason->startPos < reason->currInsert) /* backspace */
    return;

  if(reason->text->ptr == NULL)
    return;

  if(!isdigit(reason->text->ptr[0]) &&
       reason->text->ptr[0] != ' ')
  {
    reason->doit = False;
  }
}


/****************************************************************************\
*
*   decimal_check  callback procedure to allow only decimal digits, point
*                  & minus sign to be entered in text boxes
*
*   called by XmNmodifyVerifyCallback of any text box
*
*   Written by John Rutis
*
\****************************************************************************/

void decimal_check(w, tag, reason)
Widget w;
XtPointer tag;
XmTextVerifyCallbackStruct *reason;
{
  if(reason->startPos < reason->currInsert) /* backspace */
    return;

  if(reason->text->ptr == NULL)
    return;

  if(!isdigit(reason->text->ptr[0]) &&
     reason->text->ptr[0] != '-' &&
     reason->text->ptr[0] != ' ' &&
     reason->text->ptr[0] != '.')
  {
    reason->doit = False;
  }
}


/****************************************************************************\
*
*   int_check  callback procedure to allow only decimal digits and
*              minus sign to be entered in text boxes
*
*   called by XmNmodifyVerifyCallback of any text box
*
*   Written by John Rutis
*
\****************************************************************************/

void int_check(w, tag, reason)
Widget w;
XtPointer tag;
XmTextVerifyCallbackStruct *reason;
{
  if(reason->startPos < reason->currInsert) /* backspace */
    return;

  if(reason->text->ptr == NULL)
    return;

  if(!isdigit(reason->text->ptr[0]) &&
     reason->text->ptr[0] != ' ' &&
     reason->text->ptr[0] != '-')
  {
    reason->doit = False;
  }
}


/****************************************************************************\
*
*   alpha_sp_check  callback procedure to allow only alphabetic characters
*                   and the space to be entered in text boxes
*
*   called by XmNmodifyVerifyCallback of any text box
*
*   Written by John Rutis
*
\****************************************************************************/

void alpha_sp_check(w, tag, reason)
Widget w;
XtPointer tag;
XmTextVerifyCallbackStruct *reason;
{
  if(reason->startPos < reason->currInsert) /* backspace */
    return;

  if(reason->text->ptr == NULL)
    return;

  if(!isalpha(reason->text->ptr[0]) &&
     reason->text->ptr[0] != ' ')
  {
    reason->doit = False;
  }
}


/****************************************************************************\
*
*   alphanum_sp_check  callback procedure to allow only alphabetic characters,
*                      digits and the space to be entered in text boxes
*
*   called by XmNmodifyVerifyCallback of any text box
*
*   Written by John Rutis
*
\****************************************************************************/

void alphanum_sp_check(w, tag, reason)
Widget w;
XtPointer tag;
XmTextVerifyCallbackStruct *reason;
{
  if(reason->startPos < reason->currInsert) /* backspace */
    return;

  if(reason->text->ptr == NULL)
    return;

  if(!isalnum(reason->text->ptr[0]) &&
     reason->text->ptr[0] != ' ')
  {
    reason->doit = False;
  }
}


/****************************************************************************\
*
*   alpha_check  callback procedure to allow only alphabetic characters
*                to be entered in text boxes
*
*   called by XmNmodifyVerifyCallback of any text box
*
*   Written by John Rutis
*
\****************************************************************************/

void alpha_check(w, tag, reason)
Widget w;
XtPointer tag;
XmTextVerifyCallbackStruct *reason;
{
  if(reason->startPos < reason->currInsert) /* backspace */
    return;

  if(reason->text->ptr == NULL)
    return;

  if(!isalpha(reason->text->ptr[0]))
  {
    reason->doit = False;
  }
}


/****************************************************************************\
*
*   alphanum_check  callback procedure to allow only alphabetic characters
*                   and digits to be entered in text boxes
*
*   called by XmNmodifyVerifyCallback of any text box
*
*   Written by John Rutis
*
\****************************************************************************/

void alphanum_check(w, tag, reason)
Widget w;
XtPointer tag;
XmTextVerifyCallbackStruct *reason;
{
  if(reason->startPos < reason->currInsert) /* backspace */
    return;

  if(reason->text->ptr == NULL)
    return;

  if(!isalnum(reason->text->ptr[0]))
  {
    reason->doit = False;
  }
}


/****************************************************************************\
*
*   alpha_ul_sp_check  callback procedure to allow only alphabetic characters,
*                      space and underline to be entered in text boxes
*
*   called by XmNmodifyVerifyCallback of any text box
*
*   Written by John Rutis
*
\****************************************************************************/

void alpha_ul_sp_check(w, tag, reason)
Widget w;
XtPointer tag;
XmTextVerifyCallbackStruct *reason;
{
  if(reason->startPos < reason->currInsert) /* backspace */
    return;

  if(reason->text->ptr == NULL)
    return;

  if(!isalpha(reason->text->ptr[0]) &&
     reason->text->ptr[0] != '_' &&
     reason->text->ptr[0] != ' ')
  {
    reason->doit = False;
  }
}


/****************************************************************************\
*
*   data_check  procedure to check whether the value in the text box
*               matches the specification in the tag field
*
*   called by any text widget XmNloosingFocusCallback
*
*   Written by John Rutis
*
\****************************************************************************/

void data_check(w, tag, reason)
Widget w;
XtPointer tag;
XmTextVerifyCallbackStruct *reason;
{
  int	i = 0, j = 0, min, max, match = 0;
  char	*value, limit[11], template[80];

  value = XmTextGetString(w);
  sprintf(template, "%s\0", tag);

  while(template[i] != ']')
  {
    switch(template[i])
    {
      case '[':
      case ',':
        ++i;
        next_limit(&i, template, limit);

        if(template[i] == ']' ||
           template[i] == ',')
        {
          if(atoi(limit) == atoi(value))
          {
            match |= 1;
          }
          else
          {
            match |= 0;
          }
        }
      break;

      case '<':
        ++i;
        min = atoi(limit);
        next_limit(&i, template, limit);
        max = atoi(limit);

        if(min <= atoi(value) && atoi(value) <= max)
          match |= 1;
      break;

      case '!':

      break;

      default:
      break;
    }
  }

  if(match == 0)
    VUIT_Manage("text_input_error_dialog");
}


/****************************************************************************\
*
*   next_limit  procedure to pull the next limit value from the
*               limit_string
*
*   called by data_check
*
*   Written by John Rutis
*
\****************************************************************************/

void next_limit(int *position, char limit_string[81], char limit[11])
{
  int	j = 0;

  for(;;++(*position))
  {
    limit[j] = limit_string[*position];

    if(limit[j] == ']' ||
       limit[j] == ',' ||
       limit[j] == '<')
    {
      limit[j] = '\0';
      break;
    }
    else
    {
      ++j;
    }
  }
}
