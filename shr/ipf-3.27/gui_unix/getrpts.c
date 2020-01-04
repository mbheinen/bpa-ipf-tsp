static char sccsid[]="@(#)getrpts.c	20.4 2/15/95";
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
#include <string.h>
#ifdef VMS
#include <types.h>
#include <stat.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif
#include <Xm/Text.h>
#include <Xm/Xm.h>

#define TOTALREPORTS 30
#define REPORTPATH "/common/eohbdms/powerflow_reports/\0"

/*
 * The following is the array of report files and flags
 * that indicate which reports have been selected by the
 * reports diaolg box for viewing or printing
 */

extern void Vuit_MU		( char *, char * );
extern Widget widget_id		( char * );


struct report_list
{
  char	*file_name;
  char	*title;
  int	toggle;
} report_table[] =
  {
    "volt_by_own.rpt",		"UNDERVOLTAGE/OVERVOLTAGE",	0,
    "unsched.rpt",		"UNSCHEDULED REACTIVE",		0,
    "lines_by_own.rpt",		"LINES > 80% BY OWNER",		0,
    "tx_by_own.rpt",		"TRANSFORMERS > 80% BY OWNER",	0,
    "intchg_slk_bus.rpt",	"SLACK BUS GENERATION",		0,
    "input.rpt",		"INPUT",			0,
    "output.rpt",		"OUTPUT",			0,
    "solopt.rpt",		"SOLUTION OPTIONS",		0,
    "shunt.rpt",		"SHUNT SUMMARY",		0,
    "ltc.rpt",			"LTC SUMMARY",			0,
    "phase.rpt",		"PHASE SHIFT SUMMARY",		0,
    "var_cont_sum.rpt",		"VAR CONT BUS SUMMARY",		0,
    "bx.rpt",			"BX BUS SUMMARY",		0,
    "solsum.rpt",		"SOLUTION SUMMARY",		0,
    "error.rpt",		"ERROR MESSAGES",		0,
    "timing.rpt",		"TIMING",			0,
    "comp.rpt",			"COMPENSATED LINES",		0,
    "gen_by_zone.rpt",		"GENERATION & LOAD BY ZONE",	0,
    "gen_loads.rpt",		"GENERATION & LOAD BY OWNER",	0,
    "ind_loads.rpt",		"INDUSTRIAL LOADS",		0,
    "spin_res.rpt",		"SPINNING RESERVES",		0,
    "volt_by_own.rpt",		"VOLTAGE BY OWNER",		0,
    "loss_by_own.rpt",		"LOSSES BY OWNER",		0,
    "intchg_matrix.rpt",	"INTERCHANGE MATRIX",		0,
    "intchg_tie_line.rpt",	"INTERCHANGE TIE LINE",		0,
    "sum_bus.rpt",		"BUS SUMMARY",			0,
    "line_eff.rpt",		"LINE EFFICIENCY",		0,
    "tx_eff_core.rpt",		"TRANSFORMER CORE EFFICIENCY",	0,
    "tx_eff_tot.rpt",		"TOTAL TRANSFORMER EFFICIENCY",	0,
    "system_totals.rpt",	"SYSTEM TOTALS",		0
  };

int	button_number;
int	number_of_reports = 0;
int	one_many_flag = 1;  /* 0 = single, 1 = multi */

/*****************************************************************************\
*                                                                             *
*     Function:	GetValue                                                      *
*                                                                             *
*     Gets a single value - don't use for several, use XtGetValues            *
*                                                                             *
*     Called by:                                      x                       *
*                                                                             *
*     Calls:       none                                                       *
*                                                                             *
\*****************************************************************************/

void
GetValue(w, resource, value)

Widget	w;
char	*resource, *value;

{
  Arg	al[1];

  XtSetArg(al[0], resource, value);
  XtGetValues(w, al, 1);

}


/*****************************************************************************\
*                                                                             *
*     Function:	SetValue                                                      *
*                                                                             *
*     Sets a single value - don't use for several, use XtSetValues            *
*                                                                             *
*     Called by:                                      x                       *
*                                                                             *
*     Calls:       none                                                       *
*                                                                             *
\*****************************************************************************/

void
SetValue(w, resource, value)

Widget	w;     
char	*resource, *value;

{
  Arg  	al[1];

  XtSetArg(al[0], resource, value);
  XtSetValues(w, al, 1);
                               
}


/*****************************************************************************\
*                                                                             *
*                                                                             *
\*****************************************************************************/

void make_report_window();

/*
 * The function one_many_window_toggle is called by the single window
 * and individual window toggle buttons and sets the one_namy flag
 * to 0 for one window and 1 for multi windows for later use by
 * the add_text function.
 */

void one_many_window_toggle(w, button_number, reason)
Widget		w;
int		*button_number;
unsigned long	*reason;
{
  one_many_flag = *button_number;
}

/*
 * The toggle_report_array function is called by each report
 * selection button and sets the corresponding flag to 1 or 0
 * as the buttons are toggled.
 */

void toggle_report_array(w, button_toggled, reason)
Widget		w;
char		*button_toggled;
unsigned long	*reason;
{
  int i = 0;

  while((strcmp(button_toggled, report_table[i].file_name)) != 0)
  {
    if(++i > 30)
      return;
  }

  report_table[i].toggle = (!report_table[i].toggle);

  if(report_table[i].toggle == 1)
    ++number_of_reports;
  else
    --number_of_reports;
}

/*
 * The function add_text manages the report window(s) and puts the
 * appropreate text in them, catinating all the reports if a single
 * window was requested.
 */

void add_text(w, tag, reason)
Widget		w;
char		*tag;
unsigned long	*reason;

{
  int rn;

  if(number_of_reports == 0)
  {
    Vuit_MU( "M", "no_reports_chosen_dialog");
    return;
  }

  rn = number_of_reports;

  if(rn > 4 || one_many_flag == 0)
  {
    /* catinate and show in one window */
  }
  else
  {
      make_report_window(rn);
  }
}

void
make_report_window(report_number)
int report_number;

{
  char *report_text;
  char fn[80];
  char report_file_name[40];
  int  i, window_number;
  struct stat statb;
  FILE *fp;
  Widget id;

  i = TOTALREPORTS;

  window_number = report_number;
  do
  {
    strcpy(fn, REPORTPATH);

    while(report_table[i].toggle != 1)
      --i;

    if(i >= 0)
    {
      strcpy(report_file_name, report_table[i].file_name);
    }
    else return;

    strcat(fn, report_file_name);

    if((fp = fopen(fn, "r")) == NULL)
    {
      printf("\nCan't open input file %s\n", report_file_name);
    }
    else
    {
      stat(fn, &statb);

      if(!(report_text = XtMalloc((unsigned)(statb.st_size + 1))))
      {
        fprintf(stderr, "Can't alloc enough space for %s\b", fn);
        XtFree(fn);
        fclose(fp);
        return;
      }

      fread(report_text, sizeof(char), statb.st_size + 1, fp);

      XtFree(fn);
      fn[0] = 0;
      report_file_name[0] = 0;
      fclose(fp);

      report_text[statb.st_size] = 0;

      switch(window_number)
      {
        case 1:
          Vuit_MU( "M", "reports_form_1");
          id = widget_id("reports_text_1" );
          XmTextSetString(id, report_text);
          id = widget_id("reports_form_1" );
          SetValue(id, XmNdialogTitle,
            XmStringCreate(report_table[i].title, XmSTRING_DEFAULT_CHARSET));
        break;

        case 2:
          Vuit_MU( "M", "reports_form_2");
          id = widget_id("reports_text_2" );
          XmTextSetString(id, report_text);
          id = widget_id("reports_form_2" );
          SetValue(id, XmNdialogTitle,
            XmStringCreate(report_table[i].title, XmSTRING_DEFAULT_CHARSET));
        break;

        case 3:
          Vuit_MU( "M", "reports_form_3");
          id = widget_id("reports_text_3" );
          XmTextSetString(id, report_text);
          id = widget_id("reports_form_3");
          SetValue(id, XmNdialogTitle,
            XmStringCreate(report_table[i].title, XmSTRING_DEFAULT_CHARSET));
        break;

        case 4:
          Vuit_MU( "M", "reports_form_4");
          id = widget_id("reports_text_4");
          XmTextSetString(id, report_text);
          id = widget_id("reports_form_4");
          SetValue(id, XmNdialogTitle,
            XmStringCreate(report_table[i].title, XmSTRING_DEFAULT_CHARSET));
        break;

        default:
          printf("Can't find case %d\n", window_number);
        break;
      }
    }
    --i;
  }
  while((--window_number) > 0);
}
