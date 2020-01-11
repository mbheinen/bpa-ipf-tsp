static char sccsid[]="@(#)linezcalc.c	20.6 2/15/95";
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
/********** LINEZCALC.C *****************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <math.h>
#include "em.h"
#include "ipf_ipc.h" /* define BUFSIZE ... do not use BUFSIZ ***/

em_init              /* Error structure macro - must be included in */
                     /* every file.                                 */
#define MAXCND 36
#define USCANSIZE 20

extern Widget  widget_id( char * widget_name );
extern void clip_outside_blanks	 ( char * );
extern void lookup_and_get_field ( char *, char *, int );
extern void lookup_and_fill_field( char *, char *, int, int );
extern int  is_err_msg		 ( char * );
extern int  pfgetdata_cb	 ( char *, char * );
extern void copy_fn		 ( char *, char * );
extern Vuit_MU                  ( char *, char * );

static int num_cond,            /* number of conductors             */
           modify,              /* modify flag: 0 = no, 1 = yes     */
           save_file,           /* file save flag: 0 = no, 1 = yes  */
           compute_lic,         /* compute flag: 0 = no; 1 = yes    */
           number[MAXCND],      /* conductor circuit number         */
           phase[MAXCND],       /* phase number [0-9]               */
           numbnd[MAXCND];      /* bundle number [0-9]              */

static char name[MAXCND][10],   /* conductor name                   */
            bundle[MAXCND][4];  /* bundle name or number            */

static float resis[MAXCND],     /* conductor resistance             */
             diameter[MAXCND],  /* conductor diameter               */
             skin[MAXCND],      /* conductor T/D ratio              */
             horiz[MAXCND],     /* horizontal coordinate            */
             vtower[MAXCND],    /* vertical tower coordinate        */
             vmid[MAXCND],      /* vertical mid-point coordinate    */
             separ[MAXCND],     /* bundling separation              */
             angle[MAXCND];     /* bundling orientation of 1st cond */

Widget w_cond_num,              /* Conductor number                 */
       w_phase_num,             /* Phase number                     */
       w_cond_name,             /* Conductor name                   */
       w_resis,                 /* Conductor resistance             */
       w_diam,                  /* Conductor diameter               */
       w_skin,                  /* Skin-effect (T/D)                */
       w_horiz,                 /* Tower horizontal coordinate      */
       w_vtower,                /* Tower vertical coordinate        */
       w_vmid,                  /* Mid-point vertical coordinate    */
       w_bundle,                /* Bundle number                    */
       w_separ,                 /* Bundle separation                */
       w_angle;                 /* Bundle angle                     */

static FILE *fp;                /* conductor file pointer           */

static char out_buffer[BUFSIZE], in_buffer[BUFSIZE], ipf_buffer[BUFSIZE];

char unit_type[20];
char line_freq[10];
double frequency;
char wire_name[10];
char num_bundle[10];

void load_all_edit_widget_id	();
int  cond_num_cmpr		(char *, char *);
void uscan20 			(char *in, char out[][USCANSIZE], 
				int *nwrd, char limit1[], char limit2[]);
void line_z_freq_change		( Widget, XtPointer, XmAnyCallbackStruct * );
int  lookup_cp_to_field		();
void line_z_units_change	( Widget, XtPointer, XmAnyCallbackStruct *);
int  line_z_edit_insert		( Widget, int *, unsigned long *);
void line_z_edit_clear		();
int  line_z_edit_delete		( Widget, int *, unsigned long *);
int  line_z_edit_replace	( Widget, int *, unsigned long *);
int  find_xnum			( int *pos );
void delete_list_row		( int cnt );
void transf_list_row		( int cnt );
int  set_std_cond_values	( Widget, XtPointer, XmAnyCallbackStruct *cbs);

void read_lcd_file ();
void lookup_and_fill_list	( char *fld_name, char string[][10], int size );
void line_z_list_number_cb	( Widget, XtPointer, XmListCallbackStruct *);
void list_to_edit		( char *edit_fld, char *list_fld,
				   int row, int length );
void highlight_list_row		( int cnt );
void edit_to_list		( char *edit_fld, Widget list_id, int cnt );
void hi_lite			( char *widget_name, int cnt );
void set_optmenu_label		( char *widget_name, char *label );
void set_optmenu_label_id	( Widget menu_id, char *label );

void line_z_calc 		();
void trfs_loop_i		( char *list_fld_name, int array[], int cnt );
void trfs_loop_f		( char *list_fld_name, float array[], int cnt );
int  check_conductors 		(int p1, int p2, int p3, int cnt );
int output_cond_error		( int i, int j, double v1, double v2,
			 	  char *wrd, char *uname );
int check_sag			( int p1, int p2, int p3, int cnt );


void 
load_all_edit_widget_id /*****************************************************\
*
*	First routine called - - find the widget id's.
*  
*	Author Walt Powell	Aug 1992
*
*	Called by: "Calculate Impedance" pushbutton in branch jacket of
*			bus front box.
*
\*****************************************************************************/
( Widget w, int tag, unsigned long reason) 

{

  if(( w_cond_num  = widget_id( "line_z_number_list"    )) == FALSE) return; 
  if(( w_phase_num = widget_id( "line_z_phase_list"     )) == FALSE) return;
  if(( w_cond_name = widget_id( "line_z_type_list"      )) == FALSE) return;
  if(( w_cond_name = widget_id( "line_z_type_list"      )) == FALSE) return;
  if(( w_resis     = widget_id( "line_z_dia_resis_list" )) == FALSE) return;
  if(( w_diam      = widget_id( "line_z_dia_diam_list"  )) == FALSE) return;
  if(( w_skin      = widget_id( "line_z_skin_list"      )) == FALSE) return;
  if(( w_bundle    = widget_id( "line_z_bundle_list"    )) == FALSE) return;
  if(( w_separ     = widget_id( "line_z_separ_list"     )) == FALSE) return;
  if(( w_angle     = widget_id( "line_z_angle_list"     )) == FALSE) return;
  if(( w_horiz     = widget_id( "line_z_horiz_list"     )) == FALSE) return;
  if(( w_vtower    = widget_id( "line_z_vtower_list"    )) == FALSE) return;
  if(( w_vmid      = widget_id( "line_z_vmid_list"      )) == FALSE) return;

/* set up for possible error message */

   err.line  = EM_LINE;
   err.msg   = errmsg;
   err.type  = FATAL;
   err.link  = "Line Impedance Calculation";
   err.ident = sccsid;

   copy_fn( "line_miles",      "line_z_distance_text" ); /* from filedlgrtn.c */
   copy_fn( "bus_front_volts", "line_z_basekv_text" );

   if( strlen( unit_type ) ==0 ) strncpy( unit_type, "ENGLISH", 7 );
   if( strlen( line_freq ) ==0 ) strncpy( line_freq, "60",      2 );
}


void 
read_lcd_file /***************************************************************\
*	
*	Open and read a lcd file.
*  
*	Author Walt Powell	Aug 1992
*
*	Called by: "Load" ("OK") pushbutton of line_z_filesel dialog.
*
\*****************************************************************************/
(Widget w, XtPointer *tag, XmFileSelectionBoxCallbackStruct *reason)

{
   Widget menu_id, label_id;
   XmString unit_label, frequency_label;
   int i, iwrd, nwrd;
   char	*text;
   XmStringContext context;
   XmStringCharSet	charset;
   XmStringDirection	dir;
   Boolean	separator;
   FILE *infile;
   char unit_str[80];
   char buffer[81], word[20][USCANSIZE], file_name[80],
        xnum   [MAXCND][10], xphase [MAXCND][10], xname [MAXCND][10], 
        xbundle[MAXCND][10], xresis [MAXCND][10], xdiam [MAXCND][10], 
        xskin  [MAXCND][10], xvtower[MAXCND][10], xvmid [MAXCND][10], 
        xhoriz [MAXCND][10], xsepar [MAXCND][10], xangle[MAXCND][10];

   static char *dict_name[] = {"Pheasant", "Bittern", "Mallard", "Drake",  
                               "Dove",     "Ibis",    "Chukar",  "Thrasher", 
                               "Seahawk",  "Bunting" };
   
   static float dict_resis[] = {0.0722, 0.0738, 0.1183, 0.1180, 0.1687, 
                                0.2363, 0.0516, 0.397,  0.0497, 0.0787};

   static float dict_skin[]  = {0.333,  0.345,  0.286,  0.316,  0.333, 
                                0.316,  0.364,  0.387,  0.4138, 0.375};

   static float dict_diam[]  = {1.382,  1.345,  1.140,  1.108,  0.927, 
                                0.783,  1.602,  1.802,  1.603,  1.302};

   XmString motif_string, *strlist;

   motif_string = (XmString)reason->value;
   XmStringInitContext    ( &context, motif_string );
   XmStringGetNextSegment ( context, &text, &charset, &dir, &separator );
   sprintf( file_name, "%s", text );

   if ( (infile = fopen(file_name, "r")) == NULL)
   {
      sprintf (errmsg, 
      "Unable to open Line Impedance Conductor Data file %s \n", file_name);
      err.line = EM_LINE;
      err.type = FATAL;
      em_show(&err);
      return;
   }
   num_cond = 0;               /* number of conductors             */
   strcpy( unit_type, "ENGLISH" );
   strcpy( line_freq, "60" );


   for (i = 0; i < MAXCND; i++) {

      strcpy(xname  [i]," ");	/* conductor name                   */
      strcpy(xnum   [i]," ");   /* conductor circuit number         */
      strcpy(xphase [i]," ");   /* phase number [0-9]               */
      strcpy(xbundle[i]," ");	/* bundle number [0-9]              */
      strcpy(xresis [i]," ");   /* conductor resistance             */
      strcpy(xdiam  [i]," ");   /* conductor diameter               */
      strcpy(xskin  [i]," ");   /* conductor T/D ratio              */
      strcpy(xvtower[i]," ");	/* vertical tower coordinate        */
      strcpy(xvmid  [i]," ");   /* vertical mid-point coordinate    */
      strcpy(xhoriz [i]," ");   /* horizontal coordinate            */
      strcpy(xsepar [i]," ");   /* bundling separation              */
      strcpy(xangle [i]," ");	/* bundling orientation of 1st cond */
   }

   while ( fgets(buffer, 80, infile) != NULL)
   {

      /* Transfer data from fields into character arrays */

      uscan20 (buffer, word, &nwrd, "~", "= ,\n");
      iwrd = 0;
      while (iwrd < nwrd)
      {
        if (strcmp(word[iwrd], "UNITS") == 0 ||
	    strcmp(word[iwrd], "UNIT" ) == 0 )
	{
           if (strchr(word[++iwrd], '=') != NULL) iwrd++; 
           if (strcmp(word[iwrd],"ENGLISH") == 0 || 
               strcmp(word[iwrd],"English") == 0 || 
               strcmp(word[iwrd],"E") == 0	 || 
               strcmp(word[iwrd],"Eng") == 0	 || 
               strcmp(word[iwrd],"english") == 0 ) 
           {
	     set_optmenu_label( "line_z_units_optmenu", "English" );
	     strcpy( unit_type, "ENGLISH" );
           }
           else if (strcmp(word[iwrd], "METRIC") == 0 ||
                    strcmp(word[iwrd], "Metric") == 0 || 
                    strcmp(word[iwrd], "M"     ) == 0 || 
                    strcmp(word[iwrd], "metric") == 0 ) 
           {
	     set_optmenu_label( "line_z_units_optmenu", "Metric" );
	     strcpy( unit_type, "METRIC" );
           }
	   else
	   {
	     set_optmenu_label( "line_z_units_optmenu", "English" );
 	     sprintf ( errmsg, "%s\n%s(%s)%s\n",
             " Unknown word for UNITS in .lcd file",
             " Input string ",
             word[ iwrd ], 
             " should be 'ENGLISH' or 'METRIC'");
             strcpy( unit_type, "ENGLISH" );
	     err.line = EM_LINE;
	     err.type = INFO;
	     em_show(&err);
	   }

           iwrd++;
        }
	else if (strcmp(word[iwrd], "DEBUG") == 0 ||
                 strcmp(word[iwrd], "debug") == 0 || 
                 strcmp(word[iwrd], "dbg"  ) == 0 ) 
	{
          if (strchr(word[++iwrd], '=') != NULL) iwrd++; 
          iwrd++;
        }
	else if (strcmp(word[iwrd], "DISTANCE") == 0 ||
                 strcmp(word[iwrd], "DIST"    ) == 0 || 
                 strcmp(word[iwrd], "Dist"    ) == 0 || 
                 strcmp(word[iwrd], "distance") == 0 ) 
	{
          if (strchr(word[++iwrd], '=') != NULL) iwrd++; 
          /*lookup_and_fill_field( "line_z_distance_text", word[iwrd], 0, 5); */
          iwrd++;
        }
	else if (strcmp(word[iwrd], "BASEKV") == 0)
	{
          if (strchr(word[++iwrd], '=') != NULL) iwrd++; 
          /*lookup_and_fill_field( "line_z_basekv_text", word[iwrd], 0, 5); */
          iwrd++;
        }
	else if (strcmp(word[iwrd], "FREQUENCY") == 0 ||
                 strcmp(word[iwrd], "FREQ"     ) == 0 || 
                 strcmp(word[iwrd], "F"        ) == 0 || 
                 strcmp(word[iwrd], "f"        ) == 0 || 
                 strcmp(word[iwrd], "frequency") == 0 ) 
	{
          if (strchr(word[++iwrd], '=') != NULL) iwrd++; 
	  set_optmenu_label( "line_z_freq_optmenu", word[iwrd] );
          iwrd++;
        }
	else if (strcmp(word[iwrd], "CONDUCTOR") == 0 ||
                 strcmp(word[iwrd], "COND"     ) == 0 || 
                 strcmp(word[iwrd], "C"        ) == 0 || 
                 strcmp(word[iwrd], "c"        ) == 0 || 
                 strcmp(word[iwrd], "cond"     ) == 0 || 
                 strcmp(word[iwrd], "conductor") == 0 ) 
	{
          if (strchr(word[++iwrd], '=') != NULL) iwrd++; 
          if (num_cond < MAXCND)
	  {
             sprintf(xnum   [num_cond], "%d", num_cond+1);
             strcpy(xphase  [num_cond], word[iwrd]);
             strcpy(xskin   [num_cond], word[iwrd+1]);
                    skin    [num_cond] = atof(xskin[num_cond]); 
             strcpy(xresis  [num_cond], word[iwrd+2]);
                    resis   [num_cond] = atof(xresis[num_cond]); 
             strcpy(xdiam   [num_cond], word[iwrd+3]);
                    diameter[num_cond] = atof(xdiam[num_cond]); 
             strcpy(xhoriz  [num_cond], word[iwrd+4]);
             strcpy(xvtower [num_cond], word[iwrd+5]);
             strcpy(xvmid   [num_cond], word[iwrd+6]);
             strcpy(xsepar  [num_cond], word[iwrd+7]);
             strcpy(xangle  [num_cond], word[iwrd+8]);
             strcpy(xbundle [num_cond], word[iwrd+9]);

             /*  Find conductor name by matching data dictionary */

	     strcpy( xname[ num_cond ], "Other" );

             for (i = 0; i < sizeof dict_resis / sizeof dict_resis[0]; i++)
	     {
               if( fabs(skin    [num_cond] - dict_skin [i]) < 0.0001 &&
                   fabs(resis   [num_cond] - dict_resis[i]) < 0.0001 && 
                   fabs(diameter[num_cond] - dict_diam [i]) < 0.0001)
	       {
                 strcpy(xname[num_cond], dict_name[i]);
		 break;
               }
             }
             num_cond++;
          }
	  else
	  {
            sprintf (errmsg, 
        "More than %d conductors found in line impedance data file \n", MAXCND);
            err.line = EM_LINE;
            err.type = FATAL;
            em_show(&err);
          }
          iwrd = nwrd;
          iwrd++;
        }
	else
        {
           printf("*** WARNING *** Unknown keyword (%s) in .lcd file\n", 
              word[iwrd]);
           iwrd++;
	}
      }
   }

   XmListDeleteAllItems ( w_cond_num  );
   XmListDeleteAllItems ( w_phase_num );
   XmListDeleteAllItems ( w_cond_name );
   XmListDeleteAllItems ( w_resis     );
   XmListDeleteAllItems ( w_diam      );
   XmListDeleteAllItems ( w_skin      );
   XmListDeleteAllItems ( w_horiz     );
   XmListDeleteAllItems ( w_vtower    );
   XmListDeleteAllItems ( w_vmid      );
   XmListDeleteAllItems ( w_bundle    );
   XmListDeleteAllItems ( w_separ     );
   XmListDeleteAllItems ( w_angle     );

   err.line = EM_LINE;
   lookup_and_fill_list("line_z_number_list"    , xnum   , num_cond );
   lookup_and_fill_list("line_z_type_list"      , xname  , num_cond );
   lookup_and_fill_list("line_z_phase_list"     , xphase , num_cond );
   lookup_and_fill_list("line_z_dia_resis_list" , xresis , num_cond );
   lookup_and_fill_list("line_z_dia_diam_list"  , xdiam  , num_cond );
   lookup_and_fill_list("line_z_skin_list"      , xskin  , num_cond );
   lookup_and_fill_list("line_z_horiz_list"     , xhoriz , num_cond );
   lookup_and_fill_list("line_z_vtower_list"    , xvtower, num_cond );
   lookup_and_fill_list("line_z_vmid_list"      , xvmid  , num_cond );
   lookup_and_fill_list("line_z_bundle_list"    , xbundle, num_cond );
   lookup_and_fill_list("line_z_separ_list"     , xsepar , num_cond );
   lookup_and_fill_list("line_z_angle_list"     , xangle , num_cond );

   highlight_list_row( 1 );	/* make 1st row inverse video */

   fclose(infile);

/*************************************************************************\
**** control now reverts back to Motif, which awaits a user response ******
***** such as a pushbutton or a selection of one of the above items.  *****
***************************************************************************
**** Typically the next routine called will be the line_z_list_number_cb 
**** which is activated when user selects an item from any of the 12
**** list boxes.
***************************************************************************
**** Other possible user responses:
****	Change unit type to Metric or English - calls line_z_units_change
****    Change frequency - calls line_z_freq_change
****	Load a differenct .lcd file - calls this same routine again.
****	Calc. imped - calls line_z_calc.
***************************************************************************/

}

void save_lcd_cb(Widget widget, XtPointer tag,
              XmAnyCallbackStruct *callback_data)
/*****************************************************************************\
*   purpose:  Save lcd file
*
*   Author: Bill Rogers    June 2, 1993
*
*  Called by: line_z_save_pb (pushbutton) in linezcalc.u (VUIT)
*
\*****************************************************************************/
{
  Widget id;
  char *file_string;
  char message[256];
  FILE *fp;
  XmString xmnewstring;

  id = widget_id("line_z_save_text");
  file_string = XmTextGetString(id);
 
  if((int)strlen(file_string) >= 1)
  {
    if((fp = fopen(file_string, "r")) == NULL) /* if it's a new file name */
    {
      printf("WRITE LCD FILE NOT READT!\n");
    }
    else /* it's an overwrite */
    {
      fclose(fp);
      id = widget_id ( "save_coord_file_error_box");
      Vuit_MU( "M", "save_coord_file_error_box");
      sprintf(message,
              "%s exists, do you want to overwrite it?",
              file_string);
      xmnewstring = XmStringCreate(message, XmSTRING_DEFAULT_CHARSET),
      XtVaSetValues ( id, XmNmessageString, xmnewstring, NULL);
    }
  }
  XtFree(file_string);
}


/*****************************************************************************\
*	Function: uscan20.c 
*
*	This function is a crude C-equivalent of the FORTRAN uscan.f.
*       The crudeness is that array out is hard-coded: out[][USCANSIZE]
*       where USCANSIZE is specified with a #define statement. Thus, the 
*       parsed output strings are truncated to a length of USCANSIZE.
*
*       uscan20 parses the input string into an array of output
*       strings, delimited by visible delimiters limit1 and
*       invisible delimiters limit2. nwrd is the number of
*       terminators encountered.
*		
*	Author: Walt Powell		Modified: August 5, 1992
*
\*****************************************************************************/
void uscan20 (char *in, char out[][USCANSIZE], int *nwrd, char limit1[], char limit2[])
{
   char *c, *d;
   int num = 0;

   num = 0;

   d = out[num];
   for (c = in; *c != 0; c++) {
      *d = *c;
      if ( strchr(limit1, *d) != NULL) {  /* parse delimiter */
         *d = '\0';
         if (strlen(out[num]) > 0) num++;
         d = out[num];
         *d = *strchr(limit1, *c);
         d++;
         *d = '\0';
         num++;
         d = out[num];
      } else if ( strchr(limit2, *d) != NULL) { /* skip delimiter */
         *d = '\0';
         if (strlen(out[num]) > 0) num++;
         d = out[num];
      } else {
         d++;
      }
   }
   *d = *c;
   if (strlen(out[num]) == 0) num--;
   *nwrd = num;
}   




/***************************************************************************\
*	Function: cond_num_cmpr
*		  Compares two character strings as integers.
*		  Returns: -1 first string is smaller
*			    0 equal
*			    1 first string is larger
*
*	Author: Walt Powell	Aug, 1992
*
\***************************************************************************/
int cond_num_cmpr(char *s1, char *s2) 
{
   int r1 = atoi(s1);
   int r2 = atoi(s2);

   if (r1 > r2)
     return(1);
   else if (r1 < r2)
     return(-1);
   else
     return(0);

}




/***************************************************************************\
*	Function: lookup_cp_to_field
*		(lookup_and_copy_text_to_list_field)
*		 copies all the individual text boxes into the list at
*		 the top of the window.
*
*	Author: Walt Powell/Bill Rogers	August 1992	Modified:
*
*	Called by: line_z_line_number_cb
*
\***************************************************************************/
int lookup_cp_to_field( char *fld_name, char string[], char fmt[])
{
   Widget wid;
   char outbuf[20], s[20], *c;
   int loc, i;
   float x;

   err.line = EM_LINE;
   err.type = FATAL;
   
   loc = strcspn(fmt,"fds");
   switch ( fmt[loc] )
   {
     case 'f':  
      sscanf (string, "%f", &x);   /* Decode variable in "f" format */
      sprintf (outbuf, fmt, x);
     break;

     case 'd':  
      sscanf (string, "%d", &i);   /* Decode variable in "d" format */
      sprintf (outbuf, fmt, i);
     break;
 
     case 's':  
      sscanf (string, "%s", &s[0]);   /* Decode variable in "s" format */
      sprintf (outbuf, fmt, s);
     break;

     default:
      sprintf (errmsg, " Illegal format specification %s to \
lookup_cp_to_field \n", fmt);
      em_show(&err);
      return (1);
   }

   if( ( wid = widget_id( fld_name )) == FALSE) return (1);
   XmTextSetString( wid, outbuf );
   return (0);
}



void line_z_units_change ( Widget w, XtPointer tag,
				     XmAnyCallbackStruct *cbs )
/***************************************************************************\
*	Purpose: 
*
*	Author: Bill Rogers 	Aug 11, 1992	Modified:
*
*	Called by: English/Metric pulldown menu
*
\***************************************************************************/
{
  strcpy( unit_type, tag );
  printf( " unit-type set to:(%s)\n", unit_type );
}


/***************************************************************************\
*	Functioin: line_z_freq_change
*
*	Author: Bill Rogers 	Aug 11, 1992	Modified:
*
*	Called by: Frequency pulldown menu
*
\***************************************************************************/
void line_z_freq_change ( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
{

  strcpy( line_freq, tag );
  printf( " frequency set to:(%s)\n", line_freq );
  if ( line_freq[0] == '6' ) frequency = 60.0 ; 
  if ( line_freq[0] == '5' ) frequency = 50.0 ; 
  if ( line_freq[0] == 'D' ) frequency = 0.001 ; 

}



/*****************************************************************************\
*
*	Function: line_z_edit_replace
*
*       This function performs the following:
*
*       2. If n <= num_cond overwrite (modify) scrollable list data
*          and set modify flag = TRUE.
*       3. Update scrollable list display.
*		
*	Author: Bill Rogers/Walt Powell 	Modified: Sept 17, 1992
*
*	Called by: "Replace" push button callback.
*
\*****************************************************************************/
int line_z_edit_replace (w, tag, list_data) /* dbs */
Widget		w;
int		*tag;
unsigned long *list_data;

{
   int cnt;

   switch( find_xnum( &cnt ) )	/* match found */
   {
    case 1: /* Position found - replace row of elements with updated values */
	      /* and blank out Edit field                                   */

      delete_list_row ( cnt );    /* do it this way because book example */
      transf_list_row ( cnt );    /* on list replace just won't work!    */
      line_z_edit_clear();
      return ( FALSE );

    case 0: 	/* no match */
      sprintf (errmsg," Replaced conductor No. %d is not in list \n",cnt+1);

    case -1: 	/* no data (errmsg was already filled by find_xnum) */
      err.line = EM_LINE;
      err.type = INFO;
      em_show(&err);
      return ( TRUE );

   }

}


/*****************************************************************************\
*
*	Function: line_z_edit_insert
*
*       This function performs the following:
*
*       3. If n > num_cond write new line of scrollable list data
*          and set modify flag = TRUE.
*       4. Update scrollable list display.
*		
*	Author: Bill Rogers/Walt Powell		Modified: Sept 17, 1992
*
*	Called by: "Insert" push button callback.
*
\*****************************************************************************/
int line_z_edit_insert (w, tag, list_data)
Widget		w;
int		*tag;
unsigned long   *list_data;

{
   int cnt;

/** Transfer data from fields into character arrays */
   XtVaGetValues( w_cond_num, XmNitemCount, &num_cond, NULL);
  
   if( num_cond >= MAXCND )
   {
      sprintf(errmsg,"*** INFO *** Cannot add conductor.\n\
             Limit of %d conductors allowed\n", MAXCND );
      err.line = EM_LINE;
      err.type = INFO;
      em_show(&err);
      return( TRUE );
   }

   switch( find_xnum( &cnt ) )
   {
      case 0:	 /* Postion not found - insert new row                 */
      case -1:	 /* empty list */

	 transf_list_row( cnt+1 );
         line_z_edit_clear();
	 return( FALSE );

      case 1:	 /* Error - attempting to add a duplicate item         */
        sprintf(errmsg," Added conductor No. %d is already in list \n",cnt);

         err.line = EM_LINE;
         err.type = INFO;
         em_show(&err);
         return( TRUE );
   }
}


/*****************************************************************************\
*
*	Function: line_z_edit_delete
*	       This function deletes a row of list data.
*
*	Author: Bill Rogers/Walt Powell 	Modified: Sept 17, 1992
*
*	Called by: "Delete" push button callback.
*
\*****************************************************************************/
int line_z_edit_delete (w, tag, list_data) /* dbs */
Widget		w;
int		*tag;
unsigned long   *list_data;

{
   int cnt;

   switch( find_xnum( &cnt ) ) 
   {
      case 1:    /* Position found - delete entire row */
		 /* and blank out Edit field           */

         delete_list_row( cnt );
	 line_z_edit_clear();
	 return( FALSE );

      case 0:		/* Error - Deleted item not found */
         sprintf(errmsg," Deleted conductor No. %s is not in list \n",
		XmTextGetString( widget_id( "line_z_edit_number_text" ) ) );

      case -1:		/* Error - No data in list */
         err.line = EM_LINE;
         err.type = INFO;
	 em_show( &err );
         return( TRUE );

   }

}


/**************************************************************************\
*
*	Function: find_xnum
*		  loops thur Conductor number list backwards until it passes
*		  the value it is seeking. Returns 1 if match, 0 if it
*		  passed the mark.
*
*	Authur: Bill Rogers	Sept 17, 1992	Modified:
*
*	Called by: line_z_edit_delete
*		   line_z_edit_insert
*		   line_z_edit_replace
*
\****************************************************************************/
int find_xnum ( int *pos )
{
   XmString *strlist;
   char *text, *xnum;

/** Transfer data from fields into character arrays */
   XtVaGetValues( w_cond_num, XmNitemCount, &num_cond, XmNitems, &strlist,NULL);
  
   if ( num_cond < 1 )
   {
     sprintf( errmsg, "*** INFO *** No .lcd data in list.\n" );
     err.type = INFO;
     *pos = 0;
     return( -1 );
   }

   xnum = XmTextGetString( widget_id( "line_z_edit_number_text" ) );

/** Search conductor list backwards for xnum */
   for (*pos = num_cond; *pos > 0; --*pos)
   {
      char *text;
      XmStringGetLtoR( strlist[*pos-1], XmSTRING_DEFAULT_CHARSET, &text);

      if ( cond_num_cmpr(text, xnum) < 0)	/* Passed beyond */
      {
         XtFree ( text );
         return ( 0 );
      }
      else if (cond_num_cmpr(text, xnum) == 0)	/* match */
      {
         XtFree ( text );
	 return ( 1 );
      }
   }

   *pos = 0;
   return ( 0 );
}

 

/*****************************************************************************\
*
*	Function: delete_list_row
*		  deletes a horizontal row of list items.		
*
*	Author: Bill Rogers	Sept 02, 1992	Modified: Sept 02, 1992
*
*	Called by: 
\*****************************************************************************/
void delete_list_row ( int cnt )
{

  if ( cnt < 0 || cnt> MAXCND) 
  {
     err.line = EM_LINE;
     sprintf( errmsg, "*** WARNING *** Program error - bad value for cnt (%d).\n                System crash averted.", cnt );
     err.type = WARNING;
     em_show( &err );
     return;
  }

  XmListDeletePos ( w_cond_num , cnt );
  XmListDeletePos ( w_phase_num, cnt );
  XmListDeletePos ( w_cond_name, cnt );
  XmListDeletePos ( w_resis    , cnt );
  XmListDeletePos ( w_diam     , cnt );
  XmListDeletePos ( w_skin     , cnt );
  XmListDeletePos ( w_horiz    , cnt );
  XmListDeletePos ( w_vtower   , cnt );
  XmListDeletePos ( w_vmid     , cnt );
  XmListDeletePos ( w_bundle   , cnt );
  XmListDeletePos ( w_separ    , cnt );
  XmListDeletePos ( w_angle    , cnt );

}


/*****************************************************************************\
*
*	Function: transf_list_row
*		  transfer all edit data to list boxes
*
*	Author: Bill Rogers	Sept 01, 1992	Modified: Sept 01, 1992
*
*	Called by: 
\*****************************************************************************/
void transf_list_row ( int cnt )
{
  char label_data[10];
  XmString motif_string;

  edit_to_list( "line_z_edit_number_text", w_cond_num  , cnt );
  edit_to_list( "line_z_edit_phase_text" , w_phase_num , cnt );
  edit_to_list( "line_z_edit_resist_text", w_resis     , cnt );
  edit_to_list( "line_z_edit_od_text"    , w_diam      , cnt );
  edit_to_list( "line_z_edit_skin_text"  , w_skin      , cnt );
  edit_to_list( "line_z_edit_horiz_text" , w_horiz     , cnt );
  edit_to_list( "line_z_edit_vtower_text", w_vtower    , cnt );
  edit_to_list( "line_z_edit_vmid_text"  , w_vmid      , cnt );
  edit_to_list( "line_z_edit_separ_text" , w_separ     , cnt );
  edit_to_list( "line_z_edit_angle_text" , w_angle     , cnt );

  motif_string = XmStringCreateSimple( wire_name );
  XmListAddItem ( w_cond_name, motif_string, cnt );
  XmStringFree( motif_string );

  motif_string = XmStringCreateSimple( num_bundle );
  XmListAddItem ( w_bundle, motif_string, cnt );
  XmStringFree( motif_string );

  highlight_list_row( cnt );

}


/*****************************************************************************\
*
*	Function: edit_to_list
*		  transfer edit box data to list box
*
\*****************************************************************************/
void edit_to_list ( char *edit_fld, Widget list_id, int cnt )
{
  char data[10];
  XmString motif_string;

  lookup_and_get_field( edit_fld, data, sizeof(data) );
 
  motif_string = XmStringCreateSimple( data );
  XmListAddItem   ( list_id, motif_string, cnt );
}

/*****************************************************************************\
*
*	Function: highlight_list_row
*		  make entire row highlighted 
*
*	Author: Bill Rogers	Sept 08, 1992	Modified: Sept 08, 1992
*       mod by: dlc dbs sep 10 remove callbacks to prevent uncontrolled looping.
*
*	Called by: clicking one of 13 list columns in line impd. calc. popup. 
\*****************************************************************************/
void highlight_list_row ( int cnt )
{


  hi_lite( "line_z_number_list"   , cnt ); 
  hi_lite( "line_z_phase_list"    , cnt ); 
  hi_lite( "line_z_type_list"     , cnt ); 
  hi_lite( "line_z_dia_resis_list", cnt ); 
  hi_lite( "line_z_dia_diam_list" , cnt ); 
  hi_lite( "line_z_skin_list"     , cnt ); 
  hi_lite( "line_z_bundle_list"   , cnt ); 
  hi_lite( "line_z_separ_list"    , cnt ); 
  hi_lite( "line_z_angle_list"    , cnt ); 
  hi_lite( "line_z_horiz_list"    , cnt ); 
  hi_lite( "line_z_vtower_list"   , cnt ); 
  hi_lite( "line_z_vmid_list"     , cnt ); 

}

void hi_lite( char *widget_name, int cnt )
{
  Boolean notify;
  XtRemoveAllCallbacks( widget_id( widget_name ), XmNsingleSelectionCallback ); 
  XmListSelectPos( widget_id( widget_name ), cnt, notify ); 
  XtAddCallback( widget_id( widget_name ),  
    XmNsingleSelectionCallback, (XtCallbackProc)line_z_list_number_cb, 0 ); 

}



/*****************************************************************************\
*
*	Function: line_z_edit_clear
*			Copies blanks into to the widget edit text boxes
*
*	Author: Bill Rogers	Sept 01, 1992	Modified: Sept 01, 1992
*
*	Called by: 
\*****************************************************************************/
void line_z_edit_clear ()

{
   
  lookup_and_fill_field( "line_z_edit_number_text", " ", 0, 0 );
  lookup_and_fill_field( "line_z_edit_phase_text" , " ", 0, 0 );
  set_optmenu_label    ( "line_z_wire_type_optmenu"," "       );
  lookup_and_fill_field( "line_z_edit_resist_text", " ", 0, 0 );
  lookup_and_fill_field( "line_z_edit_od_text"    , " ", 0, 0 );
  lookup_and_fill_field( "line_z_edit_skin_text"  , " ", 0, 0 );
  lookup_and_fill_field( "line_z_edit_horiz_text" , " ", 0, 0 );
  lookup_and_fill_field( "line_z_edit_vtower_text", " ", 0, 0 );
  lookup_and_fill_field( "line_z_edit_vmid_text"  , " ", 0, 0 );
  set_optmenu_label    ( "line_z_bundle_optmenu"  , " "       );
  lookup_and_fill_field( "line_z_edit_separ_text" , " ", 0, 0 );
  lookup_and_fill_field( "line_z_edit_angle_text" , " ", 0, 0 );

}



int set_std_cond_values (Widget w, XtPointer tag, XmAnyCallbackStruct *cbs ) 
/*****************************************************************************\
*
*	Purpose: 
*
*	Author: Bill Rogers	Aug 14, 1992	Modified: August 14, 1992
*
*	Called by: Conducter name cascade menu 
*
\*****************************************************************************/
{
   static char *dict_name[] = {"Pheasant", "Bittern", "Mallard", "Drake",  
                               "Dove",     "Ibis",    "Chukar",  "Thrasher", 
                               "Seahawk",  "Bunting"};
   
   static float dict_resis[] = {0.0722, 0.0738, 0.1183, 0.1180, 0.1687, 
                                0.2363, 0.0516, 0.397,  0.0497, 0.0787};

   static float dict_skin[]  = {0.333,  0.345,  0.286,  0.316,  0.333, 
                                0.316,  0.364,  0.387,  0.4138, 0.375};

   static float dict_diam[]  = {1.382,  1.345,  1.140,  1.108,  0.927, 
                                0.783,  1.602,  1.802,  1.603,  1.302};
  
   int i;
   char ctag[10];

   strncpy(ctag, tag, sizeof ctag);
   strncpy(wire_name, tag, sizeof ctag);
   printf("Set values for %s, ctag %s\n", tag, ctag );
   for (i = 0; i < sizeof dict_name / sizeof dict_name[0]; i++) {
      if (strcmp(ctag, dict_name[i]) == 0) {
         sprintf(ctag,"%6.4f", dict_diam[i]);
         XmTextSetString ( widget_id( "line_z_edit_od_text"     ), ctag);
         sprintf(ctag,"%6.4f", dict_skin[i]);
         XmTextSetString ( widget_id( "line_z_edit_skin_text"   ), ctag);
         sprintf(ctag,"%6.4f", dict_resis[i]);
         XmTextSetString ( widget_id( "line_z_edit_resist_text" ), ctag);
         return;
      }
   }
   /* Error - could not locate conductor in dict_name[]  */

   sprintf (errmsg, " Could not find conductor (%s) \n", ctag);
   err.line = EM_LINE;
   err.type = INFO;
   em_show(&err);
   return;
}


/*****************************************************************************\
*
*	Function: line_z_edit_bundle 
*
*	Author: Bill Rogers	Sept 11, 1992	Modified: Sept 11, 1992
*
*	Called by: Conducter name cascade menu 
*
\*****************************************************************************/
int line_z_edit_bundle (Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )

{
  strcpy( num_bundle, tag );
  printf("num_bundle set to %s\n", num_bundle );

  return(0);
}



/*****************************************************************************\
*       Function: lookup_and_fill_list.c
*
*       Author: Bill Rogers and Walt Powell Modified: Aug 25, 1992
*
*       Called by:
*
\*****************************************************************************/

void lookup_and_fill_list( char *fld_name, char string[][10], int size )
{
  Widget wid;
  XmString items[MAXCND];
  char *c;
  int i;


  for (i = 0; i < size; i++)
  {
     items[i] = XmStringCreate(string[i],XmSTRING_DEFAULT_CHARSET);
  }     

   if(( wid = widget_id( fld_name )) == FALSE) {
      return;
   }
   XmListAddItems( wid, items, size, 1);

}

void line_z_list_number_cb ( Widget w, XtPointer client_data, 
				XmListCallbackStruct *list_data)
/*****************************************************************************\
*
*	Purpose: 
*
*       This function transfers data from all list widgets to edit dialog.
*		
*	Author: Walt Powell		Modified: August 31, 1992
*
*	Called by the following list widget callbacks:
*
*                   "line_z_angle_list"
*                   "line_z_dia_resis_list"
*                   "line_z_vmid_list"
*                   "line_z_vtower_list"
*                   "line_z_horiz_list"
*                   "line_z_dia_dia_diam_list"
*                   "line_z_skin_list"
*
\*****************************************************************************/
{
   Widget id;
   int row;
   XmString *strlist;
   char *text;
 
   row = list_data->item_position;
   highlight_list_row( row );

   err.line = EM_LINE;

   list_to_edit( "line_z_edit_number_text", "line_z_number_list"   , row, 2 );
   list_to_edit( "line_z_edit_phase_text" , "line_z_phase_list"    , row, 1 );

   id = widget_id    ( "line_z_type_list" );
   XtVaGetValues     ( id, XmNitems, &strlist, NULL );
   XmStringGetLtoR   ( strlist[row-1], XmSTRING_DEFAULT_CHARSET, &text);
   set_optmenu_label ( "line_z_wire_type_optmenu", text );
   strcpy            ( wire_name, text );
   XtFree( text );

   list_to_edit( "line_z_edit_resist_text", "line_z_dia_resis_list", row, 6 );
   list_to_edit( "line_z_edit_od_text"    , "line_z_dia_diam_list" , row, 5 );
   list_to_edit( "line_z_edit_skin_text"  , "line_z_skin_list"     , row, 5 );
/*   list_to_edit( "line_z_edit_bundle_text", "line_z_bundle_list"   , row );*/

   id = widget_id   ( "line_z_bundle_list" );
   XtVaGetValues    ( id, XmNitems, &strlist, NULL );
   XmStringGetLtoR  ( strlist[row-1], XmSTRING_DEFAULT_CHARSET, &text);
   set_optmenu_label( "line_z_bundle_optmenu", text );
   strcpy           ( num_bundle, text );
   XtFree( text );

   list_to_edit( "line_z_edit_separ_text" , "line_z_separ_list"    , row, 3 );
   list_to_edit( "line_z_edit_angle_text" , "line_z_angle_list"    , row, 3 );
   list_to_edit( "line_z_edit_horiz_text" , "line_z_horiz_list"    , row, 6 );
   list_to_edit( "line_z_edit_vtower_text", "line_z_vtower_list"   , row, 3 );
   list_to_edit( "line_z_edit_vmid_text"  , "line_z_vmid_list"     , row, 3 );


}


/*****************************************************************************\
*
*	Function: list_to_edit
*	      This function transfers single data text item back to list widget
*		And inserts at row.
*		
*	Author: Bill Rogers	Aug 28, 1992	Modified: Aug 31, 1992
*
*	Called by: line_z_list_number_cb
*
\*****************************************************************************/
void list_to_edit( char *edit_fld, char *list_fld, int row, int length )
{
   Widget id;
   XmString *strlist;
   char *text;
   int u_bound;

   id = widget_id( list_fld );
   
   if ( id )	/* NOTE: infinite loop has occured if widget id is not found */
   {
     XtVaGetValues     ( id, XmNitems, &strlist, NULL );
     XmStringGetLtoR   ( strlist[row-1],XmSTRING_DEFAULT_CHARSET, &text);

     if( ( id = widget_id( edit_fld ) ))
     {
	clip_outside_blanks( text );
	XmTextSetString( id, text );
     }


     XtFree( text );

  }

}


/*****************************************************************************\
*	Function: set_optmenu_label
*
*	Author: Bill Rogers	Aug 31, 1992	Modified: Aug 31, 1992
*
*	Called by:
\*****************************************************************************/
void set_optmenu_label( char *widget_name, char *label )
{
  Widget menu_id;

  if( ( menu_id = widget_id( widget_name ) ) == NULL ) return;

  set_optmenu_label_id( menu_id, label );

}



/*****************************************************************************\
*	Function: set_optmenu_label_id
*		Variation of above - use menu_id instead of name
*
\*****************************************************************************/
void set_optmenu_label_id( Widget menu_id, char *label )
{
  Widget label_id;

  label_id = (Widget)XmOptionButtonGadget( menu_id );
  XtVaSetValues( label_id, 
                 XmNlabelString,
                 XmStringCreate( label, XmSTRING_DEFAULT_CHARSET),
		   NULL);

}



/*****************************************************************************\
*
*	Function: line_z_calc
*
*       This function performs the following:
*       1. Preliminary data checking,
*       2. Encode data into a message to ipf,
*       3. Decode the returned message from ipf, and
*       4. Display the results in the R, X, G, B fields.
*		
*	Author: Walt Powell/Bill Rogers	Aug 92		Modified: Sept 18, 92
*
*	Called by: "Calculate" push button callback.
*
\*****************************************************************************/
void line_z_calc ( Widget w, int *tag, unsigned long *list_data) /* dbs */

{
   char *bufstr1, word[20][USCANSIZE], out_buf[133], ufeet[9], *text;
   XmString *X_string;
   int cnt, error, popul[10], i, j, flag, count, nwrd, loc;
   Widget wid;
   double distance, basekv, basemva, min_clr;
   float sag0, sag1, xvalue, yvalue, cvtoenglish;

   memset(out_buf, '\0', sizeof(out_buf));
   bufstr1 = out_buf;	/* init the pointer */
   out_buf[0] = '\0';
   compute_lic = FALSE;   /* Default: values not computed */

/*********************** Obtain units **********************************/

   strcpy(ipf_buffer," UNITS = ");
   strcat(ipf_buffer, unit_type );
   if (strstr(unit_type, "ENGLISH") != NULL)
   {
      cvtoenglish = 1.0;
      strcpy( ufeet, "feet" );
   }
   else
   {
      cvtoenglish = 3.281; 
      strcpy( ufeet, "meters" );
   }

/* set up for possible error message */
   err.line = EM_LINE;
   err.type = FATAL;

/*********************** Obtain distance **********************************/

   lookup_and_get_field( "line_z_distance_text", bufstr1, sizeof(bufstr1) ); 
   
   distance = atof(bufstr1);
   if (distance <= 0.0)
   {
      sprintf (errmsg, " Line distance must be greater than %f \n",
	    distance);
      err.line = EM_LINE;
      em_show(&err);
      return;
   }
   
   strcat(ipf_buffer," DISTANCE = ");
   strcat(ipf_buffer, bufstr1);

/*********************** Obtain frequency *******************************/

   strcat(ipf_buffer," FREQUENCY = ");
   strcat(ipf_buffer, line_freq);

/*********************** Obtain basekv **********************************/

   err.line = EM_LINE;
   lookup_and_get_field( "line_z_basekv_text", bufstr1, sizeof(bufstr1) ); 
      
   basekv = atof(bufstr1);
   if (basekv <= 1.0)
   {
     sprintf (errmsg, " Basekv (%f) must be greater than 1.0\n", basekv);
     em_show(&err);
     return;
   }
   
   strcat(ipf_buffer," BASEKV = ");
   strcat(ipf_buffer, bufstr1);

/************************* Hard code basemva ****************************/

   strcat(ipf_buffer," BASEMVA = 100.0 \n");

/************ Transfer list contents to arrays ************/
   trfs_loop_i ( "line_z_number_list" , number, cnt );
   trfs_loop_i ( "line_z_phase_list"  , phase,  cnt );

/* Transfer conductor name text into name[] */
   wid = widget_id( "line_z_type_list" );
   
   XtVaGetValues(wid,XmNitemCount,&cnt,XmNitems,&X_string,NULL);
   for (; cnt > 0; --cnt) {
      XmStringGetLtoR(X_string[cnt-1],XmSTRING_DEFAULT_CHARSET, &text );
      strcpy( name[cnt-1], text);
     XtFree(text);
   }

/* Transfer conductor data  */
   trfs_loop_f ( "line_z_dia_resis_list", resis   , cnt );
   trfs_loop_f ( "line_z_dia_diam_list" , diameter, cnt );
   trfs_loop_f ( "line_z_skin_list"     , skin    , cnt );
   trfs_loop_f ( "line_z_vtower_list"   , vtower  , cnt );
   trfs_loop_f ( "line_z_vmid_list"     , vmid    , cnt );
   trfs_loop_f ( "line_z_horiz_list"    , horiz   , cnt );
   trfs_loop_i ( "line_z_bundle_list"   , numbnd  , cnt );
   trfs_loop_f ( "line_z_separ_list"    , separ   , cnt );
   trfs_loop_f ( "line_z_angle_list"    , angle   , cnt );

/************ Test for sequentially numbered conductors ***********/

   for (i = 0; i < sizeof popul / sizeof popul[0] ; popul[i++] = 0);
   XtVaGetValues( widget_id( "line_z_number_list" ),
	XmNitemCount,&cnt,NULL);		/* get cnt value back */

   error = FALSE;

   for (i = 0; i < cnt; i++) {
      if (number[i] != i+1) {
         sprintf (errmsg, " Conductor %d is not numbered sequentially \n", 
            i+1);
         err.line = EM_LINE;
         em_show(&err);
         error = TRUE;
      }
      j = numbnd[i];
      if (j == 0) j = 1;
      popul[phase[i]] = popul[phase[i]] + j;

   }

   if (popul[1] == 0) {
      sprintf (errmsg, " No phase 1 conductor data entered \n");
      err.line = EM_LINE;
      em_show(&err);
      return;
   }

/*************** Check for complete conductor sets [1-3], [4-6] **********/

   err.line = EM_LINE;
   if (popul[1] != popul[2] || popul[1] != popul[3]) 
   {
      sprintf (errmsg, " Phases 1-2-3 do not have equal number of \
conductors: %d %d %d \n", popul[1], popul[2], popul[3]);
      em_show(&err);
      error = TRUE;
   }
   if (popul[4] > 0) 
   {
      if (popul[4] != popul[5] || popul[4] != popul[6]) {
         sprintf (errmsg, " Phases 4-5-6 do not have equal number of \
conductors: %d %d %d \n", popul[4], popul[5], popul[6]);
         em_show(&err);
         error = TRUE;
      }
   }
   if( error ) return;

/* Ck for consistent resis, dia, skin, sep, and angle on conductors phases 0 */ 

   if (popul[0] > 1) 
   {
      err.line = EM_LINE;
      error = check_conductors( 0, 0, 0, cnt );
      if (error) return;
   }

/********** Check for consistent conductors phases [1-3] **********/ 

   if (popul[1] > 0) 
   {
      err.line = EM_LINE;
      error = check_conductors( 1, 2, 3, cnt );
      if (error) return;
   }

/********** Check for consistent conductors phases [4-6] if applicable *******/ 
 
   if (popul[4] > 0)
   {
      err.line = EM_LINE;
      error = check_conductors( 4, 5, 6, cnt );
      if (error) return;
   }

/********** Compare ground wire size with phase wire ***********/

   if (popul[0] > 0)
   {
      i = 0;
      while (i < cnt && phase[i] != 0) { i++; }
      j = i + 1;
      while (j < cnt && phase[j] != 0)
      {
         if (diameter[i] > diameter[j])
         {
            sprintf (errmsg, " Ground conductor size > phase conductor \
size \n\
 conductor No. %d Phase %d Resistance %f \n\
 conductor No. %d Phase %d Resistance %f \n", 
                number[i], phase[i], resis[i], number[j], phase[j], resis[j]);
            err.line = EM_LINE;
            em_show(&err);
            error = TRUE;
         }
         j++;
      }
      if (error) return;
   } 

/************************ Check for consistent wire sag  *********************/
   error = check_sag( 0, 0, 0, cnt );
   if ( error ) return;
   error = check_sag( 1, 2, 3, cnt );
   if ( error ) return;
   error = check_sag( 4, 5, 6, cnt );
   if ( error ) return;
               
/********** Check for ground clearance for compliance with ASC **********/
   min_clr = 58.0;	/* assume 1100kv */
   if ( basekv <= 765.0 ) min_clr = 45.0;
   if ( basekv <= 500.0 ) min_clr = 44.0;
   if ( basekv <= 345.0 ) min_clr = 43.0;
   if ( basekv <= 230.0 ) min_clr = 42.0;
   if ( basekv <= 115.0 ) min_clr = 38.0;

   min_clr /= cvtoenglish;

   for (i = 1; i < cnt; i++)
   {
      distance = ( 0.3334 * vtower[i] + 0.667 * vmid[i] ) * cvtoenglish;
      if ( distance < min_clr ) 
      {
         sprintf (errmsg, " Insufficient electrical ground clearance \n\
  conductor No. %d Phase %d Average height %6.2f \n\
  minimum allowable clearance is %6.2f %s.\n\n", 
            number[i], phase[i], distance, min_clr, ufeet );
         err.line = EM_LINE;
         em_show(&err);
         error = TRUE;
      }
   } 
 
/********** Check that all conductors have distinct (x,y) coordinates  */
   for (i = 0; i < cnt; i++)
   {
      for (j = i+1; j < cnt; j++)
      {
         distance = (vtower[i] - vtower[j]) * (vtower[i] - vtower[j]) 
                  + ( horiz[i] - horiz [j]) * ( horiz[i] - horiz [j]);
         if (fabs(distance) < 0.001)
         {
            sprintf (errmsg, 
            " Conductors %d and %d have same (x,y) coordinates \n", i+1, j+1 );
            err.line = EM_LINE;
            em_show(&err);
            return;
         }
      }
   }
   if (error) return;

/********** Check for horizontal symmetry on phase 0 (gnd) circuits *****/

   if (popul[0] > 1)	/* if gnd cond exists.... */
   {
      distance = 100000.0;
      i = 0;
      j = 0;
      while (i < cnt && phase[i] != 0) { i++; } 	/* loop to gnd cond */
      while (j < cnt )
      {
         if ( j != i && phase[j] == 0 )  {
               if (fabs (vtower[i] - vtower[j]) < distance ) {
                  distance = fabs (vtower[i] - vtower[j]);
               }
         }
         j++;
      }

     if (distance > 0.01)
     {
        sprintf (errmsg, " Ground conductors %d and %d have unequal \
tower heights %f and %f \n", phase[i], phase[j], vtower[i], vtower[j]);
        err.line = EM_LINE;
        em_show(&err);
        return;
     }
   }

/********** Check for maximum number of conductors <= MAXCND **********/

   count = 0;
   for (i = 0; i < sizeof popul / sizeof popul[0]; i++)
      count = count + popul[i];
   if (count > MAXCND)
   {
      sprintf (errmsg, " Total number of conductors %d exceeds program \
limit %d \n", count, MAXCND);
      err.line = EM_LINE;
      em_show(&err);
      return;
   }

/* Complete LINE_IMPEDANCE_CALCULATION message to ipf */

   for (i = 0; i < cnt; i++)
   {
      sprintf (bufstr1, "  CONDUCTOR = %d %6.4f %6.5f %6.3f %7.2f %7.2f %7.2f \
%6.2f %6.2f %d \n", phase[i], skin[i], resis[i], diameter[i], horiz[i], 
         vtower[i], vmid[i], separ[i], angle[i], numbnd[i]);
      strcat(ipf_buffer, bufstr1);
   }

   memset(out_buffer, '\0', sizeof(out_buffer));
   pfgetdata_cb( ipf_buffer, out_buffer ); 

   /* Parse out_buffer into out_buf */

   loc = 0;
   bufstr1 = &out_buffer[loc];
   while (*bufstr1 > 0)
   {
      if (strcspn(bufstr1, "\n") != 0 || 
          strcspn(bufstr1, "\n") == strlen(bufstr1)) 
             loc = strcspn(bufstr1, "\0");
      else 
             loc = strcspn(bufstr1, "\n");
      
      strncpy(out_buf, bufstr1, loc-1);
      if (strcmp (out_buf, "LIC = ") != 0)
      {
 
      /* Parse normal output  */

         uscan20 (out_buf, word, &nwrd, "~", "= ,\n");
         error = lookup_cp_to_field("line_z_z1_R_text", word[1], "%7.5f");
         error = lookup_cp_to_field("line_z_z1_X_text", word[2], "%7.5f");
         error = lookup_cp_to_field("line_z_y_G_text", word[3], "%7.5f");
         error = lookup_cp_to_field("line_z_y_B_text", word[4], "%7.5f");
         error = lookup_cp_to_field("line_z_z0_R_text", word[5], "%7.5f");
         error = lookup_cp_to_field("line_z_z0_X_text", word[6], "%7.5f");
         compute_lic = TRUE;
       
         /* Advance string pointer to next record */

         if (bufstr1[loc] > 0) 
            bufstr1 = &bufstr1[loc+1];
         else 
            bufstr1 = &bufstr1[loc];
         
      }
      else if (is_err_msg(out_buf) )
      {
        /* Parse error messages */

         err.line = EM_LINE;
         err.msg = bufstr1;
         err.type = FATAL;
         em_show(&err);
       
         /* Advance string pointer to end-of-record */

         loc = strcspn(bufstr1, "\0");
         bufstr1 = &bufstr1[loc];
      }
      else
      {
         printf (" PF records skipped \n %s", out_buf);
       
         /* Advance string pointer to next record */

         if (bufstr1[loc] > 0) 
            bufstr1 = &bufstr1[loc+1];
         else 
            bufstr1 = &bufstr1[loc];
         
      }
   }
   return;
}     


/***************************************************************************\
*	Function: check_conductors
*		  Checks conductor group data for matching values.
*
*	Author: Bill Rogers 	Sept 15, 1992	Modified: Sept 18, 1992
*
*	Called by: line_z_calc:
*
\***************************************************************************/
int check_conductors ( int p1, int p2, int p3, int cnt )
{
  int error = FALSE, i=0, j;

  while (i < cnt && phase[i] != p1) { i++; } 	/* skip up to 1st one */

  j = i;
  while (j < cnt && (phase[j] == p1 || phase[j]==p2 || phase[j]==p3 ) )
  {
     if (i != j) 
     {
        if (resis[i] != resis[j])
           error = output_cond_error( i, j, resis[i], resis[j], "resistance",
	     	"Resistance" );

        if (diameter[i] != diameter[j])
           error = output_cond_error( i, j, diameter[i], diameter[j], "diameter",
	     	"Diameter" );

        if (skin[i] != skin[j])
           error = output_cond_error( i, j, skin[i], skin[j], "T/C", "T/C" );

        if (separ[i] != separ[j])
           error = output_cond_error( i, j, separ[i], separ[j], "separation",
	     	"Separation" );

        if (angle[i] != angle[j])
           error = output_cond_error( i, j, angle[i], angle[j], "angle",
	     	     "Angle" );
     }
     j++;
   }

   return (error);
}

/****************************************************************************\
\****************************************************************************/
int output_cond_error( int i, int j, double v1, double v2, char *wrd,
							   char *wrd2 )
{
  err.type = FATAL;
  sprintf (errmsg, " Conductor %s should be equal.\n\
  conductor No. %d Phase %d %s %f \n\
  conductor No. %d Phase %d %s %f \n", 
      wrd,
      number[i], phase[i], wrd2, v1,
      number[j], phase[j], wrd2, v2 );

  em_show(&err);
  return ( TRUE );
}


/**************************************************************************\
*	Function: check_sag
*	 Check for consistent sag for conductors
*	 Restrictions:  Ground wire sag is ALWAYS called first.
*
*	Author W.Powell/B.Rogers	Sept 22, 1992	Modified:
*
*	Called by: line_z_calc
*
\**************************************************************************/
int check_sag( int p1, int p2, int p3, int cnt )
{
   int i, j, error = FALSE;
   float sag0, sag;

   sag = 0.0;
   for (i = 1; i < cnt; i++)
   {
      if (phase[i] == p1 || phase[i] == p2 || phase[i] == p3 )
      {
         if (vtower[i] > 0.0 && vmid[i] > 0.0)
         {
            sag = vtower[i] - vmid[i];
            j = i;
	    break;
         }
      }
   } 

   if ( sag > 0.0)
   { 
      for (i = 0; i < cnt; i++)
      {
         if ((phase[i] == p1 || phase[i] == p2 || phase[i] == p3) && (j != i) &&
            vtower[i] > 0.0 && vmid[i] > 0.0 &&
            vtower[i] - vmid[i] - sag > 0.01 * vtower[i] )
	 {
            sprintf (errmsg, " Inconsistent sag (V_tower - V_mid) \n\
 conductor No. %d Phase %d Vtower %f Vmid %f \n\
 conductor No. %d Phase %d Vtower %f Vmid %f \n", 
                     number[i], phase[i], vtower[i], vmid[i],
                     number[j], phase[j], vtower[j], vmid[j]);
            err.line = EM_LINE;
            em_show(&err);
            error = TRUE;
         }
      }

      if ( p1 == 0 ) sag0 = sag;

      if ( sag < sag0 && p1 != 0 )
      {
         sprintf (errmsg, " Sag of ground wires (%f) exceeds sag of \
phases [%d-%d] (%f) \n", p1, p3, sag0, sag);
         err.line = EM_LINE;
         em_show(&err);
         error = TRUE;
      }
      if (error) return( error );
   }

/* Complete values for vtower[] and vmid[] using sag  */

   for (i = 0; i < cnt; i++)
   {
     if (phase[i] == p1 || phase[i] == p2 || phase[i] == p3 )
     {
        if (vtower[i] == 0.0)
	   vtower[i] = vmid[i] + sag;
        else if (vmid[i] == 0.0)
	   vmid[i] = vtower[i] - sag;
     }
  }
  return ( FALSE );
}



/****************************************************************************
*	Function: line_z_ok_callback
*
*	Author: Bill Rogers 	Sept 17, 1992	Modified:
*
*	Called by: "Accept" pushbutton
*
\****************************************************************************/
void line_z_ok_callback( Widget w, int *tag, unsigned long *list_data )
{
  char string[20];

  lookup_and_get_field ( "line_z_z1_R_text", string, sizeof(string) );
  lookup_and_fill_field( "line_R",  string, 1, 7 );

  lookup_and_get_field ( "line_z_z1_X_text", string, sizeof(string) );
  lookup_and_fill_field( "line_X",  string, 1, 7 );

  lookup_and_get_field ( "line_z_y_G_text", string, sizeof(string) );
  lookup_and_fill_field( "line_G",  string, 1, 7 );

  lookup_and_get_field ( "line_z_y_B_text", string, sizeof(string) );
  lookup_and_fill_field( "line_B",  string, 1, 7 );


}


/***************************************************************************\
*	Function trans_loop
*
*	Author: Bill Rogers	Sept 18, 92 	Modified: 
*
*	Called by line_z_calc
*
\***************************************************************************/
void trfs_loop_i( char *list_fld_name, int array[MAXCND], int cnt )
{
   Widget wid;
   XmString *X_string;

   if(( wid = widget_id( list_fld_name )) == FALSE)  return;

   XtVaGetValues(wid,XmNitemCount,&cnt,XmNitems,&X_string,NULL);
   for (; cnt > 0; --cnt)
   {
      char *text = 0;
      XmStringGetLtoR(X_string[cnt-1],XmSTRING_DEFAULT_CHARSET, &text);
      array[cnt-1] = atoi(text);
      XtFree(text);
   }

}




/***************************************************************************\
*	Function trans_loop
*
*	Author: Bill Rogers	Sept 18, 92 	Modified: 
*
*	Called by line_z_calc
*
\***************************************************************************/
void trfs_loop_f( char *list_fld_name, float array[MAXCND], int cnt )
{
   Widget wid;
   XmString *X_string;

   if(( wid = widget_id( list_fld_name )) == FALSE)  return;

   XtVaGetValues(wid,XmNitemCount,&cnt,XmNitems,&X_string,NULL);
   for (; cnt > 0; --cnt)
   {
      char *text = 0;
      XmStringGetLtoR(X_string[cnt-1],XmSTRING_DEFAULT_CHARSET, &text);
      array[cnt-1] = atof(text);
      XtFree(text);
   }

}
