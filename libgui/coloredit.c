static char sccsid[]="@(#)coloredit.c	1.16 9/29/97";
/*****************************************************************************\
* file: color_edit.c
* author: Bill Rogers	Dec 1995
* purpose: callback functions for interfacing with power flow
*
* Modified to also work with 16-bit color depth:  3-June-2001 J.G. Schaad
*
\*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <Xm/Xm.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>

#include "ipf_ipc.h"
#include "dmgr.h"
#include "graph_data.h"
#include "em.h"
#include "base_out.h"
#include "EdgeG.h"
#include "base_data.h"
#include "coord_data.h"
#include "ipfdebug.h"
#include "define.h"

#ifdef VMS
#include  <unixlib.h>
#include <descrip.h>
#endif

extern void lookup_and_get_field( char *, char *, int );
extern void lookup_and_fill_field( char *, char *, int, int );
extern Widget widget_id		( char *);
extern change_bus_icon_bg( unsigned long bkgrd, unsigned long frgrd  );
extern char *cstring_to_string  ( XmString motif_string );
extern Vuit_MU			( char *, char * );

void init_color_dialog	( Widget w,XtPointer *tag,XmAnyCallbackStruct *);
void edit_colorcell	( Widget w, XtPointer *tag, XtPointer *call_data );
void adjust_color	( Widget w,XtPointer *tag, XtPointer *cb_data);
void foreground_toggle	( Widget w,XtPointer *tag,XmAnyCallbackStruct *);
void save_colors	( XColor *colr );
void enable_slider_color_editing( Boolean value);
void fill_16_color_form	( Widget w,XtPointer *tag,XmAnyCallbackStruct *);
void get_all_colors	( );
void colormap_create	( );
void get_colors		( );
FILE *get_xgui_ptr	( char * );

em_init                 /* special initialization for error message box */
                        /* must be in every file using this.            */

static int private_color_limit = 41;
static Widget w1;	/* draw window widget */
static XColor color;
static XColor scolor_reset;
static unsigned long org_pixel;
static Widget pb_wid = NULL;	/* calling widget */
static Boolean main_window = FALSE;
static Widget c_dpy;
static Colormap wmap;
static long c1=0;	/* number of 1st colorcell in 16-color display */
static int max_clr_cells;
static int clr_inc, clr_in2, clr_in3;

static unsigned long r65535[65536];
static unsigned long g65535[65536];
static unsigned long b65535[65536];


void init_color_dialog( Widget w, XtPointer *tag,XmAnyCallbackStruct *cb_data)
/**************************************************************************\
* Purpose: Bring up color edit dialog - ensuring 16-color form is displayed.
*
* Called by: Any of 12 kv color pushbuttons in main display
\**************************************************************************/
{
  XColor colr_fg, colr_bg;
  XmString label;
  char wstr[128];
  char *pcmmt;
  int frgd_pixel, bkgd_pixel;

  /* colormap_create();  doesn't work - use below instead */
  get_colors();

  get_all_colors( );

  Vuit_MU( "M", "colorcell_edit_kv_pb" );
  Vuit_MU( "M", "color_edit_colorcells" );
  Vuit_MU( "U", "color_edit_slider_box" );
  Vuit_MU( "U", "colorcell_show_pb" );

	/* ensure that "SHOW COLOR" button didn't turn sliders off */
  enable_slider_color_editing( TRUE );

/*  XtSetSensitive( widget_id("background_tb"),	 TRUE );
  XtSetSensitive( widget_id("foreground_tb"),	 TRUE );*/

  pb_wid = w;

  if( w == widget_id( "kv_dc_label" ) )
    w1 = widget_id("drawwindow");
  else
    w1 = pb_wid;


  wmap = DefaultColormapOfScreen( XtScreen( w1 )); /* get ptr to colormap */

/* get colorcell (colr.pixel) associated with pushbutton background color */
  XtVaGetValues( w1, XmNforeground, &colr_fg.pixel, NULL ); 
  XtVaGetValues( w1, XmNbackground, &colr_bg.pixel, NULL ); 

  if( w == widget_id( "kv_dc_label" ) )
  {
    sprintf( wstr,
	"This is present Main Window color" );
    label = XmStringCreate((char *) wstr, XmSTRING_DEFAULT_CHARSET);
    main_window = TRUE;
  }
  else
  {
    XtVaGetValues( pb_wid, XmNlabelString, &label, NULL );
    pcmmt = cstring_to_string(label);
    sprintf( wstr, "This is present %s color",
	pcmmt );
    label = XmStringCreate((char *)wstr, XmSTRING_DEFAULT_CHARSET);

    main_window = FALSE;
  }

  XtVaSetValues( widget_id("color_edit_present_color_lbl"),
		 XmNlabelString, label, NULL );

  XtVaSetValues( widget_id("color_edit_present_color_lbl"), XmNbackground,
	 colr_bg.pixel, NULL ); 
  XtVaSetValues( widget_id("color_edit_present_color_lbl"), XmNforeground,
	 colr_fg.pixel, NULL ); 

  sprintf( wstr, "Background color # %d", colr_bg.pixel );
  label = XmStringCreate((char *)wstr, XmSTRING_DEFAULT_CHARSET);
  XtVaSetValues( widget_id("color_edit_backgrd_lbl"),
		 XmNlabelString, label, NULL );

  sprintf( wstr, "Foreground color # %d", colr_fg.pixel );
  label = XmStringCreate((char *)wstr, XmSTRING_DEFAULT_CHARSET);
  XtVaSetValues( widget_id("color_edit_foregrd_lbl"),
		 XmNlabelString, label, NULL );
 

/*  if( main_window )
  {
    XtVaSetValues( pb_wid, XmNbackground, colr_bg.pixel, NULL ); 
  }
*/
	/* Now find the colorcell's RED,GREEN and BLUE values */
  XQueryColor( XtDisplay( w1 ), wmap, &colr_bg );
/*  printf("\nBackground colorcell: %d RGB(%d,%d,%d) %d\n",
		 colr_bg.pixel, colr_bg.red, colr_bg.green, colr_bg.blue, colr_bg.flags );  */

  if( (int)XmToggleButtonGadgetGetState( widget_id("background_tb") ) )
  {
    save_colors( &colr_bg ); 	/* also set SLIDER values */
  }

	/* Change color_label colorcell ptr to match button colorcell */
  c_dpy = widget_id( "color_display_label" );
  XtVaSetValues( c_dpy, XmNbackground, colr_bg.pixel, NULL ); 

/**************************************************************************\
\*******************   NOW DO FOREGROUND   ********************************/

  XtVaGetValues( w1, XmNforeground, &colr_fg.pixel, NULL );  /*get colorcell #*/

  XQueryColor( XtDisplay( w1 ), wmap, &colr_fg );	/* Get colorcell RGB  */
/*  printf("Foreground colorcell: %d RGB(%d,%d,%d) %d\n",
		 colr_fg.pixel, colr_fg.red, colr_fg.green, colr_fg.blue, colr_fg.flags ); */

  if( (int)XmToggleButtonGadgetGetState( widget_id("foreground_tb") ) )
  {
    save_colors( &colr_fg );
  }

	/* Change color_label colorcell ptr to match button colorcell */
  XtVaSetValues( c_dpy, XmNforeground, colr_fg.pixel, NULL ); 

  fill_16_color_form( 0, 0, 0);
}


void save_colors( XColor *colr )
/************************************************************************\
*  Purpose: Save Backgrd/Foregrd colors and set slider values
*
*
\************************************************************************/
{
  char num_str[8];

  org_pixel = colr->pixel;	/* in case of reset */

    color.red   = colr->red;	/* for slider adjust later */
    color.green = colr->green;
    color.blue  = colr->blue;
    color.pixel = colr->pixel;
    scolor_reset.red   = colr->red;
    scolor_reset.green = colr->green;
    scolor_reset.blue  = colr->blue;
    XtVaSetValues( widget_id("red_scale"  ), XmNvalue, colr->red>>8,   NULL ); 
    XtVaSetValues( widget_id("green_scale"), XmNvalue, colr->green>>8, NULL ); 
    XtVaSetValues( widget_id("blue_scale" ), XmNvalue, colr->blue>>8,  NULL ); 

    sprintf( num_str, "%d", colr->pixel );
    /*lookup_and_fill_field("colorcell_num_txt", num_str, 0, 3 );*/
}

void enable_slider_color_editing( Boolean value)
{
  XtSetSensitive( widget_id("red_scale"  ),	 value );
  XtSetSensitive( widget_id("green_scale"),	 value );
  XtSetSensitive( widget_id("blue_scale" ),	 value );
  XtSetSensitive( widget_id("color_editor_reset_pb"), value );
}


void foreground_toggle(Widget w, XtPointer *tag,XmAnyCallbackStruct *cb_data)
/**************************************************************************\
*
* Called by: foreground toggle button on color editor
\**************************************************************************/
{
  XColor clr;

	/* ensure action is taken - only when button goes "ON" */
  if( (int)XmToggleButtonGadgetGetState( widget_id("foreground_tb") )) 
  {
	printf("set foreground values\n");
    XtVaGetValues( pb_wid, XmNforeground, &clr.pixel, NULL );
  }
  else
  {
	printf("set background values\n");
    XtVaGetValues( pb_wid, XmNbackground, &clr.pixel, NULL );
  }

  XQueryColor( XtDisplay( w1 ), wmap, &clr );

  XtVaSetValues( widget_id("red_scale"  ), XmNvalue, clr.red>>8,   NULL ); 
  XtVaSetValues( widget_id("green_scale"), XmNvalue, clr.green>>8, NULL ); 
  XtVaSetValues( widget_id("blue_scale" ), XmNvalue, clr.blue>>8,  NULL ); 

}


void adjust_color( Widget w, XtPointer *tag, XtPointer *call_data)
/**************************************************************************\
* Purpose: Read slider and adjust the color cell accordingly
*	Note: global color.pixel MUST be set earlier!
*
* Called by: Any of three RGB color slider bars.
\**************************************************************************/
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;
  char rgb_key;
  static last_rgb_key='0';
  char *pctag;
  unsigned long backgd, foregd;
  char num_str[8];

  int c;
  static int n_watch = 0;	/* number of colors in color watch 	*/
  static int c_watch[65536];	/* list of colorcells to watch out for	*/

  pctag = (char *)tag;
  rgb_key = pctag[0];

/*************************************************************************\
*  Problems with "color traps" - user might accidentally change color
*	to an existing color, and MOTIF will "lock" the two. i.e.
*	One colorcell will be closed up.
*  This is suplus coding - because I can't correct MOTIF!
\*************************************************************************/

  if( rgb_key != last_rgb_key )
  {
    n_watch = 0;
    switch(rgb_key)
    {
      case ('R'):
 	for( c=0; c<65536; c++ )	/* sliding red - look for green or blue */
        {
	   if( g65535[c] == color.green && b65535[c] == color.blue )
	     c_watch[++n_watch] = r65535[c];	/* this red not allowed */
	}
      break;

      case ('G'):
 	for( c=0; c<65536; c++ )	/* sliding green - look for red or blue */
        {
	   if( r65535[c] == color.red && b65535[c] == color.blue )
	     c_watch[++n_watch] = g65535[c];	/* this green not allowed */
	}
      break;

      case ('B'):
 	for( c=0; c<65536; c++ )	/* sliding blue - look for red or green */
        {
	   if( r65535[c] == color.red && g65535[c] == color.green )
	     c_watch[++n_watch] = b65535[c];	/* this blue not allowed */
	}
      break;

      default:
        printf("Error - color not set!\n");

    }
/* printf("WARNING - %d colorcells are potential color traps!!\n", n_watch); */
    last_rgb_key=rgb_key;
  }

  else if( n_watch )	/* do search through list to avoid color trap */
  {
    for( c=n_watch; c<=n_watch; c++ )
    {
      if( c_watch[c] == cbs->value )
      { /* printf("This color is a duplicate!!\n" ); */
	return;
      }
    }
  }

/**********************************************************************\
* Passed checks - change the color cell
\**********************************************************************/

  switch(rgb_key)
  {
    case ('R'):
      color.red	   = cbs->value<<8;	/* mult by 256 */
    break;

    case ('G'):
      color.green = cbs->value<<8;
    break;

    case ('B'):
      color.blue  = cbs->value<<8;
    break;
  }

/* printf("ADJUST_COLOR called (%c %d) %d (%d,%d,%d) %d\n", rgb_key, 
	 cbs->value, color.pixel, color.red, color.green, color.blue,
	color.flags ); */

  if( color.pixel>private_color_limit )	/* don't free a PRIVATE color */
    XFreeColors( XtDisplay( pb_wid ), wmap, &color.pixel, 1, 0 );

  XAllocColor( XtDisplay( pb_wid ), wmap, &color );

  sprintf( num_str, "%d", color.pixel );
  /*lookup_and_fill_field("colorcell_num_txt", num_str, 0, 3 );*/


  if( (int)XmToggleButtonGadgetGetState( widget_id("foreground_tb") ) )
  {
    XtVaSetValues( c_dpy, XmNforeground, color.pixel, NULL );
    XtVaSetValues( pb_wid, XmNforeground, color.pixel, NULL );
    foregd = color.pixel;
    if( main_window == TRUE )
      XtVaSetValues( w1, XmNforeground, color.pixel, NULL );
  }
  else
  {
    XtVaSetValues( c_dpy, XmNbackground, color.pixel, NULL );
    XtVaSetValues( pb_wid, XmNbackground, color.pixel, NULL );
    backgd = color.pixel;
    if( main_window == TRUE )
      XtVaSetValues( w1, XmNbackground, color.pixel, NULL );
  }

/*  if( main_window == TRUE )		NEEDS WORK!
    change_bus_icon_bg( backgd, foregd ); */
}




void edit_colorcell( Widget w, XtPointer *tag, XtPointer *call_data)
/**************************************************************************\
* Purpose: Set kv color to selected colorcell
*	   Set up sliderbar color for possible editing	  
*
* Called by: Any of 16 color cells
\**************************************************************************/
{
  XColor clr;
  char *str;
  char wstr[128];
  XmString label;

  /*lookup_and_get_field( "colorcell_num_txt", str, 4 );*/
  XtVaGetValues( w, XmNbackground, &clr.pixel, NULL );

  if( clr.pixel>=max_clr_cells )
  {
    printf("Too high!\n");
    return;
  }

  sprintf( wstr, "Adjusting colorcell # %d", clr.pixel );
  label = XmStringCreate((char *)wstr, XmSTRING_DEFAULT_CHARSET);
  XtVaSetValues( widget_id("color_dialog_slider_label"),
		 XmNlabelString, label, NULL );

  XQueryColor( XtDisplay( w1 ), wmap, &clr );

/*  printf("\nBackground colorcell: %d RGB(%d,%d,%d) %d\n",
		 clr.pixel, clr.red, clr.green, clr.blue, clr.flags );  */

  XtVaSetValues( c_dpy, XmNbackground, clr.pixel, NULL );
  XtVaSetValues( c_dpy, XmNforeground, clr.pixel, NULL );

  scolor_reset.red   = clr.red;	/* so RESET on sliders will work */
  scolor_reset.green = clr.green;
  scolor_reset.blue  = clr.blue;

  color.red   = clr.red;	/* for slider adjust later */
  color.green = clr.green;
  color.blue  = clr.blue;
  color.pixel = clr.pixel;
  XtVaSetValues( widget_id("red_scale"  ), XmNvalue, clr.red>>8,   NULL ); 
  XtVaSetValues( widget_id("green_scale"), XmNvalue, clr.green>>8, NULL ); 
  XtVaSetValues( widget_id("blue_scale" ), XmNvalue, clr.blue>>8,  NULL ); 

/*
*  if( color.pixel < private_color_limit )
*  {
*    enable_slider_color_editing( FALSE );
*    Vuit_MU( "M", "Warning_not_editable_lbl" );
*  }
*  else
*  {
*    enable_slider_color_editing( TRUE );
*    Vuit_MU( "U", "Warning_not_editable_lbl" );
*  }
*/

  if( (int)XmToggleButtonGadgetGetState( widget_id("foreground_tb") ) )
  {
    XtVaSetValues( pb_wid, XmNforeground, clr.pixel, NULL );
  }
  else
  {
    XtVaSetValues( pb_wid, XmNbackground, clr.pixel, NULL );
  }
}


void color_reset( Widget w, XtPointer *tag,XmAnyCallbackStruct *cb_data)
/**************************************************************************\
* Purpose: reset the color sliders to position set by init routine
*
* Called by: RESET button
\**************************************************************************/
{
  XColor colr;

/*  printf("Back to orginal color cell: %d\n", org_pixel ); */
  colr.pixel = org_pixel;

  if( colr.pixel>private_color_limit )	/* don't free a PRIVATE color */
    XFreeColors( XtDisplay( pb_wid ), wmap, &color.pixel, 1, 0 );

  colr.red   = scolor_reset.red;
  colr.green = scolor_reset.green;
  colr.blue  = scolor_reset.blue;

  XtVaSetValues( widget_id("red_scale"  ), XmNvalue, colr.red>>8,   NULL ); 
  XtVaSetValues( widget_id("green_scale"), XmNvalue, colr.green>>8, NULL ); 
  XtVaSetValues( widget_id("blue_scale" ), XmNvalue, colr.blue>>8,  NULL ); 

  XAllocColor( XtDisplay( pb_wid ), wmap, &colr );

  if( (int)XmToggleButtonGadgetGetState( widget_id("foreground_tb") ) )
  {
    XtVaSetValues( c_dpy, XmNforeground, colr.pixel, NULL );
    XtVaSetValues( pb_wid, XmNforeground, colr.pixel, NULL );
  }
  else
  {
    XtVaSetValues( c_dpy, XmNbackground, colr.pixel, NULL );
    XtVaSetValues( pb_wid, XmNbackground, colr.pixel, NULL );
  }

}



FILE *get_xgui_ptr( char *xgui_file_name )
/******************************************************************\
*
\******************************************************************/
{
  char home_dir[80];
  long llen;
  int len;
  FILE *xgui_ptr;

#ifdef VMS
  char *p;
  p = getenv ("SYS$LOGIN");
  strcpy(home_dir, p);                             /* Pointer to string value */

#elif DBOS
  strcpy (&home_dir[0], "c:\\");

#else
  char *p;
  p = getenv ("HOME");
  strcpy(home_dir, p);                             /* Pointer to string value */
  strcat(home_dir, "/" );
#endif

/*  cgetdir( &home_dir[0], &llen );      Get home directory */

  if( ( len = strlen( xgui_file_name ) ) == 0 )
  {
    strcpy( xgui_file_name, &home_dir[0] );
#ifdef VMS
    strcat( xgui_file_name, "XGUI.DAT" );       /* VAX */
#elif DBOS
    strcat( xgui_file_name, "XGUI.DAT" );
#else
    strcat( xgui_file_name, "XGUI" );           /* UNIX */
#endif

  }

  if( !strstr( xgui_file_name, "XGUI" ) )
  {
    printf("Home directory does not have \"XGUI\" in name. (XGUI Save Canceled)\
n" );
    return(NULL);
  }

#ifdef VMS
  if(( xgui_ptr = fopen(xgui_file_name, "r")) == NULL)
#else
  if(( xgui_ptr = fopen(xgui_file_name, "r+")) == NULL)
#endif
  {
    printf("Need OLD XGUI file to get data for NEW XGUI file!\n" );
    printf("Old file does not exist.\n");
    return(NULL);
  }
  return xgui_ptr;
}



void next_color_block( Widget w, XtPointer *tag,XmAnyCallbackStruct *cb_data )
{
  c1 += clr_in3;
  if( c1>=(max_clr_cells-1) ) c1 = max_clr_cells-3*(clr_in2+clr_inc)-1;
  fill_16_color_form( 0, 0, 0 );
}

void prev_color_block( Widget w, XtPointer *tag,XmAnyCallbackStruct *cb_data )
{
  c1 -= clr_in3;
  if( c1<0 ) c1 = 0;
  fill_16_color_form( 0, 0, 0 );
}

void fill_16_color_form( Widget w, XtPointer *tag,
				XmAnyCallbackStruct *cb_data )
/***********************************************************************\
* Purpose: Change form from Slider bar to 16-color form.
*
* Called by: "Use Colorcell Method" pushbutton
\***********************************************************************/
{
  Widget wid;
  XColor clr;
  XmString x_str;
  int bx;
  int m;
  long c2;
  char ccell[4];

  Vuit_MU( "U", "color_edit_slider_box" );
  Vuit_MU( "M", "color_edit_colorcells" );
  Vuit_MU( "U", "colorcell_show_pb" );
  Vuit_MU( "M", "colorcell_edit_kv_pb" );

  m = 0;
  for( bx=0; bx < 16 ; bx++ )	/* to fill 16 boxes */
  {
    c2 = c1 + m*clr_inc + (bx/4)*clr_in2;
    clr.pixel = c2;		/* get this color cell number */

    XQueryColor( XtDisplay( w1 ), wmap, &clr );

    sprintf( ccell, "cc%d", bx );	/* get wid id */
    wid = widget_id( ccell );

    XtVaSetValues( wid, XmNbackground, clr.pixel, NULL );	/* set color */
    /*XtVaSetValues( wid, XmNforeground, clr.pixel, NULL );*/

    sprintf( ccell, "%d", clr.pixel );	/* write color # in label */
    x_str =  XmStringCreateLtoR( ccell, XmSTRING_DEFAULT_CHARSET);
    XtVaSetValues( wid, XmNlabelString, x_str, NULL);

    m++;
    if( m==4 )
    {
      m = 0;
    }
  }
}

void get_all_colors( )
/**************************************************************************\
* Purpose: get RGB colors of all 256 colorcells for later referrence
*
\**************************************************************************/
{
  int c;
  XColor colr;

  w1 = widget_id("drawwindow");
  wmap = DefaultColormapOfScreen( XtScreen( w1 )); /* ptr to colormap */

  for( c=0; c<256; c++ )
  {
    colr.pixel = (unsigned long)c;
    XQueryColor( XtDisplay( w1 ), wmap, &colr );

    r65535[c] = colr.red;
    g65535[c] = colr.green;
    b65535[c] = colr.blue;

  }
/*printf("%4d %5d %5d %5d\n", colr.pixel, colr.red, colr.green, colr.blue );*/

}

/* NOT WORKING CORRECTLY 
void colormap_create()
{
  Visual *visual;
  XStandardColormap best_map_info;
  int count, screen_num=0;
  Window root;
  Display *dpy;

  dpy = XtDisplay( widget_id("drawwindow") );
  visual = DefaultVisual( dpy, screen_num );

  root = RootWindow( dpy, screen_num );

   Not fully debugged yet - XGetRBG won't compile on VAX
  if( XGetRGBColormaps( dpy, root,
	&best_map_info, &count, XA_RGB_BEST_MAP ) == 0 )
  {
     printf("RGB_BEST_MAP colormap property not set.\n");
    get_colors();
  }
  else if ( best_map_info.colormap == 0 )
  {
     printf("RGB_BEST_MAP colormap property is set.\n");
     printf("  But missing data.\n");
  }


    get_colors();	/* temp patch for above 
}
*/

void get_colors()
{
  int default_depth, screen_num = 0;
  Display *dpy;
  Colormap default_cmap;
  unsigned long foreground, background_pixel;
  XmString label;
  char wstr[128];

  dpy = XtDisplay( widget_id("drawwindow") );

  default_depth = DefaultDepth   ( dpy, screen_num );
  printf("Color map depth is: %d\n", default_depth );
  max_clr_cells = (int)(pow( 2, default_depth ));

  clr_inc = (int)(max_clr_cells/256) ;
  if( default_depth == 8 )	/* this process uses color palette */
  { 
    clr_inc = 1;
    clr_in2 = 4;
    clr_in3 = 16;
  }
  else if ( default_depth == 16 )	/* 256 shades of each color */
  {
    clr_inc = 85;	/* i.e. 256/3 for 3 steps red color increase */
    clr_in2 = 21760;	/* (65535-255)/3 for 3 steps green color increase */
    clr_in3 = 557056;	/*  for 3 steps blue color increase */
  }
  else
  {
    printf("Unable to handle %d color depth system\n", default_depth );
  }

  sprintf( wstr, "Your system supports %d colors", max_clr_cells  );
  label = XmStringCreate((char *)wstr, XmSTRING_DEFAULT_CHARSET);
  XtVaSetValues( widget_id("color_edit_label"),
		 XmNlabelString, label, NULL );

  default_cmap  = DefaultColormap( dpy, screen_num );

/*
  if(XAllocColorCells (display, default_cmap, False, plane_masks,
	1, colors, 2) == 0 
  {
    if( (XAllocColorCells (display, default_cmap, False,
	plane_masks, 0, colors, 3 ) == 0 )
    {
      background_pixel = WhitePixel(display, screen_num);
      foreground = BlackPixel(display, screen_num);
      printf("using black and white.\n");
    }
  }
*/
}
