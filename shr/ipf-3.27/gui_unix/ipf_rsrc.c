static char sccsid[]="@(#)ipf_rsrc.c	20.10 9/14/95";
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
/***
* file ipf_rsrc.c
* purpose:
*  read the command line parameters and parse them for
*  recognizeable strings.
*  Normally the resources can be read by the x application
*  which permits them to be set in the default files
*  but VMS and some other operating systems may have
*  trouble with the xt call so a backup command 
*  parser is installed.
****/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <Xm/Xm.h>
#include <X11/X.h>
#include "ipf_ipc.h"
#include "ipf_rsrc.h"
#ifndef VMS
#include <unistd.h>
#endif

int graphorder = 0;

void print_resources	( AppResData *app_resdata );

int  getResources(AppResData *app_resdata, int argc, char *argv[])
/***********************************************************************\
* this routine must be called before the
* motif window manager is called since it (motif) will
* clear out the argc and argv params
*
*				 See getXtResources for UNIX version.
* Called by: main (gui.c)
*
\***********************************************************************/
{
  int i;
  char *s, *d;
  char arg_low[120];

  /** set defaults **/
  app_resdata->autostart  = XgDefaultAutostart;
  app_resdata->casename	  = XgDefaultCasename;
  app_resdata->debug	  = XgDefaultDebug;
  app_resdata->graphorder = XgDefaultGraphorder;
  app_resdata->node	  = XgDefaultNode;
  app_resdata->server	  = XgDefaultServer;
  app_resdata->servername = XgDefaultServername;
  app_resdata->socketid	  = XgDefaultSocketid;
  app_resdata->sleep	  = XgDefaultSleep;

  for( i = 1; i < argc; i++)
  {
    for( d = arg_low, s = argv[i]; ( *d = tolower(*s) ) != '\0'; d++, s++ );
    if(      !strcmp(  arg_low, "-servername"  ) )      {
      i++; app_resdata->servername = argv[i];           }
    else if( !strncmp( arg_low, "-socket", 7   ) )      {
      i++; app_resdata->socketid = atoi( argv[i] );     }
    else if( !strcmp(  arg_low, "-node"        ) )      {
      i++; app_resdata->node = argv[i];                 }
    else if( !strcmp(  arg_low, "-sleep"       ) )      {
      i++; app_resdata->sleep = atoi( argv[i] );        }
    else if( !strcmp(  arg_low, "-debug"       ) )      {
      i++; app_resdata->debug = atoi( argv[i] );        }
    else if( !strcmp(  arg_low, "-server"      ) )      {
      app_resdata->server=0;                            }
    else if( !strcmp(  arg_low, "+server"      ) )      {
      app_resdata->server=1;                            }
    else if( !strncmp( arg_low, "-auto", 5     ) )      {
      app_resdata->autostart = 0;                       }
    else if( !strncmp( arg_low, "+auto", 5     ) )      {
      app_resdata->autostart = 1;                       }
    else if( !strncmp( arg_low, "-graph", 5     ) )      {
      app_resdata->graphorder = 0;                       }
    else if( !strncmp( arg_low, "+graph", 5     ) )      {
      app_resdata->graphorder = 1;                       }
  }

  if(1 == app_resdata->debug)
  {
    print_resources( app_resdata );
  }

  graphorder = app_resdata->graphorder;
  return(0);
}


int  getXtResources(AppResData *app_resdata, Widget toplevel_widget)
/************************************************************************\
*
* Called by: main (gui.c)	(UNIX version only)
*				 this routine does not work on the vax
*				 See getResources for VAX version.
\************************************************************************/
{
   /** get the ipf project defined resources **/
   XtGetApplicationResources( toplevel_widget,
			      app_resdata,
			      resources,
			      XtNumber(resources),
			      NULL,
			      0 );

#define perrmsg(strid) if(app_resdata->errmsg) printf(strid)

    perrmsg("DEBUG MSGS ON\n");
    if(app_resdata->errmsg)
    {
      print_resources( app_resdata );
    }

  graphorder = app_resdata->graphorder;
  return(0);
}




void print_resources( AppResData *app_resdata )
{
      printf("app_resdata->errmsg=%d\n"      , app_resdata->errmsg);
      printf("app_resdata->debug=%d\n"       , app_resdata->debug);
      printf("app_resdata->server=%d\n"      , app_resdata->server);
      printf("app_resdata->sleep=%d\n"       , app_resdata->sleep);
      printf("app_resdata->autostart=%d\n"   , app_resdata->autostart);
      printf("app_resdata->graphorder=%d\n"  , app_resdata->graphorder);
      if(app_resdata->servername != NULL) {
        printf("app_resdata->servername=%s\n", app_resdata->servername);
      }
      printf("app_resdata->socketid=%d\n"    , app_resdata->socketid);
      if(app_resdata->casename != NULL) {
        printf("app_resdata->casename=%s\n"  , app_resdata->casename);
      }
      if(app_resdata->node != NULL) {
        printf("app_resdata->node=%s\n"      , app_resdata->node);
      }
}


void Syntax(int argc, char *argv[])
/*********************************************************************\
* purpose: Display the correct command line options for calling application.
*
* Called by: main - if there is a bad command line argument
* the x intrinsic routine cleans out all command line options
* it knows from the argv and decrements argc as it goes
* so the only thing left in argv is the unrecognized options
*
* author: Dan Clark September 1991
\*********************************************************************/
{
    int i;
    static int errs = False;

    /* loop through the left over command line options and print */
    for (i=1; i<argc;i++)
      {
         if(!errs++)
            fprintf(stderr,"Invalid GUI option(s):\n");
         fprintf(stderr, "  Option: %s\n",argv[i]);
      }
/*    fprintf(stderr,"Key bindings & prefs require resource loading\n");*/
/*    fprintf(stderr,"> xrdb -merge ipf.app-defaults loads rsrcs db\n");*/
/*    fprintf(stderr,"ipf accepts all std Xt command line options\n");	*/
    fprintf(stderr,"\nUsage: %s [options]\n",argv[0]);

/*** old code to crunch options - nice format of command info too
    printf (" [-analog] [-bw <pixels>] [-digital]\n");
    printf ("   [-fg <color>] [-bg <color>] [-hd <color>] \n");
    printf ("   [-hl <color>] [-bd <color>]\n");
    printf ("   [+|-rv] [+|-synchronous]\n");
    printf ("   [-fn <font_name>] [-help] [-padding <pixels>]\n");
    printf ("   [-update <seconds>] [-display displayname]\n");
    printf ("   [-geometry geom]\n");
***/
    fprintf(stderr,"  [-bg <*background>]            [-bd <*borderColor>] \n");
    fprintf(stderr,"  [-bw <*borderWidth>]           [-display <.display>] \n");
    fprintf(stderr,"  [-fg <*foreground>]            [-font <*font>] or -fn\n");
    fprintf(stderr,"  [-geometry <.geometry>]        [-iconic <on | off>] \n");
    fprintf(stderr,"  [-name <.name>]                [+|-rv ] \n");
/*    fprintf(stderr,"  [+|-sysnchronous] \n"); */
    fprintf(stderr,"  [-title <.title>]              [-xrm <argument>] \n");
    fprintf(stderr,"  [-selectionTimeout <.selectionTimeout>] \n\n");

    fprintf(stderr," IPF additional options:\n");
    fprintf(stderr,"  [+|-errmsg]                    [-debug <integer_level>] \n");
    fprintf(stderr,"  [+|-server]                    [-servername <executable_filename>]\n");
    fprintf(stderr,"  [-socketid <socket_id_number>] [-casename <name>] \n");
    fprintf(stderr,"  [-node <name>]                 [-sleep <seconds>] \n");
    fprintf(stderr,"  [+|-autostart]                 [+|-graphorder] \n");
    fprintf(stderr,"\n");

}


#ifdef TEST

AppResData app_resdata;                 /** appplication resources **/

main(int argc, char *argv[])
{
   getResources(&app_resdata, argc, argv);
   printf("servername = %s \n", app_resdata.servername);
   printf("socketid   = %d \n", app_resdata.socketid);
   printf("node       = %s \n", app_resdata.node);
   printf("sleep      = %d \n", app_resdata.sleep);
   printf("server     = %d \n", app_resdata.server);
   printf("debug      = %d \n", app_resdata.debug);
   printf("autostart  = %d \n", app_resdata.autostart);
   printf("graphorder = %d \n", app_resdata.graphorder);
   exit(0);
}

#endif /* TEST */
