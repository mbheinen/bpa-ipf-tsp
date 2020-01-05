/*********************************************************************
*
* This is an "umbrella" main that calls one of several subroutine
* "mains" (used to be a stand-alone program) by using the name that
* the program is invoked by on the command line (argv[0]).
* On VMS, argv[0] is always set to the actual file name, so for
* the VMS case, argv[1] is used to pass the "name of program to run";
* this is done by "shifting" the "argv" strings and decrementing "argc".
*
* You must set up the following names in you environment, e.q. for "ipfsrv"
*
* (Unix)  "ln -s ipfmain ipfsrv"    ( ipfsrv -> ipfmain )
*                              (in the directory where ipfmain is installed)
*
* (VMS)   "ipfsrv :== $disk:[path.path]ipfmain.exe ipfsrv"        and
*         "define  ipfsrv  disk:[path.path]ipfmain.exe"   (for gui "exec")
*                        The "define" is needed only for programs that are
*                        "exec'ed" from another program (gui "exec's" ipfsrv).
*
* (WIN32)   ??? setup alias with "ipfsrv" as "argv[1]" ???
*
* The following are called:
*    (the syntax below is <command name> = <routine name>)
*
*  bpf         = bpf_sub.c
*  ipfsrv      = ipfsrv_sub.c
*  ipfbat      = ipfbat_sub.c
*
*  ipfplot     = ipfplot_sub.c
*  ipft        = pf_cmd.f       (test program)
*
****************************************************
*
*    These are included, but recommended to be linked separately also
*
*  ipfcut      = cutting_sub.f
*  ipfnet      = net_data_sub.f
*  ipf_reports = ipf_reports.f  (no documentation for this program)
*  ips2ipf     = ips2ipf.f
*
****************************************************
*
*    not included, this is a small stand alone utility used by the
*    GUI "Network Data Edit" function
*
*  ipf_diff    = ipf_diff.f
*
*
*  It should be noted that although this scheme minimizes the size of
*  the executable file space needed, it significantly increases the
*  virtual space (memory and swap space usage <performance issue if
*  not enough memory>) for the smaller programs (ipfcut,ipfnet,ipf_reports).
*  On a server, where several users are using "ipfcut" and "ipfnet" on a
*  regular basis, you may want to link these programs as separate
*  executables to minimize the virtual space that they need.
*
**********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#ifdef WIN32
#pragma aux cutting_sub_cw "^";
#pragma aux net_data_sub_cw "^";
#pragma aux ipf_reports_cw "^";
#pragma aux pf_cmd_cw "^";
#endif

/*
extern void cutting_sub_cw( int argc, char *argv[] );
extern void net_data_sub_cw( int argc, char *argv[] );
extern void ipf_reports_cw( int argc, char *argv[] );
extern void pf_cmd_cw( int argc, char *argv[] );
extern void bpf_sub( int argc, char *argv[] );
extern void ipfsrv_sub( int argc, char *argv[] );
extern void ipfplot_sub( int argc, char *argv[] );
extern void ipfbat_sub( int argc, char *argv[] );
*/

extern void cutting_sub_cw();
extern void net_data_sub_cw();
extern void ipf_reports_cw();
extern void pf_cmd_cw();
extern void bpf_sub();
extern void ipfsrv_sub();
extern void ipfplot_sub();
extern void ipfbat_sub();

static void (*ipf_fptr)( int argc, char *argv[] );

static struct {
   char name[15];
   void  (*fptr)();
                   }  progs[] =
{
   { "ipfcut"      , cutting_sub_cw   },
   { "ipfnet"      , net_data_sub_cw  },
   { "ipf_reports" , ipf_reports_cw   },
   { "ipft"        , pf_cmd_cw        },
   { "bpf"         , bpf_sub          },
   { "ipfsrv"      , ipfsrv_sub       },
   { "ipfplot"     , ipfplot_sub      },
   { "ipfbat"      , ipfbat_sub       },
   { "\0"          , NULL             }
};


int ipfname2func( char *name , void (**fptr)() )
{
   int i, found;
   char  *s, *d, name_lower[120];

   /*
   *   convert the "name" to lower case for "strcmp" testing
   */
   for ( d = name_lower, s = name;
         ( *d = (char)tolower( (int)*s ) ) != '\0';
         d++, s++ );

   found = FALSE;
   for (i=0; (progs[i].name[0] != '\0') && (found == FALSE); i++ ) {
      if (strncmp(progs[i].name, name_lower, strlen(progs[i].name)) == 0) {
         found = TRUE;
         *fptr = progs[i].fptr;
      }
   }
   return found;
}

void parse_filespec( char *file_name, char *filespec )
{
   char ts[60], *cp1, *cp2;

#if VMS

   cp2 = strchr( filespec, ':' );
   if ( cp2 == NULL ) cp2 = filespec; else cp2++;

   cp1 = strchr( cp2, ']' );
   if ( cp1 == NULL ) cp1 = cp2; else cp1++;

#elif WIN32

   cp2 = strchr( filespec, ':' );
   if ( cp2 == NULL ) cp2 = filespec; else cp2++;

   cp1 = strrchr( cp2, '\\' );
   if ( cp1 == NULL ) cp1 = cp2; else cp1++;

#else /* Unix */

   cp1 = strrchr( filespec, '/' );
   if ( cp1 == NULL ) cp1 = filespec; else cp1++;

#endif

   strcpy( ts, cp1 );
   cp2 = strrchr( ts, '.' );
   if ( cp2 != NULL ) *cp2 = '\0';

   strcpy( file_name, ts );

   return;

}


main( int argc, char *argv[] )
{
   int   i, j;
   char  prog_name[60];

   /*  the "program to run" ( argv[0] ) may may have a full path as the
   *   filespec, so first get just the filename
   */
   parse_filespec( prog_name, argv[0] );

   /*
   *   see if the "program to run" is in the "progs" table --
   *   if found, then run the program
   */
   if ( ipfname2func( prog_name, &ipf_fptr ) ) {
      (*ipf_fptr)( argc, argv );
      exit(0);
   }

   /*
   *   If we get to here, the above lookup failed, so
   *   shift args "left" by one to see if "program to run"
   *   was sent as argv[1] rather than argv[0]
   */
   for ( i=0, j=1 ; j < argc ; i++, j++ ) argv[i] = argv[j];
   argc--;
   argv[argc] = NULL;

   /*
   *   see if the "program to run" is in the "progs" table --
   *   if found, then run the program
   */
   if ( ipfname2func( argv[0], &ipf_fptr ) ) {
      (*ipf_fptr)( argc, argv );
      exit(0);
   }

   /*
   *   If we get to here, the second lookup failed, so
   *   print out a diagnostic and exit
   */
   printf("This program will execute one of several programs, based on\n  ");
   printf("command line argument 0 or 1 , depending on the operating ");
   printf("system.\n  If this message is given, then the string search ");
   printf("for a \"program to run\" has failed.\n\n");
   printf("The following programs are supported:\n\n");
   for (i=0; progs[i].name[0] != '\0'; i++) printf("\t%s\n",progs[i].name);
   exit(1);
}
