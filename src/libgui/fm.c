static char sccsid[]=" @(#)fm.c	20.8   2/15/95 ";
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
#include <stdlib.h>
#include "dmgr.h"
#include "fm.h"

#ifdef WIN32
#define SEPCHAR ";"
#define DIRCHAR '\\'
#define CURDIR ".\\"
#else
#ifdef VMS
#define SEPCHAR "."
#define DIRCHAR '.'
#define CURDIR "[]"
#else
#define SEPCHAR ","
#define DIRCHAR '/'
#define CURDIR "./"
#endif
#endif

/* The environment variable IPFDIRS is checked for a list of pathnames
   separated by commas.  If IPFDIRS is defined the value of it is used 
   otherwise the current directory is used.  Each path is searched for
   the requested file until found.  Opening a file with overwriting 
   permissions is only allowed in the current directory or to an absolute
   pathname.

   example IPFDIRS searches current dirrectory first

   setenv IPFDIRS "./,/usr/ipf/help/,/shr1/ipf/help,/shr2/ipf/gui/"
*/

FILE *gopen(IPFCONST char *filename, IPFCONST char *mode) {
   char *fname;
   fname = gfind(filename, mode);
   if (fname == NULL) {
      return(fopen(filename, mode));
   } else {
      return(fopen(fname,mode));
   }
}

static char file[200];

char *gfind(IPFCONST char *filename, IPFCONST char *mode) {
  static int first=1;
  static char *ipfdirs_save;
  static int ipfenvlen;
  static char *ipfdirs;
  char *ipfenv, *dir;
  FILE *fp;
  if (first) {
    first = 0;
    ipfenv = getenv("IPFDIRS");
    if (ipfenv == NULL) {
      ipfenv = CURDIR;
    }
    ipfenvlen = strlen(ipfenv) + 1;
    ipfdirs = malloc(ipfenvlen);
    ipfdirs_save = malloc(ipfenvlen);
    strcpy(ipfdirs_save,ipfenv);
  }
  if (ipfenvlen && 
      (mode[0] != 'w') &&      /* don't write except in current directory */
      (filename[0] != DIRCHAR)) {  /* filename of /... is absolute */
    strcpy(ipfdirs, ipfdirs_save);
    dir = strtok(ipfdirs, SEPCHAR);
    while (dir != NULL) {
      file[0]='\0';
      strcpy(file,dir);
      strcat(file,filename);
#ifdef DEBUG
      printf("checking %s!\n",file);
#endif
      if ((fp=fopen(file,mode)) != NULL) {
#ifdef DEBUG
      printf("found %s!\n",file);
#endif
         fclose(fp);
         return(file);
      }
      dir = strtok(NULL, SEPCHAR);
    }
  }
  return(NULL);
}


/* FORTRAN INTERFACE ROUTINE  */

#ifdef VMS
#include <descrip.h>
#include <processes.h>
int fndfil(struct dsc$descriptor_s *dsc_inbuf,
           struct dsc$descriptor_s *dsc_outbuf )
{
  char *inbuf, *outbuf;
  inbuf = dsc_inbuf->dsc$a_pointer;
  outbuf = dsc_outbuf->dsc$a_pointer;

/* The VMS version does not open the file to see if it exists, however, the
   Fortran interface is used only by the "p_plot" routine to locate the
   file "pfmaster.post".  "p_plot" uses a Fortran INQUIRE to determine
   if the file exists.  On VMS, the logical IPFDIRS must be a single path,
   as opposed to a list of paths to be searched.
*/
  strcpy(outbuf,"IPFDIRS:");
  strcat(outbuf,inbuf);
  return(0);
}
#else /* if not VMS */
#ifdef UNDERSCORE
  int  fndfil_(char *inbuf, char *outbuf)
#else
  int  fndfil(char *inbuf, char *outbuf)
#endif
{
  char *tmp;
  tmp = gfind(inbuf,"r");
  if (tmp == NULL) {
     return(1);
  } else {
     strcpy(outbuf,tmp);
     return(0);
  }
}
#endif /* VMS */


#ifdef TEST
main() {
  FILE *fp;
  printf("looking for fm.tst %s\n",gfind("fm.tst","r"));
  printf("looking for /fm.tst %s\n",gfind("/fm.tst","r"));
  printf("looking for tmp/fm.tst %s\n",gfind("tmp/fm.tst","r"));
  fp = gopen("fm.tst","r");
  fp = gopen("/fm.tst","r");
  fp = gopen("tmp/fm.tst","w");
  fp = gopen("tmp/fm.tst","w");

}
#endif

