/*                   DEBUG.C 
 
  -  Package of debug file writeout routines.  
  -  Application must provide storage for a FILE pointer. 

      */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "debug.h"
#include "dem.h"
/*                     */

extern FILE *dbg_fl;

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void dbgopen (char *debug_name)  {
  dbg_fl = fopen (debug_name,"w","mrs=133","rat=cr","rfm=var");  
  if (! dbg_fl)  {
    fprintf (stderr,"dbgopen [f]: Couldn't open debug file\n");
    exit (1);  }
  }
   
/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

int dbgclose ()  { 
  return fclose (dbg_fl);  }

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void cdbgeko (char *text)  {
  fprintf (dbg_fl,"%s\n",text);  }

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void cdbgeko2 (char *text1, char *text2) { 
  fprintf (dbg_fl,"%s%s\n",text1,text2);  }

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void cdbgwri (char *descrip, int ival)  {
  fprintf (dbg_fl,"%s%d\n",descrip,ival);  }

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void cdbgwrf (char *descrip, float fval)  {
  fprintf (dbg_fl,"%s%14.6f\n",descrip,fval);  }

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void cdbgwrs (char *descrip, char *strval)  {
  fprintf (dbg_fl,"%s_%s_\n",descrip,strval);  }

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void cdbgwrp (char *descrip, void *vptr)  {
  fprintf (dbg_fl,"%s%p\n",descrip,vptr);  }

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

