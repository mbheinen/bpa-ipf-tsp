#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "ipf_ipc.h"

#ifdef WIN32
#pragma aux plot_load_cw "^"
#endif

void plot_load_cw( char *, char *, char * );

#define FNAM_SZ1 61

void ipfplot_sub( int argc, char *argv[] )
{
   static int   status;
   static char  coord_file[FNAM_SZ1];
   static char  base1_file[FNAM_SZ1];
   static char  base2_file[FNAM_SZ1];

   if ( argc < 2 || argv[1][0] == '?' || argc > 4 ) {
      if ( argc > 4 ) fprintf(stderr, "Excessive arguments \n");
      fprintf(stderr,"Usage: ipfplot coord_file [base1_file] [base2_file]\n");
      exit(1);
   }
   memset( coord_file, '\0', FNAM_SZ1 );
   memset( base1_file, '\0', FNAM_SZ1 );
   memset( base2_file, '\0', FNAM_SZ1 );
   strcpy( coord_file, argv[1] );
   if ( argc > 2 ) strcpy( base1_file, argv[2] );
   if ( argc > 3 ) strcpy( base2_file, argv[3] );
/* the Fortran routine "plot_load" handles zero terminated strings */
   plot_load_cw( coord_file, base1_file, base2_file );
   exit(1); /* should never return */
}
