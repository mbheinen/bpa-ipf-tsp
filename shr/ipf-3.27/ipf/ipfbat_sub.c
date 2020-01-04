#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#ifdef WIN32
#pragma aux initlz_cw "^"
#pragma aux pfinit_cw "^"
#pragma aux ipf_fexit_cw "^"
#endif

#include "ipf_ipc.h"

int prs_cmdfil( char *filnam, char *inbuf, char *outbuf );
int initlz_cw( void );
void pfinit_cw( void );
void set_batch_cw( void );
void ipf_fexit_cw( void );

#define PCL_NMSZ 60

void ipfbat_sub( int argc, char *argv[] )
{
   static int   status;
   static char  pcl_file[PCL_NMSZ];
   static char  inbuf[BUFSIZE];
   static char  outbuf[BUFSIZE];

   if ( argc != 2 || argv[1][0] == '?' ) {
      if ( argc > 2 ) fprintf(stderr, "Excessive arguments \n");
      fprintf(stderr, "Usage: ipfbat pcl_file \n");
      exit(1);
   }
   status = initlz_cw();
   pfinit_cw();
   set_batch_cw();
   strcpy( pcl_file, argv[1] );
   status = prs_cmdfil( pcl_file, inbuf, outbuf );
   ipf_fexit_cw();
   exit(1); /* should not return from "ipf_fexit_cw"  */
}
