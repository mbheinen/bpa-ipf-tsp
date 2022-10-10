#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#ifdef VMS
#include <descrip.h>
#endif /* VMS */

#ifdef VMS
   void pwrflo( struct dsc$descriptor_d * );
#elif WIN32
typedef struct descriptor {
    char          *addr;
    unsigned       len;
} descriptor;

#pragma aux pwrflo "^"
   void pwrflo( descriptor * );
#else /* VMS */
#ifdef UNDERSCORE
   void pwrflo_( char * );
#else /* UNDERSCORE */
   void pwrflo( char * );
#endif /* UNDERSCORE */
#endif /* VMS */

#define PFC_NMSZ 60

void bpf_sub( int argc, char *argv[] )
{
   static int   i;
   static char  pfc_file[PFC_NMSZ];

#ifdef VMS

   static struct dsc$descriptor_d dsc_pfc_file;

   dsc_pfc_file.dsc$w_length = (unsigned short)PFC_NMSZ;
   dsc_pfc_file.dsc$b_dtype = (char)DSC$K_DTYPE_T;
   dsc_pfc_file.dsc$b_class = (char)DSC$K_CLASS_D; /* or _S  or _VS or_VSA */
   dsc_pfc_file.dsc$a_pointer = (char *)pfc_file;

#elif WIN32
   descriptor dsc_pfc_file;

   dsc_pfc_file.len = PFC_NMSZ;
   dsc_pfc_file.addr = pfc_file;

#endif /* VMS */

   if ( argc == 1 ) {
      for ( i = 0 ; i < PFC_NMSZ ; i++ ) pfc_file[i] = ' ';
   } else if ( argc != 2 || argv[1][0] == '?' ) {
      if ( argc > 2 ) fprintf(stderr, "Excessive arguments \n");
      fprintf(stderr, "Usage: bpf file_name \n");
      exit(1);
   } else {
      strcpy( pfc_file, argv[1] );
      i = strlen( pfc_file );
      while ( i < PFC_NMSZ ) {
         pfc_file[i] = ' ';
         i++;
      }
   }
   
#ifdef VMS
   pwrflo( &dsc_pfc_file );
#elif WIN32
   pwrflo( &dsc_pfc_file );
#else /* VMS */
#ifdef UNDERSCORE
   pwrflo_( pfc_file );
#else
   pwrflo( pfc_file );
#endif
#endif /* VMS */
   exit(1); /* should never return from "pwrflo" */
}
