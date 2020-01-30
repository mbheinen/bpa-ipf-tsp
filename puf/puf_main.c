#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined VMS                                               /* VMS */
#include <descrip.h>
int puf ( int *, struct dsc$descriptor_s *);
void puf_main_( int *, char ** );

#elif defined UNDERSCORE || DUNDERSCORE
int puf_ ( int *, char ** );
void puf_main_( int *, char ** );

 
#else
int puf ( int *, char ** );
void puf_main( int *, char ** );
#endif

main( int argc, char *argv[] )
{
  int status, i, j, size, f_argc = argc;
  char f_argv[10][60];

#if defined VMS                                               /* VMS */
  struct dsc$descriptor_s argv_VMS[10];
  for (i=0; i<argc; i++) {
    strcpy (f_argv[i], argv[i]);
    size = sizeof (f_argv[i]);
    for (j=strlen (f_argv[i]); j<size; j++) f_argv[i][j] = ' ';
    argv_VMS[i].dsc$a_pointer = f_argv[i];
    argv_VMS[i].dsc$w_length = size;
  }
  status = PUF( &f_argc, argv_VMS);

#elif defined UNDERSCORE || DUNDERSCORE
  for (i=0; i<argc; i++) {
    strcpy (f_argv[i], argv[i]);
    size = sizeof (f_argv[i]);
    for (j=strlen (f_argv[i]); j<size; j++) f_argv[i][j] = ' ';
  }
   puf_( &f_argc, (char **) f_argv );

#else
  for (i=0; i<argc; i++) {
    strcpy (f_argv[i], argv[i]);
    size = sizeof (f_argv[i]);
    for (j=strlen (f_argv[i]); j<size; j++) f_argv[i][j] = ' ';
  }
   puf( &f_argc, (char **) f_argv );

#endif
}

