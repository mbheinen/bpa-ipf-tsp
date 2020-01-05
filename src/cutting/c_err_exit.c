/*** c_err_exit  routine ***/
#include <stdlib.h>

#ifdef WIN32
#pragma aux c_err_exit "^";
#endif

#ifdef UNDERSCORE
void c_err_exit_( void )
#elif DUNDERSCORE
void c_err_exit__( void )
#else
void c_err_exit( void )
#endif
{
#ifdef VMS
   exit( 4 );
#else
   exit( 1 );
#endif
}

