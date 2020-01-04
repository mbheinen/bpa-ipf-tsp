/*** c_err_exit  routine ***/
#include <stdlib.h>

#if defined(WIN32) || defined(__WATCOMC__)
#pragma aux c_err_exit "^";
void c_err_exit( void )

#elif defined(UNDERSCORE)
void c_err_exit_( void )

#else
void c_err_exit( void )
#endif
{
#if defined(VMS)
   exit( 4 );
#else
   exit( 1 );
#endif
}

