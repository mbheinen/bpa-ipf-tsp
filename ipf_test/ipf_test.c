/* file: ipf_test.c */

#include <stdio.h>

#if defined UNDERSCORE
   void pf_cmd_ ();
#elif defined DUNDERSCORE
   void pf_cmd__ ();
#else
   void pf_cmd ();
#endif

int main( int argc, char *argv[] )
{
#if defined UNDERSCORE
   pf_cmd_ ();
#elif defined DUNDERSCORE
   pf_cmd__ ();
#else
    pf_cmd ();
#endif
}
