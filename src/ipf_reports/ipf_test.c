/* file: ipf_test.c */

#include <stdio.h>

#if defined UNDERSCORE
   void pf_cmd_ ();
#elif defined DUNDERSCORE
   void pf_cmd__ ();
#else
   void pf_cmd ();
#endif

main( int argc, char *argv[] )
{
#if defined UNDERSCORE
   pf_cmd_ ();
#elif defined DUNDERSCORE
   pf_cmd__ ();
#else
    pf_cmd ();
#endif
}
#if defined VMS
   void fstout( )
{
  fprintf (stderr, "IPF cannot execute /OUTAGE_SIMULATION\n");
}
#elif defined UNDERSCORE
   void fstout_( )
{
  fprintf (stderr, "IPF cannot execute /OUTAGE_SIMULATION\n");
}
#elif defined DUNDERSCORE
   void fstout__( )
{
  fprintf (stderr, "IPF cannot execute /OUTAGE_SIMULATION\n");
}
#else
   void fstout( )
{
  fprintf (stderr, "IPF cannot execute /OUTAGE_SIMULATION\n");
}
#endif
