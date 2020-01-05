#ifdef UNDERSCORE
void linflo_main_( int argc, char *argv[] );
#else
void linflo_main( int argc, char *argv[] );
#endif

main( int argc, char *argv[] )
{
#ifdef UNDERSCORE
   linfloanl_();
#else
   linfloanl();
#endif
}
