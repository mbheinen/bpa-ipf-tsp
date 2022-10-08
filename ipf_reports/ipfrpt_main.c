#if defined UNDERSCORE || DUNDERSCORE
void ipf_reports__( );
#else
void ipf_reports( );
#endif

int main( int argc, char *argv[] )
{
#if defined UNDERSCORE || DUNDERSCORE
   ipf_reports__();
#else
   ipf_reports();
#endif
}
