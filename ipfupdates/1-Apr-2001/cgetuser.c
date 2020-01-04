/*** cgetuser procedure ***/
#include <stdio.h>
#include <string.h>

extern char *cuserid(char *); /* prototype non standard function that
                                 is supported on most platforms */
/*
   data is passed back vi the following global structures, which correspond
   to the various naming conventions for FORTRAN compilers when creating
   the object file name of a COMMON called /c_userid/
*/
struct { char user[11]; }  c_userid  ;
struct { char user[11]; }  c_userid_ ;
struct { char user[11]; }  c_userid__ ;
struct { char user[11]; } _c_userid  ;
struct { char user[11]; } _c_userid_ ;
struct { char user[11]; } _Cc_userid  ;

#ifndef L_cuserid
#define L_cuserid 11
#endif

#ifdef UNDERSCORE
void cgetuser_()
#else
void cgetuser()
#endif
{
   char s[L_cuserid] ;
   cuserid(s);
   strncpy(  c_userid.user,  s, 10 );
   strncpy(  c_userid_.user, s, 10 );
   strncpy(  c_userid__.user, s, 10 );
   strncpy( _c_userid.user,  s, 10 );
   strncpy( _c_userid_.user, s, 10 );
   strncpy( _Cc_userid.user, s, 10 );
   return;
}

