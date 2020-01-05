#include <time.h>

#ifdef UNDERSCORE
void caltme_(int  atm[9])
#else
void caltme(int  atm[9])
#endif
{
   unsigned long *tptr, vtptr;
   struct tm *xtm;

   tptr = &vtptr;
   time(tptr);
   xtm = localtime(tptr);
   atm[0] = xtm->tm_sec;
   atm[1] = xtm->tm_min;
   atm[2] = xtm->tm_hour;
   atm[3] = xtm->tm_mday;
   atm[4] = xtm->tm_mon;
   atm[5] = xtm->tm_year;
   atm[6] = xtm->tm_wday;
   atm[7] = xtm->tm_yday;
   atm[8] = xtm->tm_isdst;
}
