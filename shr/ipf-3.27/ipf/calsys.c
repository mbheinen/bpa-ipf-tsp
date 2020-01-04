/*  call a system routine  */
#include <stdlib.h>
#include <stdio.h>

#if defined(VMS)
#include <descrip.h>
  int  calsys(struct dsc$descriptor_d *dsc_sysarg)
  {
    char *sysarg = (char *)dsc_sysarg->dsc$a_pointer;

#elif defined(WIN32) || defined(__WATCOMC__)
typedef struct descriptor {
    char          *addr;
    unsigned       len;
} descriptor;

#pragma aux calsys "^"
  int  calsys( descriptor *dsc_sysarg)
  {
    char *sysarg = (char *)dsc_sysarg->addr;

#elif defined(UNDERSCORE)
  int  calsys_(char *sysarg)
  {
#else
  int  calsys(char *sysarg)
  {
#endif

  int  rtncde;
  printf (" sysarg = %s \n", sysarg);
  rtncde = system(sysarg);

/* rtncde = system("pwd");  */

  printf (" rtncde = %d \n", rtncde);
  return rtncde;
  }

/*
#if defined(TEST)
main ()
 {
 int rtncde;
#if defined(UNDERSCORE)
 rtncde = calsys_("pwd");
#else
 rtncde = calsys("pwd");
#endif
 printf (" rtncde = %d \n", rtncde);
 }
#endif
*/
