#include <stdio.h>
#include <assert.h>

#if defined(WIN32) || defined(__WATCOMC__)
#pragma aux ipack_2 "^";
#endif

#ifdef UNDERSCORE
int ipack_2_ (unsigned int *a1, int *a2)
#else
int ipack_2 (unsigned int *a1, int *a2)
#endif
{
/*** This routine will not work if "int" is not a 32-bit quantity !!! ***
 *** Use "long" if "int" is not 32-bit (16-bit on some systems)       ***
 *** and "long" is a 32-bit quantity.                                 ***/
  assert( sizeof(int) == 4);
/************************************************************************/
  return *a1 << 16 | *a2;
}

#ifdef DEBUG

main(argc, argv)
int argc;
char *argv[];
{
  int pack = 1, a1, a2;
  while (pack > 0) {
    printf (" Enter arg#1 arg#2 > ");
    scanf ("%d %d", &a1, &a2);

#if defined(UNDERSCORE)
    pack = ipack_2_ (&a1, &a2);
#else
    pack = ipack_2 (&a1, &a2);
#endif
    printf ("\n pack = %x \n", pack);
  }
  exit(1);
}

#endif /* DEBUG */

