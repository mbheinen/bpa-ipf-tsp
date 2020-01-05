#include <stdio.h>
#include <assert.h>

#ifdef WIN32
#pragma aux shift "^";
#endif

#ifdef UNDERSCORE
int ipack_4_ (unsigned int *a1, int *a2, int *a3, int *a4)
#else
int ipack_4 (unsigned int *a1, int *a2, int *a3, int *a4)
#endif
{
/*** This routine will not work if "int" is not a 32-bit quantity !!! ***
 *** Use "long" if "int" is not 32-bit (16-bit on some systems)       ***
 *** and "long" is a 32-bit quantity.                                 ***/
  assert( sizeof(int) == 4);
/************************************************************************/
  return *a1 << 16 | *a2 << 8 | *a3 << 4 | *a4;
}

#ifdef DEBUG

main(argc, argv)
int argc;
char *argv[];
{
  int pack = 1, a1, a2, a3, a4;
  while (pack > 0) {
    printf (" Enter arg#1 arg#2 arg#3 arg#4 > ");
    scanf ("%d %d %d %d", &a1, &a2, &a3, &a4);

#ifdef UNDERSCORE
    pack = ipack_4_ (&a1, &a2, &a3, &a4);
#else
    pack = ipack_4 (&a1, &a2, &a3, &a4);
#endif
    printf ("\n pack = %x \n", pack);
  }
  exit(1);
}

#endif /* DEBUG */

