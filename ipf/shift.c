#include <stdio.h>
#include <assert.h>

#ifdef WIN32
#pragma aux shift "^";
#endif

#ifdef UNDERSCORE
int shift_ (unsigned int *a, int *b)
#else
int shift (unsigned int *a, int *b)
#endif
{
/*** This routine will not work if "int" is not a 32-bit quantity !!! ***
 *** Use "long" if "int" is not 32-bit (16-bit on some systems)       ***
 *** and "long" is a 32-bit quantity.                                 ***/
  assert( sizeof(int) == 4);
/************************************************************************/
  return ( *b > 0) ? *a << *b : *a >> -(*b);
}

#ifdef DEBUG

main(argc, argv)
int argc;
char *argv[];
{
  int pack = 1, i, j;
  while (pack > 0) {
    printf (" Enter arg#1 and shift > ");
    scanf ("%d %d", &i, &j);

#ifdef UNDERSCORE
    pack = shift_ (&i, &j);
#else
    pack = shift (&i, &j);
#endif
    printf ("\n pack = %x \n", pack);

    printf (" Enter arg#2 and shift > ");
    scanf ("%d %d", &i, &j);

#ifdef UNDERSCORE
    pack = pack + shift_ (&i, &j);
#else
    pack = pack + shift (&i, &j);
#endif
    printf ("\n pack = %x \n", pack);

    printf (" Enter arg#3 and shift > ");
    scanf ("%d %d", &i, &j);

#ifdef UNDERSCORE
    pack = pack + shift_ (&i, &j);
#else
    pack = pack + shift (&i, &j);
#endif
    printf ("\n pack = %x \n", pack);
  }
  exit(1);
}

#endif /* DEBUG */

