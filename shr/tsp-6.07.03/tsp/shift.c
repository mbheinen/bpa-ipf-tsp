#ifdef DEBUG

#include <stdio.h>

#ifdef UNDERSCORE
long shift_ (unsigned long *i1, long *i2);
#else
long shift (unsigned long *i1, long *i2);
#endif

main(argc, argv)
int argc;
char *argv[];
{
  long pack = 1, i, j;
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

#ifdef UNDERSCORE
long shift_ (unsigned long *a, long *b)
#else
long shift (unsigned long *a, long *b)
#endif
{
  return ( *b > 0) ? *a << *b : *a >> -(*b);
}
