#include <stdio.h>

FILE *efopen(file, mode) /* fopen file, die if can't */
char *file, *mode;
{
  FILE *fp;

  fp = fopen(file, mode);
  return fp;
}

