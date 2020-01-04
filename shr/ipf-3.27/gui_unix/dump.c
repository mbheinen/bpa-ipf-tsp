#include <stdio.h>
#include <ctype.h>
#include <string.h>

/* #define DEBUG */

FILE *efopen (char *file, char *mode);
char *progname;

main(argc, argv)
int argc;
char *argv[];
{
  FILE *f1, *f2, *efopen();
  int i, len;

  progname = argv[0];
  if (argc != 2) {
    fprintf(stderr, "Usage: dump file1 \n");
    exit(1);
  }
  f1 = efopen(argv[1], "r");
  dump(f1);
  exit(1);
}

FILE *efopen(file, mode) /* fopen file, die if can't */
char *file, *mode;
{
  FILE *fp, *fopen();

  if ((fp = fopen(file, mode)) != NULL)
    return fp;
  fprintf(stderr, "%s: Can't open file %s mode %s\n",
  progname, file, mode);
  exit(1);
}

int dump (f1)
FILE *f1;

{
  char inbuf[BUFSIZ], outbuf[BUFSIZ], *o;
  int i, size, count = 0;

  while (fgets(inbuf, sizeof inbuf, f1) != NULL) {

    printf ("\n Record %d \n %s \n", ++count, inbuf);
    size = strlen(inbuf);
    for (i = 0; i < size; i++)
      printf (" inbuf[%d] = %c %d \n", i, inbuf[i], inbuf[i]);
  }
}
