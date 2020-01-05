#include <stdio.h>
#include <ctype.h>
#include <string.h>

FILE *efopen (char *, char *);
int examine (FILE *, FILE *, int *);
int ParseRecord (FILE *, FILE *, int *);
char *progname;

main(int argc, char *argv[])
{
  FILE *f1, *f2, *efopen();
  int i, status, num_lines = 0;

  progname = argv[0];
  if (argc != 3) {
    fprintf(stderr, "Usage: examine file1 file2\n");
    exit(1);
  }
  f1 = efopen(argv[1], "r");
  f2 = efopen(argv[2], "w");
  status = examine(f1, f2, &num_lines);
  fprintf(stderr, " %d records written to file %s\n",
      num_lines, argv[2]);
  exit(1);
}

FILE *efopen(char *file, char *mode) /* fopen file, die if can't */
{
  FILE *fp, *fopen();

  if ((fp = fopen(file, mode)) != NULL)
    return fp;
  fprintf(stderr, "%s: Can't open file %s mode %s\n",
  progname, file, mode);
  exit(1);
}

int examine (FILE *f1, FILE *f2, int *num)
{
  while (ParseRecord (f1, f2, num)) {
  }
  return 0;
}

int ParseRecord (FILE *f1, FILE *f2, int *num)
{
  static char inbuf[BUFSIZ], outbuf[BUFSIZ], *o;
  int c, len=0, i, error=0;

  inbuf[len] = '\x00';

  c = fgetc (f1);
  inbuf[len++] = c;
  inbuf[len] = '\x00';
  if (c == EOF) {
    return 0;
  }

  while (c != EOF && c != '\n' && c != '\x00') {
    c = fgetc (f1);
    inbuf[len++] = c;
    inbuf[len] = '\x00';
  }
  for (i=0; i<len; i++) outbuf[i] = ' ';
  outbuf[len] = '\x00';
  for (i=0; i<len; i++) {
    if (isprint (inbuf[i]) == 0 && inbuf[i] != '\n') {
      outbuf[i] = '^';
      error++;
    }
  }
  if (error) {
    (*num)++;
    fprintf (f2, " %5d %3d [%s]\n           [%s]\n", *num, len, inbuf, outbuf);
  }
  return len;
}

