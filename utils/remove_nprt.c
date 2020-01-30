#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define RECORD_LENGTH 256

FILE *efopen (char *, char *);
int remove_nprt (FILE *, FILE *, FILE *, int *);
int ParseRecord (FILE *, FILE *, FILE *, int *);
char *progname;

main(int argc, char *argv[])
{
  FILE *f1, *f2, *f3, *efopen();
  int i, status, num_lines = 0;

  progname = argv[0];
  if (argc != 4) {
    fprintf(stderr, "Usage: remove_nprt file1 file2 file3\n");
    exit(1);
  }
  f1 = efopen(argv[1], "r");
  f2 = efopen(argv[2], "w");
  f3 = efopen(argv[3], "w");
  status = remove_nprt(f1, f2, f3, &num_lines);
  fprintf(stderr, " %d records written to file %s\n",
      num_lines, argv[2]);
  fclose (f1);
  fclose (f2);
  fclose (f3);
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

int remove_nprt (FILE *f1, FILE *f2, FILE *f3, 	int *num)
{
  while (ParseRecord (f1, f2, f3, num)) {
  }
  return 0;
}

int ParseRecord (FILE *f1, FILE *f2, FILE *f3, int *num)
{
  static char inbuf[RECORD_LENGTH], errbuf[RECORD_LENGTH], outbuf[RECORD_LENGTH], *o;
  int c, len=0, i, error=0;

  inbuf[len] = '\x00';
  (*num)++;

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
  strcpy (outbuf, inbuf);
  for (i=0; i<len; i++) errbuf[i] = ' ';
  errbuf[len] = '\x00';
  for (i=0; i<len; i++) {
    if (isprint (inbuf[i]) == 0 && inbuf[i] != '\n') {
      outbuf[i] = ' ';
      errbuf[i] = '^';
      error++;
    }
  }
  if (error) {
    fprintf (f3, " %5d %3d [%s]\n           [%s]\n", *num, len, inbuf, errbuf);
  }
  fprintf (f2, "%s", outbuf);
  return len;
}

