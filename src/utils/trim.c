#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

FILE *efopen (char *file, char *mode);
int remove_cr (FILE *f1, FILE *f2, int *num);
char *progname;

main(int argc, char *argv[])
{
  FILE *f1, *f2, *efopen();
  int i, status, num_lines = 0, infile = 0, outfile = 0, begin_col = 0,
      end_col = 80;
  char *p;

  progname = argv[0];
  for (i=1; i<argc; i++) {
    p = argv[i];
    if (*p == '-') {
      if (*(++p) == 'B') {
        begin_col = atof (++p);
      } else if (*p == 'E') {
        end_col = atof (++p);
      } else {
        fprintf (stderr, "Illegal option [%s] \n", argv[i]);
        exit (1);
      }
    } else if (infile == 0) {
      f1 = efopen(argv[i], "r");
      infile = i;
    } else if (outfile == 0) {      
      f2 = efopen(argv[i], "w");
      outfile = i;
    } else {
      fprintf(stderr, "Usage: trim [-Bnn] [-Enn] file1 file2\n");
      exit(1);
    }
  }
  if (infile == 0 || outfile == 0) {
    fprintf(stderr, "Usage: trim [-Bnn] [-Enn] file1 file2\n");
    exit(1);
  } else if (begin_col >= end_col) {
    fprintf(stderr, "Usage: trim [-Bnn] [-Enn] file1 file2\n");
    exit(1);
  }
  status = trim(f1, f2, &num_lines, begin_col, end_col);
  fprintf(stderr, " %d Lines of output written to file %s\n",
      num_lines, argv[outfile]);
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

int trim (FILE *f1, FILE *f2, int *num, int beg_col, int end_col)
{
  char inbuf[BUFSIZ], outbuf[BUFSIZ], *o;
  size_t size;
  int i;

  while (fgets(inbuf, sizeof inbuf, f1) != NULL) {
    (*num)++;
    strcpy (outbuf, inbuf);
    if ((o = strchr (outbuf,'\r')) != 0) {
      *o = '\x00';
    } else if ((o = strchr (outbuf,'\n')) != 0) {
      *o = '\x00';
    }
    size = strlen(outbuf);
    for (i=size; i<end_col; i++) {
      outbuf[i] = ' ';
    }
    outbuf[end_col] = '\x00';    
    fprintf (f2, "%s\n", &outbuf[beg_col]);
  }
  return 0;
}
