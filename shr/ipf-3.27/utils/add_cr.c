#include <stdio.h>
#include <ctype.h>
#include <string.h>

FILE *efopen (char *file, char *mode);
int add_cr (FILE *f1, FILE *f2, int *num);
char *progname;

main(int argc, char *argv[])
{
  FILE *f1, *f2, *efopen();
  int i, status, num_lines = 0;

  progname = argv[0];
  if (argc != 3) {
    fprintf(stderr, "Usage: add_cr file1 file2\n");
    exit(1);
  }
  f1 = efopen(argv[1], "r");
  f2 = efopen(argv[2], "w");
  status = add_cr(f1, f2, &num_lines);
  fprintf(stderr, " %d Lines of output written to file %s\n",
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

int add_cr (FILE *f1, FILE *f2, int *num)
{
  char inbuf[BUFSIZ], outbuf[BUFSIZ], *o;
  size_t size;
  int count = 0;

  while (fgets(inbuf, sizeof inbuf, f1) != NULL) {
    (*num)++;
    strcpy (outbuf, inbuf);
    size = strlen(outbuf);
    if ((o = strchr (outbuf,'\n')) != 0) {
      *(o++) = '\r';
      *(o++) = '\0';
    } else {
      outbuf[size] = '\r';
      outbuf[size+1] = '\0';
    }
    fprintf (f2, "%s\n", outbuf);
  }
  return 0;
}
