#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct FUNC_TABLE {
  char name[16];
  int  funcnum;
} FUNCT_TABLE;

#define CNTRL  0
#define DIGIT  1
#define GRAPH  2
#define LOWER  3
#define PRINT  4
#define PUNCT  5
#define SPACE  6
#define UPPER  7
#define XDIGIT 8

static FUNCT_TABLE isfuncs[9] = {
  "iscntrl", CNTRL,
  "isdigit", DIGIT,
  "isgraph", GRAPH,
  "islower", LOWER, 
  "isprint", PRINT, 
  "ispunct", PUNCT,
  "isspace", SPACE,
  "isupper", UPPER,
  "isxdigit", DIGIT 
};

static int numfunc = sizeof (isfuncs) / sizeof (FUNCT_TABLE);

main (int argc, char *argv[])
{
  int ch, count, i, test_result, mark=0xdb;

  if (argc < 2) {
    fprintf (stderr, "Usage: %s <fucntion_name>\n", argv[0]);
    exit (0);
  }

/* Search table for function name and pointer */

  for (i=0; i<numfunc; i++) {
    if (strcmp(argv[1], isfuncs[i].name) == 0) break;
  }
  if (i >= numfunc) {
    fprintf (stderr, "Unknown function: %s\n", argv[1]);
    exit (0);
  }

/* Mark selected characters in ASCII set */

  fprintf (stderr, "Those marked with %c satsify %s\n", mark, argv[1]);
  for (count=0, ch=0; ch<= 0xff; ch++) {
    fprintf (stderr, "%#02x ", ch);
    if (isprint(ch)) fprintf (stderr, " %c", ch);
    else fprintf (stderr, "  ");
    switch (isfuncs[i].funcnum) {
    case CNTRL: test_result = iscntrl(ch);
                break;
    case DIGIT: test_result = isdigit(ch);
                break;
    case GRAPH: test_result = isgraph(ch);
                break;
    case LOWER: test_result = islower(ch);
                break;
    case PRINT: test_result = isprint(ch);
                break;
    case PUNCT: test_result = ispunct(ch);
                break;
    case SPACE: test_result = isspace(ch);
                break;
    case UPPER: test_result = isupper(ch);
                break;
    case XDIGIT: test_result = isxdigit(ch);
                break;
    }

    if (test_result != 0) fprintf (stderr, "%c ", mark);
    else fprintf (stderr, "  ", ch);
    count++;
    if (count == 8) {
      fprintf (stderr, "\n");
      count = 0;
    }
  }
}


