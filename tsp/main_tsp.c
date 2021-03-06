#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <setjmp.h>

struct files {
  char  savifl[60],
        savofl[60],
        solfl[60],
        pltfl[60],
        prtfl[60],
        auxfl[60],
        bsefl[60],
        ctrlfl[60],
        filefl[60],
        dbgifl[60],
        dbgofl[60],
        postmstr[60];
#if defined UNDERSCORE || DUNDERSCORE
  } files_;
#else
  } files;
#endif

struct jump_err {
  jmp_buf saved_context;
#if defined UNDERSCORE || DUNDERSCORE
  } jump_err_;
#else
  } jump_err;
#endif

#if defined UNDERSCORE
void swingm_( void );
void erexit_( void );
int is_it_vms_();
#elif defined DUNDERSCORE
void swingm_( void );
void erexit_( void );
int is_it_vms__();
#else
void swingm( void );
void erexit( void );
int is_it_vms();
#endif

char *progname;

void usage_error_exit()
{
    fprintf(stderr, "Usage: tsp <file_name.fil>  \n");
    exit(1);
}

main(int argc, char *argv[])
{
  int i, size, len, inflag = 0, ifVAX, val;
  char *p, *pf; 

  progname = argv[0];

#if defined UNDERSCORE
  ifVAX = is_it_vms_(); 
#elif defined DUNDERSCORE
  ifVAX = is_it_vms__(); 
#else
  ifVAX = is_it_vms(); 
#endif

  if (!ifVAX) {
    if (argc <= 1) {
      fprintf(stderr, "No arguments \n");
      usage_error_exit();
    } else if (argc == 2 && argv[1][0] == '?' && argv[1][1] == 0) {
      usage_error_exit();
    } else if (argc > 2) {
      fprintf(stderr, "Excessive arguments \n");
      usage_error_exit();
    }
  } else {
    if (argc == 2 && argv[1][0] == '?' && argv[1][1] == 0) {
      usage_error_exit();
    } else if (argc > 2) {
      fprintf(stderr, "Excessive arguments \n");
      usage_error_exit();
    }
  }
#if defined UNDERSCORE || DUNDERSCORE	
  pf = files_.filefl;
  size = sizeof files_.filefl;
#else
  pf = files.filefl;
  size = sizeof files.filefl;
#endif
  for (p = pf; p < &pf[size]; *p++ = ' ');
  for (i = 1; i < argc; i++) {
    if (!inflag) {
      len = strlen(argv[i]);
      strncpy (pf, argv[i], len);
/*
 *    Blank fill "files.filefl"
 */
      for (p = &pf[len]; p < &pf[size]; *p++ = ' ');
      inflag++;
    } 
  }
  if (!inflag && ifVAX == 0) {
    fprintf(stderr, "Missing input file \n");
    usage_error_exit();
  }
#if defined UNDERSCORE
  if (setjmp(jump_err_.saved_context) == 0) 
    swingm_(); 
  else
    erexit_();
#elif defined DUNDERSCORE
  if (setjmp(jump_err_.saved_context) == 0) 
    swingm_(); 
  else
    erexit_();
#else
  if (setjmp(jump_err.saved_context) == 0) 
    swingm(); 
  else
    erexit();
#endif
  exit(1);
}

