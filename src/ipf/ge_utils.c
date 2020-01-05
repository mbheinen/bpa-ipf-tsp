#include <stdio.h>
#include <ctype.h>
#include <string.h>

#if defined __WATCOMC__						/* WATCOM */

int open_ge_file( int *, char *, char *, long *, long * );
int close_ge_file( int * );
int read_ge_file( int *, char *, long * );
int write_ge_file( int *, char *, long * );
int rewind_ge_file( int * );

#elif defined VMS						/* VMS */

#include <descrip.h>
int open_ge_file( int *, struct dsc$descriptor_s *, struct dsc$descriptor_s *);
int close_ge_file( int *);
int read_ge_file( int *, struct dsc$descriptor_s *);
int write_ge_file( int *, struct dsc$descriptor_s *);
int rewind_ge_file( int * );

#elif defined DBOS						/* DBOS */

int "FORTRAN" open_ge_file( int *, char *, char *, long *, long *) 
    "OPEN_GE_FILE"; 
int "FORTRAN" close_ge_file( int * ) "CLOSE_GE_FILE"; 
int "FORTRAN" read_ge_file( int *, char *, long *) "READ_GE_FILE"; 
int "FORTRAN" write_ge_file( int *, char *, long *) "WRITE_GE_FILE"; 
int "FORTRAN" rewind_ge_file( int * ) "REWIND_GE_FILE"; 

#elif defined UNDERSCORE

int open_ge_file_( int *, char *, char * );
int close_ge_file_( int * );
int read_ge_file_( int *, char * );
int write_ge_file_( int *, char * );
int rewind_ge_file_( int * );

#elif defined DUNDERSCORE

int open_ge_file__( int *, char *, char * );
int close_ge_file__( int * );
int read_ge_file__( int *, char * );
int write_ge_file__( int *, char * );
int rewind_ge_file__( int * );

#else								/* others */

int open_ge_file( int *, char *, char *, long *, long * );
int close_ge_file( int * );
int read_ge_file( int *, char *, long * );
int write_ge_file( int *, char *, long * );
int rewind_ge_file( int * );

#endif

#ifndef MAXSIZ
#define MAXSIZ 512
#endif

#define MAXFILES 4

static FILE *ge_fp[MAXFILES];       /* File pointers  */
static char file[80][MAXFILES];     /* File names     */
FILE *efopen (char *file, char *mode);
char *progname;

/* ===================================================================
   End declarations, begin module code
   ===================================================================
*/

#if defined __WATCOMC__						/* WATCOM */
int open_ge_file( int *file_num, char *filename, char *mode, 
                  long *filename_len, long *mode_len )
{
  char tempfile[80];
  strncpy (tempfile, filename, *filename_len);
  tempfile[*filename_len] = '\0';
  if (strncmp (mode, "r", 1) == 0) {
    ge_fp[*file_num] = efopen(tempfile, "r");
    strcpy (file[*file_num], tempfile);
    return (ge_fp[*file_num] == NULL ? 1 : 0);
  } else {
    ge_fp[*file_num] = efopen(tempfile, "w");
    strcpy (file[*file_num], tempfile);
    return (ge_fp[*file_num] == NULL ? 1 : 0);
  }
}

#elif defined VMS						/* VMS */
int open_ge_file( int *file_num, struct dsc$descriptor_s *filename_VMS,
                  struct dsc$descriptor_s *mode_VMS )
{
  char *filename = filename_VMS->dsc$a_pointer;
  int len = filename_VMS->dsc$w_length;
  char *mode = mode_VMS->dsc$a_pointer;

  char tempfile[80];
  strncpy (tempfile, filename, len);
  tempfile[len] = '\0';
  if (strncmp (mode, "r", 1) == 0) {
    ge_fp[*file_num] = efopen(tempfile, "r");
    strcpy (file[*file_num], tempfile);
    return (ge_fp[*file_num] == NULL ? 1 : 0);
  } else {
    ge_fp[*file_num] = efopen(tempfile, "w");
    strcpy (file[*file_num], tempfile);
    return (ge_fp[*file_num] == NULL ? 1 : 0);
  }
}

#elif defined DBOS						/* DBOS */
int "FORTRAN" open_ge_file( int *file_num, char *filename, char *mode,
                            long *len, long *mode_len ) "OPEN_GE_FILE"
{
  char tempfile[80];
  strncpy (tempfile, filename, *len);
  tempfile[*len] = '\0';
  if (strncmp (mode, "r", 1) == 0) {
    ge_fp[*file_num] = efopen(tempfile, "r");
    strcpy (file[*file_num], tempfile);
    return (ge_fp[*file_num] == NULL ? 1 : 0);
  } else {
    ge_fp[*file_num] = efopen(tempfile, "w");
    strcpy (file[*file_num], tempfile);
    return (ge_fp[*file_num] == NULL ? 1 : 0);
  }
}

#elif defined UNDERSCORE
int open_ge_file_( int *file_num, char *filename, char *mode )
{
  char tempfile[80];
  strcpy (tempfile, filename);
  if (strncmp (mode, "r", 1) == 0) {
    ge_fp[*file_num] = efopen(tempfile, "r");
    strcpy (file[*file_num], tempfile);
    return (ge_fp[*file_num] == NULL ? 1 : 0);
  } else {
    ge_fp[*file_num] = efopen(tempfile, "w");
    strcpy (file[*file_num], tempfile);
    return (ge_fp[*file_num] == NULL ? 1 : 0);
  }
}

#elif defined DUNDERSCORE
int open_ge_file__( int *file_num, char *filename, char *mode )
{
  char tempfile[80];
  strcpy (tempfile, filename);
  if (strncmp (mode, "r", 1) == 0) {
    ge_fp[*file_num] = efopen(tempfile, "r");
    strcpy (file[*file_num], tempfile);
    return (ge_fp[*file_num] == NULL ? 1 : 0);
  } else {
    ge_fp[*file_num] = efopen(tempfile, "w");
    strcpy (file[*file_num], tempfile);
    return (ge_fp[*file_num] == NULL ? 1 : 0);
  }
}

#else								/* others */
int open_ge_file( int *file_num, char *filename, char *mode, long *len, 
                  long *mode_len )
{
  char tempfile[80];
  strncpy (tempfile, filename, *len);
  tempfile[*len] = '\0';
  if (strncmp (mode, "r", 1) == 0) {
    ge_fp[*file_num] = efopen(tempfile, "r");
    strcpy (file[*file_num], tempfile);
    return (ge_fp[*file_num] == NULL ? 1 : 0);
  } else {
    ge_fp[*file_num] = efopen(tempfile, "w");
    strcpy (file[*file_num], tempfile);
    return (ge_fp[*file_num] == NULL ? 1 : 0);
  }
}
#endif

#if defined __WATCOMC__						/* WATCOM */
int rewind_ge_file( int *file_num)
{
  rewind (ge_fp[*file_num]);
  return 0;
}

#elif defined VMS						/* VMS */
int rewind_ge_file( int *file_num)
{
  rewind (ge_fp[*file_num]);
  return 0;
}

#elif defined DBOS						/* DBOS */
int "FORTRAN" rewind_ge_file( int *file_num ) "REWIND_GE_FILE"
{
  rewind (ge_fp[*file_num]);
  return 0;
}

#elif defined UNDERSCORE 

int rewind_ge_file_( int *file_num)
{
  rewind (ge_fp[*file_num]);
  return 0;
}
 
#elif defined DUNDERSCORE 

int rewind_ge_file__( int *file_num)
{
  rewind (ge_fp[*file_num]);
  return 0;
}
 
#else								/* others */
int rewind_ge_file( int *file_num)
{
  rewind (ge_fp[*file_num]);
  return 0;
}
#endif

#if defined __WATCOMC__						/* WATCOM */
int close_ge_file( int *file_num)
{
  fclose (ge_fp[*file_num]);
  return 0;
}

#elif defined VMS						/* VMS */
int close_ge_file( int *file_num)
{
  fclose (ge_fp[*file_num]);
  return 0;
}

#elif defined DBOS						/* DBOS */
int "FORTRAN" close_ge_file( int *file_num ) "CLOSE_GE_FILE"
{
  fclose (ge_fp[*file_num]);
  return 0;
}

#elif defined UNDERSCORE
int close_ge_file_( int *file_num)
{
  fclose (ge_fp[*file_num]);
  return 0;
}

#elif defined DUNDERSCORE
int close_ge_file__( int *file_num)
{
  fclose (ge_fp[*file_num]);
  return 0;
}

#else								/* others */
int close_ge_file( int *file_num)
{
  fclose (ge_fp[*file_num]);
  return 0;
}
#endif

#if defined __WATCOMC__						/* WATCOM */
int read_ge_file( int *file_num, char *text, long *text_len )
{
  int len;
  char *status, *p = text;

  status = fgets (text, MAXSIZ, ge_fp[*file_num]);
  len = strlen(text);                         /* String length           */
  if (len > 0 && text[len-1] == '\n') len--;
  if (len == 0 && (*p == '\r' || *p == '\n')) {
    *p = ' ';
    *(++p) = '\0';
    len = 1;
  }
  return len;                                 /* String length           */
}

#elif defined VMS						/* VMS */
int read_ge_file( int *file_num, struct dsc$descriptor_s *text_VMS )
{
  int len;
  char *status;
  char *p = text_VMS->dsc$a_pointer;          /* Write directly to source */

  status = fgets (p, MAXSIZ, ge_fp[*file_num]);
  if (status == 0) {
    len = 0;
  } else {
    len = strlen(p);                          /* String length           */
    if (len > 0 && p[len-1] == '\n') len--;
    if (len == 0 && (*p == '\r' || *p == '\n')) {
      *p = ' ';
      *(++p) = '\0';
      len = 1;
    }
  }
  return len;                                 /* String length           */
}

#elif defined DBOS						/* DBOS */
int "FORTRAN" read_ge_file( int *file_num, char *text, 
                            long *text_len ) "READ_GE_FILE"
{
  int len;
  char *status, *p = text;

  status = fgets (text, MAXSIZ, ge_fp[*file_num]);
  len = strlen(text);                         /* String length           */
  if (len > 0 && text[len-1] == '\n') len--;
  if (len == 0 && (*p == '\r' || *p == '\n')) {
    *p = ' ';
    *(++p) = '\0';
    len = 1;
  }
  return len;                                 /* String length           */

}

#elif defined UNDERSCORE
int read_ge_file_( int *file_num, char *text )
{
  int len;
  char *status, *p = text;

  status = fgets (text, MAXSIZ, ge_fp[*file_num]);
  len = strlen(text);                         /* String length           */
  if (len > 0 && text[len-1] == '\n') len--;
  if (len == 0 && (*p == '\r' || *p == '\n')) {
    *p = ' ';
    *(++p) = '\0';
    len = 1;
  }
  return len;                                 /* String length           */
}

#elif defined DUNDERSCORE
int read_ge_file__( int *file_num, char *text )
{
  int len;
  char *status, *p = text;

  status = fgets (text, MAXSIZ, ge_fp[*file_num]);
  len = strlen(text);                         /* String length           */
  if (len > 0 && text[len-1] == '\n') len--;
  if (len == 0 && (*p == '\r' || *p == '\n')) {
    *p = ' ';
    *(++p) = '\0';
    len = 1;
  }
  return len;                                 /* String length           */
}

#else								/* others */
int read_ge_file( int *file_num, char *text, long *text_len )
{
  int len;
  char *status, *p = text;

  status = fgets (text, MAXSIZ, ge_fp[*file_num]);
  len = strlen(text);                         /* String length           */
  if (len > 0 && text[len-1] == '\n') len--;
  if (len == 0 && (*p == '\r' || *p == '\n')) {
    *p = ' ';
    *(++p) = '\0';
    len = 1;
  }
  return len;                                 /* String length           */
}

#endif

#if defined __WATCOMC__						/* WATCOM */
int write_ge_file( int *file_num, char *text, long *text_len )
{
  char *c = text, 
       *lastc = &text[*text_len];

  for (; c < lastc; c++)
    putc (*c, ge_fp[*file_num]);
  putc ('\n', ge_fp[*file_num]);
  return (ferror(ge_fp[*file_num]) ? EOF : 0);

}

#elif defined VMS						/* VMS */
int write_ge_file( int *file_num, struct dsc$descriptor_s *text_VMS )
{
  int len = text_VMS->dsc$w_length;          /* String length           */
  char *c = text_VMS->dsc$a_pointer,
       *lastc = &c[len];

  for (; c < lastc; c++)
    putc (*c, ge_fp[*file_num]);
  putc ('\n', ge_fp[*file_num]);
  return (ferror(ge_fp[*file_num]) ? EOF : 0);
}

#elif defined DBOS						/* DBOS */
int "FORTRAN" write_ge_file( int *file_num, char *text, 
                             long *text_len ) "WRITE_GE_FILE"
{
  char *c = text, 
       *lastc = &text[*text_len];

  for (; c < lastc; c++)
    putc (*c, ge_fp[*file_num]);
  putc ('\n', ge_fp[*file_num]);
  return (ferror(ge_fp[*file_num]) ? EOF : 0);
}

#elif defined UNDERSCORE
int write_ge_file_( int *file_num, char *text )
{
  char *c = text,
       *lastc = &text[strlen(text)];

  for (; c < lastc; c++)
    putc (*c, ge_fp[*file_num]);
  putc ('\n', ge_fp[*file_num]);
  return (ferror(ge_fp[*file_num]) ? EOF : 0);
}

#elif defined DUNDERSCORE
int write_ge_file__( int *file_num, char *text )
{
  char *c = text,
       *lastc = &text[strlen(text)];

  for (; c < lastc; c++)
    putc (*c, ge_fp[*file_num]);
  putc ('\n', ge_fp[*file_num]);
  return (ferror(ge_fp[*file_num]) ? EOF : 0);
}

#else								/* others */
int write_ge_file( int *file_num, char *text, long *text_len )
{
  char *c = text,
       *lastc = &text[*text_len];

  for (; c < lastc; c++)
    putc (*c, ge_fp[*file_num]);
  putc ('\n', ge_fp[*file_num]);
  return (ferror(ge_fp[*file_num]) ? EOF : 0);
}

#endif
