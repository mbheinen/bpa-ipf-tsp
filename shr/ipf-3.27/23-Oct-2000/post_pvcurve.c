/******************************************************************************\
UTILITY:    PVcurve_post - PVCurve post processor
STRUCTURE:  Standalone 
USAGE:      pvcurve <number> <bad-file#1> <bad-file#2> ... <bat-file#n>
TYPE:       Post processes PVCurve output files
SUMMARY:    Generate voltage reactance curves.
RELATED:    PVCURVE
UPDATED:    6 October 2000
            22 October 2000
LANGUAGE:   Standard C.
DEVELOPER:  Walter Powell 360-418-8810   wlpowelljr@bpa.gov
REQUESTER:  J Randall, G Comegys
IPF:        Version 327 or above recommended.
PURPOSE:    Post processes P-V curve data files, selecting the most critical
            outage studies and rendering the output into an excel-importable
            data file. 
\******************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>

#define RECORDSIZE 132
#define BUFFERSIZE 2048
#define MAXDATA    200
#define MAXTROWS 500

int temp_critical[MAXDATA], temp_pre_critical[MAXDATA];
char *progname;
char debug_filename[60], caselog_filename[60];
FILE *debug_fp, *caselog_fp;

typedef struct {
  int solution;
  char file[60];
  char base[60];
  char outage[60];
  char source[60];
} CRIT;

typedef struct {
  int count;
  char *element[MAXDATA];
} TOKN;

CRIT critical[MAXDATA];
CRIT pre_critical[MAXDATA];

typedef struct {
  char filename[12][60];
  int  transpose_flag;
} FILENM;

void usage_error_exit();
FILE *efopen (char *, char *);
int concat (char *, int);
int concat_post (FILENM *);
int concat_pre (FILENM *);
int parse_input_file (FILE *, char *, int *, int *);
int compare_critical ( const void *, const void *);
int compare_pre_critical ( const void *, const void *);
int get_tokens (char *, TOKN *, char *, int);
int read_next_record (char *, FILE *, int *, int *);
char *extract_dir (char *);
char *strtok_x (char *, char *);

void usage_error_exit()
{  
  fprintf (stderr, "Usage: post_pvcurve -c<num> [-d <debug_file>] [-t] <file#1n.bad> [<file#2.bad> ... <file#n.bad>]\n");
  fprintf (stderr, "   -c<num> = number of critical cases selected (must be > 1) \n");
  fprintf (stderr, "   -d <debug_file> = optional debug file\n");
  fprintf (stderr, "   -t = generate transposed output file\n");
  fprintf (stderr, "   <file#1.bad> ... <file#n.bad> - individual *.bad files\n");
  exit (1);
}

/*----------------------------------------------------------------
 *
 * main:  Main program 
 *
 *        1. Open and process input *.bad files
 *        2. Select most critical cases
 *        3. Processes each critical case, concatenating and optionally
 *           transposing the data.
 *
 *----------------------------------------------------------------
 * Routine structure:
 *
 * main                      -- main driver routine
 *   usage_error_exit        -- usage error exit
 *   parse_input_file        -- parse individual *.BAD files
 *   qsort                   -- sort *.BAD structure
 *   concat                  -- generate concatenated output files
 *----------------------------------------------------------------
 */

int main(int argc, char *argv[])
{
  FILE *fptr[10];
  char fname[10][60], *ptr, outbuf[132], 
       *day[7] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"},
       *month[12] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
 
  int i, j, max_critical, transpose_flag = 0, num_input_files = 0,
      status, num_critical = 0, num_pre_critical = 0, year;
  time_t *tptr, vtptr;
  struct tm *xtm;

  progname = argv[0];
  strcpy (debug_filename, "");
  debug_fp = NULL;

  if (argc < 3) {
    fprintf(stderr, "Missing arguments \n");
    usage_error_exit();
  }
  for (i=0; i<10; i++) {
    strcpy (fname[i], "");
    fptr[i] = NULL;
  }
  for (i=1; i<argc; i++) {
    if (strncmp (argv[i], "-c", 2) == 0) {
      max_critical = atoi (&argv[i][2]);
      if (max_critical < 1) {
        fprintf(stderr, "Maximum critical studies must be > 0\n");
        usage_error_exit();
      }
    } else if (strncmp (argv[i], "-d", 2) == 0) {
      strcpy (debug_filename, argv[++i]);
      debug_fp = efopen (debug_filename, "w");
      if (debug_fp == NULL) {
        fprintf(stderr, "Cannot open file %s\n", debug_filename);
        usage_error_exit();
      }
    } else if (strncmp (argv[i], "-t", 2) == 0) {
      transpose_flag = 1;
    } else if (strncmp (argv[i], "-", 1) == 0) {
      fprintf(stderr, "Unknown option [%s]\n", argv[i]);
      usage_error_exit();
    } else {
      strcpy (fname[num_input_files], argv[i]);
      fptr[num_input_files] = efopen (fname[num_input_files], "r");
      if (fptr[num_input_files] == NULL) {
        fprintf(stderr, "Cannot open file %s\n", fname[num_input_files]);
        usage_error_exit();
      } else {
        status = parse_input_file (fptr[num_input_files], fname[num_input_files],
                                   &num_critical, &num_pre_critical);
      }
      num_input_files++;
    }
  }
  if (num_input_files == 0) {
    fprintf(stderr, "No input files opened%s\n");
    usage_error_exit();
  } else {
    for (i=0; i<num_input_files; i++) {
      fclose (fptr[i]);
    }
  }
  if (num_critical == 0) {
    fprintf(stderr, "No critical data extracted from input files%s\n", fname[2]);
    usage_error_exit();
  } else if (num_critical >= MAXDATA) {
    fprintf(stderr, "Too many critical data entities [%d] extracted from input files%s\n",
      num_critical, fname[2]);
    usage_error_exit();
  } else if (max_critical > num_critical) {
    fprintf(stderr, " [%d] 'case failed' entities found in *.BAD file(s)\n",
      num_critical);
    fprintf(stderr, " [%d] user-specified value is relaxed to [%d]\n", 
      max_critical, num_critical);
    max_critical = num_critical;
  }

/* Sort struct critical by solution number */

  for (i=0; i<MAXDATA; i++) temp_critical[i] = i;
  qsort ((void *) temp_critical, (size_t) num_critical, sizeof (int),
                  compare_critical);

/* Sort struct pre_critical by solution number */

  if (num_pre_critical > 0) {
    for (i=0; i<MAXDATA; i++) temp_pre_critical[i] = i;
    qsort ((void *) temp_pre_critical, (size_t) num_pre_critical, sizeof (int),
                    compare_pre_critical);
  }

/* Generate caselog filename from first *.BAD case */

  ptr = extract_dir (fname[0]);
  strcpy (caselog_filename, ptr);

  ptr = strstr (caselog_filename, ".BAD");
  if (ptr == NULL) ptr = strstr (caselog_filename, ".bad");
  if (ptr != NULL) {
    *ptr = '\x00';
    strcat (caselog_filename, "XX.SUM");
  } else {
    strcpy (caselog_filename, "NONAMEXX.SUM");
  }
  caselog_fp = efopen (caselog_filename, "w");

  tptr = &vtptr;
  time(tptr);
  xtm = localtime(tptr);

  fprintf (caselog_fp, " Case summary of Post-PVCURVE version 1.01\n");
  year = 1900 + xtm->tm_year;
  fprintf (caselog_fp, " Executed on %s %2d-%s-%d at %2d:%2d%:%2d\n\n",
    day[xtm->tm_wday], xtm->tm_mday, month[xtm->tm_mon], year, xtm->tm_hour, 
    xtm->tm_min, xtm->tm_sec);
  fprintf (caselog_fp, " Sorted contingency cases from input *.BAD files\n\n");
  fprintf (caselog_fp, " Solution   Outage                      File\n\n");

  fprintf (stderr, " Case summary of Post-PVCURVE version 1.01\n");
  year = 1900 + xtm->tm_year;
  fprintf (stderr, " Executed on %s %2d-%s-%d at %2d:%2d%:%2d\n\n",
    day[xtm->tm_wday], xtm->tm_mday, month[xtm->tm_mon], year, xtm->tm_hour, 
    xtm->tm_min, xtm->tm_sec);
  fprintf (stderr, " Sorted contingency cases from input *.BAD files\n\n");
  fprintf (stderr, " Solution  Outage                       File\n\n");

  for (i=0; i<num_critical; i++) {
    j = temp_critical[i];
    fprintf (caselog_fp, "     %3d    %-26s %s\n", critical[j].solution, 
      critical[j].outage, critical[j].file);
    fprintf (stderr, "     %3d    %-26s %s\n", critical[j].solution, 
      critical[j].outage, critical[j].file);
  }

  fprintf (caselog_fp, "\n Sorted pre outage cases from input *.BAD files\n\n");
  fprintf (caselog_fp, " Solution   Outage                      File\n\n");

  fprintf (stderr, "\n Sorted pre outage cases from input *.BAD files\n\n");
  fprintf (stderr, " Solution   Outage                      File\n\n");

  for (i=0; i<num_pre_critical; i++) {
    j = temp_pre_critical[i];
    fprintf (caselog_fp, "     %3d    %-26s %s\n", pre_critical[j].solution, 
      pre_critical[j].outage, pre_critical[j].file);
    fprintf (stderr, "     %3d    %-26s %s\n", pre_critical[j].solution, 
      pre_critical[j].outage, pre_critical[j].file);
  }

  for (i=0; i<max_critical; i++) {
    j = temp_critical[i];
    status = concat (critical[j].file, transpose_flag);
  }

  fclose (caselog_fp);
  fprintf (stderr, " Case summary saved on file %s\n", caselog_filename);
}

FILE *efopen(char *file, char *mode) /* fopen file, die if can't */
{
  FILE *fp;
  if ((fp = fopen(file, mode)) == NULL) {
    fprintf(stderr, "%s: Can't open file %s mode %s\n",  progname, file, mode);
    exit(1);
  }
  return fp;
}

/*----------------------------------------------------------------
 *
 * concat:  Concatenate PVCURVE files
 *
 *        1. Open 4 pre-contingency cases and 4 post-contingency cases
 *        2. Merge two sets of files according
 *           file 1: (PRE MW) + (POST DV) (PRE KV) + (POST KV) + (PRE QQ) + (POST KV)
 *                2: (PRE MW) + (PRE DV) + (PRE QQ) + (PRE KV)
 *        3. Write new files above as type *CNNXX.TXT and *_0XX.TXT (normal)
 *           and *CNNXXT.TXT and *_0XXT.TXT (transposed).
 *
 *----------------------------------------------------------------
 * Routine structure:
 *
 * concat                    -- main driver routine
 *   usage_error_exit        -- usage error exit
 *   parse_input_file        -- parse individual *.BAD files
 *----------------------------------------------------------------
 */

int concat (char *name, int transpose_flag)
{
  char tempname[60], newtempname[60], filename[12][60], 
       *ptr; 
  int i, len, status;
  static int num_new_pre_cntg_files = 0;
  FILENM filenm;
  
  filenm.transpose_flag = transpose_flag;
  strcpy (tempname, name);
  ptr = strstr (tempname, ".TXT");
  if (ptr == NULL) ptr = strstr (tempname, ".txt");
  if (ptr == NULL) {
    fprintf (stderr, "Cannot parse .TXT from input file %s\n", name);
    usage_error_exit();
  } else {
    ptr--;
    ptr--;
    *ptr = '\x00';
  }

  ptr = extract_dir (tempname);   /* ptr contains file name sans directory */
  strcpy (newtempname, ptr);

/* Generate complete set of post-contintency file names */

  strcpy (filenm.filename[4], tempname);
  strcpy (filenm.filename[5], tempname);
  strcpy (filenm.filename[6], tempname);
  strcpy (filenm.filename[7], tempname);
  strcat (filenm.filename[4], "MW.TXT");
  strcat (filenm.filename[5], "DV.TXT");
  strcat (filenm.filename[6], "KV.TXT");
  strcat (filenm.filename[7], "QQ.TXT");
  strcpy (filenm.filename[8], newtempname);
  strcpy (filenm.filename[10], newtempname);
  strcat (filenm.filename[8], "XX.TXT");
  strcat (filenm.filename[10], "XXT.TXT");

/* Generate complete set of pre-contintency file names */

/* Eliminate character C and digits NN in filename ...CNNMW.TXT */

  len = strlen (tempname);
  while (len > 0 && isdigit (tempname[len-1])) len--;
  while (len > 0 && tempname[len-1] == 'C') len--;
  tempname[len++] = '_';
  tempname[len++] = '0';
  tempname[len] = '\x00';

  ptr = extract_dir (tempname);   /* ptr contains file name sans directory */
  strcpy (newtempname, ptr);

  strcpy (filenm.filename[0], tempname);
  strcpy (filenm.filename[1], tempname);
  strcpy (filenm.filename[2], tempname);
  strcpy (filenm.filename[3], tempname);
  strcat (filenm.filename[0], "MW.TXT");
  strcat (filenm.filename[1], "DV.TXT");
  strcat (filenm.filename[2], "KV.TXT");
  strcat (filenm.filename[3], "QQ.TXT");
  strcpy (filenm.filename[9], newtempname);
  strcpy (filenm.filename[11], newtempname);
  strcat (filenm.filename[9], "XX.TXT");
  strcat (filenm.filename[11], "XXT.TXT");

/*
 * Generate post_contingency file *XX.TXT
 *
 *  (PRE MW) + (POST DV) + (PRE KV) + (POST KV) + (PRE QQ) + (POST QQ)
 *     [0]        [5]        [2]         [6]       [3]         [7]
 */

  status = concat_post ( &filenm);

/*
 * Generate pre_contingency file *_0YY.TXT
 *
 *  (PRE MW) + (PRE DV) + (PRE KV) + (POST QQ)
 *    [0]         [1]        [2]       [3]
 */

  if (num_new_pre_cntg_files++ == 0) {
    status = concat_pre ( &filenm);
  }

  return 0;
}

/*----------------------------------------------------------------
 *
 * concat_post:  Concatenate post PVCURVE files
 *
 *        1. Open 4 pre-contingency cases and 4 post-contingency cases
 *        2. Concatenate the two sets of files in the following order:
 *
 *           (PRE MW) + (POST DV) (PRE KV) + (POST KV) + (PRE QQ) + (POST KV)
 *
 *----------------------------------------------------------------
 * Routine structure:
 *
 * concat                    -- main driver routine
 *   usage_error_exit        -- usage error exit
 *   parse_input_file        -- parse individual *.BAD files
 *----------------------------------------------------------------
 */

int concat_post (FILENM *filenm)
{
  char inbuf[8][BUFFERSIZE], outbuf[6*BUFFERSIZE], post_tbuf[MAXTROWS][BUFFERSIZE],
       pre_tbuf[MAXTROWS][BUFFERSIZE], *ptr; 
  int len, i, num_rows, num_columns, status, max_token, t_row, max_t_row, 
      max_t_column;
  FILE *f[12];
  TOKN token[8];

/* Open all input files */

  for (i=0; i<8; i++) {
    f[i] = efopen(filenm->filename[i], "r");
  }

  num_rows = 0;
  max_t_row = 0;

  f[8] = efopen(filenm->filename[8], "w");
  f[10] = efopen(filenm->filename[10], "w");

  if (filenm->transpose_flag == 1) {
    for (i=0; i<MAXTROWS; i++) strcpy (post_tbuf[i], "");
    for (i=0; i<MAXTROWS; i++) strcpy (pre_tbuf[i], "");
  }

  while (fgets(inbuf[0], sizeof inbuf[0], f[0]) != NULL &&
         fgets(inbuf[1], sizeof inbuf[1], f[1]) != NULL &&
         fgets(inbuf[2], sizeof inbuf[2], f[2]) != NULL &&
         fgets(inbuf[3], sizeof inbuf[3], f[3]) != NULL &&
         fgets(inbuf[4], sizeof inbuf[4], f[4]) != NULL &&
         fgets(inbuf[5], sizeof inbuf[5], f[5]) != NULL &&
         fgets(inbuf[6], sizeof inbuf[6], f[6]) != NULL &&
         fgets(inbuf[7], sizeof inbuf[7], f[7])) {

    num_rows++;
    if (num_rows < 4) {
      strcpy (outbuf, inbuf[0]);
    } else {
      max_token = 0;
      for (i=0; i<8; i++) {
        status = get_tokens (inbuf[i], &token[i], filenm->filename[i], num_rows);
        if (token[i].count > max_token) max_token = token[i].count;
      }
      strcpy (outbuf, token[0].element[0]);           /* Step number  */
      strcat (outbuf, " | ");
      strcat (outbuf, token[0].element[1]);      
      strcat (outbuf, " | ");

      num_columns = 2;

/*    Grouped by cut planes */

      i = 2;
      while (i < max_token) {
        if (i < token[0].count) {
          strcat (outbuf, token[0].element[i]);        /* PRE MW       */
          strcat (outbuf, " | ");
          num_columns++;
        }
        i++;
      }

      i = 2;
/*    Grouped by bus */

      while (i < max_token) {
        if (i < token[5].count) {
          strcat (outbuf, token[5].element[i]);        /* POST DV      */   
          strcat (outbuf, " | ");
          num_columns++;
        }
        if (i < token[2].count) {
          strcat (outbuf, token[2].element[i]);        /* PRE KV       */    
          strcat (outbuf, " | ");
          num_columns++;
        }
        if (i < token[6].count) {
          strcat (outbuf, token[6].element[i]);        /* POST KV      */     
          strcat (outbuf, " | ");
          num_columns++;
        }
        i++;
      }

/*    Grouped by generators */

      i = 2;
      while (i < max_token) {
        if (i < token[3].count) {
          strcat (outbuf, token[3].element[i]);        /* PRE QQ       */   
          strcat (outbuf, " | ");
          num_columns++;
        }
        if (i < token[7].count) {
          strcat (outbuf, token[7].element[i]);        /* POST QQ      */      
          strcat (outbuf, " | ");
          num_columns++;
        }
        i++;
      }

/*    Write transposed data.  The ultimate number of columns (t_row<MAXTROWS) 
 *    is unknown until the last record is read in.
 */
      if (filenm->transpose_flag == 1) {
        t_row = 0;
        strcat (post_tbuf[t_row], token[0].element[0]);
        strcat (post_tbuf[t_row++], " | ");
        strcat (post_tbuf[t_row], token[0].element[1]);
        strcat (post_tbuf[t_row++], " | ");

        i = 2;
        while (i < max_token) {
          if (i < token[0].count) {
            strcat (post_tbuf[t_row], token[0].element[i]);        /* PRE MW       */
            strcat (post_tbuf[t_row++], " | ");
          }
          i++;
        }

        i = 2;
        while (i < max_token) {
          if (i < token[5].count) {
            strcat (post_tbuf[t_row], token[5].element[i]);        /* POST DV      */   
            strcat (post_tbuf[t_row++], " | ");
          }
          if (i < token[2].count) {
            strcat (post_tbuf[t_row], token[2].element[i]);        /* PRE KV       */    
            strcat (post_tbuf[t_row++], " | ");
          }
          if (i < token[6].count) {
            strcat (post_tbuf[t_row], token[6].element[i]);        /* POST KV      */     
            strcat (post_tbuf[t_row++], " | ");
          }
          i++;
        }

        i = 2;
        while (i < max_token) {
          if (i < token[3].count) {
            strcat (post_tbuf[t_row], token[3].element[i]);        /* PRE QQ       */   
            strcat (post_tbuf[t_row++], " | ");
          }
          if (i < token[7].count) {
            strcat (post_tbuf[t_row], token[7].element[i]);        /* POST QQ      */      
            strcat (post_tbuf[t_row++], " | ");
          }
          i++;
        }

        if (t_row > max_t_row) max_t_row = t_row;
        for (i=max_t_row; i<MAXTROWS; i++) {
          if (post_tbuf[i][0] != '\x00') {
            fprintf (stderr, "Overflow post_tbuf[%d] = [%s]\n", i, post_tbuf[i]);
          }
        }
      }
    }  
    fprintf (f[8], "%s\n", outbuf);
  }

  fprintf (caselog_fp, "------------------------------------------------------\n");
  fprintf (caselog_fp, " New post-contingency master file %s %d rows %d columns\n", 
    filenm->filename[8], num_rows, num_columns);

  fprintf (stderr, "------------------------------------------------------\n");
  fprintf (stderr, " New post-contingency master file %s %d rows %d columns\n", 
    filenm->filename[8], num_rows, num_columns);

  fclose (f[8]);
  if (filenm->transpose_flag == 1) {
    i = 0;
    while (post_tbuf[i][0] != '\x00') {
      fprintf (f[10], "%s\n", post_tbuf[i++]);
    }
    max_t_column = 1;
    ptr = strstr (post_tbuf[4], "|");
    while (ptr != NULL) {
      max_t_column++;
      ptr = strstr (++ptr, "|");
    }
    fclose (f[10]);

    fprintf (caselog_fp, " New transposed post-contingency master file %s %d rows %d columns\n", 
      filenm->filename[10], max_t_row, max_t_column);
    fprintf (stderr, " New transposed post-contingency master file %s %d rows %d columns\n", 
      filenm->filename[10], max_t_row, max_t_column);
  }

  for (i=0; i<1; i++)  fprintf (caselog_fp, " Input Pre-contingency files   %s\n", 
    filenm->filename[i]);  
  for (i=1; i<4; i++)  fprintf (caselog_fp, "                               %s\n", 
    filenm->filename[i]);  
  for (i=4; i<5; i++)  fprintf (caselog_fp, " Input Post-contingency files  %s\n", 
    filenm->filename[i]);  
  for (i=5; i<8; i++)  fprintf (caselog_fp, "                               %s\n", 
    filenm->filename[i]);  

  for (i=0; i<1; i++)  fprintf (stderr, " Input Pre-contingency files   %s\n", 
    filenm->filename[i]);  
  for (i=1; i<4; i++)  fprintf (stderr, "                               %s\n", 
    filenm->filename[i]);  
  for (i=4; i<5; i++)  fprintf (stderr, " Input Post-contingency files  %s\n", 
    filenm->filename[i]);  
  for (i=5; i<8; i++)  fprintf (stderr, "                               %s\n", 
    filenm->filename[i]);  


  fprintf (caselog_fp, "------------------------------------------------------\n");
  fprintf (stderr, "------------------------------------------------------\n");

/* Close all input files */

  for (i=0; i<8; i++) {
    fclose (f[i]);
  }

  return 0;
}

/*----------------------------------------------------------------
 *
 * concat_pre:  Concatenate pre PVCURVE files
 *
 *        1. Open 4 pre-contingency cases 
 *        2. Concatenate the files in the following order:
 *
 *           (PRE MW) + (PRE DV) + (PRE KV) + (PRE QQ)
 *              [0]       [1]        [2]         [3]
 *----------------------------------------------------------------
 * Routine structure:
 *
 * concat                    -- main driver routine
 *   usage_error_exit        -- usage error exit
 *   parse_input_file        -- parse individual *.BAD files
 *----------------------------------------------------------------
 */

int concat_pre (FILENM *filenm)
{
  char inbuf[8][BUFFERSIZE], outbuf[6*BUFFERSIZE], post_tbuf[MAXTROWS][BUFFERSIZE],
       pre_tbuf[MAXTROWS][BUFFERSIZE], *ptr; 
  int len, i, num_rows, num_columns, status, max_token, t_row, max_t_row, 
      max_t_column;
  FILE *f[12];
  TOKN token[8];
  
/* Open all pre-contingency input files */

  for (i=0; i<4; i++) {
    f[i] = efopen(filenm->filename[i], "r");
  }

  num_rows = 0;
  max_t_row = 0;
  f[9] = efopen(filenm->filename[9], "w");
  f[11] = efopen(filenm->filename[11], "w");

  if (filenm->transpose_flag == 1) {
    for (i=0; i<MAXTROWS; i++) strcpy (post_tbuf[i], "");
    for (i=0; i<MAXTROWS; i++) strcpy (pre_tbuf[i], "");
  }

  while (fgets(inbuf[0], sizeof inbuf[0], f[0]) != NULL &&
         fgets(inbuf[1], sizeof inbuf[1], f[1]) != NULL &&
         fgets(inbuf[2], sizeof inbuf[2], f[2]) != NULL &&
         fgets(inbuf[3], sizeof inbuf[3], f[3]) != NULL ) {

    num_rows++;
    if (num_rows < 4) {
      strcpy (outbuf, inbuf[0]);
    } else {
      max_token = 0;
      for (i=0; i<4; i++) {
        status = get_tokens (inbuf[i], &token[i], filenm->filename[i], num_rows);
        if (token[i].count > max_token) max_token = token[i].count;
      }
      strcpy (outbuf, token[0].element[0]);           /* Step number  */
      strcat (outbuf, " | ");
      strcat (outbuf, token[0].element[1]);      
      strcat (outbuf, " | ");
      num_columns = 2;

/*    Grouped by cut-planes */

      i = 2;
      while (i < max_token) {
        if (i < token[0].count) {
          strcat (outbuf, token[0].element[i]);        /* PRE MW       */
          strcat (outbuf, " | ");
          num_columns++;
        }
        i++;
      }

      i = 2;
      while (i < max_token) {
        if (i < token[1].count) {
          strcat (outbuf, token[1].element[i]);        /* PRE DV       */   
          strcat (outbuf, " | ");
          num_columns++;
        }
        if (i < token[2].count) {
          strcat (outbuf, token[2].element[i]);        /* PRE KV       */    
          strcat (outbuf, " | ");
          num_columns++;
        }
        i++;
      }

      i = 2;
      while (i < max_token) {
        if (i < token[3].count) {
          strcat (outbuf, token[3].element[i]);        /* PRE QQ       */     
          strcat (outbuf, " | ");
          num_columns++;
        }
        i++;
      }

/*    Write transposed data.  The ultimate number of columns (t_row<MAXTROWS) 
 *    is unknown until the last record is read in.
 */
      if (filenm->transpose_flag == 1) {
        t_row = 0;
        strcat (pre_tbuf[t_row], token[0].element[0]);
        strcat (pre_tbuf[t_row++], " | ");
        strcat (pre_tbuf[t_row], token[1].element[1]);
        strcat (pre_tbuf[t_row++], " | ");

        i = 2;
        while (i < max_token) {
          if (i < token[0].count) {
            strcat (pre_tbuf[t_row], token[0].element[i]);        /* PRE MW       */
            strcat (pre_tbuf[t_row++], " | ");
          }
          i++;
        }

        i = 2;
        while (i < max_token) {
          if (i < token[1].count) {
            strcat (pre_tbuf[t_row], token[1].element[i]);        /* PRE DV       */   
            strcat (pre_tbuf[t_row++], " | ");
          }
          if (i < token[2].count) {
            strcat (pre_tbuf[t_row], token[2].element[i]);        /* PRE KV       */    
            strcat (pre_tbuf[t_row++], " | ");
          }
          i++;
        }

        i = 2;
        while (i < max_token) {
          if (i < token[3].count) {
            strcat (pre_tbuf[t_row], token[3].element[i]);        /* PRE QQ       */     
            strcat (pre_tbuf[t_row++], " | ");
          }
          i++;
        }

        if (t_row > max_t_row) max_t_row = t_row;
        for (i=max_t_row; i<MAXTROWS; i++) {
          if (pre_tbuf[i][0] != '\x00') {
            fprintf (stderr, "Overflow pre_tbuf[%d] = [%s]\n", i, pre_tbuf[i]);
          }
        }
      }
    }  
    fprintf (f[9], "%s\n", outbuf);  
  }

  fprintf (caselog_fp, "------------------------------------------------------\n");
  fprintf (caselog_fp, " New pre-contingency master file %s rows %d columns %d\n", 
    filenm->filename[9], num_rows, num_columns);

  fprintf (stderr, "------------------------------------------------------\n");
  fprintf (stderr, " New pre-contingency master file %s rows %d columns %d\n", 
    filenm->filename[9], num_rows, num_columns);

  fclose (f[9]);
  if (filenm->transpose_flag == 1) {
    i = 0;
    while (pre_tbuf[i][0] != '\x00') {
      fprintf (f[11], "%s\n", pre_tbuf[i++]);
    }
    max_t_column = 0;
    ptr = strstr (pre_tbuf[0], "|");
    while (ptr != NULL) {
      max_t_column++;
      ptr = strstr (++ptr, "|");
    }
    fclose (f[11]);
    fprintf (caselog_fp, " New transposed pre-contingency master file %s %d rows %d columns\n", 
      filenm->filename[11], max_t_row, max_t_column);
    fprintf (stderr, " New transposed pre-contingency master file %s %d rows %d columns\n", 
      filenm->filename[11], max_t_row, max_t_column);
  }

  for (i=0; i<1; i++)  fprintf (caselog_fp, " Input Pre-contingency files  %s\n", 
    filenm->filename[i]);  
  for (i=1; i<4; i++)  fprintf (caselog_fp, "                              %s\n", 
    filenm->filename[i]);  
  for (i=0; i<1; i++)  fprintf (stderr, " Input Pre-contingency files  %s\n", 
    filenm->filename[i]);  
  for (i=1; i<4; i++)  fprintf (stderr, "                              %s\n", 
    filenm->filename[i]);  

  fprintf (caselog_fp, "------------------------------------------------------\n");
  fprintf (stderr, "------------------------------------------------------\n");

/* Close all input files */

  for (i=0; i<4; i++) {
    fclose (f[i]);
  }
  return 0;
}


/*----------------------------------------------------------------
 *
 * Integer function parse_input_files loads the contents of *.BAD
 * files into structure critical for subsequent processing
 *
 *----------------------------------------------------------------
 * Calling parameters:
 *
 * Parameters:
 *     *f1  - file pointer to input file
 *     *num - current index to stucture critical
 *----------------------------------------------------------------
 */

int parse_input_file (FILE *f1, char *fname, int *num_critical, 
                      int *num_pre_critical)
{
  int i, len, status = 0, eof, first, ntok, num = 0;
  char inbuf[RECORDSIZE], *token[50], *ptr, directory[60];

/* Extract the VAX or Linux directory name from the file name */

  strcpy (directory, fname);
  len = strlen (directory);
  i = len-1;
  while (i > 0 && directory[i] != ']' && directory[i] != '/' && directory[i] != '\\') i--;
  if (directory[i] == ']' || directory[i] == '/' || directory[i] == '\\') {
    directory[++i] = '\x00';
  } else {
    directory[i] = '\x00';
  }
  strcpy (critical[*num_critical].source, fname);

  status = read_next_record (inbuf, f1, &eof, &num);
  if (eof == 0) {
    fprintf (stderr, "Error reading input file %s\n", fname);
    usage_error_exit();
  }
  fprintf (stderr, " Loading file %s - %s\n", fname, inbuf);
  first = 0;
  eof = 1;
  while (eof == 1) {
    status = read_next_record (inbuf, f1, &eof, &num);
    if (eof == 1 && strncmp (inbuf, "Case failed at", 
                             strlen("Case failed at")) == 0) {
      ntok = 0;
      ptr = strstr (inbuf, "\r");
      while (ptr != NULL) {
        *ptr = ' ';
        ptr = strstr (inbuf, "\r");
      }   
      ptr = strstr (inbuf, "\n");
      while (ptr != NULL) {
        *ptr = ' ';
        ptr = strstr (inbuf, "\n");
      }   
      ptr = strtok (inbuf, " ");
      while (ptr != NULL) {
        token[ntok++] = ptr;
        ptr = strtok (NULL, " ");
      }
      assert (ntok == 12);
      critical[*num_critical].solution = atof (token[4]);
      strcpy (critical[*num_critical].file, directory);
      strcat (critical[*num_critical].file, token[ntok-1]);
      status = read_next_record (inbuf, f1, &eof, &num);

      if (eof == 0) {
        fprintf (stderr, "Missing trailer record %din input file %s\n", fname);
        usage_error_exit();
      } else {
        ptr = strtok (inbuf, " ");
        while (ptr != NULL) {
          token[ntok++] = ptr;
          ptr = strtok (NULL, " ");
        }
        strcpy (critical[*num_critical].base, token[12]);
        strcpy (critical[*num_critical].outage, token[14]);
        (*num_critical)++;
      }
    } else if (eof == 1 && strncmp (inbuf, "Pre Outage Case failed at", 
                                    strlen("Pre Outage Case failed at")) == 0) {
      ntok = 0;
      ptr = strstr (inbuf, "\r");
      while (ptr != NULL) {
        *ptr = ' ';
        ptr = strstr (inbuf, "\r");
      }   
      ptr = strstr (inbuf, "\n");
      while (ptr != NULL) {
        *ptr = ' ';
        ptr = strstr (inbuf, "\n");
      }   
      ptr = strtok (inbuf, " ");
      while (ptr != NULL) {
        token[ntok++] = ptr;
        ptr = strtok (NULL, " ");
      }
      assert (ntok == 10);
      pre_critical[*num_pre_critical].solution = atof (token[6]);
      strcpy (pre_critical[*num_pre_critical].file, directory);
      strcat (pre_critical[*num_pre_critical].file, token[ntok-1]);
      status = read_next_record (inbuf, f1, &eof, &num);

      if (eof == 0) {
        fprintf (stderr, "Missing trailer record %din input file %s\n", fname);
        usage_error_exit();
      } else {
        ptr = strtok (inbuf, " ");
        while (ptr != NULL) {
          token[ntok++] = ptr;
          ptr = strtok (NULL, " ");
        }
        strcpy (pre_critical[*num_pre_critical].base, token[10]);
        strcpy (pre_critical[*num_pre_critical].outage, "{Pre-Contingency}");
        (*num_pre_critical)++;
      }
    }
  }
  return 0;
}

/*----------------------------------------------------------------
 *
 * Integer function get_tokens loads parses the input character
 * string into tokens demarcated with "|".
 *
 *----------------------------------------------------------------
 * Calling parameters:
 *
 * Parameters:
 *     *buffer - character string to be parsed
 *     *token  - parsed tokens
 *     *filename - parsed file name for debug purposes
 *----------------------------------------------------------------
 */

int get_tokens (char *buffer, TOKN *token, char *filename, int num_rows)
{
  int ntok = 0;
  char *ptr;
  
/* replace any DOS '\r' with ' ' */

  ptr = strstr (buffer, "\r");
  while (ptr != NULL) {
    *ptr = ' ';
    ptr = strstr (buffer, "\r");
  }
  ptr = strstr (buffer, "\n");
  while (ptr != NULL) {
    *ptr = ' ';
    ptr = strstr (buffer, "\n");
  }
  ptr = strtok_x (buffer, "|");
  while (ptr != NULL) {
    token->element[ntok] = ptr;

    if (debug_fp != NULL) {
      fprintf (debug_fp, " File %s record %d token[%d] = [%s]\n", 
        filename, num_rows, ntok, token->element[ntok]);
    }

    ntok++;
    token->element[ntok] = NULL;
    ptr = strtok_x (NULL, "|");
  }
  token->count = ntok;
  return 0;
}


/*----------------------------------------------------------------
 *
 * Integer function read_next_record reads single records from the input
 * files.
 *
 *----------------------------------------------------------------
 * Calling parameters:
 *
 * Parameters:
 *     *f1  - file pointer to input file
 *     *num - current index to stucture critical
 *----------------------------------------------------------------
 */

int read_next_record (char *inbuf, FILE *f1, int *loop, int *numrec)
{
  char *ptr;

  ptr = fgets (inbuf, RECORDSIZE, f1);

  if (debug_fp != NULL) {
    fprintf (debug_fp, "Record  %5d [%s]\n", ++(*numrec), inbuf);
  }

  *loop = (ptr == NULL) ? 0 : 1;
  return 0;
}


/*----------------------------------------------------------------
 *
 * Integer function compare_critical performs the qsort comparison for
 * sorting structure critical_files by the field "solution".
 *
 *----------------------------------------------------------------
 * Calling parameters:
 *
 * Parameters:
 *     op1  - pointer to critical_files[] from pointer in temp_critical[]
 *     op2  - pointer to critical_files[] from pointer in temp_critical[]
 *----------------------------------------------------------------
 */

int compare_critical ( const void *op1, const void *op2 )
{
  const int *p1 = (const int *) op1;
  const int *p2 = (const int *) op2;
  int compare, solution1, solution2;
  solution1 = critical[*p1].solution;
  solution2 = critical[*p2].solution;
  compare = solution1 - solution2;
  return compare;
}

/*----------------------------------------------------------------
 *
 * Integer function compare_pre_critical performs the qsort comparison for
 * sorting structure pre_critical_files by the field "solution".
 *
 *----------------------------------------------------------------
 * Calling parameters:
 *
 * Parameters:
 *     op1  - pointer to critical_files[] from pointer in temp_pre_critical[]
 *     op2  - pointer to critical_files[] from pointer in temp_pre_critical[]
 *----------------------------------------------------------------
 */

int compare_pre_critical ( const void *op1, const void *op2 )
{
  const int *p1 = (const int *) op1;
  const int *p2 = (const int *) op2;
  int compare, solution1, solution2;
  solution1 = pre_critical[*p1].solution;
  solution2 = pre_critical[*p2].solution;
  compare = solution1 - solution2;
  return compare;
}

/*----------------------------------------------------------------
 *
 * Character function strtok_x is a variation of the standard
 * library function strtok.  It is contrived to circumvent the
 * problem of returning NULL strings for fields delimited by
 * consecutive delimiters -- as it must for blank delimiters.  
 * This version returns for consecutive delimiters a non-null 
 * but zero length string. NULL is reserved for end-of-record.
 *
 *----------------------------------------------------------------
 * Calling parameters:
 *
 * Parameters:
 *     inbuf  - First call - pointer to input buffer to be parsed.
 *     string - delimiter string
 *----------------------------------------------------------------
 */

char *strtok_x (char *inbuf, char *string)
{
  static char *ptr, *last_ptr, *temp_ptr;

  if (inbuf != NULL) {
    last_ptr = inbuf;
  }
  if (last_ptr == NULL) return NULL;
  ptr = strstr (last_ptr, string);
  if (ptr != NULL) {
    *ptr = '\x00';
    temp_ptr = last_ptr;
    last_ptr = &ptr[strlen(string)];
  } else if (strlen(last_ptr) > 0) {
    temp_ptr = last_ptr;
    last_ptr = &last_ptr[strlen(last_ptr)];
  } else {
    last_ptr = NULL;
    temp_ptr = NULL;
  }
  return temp_ptr;
}    


/*----------------------------------------------------------------
 *
 * Character function extract_dir extracts the VAX or Linux directory 
 * name from the file name.
 *
 *----------------------------------------------------------------
 * Calling parameters:
 *
 * Parameters:
 *     file  - file name including directory
 *----------------------------------------------------------------
 */

char *extract_dir (char *filename)
{
  int i, len = strlen (filename);
  char *ptr;

  i = len-1;
  while (i > 0 && filename[i] != ']' && 
                  filename[i] != '/' && 
                  filename[i] != '\\') i--;
  if (filename[i] == ']' || 
      filename[i] == '/' || 
      filename[i] == '\\') {
    ptr = &filename[++i];
  } else {
    ptr = filename;
  }
  return ptr;
}    
