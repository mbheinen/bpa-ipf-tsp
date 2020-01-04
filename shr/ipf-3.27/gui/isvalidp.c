static char sccsid[]="@(#)isvalidp.c	20.2 2/15/95";
/*
*           IPF V2.x: Interactive PowerFlow Version 2.x
*              Bonneville Power Administration (BPA)
*         Authors:  D. L. Clark, J. G. Coleman, W. L. Powell, 
*                B. E. Rogers, K. E. Rowell, J. L. Rutis, D. M. Stefonik, 
*                D. M. Syzmanski 
*
*                              NOTICE
*
* Interactive Powerflow (IPF) was developed by BPA and its contractors
* with about 20% of the cost supported by the Electric Power Research
* Institute (EPRI).  By mutual agreement, as described in EPRI 
* Agreement RP2746-03 entitled Graphical User Interface for 
* Powerflow, March, 1992, all results of this project--including 
* the computer program and its documentation--are to be in the 
* public domain.  In a separate Memorandum of Understanding with the 
* Western Systems Coordinating Council (WSCC), BPA agreed in March, 1992, 
* to keep WSCC informed of progress, to make its best effort to develop 
* the program according to the Guidelines adopted by the WSCC Computer 
* Program Management Subcommittee, and to make the final results 
* available for possible further development by WSCC. 
*
* This notice must appear in all copies of this software and documentation.
*/
#include <stdio.h>
#include <ctype.h>
#include <string.h>

int is_float (char *, int);
int isValidP (char *, int *);


struct B_RECORD {
   int start;
   int size;
};
struct BM_RECORD {
   int start;
   int size;
};
struct X_RECORD {
   int start;
   int size;
};
struct L_RECORD {
   int start;
   int size;
};
struct LD_RECORD {
   int start;
   int size;
};
struct LM_RECORD {
   int start;
   int size;
};
struct E_RECORD {
   int start;
   int size;
};
struct T_RECORD {
   int start;
   int size;
};
struct R_RECORD {
   int start;
   int size;
};
struct A_RECORD {
   int start;
   int size;
};
struct I_RECORD {
   int start;
   int size;
};

/* Beginning field and field width for type "B" records */

struct B_RECORD b_record[] = { 15, 4, 21, 5, 26, 5, 31, 4, 35, 4, 39, 4,
                               43, 5, 48, 5, 53, 5, 58, 4, 62, 4, 74, 4,
                               78, 3,  0, 0 };

/* Beginning field and field width for type "BM" and "BD" records  */

struct BM_RECORD bm_record[] = { 15, 4, 24, 2, 26, 5, 31, 5, 36, 5, 41, 5, 
                                 46, 5, 59, 4, 64, 3, 67, 3, 70, 6, 76, 6,  
                                  0, 0 };

/* Beginning field and field width for type "X" records  */

struct X_RECORD x_record[] = { 15, 4, 29, 4, 33, 1, 34, 5, 39, 1, 40, 5, 
                               45, 1, 46, 5, 51, 1, 52, 5, 57, 1, 58, 5,
                               63, 1, 64, 5, 69, 1, 70, 5, 75, 1, 76, 5,
                                0, 0 };

/* Beginning fields and field width for type "L" records  */

struct L_RECORD l_record[] = { 15, 4, 19, 1, 28, 4, 33, 1, 34, 4, 38, 1,
                               39, 6, 45, 6, 51, 6, 57, 6, 63, 4, 81, 4,
                               85, 4,  0, 0 };

/* Beginning fields and field width for type "LD" records */

struct LD_RECORD ld_record[] = { 15, 4, 28, 4, 34, 4, 38, 6, 44, 6, 50, 6,
                                 57, 5, 62, 5, 67, 4, 71, 4, 75, 4, 81, 4,
                                 85, 4,  0, 0 };

/* Beginning fields and field width for type "LM" records */

struct LD_RECORD lm_record[] = { 15, 4, 28, 4, 34, 4, 38, 6, 44, 6, 50, 6,
                                 74, 4, 81, 4, 85, 4,  0, 0 };

/* Beginning field and field width for type "E" records  */

struct E_RECORD e_record[] = { 15, 4, 19, 1, 28, 4, 33, 1, 34, 4, 38, 1, 
                               39, 6, 45, 6, 51, 6, 57, 6, 63, 6, 69, 6,
                               81, 4, 85, 4,  0, 0 };

/* Beginning field and field width for type "R" records  */

struct R_RECORD r_record[] = { 15, 4, 19, 1, 28, 4, 42, 4, 46, 5, 51, 5,
                               56, 2, 58, 5, 63, 5,  0, 0 };

/* Beginning field and field width for type "T" records  */

struct T_RECORD t_record[] = { 15, 4, 19, 1, 28, 4, 33, 1, 34, 4, 38, 1, 
                               39, 6, 45, 6, 51, 6, 57, 6, 63, 5, 68, 5,
                               81, 4, 85, 4, 89, 4,  0, 0 };

/* Beginning field and field width for type "A" records  */

struct A_RECORD a_record[] = { 22, 4, 27, 8, 73, 4, 77, 4,  0, 0 }; 

/* Beginning field and field width for type "I records  */

struct I_RECORD i_record[] = { 27, 8, 0, 0 };

#ifdef DEBUG

char *progname;

main(argc, argv)
int argc;
char *argv[];
{
  char record[141];
  int field;
  FILE *f1, *efopen();

  progname = argv[0];
  if (argc != 2) {
    fprintf(stderr, "Usage: isValidP file1 \n");
    exit(1);
  }

  f1 = efopen(argv[1], "r");

  while ( fgets(record, 140, f1) != NULL) {
     fprintf (stderr, " Processing %s ", record);
     if (isValidP (record, &field) == 0) {
        fprintf(stderr, " Bad data in field %d \n", field);
        fprintf(stderr, " Enter <return> to continue \n");
        gets(record);
     }
  }
  exit(0);
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

#endif

/***********************************************************************
* file: isValidP.c
* purpose: Establish the validity of an input record. This function
*          returns TRUE if all fields are valid, FALSE otherwise.
*          "field" identifies the first field which contains data
*          errors.
*          Note that the test for floating point is approximate.
*          Implicit decimal points are ignored.
*
* author: Walt Powell	November 6, 1992
*
* note:
*  all fields are defined as characters
*
************************************************************************
*/
int isValidP (char * record, int * field)
{

   int i, start, len;

   if (strchr ("B+XLRET", record[0]) != NULL) {
      if (strchr (" MDRO", record[2]) == NULL) {
	 *field = 3;
	 return (0);         /* error - Illegal change type */
      }
   } else if (strchr ("AI", record[0]) != NULL) {
      if (strchr (" MD", record[2]) == NULL) {
	 *field = 3;
	 return (0);         /* error - Illegal change type */
      }
   } else {
      *field = 1;
      return (0);            /* error - Illegal record type */
   }
   if (record[0] == 'A') {
      if (strchr (" C1234", record[1]) == NULL) {
	 *field = 2;
	 return (0);         /* error - Illegal subtype */
      }
   } else if (record[0] == 'B') {
      if (strchr (" ESCDVQGOTXMF", record[1]) == NULL) {
	 *field = 2;
	 return (0);         /* error - Illegal subtype */
      }
   } else if (record[0] == 'L') {
      if (strchr (" DM", record[1]) == NULL) {
	 *field = 2;
	 return (0);         /* error - Illegal subtype */
      }
   } else if (record[0] == 'T') {
      if (strchr (" P", record[1]) == NULL) {
	 *field = 2;
	 return (0);         /* error - Illegal subtype */
      }
   } else if (record[0] == 'R') {
      if (strchr (" VPQRMZ", record[1]) == NULL) {
	 *field = 2;
	 return (0);         /* error - Illegal subtype */
      }
   }
   /* check floating fields for a-c bus records */

   if (record[0] == 'B' && record[1] != 'D' &&
       record[1] != 'M') {
      i = 0;
      while (b_record[i].start > 0 &&
	     b_record[i].start < strlen(record)) {
	 start = b_record[i].start;
	 len = b_record[i].size;
	 if (is_float (&record[start-1], len) == 0) {
	    *field = start;
	    return (0);         /* error - Illegal floating value */
	 }
	 i++;
      }

   /* check floating fields for types BM and BD buses */

   } else if (record[0] == 'B' && (record[1] != 'D' ||
	      record[1] == 'M')) {
      i = 0;
      while (bm_record[i].start > 0 &&
	     bm_record[i].start < strlen(record)) {
	 start = bm_record[i].start;
	 len = bm_record[i].size;
	 if (is_float (&record[start-1], len) == 0) {
	    *field = start;
	    return (0);         /* error - Illegal floating value */
	 }
	 i++;
      }

   /* check floating fields for "+" bus records */

   } else if (record[0] == '+') {
      i = 0;
      while (b_record[i].start > 0 &&
	     b_record[i].start < strlen(record)) {
	 start = b_record[i].start;
	 len = b_record[i].size;
	 if (is_float (&record[start-1], len) == 0) {
	    *field = start;
	    return (0);         /* error - Illegal floating value */
	 }
	 i++;
      }

   /* check floating fields for "X" bus records */

   } else if (record[0] == 'X') {
      i = 0;
      while (x_record[i].start > 0 &&
	     x_record[i].start < strlen(record)) {
	 start = x_record[i].start;
	 len = x_record[i].size;
	 if (is_float (&record[start-1], len) == 0) {
	    *field = start;
	    return (0);         /* error - Illegal floating value */
	 }
	 i++;
      }

   /* check floating values for type "L" records  */

   } else if (record[0] == 'L' && record[1] != 'D' &&
	      record[1] != 'M') {
      i = 0;
      while (l_record[i].start > 0 &&
	     l_record[i].start < strlen(record)) {
	 start = l_record[i].start;
	 len = l_record[i].size;
	 if (is_float (&record[start-1], len) == 0) {
	    *field = start;
	    return (0);         /* error - Illegal floating value */
	 }
	 i++;
      }

   /* check floating values for type "LD" records  */

   } else if (record[0] == 'L' && record[1] == 'D') {
      i = 0;
      while (ld_record[i].start > 0 &&
	     ld_record[i].start < strlen(record)) {
	 start = ld_record[i].start;
	 len = ld_record[i].size;
	 if (is_float (&record[start-1], len) == 0) {
	    *field = start;
	    return (0);         /* error - Illegal floating value */
	 }
	 i++;
      }

   /* check floating values for type "LM" records  */

   } else if (record[0] == 'L' && record[1] == 'M') {
      i = 0;
      while (lm_record[i].start > 0 &&
	     lm_record[i].start < strlen(record)) {
	 start = lm_record[i].start;
	 len = lm_record[i].size;
	 if (is_float (&record[start-1], len) == 0) {
	    *field = start;
	    return (0);         /* error - Illegal floating value */
	 }
	 i++;
      }

   /* check floating values for type "E" records  */

   } else if (record[0] == 'E' ) {
      i = 0;
      while (e_record[i].start > 0 &&
	     e_record[i].start < strlen(record)) {
	 start = e_record[i].start;
	 len = e_record[i].size;
	 if (is_float (&record[start-1], len) == 0) {
	    *field = start;
	    return (0);         /* error - Illegal floating value */
	 }
	 i++;
      }

   /* check floating values for type "R" records  */

   } else if (record[0] == 'R') {
      i = 0;
      while (r_record[i].start > 0 &&
	     r_record[i].start < strlen(record)) {
	 start = r_record[i].start;
	 len = r_record[i].size;
	 if (is_float (&record[start-1], len) == 0) {
	    *field = start;
	    return (0);         /* error - Illegal floating value */
	 }
	 i++;
      }

   /* check floating values for type "T" records  */

   } else if (record[0] == 'T') {
      i = 0;
      while (t_record[i].start > 0 &&
	     t_record[i].start < strlen(record)) {
	 start = t_record[i].start;
	 len = t_record[i].size;
	 if (is_float (&record[start-1], len) == 0) {
	    *field = start;
	    return (0);         /* error - Illegal floating value */
	 }
	 i++;
      }

   /* check floating values for type "A" records  */

   } else if (record[0] == 'A') {
      i = 0;
      while (a_record[i].start > 0 &&
	     a_record[i].start < strlen(record)) {
	 start = a_record[i].start;
	 len = a_record[i].size;
	 if (is_float (&record[start-1], len) == 0) {
	    *field = start;
	    return (0);         /* error - Illegal floating value */
	 }
	 i++;
      }

   /* check floating values for type "I" records  */

   } else if (record[0] == 'I') {
      i = 0;
      while (i_record[i].start > 0 &&
	     i_record[i].start < strlen(record)) {
	 start = i_record[i].start;
	 len = i_record[i].size;
	 if (is_float (&record[start-1], len) == 0) {
	    *field = start;
	    return (0);         /* error - Illegal floating value */
	 }
	 i++;
      }
   }
   return (1);
}
