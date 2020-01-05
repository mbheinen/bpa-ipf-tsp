static char sccsid[]="@(#)substation.c	20.3 2/15/95";
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
/*****************************************************************************\
* file: substation.c
* author: Dan Clark
* purpose: handle and translate coordinates from substation info
*
*
\*****************************************************************************/
#include <stdio.h>
#include <assert.h>
#include <Xm/Xm.h>

#include "dmgr.h"
#include "substation.h"

extern int exit		();
extern int stringPart	( char *, char *, int, int );
extern int atoi_cnt	( char *str, int size );

/*****************************************************************************\
*	Function: readsubdata.c
*  		open and read the base file into a dbmgr table
*		
*	Author: Dan Clark	Dec, 1991	Modified: Dec 17, 1991
*
*	Called by: 
*
\*****************************************************************************/

int readSubData(widget, tag, callback_data)
Widget		widget;
char		*tag;
XmFileSelectionBoxCallbackStruct	*callback_data;
{
  char          *compound_string; /* x compound string of file name */
  caddr_t       context;        /* x context of compound string */
  char          *text;          /* text pointer to file name */
  XmStringCharSet       charset;/* x return char set (unused) */
  XmStringDirection     dir;    /* x return directory of file (unused) */
  Boolean       separator;      /* x separator for file string (unused) */

  char base_input_file_name[81];	/* name of input file */
  FILE *fopen(), *fptr;         /* ptr to file returned by open */

  SUBRECORD arecord;		/* substation data space filled in struct */
  SUBSTATIONRECORD record;	/* substation data converted */
  SUBSTATIONRECORD *rec;	/* ptr to a card of data in coord struct */
  SUBSTATIONRECORD *newrec;	/* ptr to a card of data in coord struct */

  char card_data[162];		/* card data directly from coordinate file */

  int numCards;			/* the number of coordinate file cards read */
  int	put_data = FALSE;	/* flag turning on insertion of data */

  DB_STAT stat;			/* status returned from data manager calls */


  /*** create the table for raw coordinate data ***/
  stat = db_create(sub_schema, SUB_KEY_CNT, SUB_PARTITIONS, &sub_db); 
  /* base_schema - schema ptr, 4 - fields, 0 - partitions, base_db - tbl name */
  if(stat != D_OKAY) {
     printf("error createing base db\n");
     exit(1);
  }
  assert(stat == D_OKAY);


  /*** get the filename from the file selection widget ***/
  /* Get the compound string containing the file name selected */
  compound_string = callback_data->value;

  /* Extract the file name from the compound string */
  XmStringInitContext(&context, compound_string);
  XmStringGetNextSegment(context, &text, &charset, &dir, &separator);

  /* Print the string */
  /* printf("Control File Name: %s\n", text); */
  sprintf(base_input_file_name, "%s", text);

  /* Free the context */
  XmStringFreeContext(context);


  /*** open the coordinate file ***/
  if ( ( fptr = fopen(base_input_file_name, "r" ) ) == 0 ) 
  {
     printf("\n ==================================================\n");
     printf(  " =** UNABLE TO OPEN BASE TEST FILE              **=\n");
     printf(  " ==================================================\n\n");
     return(1);
  }
  printf("Begin reading base file [%s]\n", base_input_file_name);


  /*** put all data into the database from the base test file ***/
  rec = &record;
  numCards = 0;
  while ((fgets( card_data, 132, fptr )) != NULL)
  {
     stringPart(card_data, &arecord, 0, 70);

     stringPart(arecord.code, record.code, 0, 4);
     stringPart(arecord.name, record.name, 0, 25);
     record.index = atoi_cnt( arecord.index, 4);
     sscanf( arecord.x, "%lf", &record.x);
     sscanf( arecord.y, "%lf", &record.y);

     stat = db_insert(&sub_db, &rec, &newrec);
     if(stat != D_OKAY) {
       printf("ERROR base record insert base_db table stat %d\n",stat);
       printf("  record failed is %s\n", rec);
       exit(1);

     assert(stat == D_OKAY);
     numCards++;
     }
  }

  /*** close the coordinate file ***/
  fclose (fptr);
  printf("Completed reading base file # cards read %d\n", numCards);

  return(0);

}

/*****************************************************************************\
*	Function: printbasedata.c 
*		
*	Author: Dan Clark	Dec, 1991	Modified: Dec 17, 1991
*
*	Called by: 
*
\*****************************************************************************/

int printSubData(widget, tag, callback_data)
Widget		widget;
caddr_t		*tag;
caddr_t		*callback_data;
{
  SUBSTATIONRECORD keyvalrecord;	/* storage for db_search key values */
				/* used for any search */
  SUBSTATIONRECORD *keyvalrec;	/* ptr to key values in db_search  */ 
				/* or ptr to current record in db_next */
				/* similar to a cursor in sql */
  SUBSTATIONRECORD *nextrec;	/* ptr assigned to address of next rec */
				/* used by db_search & db_next */

  DB_STAT stat;		/* status returned for general db access */

  /** retrieve all the records (in key order) and write to output file **/
  stat = db_first(&sub_db, &nextrec, SUB_Y);
  if (stat != D_FOUND)
  {
    printf("ERROR db_search did not find any records \n"); return(1);
  }

  while(stat == D_FOUND)
  {
    /* printf("%s\n", nextrec);  */
    printf("%4.4s ", nextrec->code);
    printf("%25.25s ", nextrec->name);
    printf("( %10.3lf x ", nextrec->x);
    printf("%10.3lf ) ", nextrec->y);
    printf("\n");
    keyvalrec = nextrec;
    stat = db_next(&sub_db, &keyvalrec, &nextrec, SUB_Y);
  }

}
