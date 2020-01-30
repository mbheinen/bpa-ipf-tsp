static char sccsid[] = "@(#)base_data.c	5.7     1/27/93";	/* used by library managment sys */
/********************************************************************\
* file: base_data.c
* author: Dan Clark
* purpose: manipulate base data and internal tables of base data
*
\*********************************************************************/
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include <Xm.h>

#include "dmgr.h"
#include "base_data.h"
#include "ipf_ipc.h"

#include "em.h"
em_init

void stringPart(char longString[],
                char returnString[],
                int  start_col,
                int  end_col);
extern void ipc_synch_rw(char *rtntext, char *text);
DB_STAT findfirstbusrec( PFBASERECORD **nextbusrec);

DBID base_db;
DBID bus_br_db;
static char outbuf_array[BUFSIZE];
static char inbuf_array [BUFSIZE];
static char out_buffer  [BUFSIZE];

/********************************************************************
*
* header for base data
* purpose: define pointers to active functions for this class of routine
* 
**********************************************************************
*/
#define MAXNAME 132
typedef struct fileObjectClass {
  int (*topen)();
  int (*tclose)();
  int (*tread)();
  int (*twrite)();
  int (*tprint)();
  int (*txdisplay)();
  int (*putname)();
  int (*getname)();
  char name[MAXNAME];
} FileObjectClass;
DB_STAT createBaseTbl_c();


#ifdef REMOVE
int createbasetbl(Widget, char *, XmFileSelectionBoxCallbackStruct *);
int readBaseData(Widget, char *, XmFileSelectionBoxCallbackStruct *);
int printBaseData(Widget, caddr_t *, caddr_t *);
int putbasename(char *);
int noop();

FileObjectClass baseObject = {
  createbasetbl,
  noop,
  readBaseData,
  noop,
  printBaseData,
  noop,
  putbasename,
  noop,
  "TEST00.BSE"
};

int noop() 
{ 
  printf("noop: not yet implemented\n");
}

int putbasename(char * filename)
{
  int status = 0;
  char *rtnstr;

  printf("putbasename:\n");
  printf(" old filename %s\n", baseObject.name); 
  printf(" received filename %s\n", filename);
  rtnstr = strncpy(baseObject.name, filename, MAXNAME);
  if(rtnstr == NULL) {
    status = -1;
  }
  printf(" new filename %s\n", baseObject.name); 
  return(status);
}

#endif

/********************************************************************\
*	cmp_type_name_base
*	
*	Called by: SCHEMA base_schema
*
\********************************************************************/
long cmp_type_name_base(char *a, char *b, long len)
{
  PFBASERECORD *keyrec;
  PFBASERECORD *checkrec;
  int rtncmp1;
  int rtncmp2;

  keyrec = (PFBASERECORD *)a;
  checkrec = (PFBASERECORD *)b;
/***
  printf("keyrec %s\n", keyrec);
  printf("checkrec %s\n", checkrec);
***/

  rtncmp1 = strncmp(keyrec->bus.type, checkrec->bus.type, 1);
  rtncmp2 = strncmp(keyrec->bus.name, checkrec->bus.name, 12);

  if(rtncmp1 > 0)
     return(1);
  else
     if(rtncmp1 < 0)
        return(-1);
  else
     if(rtncmp2 > 0)
        return(1);
  else
     if(rtncmp2 < 0)
        return(-1);
  else
     return(0);
}


static SCHEMA base_schema[] =
{
   { 'n', 'c', sizeof(PFBASERECORD), 0 },       /* size of record stored */
   { 'd', 'c', sizeof(pbranch->type), 
               offsetof(PFBRANCH, type) }, /* BSE_TYPE */
                        /* first character in any record */
                        /* d - dup keys, c - char, 1 length, 0 start pos */
   { 'd', 'c', sizeof(pbranch->name1)+sizeof(pbranch->base1),
               offsetof(PFBRANCH, name1) }, /* BSE_NAME1_BASE1 key */
   { 'd', 'c', sizeof(pbranch->name2)+sizeof(pbranch->base2),
               offsetof(PFBRANCH, name2) }, /* BSE_NAME2_BASE2 key */
   { 'd', 'u',sizeof(PFBASERECORD), 0, 0, cmp_type_name_base }, 
     /* BSE_TYPE_NAME1_BASE1 */
 };

#define BSE_KEY_CNT 5
#define BSE_PARTITIONS 0


/****************************************************************************\
*	Function: createbasetbl.c
*		
*	Author: Dan Clark	Dec, 1991	Modified: Dec 17, 1991
*
*	Called by: loadBase (2), readBaseData (2)
*
\****************************************************************************/
int createbasetbl( Widget wid, XtPointer tag, XmAnyCallbackStruct *cbs )
{

  DB_STAT stat;			/* status returned from data manager calls */

  stat =  createBaseTbl_c();
  assert(stat == D_OKAY);

}






/****************************************************************************\
*	Function: createBaseTbl_c
*		  Same as above - callable from C.
*
*	Called by: pf_cb.c
\****************************************************************************/
DB_STAT createBaseTbl_c()
{
  DB_STAT stat;

  stat = db_create((SCHEMA *)base_schema, BSE_KEY_CNT, BSE_PARTITIONS, &base_db); 
  /* base_schema - schema ptr, 4 - fields, 0 - partitions, base_db - tbl name */

  if(stat != D_OKAY) {
     err.line = EM_LINE;
     err.type = FATAL;
     err.msg  = out_buffer;
     err.link = "base_data.c dmgr routines.";
     err.ident= sccsid;
     sprintf( out_buffer, "*** FATAL *** Error creating a base dmgr db\n\
              Most likely problem: Out of memory.");
     em_show( &err );
  }
  return stat;
}


/****************************************************************************\
* Function: loadbase	( loadBaseFromPF )
* Author: Dan Clark	Mar, 1992
* purpose:
*  input: coord_db.  The coord_db is a dmgr table that has
*  all the records from the coordinate file.  This in memory
*  table (coord_db) is used to determine the bus names which
*  are the keys passed to the interactive powerflow program.
*  use the buses designated in the coordinate file 
*  powerflow returns full wscc record ascii info 
*  as the keys to read in a subset of the busses from the 
*  base data curently resident in powerflow.
*  store the data returned in a in memory table.
*  this table is used for one primary function which
*  is to create a graph table of busses and branches.
*
*  This table can serve as a basis for development of list
*  screens also.  But the list screen will show only the busses
*  corresponding to the busses which have been designated int
*  the coordinate file.
*
* enhancements:
*  should be a faster and easier way to setup the bus key from
*  the coordinate file data to eliminate all the str functions
*
\*****************************************************************/

int bus_box_fill_flag;
int branch_box_fill_flag;
int area_box_fill_flag;

int loadBase(Widget widget, XtPointer tag, 
             XmFileSelectionBoxCallbackStruct *callback_data)
{
#include "coord_data.h"
#define MAXBUSREC 256
  COORDFILERECORD keyvalrecord;
  COORDFILERECORD *keyvalrec;  
  COORDFILERECORD *nextrec;   
  COORDFILERECORD *busvalrec;
  COORDFILERECORD *nextbusrec;
  DB_STAT statbus;    /* status returned for general db access */

  char *pfcmdstr = outbuf_array;
  char *pfdataret = inbuf_array;
  PFBUS buskey_array;
  PFBUS *buskey = &buskey_array;
  char *psubstr;
  char *pfoundstr;
  char outstr_array[256];
  char *outstr = outstr_array;

  PFBASERECORD basekeyvalrecord;	/* storage for db_search key values */
				/* used for any search */
  PFBASERECORD *basekeyvalrec;	/* ptr to key values in db_search  */
				/* or ptr to current record in db_next */
				/* similar to a cursor in sql */
  PFBASERECORD *retrec;	/* ptr assigned to address of returned rec */
				/* used by db_search & db_next */
  PFBASERECORD record;	/* card data space filled in coord struct */
  PFBASERECORD *rec;		/* ptr to a card of data in coord struct */
  PFBASERECORD *newrec;		/* ptr to a card of data in coord struct */
  char card_data[162];		/* card data directly from coordinate file */

  int numCards = 0;		/* the number of coordinate file cards read */
  int	put_data = FALSE;	/* flag turning on insertion of data */

  DB_STAT stat;			/* status returned from data manager calls */

  rec = &record;

#ifdef MERGEBASE
  /** if the tag is zero create a new base table **/
  /** if not then MERGE in the last graph base data with this stuff **/
  if(tag != NULL) {
    if(tag[0] != 'M') {
      createbasetbl(0,0,0);
    }
  }
#endif
  /** if no base table exists create one **/
  if(db_valid_db(&base_db) != D_OKAY) {
    createbasetbl(0,0,0);
  } else {
    printf("Note: Deleting old base table\n");
    db_destroy(&base_db);
    createbasetbl(0,0,0);
  }

  /** make sure the coordinate file has been loaded **/
  if (db_valid_db( &coord_db) != D_OKAY ) {
    printf("WARNING: loadbase - no coordinate file selected\n");
    printf("  current system requires selection of a coord file\n");
    printf("  no base data retreived from powerflow\n");
    return;
  }
  /* use busses from coord file to drive retrieving base data */
  busvalrec = &keyvalrecord;
  strncpy(busvalrec->key, "B", 1);
  strncpy(busvalrec->bus.name, "            ", 12);
  statbus = db_search(&coord_db, &busvalrec, &nextbusrec, 
            COORD_KEY_NAME1_BASE1);
  if(statbus != D_FOUND)
  {
     printf("Note: positioned before first bus\n");
     statbus = D_FOUND;
  }
  
  while(statbus == D_FOUND)
  {
   if(nextbusrec->bus.key[0] != 'B')
   {
      printf("Note: forcing end - next ran past bus recs\n");
      printf("last %s\n", nextbusrec);
      break;
   }
   numCards++;
   /* printf("bus: %s\n", nextbusrec); */

   memset(buskey,' ', sizeof(buskey));
   strncpy(buskey->type, nextbusrec->bus.key, sizeof(buskey->type));
   strncpy(buskey->subtype, " ", sizeof(buskey->subtype));
   strncpy(buskey->chgcde, " ", sizeof(buskey->chgcde));
   strncpy(buskey->owner, "   ", sizeof(buskey->owner));
   strncpy(buskey->name, nextbusrec->bus.name, sizeof(buskey->name));
   strncpy(buskey->base, nextbusrec->bus.base, sizeof(buskey->base));
   strncpy(buskey->zone, "  ", sizeof(buskey->zone));
   buskey->data[0] = '\0';
   /* printf("buskey = %s\n", buskey); */

   sprintf(pfcmdstr, "/get_data,type=input\n%s\n%s\n", buskey, ENDOFMSG);
   /* printf("pfcmdstr = %s\n", pfcmdstr); */

   ipc_synch_rw(pfdataret, pfcmdstr);

   /* get rid of command returned */
   psubstr = strtok(pfdataret,SEPCHARS);
   /* now get all the lines of data */
   psubstr = strtok(NULL,SEPCHARS);
   while( psubstr != NULL ) {
     /* check for the logical end of the message */
     if( !strncmp(psubstr, ENDOFMSG, sizeof(ENDOFMSG))) {
       break;
     }
#ifdef REMOVE
     /* skip warning messages and control messages */
     if( psubstr[0] == '\\' || psubstr[0] == '\*'){
       printf("skip warning %30.30s\n", psubstr);
       continue;
     }
#endif
     strncpy(outstr, psubstr, MAXBUSREC);
     /* printf("bus data found =%s*\n", outstr); */

     stringPart(outstr, record.any, 0, sizeof(PFBASERECORD) + 1);
     stat = db_insert(&base_db, &rec, &newrec); 
     if(stat != D_OKAY) {
       printf("ERROR base record insert base_db table stat %d\n",stat);
       printf("  record failed is %s\n", rec);
       exit(1);
     }
     assert(stat == D_OKAY);

     psubstr = strtok(NULL,SEPCHARS);
   } /* end of loop  through data lines */

   /** get the next bus **/
   busvalrec = nextbusrec;
   statbus = db_next(&coord_db, &busvalrec,
             &nextbusrec,COORD_KEY_NAME1_BASE1);

  } /* end bus loop */
  
  bus_box_fill_flag    = FALSE;	/* bus selection box not loaded */
  branch_box_fill_flag = FALSE;	/* branch  "      "   "    "    */
  area_box_fill_flag   = FALSE;	/* area    "      "   "    "    */

  return 0;

}


/****************************************************************************\
* Function: loadbase	( loadBaseFromPF )
* Author: Dan Clark	Mar, 1992
* purpose:
*  input: coord_db.  The coord_db is a dmgr table that has
*  all the records from the coordinate file.  This in memory
*  table (coord_db) is used to determine the bus names which
*  are the keys passed to the interactive powerflow program.
*  use the buses designated in the coordinate file 
*  powerflow returns full wscc record ascii info 
*  as the keys to read in a subset of the busses from the 
*  base data curently resident in powerflow.
*  store the data returned in a in memory table.
*  this table is used for one primary function which
*  is to create a graph table of busses and branches.
*
*  This table can serve as a basis for development of list
*  screens also.  But the list screen will show only the busses
*  corresponding to the busses which have been designated int
*  the coordinate file.
*
* enhancements:
*  should be a faster and easier way to setup the bus key from
*  the coordinate file data to eliminate all the str functions
*
\*****************************************************************/

int psloadBase(Widget wid, XtPointer tag, 
               XmFileSelectionBoxCallbackStruct *callback_data)
{
#include "coord_data.h"
#define MAXBUSREC 256
  PSCoordFileRecord keyvalrecord;
  PSCoordFileRecord *keyvalrec;  
  PSCoordFileRecord *nextrec;   
  PSCoordFileRecord *busvalrec;
  PSCoordFileRecord *nextbusrec;
  DB_STAT statbus;    /* status returned for general db access */

  char *pfcmdstr = outbuf_array;
  char *pfdataret = inbuf_array;
  PFBUS buskey_array;
  PFBUS *buskey = &buskey_array;
  char *psubstr;
  char *pfoundstr;
  char outstr_array[256];
  char *outstr = outstr_array;
  char *pctag = (char *)tag;

  PFBASERECORD basekeyvalrecord;	/* storage for db_search key values */
				/* used for any search */
  PFBASERECORD *basekeyvalrec;	/* ptr to key values in db_search  */
				/* or ptr to current record in db_next */
				/* similar to a cursor in sql */
  PFBASERECORD *retrec;	/* ptr assigned to address of returned rec */
				/* used by db_search & db_next */
  PFBASERECORD record;	/* card data space filled in coord struct */
  PFBASERECORD *rec;		/* ptr to a card of data in coord struct */
  PFBASERECORD *newrec;		/* ptr to a card of data in coord struct */
  char card_data[162];		/* card data directly from coordinate file */

  int numCards = 0;		/* the number of coordinate file cards read */
  int	put_data = FALSE;	/* flag turning on insertion of data */

  DB_STAT stat;			/* status returned from data manager calls */

  rec = &record;

#ifdef MERGEBASE
  /** if the tag is zero create a new base table **/
  /** if not then MERGE in the last graph base data with this stuff **/
  if(pctag != NULL) {
    if(pctag[0] != 'M') {
      createbasetbl(0,0,0);
    }
  }
#endif
  /** if no base table exists create one **/
  if(db_valid_db(&base_db) != D_OKAY) {
    createbasetbl(0,0,0);
  } else {
    printf("Note: Deleting old base table\n");
    db_destroy(&base_db);
    createbasetbl(0,0,0);
  }

  /** make sure the coordinate file has been loaded **/
  if (db_valid_db( &pscor_db) != D_OKAY ) {
    printf("WARNING: loadbase - no coordinate file selected\n");
    printf("  current system requires selection of a coord file\n");
    printf("  no base data retreived from powerflow\n");
    return;
  }
  /* use busses from coord file to drive retrieving base data */
  busvalrec = &keyvalrecord;
  strncpy(busvalrec->bus.key, "B", 1);
  strncpy(busvalrec->bus.name, "            ", 12);
  statbus = db_search(&pscor_db, &busvalrec, &nextbusrec, 
            COORD_KEY_NAME1_BASE1);
  if(statbus != D_FOUND)
  {
     printf("Note: positioned before first bus\n");
     statbus = D_FOUND;
  }
  
  while(statbus == D_FOUND)
  {
   if(nextbusrec->bus.key[0] != 'B')
   {
      printf("Note: forcing end - next ran past bus recs\n");
      printf("last %s\n", nextbusrec);
      break;
   }
   numCards++;
   /* printf("bus: %s\n", nextbusrec); */

   memset(buskey,' ', sizeof(buskey));
   strncpy(buskey->type, nextbusrec->bus.key, sizeof(buskey->type));
   strncpy(buskey->subtype, " ", sizeof(buskey->subtype));
   strncpy(buskey->chgcde, " ", sizeof(buskey->chgcde));
   strncpy(buskey->owner, "   ", sizeof(buskey->owner));
   strncpy(buskey->name, nextbusrec->bus.name, sizeof(buskey->name));
   strncpy(buskey->base, nextbusrec->bus.base, sizeof(buskey->base));
   strncpy(buskey->zone, "  ", sizeof(buskey->zone));
   buskey->data[0] = '\0';
   /* printf("buskey = %s\n", buskey); */

   sprintf(pfcmdstr, "/get_data,type=input\n%s\n%s\n", buskey, ENDOFMSG);
   /* printf("pfcmdstr = %s\n", pfcmdstr); */

   ipc_synch_rw(pfdataret, pfcmdstr);

   /* get rid of command returned */
   psubstr = strtok(pfdataret,SEPCHARS);
   /* now get all the lines of data */
   psubstr = strtok(NULL,SEPCHARS);
   while( psubstr != NULL ) {
     /* check for the logical end of the message */
     if( !strncmp(psubstr, ENDOFMSG, sizeof(ENDOFMSG))) {
       break;
     }
#ifdef REMOVE
     /* skip warning messages and control messages */
     if( psubstr[0] == '\\' || psubstr[0] == '\*'){
       printf("skip warning %30.30s\n", psubstr);
       continue;
     }
#endif
     strncpy(outstr, psubstr, MAXBUSREC);
     /* printf("bus data found =%s*\n", outstr); */

     stringPart(outstr, record.any, 0, sizeof(PFBASERECORD) + 1);
     stat = db_insert(&base_db, &rec, &newrec); 
     if(stat != D_OKAY) {
       printf("ERROR base record insert base_db table stat %d\n",stat);
       printf("  record failed is %s\n", rec);
       exit(1);
     }
     assert(stat == D_OKAY);

     psubstr = strtok(NULL,SEPCHARS);
   } /* end of loop  through data lines */

   /** get the next bus **/
   busvalrec = nextbusrec;
   statbus = db_next(&pscor_db, &busvalrec,
             &nextbusrec,COORD_KEY_NAME1_BASE1);

  } /* end bus loop */
  
  bus_box_fill_flag    = FALSE;	/* bus selection box not loaded */
  branch_box_fill_flag = FALSE;	/* branch  "      "   "    "    */
  area_box_fill_flag   = FALSE;	/* area    "      "   "    "    */

  return 0;

}


/****************************************************************\
* Function: readBaseDataFile.c	(loadBaseFromASCII - Not used)
*
* Author: Dan Clark	Dec, 1991	Modified: Dec 17, 1991
* purpose:
*   open and read the an ascii base data file directly into
*   the graphics front end.  This function will load any wscc formatted
*   data file into an in memory table for subsequent processing.
*
*   this is a useful function for testing the functions on the graphics
*   front end prior to having communications established with the
*   powerflow application.  
*   this process may also be useful for bypassing the ipc channel
*   for large volumes of data.
* 
* this function is not used as of Mar 20, 1992
*
\*****************************************************************/

int readBaseDataFile(Widget wid, XtPointer tag,
                     XmFileSelectionBoxCallbackStruct *callback_data )
{
#ifdef DIRECTCALL
  char          *compound_string; /* x compound string of file name */
  caddr_t       context;        /* x context of compound string */
  char          *text;          /* text pointer to file name */
  XmStringCharSet       charset;/* x return char set (unused) */
  XmStringDirection     dir;    /* x return directory of file (unused) */
  Boolean       separator;      /* x separator for file string (unused) */
#endif

  char base_input_file_name[81];	/* name of input file */
  FILE *fopen(), *fptr;         /* ptr to file returned by open */

  PFBASERECORD keyvalrecord;	/* storage for db_search key values */
				/* used for any search */
  PFBASERECORD *keyvalrec;	/* ptr to key values in db_search  */
				/* or ptr to current record in db_next */
				/* similar to a cursor in sql */
  PFBASERECORD *retrec;	/* ptr assigned to address of returned rec */
				/* used by db_search & db_next */
  PFBASERECORD record;	/* card data space filled in coord struct */
  PFBASERECORD *rec;		/* ptr to a card of data in coord struct */
  PFBASERECORD *newrec;		/* ptr to a card of data in coord struct */
  char card_data[162];		/* card data directly from coordinate file */

  int numCards = 0;		/* the number of coordinate file cards read */
  int	put_data = FALSE;	/* flag turning on insertion of data */

  DB_STAT stat;			/* status returned from data manager calls */

#ifdef DIRECTCALL
  text = get_file_name_from(callback_data);
#ifdef JLR
  /*** get the filename from the file selection widget ***/
  /* Get the compound string containing the file name selected */
  compound_string = callback_data->value;

  /* Extract the file name from the compound string */
  XmStringInitContext(&context, compound_string);
  XmStringGetNextSegment(context, &text, &charset, &dir, &separator);

  /* Free the context */
  XmStringFreeContext(context);
#endif

  /* Print the string */
  /* printf("Control File Name: %s\n", text); */
  sprintf(base_input_file_name, "%s", text);
#else
  sprintf(base_input_file_name, "%s", tag);
#endif


  /*** open the coordinate file ***/
  if ( ( fptr = fopen(base_input_file_name, "r" ) ) == 0 ) 
  {
     printf("\n ==================================================\n");
     printf(  " =** UNABLE TO OPEN BASE TEST FILE              **=\n");
     printf(  " ==================================================\n\n");
     return(1);
  }


/***
  printf("Note: Deleting old base table\n");
  stat = db_destroy(&base_db);
***/

  /** if no base table exists create one **/
  if(db_valid_db(&base_db) != D_OKAY) {
    createbasetbl(0,0,0);
  } else {
    printf("Note: Deleting old base table\n");
    db_destroy(&base_db);
    createbasetbl(0,0,0);
  }

  printf("Begin reading base file [%s]\n", base_input_file_name);

  /*** put all data into the database from the base test file ***/
  rec = &record;
  numCards = 0;
  put_data = TRUE;
  while ((fgets( card_data, 132, fptr )) != NULL)
  {
     stringPart(card_data, record.any, 0, 80);
     stat = db_insert(&base_db, &rec, &newrec); 
     if(stat != D_OKAY) {
       printf("ERROR base record insert base_db table stat %d\n",stat);
       printf("  record failed is %s\n", rec);
       exit(1);
     }
     assert(stat == D_OKAY);
     numCards++;
  }

  /*** close the coordinate file ***/
  fclose (fptr);
  printf("Completed reading base file # cards read %d\n", numCards);

  bus_box_fill_flag    = FALSE;		/* bus selection box not loaded */
  branch_box_fill_flag = FALSE;		/* branch  "      "   "    "    */
  area_box_fill_flag   = FALSE;		/* area    "      "   "    "    */

  return(0);

}


/*****************************************************************************\
*	Function: printbasedata.c 
*		
*	Author: Dan Clark	Dec, 1991	Modified: Oct 05, 1992
*
*	Called by: (none - should be needed later )
*
\*****************************************************************************/
int printBaseData(Widget wid, XtPointer	tag, XmAnyCallbackStruct *cbs)
{
  PFBASERECORD keyvalrecord;	/* storage for db_search key values */
				/* used for any search */
  PFBASERECORD *keyvalrec;	/* ptr to key values in db_search  */
				/* or ptr to current record in db_next */
				/* similar to a cursor in sql */
  PFBASERECORD *nextrec;	/* ptr assigned to address of next rec */
				/* used by db_search & db_next */
  PFBASERECORD *branchvalrec;/* ptr to key for branch search */
  PFBASERECORD *nextbranchrec;/* ptr returned for next branch rec */
  PFBASERECORD *busvalrec;	/* ptr to key for bus search */
  PFBASERECORD *nextbusrec;	/* ptr returned for next bus rec */

  DB_STAT stat;		/* status returned for general db access */
  DB_STAT statbus;	/* status returned for bus queries of db */
  DB_STAT statbranch;	/* status returned for branch queries of db */

  /** retrieve all the records (in key order) and write to output file **/
  stat = db_first(&base_db, &nextrec, BSE_TYPE);
  if (stat != D_FOUND)
  {
    printf("ERROR db_search did not find any records \n");
    return(1);
  }

  while(stat == D_FOUND)
  {
    printf("%s\n", nextrec); 
    keyvalrec = nextrec;
    stat = db_next(&base_db, &keyvalrec, &nextrec, BSE_TYPE);
  }

  /************************************************************/
  /*  return before executing sorted print */
  /* return(0); */
  printf("Now print in sorted order\n");

  /** retrieve the first Bus record **/
  statbus = findfirstbusrec ( &nextbusrec );

/*
  busvalrec = &keyvalrecord;
  strncpy(busvalrec->type, "B", 1);
  statbus = db_search(&base_db,&busvalrec, &nextbusrec, BSE_TYPE);
  if(statbus != D_FOUND)
  { 
     printf("Warning: no busses found in table\n");
  }
*/

  /** write each bus record followed by all the associated branch records **/
  while(statbus == D_FOUND)
  {
    printf("%s\n", nextbusrec);

    /** retrieve the first branch Record for this bus **/
    branchvalrec = &keyvalrecord;
    strncpy(branchvalrec->branch.type, " ", 1);
    strncpy(branchvalrec->branch.name1, nextbusrec->bus.name , 12);
    strncpy(branchvalrec->branch.name2, "            " , 12);
    statbranch = db_search(&base_db,&branchvalrec,&nextbranchrec,BSE_NAME1_BASE1);
    if(statbranch != D_FOUND)
    { 
      printf("Note: no branches found in table for bus\n");
    }

    /** write each branch record for bus **/
    while( statbranch == D_FOUND)
    {
      if(nextbranchrec->branch.type[0] == nextbusrec->bus.type[0])
      {
        printf("Note: skipping write of Bus record \n");
      } else {
        printf("%s\n", nextbranchrec);
      }

      /* get the next branch for this bus **/
      branchvalrec = nextbranchrec;
      statbranch = db_keynext(&base_db, &branchvalrec, &nextbranchrec, BSE_NAME1_BASE1);
    } /* end branch loop */

    /** retrieve the first farend branch Record for this bus **/
    branchvalrec = &keyvalrecord;
    strncpy(branchvalrec->branch.type, " ", 1);
    strncpy(branchvalrec->branch.name1, "            " , 12);
    strncpy(branchvalrec->branch.name2, nextbusrec->bus.name , 12);
    statbranch = db_search(&base_db,&branchvalrec,&nextbranchrec,BSE_NAME2_BASE2);
    if(statbranch != D_FOUND)
    { 
      printf("Note: no branches found in table for bus\n");
    }

    /** write each branch record for bus **/
    while( statbranch == D_FOUND)
    {
      if(nextbranchrec->branch.type[0] == nextbusrec->bus.type[0])
      {
        printf("Note: skipping write of Bus record \n");
      } else {
        printf("%s\n", nextbranchrec);
      }

      /* get the next branch for this bus **/
      branchvalrec = nextbranchrec;
      statbranch = db_keynext(&base_db, &branchvalrec, &nextbranchrec, BSE_NAME2_BASE2);
    } /* end branch loop */

    /** get the next bus **/
    busvalrec = nextbusrec;
    statbus = db_keynext(&base_db, &busvalrec, &nextbusrec, BSE_TYPE);
  } /* end bus loop */

  return(0);

}


int loadBusBranch(Widget wid, XtPointer *tag, XmAnyCallbackStruct *cbs)
{
  char *pfcmdstr = outbuf_array;
  char *pfdataret = inbuf_array;
  PFBUS buskey_array;
  PFBUS *buskey = &buskey_array;
  char *psubstr;
  char *pfoundstr;
  char outstr_array[256];
  char *outstr = outstr_array;
  char *pctag = (char *)tag;

  PFBASERECORD basekeyvalrecord;	/* storage for db_search key values */
				/* used for any search */
  PFBASERECORD *basekeyvalrec;	/* ptr to key values in db_search  */
				/* or ptr to current record in db_next */
				/* similar to a cursor in sql */
  PFBASERECORD *retrec;	/* ptr assigned to address of returned rec */
				/* used by db_search & db_next */
  PFBASERECORD record;	/* card data space filled in coord struct */
  PFBASERECORD *rec;		/* ptr to a card of data in coord struct */
  PFBASERECORD *newrec;		/* ptr to a card of data in coord struct */
  char card_data[162];		/* card data directly from coordinate file */

  int numCards = 0;		/* the number of coordinate file cards read */
  int	put_data = FALSE;	/* flag turning on insertion of data */

  DB_STAT stat;			/* status returned from data manager calls */
  char bus_namevolt[30];

  rec = &record;

  stringPart(pctag, bus_namevolt, 0, 17);
  printf("loadBusBranch: bus_namevolt=(%s)\n", bus_namevolt);
  /** if no base table exists create one **/
  if(db_valid_db(&bus_br_db) != D_OKAY) {
  /*** create the table for raw coordinate data ***/
  stat = db_create((SCHEMA *)base_schema, BSE_KEY_CNT, BSE_PARTITIONS, &bus_br_db); 
  /* base_schema - schema ptr, 4 - fields, 0 - partitions, base_db - tbl name */
  if(stat != D_OKAY) {
     printf("error createing base db\n");
     exit(1);
  }
  assert(stat == D_OKAY);

  } else {
    printf("Note: Deleting old base table\n");
    db_destroy(&bus_br_db);
  /*** create the table for raw coordinate data ***/
  stat = db_create((SCHEMA *)base_schema, BSE_KEY_CNT, BSE_PARTITIONS, &bus_br_db); 
  /* base_schema - schema ptr, 4 - fields, 0 - partitions, base_db - tbl name */
  if(stat != D_OKAY) {
     printf("error createing base db\n");
     exit(1);
  }
  assert(stat == D_OKAY);
  }
   /* printf("buskey = %s\n",pctag); */

   sprintf(pfcmdstr, "/get_data,type=input\n%s\n%s\n", bus_namevolt, ENDOFMSG);
   printf("pfcmdstr = %s\n", pfcmdstr); 

   ipc_synch_rw(pfdataret, pfcmdstr);

   /* get rid of command returned */
   psubstr = strtok(pfdataret,SEPCHARS);
   /* now get all the lines of data */
   psubstr = strtok(NULL,SEPCHARS);
   while( psubstr != NULL ) {
     /* check for the logical end of the message */
     if( !strncmp(psubstr, ENDOFMSG, sizeof(ENDOFMSG))) {
       break;
     }
#ifdef REMOVE
     /* skip warning messages and control messages */
     if( psubstr[0] == '\\' || psubstr[0] == '\*'){
       printf("skip warning %30.30s\n", psubstr);
       continue;
     }
#endif
     strncpy(outstr, psubstr, MAXBUSREC);
     /* printf("bus data found =%s*\n", outstr); */

     stringPart(outstr, record.any, 0, sizeof(PFBASERECORD) + 1);
     stat = db_insert(&bus_br_db, &rec, &newrec); 
     if(stat != D_OKAY) {
       printf("ERROR base record insert bus_br_db table stat %d\n",stat);
       printf("  record failed is %s\n", rec);
       exit(1);
     }
     assert(stat == D_OKAY);

     psubstr = strtok(NULL,SEPCHARS);
   } /* end of loop  through data lines */
}


/****************************************************************************\
*	Function: findfirstbusrec
*		  gets first bus record in base_db 
*
*	Author: Bill Rogers	Oct 5, 1992
*
*	Called by:
*
\****************************************************************************/
DB_STAT findfirstbusrec( PFBASERECORD **nextbusrec)
     /* ptr returned for next bus rec        */
{

  PFBASERECORD *busvalrec;      /* ptr to key for bus search            */
  PFBASERECORD keyvalrecord;    /* storage for db_search key values     */
  PFBASERECORD *nextrec;        /* ptr assigned to address of next rec  */
  DB_STAT stat;

  /** if no base table exists just return (no data ) for list **/
  if( base_db.nkeys == (long)0 )
  {
    printf(" *** WARNING *** base_db is empty\n" );
    return( D_EOF );
  }

  busvalrec = &keyvalrecord;
  strncpy(busvalrec->type, "B", 1 );
  strncpy(busvalrec->bus.name, "            ", 12 );
  stat = db_search( &base_db, &busvalrec, &nextrec, BSE_TYPE_NAME1_BASE1);

  *nextbusrec = nextrec;
  if(stat == D_NOTFOUND) {
    stat = D_FOUND;  /* set to found -- positioned just before first bus */
  }
  return(stat);

#ifdef NOGOOD
     if(stat == D_EOF )
       return( D_NOTFOUND );	/* apparent no entries */
     else
       return( D_FOUND );	/* Note: even if not found, pointer is set */
#endif

}
