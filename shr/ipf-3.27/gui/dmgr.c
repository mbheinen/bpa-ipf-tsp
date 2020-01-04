static char sccsid[]="@(#)dmgr.c	20.4 2/15/95";
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
/* The following can be defined when compiling.
   IPFCONST - Replaces "const", if defined.  Define as nothing for older compilers.
   DEBUG - If defined, will create statistics and diagnostics while running.
   NDEBUG - The standard switch to ignore asserts in functions, if defined.
   SLEEP - Replaces "sleep", if defined.  Define as nothing for older compilers.
   SMALL - If defined, will use less memory (for small memory models) with TEST.
   TEST - If defined, a main routine is included to test all functions.
*/
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef VMS
#include <signal.h>   /* location of sleep info for VMS */
#else
#include <unistd.h>
#endif

#include "dmgr.h"

/*****************************************************************************
*****************************************************************************/
/* Exit with error condition 1 after displaying message */
static void db_exit(char *string)
{
  printf(string);
  exit(1);
}

/* Maximum number of pointers on a skip list node */
#define MAXLEVEL 32L
/* Default ratio = PARTITION/MAXLEVEL.  50% yields balance of time & space. */
#define PARTITION 16

#ifdef DEBUG
  /* Keep track of all memory allocations to verify everything we used
     was deleted */
  long tot_alloc = 0;
  long tot_freed = 0;
  long tot_lnkrecs_created = 0;
  long tot_lnkrecs_deleted = 0;
  long array[MAXLEVEL + 1];  /* Store statistics on node efficiency */

  /* Consistent random functions always produce the same results for testing */
#define RAND()   slrand()
#define SRAND(u) slsrand(u)

  static unsigned long next = 1;
  static void slsrand(unsigned int seed)
  {
    next = seed;
  }

  static int slrand(void)
  {
    next = next * 1103515245L + 12345L;  /* This assumes no overflow checking */
    return((int) ((next / 65536L) % ((unsigned long)RAND_MAX + 1L)));
  }
#else /* !DEBUG */

#define RAND()   rand()
#define SRAND(u) srand(u)

#endif /* !DEBUG */

/*****************************************************************************
*****************************************************************************/
/* Return a random number in the range 0..(limit-1) */
static long slrandom(long limit)
{
  long num;
  num = (long) RAND();
  return((long)(num % limit));
}

/*****************************************************************************
*****************************************************************************/
/* Primitive locking.  Might not work on multi-processor,
   multi-thread machines or with compilers that do not use memory increments &
   decrements.  Perhaps locking is not needed at all.
*/
#define GETLOCK db->locked
#define ENDLOCK /* If some implementation needs this at end of GETLOCK block */
void db_resetlock(DBID *db)
{
  --db->locked;
}

void db_setlock(DBID *db)
{
  /* Wait until there are no more locks on the db */
  while(db->locked++ != 0)
  {
    --db->locked;
    printf("database locked\n");
    SLEEP(1);  /* Pause process for a second on systems that allow it */
  }
}

/*****************************************************************************
*****************************************************************************/
/* Generate a random number of pointers in range 0..(MAXLEVEL-1) for the new
   node.  Use partition/MAXLEVEL as ratio for time vs. space tradeoff. */
static long db_randomlevel(long partition)
{
  long newlevel = 1;
  while (slrandom(MAXLEVEL) < partition && newlevel < MAXLEVEL) ++newlevel;
  return(newlevel);
}

/*****************************************************************************
*****************************************************************************/
/* Built-in comparison functions are local.
   User supplied functions are registered via db_create. */
static long db_strcmp(VOIDPTR a, VOIDPTR b, long len)
{
  return((long)memcmp(a, b, (size_t)len));
}

static long db_dblcmp(VOIDPTR a, VOIDPTR b, long len)
{
  double tmp = *((double *)a) - *((double *)b);
  if (tmp < 0) return(-1L);
  return((tmp > 0) ? 1L : 0L);
}

static long db_fltcmp(VOIDPTR a, VOIDPTR b, long len)
{
  float tmp = *((float *)a) - *((float *)b);
  if (tmp < 0) return(-1L);
  return((tmp > 0) ? 1L : 0L);
}

static long db_intcmp(VOIDPTR a, VOIDPTR b, long len)
{
  return(*((long *)a) - *((long *)b));
}

/*****************************************************************************
*****************************************************************************/
/* Create a new dbnode of length datalen + room for all pointers.
      DATA FORMAT
_______________________________________________________________________________
|    | link 1   | link 0   | user data   | num of | key 0    | key 1    |     |
| ...| pointers | pointers |             | levels | pointers | pointers | ... |
-------------------------------------------------------------------------------
   All pointers point to the start of user data.  Since the fields are created
   from the schema in reverse order, key 0 is the last key field in the schema
   and link 0 is the last linked list field in the schema.
*/
static DB_STAT db_newnode(LNKPTR *temp, CNTTYP nlevel, long nkeys, long datalen,
    long nlls)
/* nlls = number of linked list pointer sets in above data format, 3 pointers
          in each set: flags, previous, next. */
{
  LNKPTR tmprec;
  long i = nlls;
  DB_STAT db_status;
  long llsptrslen = sizeof(LNKPTR) * 3 * nlls;
  long reclen = llsptrslen + datalen + sizeof(CNTTYP) +
    sizeof(LNKPTR) * nlevel * nkeys;
  /* Memory is initialized to zero so all pointers have a NULL value */
  LNKPTR *lptr = (LNKPTR *)calloc((size_t)reclen, (size_t)1);
#ifdef DEBUG
  tot_alloc += reclen;      /* Total of all in-use memory */
#endif
  if (lptr == NULL) db_status = D_NOMEMORY;
  else
  {
    db_status = D_OKAY;
    lptr = (LNKPTR *)((char *)lptr + llsptrslen);  /* Point to the user data */
    /* Store the # of levels into our record.
       Number of bits per character must = 8. */
    *((CNTTYP *)((char *)lptr + datalen)) = nlevel;
    *temp = (LNKPTR)lptr;
    /* Loop through all link pointers to set the "next" and "previous" pointers
       to point to the position right after the "next" pointer.  This is
       consistent with the pointers within link records.  Ie, the "next" and
       "previous" link 0 pointers will point to the record user data.
    */
    while (--i >= 0)
    {
      tmprec = (LNKPTR)lptr;
      *(--lptr) = tmprec;
      *(--lptr) = tmprec;
      *(--lptr) = (LNKPTR)1;                  /* Mark as db record */
    }
  }
  return(db_status);
}

/*****************************************************************************
*****************************************************************************/
/* Initialize the database structure.
*/
void db_init(DBID *db)
{
  db->schema = NULL;
}

/*****************************************************************************
*****************************************************************************/
/* Check if we have a valid database. (was it ever created?  destroyed?)
*/
DB_STAT db_valid_db(DBID *db)
{
  if (db->schema == NULL) return(D_INVALID_DB);
  else return(D_OKAY);
}

/*****************************************************************************
*****************************************************************************/
/* Destroy our database and free all memory as fast as possible using key 0.
   This does not delete any records that were not indexed with key 0 (the last
   key field in the schema).
*/
DB_STAT db_destroy(DBID *db)
{
  long fld;
  LNKPTR *startlnk;
  long numlls = db->nlls;
  VOIDPTR ptr;
  LNKPTR *tnode = (LNKPTR *)db->firstnode;      /* Root node of database */
  LNKPTR *lnode;
  long ptr_offset = db->datalen + sizeof(CNTTYP);  /* Offset to first pointer */
  db_setlock(db);
  /* Free each record in the data base by walking level 1 pointers of key 0 */
  do
  {
#ifdef DEBUG
    /* Calculate how much memory we are about to free */
    tot_freed += sizeof(LNKPTR) * 3 * numlls + ptr_offset +
      sizeof(LNKPTR) * (*((CNTTYP *)((char *)tnode + db->datalen)) * db->nkeys);
#endif
    startlnk = tnode;   /* Remember db record to check for end of linked list */
    /* Loop through all linked lists on this db record */
    for (fld = numlls; --fld >= 0; )
    {
      lnode = (LNKPTR *)startlnk[-1];
      /* Free each linked list record until back at db record */
      while(lnode != startlnk)
      {
        ptr = (VOIDPTR)(lnode - 3);
        lnode = (LNKPTR *)lnode[-1];
        free(ptr);
#ifdef DEBUG
        ++tot_lnkrecs_deleted;
#endif
      }
      startlnk -= 3;    /* Adjust for next linked list */
    }
    /* Pointer to our next node */
    tnode = (LNKPTR *)*(LNKPTR *)((char *)tnode + ptr_offset);
    free((VOIDPTR)startlnk);
  } while(tnode != 0);  /* Continue until we hit end of database */
  /* Wipe out schema so any stray references to it will cause a memory fault */
  memset((VOIDPTR)db->schema, (int)'\0', sizeof(SCHEMA));
  db_resetlock(db);
  free((VOIDPTR)db->schema);
  db_init(db);
  return(D_OKAY);
}

/*****************************************************************************
*****************************************************************************/
/* Delete a key from a single index */
static DB_STAT xdelete(DBID *db, IPFCONST LNKPTR *rec, long fldnum)
{
  long compare;                                 /* Return value from compare */
  LNKPTR recptr = *rec;
  /* Compare function from schema or registered with db_create */
  long (*cmp_fcn)() = (*db->schema)[fldnum].ft_cmpfn;
  long len = (*db->schema)[fldnum].ft_len;      /* Length of field */
  /* Offset of key from start of record */
  long keyoffset = (*db->schema)[fldnum].ft_offset;
  VOIDPTR reckey = (char *)recptr + keyoffset;
  CNTTYP level = db->max_level;  /* Start at the highest used pointer level */
  /* Get keynumber from schema */
  long keynum = (*db->schema)[fldnum].ft_keynum;
  long datalen = db->datalen;   /* User data length in a record */
  long ptr_offset = datalen + sizeof(CNTTYP);   /* Offset to first pointer */
  CNTTYP reclevels = *(CNTTYP *)((char *)recptr + datalen);
  /* Pointer to start of level pointers for key */
  LNKPTR *lnodeptr = (LNKPTR *)((char *)recptr + ptr_offset +
    reclevels * keynum * sizeof(LNKPTR));
  LNKPTR current = db->firstnode;
  LNKPTR next;
  assert(keynum < db->nkeys);   /* Verify valid key number */
  /* Verify valid offset + length specified.
     Otherwise, could corrupt memory if we continue. */
  assert((keyoffset + len) <= datalen);
  /* Walk data base via level pointers trying to find record to delete */
  do
  {
    --level;
    /* Walk pointers of a level trying to find record until end of db */
    while ((next = ((LNKPTR *)((char *)current + ptr_offset))[
        *(CNTTYP *)((char *)current + datalen) * keynum + level]) != 0)
    {
      compare = (*cmp_fcn)((VOIDPTR)((char *)next + keyoffset), reckey, len);
      if (compare < 0)
      {
        current = next;   /* Since not at the record value yet, continue walk */
        continue;
      }
      if (compare > 0) break;  /* Since passed record value, go to next level */
      if (next < recptr)
      {
        current = next;     /* Since not at record address yet, continue walk */
        continue;
      }
      if (next > recptr) break;      /* Since passed record, go to next level */
      /* Record to delete was found.  Adjust the level pointers from this level
         down to bypass the record to be deleted. */
      do
      {
        ((LNKPTR *)((char *)current + ptr_offset))[*(CNTTYP *)((char *)current +
          datalen) * keynum + level] = lnodeptr[level];   /* Adjust level ptr */
        if (--level < 0) return(D_OKAY);
        /* Walk pointers at next level until record to delete is found again */
        while ((next = ((LNKPTR *)((char *)current + ptr_offset))[
            *(CNTTYP *)((char *)current + datalen) * keynum + level]) != recptr)
          current = next;
      } while (1);
    }
  } while (level > 0);
  return(D_NOTFOUND);
}

/*****************************************************************************
*****************************************************************************/
/* Delete the specified record from all indexes and free all used memory */
DB_STAT db_delete(DBID *db, VOIDPTR rec)
{
  LNKPTR *startlnk;
  long numlls = db->nlls;
  VOIDPTR ptr;
  LNKPTR *tnode = (LNKPTR *)*(LNKPTR *)rec;
  LNKPTR *lnode;
  DB_STAT stat;
  long fld = db->nfields;     /* Number of fields in the schema offset from 0 */
  long atleastonce = 0;
  long datalen = db->datalen;   /* User data length in a record */
  /* Length of all pointers for a single key */
  long keylen = *(CNTTYP *)((char *)tnode + datalen) * sizeof(LNKPTR);
  char *keystart = (char *)tnode + datalen + sizeof(CNTTYP);
#ifdef DEBUG
  /* Calculate total memory to be freed */
  long reclen = sizeof(LNKPTR) * 3 * numlls + datalen + sizeof(CNTTYP) +
    keylen * db->nkeys;
#endif
  assert(tnode);        /* Verify record pointer is indeed a pointer */
  db_setlock(db);
  /* Remove the record from all field indexes */
  for ( ; fld > 0; --fld)
  {
    if ((*db->schema)[fld].ft_key == 'n') continue;     /* Skip over non-keys */
    if (*(LNKPTR *)(keystart + (*db->schema)[fld].ft_keynum * keylen) ==
        (LNKPTR)tnode)
      continue;         /* Skip over optional keys (record points to itself) */
    /* Delete the index for this field */
    stat = xdelete(db, (LNKPTR *)rec, fld);
    if (stat != D_OKAY)
    {
      if (atleastonce != 0L)
      {
        /* Record was found in one index, but not all indexes.
           We must either delete the rest or restore those already deleted.
           This should never happen unless memory is corrupted.
        */
        db_exit("Corrupted memory found during delete!!!\n");
      }
#ifdef DEBUG
      printf("record not found to delete\n");
#endif /* DEBUG */
      db_resetlock(db);
      return(stat);
    }
    atleastonce = 1;
  }
  startlnk = tnode;     /* Remember db record to check for end of linked list */
  /* Loop through all linked lists on the db record to be deleted */
  for (fld = numlls; --fld >= 0; )
  {
    lnode = (LNKPTR *)startlnk[-1];
    /* Free each linked list record until back at db record */
    while(lnode != startlnk)
    {
      ptr = (VOIDPTR)(lnode - 3);
      lnode = (LNKPTR *)lnode[-1];
      free(ptr);
#ifdef DEBUG
      ++tot_lnkrecs_deleted;
#endif
    }
    startlnk -= 3;      /* Adjust for next linked list */
  }
#ifdef DEBUG
  tot_freed += reclen;
#endif
  free((VOIDPTR)startlnk);
  *(LNKPTR *)rec = NULL;        /* Indicate rec no longer points to a db rec */
  db_resetlock(db);
  return(D_OKAY);
}

/*****************************************************************************
*****************************************************************************/
/* Initialize all indexes according to the SCHEMA definition */
DB_STAT db_create(IPFCONST SCHEMA *sch, IPFCONST long numfields, IPFCONST long partition,
    DBID *db)
{
  char string[80];
  long fld = numfields - 1;
  DB_STAT stat;
  if (sch[0].ft_key != 'N' && sch[0].ft_key != 'n')
    db_exit("First schema key type must be 'n'\n");
  if (sch[0].ft_type != 'C' && sch[0].ft_type != 'c')
    db_exit("First schema field type must be 'c'\n");
  db->datalen = (sch[0].ft_len + sizeof(dmgrWORD) - 1) / sizeof(dmgrWORD) *
      sizeof(dmgrWORD);
#ifdef DEBUG
  if (sch[0].ft_len % sizeof(dmgrWORD) != 0)
    printf(
      "data base record length is increased to a word boundary length of %ld\n",
      db->datalen);
#endif /* DEBUG */
  db->locked = 0;
  db_setlock(db);
#ifdef DEBUG
#if defined(__STDC__)
  puts("ANSI C conforming compiler\n");
#else
  puts("NOT an ANSI C compiler\n");
#endif /* __STDC__ */
#endif /* DEBUG */
  db->nfields = fld;    /* Number of fields in the schema offset from 0 */
  db->nkeys = 0;
  db->nlls = 0;
  db->max_level = 1;
  /* If the partition is not specified, default to PARTITION */
  db->partition = (partition > 0) ? partition : PARTITION;
  /* Make room for all the fields in the schema */
  db->schema = (SCHEMA (*)[])malloc((size_t)(sizeof(SCHEMA) * numfields));
  if (db->schema == NULL) db_exit("out of memory to create database\n");
  /* Check all schema field values in reverse order */
  for ( ; fld >= 0; --fld)
  {
    if (((*db->schema)[fld].ft_offset = sch[fld].ft_offset) < 0)
      db_exit("offset in schema may not be negative\n");
    if (((*db->schema)[fld].ft_len = sch[fld].ft_len) < 0)
      db_exit("length in schema may not be negative\n");
    switch(sch[fld].ft_key)
    {
      /* Field must have unique keys */
      case 'U':
      case 'u': (*db->schema)[fld].ft_keynum = db->nkeys++;
                (*db->schema)[fld].ft_key = 'u';
                break;
      /* Field may have duplicate keys */
      case 'D':
      case 'd': (*db->schema)[fld].ft_keynum = db->nkeys++;
                (*db->schema)[fld].ft_key = 'd';
                break;
      /* Field is not a key */
      case 'N':
      case 'n': (*db->schema)[fld].ft_key = 'n';
                break;
      default: sprintf(&string[0],
                   "unknown schema key type %c\n",sch[fld].ft_key);
               db_exit(&string[0]);
    }
    switch(sch[fld].ft_type)
    {
      /* Integer field */
      case 'I':
      case 'i': if (sch[fld].ft_len != sizeof(long))
                {
                  sprintf(&string[0],
                      "Schema field type i must have a length of %2d\n",
                      sizeof(long));
                  db_exit(&string[0]);
                }
                (*db->schema)[fld].ft_type = 'i';
                (*db->schema)[fld].ft_cmpfn = db_intcmp;
                break;
      /* Character field */
      case 'C':
      case 'c': (*db->schema)[fld].ft_type = 'c';
                (*db->schema)[fld].ft_cmpfn = db_strcmp;
                break;
      /* Float field */
      case 'F':
      case 'f': if (sch[fld].ft_len != sizeof(float))
                {
                  sprintf(&string[0],
                      "Schema field type f must have a length of %2d\n",
                      sizeof(float));
                  db_exit(&string[0]);
                }
                (*db->schema)[fld].ft_type = 'f';
                (*db->schema)[fld].ft_cmpfn = db_fltcmp;
                break;
      /* Double field */
      case 'D':
      case 'd': if (sch[fld].ft_len != sizeof(double))
                {
                  sprintf(&string[0],
                      "Schema field type d must have a length of %2d\n",
                      sizeof(double));
                  db_exit(&string[0]);
                }
                (*db->schema)[fld].ft_type = 'd';
                (*db->schema)[fld].ft_cmpfn = db_dblcmp;
                break;
      /* Linked list field */
      case 'L':
      case 'l': if ((*db->schema)[fld].ft_key != 'n')
                {
                  /* A linked list cannot be a key */
                  db_exit("Schema key type must be n for field type l\n");
                }
                (*db->schema)[fld].ft_type = 'l';
                (*db->schema)[fld].ft_offset = db->nlls++;
                (*db->schema)[fld].ft_len = (sch[fld].ft_len + sizeof(dmgrWORD) -
                  1) / sizeof(dmgrWORD) * sizeof(dmgrWORD);
#ifdef DEBUG
                if (sch[fld].ft_len % sizeof(dmgrWORD) != 0)
                  printf(
    "linked list record length is increased to a word boundary length of %ld\n",
                      (*db->schema)[fld].ft_len);
#endif
                continue;  /* Skip the data length check for linked lists */
      /* User-supplied function field */
      case 'U':
      case 'u':
#ifdef DEBUG
                printf("registering user supplied compare fcn\n");
#endif /* DEBUG */
                (*db->schema)[fld].ft_type = 'u';
                (*db->schema)[fld].ft_cmpfn = sch[fld].ft_cmpfn;
                break;
      default: db_exit("unrecognized function type in db_create\n");
    }
    if (sch[fld].ft_offset + sch[fld].ft_len > db->datalen)
      db_exit("Offset+len for non-l field type is > than 1st schema length\n");
  }
  if (db->nkeys == 0) db_exit("There must be at least one key in the schema\n");
  /* Create a new node and use it as the header of the list.
     Leave room for MAXLEVEL pointers. */
  stat = db_newnode(&(db->firstnode), (CNTTYP)MAXLEVEL, db->nkeys, db->datalen,
      db->nlls);
#ifdef DEBUG
  printf("dbid=%x, partition=%ld, nkeys=%ld, nfields=%ld\n",
      db, db->partition, db->nkeys, db->nfields);
  printf("      datalen=%ld, max_level=%ld, firstnode=%x, stat=%d\n",
      db->datalen, (long)db->max_level, db->firstnode, (int)stat);
#endif
  db_resetlock(db);
  return(stat);
}

/*****************************************************************************
*****************************************************************************/
/* Find the first occurrence of a key in db for search */
static DB_STAT xsearch(DBID *db, IPFCONST LNKPTR *rec, LNKPTR *retrec, long fldnum)
{
  long compare = 1L;
  /* Compare function from schema */
  long (*cmp_fcn)() = (*db->schema)[fldnum].ft_cmpfn;
  long len = (*db->schema)[fldnum].ft_len;
  /* Offset of key in record */
  long keyoffset = (*db->schema)[fldnum].ft_offset;
  VOIDPTR reckey = (char *)*rec + keyoffset;
  CNTTYP level = db->max_level - 1;
  long keynum = (*db->schema)[fldnum].ft_keynum;
  long datalen = db->datalen;   /* User data length in a record */
  long ptr_offset = datalen + sizeof(CNTTYP);   /* Offset to first pointer */
  LNKPTR current = db->firstnode;       /* Header node of data base */
  LNKPTR next;
  assert(keynum < db->nkeys);   /* Verify valid key number */
  assert((keyoffset + len) <= datalen) ;   /* Verify valid keyoffset + length */
  /* Loop through all levels of key, starting with highest level we have used */
  do
  {
    /* Walk pointers at this level until end of data base */
    while((next = ((LNKPTR *)((char *)current + ptr_offset))[
        *(CNTTYP *)((char *)current + datalen) * keynum + level]) != 0)
    {
      /* Compare our reference key to the current database record key */
      compare = (*cmp_fcn)((VOIDPTR)((char *)next + keyoffset), reckey, len);
      if (compare >= 0)
        break;  /* Since we are at or after the key to find, go down a level */
      current = next;
    }
  } while (--level >= 0);
  *retrec = next;       /* Store record address in user supplied variable */
  if (compare != 0L) return(D_NOTFOUND);
  return(D_FOUND);
}

/*****************************************************************************
*****************************************************************************/
/* Return the next record after a specified record in the database.
   This will be the record with the next duplicate or higher value as defined
   by the compare function registered for this field in the schema.
*/
DB_STAT db_next(IPFCONST DBID *db, IPFCONST VOIDPTR rec, VOIDPTR retrec,
    IPFCONST long fldnum)
{
  char *lnode = (char *)*(LNKPTR *)rec;
  long datalen = db->datalen;   /* User data length in a record */
  char *tnode = lnode + datalen;        /* Pointer to number of levels */
  long keynum = (*db->schema)[fldnum].ft_keynum;  /* Keynumber for this field */
  assert(lnode);        /* Verify passed record pointer is indeed a pointer */
  assert(fldnum <= db->nfields);        /* Verify field number */
  assert((*db->schema)[fldnum].ft_key != 'n');  /* Verify it is key field */
  while (GETLOCK);  /* Wait until the data base is not locked for an update */
  if ((*(LNKPTR *)retrec = ((LNKPTR *)(tnode + sizeof(CNTTYP)))[
      *(CNTTYP *)tnode * keynum]) != 0)
  {
    ENDLOCK;    /* End of GETLOCK block */
    return(D_FOUND);
  }
  else
  {
    ENDLOCK;    /* End of GETLOCK block */
    return(D_EOF);
  }
}

/*****************************************************************************
*****************************************************************************/
/* Return the first record in database as sorted by a specified field number */
DB_STAT db_first(IPFCONST DBID *db, VOIDPTR retrec, IPFCONST long fldnum)
{
  return(db_next(db, (VOIDPTR)&db->firstnode, retrec, fldnum));
}

/*****************************************************************************
*****************************************************************************/
/* Return the next record after a specified record in the database, but only
   return a FOUND status if the key for the specified field number in the next
   record matches the key in the specified record (ie, the next record
   has a duplicate key value)
*/
DB_STAT db_keynext(IPFCONST DBID *db, IPFCONST VOIDPTR rec, VOIDPTR retrec,
    IPFCONST long fldnum)
{
  DB_STAT stat = db_next(db, rec, retrec, fldnum); /* Get the next record */
  if (stat == D_FOUND)
  {
    long keyoffset = (*db->schema)[fldnum].ft_offset;
    if ((*(*db->schema)[fldnum].ft_cmpfn)(
        (VOIDPTR)((char *)(*(LNKPTR *)rec) + keyoffset),
        (VOIDPTR)((char *)(*(LNKPTR *)retrec) + keyoffset),
        (*db->schema)[fldnum].ft_len) == 0)
      return(D_FOUND);
    else return(D_EOF);   /* The next record does not have the same key value */
  }
  else return(stat);
}

/*****************************************************************************
*****************************************************************************/
/* Search for a specified key value in the database.
   Return a pointer to the first record with that key value or the first record
   with a higher key value.
*/
DB_STAT db_search(DBID *db, IPFCONST VOIDPTR searchrec, VOIDPTR retrec,
    IPFCONST long fldnum)
{
  DB_STAT stat;
  assert(fldnum <= db->nfields);        /* Verify field number */
  assert((*db->schema)[fldnum].ft_key != 'n');  /* Verify it is key field */
  db_setlock(db);
  stat = xsearch(db, searchrec, retrec, fldnum);
  db_resetlock(db);
  if (*(LNKPTR *)retrec != 0) return(stat);
  else return(D_EOF);  /* Key was not found and would be put at the index end */
}

/*****************************************************************************
*****************************************************************************/
/* Find the position of a key in the data base for insert, keeping
   the addresses of records with the same key in ascending order */
static void isearch(DBID *db, IPFCONST LNKPTR irec, IPFCONST LNKPTR *rec, LNKPTR *tmp,
    long fldnum)
{
  long compare;
  /* Compare function from schema */
  long (*cmp_fcn)() = (*db->schema)[fldnum].ft_cmpfn;
  long len = (*db->schema)[fldnum].ft_len;
  /* Offset of key in record */
  long keyoffset = (*db->schema)[fldnum].ft_offset;
  VOIDPTR reckey = (char *)*rec + keyoffset;
  CNTTYP level = db->max_level - 1;
  long keynum = (*db->schema)[fldnum].ft_keynum;
  long datalen = db->datalen;   /* User data length in a record */
  long ptr_offset = datalen + sizeof(CNTTYP);   /* Offset to first pointer */
  LNKPTR current=db->firstnode;         /* Header node of data base */
  LNKPTR next;
  assert(keynum < db->nkeys);   /* Verify valid key number */
  assert((keyoffset + len) <= datalen) ;   /* Verify valid keyoffset + length */
  /* Loop through all levels of key, starting with highest level we have used */
  do
  {
    /* Walk pointers at this level until end of data base */
    while((next = ((LNKPTR *)((char *)current + ptr_offset))[
        *(CNTTYP *)((char *)current + datalen) * keynum + level]) != 0)
    {
      /* Compare our reference key to the current database record key */
      compare = (*cmp_fcn)((VOIDPTR)((char *)next + keyoffset), reckey, len);
      if (compare > 0)
        break;  /* Since we are after insertion point for key, go save point */
      /* If we found the key and are after the insertion point for the record
         address, go remember position */
      if ((compare == 0) && (next >= irec)) break;
      current = next;
    }
    /* Maintain list of siblings.  They are needed to insert. */
    tmp[level] = current;
  } while (--level >= 0);
  return;
}

/*****************************************************************************
*****************************************************************************/
/* Update data base record with the data from newrec.
   If any keys have changed, then we must update those indexes. */
DB_STAT db_update(DBID *db, VOIDPTR rec, IPFCONST VOIDPTR newrec)
{
  long keynum;
  long keyoffset;
  char *nnode = (char *)*(LNKPTR *)newrec;
  LNKPTR onode = *(LNKPTR *)rec;
  char *lnode = (char *)onode;
  LNKPTR retrec;
  /* Storage for the sibling pointers needed to insert key in an index */
  LNKPTR temp[MAXLEVEL];
  LNKPTR *tempptr;
  char *tmpptr;
  long i;
  long fld = db->nfields;     /* Number of fields in the schema offset from 0 */
  long datalen = db->datalen;   /* User data length in a record */
  CNTTYP newlevel = *(CNTTYP *)(lnode + datalen);       /* Number of levels */
  long keylen = newlevel * sizeof(LNKPTR);
  long ptr_offset = datalen + sizeof(CNTTYP);   /* Offset to first pointer */
  char *pnode = lnode + ptr_offset;
  DB_STAT stat;
  long atleastonce = 0;
  LNKPTR *lptr;
  assert(onode);        /* Verify passed record pointer is indeed pointer */
  db_setlock(db);
  /* Check all unique key fields to make sure no duplicates will occur.
     Field 0 is never a key. */
  for ( ; fld > 0; --fld)
    if ((*db->schema)[fld].ft_key == 'u')
    {
      if (*(LNKPTR *)(pnode + (*db->schema)[fld].ft_keynum * keylen) == onode)
        continue;       /* Skip optional keys (record points to itself) */
      keyoffset = (*db->schema)[fld].ft_offset;
      /* If a key on newrec is different than the old record, check if the key
         value on newrec is already in the data base */
      if ((*(*db->schema)[fld].ft_cmpfn)(lnode + keyoffset, nnode + keyoffset,
          (*db->schema)[fld].ft_len) != 0)
        if (xsearch(db, (LNKPTR *)newrec, &retrec, fld) == D_FOUND)
        {
          db_resetlock(db);
          return(D_DUPLICATE);
        }
    }
  fld = db->nfields;
  /* Re-index all new key values */
  for ( ; fld > 0; --fld)
  {
    if ((*db->schema)[fld].ft_key == 'n') continue;    /* Skip non-key fields */
    keynum = (*db->schema)[fld].ft_keynum;
    lptr = (LNKPTR *)(pnode + keynum * keylen);  /* Point to pointers for key */
    if (*lptr == onode)
      continue;         /* Skip optional key (record points to itself) */
    /* Key offset from start of record */
    keyoffset = (*db->schema)[fld].ft_offset;
    /* Compare each key field to see if it was modified */
    if ((*(*db->schema)[fld].ft_cmpfn)(lnode + keyoffset, nnode + keyoffset,
        (*db->schema)[fld].ft_len) != 0)
    {
      /* Key was modified so delete the old key */
      stat = xdelete(db, (LNKPTR *)rec, fld);
      if (stat != D_OKAY)
      {
        if (atleastonce != 0L)
        {
          /* An error occurred in removing a key from an index after finding
             the record in a prior index.
             This should never happen unless memory is corrupted.
          */
          db_exit("Corrupted memory found during update!!!\n");
        }
#ifdef DEBUG
        printf("record not found to update\n");
#endif
        db_resetlock(db);
        return(stat);
      }
      /* Find the correct location to store the new key on all levels */
      isearch(db, onode, (LNKPTR *)newrec, &temp[0], fld);
      /* Insert new key into data base by adjusting all of its level pointers
         and the level pointers of prior records in the data base */
      for (i = 0; i < newlevel; ++i)
      {
        tmpptr = (char *)temp[i];
        tempptr = (LNKPTR *)(tmpptr + ptr_offset +
            (*(CNTTYP *)(tmpptr + datalen) * keynum + i) * sizeof(LNKPTR));
        lptr[i] = *tempptr;     /* Adjust level pointer on old record */
        *tempptr = onode;  /* Set level pointer on a prior record to old rec */
      }
      atleastonce = 1;
    }
    else
    {
#ifdef DEBUG
      printf("no key change for field #%ld\n", fld);
#endif
    }
  }
  /* Copy the new data into the database */
  memcpy((VOIDPTR)onode, (VOIDPTR)nnode, (size_t)datalen);
  db_resetlock(db);
  return(D_OKAY);
}
/*****************************************************************************
*****************************************************************************/
/* Allocate enough memory to hold record and key pointers.
   Then insert record into the database, updating all appropriate indexes. */
DB_STAT db_insert(DBID *db, IPFCONST VOIDPTR newrec, VOIDPTR retrec)
{
  long numlls = db->nlls;
  long keynum;
  char *pnode;
  LNKPTR *lptr;
  LNKPTR *tempptr;
  char *tmpptr;
  LNKPTR lnode;
  LNKPTR tmprec;
  /* Storage for the sibling pointers needed to insert key in an index */
  LNKPTR temp[MAXLEVEL];
  CNTTYP i;
  long fld = db->nfields;     /* Number of fields in the schema offset from 0 */
  long datalen = db->datalen;   /* User data length in a record */
  long ptr_offset = datalen + sizeof(CNTTYP);   /* Offset to first pointer */
  /* Generate a random number of level pointers to put on this node */
  CNTTYP newlevel = (CNTTYP)db_randomlevel(db->partition);
  DB_STAT stat;
  db_setlock(db);
  /* Check all unique key fields to make sure no duplicates will occur.
     Field 0 is never a key. */
  for ( ; fld > 0; --fld)
    if ((*db->schema)[fld].ft_key == 'u')
      if (xsearch(db, (LNKPTR *)newrec, &tmprec, fld) == D_FOUND)
      {
        db_resetlock(db);
        return(D_DUPLICATE);
      }
  /* Create a new node large enough to hold the user supplied data, the level,
     the key pointers, and any link pointers */
  stat = db_newnode(&lnode, newlevel, db->nkeys, datalen, numlls);
  if (stat != D_OKAY)
  {
    db_resetlock(db);
    return(stat);       /* Probably out of memory */
  }
  pnode = (char *)lnode + ptr_offset;   /* Pointer to start of key pointers */
#ifdef DEBUG
  array[newlevel]++;
#endif
  /* Copy the data from user space into our newly created node */
  memcpy((VOIDPTR)lnode, (VOIDPTR)*(LNKPTR *)newrec, (size_t)datalen);
  fld = db->nfields;
  /* Index all key values */
  for ( ; fld > 0; --fld)
  {
    if ((*db->schema)[fld].ft_key == 'n') continue;    /* Skip non-key fields */
    /* Find a spot in the index to store the key on all levels.
       The pointers to the siblings are stored in the temp array. */
    isearch(db, lnode, (LNKPTR *)newrec, &temp[0], fld);
    if (newlevel > db->max_level)
    {
      /* Since we reached a new level, prepare to point all new level pointers
         on the first node to our new record */
      for (i = db->max_level; i < newlevel; ++i)
      {
        temp[i] = db->firstnode;
      }
      db->max_level = newlevel;
    }
    keynum = (*db->schema)[fld].ft_keynum;
    /* Point to level pointers for key */
    lptr = (LNKPTR *)(pnode + newlevel * keynum * sizeof(LNKPTR));
    /* Insert key into data base by adjusting all of its level pointers
       and the level pointers of prior records in the data base */
    for (i = 0; i < newlevel; ++i)
    {
      tmpptr = (char *)temp[i];
      tempptr = (LNKPTR *)(tmpptr + ptr_offset +
          (*(CNTTYP *)(tmpptr + datalen) * keynum + i) * sizeof(LNKPTR));
      lptr[i] = *tempptr;       /* Adjust level pointer on new record */
      *tempptr = lnode;  /* Set level pointer on a prior record to new record */
    }
  }
  *(LNKPTR *)retrec = lnode;    /* Return pointer to new record */
  db_resetlock(db);
  return(D_OKAY);
}

/*****************************************************************************
*****************************************************************************/
/* Allocate enough memory to hold record and key pointers.  Then insert record
   into the database, updating all appropriate indexes except optional keys.
   Note that if key 0 (the last key field in the schema) is not indexed,
   db_destroy will not delete this record; db_delete would have to be called
   with this specific record to delete it and free its associated memory.
   Also ll_delete_all_lists would not look at this record.
*/
DB_STAT db_insert_opt(DBID *db, IPFCONST VOIDPTR newrec, VOIDPTR retrec,
    IPFCONST char *optstring)
{
  long numlls = db->nlls;
  long keynum;
  char *pnode;
  LNKPTR *lptr;
  LNKPTR *tempptr;
  char *tmpptr;
  LNKPTR lnode;
  LNKPTR tmprec;
  /* Storage for the sibling pointers needed to insert key in an index */
  LNKPTR temp[MAXLEVEL];
  CNTTYP i;
  long fld = db->nfields;     /* Number of fields in the schema offset from 0 */
  long datalen = db->datalen;   /* User data length in a record */
  long ptr_offset = datalen + sizeof(CNTTYP);   /* Offset to first pointer */
  /* Generate a random number of level pointers to put on this node */
  CNTTYP newlevel = (CNTTYP)db_randomlevel(db->partition);
  /* Length of all level pointers for one key to be put on this node */
  long keylen = newlevel * sizeof(LNKPTR);
  DB_STAT stat;
  db_setlock(db);
  /* Check all unique key fields (except for the fields to exclude based
     on a corresponding "-" in the optstring array) to make sure no duplicates
     will occur.  Since field 0 is never a key, the first character (element 0)
     in the optstring array corresponds to field 1 (ie, the second field
     in the schema).
  */
  for ( ; fld > 0; --fld)
    if ((*db->schema)[fld].ft_key == 'u' && optstring[fld - 1] != '-')
      if (xsearch(db, (LNKPTR *)newrec, &tmprec, fld) == D_FOUND)
      {
        db_resetlock(db);
        return(D_DUPLICATE);
      }
  /* Create a new node large enough to hold the user supplied data, the level,
     the key pointers, and any link pointers */
  stat = db_newnode(&lnode, newlevel, db->nkeys, datalen, numlls);
  if (stat != D_OKAY)
  {
    db_resetlock(db);
    return(stat);       /* Probably out of memory */
  }
  pnode = (char *)lnode + ptr_offset;   /* Pointer to start of key pointers */
#ifdef DEBUG
  array[newlevel]++;
#endif
  /* Copy the data from user space into our newly created node */
  memcpy((VOIDPTR)lnode, (VOIDPTR)*(LNKPTR *)newrec, (size_t)datalen);
  fld = db->nfields;
  /* Index all key values */
  for ( ; fld > 0; --fld)
  {
    if ((*db->schema)[fld].ft_key == 'n') continue;    /* Skip non-key fields */
    keynum = (*db->schema)[fld].ft_keynum;
    /* Point to level pointers for key */
    lptr = (LNKPTR *)(pnode + keynum * keylen);
    if (optstring[fld - 1] == '-')
    {
      /* Since this is an optional key not to be indexed, set all of its level
         pointers to point to this record.  Other routines will then know that
         the key on this record was not indexed.
      */
      for (i = 0; i < newlevel; ++i)
      {
        *lptr = lnode;
        ++lptr;
      }
      continue;         /* Go continue with the next field */
    }
    /* Find a spot in the index to store the key on all levels.
       The pointers to the siblings are stored in the temp array. */
    isearch(db, lnode, (LNKPTR *)newrec, &temp[0], fld);
    if (newlevel > db->max_level)
    {
      /* Since we reached a new level, prepare to point all new level pointers
         on the first node to our new record */
      for (i = db->max_level; i < newlevel; ++i)
      {
        temp[i] = db->firstnode;
      }
      db->max_level = newlevel;
    }
    /* Insert key into data base by adjusting all of its level pointers
       and the level pointers of prior records in the data base */
    for (i = 0; i < newlevel; ++i)
    {
      tmpptr = (char *)temp[i];
      tempptr = (LNKPTR *)(tmpptr + ptr_offset +
          (*(CNTTYP *)(tmpptr + datalen) * keynum + i) * sizeof(LNKPTR));
      lptr[i] = *tempptr;       /* Adjust level pointer on new record */
      *tempptr = lnode;  /* Set level pointer on a prior record to new record */
    }
  }
  *(LNKPTR *)retrec = lnode;    /* Return pointer to new record */
  db_resetlock(db);
  return(D_OKAY);
}

/*****************************************************************************
*****************************************************************************/
/* Create a new linked list record of length datalen + room for all pointers.
DATA FORMAT
__________________________
| link     | user data   |
| pointers |             |
--------------------------
All pointers point to the start of user data.
*/
static DB_STAT db_newlink(LNKPTR *temp, long nptrs, long datalen)
{
  long llsptrslen = sizeof(LNKPTR) * nptrs;
  long reclen = llsptrslen + datalen;
  /* Memory is initialized to zero so that the record is coded as a linked list
     record (low bit of the flags = 0) */
  *temp = (LNKPTR)calloc((size_t)reclen, (size_t)1);
#ifdef DEBUG
  ++tot_lnkrecs_created;
#endif
  if (*temp == NULL) return(D_NOMEMORY);
  else
  {
    *temp = (LNKPTR)((char *)*temp + llsptrslen);   /* Point to the user data */
    return(D_OKAY);
  }
}

/*****************************************************************************
*****************************************************************************/
/* Allocate enough memory to hold a linked list record with its pointers
   for a specified linked list field.  Then insert the linked list record
   at one of the ends of the linked list on a specified database record.
*/
DB_STAT ll_insert(DBID *db, VOIDPTR rec, VOIDPTR retlnkrec, IPFCONST long fldnum,
    IPFCONST char order)
{
  char string[80];
  LNKPTR nptr;
  /* Pointer to just passed the three linked list pointers in the database
     record for the specified linked list field.  The ends of the linked list
     point to this position.
  */
  LNKPTR lnode = (LNKPTR)((LNKPTR *)*(LNKPTR *)rec -
      (*db->schema)[fldnum].ft_offset * 3);
  /* Pointer to the linked list next pointer in the database record */
  LNKPTR *t = (LNKPTR *)lnode - 1;
  LNKPTR *n;
  DB_STAT stat;
  assert(fldnum <= db->nfields);        /* Verify field number */
  assert((*db->schema)[fldnum].ft_type == 'l');   /* Verify linked list field */
  assert(*(LNKPTR *)rec);   /* Verify passed record pointer is indeed pointer */
  assert(((long)t[-2] & 1L) == 1L);    /* Verify rec pointer points to db rec */
  /* Create a new linked list record large enough to hold the user data and
     the three link pointers (flags, previous, next) */
  stat = db_newlink(&nptr, 3L, (*db->schema)[fldnum].ft_len);
  if (stat != D_OKAY) return(stat);
  db_setlock(db);
  n = (LNKPTR *)nptr - 1;       /* Pointer to next pointer in linked list rec */
  switch(order)
  {
    /* Insert new linked list record as first in linked list */
    case 'F':
    case 'f': n[0] = t[0];      /* Point new record to previous first record */
              ((LNKPTR *)t[0])[-2] = nptr;   /* Point 1st rec back to new rec */
              t[0] = nptr;      /* Point database record to new record */
              n[-1] = lnode;    /* Point new record back to database record */
              break;
    /* Insert new linked list record as last in linked list */
    case 'L':
    case 'l': n[-1] = t[-1];    /* Point new record back to previous last rec */
              ((LNKPTR *)t[-1])[-1] = nptr;     /* Point last rec to new rec */
              t[-1] = nptr;     /* Point database record back to new record */
              n[0] = lnode;     /* Point new record to database record */
              break;
    default: sprintf(&string[0], "unsupported linked list ordering %c\n",
                 order);
             db_exit(&string[0]);
  }
  *(LNKPTR *)retlnkrec = nptr;  /* Return pointer to new linked list record */
  db_resetlock(db);
  return(D_OKAY);
}

/*****************************************************************************
*****************************************************************************/
/* Allocate enough memory to hold a linked list record with its pointers.
   Then insert the new linked list record either after or before a specified
   "linked list" record.  No checking is done to verify the specified record
   is a linked list record.  This allows passing a pointer that is just passed
   a set of linked list pointers within a database record instead.
   Since a database record pointer is just passed the pointers for linked
   list 1, it can be passed to this routine without adjustment to insert a new
   linked list record at one of the ends of linked list 1.
   This routine also allows varying data lengths within a linked list.
*/
DB_STAT ll_insert_in(VOIDPTR lnkrec, VOIDPTR retlnkrec, IPFCONST long lnklen,
    IPFCONST char insert_type)
{
  char string[80];
  LNKPTR nptr;
  LNKPTR lnode = *(LNKPTR *)lnkrec;
  /* Pointer to the linked list next pointer in the specified linked list rec */
  LNKPTR *t = (LNKPTR *)lnode - 1;
  LNKPTR *n;
  DB_STAT stat;
  assert(lnode);       /* Verify passed linked list pointer is indeed pointer */
  /* Create a new linked list record large enough to hold the user data and
     the three link pointers (flags, previous, next) */
  if ((stat = db_newlink(&nptr, 3L, lnklen)) != D_OKAY) return(stat);
  n = (LNKPTR *)nptr - 1;       /* Pointer to next pointer in linked list rec */
  switch(insert_type)
  {
    /* Insert new linked list record after specified linked list record */
    case 'A':
    case 'a': n[0] = t[0];      /* Point new record to specified next record */
              ((LNKPTR *)t[0])[-2] = nptr;  /* Point next rec back to new rec */
              t[0] = nptr;      /* Point specified record to new record */
              n[-1] = lnode;    /* Point new record back to specified record */
              break;
    /* Insert new linked list record before specified linked list record */
    case 'B':
    case 'b': n[-1] = t[-1];  /* Point new rec back to specified previous rec */
              ((LNKPTR *)t[-1])[-1] = nptr;  /* Point previous rec to new rec */
              t[-1] = nptr;     /* Point specified record back to new record */
              n[0] = lnode;     /* Point new record to specified record */
              break;
    default: sprintf(&string[0], "unsupported linked list insertion type %c\n",
                 insert_type);
             db_exit(&string[0]);
  }
  *(LNKPTR *)retlnkrec = nptr;  /* Return pointer to new linked list record */
  return(D_OKAY);
}

/*****************************************************************************
*****************************************************************************/
/* Return the previous linked list record from a specified linked list record */
DB_STAT ll_prev(IPFCONST VOIDPTR lnkrec, VOIDPTR retlnkrec)
{
  assert(*(LNKPTR *)lnkrec);    /* Verify passed pointer is indeed pointer */
  if (((long)((LNKPTR *)(*(LNKPTR *)retlnkrec =
      ((LNKPTR *)*(LNKPTR *)lnkrec)[-2]))[-3] & 1L) != 1L)
    return(D_FOUND);    /* Flags of previous record indicates linked list rec */
  else return(D_EOF);   /* Flags of previous record indicates database record */
}

/*****************************************************************************
*****************************************************************************/
/* Return the next linked list record after a specified linked list record */
DB_STAT ll_next(IPFCONST VOIDPTR lnkrec, VOIDPTR retlnkrec)
{
  assert(*(LNKPTR *)lnkrec);    /* Verify passed pointer is indeed pointer */
  if (((long)((LNKPTR *)(*(LNKPTR *)retlnkrec =
      ((LNKPTR *)*(LNKPTR *)lnkrec)[-1]))[-3] & 1L) != 1L)
    return(D_FOUND);    /* Flags of next record indicates linked list record */
  else return(D_EOF);   /* Flags of next record indicates database record */
}

/*****************************************************************************
*****************************************************************************/
/* Return the last linked list record for a specified linked list field
   on the specified database record */
DB_STAT ll_last(IPFCONST DBID *db, IPFCONST VOIDPTR rec, VOIDPTR retlnkrec,
    IPFCONST long fldnum)
{
  /* Pointer to just passed the three linked list pointers in the database
     record for the specified linked list field.  The ends of the linked list
     point to this position.
  */
  LNKPTR *lnode = (LNKPTR *)*(LNKPTR *)rec -
      (*db->schema)[fldnum].ft_offset * 3;
  assert(fldnum <= db->nfields);        /* Verify field number */
  assert((*db->schema)[fldnum].ft_type == 'l');   /* Verify linked list field */
  assert(*(LNKPTR *)rec);   /* Verify passed record pointer is indeed pointer */
  /* Verify specified record pointer points to database record (low linked list
     flags bit = 1) */
  assert(((long)lnode[-3] & 1L) == 1L);
  if ((*(LNKPTR *)retlnkrec = lnode[-2]) != (LNKPTR)lnode) return(D_FOUND);
  else return(D_EOF);  /* Previous linked list pointer points to database rec */
}

/*****************************************************************************
*****************************************************************************/
/* Return the first linked list record for a specified linked list field
   on the specified database record */
DB_STAT ll_first(IPFCONST DBID *db, IPFCONST VOIDPTR rec, VOIDPTR retlnkrec,
    IPFCONST long fldnum)
{
  /* Pointer to just passed the three linked list pointers in the database
     record for the specified linked list field.  The ends of the linked list
     point to this position.
  */
  LNKPTR *lnode = (LNKPTR *)*(LNKPTR *)rec -
      (*db->schema)[fldnum].ft_offset * 3;
  assert(fldnum <= db->nfields);        /* Verify field number */
  assert((*db->schema)[fldnum].ft_type == 'l');   /* Verify linked list field */
  assert(*(LNKPTR *)rec);   /* Verify passed record pointer is indeed pointer */
  /* Verify specified record pointer points to database record (low linked list
     flags bit = 1) */
  assert(((long)lnode[-3] & 1L) == 1L);
  if ((*(LNKPTR *)retlnkrec = lnode[-1]) != (LNKPTR)lnode) return(D_FOUND);
  else return(D_EOF);   /* Next linked list pointer points to database record */
}

/*****************************************************************************
*****************************************************************************/
/* Delete the specified linked list record from the linked list and free its
   memory */
DB_STAT ll_delete(VOIDPTR lnkrec)
{
  LNKPTR *lnode = (LNKPTR *)*(LNKPTR *)lnkrec;
  LNKPTR next = lnode[-1];      /* Pointer to next linked list record */
  LNKPTR prior = lnode[-2];     /* Pointer to prior linked list record */
  assert(lnode);   /* Verify passed linked list rec pointer is indeed pointer */
  /* Verify specified linked list record pointer points to a linked list record
     (low flags bit does not = 1) */
  assert(((long)lnode[-3] & 1L) != 1L);
  /* The following statements will also adjust linked list pointers on database
     records, if the linked list record is at one of the ends of the linked list
  */
  ((LNKPTR *)prior)[-1] = next;         /* Point prior record to next record */
  ((LNKPTR *)next)[-2] = prior;         /* Point next rec back to prior rec */
  free((VOIDPTR)(lnode - 3));   /* Free linked list record memory */
#ifdef DEBUG
  ++tot_lnkrecs_deleted;
#endif
  return(D_OKAY);
}

/*****************************************************************************
*****************************************************************************/
/* Delete the linked list for the specified field on the specified record and
   free its memory */
DB_STAT ll_delete_list(DBID *db, VOIDPTR rec, IPFCONST long fldnum)
{
  VOIDPTR ptr;
  /* Pointer to just passed the three linked list pointers in the database
     record for the specified linked list field.  The ends of the linked list
     point to this position.
  */
  LNKPTR *startlnk = (LNKPTR *)*(LNKPTR *)rec -
      (*db->schema)[fldnum].ft_offset * 3;
  LNKPTR *lnode = (LNKPTR *)startlnk[-1];  /* Pointer to next linked list rec */
  assert(fldnum <= db->nfields);        /* Verify field number */
  assert((*db->schema)[fldnum].ft_type == 'l');   /* Verify linked list field */
  assert(*(LNKPTR *)rec);   /* Verify passed record pointer is indeed pointer */
  /* Verify specified record pointer points to database record (low linked list
     flags bit = 1) */
  assert(((long)startlnk[-3] & 1L) == 1L);
  db_setlock(db);
  /* Free each linked list record until back at db record */
  while(lnode != startlnk)
  {
    ptr = (VOIDPTR)(lnode - 3);
    lnode = (LNKPTR *)lnode[-1];
    free(ptr);
#ifdef DEBUG
    ++tot_lnkrecs_deleted;
#endif
  }
  startlnk[-1] = (LNKPTR) startlnk;     /* Set "next" ptr to indicate no list */
  startlnk[-2] = (LNKPTR) startlnk;    /* Set "prior" ptr to indicate no list */
  db_resetlock(db);
  return(D_OKAY);
}

/*****************************************************************************
*****************************************************************************/
/* Delete the linked list for the specified field on all database records and
   free its memory.  If the specified field number is zero, all linked list
   records in the database will be deleted.  This routine does not check
   any records that were not indexed by key 0 (the last key field
   in the schema).
*/
DB_STAT ll_delete_all_lists(DBID *db, IPFCONST long fldnum)
{
  long fld;
  LNKPTR *startlnk;
  long numlls;
  long llsptrslen;
  VOIDPTR ptr;
  long ptr_offset = db->datalen + sizeof(CNTTYP);  /* Offset to first pointer */
  LNKPTR *lnode;
  LNKPTR *tnode = (LNKPTR *)db->firstnode;      /* Root node of database */
  db_setlock(db);
  if (fldnum != 0L)
  {
    /* Set up to delete one specific linked list */
    assert(fldnum <= db->nfields);      /* Verify field number */
    assert((*db->schema)[fldnum].ft_type == 'l');       /* Verify linked list */
    numlls = 1;
    llsptrslen = (*db->schema)[fldnum].ft_offset * 3;   /* Offset to list */
  }
  else
  {
    /* Set up to delete all linked lists */
    numlls = db->nlls;
    llsptrslen = 0;     /* Offset to first linked list to delete */
  }
  /* Free linked lists on each record in the data base by walking level 1
     pointers of key 0 */
  do
  {
    startlnk = tnode - llsptrslen;   /* Remember rec to check for end of list */
    /* Loop through all linked lists that need to be deleted on this db rec */
    for (fld = numlls; --fld >= 0; )
    {
      lnode = (LNKPTR *)startlnk[-1];
      /* Free each linked list record until back at db record */
      while(lnode != startlnk)
      {
        ptr = (VOIDPTR)(lnode - 3);
        lnode = (LNKPTR *)lnode[-1];
        free(ptr);
#ifdef DEBUG
        ++tot_lnkrecs_deleted;
#endif
      }
      startlnk[-1] = (LNKPTR)startlnk;  /* Set "next" ptr to indicate no list */
      startlnk[-2] = (LNKPTR)startlnk;  /* Set prior ptr to indicate no list */
      startlnk -= 3;    /* Adjust for next linked list */
    }
  } while((tnode = (LNKPTR *)*(LNKPTR *)((char *)tnode + ptr_offset)) != 0);
  db_resetlock(db);
  return(D_OKAY);
}

/*****************************************************************************
*****************************************************************************/
/* Display string of a specified length, translating all nondisplayable
   characters as two-digit hexadecimal numbers */
static void print_string(unsigned char *tptr, long count)
{
  int c;
  /* Display each character of the string using the specified length */
  while (count-- > 0L)
  {
    if (isprint(c = (int)*tptr) != 0)
      if (c == '@') printf("@@");   /* Display "@@" to indicate a regular "@" */
      else putchar(c);  /* Display displayable character */
    else printf("@%2x",c);   /* Display nondisplayable as 2-digit hex after @ */
    ++tptr;
  }
  putchar((int)'\n');   /* Go to next line after displaying string */
}

/*****************************************************************************
*****************************************************************************/
/* Display specified linked list record starting at a specified position
   in the record and using a specified length.  No error checking is done
   in this routine.
*/
void ll_show_link(IPFCONST VOIDPTR lnkrec, IPFCONST long start, IPFCONST long len)
{
  long count = len;     /* Temporary field so original field is not changed */
  LNKPTR *tnode = (LNKPTR *)*(LNKPTR *)lnkrec;
  unsigned char *tptr = (unsigned char *)tnode + start;
  /* Display linked list record address, "next" address, "prior" address, and
     flags, using long hexidecimal numbers for all of them */
  printf("lnk %lx n=%lx p=%lx f=%lx:", (long)tnode, (long)tnode[-1],
      (long)tnode[-2], (long)tnode[-3]);
  print_string(tptr, count);    /* Display specified part of rec on same line */
}

/*****************************************************************************
*****************************************************************************/
/* Display specified database record starting at a specified field and using
   a specified maximum number of fields, a maximum number of characters
   to display for char fields, and a maximum number of characters to display
   for linked list records.  Any of these maximums may be zero.  No error
   checking is done in this routine.
*/
void db_show_node(IPFCONST DBID *db, IPFCONST VOIDPTR rec, IPFCONST long fldstart,
    IPFCONST long fields, IPFCONST long maxcharlen, IPFCONST long maxlnklen)
{
  long fld = fldstart;
  long fldend = fldstart + fields;
  long count;
  LNKPTR *startlnk;
  LNKPTR *lnode;
  long offset;
  long datalen = db->datalen;   /* User data length in a record */
  long ptr_offset = datalen + sizeof(CNTTYP);   /* Offset to first pointer */
  char *tnode = (char *)*(LNKPTR *)rec;
  CNTTYP lnodelevels = *(CNTTYP *)(tnode + datalen);  /* Number of level ptrs */
  long levellen = lnodelevels * sizeof(LNKPTR);     /* Length of ptrs per key */
  /* In case this is the root node, only show used level ptrs */
  if (lnodelevels > db->max_level) lnodelevels = db->max_level;
  /* Do not go beyond the last field */
  if (fldend > db->nfields) fldend = db->nfields + 1;
  printf("record address %lx\n", (long)tnode);
  /* Display the requested fields on the database record */
  for ( ; fld < fldend; ++fld)
  {
    printf("f%ld=", fld);       /* Display field number */
    if ((*db->schema)[fld].ft_key != 'n')
    {
      /* For key fields, display the key number and the level pointers
         in hexadecimal on the same line as the field number */
      offset = (*db->schema)[fld].ft_keynum;
      lnode = (LNKPTR *)(tnode + ptr_offset + offset * levellen);
      printf("k%ld:", offset);  /* Display key number */
      for (count = 1; count <= lnodelevels; ++count)
      {
        printf("l%ld=%lx ", count, (long)*lnode);    /* Display level # & ptr */
        ++lnode;
      }
      printf("\n   ");
    }
    offset = (*db->schema)[fld].ft_offset;      /* Offset of field in record */
    switch((*db->schema)[fld].ft_type)
    {
      /* Integer field */
      case 'i': printf("integer value of %ld\n", *(long *)(tnode + offset));
                break;
      /* Linked list field */
      case 'l': count = (*db->schema)[fld].ft_len;
                if (count > maxlnklen) count = maxlnklen;      /* Stay <= max */
                startlnk = (LNKPTR *)tnode - offset * 3;       /* Ptr to list */
                lnode = (LNKPTR *)startlnk[-1];
                /* Display linked list number, linked list address on database
                   record, "next" address, "prior" address, and flags, using
                   long hexidecimal numbers
                */
                printf(
                   "link number %ld address %lx next=%lx prior=%lx flags=%lx\n",
                   offset, (long)startlnk, (long)lnode, (long)startlnk[-2],
                   (long)startlnk[-3]);
                /* Display each linked list record until back at db record */
                while (lnode != startlnk)
                {
                  printf("   ");        /* Indent linked list record info */
                  ll_show_link((VOIDPTR)&lnode, 0l, count);     /* Show rec */
                  lnode = (LNKPTR *)lnode[-1];
                }
                break;
      /* Float field */
      case 'f': printf("float value of %g\n",
                    (double)*(float *)(tnode + offset));
                break;
      /* Double field */
      case 'd': printf("double value of %g\n", *(double *)(tnode + offset));
                break;
      /* String field */
      default:  count = (*db->schema)[fld].ft_len;
                if (count > maxcharlen) count = maxcharlen;    /* Stay <= max */
                print_string((unsigned char *)(tnode + offset), count);
    }
  }
}

/*****************************************************************************
*****************************************************************************/
/* Display all database records starting at a specified field and using
   a specified maximum number of fields, a maximum number of characters
   to display for char fields, and a maximum number of characters to display
   for linked list records.  Any of these maximums may be zero.  This routine
   uses a specified key field and does not show any record that was not indexed
   by that key.  No error checking is done in this routine.
*/
void db_show_db(IPFCONST DBID *db, IPFCONST long fld, IPFCONST long fldstart,
    IPFCONST long fields, IPFCONST long maxcharlen, IPFCONST long maxlnklen)
{
  long datalen = db->datalen;   /* User data length in a record */
  long ptr_offset = datalen + sizeof(CNTTYP);   /* Offset to first pointer */
  long levellen = (*db->schema)[fld].ft_keynum * sizeof(LNKPTR);
  LNKPTR tnode = db->firstnode;         /* Root node of database */
  /* Display each record in the data base by walking level 1 pointers
     of key 0 */
  do
  {
    db_show_node(db, (VOIDPTR)&tnode, fldstart, fields, maxcharlen, maxlnklen);
    putchar('\n');      /* Put blank line between records */
  } while ((tnode = *(LNKPTR *)((char *)tnode + ptr_offset +
      (*(CNTTYP *)((char *)tnode + datalen) * levellen))) != 0);
}

/*****************************************************************************
*****************************************************************************/
#ifdef TEST

/* Include a main routine to test all non-aborting lines of all functions */

#ifdef NDEBUG
/* Turn asserts back on for the test procedure */
#undef NDEBUG
#ifdef assert
#undef assert
#endif /* assert */
#include <assert.h>
#endif /* NDEBUG */

#ifdef SMALL
/* Use a small database test for small memory models */
#define LISTSIZE 100
#define MIDSTART 31
#define MIDEND 49
#else /* !SMALL */
/* Use a large database test */
#define LISTSIZE 20000
#define MIDSTART 201
#define MIDEND 219
#endif /* !SMALL */

/* User comparison test routine */
static long my_cmp(VOIDPTR c, VOIDPTR d, long len)
{
  char *a = (char *)c;
  char *b = (char *)d;
  if (a[2] > b[2]) return(-1);
  else if (a[2] < b[2]) return(1L);
  else if (a[0] > b[0]) return(-1L);
  else if (a[0] < b[0]) return(1L);
  else return(0L);
}

SCHEMA test[] =
{
  { 'n', 'c',            199, 0 },
  { 'd', 'i',   sizeof(long), sizeof(double) },
  { 'd', 'f',  sizeof(float), sizeof(double) + sizeof(long) },
  { 'd', 'u',             20, sizeof(double) + sizeof(long) + sizeof(float) + 2,
                                  0, my_cmp },
  { 'n', 'L',              1, 0 },
  { 'u', 'd', sizeof(double), 0 },
  { 'd', 'C',              3,
                            sizeof(double) + sizeof(long) + sizeof(float) + 2 },
  { 'n', 'i',   sizeof(long), 0 },
};

main()
{
  long x, y;
  double dub1 = 1.0;
  DB_STAT stat;
  DBID db;
  struct recs
  {
    double dub;
    long x;
    float y;
    char str[200];
  } tmpx, tmpy;
  LNKPTR rec = (LNKPTR)&tmpx;
  LNKPTR uprec = (LNKPTR)&tmpy;
  LNKPTR retrec;
  LNKPTR newrec;
  LNKPTR llretrec;
  LNKPTR lltmprec;
  LNKPTR firstrec;
  SRAND((unsigned)673);
  tmpx.x = 1234;
  tmpx.y = 1;

  /* Test database record creation routines */
  db_init(&db);                 /* initialize our database structure */
  stat = db_valid_db(&db);     /* do we have a valid database? */
  assert(D_OKAY != stat);     /* we expect an error since we did not db_create*/
  stat = db_create(&test[0], 7L, 4L, &db);
  assert(D_OKAY == stat);      /* Test database creation should be successful */
  printf("create db=%x, part=%ld, nkeys=%ld, nfields=%ld, datalen=%ld,",
      db, db.partition, db.nkeys, db.nfields, db.datalen);
  printf(" max_level=%ld\n", (long)db.max_level);
  stat = db_valid_db(&db);     /* do we have a valid database? */
  assert(D_OKAY == stat);     /* we expect an D_OKAY since we did db_create*/
  /* Display test schema */
  for (x = 0; x <= db.nfields; ++x)
    printf(" x=%ld, key=%c, type=%c, len=%ld, offset=%ld\n",
        x, (*db.schema)[x].ft_key, (*db.schema)[x].ft_type,
        (*db.schema)[x].ft_len, (*db.schema)[x].ft_offset);
  assert(D_EOF == db_first(&db, &retrec, 1L));  /* First of an empty database */
  assert(D_EOF == db_search(&db, &rec, &retrec, 1L));   /* Search empty db */
  tmpx.dub = dub1++;
  assert(D_OKAY == db_insert(&db, &rec, &newrec));      /* Successful insert */
  tmpx.dub = dub1++;
  assert(D_OKAY == db_insert(&db, &rec, &newrec));   /* 2nd successful insert */
  --tmpx.dub;
  assert(D_DUPLICATE == db_update(&db, &newrec, &rec));      /* Duplicate dub */
  assert(D_DUPLICATE == db_insert(&db, &rec, &newrec));      /* Duplicate dub */
  assert(D_OKAY == db_insert_opt(&db, &rec, &newrec, "1234-6"));  /* Optional */
  assert(D_DUPLICATE == db_insert_opt(&db, &rec, &newrec, "123456"));  /* Dup */
  assert(D_OKAY == db_update(&db, &newrec, &rec));      /* Update nothing */
  assert(D_OKAY == db_delete(&db, &newrec));    /* Successful delete */
  tmpy.dub = 0.0;
/*assert(D_NOTFOUND == db_update(&db, &rec, &uprec));*/  /* Rec is not db ptr */
  assert(D_FOUND == db_first(&db, &retrec, 1L));    /* Get first record again */

  /* Test linked list routines */
  assert(D_EOF == ll_first(&db, &retrec, &llretrec, 4L));   /* 1st of no list */
  assert(D_OKAY == ll_insert(&db, &retrec, &llretrec, 4L, 'F'));  /* New list */
  assert(D_FOUND == ll_first(&db, &retrec, &llretrec, 4L));  /* First of list */
  printf("\nThis is the first link record without showing user data\n\n");
  ll_show_link(&llretrec, 0L, 0L);      /* Show linked list record */
  putchar('\n');        /* Put blank line after the record display */
  assert(D_OKAY == ll_delete(&llretrec));       /* Delete linked list record */
  assert(D_OKAY == ll_insert(&db, &retrec, &llretrec, 4L, 'F'));  /* Recreate */
  assert(D_OKAY == ll_insert(&db, &retrec, &llretrec, 4L, 'F'));   /* Another */
  assert(D_OKAY == ll_delete_list(&db, &retrec, 4L));   /* Delete linked list */
  assert(D_OKAY == ll_insert(&db, &retrec, &llretrec, 4L, 'F'));  /* Recreate */
  assert(D_OKAY == ll_insert(&db, &retrec, &llretrec, 4L, 'F'));   /* Another */
  assert(D_OKAY == ll_delete_all_lists(&db, 4L));   /* Delete list everywhere */
  assert(D_OKAY == ll_insert(&db, &retrec, &llretrec, 4L, 'F'));  /* Recreate */
  assert(D_OKAY == ll_insert(&db, &retrec, &llretrec, 4L, 'F'));   /* Another */
  assert(D_OKAY == ll_delete_all_lists(&db, 0L));  /* Delete all linked lists */
  assert(D_EOF == ll_first(&db, &retrec, &llretrec, 4L));   /* Verify no list */
  assert(D_EOF == ll_last(&db, &retrec, &llretrec, 4L));   /* Last of no list */
  assert(D_OKAY == ll_insert(&db, &retrec, &llretrec, 4L, 'F'));  /* Recreate */
  assert(D_FOUND == ll_first(&db, &retrec, &llretrec, 4L));    /* Verify list */
  assert(D_OKAY == ll_insert(&db, &retrec, &llretrec, 4L, 'F'));   /* Another */
  assert(D_FOUND == ll_first(&db, &retrec, &llretrec, 4L));   /* First of two */
  assert(D_EOF == ll_prev(&llretrec, &lltmprec));   /* No previous before 1st */
  assert(D_FOUND == ll_next(&llretrec, &llretrec));     /* Next after first */
  assert(D_FOUND == ll_prev(&llretrec, &llretrec));     /* Back to first */
  assert(D_OKAY == ll_insert(&db, &retrec, &llretrec, 4L, 'F'));   /* Add 3rd */
  assert(D_OKAY == ll_insert(&db, &retrec, &llretrec, 4L, 'F'));   /* Add 4th */
  assert(D_FOUND == ll_first(&db, &retrec, &llretrec, 4L));     /* Find first */
  assert(D_FOUND == ll_next(&llretrec, &llretrec));   /* Find second list rec */
  assert(D_FOUND == ll_next(&llretrec, &llretrec));    /* Find third list rec */
  assert(D_FOUND == ll_next(&llretrec, &llretrec));   /* Find fourth list rec */
  assert(D_EOF == ll_next(&llretrec, &lltmprec));      /* Next shows list end */
  assert(D_FOUND == ll_prev(&llretrec, &llretrec));   /* Back to 3rd list rec */
  assert(D_FOUND == ll_prev(&llretrec, &llretrec));   /* Back to 2nd list rec */
  assert(D_FOUND == ll_prev(&llretrec, &llretrec));   /* Back to 1st list rec */
  assert(D_EOF == ll_prev(&llretrec, &llretrec));   /* Previous at list start */
  assert(D_OKAY == ll_insert(&db, &retrec, &llretrec, 4L, 'L'));  /* New last */
  assert(D_OKAY == ll_insert_in(&llretrec, &llretrec, 4L, 'A'));  /* In after */
  assert(D_OKAY == ll_insert_in(&llretrec, &llretrec, 4L, 'B'));    /* Before */
  assert(D_FOUND == ll_last(&db, &retrec, &llretrec, 4L));      /* Find last */

  /* Test database key and record display routines */
  firstrec = retrec;
  assert(D_FOUND == db_keynext(&db, &firstrec, &retrec, 1L));  /* Get 2nd key */
  firstrec = retrec;
  assert(D_EOF == db_keynext(&db, &firstrec, &retrec, 1L));   /* No more keys */
  assert(D_FOUND == db_search(&db, &rec, &retrec, 1L));  /* Search for record */
  memcpy((VOIDPTR)&tmpy, (VOIDPTR)retrec, sizeof(tmpy));
  tmpy.x = -4;
  assert(D_OKAY == db_update(&db, &retrec, &uprec));  /* Update rec with "-4" */
  assert(D_FOUND == db_first(&db, &retrec, 1L));        /* "-4" is lowest key */
  assert(-4 == ((struct recs *)retrec)->x);     /* Verify "-4" is on 1st rec */
  printf("\nThis is the first record (-4) without any fields\n\n");
  db_show_node(&db, &retrec, 0L, 0L, 0L, 0L);   /* Show "-4" database record */
  printf(
     "\nThis is the first record with fields and links but no char values\n\n");
  db_show_node(&db, &retrec, 0L, 9999L, 0L, 3L);      /* Show "-4" rec fields */
  putchar('\n');        /* Put blank line after displays of "-4" database rec */
  firstrec = retrec;
  assert(D_EOF == db_keynext(&db, &firstrec, &retrec, 1L));  /* No other "-4" */
  assert(D_FOUND == db_next(&db, &firstrec, &retrec, 1L));  /* Next is "1234" */
  assert(1234 == ((struct recs *)retrec)->x);   /* Verify db record is "1234" */
  firstrec = retrec;
  assert(D_EOF == db_next(&db, &firstrec, &rec, 1L));  /* "1234" still at end */
  rec = retrec;
  assert(D_FOUND == db_search(&db, &rec, &retrec, 1L));  /* Search for "1234" */
  assert(1234 == ((struct recs *)retrec)->x);   /* Verify "1234" was found */
  rec = uprec;
  assert(D_FOUND == db_search(&db, &rec, &retrec, 1L));    /* Search for "-4" */
  assert(-4 == ((struct recs *)retrec)->x);     /* Verify "-4" rec was found */
  printf(
     "\nThis is the data base (firstnode, -4 rec, 1234 rec) before delete\n\n");
  db_show_db(&db, 1L, 0L, 9999L, 25L, 13L);     /* Show entire database */
  putchar('\n');        /* Put blank line after database display */
  assert(D_OKAY == db_delete(&db, &retrec));    /* Delete "-4" database rec */
  assert(D_NOTFOUND == db_search(&db, &rec, &retrec, 1L));  /* Verify no "-4" */

  /* Do some volume testing of the database record routines */
  tmpy.dub = dub1++;
  assert(D_OKAY == db_insert(&db, &rec, &newrec));  /* Add a second rec again */
  tmpy.dub = dub1++;
  assert(D_OKAY == db_insert(&db, &rec, &newrec));      /* Add third db rec */
  tmpy.dub = dub1++;
  assert(D_OKAY == db_insert(&db, &rec, &newrec));      /* Add fourth db rec */
  rec = (LNKPTR)&tmpx;
  /* Create number of sequential records via the first key (an integer field) */
  for (x = 0; x < LISTSIZE; ++x)
  {
    tmpx.x = x;
    tmpx.y = (float)(RAND() % 32767);
    sprintf(&tmpx.str[0], "%d          ", RAND());
    tmpx.dub = dub1++;
    assert(D_OKAY == db_insert(&db, &rec, &newrec));    /* Verify insert */
  }
  /* Search for all of the records just created */
  for (x = 0; x < LISTSIZE; ++x)
  {
    tmpx.x = x;
    assert(D_FOUND == db_search(&db, &rec, &retrec, 1L));     /* Verify found */
  }
  tmpx.x = LISTSIZE / 2;
  assert(D_FOUND == db_search(&db, &rec, &retrec, 1L));    /* Find middle rec */
  tmpx.y = 1;
  assert(D_FOUND == db_search(&db, &rec, &retrec, 2L));   /* Find key field 2 */
  /* Walk the rest of the database via the second key field (a float field) */
  while ((stat = db_next(&db, &retrec, &rec, 2L)) == D_FOUND) retrec = rec;
  stat = db_first(&db, &retrec, 3L);
  /* Walk entire database via third key field (a user defined function field) */
  for ( ; stat == D_FOUND; stat = db_next(&db, &rec, &retrec, 3L))
  {
    /* Sample casting of returned rec ptr to user specified record structure */
    struct recs *r = (struct recs *)retrec;
    rec = retrec;
  }
  rec = (LNKPTR)&tmpx;
  /* Delete 19 of the middle records in the database via first key field */
  for (x = MIDSTART; x <= MIDEND; ++x)
  {
    tmpx.x = x;
    if (db_search(&db, &rec, &retrec, 1L) == D_FOUND) db_delete(&db, &retrec);
    else printf("not found %ld\n", x);  /* The records should always be found */
  }
  tmpx.x = MIDSTART + 11;
  assert(D_NOTFOUND == db_search(&db, &rec, &retrec, 1L));   /* Verify delete */
  /* Walk the rest of of the database from the middle via the first key field */
  while ((stat = db_next(&db, &retrec, &rec, 1L)) == D_FOUND)
  {
    /* Sample casting of returned rec ptr to user specified record structure */
    struct recs *r = (struct recs *)rec;
    retrec = rec;
  }
  rec = (LNKPTR)&tmpx;
  /* Delete the sequential records in reverse order via the first key */
  for (x = LISTSIZE; x > 0; --x)
  {
    tmpx.x = x;
    if (db_search(&db, &rec, &retrec, 1L) == D_FOUND) db_delete(&db, &retrec);
    else
      printf("not found %ld (%d & %d thru %d should not be found)\n",
          x, LISTSIZE, MIDEND, MIDSTART);     /* Middle records are not there */
  }
  tmpx.x = 999999;
  assert(D_NOTFOUND == db_delete(&db, &rec));  /* Try non-existent rec delete */
  tmpx.dub = dub1++;
  assert(D_OKAY == db_insert(&db, &rec, &newrec));      /* Add record at end */
  assert(D_OKAY == ll_insert(&db, &newrec, &llretrec, 4L, 'F'));  /* Add list */
  db_destroy(&db);   /* Delete all records (0, 4 1234's, 999999, list record) */
  stat = db_valid_db(&db);     /* do we have a valid database? */
  assert(D_OKAY != stat);     /* we expect an error since we did a db_destroy */
#ifdef DEBUG
  printf("pointer distribution follows\n");
  /* Show the number of recs that were created for each number of level ptrs */
  for (x = 0;x <= MAXLEVEL; ++x) printf("%ld ", array[x]);
  printf("\n");
  printf("alloced=%ld, freed=%ld\n", tot_alloc, tot_freed);
  printf("linkrecs created = %ld, deleted = %ld\n", tot_lnkrecs_created,
      tot_lnkrecs_deleted);
  assert(tot_lnkrecs_created == tot_lnkrecs_deleted);   /* Deleted all lists */
  assert(tot_alloc == tot_freed);       /* Verify all db rec memory was freed */
#endif /* DEBUG */
  db_exit("all tests were successful\n");  /* Display success and exit with 1 */
  return(0);    /* This should not be reached since db_exit will exit */
}
#endif /* TEST */
