/* The following can be defined when compiling.
   IPFCONST - Replaces "const", if defined.  Define as nothing for older compilers.
   SLEEP - Replaces "sleep", if defined.  Define as nothing for older compilers.
*/
/* #ifndef IPFCONST */
#ifdef IPF_CONST_NO
#define IPFCONST 
#else
#define IPFCONST const 
#endif
#ifndef SLEEP
#define SLEEP(a) sleep(a)
#endif
typedef char * dmgrWORD;
typedef char * LNKPTR;
typedef void * VOIDPTR;
typedef long CNTTYP;

typedef struct schem
{
  char ft_key;
  char ft_type;
  long ft_len;
  long ft_offset;
  long ft_keynum;
  long (*ft_cmpfn)();
} SCHEMA;

typedef struct
{
  SCHEMA (*schema)[];
  long partition;
  long nkeys;
  long nlls;
  long nfields;
  long datalen;
  CNTTYP max_level;
  LNKPTR firstnode;
  long locked;
} DBID;

enum DBSTAT {D_OKAY, D_NOMEMORY, D_FOUND, D_NOTFOUND, D_DUPLICATE, D_EOF,
             D_INVALIDFIELDFORLINK, D_INVALID_DB};
typedef enum DBSTAT DB_STAT;

void db_resetlock	(DBID *db);
void db_setlock		(DBID *db);

void db_init		(DBID *db);
DB_STAT db_valid_db	(DBID *db);
DB_STAT db_destroy	(DBID *db);
DB_STAT db_delete	(DBID *db, VOIDPTR rec);
DB_STAT db_create	(IPFCONST SCHEMA *sch, IPFCONST long numfields,
			 IPFCONST long partition, DBID *db);
DB_STAT db_next		(IPFCONST DBID *db, IPFCONST VOIDPTR rec, VOIDPTR retrec,
			 IPFCONST long fldnum);
DB_STAT db_first	(IPFCONST DBID *db, VOIDPTR retrec, IPFCONST long fldnum);
DB_STAT db_keynext	(IPFCONST DBID *db, IPFCONST VOIDPTR rec, VOIDPTR retrec,
			 IPFCONST long fldnum);
DB_STAT db_search	(DBID *db, IPFCONST VOIDPTR searchrec, VOIDPTR retrec,
			 IPFCONST long fldnum);
DB_STAT db_update	(DBID *db, VOIDPTR rec, IPFCONST VOIDPTR newrec);
DB_STAT db_insert	(DBID *db, IPFCONST VOIDPTR newrec, VOIDPTR retrec);
DB_STAT db_insert_opt	(DBID *db, IPFCONST VOIDPTR newrec, VOIDPTR retrec,
			  IPFCONST char *optstring);

DB_STAT ll_insert	(DBID *db, VOIDPTR rec, VOIDPTR retlnkrec,
			  IPFCONST long fldnum, IPFCONST char order);
DB_STAT ll_insert_in	(VOIDPTR lnkrec, VOIDPTR retlnkrec, IPFCONST long lnklen,
			  IPFCONST char insert_type);
DB_STAT ll_prev		(IPFCONST VOIDPTR lnkrec, VOIDPTR retlnkrec);
DB_STAT ll_next		(IPFCONST VOIDPTR lnkrec, VOIDPTR retlnkrec);
DB_STAT ll_last		(IPFCONST DBID *db, IPFCONST VOIDPTR rec, VOIDPTR retlnkrec,
			  IPFCONST long fldnum);
DB_STAT ll_first	(IPFCONST DBID *db, IPFCONST VOIDPTR rec, VOIDPTR retlnkrec,
			  IPFCONST long fldnum);
DB_STAT ll_delete	(VOIDPTR lnkrec);
DB_STAT ll_delete_list	(DBID *db, VOIDPTR rec, IPFCONST long fldnum);
DB_STAT ll_delete_all_lists(DBID *db, IPFCONST long fldnum);

void ll_show_link	(IPFCONST VOIDPTR lnkrec, IPFCONST long start,IPFCONST long len);
void db_show_node	(IPFCONST DBID *db, IPFCONST VOIDPTR rec, IPFCONST long fldstart,
			  IPFCONST long fields, IPFCONST long maxcharlen,
			  IPFCONST long maxlnklen);
void db_show_db		(IPFCONST DBID *db, IPFCONST long fld, IPFCONST long fldstart,
			  IPFCONST long fields, IPFCONST long maxcharlen,
			  IPFCONST long maxlnklen);

#define LL_ORDER_FIRST   'f'
#define LL_ORDER_LAST    'l'
#define LL_INSERT_AFTER  'a'
#define LL_INSERT_BEFORE 'b'
