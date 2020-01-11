static char sccsid[]="@(#)dmgrdoc.c	20.2 2/15/95";
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
#ifdef NEVER

*******************************************************************************

dmgr (3d)

     NAME

dmgr - miscellaneous notes for the dmgr data manager routines

     DESCRIPTION

The dmgr subroutines provide a rapid access method for a single record format.
The records and indexes are stored in memory, and the algorithm is derived from
"Skip Lists: A Probabilistic Alternative to Balanced Trees" by William Pugh,
printed in Communications of the ACM June 1990 Vol 33 Number 6.

The keys may overlap,
    eg. A 12 byte character key may contain a 4 byte integer key.

Definitions:
   record:      A collection of related fields which are sorted and accessed
                as a unit.
   key:         A field thru which rapid or sorted access to a record is
                desired.
   schema:      Content and organization of fields and keys in a record.


The schema is comprised of an array of the following structure.
   ft_key:      Character field which identifies the key status as follows:
                   'n' = non-key.
                   'd' = duplicates allowed.
                   'u' = no duplicates allowed (unique).
   ft_type:     Character field which identifies the key type as follows:
                   'c' = character data.
                   'i' = long integer.
                   'd' = double.
                   'f' = float.
                   'l' = linked list field; ft_key must be 'n' (non-key field).
                   'u' = field using a user comparison function.
   ft_len:      Integer specifying the length of the field in bytes.
                It must be the size of a long integer for ft_type of 'i'.
                It must be the size of a double for ft_type of 'd'.
                It must be the size of a float for ft_type of 'f'.
                For a linked list field (ft_type of 'l'), this is the default
                size of the user data on the linked list record.
   ft_offset:   Integer specifying offset from the beginning of the record
                where this field starts.
                For a linked list field (ft_type of 'l'), this is set
                by the data manager to indicate the linked list number
                associated with the field.  These numbers are set in reverse.
                Therefore, linked list 0 is the last linked list field
                in the schema.
   ft_keynum:   Integer field that is set by the data manager to indicate
                the key number associated with the field.  These numbers are
                set in reverse.  Therefore, key 0 is the last key field
                in the schema.
   ft_cmpfn:    Pointer to a user comparison function name when ft_type is 'u'.
                The function must have the following parameters:
                   my_cmp(void *a, void *b, long len)
                and must return a long integer specifying a < b, a > b, a == b
                as < 0, > 0, 0 respectively.

The first element of the array describes the length of the record.  This first
element is field 0.  It must be a non-key character field (ft_key of "n" and
ft_type of "c").  The length of the record and the lengths of linked list data
areas will be rounded up to the nearest multiple of the size of a character
pointer, since some compilers only work on word boundaries.

All other elements describe the fields or keys to the data manager.  The schema
must contain at least one key field.  The length and offset of all elements
must be within the length of the record.

The above fields are defined in the include file dmgr.h .  An example
of a schema follows:

   SCHEMA test[] =
   {
     { 'n', 'c', 40, 0 },
     { 'd', 'i', sizeof(long), 0 },
     { 'u', 'i', sizeof(long), 4 }
   };

Most dmgr routines return a DBSTAT enumerated data type as defined in dmgr.h .

All dmgr routines which expect a 'LNKPTR *' parameter will accept a pointer to
other data types (eg. struct *).

The following can be defined when compiling.
  CONST - Replaces "const", if defined.  Define as nothing for older compilers.
  SLEEP - Replaces "sleep", if defined.  Define as nothing for older compilers.

*******************************************************************************

db_create (3d)

     NAME

db_create - create a dmgr database

     SYNOPSIS

DB_STAT db_create(CONST SCHEMA *sch, CONST long numfields,
                  CONST long partition, DBID *db)

     DESCRIPTION

This function creates a database and fills the DBID structure with the required
information.  The DBID information should not be modified or used
for any purpose other than to identify the database to other dmgr routines.

     PARAMETERS

sch             Pointer to an array of SCHEMA.
numfields       Total number of fields declared in the sch array.
partition       Can be used to alter the tradeoff ratio between speed
                of application and memory requirements.  Unless you understand
                the algorithm as described in ACM, this should always be 0.
db              Pointer to a DBID structure which is used to identify
                the database to other dmgr routines.

     RETURNS

D_OKAY
D_NOMEMORY

*******************************************************************************

db_delete (3d)

     NAME

db_delete - delete a dmgr database record

     SYNOPSIS

DB_STAT db_delete(DBID *db, LNKPTR *rec)

     DESCRIPTION

This function deletes the database record pointed to by rec from all indexes,
deletes any linked lists on the record, and frees all associated memory.

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.
rec             Pointer to a previously found database record.  After deleting
                the found record, this is set to NULL.

     RETURNS

D_OKAY
D_NOTFOUND

*******************************************************************************

db_destroy (3d)

     NAME

db_destroy - erase a dmgr database

     SYNOPSIS

DB_STAT db_destroy(DBID *db)

     DESCRIPTION

This function erases a database and frees all its memory in the fastest and
most efficient fashion using key 0 (the last key field in the schema).
This function does not delete any record that was not indexed by key 0.

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.

     RETURNS

D_OKAY

*******************************************************************************

db_first (3d)

     NAME

db_first - return the first record in a dmgr database for a key

     SYNOPSIS

DB_STAT db_first(CONST DBID *db, LNKPTR *retrec, CONST long fldnum)

     DESCRIPTION

This function returns the first record in the database as sorted by a specified
field number.

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.
retrec          A pointer to the newly found record is returned in this
                parameter.  If the database is empty, this field is set to zero
                and this routine returns D_EOF.
fldnum          Field number of a key on which to do a first.

     RETURNS

D_FOUND
D_EOF

*******************************************************************************

db_insert (3d)

     NAME

db_insert - store record in a dmgr database

     SYNOPSIS

DB_STAT db_insert(DBID *db, CONST LNKPTR *newrec, LNKPTR *retrec)

     DESCRIPTION

This function stores the record in the database db.  A copy of the record
is stored in memory (as long as no unique keys would be duplicated) and
is maintained by the dmgr routines.  The appropriate indexes are updated.

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.
newrec          Pointer to a record to store.  A copy of the user data in this
                record is stored in memory.
retrec          A pointer to the newly created copy of the record is returned
                in this parameter.

     RETURNS

D_OKAY
D_DUPLICATE
D_NOMEMORY

*******************************************************************************

db_insert_opt (3d)

     NAME

db_insert_opt - store record in a dmgr database without indexing some keys

     SYNOPSIS

DB_STAT db_insert_opt(DBID *db, CONST LNKPTR *newrec, LNKPTR *retrec,
                      CONST char *optstring)

     DESCRIPTION

This function stores the record in the database db while allowing some key
fields to optionally not be indexed.  A copy of the record is stored in memory
(as long as no unique keys would be duplicated that are to be indexed) and
is maintained by the dmgr routines.  Since db_destroy and ll_delete_all_lists
use key 0 (the last key field in the schema) to go through the database,
it is recommended that the last key in the schema should always be indexed.

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.
newrec          Pointer to a record to store.  A copy of the user data in this
                record is stored in memory.
retrec          A pointer to the newly created copy of the record is returned
                in this parameter.
optstring       Pointer to a string.  A key field will not be indexed if it has
                a corresponding "-" in this string.  The first character
                in this string corresponds to field 1 (since field 0
                is the length of the record and may never be a key).
                For example, suppose the schema has 6 fields (field 0
                specifying the record length; plus 5 other fields)
                with the third and fifth fields (fields 2 and 4) being key
                fields.  The optstring "1-345" would not index field 2
                (since it has a corresponding "-") but would still index
                field 4.  The digits could actually be anything except dashes
                but using digits eliminates counting mistakes.

     RETURNS

D_OKAY
D_DUPLICATE
D_NOMEMORY

*******************************************************************************

db_keynext (3d)

     NAME

db_keynext - return next record in a dmgr database and check for duplicate key

     SYNOPSIS

DB_STAT db_keynext(CONST DBID *db, CONST LNKPTR *rec, LNKPTR *retrec,
                   CONST long fldnum)

     DESCRIPTION

This function returns the next record in the database, but only returns
a D_FOUND status if the key for the specified field number in the next record
matches the key in rec (ie, the next record has the same key value).
Otherwise it returns D_EOF.

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.
rec             Pointer to a previously found database record.
retrec          A pointer to the newly found record is returned in this
                parameter.  If rec points to the last record in the index,
                this field is set to zero.
fldnum          Field number of a key on which to do a next.

     RETURNS

D_FOUND
D_EOF

*******************************************************************************

db_next (3d)

     NAME

db_next - return next record in a dmgr database for a key

     SYNOPSIS

DB_STAT db_next(CONST DBID *db, CONST LNKPTR *rec, LNKPTR *retrec,
                CONST long fldnum)

     DESCRIPTION

This function returns the next record in the database based upon fldnum.
This is the record with the next duplicate or higher value as defined
by the compare function registered for this field in the schema.

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.
rec             Pointer to a previously found database record.
retrec          A pointer to the newly found record is returned in this
                parameter.  If rec points to the last record in the index,
                this field is set to zero and this routine returns D_EOF.
fldnum          Field number of a key on which to do a next.

     RETURNS

D_FOUND
D_EOF

*******************************************************************************

db_resetlock (3d)

     NAME

db_resetlock - reset one dmgr database lock

     SYNOPSIS

void db_resetlock(DBID *db)

     DESCRIPTION

This function resets one of the locks used by db_setlock.  (See db_setlock).

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.

     RETURNS

nothing

     SEE ALSO

db_setlock

*******************************************************************************

db_search (3d)

     NAME

db_search - search for key in a dmgr database

     SYNOPSIS

DB_STAT db_search(DBID *db, CONST LNKPTR *searchrec, LNKPTR *retrec,
                  CONST long fldnum)

     DESCRIPTION

This function searches on the specified key, thru all records in database db.
The parameter searchrec is a record that contains the key value to find
in the same offset position as the key field in the database.  This routine
returns a pointer to the first record found with that key value
(D_FOUND is returned) or to the first record with a higher key value
(D_NOTFOUND is returned).

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.
searchrec       Pointer to a record with the value to find.
retrec          A pointer to the newly found record is returned in this
                parameter.  If the key value to find is greater than all
                of the key values in the database, this field is set to zero
                and this routine returns D_EOF.
fldnum          Field number of a key on which to do a search.

     RETURNS

D_FOUND
D_NOTFOUND
D_EOF

*******************************************************************************

db_setlock (3d)

     NAME

db_setlock - wait for all locks to be reset in a dmgr database and then set one

     SYNOPSIS

void db_setlock(DBID *db)

     DESCRIPTION

This function is used to wait for all other processes to release their locks
on the database and then to place a lock on the database for the current
process.  The locking technique is primitive.  All of the database record
routines already use db_setlock and db_resetlock, but not all of the linked
list record and show routines use db_setlock and db_resetlock.  Therefore ONLY
USE these routines if a linked list routine or a show routine is critical and
ONLY USE for db_show_db, db_show_node, ll_delete, ll_first, ll_insert_in,
ll_last, ll_next, ll_prev, or ll_show_link.  The procedure to use is to call
db_setlock once, then call one of these routines, and then call db_resetlock
once.  If this procedure is not followed when using db_setlock and
db_resetlock, some process could wait forever for a lock to be released.

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.

     RETURNS

nothing

*******************************************************************************

db_show_db (3d)

     NAME

db_show_db - display dmgr database

     SYNOPSIS

void db_show_db(CONST DBID *db, CONST long fld, CONST long fldstart,
                CONST long fields, CONST long maxcharlen, CONST long maxlnklen)

     DESCRIPTION

This function displays the database by traversing a specified key field and
using db_show_node for each record.  The first record shown is the internal
first node created by db_create.  No error checking is done in this routine.

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.
fld             Field number of a key on which to traverse the database.
fldstart        Starting field number to display.  Zero would start
                at the first field.
fields          Total number of fields to display.  No fields will be displayed
                if this is zero.  Use a large number to display all fields
                starting at fldstart.
maxcharlen      Maximum length of any character field to display.  No portion
                of any character field will be displayed if this is zero.
maxlnklen       Maximum length of any linked list record user data to display.
                No portion of any linked list record user data will be
                displayed if this is zero.

     RETURNS

nothing

*******************************************************************************

db_show_node (3d)

     NAME

db_show_node - display a dmgr database record

     SYNOPSIS

void db_show_node(CONST DBID *db, CONST LNKPTR *rec, CONST long fldstart,
                  CONST long fields, CONST long maxcharlen,
                  CONST long maxlnklen)

     DESCRIPTION

This function displays the database record.  On the first line, it shows
the record address using a long hexidecimal number.  Then each of the requested
fields is displayed.  The field number is displayed followed by an "=".
If the field is a key field, the key number is then displayed, followed by each
of the level numbers and level pointers using long hexidecimal numbers.
Then the key value is displayed on the next line, indented 3 spaces.
If the field is not a key field, its value is displayed on the same line
as the field number.  If the field is a linked list field, the linked list
number, linked list address on the database record, the "next" address,
the "prior" address, and the flags are displayed using hexidecimal numbers
on the same line as the field number.  Then each record in the linked list
is displayed indented using ll_show_link, showing the linked list record user
data from the start up to the specified maximum linked list length.
If the field is a character field, it is displayed up to the specified maximum
character length.  Any character that is not printable will be shown as an "@"
followed by a 2-digit hexidecimal.  Any "@" in the user data will be shown
as "@@".  No error checking is done in this routine.

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.
rec             Pointer to a previously found database record.
fldstart        Starting field number to display.  Zero would start
                at the first field.
fields          Total number of fields to display.  No fields will be displayed
                if this is zero.  Use a large number to display all fields
                starting at fldstart.
maxcharlen      Maximum length of any character field to display.  No portion
                of any character field will be displayed if this is zero.
maxlnklen       Maximum length of any linked list record user data to display.
                No portion of any linked list record user data will be
                displayed if this is zero.

     RETURNS

nothing

*******************************************************************************

db_update (3d)

     NAME

db_update - update a dmgr database record

     SYNOPSIS

DB_STAT db_update(DBID *db, CONST LNKPTR *rec, CONST LNKPTR *newrec)

     DESCRIPTION

This function updates the database record pointed to by rec with the new
information in newrec.  The appropriate indexes are updated.  Any optional key
fields that were not previously indexed on rec will still not be indexed.
If no key field is being changed, this routine simply copies the information
from newrec to rec and returns D_OKAY, without checking if rec
is in the database.

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.
rec             Pointer to a previously found database record.
newrec          Pointer to new data.  The user data in this record will
                be placed in rec.

     RETURNS

D_OKAY
D_DUPLICATE
D_NOTFOUND

*******************************************************************************

ll_delete (3d)

     NAME

ll_delete - delete a dmgr linked list record

     SYNOPSIS

DB_STAT ll_delete(LNKPTR *lnkrec)

     DESCRIPTION

This function deletes the linked list record pointed to by lnkrec, adjusts
the linked list that contained it, and frees all associated memory.

     PARAMETERS

lnkrec          Pointer to a previously found linked list record.

     RETURNS

D_OKAY

*******************************************************************************

ll_delete_all_lists (3d)

     NAME

ll_delete_all_lists - delete a linked list from a dmgr database

     SYNOPSIS

DB_STAT ll_delete_all_lists(DBID *db, CONST long fldnum)

     DESCRIPTION

This function deletes the linked list for a specified linked list field on all
database records and frees all associated memory.  If the specified field
number is zero, all linked list records in the database are deleted.
This function does not check any records that were not indexed by key 0
(the last key field in the schema).

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.
fldnum          Field number of the linked list to delete or zero if all linked
                lists are to be deleted.

     RETURNS

D_OKAY

*******************************************************************************

ll_delete_list (3d)

     NAME

ll_delete_list - delete a linked list from a dmgr database record

     SYNOPSIS

DB_STAT ll_delete_list(DBID *db, LNKPTR *rec, CONST long fldnum)

     DESCRIPTION

This function deletes the linked list for a specified linked list field
on the specified database record and frees all associated memory.

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.
rec             Pointer to a previously found database record.
fldnum          Field number of the linked list to delete.

     RETURNS

D_OKAY

*******************************************************************************

ll_first (3d)

     NAME

ll_first - return the first linked list record from a dmgr database record

     SYNOPSIS

DB_STAT ll_first(CONST DBID *db, CONST LNKPTR *rec, LNKPTR *retlnkrec,
                 CONST long fldnum)

     DESCRIPTION

This function returns the first linked list record for a specified linked list
field on the specified database record.

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.
rec             Pointer to a previously found database record.
retlnkrec       A pointer to the newly found linked list record is returned
                in this parameter.  If the linked list is empty, this routine
                returns D_EOF and sets retlnkrec to point to a linked list
                pointer within the database record so that ll_insert_in
                can be used from retlnkrec.  DO NOT put anything
                into the record pointed to by retlnkrec in this case since that
                would corrupt pointers within the database record.
fldnum          Field number of the linked list in which to find a record.

     RETURNS

D_FOUND
D_EOF

*******************************************************************************

ll_insert (3d)

     NAME

ll_insert - create a linked list record from a dmgr database record

     SYNOPSIS

DB_STAT ll_insert(DBID *db, LNKPTR *rec, LNKPTR *retlnkrec, CONST long fldnum,
                  CONST char order)

     DESCRIPTION

This function creates a new linked list record and inserts it at one
of the linked list ends for the linked list field on the database record.
The default user data length defined in the schema for this linked list field
will be used.  User data up to this default length may be put into the returned
record.

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.
rec             Pointer to a previously found database record.
retlnkrec       A pointer to the newly created linked list record is returned
                in this parameter.
fldnum          Field number of the linked list in which to add a record.
order           Location to insert new record.  Currently supported:
                        'f' = insert as first record in the linked list.
                              (LL_ORDER_FIRST is defined as 'f' in dmgr.h)
                        'l' = insert as last record in the linked list.
                              (LL_ORDER_LAST is defined as 'l' in dmgr.h)

     RETURNS

D_OKAY
D_NOMEMORY

*******************************************************************************

ll_insert_in (3d)

     NAME

ll_insert_in - create a linked list record by another dmgr linked list record

     SYNOPSIS

DB_STAT ll_insert_in(LNKPTR *lnkrec, LNKPTR *retlnkrec, CONST long lnklen,
                     CONST char insert_type)

     DESCRIPTION

This function creates a new linked list record and inserts it before or after
a specific "linked list" record.  No checking is done to verify the specific
record is a linked list record.  This allows passing a pointer that is just
passed a set of linked list pointers within a database record instead.
Since a database record pointer is just passed the pointers for linked list 1,
it can be passed to this routine without adjustment to insert a new linked list
record at one of the ends of linked list 1.  Unless the internal structure
of a database record is understood, only use this routine with lnkrec pointing
to a linked list record for any linked list or pointing to a database record
for linked list 1.  This routine also allows varying data lengths
within a linked list.  User data up to lnklen length may be put
into the returned record.

     PARAMETERS

lnkrec          Pointer to a previously found "linked list" record.
retlnkrec       A pointer to the newly created linked list record is returned
                in this parameter.
lnklen          Length in bytes to reserve for user data in the newly created
                linked list record.
insert_type     Location to insert new record.  Currently supported:
                        'a' = insert new linked list record after specified
                              "linked list" record in the linked list.
                              (LL_INSERT_AFTER is defined as 'a' in dmgr.h)
                        'l' = insert new linked list record before specified
                              "linked list" record in the linked list.
                              (LL_INSERT_BEFORE is defined as 'b' in dmgr.h)

     RETURNS

D_OKAY
D_NOMEMORY

*******************************************************************************

ll_last (3d)

     NAME

ll_last - return the last linked list record from a dmgr database record

     SYNOPSIS

DB_STAT ll_last(CONST DBID *db, CONST LNKPTR *rec, LNKPTR *retlnkrec,
                CONST long fldnum)

     DESCRIPTION

This function returns the last linked list record for a specified linked list
field on the specified database record.

     PARAMETERS

db              Pointer to a DBID structure filled in by db_create.
rec             Pointer to a previously found database record.
retlnkrec       A pointer to the newly found linked list record is returned
                in this parameter.  If the linked list is empty, this routine
                returns D_EOF and sets retlnkrec to point to a linked list
                pointer within the database record so that ll_insert_in
                can be used from retlnkrec.  DO NOT put anything
                into the record pointed to by retlnkrec in this case since that
                would corrupt pointers within the database record.
fldnum          Field number of the linked list in which to find a record.

     RETURNS

D_FOUND
D_EOF

*******************************************************************************

ll_next (3d)

     NAME

ll_next - return the next linked list record after a dmgr linked list record

     SYNOPSIS

DB_STAT ll_next(CONST LNKPTR *lnkrec, LNKPTR *retlnkrec)

     DESCRIPTION

This function returns the linked list record attached next to a specific
"linked list" record.

     PARAMETERS

lnkrec            Pointer to a previously found "linked list" record.
retlnkrec         A pointer to the newly found linked list record is returned
                  in this parameter.  If lnkrec points to the last linked list
                  record in a linked list, this routine returns D_EOF and sets
                  retlnkrec to point to a linked list pointer within a database
                  record so that another ll_next from retlnkrec will return
                  the first linked list record within the linked list.  DO NOT
                  put anything into the record pointed to by retlnkrec
                  in this case since that would corrupt pointers
                  within the database record.

     RETURNS

D_FOUND
D_EOF

*******************************************************************************

ll_prev (3d)

     NAME

ll_prev - return previous linked list record before a dmgr linked list record

     SYNOPSIS

DB_STAT ll_prev(CONST LNKPTR *lnkrec, LNKPTR *retlnkrec)

     DESCRIPTION

This function returns the linked list record attached previous to a specific
"linked list" record.

     PARAMETERS

lnkrec          Pointer to a previously found "linked list" record.
retlnkrec       A pointer to the newly found linked list record is returned
                in this parameter.  If lnkrec points to the first linked list
                record in a linked list, this routine returns D_EOF and sets
                retlnkrec to point to a linked list pointer within a database
                record so that another ll_prev from retlnkrec will return
                the last linked list record within the linked list.  DO NOT put
                anything into the record pointed to by retlnkrec in this case
                since that would corrupt pointers within the database record.

     RETURNS

D_FOUND
D_EOF

*******************************************************************************

ll_show_link (3d)

     NAME

ll_show_link - display a dmgr linked list record

     SYNOPSIS

void ll_show_link(CONST LNKPTR *lnkrec, CONST long start, CONST long len)

     DESCRIPTION

This function displays the linked list record.  It shows the linked list record
address, the "next" address, the "prior" address, and the flags field, using
long hexidecimal numbers for all of them.  Then a ":" is displayed, followed
by a specified part of the user data.  Any character in the user data that
is not printable will be shown as an "@" followed by a 2-digit hexidecimal.
Any "@" in the user data will be shown as "@@".  No error checking is done
in this routine.

     PARAMETERS

lnkrec          Pointer to a previously found "linked list" record.
start           Offset to the portion of user data to display.  Zero would
                start at the beginning of the user data (where lnkrec points).
len             Length of the portion of user data to display.  No portion
                of user data will be displayed if this is zero.

     RETURNS

nothing

*******************************************************************************

The following is a simple example program for creating and traversing
a database.

#endif

#include <stdio.h>
#include <stdlib.h>
#include "dmgr.h"
SCHEMA sch[] =
{
  { 'n', 'c', 40, 0 },
  { 'u', 'i', sizeof(long), 0 },
  { 'd', 'i', sizeof(long), 4 },
  { 'd', 'c', 4, 8 },
  { 'd', 'c', 4, 10}
};

main()
{
  DB_STAT stat;
  DBID db;

  struct rx
  {
    long x;
    long y;
    char s1[4];
    char s2[4];
  } tempx;

  LNKPTR rec = (LNKPTR)&tempx;
  LNKPTR temprec = (LNKPTR)&tempx;
  LNKPTR retrec;

  if ((stat = db_create(&sch[0], 5L, 0L, &db)) != D_OKAY)
  {
    printf("error opening database = %d\n",stat);
    exit(1);    /* Cannot continue without database */
  }

  tempx.x = 0;
  do    /* Create lots of records */
  {
    if (tempx.x % 10) tempx.y = 27L;    /* Every 10th record has value of 27 */
    else tempx.y = 43L;                 /* The rest have value of 43 */
    if ((stat = db_insert(&db, &rec, &retrec)) != D_OKAY)
      printf("error inserting = %d\n", stat);
  } while(tempx.x++ < 100);     /* Each x must be unique according to SCHEMA */

  tempx.x = 0L;
  do    /* Find all records which were stored */
  {
    if ((stat = db_search(&db, &rec, &retrec, 1L)) != D_FOUND)
      printf("error searching = %d\n", stat);
  } while(tempx.x++ < 100);     /* Each x must be unique according to SCHEMA */

  /* Shorter code to scan all records; note use of db_first */
  for (stat = db_first(&db, &retrec, 1L); stat == D_FOUND;
      stat = db_next(&db, &rec, &retrec, 1L))
  {
    struct rx *r = (struct rx *)retrec;
    printf("%4ld", r->x);
    rec = retrec;
  }
  printf("\ndone with full scan (should have shown 0 through 100)\n");

  /* Shorter code to scan records with y of 43; note use of db_keynext */
  tempx.y = 43L;
  for (stat = db_search(&db, &temprec, &retrec, 2L); stat == D_FOUND;
      stat = db_keynext(&db, &rec, &retrec, 2L))
  {
    struct rx *r = (struct rx *)retrec;
    printf("%4ld", r->x);
    rec = retrec;
  }
  printf("\ndone with scan of 43 (should have shown 0, 10, 20, ... 100)\n");

  /* Shorter code to scan records with y of 27; note use of db_keynext */
  tempx.y = 27L;
  for (stat = db_search(&db, &temprec, &retrec, 2L); stat == D_FOUND;
      stat = db_keynext(&db, &rec, &retrec, 2L))
  {
    struct rx *r = (struct rx *)retrec;
    printf("%4ld", r->x);
    rec = retrec;
  }
  printf("\ndone with scan of 27\n");
  printf("(should have shown 1 through 99 excluding multiples of 10)\n");

  if ((stat = db_destroy(&db)) != D_OKAY)
    printf("error destroying database = %d\n", stat);
}
