static char sccsid[]="@(#)chgdata.c	20.4 2/15/95";
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
/*****
* file: chgdata.c
* author: Dan Clark	Aug 5, 1992
* purpose: track all changes made to coordinate data
*  which can be used for two purposes:
*  1) merge with original coordinate file to produce new coordinates
*  2) utilize for an UNDO function to remove any number of previous chgs
****/
#include <stdio.h>
#include <stddef.h>
#include "dmgr.h"
#include "chgdata.h"
#include "ipfdebug.h"

static CHGANY *pchgrec;    

#define LINK_DATA_SIZE sizeof(ChgLink)
#define LINK_OFFSET 0

int chg_getdbid		( DBID **retchg_db );
int chg_create		( );
int chg_push		( CHANGES *pchgnew );
int chg_pop		( CHANGES **pchgpop );
int chg_peek		( CHANGES **pchgpeek );

static SCHEMA chg_schema[] =
{
   { 'n', 'c', sizeof(CHANGES), 0}, 
   { 'u', 'i', sizeof(pchgrec->idx), offsetof(CHGANY, idx)}, /* CHG_IDX */
 /*   { 'u', 'i', sizeof(pchgrec->cde), offsetof(CHGANY, cde)}, */ /* CHG_IDX */
};
#define CHG_KEY_CNT (int)(sizeof(chg_schema)/sizeof(chg_schema[0]))
#define CHG_PARTITIONS 0
#define ERROR -1

static DBID chg_db;
static DB_STAT stat;	/* status returned from data manager calls */
static long chgcnt = 0;

int chg_getdbid(DBID **retchg_db)
{
  if(db_valid_db(&chg_db) != D_OKAY) {
    printf("ERROR: change database never created\n");
    return(ERROR);
  } else {
    *retchg_db = &chg_db;
    return(0);
  }
}

int chg_create()
{

  db_init(&chg_db);

  if(db_valid_db(&chg_db) == D_OKAY) {
    printf("ERROR - change db already created\n");
    return(ERROR);
  }

  stat = db_create((SCHEMA *)chg_schema, CHG_KEY_CNT, CHG_PARTITIONS, &chg_db);
  if(stat != D_OKAY) {
    printf("error creating chg db\n");
    return(ERROR);
  }

  return(0);
}

int chg_push(CHANGES *pchgnew)
{
  CHANGES *pchgret = 0;
  
  if(pchgnew == NULL) {
    printf("ERROR - invalid address of change record\n");
    return(ERROR);
  }
  
  pchgnew->chgany.idx = (long)chgcnt;
  stat = db_insert(&chg_db, &pchgnew, &pchgret);
  if(stat != D_OKAY) {
    printf("chg_push: ERROR - insert of new chg record failed\n");
    return(ERROR);
  }
  chgcnt++;
  if (ipfdebug & DB_TraceMask ) {
    printf("chg_push: idx %d cde %d key %s\n", 
          pchgret->chgany.idx, pchgret->chgany.cde, pchgret->chgany.key);
  }
  return(0);
}

int chg_pop(CHANGES **pchgpop)
{
  CHANGES *pchgtmp;
  stat = db_first(&chg_db, &pchgtmp, CHG_IDX);
  if(stat != D_FOUND) {
    printf("chg_pop: ERROR - find of first chg record failed\n");
    return(ERROR);
  }
  *pchgpop = pchgtmp;
  if (ipfdebug & DB_TraceMask ) {
    printf("chg_pop: idx %d cde %d key %s\n", 
          pchgtmp->chgany.idx, pchgtmp->chgany.cde, pchgtmp->chgany.key);
  }
  stat = db_delete(&chg_db, &pchgtmp);
  if(stat != D_OKAY) {
    printf("chg_pop: ERROR - delete of chg record failed\n");
    return(ERROR);
  }
  chgcnt--;
  return(0);
}

int chg_peek(CHANGES **pchgpeek)
{
  CHANGES *pchgtmp = 0;
  stat = db_first(&chg_db, &pchgtmp, CHG_IDX);
  if(stat != D_FOUND) {
    printf("chg_peek: ERROR - remove of chg record failed\n");
    return(ERROR);
  }
  if (ipfdebug & DB_TraceMask ) {
    printf("chg_peek: idx %d cde %d key %s\n", 
      pchgtmp->chgany.idx, pchgtmp->chgany.cde, pchgtmp->chgany.key);
  }
  *pchgpeek = pchgtmp;
  return(0);
}
