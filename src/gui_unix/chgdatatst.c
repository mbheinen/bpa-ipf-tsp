static char sccsid[]="@(#)chgdatatst.c	20.2 2/15/95";
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
main(int argc, char *argv[])
{
#include "dmgr.h"
#include "chgdata.h"

   CHGANY chg;
   CHGANY *pchg;
   CHGANY *pchgnxt;
   DBID *pchg_db;
   DB_STAT stat;
   int cnt = 0;

   chg_create();

   chg.idx = 0;
   chg.cde = CHGCDE_NONE;
   sprintf(chg.key, "%20.20s", "COORD DATA KEY");
   sprintf(chg.data, "%d", cnt); cnt++;

   printf("savePosition: CHANGE RECORD: ");
   printf("cde* %d *", chg.cde);
   printf("key* %s *", chg.key);
   printf("data* %s *", chg.data);
   printf("\n");

   chg_push(&chg); 
   sprintf(chg.data, "%d", cnt); cnt++;
   chg_push(&chg);
   sprintf(chg.data, "%d", cnt); cnt++;
   chg_push(&chg);
   sprintf(chg.data, "%d", cnt); cnt++;
   chg_push(&chg);
   sprintf(chg.data, "%d", cnt); cnt++;
   chg_push(&chg);
   
   chg_getdbid(&pchg_db);
   stat = db_first(pchg_db, &pchgnxt, CHG_IDX);
   while(stat == D_FOUND) {
     printf("idx* %d *", chg.idx);
     printf("cde* %d *", chg.cde);
     printf("key* %s *", chg.key);
     printf("data* %s *", chg.data);
     printf("\n");

     stat = db_next(pchg_db, &pchgnxt, &pchgnxt, CHG_IDX);
   };

   chg_peek(&pchg);
   printf("savePosition peek: CHANGE RECORD: ");
   printf("idx* %d *", pchg->idx);
   printf("cde* %d *", pchg->cde);
   printf("key* %s *", pchg->key);
   printf("data* %s *", pchg->data);
   printf("\n");

   chg_pop(&pchg);
   printf("savePosition peek: CHANGE RECORD: ");
   printf("idx* %d *", pchg->idx);
   printf("cde* %d *", pchg->cde);
   printf("key* %s *", pchg->key);
   printf("data* %s *", pchg->data);
   printf("\n");

   db_destroy(pchg_db);

} 
