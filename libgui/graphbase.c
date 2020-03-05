static char sccsid[]="@(#)graphbase.c	20.2 5/16/94";
/*****************************************************************************\
*	Function: buildgraphbase.c 
*		
*	Author: Dan Clark	Dec, 1991	Modified: Dec 17, 1991
*
*	Called by: 
*
\*****************************************************************************/
#include <stdio.h>
#include <assert.h>
#include <Xm/Xm.h>
#include "dmgr.h"
#include "base_data.h"
#include "coord_data.h"
#include "graph_data.h"

int buildGraphBase(widget, tag, callback_data)
Widget		widget;
caddr_t		*tag;
caddr_t		*callback_data;
{
  PFBASERECORD keyvalrecord;	/* storage for db_search key values */
				/* used for any search */
  PFBASERECORD *keyvalrec;	/* ptr to key values in db_search  */
				/* or ptr to current record in db_next */
				/* similar to a cursor in sql */
  PFBASERECORD *pfarendrec;
/* PFBRANCH */
  PFBASERECORD *branchvalrec;	/* ptr to key for branch search */
  PFBASERECORD *nextbranchrec;	/* ptr returned for next branch rec */
/* PFBUS */
  PFBASERECORD *busvalrec;	/* ptr to key for bus search */
  PFBASERECORD *nextbusrec;	/* ptr returned for next bus rec */

  COORDFILERECORD coordkeyvalrecord;	/* storage for db_search key values */
  COORDFILERECORD *coordkeyvalrec;	/* ptr to key values in db_search  */
  BRANCHRECORD *coordbranchvalrec;	/* ptr to key for branch search */
  BRANCHRECORD *coordnextbranchrec;	/* ptr returned for next branch rec */
  BUSRECORD *coordbusvalrec;	/* ptr to key for bus search */
  BUSRECORD *coordnextbusrec;	/* ptr returned for next bus rec */

  GraphElement graphrecord;
  GraphElement *pgraphrecord = &graphrecord;
  GraphElement *pgraphbusrec;
  GraphElement *pgraphnamerec;
  GraphElement *pgraphprevbusrec;
  GraphElement *pgraphbranchrec;
  GraphElement *pgraphfarendbusrec;
  GraphElement *linkgraphrec;
  GraphLink *pgraphlink;
  DB_STAT stattmp;

  DB_STAT stat;		/* status returned for general db access */
  DB_STAT statbus;	/* status returned for bus queries of base db */
  DB_STAT statcoordbus;	/* status returned for bus queries of coord db */
  DB_STAT statbranch;	/* status returned for branch queries of db */
  DB_STAT statgraph;	/* status returned from graph db operations */
  DB_STAT stat_link;	/* status returned from graph ll operations */
  int rtncmp;

 /** make sure the base file has been loaded **/
  if( base_db.nkeys == (long)0 )  {
    printf("WARNING: build graph from base - no base file selected\n");
    return;
  }

    /** retrieve the first Bus record from the base table **/
    busvalrec = &keyvalrecord;
    strncpy(busvalrec->bus.type, "B", 1);
    strncpy(busvalrec->bus.name, "            ", 12);
    statbus = db_search(&base_db,&busvalrec, &nextbusrec, BSE_TYPE_NAME1_BASE1);
    if(statbus == D_EOF) {
      printf("WARNING: build graph from base - no valid coord data in residence \n");
      return;
    }
     
    if(statbus != D_FOUND)
    { 
#ifdef DEBUG
     printf("Note: positioned before first bus rec in base file\n");
#endif
     statbus = D_FOUND;
    }

  /** for all bus records in base file **/
  /** create a graph vertex node for each bus record **/
  /** and a create a link between each graph bus vertex node **/
  /** and the associated graph branch edge nodes **/
  while(statbus == D_FOUND)
  {
#ifdef DEBUG
    printf("basebus: %s\n", nextbusrec); 
#endif
    /* setup the graph node key values for a base bus vertex  */ 
    /*   & insert record into table */
    pgraphrecord = &graphrecord;
    /* memset (graphrecord.key, NULL, sizeof(graphrecord.key));  */
    strncpy(pgraphrecord->key.pfbus.type,"                              ", 30); 
    graphrecord.display = GraphDisplayOff;
    graphrecord.level = 0;
    graphrecord.type = GraphTypeVertex;
    graphrecord.class = GraphClassVertexBus;
    graphrecord.source = GraphSourceBase;
    graphrecord.x = 0;
    graphrecord.y = 0;
    graphrecord.wid = 0;
    strncpy(pgraphrecord->vertex_id, nextbusrec->bus.name, 
            sizeof(pgraphrecord->vertex_id)); 
    memset( pgraphrecord->farvertex_id, ' ',
              sizeof(pgraphrecord->farvertex_id) );
    strncpy(pgraphrecord->key.pfbus.type, nextbusrec->type, sizeof(PFBusKey)); 

    statgraph = db_insert_opt(&graph_db, &pgraphrecord, &pgraphbusrec, optsv);
    if(statgraph != D_OKAY) {
      printf("ERROR base record insert graph table stat %d\n",stat);
      printf("  record failed is %s\n", pgraphrecord->vertex_id);
      return(-1);
    }
    assert(statgraph == D_OKAY);
/*** now add a label for the graph vertices ***/
    pgraphrecord = &graphrecord;
    /* memset (graphrecord.key, NULL, sizeof(graphrecord.key));  */
    strncpy(pgraphrecord->key.pfbus.type,"                              ", 30); 
    graphrecord.display = GraphDisplayOff;
    graphrecord.level = 0;
    graphrecord.type = GraphTypeVertex;
    graphrecord.class = GraphClassVertexName;
    graphrecord.source = GraphSourceBase;
    graphrecord.x = 0;
    graphrecord.y = 0;
    graphrecord.wid = 0;
    memset( pgraphrecord->vertex_id, ' ',
              sizeof(pgraphrecord->vertex_id) );
    memset( pgraphrecord->farvertex_id, ' ',
              sizeof(pgraphrecord->farvertex_id) );
    strncpy(pgraphrecord->key.pfbus.type, nextbusrec->type, sizeof(PFBusKey)); 

    statgraph = db_insert_opt(&graph_db, &pgraphrecord, &pgraphnamerec, optsn);
    if(statgraph != D_OKAY) {
      printf("ERROR base record insert graph table stat %d\n",stat);
      printf("  record failed is %s\n", pgraphrecord->vertex_id);
      return(-1);
    }
    assert(statgraph == D_OKAY);
    /*** now build the link from bus vertex to label ***/
    stat = ll_insert(&graph_db, &pgraphbusrec,  &pgraphlink,
                     GRAPH_VERTEX_LINK, LL_ORDER_FIRST);
    assert(stat == D_OKAY);
    pgraphlink->graphnode = (GraphElement *)pgraphnamerec;

    /** retrieve the first branch Record for this bus **/
    branchvalrec = &keyvalrecord;
    strncpy(branchvalrec->branch.type, " ", 1);
    strncpy(branchvalrec->branch.name1, nextbusrec->bus.name , 12);
    strncpy(branchvalrec->branch.name2, "            " , 12);
    statbranch = 
        db_search(&base_db,&branchvalrec,&nextbranchrec,BSE_NAME1_BASE1);
    if(statbranch != D_FOUND)
    { 
#ifdef DEBUG
       printf("Note: no nearend branches found in table for bus\n"); 
#endif
    }

    /** loop through create branch records for bus **/
    while( statbranch == D_FOUND)
    {
#ifdef DLCnov4
      if(nextbranchrec->branch.type[0] == nextbusrec->bus.type[0])
#endif
      if((nextbranchrec->branch.type[0] == nextbusrec->bus.type[0]) ||
          (nextbranchrec->branch.type[0] == '+') ||
          (nextbranchrec->branch.type[0] == 'Q') ||
          (nextbranchrec->branch.type[0] == 'X') )

      {
#ifdef DEBUG
         printf("Note: skipping write of nearend Bus record \n"); 
#endif
      } else { 
#ifdef DEBUG
         printf("%s\n", nextbranchrec); 
#endif
         /** check for farend bus **/
         keyvalrec = &keyvalrecord;
         strncpy(keyvalrec->bus.type, "B", 1);
         strncpy(keyvalrec->bus.name, nextbranchrec->branch.name2, 12);
         stattmp =
                db_search(&base_db,&keyvalrec,&pfarendrec,BSE_TYPE_NAME1_BASE1);
         if(stattmp == D_FOUND)
         {
         rtncmp = strncmp(nextbranchrec->branch.name2, nextbusrec->bus.name, 12);
         if(rtncmp > 0) {
             /* insert new graph edge */
             pgraphrecord = &graphrecord;
             strncpy(pgraphrecord->key.pfbus.type,"                              ", 30); 
             graphrecord.display = GraphDisplayOff;
             graphrecord.level = 0;
             graphrecord.type = GraphTypeEdge;
             graphrecord.class = GraphClassEdgeSection;
             graphrecord.source = GraphSourceBase;
             graphrecord.x = 0;
             graphrecord.y = 0;
#ifdef REPLACE
             memset( pgraphrecord->vertex_id, ' ',
                sizeof(pgraphrecord->vertex_id) );
             memset( pgraphrecord->farvertex_id, ' ',
                sizeof(pgraphrecord->farvertex_id) );
#endif
             strncpy(pgraphrecord->key.pfbranch.type, nextbranchrec->type, 
               sizeof(PFBranchKey)); 
             strncpy(pgraphrecord->vertex_id, &nextbranchrec->branch.name1[0], 
               sizeof(pgraphrecord->vertex_id)); 
             strncpy(pgraphrecord->farvertex_id, &nextbranchrec->branch.name2[0], 
              sizeof(pgraphrecord->farvertex_id)); 
  
             statgraph = db_insert_opt(&graph_db, &pgraphrecord, &pgraphbranchrec, optse);
             if(statgraph != D_OKAY) {
               printf("ERROR insert graph table stat %d\n",statgraph);
               printf("  record failed is %s\n", pgraphrecord);
             }
             assert(statgraph == D_OKAY);

         } else {
           /* otherwise search for graph node (since it was already inserted) */
           pgraphrecord = &graphrecord;
           graphrecord.type = GraphTypeVertex;
           graphrecord.class = GraphClassVertexBus;
           graphrecord.source = GraphSourceBase;
           strncpy(pgraphrecord->vertex_id, nextbranchrec->branch.name2, 
            sizeof(pgraphrecord->vertex_id)); 
           stat = db_search(&graph_db, &pgraphrecord, &pgraphfarendbusrec,
                            GRAPH_TYPE_SOURCE_VERTEX_ID);
           assert(stat == D_FOUND);
             /* walk edge links until proper edge found */ 
             pgraphbranchrec = 0;
             stat_link = ll_first(&graph_db, &pgraphfarendbusrec, &pgraphlink,
                 GRAPH_EDGE_LINK);
             if (stat_link != D_FOUND) {
               printf("ERROR: no edge links\n");
             }
             while(stat_link == D_FOUND ) {
               linkgraphrec = (GraphElement *)pgraphlink->graphnode;
               rtncmp = strncmp(nextbranchrec->branch.type, 
                                  linkgraphrec->key.pfbranch.type, 
                                  sizeof(linkgraphrec->key.pfbranch));
               if(rtncmp != 0) {
#ifdef DEBUG
                   printf("Note: not this record\n");
                   printf("linkgraphrec el %32.32s\n", 
                           linkgraphrec->key.pfbranch.type);
                   printf("nextbranchrec el %32.32s\n", 
                           nextbranchrec->branch.type);
#endif
                   stat_link = ll_next(&pgraphlink, &pgraphlink);
               } else {
#ifdef DEBUG
                   printf("Note: found\n");
                   printf("linkgraphrec el %32.32s\n", 
                           linkgraphrec->key.pfbranch.type);
                   printf("nextbranchrec el %32.32s\n", 
                           nextbranchrec->branch.type);
#endif
                   pgraphbranchrec = (GraphElement *)linkgraphrec;
                   break;
               }
             } /* eliwh loop through all edge links */
             assert(pgraphbranchrec != 0);
         } /* esle fi we have used an already found bus record */
 
        /*** now build the link from bus to branch ***/
        stat = ll_insert(&graph_db, &pgraphbusrec,  &pgraphlink,
                   GRAPH_EDGE_LINK, LL_ORDER_FIRST);
        assert(stat == D_OKAY);
        pgraphlink->graphnode = (GraphElement *)pgraphbranchrec;

        /*** build the link back from branch to bus ***/
        stat = ll_insert(&graph_db, &pgraphbranchrec,  &pgraphlink,
                   GRAPH_VERTEX_LINK, LL_ORDER_FIRST);
        assert(stat == D_OKAY);
        pgraphlink->graphnode = pgraphbusrec;

         } /* verified that other bus was in coord file */
      } /* end setup branch logic */

      /* get the next branch for this bus **/
      branchvalrec = nextbranchrec;
      statbranch = 
        db_keynext(&base_db, &branchvalrec, &nextbranchrec, BSE_NAME1_BASE1);
    } /* end branch loop */

    /** retrieve the first farend branch Record for this bus **/
    branchvalrec = &keyvalrecord;
    strncpy(branchvalrec->branch.type, " ", 1);
    strncpy(branchvalrec->branch.name1, "            " , 12);
    strncpy(branchvalrec->branch.name2, nextbusrec->bus.name , 12);
    statbranch = db_search(&base_db,&branchvalrec,&nextbranchrec,BSE_NAME2_BASE2);
    if(statbranch != D_FOUND)
    { 
#ifdef DEBUG
      printf("Note: no farend branches found in table for bus\n");
#endif
    }

    while( statbranch == D_FOUND)
    {
      if((nextbranchrec->branch.type[0] == nextbusrec->bus.type[0]) ||
          (nextbranchrec->branch.type[0] == '+') ||
          (nextbranchrec->branch.type[0] == 'Q') ||
          (nextbranchrec->branch.type[0] == 'X') )
#ifdef DLCnov4
      if(nextbranchrec->branch.type[0] == nextbusrec->bus.type[0])
#endif
      {
#ifdef DEBUG
        printf("Note: skipping write of farend Bus record \n"); 
#endif
      } else {
#ifdef DEBUG
        printf("%s\n", nextbranchrec); 
#endif
         keyvalrec = &keyvalrecord;
         strncpy(keyvalrec->bus.type, "B", 1);
         strncpy(keyvalrec->bus.name, nextbranchrec->branch.name1, 12);
         stattmp =
                db_search(&base_db,&keyvalrec,&pfarendrec,BSE_TYPE_NAME1_BASE1);
         if(stattmp == D_FOUND)
         {
       rtncmp = strncmp(nextbranchrec->branch.name1, nextbusrec->bus.name, 12);
       if(rtncmp > 0) {
        pgraphrecord = &graphrecord;
    strncpy(pgraphrecord->key.pfbus.type,"                              ", 30); 
        graphrecord.display = GraphDisplayOff;
        graphrecord.level = 0;
        graphrecord.type = GraphTypeEdge;
        graphrecord.class = GraphClassEdgeSection;
        graphrecord.source = GraphSourceBase;
        graphrecord.x = 0;
        graphrecord.y = 0;
#ifdef REPLACE
    memset( pgraphrecord->vertex_id, ' ',
              sizeof(pgraphrecord->vertex_id) );
    memset( pgraphrecord->farvertex_id, ' ',
              sizeof(pgraphrecord->farvertex_id) );
#endif
        strncpy(pgraphrecord->key.pfbranch.type, nextbranchrec->type, 
                sizeof(PFBranchKey)); 
    strncpy(pgraphrecord->vertex_id, &nextbranchrec->branch.name1[0], 
            sizeof(pgraphrecord->vertex_id)); 
    strncpy(pgraphrecord->farvertex_id, &nextbranchrec->branch.name2[0], 
            sizeof(pgraphrecord->farvertex_id)); 
        statgraph = db_insert_opt(&graph_db, &pgraphrecord, &pgraphbranchrec, optse);
        if(statgraph != D_OKAY) {
          printf("ERROR insert graph table stat %d\n",statgraph);
          printf("  record failed is %s\n", pgraphrecord);
          return; /* exit(1); */
        }
        assert(statgraph == D_OKAY);
      } else {
      /* otherwise search for graph node (since it was already inserted) */
    pgraphrecord = &graphrecord;
    graphrecord.type = GraphTypeVertex;
    graphrecord.class = GraphClassVertexBus;
    graphrecord.source = GraphSourceBase;
    strncpy(pgraphrecord->vertex_id, nextbranchrec->branch.name1, 
            sizeof(pgraphrecord->vertex_id)); 
    stat = db_search(&graph_db, &pgraphrecord, &pgraphfarendbusrec,
           GRAPH_TYPE_SOURCE_VERTEX_ID);
    if(stat != D_FOUND) {
      printf("ERROR no farend bus record found %d\n",stat);
      printf("  search failed on %s\n", pgraphrecord->vertex_id);
    }
    assert(stat == D_FOUND);
    /* walk edge links until proper edge found */
    pgraphbranchrec = 0;
    stat_link = ll_first(&graph_db, &pgraphfarendbusrec, &pgraphlink,
                GRAPH_EDGE_LINK);
    if (stat_link != D_FOUND) {
      printf("ERROR: no edge links\n");
    }
    while(stat_link == D_FOUND ) {
      linkgraphrec = (GraphElement *)pgraphlink->graphnode;
      rtncmp = strncmp(nextbranchrec->branch.type, 
       linkgraphrec->key.pfbranch.type, 
       sizeof(linkgraphrec->key.pfbranch));
      if(rtncmp != 0) {
#ifdef DEBUG
        printf("Note: not this record\n");
        printf("el %32.32s\n", linkgraphrec->key.pfbranch.type);
#endif
        stat_link = ll_next(&pgraphlink, &pgraphlink);
        /* stat_link = ll_next(&graph_db, &pgraphlink, &pgraphlink, 
                    GRAPH_EDGE_LINK); */
      } else {
#ifdef DEBUG
        printf("found\n");
        printf("el %32.32s\n", linkgraphrec->key.pfbranch.type);
#endif
        pgraphbranchrec = (GraphElement *)linkgraphrec;
        break;
      }
    }
    assert(pgraphbranchrec != 0);
   }


        /*** now build the link from bus to branch ***/
        stat = ll_insert(&graph_db, &pgraphbusrec,  &pgraphlink,
                   GRAPH_EDGE_LINK, LL_ORDER_FIRST);
        assert(stat == D_OKAY);
        pgraphlink->graphnode = pgraphbranchrec;

        /*** build the link back from branch to bus ***/
        stat = ll_insert(&graph_db, &pgraphbranchrec,  &pgraphlink,
                   GRAPH_VERTEX_LINK, LL_ORDER_FIRST);
        assert(stat == D_OKAY);
        pgraphlink->graphnode = pgraphbusrec;

         } /* verified that other bus was on coord file */
      }

      /* get the next branch for this bus **/
      branchvalrec = nextbranchrec;
      statbranch = 
        db_keynext(&base_db, &branchvalrec, &nextbranchrec, BSE_NAME2_BASE2);
    } /* end branch loop */

    /** get the next base bus **/
    busvalrec = nextbusrec;
    statbus = 
       db_next(&base_db, &busvalrec, &nextbusrec, BSE_TYPE_NAME1_BASE1);
    if(D_EOF == statbus) {
      continue;
    }
    if(nextbusrec->type[0] != 'B')
    {
#ifdef DEBUG
       printf("Note: forcing end - gone past base bus records\n");
       printf("last: %s\n", nextbusrec);
#endif
       statbus = D_NOTFOUND;
    }

  } /* end bus loop */

  return(0);

}
