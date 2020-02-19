/******************************************************************************\
UTILITY:    LINEFLOW
STRUCTURE:  common CFLOW architecture.
TYPE:       Powerflow (IPF) data extraction.
SUMMARY:    Generates a table of branch quantities in requested branches for
            multiple base cases.  Supports sorting, screening, and scripting.
RELATED:    FINDOUT, MIMIC, CFUSE
SEE ALSO:   TO_DECWRITE.HELP, VMS_TO_PC_TO_EXCEL.HELP
UPDATED:    April 7, 1997
LANGUAGE:   Standard C.  CFLOW libraries.  cf_util.h
DEVELOPER:  William D. Rogers, BPA, TOP, 230-3806, wdrogers@bpa.gov
REQUESTER:  Mark S. Bond
USERS:      Kyle Kohne, Larry Stadler, Kendall Rydell, Sylvia Wiggerhaus...
IPF:        Version 318 or above recommended.
PURPOSE:    Generate a table of values showing the requested branch quantities
            for multiple base cases.  Selects lines by branch list, bus, kV,
            owner, zone, loading level, or matches to 'wild card' input.  Sorts 
            alphabetically, or by owner, zone, kV, loading (in percent), or
            according to input order of branches in a list.  Generates a control
            script that allows repetitive similar studies to be performed
            automatically.

            Reports the following quantities: loading in Amps or MVA and percent
            of critical rating; or, power in, power out, and losses in MW.

            Data fields in the output report table are character delimited to 
            ease importing to MS Excel or DECwrite.
\******************************************************************************/
/******************************* #include *************************************/
#include "cflowlib.h"
#include "cf_util.h"
/***************************** end #include ***********************************/
/******************************* #define **************************************/

#define  FF_IPF_VERSION   318            /* GPF.EXE_V318 or above recommended */
#define  FF_PAGE_LENGTH   61
#define  DOT_LINE         "..................................................."
#define  FF_WILD          '?'
#define  LOG_SPEC         "lineflow.log"
#define  OUT_NAME         "lineflow"
#define  OK               0
#define  DFLT_CASE_PER_PAGE       6             /* 6 cases fit in 132 columns */
#define  DFLT_MAX_LOAD_PCT       80
#define  QUERY_BASE (int) (1<< 0)  /* prompt for basecases */
#define  QUERY_DATA (int) (1<< 1)  /* prompt for file listing branch/bus */
#define  QUERY_LIST (int) (1<< 2)  /* prompt for list of branches/busses */
#define  QUERY_OWNR (int) (1<< 3)  /* prompt for owners of interest */
#define  QUERY_ZONE (int) (1<< 4)  /* prompt for zones of interest */
#define  QUERY_BSKV (int) (1<< 5)  /* prompt for base kV of interest */
#define  QUERY_LOAD (int) (1<< 6)  /* prompt for MAX PCT LOADING */
#define  QUERY_SORT (int) (1<< 7)  /* prompt for sort order */
#define  QUERY_FORM (int) (1<< 8)  /* prompt for report format */
#define  QUERY_NAME (int) (1<< 9)  /* prompt for report name */
#define  QUERY_TXTH (int) (1<<10)  /* prompt for checking tx thermal rating */
#define  QUERY_WDTH (int) (1<<11)  /* prompt for report width in cases/table */
#define  QUERY__LIS (int) (1<<12)  /* prompt for whether or not to make .LIS */
#define  QUERY__TRC (int) (1<<13)  /* prompt for whether or not to make .TRC */
#define  QUERY_CONT (int) (1<<14)  /* prompt for continuation of run */
#define  QUERY_TYPE (int) (1<<15)  /* prompt for branch_loading or mw_loss */
#define  QUERY_TXMW (int) (1<<16)  /* prompt for MAX_TX_MW */
#define  READ_INC        (int)    1 /* found /INCLUDE card in CFLOW data file */
#define  READ_BSE        (int)    2 /* found /BASECAS card in CFLOW data file */
#define  READ_BRN        (int)    3 /* found /BRANCH  card in CFLOW data file */
#define  READ_BUS        (int)    4 /* found /BUS     card in CFLOW data file */
#define  READ_OWN        (int)    5 /* found /OWNERS  card in CFLOW data file */
#define  READ_ZON        (int)    6 /* found /ZONE    card in CFLOW data file */
#define  READ_BKV        (int)    7 /* found /BASE_KV card in CFLOW data file */
#define  READ_LIM        (int)    8 /* found /LIMITS  card in CFLOW data file */
#define  READ_REP        (int)    9 /* found /REPORT  card in CFLOW data file */
#define  SORT_BY_ALPHA   (int)    1 
#define  SORT_BY_BASEKV  (int)    2
#define  SORT_BY_OWNER   (int)    3 
#define  SORT_BY_ZONE    (int)    4 
#define  SORT_BY_INPUT   (int)    5 
#define  SORT_BY_SOLN    (int)    6
#define  TYPE_NONE       (int)    0
#define  TYPE_TRACE      (int)    1
#define  TYPE_LOADING    (int)    1
#define  TYPE_MW_LOSS    (int)    2
#define  FORM_DATA_FULL  (int)    1 
#define  FORM_PAGE_WIDE  (int)    2
#define  CHEK_TXTH       (int)    1
#define  TRACE_NO        (int)    0
#define  TRACE_YES       (int)    1
#define  LIST_NO         (int)    0
#define  LIST_YES        (int)    1
#define  MSG_CRTCL    "\n. CRITCL RATING taken from the first non-zero case.\n"
#define  MSG_UNITS    ". Load in AMPS for lines and MVA for transformers.\n"
#define  MSG_FLAGS    ". F Rating flags indicate type of critical rating.\n"
#define  MSG_LWRCS    ". Lowercase flag indicates rating differs from listed value.\n"
#define  MSG_SYMBL    ". T Thermal, N Nominal, B Bottleneck, E Emergency\n"
#define  TRUE 1
#define  FALSE 0
/***************************** end #define ************************************/
/******************************* typedef **************************************/
typedef struct {
  float  actual;  /* MVA, AMPS, or KV, or absolute (new-ref) */ /* Pout  */
  float  percent; /* T, L, E: percent of critical rating */     /* Pin   */
  float  rating;  /* branch rating */                           /* Ploss */
  char   code;    /* T, L, E: rating code: T, N, E, or B */
} ff_soln;

typedef struct traceRecord {
  Link   *baseList;
  Link   *brchList;
  Link   *busList;
  Link   *maskList;
  Link   *ownrList;
  Link   *zoneList;
  Link   *bskvList;
  Link   *mainList;
  int     stat;                                         /* not being used yet */
  int     query;
  int     sort;
  int     type;            /* probably should be replaced by trace->rpt->type */
  int     nCases;
  float   maxLoadPct;
  int     tx_thermal;
  cf_Out *trc;
  cf_Out *rpt;
  cf_Out *lis;
  char    outName[FILENAME_MAX];
  char    timeStamp[CF_STRSIZE];
  char    userID[CF_STRSIZE];
  char    IPFversion[CF_STRSIZE];
  char    IPFexecute[CF_STRSIZE];
} Trace;
/******************************* end typedef **********************************/

/* top LINEFLOW functions - called by main() **********************************/
void  initializeTrace(Trace *trace);
void  processCommandLine(Trace *trace, int argc, char *argv[]);
void  instructions(Trace *trace);
void  checkIPFversion(Trace *trace);
void  promptUser(Trace *trace);
void  openReport(cf_Out *rpt);
void  printTrace(Trace *trace);
void  queryContinue(Trace *trace);
void  collectData(Trace *trace);
void  printReport(Trace *trace, cf_Out *rpt);
void  displayData(Trace *trace);
void  printList(Trace *trace);
void  finalRemarks(Trace *trace);
/* end LINEFLOW functions - called by main() **********************************/

/* top LINEFLOW functions - report functions **********************************/
void     printHeader(cf_Out *rpt, Link *curCase);
void     printFlowHeader(cf_Out *rpt, Link *curCase);
void     printLossHeader(cf_Out *rpt, Link *curCase);
void     printBranch(cf_Out *rpt, cf_Branch *id);
void     printSoln(cf_Out *rpt, Link *curSoln);
void     printFlow(cf_Out *rpt, Link *curSoln);
void     printLoss(cf_Out *rpt, Link *curSoln);
void     pageFooter(cf_Out *rpt, Link *nxtRow, Link *nxtCase);
void     ff_tableLegend(cf_Out *rpt);
void     ff_printBlankLines(cf_Out *rpt, int n);
int      ff_printPageFooter(cf_Out *rpt, int m);
void     printTableHeader(cf_Out *rpt, Link *curCse, char *l, char *r, char *t);
/* end LINEFLOW functions - report functions **********************************/

/* top LINEFLOW functions - support functions *********************************/
Link  *getByBranch(Trace *trace, Link *curCase);
Link  *getByBus(Trace *trace);
Link  *getByNext(Trace *trace);
void   getSolnByList(Link *curRow, int type);
int    getIndex(Link *newRow, Link *xstRow, Link *list);
int    mergeRowLists(Link **mainList, Link *newList, int nCases, Trace *trace);
void   screenZone(Trace *trace, Link **brchList);
void   screenSoln(Trace *trace);
Link  *getSoln(Link *brchLink, int type);
int    srtRow(Link *newRow, Link *rowPtr, Trace *trace);
int    srtID(Link *newID, Link *xstID, int sort);
int    srtBranch(cf_Branch *b1, cf_Branch *b2, int sort);
int    srtByInput(Link *newRow, Link *xstRow, Link *brchList);
void   sortBySoln(Trace *trace);
int    cmpLoading(Link *newRow, Link *xstRow);
int    validMask(Link *maskList, pf_rec r);
int    validBranch(Trace *trace, pf_rec r);
int    validType(char *type);
int    validLoad(float maxLoadPct, Link *idLink);
void   getZones(cf_Branch *r);
/* end LINEFLOW functions - support functions *********************************/

/* top LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/
Link  *ff_stream2List(FILE *readMe, Trace *trace, Link **expList);
void   ff_report(char *s, Trace *trace);
void   ff_limits(char *s, Trace *trace);
Link  *ff_insRow(Link **mainList, Link *rowPtr, Link *newRow, Trace *trace);
void   ff_expList(Trace *trace, Link *dataList, Link **expList);
void   ff_appSoln2Row(Link *curRow, Link *solnLink, int nCases);
/* end LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/

int main(int argc, char *argv[])
{
  Trace trace;

  time(&CF_time0);
  initializeTrace(&trace);
  processCommandLine(&trace, argc, argv);
  instructions(&trace);
  checkIPFversion(&trace);
  promptUser(&trace);
  openReport(trace.trc);
  printTrace(&trace);
  queryContinue(&trace);
  openReport(trace.rpt);
  openReport(trace.lis);
  cflow_pf_init();
  collectData(&trace);
  printReport(&trace, trace.rpt);
/*  displayData(&trace); */
  printList(&trace);
  finalRemarks(&trace);
  time(&CF_time1);
  cf_logUse("LineFlow", trace.IPFversion, trace.userID);
  return 0;
}
void initializeTrace(Trace *trace)
{
  memset(trace, '\0', sizeof(trace));
  trace->baseList         =   NULL;
  trace->brchList         =   NULL;
  trace->busList          =   NULL;
  trace->maskList         =   NULL;
  trace->ownrList         =   NULL;
  trace->zoneList         =   NULL;
  trace->bskvList         =   NULL;
  trace->mainList         =   NULL;
  trace->query        	  = ( QUERY_BASE | QUERY_DATA | QUERY_LIST |
                              QUERY_OWNR | QUERY_ZONE | QUERY_BSKV |
                              QUERY_LOAD | QUERY_SORT | QUERY_FORM |
                              QUERY_NAME | QUERY_TXTH | QUERY_WDTH |
                              QUERY__LIS | QUERY__TRC | QUERY_CONT |
                              QUERY_TYPE );
  trace->maxLoadPct = DFLT_MAX_LOAD_PCT;    /* typically default is 80.0% */
  trace->tx_thermal = !CHEK_TXTH;      /* default is to not check thermal */
  trace->sort       = SORT_BY_ALPHA;
  trace->type       = TYPE_LOADING;
  trace->nCases     = 0;
  strncpy(trace->IPFexecute, getenv("IPFSRV_CF"), CF_STRSIZE);
  cf_parse(trace->IPFexecute, trace->IPFversion, 'F');
  cuserid(trace->userID);
  cf_time(trace->timeStamp, CF_STRSIZE, CF_TIMESTAMP);
  strcpy(CF_logSpec, LOG_SPEC);
  strcpy(trace->outName, OUT_NAME);
  trace->trc = cf_initOut("", trace->timeStamp, TRACE_YES, FORM_DATA_FULL);
  trace->rpt = cf_initOut("", trace->timeStamp, TYPE_NONE, FORM_DATA_FULL);
  trace->lis = cf_initOut("", trace->timeStamp, LIST_YES,  FORM_DATA_FULL);
}
void processCommandLine(Trace *trace, int argc, char *argv[])
{
  Link *list, *topLink;
  int   i;
  topLink = NULL;
  if (argc <= 1) return;
  for (i = argc; --i > 0; cf_appList(&topLink, list)) { 
    list = cf_text2Link(argv[i]);
  }
  ff_expList(trace, topLink, &trace->baseList);
  return;
}
void instructions(Trace *trace)
{
  if (trace->query==0) return;     /* there is not going to be any queries */
  printf("\n                      Welcome to LINEFLOW - updated 04-07-97");
  printf("\n");
  printf("\n - Defaults in brackets [].  Press Ctrl-Y & type STOP to abort.");
  printf("\n - Use .trc, .dat, or .lis or append / to designate data files.");
  printf("\n - Powerflow version %d recommended.", FF_IPF_VERSION);
  printf("\n");
}
void checkIPFversion(Trace *trace)
{
  char *vp;
  int   vn;
  vp = NULL;
  vp = strstr(trace->IPFversion, ".EXE_V");
  if ( vp != NULL ) {
    vp += strlen(".EXE_V");
    sscanf(vp, "%d", &vn);
  }
  else {
    vp = 0;
    vn = 0;
  }
  if (vn != FF_IPF_VERSION) {
    printf("\n - - - - - - - - - - - -  Warning  - - - - - - - - - - - -\n");
    printf(" LINEFLOW was tested for use with IPF version %d.\n",FF_IPF_VERSION);
    printf(" You are currently using IPF version %d.\n", vn);
    printf(  " - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n");
  }
}
void promptUser(Trace *trace)     
{
  char query[CF_INBUFSIZE];
  Link *list;

  printf("%s", DOT_LINE);
  if (trace->query & QUERY_BASE) {
    printf("\n\n/BASECASE, /TRACE, or /INCLUDE");
    cf_nprompt("\n > Enter list of Basecases (and/or data files) : ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->baseList);
  }
  if (trace->baseList==NULL) {
    printf("\n\n *** Warning ***, No basecases have been specified!");
    printf("\n\n/BASECASE, /TRACE, or /INCLUDE");
    cf_nprompt("\n > Enter list of Basecases (and/or data files) : ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->baseList);
  }
  cf_exit(trace->baseList==NULL, "\n *** Quitting ***, No data or basecase files!\n");

  if (trace->query == QUERY_CONT) trace->query &= ~(QUERY_CONT);

  if (trace->query & (QUERY_NAME | QUERY_SORT | QUERY_FORM | QUERY_WDTH)) {
    printf("\n\n/REPORT");
  }
  if (trace->query & QUERY_NAME) {
    cf_sprompt("\n > Enter output name,     NAME = [%s]: ", trace->outName, trace->outName);
  }
  if (trace->query & QUERY_SORT) {
    printf("\n");
    printf("\n Specify sort Priority (by number)");
    printf("\n   1. ALPHA-base_kv   3. OWNER-alpha-base_kv    5. INPUT order");
    printf("\n   2. BASE_KV-alpha   4.  ZONE-alpha-base_kv    6. SOLUTION");
    cf_iprompt("\n > Enter sort choice,    SORT = [ALPHA]: ", trace->sort, &trace->sort);
  }
  if (trace->query & QUERY_TYPE) {
    printf("\n");
    printf("\n Specify the type of output report (by number)");
    printf("\n   1.  Line and Transformer Loading       - BRANCH_LOADING.");
    printf("\n   2.  Line and Transformer MW and Losses - BRANCH_MW_LOSS.");
    cf_iprompt("\n > Enter report choice,     TYPE = [%d]: ", trace->type, &trace->type);
  }
  if (trace->type==1) {
    trace->rpt->type = TYPE_LOADING;
  }
  if (trace->type==2) {
    trace->rpt->type = TYPE_MW_LOSS;
  }

  if ( !(trace->type & TYPE_LOADING) ) trace->query &= ~(QUERY_LOAD);
  if ( !(trace->type & TYPE_LOADING) ) trace->query &= ~(QUERY_TXTH);

  if (trace->query & QUERY_FORM) {
    printf("\n");
    printf("\n Specify output report format (by number)");
    printf("\n   1. DATA (Excel)  2. PAGE (Landscape 132 x 61) ");
    cf_iprompt("\n > Enter format choice, FORMAT = [DATA]: ", trace->trc->form, &trace->trc->form);
  }
  trace->rpt->form = trace->trc->form;
  if (trace->baseList!=NULL && trace->baseList->next==NULL) {
    trace->query &= ~(QUERY_WDTH);        /* only one basecase present */
  }
  if (trace->type==TYPE_LOADING) {
    if (trace->query & QUERY_WDTH) {   /* get default width */
        trace->trc->wide = 6;
    }
  }
  else if (trace->type==TYPE_MW_LOSS) {
    if (trace->query & QUERY_WDTH) {   /* get default width */
        trace->trc->wide = 3;
    }
  }
  if (trace->query & QUERY_WDTH) {
    printf("\n");
    printf("\n Specify the maximum number of cases per table (%d fit in 132 columns).", trace->trc->wide);
    cf_iprompt("\n > Enter width,   CASES_PER_TABLE = [%d]: ", trace->trc->wide, &trace->trc->wide);
  }
  trace->rpt->wide = trace->trc->wide;
  if (trace->query & QUERY_DATA) {
    printf("\n\n/INCLUDE");
    cf_nprompt("\n > Enter list of files with branch or bus lists: ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->query & QUERY_LIST) {
    printf("\n\n/BRANCH or /BUS");
    printf("\n > Enter a list of busses or branches: ('%c' - wild card character)", FF_WILD);
    printf("\n   > Tycown< BUS1 ><V1> < BUS2 ><V2>cs (Lowercase fields optional)");
    do {
        cf_nprompt("\n   > ", "", CF_INBUFSIZE, query);
        if (strchr(query, FF_WILD)==NULL) {
            list = cf_id2Link(query, 'L');
            if (query[0]=='B')       cf_appList(&trace->busList, list);
            else                     cf_appList(&trace->brchList, list);
        }
        else {
            list = cf_text2Link(query);
            cf_appList(&trace->maskList, list);
        }
    } while (!cf_isblank(query));
  }
  if (trace->query & (QUERY_OWNR | QUERY_ZONE | QUERY_BSKV)) {
    printf("\n\n/OWNER, /ZONE, /BASE_KV");
  }
  if (trace->query & QUERY_OWNR) {
    cf_nprompt("\n > Enter owners of interest..[all owners='%s']: ", "***", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    cf_appList(&trace->ownrList, list);
  }
  if (trace->query & QUERY_ZONE) {
    cf_nprompt("\n > Enter zones of interest.....[all zones='%s']: ", "**", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    cf_appList(&trace->zoneList, list);
  }
  if (trace->query & QUERY_BSKV) {
    cf_nprompt("\n > Enter base_kv of interest.[all base kV='%s']: ", ">0", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    cf_appList(&trace->bskvList, list);
  }
  if (trace->query & (QUERY_LOAD | QUERY_TXTH)) {
    printf("\n\n/LIMITS");
  }
/*
  if (trace->query & QUERY_TXMW) {
    cf_fprompt("\n > Enter max TX power(MW),    MAX_TX_MW = [%2.0f%%]: ", trace->maxTxMW, &trace->maxTxMW);
  }
*/
  if (trace->query & QUERY_LOAD) {
    cf_fprompt("\n > Enter max loading in %%, MAX_LOAD_PCT = [%2.0f%%]: ", trace->maxLoadPct, &trace->maxLoadPct);
  }
  if (trace->query & QUERY_TXTH) {
    printf("\n");
    printf("\n Transformer loading is checked against bottleneck and emergency ratings.");
    trace->tx_thermal =
        cf_yprompt("\n > Also check thermal? CHECK_TX_THERMAL =  [NO]: ", 'N');
  }
  printf("\n");
  cf_strsuf(trace->trc->spec, trace->outName, '.', ".trc");
  cf_strsuf(trace->rpt->spec, trace->outName, '.', ".rpt");
  cf_strsuf(trace->lis->spec, trace->outName, '.', ".lis");
  printf("\n%s\n", DOT_LINE);
  return;
}
void openReport(cf_Out *rpt)
{
  if (rpt->type==TYPE_NONE) return;
  rpt->file = cf_openFile(rpt->spec, "w");
  cf_exit(rpt->file==NULL, "Quitting!\n");
}
void printTrace(Trace *trace)
{
  FILE *fp;

  if (trace->trc->type==TRACE_NO) return;
  fp = trace->trc->file;
  if (fp == NULL) fp = stderr;
  fprintf(fp, ". %s %s %s %s\n", trace->trc->spec, trace->trc->time, trace->userID,
    trace->IPFversion);

  cf_printList(fp, trace->baseList, CF_oneStyl, "/BASECASES\n");
  fprintf(fp, "/REPORT\n");
  fprintf(fp, "  NAME = %s\n", trace->outName);
  if (trace->type     ==TYPE_LOADING  ) fprintf(fp,"  TYPE = BRANCH_LOADING\n");
  if (trace->type     ==TYPE_MW_LOSS  ) fprintf(fp,"  TYPE = BRANCH_MW_LOSS\n");
  if (trace->sort     ==SORT_BY_ALPHA ) fprintf(fp,"  SORT = ALPHA\n");
  if (trace->sort     ==SORT_BY_BASEKV) fprintf(fp,"  SORT = BASE_KV\n");
  if (trace->sort     ==SORT_BY_OWNER ) fprintf(fp,"  SORT = OWNER\n");
  if (trace->sort     ==SORT_BY_ZONE  ) fprintf(fp,"  SORT = ZONE\n");
  if (trace->sort     ==SORT_BY_INPUT ) fprintf(fp,"  SORT = INPUT\n");
  if (trace->sort     ==SORT_BY_SOLN  ) fprintf(fp,"  SORT = SOLUTION\n");
  if (trace->trc->form==FORM_DATA_FULL) fprintf(fp,"  FORMAT = DATA\n");
  if (trace->trc->form==FORM_PAGE_WIDE) fprintf(fp,"  FORMAT = PAGE\n");
  if (trace->lis->type==LIST_YES      ) fprintf(fp,"  LIST = YES\n");
  if (trace->lis->type==LIST_NO       ) fprintf(fp,"  LIST = NO\n");
  if (trace->trc->type==TRACE_YES     ) fprintf(fp,"  TRACE = YES\n");
  if (trace->trc->type==TRACE_NO      ) fprintf(fp,"  TRACE = NO\n");
  fprintf(fp, "  CASES_PER_TABLE = %d\n", trace->trc->wide);
  fprintf(fp, "/LIMITS\n");
  if (trace->type==TYPE_LOADING) {
    fprintf(fp, "  MAX_LOAD_PCT = %6.2f\n", trace->maxLoadPct);
    if ( trace->tx_thermal==CHEK_TXTH ) fprintf(fp,"  CHECK_TX_THERMAL = YES\n");
    if ( trace->tx_thermal!=CHEK_TXTH ) fprintf(fp,"  CHECK_TX_THERMAL = NO\n");
  }
  cf_printList(fp, trace->ownrList, CF_dznStyl, "/OWNER\n");
  cf_printList(fp, trace->zoneList, CF_dznStyl, "/ZONE\n");
  cf_printList(fp, trace->bskvList, CF_sixStyl, "/BASE_KV\n");
  cf_printList(fp, trace->brchList, CF_lisStyl, "/BRANCH\n.ycown< BUS1 ><V1> < BUS2 ><V2>cs\n");
  cf_printList(fp, trace->maskList, CF_lisStyl, "/BRANCH\n.ycown< BUS1 ><V1> < BUS2 ><V2>cs\n");
  cf_printList(fp, trace->busList,  CF_lisStyl, "/BUS\n.ycown< BUS  ><KV>zn\n");
  fclose(fp);
}
void queryContinue(Trace *trace)
{
  int  yes;

  if (trace->trc->type==TRACE_YES)
    printf("\nTrace written to %s", trace->trc->spec);
  if (CF_logFile != NULL)
    printf("\nError Summary written to %s", CF_logSpec);
  if ( trace->query & QUERY_CONT) {
    yes = cf_yprompt("\n > Do you want to continue this run? [%c]: ", 'Y');
    cf_exit(!yes, "Quitting!");
  }
  printf("\n");
}
void collectData(Trace *trace)
{
  Link *curCase, *brchList;
  int   nCases = 0;
  time_t time2, time1;
  time(&time1);
  curCase = trace->baseList;
  while (curCase!=NULL) {
    if (pf_load_oldbase(curCase->data)!=OK) { /* can't load */
        cf_logErr(" Cannot load this base case: %s\n", curCase->data);
        curCase = cf_delLink(&trace->baseList, curCase);
        continue;
    }
    fprintf(stdout, " Searching %s\n", curCase->data);

    brchList = getByBranch(trace, curCase);
    screenZone(trace, &brchList);
    getSolnByList(brchList, trace->type);
    mergeRowLists(&trace->mainList, brchList, nCases, trace);

    brchList = getByBus(trace);
    screenZone(trace, &brchList);
    getSolnByList(brchList, trace->type);
    mergeRowLists(&trace->mainList, brchList, nCases, trace);

    if (trace->busList==NULL && trace->brchList==NULL) {
        brchList = getByNext(trace);
        screenZone(trace, &brchList);
        getSolnByList(brchList, trace->type);
        mergeRowLists(&trace->mainList, brchList, nCases, trace);
    }

    nCases++;
    curCase = curCase->next;
  }
  screenSoln(trace);
  sortBySoln(trace);
  time(&time2);
  printf("collectData() run time: %.1f seconds\n", difftime(time2, time1));
  return;
}
void printReport(Trace *trace, cf_Out *rpt)
{
  int   i, doN, skipN, width;
  Link *curRow, *curID, *curSoln, *curCase, *casePtr;

  if (rpt->type==TYPE_NONE) return;

  skipN = 0;
  curCase = trace->baseList;
  if (curCase==NULL || trace->mainList==NULL) {
    fprintf(rpt->file, "*** Warning: No Data Found! ***");
  }
  for (casePtr=curCase;curCase!=NULL && trace->mainList!=NULL;curCase=casePtr) {
    for (doN=0; casePtr!=NULL && doN<trace->trc->wide; casePtr=casePtr->next)
        doN++;
    rpt->wide = doN;
    printHeader(rpt, curCase);
    for (curRow = trace->mainList; curRow != NULL; curRow=curRow->next) {
        if ( !(rpt->form & FORM_DATA_FULL) && rpt->line==0 ) 
            printHeader(rpt, curCase);
        curID = (Link *) curRow->data;
        curSoln = curID->next;
        for (i=skipN; i > 0 && curSoln!=NULL; curSoln = curSoln->next) i--;
        printBranch(rpt, (cf_Branch *) curID->data);
        printSoln(rpt, curSoln);
        if ( !(rpt->form & FORM_DATA_FULL) )
            pageFooter(rpt, curRow->next, casePtr);
    }
    skipN += doN;
  }
}
void printHeader(cf_Out *rpt, Link *curCase)
{
       if (rpt->type==TYPE_LOADING) printFlowHeader(rpt, curCase);
  else if (rpt->type==TYPE_MW_LOSS) printLossHeader(rpt, curCase);
  return;
}
void printSoln(cf_Out *rpt, Link *curSoln)
{
  if (rpt->type==TYPE_LOADING) {
    printFlow(rpt, curSoln);
  }
  else if (rpt->type==TYPE_MW_LOSS) {
    printLoss(rpt, curSoln);
  }
  return;
}
void displayData(Trace *trace)
{
  Link *curRow, *solnLink, *idLink;
  ff_soln *curSoln;
  char id[CF_RECSIZE];
  pf_rec r;
  for (curRow=trace->mainList; curRow!=NULL; curRow=curRow->next) {
    idLink = (Link *) curRow->data;
    cf_branch2rec(idLink->data, &r);
    pf_rec_b2a(id, &r, "I");
    fprintf(stdout, "%2.2s |%3.3s|%25.25s|", &id[0], &id[3], &id[6]);
    for (solnLink=idLink->next; solnLink!=NULL; solnLink=solnLink->next) {
        curSoln = (ff_soln *) solnLink->data;
        if (curSoln!=NULL)
            fprintf(stdout," %6.1f| %4.0f|%1.1s|", curSoln->actual, 
                curSoln->percent, &curSoln->code);
        else
            fprintf(stdout," %6.1f| %4.0f|%1.1s|", 0, 0, " ");
    }
    fprintf(stdout, "\n");
  }
  return;
}
void printList(Trace *trace)
{
  FILE *fp;

  if (trace->lis->type==TYPE_NONE) return;
  fp = trace->lis->file;
  if (fp == NULL) fp = stderr;
  fprintf(fp, ". %s %s %s %s\n", trace->lis->spec, trace->lis->time, trace->userID,
    trace->IPFversion);
  cf_printList(fp, trace->mainList, CF_lisStyl, "/BRANCH\n.ycown< BUS1 ><V1> < BUS2 ><V2>cs\n");
}
void finalRemarks(Trace *trace)
{
  printf("\n");
  printf("\nMemory allocation (bytes): Cur:%d Max:%d Alloc:%d Freed:%d\n",
    CF_memCurAlloc, CF_memMaxAlloc, CF_memTotAlloc, CF_memTotFreed);
  printf("\nOutput report written to %s", trace->rpt->spec);
  if (trace->trc->type==TRACE_YES)
    printf("\nTrace  report written to %s", trace->trc->spec);
  if (trace->lis->type==LIST_YES )
    printf("\nList   report written to %s", trace->lis->spec);
  if (CF_logFile != NULL) printf("\nError  report written to %s", CF_logSpec);
  printf("\n");
}
void printFlowHeader(cf_Out *rpt, Link *curCase)
{
  static char *HdLd[] = { /* lead */
    ".  |   |      BRANCH_LOADING     | | |  |  |    | |",
    ".  |   |                         | | |  |  |    | |",
    ".  |   |      BRANCH NAME        | | |  |  |CRTC| |",
    ".Yc|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|S|Z1|Z2|RATG|F|",
    "",
  };
  static char *HdRp[] = { /* repeat */
    "%10.10s|||",
    "[        ]|||",
    " LOAD |LOAD||",
    "ACTUAL|(%%)|F|",
    "",
  };
  printTableHeader(rpt, curCase, HdLd[0], HdRp[0], "\n");
  printTableHeader(rpt, NULL,    HdLd[1], HdRp[1], "\n");
  printTableHeader(rpt, NULL,    HdLd[2], HdRp[2], "\n");
  printTableHeader(rpt, NULL,    HdLd[3], HdRp[3], "\n");
  printTableHeader(rpt, NULL,    HdLd[4], HdRp[4], "\n");
  rpt->line += 5;
  return;
}
void printLossHeader(cf_Out *rpt, Link *curCase)
{ /* COULD USE DEFINE STATEMENTS */
  static char *HdLd[] = { /* lead */
    ".  |   |      BRANCH_MW_LOSS     | | |  |  |",
    ".  |   |                         | | |  |  |",
    ".  |   |      BRANCH NAME        | | |  |  |",
    ".Yc|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|S|Z1|Z2|",
    "",
  };
  static char *HdRp[] = { /* repeat */
    "%21.21s|||",
    "[                   ]|||",
    " P,bus1| P,bus2| Losses|",
    "  (MW) |  (MW) |  (MW) |",
    "",
  };
  printTableHeader(rpt, curCase, HdLd[0], HdRp[0], "\n");
  printTableHeader(rpt, NULL,    HdLd[1], HdRp[1], "\n");
  printTableHeader(rpt, NULL,    HdLd[2], HdRp[2], "\n");
  printTableHeader(rpt, NULL,    HdLd[3], HdRp[3], "\n");
  printTableHeader(rpt, NULL,    HdLd[4], HdRp[4], "\n");
  rpt->line += 5;
  return;
}
void printTableHeader(cf_Out *rpt, Link *curLink, char *l, char *r, char *t)
{
  int width, kind;

  fprintf(rpt->file, l);                                            /*  lead  */
  for (width = rpt->wide; width > 0; width--) {                     /* repeat */
    kind = (curLink==NULL) ? 0 : curLink->kind;
    if (kind==0) {
        fprintf(rpt->file, r);
    }
    else if (kind==CF_KIND_STR) {
        fprintf(rpt->file, r, curLink->data);                    /* case name */
    }
    if (curLink!=NULL) curLink = curLink->next;
  }
  fprintf(rpt->file, t);                                            /*  tail  */
}
void printBranch(cf_Out *rpt, cf_Branch *ID)
{
  char s[CF_RECSIZE], out[CF_STRSIZE];
  pf_rec r;
  cf_branch2rec(ID, &r);
  pf_rec_b2a(s, &r, "I");
  sprintf(out, "%-2.2s |%-3.3s|%-12.12s %-12.12s|%1.1s|%1.1s|%-2.2s|%-2.2s|",
    &s[0], &s[3], &s[6], &s[19], &s[31], &s[32], ID->bus1_zone, ID->bus2_zone);
  fprintf(rpt->file, "%s", out);
}
void printFlow(cf_Out *rpt, Link *solnLink)
{
  Link    *refLink, *curLink;
  ff_soln *refSoln, *curSoln;
  char code;
  int columns;

  columns = rpt->wide;
  refLink = solnLink;            /* find first valid data for CRITICAL RATING */
  while ( refLink!=NULL && refLink->data==NULL ) refLink = refLink->next;
  refSoln = refLink==NULL ? NULL : (ff_soln *) refLink->data;
  if (refSoln!=NULL) {
    fprintf(rpt->file, "%4.0f|%1.1s|", refSoln->rating, &refSoln->code);
  }
  else fprintf(rpt->file, "    | |");
  for (curLink = solnLink; curLink!=NULL; curLink=curLink->next) {
    if (columns-- <= 0) break;
    curSoln = (ff_soln *) curLink->data;
    if (curSoln==NULL) fprintf(rpt->file, "      |   | |");
    else {
        code = curSoln->code;
        if (refSoln!=NULL && (curSoln->rating!=refSoln->rating) ) {
            code = tolower(curSoln->code);
        }
        fprintf(rpt->file, "%6.1f|%3.0f|%1.1s|",
            curSoln->actual, curSoln->percent, &code);
    }
  }
  while (columns-- > 0) fprintf(rpt->file, "      |   | |");
  fprintf(rpt->file, "\n");
  rpt->line += 1;
  return;
}
void printLoss(cf_Out *rpt, Link *solnLink)
{
  Link    *curLink;
  ff_soln *curSoln;
  int columns;

  columns = rpt->wide;
  for (curLink = solnLink; curLink!=NULL; curLink=curLink->next) {
    if (columns-- <= 0) break;
    curSoln = (ff_soln *) curLink->data;
    if (curSoln==NULL) fprintf(rpt->file, "       |       |       |");
    else {
        fprintf(rpt->file,"%7.1f|%7.1f|%7.3f|",
            curSoln->percent, curSoln->actual, curSoln->rating);
    }
  }
  while (columns-- > 0) fprintf(rpt->file, "       |       |       |");
  fprintf(rpt->file, "\n");
  rpt->line += 1;
  return;
}
void pageFooter(cf_Out *rpt, Link *nxRow, Link *nxCase)
{
  if (nxRow!=NULL) {
    ff_printPageFooter(rpt, 3);
  }
  else if (nxRow==NULL && nxCase!=NULL) {
    ff_printPageFooter(rpt, 9);
    fprintf(rpt->file, "%s\n", DOT_LINE);
    rpt->line++;
    ff_printPageFooter(rpt, FF_PAGE_LENGTH);
  }
  else if (nxRow==NULL && nxCase==NULL) {
    ff_printPageFooter(rpt, 7);
    ff_tableLegend(rpt);
    ff_printPageFooter(rpt, FF_PAGE_LENGTH);
  }
}
void ff_tableLegend(cf_Out *rpt)
{
  fprintf(rpt->file,"%s", MSG_CRTCL);
  fprintf(rpt->file,"%s", MSG_UNITS);
  fprintf(rpt->file,"%s", MSG_FLAGS);
  fprintf(rpt->file,"%s", MSG_LWRCS);
  fprintf(rpt->file,"%s", MSG_SYMBL);
  rpt->line += 6;
  return;
}
int ff_printPageFooter(cf_Out *rpt, int m)
/* Guarantee room for m lines before page break */
{
  int n;

  n = cf_cntchr(CF_PAGE_FOOTER, '\n');
  rpt->line = Cf_imod(rpt->line, FF_PAGE_LENGTH);
  if ( (FF_PAGE_LENGTH - n - rpt->line) > m ) return 0;

  ff_printBlankLines(rpt, n);
  fprintf(rpt->file, CF_PAGE_FOOTER, rpt->time, rpt->page);
  rpt->line = Cf_imod(rpt->line + n, FF_PAGE_LENGTH);
  rpt->page++;
  return 1;
}
void ff_printBlankLines(cf_Out *rpt, int n)
{
  while (rpt->line < FF_PAGE_LENGTH - n) {
    fprintf(rpt->file, "\n");
    rpt->line++;
  }
  return;
}
void sortBySoln(Trace *trace)
{
  Link *curRow, *oldMain, *curMain, *addRow;

  if (trace->sort!=SORT_BY_SOLN) return;
  oldMain = trace->mainList;
  trace->mainList = NULL;
  curMain = NULL;
  curRow  = oldMain;
  while (curRow != NULL) {
    addRow = curRow;
    curRow = curRow->next;
    addRow->prev = NULL;
    addRow->next = NULL;
    curMain = ff_insRow(&trace->mainList, curMain, addRow, trace);
  }
}
Link *getByBranch(Trace *trace, Link *curCase)
{
  Link *brchList, *curRow, *curBrch, *idLink, *newRow;
  cf_Branch *brID, *nwID;
  pf_rec r;
  int e;

/* ************* added stuff ***************** */
  int found_flg;
/* ************* added stuff ***************** */

  brchList = NULL;
  curRow = brchList;

   for (curBrch=trace->brchList; curBrch!=NULL; curBrch=curBrch->next) {
   brID = (cf_Branch *) curBrch->data;

/* ************* added stuff ***************** */
    found_flg = FALSE;
/* ************* added stuff ***************** */

    cf_branch2rec(brID, &r);

    for ( e=pf_rec_branch(&r, "G"); e==OK; e=pf_rec_branch(&r, "N2") ) {
/*      cf_rec_l2h(&r.i.branch); */

        if ( !validBranch(trace, r) ) break;

        idLink = cf_newLink(sizeof(cf_Branch));
        if ( idLink==NULL || idLink->data==NULL ) continue;
        idLink->kind = CF_KIND_BRN;
        nwID = (cf_Branch *) idLink->data;
        cf_rec2branch(&r, nwID);
        nwID->rating = cf_getRating(&r, trace->tx_thermal, &nwID->code);
        newRow = cf_link2row(idLink);
        curRow = ff_insRow(&brchList, curRow, newRow, trace);

/* ************* added stuff ***************** */
    found_flg = TRUE;
/* ************* added stuff ***************** */

        if ( isdigit(brID->ckt_id) ) break; /* if specif'd, get only this ckt */
        if ( brID->section!=0 ) break; /* if specified, get only this section */

    }

/* ************* added stuff ***************** */
  if (found_flg == FALSE)
  	cf_logErr("  %3s|%9s|%7.2f|%9s|%7.2f|%1s|%1d is missing in case %s \n",\
 &brID->type,&brID->bus1_name,brID->bus1_kv, &brID->bus2_name,brID->bus2_kv\
 ,&brID->ckt_id,brID->section,curCase->data);
/* ************* added stuff ***************** */

  }
  return brchList;
}
Link *getByBus(Trace *trace)
{
  Link *byBusList, *curRow, *curBus, *idLink, *newRow;
  cf_Bus *busID;
  cf_Branch *nwID;
  pf_rec r;
  int e;

  byBusList = NULL;
  curRow = byBusList;
  for (curBus=trace->busList; curBus!=NULL; curBus=curBus->next) {
    busID = (cf_Bus *) curBus->data;
    pf_init_branch(&r, "L*", busID->name, busID->kv, "", 0, ' ', 0);
    for ( e=pf_rec_branch(&r, "F1"); e==OK; e=pf_rec_branch(&r, "N1") ) {
        cf_rec_l2h(&r.i.branch);
        if ( !validBranch(trace, r) ) continue;
        idLink = cf_newLink(sizeof(cf_Branch));
        if ( idLink==NULL || idLink->data==NULL ) continue;
        idLink->kind = CF_KIND_BRN;
        nwID = (cf_Branch *) idLink->data;
        cf_rec2branch(&r, nwID);
        nwID->rating = cf_getRating(&r, trace->tx_thermal, &nwID->code);
        newRow = cf_link2row(idLink);
        curRow = ff_insRow(&byBusList, curRow, newRow, trace);
    }
  }
  return byBusList;
}
Link *getByNext(Trace *trace)
{ /* skip this if other 'getBy...' has been performed. */
  Link *byNextList, *curRow, *idLink, *newRow;
  cf_Branch *nwID;
  pf_rec r;
  int e;

  byNextList = NULL;
  curRow = byNextList; 
  for ( e=pf_rec_branch(&r, "F"); e==OK; e=pf_rec_branch(&r, "N") ) {
    cf_rec_l2h(&r.i.branch);
    if ( !validBranch(trace, r) ) continue;
    idLink = cf_newLink(sizeof(cf_Branch));
    if ( idLink==NULL || idLink->data==NULL ) continue;
    idLink->kind = CF_KIND_BRN;
    nwID = (cf_Branch *) idLink->data;
    cf_rec2branch(&r, nwID);
    nwID->rating = cf_getRating(&r, trace->tx_thermal, &nwID->code);
    newRow = cf_link2row(idLink);
    curRow = ff_insRow(&byNextList, curRow, newRow, trace); 
  }
  return byNextList;
}
int validBranch(Trace *trace, pf_rec r)
{ /* return 1 if valid, 0 if not valid */
  if ( !validType(r.i.branch.type) ) return 0;
  if ( !cf_validOwner(trace->ownrList, r.i.branch.owner) ) return 0;
  if ( !cf_validBasekv(trace->bskvList, 
        r.i.branch.bus1_kv, r.i.branch.bus2_kv) ) return 0;
  if ( !validMask(trace->maskList, r) ) return 0;
  return 1; /* otherwise, assume to be valid */
}
void screenZone(Trace *trace, Link **brchList)
{
  Link *curRow, *idLink;
  cf_Branch *idData;
  curRow = *brchList;

  while (curRow != NULL) {
    idLink = (Link *) curRow->data;
    idData = (cf_Branch *) idLink->data;
    getZones(idData);
    if (!cf_validZone(trace->zoneList, idData->bus1_zone, idData->bus2_zone)) {
        curRow = cf_delRow(brchList, curRow);
    }
    else {
        curRow = curRow->next;
    }
  }
}
void getSolnByList(Link *curRow, int chek)
{
  while (curRow != NULL) {
    getSoln((Link *) curRow->data, chek);
    curRow = curRow->next;
  }
}
void screenSoln(Trace *trace)
{
  Link *curRow;

  curRow = trace->mainList;
  while (curRow != NULL) {
    if (trace->type & TYPE_LOADING) {
        if ( !validLoad(trace->maxLoadPct, curRow->data) ) {
            curRow = cf_delRow(&trace->mainList, curRow);
            continue;
        }
    }
    curRow = curRow->next;
  }
}
int validLoad(float maxLoadPct, Link *idLink)
{ /* if no solution is found in any case, branch will not be reported */
  Link *solnLink;
  ff_soln *solnData;

  solnLink = (Link *) idLink->next;
  while (solnLink!=NULL) {
    solnData = (ff_soln *) solnLink->data;
    if ( (solnData != NULL) && (solnData->percent > maxLoadPct) ) return 1;
    solnLink = (Link *) solnLink->next;
  }
  return 0;
}
int mergeRowLists(Link **mainList, Link *newList, int nCases, Trace *trace)
{
  Link *curRow, *newPair, *newRow, *idLink, *solnLink;
  curRow = *mainList;
  for (newPair=newList; newPair!=NULL; ) {
    newRow = newPair;
    newPair = newPair->next;
    newRow->next = NULL;
    newRow->prev = NULL;
    if (newPair!=NULL) newPair->prev = NULL;
    idLink = (Link *) newRow->data;
    solnLink = idLink->next;
    idLink->next = NULL;
    if (idLink==NULL || solnLink==NULL) continue;
    curRow = ff_insRow(mainList, curRow, newRow, trace);
    ff_appSoln2Row(curRow, solnLink, nCases);
  }
  return 0;
}
int validMask(Link *maskList, pf_rec r)
{
  Link *maskLink;
  char *maskData, netData[CF_RECSIZE], netVal[34], mskVal[34];
  int   n;

  if (maskList==NULL || maskList->data==NULL) return 1;
  for (maskLink=maskList; maskLink!=NULL; maskLink=maskLink->next) {
    maskData = (char *) maskLink->data;
    pf_rec_b2a(netData, &r, "I");
    sprintf(mskVal, "%-33.33s", maskData);
    sprintf(netVal, "%-33.33s", netData);
    for (n=0; mskVal[n]!='\0'; n++) {
        if (mskVal[n] == '?') netVal[n] = '?';
        if (mskVal[n] == '*') netVal[n] = '*';
        if (mskVal[n] == ' ') netVal[n] = ' ';
        if (mskVal[n] == '0') netVal[n] = '0';
    }
    if (strncmp(mskVal, netVal, 33)==0) return 1;                     /* keep */
  }
  return 0;
}
int validType(char *type)
{ /* return 1 if type is valid, else 0; hard coded to reject R cards */
  if (type[0]=='R') return 0;
  else return 1;
}
void  getZones(cf_Branch *r)
{
  pf_rec b;
  if (r->bus1_zone[0] == '\0') {
    pf_init_bus(&b, "B*", r->bus1_name, r->bus1_kv);
    if (pf_rec_bus(&b, "G")==OK)
        strncpy(r->bus1_zone, b.i.ACbus.zone, 2);
  }
  if (r->bus2_zone[0] == '\0') {
    pf_init_bus(&b, "B*", r->bus2_name, r->bus2_kv);
    if (pf_rec_bus(&b, "G")==OK)
        strncpy(r->bus2_zone, b.i.ACbus.zone, 2);
  }
}
Link *getSoln(Link *brchLink, int type)
{
  pf_rec r;
  Link *solnLink;
  ff_soln *soln;
  cf_Branch *id;
  cf_branch2rec(brchLink->data, &r);
  if (pf_rec_branch(&r, "O")==OK) {
    solnLink = cf_newLink(sizeof(ff_soln));
    soln = (ff_soln *) solnLink->data;
    id = (cf_Branch *) brchLink->data;
    if (type & (TYPE_LOADING)) {
        if (id->type[0]=='T') {
            soln->actual = r.s.branch.tot_xfmr_load_mva;
        }
        else {
            soln->actual = r.s.branch.tot_line_load_amps;
        }
        soln->rating = id->rating; /* rating should be part of the soln */
        soln->code    = id->code;   /* copy rating, code to soln from id */
        soln->percent = (soln->rating>0) ? (100* soln->actual/soln->rating) : 0;
    }
    else if (type & (TYPE_MW_LOSS)) {
        soln->percent = r.s.branch.Pin;   /* MW, flow at bus1 (high kV) */
        soln->actual  = r.s.branch.Pout;  /* MW, flow at bus2 (low kV)  */
        soln->rating  = r.s.branch.Ploss; /* Loss in MW */
    }
  }
  else
    solnLink = cf_newLink(0);
  brchLink->next = solnLink;
  solnLink->prev = brchLink;
  return solnLink;
}
void ff_appSoln2Row(Link *curRow, Link *solnLink, int skip)
{
  Link *outgLink, *linkPtr;
  linkPtr  = (Link *) curRow->data;
  while (skip-- > 0) {
    if (linkPtr->next==NULL) {
        linkPtr->next = cf_newLink(0);
        if (linkPtr->next!=NULL) linkPtr->next->prev=linkPtr;
    }
    linkPtr = linkPtr->next;
  }
  linkPtr->next = solnLink;
  solnLink->prev = linkPtr;
}
Link *ff_insRow(Link **table, Link *rowPtr, Link *newRow, Trace *trace)
{
  int cmpPtr;

  while (1) {
    if (rowPtr==NULL) {
        *table = newRow;
        break;
    }
    cmpPtr = srtRow(newRow, rowPtr, trace);
    if (cmpPtr < 0) {
        if (rowPtr->prev == NULL) {                             /* insert row */
            rowPtr->prev = newRow;
            *table = newRow;
            newRow->next = rowPtr;
            break;
        }
        else {
            rowPtr = rowPtr->prev;
            continue;
        }
    }
    if (cmpPtr==0) {/* shouldn't be same as last if called by buildReportTable*/
        cf_freeRow(newRow);
        newRow = rowPtr;
        break;
    }
    if (cmpPtr > 0) {
        if (rowPtr->next==NULL) {                               /* insert row */
            rowPtr->next = newRow;
            newRow->prev = rowPtr;
            break;
        }
        else if (srtRow(newRow, rowPtr->next, trace) < 0) {
            newRow->next = rowPtr->next;                     /* insert newRow */
            rowPtr->next = newRow;
            newRow->prev = rowPtr;
            newRow->next->prev = newRow;
            break;
        }
        else rowPtr = rowPtr->next;
    }
  }
  return newRow;
}
int srtRow(Link *newRow, Link *xstRow, Trace *trace)
{
  if (trace->sort==SORT_BY_INPUT)
    return srtByInput(newRow, xstRow, trace->brchList);
  else if (trace->sort==SORT_BY_SOLN)
    return cmpLoading(newRow, xstRow);
  else
    return srtID(newRow, xstRow, trace->sort);
}
int cmpLoading(Link *newRow, Link *xstRow)
{ /* compares only the loading in the first case */
  Link *newIDLink, *xstIDLink;
  ff_soln *newSoln, *xstSoln;
  cf_Branch *newID, *xstID;

  newIDLink   = (Link *)      newRow->data;
  xstIDLink   = (Link *)      xstRow->data;
  newID       = (cf_Branch *) newIDLink->data;
  xstID       = (cf_Branch *) xstIDLink->data;
  if ( newID->type[0] < xstID->type[0] ) return  1;            /* [type]-sort */
  if ( newID->type[0] > xstID->type[0] ) return -1;            /* [type]-sort */
  if (newIDLink->next==NULL || xstIDLink->next==NULL) return -1;
  newSoln     = (ff_soln *)   newIDLink->next->data;
  xstSoln     = (ff_soln *)   xstIDLink->next->data;
  if (newSoln==NULL || xstSoln==NULL) return 1;
  if (newSoln->percent < xstSoln->percent) return  1;
  if (newSoln->percent > xstSoln->percent) return -1;
  return 0;
}
int srtByInput(Link *newRow, Link *xstRow, Link *brchList)
{
  return getIndex(newRow->data, xstRow->data, brchList);
}
int getIndex(Link *newData, Link *xstData, Link *list)
/* determine which branch comes first in the input list */
/* uses start-at-beginning-and-search-whole-list approach; could be improved */
{
  if (list==NULL) return cf_cmpBranch(newData->data, xstData->data);
  if (cf_cmpBranch(newData->data, xstData->data)==0) return 0;/*newRow==xstRow*/
  for (; list!=NULL; list=list->next) {
    if (cf_cmpBranch(list->data, newData->data)==0) return -1;
    if (cf_cmpBranch(list->data, xstData->data)==0) return  1;
  }
  return 1;                                           /* default return value */
}
int srtID(Link *newID, Link *xstID, int sort)
{
  Link *newData, *xstData;
  if (newID==NULL || xstID==NULL) return 1;
  newData = (Link *) newID->data;
  xstData = (Link *) xstID->data;
  if (newData==NULL || xstData==NULL) return -1;
  if (newData->data==NULL || xstData->data==NULL) return -1;
  return srtBranch(newData->data, xstData->data, sort);
}
int srtBranch(cf_Branch *b1, cf_Branch *b2, int sort) /* sort branch */
{
  int c;
  float d;
  if ( (c=strcmp(b1->type, b2->type)) != 0 ) return c;         /* [type]-sort */
  if (sort==SORT_BY_ZONE) {                                /* [zone]-alpha-kv */
    if ( (c=strcmp(b1->bus1_zone, b2->bus1_zone)) != 0 ) return c;
    if ( (c=strcmp(b1->bus2_zone, b2->bus2_zone)) != 0 ) return c;
  }
  else if (sort==SORT_BY_OWNER) {                         /* [owner]-alpha-kv */
    if ( (c=strcmp(b1->owner, b2->owner)) != 0 ) return c;
  }
  else if (sort==SORT_BY_BASEKV) {                           /* [kv]-alpha-kv */
    if ( fabs(d = b1->bus1_kv - b2->bus1_kv) > .001 ) return d;
    if ( fabs(d = b1->bus2_kv - b2->bus2_kv) > .001 ) return d;
  }
  return cf_cmpBranch((pf_branch *) b1, (pf_branch *) b2);         /* [alpha-kv] */
}
void ff_expList(Trace *trace, Link *dataList, Link **expList)
{ /* expand double-linked list */
  Link *curLink, *newList;
  FILE *readMe;

  curLink = dataList;
  while (curLink != NULL) {
    if ( cf_isDataFile(curLink->data) ){/*DAT, LIS, or TRC to be opened & read*/
        readMe = cf_openFile(curLink->data, "r");/* it's okay if readMe==NULL */
        ff_stream2List(readMe, trace, expList);
        fclose(readMe);
    }
    else if (expList!=NULL) { /* expansion list */
        newList = cf_text2List(curLink->data);
        cf_appList(expList, newList);
    }
    curLink = cf_delLink(&dataList, curLink);
  }
}
Link *ff_stream2List(FILE *readMe, Trace *trace, Link **expList)
{
  char str[CF_INBUFSIZE], STR[CF_INBUFSIZE];
  Link *list;
  int mode = READ_INC; /* default /INCLUDE */
  while (fgets(str, CF_INBUFSIZE, readMe)!=NULL) { 
    if ( cf_iscomment(str) ) continue;
    strcpy(STR, str);
    cf_str2upper(STR);
    if ( strstr(STR, "/INCLUDE" )!=NULL ) { mode = READ_INC; continue; }
    if ( strstr(STR, "/BASECASE")!=NULL ) { mode = READ_BSE; continue; }
    if ( strstr(STR, "/BRANCH"  )!=NULL ) { mode = READ_BRN; continue; }
    if ( strstr(STR, "/BUS"     )!=NULL ) { mode = READ_BUS; continue; }
    if ( strstr(STR, "/OWNER"   )!=NULL ) { mode = READ_OWN; continue; }
    if ( strstr(STR, "/ZONE"    )!=NULL ) { mode = READ_ZON; continue; }
    if ( strstr(STR, "/BASE_KV" )!=NULL ) { mode = READ_BKV; continue; }
    if ( strstr(STR, "/LIMITS"  )!=NULL ) { mode = READ_LIM; continue; }
    if ( strstr(STR, "/REPORT"  )!=NULL ) { mode = READ_REP; continue; }
    if ( STR[0]=='/' ) { mode = 0; continue; }     /* unrecognized slash card */
    if ( mode == 0 ) continue;         /* note: mode is defaulted to READ_INC */
    if ( mode == READ_INC ) {
        list = cf_text2List(str);
        ff_expList(trace, list, expList);
    }
    if ( mode == READ_BSE ) {
        list = cf_text2List(str);
        if (list!=NULL) trace->query &= ~(QUERY_BASE);
        cf_appList(&trace->baseList, list);
    }
    if ( mode == READ_BRN ) {
        if (strchr(str, FF_WILD)==NULL) {
            list = cf_id2Link(str, 'I');                  /* changed from 'L' */
            cf_appList(&trace->brchList, list);
        }
        else {
            list = cf_text2Link(str);
            cf_appList(&trace->maskList, list);
        }
        if (list!=NULL) trace->query &= ~(QUERY_DATA);
        if (list!=NULL) trace->query &= ~(QUERY_LIST);
        if (list!=NULL) trace->query &= ~(QUERY_ZONE);
        if (list!=NULL) trace->query &= ~(QUERY_OWNR);
        if (list!=NULL) trace->query &= ~(QUERY_BSKV);
    }
    if ( mode == READ_BUS ) {
        if (strchr(str, FF_WILD)==NULL) {
            list = cf_id2Link(str, 'I');
            cf_appList(&trace->busList, list);
        }
        else {
            list = cf_text2Link(str);
            cf_appList(&trace->maskList, list);
        }
        if (list!=NULL) trace->query &= ~(QUERY_DATA);
        if (list!=NULL) trace->query &= ~(QUERY_LIST);
        if (list!=NULL) trace->query &= ~(QUERY_ZONE);
        if (list!=NULL) trace->query &= ~(QUERY_OWNR);
        if (list!=NULL) trace->query &= ~(QUERY_BSKV);
    }
    if ( mode == READ_OWN ) {
        list = cf_text2List(str);
        if (list!=NULL) trace->query &= ~(QUERY_OWNR);
        if (list!=NULL) trace->query &= ~(QUERY_DATA);
        if (list!=NULL) trace->query &= ~(QUERY_LIST);
        cf_appList(&trace->ownrList, list);
    }
    if ( mode == READ_ZON ) {
        list = cf_text2List(str);
        if (list!=NULL) trace->query &= ~(QUERY_ZONE);
        if (list!=NULL) trace->query &= ~(QUERY_DATA);
        if (list!=NULL) trace->query &= ~(QUERY_LIST);
        cf_appList(&trace->zoneList, list);
    }
    if ( mode == READ_BKV ) {
        list = cf_text2List(str);
        if (list!=NULL) trace->query &= ~(QUERY_BSKV);
        if (list!=NULL) trace->query &= ~(QUERY_DATA);
        if (list!=NULL) trace->query &= ~(QUERY_LIST);
        cf_appList(&trace->bskvList, list);
    }
    if ( mode == READ_LIM ) ff_limits(str, trace);
    if ( mode == READ_REP ) ff_report(str, trace);
  }
  return NULL;
}
void ff_limits(char *s, Trace *trace)
{
  float f;
  if (strstr(s, "MAX_LOAD_PCT")!=NULL) {
    sscanf(s, "%*s = %f", &f);
    trace->maxLoadPct = f;
    trace->query &= ~(QUERY_LOAD);
  }
  if (strstr(s, "CHECK_TX_THERMAL")!=NULL) {
    if (strstr(s, "NO" )!=NULL) trace->tx_thermal = 0;
    if (strstr(s, "YES")!=NULL) trace->tx_thermal = 1;
    trace->query &= ~(QUERY_TXTH);
  }
}
void ff_report(char *s, Trace *trace)
{
  if (strstr(s, "NAME"           )!=NULL) {
    sscanf(s, "%*s = %s", trace->outName);
    trace->query &= ~(QUERY_NAME);
    return;
  }
  if (strstr(s, "CASES_PER_TABLE")!=NULL) {
    sscanf(s, "%*s = %d", &trace->trc->wide);
    trace->query &= ~(QUERY_WDTH);
  }
  if (strstr(s, "TYPE"           )!=NULL) {
    if (strstr(s, "BRANCH_LOADING"  )!=NULL) trace->type = TYPE_LOADING;
    if (strstr(s, "BRANCH_MW_LOSS"  )!=NULL) trace->type = TYPE_MW_LOSS;
    if (trace->type == TYPE_MW_LOSS)
        trace->query &= ~(QUERY_LOAD | QUERY_TXTH);
    trace->query &= ~(QUERY_TYPE);
    return;
  }
  if (strstr(s, "FORMAT"    )!=NULL) {
    if (strstr(s, "DATA" )!=NULL) trace->trc->form = FORM_DATA_FULL;
    if (strstr(s, "PAGE" )!=NULL) trace->trc->form = FORM_PAGE_WIDE;
    trace->query &= ~(QUERY_FORM);
  }
  if (strstr(s, "SORT"  )!=NULL) {
    if (strstr(s, "ALPHA"      )!=NULL) trace->sort = SORT_BY_ALPHA;
    if (strstr(s, "BASE_KV"    )!=NULL) trace->sort = SORT_BY_BASEKV;
    if (strstr(s, "OWNER"      )!=NULL) trace->sort = SORT_BY_OWNER;
    if (strstr(s, "ZONE"       )!=NULL) trace->sort = SORT_BY_ZONE;
    if (strstr(s, "INPUT"      )!=NULL) trace->sort = SORT_BY_INPUT;
    if (strstr(s, "SOLUTION"   )!=NULL) trace->sort = SORT_BY_SOLN;
    trace->query &= ~(QUERY_SORT);
  }
  if (strstr(s, "LIST"    )!=NULL) {
    if (strstr(s, "YES")!=NULL) trace->lis->type = LIST_YES;
    if (strstr(s, "NO" )!=NULL) trace->lis->type = LIST_NO;
    trace->query &= ~(QUERY__LIS);
  }
  if (strstr(s, "TRACE"   )!=NULL) {
    if (strstr(s, "YES")!=NULL) trace->trc->type = TRACE_YES;
    if (strstr(s, "NO" )!=NULL) trace->trc->type = TRACE_NO;
    trace->query &= ~(QUERY__TRC);
  }
}
