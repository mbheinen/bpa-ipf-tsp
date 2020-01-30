/******************************************************************************\
UTILITY:    SIX - Solution and Input eXtraction
TYPE:       Powerflow (IPF) data extraction.
SUMMARY:    Generates a table of lineflows in requested branches for multiple 
            cases.  Supports sorting, screening, and scripting.
RELATED:    FINDOUT, MIMIC
SEE ALSO:   TO_DECWRITE.HELP, VMS_TO_PC_TO_EXCEL.HELP
UPDATED:    March 5, 1996
LANGUAGE:   Standard C.  CFLOW libraries.  cf_util.h
DEVELOPER:  William D. Rogers, BPA, TEOS (detail), 230-3806, wdrogers@bpa.gov
REQUESTER:  Paul Ferron
USERS:      Paul Ferron
IPF:        Version 315 or above recommended.
PURPOSE:    Load a basecase, solve it, and print out the network and solution
            data in a character-delimited format.
\******************************************************************************/
/******************************* #include *************************************/
#include "cflowlib.h"
#include "cf_util.h"
/***************************** end #include ***********************************/
/******************************* #define **************************************/
#define  OK               0
#define  MAX_IN           150
#define  FF_WILD          '?'
#define  PAGE_LENGTH      61
#define  DOT_LINE         "..................................................."
#define  LOG_SPEC         "SIX.LOG"
#define  DFLT_CASE_PER_PAGE       6             /* 6 cases fit in 132 columns */
#define  DFLT_MAX_LOAD_PCT       80
#define  QUERY_BASE (int) (1<< 0)  /* prompt for basecases */
#define  QUERY__RUN (int) (1<< 1)  /* prompt for continuation of run */
#define  READ_INC        (int)    1 /* found /INCLUDE card in CFLOW data file */
#define  READ_BSE        (int)    2 /* found /BASECAS card in CFLOW data file */
#define  READ_REP        (int)    9 /* found /REPORT  card in CFLOW data file */
/***************************** end #define ************************************/
/******************************* enum *****************************************/
/***************************** end enum ***************************************/
/******************************* typedef **************************************/

typedef struct traceRecord {
    struct LinkRecord *basecaseList;
    int                query;
    char               ipf_version[MAX_IN];
    char               time_stamp[MAX_IN];
    char               user_id[MAX_IN];
} Trace;
/******************************* end typedef **********************************/

/* top LINEFLOW functions - called by main() **********************************/
void   initializeTrace(Trace *trace);
void   processCommandLine(Trace *trace, int argc, char *argv[]);
void   instructions(Trace trace);
void   promptUser(Trace *trace);
void   queryContinue(Trace trace);
void   extractData(Trace *trace);
void   displayData(Trace *trace);
void   finalRemarks(Trace trace);
/* end LINEFLOW functions - called by main() **********************************/

/* top LINEFLOW functions - report functions **********************************/
void   printArea(FILE *fp, pf_rec *r);
void   printAreaN(FILE *fp, pf_rec *r);
void   printItie(FILE *fp, pf_rec *r);
void   printXdata(FILE *fp, pf_rec *r);
void   printACbus(FILE *fp, pf_rec *r);
void   printCBus(FILE *fp, pf_rec *r);
void   printBDbus(FILE *fp, pf_rec *r);
void   printBMbus(FILE *fp, pf_rec *r);
void   printE_branch(FILE *fp, pf_rec *r);
void   printL_branch(FILE *fp, pf_rec *r);
void   printLDbranch(FILE *fp, pf_rec *r);
void   printLMbranch(FILE *fp, pf_rec *r);
void   printT_branch(FILE *fp, pf_rec *r);
void   printTPbranch(FILE *fp, pf_rec *r);
void   printR_branch(FILE *fp, pf_rec *r);
void   printRMbranch(FILE *fp, pf_rec *r);
void   printError(pf_rec *r);
/* end LINEFLOW functions - report functions **********************************/

/* top LINEFLOW functions - support functions *********************************/
void   extArea(Trace *trace, FILE *fp);
void   extItie(Trace *trace, FILE *fp);
void   extXdata(Trace *trace, FILE *fp);
void   extBus(Trace *trace, FILE *fp);
void   extCBus(Trace *trace, FILE *fp);
void   extBranch(Trace *trace, FILE *fp);
/* end LINEFLOW functions - support functions *********************************/

/* top LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/
Link  *ff_stream2List(FILE *readMe, Trace *trace, Link **expList);
void   ff_newName(char *oldspec, char *newspec, char *type);
void   ff_expList(Trace *trace, Link *dataList, Link **expList);
void   ff_fprintList(FILE *fp, Link *link, char *tag,char *tab,char type,int n);
/* end LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/

/* top CF_UTIL.H candidate functions ******************************************/
/* end CF_UTIL.H candidate functions ******************************************/

main(int argc, char *argv[])
{
  Trace trace;

  initializeTrace(&trace);
  processCommandLine(&trace, argc, argv);
  instructions(trace);
  promptUser(&trace);
  queryContinue(trace);
  pf_cflow_init(argc, argv);
  extractData(&trace);
  finalRemarks(trace);
}
void initializeTrace(Trace *trace)
{ /* special function to do set up/init for LINEFLOW */
  trace->basecaseList =   NULL;
  trace->query        = ( QUERY_BASE | QUERY__RUN );
  strncat(trace->ipf_version, getenv("IPFSRV_CF"), MAX_IN);
  cuserid(trace->user_id);
  cf_time(trace->time_stamp, MAX_IN, "  %d-%b-%Y %H:%M:%S");
  strcpy(CF_logSpec, LOG_SPEC);
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
  ff_expList(trace, topLink, &trace->basecaseList);
  if (cf_cntLink(trace->basecaseList)) trace->query &= ~(QUERY_BASE);
  if (trace->query == QUERY__RUN) trace->query &= ~(QUERY__RUN);
  return;
}
void instructions(Trace trace)
{
  if (trace.query==0) return;     /* there is not going to be any queries */
  printf("\n        Welcome to SIX - [S]olution and [I]nput data e[X]traction");
  printf("\n - Supported cards: A, A1, A2, A3, A4, I, B, BC, BE, BF, BG, BQ, BS, BT, BV, BX");
  printf("\n - Supported cards: BD, BM, E, L, LD, LM, T, TP, R, RN, RQ, RV, RM, RP, X, +");
  printf("\n");
  printf("\n - Default values shown in brackets [].  Strike return if value is acceptable.");
  printf("\n - Press Ctrl-Y and type STOP to abort the program at any time.");
  printf("\n - Files ending in .DAT, .LIS, or .TRC or followed by / are opened and read as data files.");
  printf("\n - Recommended for use with Powerflow v.315 and above.");
  printf("\n - IPF Executible Used: IPFSRV_CF == \"%s\"", trace.ipf_version);
  printf("\n");
}
void promptUser(Trace *trace)     
{
  char query[MAX_IN];
  Link *list;

  printf("%s", DOT_LINE);
  if (trace->query & QUERY_BASE) {
    printf("\n\n/BASECASE or /INCLUDE");
    cf_nprompt("\n > Enter list of Basecases (and/or data files) : ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->basecaseList);
  }
  cf_exit(trace->basecaseList==NULL, "No data or basecase files!  Quitting!\n");
  if (trace->query == QUERY__RUN) trace->query &= ~(QUERY__RUN);

  printf("%s\n", DOT_LINE);
  return;
}
void queryContinue(Trace trace)
{
  int  yes;
  if ( trace.query & QUERY__RUN) {
    yes = cf_yprompt("\n > Do you want to continue this run?[%c]: ", 'Y');
    cf_exit(!yes, "Quitting!");
  }
}
void extractData(Trace *trace)
{
  Link *curCase;
  int   nCases = 0;
  char  ara_name[FILENAME_MAX], bus_name[FILENAME_MAX], brn_name[FILENAME_MAX];
  FILE *ara_fp, *bus_fp, *brn_fp;
  time_t time2, time1;
  time(&time1);
  curCase = trace->basecaseList;
  while (curCase!=NULL) {
    if (pf_load_oldbase(curCase->data)!=OK) { /* can't load */
        cf_logErr(" Cannot load this base case: %s\n", curCase->data);
        curCase = cf_delLink(&trace->basecaseList, curCase);
        continue;
    }
    ff_newName(curCase->data, ara_name, ".SXA");
    ff_newName(curCase->data, bus_name, ".SXB");
    ff_newName(curCase->data, brn_name, ".SXL");
    if ( (ara_fp=cf_openFile(ara_name, "w")) == NULL ) ara_fp = stdout;
    if ( (bus_fp=cf_openFile(bus_name, "w")) == NULL ) bus_fp = stdout;
    if ( (brn_fp=cf_openFile(brn_name, "w")) == NULL ) brn_fp = stdout;
    if ( ara_fp == stdout ) strcpy(ara_name, "stdout");
    if ( bus_fp == stdout ) strcpy(bus_name, "stdout");
    if ( brn_fp == stdout ) strcpy(brn_name, "stdout");
    fprintf(stdout, " Extracting %s (A, I cards) to %s\n", curCase->data, ara_name);
    fprintf(stdout, " Extracting %s (B, +, X cards) to %s\n", curCase->data, bus_name);
    fprintf(stdout, " Extracting %s (E, L, T, R cards) to %s\n", curCase->data, brn_name);

/*    printf(" Computing Area and Intertie Solution.\n"); */
    pf_solve_area('O');            /* needed to calculate intertie quantities */

    extArea(trace, ara_fp);
    extItie(trace, ara_fp);

    extXdata(trace, bus_fp);
    extBus(trace, bus_fp);
    extCBus(trace, bus_fp);

    extBranch(trace, brn_fp);

    nCases++;
    curCase = curCase->next;
  }
  time(&time2);
  printf("extractData() run time: %.1f seconds\n", difftime(time2, time1));
  return;
}
void finalRemarks(Trace trace)
{
  printf("\n");
  printf("\nMemory allocation (bytes): Cur:%d Max:%d Alloc:%d Freed:%d\n",
    CF_memCurAlloc, CF_memMaxAlloc, CF_memTotAlloc, CF_memTotFreed);
  if (CF_logFile != NULL) printf("\nError  report written to %s", CF_logSpec);
  printf("\n");
}
void extArea(Trace *trace, FILE *fp)
{/* note: i'm not picking up A1, A2, etc. */
  pf_rec r;
  int e;
  char type[3];

  for ( e=pf_rec_area(&r, "F"); e==OK; e=pf_rec_area(&r, "N") ) {
    strncpy(type, r.i.area.type, 3);
    if (type[0]!='A') { cf_logErr("Invalid area type: %s\n", type); continue; }
    pf_rec_area(&r, "O");
    if      (type[0]=='A' && type[1]=='1') printAreaN(fp, &r);
    else if (type[0]=='A' && type[1]=='2') printAreaN(fp, &r);
    else if (type[0]=='A' && type[1]=='3') printAreaN(fp, &r);
    else if (type[0]=='A' && type[1]=='4') printAreaN(fp, &r);
    else if (type[0]=='A')                 printArea(fp, &r);
    else                                   printError(&r);
  }
  return;
}
void extItie(Trace *trace, FILE *fp)
{
  pf_rec r;
  int e;

  for ( e=pf_rec_itie(&r, "F"); e==OK; e=pf_rec_itie(&r, "N") ) {
    pf_rec_itie(&r, "O");
    printItie(fp, &r);
  }
  return;
}
void extXdata(Trace *trace, FILE *fp)
{
  pf_rec r;
  int e;
  char type[3];

  for ( e=pf_rec_xdata(&r, "F"); e==OK; e=pf_rec_xdata(&r, "N") ) {
    strncpy(type, r.i.xdata.type, 3);
    pf_rec_xdata(&r, "O");
    printXdata(fp, &r);
  }
  return;
}
void extBus(Trace *trace, FILE *fp)
{
  pf_rec r;
  int e;
  char type[3];

  for ( e=pf_rec_bus(&r, "F"); e==OK; e=pf_rec_bus(&r, "N") ) {
    strncpy(type, r.i.ACbus.type, 3);
    pf_rec_bus(&r, "O");
    if      (type[0]=='B' && type[1]=='D') printBDbus(fp, &r);
    else if (type[0]=='B' && type[1]=='M') printBMbus(fp, &r);
    else if (type[0]=='B')                 printACbus(fp, &r);
    else                                   printError(&r);
  }
  return;
}
void extCBus(Trace *trace, FILE *fp)
{
  pf_rec r;
  int e;

  for ( e=pf_rec_cbus(&r, "F"); e==OK; e=pf_rec_cbus(&r, "N") ) {
    pf_rec_bus(&r, "O");
    printCBus(fp, &r);
  }
  return;
}
void extBranch(Trace *trace, FILE *fp)
{
  pf_rec r;
  int e;
  char type[3];

  for ( e=pf_rec_branch(&r, "F"); e==OK; e=pf_rec_branch(&r, "N") ) {
    if (cf_rec_l2h(&r.i.branch)==-1) continue;
    pf_rec_branch(&r, "O");
    strncpy(type, r.i.branch.type, 3);
    if      (type[0]=='E')                 printE_branch(fp, &r);
    else if (type[0]=='L' && type[1]=='D') printLDbranch(fp, &r);
    else if (type[0]=='L' && type[1]=='M') printLMbranch(fp, &r);
    else if (type[0]=='L')                 printL_branch(fp, &r);
    else if (type[0]=='T' && type[1]=='P') printTPbranch(fp, &r);
    else if (type[0]=='T')                 printT_branch(fp, &r);
    else if (type[0]=='R' && type[1]=='M') printRMbranch(fp, &r);
    else if (type[0]=='R' && type[1]=='P') printRMbranch(fp, &r);
    else if (type[0]=='R' && type[1]=='N') printR_branch(fp, &r);
    else if (type[0]=='R' && type[1]=='Q') printR_branch(fp, &r);
    else if (type[0]=='R' && type[1]=='V') printR_branch(fp, &r);
    else if (type[0]=='R')                 printR_branch(fp, &r);
    else                                   printError(&r);
  }
  return;
}
void printArea(FILE *fp, pf_rec *r)
{
/* input data */
  fprintf(fp, "%c,",     r->i.area.type[0]);
  fprintf(fp, "%c,",     r->i.area.type[1]);
  fprintf(fp, "\"%s\",", r->i.area.name);
  fprintf(fp, "\"%s\",", r->i.area.sbus_name);
  fprintf(fp, "%g,",     r->i.area.sbus_kv);
  fprintf(fp, "%g,",     r->i.area.sched_export);
  fprintf(fp, "%0.2s,",  r->i.area.zone0);
  fprintf(fp, "%0.2s,",  r->i.area.zone1);
  fprintf(fp, "%0.2s,",  r->i.area.zone2);
  fprintf(fp, "%0.2s,",  r->i.area.zone3);
  fprintf(fp, "%0.2s,",  r->i.area.zone4);
  fprintf(fp, "%0.2s,",  r->i.area.zone5);
  fprintf(fp, "%0.2s,",  r->i.area.zone6);
  fprintf(fp, "%0.2s,",  r->i.area.zone7);
  fprintf(fp, "%0.2s,",  r->i.area.zone8);
  fprintf(fp, "%0.2s,",  r->i.area.zone9);
  fprintf(fp, "%g,",     r->i.area.max_Vpu);
  fprintf(fp, "%g,",     r->i.area.min_Vpu);
/* solution data */
  fprintf(fp, "%g,",     r->s.area.Pgen);
  fprintf(fp, "%g,",     r->s.area.Pload);
  fprintf(fp, "%g,",     r->s.area.Ploss);
  fprintf(fp, "%g ",     r->s.area.Pexport);
  fprintf(fp, "\n");
}
void printAreaN(FILE *fp, pf_rec *r)
{
/* input data */
  fprintf(fp, "%c,",     r->i.area.type[0]);
  fprintf(fp, "%c,",     r->i.area.type[1]);
  fprintf(fp, "\"%s\",", r->i.area.name);
  fprintf(fp, ",");      /* slack bus name */
  fprintf(fp, ",");      /* slack bus kv   */
  fprintf(fp, ",");      /* sched. export  */
  fprintf(fp, "%0.2s,",  r->i.area.zone0);
  fprintf(fp, "%0.2s,",  r->i.area.zone1);
  fprintf(fp, "%0.2s,",  r->i.area.zone2);
  fprintf(fp, "%0.2s,",  r->i.area.zone3);
  fprintf(fp, "%0.2s,",  r->i.area.zone4);
  fprintf(fp, "%0.2s,",  r->i.area.zone5);
  fprintf(fp, "%0.2s,",  r->i.area.zone6);
  fprintf(fp, "%0.2s,",  r->i.area.zone7);
  fprintf(fp, "%0.2s,",  r->i.area.zone8);
  fprintf(fp, "%0.2s ",  r->i.area.zone9);
/* solution data */ /* Not applicable */
  fprintf(fp, "\n");
}
void printItie(FILE *fp, pf_rec *r)
{
/* input data */
  fprintf(fp, "%c,", r->i.itie.type[0]);
  fprintf(fp, "\"%s\",", r->i.itie.area1_name);
  fprintf(fp, "\"%s\",", r->i.itie.area2_name);
  fprintf(fp, "%g,", r->i.itie.sched_export);
/* solution data */
  fprintf(fp, "%g,", r->s.itie.Pexport);
  fprintf(fp, "%g,", r->s.itie.Pcirc);
  fprintf(fp, "%d ", r->s.itie.input_exists);
  fprintf(fp, "\n");
}
void printXdata(FILE *fp, pf_rec *r)
{
/* input data */
  fprintf(fp, "%c,", r->i.xdata.type[0]);
  fprintf(fp, "%c,", r->i.xdata.type[1]);
  fprintf(fp, "%s,", r->i.xdata.owner);
  fprintf(fp, "\"%s\",", r->i.xdata.bus_name);
  fprintf(fp, "%g,", r->i.xdata.bus_kv);
  fprintf(fp, "\"%s\",", r->i.xdata.rmt_name);
  fprintf(fp, "%g,", r->i.xdata.rmt_kv);
  fprintf(fp, "%d,", r->i.xdata.seg1_num_steps);
  fprintf(fp, "%g,", r->i.xdata.seg1_delta_mva);
  fprintf(fp, "%d,", r->i.xdata.seg2_num_steps);
  fprintf(fp, "%g,", r->i.xdata.seg2_delta_mva);
  fprintf(fp, "%d,", r->i.xdata.seg3_num_steps);
  fprintf(fp, "%g,", r->i.xdata.seg3_delta_mva);
  fprintf(fp, "%d,", r->i.xdata.seg4_num_steps);
  fprintf(fp, "%g,", r->i.xdata.seg4_delta_mva);
  fprintf(fp, "%d,", r->i.xdata.seg5_num_steps);
  fprintf(fp, "%g,", r->i.xdata.seg5_delta_mva);
  fprintf(fp, "%d,", r->i.xdata.seg6_num_steps);
  fprintf(fp, "%g,", r->i.xdata.seg6_delta_mva);
  fprintf(fp, "%d,", r->i.xdata.seg7_num_steps);
  fprintf(fp, "%g,", r->i.xdata.seg7_delta_mva);
  fprintf(fp, "%d,", r->i.xdata.seg8_num_steps);
  fprintf(fp, "%g,", r->i.xdata.seg8_delta_mva);
/* solution data */
  fprintf(fp, "%g,", r->s.xdata.seg1_sch_units);
  fprintf(fp, "%g,", r->s.xdata.seg1_used_units);
  fprintf(fp, "%g,", r->s.xdata.seg1_mvar_per_unit);
  fprintf(fp, "%g,", r->s.xdata.seg2_sch_units);
  fprintf(fp, "%g,", r->s.xdata.seg2_used_units);
  fprintf(fp, "%g,", r->s.xdata.seg2_mvar_per_unit);
  fprintf(fp, "%g,", r->s.xdata.seg3_sch_units);
  fprintf(fp, "%g,", r->s.xdata.seg3_used_units);
  fprintf(fp, "%g,", r->s.xdata.seg3_mvar_per_unit);
  fprintf(fp, "%g,", r->s.xdata.seg4_sch_units);
  fprintf(fp, "%g,", r->s.xdata.seg4_used_units);
  fprintf(fp, "%g,", r->s.xdata.seg4_mvar_per_unit);
  fprintf(fp, "%g,", r->s.xdata.seg5_sch_units);
  fprintf(fp, "%g,", r->s.xdata.seg5_used_units);
  fprintf(fp, "%g,", r->s.xdata.seg5_mvar_per_unit);
  fprintf(fp, "%g,", r->s.xdata.seg6_sch_units);
  fprintf(fp, "%g,", r->s.xdata.seg6_used_units);
  fprintf(fp, "%g,", r->s.xdata.seg6_mvar_per_unit);
  fprintf(fp, "%g,", r->s.xdata.seg7_sch_units);
  fprintf(fp, "%g,", r->s.xdata.seg7_used_units);
  fprintf(fp, "%g,", r->s.xdata.seg7_mvar_per_unit);
  fprintf(fp, "%g,", r->s.xdata.seg8_sch_units);
  fprintf(fp, "%g,", r->s.xdata.seg8_used_units);
  fprintf(fp, "%g ", r->s.xdata.seg8_mvar_per_unit);
  fprintf(fp, "\n");
}
void printACbus(FILE *fp, pf_rec *r)
{
/* input data */
  fprintf(fp, "%c,", r->i.ACbus.type[0]);
  fprintf(fp, "%c,", r->i.ACbus.type[1]);
  fprintf(fp, "%s,", r->i.ACbus.owner);
  fprintf(fp, "\"%s\",", r->i.ACbus.name);
  fprintf(fp, "%g,", r->i.ACbus.kv);
  fprintf(fp, "%s,", r->i.ACbus.zone);
  fprintf(fp, "%g,", r->i.ACbus.Pload);
  fprintf(fp, "%g,", r->i.ACbus.Qload);
  fprintf(fp, "%g,", r->i.ACbus.Pshunt);
  fprintf(fp, "%g,", r->i.ACbus.Qshunt);
  fprintf(fp, "%g,", r->i.ACbus.Pmax);
  fprintf(fp, "%g,", r->i.ACbus.Pgen);
  fprintf(fp, "%g,", r->i.ACbus.Qsch_Qmax);
  fprintf(fp, "%g,", r->i.ACbus.Qmin);
  fprintf(fp, "%g,", r->i.ACbus.Vhold_Vmax);
  fprintf(fp, "%g,", r->i.ACbus.Vmin_Vdeg);
  fprintf(fp, "\"%s\",", r->i.ACbus.rmt_name);
  fprintf(fp, "%g,", r->i.ACbus.rmt_kv);
  fprintf(fp, "%g,", r->i.ACbus.pct_vars);
/* solution data */
  fprintf(fp, "%g,", r->s.ACbus.Pgen);
  fprintf(fp, "%g,", r->s.ACbus.Qgen);
  fprintf(fp, "%g,", r->s.ACbus.Vmag);
  fprintf(fp, "%g,", r->s.ACbus.Vdeg);
  fprintf(fp, "%g,", r->s.ACbus.Pload);
  fprintf(fp, "%g,", r->s.ACbus.Qload);
  fprintf(fp, "%g,", r->s.ACbus.Bshunt_used);
  fprintf(fp, "%g,", r->s.ACbus.Bshunt_sch);
  fprintf(fp, "%g,", r->s.ACbus.Bshunt_used_cap);
  fprintf(fp, "%g,", r->s.ACbus.Bshunt_sch_cap);
  fprintf(fp, "%g,", r->s.ACbus.Bshunt_used_rx);
  fprintf(fp, "%g,", r->s.ACbus.Bshunt_sch_rx);
  fprintf(fp, "%g ", r->s.ACbus.Qunsch);
  fprintf(fp, "\n");
}
void printCBus(FILE *fp, pf_rec *r)
{
/* input data */
  fprintf(fp, "%c,", r->i.cbus.type[0]);
  fprintf(fp, "%c,", r->i.cbus.type[1]); /* code */
  fprintf(fp, "%s,", r->i.cbus.owner);
  fprintf(fp, "\"%s\",", r->i.cbus.name);
  fprintf(fp, "%g,", r->i.cbus.kv);
  fprintf(fp, "%s,", r->i.cbus.code_year);
  fprintf(fp, "%g,", r->i.cbus.Pload);
  fprintf(fp, "%g,", r->i.cbus.Qload);
  fprintf(fp, "%g,", r->i.cbus.Gshunt);
  fprintf(fp, "%g,", r->i.cbus.Bshunt);
  fprintf(fp, "%g,", r->i.cbus.Pgen);
  fprintf(fp, "%g,", r->i.cbus.Qgen_Qmax);
  fprintf(fp, "%g,", r->i.cbus.Qmin);
/* solution data */
  fprintf(fp, "%g,", r->s.cbus.Pgen);
  fprintf(fp, "%g,", r->s.cbus.Qgen);
  fprintf(fp, "%g,", r->s.cbus.Pload);
  fprintf(fp, "%g,", r->s.cbus.Qload);
  fprintf(fp, "%g,", r->s.cbus.Gshunt);
  fprintf(fp, "%g ", r->s.cbus.Bshunt);
  fprintf(fp, "\n");
}
void printBDbus(FILE *fp, pf_rec *r)
{
/* input data */
  fprintf(fp, "%c,", r->i.DCbus.type[0]);
  fprintf(fp, "%c,", r->i.DCbus.type[1]);
  fprintf(fp, "%s,", r->i.DCbus.owner);
  fprintf(fp, "\"%s\",", r->i.DCbus.name);
  fprintf(fp, "%g,", r->i.DCbus.kv);
  fprintf(fp, "%s,", r->i.DCbus.zone);
  fprintf(fp, "%d,", r->i.DCbus.bridges_per_ckt);
  fprintf(fp, "%g,", r->i.DCbus.smooth_rx_mh);
  fprintf(fp, "%g,", r->i.DCbus.alpha_min_deg);
  fprintf(fp, "%g,", r->i.DCbus.alpha_stop_deg);
  fprintf(fp, "%g,", r->i.DCbus.valve_drop_per_bridge_volts);
  fprintf(fp, "%g,", r->i.DCbus.bridge_current_rating_amps);
  fprintf(fp, "\"%s\",", r->i.DCbus.commutating_bus_name);
  fprintf(fp, "%g,", r->i.DCbus.commutating_bus_kv);
  fprintf(fp, ",");        /* last 5 fields not needed for BD busses; BM only */
  fprintf(fp, ",");
  fprintf(fp, ",");
  fprintf(fp, ",");
  fprintf(fp, ",");
/* solution data */
  fprintf(fp, "%g,", r->s.DCbus.P_DC);
  fprintf(fp, "%g,", r->s.DCbus.Q_DC);
  fprintf(fp, "%g,", r->s.DCbus.V_DC);
  fprintf(fp, "%g,", r->s.DCbus.converter_deg);
  fprintf(fp, "%g,", r->s.DCbus.P_valve_losses);
  fprintf(fp, "%g ", r->s.DCbus.Q_valve_losses);
  fprintf(fp, "\n");
}
void printBMbus(FILE *fp, pf_rec *r)
{
/* input data */
  fprintf(fp, "%c,", r->i.DCbus.type[0]);
  fprintf(fp, "%c,", r->i.DCbus.type[1]);
  fprintf(fp, "%s,", r->i.DCbus.owner);
  fprintf(fp, "\"%s\",", r->i.DCbus.name);
  fprintf(fp, "%g,", r->i.DCbus.kv);
  fprintf(fp, "%s,", r->i.DCbus.zone);
  fprintf(fp, "%d,", r->i.DCbus.bridges_per_ckt);
  fprintf(fp, "%g,", r->i.DCbus.smooth_rx_mh);
  fprintf(fp, "%g,", r->i.DCbus.alpha_min_deg);
  fprintf(fp, "%g,", r->i.DCbus.alpha_stop_deg);
  fprintf(fp, "%g,", r->i.DCbus.valve_drop_per_bridge_volts);
  fprintf(fp, "%g,", r->i.DCbus.bridge_current_rating_amps);
  fprintf(fp, "\"%s\",", r->i.DCbus.commutating_bus_name);
  fprintf(fp, "%g,", r->i.DCbus.commutating_bus_kv);
  fprintf(fp, "%c,", r->i.DCbus.converter_code);
  fprintf(fp, "%g,", r->i.DCbus.alpha_gamma_N_deg);/* last 4 not needed on BD */
  fprintf(fp, "%g,", r->i.DCbus.gamma_0_deg);
  fprintf(fp, "%g,", r->i.DCbus.P_sched);
  fprintf(fp, "%g,", r->i.DCbus.V_sched);
/* solution data */
  fprintf(fp, "%g,", r->s.DCbus.P_DC);
  fprintf(fp, "%g,", r->s.DCbus.Q_DC);
  fprintf(fp, "%g,", r->s.DCbus.V_DC);
  fprintf(fp, "%g,", r->s.DCbus.converter_deg);
  fprintf(fp, "%g,", r->s.DCbus.P_valve_losses);
  fprintf(fp, "%g ", r->s.DCbus.Q_valve_losses);
  fprintf(fp, "\n");
}
void printE_branch(FILE *fp, pf_rec *r)
{
/* input data */
  fprintf(fp, "%c,", r->i.E.type[0]);
  fprintf(fp, "%s,", r->i.E.owner);
  fprintf(fp, "\"%s\",", r->i.E.bus1_name);
  fprintf(fp, "%g,", r->i.E.bus1_kv);
  fprintf(fp, "%d,", r->i.E.meter);
  fprintf(fp, "\"%s\",", r->i.E.bus2_name);
  fprintf(fp, "%g,", r->i.E.bus2_kv);
  fprintf(fp, "%c,", r->i.E.ckt_id);
  fprintf(fp, "%d,", r->i.E.section);
  fprintf(fp, "%g,", r->i.E.total_rating);
  fprintf(fp, "%d,", r->i.E.num_ckts);
  fprintf(fp, "%g,", r->i.E.r);
  fprintf(fp, "%g,", r->i.E.x);
  fprintf(fp, "%g,", r->i.E.g1);
  fprintf(fp, "%g,", r->i.E.b1);
  fprintf(fp, "%g,", r->i.E.g2);
  fprintf(fp, "%g,", r->i.E.b2);
  fprintf(fp, "%s,", r->i.E.date_in);
  fprintf(fp, "%s,", r->i.E.date_out);
  fprintf(fp, "%g,", r->i.E.thermal_rating);
  fprintf(fp, "%g,", r->i.E.bottleneck_rating);
/* solution data */
  fprintf(fp, "%g,", r->s.branch.Pin);
  fprintf(fp, "%g,", r->s.branch.Qin);
  fprintf(fp, "%g,", r->s.branch.Pout);
  fprintf(fp, "%g,", r->s.branch.Qout);
  fprintf(fp, "%g,", r->s.branch.Ploss);
  fprintf(fp, "%g,", r->s.branch.Qloss);
  fprintf(fp, "%g,", r->s.branch.crit_line_load_amps);
  fprintf(fp, "%g,", r->s.branch.crit_line_rat_amps);
  fprintf(fp, "%c,", r->s.branch.crit_line_rat_code);
  fprintf(fp, "%d,", r->s.branch.crit_line_load_term);
  fprintf(fp, "%g,", r->s.branch.tot_line_load_pct);
  fprintf(fp, "%g ", r->s.branch.tot_line_load_amps);
  fprintf(fp, "\n");
}
void printL_branch(FILE *fp, pf_rec *r)
{
/* input data */
  fprintf(fp, "%c,", r->i.L.type[0]);
  fprintf(fp, "%c,", r->i.L.type[1]);
  fprintf(fp, "%s,", r->i.L.owner);
  fprintf(fp, "\"%s\",", r->i.L.bus1_name);
  fprintf(fp, "%g,", r->i.L.bus1_kv);
  fprintf(fp, "%d,", r->i.L.meter);
  fprintf(fp, "\"%s\",", r->i.L.bus2_name);
  fprintf(fp, "%g,", r->i.L.bus2_kv);
  fprintf(fp, "%c,", r->i.L.ckt_id);
  fprintf(fp, "%d,", r->i.L.section);
  fprintf(fp, "%g,", r->i.L.total_rating);
  fprintf(fp, "%d,", r->i.L.num_ckts);
  fprintf(fp, "%g,", r->i.L.r);
  fprintf(fp, "%g,", r->i.L.x);
  fprintf(fp, "%g,", r->i.L.g);
  fprintf(fp, "%g,", r->i.L.b);
  fprintf(fp, "%g,", r->i.L.miles);
  fprintf(fp, "\"%s\",", r->i.L.descrip);
  fprintf(fp, "%s,", r->i.L.date_in);
  fprintf(fp, "%s,", r->i.L.date_out);
  fprintf(fp, "%g,", r->i.L.thermal_rating);
  fprintf(fp, "%g,", r->i.L.bottleneck_rating);
/* solution data */
  fprintf(fp, "%g,", r->s.branch.Pin);
  fprintf(fp, "%g,", r->s.branch.Qin);
  fprintf(fp, "%g,", r->s.branch.Pout);
  fprintf(fp, "%g,", r->s.branch.Qout);
  fprintf(fp, "%g,", r->s.branch.Ploss);
  fprintf(fp, "%g,", r->s.branch.Qloss);
  fprintf(fp, "%g,", r->s.branch.crit_line_load_amps);
  fprintf(fp, "%g,", r->s.branch.crit_line_rat_amps);
  fprintf(fp, "%c,", r->s.branch.crit_line_rat_code);
  fprintf(fp, "%d,", r->s.branch.crit_line_load_term);
  fprintf(fp, "%g,", r->s.branch.tot_line_load_pct);
  fprintf(fp, "%g ", r->s.branch.tot_line_load_amps);
  fprintf(fp, "\n");
}
void printLDbranch(FILE *fp, pf_rec *r)
{
/* input data */
  fprintf(fp, "%c,",     r->i.LD.type[0]);
  fprintf(fp, "%c,",     r->i.LD.type[1]);
  fprintf(fp, "%s,",     r->i.LD.owner);
  fprintf(fp, "\"%s\",", r->i.LD.bus1_name);
  fprintf(fp, "%g,",     r->i.LD.bus1_kv);
  fprintf(fp, "\"%s\",", r->i.LD.bus2_name);
  fprintf(fp, "%g,",     r->i.LD.bus2_kv);
  fprintf(fp, "%g,",     r->i.LD.total_rating);
  fprintf(fp, "%g,",     r->i.LD.R);
  fprintf(fp, "%g,",     r->i.LD.L_mh);
  fprintf(fp, "%g,",     r->i.LD.C_uf);
  fprintf(fp, "%c,",     r->i.LD.I_or_R_control);
  fprintf(fp, "%g,",     r->i.LD.P_sched);
  fprintf(fp, "%g,",     r->i.LD.V_sched);
  fprintf(fp, "%g,",     r->i.LD.alpha_N_deg);
  fprintf(fp, "%g,",     r->i.LD.gamma_0_deg);
  fprintf(fp, "%g,",     r->i.LD.miles);
  fprintf(fp, "%g,",     r->i.LD.thermal_rating);
  fprintf(fp, "%g,",     r->i.LD.bottleneck_rating);
/* solution data */
  fprintf(fp, "%g,", r->s.branch.Pin);
  fprintf(fp, "%g,", r->s.branch.Pout);
  fprintf(fp, "%g,", r->s.branch.Ploss);
  fprintf(fp, "%g,", r->s.branch.crit_line_load_amps);
  fprintf(fp, "%g,", r->s.branch.crit_line_rat_amps);
  fprintf(fp, "%c,", r->s.branch.crit_line_rat_code);
  fprintf(fp, "%d,", r->s.branch.crit_line_load_term);
  fprintf(fp, "%g,", r->s.branch.tot_line_load_pct);
  fprintf(fp, "%g ", r->s.branch.tot_line_load_amps);
  fprintf(fp, "\n");
}
void printLMbranch(FILE *fp, pf_rec *r)
{
/* input data */
  fprintf(fp, "%c,", r->i.LM.type[0]);
  fprintf(fp, "%c,", r->i.LM.type[1]);
  fprintf(fp, "%s,", r->i.LM.owner);
  fprintf(fp, "\"%s\",", r->i.LM.bus1_name);
  fprintf(fp, "%g,", r->i.LM.bus1_kv);
  fprintf(fp, "%d,", r->i.LM.meter);
  fprintf(fp, "\"%s\",", r->i.LM.bus2_name);
  fprintf(fp, "%g,", r->i.LM.bus2_kv);
  fprintf(fp, "%g,", r->i.LM.total_rating);
  fprintf(fp, "%g,", r->i.LM.R);
  fprintf(fp, "%g,", r->i.LM.L_mh);
  fprintf(fp, "%g,", r->i.LM.C_uf);
  fprintf(fp, "%g,", r->i.LM.miles);
  fprintf(fp, "%s,", r->i.LM.date_in);
  fprintf(fp, "%s,", r->i.LM.date_out);
  fprintf(fp, "%g,", r->i.LM.thermal_rating);
  fprintf(fp, "%g,", r->i.LM.bottleneck_rating);
/* solution data */
  fprintf(fp, "%g,", r->s.branch.Pin);
  fprintf(fp, "%g,", r->s.branch.Pout);
  fprintf(fp, "%g,", r->s.branch.Ploss);
  fprintf(fp, "%g,", r->s.branch.crit_line_load_amps);
  fprintf(fp, "%g,", r->s.branch.crit_line_rat_amps);
  fprintf(fp, "%c,", r->s.branch.crit_line_rat_code);
  fprintf(fp, "%d,", r->s.branch.crit_line_load_term);
  fprintf(fp, "%g,", r->s.branch.tot_line_load_pct);
  fprintf(fp, "%g ", r->s.branch.tot_line_load_amps);
  fprintf(fp, "\n");
}
void printT_branch(FILE *fp, pf_rec *r)
{
/* input data */
  fprintf(fp, "%c,", r->i.T.type[0]);
  fprintf(fp, "%c,", r->i.T.type[1]);
  fprintf(fp, "%s,", r->i.T.owner);
  fprintf(fp, "\"%s\",", r->i.T.bus1_name);
  fprintf(fp, "%g,", r->i.T.bus1_kv);
  fprintf(fp, "%d,", r->i.T.meter);
  fprintf(fp, "\"%s\",", r->i.T.bus2_name);
  fprintf(fp, "%g,", r->i.T.bus2_kv);
  fprintf(fp, "%c,", r->i.T.ckt_id);
  fprintf(fp, "%d,", r->i.T.section);
  fprintf(fp, "%g,", r->i.T.total_rating);
  fprintf(fp, "%d,", r->i.T.num_ckts);
  fprintf(fp, "%g,", r->i.T.r);
  fprintf(fp, "%g,", r->i.T.x);
  fprintf(fp, "%g,", r->i.T.g);
  fprintf(fp, "%g,", r->i.T.b);
  fprintf(fp, "%g,", r->i.T.tap1);
  fprintf(fp, "%g,", r->i.T.tap2);
  fprintf(fp, "%s,", r->i.T.date_in);
  fprintf(fp, "%s,", r->i.T.date_out);
  fprintf(fp, "%g,", r->i.T.thermal_rating);
  fprintf(fp, "%g,", r->i.T.bottleneck_rating);
  fprintf(fp, "%g,", r->i.T.emergency_rating);
/* solution data */
  fprintf(fp, "%g,", r->s.branch.Pin);
  fprintf(fp, "%g,", r->s.branch.Qin);
  fprintf(fp, "%g,", r->s.branch.Pout);
  fprintf(fp, "%g,", r->s.branch.Qout);
  fprintf(fp, "%g,", r->s.branch.Ploss);
  fprintf(fp, "%g,", r->s.branch.Qloss);
  fprintf(fp, "%g,", r->s.branch.crit_xfmr_load_mva);
  fprintf(fp, "%g,", r->s.branch.crit_xfmr_rat_mva);
  fprintf(fp, "%c,", r->s.branch.crit_xfmr_rat_code);
  fprintf(fp, "%d,", r->s.branch.crit_xfmr_load_term);
  fprintf(fp, "%g,", r->s.branch.tot_xfmr_load_pct);
  fprintf(fp, "%g,", r->s.branch.tot_xfmr_load_mva);
  fprintf(fp, "%g,", r->s.branch.tap1);
  fprintf(fp, "%g ", r->s.branch.tap2);
  fprintf(fp, "\n");
}
void printTPbranch(FILE *fp, pf_rec *r)
{
/* input data */
  fprintf(fp, "%c,", r->i.TP.type[0]);
  fprintf(fp, "%c,", r->i.TP.type[1]);
  fprintf(fp, "%s,", r->i.TP.owner);
  fprintf(fp, "\"%s\",", r->i.TP.bus1_name);
  fprintf(fp, "%g,", r->i.TP.bus1_kv);
  fprintf(fp, "%d,", r->i.TP.meter);
  fprintf(fp, "\"%s\",", r->i.TP.bus2_name);
  fprintf(fp, "%g,", r->i.TP.bus2_kv);
  fprintf(fp, "%c,", r->i.TP.ckt_id);
  fprintf(fp, "%d,", r->i.TP.section);
  fprintf(fp, "%g,", r->i.TP.total_rating);
  fprintf(fp, "%d,", r->i.TP.num_ckts);
  fprintf(fp, "%g,", r->i.TP.r);
  fprintf(fp, "%g,", r->i.TP.x);
  fprintf(fp, "%g,", r->i.TP.g);
  fprintf(fp, "%g,", r->i.TP.b);
  fprintf(fp, "%g,", r->i.TP.phase_shift_deg);
  fprintf(fp, "%g,", r->i.TP.tap2);
  fprintf(fp, "%s,", r->i.TP.date_in);
  fprintf(fp, "%s,", r->i.TP.date_out);
  fprintf(fp, "%g,", r->i.TP.thermal_rating);
  fprintf(fp, "%g,", r->i.TP.bottleneck_rating);
  fprintf(fp, "%g,", r->i.TP.emergency_rating);
/* solution data */
  fprintf(fp, "%g,", r->s.branch.Pin);
  fprintf(fp, "%g,", r->s.branch.Qin);
  fprintf(fp, "%g,", r->s.branch.Pout);
  fprintf(fp, "%g,", r->s.branch.Qout);
  fprintf(fp, "%g,", r->s.branch.Ploss);
  fprintf(fp, "%g,", r->s.branch.Qloss);
  fprintf(fp, "%g,", r->s.branch.crit_xfmr_load_mva);
  fprintf(fp, "%g,", r->s.branch.crit_xfmr_rat_mva);
  fprintf(fp, "%c,", r->s.branch.crit_xfmr_rat_code);
  fprintf(fp, "%d,", r->s.branch.crit_xfmr_load_term);
  fprintf(fp, "%g,", r->s.branch.tot_xfmr_load_pct);
  fprintf(fp, "%g,", r->s.branch.tot_xfmr_load_mva);
  fprintf(fp, "%g,", r->s.branch.tap1);
  fprintf(fp, "%g ", r->s.branch.tap2);
  fprintf(fp, "\n");
}
void printR_branch(FILE *fp, pf_rec *r)
{ /* for types R, RN, RQ, RV */
/* input data */
  fprintf(fp, "%c,",     r->i.R.type[0]);
  fprintf(fp, "%c,",     r->i.R.type[1]);
  fprintf(fp, "%s,",     r->i.R.owner);
  fprintf(fp, "\"%s\",", r->i.R.bus1_name);
  fprintf(fp, "%g,",     r->i.R.bus1_kv);
  fprintf(fp, "%d,",     r->i.R.var_tap_side);
  fprintf(fp, "\"%s\",", r->i.R.bus2_name);
  fprintf(fp, "%g,",     r->i.R.bus2_kv);
  fprintf(fp, "\"%s\",", r->i.R.rmt_bus_name);
  fprintf(fp, "%g,",     r->i.R.rmt_bus_kv);
  fprintf(fp, "%g,",     r->i.R.max_tap);
  fprintf(fp, "%g,",     r->i.R.min_tap);
  fprintf(fp, "%d,",     r->i.R.num_taps);
  fprintf(fp, "%g,",     r->i.R.Qmax);
  fprintf(fp, "%g,",     r->i.R.Qmin);
  fprintf(fp, "%s,",     r->i.R.date_in);
  fprintf(fp, "%s ",     r->i.R.date_out);
/* solution data */ /* not applicable */
  fprintf(fp, "\n");
}
void printRMbranch(FILE *fp, pf_rec *r)
{ /* for types RM, RP */
/* input data */
  fprintf(fp, "%c,",     r->i.RM.type[0]);
  fprintf(fp, "%c,",     r->i.RM.type[1]);
  fprintf(fp, "%s,",     r->i.RM.owner);
  fprintf(fp, "\"%s\",", r->i.RM.bus1_name);
  fprintf(fp, "%g,",     r->i.RM.bus1_kv);
  fprintf(fp, "%d,",     r->i.RM.var_tap_side);
  fprintf(fp, "\"%s\",", r->i.RM.bus2_name);
  fprintf(fp, "%g,",     r->i.RM.bus2_kv);
  fprintf(fp, "\"%s\",", r->i.RM.rmt_bus_name);
  fprintf(fp, "%g,",     r->i.RM.rmt_bus_kv);
  fprintf(fp, "%g,",     r->i.RM.max_phase_shift_deg);
  fprintf(fp, "%g,",     r->i.RM.min_phase_shift_deg);
  fprintf(fp, "%d,",     r->i.RM.num_taps);
  fprintf(fp, "%g,",     r->i.RM.Pmax);
  fprintf(fp, "%g,",     r->i.RM.Pmin);
  fprintf(fp, "%s,",     r->i.RM.date_in);
  fprintf(fp, "%s ",     r->i.RM.date_out);
/* solution data */ /* not applicable */
  fprintf(fp, "\n");
}
void printError(pf_rec *r)
{
  char net[256];
  pf_rec_b2a(net, r, "I");
  cf_logErr("Record not available for export: %s\n", net);
  return;
}
void ff_newName(char *oldspec, char *newspec, char *type)
{
  char s[FILENAME_MAX];
  cf_parse(oldspec, s, 'P');  /* PATH */
  strcpy(newspec, s);
  cf_parse(oldspec, s, 'N');  /* NAME */
  strcat(newspec, s);
  strcat(newspec, type);      /* TYPE */
}
void ff_fprintList(FILE *fp, Link *link, char *tag, char *tab, char type, int n)
{ /* n is the number of links printed per row */
  int i;
  pf_rec r;
  char net_data[MAX_IN];
  if (link==NULL) return;
  fprintf(fp, "%s\n", tag);
  for (i=n; link!=NULL; link=link->next) {
    if (i==0) i = n;
    if (i==n) fprintf(fp, "%s", tab);
    if (type=='s') fprintf(fp, "%s", link->data);
    if (type=='f') fprintf(fp, "%6.2f", link->data);
    if (type=='I') {
        pf_rec_b2a(net_data, &r, "I");
        fprintf(fp, "%20.20s", net_data);
    }
    if (type=='i') {
        pf_rec_b2a(net_data, &r, "I");
        fprintf(fp, "%33.33s", net_data);
    }
    if (link->next!=NULL && n!=1 && --i>0) fprintf(fp, ", ");
    else fprintf(fp, "\n");
  }
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
  char str[MAX_IN];
  Link *list;
  int mode = READ_INC; /* default /INCLUDE */
  while (fgets(str, MAX_IN, readMe)!=NULL) { 
    if ( cf_iscomment(str) ) continue;
    cf_str2upper(str);
    if ( strstr(str, "/INCLUDE" )!=NULL ) { mode = READ_INC; continue; }
    if ( strstr(str, "/BASECASE")!=NULL ) { mode = READ_BSE; continue; }
    if ( str[0]=='/' ) { mode = 0; continue; }     /* unrecognized slash card */
    if ( mode == 0 ) continue;         /* note: mode is defaulted to READ_INC */
    if ( mode == READ_INC ) {
        list = cf_text2List(str);
        ff_expList(trace, list, expList);
    }
    if ( mode == READ_BSE ) {
        list = cf_text2List(str);
        if (list!=NULL) trace->query &= ~(QUERY_BASE);
        cf_appList(&trace->basecaseList, list);
    }
  }
  return NULL;
}
/************ documented, common CF_UTIL.H candidates prototypes **************/
/* all documented, CF_UTIL.H candidates shall be designated, cf_name().       */

/******** end documented, common CF_UTIL.H candidates prototypes **************/
