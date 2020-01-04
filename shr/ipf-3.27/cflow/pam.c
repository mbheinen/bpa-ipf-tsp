/******************************************************************************\
UTILITY:    PAM (Power Angle Monitor)
STRUCTURE:  common CFLOW architecture
TYPE:       Powerflow (IPFSRV), bus magnitude, phase; cut-plane P and Q flows
SUMMARY:    Monitor power angle across the system
RELATED:    LineFlow
SEE ALSO:   PAM.FEATURES, LINEFLOW.FEATURES
UPDATED:    April 3, 1997
LANGUAGE:   Standard C.  CFLOW Libraries.  CF_UTIL.H.
DEVELOPER:  William D. Rogers, BPA, TOP, 230-3806, wdrogers@bpa.gov
REQUESTER:  Dmitry Kosterev (TNE), Jules Esztergalyos (TNE), Terry Doern (TOON)
USERS:      Dmitry Kosterev
IPF:        Version 318 or above recommended.
PURPOSE:    Generates reports detailing the system power angle at key points.
            Applies system wide changes, then takes branch or common-mode
            outage and solves.  Reports bus voltage phase and magnitude and
            line P and Q quantities with user-defined cut-plane totals.
\******************************************************************************/
/******************************* #include *************************************/
#include "cflowlib.h"
#include "cf_util.h"
/***************************** end #include ***********************************/
/******************************* #define **************************************/
#ifdef VMS
#define  FILE_KILL        "delete"
#define  FILE_PURG        "purge"
#define  FILE_SUFX        ";*"
#else /* UNIX */
#define  FILE_KILL        "rm"
#define  FILE_PURG        "rm"
#define  FILE_SUFX        ""
#endif

#define  FF_IPF_VERSION   318            /* GPF.EXE_V318 or above recommended */
#define  FF_PAGE_LENGTH    61
#define  DOT_LINE         "..................................................."
#define  LOG_SPEC         "pam.log"
#define  OUT_NAME         "pam"
#define  QUERY_BASE  (int) (1<< 0) /* prompt for basecases */
#define  QUERY_OUTG  (int) (1<< 1) /* prompt for branch-outages */
#define  QUERY_MBUS  (int) (1<< 2) /* prompt for bus list */
#define  QUERY_USRA  (int) (1<< 3) /* prompt for user analysis files */
#define  QUERY_NAME  (int) (1<< 4) /* prompt for output report file name */
#define  QUERY_COMO  (int) (1<< 5) /* prompt for common_mode outages list */
#define  QUERY_CMOD  (int) (1<< 6) /* prompt for common_mode data file */
#define  QUERY_PCHG  (int) (1<< 7) /* prompt for /PRI_CHANGE files */
#define  QUERY_SOLN  (int) (1<< 8) /* prompt for solution qualifiers */
#define  QUERY_CONT  (int) (1<< 9) /* prompt for whether to continue run */
#define  QUERY_CTRL  (int) (1<<10) /* prompt for auxillary control file */
#define  QUERY_CUTP  (int) (1<<11) /* prompt for /CUT_PLANE data */
#define  QUERY_CUTM  (int) (1<<12) /* prompt for /CUT_PLANE master data */
#define  QUERY_OWNR  (int) (1<<13) /* prompt for owners of interest */
#define  QUERY_ZONE  (int) (1<<14) /* prompt for zones of interest */
#define  QUERY_BSKV  (int) (1<<15) /* prompt for base kV of interest */
#define  QUERY_ANGL  (int) (1<<16) /* prompt for angle bus list */
#define  READ_INC    (int)      1  /* found /INCLUDE card in CFLOW data file */
#define  READ_BSE    (int)      2  /* found /BASECAS card in CFLOW data file */
#define  READ_PCH    (int)      3  /* found /PRI_CHA card in CFLOW data file */
#define  READ_REP    (int)      4  /* found /REPORT  card in CFLOW data file */
#define  READ_TRC    (int)      5  /* found /TRACE   card in CFLOW data file */
#define  READ_MON    (int)      6  /* found /MONITORED_BUS   CFLOW data file */
#define  READ_OTG    (int)      7  /* found /OUTAGE  card in CFLOW data file */
#define  READ_SOL    (int)      8  /* found /SOLUTION     in CFLOW data file */
#define  READ_COM    (int)      9  /* found /COMMON_MODE  in CFLOW data file */
#define  READ_CUT    (int)     10  /* found /CUT_PLANE    in CFLOW data file */
#define  READ_OWN    (int)     11  /* found /OWNERS  card in CFLOW data file */
#define  READ_ZON    (int)     12  /* found /ZONE    card in CFLOW data file */
#define  READ_BKV    (int)     13  /* found /BASE_KV card in CFLOW data file */
#define  READ_ANG    (int)     14  /* found /ANGLES  card in CFLOW data file */
#define  OUTG_NONE   (int)      0
#define  OUTG_BRCH   (int)      1
#define  OUTG_COMO   (int)      2
#define  TRACE_NO    (int)      0
#define  TRACE_YES   (int)      1
#define  TYPE_NONE   (int)      0
#define  FAIL_CRIT   (int) (1<< 0) /* critical failure */
#define  FAIL_BASE   (int) (1<< 1)
#define  FAIL_PCHG   (int) (1<< 2)
#define  FAIL_CMMD   (int) (1<< 3)
#define  FAIL_CHNG   (int) (1<< 4)
#define  FAIL_SOLV   (int) (1<< 5)
#define  FAIL_USAN   (int) (1<< 6)
#define  SORT_BY_SOLN  (int)    6
#define  PAGE_BREAK  "************************************************************************************************************************************"
#define  PAGE_MARK   "------------------------------------------------------------------------------------------------------------------------------------"
/***************************** end #define ************************************/
/******************************* typedef **************************************/
typedef struct processStep {
  int                    type;     /* 0 = no outage, 1 = branch, 2 = change */
  char  oldBase[FILENAME_MAX];     /* load this base case */
  char  priChng[FILENAME_MAX];     /* primary change file */
  char  usrRept[FILENAME_MAX];     /* save user analysis with this name */
  void              *outgLink;     /* outage: branch, change, or common_mode */
} Step;

typedef struct traceRecord {
  Link   *baseList;
  Link   *pchgList;
  Link   *outgList;
  Link   *comoList;
  Link   *mbusList;
  Link   *anglList;
  Link   *vlowList;
  Link   *highList;                                    /* highest branch load */
  Link   *trceList;
  Link   *stepList;
  Link   *cutpList;                                         /* cut-plane list */
  Link   *ownrList;                                        /* screen by owner */
  Link   *zoneList;                                         /* screen by zone */
  Link   *bskvList;                                      /* screen by base kv */
  int     stat;
  int     query;
  cf_Out *trc;
  cf_Out *rpt;
  cf_Out *tbl;
  FILE   *comFile;                          /* master common-mode outage file */
  char    usrSpec[FILENAME_MAX];
  char    conSpec[FILENAME_MAX];
  char    comSpec[FILENAME_MAX];
  char    cutSpec[FILENAME_MAX];
  char    outName[FILENAME_MAX];
  char    solution[CF_IPCSIZE];
  char    timeStamp[CF_STRSIZE];
  char    userID[CF_STRSIZE];
  char    IPFversion[CF_STRSIZE];
  char    IPFexecute[CF_STRSIZE];
} Trace;
/******************************* end typedef **********************************/
/* top MIMIC functions - called by main() *************************************/
void  initializeTrace(Trace *trace);
void  processCommandLine(Trace *trace, int argc, char *argv[]);
void  instructions(Trace *trace);
void  checkIPFversion(Trace *trace);
void  promptUser(Trace *trace);
void  checkFileLists(Trace *trace);
void  fillGroups(Trace *trace);
void  assembleSteps(Trace *trace);
void  buildTrace(Trace *trace);
void  printTrace(Trace *trace);
void  queryContinue(Trace *trace);
void  openReport(cf_Out *rpt);
void  dispatchEngine(Trace *trace);
void  finalRemarks(Trace *trace);
void  cleanUpFiles(Trace *trace);
/* end MIMIC functions - called by main() *************************************/

/* top MIMIC functions - report functions *************************************/
void   ff_printMainHeader(cf_Out *rpt, Trace *trace);
void   ff_printMainFooter(cf_Out *rpt);
void   ff_printBlankLines(cf_Out *rpt, int n);
int    ff_printPageFooter(cf_Out *rpt, int m);
int    ff_printErrMessage(cf_Out *rpt, int stat);
void   ff_printOutgReport(cf_Out *rpt, int stat, Step *step);
void   ff_printLoadReport(cf_Out *rpt, int stat, Link *cutpList);
void   ff_printLineReport(cf_Out *rpt, int stat, Link *cutpList);
void   ff_printCutpReport(cf_Out *rpt, int stat, Link *cutpList);
void   ff_printAnglReport(cf_Out *rpt, int stat, Link *anglList);
void   ff_printVoltReport(cf_Out *rpt, int stat, Link *mbusList);
void   ff_printVLowReport(cf_Out *rpt, int stat, Link *vlowList);
void   ff_printHighReport(cf_Out *rpt, int stat, Link *highList, int mode);
void   ff_printSummReport(cf_Out *rpt, int stat, Step *step, Link *mbusList, Link *cutpList);
void   ff_printOutgHeader(cf_Out *rpt);  
void   ff_printLoadHeader(cf_Out *rpt);
void   ff_printLineHeader(cf_Out *rpt);
void   ff_printCutpHeader(cf_Out *rpt);
void   ff_printAnglHeader(cf_Out *rpt);
void   ff_printVoltHeader(cf_Out *rpt);
void   ff_printSummHeader(cf_Out *rpt, Trace *trace, Step *step);
void   ff_printVLowHeader(cf_Out *rpt);
void   ff_printHighHeader(cf_Out *rpt, int mode);
void   ff_printOutgPoints(cf_Out *rpt, int stat, Step *step);
void   ff_printLoadPoints(cf_Out *rpt, int stat, Link *cutpList);
void   ff_printLinePoints(cf_Out *rpt, int stat, Link *cutpList);
void   ff_printCutpPoints(cf_Out *rpt, int stat, Link *cutpList);
void   ff_printAnglPoints(cf_Out *rpt, int stat, Link *mbusList);
void   ff_printVoltPoints(cf_Out *rpt, int stat, Link *mbusList);
void   ff_printVLowPoints(cf_Out *rpt, int stat, Link *vlowList);
void   ff_printHighPoints(cf_Out *rpt, int stat, Link *highList, int mode);
void   ff_printSummPoints(cf_Out *rpt, int stat, Step *step, Link *mbus, Link *cutp);
void   printTableHeader(cf_Out *rpt, Link *curLink, char *l, char *r, char *t);
void   printTableLabels(cf_Out *rpt, Link *curLink, char *l, char *r, char *t);
/* end MIMIC functions - report functions *************************************/

/* top MIMIC functions - support functions ************************************/
void   checkFileList(Link *fileList);
int    findFile(Link *curFile, char *path);
void   de_curve(int *stat, Trace *trace, Step *step, pf_rec *r);
void   de_cut_plane(int *stat, Link *cutpList);
void   de_get_angle(int *stat, Link *anglList);
void   de_bus_angle(int *stat, Link *mbusList);
void   de_highest_load(int *stat, Trace *trace,Link **highList);
void   de_load_oldbase(int *stat, char *file);
int    de_load_changes(int *stat, char *file);
void   de_lowest_PU(int *stat, Trace *trace, Link **vlowList);
void   de_user_analysis(int *stat, char *infile, char *outfile);
void   de_take_brch_outg(int *stat, int type, pf_rec *brnOutg);
void   de_take_como_outg(int *stat, int type, Link *comOutg);
int    de_common_mode_outage(int go, Trace *trace, Step *step);
void   de_command(int *stat, char *cmd, char *data);
void   de_solution(int *stat, char *solution, char *file);
/* end MIMIC functions - support functions ************************************/

/* top LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/
void    ff_stream2List(FILE *readMe, Trace *trace, Link **expList);
void    ff_report(char *s, Trace *trace);
void    ff_limits(char *s, Trace *trace);
void    ff_traces(char *s, Trace *trace);
void    ff_expList(Trace *trace, Link *dataList, Link **expList);
int     ff_cmpPU(Link *newLink, Link *xstLink, int sort);
int     ff_cmpLoad(Link *newLink, Link *xstLink, int sort);
/* end LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/

cf_Style FF_angStyl = { "",   CF_TAG_ALWAYS,  2 };

int main(int argc, char *argv[])
{
  Trace trace;

  time(&CF_time0);
  initializeTrace(&trace);
  processCommandLine(&trace, argc, argv);
  instructions(&trace);
  checkIPFversion(&trace);
  promptUser(&trace);
  checkFileLists(&trace);
  fillGroups(&trace);
  assembleSteps(&trace);
  buildTrace(&trace);
  openReport(trace.trc);
  printTrace(&trace);
  queryContinue(&trace);
  openReport(trace.rpt);
  openReport(trace.tbl);
  pf_cflow_init(argc, argv);      /* initialize cflow connection to powerflow */
  dispatchEngine(&trace);
  pf_cflow_exit();
  finalRemarks(&trace);
  cleanUpFiles(&trace);
  time(&CF_time1);
  cf_logUse("PAM", trace.IPFversion, trace.userID);
  return 0;
}
/* end of main() */
void  initializeTrace(Trace *trace)
{
  memset(trace, '\0', sizeof(trace));
  trace->baseList         =   NULL;
  trace->pchgList         =   NULL;
  trace->outgList         =   NULL;
  trace->mbusList         =   NULL;
  trace->anglList         =   NULL;
  trace->vlowList         =   NULL;
  trace->highList         =   NULL;
  trace->trceList         =   NULL;
  trace->stepList         =   NULL;
  trace->comoList         =   NULL;
  trace->cutpList         =   NULL;
  trace->ownrList         =   NULL;
  trace->zoneList         =   NULL;
  trace->bskvList         =   NULL;
  trace->stat             =   0;
  trace->query            = ( QUERY_BASE | QUERY_OUTG | QUERY_MBUS |
                              QUERY_OWNR | QUERY_ZONE | QUERY_BSKV |
                              QUERY_USRA | QUERY_NAME | QUERY_COMO |
                              QUERY_CMOD | QUERY_PCHG | QUERY_SOLN |
                              QUERY_CONT | QUERY_CTRL | QUERY_CUTP |
			      QUERY_CUTM | QUERY_ANGL );
  strcpy(trace->solution, "/SOLUTION\n");
  strncpy(trace->IPFexecute, getenv("IPFSRV_CF"), CF_STRSIZE);
  cf_parse(trace->IPFexecute, trace->IPFversion, 'F');
  cuserid(trace->userID);
  cf_time(trace->timeStamp, CF_STRSIZE, CF_TIMESTAMP);
  strcpy(CF_logSpec, LOG_SPEC);
  strcpy(trace->outName, OUT_NAME);
  trace->trc = cf_initOut("", trace->timeStamp, TRACE_YES, 0);
  trace->rpt = cf_initOut("", trace->timeStamp, TYPE_NONE, 0);
  trace->tbl = cf_initOut("", trace->timeStamp, TYPE_NONE, 0);
}
void processCommandLine(Trace *trace, int argc, char *argv[])
/* note: could generalize by passing in a default list to use */
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
  printf("\n                      Welcome to PAM - updated 04-03-97");
  printf("\n");
  printf("\n - Defaults in brackets [].  Press Ctrl-Y & type STOP to abort.");
  printf("\n - Use .trc, .dat, or .lis or append / to designate data files.");
  printf("\n - Warning: %s.PFD and %s.PFO deleted.", trace->userID, trace->userID);
  printf("\n - IPF Executable Used: IPFSRV_CF == \"%s\"", trace->IPFexecute);
  printf("\n");
}
void checkIPFversion(Trace *trace)
{
  char *vp;
  int   vn;

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
    printf("\a\n - - - - - - - - - - - -  Warning  - - - - - - - - - - - -\a\n");
    printf(" PAM was tested for use with IPF version %d.\n",FF_IPF_VERSION);
    printf(" You are currently using IPF version %d.\n", vn);
    printf(" PAM may not work as expected with IPF version %d.\n", vn);
    printf("\a - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\a\n");
  }
}
void promptUser(Trace *trace)
{
  char query[CF_INBUFSIZE];
  Link *list, *row, *link;

  printf("%s", DOT_LINE);
  if (trace->query & QUERY_BASE) {
    printf("\n\n/BASECASE, /TRACE, or /INCLUDE");
    cf_nprompt("\n > Enter list of Basecase, *.TRC, or *.DAT files : ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->baseList);
  }
  if (trace->baseList==NULL && trace->stepList==NULL) {
    cf_exit(1, "No data or basecase files!  Quitting!\n");
  }

  if (trace->query == QUERY_CONT) trace->query &= ~(QUERY_CONT);

  if (trace->query & QUERY_PCHG) {
    printf("\n\n/PRI_CHANGE");
    cf_nprompt("\n > Enter list of Primary Change (or *.DAT) files : ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->pchgList);
  }
  if (trace->query & QUERY_MBUS) {
    printf("\n\n/MONITORED_BUS");
    cf_nprompt("\n > Enter file of busses to monitor               : ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->query & QUERY_MBUS) {
    printf(    "\n > Enter list of busses to monitor               : ");
    printf(    "\n   > Btc<O>< Bus  ><KV>");
    do {
        cf_nprompt("\n   > ", "", CF_INBUFSIZE, query);
        list = cf_rec2Link(query);
        cf_appList(&trace->mbusList, list);
    } while (!cf_isblank(query));
  }
  if (trace->query & QUERY_ANGL) {
    printf("\n\n/ANGLES");
    cf_nprompt("\n > Enter file of bus angle differences to monitor: ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->query & QUERY_ANGL) {
    printf(    "\n > Enter list of bus angle differences to monitor: ");
    printf(    "\n   > Btc<O>< Bus  ><KV>ZN, Btc<O>< Bus  ><KV>ZN");
    do {
        cf_nprompt("\n   > ", "", CF_INBUFSIZE, query);
        query[21] = '\0';
        link = cf_rec2Link(&query[0]);
        row = cf_link2row(link);
        list = (Link *) row->data;                
        link = cf_rec2Link(&query[22]);
        cf_appList(&list, link);
        cf_appList(&trace->anglList, row);
    } while (!cf_isblank(query));
  }
  if (trace->query & QUERY_CUTM) {
    printf("\n\n/REPORT");
    cf_sprompt("\n > Enter main cut-plane file,   CUT_PLANE_DATA = : ", "", trace->cutSpec);
  }
  if (trace->query & QUERY_CUTP) {
    printf("\n\n/CUT_PLANE"); 
    cf_nprompt("\n > Enter file of cut-plane identifiers/branches  :", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->query & QUERY_CUTP) {
    printf(    "\n > Enter list of cut-plane identifiers/branches  :");
    printf(    "\n   : Lyc<O>< Bus1 ><V1> < Bus2 ><V2>C");
    list = NULL;
    do {
        cf_nprompt("\n   : ", "", CF_INBUFSIZE, query);
        if (cf_isblank(query)) break;
        cf_str2upper(query);
        if (strstr(query, "CUT-PLANE")) {
            link = cf_tag2link(query);
            row = cf_link2row(link);
            cf_appList(&trace->cutpList, row);
            list = (Link *) row->data;                
            continue;
        }
        else {
            if (list==NULL) {
                link = cf_tag2link("> cut-plane");
                row = cf_link2row(link);
                cf_appList(&trace->cutpList, row);
                list = (Link *) row->data;                
            }
            link = cf_rec2Link(query);
            cf_appList(&list, link);
        }
    } while (1);
  }
  if (trace->query & (QUERY_OWNR | QUERY_ZONE | QUERY_BSKV)) {
    printf("\n/OWNER, /ZONE, /BASE_KV");
  }
  if (trace->query & QUERY_OWNR) {
    cf_nprompt("\n > Enter owners of interest.....[all owners='%s']: ", "***", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    cf_appList(&trace->ownrList, list);
  }
  if (trace->query & QUERY_ZONE) {
    cf_nprompt("\n > Enter zones of interest........[all zones='%s']: ", "**", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    cf_appList(&trace->zoneList, list);
  }
  if (trace->query & QUERY_BSKV) {
    cf_nprompt("\n > Enter base_kv of interest....[all base kV='%s']: ", ">0", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    cf_appList(&trace->bskvList, list);
  }
  if (trace->query & (QUERY_OWNR | QUERY_ZONE | QUERY_BSKV)) {
    printf("\n");
  }
  if (trace->query & QUERY_OUTG) {
    printf("\n\n/OUTAGE");
    cf_nprompt("\n > Enter file of branches for single-line outages: ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->query & QUERY_OUTG) {
    printf(    "\n > Enter list of branches for single-line outages: ");
    printf(    "\n   > Tyc<O>< Bus1 ><V1> < Bus2 ><V2>C");
    do {
        cf_nprompt("\n   > ", "", CF_INBUFSIZE, query);
        list = cf_id2Link(query, 'I');
        cf_appList(&trace->outgList, list);
    } while (!cf_isblank(query));
  }
  if (trace->query & QUERY_CMOD) {
    printf("\n\n/REPORT");
    cf_sprompt("\n > Enter main common-mode file, COMMON_MODE_DATA=: ", "", trace->comSpec);
  }
  if (trace->query & QUERY_COMO) {
    printf("\n\n/COMMON_MODE"); 
    cf_nprompt("\n > Enter file of common-mode outages/identifiers : ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->query & QUERY_COMO) {
    printf(    "\n > Enter list of common-mode outages/identifiers : ");
    printf(    "\n   : Tyc<O>< Bus1 ><V1> < Bus2 ><V2>C");
    list = NULL;
    do {
        cf_nprompt("\n   : ", "", CF_INBUFSIZE, query);
        if (cf_isblank(query)) break;
        cf_str2upper(query);
        if (strstr(query, "MODE")) {
            link = cf_tag2link(query);
            row = cf_link2row(link);
            cf_appList(&trace->comoList, row);
            list = (Link *) row->data;                
            continue;
        }
        else {
            if (list==NULL) {
                link = cf_tag2link("> mode outage");
                row = cf_link2row(link);
                cf_appList(&trace->comoList, row);
                list = (Link *) row->data;
            }
            link = cf_text2Link(query);
            cf_appList(&list, link);
        }
    } while (1);
  }
  if (trace->query & (QUERY_USRA | QUERY_NAME | QUERY_CTRL)) {
    printf("\n\n/REPORT");
  }
  if (trace->query & QUERY_NAME) {
    cf_sprompt("\n > Enter output files default name,  NAME = [%s]: ", trace->outName, trace->outName);
  }
  if (trace->query & QUERY_USRA) {
    cf_sprompt("\n > Enter user analysis file,     USER_ANALYSIS = : ", "", trace->usrSpec);
  }
  if (trace->query & QUERY_CTRL) {
    cf_sprompt("\n > Enter auxillary control file,  CONTROL_FILE = : ", "", trace->conSpec);
  }
  if (trace->query & QUERY_SOLN) {
    printf("\n\n/SOLUTION"); 
    cf_nprompt("\n > Enter file of solution parameters/qualifiers  : ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->query & QUERY_SOLN) {
    printf("\n\n/SOLUTION       (Enter /solution qualifier and parameters)");
    cf_nprompt("\n: ", "", CF_INBUFSIZE, query);
    while (!cf_isblank(query)) {
        cf_aprint(trace->solution, "%s\n", query);
        cf_nprompt("\n: ", "", CF_INBUFSIZE, query);
    }
  }
  printf("\n");
  cf_strsuf(trace->trc->spec, trace->outName, '.', ".trc");
  cf_strsuf(trace->rpt->spec, trace->outName, '.', ".rpt");
  cf_strsuf(trace->tbl->spec, trace->outName, '.', ".tbl");
  printf("\n%s\n", DOT_LINE);
  return;
}
void printTrace(Trace *trace)
{
  FILE *fp;
  Link *row, *list;

  if (trace->trc->type==TRACE_NO) return;
  fp = trace->trc->file;
  if (fp == NULL) fp = stderr;
  fprintf(fp, ". %s %s %s %s\n", trace->trc->spec, trace->trc->time, trace->userID,
    trace->IPFversion);

  fprintf(fp, "/REPORT\n");
  fprintf(fp, "  NAME = %s\n", trace->outName);
  fprintf(fp, "  USER_ANALYSIS = %s\n", trace->usrSpec);
  fprintf(fp, "  CONTROL_FILE = %s\n", trace->conSpec);
  fprintf(fp, "  COMMON_MODE_DATA = %s\n", trace->comSpec);
  fprintf(fp, "  CUT_PLANE_DATA = %s\n", trace->cutSpec);
  if (trace->trc->type==TRACE_YES) fprintf(fp, "  TRACE = YES\n");
  if (trace->trc->type==TRACE_NO ) fprintf(fp, "  TRACE = NO\n");

  cf_printList(fp, trace->ownrList, CF_dznStyl, "/OWNER\n");
  cf_printList(fp, trace->zoneList, CF_dznStyl, "/ZONE\n");
  cf_printList(fp, trace->bskvList, CF_sixStyl, "/BASE_KV\n");

  if ( trace->solution[0]!=0 ) {
    fprintf(fp, "%s", trace->solution);
  }

  if (trace->baseList==NULL) {
    cf_printList(fp,  trace->mbusList, CF_recStyl, "/MONITORED_BUS\n");
    cf_printGroup(fp, trace->anglList, FF_angStyl, "/ANGLES\n.tc<O>< Bus  ><KV>ZN, Btc<O>< Bus  ><KV>ZN\n");
    cf_printGroup(fp, trace->cutpList, CF_recStyl, "/CUT_PLANE\n");
    cf_printList(fp,  trace->trceList, CF_oneStyl, "/TRACE\n");
  }
  else {
    cf_printList(fp,  trace->baseList, CF_oneStyl, "/BASECASE\n");
    cf_printList(fp,  trace->pchgList, CF_oneStyl, "/PRI_CHANGE\n");
    cf_printList(fp,  trace->mbusList, CF_recStyl, "/MONITORED_BUS\n");
    cf_printGroup(fp, trace->anglList, FF_angStyl, "/ANGLES\n.tc<O>< Bus  ><KV>ZN, Btc<O>< Bus  ><KV>ZN\n");
    cf_printGroup(fp, trace->cutpList, CF_recStyl, "/CUT_PLANE\n");
    cf_printList(fp,  trace->outgList, CF_recStyl, "/OUTAGE\n");
    cf_printGroup(fp, trace->comoList, CF_recStyl, "/COMMON_MODE\n");
    cf_printList(fp,  trace->trceList, CF_dotStyl, "./TRACE\n");
  }

  fclose(fp);
}
void  checkFileLists(Trace *trace)
{
  checkFileList(trace->baseList);
  checkFileList(trace->pchgList);
}
void  fillGroups(Trace *trace)
{
  cf_fillGroup(trace->comSpec, &(trace->comoList), "MODE");
  cf_fillGroup(trace->cutSpec, &(trace->cutpList), "CUT-PLANE");
}
void  assembleSteps(Trace *trace)
{
  Link   *stepLink;
  Link   *curBase, *curPchg, *curBrch, *curComo;
  Step   *step;
  int     bran, como, outg, type;
  char    baseName[FILENAME_MAX], pchgName[FILENAME_MAX], comoName[CF_STRSIZE];
  pf_rec *brch;

/* test mimic function : Loop through all cases in base case list */
  if (trace->stepList != NULL) return;
  stepLink= trace->stepList;
  curBase = trace->baseList;
  curPchg = trace->pchgList;
  curBrch = trace->outgList;
  curComo = trace->comoList;
  bran = 1; como = 1; outg = 1; type = 'X';
  while (curBase != NULL) {
    stepLink = cf_addLink(stepLink, sizeof(Step));
    step = (Step *) stepLink->data;
    if (trace->stepList==NULL) trace->stepList = stepLink;

    strcpy(step->oldBase, (char *) curBase->data);

    if (curPchg!=NULL) {
        strcpy(step->priChng, (char *) curPchg->data);
    }
    if (curBrch!=NULL) {
        step->outgLink = cf_malloc(sizeof(pf_rec));
        cf_branch2rec((cf_Branch *) curBrch->data, (pf_rec *) step->outgLink);
        step->type = OUTG_BRCH;
        type = 'B';
        outg = bran;
    }
    else if (curComo!=NULL) {
        step->outgLink = (Link *) curComo->data;
        step->type = OUTG_COMO;
        type = 'C';
        outg = como;
    }
    else {
        step->type = OUTG_NONE;
    }

    cf_parse(step->oldBase, baseName, 'N');
    if ( strlen(step->priChng)>0 ) {
        cf_parse(step->priChng, pchgName, 'N');
        cf_aprint(baseName, "-%.10s", pchgName);
    }
    if (trace->usrSpec[0]!='\0') {
        if (type=='X') {
            sprintf(step->usrRept, "%.17s.usr", baseName);
        }
        else {
            sprintf(step->usrRept, "%.17s-%c%02d.usr",
                baseName, type, outg);
        }
    }

/* APLY code */
    if      (curBrch!=NULL) { curBrch = curBrch->next; bran++; }
    else if (curComo!=NULL) { curComo = curComo->next; como++; }
    if (curBrch==NULL && curComo==NULL) {
        if (curPchg!=NULL) curPchg = curPchg->next;
    }
    if (curBrch==NULL && curComo==NULL && curPchg==NULL) {
        curBase = curBase->next;
    }
    if (curBrch==NULL && curComo==NULL) {
        curBrch = trace->outgList; bran = 1;
        curComo = trace->comoList; como = 1;
    }
    if (curPchg==NULL) {
        curPchg = trace->pchgList;
    }
/* end APLY */
  }
}
void queryContinue(Trace *trace)
{
  int yes;

  if (trace->trc->type == TRACE_YES)
    printf("\nTrace written to %s", trace->trc->spec);
  if (CF_logFile != NULL)
    printf("\nError Summary written to %s", CF_logSpec);
  printf("\n%s\n", DOT_LINE);
  cf_printList(stdout, trace->trceList, CF_oneStyl, "/TRACE\n");
  printf("%s\n", DOT_LINE);
  if ( trace->query & QUERY_CONT) {
    printf(" Please examine the above /TRACE of PAM operations.");
    yes = cf_yprompt("\n > Do you want to continue this run? [%c]: ", 'Y');
    cf_exit(!yes, "Quitting!");
  }
}
void openReport(cf_Out *rpt)
{
  rpt->file = cf_openFile(rpt->spec, "w");
  cf_exit(rpt->file==NULL, "Quitting!\n");
}
void  dispatchEngine(Trace *trace)
{
  Step *step;
  Link *stepLink;
  char  lastBase[FILENAME_MAX], lastPchg[FILENAME_MAX];

/**************** BEGINNING OF POWERFLOW DISPATCH ENGINE **********************/
  ff_printMainHeader(trace->rpt, trace);

  lastBase[0] = '\0';
  lastPchg[0] = '\0';

  printf("Processing... \n");
  for (stepLink = trace->stepList; stepLink != NULL; stepLink = stepLink->next){
    step = (Step *) stepLink->data;
    trace->stat = 0;

    cleanUpFiles(trace);

    de_load_oldbase(&trace->stat, step->oldBase);
    de_command(&trace->stat, "/INCLUDE, FILE=", trace->conSpec);
    de_load_changes(&trace->stat, step->priChng);

    de_take_brch_outg(&trace->stat, step->type, step->outgLink);
    de_take_como_outg(&trace->stat, step->type, step->outgLink);

    de_solution(&trace->stat, trace->solution, step->oldBase);

    de_user_analysis(&trace->stat, trace->usrSpec, step->usrRept);
    de_cut_plane(&trace->stat, trace->cutpList);
    de_bus_angle(&trace->stat, trace->mbusList);
    de_get_angle(&trace->stat, trace->anglList);
    de_lowest_PU(&trace->stat, trace, &trace->vlowList);
    de_highest_load(&trace->stat, trace, &trace->highList);

    ff_printOutgReport(trace->rpt, trace->stat, step);
    ff_printCutpReport(trace->rpt, trace->stat, trace->cutpList);
    ff_printAnglReport(trace->rpt, trace->stat, trace->anglList);
    ff_printLineReport(trace->rpt, trace->stat, trace->cutpList);
    ff_printHighReport(trace->rpt, trace->stat, trace->highList, 115);
    ff_printHighReport(trace->rpt, trace->stat, trace->highList, 230);
    ff_printHighReport(trace->rpt, trace->stat, trace->highList, 345);
    ff_printHighReport(trace->rpt, trace->stat, trace->highList, 500);
    ff_printHighReport(trace->rpt, trace->stat, trace->highList, 'T');
    ff_printVLowReport(trace->rpt, trace->stat, trace->vlowList);
    ff_printVoltReport(trace->rpt, trace->stat, trace->mbusList);
/*
    ff_printLoadReport(trace->rpt, trace->stat, trace->cutpList);
*/
    ff_printPageFooter(trace->rpt, FF_PAGE_LENGTH);

    if (strcmp(lastBase, step->oldBase) || strcmp(lastPchg, step->priChng)) {
        ff_printSummHeader(trace->tbl, trace, step);
    }
    ff_printSummReport(trace->tbl, trace->stat, step, trace->mbusList,
        trace->cutpList);

    strcpy(lastBase, step->oldBase);
    strcpy(lastPchg, step->priChng);
  }
  ff_printMainFooter(trace->rpt);

  return;
/**************** END OF POWERFLOW DISPATCH ENGINE ****************************/
}
void  finalRemarks(Trace *trace)
{
  printf("\n");
  printf("\nMemory allocation (bytes): Cur:%d Max:%d Alloc:%d Freed:%d\n",
    CF_memCurAlloc, CF_memMaxAlloc, CF_memTotAlloc, CF_memTotFreed);
  printf("\nOutput report written to %s", trace->rpt->spec);
  printf("\nTable  report written to %s", trace->tbl->spec);
  if (trace->trc->type == TRACE_YES)
    printf("\nTrace  report written to %s", trace->trc->spec);
  if (CF_logFile != NULL) printf("\nError  report written to %s", CF_logSpec);
  printf("\n");
}
void  cleanUpFiles(Trace *trace)
{
  char s[CF_OUTBUFSIZE];

  sprintf(s, "%s %s.PFO", FILE_PURG, trace->userID); system(s);
  sprintf(s, "%s %s.PFD", FILE_PURG, trace->userID); system(s);
}
void de_user_analysis(int *stat, char *infile, char *outfile)
{
  char cmd[CF_IPCSIZE];

  if (*stat & FAIL_CRIT) return;

  if (strlen(infile)==0) return;
/* apply user-analysis file */

  sprintf(cmd,"/report,select user_analysis,file=%s,output=%s",infile, outfile);
  printf("  %s\n", cmd);
  if (pf_command(cmd)!=0) {
    cf_logErr("Skipping changes:  Cannot use %s\n", infile);
    *stat |= FAIL_USAN;
    return;
  }
/*
  if (pf_user_report(trace->usrSpec, step->usrRept, action)!=0) {
    cf_logErr("Skipping changes:  Cannot use %s\n", trace->usrSpec);
    return;
  }
*/
  printf("  Writing User Analysis report to %s ...\n", outfile);
}
void de_cut_plane(int *stat, Link *cutpList)
{
  Link  *curCutp, *curLine;
  pf_rec *b;

  if (*stat & FAIL_CRIT) return;
  if (cutpList==NULL) return;
  printf("  Getting cut-plane flows...\n");
  for (curCutp=cutpList; curCutp!=NULL; curCutp=curCutp->next) {
    curLine = (Link *) curCutp->data; curLine = curLine->next;
    for ( ; curLine!=NULL; curLine=curLine->next) {
        if (curLine->kind==CF_KIND_REC) {
            pf_rec_branch(curLine->data, "G");
            memset(&((pf_rec *)curLine->data)->s.branch, '\0', sizeof(pf_branch_soln));
            pf_rec_branch(curLine->data, "O");
        }
    }
  }
  return;
}
void de_bus_angle(int *stat, Link *mbusList)
{
  Link  *curMbus;

  if (*stat & FAIL_CRIT) return;
  if (mbusList==NULL) return;
  printf("  Getting bus quantities...\n");
  for (curMbus=mbusList; curMbus!=NULL; curMbus=curMbus->next) {
    pf_rec_bus((pf_rec *) curMbus->data, "G");
    memset(&((pf_rec *)curMbus->data)->s.ACbus, '\0', sizeof(pf_bus_AC_soln));
    pf_rec_bus((pf_rec *) curMbus->data, "O");
  }
  return;
}
void de_get_angle(int *stat, Link *anglList)
{
  Link  *curAngl, *curAbus;

  if (*stat & FAIL_CRIT) return;
  if (anglList==NULL) return;
  printf("  Getting bus angles...\n");
  for (curAngl=anglList; curAngl!=NULL; curAngl=curAngl->next) {
    curAbus = (Link *) curAngl->data;
    for ( ; curAbus!=NULL; curAbus=curAbus->next) {
        if (curAbus->kind==CF_KIND_REC)
            pf_rec_bus(curAbus->data, "G");
            memset(&((pf_rec *)curAbus->data)->s.ACbus, '\0', sizeof(pf_bus_AC_soln));
            pf_rec_bus(curAbus->data, "O");
    }
  }
  return;
}
void de_highest_load(int *stat, Trace *trace, Link **highList)
{
  int    e, nRec;
  pf_rec r;
  Link  *newLink, *curLink;
  char  id[CF_RECSIZE];

  if (*stat & FAIL_CRIT) return;
  printf("  Finding 10 Most Heavily Loaded Branches (%% of Rating)...\n");
  nRec = 0;
  cf_freeList(*highList);
  curLink = NULL;
  for (e = pf_rec_branch(&r, "F"); e == 0; e = pf_rec_branch(&r, "N")) {
    if (r.i.branch.type[0]=='R') continue;
    if (cf_rec_l2h(&r.i.branch) != 1) continue;
    pf_rec_b2a(id, &r, "I");
/*    printf("%33.33s\n", id); */

    if (!cf_validOwner(trace->ownrList, r.i.branch.owner)) continue;
    if (!cf_validBasekv(trace->bskvList, r.i.branch.bus1_kv, r.i.branch.bus2_kv)) continue;
    newLink = cf_newLink(sizeof(pf_rec));
    if (newLink==NULL || newLink->data==NULL) continue;
    memcpy(newLink->data, &r, sizeof(pf_rec));
    pf_rec_bus((pf_rec *) newLink->data, "O");
    cf_insLink(highList, &curLink, newLink, (long) SORT_BY_SOLN, CF_INS_AFTER,
        (int (*)(Link*, Link*, int))(ff_cmpLoad));

    nRec++;
  }
  cf_logErr("Total of %d branches.\n", nRec);
  return;
}
void de_lowest_PU(int *stat, Trace *trace, Link **vlowList)
{
  int    e, nRec;
  pf_rec r;
  Link  *newLink, *curLink;
  char  id[CF_RECSIZE];

  if (*stat & FAIL_CRIT) return;
  printf("  Finding 10 Lowest bus voltages (in P.U.)...\n");
  nRec = 0;
  cf_freeList(*vlowList);
  curLink = NULL;
  for (e = pf_rec_bus(&r, "F"); e == 0; e = pf_rec_bus(&r, "N")) {
/*
    pf_rec_b2a(id, &r, "I");
    printf("%20.20s\n", id);
*/
    if (!cf_validOwner(trace->ownrList, r.i.ACbus.owner)) continue;
    if (!cf_validZone(trace->zoneList, r.i.ACbus.zone, r.i.ACbus.zone)) continue;
    if (!cf_validBasekv(trace->bskvList, r.i.ACbus.kv, r.i.ACbus.kv)) continue;
    newLink = cf_newLink(sizeof(pf_rec));
    if (newLink==NULL || newLink->data==NULL) continue;
    memcpy(newLink->data, &r, sizeof(pf_rec));
    pf_rec_bus((pf_rec *) newLink->data, "O");
    cf_insLink(vlowList, &curLink, newLink, (long) SORT_BY_SOLN, CF_INS_AFTER,
        (int (*)(Link*, Link*, int))(ff_cmpPU));

    nRec++;
  }
  cf_logErr("Total of %d buses.\n", nRec);
  return;
}
void checkFileList(Link *fileList)
{
  Link *curFile;
  FILE *fp;
  char *name, spec[FILENAME_MAX];
  for (curFile=fileList; curFile!=NULL; curFile=curFile->next) {
    name = (char *) curFile->data;
    if (findFile(curFile, ""             )>=0) continue;
    if (findFile(curFile, "IPF_TDAT:"    )>=0) continue; /*cor files need this*/
    if (findFile(curFile, "BASECASE_DIR:")>=0) continue; /* ipf does this     */
    if (findFile(curFile, "WSCCEOFC_DIR:")>=0) continue; /* progressive search*/
    if (findFile(curFile, "WSCCBASE_DIR:")>=0) continue; /* can eliminate this*/
    cf_logErr(" Warning - Can't find file: %s\n", name);
  }
}
int findFile(Link *curFile, char *path)
{
  FILE *fp;
  char name[FILENAME_MAX], spec[FILENAME_MAX];
  strcpy(name, curFile->data);
  if (strlen(name)==0) return 1;
  sprintf(spec, "%s%s", path, name);
  if ((fp=fopen(spec, "r"))==NULL) return -1;           /* test for existance */
  else {
    fclose(fp);
    cf_free(curFile->data, curFile->size);
    curFile->size = strlen(spec) + 1;            /* resize link->data element */
    curFile->data = cf_malloc(curFile->size);
    strcpy(curFile->data, spec);                        /* spec = path + name */
    if (strlen(path)>0) cf_logErr(" File %s found at %s\n", name, spec); 
  }
  return 0;
}
void buildTrace(Trace *trace)
{
  Link *curLink, *stepLink;
  Step *step;
  char  newLine[1024], net_data[132];

  if (trace->trceList!=NULL) return;
  curLink = NULL;
  for (stepLink = trace->stepList; stepLink!=NULL; stepLink = stepLink->next ) {
    step = (Step *) stepLink->data;
    sprintf(newLine, "%s ", step->oldBase);

    if (strlen(step->priChng)!=0) {
        cf_aprint(newLine, "+ %s ", step->priChng);
    }
    if (step->type==OUTG_BRCH) {
        pf_rec_b2a(net_data, (pf_rec *) step->outgLink, "I");
        net_data[2] = 'D';
        cf_strsub(net_data, " ", '#');
        cf_aprint(newLine, "- [%26.26s] ", &net_data[6]);
    }
    if (step->type==OUTG_COMO) {
        cf_link2tagName((Link *)step->outgLink, net_data);
        cf_strsub(net_data, " ", '#');
        cf_aprint(newLine, "- {%s} ", net_data);
    }
    if (trace->usrSpec[0]!='\0') {
        cf_aprint(newLine, "& %s ", trace->usrSpec);
    }
    if (step->usrRept[0]!='\0') {
        cf_aprint(newLine, "> %s ", step->usrRept);
    }
    curLink = cf_addLink(curLink, (size_t) strlen(newLine) + 1);
    if (curLink!=NULL) {
        strcpy((char *) curLink->data, newLine);
        curLink->kind = CF_KIND_STR;
    }
    if (trace->trceList==NULL) trace->trceList = curLink;
  }
  return;
}
/* end function definitions */
/* proposed CF_UTIL.H functions */
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
void ff_stream2List(FILE *readMe, Trace *trace, Link **expList)
{
  char str[CF_INBUFSIZE], STR[CF_INBUFSIZE], *cp;
  Link *list, *link, *row;
  int mode = READ_INC;/* default /INCLUDE */
  while (fgets(str, CF_INBUFSIZE, readMe)!=NULL) { 
    if ( cf_iscomment(str) ) continue;
    strcpy(STR, str);
    cf_str2upper(STR);
    if ( strstr(STR,"/INCLUDE"      )!=NULL ) { mode = READ_INC; }
    if ( strstr(STR,"/REPORT"       )!=NULL ) { mode = READ_REP; }
    if ( strstr(STR,"/BASECASE"     )!=NULL ) { mode = READ_BSE; }
    if ( strstr(STR,"/OWNER"        )!=NULL ) { mode = READ_OWN; }
    if ( strstr(STR,"/ZONE"         )!=NULL ) { mode = READ_ZON; }
    if ( strstr(STR,"/BASE_KV"      )!=NULL ) { mode = READ_BKV; }
    if ( strstr(STR,"/MONITORED_BUS")!=NULL ) { mode = READ_MON; trace->query &= ~(QUERY_MBUS); }
    if ( strstr(STR,"/ANGLES"       )!=NULL ) { mode = READ_ANG; trace->query &= ~(QUERY_ANGL); }
    if ( strstr(STR,"/PRI_CHANGE"   )!=NULL ) { mode = READ_PCH; trace->query &= ~(QUERY_PCHG); }
    if ( strstr(STR,"/OUTAGE"       )!=NULL ) { mode = READ_OTG; trace->query &= ~(QUERY_OUTG); }
    if ( strstr(STR,"/COMMON_MODE"  )!=NULL ) { mode = READ_COM; trace->query &= ~(QUERY_COMO); }
    if ( strstr(STR,"/CUT"          )!=NULL ) { mode = READ_CUT; trace->query &= ~(QUERY_CUTP); }
    if ( strstr(STR,"/SOLUTION"     )!=NULL ) { mode = READ_SOL; trace->query &= ~(QUERY_SOLN); }
    if ( strstr(STR,"/TRACE"        )!=NULL ) { mode = READ_TRC; }
    if ( mode == 0 ) continue;         /* note: mode is defaulted to READ_INC */
    if ( STR[0]=='/' ) {
        if ( (cp=strpbrk(str, "|,"))!=NULL ) strcpy(str, (++cp));
        else continue;
    }
    if ( mode == READ_INC ) {
        list = cf_text2List(str);
        ff_expList(trace, list, expList);
    }
    if ( mode == READ_BSE ) {
        list = cf_text2List(str);
        if (list!=NULL) trace->query &= ~(QUERY_BASE);
        cf_appList(&trace->baseList, list);
    }
    if ( mode == READ_PCH ) {
        list = cf_text2List(str);
        cf_appList(&trace->pchgList, list);
        trace->query &= ~(QUERY_PCHG);
    }
    if ( mode == READ_OWN ) {
        list = cf_text2List(str);
        if (list!=NULL) {
            trace->query &= ~(QUERY_OWNR);
        }
        cf_appList(&trace->ownrList, list);
    }
    if ( mode == READ_ZON ) {
        list = cf_text2List(str);
        if (list!=NULL) {
            trace->query &= ~(QUERY_ZONE);
        }
        cf_appList(&trace->zoneList, list);
    }
    if ( mode == READ_BKV ) {
        list = cf_text2List(str);
        if (list!=NULL) {
            trace->query &= ~(QUERY_BSKV);
        }
        cf_appList(&trace->bskvList, list);
    }
    if ( mode == READ_OTG ) {
        list = cf_id2Link(str, 'I');
        cf_appList(&trace->outgList, list);
        if (list!=NULL) {
            trace->query &= ~(QUERY_OUTG);
            trace->query &= ~(QUERY_COMO);
        }
    }
    if ( mode == READ_COM ) {
        if (strstr(STR, "MODE")) {
            link = cf_tag2link(str);
            row = cf_link2row(link);
            cf_appList(&trace->comoList, row);
            list = (Link *) row->data;                
        }
        else {
            if (list==NULL) {
                link = cf_tag2link("> mode");
                row = cf_link2row(link);
                cf_appList(&trace->comoList, row);
                list = (Link *) row->data;                
            }
            link = cf_text2Link(str);
            cf_appList(&list, link);
        }
        trace->query &= ~(QUERY_OUTG);
        trace->query &= ~(QUERY_COMO);
    }
    if ( mode == READ_CUT ) {
        if (strstr(STR, "CUT-PLANE")) {
            link = cf_tag2link(str);
            row = cf_link2row(link);
            cf_appList(&trace->cutpList, row);
            list = (Link *) row->data;                
        }
        else {
            if (list==NULL) {
                link = cf_tag2link("> cut-plane");
                row = cf_link2row(link);
                cf_appList(&trace->cutpList, row);
                list = (Link *) row->data;
            }
            link = cf_rec2Link(str);
            cf_appList(&list, link);
        }
        trace->query &= ~(QUERY_CUTP);
    }
    if ( mode == READ_MON ) {
        list = cf_rec2Link(str);
        cf_appList(&trace->mbusList, list);
        trace->query &= ~(QUERY_MBUS);
    }
    if ( mode == READ_ANG ) {
        str[21] = '\0';
        link = cf_rec2Link(&str[0]);
        row = cf_link2row(link);
        list = (Link *) row->data;
        link = cf_rec2Link(&str[22]);
        cf_appList(&list, link);
        cf_appList(&trace->anglList, row);
        trace->query &= ~(QUERY_ANGL);
    }
    if ( mode == READ_SOL ) {
        strcat(trace->solution, str);
        strcat(trace->solution, "\n");
        trace->query &= ~(QUERY_SOLN);
    }
    if ( mode == READ_REP ) ff_report(str, trace);
    if ( mode == READ_TRC ) ff_traces(str, trace);
  }
  return;
}
void ff_traces(char *s, Trace *trace)
{
  Step *step;
  Link *stepLink, *row;
  char  op, *cp;

  char  name[132], tag[CF_STRSIZE];
  char  type[3], n1[9], v1[5], n2[9], v2[5], ct;
  int   conv;
  float kv1, kv2;

  stepLink = cf_newLink(sizeof(Step));
  step = (Step *) stepLink->data;
  for (cp=strtok(s, " "), op = ' '; cp != NULL; cp=strtok(NULL, " ")) {
    if (strchr("+-@&^>", *cp)) { op = *cp; continue; }
    if (op == ' ') strcpy(step->oldBase, cp);
    if (op == '+') strcpy(step->priChng, cp);
    if (op == '-') {
        if (*cp == '[') {
            cf_strsub(cp, "#", ' ');
            step->outgLink = cf_malloc(sizeof(pf_rec));
            sscanf(cp,"[%8[^\n]%4[^\n]%*c%8[^\n]%4[^\n]%c]", n1,v1,n2,v2,&ct);
            sscanf(v1, "%f", &kv1);
            sscanf(v2, "%f", &kv2);
            if (kv1==kv2)
                strcpy(type, "L*");
            else
                strcpy(type, "T*");
            pf_init_branch((pf_rec *) step->outgLink,type,n1,kv1,n2,kv2,ct,0);
            step->type = OUTG_BRCH;
        }
        else if (*cp == '{') { /* common-mode outages */
            conv = sscanf(cp,"{%[^}\n]", (char *) name);
            if (conv!=1) continue;
            cf_strsub(name, "#\n", ' ');
            sprintf(tag, "> mode %s", name);
            step->outgLink = cf_tag2link(tag);
            row = cf_link2row(step->outgLink);
            cf_appList(&trace->comoList, row);
            step->type = OUTG_COMO;
        }
    }
    if (op == '&') strcpy(trace->usrSpec, cp);
    if (op == '>') strcpy(step->usrRept, cp);
  }

  cf_appList(&trace->stepList, stepLink);

  trace->query &= ~( QUERY_BASE | QUERY_PCHG | QUERY_OUTG |
                     QUERY_USRA | QUERY_COMO );

  /* qvcurve uses traces in tact without decomposing them into lists */

  return;
}
void ff_report(char *s, Trace *trace)
{
  char *sp, *tp;
  for (sp = strtok(s, " ,="); sp!=NULL; sp = strtok(NULL, " ,=")) {
    tp=strtok(NULL, ", =");
    if (strstr(sp, "USER_ANALYSIS")!=NULL) {
        trace->query &= ~(QUERY_USRA);
        if (tp==NULL) return;
        strcpy(trace->usrSpec, tp);
        continue;
    }
    if (strstr(sp, "CONTROL_FILE" )!=NULL) {
        trace->query &= ~(QUERY_CTRL);
        if (tp==NULL) return;
        strcpy(trace->conSpec, tp);
        continue;
    }
    if (strstr(sp, "COMMON_MODE_DATA")!=NULL) {
        trace->query &= ~(QUERY_CMOD);
        if (tp==NULL) return;
        strcpy(trace->comSpec, tp);
        continue;
    }
    if (strstr(sp, "CUT_PLANE_DATA")!=NULL) {
        trace->query &= ~(QUERY_CUTM);
        if (tp==NULL) return;
        strcpy(trace->cutSpec, tp);
        continue;
    }
    if (tp==NULL) return;

    if (strstr(sp, "NAME")!=NULL) {
        trace->query &= ~(QUERY_NAME);
        strcpy(trace->outName, tp);
        continue;
    }
    if (strstr(sp, "TRACE")!=NULL) {
        if (strstr(tp, "YES")!=NULL) trace->trc->type = TRACE_YES;
        if (strstr(tp, "NO" )!=NULL) trace->trc->type = TRACE_NO;
        continue;
    }
  }
  return;
}
void ff_printMainHeader(cf_Out *rpt, Trace *trace)
{
  int n = 0;
  FILE *fp = rpt->file;
  cf_Style scrnTag;

  fprintf(fp, "%s\n", PAGE_BREAK); n++;

  fprintf(fp,   "/REPORT        | ");
  fprintf(fp,   "  NAME = %s\n", trace->outName); n++;
  if (strlen(trace->usrSpec)>0) {
      fprintf(fp,   "  USER_ANALYSIS = %s\n", trace->usrSpec); n++;
  }
  if (strlen(trace->conSpec)>0) {
      fprintf(fp,   "  CONTROL_FILE = %s\n", trace->conSpec); n++;
  }
  if (strlen(trace->comSpec)>0) {
      fprintf(fp,   "  COMMON_MODE_DATA = %s\n", trace->comSpec); n++;
  }
  if (strlen(trace->cutSpec)>0) {
      fprintf(fp,   "  CUT_PLANE_MASTER = %s\n", trace->cutSpec); n++;
  }
  fprintf(fp,   "/REPORT        | ");
  fprintf(fp,   "  TRACE =");
  if (trace->trc->type==TRACE_YES) fprintf(fp, " YES");
  if (trace->trc->type==TRACE_NO ) fprintf(fp, " NO");
  fprintf(fp, "\n");  n++;

  n += cf_printList(fp, trace->ownrList, CF_dznStyl, "/OWNER         |");
  n += cf_printList(fp, trace->zoneList, CF_dznStyl, "/ZONE          |");
  n += cf_printList(fp, trace->bskvList, CF_sixStyl, "/BASE_KV       |");

  fprintf(fp, "%s\n", PAGE_BREAK); n++;
  rpt->line += n;
  return;
}
void ff_printOutgHeader(cf_Out *rpt)
{
  char *HdOutgLd[] = { /* lead */
    " ",
    "            |  Primary  |                          |",
    "  Basecase  |  Change   |          Outage          |",
    "------------|-----------|--------------------------|",
  };
  fprintf(rpt->file, "%s\n", HdOutgLd[0]);
  fprintf(rpt->file, "%s\n", HdOutgLd[1]);
  fprintf(rpt->file, "%s\n", HdOutgLd[2]);
  fprintf(rpt->file, "%s\n", HdOutgLd[3]);
  rpt->line += 4;
  return;
}
void ff_printOutgPoints(cf_Out *rpt, int stat, Step *step)
{
  char  newLine[1024], str[132];

  if (ff_printPageFooter(rpt, 1)) ff_printOutgHeader(rpt);

  sprintf(newLine, "%-12.12s|", step->oldBase);
  cf_aprint(newLine, "%-11.11s|", step->priChng);

  if (step->type==OUTG_BRCH) {
    pf_rec_b2a(str, (pf_rec *) step->outgLink, "I");
    str[2] = 'D';
    cf_aprint(newLine, "%26.26s|", &str[6]);
  }
  else if (step->type==OUTG_COMO) {
    cf_link2tagName((Link *)step->outgLink, str);
    cf_aprint(newLine, "%-26.26s|", str);
  }
  else {
    cf_aprint(newLine, "%-26.26s|", "No Outage Taken");
  }
  if (stat!=0) {
    memcpy(&newLine[45], "(Err)", 5);
  }
  fprintf(rpt->file, "%s\n", newLine);
  rpt->line++;

  fflush(rpt->file);
}
void ff_printOutgReport(cf_Out *rpt, int stat, Step *step)
{
  Link *stepLink;

  ff_printPageFooter(rpt, 6);  /* add here if removed from ff_printLoadHeader */
  ff_printOutgHeader(rpt);
  ff_printOutgPoints(rpt, stat, step);
  return;
}
void ff_printCutpReport(cf_Out *rpt, int stat, Link *cutpList)
{
  Link *stepLink;

  ff_printPageFooter(rpt, 6);  /* add here if removed from ff_printLoadHeader */
  if (ff_printErrMessage(rpt, stat)) return;
  ff_printCutpHeader(rpt);
  ff_printCutpPoints(rpt, stat, cutpList);
  return;
}
void ff_printAnglReport(cf_Out *rpt, int stat, Link *anglList)
{
  Link *stepLink;

  ff_printPageFooter(rpt, 6);
  if (ff_printErrMessage(rpt, stat)) return;
  ff_printAnglHeader(rpt);
  ff_printAnglPoints(rpt, stat, anglList);
  return;
}
void ff_printLoadReport(cf_Out *rpt, int stat, Link *cutpList)
{
  Link *stepLink;

  ff_printPageFooter(rpt, 6);  /* add here if removed from ff_printLoadHeader */
  if (ff_printErrMessage(rpt, stat)) return;
  ff_printLoadHeader(rpt);
  ff_printLoadPoints(rpt, stat, cutpList);
  return;
}
void ff_printLineReport(cf_Out *rpt, int stat, Link *cutpList)
{
  Link *stepLink;

  ff_printPageFooter(rpt, 6);
  if (ff_printErrMessage(rpt, stat)) return;
  ff_printLinePoints(rpt, stat, cutpList);
  return;
}
void ff_printVoltReport(cf_Out *rpt, int stat, Link *mbusList)
{
  Link *stepLink;

  if (mbusList==NULL) return;
  ff_printPageFooter(rpt, 6);
  if (ff_printErrMessage(rpt, stat)) return;
  ff_printVoltHeader(rpt);
  ff_printVoltPoints(rpt, stat, mbusList);
  return;
}
void ff_printVLowReport(cf_Out *rpt, int stat, Link *vlowList)
{
  Link *stepLink;

  ff_printPageFooter(rpt, 6);
  if (ff_printErrMessage(rpt, stat)) return;
  ff_printVLowHeader(rpt);
  ff_printVLowPoints(rpt, stat, vlowList);
  return;
}
void ff_printHighReport(cf_Out *rpt, int stat, Link *highList, int mode)
{
  Link *stepLink;

  ff_printPageFooter(rpt, 6);
  if (ff_printErrMessage(rpt, stat)) return;
  ff_printHighHeader(rpt, mode);
  ff_printHighPoints(rpt, stat, highList, mode);
  return;
}
void ff_printLoadHeader(cf_Out *rpt)
{
  char *HdFlowLd[] = { /* lead */
    " ",
    "   |   |    CUT_PLANE_LOADING    | |      |      |      |",
    "   |   |      BRANCH NAME        | | Load | Pin  | Qin  |",
    "TYc|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|(AMPS)| (MW) |(MVAR)|",
  };
  fprintf(rpt->file, "%s\n", HdFlowLd[0]);
  fprintf(rpt->file, "%s\n", HdFlowLd[1]);
  fprintf(rpt->file, "%s\n", HdFlowLd[2]);
  fprintf(rpt->file, "%s\n", HdFlowLd[3]);
  rpt->line += 4;
  return;
}
void ff_printLoadPoints(cf_Out *rpt, int stat, Link *cutpList)
{
  Link  *curCutp, *curLine;
  pf_rec *m;
  float  Load, Pin, Qin;
  char  id[132], cutName[CF_STRSIZE];

  strcpy(cutName, "cut-plane");
  for (curCutp=cutpList; curCutp!=NULL; curCutp=curCutp->next) {
    Load = 0;
    Pin = 0;
    Qin = 0;
    for (curLine=(Link *) curCutp->data; curLine!=NULL; curLine=curLine->next) {
        if (curLine->kind==CF_KIND_TAG) {
            cf_link2tagName(curLine, cutName);
            continue;
        }
        if (ff_printPageFooter(rpt, 2)) ff_printLoadHeader(rpt);
        m = (pf_rec *) curLine->data;
        pf_rec_b2a(id, m, "I");
        fprintf(rpt->file,
            "%-2.2s |%-3.3s|%-12.12s %-12.12s|%1.1s|%6.0f|%6.1f|%6.1f|\n",
            &id[0], &id[3], &id[6], &id[19], &id[31],
            m->s.branch.tot_line_load_amps, m->s.branch.Pin, m->s.branch.Qin);
        Load += m->s.branch.tot_line_load_amps;
        Pin  += m->s.branch.Pin;
        Qin  += m->s.branch.Qin;
        rpt->line++;
    }
    if (ff_printPageFooter(rpt, 2)) ff_printLoadHeader(rpt);
    fprintf(rpt->file,
        "-----------------------------------|------|------|------|\n");
    rpt->line++;
    fprintf(rpt->file,
        "TOTALS FOR: %-23.23s|%6.0f|%6.1f|%6.1f|\n\n",
        cutName, Load, Pin, Qin);
    rpt->line += 2;
  }
  fflush(rpt->file);
  return;
}
void ff_printLineHeader(cf_Out *rpt)
{
  char *HdFlowLd[] = { /* lead */
    " ",
    "   |   |CUT-PLANE BRANCH LOADING | |Rating| | Pin  | Qin  | Load | Load | Qloss|",
    "Tyc|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|(AMPS)|F| (MW) |(MVAR)|(AMPS)|  (%) |(MVAR)|",
    "---|---|-------------------------|-|------|-|------|------|------|------|------|",
  };
  fprintf(rpt->file, "%s\n", HdFlowLd[0]);
  fprintf(rpt->file, "%s\n", HdFlowLd[1]);
  fprintf(rpt->file, "%s\n", HdFlowLd[2]);
  fprintf(rpt->file, "%s\n", HdFlowLd[3]);
  rpt->line += 4;
  return;
}
void ff_printLinePoints(cf_Out *rpt, int stat, Link *cutpList)
{
  Link  *curCutp, *curLine;
  pf_rec *m;
  float  Rating, Load, Pin, Qin, Pct, Qloss;
  char  id[132], cutName[CF_STRSIZE], Code;

  strcpy(cutName, "cut-plane");
  for (curCutp=cutpList; curCutp!=NULL; curCutp=curCutp->next) {
    if (ff_printPageFooter(rpt, 2)) ff_printLineHeader(rpt);
    for (curLine=(Link *) curCutp->data; curLine!=NULL; curLine=curLine->next) {
        if (curLine->kind==CF_KIND_TAG) {
            cf_link2tagName(curLine, cutName);
            fprintf(rpt->file, "\n %-23.23s CUT-PLANE LINES:\n", cutName);
            rpt->line += 2;
            ff_printLineHeader(rpt);
            continue;
        }
        m = (pf_rec *) curLine->data;
        pf_rec_b2a(id, m, "I");

        Rating = cf_getRating(m, 0, &Code);
        Pin    = m->s.branch.Pin;
        Qin    = m->s.branch.Qin;
        Load   = m->s.branch.tot_line_load_amps;
        Pct    = 100 * Cf_ratio(Load, Rating);
        Qloss  = m->s.branch.Qloss;

        fprintf(rpt->file,
            "%-2.2s |%-3.3s|%-25.25s|%1.1s|%6.0f|%c|%6.0f|%6.0f|%6.0f|%6.1f|%6.1f|\n",
            &id[0],&id[3],&id[6],&id[31],Rating,Code,Pin,Qin,Load,Pct,Qloss);

        rpt->line++;
    }
  }
  fflush(rpt->file);
  return;
}
void ff_printCutpHeader(cf_Out *rpt)
{
  char *HdLd[] = { /* lead */
    " ",
    " CUT-PLANE FLOWS:                  |Pin(MW)|",
    "-----------------------------------|-------|",
  };
  fprintf(rpt->file, "%s\n", HdLd[0]);
  fprintf(rpt->file, "%s\n", HdLd[1]);
  fprintf(rpt->file, "%s\n", HdLd[2]);
  rpt->line += 3;
  return;
}
void ff_printCutpPoints(cf_Out *rpt, int stat, Link *cutpList)
{
  Link  *curCutp, *curLine;
  pf_rec *m;
  float  Pin;
  char  id[132], cutName[CF_STRSIZE];

  strcpy(cutName, "cut-plane");
  for (curCutp=cutpList; curCutp!=NULL; curCutp=curCutp->next) {
    Pin = 0;
    for (curLine=(Link *) curCutp->data; curLine!=NULL; curLine=curLine->next) {
        if (curLine->kind==CF_KIND_TAG) {
            cf_link2tagName(curLine, cutName);
            continue;
        }
        m = (pf_rec *) curLine->data;
        Pin  += m->s.branch.Pin;
    }
    if (ff_printPageFooter(rpt, 2)) ff_printCutpHeader(rpt);
    fprintf(rpt->file, " %-34.34s|%6.0f |\n", cutName, Pin);
    rpt->line += 1;
  }
  fflush(rpt->file);
  return;
}
void ff_printAnglHeader(cf_Out *rpt)
{
  char *HdLd[] = { /* lead */
    " ",
    " BUS ANGLE DIFFERENCE:                       |Angle 1|Angle 2|A1 - A2|",
    "B  OWN< Bus1 ><V1>ZN  -  B  OWN< BUS2 ><V2>ZN|Degrees|Degrees|Degrees|",
    "---------------------------------------------|-------|-------|-------|",
  };
  fprintf(rpt->file, "%s\n", HdLd[0]);
  fprintf(rpt->file, "%s\n", HdLd[1]);
  fprintf(rpt->file, "%s\n", HdLd[2]);
  fprintf(rpt->file, "%s\n", HdLd[3]);
  rpt->line += 4;
  return;
}
void ff_printAnglPoints(cf_Out *rpt, int stat, Link *anglList)
{
  Link   *curAngl, *curAbus;
  char    id1[132], id2[132];
  pf_rec *b1, *b2;

  for (curAngl=anglList; curAngl!=NULL; curAngl=curAngl->next) {
    if (ff_printPageFooter(rpt, 2)) ff_printAnglHeader(rpt);
    curAbus = (Link *) curAngl->data;
    for ( ; curAbus!=NULL; curAbus=curAbus->next) {
        if (curAbus->kind!=CF_KIND_REC) continue;
        if (curAbus->data==NULL||curAbus->next==NULL||curAbus->next->data==NULL)
            continue;
        b1 = (pf_rec *) curAbus->data;
        b2 = (pf_rec *) curAbus->next->data;
        pf_rec_b2a(id1, b1, "I");
        pf_rec_b2a(id2, b2, "I");
        fprintf(rpt->file, "%-2.2s %-3.3s%-12.12s%2.2s  -  ",
            &id1[0], &id1[3], &id1[6], &id1[18]);
        fprintf(rpt->file, "%-2.2s %-3.3s%-12.12s%2.2s| %6.1f| %6.1f| %6.1f|\n",
            &id2[0], &id2[3], &id2[6], &id2[18], b1->s.ACbus.Vdeg, 
            b2->s.ACbus.Vdeg, b1->s.ACbus.Vdeg - b2->s.ACbus.Vdeg);
        rpt->line++;
    }
  }
  fflush(rpt->file);
  return;
}
void ff_printVoltHeader(cf_Out *rpt)
{
  char *HdVoltLd[] = { /* lead */
    " ",
    "   |   |BUS_VOLTAGE |  |      |      |",
    "   |   |  BUS NAME  |  | Vmag | Vdeg |",
    "TYc|OWN|< BUS1 ><V1>|ZN| (kV) | (deg)|",
  };
  fprintf(rpt->file, "%s\n", HdVoltLd[0]);
  fprintf(rpt->file, "%s\n", HdVoltLd[1]);
  fprintf(rpt->file, "%s\n", HdVoltLd[2]);
  fprintf(rpt->file, "%s\n", HdVoltLd[3]);
  rpt->line += 4;
  return;
}
void ff_printVoltPoints(cf_Out *rpt, int stat, Link *mbusList)
{
  Link  *curMbus;
  pf_rec *m;
  char  id[132];

  for (curMbus=mbusList; curMbus!=NULL; curMbus=curMbus->next) {
    if (ff_printPageFooter(rpt, 2)) ff_printVoltHeader(rpt);
    m = (pf_rec *) curMbus->data;
    pf_rec_b2a(id, m, "I");
    fprintf(rpt->file, "%-2.2s |%-3.3s|%-12.12s|%2.2s|%6.1f|%6.1f|\n",
         &id[0], &id[3], &id[6], &id[18], m->s.ACbus.Vmag, m->s.ACbus.Vdeg);
    rpt->line++;
  }
  fflush(rpt->file);
  return;
}
void ff_printVLowHeader(cf_Out *rpt)
{
  char *HdVLowLd[] = { /* lead */
    " ",
    "   |   |10 Lowest PU|  |      |",
    "   |   |  BUS NAME  |  | Vmag |",
    "TYc|OWN|< BUS1 ><V1>|ZN| (PU) |",
  };
  fprintf(rpt->file, "%s\n", HdVLowLd[0]);
  fprintf(rpt->file, "%s\n", HdVLowLd[1]);
  fprintf(rpt->file, "%s\n", HdVLowLd[2]);
  fprintf(rpt->file, "%s\n", HdVLowLd[3]);
  rpt->line += 4;
  return;
}
void ff_printVLowPoints(cf_Out *rpt, int stat, Link *vlowList)
{
  Link   *curLink;
  pf_rec *m;
  char    id[132];
  int     n;

  n = 0;
  for (curLink=vlowList; curLink!=NULL; curLink=curLink->next) {
    if (ff_printPageFooter(rpt, 2)) ff_printVLowHeader(rpt);
    m = (pf_rec *) curLink->data;
    pf_rec_b2a(id, m, "I");
    fprintf(rpt->file, "%-2.2s |%-3.3s|%-12.12s|%2.2s|%6.3f|\n",
         &id[0], &id[3], &id[6], &id[18], (m->s.ACbus.Vmag / m->i.ACbus.kv));
    rpt->line++;
    n++;
    if (n >= 10) break;
  }
  fflush(rpt->file);
  return;
}
void ff_printHighHeader(cf_Out *rpt, int mode)
{
  char *HdLd[] = { /* lead */
    " ",
    "   |   | 10 Highest 115 kV Loads | |Rating| | Pin  | Qin  | Load | Load | Qloss|",
    "   |   | 10 Highest 230 kV Loads | |Rating| | Pin  | Qin  | Load | Load | Qloss|",
    "   |   | 10 Highest 345 kV Loads | |Rating| | Pin  | Qin  | Load | Load | Qloss|",
    "   |   | 10 Highest 500 kV Loads | |Rating| | Pin  | Qin  | Load | Load | Qloss|",
    "   |   | 10 Highest Branch Loads | |Rating| | Pin  | Qin  | Load | Load | Qloss|",
    "   |   | 10 Highest Transformers | |Rating| | Pin  | Qin  | Load | Load | Qloss|",
    "TYc|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|(Amps)|F| (MW) |(MVAR)|(Amps)|  (%) |(MVAR)|",
    "TYc|OWN|< BUS1 ><V1> < BUS2 ><V2>|C| (MVA)|F| (MW) |(MVAR)| (MVA)|  (%) |(MVAR)|",
  };
  int units, title;

  switch (mode) {
    case 115 : title = 1; units = 7; break;
    case 230 : title = 2; units = 7; break;
    case 345 : title = 3; units = 7; break;
    case 500 : title = 4; units = 7; break;
    default  : title = 5; units = 7; break;
    case 'T' : title = 6; units = 8; break;
  }
  fprintf(rpt->file, "%s\n", HdLd[0]);
  fprintf(rpt->file, "%s\n", HdLd[title]);
  fprintf(rpt->file, "%s\n", HdLd[units]);
  rpt->line += 3;
  return;
}
void ff_printHighPoints(cf_Out *rpt, int stat, Link *highList, int mode)
{
  Link   *curLink;
  pf_rec *m;
  char    id[132];
  int     n;
  float   Voltage, Rating, Load, Pin, Qin, Pct, Qloss;
  char    Code, Type;

  n = 0;
  for (curLink=highList; curLink!=NULL; curLink=curLink->next) {
    if (ff_printPageFooter(rpt, 2)) ff_printHighHeader(rpt, mode);
    m = (pf_rec *) curLink->data;
    pf_rec_b2a(id, m, "I");
    Voltage = m->i.branch.bus1_kv;
    Type    = m->i.branch.type[0];
    switch (mode) {
        case 'T' :  if (Type!='T') continue;
                    break;
        case 115 :  if (Type=='T') continue;
                    if ((int) Voltage != 115) continue;
                    break;
        case 230 :  if (Type=='T') continue;
                    if ((int) Voltage != 230) continue;
                    break;
        case 345 :  if (Type=='T') continue;
                    if ((int) Voltage != 345) continue;
                    break;
        case 500 :  if (Type=='T') continue;
                    if ((int) Voltage < 500) continue;
                    break;
        default  :  break;
    }
    Rating = cf_getRating(m, 0, &Code);
    Pin    = m->s.branch.Pin;
    Qin    = m->s.branch.Qin;
    Load   = cf_getLoad(m);
    Pct    = 100 * Cf_ratio(Load, Rating);
    Qloss  = m->s.branch.Qloss;

    fprintf(rpt->file,
        "%-2.2s |%-3.3s|%-25.25s|%1.1s|%6.0f|%c|%6.0f|%6.0f|%6.0f|%6.1f|%6.1f|\n",
         &id[0],&id[3],&id[6],&id[31],Rating,Code,Pin,Qin,Load,Pct,Qloss);
    rpt->line++;
    n++;
    if (n >= 10) break;
  }
  fflush(rpt->file);
  return;
}
void ff_printSummReport(cf_Out *rpt, int stat, Step *step, Link *mbusList, Link *cutpList)
{ /* Excel-ready table, no page breaks */
  Link *stepLink;

  ff_printSummPoints(rpt, stat, step, mbusList, cutpList);
  return;
}
void ff_printSummHeader(cf_Out *rpt, Trace *trace, Step *step)
{
  char HdLabel[CF_STRSIZE];
  static char *HdLd[] = { /* lead */
    "",
    "      BASECASE      |  PRIMARY  CHANGES  |",
    "--------------------|--------------------|",
    "%-20.20s|%-20.20s|",
    "----------------------------------------||",
  };
  static char *HdRpAn[] = { /* repeat */
    "",
    " ANGLE (DEG)|",
    "------------|",
    "%-12.12s|",
    "------------|",
  };
  static char *HdRpFl[] = { /* repeat */
    "",
    "FLOW(MW)|",
    "--------|",
    "%-8.8s|",
    "--------|",
  };
  static char *HdTl[] = { /* tail */
    "",
    "\n",
  };
  sprintf(HdLabel, HdLd[3], step->oldBase, step->priChng);
  printTableHeader(rpt, trace->mbusList, HdLd[0], HdRpAn[0], HdTl[0]);
  printTableHeader(rpt, trace->cutpList, HdLd[0], HdRpFl[0], HdTl[1]);
  printTableHeader(rpt, trace->mbusList, HdLd[1], HdRpAn[1], HdTl[0]);
  printTableHeader(rpt, trace->cutpList, HdLd[0], HdRpFl[1], HdTl[1]);
  printTableHeader(rpt, trace->mbusList, HdLd[2], HdRpAn[2], HdTl[0]);
  printTableHeader(rpt, trace->cutpList, HdLd[0], HdRpFl[2], HdTl[1]);
  printTableLabels(rpt, trace->mbusList, HdLabel, HdRpAn[3], HdTl[0]);
  printTableLabels(rpt, trace->cutpList, HdLd[0], HdRpFl[3], HdTl[1]);
  printTableHeader(rpt, trace->mbusList, HdLd[4], HdRpAn[4], HdTl[0]);
  printTableHeader(rpt, trace->cutpList, HdLd[0], HdRpFl[4], HdTl[1]);
  rpt->line += 5;
  return;
}
void printTableHeader(cf_Out *rpt, Link *curLink, char *l, char *r, char *t)
{
  fprintf(rpt->file, l);                                            /*  lead  */
  for (; curLink!=NULL; curLink=curLink->next) {                    /* repeat */
    fprintf(rpt->file, r);
  }
  fprintf(rpt->file, t);                                            /*  tail  */
}
void printTableLabels(cf_Out *rpt, Link *curLink, char *l, char *r, char *t)
{
  int   width, kind;
  Link *data;
  char  id[CF_RECSIZE];

  fprintf(rpt->file, l);                                            /*  lead  */
  for (; curLink!=NULL; curLink=curLink->next) {                    /* repeat */
    kind = curLink->kind;
    data = curLink->data;
    if (kind==CF_KIND_LINK) {
        data = (Link *)(curLink->data);
        kind = ((Link *)(curLink->data))->kind;
    }
    if (kind==CF_KIND_REC) {
        pf_rec_b2a(id, (pf_rec *) data, "I");
        fprintf(rpt->file, r, &id[6]); /* repeat */
    }
    if (kind==CF_KIND_TAG) {
        cf_link2tagName((Link *) data, id);
        fprintf(rpt->file, r, id);    /* repeat */
    }
  }
  fprintf(rpt->file, t);                                            /*  tail  */
}
void ff_printSummPoints(cf_Out *rpt, int stat, Step *step, Link *mbusList, Link *cutpList)
{
  Link    *curLink, *curData;
  pf_rec *r;
  char    id[132], net_data[132];
  float   Load;

  if (step->type==OUTG_BRCH) {
    pf_rec_b2a(net_data, (pf_rec *) step->outgLink, "I");
    net_data[2] = 'D';
    fprintf(rpt->file, "%33.33s       ||", net_data);
  }
  if (step->type==OUTG_COMO) {
    cf_link2tagName((Link *)step->outgLink, net_data);
    fprintf(rpt->file, "%-40.40s||", net_data);
  }
  if (ff_printErrMessage(rpt, stat)) return;
  for (curLink=mbusList; curLink!=NULL; curLink=curLink->next) {
    r = (pf_rec *) curLink->data;
    fprintf(rpt->file, "%12.2f|", r->s.ACbus.Vdeg);
  }
  for (curLink=cutpList; curLink!=NULL; curLink=curLink->next) {
    Load = 0;
    curData = (Link *) curLink->data;  /* skip first Link (it is tag data) */
    for (curData=(Link *)curData->next; curData!=NULL; curData=curData->next) {
        r = (pf_rec *) curData->data;
        Load += r->s.branch.tot_line_load_amps;
    }
    fprintf(rpt->file, "%8.1f|", Load);
  }
  rpt->line++;
  fprintf(rpt->file, "\n");
  fflush(rpt->file);
  return;
}
void ff_printMainFooter(cf_Out *rpt)
{
/*  ff_printPageFooter(rpt, FF_PAGE_LENGTH); */
/*  maybe print a report legend */
  fclose(rpt->file);
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
void de_load_oldbase(int *stat, char *file)
{
  if (*stat & FAIL_CRIT) return;
  if (strlen(file)==0) return;
  printf("\n  Loading Base... %s\n", file);
  if (pf_load_oldbase(file)!=0) {
    cf_logErr("Skipping base case: Cannot load %s\n", file);
    cf_logErr("%s\n", err_buf);
    *stat |= FAIL_BASE;
    *stat |= FAIL_CRIT;
  }
  return;
}
int de_load_changes(int *stat, char *file)
{
  if (*stat & FAIL_CRIT) return;
  if (strlen(file)==0) return;
  printf("  Applying Primary Changes... %s\n", file);
  if (pf_load_changes(file)!=0) {
    cf_logErr("Skipping changes:  Cannot use %s\n", file);
    cf_logErr("%s\n", err_buf);
    *stat |= FAIL_CHNG;
  }
}
void de_take_brch_outg(int *stat, int type, pf_rec *brnOutg)
{
  pf_rec b;
  char   s[CF_RECSIZE];

  if (*stat & FAIL_CRIT) return;
  if (type!=OUTG_BRCH) return;
  printf("  Taking Branch Outage...\n");
  memset(&b, '\0', sizeof(pf_rec));
  memcpy(&b, brnOutg, sizeof(pf_rec));
  pf_rec_b2a(s, &b, "D");
  printf("    %s\n", s);
  if (pf_rec_branch(&b, "D")!=0) {                           /* take outage */
    cf_logErr("de_take_brch_outage:  Cannot take outage!\n");
    cf_logErr(s);
    *stat |= FAIL_CHNG;
    *stat |= FAIL_CRIT;
  }
  return;
}
void de_take_como_outg(int *stat, int type, Link *comoOutg)
{
  char cmd[CF_IPCSIZE], tagName[CF_STRSIZE];

  if (*stat & FAIL_CRIT) return;
  if (type!=OUTG_COMO) return;

  strcpy(cmd, "/changes, file = *\n");

  while (comoOutg!=NULL) {
    if (comoOutg->kind==CF_KIND_TAG) {
        cf_link2tagName(comoOutg, tagName);
        printf("  Taking Common-mode Outage... %s\n", tagName);
    }
    if (comoOutg->kind==CF_KIND_STR) {
        strcat(cmd, comoOutg->data);
    }
    comoOutg=comoOutg->next;
  }
  printf("%s\n", cmd);
  if (pf_command(cmd)!=0) {
    cf_logErr("de_take_como_outage:  Cannot take outage!\n");
    cf_logErr(cmd);
    *stat |= FAIL_CHNG;
    *stat |= FAIL_CRIT;
  }
  return;
}
void de_command(int *stat, char *cmd, char *data)
{
  char s[CF_IPCSIZE];

  if (*stat & FAIL_CRIT) return;
  if (strlen(cmd)==0) return;
  sprintf(s, "%s%s\n", cmd, data);
  printf(s);
  if (pf_command(s)!=0) {
    cf_logErr(s);
    cf_logErr("de_command:  Command Failed!\n");
    *stat |= FAIL_CMMD;
  }
  return;
}
void de_solution(int *stat, char *solution, char *file)
{
  if (*stat & FAIL_CRIT) return;
  if (strlen(solution)==0) return;
  printf("\n  Solving Base... %s\n", file);
  printf("%s\n", solution);
  if (pf_command(solution)!=0) {
    cf_logErr(solution);
    cf_logErr("de_solution:  Cannot solve basecase!\n");
    *stat |= FAIL_SOLV;
    *stat |= FAIL_CRIT;
  }
  return;
}
int ff_cmpPU(Link *newLink, Link *xstLink, int sort)
{ /* avoid comp==0 in the insLink function */
  pf_rec *newData, *xstData;
  float   newPU, xstPU;

  if (newLink==NULL || xstLink==NULL) return 1;
  if (newLink->data==NULL || xstLink->data==NULL) return -1;
  newData = (pf_rec *) newLink->data;
  xstData = (pf_rec *) xstLink->data;
  newPU = Cf_ratio(newData->s.ACbus.Vmag, newData->i.ACbus.kv);
  xstPU = Cf_ratio(xstData->s.ACbus.Vmag, xstData->i.ACbus.kv);
  return (newPU - xstPU) < 0 ? -1 : 1; /* lowest first */
}
int ff_cmpLoad(Link *newLink, Link *xstLink, int sort)
{ /* avoid comp==0 in the insLink function */
  pf_rec *newData, *xstData;
  float   newRating, xstRating, newLoad, xstLoad, newPct, xstPct;
  char    newCode, xstCode;
  int     cmp;

  if (newLink==NULL || xstLink==NULL) return 1;
  if (newLink->data==NULL || xstLink->data==NULL) return -1;
  newData = (pf_rec *) newLink->data;
  xstData = (pf_rec *) xstLink->data;
  newRating = cf_getRating(newData, 0, &newCode);
  xstRating = cf_getRating(xstData, 0, &xstCode);
  newLoad   = cf_getLoad(newData);
  xstLoad   = cf_getLoad(xstData);
  newPct = 100 * Cf_ratio(newLoad, newRating);
  xstPct = 100 * Cf_ratio(xstLoad, xstRating);
  return (xstPct - newPct) < 0 ? -1 : 1; /* highest first */
}
int ff_printErrMessage(cf_Out *rpt, int stat)
{
  if (!(stat & FAIL_CRIT)) return 0;
  fprintf(rpt->file, "Critical Failure: see log file %s\n", CF_logSpec);
  rpt->line++;
  fflush(rpt->file);
  return 1;
}
Link *cf_topList(Link *cur)
{
  while (cur != NULL && cur->prev != NULL) cur = cur->prev;
  return cur;
}
