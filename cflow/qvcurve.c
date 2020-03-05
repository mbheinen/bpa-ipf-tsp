/******************************************************************************\
UTILITY:    QVcurve, added Cathy's /GEN_DROP command
STRUCTURE:  common CFLOW architecture.
TYPE:       Powerflow (IPFSRV), /CHANGE_PARAMETER reactance voltage curve.
SUMMARY:    Generate voltage reactance curves.
RELATED:    LINEFLOW, FINDOUT, CFUSE, MIMIC
SEE ALSO:   MODES.TXT
UPDATED:    January 09, 1997
LANGUAGE:   Standard C.  CFLOW Libraries.  CF_UTIL.H.
DEVELOPER:  William D. Rogers, BPA, TEOS, 230-3806, wdrogers@bpa.gov
REQUESTER:  Scott Kinney, BPA, TEOC, 230-3905
USERS:      S. Kinney, K. Kohne, M. Rodrigues, G. Comegys, B. Tesema
IPF:        Version 317 or above recommended.
PURPOSE:    Generate new cases given a list of base cases and a list of change
            files.  Check the new cases for over and under voltages, overloads,
            and excessive voltage and loading changes.
\******************************************************************************/
/******************************* #include *************************************/
#include <cflowlib.h>
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

#define  OK               0
#define  MAX_IN           264
#define  MAX_CURVES       6
#define  FF_IPF_VERSION   327            /* GPF.EXE_V327 or above recommended */
#define  PAGE_LENGTH      61
#define  PAGE_WIDTH       132
#define  DOT_LINE         "..................................................."
#define  LOG_SPEC         "qvcurve.log"
#define  RPT_SPEC         "qvcurve.rpt"
#define  QUERY_BASE  (int) (1<< 0) /* prompt for basecases */
#define  QUERY_BRCH  (int) (1<< 1) /* prompt for branch-outages */
#define  QUERY_QBUS  (int) (1<< 2) /* prompt for bus list */
#define  QUERY_USRA  (int) (1<< 3) /* prompt for user analysis files */
#define  QUERY_FILE  (int) (1<< 4) /* prompt for output report file name */
#define  QUERY_LMTS  (int) (1<< 6) /* prompt for limits */
#define  QUERY_COMO  (int) (1<< 7) /* prompt for common_mode outages list */
#define  QUERY_CMOD  (int) (1<< 8) /* prompt for common_mode data file */
#define  QUERY_PCHG  (int) (1<< 9) /* prompt for /PRI_CHANGE files */
#define  QUERY_SOLN  (int) (1<<10) /* prompt for solution qualifiers */
#define  QUERY_CHBT  (int) (1<<11) /* prompt for /CHANGE_BUS_TYPE mapping */
#define  QUERY_OUTP  (int) (1<<12) /* prompt for /OUTPUT file names */
#define  QUERY_SETU  (int) (1<<13) /* prompt for /SETUP file names */
#define  QUERY_CONT  (int) (1<<14) /* prompt for whether to continue run */
#define  QUERY_CURV  (int) (1<<15) /* prompt for number of curves per plot */
#define  QUERY_AGCC  (int) (1<<16) /* prompt for /AGC card data */
#define  QUERY_GEND  (int) (1<<17) /* prompt for /GEN_DROP card data */
#define  READ_INC    (int)      1  /* found /INCLUDE card in CFLOW data file */
#define  READ_BSE    (int)      2  /* found /BASECAS card in CFLOW data file */
#define  READ_PCH    (int)      3  /* found /PRI_CHA card in CFLOW data file */
#define  READ_REP    (int)      4  /* found /REPORT  card in CFLOW data file */
#define  READ_TRC    (int)      5  /* found /TRACE   card in CFLOW data file */
#define  READ_BUS    (int)      6  /* found /BUS     card in CFLOW data file */
#define  READ_BRN    (int)      7  /* found /BRANCH  card in CFLOW data file */
#define  READ_SOL    (int)      8  /* found /SOLUTION     in CFLOW data file */
#define  READ_LIM    (int)      9  /* found /LIMITS  card in CFLOW data file */
#define  READ_COM    (int)     10  /* found /COMMON_MODE  in CFLOW data file */
#define  READ_CBT    (int)     11  /* found /CHANGE_BUS_TYPE card  data file */
#define  READ_OTP    (int)     12  /* found /OUTPUT  card in CFLOW data file */
#define  READ_SET    (int)     13  /* found /SETUP   card in CFLOW data file */
#define  READ_AGC    (int)     14  /* found /AGC     card in CFLOW data file */
#define  READ_GDR    (int)     15  /* found /GEN_DROP     in CFLOW data file */
#define  OUTG_NONE   (int)      0
#define  OUTG_BRCH   (int)      1
#define  OUTG_COMO   (int)      2
#define  TRACE_NO    (int)      0
#define  TRACE_YES   (int)      1
#define  FAIL_BASE   (int) (1<< 0)
#define  FAIL_PCHG   (int) (1<< 1)
#define  FAIL_CHNG   (int) (1<< 2)
#define  FAIL_SOLV   (int) (1<< 3)
#define  PAGE_BREAK "************************************************************************************************************************************"
#define  PAGE_MARK  "------------------------------------------------------------------------------------------------------------------------------------"
/***************************** end #define ************************************/
/******************************* enum *****************************************/
/***************************** end enum ***************************************/
/******************************* typedef **************************************/
typedef struct {    /* ff_branch */
    char   type[3];
    char   owner[4];
    char   bus1_name[9];
    float  bus1_kv;
    int    meter;
    char   bus2_name[9];
    float  bus2_kv;
    char   ckt_id;
    int    section;
    float  rating;
    char   code;
    float  nominal;
    float  thermal;
    float  emergency;
    float  bottleneck;
    char   date_in[4];
    char   bus1_zone[3];
    char   bus2_zone[3];
} ff_branch; /* modified form of pf_branch */

typedef struct {    /* ff_bus */  
    char   type[3];
    char   owner[4];
    char   name[9];
    float  kv;
    char   zone[3];
} ff_bus; /* short form of pf_AC_bus and pf_DC_bus */

typedef struct processStep {
  int                    type;     /* 0 = no outage, 1 = branch, 2 = change */
  char  oldBase[FILENAME_MAX];     /* load this base case */
  char  priChng[FILENAME_MAX];     /* primary change file */
  void              *outgLink;     /* outage: branch, change, or common_mode */
  pf_rec                busID;     /* do QV at this bus */
  char  qvcRept[FILENAME_MAX];     /* save QV curve with this name */
  char  usrRept[FILENAME_MAX];     /* save user analysis with this name */
  float                 Qmin;
  float              VatQmin;
  float           dVdQatQmin;
  float                 Vmin;
  float              QatVmin;
  char       subTitle[MAX_IN];
  char         legend[MAX_IN];
} Step;

typedef struct xyPlotSetup {
  char     setUpXY[FILENAME_MAX];    /* setup file name for PSAP 22 XYPLOTTER */
  char     setUpPS[FILENAME_MAX];    /* setup file name for PostScript */
  int             numberOfCurves;    /* up to 5 allowed */
  char        superTitle[MAX_IN];    /* super title for plot */
  char  qvpFile[6][FILENAME_MAX];    /* array of six qvpt point files */
  char       subTitle[6][MAX_IN];    /* array of six subtitles */
  char         legend[6][MAX_IN];    /* array of six legends */
  char        xAxisTitle[MAX_IN];    /* x axis title */
  char        yAxisTitle[MAX_IN];    /* y axis title */
} Plot;

typedef struct traceRecord {
  Link  *baseList;
  Link  *pchgList;
  Link  *outpList;
  Link  *brchList;
  Link  *comoList;
  Link  *qbusList;
  Link  *trceList;
  Link  *stepList;
  Link  *plotList;
  Link  *setuList;
  int    trace;
  int    fail;
  long   query;
  FILE  *rptFile;
  FILE  *comFile;
  char   rptSpec[FILENAME_MAX];
  char   trcSpec[FILENAME_MAX];
  char   usrSpec[FILENAME_MAX];
  char   comSpec[FILENAME_MAX];
  char   solution[CFLOW_IPC_BUFF_SIZE];
  char   change_bus_types[CFLOW_IPC_BUFF_SIZE];
  char   agc[CFLOW_IPC_BUFF_SIZE];
  char   gen_drop[CFLOW_IPC_BUFF_SIZE];
  float  VXmax;
  float  VXmin;
  float  dVX;
  char   timeStamp[MAX_IN];
  char   userID[MAX_IN];
  char   IPFversion[MAX_IN];
  char   IPFexecute[MAX_IN];
  int    pageNumber;
  int    lineNumber;
  int    curvNumber;
  int    curvesPerPlot;
  int    xAxisType;
  int    yAxisType;
  int    xDiv;
  int    xGrd;
  int    yDiv;
  int    yGrd;
  int    nthPoint;
  float  firstX;
  float  lastX;
  float  firstY;
  float  lastY;
  float  XMIN;
  float  XMAX;
  float  YMIN;
  float  YMAX;
} Trace;

/******************************* end typedef **********************************/
/* top MIMIC functions - called by main() *************************************/
void  initializeTrace(Trace *trace);
void  processCommandLine(Trace *trace, int argc, char *argv[]);
void  instructions(Trace *trace);
void  checkIPFversion(Trace *trace);
void  promptUser(Trace *trace);
void  checkFileLists(Trace *trace);
void  getCoMoList(Trace *trace);
void  assembleSteps(Trace *trace);
void  buildTrace(Trace *trace);
void  printTrace(Trace *trace);
void  queryContinue(Trace *trace);
void  openReport(Trace *trace);
void  dispatchEngine(Trace *trace);
void  buildSetup(Trace *trace);
void  printXYsetup(FILE *fp, Trace *trace, Plot *plot);
void  printPSsetup(FILE *fp, Trace *trace, Plot *plot);
void  finalRemarks(Trace *trace);
void  cleanUpFiles(Trace *trace);
/* end MIMIC functions - called by main() *************************************/

/* top MIMIC functions - report functions *************************************/
void   printMainHeader(Trace *trace);
void   printMainFooter(Trace *trace);
void   printSubReport(Trace *trace, Step *step);
void   printQminHeader(Trace *trace);                           
void   printQminData(Trace *trace, Step *step);
void   printBlanks(Trace *trace);
void   printFooter(Trace *trace);
/* end MIMIC functions - report functions *************************************/

/* top MIMIC functions - support functions ************************************/
void   makeNewName(Step *curStep, int *outgNum);
void   checkFileList(Link *fileList);
int    findFile(Link *curFile, char *path);
int    ff_curve(Trace *trace, Step *step, pf_rec *r);
void   ff_user_analysis(Trace *trace, Step *step, char action);
int    ff_ch_par_qv(char *fn, char *bus, char *kv, float x, float *v, float *q);
void   ff_take_como_outg(FILE *comFile, char *comOutg);
void   ff_scale(float min, float max, float *first, float *last, float inc);
/* end MIMIC functions - support functions ************************************/

/* top LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/
void   ff_stream2List(FILE *readMe, Trace *trace, Link **expList);
void   ff_report(char *s, Trace *trace);
void   ff_limits(char *s, Trace *trace);
void   ff_traces(char *s, Trace *trace);
void   ff_traceName(Trace *trace);
Link  *ff_insRow(Link **table, Link *rowPtr, Link *newRow, Trace *trace);
void   ff_expList(Trace *trace, Link *dataList, Link **expList);
void   ff_getList(Trace *trace, char *dataFile, Link **expList);
void   ff_freeRowList(Link **rowList);
void   mf_fprintList(FILE *fp,Link *link,char *tag,char *tab, char type, int n);
void   ff_branch2rec(ff_branch *branch, pf_rec *rec);
void   ff_rec2branch(pf_rec *rec, ff_branch *branch);
void   ff_bus2rec(ff_bus *bus, pf_rec *rec);
void   ff_rec2bus(pf_rec *rec, ff_bus *bus);
Link  *ff_id2Link(char *s);
char  *ff_strclr(char *s, char fill);
char  *ff_strfil(char *s, char fill);
char  *ff_strsub(char *str, char *sub, char fill);
/* end LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/

/* top CF_UTIL.H candidate functions - common to: MIMIC, LINEFLOW, FINDOUT ****/
/*   all documented, CF_UTIL.H candidates shall be designated, vf_name().     */
/* end CF_UTIL.H candidate functions - common to: MIMIC, LINEFLOW, FINDOUT ****/
char         *strrep(char *s);

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
  getCoMoList(&trace);
  assembleSteps(&trace);
  buildTrace(&trace);
  printTrace(&trace);
  queryContinue(&trace);
  openReport(&trace);
  pf_cflow_init(argc, argv);      /* initialize cflow connection to powerflow */
  dispatchEngine(&trace);
  buildSetup(&trace);
  pf_cflow_exit();
  finalRemarks(&trace);
  cleanUpFiles(&trace);
  time(&CF_time1);
  cf_logUse("QVcurve", trace.IPFversion, trace.userID);
  return 0;
}
/* end of main() */
void  initializeTrace(Trace *trace)
{
  memset(trace, '\0', sizeof(trace));
  trace->baseList         =   NULL;
  trace->pchgList         =   NULL;
  trace->outpList         =   NULL;
  trace->brchList         =   NULL;
  trace->qbusList         =   NULL;
  trace->trceList         =   NULL;
  trace->stepList         =   NULL;
  trace->comoList         =   NULL;
  trace->plotList         =   NULL;
  trace->setuList         =   NULL;
  trace->trace            =   TRACE_YES;
  trace->fail             =   0;
  trace->query            = ( QUERY_BASE | QUERY_BRCH | QUERY_QBUS |
                              QUERY_USRA | QUERY_FILE | QUERY_SETU |
                              QUERY_LMTS | QUERY_COMO | QUERY_CHBT |
                              QUERY_CMOD | QUERY_PCHG | QUERY_SOLN |
                              QUERY_OUTP | QUERY_CONT | QUERY_CURV |
                              QUERY_AGCC | QUERY_GEND );
  strcpy(trace->solution, "/SOLUTION\n");
  trace->change_bus_types[0] = '\0';
  trace->agc[0] = '\0';
  trace->gen_drop[0] = '\0';
  trace->VXmax            = 1.10; /* typical 1.10 */
  trace->VXmin            = 0.90; /* typical 0.70 */
  trace->dVX              = 0.05; /* typical 0.02 */
  strcpy(trace->rptSpec, RPT_SPEC);
  strncpy(trace->IPFexecute, getenv("IPFSRV_CF"), MAX_IN);
  cf_parse(trace->IPFexecute, trace->IPFversion, 'F');
  cuserid(trace->userID);
  cf_time(trace->timeStamp, MAX_IN, CF_TIMESTAMP);
  strcpy(CF_logSpec, LOG_SPEC);
  trace->pageNumber       = 1;
  trace->lineNumber       = 0;
  trace->curvNumber       = 0;
  trace->curvesPerPlot    = 5;
  trace->xAxisType        = 1;
  trace->yAxisType        = 1;
  trace->xDiv             = 10;
  trace->xGrd             = 2;
  trace->yDiv             = 25;
  trace->yGrd             = 0;
  trace->nthPoint         = 1;
  trace->firstX           = trace->VXmin;
  trace->lastX            = trace->VXmax;
  trace->XMIN             = 0;
  trace->XMAX             = 0;
  trace->firstY           = 0;
  trace->lastY            = 0;
  trace->YMIN             = 0;
  trace->YMAX             = 0;
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
  printf("\n                      Welcome to QVcurve - updated 01-09-97");
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
    printf(" QVcurve was tested for use with IPF version %d.\n",FF_IPF_VERSION);
    printf(" You are currently using IPF version %d.\n", vn);
    printf(" QVcurve may not work as expected with IPF version %d.\n", vn);
    printf("\a - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\a\n");
  }
}
void promptUser(Trace *trace)
{
  char query[MAX_IN];
  Link *list;

  printf("%s", DOT_LINE);
  if (trace->query & QUERY_BASE) {
    printf("\n\n/BASECASE, /TRACE, or /INCLUDE");
    cf_nprompt("\n > Enter list of Basecase, *.TRC, or *.DAT files : ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->baseList);
  }
  if (trace->baseList==NULL && trace->stepList==NULL) {
    cf_exit(1, "No data or basecase files!  Quitting!\n");
  }

  if (trace->query == QUERY_CONT) trace->query &= ~(QUERY_CONT);

  if (trace->query & QUERY_PCHG) {
    printf("\n\n/PRI_CHANGE");
    cf_nprompt("\n > Enter list of Primary Change (or *.DAT) files : ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->pchgList);
  }
  if (trace->query & QUERY_OUTP) {
    printf("\n\n/OUTPUT");
    cf_nprompt("\n > Enter list of names for .qvpt and .user files : ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->outpList);
  }
  if (trace->query & QUERY_QBUS) {
    printf("\n\n/BUS");
    cf_nprompt("\n > Enter file of critical busses for Q-V analysis: ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->query & QUERY_QBUS) {
    printf(    "\n > Enter list of critical busses for Q-V analysis: ");
    printf(    "\n   > Btc<O>< Bus  ><KV>");
    do {
        cf_nprompt("\n   > ", "", MAX_IN, query);
        list = ff_id2Link(query);
        cf_appList(&trace->qbusList, list);
    } while (!cf_isblank(query));
  }
  if (trace->query & QUERY_BRCH) {
    printf("\n\n/BRANCH");
    cf_nprompt("\n > Enter file of branches for single-line outages: ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->query & QUERY_BRCH) {
    printf(    "\n > Enter list of branches for single-line outages: ");
    printf(    "\n   > Tyc<O>< Bus1 ><V1> < Bus2 ><V2>C");
    do {
        cf_nprompt("\n   > ", "", MAX_IN, query);
        list = ff_id2Link(query);
        cf_appList(&trace->brchList, list);
    } while (!cf_isblank(query));
  }
  if (trace->query & QUERY_CMOD) {
    printf("\n\n/REPORT");
    cf_sprompt("\n > Enter main common-mode file, COMMON_MODE_DATA=: ", "", trace->comSpec);
  }
  if (strlen(trace->comSpec)==0) {
    trace->query &= ~(QUERY_COMO);
  }
  if (trace->query & QUERY_COMO) {
    printf("\n\n/COMMON_MODE"); 
    cf_nprompt("\n > Enter file of common-mode outage identifiers  : ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->query & QUERY_COMO) {
    printf(    "\n > Enter list of common-mode outage identifiers  : ");
    do {
        cf_nprompt("\n   > ", "", MAX_IN, query);
        list = cf_text2Link(query);
        cf_appList(&trace->comoList, list);
    } while (!cf_isblank(query));
  }
  if (trace->qbusList==NULL && trace->stepList==NULL) {
    cf_exit(1, "No buses specified for Q-V analysis!  Quitting!\n");
  }
  if (trace->query & (QUERY_USRA | QUERY_FILE | QUERY_CURV)) {
    printf("\n\n/REPORT");
  }
  if (trace->query & QUERY_FILE) {
    cf_sprompt("\n > Enter summary report name, FILE =[%s]: ", trace->rptSpec, trace->rptSpec);
    cf_parse(trace->rptSpec, query, 'T');
    if (strlen(query)==0) strcat(trace->rptSpec, ".rpt");
  }
  if (trace->query & QUERY_USRA) {
    cf_sprompt("\n > Enter user analysis file,     USER_ANALYSIS = : ", "", trace->usrSpec);
  }
  if (trace->query & QUERY_CURV) {
    printf("\n Each plot can have from 1 to 6 curves:");
    cf_iprompt("\n > Enter number of curves,  CURVES_PER_PLOT = [%d]: ", trace->curvesPerPlot, &trace->curvesPerPlot);
    if ( trace->curvesPerPlot < 1 ) trace->curvesPerPlot = 1;
    if ( trace->curvesPerPlot > 6 ) trace->curvesPerPlot = 6;
  }
  if (trace->query & QUERY_LMTS) {
    printf("\n\n/LIMITS");
    printf("\n For Q-V curve analysis: Enter Min, Max, and Delta Voltage:");
    cf_fprompt("\n > Max BE bus voltage in PU,  VX_MAX_PU = [%5.3f]: ",trace->VXmax, &trace->VXmax);
    cf_fprompt("\n > Min BE bus voltage in PU,  VX_MIN_PU = [%5.3f]: ",trace->VXmin, &trace->VXmin);
    cf_fprompt("\n > Max voltage change in PU,DELTA_VX_PU = [%5.3f]: ",trace->dVX,   &trace->dVX  );
  }
  if (trace->query & (QUERY_SOLN | QUERY_CHBT | QUERY_AGCC | QUERY_GEND)) {
    printf("\n\n/SOLUTION, /CHANGE_BUS_TYPES, /AGC, or /GEN_DROP"); 
    cf_nprompt("\n > Enter file of sol'n and change_bus_type defaults: ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->query & QUERY_SOLN) {
    printf("\n\n/SOLUTION       (Enter /solution qualifier and parameters)");
    cf_nprompt("\n: ", "", MAX_IN, query);
    while (!cf_isblank(query)) {
        cf_aprint(trace->solution, "%s\n", query);
        cf_nprompt("\n: ", "", MAX_IN, query);
    }
  }
  if (trace->query & QUERY_CHBT) {
    printf("\n\n/CHANGE_BUS_TYPES (Enter /change_bus_type qualifiers and data)");
    cf_nprompt("\n> /CHANGE_BUS_TYPES, ", "", MAX_IN, query);
    while (!cf_isblank(query)) {
        if (strlen(trace->change_bus_types)==0)
            sprintf(trace->change_bus_types, "/CHANGE_BUS_TYPES, %s\n", query);
        else
            cf_aprint(trace->change_bus_types, "%s\n", query);
        cf_nprompt("\n: ", "", MAX_IN, query);
    }
  }
  if (trace->query & QUERY_AGCC) {
    printf("\n\n/AGC (bus id data is fixed format, the rest is free field)");
    printf("\n: B     < Bus  ><KV>   Pmin=<##>, Pmax=<##>, Pgen=<##>, %=<##>");
    cf_nprompt("\n: ", "", MAX_IN, query);
    while (!cf_isblank(query)) {
        if (strlen(trace->agc)==0) sprintf(trace->agc, "/AGC\n");
        cf_aprint(trace->agc, "%s\n", query);
        cf_nprompt("\n: ", "", MAX_IN, query);
    }
  }
  if (trace->query & QUERY_GEND) {
    printf("\n\n/GEN_DROP (Enter /gen_drop qualifiers and data)");
    cf_nprompt("\n> /GEN_DROP, ", "", MAX_IN, query);
    while (!cf_isblank(query)) {
        if (strlen(trace->gen_drop)==0)
            sprintf(trace->gen_drop, "/GEN_DROP, %s\n", query);
        else
            cf_aprint(trace->gen_drop, "%s\n", query);
        cf_nprompt("\n: ", "", MAX_IN, query);
    }
  }
  printf("\n");
  ff_traceName(trace);
  printf("\n%s\n", DOT_LINE);
  return;
}
void printTrace(Trace *trace)
{
  FILE *fp;

  if (trace->trace==TRACE_NO) return;
  fp = cf_openFile(trace->trcSpec, "w");
  if (fp == NULL) fp = stderr;
  fprintf(fp, ". %s %s %s %s\n", trace->trcSpec, trace->timeStamp, trace->userID,
    trace->IPFversion);


  fprintf(fp, "/REPORT\n");
  fprintf(fp, "  FILE = %s\n", trace->rptSpec);
  fprintf(fp, "  USER_ANALYSIS = %s\n", trace->usrSpec);
  fprintf(fp, "  CURVES_PER_PLOT = %d\n", trace->curvesPerPlot);
  fprintf(fp, "  COMMON_MODE_DATA = %s\n", trace->comSpec);
  if ( trace->trace == TRACE_YES) fprintf(fp, "  TRACE = YES\n");
  if ( trace->trace == TRACE_NO ) fprintf(fp, "  TRACE = NO\n");

  fprintf(fp, "/LIMITS\n");
  fprintf(fp, "  VX_MAX_PU   = %5.3f\n", trace->VXmax);
  fprintf(fp, "  VX_MIN_PU   = %5.3f\n", trace->VXmin);
  fprintf(fp, "  DELTA_VX_PU = %5.3f\n", trace->dVX);

  if ( trace->change_bus_types[0]!=0 ) {
    fprintf(fp, "%s", trace->change_bus_types);
  }
  else {
    fprintf(fp, "/CHANGE_BUS_TYPES\n");
  }
  if ( trace->agc[0]!=0 ) {
    fprintf(fp, "%s", trace->agc);
  }
  else {
    fprintf(fp, "/AGC\n");
  }
  if ( trace->gen_drop[0]!=0 ) {
    fprintf(fp, "%s", trace->gen_drop);
  }
  else {
    fprintf(fp, "/GEN_DROP\n");
  }
  if ( trace->solution[0]!=0 ) {
    fprintf(fp, "%s", trace->solution);
  }

  if (trace->baseList==NULL) {
    mf_fprintList(fp, trace->trceList, "/TRACE\n",        " ",  's',  1);
  }
  else {
    mf_fprintList(fp, trace->baseList, "/BASECASE\n",     "  ", 's',  1);
    mf_fprintList(fp, trace->pchgList, "/PRI_CHANGE\n",   "  ", 's',  1);
    mf_fprintList(fp, trace->outpList, "/OUTPUT\n",       "  ", 's',  1);
    mf_fprintList(fp, trace->setuList, "/SETUP\n",        "  ", 's',  1);
    mf_fprintList(fp, trace->qbusList, "/BUS\n",          "",   'I',  1);
    mf_fprintList(fp, trace->brchList, "/BRANCH\n",       "",   'i',  1);
    mf_fprintList(fp, trace->comoList, "/COMMON_MODE\n",  "",   's',  1);
    mf_fprintList(fp, trace->trceList, "./TRACE\n",       ". ", 's',  1);
  }
  fclose(fp);
}
void  checkFileLists(Trace *trace)
{
  checkFileList(trace->baseList);
  checkFileList(trace->pchgList);
}
void  getCoMoList(Trace *trace)
{
  char s[MAX_IN], c[MAX_IN];
  Link *curComo;

  if ( strlen(trace->comSpec)==0 ) {
    trace->comoList = NULL;
  }
  else if ((trace->comFile=fopen(trace->comSpec, "r"))==NULL) {
    cf_logErr("Skipping common-mode outages: Cannot find %s\n", trace->comSpec);
    trace->comoList = NULL;
  }
  else if (trace->comoList==NULL && trace->stepList==NULL) {
    rewind(trace->comFile);
    while ( fgets(s, MAX_IN, trace->comFile) != NULL ) {
        if ( s[0] != '>' ) continue;              /* skip comments, skip data */
        if ( strstr(s, "common_mode")!=NULL ) continue;
        if ( strstr(s, "mode ")==NULL ) continue;
        if ( sscanf(s, "> mode %80[^\n]", c) == 1 ) {
            printf("  Found: {%s}\n", c);
            curComo = cf_text2Link(c);
            cf_appList(&trace->comoList, curComo);
        }
    }
  }
}
void  assembleSteps(Trace *trace)
{
  Link   *stepLink, *plotLink;
  Link   *curBase, *curPchg, *curBrch, *curComo, *curQbus, *curOutp, *curSetu;
  Step   *step;
  Plot   *plot;
  int     bran, como, outg, qbus, curv, setu, type;
  char    baseName[FILENAME_MAX], pchgName[FILENAME_MAX], curvName[MAX_IN];
  char    idenName[FILENAME_MAX];
  pf_rec *brch;

/* test mimic function : Loop through all cases in base case list */
  if (trace->stepList != NULL) return;
  stepLink= trace->stepList;
  plotLink= trace->plotList;
  curSetu = trace->setuList;
  curBase = trace->baseList;
  curPchg = trace->pchgList;
  curOutp = trace->outpList;
  curBrch = trace->brchList;
  curComo = trace->comoList;
  curQbus = trace->qbusList;
  bran = 1; como = 1; outg = 1; qbus = 1; curv = 1; setu = 1; type = 'X';
  while (curBase != NULL) {
    stepLink = cf_addLink(stepLink, sizeof(Step));
    step = (Step *) stepLink->data;
    if (trace->stepList==NULL) trace->stepList = stepLink;

    strcpy(step->oldBase, (char *) curBase->data);

    if (curPchg!=NULL) {
        strcpy(step->priChng, (char *) curPchg->data);
    }
    if (curQbus!=NULL) {
        ff_bus2rec((ff_bus *) curQbus->data, (pf_rec *) &step->busID);
    }
    if (curBrch!=NULL) {
        step->outgLink = cf_malloc(sizeof(pf_rec));
        ff_branch2rec((ff_branch *) curBrch->data, (pf_rec *) step->outgLink);
        step->type = OUTG_BRCH;
        type = 'B';
        outg = bran;
    }
    else if (curComo!=NULL) {
        step->outgLink = cf_malloc(strlen(curComo->data)+1);
        strcpy(step->outgLink, curComo->data);
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
    if (curOutp==NULL) {
        if (type=='X') sprintf(idenName, "%02d", qbus);
        else           sprintf(idenName, "%c%02d-%02d", type, outg, qbus);
        sprintf(step->qvcRept, "%.17s-%s.qvpt", baseName, idenName);
    }
    else {
        strcpy(idenName, curOutp->data);
        cf_strsuf(step->qvcRept, curOutp->data, '.', ".qvpt");
    }
    if (trace->usrSpec[0]!='\0') {
        cf_strsuf(step->usrRept, step->qvcRept, '.', ".user");
    }

/* APLY code */
    if (curQbus!=NULL) {
        curQbus = curQbus->next; qbus++;
    }
    if (curQbus==NULL) {
        if      (curBrch!=NULL) { curBrch = curBrch->next; bran++; }
        else if (curComo!=NULL) { curComo = curComo->next; como++; }
    }
    if (curQbus==NULL && curBrch==NULL && curComo==NULL) {
        if (curPchg!=NULL) curPchg = curPchg->next;
    }
    if (curQbus==NULL && curBrch==NULL && curComo==NULL && curPchg==NULL) {
        curBase = curBase->next;
    }
    if (curOutp!=NULL) curOutp = curOutp->next;
    if (curQbus==NULL) {
        curQbus = trace->qbusList; qbus = 1;
    }
    if (curBrch==NULL && curComo==NULL) {
        curBrch = trace->brchList; bran = 1;
        curComo = trace->comoList; como = 1;
    }
    if (curPchg==NULL) {
        curPchg = trace->pchgList;
    }
/* end APLY */

    if (curv==1) {
        plotLink = cf_addLink(plotLink, sizeof(Plot));
        if (plotLink==NULL) continue;
        plot = (Plot *) plotLink->data;
        if (trace->plotList==NULL) trace->plotList = plotLink;

        cf_parse(trace->rptSpec, plot->setUpXY, 'N');
        cf_parse(trace->rptSpec, plot->setUpPS, 'N');
        sprintf(plot->superTitle, "QV Analysis");
        sprintf(plot->xAxisTitle, "Critical Bus Voltage (P.U.)");
        sprintf(plot->yAxisTitle, "Reactive at Critical Bus (MVAR)");

        if (curSetu!=NULL) {
            cf_strsuf(plot->setUpXY, curSetu->data, '.', ".setup");
            cf_strsuf(plot->setUpPS, curSetu->data, '.', ".xyc");
            curSetu = curSetu->next;
        }
        else {
            cf_aprint(plot->setUpXY, "_%d.setup", setu);
            cf_aprint(plot->setUpPS, "_%d.xyc", setu);
            setu++;
        }
    }

    if (step->type == OUTG_BRCH) {
        brch = (pf_rec *) step->outgLink;
        sprintf(curvName, "%8.8s-%8.8s %5.1f     (%8.8s %3.0f)",
            brch->i.branch.bus1_name, brch->i.branch.bus2_name,
            brch->i.branch.bus1_kv,
            step->busID.i.ACbus.name, step->busID.i.ACbus.kv);
    }
    else if (step->type == OUTG_COMO) {
        sprintf(curvName, "%-27.27s (%8.8s %3.0f)",
            step->outgLink, step->busID.i.ACbus.name, step->busID.i.ACbus.kv);
    }
    else { /* step->type == OUTG_NONE */
        sprintf(curvName, "(%8.8s %3.0f)",
            step->busID.i.ACbus.name, step->busID.i.ACbus.kv);
    }
    if (curOutp==NULL) {
        sprintf(plot->legend[curv-1], "%-10.10s", idenName);
        sprintf(step->legend, "%-12.12s", idenName);
    }
    else {
        sprintf(plot->legend[curv-1], "%-10.10s", curOutp->data);
        sprintf(step->legend, "%-12.12s", curOutp->data);
    }
    if (trace->curvesPerPlot==MAX_CURVES) {
      if (curv==1) /*overwrite title*/
        sprintf(plot->superTitle, "%-42.42s %7.7s", curvName, idenName);
      else
        sprintf(plot->subTitle[curv-2], "%-42.42s %7.7s", curvName, idenName);
    }
    else {
        sprintf(plot->subTitle[curv-1], "%-42.42s %7.7s", curvName, idenName);
    }
    sprintf(step->subTitle, "%-42.42s %7.7s", curvName, idenName);
    strcpy(plot->qvpFile[curv-1], step->qvcRept);

    plot->numberOfCurves = curv;
    curv++;
    if (curv > trace->curvesPerPlot) curv = 1;
  }
}
void queryContinue(Trace *trace)
{
  int yes;

  if (trace->trace == TRACE_YES)
    printf("\nTrace written to %s", trace->trcSpec);
  if (CF_logFile != NULL)
    printf("\nError Summary written to %s", CF_logSpec);
  printf("\n%s\n", DOT_LINE);
  mf_fprintList(stdout, trace->trceList, "/TRACE\n",      "  ", 's',  1);
  printf("%s\n", DOT_LINE);
  if ( trace->query & QUERY_CONT) {
    printf(" Please examine the above /TRACE of QVcurve operations.");
    yes = cf_yprompt("\n > Do you want to continue this run? [%c]: ", 'Y');
    cf_exit(!yes, "Quitting!");
  }
}
void openReport(Trace *trace)
{
  trace->rptFile = cf_openFile(trace->rptSpec, "w");
  cf_exit(trace->rptFile==NULL, "Quitting!\n");
  return;
}
void  dispatchEngine(Trace *trace)
{
  Step *step;
  char  s[CFLOW_IPC_BUFF_SIZE];
  Link *stepLink;
  int   go;
  pf_rec b, m;

/**************** BEGINNING OF POWERFLOW DISPATCH ENGINE **********************/
  printMainHeader(trace);
  printQminHeader(trace);

  printf("Processing... \n");

  for (stepLink = trace->stepList; stepLink != NULL; stepLink = stepLink->next){
    step = (Step *) stepLink->data;
    go = 1;
    trace->fail = 0;

    cleanUpFiles(trace);

    printf("\n  Loading Base...%s\n", step->oldBase);
    if (pf_load_oldbase(step->oldBase)!=0) {
        cf_logErr("Skipping base case: Cannot load %s\n", step->oldBase);
        trace->fail = FAIL_BASE;
        go = 0;
    }
    if (go && trace->change_bus_types[0]!='\0') {
        printf("%s", trace->change_bus_types);               
        if (pf_command(trace->change_bus_types)!=0) {
            cf_logErr("Skipping /CHANGE_BUS_TYPES\n");
            trace->fail = FAIL_SOLV;
            go = 0;
        }
    }
    if (go && strlen(trace->agc)!=0) {
        printf("%s", trace->agc);               
        if (pf_command(trace->agc)!=0) {
            cf_logErr("Skipping /AGC\n");
            trace->fail = FAIL_SOLV;
            go = 0;
        }
    }
    if (go && trace->gen_drop[0]!='\0') {
        printf("%s", trace->gen_drop);               
        if (pf_command(trace->gen_drop)!=0) {
            cf_logErr("Skipping /GEN_DROP\n");
            trace->fail = FAIL_SOLV;
            go = 0;
        }
    }
    if (go && strlen(step->priChng)!=0) {            /* apply primary changes */
        printf("  Applying Primary Changes... %s\n", step->priChng);
        if (pf_load_changes(step->priChng)!=0) {
            cf_logErr("Skipping changes:  Cannot use %s\n", step->priChng);
            cf_logErr("%s\n", err_buf);
            trace->fail = FAIL_PCHG;
            go = 0;
        }
    }

    ff_user_analysis(trace, step, 'A');

    if (go && step->type==OUTG_BRCH) {                       /* apply changes */
        printf("  Taking Branch Outage...\n");
        memset(&b, '\0', sizeof(pf_rec));
        memcpy(&b, step->outgLink, sizeof(pf_rec));
        pf_rec_b2a(s, &b, "D");
        printf("    %s\n", s);
        pf_rec_branch(&b, "D");                                /* take outage */
    }
    if (go && step->type==OUTG_COMO) {            /* apply common-mode outage */
        printf("  Taking Common-mode Outage... %s\n", (char *) step->outgLink);
        ff_take_como_outg(trace->comFile, (char *) step->outgLink);
    }
    if (go) {
        printf("  Modifying QV bus...\n");
        memset(&m, '\0', sizeof(pf_rec));
        memcpy(&m.i.ACbus, &step->busID, sizeof(pf_AC_bus));
        pf_rec_bus(&m, "G");                             /* get target QV bus */

        m.i.ACbus.Vhold_Vmax = trace->VXmax;
        strcpy(m.i.ACbus.type, "BE");
        pf_rec_b2a(s, &m, "M");
        printf("    %s\n", s);
        pf_rec_bus(&m, "M");                          /* modify target QV bus */
    }
    if (go) {
        printf("  Solving...\n");
        printf("%s", trace->solution);
        if (pf_command(trace->solution)!=0) {        /* solution + qualifiers */
            cf_logErr("Skipping base case:  Cannot solve %s\n", step->oldBase);
            trace->fail = FAIL_SOLV;
            go = 0;
        }
    }
    printf("  Making QV curve...\n");
    ff_curve(trace, step, &m);
    printf("\n\r");

    printSubReport(trace, step);
  }
  printMainFooter(trace);

  return;
/**************** END OF POWERFLOW DISPATCH ENGINE ****************************/
}
void  buildSetup(Trace *trace)
{
  FILE *fp;
  Plot *plot;
  char  s[CFLOW_IPC_BUFF_SIZE], data[MAX_IN];
  Link *plotLink;

  for (plotLink = trace->plotList; plotLink != NULL; plotLink = plotLink->next){
    plot = (Plot *) plotLink->data;
    printf("Building Setup File... %s\n", plot->setUpXY);
    fp = cf_openFile(plot->setUpXY, "w");
    if (fp==NULL) continue;
    printXYsetup(fp, trace, plot);
    fclose(fp);

    printf("Building Setup File... %s\n", plot->setUpPS);
    fp = cf_openFile(plot->setUpPS, "w");
    if (fp==NULL) continue;
    printPSsetup(fp, trace, plot);
    fclose(fp);
  }
}
void printXYsetup(FILE *fp, Trace *trace, Plot *plot)
{
  int  curv;

  ff_scale(trace->XMIN, trace->XMAX, &trace->firstX, &trace->lastX, 0.10);
  ff_scale(trace->YMIN, trace->YMAX, &trace->firstY, &trace->lastY, 500.);
  trace->yDiv   = (int) ((trace->lastY - trace->firstY) / 100);
  trace->xDiv   = (int) ((trace->lastX - trace->firstX) / 0.02);

  trace->nthPoint = (int) ( (trace->lastX-trace->firstX)/(10*trace->dVX) );
  if (trace->nthPoint==0) trace->nthPoint = 1;

  fprintf(fp, "Orientation : (P)ortrait or (L)andscape (default is P) > P\n");
  fprintf(fp, "Enter FILE NAME(S) followed by <Return>, Enter a <Return> to end.\n");
  fprintf(fp, "\n");
  fprintf(fp, "   File name(s)                                 Pts      *Mode       Legend\n");
  for (curv = 1; curv <= plot->numberOfCurves; curv++) {
    fprintf(fp, " %d %-32.32s             0           +%-2d      %-.10s\n",
        curv, plot->qvpFile[curv-1], trace->nthPoint, plot->legend[curv-1]);
  }
  fprintf(fp, "\n");
  fprintf(fp, "  *Mode : 0 for connection points by straight lines, without symbols.\n");
  fprintf(fp, "         +n for connecting points by straight lines, with symbols.\n");
  fprintf(fp, "         -n plotting symbols at points, no lines. Every nth point is plotted.\n");
  fprintf(fp, "\n");
  fprintf(fp, "\n");
  fprintf(fp, "                             X AXIS TYPE & PARAMETERS\n");
  fprintf(fp, "\n");
  fprintf(fp, "\n");
  fprintf(fp, "         Types of axes available : 1  - linear\n");
  fprintf(fp, "                                   2  - log\n");
  fprintf(fp, "                                   3  - normal prob.\n");
  fprintf(fp, "                                   4  - extreme prob.\n");
  fprintf(fp, "                                   5  - months\n");
  fprintf(fp, "        Enter (default type is 1) >1<\n");
  fprintf(fp, "\n");
  fprintf(fp, "\n");
  fprintf(fp, "  XMIN =  %f, XMAX =  %f\n", trace->XMIN, trace->XMAX);
  fprintf(fp, "\n");
  fprintf(fp, "     Number of divisions on axis  >%-2d<                Draw grid (Y/N)  >Y<\n", trace->xDiv);
  fprintf(fp, "     Number of grid subdivisions  >%-2d<     Digits after decimal point  >2<\n", trace->xGrd);
  fprintf(fp, "                     First value  >%-5.3f<                  Last value  >%-5.3f<\n", trace->firstX, trace->lastX);
  fprintf(fp, "\n");
  fprintf(fp, " X axis title : %s\n", plot->xAxisTitle);
  fprintf(fp, " X axis title :\n");
  fprintf(fp, "\n");
  fprintf(fp, "\n");
  fprintf(fp, "                             Y AXIS TYPE & PARAMETERS\n");
  fprintf(fp, "\n");
  fprintf(fp, "\n");
  fprintf(fp, "         Types of axes available : 1  - linear\n");
  fprintf(fp, "                                   2  - log\n");
  fprintf(fp, "        Enter (default type is 1) >1<\n");
  fprintf(fp, "\n");
  fprintf(fp, "\n");
  fprintf(fp, "  YMIN =  %f, YMAX =  %f\n", trace->YMIN, trace->YMAX);
  fprintf(fp, "\n");
  fprintf(fp, "     Number of divisions on axis  >%-2d<                Draw grid (Y/N)  >Y<\n", trace->yDiv);
  fprintf(fp, "     Number of grid subdivisions  >%-2d<     Digits after decimal point  >0<\n", trace->yGrd);
  fprintf(fp, "                     First value  >%-5.1f<                Last value  >%-5.1f<\n", trace->firstY, trace->lastY);
  fprintf(fp, "\n");
  fprintf(fp, " Y axis title : %s\n", plot->yAxisTitle);
  fprintf(fp, " Y axis title :\n");
  fprintf(fp, "\n");
  fprintf(fp, "\n");
  fprintf(fp, "  1 supertitle and 5 subtitles are allowed with a maximum of 50 characters\n");
  fprintf(fp, "  per title.\n");
  fprintf(fp, "\n");
  fprintf(fp, "  Supertitle > %s\n", plot->superTitle);
  fprintf(fp, "\n");
  if (trace->curvesPerPlot == MAX_CURVES) {
    for (curv = 1; curv <= plot->numberOfCurves-1; curv++) {
        fprintf(fp, "  Subtitle %d > %s\n", curv, plot->subTitle[curv-1]);
    }
  }
  else {
    for (curv = 1; curv <= plot->numberOfCurves; curv++) {
        fprintf(fp, "  Subtitle %d > %s\n", curv, plot->subTitle[curv-1]);
    }
  }
}
void printPSsetup(FILE *fp, Trace *trace, Plot *plot)
{
  int  curv;

  ff_scale(trace->XMIN, trace->XMAX, &trace->firstX, &trace->lastX, 0.10);
  ff_scale(trace->YMIN, trace->YMAX, &trace->firstY, &trace->lastY, 500.);
  trace->yDiv   = (int) ((trace->lastY - trace->firstY) / 100);
  trace->xDiv   = (int) ((trace->lastX - trace->firstX) / 0.02);
  trace->nthPoint = (int) ( (trace->lastX-trace->firstX)/(10*trace->dVX) );
  if (trace->nthPoint==0) trace->nthPoint = 1;

  fprintf(fp, "/SUPER_TITLE\n");
  fprintf(fp, "QV Analysis\n");
  fprintf(fp, "/X_TITLE\n");
  fprintf(fp, "%s\n", plot->xAxisTitle);
  fprintf(fp, "/Y_TITLE\n");
  fprintf(fp, "%s\n", plot->yAxisTitle);
  fprintf(fp, "/SUB_TITLE\n");
  fprintf(fp, "/LIMITS\n");
  fprintf(fp, "XMIN = %f\n", trace->firstX);
  fprintf(fp, "XMAX = %f\n", trace->lastX);
  fprintf(fp, "XINC = %f\n", 0.02);
  fprintf(fp, "YMIN = %f\n", trace->firstY);
  fprintf(fp, "YMAX = %f\n", trace->lastY);
  fprintf(fp, "YINC = %f\n", 100.);
  fprintf(fp, "MARK = %d\n", trace->nthPoint);
  fprintf(fp, "/XY_DATA\n");
  for (curv = 1; curv <= plot->numberOfCurves; curv++) {
    fprintf(fp, "%s\n", plot->qvpFile[curv-1]);
  }
}
void  finalRemarks(Trace *trace)
{
  printf("\n");
  printf("\nMemory allocation (bytes): Cur:%d Max:%d Alloc:%d Freed:%d\n",
    CF_memCurAlloc, CF_memMaxAlloc, CF_memTotAlloc, CF_memTotFreed);
  printf("\nOutput report written to %s", trace->rptSpec);
  if (trace->trace == TRACE_YES)
    printf("\nTrace  report written to %s", trace->trcSpec);
  if (CF_logFile != NULL) printf("\nError  report written to %s", CF_logSpec);
  printf("\n");
}
void  cleanUpFiles(Trace *trace)
{
  char s[MAX_IN];

  sprintf(s, "%s %s.PFO", FILE_PURG, trace->userID); system(s);
  sprintf(s, "%s %s.PFD", FILE_PURG, trace->userID); system(s);
  sprintf(s, "%s FOR027.DAT%s", FILE_KILL, FILE_SUFX); system(s);
  sprintf(s, "%s FOR023.DAT%s", FILE_KILL, FILE_SUFX); system(s);
}
int ff_curve(Trace *trace, Step *step, pf_rec *r)
{
  FILE *fp;
  char  id[132], name[9], volt[5];
  float VX, VXmax, VXmin, dVX, Vbase, v, q;
  float Qmn, Vmn, Vq, Qv, Qmn_1, Vq_1, q_1, v_1, dVdQ;
  int   rtn;

  fp = cf_openFile(step->qvcRept, "w");
  if (fp==NULL) return -1;

  pf_rec_b2a(id, r, "I");
  sprintf(name, "%-8.8s", &id[6]);
  sprintf(volt, "%-4.4s", &id[14]);
  ff_strfil(name, '#');
  Vbase = r->i.ACbus.kv;

  VXmax = trace->VXmax;
  VXmin = trace->VXmin;
  dVX   = trace->dVX;

  VX = VXmax;
  rtn = ff_ch_par_qv("NULL", name, volt, VX, &v, &q);
  if (rtn!=0) {
    fprintf(fp, "C1 %42.42s NO SOLN\n", step->subTitle);
    strcpy(step->legend, "NO SOLN");
    fprintf(fp, "C2 %s\n", step->legend);
    fprintf(fp, "0 0\n", v, q);
    printf("%s %s, V =%13.10f (PU)  NO SOLUTION\n", name, volt, VX);
    return rtn;
  }
  fprintf(fp, "C1 %s\n", step->subTitle);
  fprintf(fp, "C2 %s\n", step->legend);
  fprintf(fp, "%.10f %.10f\n", v, q);
  printf("%s %s, V =%13.10f (PU)  Q =%17.10f (MVAR)\n", name, volt, v, q);

  trace->curvNumber++;

  Qmn = q; Vq = v;
  Vmn = v; Qv = q;
  q_1 = q; v_1 = v;

  if ( (q > trace->YMAX) || (trace->curvNumber==1) ) trace->YMAX = q;
  if ( (q < trace->YMIN) || (trace->curvNumber==1) ) trace->YMIN = q;

  if ( (v > trace->XMAX) || (trace->curvNumber==1) ) trace->XMAX = v;
  if ( (v < trace->XMIN) || (trace->curvNumber==1) ) trace->XMIN = v;

  for (VX = VXmax - (1*dVX); VX >= VXmin; VX = VX - dVX) {
    q_1 = q;
    v_1 = v;
    rtn = ff_ch_par_qv("NULL", name, volt, VX, &v, &q);
    if (rtn!=0) {
        printf("%s %s, V =%13.10f (PU)  NO SOLUTION\n", name, volt, v);
        break;
    }
    fprintf(fp, "%.10f %.10f\n", v, q);
    if (q > trace->YMAX) trace->YMAX = q;
    if (q < trace->YMIN) trace->YMIN = q;
    if (q <= Qmn) {
        Qmn = q;
        Vq = v;
        Qmn_1 = q_1;
        Vq_1  = v_1;
    }
    if (v > trace->XMAX) trace->XMAX = v;
    if (v < trace->XMIN) trace->XMIN = v;
    if (v <= Vmn) {
        Vmn = v;
        Qv = q;
    }
    if (q!=q_1) {
        dVdQ = (Vbase * (v - v_1)) / (q - q_1);
    }
    else dVdQ = 0;
    printf("%s %s, V =%13.10f (PU)  Q =%17.10f (MVAR)  dVdQ =%8.3f (kV/MVAR)\n", name, volt, v, q, dVdQ);
  }
  step->Qmin = Qmn;
  step->VatQmin = Vq;
  step->Vmin = Vmn;
  step->QatVmin = Qv;
  if (Qmn==Qmn_1) step->dVdQatQmin = 0;
  else {
    step->dVdQatQmin = (Vbase * (Vq_1 - Vq)) / (Qmn_1 - Qmn);
  }

  return 0;
}
int ff_ch_par_qv(char *file, char *name, char *kv, float x, float *v, float *q)
{
  char data[CFLOW_IPC_BUFF_SIZE];
  int  ipf_stat;

  sprintf(data,"/CHANGE_PARAMETER,FILE=%s,BUS=%8.8s %4.4s,VX=%f,QY=?\n(END)\n",
    file, name, kv, x);

  ipf_stat = pf_command(data);
  if ( sscanf(reply_pf, "%*17c V  = %f Q  = %f", v, q) != 2 ) return -1;
  if ( strstr(reply_pf, "failed") != NULL ) return -1;
  return ipf_stat;
}
void ff_user_analysis(Trace *trace, Step *step, char action)
{
  char cmd[CFLOW_IPC_BUFF_SIZE];
  if (strlen(trace->usrSpec)==0) return;

/* apply user-analysis file */

  sprintf(cmd,"/report,select user_analysis,file=%s,output=%s", trace->usrSpec,
    step->usrRept);
  printf("  %s\n", cmd);
  if (pf_command(cmd)!=0) {
    cf_logErr("Skipping changes:  Cannot use %s\n", trace->usrSpec);
    return;
  }

/*
  if (pf_user_report(trace->usrSpec, step->usrRept, action)!=0) {
    cf_logErr("Skipping changes:  Cannot use %s\n", trace->usrSpec);
    return;
  }
*/
  printf("  Writing User Analysis report to %s ...\n", step->usrRept);
}
void ff_take_como_outg(FILE *comFile, char *comOutg)
{
  char s[MAX_IN], cmd[CFLOW_IPC_BUFF_SIZE];
  int  mode;

  mode = 0;                                      /* searching */
  rewind(comFile);
  strcpy(cmd, "/changes, file = *\n");
  while (fgets(s, MAX_IN, comFile)!=NULL) {
    if ( s[0] == '.' ) continue;                 /* skip comments */
    if ( s[0] == '>' ) {
        if (mode==0) mode = 1;                   /* command mode */
        if (mode==2) {                           /* submit changes */
            printf("%s\n", cmd);
            pf_command(cmd);
            return;                     /* done with this outage */
        }
    }
    if ( mode == 1 ) {
        if ( strstr(s, "common_mode")!=NULL ) {
            mode = 0;
        }
        else if ( strstr(s, "mode ")!=NULL ) {
            mode = 1;
        }
        else {                                   /* unrecognized command */
            mode = 0;                            /* searching */
        }
    }
    if ( mode==1 ) {
        if ( strstr(s, comOutg)!=NULL ) {
            printf("  Found: %s\n", s);
            mode = 2;                            /* found changes */
            continue;
        }
        else mode = 0;                           /* searching */
    }
    if ( mode==2 ) {
        strcat(cmd, s);                          /* append to command */
    }
  }
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
        ff_strfil(net_data, '#');
        cf_aprint(newLine, "- [%26.26s] ", &net_data[6]);
    }
    if (step->type==OUTG_COMO) {
        strcpy(net_data, (char *) step->outgLink);
        ff_strfil(net_data, '#');
        cf_aprint(newLine, "- {%s} ", net_data);
    }

    pf_rec_b2a(net_data, (pf_rec *) &step->busID, "I");
    ff_strfil(net_data, '#');
    cf_aprint(newLine, "@ (%12.12s) ", &net_data[6]);

    if (trace->usrSpec[0]!='\0') {
        cf_aprint(newLine, "& %s ", trace->usrSpec);
    }
    if (step->qvcRept[0]!='\0') {
        cf_aprint(newLine, "> %s ", step->qvcRept);
    }
    if (step->usrRept[0]!='\0') {
        cf_aprint(newLine, "^ %s ", step->usrRept);
    }
    curLink = cf_addLink(curLink, (size_t) strlen(newLine) + 1);
    if (curLink!=NULL) strcpy((char *) curLink->data, newLine);
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
void ff_getList(Trace *trace, char *dataFile, Link **expList)
{ /* version of ff_expList for use with only one file name */
  FILE *readMe;

  readMe = cf_openFile(dataFile, "r");/* it's okay if readMe==NULL */
  ff_stream2List(readMe, trace, expList);
  fclose(readMe);
}
void ff_stream2List(FILE *readMe, Trace *trace, Link **expList)
{
  char str[MAX_IN], STR[MAX_IN], *cp;
  Link *list, *link;
  int mode = READ_INC;/* default /INCLUDE */
  while (fgets(str, MAX_IN, readMe)!=NULL) { 
    if ( cf_iscomment(str) ) continue;
    strcpy(STR, str);
    cf_str2upper(STR);
    if ( strstr(STR, "/INCLUDE"     )!=NULL ) { mode = READ_INC; }
    if ( strstr(STR, "/REPORT"      )!=NULL ) { mode = READ_REP; }
    if ( strstr(STR, "/BASECASE"    )!=NULL ) { mode = READ_BSE; }
    if ( strstr(STR, "/BUS"         )!=NULL ) { mode = READ_BUS; }
    if ( strstr(STR, "/OUTPUT"      )!=NULL ) { mode = READ_OTP; trace->query &= ~(QUERY_OUTP); }
    if ( strstr(STR, "/SETUP"       )!=NULL ) { mode = READ_SET; trace->query &= ~(QUERY_SETU); }
    if ( strstr(STR, "/PRI_CHANGE"  )!=NULL ) { mode = READ_PCH; trace->query &= ~(QUERY_PCHG); }
    if ( strstr(STR, "/BRANCH"      )!=NULL ) { mode = READ_BRN; trace->query &= ~(QUERY_BRCH); }
    if ( strstr(STR, "/COMMON_MODE" )!=NULL ) { mode = READ_COM; trace->query &= ~(QUERY_COMO); }
    if ( strstr(STR, "/SOLUTION"    )!=NULL ) { mode = READ_SOL; trace->query &= ~(QUERY_SOLN); }
    if ( strstr(STR, "/CHANGE_BUS_T")!=NULL ) { mode = READ_CBT; trace->query &= ~(QUERY_CHBT); }
    if ( strstr(STR, "/AGC"         )!=NULL ) { mode = READ_AGC; trace->query &= ~(QUERY_AGCC); }
    if ( strstr(STR, "/GEN_DROP"    )!=NULL ) { mode = READ_GDR; trace->query &= ~(QUERY_GEND); }
    if ( strstr(STR, "/TRACE"       )!=NULL ) { mode = READ_TRC; }
    if ( strstr(STR, "/LIMITS"      )!=NULL ) { mode = READ_LIM; }
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
    if ( mode == READ_OTP ) {
        list = cf_text2List(str);
        cf_appList(&trace->outpList, list);
        trace->query &= ~(QUERY_OUTP);
    }
    if ( mode == READ_SET ) {
        list = cf_text2List(str);
        cf_appList(&trace->setuList, list);
        trace->query &= ~(QUERY_SETU);
    }
    if ( mode == READ_BRN ) {
        list = ff_id2Link(str);
        cf_appList(&trace->brchList, list);
        if (list!=NULL) {
            trace->query &= ~(QUERY_BRCH);
            trace->query &= ~(QUERY_COMO);
        }
    }
    if ( mode == READ_COM ) {
        list = cf_text2Link(str);
        cf_appList(&trace->comoList, list);
        if (list!=NULL) {
            trace->query &= ~(QUERY_BRCH);
            trace->query &= ~(QUERY_COMO);
        }
    }
    if ( mode == READ_BUS ) {
        list = ff_id2Link(str);
        cf_appList(&trace->qbusList, list);
        trace->query &= ~(QUERY_QBUS);
    }
    if ( mode == READ_SOL ) {
        strcat(trace->solution, str);
        strcat(trace->solution, "\n");
        trace->query &= ~(QUERY_SOLN);
    }
    if ( mode == READ_CBT ) {
        if (trace->change_bus_types[0]=='\0')
            sprintf(trace->change_bus_types, "/CHANGE_BUS_TYPES,");
        strcat(trace->change_bus_types, str);
        strcat(trace->change_bus_types, "\n");
        trace->query &= ~(QUERY_CHBT);
    }
    if ( mode == READ_AGC ) {
        if (trace->agc[0]=='\0')
            sprintf(trace->agc, "/AGC\n");
        strcat(trace->agc, str);
        strcat(trace->agc, "\n");
        trace->query &= ~(QUERY_AGCC);
    }
    if ( mode == READ_GDR ) {
        if (trace->gen_drop[0]=='\0')
            sprintf(trace->gen_drop, "/GEN_DROP,");
        strcat(trace->gen_drop, str);
        strcat(trace->gen_drop, "\n");
        trace->query &= ~(QUERY_GEND);
    }
    if ( mode == READ_REP ) ff_report(str, trace);
    if ( mode == READ_LIM ) ff_limits(str, trace);
    if ( mode == READ_TRC ) ff_traces(str, trace);
  }
  return;
}
void ff_traces(char *s, Trace *trace)
{
  Step *step, *trcData;
  Link *stepLink, *trcLink;
  char  op, *cp;

  char  brn[132], bus[132];
  char  type[3], n1[9], v1[5], n2[9], v2[5], ct;
  int   conv;
  float kv1, kv2;

  stepLink = cf_newLink(sizeof(Step));
  step = (Step *) stepLink->data;
  for (cp=strtok(s, " "), op = ' '; cp != NULL; cp=strtok(NULL, " ")) {
    if (strchr("+-@&^>", *cp)) { op = *cp; continue; }
/*    if (ispunct(*cp)) { op = *cp; continue; } */
    if (op == ' ') strcpy(step->oldBase, cp);
    if (op == '+') strcpy(step->priChng, cp);
    if (op == '-') {
        if (*cp == '[') {
            step->outgLink = cf_malloc(sizeof(pf_rec));
            sscanf(cp,"[%8[^\n]%4[^\n]%*c%8[^\n]%4[^\n]%c]",
                n1,v1,n2,v2,&ct);
            ff_strclr(n1, '#');  sscanf(v1, "%f", &kv1);
            ff_strclr(n2, '#');  sscanf(v2, "%f", &kv2);
            if (kv1==kv2)
                strcpy(type, "L*");
            else
                strcpy(type, "T*");
            pf_init_branch((pf_rec *) step->outgLink,type,n1,kv1,n2,kv2,ct,0);
            step->type = OUTG_BRCH;
        }
        else if (*cp == '{') {
            step->outgLink = cf_malloc(strlen(cp)-1);/* len of cp - 2 for }'s */
            conv = sscanf(cp,"{%[^}\n]", (char *) step->outgLink);
            if (conv!=1) continue;
            ff_strclr((char *) step->outgLink, '#');
            step->type = OUTG_COMO;
        }
    }
    if (op == '@') {
        sscanf(cp, "(%8[^\n]%4[^\n])", n1, v1);
        ff_strclr(n1, '#');
        sscanf(v1, "%f", &kv1);
        pf_init_bus(&step->busID, "B ", n1, kv1);
    }
    if (op == '&') strcpy(trace->usrSpec, cp);
    if (op == '>') strcpy(step->qvcRept, cp);
    if (op == '^') strcpy(step->usrRept, cp);
  }

  cf_appList(&trace->stepList, stepLink);

  trace->query &= ~( QUERY_BASE | QUERY_PCHG | QUERY_BRCH | QUERY_QBUS |
                     QUERY_USRA | QUERY_COMO | QUERY_OUTP | QUERY_SETU );
/*
 the following is commented out so that qvcurve will use traces in tack without
 trying to de-compose them into lists.
*/

/*
  if (*step->oldBase!='\0') {
    trcLink = cf_newLink(strlen(step->oldBase)+1);
    strcpy((char *) trcLink->data, step->oldBase);
    cf_appList(&trace->baseList, trcLink);
  }
  if (*step->priChng!='\0') {
    trcLink = cf_newLink(strlen(step->priChng)+1);
    strcpy((char *) trcLink->data, step->priChng);
    cf_appList(&trace->pchgList, trcLink);
  }
  if (step->type==OUTG_BRCH) {
    pf_rec_b2a(brn, (pf_rec *) step->outgLink, "I");
    trcLink = ff_id2Link(brn);
    cf_appList(&trace->brchList, trcLink);
  }
  if (step->type==OUTG_COMO) {
    trcLink = cf_newLink(strlen(step->outgLink)+1);
    strcpy((char *) trcLink->data, (char *) step->outgLink);
    cf_appList(&trace->comoList, trcLink);
  }
  pf_rec_b2a(bus, (pf_rec *) &step->busID, "I");
  trcLink = ff_id2Link(bus);
  cf_appList(&trace->qbusList, trcLink);
*/
  return;
}
void ff_limits(char *s, Trace *trace)
{
  char  *sp, *tp, t[MAX_IN];

  for (sp = strtok(s, " ,="); sp!=NULL; sp = strtok(NULL, " ,=")) {
    tp=strtok(NULL, ", ="); if (tp==NULL) return;
    if (strstr(sp, "VX_MAX_PU"   )!=NULL) {
        sscanf(tp, "%f", &trace->VXmax);
        trace->query &= ~(QUERY_LMTS);
    }
    if (strstr(sp, "VX_MIN_PU"   )!=NULL) {
        sscanf(tp, "%f", &trace->VXmin);
        trace->query &= ~(QUERY_LMTS);
    }
    if (strstr(sp, "DELTA_VX_PU" )!=NULL) {
        sscanf(tp, "%f", &trace->dVX);
        trace->query &= ~(QUERY_LMTS);
    }
  }
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
    if (strstr(sp, "COMMON_MODE_DATA")!=NULL) {
        trace->query &= ~(QUERY_CMOD);
        if (tp==NULL) return;
        strcpy(trace->comSpec, tp);
        continue;
    }
    if (tp==NULL) return;

    if (strstr(sp, "FILE")!=NULL) {
        trace->query &= ~(QUERY_FILE);
        strcpy(trace->rptSpec, tp);
        continue;
    }
    if (strstr(sp, "CURVES_PER_PLOT" )!=NULL) {
        sscanf(tp, "%d", &trace->curvesPerPlot);
        trace->query &= ~(QUERY_CURV);
    }
    if (strstr(sp, "TRACE")!=NULL) {
        if (strstr(tp, "YES")!=NULL) trace->trace = TRACE_YES;
        if (strstr(tp, "NO" )!=NULL) trace->trace = TRACE_NO;
        continue;
    }
  }
  return;
}
void ff_traceName(Trace *trace)
{
  char s[FILENAME_MAX];
  cf_parse(trace->rptSpec, s, 'P');  /* PATH */
  strcpy(trace->trcSpec, s);
  cf_parse(trace->rptSpec, s, 'N');  /* NAME */
  strcat(trace->trcSpec, s);
  strcat(trace->trcSpec, ".trc");    /* TYPE */
}
void ff_traceName_(Trace *trace)
{
  char s[FILENAME_MAX];
  cf_parse(trace->trcSpec, s, 'P');  /* PATH */
  strcpy(CF_logSpec, s);
  cf_parse(trace->trcSpec, s, 'N');  /* NAME */
  strcat(CF_logSpec, s);
  strcat(CF_logSpec, ".log");    /* TYPE */
}
void mf_fprintList(FILE *fp, Link *link, char *tag, char *tab, char type, int n)
{ /* n is the number of links printed per row */
  int i;
  pf_rec r;
  char net_data[MAX_IN];

  if (tag[strlen(tag)-1]=='\n')         /* always print tag if ending in '/n' */
    fprintf(fp, "%s", tag);
  if (link==NULL) return;
  if (tag[strlen(tag)-1]!='\n')  /* print tag lacking '/n' only if list!=NULL */
    fprintf(fp, "%s\n", tag);
  for (i=n; link!=NULL; link=link->next) {
    if (i==0) i = n;
    if (i==n) fprintf(fp, "%s", tab);
    if (type=='s') fprintf(fp, "%s", link->data);
    if (type=='f') fprintf(fp, "%6.2f", link->data);
    if (type=='I') {
        ff_bus2rec((ff_bus *) link->data, &r);
        pf_rec_b2a(net_data, &r, "I");
        fprintf(fp, "%20.20s", net_data);
    }
    if (type=='i') {
        ff_branch2rec((ff_branch *) link->data, &r);
        pf_rec_b2a(net_data, &r, "I");
        fprintf(fp, "%33.33s", net_data);
    }
    if (link->next!=NULL && n!=1 && --i>0) fprintf(fp, ", ");
    else fprintf(fp, "\n");
  }
}
void printMainHeader(Trace *trace)
{
  int n = 0;
  FILE *fp = trace->rptFile;

  fprintf(fp, "%s\n", PAGE_BREAK); n++;

  fprintf(fp,   "/REPORT        | ", trace->rptSpec);
  fprintf(fp,   "  FILE = %s\n", trace->rptSpec); n++;
  if (strlen(trace->usrSpec)>0) {
      fprintf(fp,   "  USER_ANALYSIS = %s\n", trace->usrSpec); n++;
  }
  if (strlen(trace->comSpec)>0) {
      fprintf(fp,   "  COMMON_MODE_DATA = %s\n", trace->comSpec); n++;
  }
  fprintf(fp,   "/REPORT        | ", trace->rptSpec);
  fprintf(fp,   "  CURVES_PER_PLOT = %d,", trace->curvesPerPlot);
  fprintf(fp,   "  TRACE =");
  if (  trace->trace==TRACE_YES ) fprintf(fp, " YES");
  if (  trace->trace==TRACE_NO  ) fprintf(fp, " NO");
  fprintf(fp, "\n");  n++;

  fprintf(fp,   "/LIMITS        | ");
  fprintf(fp,   "  VX_MAX_PU = %5.3f,",  trace->VXmax);
  fprintf(fp,   "  VX_MIN_PU = %5.3f,",  trace->VXmin);
  fprintf(fp,   "  DELTA_VX_PU = %5.3f\n", trace->dVX); n++;

  fprintf(fp, "%s\n", PAGE_BREAK); n++;
  trace->lineNumber += n;
  return;
}
void printMainFooter(Trace *trace)
{
  if (trace->lineNumber!=0 && trace->lineNumber!=PAGE_LENGTH) {
    printBlanks(trace);
    printFooter(trace);
  }
  fclose(trace->rptFile);
}
void printSubReport(Trace *trace, Step *step)
{
  Link *stepLink;

  if (trace->lineNumber >= PAGE_LENGTH-2) {
    printFooter(trace);
    printQminHeader(trace);
  }
  printQminData(trace, step);
  trace->lineNumber++;
  fflush(trace->rptFile);
  return;
}
void printQminHeader(Trace *trace)
{
  FILE *fp = trace->rptFile;
  fprintf(fp, "\n");
  fprintf(fp, 
    "            |  Primary  |                          |            |  Qmin  |V @Qmin|dV/dQ @Qmin|Vmin |Q @ Vmin|                      |\n");
  fprintf(fp, 
    "  Basecase  |  Change   |          Outage          |    Bus     | (MVAR) | (PU)  | (kV/MVAR) |(PU) | (MVAR) |  *.QVPT Output File  |\n");
  fprintf(fp, 
    "------------|-----------|--------------------------|------------|--------|-------|-----------|-----|--------|----------------------|\n");
  trace->lineNumber += 4;
  return;
}
void printQminData(Trace *trace, Step *step)
{
  char  newLine[1024], str[132];

  sprintf(newLine, "%-12.12s|", step->oldBase);
  cf_aprint(newLine, "%-11.11s|", step->priChng);

  if (step->type==OUTG_BRCH) {
    pf_rec_b2a(str, (pf_rec *) step->outgLink, "I");
    str[2] = 'D';
    cf_aprint(newLine, "%26.26s|", &str[6]);
  }
  else if (step->type==OUTG_COMO) {
    cf_aprint(newLine, "%-26.26s|", (char *) step->outgLink);
  }
  else {
    cf_aprint(newLine, "%-26.26s|", "No Outage Taken");
  }

  pf_rec_b2a(str, (pf_rec *) &step->busID, "I");
  cf_aprint(newLine, "%12.12s|", &str[6]);

  cf_aprint(newLine, "%8.2f|", step->Qmin);
  cf_aprint(newLine, " %5.3f |", step->VatQmin);
  cf_aprint(newLine, " %9.6f |", step->dVdQatQmin);
  cf_aprint(newLine, "%5.3f|", step->Vmin);
  cf_aprint(newLine, "%8.2f|", step->QatVmin);

  if (step->qvcRept[0]!='\0') {
    cf_parse(step->qvcRept, str, 'N');
    cf_aprint(newLine, "%-22.22s|", str);
  }

  fprintf(trace->rptFile, newLine);
  fprintf(trace->rptFile, "\n");
}
void printFooter(Trace *trace)
{
  fprintf(trace->rptFile, "\nPAGE %4d\n", trace->pageNumber);
  trace->lineNumber += 2;
  trace->pageNumber++;
  trace->lineNumber -= PAGE_LENGTH;
  return;
}
void printBlanks(Trace *trace)
{
  while (trace->lineNumber < PAGE_LENGTH-2) {
    fprintf(trace->rptFile, "\n");
    trace->lineNumber++;
  }
  return;
}
/************ documented, common CF_UTIL.H candidates prototypes **************/
/* all documented, CF_UTIL.H candidates shall be designated, cf_name().       */
void ff_bus2rec(ff_bus *bus, pf_rec *rec)
{
  memset(rec, '\0', sizeof(pf_rec));
  memcpy(&rec->i.ACbus, bus, sizeof(ff_bus));
}
void ff_rec2bus(pf_rec *rec, ff_bus *bus)
{
  memset(bus, '\0', sizeof(ff_bus));
  memcpy(bus, &rec->i.ACbus, sizeof(ff_bus));
}
void ff_branch2rec(ff_branch *branch, pf_rec *rec)
{
  memset(rec, '\0', sizeof(pf_rec));
  strcpy(rec->i.branch.type,      branch->type);
  strcpy(rec->i.branch.owner,     branch->owner);
  strcpy(rec->i.branch.bus1_name, branch->bus1_name);
  strcpy(rec->i.branch.bus2_name, branch->bus2_name);
  strcpy(rec->i.branch.date_in,   branch->date_in);
  rec->i.branch.bus1_kv           = branch->bus1_kv;
  rec->i.branch.bus2_kv           = branch->bus2_kv;
  rec->i.branch.ckt_id            = branch->ckt_id;
  rec->i.branch.section           = branch->section;
  rec->i.branch.total_rating      = branch->nominal;
  rec->i.branch.thermal_rating    = branch->thermal;
  rec->i.branch.bottleneck_rating = branch->bottleneck;
  rec->i.branch.emergency_rating  = branch->emergency;
}
void ff_rec2branch(pf_rec *rec, ff_branch *branch)
{
  memset(branch, '\0', sizeof(ff_branch));
  strcpy(branch->type,      rec->i.branch.type);
  strcpy(branch->owner,     rec->i.branch.owner);
  strcpy(branch->bus1_name, rec->i.branch.bus1_name);
  strcpy(branch->bus2_name, rec->i.branch.bus2_name);
  strcpy(branch->date_in,   rec->i.branch.date_in);
  branch->bus1_kv    = rec->i.branch.bus1_kv;
  branch->bus2_kv    = rec->i.branch.bus2_kv;
  branch->ckt_id     = rec->i.branch.ckt_id;
  branch->section    = rec->i.branch.section;
  branch->nominal    = rec->i.branch.total_rating;
  branch->thermal    = rec->i.branch.thermal_rating;
  branch->bottleneck = rec->i.branch.bottleneck_rating;
  branch->emergency  = rec->i.branch.emergency_rating;
}
/******** end documented, common CF_UTIL.H candidates prototypes **************/
/* strrep:  make a replicate of s */ /* formerly duplicate, strdup */
char *strrep(char *s)
{
    char *p;

    p = (char *) malloc(strlen(s)+1); /* +1 for '\0' */
    if (p != NULL)
        strcpy(p, s);
    return p;
}
Link *ff_id2Link(char *s)
{ /* create pf_rec structure */
  Link *link;
  pf_rec r;
  size_t size;
  if (cf_isblank(s)) return NULL;
  cf_str2upper(s);
  size = (s[0]=='B') ? sizeof(ff_bus) : sizeof(ff_branch); 
  link = cf_newLink(size);
  if (link==NULL || link->data==NULL) return NULL;
  pf_rec_a2b(s, &r, "I");
  if (s[0]=='B') {
    ff_rec2bus(&r, (ff_bus *) link->data);
  }
  else {
    cf_rec_l2h((pf_branch *) &r);
    ff_rec2branch(&r, (ff_branch *) link->data);
  }
  return link;
}
char *ff_strclr(char *str, char fill)
{ /* replace all occurances of fill character in str with blank */
  char *cp;
  for ( cp = str ; *cp != '\0' ; cp++ ) if (*cp == fill) *cp = ' ';
  return str;
}
char *ff_strfil(char *str, char fill)
{ /* replace all occurances of blank in str with fill character */
  char *cp;
  for ( cp = str ; *cp != '\0' ; cp++ ) if (*cp == ' ') *cp = fill;
  return str;
}
char *ff_strsub(char *str, char *sub, char fill)
{ /* replace all occurances of any character in sub with fill character */
  char *cp;
  for ( cp = str ; *cp != '\0' ; cp++ ) if (strchr(sub, *cp)!=NULL) *cp = fill;
  return str;
}
void ff_scale(float min, float max, float *lower, float *upper, float inc)
{
  for (*upper =      0; *upper < 0.99999*max; *upper += inc);
  for (*lower = *upper; *lower > 1.00001*min; *lower -= inc);
}
