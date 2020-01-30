/******************************************************************************\
UTILITY:    QVcurve_Pro
STRUCTURE:  common CFLOW architecture.
TYPE:       Powerflow (IPFSRV), /CHANGE_PARAMETER voltage-reactance curve.
SUMMARY:    Generate voltage reactance curves.
RELATED:    LINEFLOW, FINDOUT, CFUSE, MIMIC
SEE ALSO:   QVcurve
UPDATED:    March 10, 1999 (see QV_Version below) 
LANGUAGE:   Standard C.  CFLOW Libraries.  CF_UTIL.H.
DEVELOPER:  William D. Rogers, BPA, TOP, 230-3806, wdrogers@bpa.gov (Former BPA)
DEVELOPER:  David M. Stefonek, BPA, TOP, (360)418-8823, dmstefonek@bpa.gov
REQUESTER:  Mark Bond
USERS:      S Kinney, K Kohne, M Rodrigues, G Comegys, B Tesema, C Matthews,
USERS:      J Randall
IPF:        Version 323 or above recommended.
PURPOSE:    Automates production of Q-V curve plot files and plot routine setup
            files for multiple base cases, outages, and critical buses.
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

#define  QV_Version       "101"
#define  OK               0
#define  MAX_IN           264
#define  MAX_CFLOW_BUF    4096        /* note: CFLOW_IPC_BUFF_SIZE = 4096 */
#define  MAX_CURVES       6           /* maximum number of curves for PSAP 22 */
#define  FF_IPF_VERSION   327         /* GPF.EXE_V323 or above recommended */
#define  FF_PAGE_LENGTH   61
#define  PAGE_WIDTH       132
#define  DOT_LINE         "..................................................."
/********** Temporary Name for debug file -- ".qry" will be added ***********/
#define  QRY_SPEC         "qvcurve"
/********** Temporary Name for debug file -- ".qry" will be added ***********/
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
#define  QUERY_GBUS  (int) (1<<17) /* prompt for gen list */
#define  QUERY_CTRL  (int) (1<<18) /* prompt for auxillary control file */
#define  QUERY_CUTP  (int) (1<<19) /* prompt for /CUT_PLANE data */

#define  QUERY_POS   (int) (1<<20) /* prompt for preSolOut switch */

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
#define  READ_GEN    (int)     15  /* found /GENERATOR    in CFLOW data file */
#define  READ_CUT    (int)     16  /* found /CUT_PLANE    in CFLOW data file */
#define  OUTG_NONE   (int)      0
#define  OUTG_BRCH   (int)      1
#define  OUTG_COMO   (int)      2
#define  TRACE_NO    (int)      0
#define  TRACE_YES   (int)      1
#define  FAIL_CRIT   (int) (1<< 0) /* critical failure */
#define  FAIL_BASE   (int) (1<< 1) /* "FAIL_CRIT" also set when "FAIL_BASE" is set into *stat */ 
#define  FAIL_PCHG   (int) (1<< 2) /* not used */ 
#define  FAIL_CMMD   (int) (1<< 3) /* Command Failed Communications Failure? */
#define  FAIL_CHNG   (int) (1<< 4) /* Could not load change file */ 
#define  FAIL_SOLV   (int) (1<< 5) /* "FAIL_CRIT" also set when "FAIL_SOLVE" is set into *stat  */
#define  FAIL_CURV   (int) (1<< 6) /* de_curve failed in file open or in "ff_ch_par_qv"  */
#define  FAIL_VATQ   (int) (1<< 7) /* de_vatq failed in "ff_ch_par_vq" */
#define  FAIL_QATV   (int) (1<< 8) /* de_qatv failed in "ff_ch_par_qv" */
#define  FAIL_USAN   (int) (1<< 9) /* could not read file in "de_user_analysis" */ 
#define  FF_PAGE_FOOTER "\n%-122.122s PAGE %4d\n"
#define  PAGE_BREAK  "************************************************************************************************************************************"
#define  PAGE_MARK   "------------------------------------------------------------------------------------------------------------------------------------"
/***************************** end #define ************************************/
/******************************* typedef **************************************/
typedef struct {
  FILE  *file;
  int    line;
  int    page;
  char  *time;
  char   spec[FILENAME_MAX];
} ff_Report;

typedef struct {
  float  Qmax;
  float  Qgen;
  float  Qres;
  float  Vgen;
} ff_soln;

typedef struct processStep {
  int                    type;     /* 0 = no outage, 1 = branch, 2 = change */
  char  oldBase[FILENAME_MAX];     /* load this base case */
  char  priChng[FILENAME_MAX];     /* primary change file */
  void              *outgLink;     /* outage: branch, change, or common_mode */
  pf_rec                busID;     /* do QV at this bus */
  char  qvcRept[FILENAME_MAX];     /* save QV curve with this name */
  char  usrRept[FILENAME_MAX];     /* save user analysis with this name */
  float                 Qmin;      /* Qknee */
  float              VatQmin;
  float           dVdQatQmin;
  float                 Vmin;
  float              QatVmin;
  float                VatQ0;      /* Voltage when Q = 0 */
  float               Qshunt;      /* Q shunt at 1.0 PU */
  float              Qmargin;      /* Qknee + Qshunt*(Vop^2) */
  float          QmaxBaseTot;
  float          QresBaseTot;
  float          QresZeroTot;
  float          QresKneeTot;
  char       subTitle[MAX_IN];
  char         legend[MAX_IN];
} Step;

typedef struct xyPlotSetup {
  char     setUpXY[FILENAME_MAX];    /* setup file name for PSAP 22 XYPLOTTER */
  char     setUpPS[FILENAME_MAX];    /* setup file name for PostScript */
  int             numberOfCurves;    /* up to 5 allowed */
  char        superTitle[MAX_IN];    /* super title for plot */
  char  qvpFile[8][FILENAME_MAX];    /* array of 8 qvpt point files */
  char       subTitle[8][MAX_IN];    /* array of 8 subtitles */
  char         legend[8][MAX_IN];    /* array of 8 legends */
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
  Link  *gbusList; /* generators */
  Link  *trceList;
  Link  *stepList;
  Link  *plotList;
  Link  *setuList;
  Link  *qresList; /* Q reserve summary report list */
  Link  *cutpList; /* Cut plane list */
  int    trace;
  int    stat;
  long   query;
  ff_Report *trc;
  ff_Report *rpt;
  ff_Report *gen;
  FILE  *comFile;
  char   usrSpec[FILENAME_MAX];
  char   conSpec[FILENAME_MAX];
  char   comSpec[FILENAME_MAX];
  char   solution[MAX_CFLOW_BUF];
  char   change_bus_types[MAX_CFLOW_BUF];
  char   agc[MAX_CFLOW_BUF];
  float  VXmax;
  float  VXmin;
  float  dVX;
  char   timeStamp[MAX_IN];
  char   userID[MAX_IN];
  char   IPFversion[MAX_IN];
  char   IPFexecute[MAX_IN];
  int    curvNumber;
  int    curvesPerPlot;
  char   preOutSol[4];   /* controls pre outage solution in "dispatchEngine" */ 
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
void  openReport(ff_Report *rpt);
void  dispatchEngine(Trace *trace);
void  buildSetup(Trace *trace);
void  printXYsetup(FILE *fp, Trace *trace, Plot *plot);
void  printPSsetup(FILE *fp, Trace *trace, Plot *plot);
void  finalRemarks(Trace *trace);
void  cleanUpFiles(Trace *trace);
/* end MIMIC functions - called by main() *************************************/

/* top MIMIC functions - report functions *************************************/
void   ff_printMainHeader(Trace *trace, ff_Report *rpt);
void   ff_printMainFooter(ff_Report *rpt);
void   ff_printBlanks(ff_Report *rpt, int n);
int    ff_printPageFooter(ff_Report *rpt, int m);
void   ff_printQminReport(Trace *trace, Step *step, ff_Report *rpt);
void   ff_printQresReport(Trace *trace, Step *step, ff_Report *rpt);
void   ff_printFlowReport(Trace *trace, Step *step, ff_Report *rpt);
void   ff_printQminHeader(ff_Report *rpt);  
void   ff_printQresHeader(ff_Report *rpt);
void   ff_printFlowHeader(ff_Report *rpt);
void   ff_printQminData(Trace *trace, Step *step, ff_Report *rpt);
void   ff_printQresData(Trace *trace, Step *step, ff_Report *rpt);
void   ff_printFlowData(Trace *trace, Step *step, ff_Report *rpt);
void   ff_printQresPage(ff_Report *rpt, int n);
void   ff_printFlowPage(ff_Report *rpt, int n);
/* end MIMIC functions - report functions *************************************/

/* top MIMIC functions - support functions ************************************/
void   checkFileList(Link *fileList);
int    findFile(Link *curFile, char *path);
void   de_curve(int *stat, Trace *trace, Step *step, pf_rec *r);
void   de_cut_plane(int *stat, Link *cutpList);
void   de_vatq(int *stat, pf_rec *r, float *VY, float QX);
void   de_qatv(int *stat, pf_rec *r, float *QY, float VX);
void   de_qreserve(int *stat, Link *genList);
int    ff_ch_par_qv(char *fn, char *bus, char *kv, float x, float *v, float *q);
int    ff_ch_par_vq(char *fn, char *bus, char *kv, float x, float *v, float *q);
void   ff_scale(float min, float max, float *first, float *last, float inc);
void   totalGenData(Trace *trace, Step *step);
void   de_load_oldbase(int *stat, char *file);
void   de_load_changes(int *stat, char *file);
void   de_user_analysis(int *stat, char *infile, char *outfile);
void   de_take_brch_outg(int *stat, int type, pf_rec *brnOutg);
void   de_take_como_outg(int *stat, int type, char   *comOutg, FILE *comFile);
int    de_common_mode_outage(int go, Trace *trace, Step *step);
void   de_get_bus(int *stat, pf_rec *busID, float *Qshunt);
void   de_modify_bus(int *stat, pf_rec *m, float VXmax);
void   de_command(int *stat, char *cmd, char *data);
void   de_solution(int *stat, char *solution, char *file);
/* end MIMIC functions - support functions ************************************/

/* top LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/
void   ff_stream2List(FILE *readMe, Trace *trace, Link **expList);
void   ff_report(char *s, Trace *trace);
void   ff_limits(char *s, Trace *trace);
void   ff_traces(char *s, Trace *trace);
Link  *ff_insRow(Link **table, Link *rowPtr, Link *newRow, Trace *trace);
void   ff_expList(Trace *trace, Link *dataList, Link **expList);
void   ff_getList(Trace *trace, char *dataFile, Link **expList);
/* end LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/

int main(int argc, char *argv[])
{
  Trace trace;
/******************** debug stuff ********************************/
/*  cf_debug = 1;  */
/*  cf_debug = 0;  */
  cf_debug = 0;
/******************** debug stuff ********************************/

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
  openReport(trace.trc);
  printTrace(&trace);
  queryContinue(&trace);
  openReport(trace.rpt);
  openReport(trace.gen);
  cflow_pf_init();          /* initialize cflow connection to powerflow */
  dispatchEngine(&trace);
  buildSetup(&trace);
  finalRemarks(&trace);
  cleanUpFiles(&trace);
  time(&CF_time1);
  cf_logUse("QVcurve_Pro", trace.IPFversion, trace.userID);
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
  trace->gbusList         =   NULL;
  trace->trceList         =   NULL;
  trace->stepList         =   NULL;
  trace->comoList         =   NULL;
  trace->plotList         =   NULL;
  trace->setuList         =   NULL;
  trace->qresList         =   NULL;
  trace->cutpList         =   NULL;
  trace->trace            =   TRACE_YES;
  trace->stat             =   0;
  trace->query            = ( QUERY_BASE | QUERY_BRCH | QUERY_QBUS |
                              QUERY_USRA | QUERY_FILE | QUERY_SETU |
                              QUERY_LMTS | QUERY_COMO | QUERY_CHBT |
                              QUERY_CMOD | QUERY_PCHG | QUERY_SOLN |
                              QUERY_OUTP | QUERY_CONT | QUERY_CURV |
                              QUERY_AGCC | QUERY_GBUS | QUERY_CTRL |
                              QUERY_CUTP | QUERY_POS);
  strcpy(trace->solution, "/SOLUTION\n");
  trace->change_bus_types[0] = '\0';
  trace->agc[0] = '\0';
  trace->VXmax            = 1.10; /* typical 1.10 */
  trace->VXmin            = 0.90; /* typical 0.70 */
  trace->dVX              = 0.05; /* typical 0.02 */
  strncpy(trace->IPFexecute, getenv("IPFSRV_CF"), MAX_IN);
  cf_parse(trace->IPFexecute, trace->IPFversion, 'F');
  cuserid(trace->userID);
  cf_time(trace->timeStamp, MAX_IN, CF_TIMESTAMP);
  strcpy(CF_logSpec, LOG_SPEC);
  trace->trc              = (ff_Report *) cf_malloc(sizeof(ff_Report));
  trace->trc->file        = NULL;
  trace->trc->line        = 0;
  trace->trc->page        = 1;
  trace->trc->time        = trace->timeStamp;
  trace->rpt              = (ff_Report *) cf_malloc(sizeof(ff_Report));
  trace->rpt->file        = NULL;
  trace->rpt->line        = 0;
  trace->rpt->page        = 1;
  trace->rpt->time        = trace->timeStamp;
  strcpy(trace->rpt->spec, RPT_SPEC);
  trace->gen              = (ff_Report *) cf_malloc(sizeof(ff_Report));
  trace->gen->file        = NULL;
  trace->gen->line        = 0;
  trace->gen->page        = 1;
  trace->gen->time        = trace->timeStamp;
  trace->curvNumber       = 0;
  trace->curvesPerPlot    = 5;
  strcpy(trace->preOutSol, "YES");
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

/******************   query log and BAD file  *******************/
  cf_strsuf(CF_logQSpec, QRY_SPEC, '.', ".qry");
/*  cf_strsuf(CF_logZSpec, trace->outSeri, '.', ".BAD"); */
/******************   query log and BAD file  *******************/

}
void processCommandLine(Trace *trace, int argc, char *argv[])
/* note: could generalize by passing in a default list to use */
{
  Link *list, *topLink;
  int   i, start = 1;
  char c;

  topLink = NULL;

  if (argc <= 1) return;
  for (i = 1; i < argc; i++) {
    if ( argv[i][0] == '-' ) {
      c = (char) toupper( (int) argv[i][1] );
      switch ( c ) {
      case 'C' :
      case 'W' :
        start = i+2;
        break;
      case 'N' :
        start = i+1;
        break;
      }
    }
  }
  for (i = argc; --i > start-1; cf_appList(&topLink, list)) { 
    list = cf_text2Link(argv[i]);
  }
  ff_expList(trace, topLink, &trace->baseList);
  return;
}
void instructions(Trace *trace)
{
  printf("\n   Welcome to QVcurve_Pro Version V%s - updated 03-10-99",QV_Version);
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
  Link *list, *row;

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
        list = cf_id2Link(query, 'I');
        cf_appList(&trace->qbusList, list);
    } while (!cf_isblank(query));
  }
  if (trace->query & QUERY_GBUS) {
    printf("\n\n/GEN");
    cf_nprompt("\n > Enter file of generator buses for Qres summary: ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->query & QUERY_GBUS) {
    printf(    "\n > Enter list of generator buses for Qres summary: ");
    printf(    "\n   > Btc<O>< Bus  ><KV>");
    do {
        cf_nprompt("\n   > ", "", MAX_IN, query);
        list = cf_id2Link(query, 'I');
        if (list!=NULL) {
            row = cf_link2row(list);
            cf_appList(&trace->gbusList, row);
        }
    } while (!cf_isblank(query));
  }
  if (trace->query & QUERY_CUTP) {
    printf("\n\n/CUT_PLANE"); 
    cf_nprompt("\n > Enter file of monitored cut-plane descriptions: ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->query & QUERY_CUTP) {
    printf(    "\n > Enter list of monitored cut-plane descriptions: ");
    printf(    "\n   > Lyc<O>< Bus1 ><V1> < Bus2 ><V2>C");
    do {
        cf_nprompt("\n   > ", "", MAX_IN, query);
        list = cf_rec2Link(query);
        if (list!=NULL) {
            row = cf_link2row(list);
            cf_appList(&trace->cutpList, row);
        }
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
        list = cf_id2Link(query, 'L');
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
  if (trace->query & (QUERY_USRA | QUERY_FILE | QUERY_CURV | QUERY_CTRL | QUERY_POS)) {
    printf("\n\n/REPORT");
  }
  if (trace->query & QUERY_FILE) {
    cf_sprompt("\n > Enter summary report name, FILE =[%s]: ", trace->rpt->spec, trace->rpt->spec);
    cf_parse(trace->rpt->spec, query, 'T');
    if (strlen(query)==0) strcat(trace->rpt->spec, ".rpt");
  }
  if (trace->query & QUERY_USRA) {
    cf_sprompt("\n > Enter user analysis file,     USER_ANALYSIS = : ", "", trace->usrSpec);
  }
  if (trace->query & QUERY_CTRL) {
    cf_sprompt("\n > Enter auxillary control file,  CONTROL_FILE = : ", "", trace->conSpec);
  }
  if (trace->query & QUERY_CURV) {
    printf("\n Each plot can have from 1 to 8 curves (6 max for PSAP 22):");
    cf_iprompt("\n > Enter number of curves,  CURVES_PER_PLOT = [%d]: ", trace->curvesPerPlot, &trace->curvesPerPlot);
    if ( trace->curvesPerPlot < 1) trace->curvesPerPlot = 1;
    if ( trace->curvesPerPlot > 8) trace->curvesPerPlot = 8;
  }

/* controls pre outage solution in "dispatchEngine" */
 if (trace->query & QUERY_POS) {
    cf_sprompt("\n > Do you want a pre outage solution and reactive calculation? ",trace->preOutSol ,trace->preOutSol);
    cf_stoupper(trace->preOutSol);
    if (trace->preOutSol[0] == 'N') 
    {
        strcpy(trace->preOutSol, "NO");
    }
    else
    {
        strcpy(trace->preOutSol, "YES");
    }
  }
/* controls pre outage solution in "dispatchEngine" */

  if (trace->query & QUERY_LMTS) {
    printf("\n\n/LIMITS");
    printf("\n For Q-V curve analysis: Enter Min, Max, and Delta Voltage:");
    cf_fprompt("\n > Max BE bus voltage in PU,  VX_MAX_PU = [%5.3f]: ",trace->VXmax, &trace->VXmax);
    cf_fprompt("\n > Min BE bus voltage in PU,  VX_MIN_PU = [%5.3f]: ",trace->VXmin, &trace->VXmin);
    cf_fprompt("\n > Max voltage change in PU,DELTA_VX_PU = [%5.3f]: ",trace->dVX,   &trace->dVX  );
  }
  if (trace->query & (QUERY_SOLN | QUERY_CHBT | QUERY_AGCC)) {
    printf("\n\n/SOLUTION, /CHANGE_BUS_TYPES, or /AGC"); 
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
  printf("\n");
  cf_strsuf(trace->trc->spec, trace->rpt->spec, '.', ".trc");
  cf_strsuf(trace->gen->spec, trace->rpt->spec, '.', ".gen");
  printf("\n%s\n", DOT_LINE);
  return;
}
void printTrace(Trace *trace)
{
  FILE *fp;
  Link *row, *list;

  if (trace->trace==TRACE_NO) return;
  fp = trace->trc->file;
  if (fp == NULL) fp = stderr;
  fprintf(fp, ". %s %s %s %s QVcurve version V%s\n", trace->trc->spec, 
   trace->trc->time, trace->userID,trace->IPFversion, QV_Version);

  fprintf(fp, "/REPORT\n");
  fprintf(fp, "  FILE = %s\n", trace->rpt->spec);
  fprintf(fp, "  USER_ANALYSIS = %s\n", trace->usrSpec);
  fprintf(fp, "  CONTROL_FILE = %s\n", trace->conSpec);
  fprintf(fp, "  CURVES_PER_PLOT = %d\n", trace->curvesPerPlot);
  fprintf(fp, "  PRE_OUT_SOL = %s\n", trace->preOutSol);
  fprintf(fp, "  COMMON_MODE_DATA = %s\n", trace->comSpec);
  if ( trace->trace == TRACE_YES) fprintf(fp, "  TRACE = YES\n");
  if ( trace->trace == TRACE_NO ) fprintf(fp, "  TRACE = NO\n");

  fprintf(fp, "/LIMITS\n");
  fprintf(fp, "  VX_MAX_PU   = %5.3f\n", trace->VXmax);
  fprintf(fp, "  VX_MIN_PU   = %5.3f\n", trace->VXmin);
  fprintf(fp, "  DELTA_VX_PU = %5.3f\n", trace->dVX);

  if ( trace->change_bus_types[0]!=0 ) {
/********** stuff to allow multiple "/CHANGE_BUS_TYPE" records ************/
    fprintf(fp, "/CHG_BUS_TYPES\n");
    fprintf(fp, "%s", trace->change_bus_types);
  }
  else {
/********** stuff to allow multiple "/CHANGE_BUS_TYPE" records ************/
/*    fprintf(fp, "/CHANGE_BUS_TYPES\n"); */
    fprintf(fp, "/CHG_BUS_TYPES\n");
  }
  if ( trace->agc[0]!=0 ) {
    fprintf(fp, "%s", trace->agc);
  }
  else {
    fprintf(fp, "/AGC\n");
  }
  if ( trace->solution[0]!=0 ) {
    fprintf(fp, "%s", trace->solution);
  }

  if (trace->baseList==NULL) {
    cx_printList(fp, trace->trceList, "/TRACE\n",      " ",  'p', "%s", 1);
  }
  else {
    cx_printList(fp, trace->baseList, "/BASECASE\n",   "  ", 'p', "%s", 1);
    cx_printList(fp, trace->pchgList, "/PRI_CHANGE\n", "  ", 'p', "%s", 1);
    cx_printList(fp, trace->outpList, "/OUTPUT\n",     "  ", 'p', "%s", 1);
    cx_printList(fp, trace->setuList, "/SETUP\n",      "  ", 'p', "%s", 1);
    cx_printList(fp, trace->qbusList, "/BUS\n",        "",   'b', "%20.20s", 1);
    cx_printList(fp, trace->gbusList, "/GENERATORS\n", "",   'B', "%20.20s", 1);
    cx_printRowList(fp,trace->cutpList,"/CUT_PLANES\n",">\n",'r', "%33.33s", 1);
    cx_printList(fp, trace->brchList, "/BRANCH\n",     "",   'l', "%33.33s", 1);
    cx_printList(fp, trace->comoList, "/COMMON_MODE\n","",   'p', "%s", 1);
    cx_printList(fp, trace->trceList, "./TRACE\n",     ". ", 'p', "%s", 1);
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
        cf_str2upper(s);
        if ( s[0] != '>' ) continue;              /* skip comments, skip data */
/*        if ( strstr(s, "common_mode")!=NULL ) continue;  */
        if ( strstr(s, "COMMON_MODE")!=NULL ) continue;
/*        if ( strstr(s, "mode ")==NULL ) continue;        */
        if ( strstr(s, "MODE ")==NULL ) continue;
        if ( sscanf(s, ">%*s %80[^\n]", c) == 1 ) {
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
  int     base;
  char    qvcName[FILENAME_MAX];
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
  base = 0;
  while (curBase != NULL) {
    stepLink = cf_addLink(stepLink, sizeof(Step));
    step = (Step *) stepLink->data;
    if (trace->stepList==NULL) trace->stepList = stepLink;

    strcpy(step->oldBase, (char *) curBase->data);

    if (curPchg!=NULL) {
        strcpy(step->priChng, (char *) curPchg->data);
    }
    if (curQbus!=NULL) {
        cf_bus2rec((cf_Bus *) curQbus->data, (pf_rec *) &step->busID);
    }
    if (curBrch!=NULL) {
        step->outgLink = cf_malloc(sizeof(pf_rec));
        cf_branch2rec((cf_Branch *) curBrch->data, (pf_rec *) step->outgLink);
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

/*********** change naming convention for ".qvpt> file names ************/
/*        sprintf(step->qvcRept, "%.17s-%s.qvpt", baseName, idenName); */
          cf_parse(trace->rpt->spec,qvcName, 'N');
        sprintf(step->qvcRept, "%s-%d-%s.qvpt", qvcName, base, idenName);
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
        curBase = curBase->next; base++;
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

        cf_parse(trace->rpt->spec, plot->setUpXY, 'N');
        cf_parse(trace->rpt->spec, plot->setUpPS, 'N');
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
    printf("\nTrace written to %s", trace->trc->spec);
  if (CF_logFile != NULL)
    printf("\nError Summary written to %s", CF_logSpec);
  printf("\n%s\n", DOT_LINE);
  cx_printList(stdout, trace->trceList, "/TRACE\n",     "  ", 'p', "%s", 1);
  printf("%s\n", DOT_LINE);
  if ( trace->query & QUERY_CONT) {
    printf(" Please examine the above /TRACE of QVcurve operations.");
    yes = cf_yprompt("\n > Do you want to continue this run? [%c]: ", 'Y');
    cf_exit(!yes, "Quitting!");
  }
}
void openReport(ff_Report *rpt)
{
  rpt->file = cf_openFile(rpt->spec, "w");
  cf_exit(rpt->file==NULL, "Quitting!\n");
}
void  dispatchEngine(Trace *trace)
{
  Step *step;
  Link *stepLink;

/**************** BEGINNING OF POWERFLOW DISPATCH ENGINE **********************/
  ff_printMainHeader(trace, trace->rpt);
  ff_printMainHeader(trace, trace->gen);
  ff_printQminHeader(trace->rpt);
  printf("Processing... \n");
/******************  BAD file not needed for now *************************/
/*  cf_strsuf(CF_logZSpec, trace->rpt->spec, '.', ".BAD");               */ 
/*  cf_logBAD("Begin BAD report for series %s. \n\n", trace->rpt->spec); */
/******************  BAD file not needed for now *************************/


  for (stepLink = trace->stepList; stepLink != NULL; stepLink = stepLink->next){
    step = (Step *) stepLink->data;
    trace->stat = 0;

    cleanUpFiles(trace);

    de_load_oldbase(&trace->stat, step->oldBase);
    de_command(&trace->stat, "/INCLUDE, FILE=", trace->conSpec);
    de_command(&trace->stat, trace->change_bus_types, "");
    de_command(&trace->stat, trace->agc, "");
    de_load_changes(&trace->stat, step->priChng);
    de_user_analysis(&trace->stat, trace->usrSpec, step->usrRept);

/* controls pre outage solution in "dispatchEngine" */
    if (!strcmp(trace->preOutSol, "YES"))
    {
      de_solution(&trace->stat, trace->solution, step->oldBase);
      de_qreserve(&trace->stat, trace->gbusList);               
    }
/* controls pre outage solution in "dispatchEngine" */

    de_take_brch_outg(&trace->stat, step->type, step->outgLink);
    de_take_como_outg(&trace->stat, step->type, step->outgLink, trace->comFile);

/* controls pre outage solution in "dispatchEngine" */
    if (!strcmp(trace->preOutSol, "YES"))
    {
      de_get_bus(&trace->stat, &step->busID, &step->Qshunt);   /* relies on solution data */
      de_modify_bus(&trace->stat, &step->busID, trace->VXmax); /*gives james ramdall undesired results*/   
    }
    de_solution(&trace->stat, trace->solution, step->oldBase);
    de_cut_plane(&trace->stat, trace->cutpList);
    de_curve(&trace->stat, trace, step, &step->busID);

    de_modify_bus(&trace->stat, &step->busID, trace->VXmax);
    de_solution(&trace->stat, trace->solution, step->oldBase);
    de_vatq(&trace->stat, &step->busID, &step->VatQ0, 0);
    de_qreserve(&trace->stat, trace->gbusList);

    de_modify_bus(&trace->stat, &step->busID, trace->VXmax);
    de_solution(&trace->stat, trace->solution, step->oldBase);
    de_qatv(&trace->stat, &step->busID, &step->Qmin, step->VatQmin);   /* set Q to Qmin */
    de_qreserve(&trace->stat, trace->gbusList);

    totalGenData(trace, step);

    ff_printQminReport(trace, step, trace->rpt);
    ff_printQresReport(trace, step, trace->gen);
    ff_printFlowReport(trace, step, trace->gen);
  }
  ff_printMainFooter(trace->rpt);
  ff_printMainFooter(trace->gen);
/******************  BAD file not needed for now *************************/
/*  cf_logBAD("End BAD report for series %s. \n", trace->rpt->spec);     */ 
/******************  BAD file not needed for now *************************/
  return;
/**************** END OF POWERFLOW DISPATCH ENGINE ****************************/
}
void  buildSetup(Trace *trace)
{
  FILE *fp;
  Plot *plot;
  char  s[MAX_CFLOW_BUF], data[MAX_IN];
  Link *plotLink;

  for (plotLink = trace->plotList; plotLink != NULL; plotLink = plotLink->next){
    plot = (Plot *) plotLink->data;

    if (trace->curvesPerPlot <= MAX_CURVES) {
        printf("Building Setup File... %s\n", plot->setUpXY);
        fp = cf_openFile(plot->setUpXY, "w");
        if (fp==NULL) continue;
        printXYsetup(fp, trace, plot);
        fclose(fp);
    }
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
  printf("\nOutput report written to %s", trace->rpt->spec);
  printf("\nGen.   report written to %s", trace->gen->spec);
  if (trace->trace == TRACE_YES)
    printf("\nTrace  report written to %s", trace->trc->spec);
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
void de_curve(int *stat, Trace *trace, Step *step, pf_rec *r)
{
  FILE *fp;
  char  id[132], name[9], volt[5];
  float VX, VXmax, VXmin, dVX, Vbase, v, q;
  float Qmn, Vmn, Vq, Qv, Qmn_1, Vq_1, q_1, v_1, dVdQ, q0;
  int   rtn;

/***** initalize voltage starting point for curve at last solution voltage ****/
  int steps;   /* factious number of dVX steps for calculating 2nd VX curve point */
  float Vsol;  /* solution voltage in KV */ 
    

  if (*stat & FAIL_CRIT) return;
  fp = cf_openFile(step->qvcRept, "w");
  if (fp==NULL) {
    *stat |= FAIL_CURV;
    return;
  }
  printf("  Making QV curve...\n");

  pf_rec_b2a(id, r, "I");
  sprintf(name, "%-8.8s", &id[6]);
  sprintf(volt, "%-4.4s", &id[14]);

  cf_strsub(name, " ", '#');
  Vbase = r->i.ACbus.kv;

  VXmax = trace->VXmax;
  VXmin = trace->VXmin;
  dVX   = trace->dVX;

/***** initalize voltage starting point for curve at last solution voltage ****/
/*  VX = VXmax; */
  pf_rec_bus(r, "O");      /* get solution data    */
  Vsol = r->s.ACbus.Vmag;  /* get solution voltage */
  VX = Vsol / Vbase;       /* get voltage for 1-st curve point */
  steps = VX / dVX + 1;    /* get integer number of steps used to find 2-nd curve point */   

  rtn = ff_ch_par_qv("NULL", name, volt, VX, &v, &q);

  if (rtn!=0) {
    fprintf(fp, "C1 %42.42s NO SOLN\n", step->subTitle);
    strcpy(step->legend, "NO SOLN");
    fprintf(fp, "C2 %s\n", step->legend);
    fprintf(fp, "0 0\n", v, q);
    printf("%s %s, V =%13.10f (PU)  NO SOLUTION\n", name, volt, VX);
    *stat |= FAIL_CURV;
/************ stuff to close a file *************/
    fclose (fp);
/************ stuff to close a file *************/
    return;
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

/***** initalize voltage starting point for curve at last solution voltage ****/
  VXmax = dVX * steps;   /* VXmax should be a "nice" step just above the solved 
        starting voltage. The "for" logic below will put it just below the starting voltage */   

  for (VX = VXmax - (1*dVX); VX >= VXmin; VX = VX - dVX) {
    q_1 = q;
    v_1 = v;
    rtn = ff_ch_par_qv("NULL", name, volt, VX, &v, &q);
    if (rtn!=0) {
        printf("%s %s, V =%13.10f (PU)  NO SOLUTION\n", name, volt, VX);
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
/* the following calculates Q reserve levels */
/*
    if (q_1>0 && q<0) {
        rtn = ff_ch_par_vq("NULL", name, volt, 0, &step->VatQ0, &q0);
        de_qreserve(trace);
    }
*/
  }
  step->Qmin = Qmn;
  step->VatQmin = Vq;
  step->Vmin = Vmn;
  step->QatVmin = Qv;
  if (Qmn==Qmn_1) step->dVdQatQmin = 0;
  else {
    step->dVdQatQmin = (Vbase * (Vq_1 - Vq)) / (Qmn_1 - Qmn);
  }
  step->Qmargin = Qmn + (step->Qshunt * Vq * Vq);

  printf("\n\r");

/************ stuff to close a file *************/
    fclose (fp);
/************ stuff to close a file *************/

  return;
}
void de_vatq(int *stat, pf_rec *r, float *VY, float QX)
{
  char  id[132], name[9], volt[5];
  float v, q;

  if (*stat & FAIL_CRIT) return;
  printf("  Finding V at Q = %8.2f\n", QX);
  pf_rec_b2a(id, r, "I");
  sprintf(name, "%-8.8s", &id[6]);
  sprintf(volt, "%-4.4s", &id[14]);
  cf_strsub(name, " ", '#');

  if (ff_ch_par_vq("NULL", name, volt, QX, &v, &q) !=0 ) {
    printf("%s %s, Q =%17.10f (MVAR)  NO SOLUTION\n", name, volt, QX);
    *stat |= FAIL_VATQ;
    return;
  }
  printf("%s %s, V =%13.10f (PU)  Q =%17.10f (MVAR)\n", name, volt, v, q);
  *VY = v;

  return;
}
void de_qatv(int *stat, pf_rec *r, float *QY, float VX)
{
  char  id[132], name[9], volt[5];
  float v, q;

  if (*stat & FAIL_CRIT) return;
  printf("  Finding Q at V = %8.2f\n", VX);
  pf_rec_b2a(id, r, "I");
  sprintf(name, "%-8.8s", &id[6]);
  sprintf(volt, "%-4.4s", &id[14]);
  cf_strsub(name, " ", '#');

  if (ff_ch_par_qv("NULL", name, volt, VX, &v, &q) != 0) {
    printf("%s %s, V =%13.10f (PU)  NO SOLUTION\n", name, volt, VX);
    *stat |= FAIL_QATV;
    return;
  }
/* step->QatVmin = q; */
  printf("%s %s, V =%13.10f (PU)  Q =%17.10f (MVAR)\n", name, volt, v, q);

  return;
}
void de_qreserve(int *stat, Link *genList)
{
  Link *curGbus, *curQres, *gbusLink, *solnLink;
  char  id[132], name[9], volt[5];
  pf_rec m;
  ff_soln *solnData;

  if (*stat & FAIL_CRIT) return;
  printf("  Getting Generator Reactance...\n");
  curGbus = genList;
  while (curGbus!=NULL) {
    gbusLink = (Link *) curGbus->data;
    cf_bus2rec((cf_Bus *) gbusLink->data, &m);
    pf_rec_bus(&m, "G");                             /* get target QV bus */
    pf_rec_bus(&m, "O");                             /* get target QV bus */
    pf_rec_b2a(id, &m, "I");
    sprintf(name, "%-8.8s", &id[6]);
    sprintf(volt, "%-4.4s", &id[14]);
    solnLink = cf_newLink(sizeof(ff_soln));
    cf_appList(&gbusLink, solnLink);
    solnData = (ff_soln *) solnLink->data;
    solnData->Qmax = m.i.ACbus.Qsch_Qmax;
    solnData->Qgen = m.s.ACbus.Qgen;
    solnData->Qres = m.i.ACbus.Qsch_Qmax - m.s.ACbus.Qgen;
    solnData->Vgen = m.s.ACbus.Vmag;
    printf("%s%s, Qmax =%15.8f (MVAR)  Qgen =%15.8f (MVAR)  Qres =%15.8f (MVAR)  Vgen =%13.10f (PU)\n", name, volt,
        solnData->Qmax, solnData->Qgen, solnData->Qres, solnData->Vgen);
    curGbus = curGbus->next;
  }
  return;
}
int ff_ch_par_vq(char *file, char *name, char *kv, float x, float *v, float *q)
{
  char data[MAX_CFLOW_BUF];
  int  ipf_stat;
  int  rtn;

  sprintf(data,"/CHANGE_PARAMETER,FILE=%s,BUS=%8.8s %4.4s,QX=%f,VY=?\n(END)\n",
    file, name, kv, x);
/*  rtn = pf_command(data); */
  ipf_stat = pf_command(data); 
  if ( sscanf(reply_pf, "%*17c Q  = %f V  = %f", q, v) != 2 ) rtn = -1;
  if ( strstr(reply_pf, "failed") != NULL ) rtn = -1;
/*  if (rtn!=0) { */
  if (ipf_stat!=0) {
    printf("%s %s, Q =%13.10f (MVAR)  NO SOLUTION\n", name, kv, x);
/******************  BAD file not needed for now *************************/
/*    cf_logBAD("FAILED SOLUTION -- %s\n", data);                        */
/******************  BAD file not needed for now *************************/
    return;
  }
  printf("%s %s, Q =%13.10f (MVAR)  V =%17.10f (PU)\n", name, kv, *q, *v);
  return ipf_stat; 
/*  return rtn; */
}
int ff_ch_par_qv(char *file, char *name, char *kv, float x, float *v, float *q)
{
  char data[MAX_CFLOW_BUF];
  int  ipf_stat;
  int rtn;
  sprintf(data,"/CHANGE_PARAMETER,FILE=%s,BUS=%8.8s %4.4s,VX=%f,QY=?\n(END)\n",
    file, name, kv, x);

  ipf_stat = pf_command(data);
  if ( sscanf(reply_pf, "%*17c V  = %f Q  = %f", v, q) != 2 ) rtn -1;
  if ( strstr(reply_pf, "failed") != NULL ) rtn -1;
  
/*  if (rtn!=0)  */
  if (ipf_stat!=0) 
   {
    printf("%s %s, Q =%13.10f (MVAR)  NO SOLUTION\n", name, kv, x);
/******************  BAD file not needed for now *************************/
/*    cf_logBAD("FAILED SOLUTION -- %s\n", data);                        */
/******************  BAD file not needed for now *************************/
   }

  return ipf_stat;
}
void totalGenData(Trace *trace, Step *step)
{
  Link    *curGbus, *gbusData, *solnLink;
  ff_soln *solnData;
  float    QmaxBaseTot, QresBaseTot, QresZeroTot, QresKneeTot;

  QmaxBaseTot = 0; QresBaseTot = 0; QresZeroTot = 0; QresKneeTot = 0;

  curGbus = trace->gbusList;
  while (curGbus!=NULL) {
    gbusData = (Link *) curGbus->data;

    solnLink = gbusData->next;
    solnData = (ff_soln *) solnLink->data;
    QmaxBaseTot += solnData->Qmax;
    QresBaseTot += solnData->Qres;

    solnLink = solnLink->next;
    solnData = (ff_soln *) solnLink->data;
    QresZeroTot += solnData->Qres;

    solnLink = solnLink->next;
    solnData = (ff_soln *) solnLink->data;
    QresKneeTot += solnData->Qres;

    curGbus = curGbus->next;
  }
  printf("QmaxBaseTot=%f,  QresBaseTot=%f,  QresZeroTot=%f,  QresKneeTot=%f\n",
    QmaxBaseTot, QresBaseTot, QresZeroTot, QresKneeTot);

  step->QmaxBaseTot = QmaxBaseTot;
  step->QresBaseTot = QresBaseTot;
  step->QresZeroTot = QresZeroTot;
  step->QresKneeTot = QresKneeTot;

  return;
}
void de_user_analysis(int *stat, char *infile, char *outfile)
{
  char cmd[MAX_CFLOW_BUF];

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

  if (*stat & FAIL_CRIT) return;
  if (cutpList==NULL) return;
  printf("  Getting cut-plane flows...\n");
  for (curCutp=cutpList; curCutp!=NULL; curCutp=curCutp->next) {
    for (curLine=(Link *)curCutp->data; curLine!=NULL; curLine=curLine->next) {
        pf_rec_branch(curLine->data, "O");
    }
  }
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
        strcpy(net_data, (char *) step->outgLink);
        cf_strsub(net_data, " ", '#');
        cf_aprint(newLine, "- {%s} ", net_data);
    }

    pf_rec_b2a(net_data, (pf_rec *) &step->busID, "I");
    cf_strsub(net_data, " ", '#');
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
  Link *list, *link, *row;
  int mode = READ_INC;/* default /INCLUDE */
/********** stuff to allow multiple "/CHANGE_BUS_TYPE" records ************/
  int header_only;

  while (fgets(str, MAX_IN, readMe)!=NULL) { 
    if ( cf_iscomment(str) ) continue;
    strcpy(STR, str);
    cf_str2upper(STR);

/********** stuff to allow multiple "/CHANGE_BUS_TYPE" records ************/
    header_only = 0;

    if ( strstr(STR, "/INCLUDE"     )!=NULL ) { mode = READ_INC; }
    if ( strstr(STR, "/REPORT"      )!=NULL ) { mode = READ_REP; }
    if ( strstr(STR, "/BASECASE"    )!=NULL ) { mode = READ_BSE; }
    if ( strstr(STR, "/BUS"         )!=NULL ) { mode = READ_BUS; }
    if ( strstr(STR, "/GEN"         )!=NULL ) { mode = READ_GEN; }
    if ( strstr(STR, "/OUTPUT"      )!=NULL ) { mode = READ_OTP; trace->query &= ~(QUERY_OUTP); }
    if ( strstr(STR, "/SETUP"       )!=NULL ) { mode = READ_SET; trace->query &= ~(QUERY_SETU); }
    if ( strstr(STR, "/PRI_CHANGE"  )!=NULL ) { mode = READ_PCH; trace->query &= ~(QUERY_PCHG); }
    if ( strstr(STR, "/BRANCH"      )!=NULL ) { mode = READ_BRN; trace->query &= ~(QUERY_BRCH); }
    if ( strstr(STR, "/COMMON_MODE" )!=NULL ) { mode = READ_COM; trace->query &= ~(QUERY_COMO); }
    if ( strstr(STR, "/CUT"         )!=NULL ) { mode = READ_CUT; trace->query &= ~(QUERY_CUTP); }
    if ( strstr(STR, "/SOLUTION"    )!=NULL ) { mode = READ_SOL; trace->query &= ~(QUERY_SOLN); }
/********** stuff to allow multiple "/CHANGE_BUS_TYPE" records ************/
/*    if ( strstr(STR, "/CHANGE_BUS_T")!=NULL ) { mode = READ_CBT; trace->query &= ~(QUERY_CHBT); } */
    if ( strstr(STR, "/CHG_BUS_TYPE")!=NULL ) { mode = READ_CBT; trace->query &= ~(QUERY_CHBT); 
    header_only = 1;
    } 

    if ( strstr(STR, "/AGC"         )!=NULL ) { mode = READ_AGC; trace->query &= ~(QUERY_AGCC); }
    if ( strstr(STR, "/TRACE"       )!=NULL ) { mode = READ_TRC; }
    if ( strstr(STR, "/LIMITS"      )!=NULL ) { mode = READ_LIM; }
    if ( mode == 0 ) continue;         /* note: mode is defaulted to READ_INC */
/********** stuff to allow multiple "/CHANGE_BUS_TYPE" records ************/
/*    if ( STR[0]=='/' ) {   */
    if ( STR[0]=='/' && mode != READ_CBT) {   
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
        list = cf_id2Link(str, 'L');
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
    if ( mode == READ_CUT ) {
        if (str[0]=='>') {
            row = NULL; list = NULL;
        } 
        else {
            link = cf_rec2Link(str);
            if (list==NULL) {
                row = cf_link2row(link);
                cf_appList(&trace->cutpList, row);
                list = (Link *) row->data;
            }
            else {
                cf_appList(&list, link);
            }
        }
        trace->query &= ~(QUERY_CUTP);
    }
    if ( mode == READ_BUS ) {
        list = cf_id2Link(str, 'I');
        cf_appList(&trace->qbusList, list);
        trace->query &= ~(QUERY_QBUS);
    }
    if ( mode == READ_GEN ) {
        list = cf_id2Link(str, 'I');
        row = cf_link2row(list);
        cf_appList(&trace->gbusList, row);
        trace->query &= ~(QUERY_GBUS);
    }
    if ( mode == READ_SOL ) {
        strcat(trace->solution, str);
        strcat(trace->solution, "\n");
        trace->query &= ~(QUERY_SOLN);
    }
    if ( mode == READ_CBT ) {
/********** stuff to allow multiple "/CHANGE_BUS_TYPE" records ************/
        if (header_only != 1)                           
        { 
/*        if (trace->change_bus_types[0]=='\0')                           */
/*            sprintf(trace->change_bus_types, "/CHANGE_BUS_TYPES,");     */
/**            sprintf(trace->change_bus_types, "./CHANGE_BUS_TYPES,");   ***/
        strcat(trace->change_bus_types, str);
        strcat(trace->change_bus_types, "\n");
        trace->query &= ~(QUERY_CHBT);
        }
    }
    if ( mode == READ_AGC ) {
        if (trace->agc[0]=='\0')
            sprintf(trace->agc, "/AGC\n");
        strcat(trace->agc, str);
        strcat(trace->agc, "\n");
        trace->query &= ~(QUERY_AGCC);
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
    if (op == ' ') strcpy(step->oldBase, cp);
    if (op == '+') strcpy(step->priChng, cp);
    if (op == '-') {
        if (*cp == '[') {
            step->outgLink = cf_malloc(sizeof(pf_rec));
            sscanf(cp,"[%8[^\n]%4[^\n]%*c%8[^\n]%4[^\n]%c]",
                n1,v1,n2,v2,&ct);

            cf_strsub(n1, "#", ' ');  sscanf(v1, "%f", &kv1);
            cf_strsub(n2, "#", ' ');  sscanf(v2, "%f", &kv2);
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
            cf_strsub((char *) step->outgLink, "#", ' ');
            step->type = OUTG_COMO;
        }
    }
    if (op == '@') {
        sscanf(cp, "(%8[^\n]%4[^\n])", n1, v1);
        cf_strsub(n1, "#", ' ');
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

  /* qvcurve uses traces in tact without decomposing them into lists */

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
    if (tp==NULL) return;

    if (strstr(sp, "FILE")!=NULL) {
        trace->query &= ~(QUERY_FILE);
        strcpy(trace->rpt->spec, tp);
        continue;
    }
    if (strstr(sp, "CURVES_PER_PLOT" )!=NULL) {
        sscanf(tp, "%d", &trace->curvesPerPlot);
        trace->query &= ~(QUERY_CURV);
    }

/* controls pre outage solution in "dispatchEngine" */
    if (strstr(sp, "PRE_OUT_SOL" )!=NULL) {
        strcpy(trace->preOutSol, tp);
        trace->query &= ~(QUERY_POS);
    }
/* controls pre outage solution in "dispatchEngine" */

    if (strstr(sp, "TRACE")!=NULL) {
        if (strstr(tp, "YES")!=NULL) trace->trace = TRACE_YES;
        if (strstr(tp, "NO" )!=NULL) trace->trace = TRACE_NO;
        continue;
    }
  }
  return;
}
void ff_printMainHeader(Trace *trace, ff_Report *rpt)
{
  int n = 0;
  FILE *fp = rpt->file;

  fprintf(fp, "%s\n", PAGE_BREAK); n++;

  fprintf(fp,   "/REPORT        | ");
  fprintf(fp,   "  FILE = %s\n", trace->rpt->spec); n++;
  if (strlen(trace->usrSpec)>0) {
      fprintf(fp,   "  USER_ANALYSIS = %s\n", trace->usrSpec); n++;
  }
  if (strlen(trace->conSpec)>0) {
      fprintf(fp,   "  CONTROL_FILE = %s\n", trace->conSpec); n++;
  }
  if (strlen(trace->comSpec)>0) {
      fprintf(fp,   "  COMMON_MODE_DATA = %s\n", trace->comSpec); n++;
  }
  fprintf(fp,   "/REPORT        | ");
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
  rpt->line += n;
  return;
}
void ff_printQminReport(Trace *trace, Step *step, ff_Report *rpt)
{
  ff_printPageFooter(rpt, 6);
  ff_printQminData(trace, step, rpt);
}
void ff_printQminHeader(ff_Report *rpt)
{
  char *HdQminLd[] = { /* lead */
    " ",
    "            |  Primary  |                          |            | Qknee|Qlast |Qmargn|Vknee|Vlast| Vop | Qres | Qres |      |",
    "  Basecase  |  Change   |          Outage          |    Bus     |(MVAR)|(MVAR)|(MVAR)|(PU) |(PU) |(PU) | @Qop |@Qknee|*.QVPT|",
    "------------|-----------|--------------------------|------------|------|------|------|-----|-----|-----|------|------|------|",
  };
  fprintf(rpt->file, "%s\n", HdQminLd[0]);
  fprintf(rpt->file, "%s\n", HdQminLd[1]);
  fprintf(rpt->file, "%s\n", HdQminLd[2]);
  fprintf(rpt->file, "%s\n", HdQminLd[3]);
  rpt->line += 4;
  return;
}
void ff_printQminData(Trace *trace, Step *step, ff_Report *rpt)
{
  char  newLine[1024], str[132];

  if (ff_printPageFooter(rpt, 1)) ff_printQminHeader(rpt);

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

  cf_aprint(newLine, "%6.0f|", step->Qmin);
  cf_aprint(newLine, "%6.0f|", step->QatVmin);
  cf_aprint(newLine, "%6.0f|", step->Qmargin);
  cf_aprint(newLine, "%5.3f|", step->VatQmin);
  cf_aprint(newLine, "%5.3f|", step->Vmin);
  cf_aprint(newLine, "%5.3f|", step->VatQ0);
  cf_aprint(newLine, "%6.1f|", step->QresZeroTot);
  cf_aprint(newLine, "%6.1f|", step->QresKneeTot);

  if (step->legend[0]!='\0') {
    cf_aprint(newLine, "%-6.6s|", step->legend);
  }
  else {
    cf_aprint(newLine, "%-6.6s|", "Unspecified");
  }
/******* FLAG_AN_ERROR in the *.RPT file *********/
  if ((trace->stat & FAIL_CRIT) !=0) {
    memcpy(&newLine[45], "(Err)", 5);
  }
  fprintf(rpt->file, "%s\n", newLine);
  rpt->line++;

  fflush(rpt->file);
}
void ff_printQresReport(Trace *trace, Step *step, ff_Report *rpt)
{
  Link *stepLink;

  ff_printPageFooter(rpt, 6);
  ff_printQminHeader(rpt);
  ff_printQminData(trace, step, rpt);
  ff_printPageFooter(rpt, 6);
  ff_printQresHeader(trace->gen);
  ff_printQresData(trace, step, rpt);
  return;
}
void ff_printQresHeader(ff_Report *rpt)
{
  char *HdQresLd[] = { /* lead */
    " ",
    "             |Basecase|Basecase|  Vop   |  Vknee |",
    "  Generator  |  Qmax  |  Qres  |  Qres  |  Qres  |",
    "-------------|--------|--------|--------|--------|",
  };
  fprintf(rpt->file, "%s\n", HdQresLd[0]);
  fprintf(rpt->file, "%s\n", HdQresLd[1]);
  fprintf(rpt->file, "%s\n", HdQresLd[2]);
  fprintf(rpt->file, "%s\n", HdQresLd[3]);
  rpt->line += 4;
  return;
}
void ff_printQresData(Trace *trace, Step *step, ff_Report *rpt)
{
  Link    *curGbus, *curQres, *gbusData, *solnLink;
  char     id[132], newLine[132];
  pf_rec   m;
  ff_soln *solnData;

  curGbus = trace->gbusList;
  while (curGbus!=NULL) {
    if (ff_printPageFooter(rpt, 2)) ff_printQresHeader(rpt);
    gbusData = (Link *) curGbus->data;

    cf_bus2rec((cf_Bus *) gbusData->data, &m);
    pf_rec_b2a(id, &m, "I");
    sprintf(newLine, "%-8.8s %-4.4s|", &id[6], &id[14]);

    solnLink = gbusData->next;
    solnData = (ff_soln *) solnLink->data;
    cf_aprint(newLine, "%8.2f|", solnData->Qmax);
    cf_aprint(newLine, "%8.2f|", solnData->Qres);

    solnLink = solnLink->next;
    solnData = (ff_soln *) solnLink->data;
    cf_aprint(newLine, "%8.2f|", solnData->Qres);

    solnLink = solnLink->next;
    solnData = (ff_soln *) solnLink->data;
    cf_aprint(newLine, "%8.2f|", solnData->Qres);

    fprintf(rpt->file, newLine);
    fprintf(rpt->file, "\n");
    rpt->line++;
    curGbus = curGbus->next;
  }
  if (ff_printPageFooter(rpt, 2)) ff_printQresHeader(rpt);
  fprintf(rpt->file, "-------------|--------|--------|--------|--------|\n");
  rpt->line++;
  fprintf(rpt->file, "Total Q(MVAR)|%8.2f|%8.2f|%8.2f|%8.2f|\n",
    step->QmaxBaseTot, step->QresBaseTot, step->QresZeroTot, step->QresKneeTot);
  rpt->line++;

  fflush(rpt->file);
  return;
}
void ff_printFlowReport(Trace *trace, Step *step, ff_Report *rpt)
{
  Link *stepLink;

  ff_printPageFooter(rpt, 6);  /* add here if removed from ff_printFlowHeader */
  ff_printFlowHeader(rpt);
  ff_printFlowData(trace, step, rpt);
  ff_printPageFooter(rpt, FF_PAGE_LENGTH);
  return;
}
void ff_printFlowHeader(ff_Report *rpt)
{
  char *HdFlowLd[] = { /* lead */
    " ",
    ".  |   |    CUT_PLANE_LOADING    | |      |",
    ".  |   |      BRANCH NAME        | | LOAD |",
    ".Yc|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|(AMPS)|",
  };
  fprintf(rpt->file, "%s\n", HdFlowLd[0]);
  fprintf(rpt->file, "%s\n", HdFlowLd[1]);
  fprintf(rpt->file, "%s\n", HdFlowLd[2]);
  fprintf(rpt->file, "%s\n", HdFlowLd[3]);
  rpt->line += 4;
  return;
}
void ff_printFlowData(Trace *trace, Step *step, ff_Report *rpt)
{
  Link  *curCutp, *curLine;
  pf_rec *m;
  float  Load;
  char  id[132];

  for (curCutp=trace->cutpList; curCutp!=NULL; curCutp=curCutp->next) {
    Load = 0;
    for (curLine=(Link *)curCutp->data; curLine!=NULL; curLine=curLine->next) {
        if (ff_printPageFooter(rpt, 2)) ff_printFlowHeader(rpt);
        m = (pf_rec *) curLine->data;
        pf_rec_b2a(id, m, "I");
        fprintf(rpt->file,
            "%-2.2s |%-3.3s|%-12.12s %-12.12s|%1.1s|%6.0f|\n",
            &id[0], &id[3], &id[6], &id[19], &id[31],
            m->s.branch.tot_line_load_amps);
        Load += m->s.branch.tot_line_load_amps;
        rpt->line++;
    }
    if (ff_printPageFooter(rpt, 2)) ff_printFlowHeader(rpt);
    fprintf(rpt->file, "-----------------------------------|------|\n");
    rpt->line++;
    fprintf(rpt->file, "   TOTAL CUT-PLANE FLOW   (AMPS)   |%6.0f|\n\n", Load);
    rpt->line += 2;
  }
  fflush(rpt->file);
  return;
}
void ff_printMainFooter(ff_Report *rpt)
{
/*  ff_printPageFooter(rpt, FF_PAGE_LENGTH); */
/*  maybe print a report legend */
  fclose(rpt->file);
}
int ff_printPageFooter(ff_Report *rpt, int m)
/* Guarantee room for m lines before page break */
{
  int n;

  n = cf_cntchr(FF_PAGE_FOOTER, '\n');
  rpt->line = Cf_imod(rpt->line, FF_PAGE_LENGTH);
  if ( (FF_PAGE_LENGTH - n - rpt->line) > m ) return 0;

  ff_printBlanks(rpt, n);
  fprintf(rpt->file, FF_PAGE_FOOTER, rpt->time, rpt->page);
  rpt->line = Cf_imod(rpt->line + n, FF_PAGE_LENGTH);
  rpt->page++;
  return 1;
}
void ff_printBlanks(ff_Report *rpt, int n)
{
  while (rpt->line < FF_PAGE_LENGTH - n) {
    fprintf(rpt->file, "\n");
    rpt->line++;
  }
  return;
}
/************ documented, common CF_UTIL.H candidates prototypes **************/
/* all documented, CF_UTIL.H candidates shall be designated, cf_name().       */
/******** end documented, common CF_UTIL.H candidates prototypes **************/
void ff_scale(float min, float max, float *lower, float *upper, float inc)
{
  for (*upper =      0; *upper < 0.99999*max; *upper += inc);
  for (*lower = *upper; *lower > 1.00001*min; *lower -= inc);
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
void de_load_changes(int *stat, char *file)
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
  char   s[MAX_IN];

  if (*stat & FAIL_CRIT) return;
  if (type!=OUTG_BRCH) return;
  printf("  Taking Branch Outage...\n");
  memset(&b, '\0', sizeof(pf_rec));
  memcpy(&b, brnOutg, sizeof(pf_rec));
  pf_rec_b2a(s, &b, "D");
  printf("    %s\n", s);
  pf_rec_branch(&b, "D");                                /* take outage */
  return;
}
void de_take_como_outg(int *stat, int type, char *comOutg, FILE *comFile)
{
  char s[MAX_IN], cmd[MAX_CFLOW_BUF];
  int  mode;

  if (*stat & FAIL_CRIT) return;
  if (type!=OUTG_COMO) return;
  printf("  Seeking Common-mode Outage... %s\n", comOutg);
  mode = 0;                                      /* searching */
  rewind(comFile);

  strcpy(cmd, "/changes, file = *\n"); /* Original structure in QVCURVE  */
/*   strcpy(cmd, "\n");   This is the structure in PVCURVE -- may not be OK in QVCURVE */          

  while (fgets(s, MAX_IN, comFile)!=NULL) {
    cf_str2upper(s);
    if ( s[0] == '.' ) continue;                 /* skip comments */

/************************ permit commands in common mode file ***************/
/*    if ( s[0] == '>' ) {    */
    if ( strstr(s, "> MODE") != NULL) {  
/************************ permit commands in common mode file ***************/

        if (mode==0) mode = 1;                   /* command mode */
        if (mode==2) {                           /* submit changes */
            printf("%s\n", cmd);
            pf_command(cmd);
            return;                     /* done with this outage */
        }
    }
    if ( mode == 1 ) {
        if ( strstr(s, "COMMON_MODE")!=NULL ) {
            mode = 0;
        }
        else if ( strstr(s, "MODE ")!=NULL ) {
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
  if (mode==2) {                                 /* last co-mo in file */
    printf("%s\n", cmd);
    pf_command(cmd);                             /* submit changes */
    return;                                      /* done with this outage */
  }
}
void de_get_bus(int *stat, pf_rec *busID, float *Qshunt)
{
  int err;
  if (*stat & FAIL_CRIT) return;
  printf("  Getting QV bus data...\n");
  err = pf_rec_bus(busID, "G"); /* get target QV bus */
  if (err == 1)
  {
      *Qshunt = 0.0;  /********* Bus could not be found ************/
  }
  else
  {   
     pf_rec_bus(busID, "O");                             /* get target QV bus */
    *Qshunt =
     (busID->i.ACbus.Qshunt)*pow(busID->i.ACbus.kv,2)/pow(busID->s.ACbus.Vmag,2);
  }
  return;
}
void de_modify_bus(int *stat, pf_rec *m, float VXmax)
{
  char s[MAX_IN];

  if (*stat & FAIL_CRIT) return;
  printf("  Modifying QV bus...\n");
  m->i.ACbus.Vhold_Vmax = VXmax;
  strcpy(m->i.ACbus.type, "BE");
  pf_rec_b2a(s, m, "M");
  printf("    %s\n", s);
  pf_rec_bus(m, "M");                          /* modify target QV bus */
  return;
}
void de_command(int *stat, char *cmd, char *data)
{
  char s[MAX_CFLOW_BUF];

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
