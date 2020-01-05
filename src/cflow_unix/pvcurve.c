/******************************************************************************\
UTILITY:    PVcurve
STRUCTURE:  common CFLOW architecture.
TYPE:       Powerflow (IPFSRV), /CHANGE_PARAMETER voltage-reactance curve.
SUMMARY:    Generate voltage reactance curves.
RELATED:    LINEFLOW, FINDOUT, CFUSE, MIMIC
SEE ALSO:   QVcurve
UPDATED:    April 7, 1997
LANGUAGE:   Standard C.  CFLOW Libraries.  CF_UTIL.H.
DEVELOPER:  William D. Rogers, BPA, TOP, 230-3806, wdrogers@bpa.gov
REQUESTER:  Mark Bond
USERS:      S Kinney, K Kohne, M Rodrigues, G Comegys, B Tesema, C Matthews,...
IPF:        Version 317 or above recommended.
PURPOSE:    Automates production of P-V curve plot files and plot routine setup
            files for multiple base cases and outages.
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

#define  OK               0
#define  MAX_IN           264
#define  MAX_CFLOW_BUF    8192            /* note: CFLOW_IPC_BUFF_SIZE = 8192 */
#define  MAX_CURVES       6           /* maximum number of curves for PSAP 22 */
#define  FF_IPF_VERSION   317            /* GPF.EXE_V317 or above recommended */
#define  FF_PAGE_LENGTH   61
#define  PAGE_WIDTH       132
#define  DOT_LINE         "..................................................."
#define  OUT_NAME         "PVCURVE"
#define  OUT_SERI         "P"
#define  QUERY_BASE  (int) (1<< 0) /* prompt for basecases */
#define  QUERY_BRCH  (int) (1<< 1) /* prompt for branch-outages */
#define  QUERY_PBUS  (int) (1<< 2) /* prompt for perturbed bus list */
#define  QUERY_USRA  (int) (1<< 3) /* prompt for user analysis files */
#define  QUERY_NAME  (int) (1<< 4) /* prompt for output report file name */
#define  QUERY_BLIM  (int) (1<< 6) /* prompt for BUS PERTURBATION limits */
#define  QUERY_LLIM  (int) (1<< 7) /* prompt for LOAD PERTURBATION limits */
#define  QUERY_COMO  (int) (1<< 8) /* prompt for common_mode outages list */
#define  QUERY_CMOD  (int) (1<< 9) /* prompt for common_mode data file */
#define  QUERY_PCHG  (int) (1<<10) /* prompt for /PRI_CHANGE files */
#define  QUERY_SOLN  (int) (1<<11) /* prompt for solution qualifiers */
#define  QUERY_CHBT  (int) (1<<12) /* prompt for /CHANGE_BUS_TYPE mapping */
#define  QUERY_CONT  (int) (1<<13) /* prompt for whether to continue run */
#define  QUERY_MBUS  (int) (1<<14) /* prompt for monitored bus list */
#define  QUERY_GBUS  (int) (1<<15) /* prompt for generator bus list */
#define  QUERY_CTRL  (int) (1<<16) /* prompt for auxillary control file */
#define  QUERY_CUTP  (int) (1<<17) /* prompt for /CUT_PLANE data */
#define  QUERY_PERT  (int) (1<<18) /* prompt for PERTURBATION TYPE */
#define  QUERY_OWNR  (int) (1<<19) /* prompt for owners of interest */
#define  QUERY_ZONE  (int) (1<<20) /* prompt for zones of interest */
#define  QUERY_BSKV  (int) (1<<21) /* prompt for base kV of interest */
#define  QUERY_SERI  (int) (1<<22) /* prompt for output series code */
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
#define  READ_MON    (int)     15  /* found /MONITORED_BUS   CFLOW data file */
#define  READ_GEN    (int)     16  /* found /GENERATOR_BUS   CFLOW data file */
#define  READ_CUT    (int)     17  /* found /CUT_PLANE    in CFLOW data file */
#define  OUTG_NONE   (int)     'X'
#define  OUTG_BRCH   (int)     'L'
#define  OUTG_COMO   (int)     'C'
#define  PERTURB_B   (int)      1  /* CHANGE_PARAMETER, BUS_PERTURBATION  = 1 */
#define  PERTURB_L   (int)      2  /* CHANGE_PARAMETER, LOAD_PERTURBATION = 2 */
#define  TRACE_NO    (int)      0
#define  TRACE_YES   (int)      1
#define  FAIL_CRIT   (int) (1<< 0) /* critical failure */
#define  FAIL_BASE   (int) (1<< 1)
#define  FAIL_PCHG   (int) (1<< 2)
#define  FAIL_CMMD   (int) (1<< 3)
#define  FAIL_CHNG   (int) (1<< 4)
#define  FAIL_SOLV   (int) (1<< 5)
#define  FAIL_CURV   (int) (1<< 7)
#define  FAIL_USAN   (int) (1<< 8)
#define  FAIL_CHPA   (int) (1<< 9)
#define  FAIL_OPEN   (int) (1<<10)
#define  FF_PAGE_FOOTER "\n%-122.122s PAGE %4d\n"
#define  PAGE_BREAK  "************************************************************************************************************************************"
#define  PAGE_MARK   "------------------------------------------------------------------------------------------------------------------------------------"
/***************************** end #define ************************************/
/******************************* typedef **************************************/
typedef struct CellRecord {
    struct CellRecord *prev;
    size_t             size;
    int                kind;
    void              *data;
    struct CellRecord *next;
} Cell; /* cf_Cell */

typedef struct {
  FILE  *file;
  int    line;
  int    page;
  char  *time;
  char   spec[FILENAME_MAX];
} ff_Report;

typedef struct {
  float  kv;
  float  pu;
  float  dvdq;
} ff_soln;

typedef struct processStep {
  int                    type;     /* 'X'=no outage,'L'=branch,'C'=common-mode*/
  char  oldBase[FILENAME_MAX];     /* load this base case */
  char  priChng[FILENAME_MAX];     /* primary change file */
  void              *outgLink;     /* outage: branch, change, or common_mode */
  pf_rec                busID;     /* do PV at this bus */
  char    prePV[FILENAME_MAX];     /* pre-outage  PV curve file */
  char    pstPV[FILENAME_MAX];     /* post-outage PV curve file */
  char    preMW[FILENAME_MAX];     /* pre-outage  Cut-plane loads in MW file */
  char    pstMW[FILENAME_MAX];     /* post-outage Cut-plane loads in MW file */
  char    preKV[FILENAME_MAX];     /* pre-outage  monitored bus kV file */
  char    pstKV[FILENAME_MAX];     /* post-outage monitored bus kV file */
  char    preDV[FILENAME_MAX];     /* pre-outage  monitored bus dV/dQ file */
  char    pstDV[FILENAME_MAX];     /* post-outage monitored bus dV/dQ file */
  char    preQQ[FILENAME_MAX];     /* pre-outage  Qres and Qgen file */
  char    pstQQ[FILENAME_MAX];     /* post-outage Qres and Qgen file */
  char    preUA[FILENAME_MAX];     /* pre-outage  user analysis file */
  char    pstUA[FILENAME_MAX];     /* post-outage user analysis file */
  char         title1[MAX_IN];     /* pre-outage title */
  char         title2[MAX_IN];     /* post-outage title */
} Step;

typedef struct traceRecord {
  Link  *baseList;
  Link  *pchgList;
  Link  *brchList;
  Link  *comoList;
  Link  *pbusList; /* perturbed buses */
  Link  *mbusList; /* monitored buses */
  Link  *gbusList; /* generator buses */
  Link  *trceList;
  Link  *stepList;
  Link  *qresList; /* Q reserve summary report list */
  Link  *cutpList; /* Cut plane list */
  int    trace;
  int    stat;
  long   query;
  int    perturb; /* BUS_PERTURBATION = 1, LOAD_PERTURBATION = 2 */
  ff_Report *trc;
  FILE  *comFile;
  char   usrSpec[FILENAME_MAX];
  char   conSpec[FILENAME_MAX];
  char   comSpec[FILENAME_MAX];
  char   outName[FILENAME_MAX];      /* output file names based on outName */
  char   outSeri[FILENAME_MAX];      /* output code name identifier */
  char   solution[MAX_CFLOW_BUF];    /* solution qualifiers */
  char   change_bus_types[MAX_CFLOW_BUF];
  float  Pstart;
  float  Pstop;
  float  dP;
  float  Ppct;
  float  Qpct;
  int    Pnum;
  char   Pzones[MAX_IN];
  char   Pareas[MAX_IN];
  char   Powners[MAX_IN];
  char   timeStamp[MAX_IN];
  char   userID[MAX_IN];
  char   IPFversion[MAX_IN];
  char   IPFexecute[MAX_IN];
  int    curvNumber;
} Trace;

/************************ new stuff ******************************/
typedef struct {
  int   cut_plane;
} Data_chk;

  pf_rec                *pf_rec_dbg;
  Data_chk              data_chk;

/************************ new stuff ******************************/

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
void  finalRemarks(Trace *trace);
void  cleanUpFiles(Trace *trace);
/* end MIMIC functions - called by main() *************************************/

/* top MIMIC functions - report functions *************************************/
void   ff_printMainHeader(Trace *trace, ff_Report *rpt);
void   ff_printMainFooter(ff_Report *rpt);
void   ff_printBlanks(ff_Report *rpt, int n);
int    ff_printPageFooter(ff_Report *rpt, int m);
void   ff_printDvdqHeader(FILE *fp, Trace *trace, Step *step);
void   ff_printDvdqData(FILE *fp, Trace *trace, float P);
void   ff_printVoltHeader(FILE *fp, Trace *trace, Step *step);
void   ff_printVoltData(FILE *fp, Trace *trace, float P);
void   ff_printFlowHeader(ff_Report *rpt);
void   ff_printFlowData(ff_Report *rpt, Trace *trace, Step *step);
void   ff_printLoadHeader(FILE *fp, Trace *trace, Step *step);
void   ff_printLoadData(FILE *fp, Trace *trace, float P);
void   ff_printQresHeader(FILE *fp, Trace *trace, Step *step);
void   ff_printQresData(FILE *fp, Trace *trace, float P);
void   printHdrUnits(FILE *fp, Link *curLink, char *l, char *r);
void   printHdrNames(FILE *fp, Trace *trace, Step *step, char *l, char *r);
void   printHdrBuses(FILE *fp, Link *curLink, Step *step, char *l, char *r);
void printLoadHdrUnits(FILE *fp, Trace *trace, char *l, char *r);
void printLoadHdrNames(FILE *fp, Trace *trace, Step *step, char *l, char *r);
/* end MIMIC functions - report functions *************************************/

/* top MIMIC functions - support functions ************************************/
void   checkFileList(Link *fileList);
int    findFile(Link *curFile, char *path);
void   de_bus_values(int *stat, Link *mbusList);
void   de_get_busList(int *stat, Link *gbusList);

/********************* new stuff ****************************/
/* void   de_cut_plane(int *stat, Link *cutpList); */
void   de_cut_plane(int *stat, Link *cutpList, Step *step);
/********************* new stuff ****************************/

void   de_pv_curve(Trace *trace, Step *step);
int    ff_ch_par_qv(char *fn, char *bus, char *kv, float x, float *v, float *q);
int    ff_ch_par_vq(char *fn, char *bus, char *kv, float x, float *v, float *q);
void   ff_scale(float min, float max, float *first, float *last, float inc);
void   de_load_oldbase(int *stat, char *file);
void   de_load_changes(int *stat, char *file);
FILE  *de_open_with_title(int *stat, char *filename, char *mode, char *title);
void   de_user_analysis(int *stat, char *infile, char *outfile);
void   de_take_brch_outg(int *stat, int type, pf_rec *brnOutg);
void   de_take_como_outg(int *stat, int type, char   *comOutg, FILE *comFile);
int    de_common_mode_outage(int go, Trace *trace, Step *step);
void   de_command(int *stat, char *cmd, char *data);
void   de_solution(int *stat, char *solution, char *file);
void   de_ch_par_gen(int *stat, pf_rec *bus, char *mon_str, float *mon_val,
    char *set_str, float set_val, float *x, float *y);
void   de_ch_par_load(int *stat, pf_rec *bus, char *mon_str, float *mon_val,
    char *P_str, float P_val, char *Q_str, float Q_val,
    char *zones, char *owners, char *areas, float *x, float *y);
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
/*************************** debug stuff *********************************/
cf_debug = 1;
/*************************** debug stuff *********************************/

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
  if (trace.trace==TRACE_YES) openReport(trace.trc);
  printTrace(&trace);
  queryContinue(&trace);

/* **************** debug stuff ********************* */
  pf_cflow_init(argc, argv);  /*   initialize cflow connection to powerflow */
/*  argv[1] = "-N"; */
/*  pf_cflow_init(2, argv);     initialize cflow connection to powerflow */
/* **************** debug stuff ********************* */

  dispatchEngine(&trace);
  pf_cflow_exit();
  finalRemarks(&trace);
  cleanUpFiles(&trace);
  time(&CF_time1);
  cf_logUse("PVcurve", trace.IPFversion, trace.userID);
  return 0;
}
/* end of main() */
void  initializeTrace(Trace *trace)
{
  memset(trace, '\0', sizeof(trace));
  trace->baseList         =   NULL;
  trace->pchgList         =   NULL;
  trace->brchList         =   NULL;
  trace->pbusList         =   NULL;
  trace->mbusList         =   NULL;
  trace->gbusList         =   NULL;
  trace->trceList         =   NULL;
  trace->stepList         =   NULL;
  trace->comoList         =   NULL;
  trace->cutpList         =   NULL;
  trace->trace            =   TRACE_YES;
  trace->stat             =   0;
  trace->query            = ( QUERY_BASE | QUERY_BRCH | QUERY_PBUS |
                              QUERY_USRA | QUERY_NAME | QUERY_BLIM |
                              QUERY_LLIM | QUERY_COMO | QUERY_CHBT |
                              QUERY_OWNR | QUERY_ZONE | QUERY_BSKV |
                              QUERY_CMOD | QUERY_PCHG | QUERY_SOLN |
                              QUERY_CONT | QUERY_MBUS | QUERY_GBUS |
                              QUERY_CTRL | QUERY_CUTP | QUERY_PERT |
                              QUERY_SERI );
  trace->solution[0]         = '\0';
  trace->change_bus_types[0] = '\0';
  trace->perturb         = PERTURB_B;
  trace->Pstart          = 0.;
  trace->Pstop           = -500.;
  trace->dP              = -100.;
  trace->Ppct            = 5.0;
  trace->Qpct            = 5.0;
  trace->Pnum            = 10;
  trace->Pzones[0]       = '\0';
  trace->Pareas[0]       = '\0';
  trace->Powners[0]      = '\0';
  strncpy(trace->IPFexecute, getenv("IPFSRV_CF"), MAX_IN);
  cf_parse(trace->IPFexecute, trace->IPFversion, 'F');
  cuserid(trace->userID);
  cf_time(trace->timeStamp, MAX_IN, CF_TIMESTAMP);
  strcpy(trace->outName, OUT_NAME);
  strcpy(trace->outSeri, OUT_SERI);
  cf_strsuf(CF_logSpec, trace->outName, '.', ".log");
  trace->trc              = (ff_Report *) cf_malloc(sizeof(ff_Report));
  trace->trc->file        = NULL;
  trace->trc->line        = 0;
  trace->trc->page        = 1;
  trace->trc->time        = trace->timeStamp;
  trace->trc->spec[0]     = '\0';
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
  printf("\n                      Welcome to PVcurve - updated 4-7-97");
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
    printf(" PVcurve was tested for use with IPF version %d.\n",FF_IPF_VERSION);
    printf(" You are currently using IPF version %d.\n", vn);
    printf(" PVcurve may not work as expected with IPF version %d.\n", vn);
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
  if (trace->query & QUERY_PBUS) {
    printf("\n\n/BUS");
    cf_nprompt("\n > Enter file of perturbed buses for P-V analysis: ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->query & QUERY_PBUS) {
    printf(    "\n > Enter list of perturbed buses for P-V analysis: ");
    printf(    "\n   > Btc<O>< Bus  ><KV>");
    do {
        cf_nprompt("\n   > ", "", MAX_IN, query);
        list = cf_id2Link(query, 'I');
        cf_appList(&trace->pbusList, list);
    } while (!cf_isblank(query));
  }
  if (trace->query & QUERY_MBUS) {
    printf("\n\n/MONITORED_BUSES");
    cf_nprompt("\n > Enter file of monitored buses for dvdq summary: ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->query & QUERY_MBUS) {
    printf(    "\n > Enter list of monitored buses for dvdq summary: ");
    printf(    "\n   > Btc<O>< Bus  ><KV>");
    do {
        cf_nprompt("\n   > ", "", MAX_IN, query);
        list = cf_id2Link(query, 'I');
        if (list!=NULL) {
            row = cf_link2row(list);
            cf_appList(&trace->mbusList, row);
        }
    } while (!cf_isblank(query));
  }
  if (trace->query & QUERY_GBUS) {
    printf("\n\n/GENERATOR_BUSES");
    cf_nprompt("\n > Enter file of generator buses for Qres summary: ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->query & QUERY_GBUS) {
    printf(    "\n > Enter list of generator buses for Qres summary: ");
    printf(    "\n   > Btc<O>< Bus  ><KV>");
    do {
        cf_nprompt("\n   > ", "", MAX_IN, query);
        list = cf_rec2Link(query);
        if (list!=NULL) {
            cf_appList(&trace->gbusList, list);
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
        list = cf_id2Link(query, 'I');
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
  if (trace->pbusList==NULL && trace->stepList==NULL) {
    cf_exit(1, "No buses specified for P-V analysis!  Quitting!\n");
  }
  if (trace->query & (QUERY_USRA | QUERY_NAME | QUERY_SERI | QUERY_CTRL)) {
    printf("\n\n/REPORT");
  }
  if (trace->query & QUERY_NAME) {
    cf_sprompt("\n > Enter default output name,    NAME = [%s]: ", trace->outName, trace->outName);
  }
  if (trace->query & QUERY_SERI) {
    cf_sprompt("\n > Enter output series code,   SERIES = [%s]: ", trace->outSeri, trace->outSeri);
  }
  if (trace->query & QUERY_USRA) {
    cf_sprompt("\n > Enter user analysis file,     USER_ANALYSIS = : ", "", trace->usrSpec);
  }
  if (trace->query & QUERY_CTRL) {
    cf_sprompt("\n > Enter auxillary control file,  CONTROL_FILE = : ", "", trace->conSpec);
  }
  if (trace->query & QUERY_PERT) {
    printf("\n");
    printf("\n Specify the type of change_parameter command to perform");
    printf("\n   1.  BUS Perturbation      2.  LOAD Perturbation");
    cf_iprompt("\n > Enter choice, PERTURBATION = [%d]: ", trace->perturb, &trace->perturb);
  }
  if ( !(trace->perturb & PERTURB_B) ) trace->query &= ~(QUERY_BLIM);
  if ( !(trace->perturb & PERTURB_L) ) trace->query &= ~(QUERY_LLIM);
  if (trace->query & QUERY_BLIM) {
    printf("\n\n/LIMITS");
    printf("\n P-V curve bus perturbation: Enter Start and Stop Generation and step size:");
    cf_fprompt("\n > Starting bus generation, GEN_START = [%6.1f] : ",trace->Pstart, &trace->Pstart);
    cf_fprompt("\n > Ending bus generation,   GEN_STOP  = [%6.1f] : ",trace->Pstop, &trace->Pstop);
    cf_fprompt("\n > Generation step size,    GEN_STEP  = [%6.1f] : ",trace->dP,   &trace->dP  );
  }
  if (trace->query & QUERY_LLIM) {
    printf("\n\n/LIMITS");
    printf("\n P-V curve load perturbation: Enter change per step and number of steps:");
    cf_fprompt("\n > Load change per step (%%),LOAD_P_PCT = [%5.2f] : ",trace->Ppct, &trace->Ppct);
    cf_fprompt("\n > Load change per step (%%),LOAD_Q_PCT = [%5.2f] : ",trace->Qpct, &trace->Qpct);
    cf_iprompt("\n > Number of steps,            LOAD_STEPS =  [%2d] : ",trace->Pnum, &trace->Pnum);
    cf_nprompt("\n > Enter zones  to be perturbed, LOAD_ZONES  = [%s] : ", trace->Pzones, MAX_IN, trace->Pzones);
    cf_nprompt("\n > Enter areas  to be perturbed, LOAD_AREAS  = [%s] : ", trace->Pareas, MAX_IN, trace->Pareas);
    cf_nprompt("\n > Enter owners to be perturbed, LOAD_OWNERS = [%s] : ", trace->Powners, MAX_IN, trace->Powners);
    cf_str2upper(trace->Pzones);
    cf_str2upper(trace->Pareas);
    cf_str2upper(trace->Powners);
  }
  if (trace->query & (QUERY_SOLN | QUERY_CHBT)) {
    printf("\n\n/SOLUTION, /CHANGE_BUS_TYPES"); 
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
  printf("\n");
  cf_strsuf(trace->trc->spec, trace->outName, '.', ".trc");
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
  fprintf(fp, ". %s %s %s %s\n", trace->trc->spec, trace->trc->time, trace->userID,
    trace->IPFversion);

  fprintf(fp, "/REPORT\n");
  fprintf(fp, "  NAME = %s\n", trace->outName);
  fprintf(fp, "  SERIES = %s\n", trace->outSeri);
  fprintf(fp, "  USER_ANALYSIS = %s\n", trace->usrSpec);
  fprintf(fp, "  CONTROL_FILE = %s\n", trace->conSpec);
  fprintf(fp, "  COMMON_MODE_DATA = %s\n", trace->comSpec);
  if ( trace->perturb & PERTURB_B ) fprintf(fp, "  PERTURBATION = BUS\n");
  if ( trace->perturb & PERTURB_L ) fprintf(fp, "  PERTURBATION = LOAD\n");
  if ( trace->trace == TRACE_YES)   fprintf(fp, "  TRACE = YES\n");
  if ( trace->trace == TRACE_NO )   fprintf(fp, "  TRACE = NO\n");

  fprintf(fp, "/LIMITS\n");
  if (trace->perturb==PERTURB_B) {
    fprintf(fp, "  GEN_START = %5.3f\n", trace->Pstart);
    fprintf(fp, "  GEN_STOP  = %5.3f\n", trace->Pstop);
    fprintf(fp, "  GEN_STEP  = %5.3f\n", trace->dP);
  }
  if (trace->perturb==PERTURB_L) {
    fprintf(fp, "  LOAD_P_PCT  = %5.3f\n", trace->Ppct);
    fprintf(fp, "  LOAD_Q_PCT  = %5.3f\n", trace->Qpct);
    fprintf(fp, "  LOAD_STEPS  = %d\n",    trace->Pnum);
    fprintf(fp, "  LOAD_ZONES  = %s\n",    trace->Pzones);
    fprintf(fp, "  LOAD_AREAS  = %s\n",    trace->Pareas);
    fprintf(fp, "  LOAD_OWNERS = %s\n",    trace->Powners);
  }
  if ( trace->change_bus_types[0]!=0 ) {
    fprintf(fp, "%s", trace->change_bus_types);
  }
  else {
    fprintf(fp, "/CHANGE_BUS_TYPES\n");
  }
  fprintf(fp, "/SOLUTION\n");
  fprintf(fp, "%s", trace->solution);

  if (trace->baseList==NULL) {
    cx_printList(fp, trace->trceList, "/TRACE\n",      " ",  'p', "%s", 1);
  }
  else {
    cx_printList(fp, trace->baseList, "/BASECASE\n",   "  ", 'p', "%s", 1);
    cx_printList(fp, trace->pchgList, "/PRI_CHANGE\n", "  ", 'p', "%s", 1);
    cx_printList(fp, trace->pbusList, "/BUS\n",        "",   'b', "%20.20s", 1);
    cx_printList(fp, trace->mbusList, "/MONITORED_BUS\n", "",'B', "%20.20s", 1);
    cx_printList(fp, trace->gbusList, "/GENERATOR_BUS\n", "",'r', "%20.20s", 1);
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
        if ( strstr(s, "COMMON_MODE")!=NULL ) continue;
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
  Link   *stepLink, *curBase, *curPchg, *curBrch, *curComo, *curPbus;
  Step   *step;
  int     base, chng, bran, como, outg, pbus;
  char    net_data[MAX_IN];

/* test mimic function : Loop through all cases in base case list */
  if (trace->stepList != NULL) return;
  stepLink= trace->stepList;
  curBase = trace->baseList;
  curPchg = trace->pchgList;
  curBrch = trace->brchList;
  curComo = trace->comoList;
  curPbus = trace->pbusList;
  base = 0; chng = 0; bran = 0; como = 0; outg = 0; pbus = 0;
  while (curBase != NULL) {
    stepLink = cf_addLink(stepLink, sizeof(Step));
    step = (Step *) stepLink->data;
    if (trace->stepList==NULL) trace->stepList = stepLink;

    strcpy(step->oldBase, (char *) curBase->data);

    if (curPchg!=NULL) {
        strcpy(step->priChng, (char *) curPchg->data);
    }
    if (curPbus!=NULL) {
        cf_bus2rec((cf_Bus *) curPbus->data, (pf_rec *) &step->busID);
    }
    if (curBrch!=NULL) {
        step->outgLink = cf_malloc(sizeof(pf_rec));
        cf_branch2rec((cf_Branch *) curBrch->data, (pf_rec *) step->outgLink);
        step->type = OUTG_BRCH;
        outg = bran;
    }
    else if (curComo!=NULL) {
        step->outgLink = cf_malloc(strlen(curComo->data)+1);
        strcpy(step->outgLink, curComo->data);
        step->type = OUTG_COMO;
        outg = como;
    }
    else {
        step->type = OUTG_NONE;
        outg = 0;
    }

    sprintf(step->prePV,"%s%d%d%d%c%dPV.TXT", trace->outSeri, base, chng, pbus, '_', outg);
    sprintf(step->pstPV,"%s%d%d%d%c%dPV.TXT", trace->outSeri, base, chng, pbus, step->type, outg);
    sprintf(step->preKV,"%s%d%d%d%c%dKV.TXT", trace->outSeri, base, chng, pbus, '_', outg);
    sprintf(step->pstKV,"%s%d%d%d%c%dKV.TXT", trace->outSeri, base, chng, pbus, step->type, outg);
    sprintf(step->preMW,"%s%d%d%d%c%dMW.TXT", trace->outSeri, base, chng, pbus, '_', outg);
    sprintf(step->pstMW,"%s%d%d%d%c%dMW.TXT", trace->outSeri, base, chng, pbus, step->type, outg);
    sprintf(step->preDV,"%s%d%d%d%c%dDV.TXT", trace->outSeri, base, chng, pbus, '_', outg);
    sprintf(step->pstDV,"%s%d%d%d%c%dDV.TXT", trace->outSeri, base, chng, pbus, step->type, outg);
    sprintf(step->preQQ,"%s%d%d%d%c%dQQ.TXT", trace->outSeri, base, chng, pbus, '_', outg);
    sprintf(step->pstQQ,"%s%d%d%d%c%dQQ.TXT", trace->outSeri, base, chng, pbus, step->type, outg);
    if (trace->usrSpec[0]!='\0') {
        sprintf(step->preUA,"%s%d%d%d%c%dUA.TXT", trace->outSeri, base, chng, pbus, '-', outg);
        sprintf(step->pstUA,"%s%d%d%d%c%dUA.TXT", trace->outSeri, base, chng, pbus, step->type, outg);
    }

/* APLY code */
    if (curPbus!=NULL) {
        curPbus = curPbus->next; pbus++;
    }
    if (curPbus==NULL) {
        if      (curBrch!=NULL) { curBrch = curBrch->next; bran++; }
        else if (curComo!=NULL) { curComo = curComo->next; como++; }
    }
    if (curPbus==NULL && curBrch==NULL && curComo==NULL) {
        if (curPchg!=NULL) { curPchg = curPchg->next; chng++; }
    }
    if (curPbus==NULL && curBrch==NULL && curComo==NULL && curPchg==NULL) {
        curBase = curBase->next; base++;
    }
    if (curPbus==NULL) {
        curPbus = trace->pbusList; pbus = 0;
    }
    if (curBrch==NULL && curComo==NULL) {
        curBrch = trace->brchList; bran = 0;
        curComo = trace->comoList; como = 0;
    }
    if (curPchg==NULL) {
        curPchg = trace->pchgList;
    }
/* end APLY */

/* build qvpt file header */
    sprintf(step->title1, "%s ", step->oldBase);
    sprintf(step->title2, "%s ", step->oldBase);
    if (strlen(step->priChng)!=0) {
        cf_aprint(step->title1, "+ %s ", step->priChng);
        cf_aprint(step->title2, "+ %s ", step->priChng);
    }
    if (step->type==OUTG_BRCH) {
        pf_rec_b2a(net_data, (pf_rec *) step->outgLink, "I");
        net_data[2] = 'D';
        cf_strsub(net_data, " ", '#');
        cf_aprint(step->title2, "- [%26.26s] ", &net_data[6]);
    }
    if (step->type==OUTG_COMO) {
        strcpy(net_data, (char *) step->outgLink);
        cf_strsub(net_data, " ", '#');
        cf_aprint(step->title2, "- {%s} ", net_data);
    }
    pf_rec_b2a(net_data, (pf_rec *) &step->busID, "I");
    cf_strsub(net_data, " ", '#');
    cf_aprint(step->title1, "@ (%12.12s) ", &net_data[6]);
    cf_aprint(step->title1, "> %-8.8s ", step->prePV);
    cf_aprint(step->title1, "> %-8.8s ", step->preDV);
    cf_aprint(step->title1, "> %-8.8s ", step->preKV);
    cf_aprint(step->title1, "> %-8.8s ", step->preMW);
    cf_aprint(step->title1, "> %-8.8s ", step->preQQ);
    if (trace->usrSpec[0]!='\0') {
        cf_aprint(step->title1, "> %-8.8s ", step->preUA);
    }
    cf_aprint(step->title2, "@ (%12.12s) ", &net_data[6]);
    cf_aprint(step->title2, "> %-8.8s ", step->pstPV);
    cf_aprint(step->title2, "> %-8.8s ", step->pstDV);
    cf_aprint(step->title2, "> %-8.8s ", step->pstKV);
    cf_aprint(step->title2, "> %-8.8s ", step->pstMW);
    cf_aprint(step->title2, "> %-8.8s ", step->pstQQ);
    if (trace->usrSpec[0]!='\0') {
        cf_aprint(step->title2, "> %-8.8s ", step->pstUA);
    }
    cf_aprint(step->title1, "\n");
    cf_aprint(step->title2, "\n");

/* end build qvpt file header */
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
    printf(" Please examine the above /TRACE of PVcurve operations.");
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
  printf("Processing... \n");
  for (stepLink = trace->stepList; stepLink != NULL; stepLink = stepLink->next){
    step = (Step *) stepLink->data;
    trace->stat = 0;

    cleanUpFiles(trace);
    de_pv_curve(trace, step);
  }

  return;
/**************** END OF POWERFLOW DISPATCH ENGINE ****************************/
}
void  finalRemarks(Trace *trace)
{
  printf("\n");
  printf("\nMemory allocation (bytes): Cur:%d Max:%d Alloc:%d Freed:%d\n",
    CF_memCurAlloc, CF_memMaxAlloc, CF_memTotAlloc, CF_memTotFreed);
  if (trace->trace == TRACE_YES)
    printf("\nTrace report written to %s", trace->trc->spec);
  if (CF_logFile != NULL) printf("\nError report written to %s", CF_logSpec);
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
    cf_aprint(newLine, "> %-6.6s... ", step->prePV);
    cf_aprint(newLine, "> %-6.6s... ", step->pstPV);
/*
    cf_aprint(newLine, "> %-8.8s ", step->preDV);
    cf_aprint(newLine, "> %-8.8s ", step->pstDV);
    cf_aprint(newLine, "> %-8.8s ", step->preKV);
    cf_aprint(newLine, "> %-8.8s ", step->pstKV);
    cf_aprint(newLine, "> %-8.8s ", step->preMW);
    cf_aprint(newLine, "> %-8.8s ", step->pstMW);
    cf_aprint(newLine, "> %-8.8s ", step->preQQ);
    cf_aprint(newLine, "> %-8.8s ", step->pstQQ);
    if (trace->usrSpec[0]!='\0') {
        cf_aprint(newLine, "> %-8.8s ", step->preUA);
        cf_aprint(newLine, "> %-8.8s ", step->pstUA);
    }
*/
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
  while (fgets(str, MAX_IN, readMe)!=NULL) { 
    if ( cf_iscomment(str) ) continue;
    strcpy(STR, str);
    cf_str2upper(STR);
    if ( strstr(STR, "/INCLUDE"     )!=NULL ) { mode = READ_INC; }
    if ( strstr(STR, "/REPORT"      )!=NULL ) { mode = READ_REP; }
    if ( strstr(STR, "/BASECASE"    )!=NULL ) { mode = READ_BSE; }
    if ( strstr(STR, "/BUS"         )!=NULL ) { mode = READ_BUS; }
    if ( strstr(STR, "/MONITORED_BU")!=NULL ) { mode = READ_MON; trace->query &= ~(QUERY_MBUS); }
    if ( strstr(STR, "/GENERATOR_BU")!=NULL ) { mode = READ_GEN; trace->query &= ~(QUERY_GBUS); }
    if ( strstr(STR, "/PRI_CHANGE"  )!=NULL ) { mode = READ_PCH; trace->query &= ~(QUERY_PCHG); }
    if ( strstr(STR, "/BRANCH"      )!=NULL ) { mode = READ_BRN; trace->query &= ~(QUERY_BRCH); }
    if ( strstr(STR, "/COMMON_MODE" )!=NULL ) { mode = READ_COM; trace->query &= ~(QUERY_COMO); }
    if ( strstr(STR, "/CUT"         )!=NULL ) { mode = READ_CUT; trace->query &= ~(QUERY_CUTP); }
    if ( strstr(STR, "/SOLUTION"    )!=NULL ) { mode = READ_SOL; trace->query &= ~(QUERY_SOLN); }
    if ( strstr(STR, "/CHANGE_BUS_T")!=NULL ) { mode = READ_CBT; trace->query &= ~(QUERY_CHBT); }
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
    if ( mode == READ_BRN ) {
        list = cf_id2Link(str, 'I');
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
        cf_appList(&trace->pbusList, list);
        trace->query &= ~(QUERY_PBUS);
    }
    if ( mode == READ_MON ) {
        list = cf_id2Link(str, 'I');
        row = cf_link2row(list);
        cf_appList(&trace->mbusList, row);
        trace->query &= ~(QUERY_MBUS);
    }
    if ( mode == READ_GEN ) {
        link = cf_rec2Link(str);
        cf_appList(&trace->gbusList, link);
        trace->query &= ~(QUERY_GBUS);
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
  }

  cf_appList(&trace->stepList, stepLink);

  trace->query &= ~( QUERY_BASE | QUERY_PCHG | QUERY_BRCH | QUERY_PBUS |
                     QUERY_USRA | QUERY_COMO );

  /* qvcurve uses traces in tact without decomposing them into lists */

  return;
}
void ff_limits(char *s, Trace *trace)
{
  char  *sp, *tp, t[MAX_IN];

  if (strstr(s, "GEN_START"  )!=NULL) {
    sscanf(s, " GEN_START = %f", &trace->Pstart);
    trace->query &= ~(QUERY_BLIM);
  }
  if (strstr(s, "GEN_STOP"   )!=NULL) {
    sscanf(s, " GEN_STOP = %f", &trace->Pstop);
    trace->query &= ~(QUERY_BLIM);
  }
  if (strstr(s, "GEN_STEP"   )!=NULL) {
    sscanf(s, " GEN_STEP = %f", &trace->dP);
    trace->query &= ~(QUERY_BLIM);
  }
  if (strstr(s, "LOAD_P_PCT" )!=NULL) {
    sscanf(s, " LOAD_P_PCT = %f", &trace->Ppct);
    trace->query &= ~(QUERY_LLIM);
  }
  if (strstr(s, "LOAD_Q_PCT" )!=NULL) {
    sscanf(s, " LOAD_Q_PCT = %f", &trace->Qpct);
    trace->query &= ~(QUERY_LLIM);
  }
  if (strstr(s, "LOAD_STEPS" )!=NULL) {
    sscanf(s, " LOAD_STEPS = %d", &trace->Pnum);
    trace->query &= ~(QUERY_LLIM);
  }
  if (strstr(s, "LOAD_ZONES" )!=NULL) {
    sscanf(s, " LOAD_ZONES = %s", &trace->Pzones);
    trace->query &= ~(QUERY_LLIM);
  }
  if (strstr(s, "LOAD_AREAS" )!=NULL) {
    sscanf(s, " LOAD_AREAS = %s", &trace->Pareas);
    trace->query &= ~(QUERY_LLIM);
  }
  if (strstr(s, "LOAD_OWNERS")!=NULL) {
    sscanf(s, " LOAD_OWNERS = %s", &trace->Powners);
    trace->query &= ~(QUERY_LLIM);
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

    if (strstr(sp, "NAME")!=NULL) {
        trace->query &= ~(QUERY_NAME);
        strcpy(trace->outName, tp);
        continue;
    }
    if (strstr(sp, "SERIES")!=NULL) {
        trace->query &= ~(QUERY_SERI);
        strcpy(trace->outSeri, tp);
        continue;
    }
    if (strstr(sp, "TRACE")!=NULL) {
        if (strstr(tp, "YES")!=NULL) trace->trace = TRACE_YES;
        if (strstr(tp, "NO" )!=NULL) trace->trace = TRACE_NO;
        continue;
    }
    if (strstr(sp, "PERTURB")!=NULL) {
        if (strstr(tp, "BUS" )!=NULL) trace->perturb = PERTURB_B;
        if (strstr(tp, "LOAD")!=NULL) trace->perturb = PERTURB_L;
        trace->query &= ~(QUERY_PERT);
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
  fprintf(fp,   "  NAME = %s\n", trace->outName); n++;
  fprintf(fp,   "  SERIES = %s\n", trace->outSeri); n++;
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
  fprintf(fp,   "  TRACE =");
  if (  trace->trace==TRACE_YES ) fprintf(fp, " YES");
  if (  trace->trace==TRACE_NO  ) fprintf(fp, " NO");
  fprintf(fp, "\n");  n++;

  fprintf(fp,   "/LIMITS        | ");
  if (trace->perturb==PERTURB_B) {
    fprintf(fp, "  GEN_START = %5.3f\n", trace->Pstart);
    fprintf(fp, "  GEN_STOP  = %5.3f\n", trace->Pstop);
    fprintf(fp, "  GEN_STEP  = %5.3f\n", trace->dP);
  }
  if (trace->perturb==PERTURB_L) {
    fprintf(fp, "  LOAD_PCT    = %5.3f\n", trace->Ppct);
    fprintf(fp, "  LOAD_STEPS  = %d\n",    trace->Pnum);
    fprintf(fp, "  LOAD_ZONES  = %s\n",    trace->Pzones);
    fprintf(fp, "  LOAD_AREAS  = %s\n",    trace->Pareas);
    fprintf(fp, "  LOAD_OWNERS = %s\n",    trace->Powners);
  }
  fprintf(fp, "%s\n", PAGE_BREAK); n++;
  rpt->line += n;
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
  strcpy(cmd, "/changes, file = *\n");
  while (fgets(s, MAX_IN, comFile)!=NULL) {
    cf_str2upper(s);
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
void de_command(int *stat, char *cmd, char *data)
{
  char s[MAX_CFLOW_BUF];

  if (*stat & FAIL_CRIT) return;
  if (strlen(cmd)==0) return;
  if (strlen(data)==0) return;
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
  char s[MAX_CFLOW_BUF];

  if (*stat & FAIL_CRIT) return;
  printf("\n  Solving Base... %s\n", file);
  sprintf(s, "/SOLUTION\n%s", solution);
  printf("%s\n", s);
  if (pf_command(s)!=0) {
    cf_logErr(s);
    cf_logErr("de_solution:  Cannot solve basecase!\n");
    *stat |= FAIL_SOLV;
    *stat |= FAIL_CRIT;
  }
  return;
}
void de_pv_curve(Trace *trace, Step *step)
{
  FILE *pv0, *pv1, *dv0, *dv1, *kv0, *kv1, *mw0, *mw1, *qq0, *qq1;
  float Xstart, Xstop, dX, X, Y, x, y, Ppct, Qpct, Xout;
  char  title[MAX_IN];
  pf_rec *r;

/************************ new stuff ******************************
 Data_chk data_chk;
************************ new stuff ******************************/

  printf("  Making PV curve...\n");

/************************ new stuff ******************************/
  data_chk.cut_plane = 1;
/************************ new stuff ******************************/

  sprintf(title, "C1 %sC2 V (PU)   P (MW)", step->title1);
  pv0 = de_open_with_title(&trace->stat, step->prePV, "w", title);

  sprintf(title, "C1 %sC2 V (PU)   P (MW)", step->title2);
  pv1 = de_open_with_title(&trace->stat, step->pstPV, "w", title);

  dv0 = de_open_with_title(&trace->stat, step->preDV, "w", step->title1);
  dv1 = de_open_with_title(&trace->stat, step->pstDV, "w", step->title2);
  kv0 = de_open_with_title(&trace->stat, step->preKV, "w", step->title1);
  kv1 = de_open_with_title(&trace->stat, step->pstKV, "w", step->title2);
  mw0 = de_open_with_title(&trace->stat, step->preMW, "w", step->title1);
  mw1 = de_open_with_title(&trace->stat, step->pstMW, "w", step->title2);
  qq0 = de_open_with_title(&trace->stat, step->preQQ, "w", step->title1);
  qq1 = de_open_with_title(&trace->stat, step->pstQQ, "w", step->title2);

  if (trace->stat & FAIL_CRIT) return;

  ff_printDvdqHeader(dv0, trace, step);
  ff_printDvdqHeader(dv1, trace, step);
  ff_printVoltHeader(kv0, trace, step);
  ff_printVoltHeader(kv1, trace, step);
  ff_printLoadHeader(mw0, trace, step);
  ff_printLoadHeader(mw1, trace, step);
  ff_printQresHeader(qq0, trace, step); /* qres, qgen */
  ff_printQresHeader(qq1, trace, step);

  if (trace->perturb & PERTURB_B) {
    Xstart = trace->Pstart;
    Xstop  = trace->Pstop;
    dX = (trace->Pstart > trace->Pstop) ? -1*fabs(trace->dP) : fabs(trace->dP);
  }
  else if (trace->perturb & PERTURB_L) {
    Xstart =  0;
    Xstop  =  trace->Pnum;
    dX     =  1;
  }
  else return;
  r = &step->busID;

  X = Xstart;
  while (1) {
    if ( (Xstart >= Xstop) && (X < Xstop) ) break;
    if ( (Xstart <= Xstop) && (X > Xstop) ) break;

    printf("Xstart = %f, X = %f, Xstop = %f\n", Xstart, X, Xstop);

    de_load_oldbase(&trace->stat, step->oldBase);
    de_load_changes(&trace->stat, step->priChng);

    pf_user_init_def();
    de_user_analysis(&trace->stat, trace->usrSpec, step->preUA);
    de_solution(&trace->stat, trace->solution, step->oldBase);
    if (trace->perturb & PERTURB_B) {
        de_ch_par_gen(&trace->stat, r, "V", &Y, "P", X, &x, &y);
        Xout = X;
    }
    if (trace->perturb & PERTURB_L) {
        Ppct = X * trace->Ppct;
        Qpct = X * trace->Qpct;
        if (Ppct==0) Ppct = .00001;
        de_ch_par_load(&trace->stat,r, "V", &Y, "P", Ppct, "Q", Qpct,
            trace->Pzones, trace->Powners, trace->Pareas, &x, &y);
        Xout = Ppct;
    }
    if (trace->stat & FAIL_CRIT) break;

    fprintf(pv0, "%.10f %.10f\n", x, y);

/************************ new stuff ******************************/
/*    de_cut_plane(&trace->stat, trace->cutpList); */
 
   de_cut_plane(&trace->stat, trace->cutpList, step);
   data_chk.cut_plane = 0;
/************************ new stuff ******************************/

    de_bus_values(&trace->stat, trace->mbusList);
    de_get_busList(&trace->stat, trace->gbusList);
    ff_printLoadData(mw0, trace, Xout);
    ff_printDvdqData(dv0, trace, Xout);
    ff_printVoltData(kv0, trace, Xout);
    ff_printQresData(qq0, trace, Xout);

    de_command(&trace->stat, "/INCLUDE_CONTROL, FILE = ", trace->conSpec);
    de_command(&trace->stat, trace->change_bus_types, " ");
    de_take_brch_outg(&trace->stat, step->type, step->outgLink);
    de_take_como_outg(&trace->stat, step->type, step->outgLink, trace->comFile);

    pf_user_init_def();
    de_user_analysis(&trace->stat, trace->usrSpec, step->pstUA);
    de_solution(&trace->stat, trace->solution, step->oldBase);
/*
   the following change-param commands are submitted for the single purpose of
   retrieving the P-V values for the target bus.  these values could possibly be
   retrieved explicitly with another command
*/
    if (trace->perturb & PERTURB_B) {
        de_ch_par_gen(&trace->stat, r, "V", &Y, "P", X, &x, &y);
    }
    if (trace->perturb & PERTURB_L) {
        Ppct = 0; /* no change from pre-outage */
        Qpct = 0; /* no change from pre-outage */
        if (Ppct==0) Ppct = .00001;
        de_ch_par_load(&trace->stat,r, "V", &Y, "P", Ppct, "Q", Qpct,
            trace->Pzones, trace->Powners, trace->Pareas, &x, &y);
    }
    if (trace->stat & FAIL_CRIT) break;

    fprintf(pv1, "%.10f %.10f\n", x, y);

/************************* debug stuff *******************************/
/*    de_cut_plane(&trace->stat, trace->cutpList); */
    de_cut_plane(&trace->stat, trace->cutpList, step);
/************************* debug stuff *******************************/

    de_bus_values(&trace->stat, trace->mbusList);
    de_get_busList(&trace->stat, trace->gbusList);
    ff_printLoadData(mw1, trace, Xout);
    ff_printDvdqData(dv1, trace, Xout);
    ff_printVoltData(kv1, trace, Xout);
    ff_printQresData(qq1, trace, Xout);
    X = X + dX;
  }
  printf(" Data written to %s\n", step->prePV);
  printf(" Data written to %s\n", step->pstPV);
  printf(" Data written to %s\n", step->preKV);
  printf(" Data written to %s\n", step->pstKV);
  printf(" Data written to %s\n", step->preDV);
  printf(" Data written to %s\n", step->pstDV);
  printf(" Data written to %s\n", step->preMW);
  printf(" Data written to %s\n", step->pstMW);
  printf(" Data written to %s\n", step->preQQ);
  printf(" Data written to %s\n", step->pstQQ);
  if (trace->usrSpec[0]!='\0') {
    printf(" Data written to %s\n", step->preUA);
    printf(" Data written to %s\n", step->pstUA);
  }  
  fclose(pv0);
  fclose(pv1);
  fclose(dv0);
  fclose(dv1);
  fclose(kv0);
  fclose(kv1);
  fclose(mw0);
  fclose(mw1);
  fclose(qq0);
  fclose(qq1);
}
void de_ch_par_gen(int *stat, pf_rec *bus, char *mon_str, float *mon_val,
    char *set_str, float set_val, float *x, float *y)
{
  char  data[MAX_CFLOW_BUF], id[132], name[9], volt[5], xc, yc;
  float rx, ry;
  int   rtn;

  if (*stat & FAIL_CRIT) return;
  printf("  Finding %s at %s = %8.2f\n", mon_str, set_str, set_val);

  pf_rec_b2a(id, bus, "I");
  sprintf(name, "%-8.8s", &id[6]);
  sprintf(volt, "%-4.4s", &id[14]);
  cf_strsub(name, " ", '#');

  sprintf(data,
    "/CHANGE_PARAMETER,FILE=NULL,BUS=%8.8s %4.4s,%s=%f,%s=?\n(END)\n",
    name, volt, set_str, set_val, mon_str);
  printf("%s\n", data);
  rtn = pf_command(data);
  if (rtn!=0) {
    printf("%s %s, %s = %13.10f  NO SOLUTION\n", name, volt, set_str, set_val);
    *stat |= FAIL_CHPA;
    return;
  }
  if ( sscanf(reply_pf, "%*17c %c  = %f %c  = %f", &xc, x, &yc, y)!=4) rtn = -1;
  if ( strstr(reply_pf, "failed") != NULL ) rtn = -1;
  if (rtn!=0) {
    *stat |= FAIL_CHPA;
    return;
  }

  printf("%s %s, %c =%13.10f  %c =%17.10f\n", name, volt, xc, *x,
    yc, *y);

  return;
}
void de_ch_par_load(int *stat, pf_rec *bus, char *mon_str, float *mon_val,
    char *P_str, float P_val, char *Q_str, float Q_val,
    char *zones, char *owners, char *areas, float *x, float *y)
{
  char  data[MAX_CFLOW_BUF], id[132], name[9], volt[5], xc[3], yc[3];
  float rx, ry;
  int   rtn;

  if (*stat & FAIL_CRIT) return;
  printf("  Finding %s at %s = %8.2f and %s = %8.2f\n",
    mon_str, P_str, P_val, Q_str, Q_val);

  pf_rec_b2a(id, bus, "I");
  sprintf(name, "%-8.8s", &id[6]);
  sprintf(volt, "%-4.4s", &id[14]);
  cf_strsub(name, " ", '#');

  sprintf(data, "/CHANGE_PARAMETER,FILE=NULL,BUS=%8.8s %4.4s,%s=? -\n",
    name, volt, mon_str);
  cf_aprint(data,
    "%%LOAD_CHANGE  %%%s=%f, %%%s=%f", P_str, P_val, Q_str, Q_val);
  if (zones[0] !='\0') cf_aprint(data, ", -\nZONES=%s", zones);
  if (owners[0]!='\0') cf_aprint(data, ", -\nOWNERS=%s", owners);
  if (areas[0] !='\0') cf_aprint(data, ", -\nAREAS=%s", areas);
  cf_aprint(data, "\n(END)\n");

  printf("%s\n", data);
  rtn = pf_command(data);
  if (rtn!=0) {
    printf("%s %s, %s = %13.10f  NO SOLUTION\n", name, volt, P_str, P_val);
    *stat |= FAIL_CHPA;
    return;
  }
  printf(reply_pf);
  if ( sscanf(reply_pf, "%*17c %s  = %f %s  = %f", yc, y, xc, x)!=4) rtn = -1;
  if ( strstr(reply_pf, "failed") != NULL ) rtn = -1;
  if (rtn!=0) {
    *stat |= FAIL_CHPA;
    return;
  }

  printf("%s %s, %s =%13.10f  %s =%17.10f\n", name, volt, yc, *y,
    xc, *x);

  return;
}
void de_cut_plane(int *stat, Link *cutpList, Step *step)
{
  Link  *curCutp, *curLine;

/************************ new stuff ******************************/
  int error;   
/************************ new stuff ******************************/

  if (*stat & FAIL_CRIT) return;
  if (cutpList==NULL) return;
  printf("  Getting cut-plane flows...\n");
  for (curCutp=cutpList; curCutp!=NULL; curCutp=curCutp->next) {
    for (curLine=(Link *)curCutp->data; curLine!=NULL; curLine=curLine->next) {
        memset(&((pf_rec *)curLine->data)->s.branch, '\0', sizeof(pf_branch_soln));
        error = pf_rec_branch(curLine->data, "O");

/* *********************** new stuff ********************* */
  if (data_chk.cut_plane == 1 && error != 0) {  
    pf_rec_dbg = (pf_rec *)curLine->data;
    cf_logErr(" %3s|%9s|%7.2f|%9s|%7.2f|%1c is missing in case %s \n", 
      pf_rec_dbg->i.branch.type, pf_rec_dbg->i.branch.bus1_name, pf_rec_dbg->i.branch.bus1_kv,
      pf_rec_dbg->i.branch.bus2_name, pf_rec_dbg->i.branch.bus2_kv, pf_rec_dbg->i.branch.ckt_id, step->oldBase); 
  } 
/* *********************** new stuff ********************* */

    }
  }
  return;
}
void de_bus_values(int *stat, Link *mbusList)
{
  Link  *curLink, *curData, *solnLink;
  ff_soln *curSoln;
  float  dvdq, pu, kv;
  pf_rec b;
  int    n;
  char   symbol[7];

  if (*stat & FAIL_CRIT) return;
  if (mbusList==NULL) return;
  printf("  Getting bus quantities...\n");
  pf_user_init_def();
/* set up definitions */
  n = 1;
  for (curLink=mbusList; curLink!=NULL; curLink=curLink->next) {
    curData = (Link *) curLink->data;
    cf_bus2rec((cf_Bus *) curData->data, &b);
    sprintf(symbol, "MDVQ%d", n);
    pf_user_bus(symbol, &b, ".DVQ");        /* dVdQ, kV/MVAR */
    sprintf(symbol, "MVK%d", n);
    pf_user_bus(symbol, &b, ".VK");         /* volts, kV */
    n++;
    if (n>=100) { cf_logErr("Only reading first 99 buses!\n"); break; }
  }
  pf_user_sub_def("BASE");

/* read definitions */
  n = 1;
  for (curLink=mbusList; curLink!=NULL; curLink=curLink->next) {
    curData = (Link *) curLink->data;
    cf_bus2rec((cf_Bus *) curData->data, &b);
    sprintf(symbol, "MDVQ%d", n);
    pf_user_quantity(symbol, ".DVQ", &dvdq);
    sprintf(symbol, "MVK%d", n);
    pf_user_quantity(symbol, ".VK", &kv);
/*    printf("%f\n", dvdq); */
    if (curData->next==NULL)
        solnLink = cf_addLink(curData, sizeof(ff_soln));
    else
        solnLink = curData->next;
    curSoln = (ff_soln *) solnLink->data;
    curSoln->dvdq = dvdq;
    curSoln->kv = kv;
    n++;
    if (n>=100) { cf_logErr("Only reading first 99 buses!\n"); break; }
  }
  return;
}
void de_get_busList(int *stat, Link *gbusList)
{
  Link  *curGbus;

  if (*stat & FAIL_CRIT) return;
  if (gbusList==NULL) return;
  printf("  Getting generator bus quantities...\n");
  for (curGbus=gbusList; curGbus!=NULL; curGbus=curGbus->next) {
    pf_rec_bus((pf_rec *) curGbus->data, "G");
    pf_rec_bus((pf_rec *) curGbus->data, "O");
  }
  return;
}
void ff_printDvdqData(FILE *fp, Trace *trace, float P)
{
  Link  *curLink, *curData, *solnLink;
  ff_soln *curSoln;

  if (trace->stat & FAIL_CRIT) return;
  fprintf(fp, "  %9.2f |", P);
  for (curLink=trace->mbusList; curLink!=NULL; curLink=curLink->next) {
    curData = (Link *) curLink->data;
    solnLink = curData->next;
    curSoln = (ff_soln *) solnLink->data;
    fprintf(fp, "    %7.4f |", curSoln->dvdq);
  }
  fprintf(fp, "\n");
  return;
}
void ff_printDvdqHeader(FILE *fp, Trace *trace, Step *step)
{
  static char *HdLd[] = { /* lead */
    "    Pgen    |",
    "    (MW)    |",
    "%-12.12s|",
  };
  static char *HdRp[] = { /* repeat */
    "   dVdQ     |",
    " (kV/MVAR)  |",
    "%-12.12s|",
  };
  printHdrUnits(fp, trace->mbusList, HdLd[0], HdRp[0]);
  printHdrUnits(fp, trace->mbusList, HdLd[1], HdRp[1]);
  printHdrNames(fp, trace, step, HdLd[2], HdRp[2]);
  return;
}
void printHdrUnits(FILE *fp, Link *curLink, char *l, char *r)
{
  fprintf(fp, l); /* lead */
  for (; curLink!=NULL; curLink=curLink->next) {
    fprintf(fp, r); /* repeat */
  }
  fprintf(fp, "\n");
}
void printHdrNames(FILE *fp, Trace *trace, Step *step, char *l, char *r)
{
  Link *curLink, *curData;
  char    id[132];
  pf_rec b;

  pf_rec_b2a(id, &step->busID, "I");
  fprintf(fp, l, &id[6]); /* lead */
  for (curLink=trace->mbusList; curLink!=NULL; curLink=curLink->next) {
    curData = (Link *) curLink->data;
    cf_bus2rec((cf_Bus *) curData->data, &b);
    pf_rec_b2a(id, &b, "I");
    fprintf(fp, r, &id[6]); /* repeat */
  }
  fprintf(fp, "\n");
}
void printHdrBuses(FILE *fp, Link *curLink, Step *step, char *l, char *r)
{
  char    id[132];
  pf_rec *b;

  pf_rec_b2a(id, &step->busID, "I");
  fprintf(fp, l, &id[6]); /* lead */
  for (; curLink!=NULL; curLink=curLink->next) {
    b = (pf_rec *) curLink->data;
    pf_rec_b2a(id, b, "I");
    fprintf(fp, r, &id[6]); /* repeat */
  }
  fprintf(fp, "\n");
}
void ff_printVoltData(FILE *fp, Trace *trace, float P)
{
  Link  *curLink, *curData, *solnLink;
  ff_soln *curSoln;

  if (trace->stat & FAIL_CRIT) return;
  fprintf(fp, "  %9.2f |", P);
  for (curLink=trace->mbusList; curLink!=NULL; curLink=curLink->next) {
    curData = (Link *) curLink->data;
    solnLink = curData->next;
    curSoln = (ff_soln *) solnLink->data;
    fprintf(fp, "     %6.1f |", curSoln->kv);
  }
  fprintf(fp, "\n");
  return;
}
void ff_printVoltHeader(FILE *fp, Trace *trace, Step *step)
{
  static char *HdLd[] = { /* lead */
    "    Pgen    |",
    "    (MW)    |",
    "%-12.12s|",
  };
  static char *HdRp[] = { /* repeat */
    "  Voltage   |",
    "    (kV)    |",
    "%-12.12s|",
  };
  printHdrUnits(fp, trace->mbusList, HdLd[0], HdRp[0]);
  printHdrUnits(fp, trace->mbusList, HdLd[1], HdRp[1]);
  printHdrNames(fp, trace, step, HdLd[2], HdRp[2]);
  return;
}
void ff_printLoadHeader(FILE *fp, Trace *trace, Step *step)
{
  static char *HdLd[] = { /* lead */
    "    Pgen    |",
    "    (MW)    |",
    "%-12.12s|",
  };
  static char *HdRp[] = { /* repeat */
    " Pin  |",
    " (MW) |",
    "CUT %2d|",
  };
  printLoadHdrUnits(fp, trace, HdLd[0], HdRp[0]);
  printLoadHdrUnits(fp, trace, HdLd[1], HdRp[1]);
  printLoadHdrNames(fp, trace, step, HdLd[2], HdRp[2]);
  return;
}
void printLoadHdrUnits(FILE *fp, Trace *trace, char *l, char *r)
{
  Link *curLink;

  fprintf(fp, l); /* lead */
  for (curLink=trace->cutpList; curLink!=NULL; curLink=curLink->next) {
    fprintf(fp, r); /* repeat */
  }
  fprintf(fp, "\n");
}
void printLoadHdrNames(FILE *fp, Trace *trace, Step *step, char *l, char *r)
{
  Link  *curLink;
  char   id[132];
  int    n;

  pf_rec_b2a(id, &step->busID, "I");
  fprintf(fp, l, &id[6]); /* lead */
  n = 1;
  for (curLink=trace->cutpList; curLink!=NULL; curLink=curLink->next) {
    fprintf(fp, r, n); /* repeat */
    n++;
  }
  fprintf(fp, "\n");
}
void ff_printQresHeader(FILE *fp, Trace *trace, Step *step)
{
  static char *HdLd[] = { /* lead */
    "    Pgen    |",
    "    (MW)    |",
/* ********************** new stuff ******************** */
    "%-12.12s",
/*    "%-12.12s|", */
/* ********************** new stuff ******************** */
  };
  static char *HdRp[] = { /* repeat */
    " Qgen | Qres |",
    " (MW) | (MW) |",
    "|%-12.12s|",
  };
  printHdrUnits(fp, trace->gbusList, HdLd[0], HdRp[0]);
  printHdrUnits(fp, trace->gbusList, HdLd[1], HdRp[1]);
  printHdrBuses(fp, trace->gbusList, step, HdLd[2], HdRp[2]);
  return;
}
void ff_printQresData(FILE *fp, Trace *trace, float P)
{
  pf_rec *b;
  Link  *curLink, *curData, *solnLink;
  ff_soln *curSoln;

  if (trace->stat & FAIL_CRIT) return;
  fprintf(fp, "  %9.2f |", P);
  for (curLink=trace->gbusList; curLink!=NULL; curLink=curLink->next) {
    b = (pf_rec *) curLink->data;
    fprintf(fp, "%6.1f|", b->s.ACbus.Qgen);
    fprintf(fp, "%6.1f|", b->i.ACbus.Qsch_Qmax - b->s.ACbus.Qgen);
  }
  fprintf(fp, "\n");
  return;
}
void ff_printFlowHeader(ff_Report *rpt)
{
  char *HdFlowLd[] = { /* lead */
    " ",
    ".  |   |    CUT_PLANE_LOADING    | |      |",
    ".  |   |      BRANCH NAME        | | LOAD |",
    ".Yc|OWN|< BUS1 ><V1> < BUS2 ><V2>|C| (MW) |",
  };
  fprintf(rpt->file, "%s\n", HdFlowLd[0]);
  fprintf(rpt->file, "%s\n", HdFlowLd[1]);
  fprintf(rpt->file, "%s\n", HdFlowLd[2]);
  fprintf(rpt->file, "%s\n", HdFlowLd[3]);
  rpt->line += 4;
  return;
}
void ff_printFlowData(ff_Report *rpt, Trace *trace, Step *step)
{
  Link  *curCutp, *curLine;
  pf_rec *m;
  float   Load;
  char    id[132];

  for (curCutp=trace->cutpList; curCutp!=NULL; curCutp=curCutp->next) {
    Load = 0;
    for (curLine=(Link *)curCutp->data; curLine!=NULL; curLine=curLine->next) {
        m = (pf_rec *) curLine->data;
        pf_rec_b2a(id, m, "I");
        fprintf(rpt->file,
            "%-2.2s |%-3.3s|%-12.12s %-12.12s|%1.1s|%6.0f|\n",
            &id[0], &id[3], &id[6], &id[19], &id[31],
            m->s.branch.Pin);
        Load += m->s.branch.Pin;
        rpt->line++;
    }
    fprintf(rpt->file, "-----------------------------------|------|\n");
    rpt->line++;
    fprintf(rpt->file, "   TOTAL CUT-PLANE FLOW    (MW)    |%6.0f|\n\n", Load);
    rpt->line += 2;
  }
  fflush(rpt->file);
  return;
}
void ff_printLoadData(FILE *fp, Trace *trace, float P)
{
  Link  *curCutp, *curLine;
  pf_rec *m;
  float   Load;
  char    id[132];

  if (trace->stat & FAIL_CRIT) return;
  fprintf(fp, "  %9.2f |", P);
  for (curCutp=trace->cutpList; curCutp!=NULL; curCutp=curCutp->next) {
    Load = 0;
    for (curLine=(Link *)curCutp->data; curLine!=NULL; curLine=curLine->next) {
        m = (pf_rec *) curLine->data;
        Load += m->s.branch.Pin;
    }
    fprintf(fp, "%6.0f|", Load);
  }
  fprintf(fp, "\n");
  fflush(fp);
  return;
}
FILE *de_open_with_title(int *stat, char *filename, char *mode, char *title)
{
  FILE *fp;

  fp = cf_openFile(filename, "w");
  if (fp==NULL)
    *stat |= FAIL_OPEN;
  else
    fprintf(fp, "%s\n", title);
  return fp;
}
