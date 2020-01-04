/******************************************************************************\
UTILITY:    MIMIC
STRUCTURE:  common CFLOW architecture.
TYPE:       Powerflow (IPFSRV) case generation and screening. 
SUMMARY:    Generate new cases and check for loading and voltage violations.
RELATED:    LINEFLOW, FINDOUT
SEE ALSO:   MODES.TXT
UPDATED:    April 9, 1997
LANGUAGE:   Standard C.  CFLOW Libraries.  CF_UTIL.H.
DEVELOPER:  William D. Rogers, BPA, TEOS, 230-3806, wdrogers@bpa.gov
REQUESTER:  Gordon Young, BPA, TEOB, 230-4430
USERS:      Gordon Young, Paul Ferron, Berhanu Tesema, Angela DeClerck
IPF:        Version 318 or above recommended.
PURPOSE:    Generate new cases given a list of base cases and a list of change
            files.  Check the new cases for over and under voltages, overloads,
            and excessive voltage and loading changes.
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
#define  FF_WILD          '?'
#define  FF_IPF_VERSION   318            /* GPF.EXE_V318 or above recommended */
#define  FF_PAGE_LENGTH    61
#define  FF_PAGE_WIDTH    132
#define  DOT_LINE         "..................................................."
#define  LOG_SPEC         "mimic.log"
#define  OUT_NAME         "mimic"
#define  QUERY_BASE  (int) (1<< 0) /* prompt for basecases */
#define  QUERY_PCHG  (int) (1<< 1) /* prompt for primary changes */
#define  QUERY_ACHG  (int) (1<< 2) /* prompt for auxillary changes */
#define  QUERY_NEWB  (int) (1<< 3) /* prompt for new base case names */
#define  QUERY_REFB  (int) (1<< 4) /* prompt for reference base cases */
#define  QUERY_CORD  (int) (1<< 5) /* prompt for coordinate files */
#define  QUERY_NAME  (int) (1<< 6) /* prompt for output report name */
#define  QUERY_PLOT  (int) (1<< 7) /* prompt for plot options */
#define  QUERY_TXTH  (int) (1<< 8) /* prompt for checking tx thermal rating */
#define  QUERY_ZRAT  (int) (1<< 9) /* prompt for branches with zero ratings */
#define  QUERY_OWNR  (int) (1<<10) /* prompt for owners of interest */
#define  QUERY_ZONE  (int) (1<<11) /* prompt for zones of interest */
#define  QUERY_BSKV  (int) (1<<12) /* prompt for base kV of interest */
#define  QUERY_CHEK  (int) (1<<13) /* prompt for check 1, 2, or 3 */
#define  QUERY_COMP  (int) (1<<14) /* prompt for check/compare 4, 5, or 6 */
#define  QUERY_SAVE  (int) (1<<15) /* prompt for save new basecases */
#define  QUERY_SHOW  (int) (1<<16) /* prompt for include change file in report*/
#define  QUERY_COMM  (int) (1<<17) /* prompt for comment string */
#define  QUERY_VOLT  (int) (1<<18) /* prompt for voltage limits */
#define  QUERY_DVLT  (int) (1<<19) /* prompt for delta voltage limits */
#define  QUERY_LOAD  (int) (1<<20) /* prompt for loading limits */
#define  QUERY_DLDG  (int) (1<<21) /* prompt for delta loading limits */
#define  QUERY_APLY  (int) (1<<22) /* prompt for change application mode */
#define  QUERY_PLTN  (int) (1<<23) /* prompt for names for postscript files */
#define  QUERY_SOLN  (int) (1<<24) /* prompt for /SOLUTION qualifiers */
#define  QUERY_OUTG  (int) (1<<25) /* prompt for /OUTAGE_SIMULATION qualifiers*/
#define  QUERY_FILE  (int) (1<<26) /* prompt for /OUTAGE_SIM control file */
#define  QUERY_DATA  (int) (1<<27) /* prompt for /INCLUDE data files */
#define  QUERY__LIS  (int) (1<<28) /* prompt for whether or not to make .LIS */
#define  READ_INC    (int)      1  /* found /INCLUDE card in CFLOW data file */
#define  READ_BSE    (int)      2  /* found /BASECAS card in CFLOW data file */
#define  READ_PCH    (int)      3  /* found /PRI_CHA card in CFLOW data file */
#define  READ_ACH    (int)      4  /* found /AUX_CHA card in CFLOW data file */
#define  READ_NEW    (int)      5  /* found /NEW_BAS card in CFLOW data file */
#define  READ_REF    (int)      6  /* found /REF_BAS card in CFLOW data file */
#define  READ_COR    (int)      7  /* found /COORDIN card in CFLOW data file */
#define  READ_REP    (int)      8  /* found /REPORT  card in CFLOW data file */
#define  READ_LIM    (int)      9  /* found /LIMITS  card in CFLOW data file */
#define  READ_OWN    (int)     10  /* found /OWNERS  card in CFLOW data file */
#define  READ_ZON    (int)     11  /* found /ZONE    card in CFLOW data file */
#define  READ_BKV    (int)     12  /* found /BASE_KV card in CFLOW data file */
#define  READ_COM    (int)     13  /* found /COMMENT card in CFLOW data file */
#define  READ_TRC    (int)     14  /* found /TRACE   card in CFLOW data file */
#define  READ_PLT    (int)     15  /* found /PLOT_NAME    in CFLOW data file */
#define  READ_SOL    (int)     16  /* found /SOLUTION     in CFLOW data file */
#define  READ_BUS    (int)     17  /* found /BUS     card in CFLOW data file */
#define  READ_BRN    (int)     18  /* found /BRANCH  card in CFLOW data file */
#define  OUTG_NONE   (int)      0
#define  OUTG_BRCH   (int)      1
#define  OUTG_COMO   (int)      2
#define  TRACE_NO    (int)      0
#define  TRACE_YES   (int)      1
#define  LIST_NO     (int)      0
#define  LIST_YES    (int)      1
#define  TYPE_NONE   (int)      0
#define  TYPE_MIMIC  (int)      1
#define  SAVE_NO     (int)      0
#define  SAVE_YES    (int)      1
#define  SHOW_NO     (int)      0
#define  SHOW_YES    (int)      1
#define  NAMESEED    (int)      0
#define  SORT_BY_ALPHA  (int)   1
#define  SORT_BY_BASEKV (int)   2
#define  SORT_BY_OWNER  (int)   3
#define  SORT_BY_ZONE   (int)   4
#define  SORT_BY_INPUT  (int)   5
#define  SORT_BY_SOLN   (int)   6
#define  FAIL_CRIT   (int) (1<< 0) /* critical failure */
#define  FAIL_BASE   (int) (1<< 1)
#define  FAIL_PCHG   (int) (1<< 2)
#define  FAIL_ACHG   (int) (1<< 3)
#define  FAIL_CMMD   (int) (1<< 4)
#define  FAIL_CHNG   (int) (1<< 5)
#define  FAIL_SOLV   (int) (1<< 6)
#define  FAIL_SAVE   (int) (1<< 7)
#define  FAIL_USAN   (int) (1<< 8)
#define  FAIL_REFB   (int) (1<< 9)
#define  FAIL_OUTG   (int) (1<<10)
#define  FAIL_CPSQ   (int) (1<<11)
#define  FAIL_LDRF   (int) (1<<12)
#define  PAGE_BREAK "************************************************************************************************************************************"
#define  PAGE_MARK  "------------------------------------------------------------------------------------------------------------------------------------"
#define  SUM_OVUV   "Check Bus Voltages:  Summary of Voltage Violations"
#define  SUM_EXDV   "Check and Compare Bus Voltages:  Summary of Voltage Violations and Voltage Changes"
#define  SUM_OVLD_XFMR   "Check Branch Loading:  Summary of Overloaded  Transformers"
#define  SUM_EXDL_XFMR   "Check and Compare Branch Loading:  Summary of Overloaded Transformers and Loading Changes"
#define  SUM_OVLD_LINE   "Check Branch Loading:  Summary of Overloaded Lines"
#define  SUM_EXDL_LINE   "Check and Compare Branch Loading:  Summary of Overloaded Lines and Loading Changes"
#define  SUM_XFMR_TAPS   "Check Transformer Taps:  Summary of Tap Positions"
#define  SUM_XFMR_DTAP   "Check Transformer Tap Changes:  Summary of Tap Positions and Tap Changes"
#define  DATA_VOLT   (int)      1
#define  DATA_DVLT   (int)      2
#define  DATA_TAPS   (int)      3
#define  DATA_DTAP   (int)      4
#define  DATA_LINE   (int)      5
#define  DATA_XFMR   (int)      6
#define  DATA_DLIN   (int)      7
#define  DATA_DXFM   (int)      8
#define  GET_BUS_LIS (int) (1<< 0)
#define  GET_BUS_ZON (int) (1<< 1)
#define  GET_BRN_LIS (int) (1<< 2)
#define  GET_BRN_ZON (int) (1<< 3)
#define  GET_BRN_RAT (int) (1<< 4)
#define  CHEK_NONE   (int)      0
#define  CHEK_VOLT   (int) (1<< 0)
#define  CHEK_DVLT   (int) (1<< 1)
#define  CHEK_LOAD   (int) (1<< 2)
#define  CHEK_DLDG   (int) (1<< 3)
#define  CHEK_TAPS   (int) (1<< 4)
#define  CHEK_DTAP   (int) (1<< 5)
#define  CHEK_ZRAT   (int) (1<< 6) /* check for branches with all ratings zero */
#define  CHEK_TXTH   (int) (1<< 7) /* check load against tx thermal rating */
#define  CHEK_OUTG   (int) (1<< 8) /* perform /OUTAGE_SIMULATION */
#define  PLOT_PRNT   (int) (1<< 0) /* print post script plot file */
#define  PLOT_SAVE   (int) (1<< 1) /* save plot as post script file */
#define  PLOT_DIFF   (int) (1<< 2) /* print a difference plot */
#define  NUN_SSS  "0: No changes submitted, just Solve, Screen, and Sort."
#define  E2E_E2E  "1: Apply EACH PRIMARY change to EVERY BASE, then apply EACH AUXILLARY change to EVERY changed BASE."
#define  E2E_N2N  "2: Apply EACH PRIMARY change to EVERY BASE, then apply NEXT AUXILLARY change to NEXT changed BASE."
#define  N2N_E2E  "3: Apply NEXT PRIMARY change to NEXT BASE, then apply EACH AUXILLARY change to EVERY changed BASE."
#define  N2N_N2N  "4: Apply NEXT PRIMARY change to NEXT BASE, then apply NEXT AUXILLARY change to NEXT changed BASE."
#define  B_HASHSIZE 30
#define  HASHSIZE 30
/***************************** end #define ************************************/
/******************************* enum *****************************************/
/***************************** end enum ***************************************/
/******************************* typedef **************************************/
typedef struct {
  float  baseKV;  /* B: base kv */
  float  VmagKV;  /* B: Vmag in kv */
  float  VmagPU;  /* B: Vmag in PU */
  float  VmagPU1; /* T: Vmag in KV at bus1 */
  float  VmagPU2; /* T: Vmag in KV at bus2 */
  float  mva1;    /* T: load in mva at bus 1 */
  float  mva2;    /* T: load in mva at bus 2 */
  float  amps;    /* L, E: highest load in amps at any section/bus */
  float  percent; /* T, L, E: percent of critical rating */
  char   code;    /* T, L, E: rating code: T, N, E, or B */
  float  tap1;    /* T: tap setting in kv at bus 1 */
  float  tap2;    /* T: tap setting in kv at bus 2 */
  float  max_tap; /* T: max tap setting in kv */
  float  min_tap; /* T: min tap setting in kv */
  int    num_taps;/* T: number of taps */
} ff_soln;

typedef struct processStep {
  int  useChng;   /* 0=don't apply primary changes, 1 = do apply */
  int  useAuxc;   /* 0=don't apply auxillary changes, 1 = do apply */
  char oldBase[FILENAME_MAX];
  char newBase[FILENAME_MAX];
  char refBase[FILENAME_MAX];
  char corFile[FILENAME_MAX];
  char ps_File[FILENAME_MAX];
  char pfoFile[FILENAME_MAX];
  char priChng[FILENAME_MAX];
  char auxChng[FILENAME_MAX];
} Step;

typedef struct traceRecord {
  int     base;     /* 1=single, 2=list */
  int     nwbs;     /* 0=none, 1=single, 2=list */
  int     rfbs;     /* 0=none, 1=single, 2=list */
  int     coor;     /* 0=none, 1=single, 2=list */
  int     chng;     /* 0=none, 1=single, 2=list */
  int     auxc;     /* 0=none, 1=single, 2=list */ /* E-E each-to-every */
  Link   *baseList;
  Link   *pchgList;
  Link   *achgList;
  Link   *newbList;
  Link   *refbList;
  Link   *cordList;
  Link   *plotList;
  Link   *ownrList;
  Link   *zoneList;
  Link   *bskvList;
  Link   *trceList;
  Link   *busList;
  Link   *lineList;
  Link   *xfmrList;
  Link   *stepList;
  long    query;
  int     sort;
  int     aply; /* 0(none) 1(pE-E aE-E) 2(pE-E a1-1) 3(p1-1 aE-E) 4(p1-1 a1-1)*/
  int     save; /* 0=don't save the new base cases, 1=do save new base cases */
  int     show; /* 0=don't show change files in report, 1=do show them */
  int     chek; /* indicates type of screening to perform */
  int     plot; /* indicates plot options */
  int     stat;
  int     get;
  char    checks[CF_INBUFSIZE];
  char    comment[CF_INBUFSIZE];
  char    queue[CF_INBUFSIZE];                      /* print queue for plots */
  char    label[CF_INBUFSIZE];          /* describes change application mode */
  int     newNameSeed;
  float   max500;
  float   min500;
  float   maxBus;
  float   minBus;
  float   min500EH;
  float   minBusEH;
  float   DVmax500;
  float   DVmin500;
  float   DVmaxBus;
  float   DVminBus;
  float   DVmin500EH;
  float   DVminBusEH;
  float   DVmaxDelta;
  float   maxLoad;
  float   DLmaxLoad;
  float   DLthrLoad;
  float   DLmaxDelta;
  cf_Out *trc;
  cf_Out *rpt;
  cf_Out *lis;
  char    conSpec[FILENAME_MAX];
  char    outName[FILENAME_MAX];
  char    solution[CF_IPCSIZE];
  char    timeStamp[CF_STRSIZE];
  char    userID[CF_STRSIZE];
  char    IPFversion[CF_STRSIZE];
  char    IPFexecute[CF_STRSIZE];
} Trace;

struct nlist {  /* table entry */
    struct nlist *next;    /* next entry in chain */
    char         *id;      /* R-card: bus1,bus2; B-card: bus1 */
    char          tag1[3]; /* zone */
    int           num1;    /* num_taps */
    float         val1;    /* max_tap or Vpu,new */
    float         val2;    /* min_tap */
};
/******************************* end typedef **********************************/

/* top MIMIC functions - called by main() *************************************/
void  initializeTrace(Trace *trace);
void  processCommandLine(Trace *trace, int argc, char *argv[]);
void  instructions(Trace *trace);
void  checkIPFversion(Trace *trace);
void  promptUser(Trace *trace);
void  checkFileLists(Trace *trace);
void  assembleSteps(Trace *trace);
void  buildTrace(Trace *trace);
void  printTrace(Trace *trace);
void  openReport(cf_Out *rpt);
void  queryContinue(Trace *trace);
void  dispatchEngine(Trace *trace);
void  finalRemarks(Trace *trace);
/* end MIMIC functions - called by main() *************************************/

/* top MIMIC functions - report functions *************************************/
void   ff_printMainReport(cf_Out *rpt, Trace *trace, Step *step);
void   ff_printMainHeader(cf_Out *rpt, Trace *trace, Step *step);
void   ff_printChngRecord(cf_Out *rpt, int show, Step *curStep);
void   ff_printTrceFooter(cf_Out *rpt, Step *curStep);
void   ff_printFailReport(cf_Out *rpt, int stat, Step *step);
void   printList(Trace *trace);
void   printSubReport(cf_Out *rpt, Trace *trace, Link *list, Step *step, int data, int chek);
void   printSubHeader(cf_Out *rpt, Step *step, int data);
int    printContents(char *spec, FILE *fp);
void   printVoltHeader(cf_Out *rpt, Step *step);
void   printXfmrHeader(cf_Out *rpt, Step *step);
void   printLineHeader(cf_Out *rpt, Step *step);
void   printTapsHeader(cf_Out *rpt, Step *step);
void   printDeltaVoltHeader(cf_Out *rpt, Step *step);
void   printDeltaXfmrHeader(cf_Out *rpt, Step *step);
void   printDeltaLineHeader(cf_Out *rpt, Step *step);
void   printDeltaTapsHeader(cf_Out *rpt, Step *step);
void   printSolnData(Link *idLink, FILE *fp, int data);
void   printVoltData(Link *idLink, FILE *fp);
void   printDeltaVoltData(Link *idLink, FILE *fp);
void   printLineData(Link *idLink, FILE *fp);
void   printXfmrData(Link *idLink, FILE *fp);
void   printDeltaLineData(Link *idLink, FILE *fp);
void   printDeltaXfmrData(Link *idLink, FILE *fp);
void   printTapsData(Link *idLink, FILE *fp);
void   printDeltaTapsData(Link *idLink, FILE *fp);
void   ff_printBlankLines(cf_Out *rpt, int n);
/* end MIMIC functions - report functions *************************************/

/* top MIMIC functions - support functions ************************************/
void   buildBusList(Trace *trace);
void   buildBranchList(Trace *trace);
void   getBranchRatings(Trace *trace, Link **brchList);
void   getBranchZones(Trace *trace, Link **brchList);
void   getBusZones(Trace *trace, Link **busList);
void   stuffBusList(Link *curLink, char rec);
void   stuffBranchList(Link *curRow, char rec, int chek);
void   getTaps(Link *xfmrRow);
float  getBusKV(pf_rec *b);
void   getZone(pf_rec *b, char *zone);
void   checkSolution(Trace *trace, Step *step);
void   checkTaps(Trace *trace, Link *xfmrList);
void   checkDeltaTaps(Trace *trace, Link *xfmrList);
void   checkVolt(Trace *trace, Link *idLink, int EHwinter);
int    voltLimit(Trace *trace, ff_soln *newSoln, int EHwinter);
int    deltaVoltLimit(Trace *trace, Link *newLink, int EHwinter);
void   checkLoad(Trace *trace, Link *idLink);
int    loadLimit(Trace *trace, Link *idLink, int chek);
int    deltaLoadLimit(Trace *trace, Link *idLink);
int    getLoading(pf_rec *r, int chk, float *LowLoad, float *Pct1, float *Pct2);
void   checkDeltaVolt(Trace *trace, Link *idLink, int EHwinter);
void   checkDeltaLoad(Trace *trace, Link *idLink);
int    heavy_winter(char *bse_spec);
float  calcRating(pf_rec *n, int tx, char *code);
void   makeNewName(Step *curStep, int *newNameSeed);
void   checkFileList(Link *fileList);
int    findFile(Link *curFile, char *path);
int    srtRow(Link *newRow, Link *xstRow, Trace *trace);
int    srtID(Link *newLink, Link *xstLink, int sort);
int    srtBranch(pf_branch *b1, pf_branch *b2, int sort);
int    srtBus(pf_AC_bus *b1, pf_AC_bus *b2, int sort);
/* end MIMIC functions - support functions ************************************/

/* top LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/
void   ff_stream2List(FILE *readMe, Trace *trace, Link **expList);
void   ff_report(char *s, Trace *trace);
void   ff_limits(char *s, Trace *trace);
void   ff_traces(char *s, Trace *trace);
void   ff_traceName(Trace *trace);
Link  *ff_insRow(Link **table, Link *rowPtr, Link *newRow, Trace *trace);
void   ff_expList(Trace *trace, Link *dataList, Link **expList);
/* end LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/

/* top dispatchEngine functions ***********************************************/
void   cleanUpFiles(Trace *trace);
void   de_branch_list(int *stat, int use, Trace *trace);
void   de_bus_list(int *stat, int use, Trace *trace);
void   de_check_plot_queue(int *stat, int use);
void   de_command(int *stat, char *cmd, char *data);
void   de_comments(int *stat, int use, Step *step);
int    de_common_mode_outage(int go, Trace *trace, Step *step);
void   de_curve(int *stat, Trace *trace, Step *step, pf_rec *r);
void   de_cut_plane(int *stat, Link *cutpList);
void   de_get_bus(int *stat, pf_rec *busID, float *Qshunt);
void   de_load_changes(int *stat, int use, char *file, int fail);
void   de_load_oldbase(int *stat, char *file);
void   de_load_refbase(int *stat, int chek, int plot, char *file);
void   de_modify_bus(int *stat, pf_rec *m, float VXmax);
void   de_outage_sim(int *stat, int use, char *inFile, char *outFile);
void   de_plot(int *stat, int use, char *corFile, char *psFile);
void   de_qatv(int *stat, pf_rec *r, float *QY, float VX);
void   de_qreserve(int *stat, Link *genList);
void   de_save_newbase(int *stat, int use, char *file);
void   de_solution(int *stat, int use, char *solution);
void   de_submit_plot(int *stat, int use, char *file);
void   de_take_brch_outg(int *stat, int type, pf_rec *brnOutg);
void   de_take_como_outg(int *stat, int type, char   *comOutg, FILE *comFile);
void   de_user_analysis(int *stat, char *infile, char *outfile);
void   de_vatq(int *stat, pf_rec *r, float *VY, float QX);
/* end dispatchEngine functions ***********************************************/

/* top CF_UTIL.H candidate functions - common to: MIMIC, LINEFLOW, FINDOUT ****/
/*   all documented, CF_UTIL.H candidates shall be designated, vf_name().     */
/* end CF_UTIL.H candidate functions - common to: MIMIC, LINEFLOW, FINDOUT ****/
unsigned      hash(char *s);
struct nlist *install(char *id, char *tag1, int num1, float val1, float val2);
char         *strrep(char *s);
struct nlist *lookup(char *s);
void hashRcard(pf_rec *r);
struct nlist *hashBcard(pf_rec *b);
static struct nlist *hashtab[HASHSIZE];            /* R-card and B-card table */

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
  assembleSteps(&trace);
  buildTrace(&trace);
  openReport(trace.trc);
  printTrace(&trace);
  queryContinue(&trace);
  openReport(trace.rpt);
  openReport(trace.lis);
  cflow_pf_init();           /* initialize cflow connection to powerflow */
  dispatchEngine(&trace);
  finalRemarks(&trace);
  time(&CF_time1);
  cf_logUse("MIMIC", trace.IPFversion, trace.userID);
  return 0;
}
/* end of main() */
void  initializeTrace(Trace *trace)
{
  memset(trace, '\0', sizeof(trace));
  trace->baseList         =   NULL;
  trace->pchgList         =   NULL;
  trace->achgList         =   NULL;
  trace->newbList         =   NULL;
  trace->refbList         =   NULL;
  trace->cordList         =   NULL;
  trace->plotList         =   NULL;
  trace->ownrList         =   NULL;
  trace->zoneList         =   NULL;
  trace->bskvList         =   NULL;
  trace->trceList         =   NULL;
  trace->busList          =   NULL;
  trace->lineList         =   NULL;
  trace->xfmrList         =   NULL;
  trace->stepList         =   NULL;
  trace->stat             =   0;
  trace->query            = ( QUERY_BASE | QUERY_PCHG | QUERY_ACHG |
                              QUERY_NEWB | QUERY_REFB | QUERY_CORD |
                              QUERY_OWNR | QUERY_ZONE | QUERY_BSKV |
                              QUERY_CHEK | QUERY_COMP | QUERY_SAVE |
                              QUERY_SHOW | QUERY_COMM | QUERY_VOLT |
                              QUERY_DVLT | QUERY_LOAD | QUERY_DLDG |
                              QUERY_APLY | QUERY_PLTN | QUERY_PLOT |
                              QUERY_NAME | QUERY_TXTH | QUERY_ZRAT |
                              QUERY_SOLN | QUERY_OUTG | QUERY_FILE |
                              QUERY_DATA | QUERY__LIS );
  trace->save             =   SAVE_NO;
  trace->show             =   SHOW_NO;
  trace->chek             =   CHEK_NONE;
  trace->aply             =   0;
  trace->sort             =   SORT_BY_ALPHA;
  trace->plot             =   0;
  trace->get              = ( GET_BUS_LIS | GET_BRN_LIS );
  trace->newNameSeed      =   NAMESEED;
  trace->max500     = 1.100;   /* replace with #defines */
  trace->min500     = 1.000;
  trace->maxBus     = 1.052;
  trace->minBus     = 0.950;
  trace->min500EH   = 1.050;
  trace->minBusEH   = 1.000;
  trace->DVmax500   = 1.100;   /* replace with #defines */
  trace->DVmin500   = 1.000;
  trace->DVmaxBus   = 1.052;
  trace->DVminBus   = 0.950;
  trace->DVmin500EH = 1.050;
  trace->DVminBusEH = 1.000;
  trace->DVmaxDelta = 1.000;
  trace->maxLoad    = 100.0;
  trace->DLmaxLoad  = 100.0;
  trace->DLthrLoad  = 95.00;
  trace->DLmaxDelta = 2.000;
  trace->comment[0] = '\0';
  trace->solution[0]= '\0'; 
  strncpy(trace->IPFexecute, getenv("IPFSRV_CF"), CF_STRSIZE);
  cf_parse(trace->IPFexecute, trace->IPFversion, 'F');
  cuserid(trace->userID);
  cf_time(trace->timeStamp, CF_STRSIZE, CF_TIMESTAMP);
  strcpy(CF_logSpec, LOG_SPEC);
  strcpy(trace->outName, OUT_NAME);
  trace->trc = cf_initOut("", trace->timeStamp, TRACE_YES, 0);
  trace->rpt = cf_initOut("", trace->timeStamp, TYPE_MIMIC, 0);
  trace->lis = cf_initOut("", trace->timeStamp, TYPE_NONE, 0);
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
  printf("\n                      Welcome to MIMIC - updated 04-09-97");
  printf("\n");
  printf("\n - Defaults in brackets [].  Press Ctrl-Y & type STOP to abort.");
  printf("\n - Use .trc, .dat, or .lis or append / to designate data files.");
  printf("\n - Warning: scratch.1, user_ana.scr, %s.PFD, and %s.PFO deleted.", trace->userID, trace->userID);
  printf("\n - IPF Executable Used: IPFSRV_CF == \"%s\"", trace->IPFexecute);
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
    printf(" MIMIC was tested for use with IPF version %d.\n", FF_IPF_VERSION);
    printf(" You are currently using IPF version %d.\n", vn);
    printf(  " - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n");
  }
}
void promptUser(Trace *trace)
{
  char  query[CF_INBUFSIZE];
  Link *list;

  char  s[CF_INBUFSIZE];
  int   choice, yes;

  printf("%s", DOT_LINE);
  if (trace->query & QUERY_BASE) {
    printf("\n\n/BASECASE, /TRACE, or /INCLUDE");
    cf_nprompt("\n > Enter list of Basecases (and/or data files) : ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->baseList);
  }
  if (trace->baseList==NULL && trace->stepList==NULL) {
    printf("\n\n *** Warning ***, No basecases have been specified!");
    printf("\n\n/BASECASE, /TRACE, or /INCLUDE");
    cf_nprompt("\n > Enter list of Basecases (and/or data files) : ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->baseList);
  }
  if (trace->baseList==NULL && trace->stepList==NULL) {
    cf_exit(1, "No data or basecase files!  Quitting!\n");
  }
  if (trace->query & QUERY_PCHG) {
    printf("\n\n/PRI_CHANGE");
    cf_nprompt("\n > Enter list of Primary-Change(or data files) : ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->pchgList);
  }
  if (cf_cntLink(trace->pchgList)==0) {
    trace->query &= ~(QUERY_ACHG);
    trace->query &= ~(QUERY_NEWB);
  }
  if (trace->query & QUERY_ACHG) {
    printf("\n\n/AUX_CHANGE");
    cf_nprompt("\n > Enter list of Auxillary-Chng(or data files) : ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->achgList);
  }
  if (trace->query & QUERY_NEWB) {
    printf("\n\n/NEW_BASE");
    cf_nprompt("\n > Enter list of New Base names(or data files) : ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->newbList);
  }
  if (trace->query & QUERY_REFB) {
    printf("\n\n/REF_BASE");
    cf_nprompt("\n > Enter list of Ref base cases(or data files) : ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->refbList);
    if (  trace->refbList==NULL && cf_cntLink(trace->pchgList)!=0 )
        trace->refbList = trace->baseList;
  }
  if (trace->query & QUERY_CORD) {
    printf("\n\n/COORDINATE");
    cf_nprompt("\n > Enter list of Coordinate files or data files: ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->cordList);
  }
  if (trace->query & (QUERY_NAME | QUERY_PLOT)) {
    printf("\n\n/REPORT");
  }
  if (trace->query & QUERY_NAME) {
    cf_sprompt("\n > Enter output report name, NAME = [%s]: ", trace->outName, trace->outName);
  }
  if (cf_cntLink(trace->cordList)==0) {
    trace->query &= ~(QUERY_PLOT);
  }
  if (trace->query & QUERY_PLOT) {
    printf("\n\n Plot Options:  P - print plot file.  S - save plot file.  D - difference plots.");
    cf_sprompt("\n > Enter P, S, and/or D,      PLOT = [%s]: ", "PS", query);
    cf_stoupper(query);
    if (cf_strcon(query, "P")) trace->plot |= PLOT_PRNT;
    if (cf_strcon(query, "S")) trace->plot |= PLOT_SAVE;
    if (cf_strcon(query, "D")) trace->plot |= PLOT_DIFF;
  }
  printf("\n");


/* 0=none, 1=single, 2=list */
  trace->base = cf_cntLink(trace->baseList);
  trace->nwbs = cf_cntLink(trace->newbList);
  trace->rfbs = cf_cntLink(trace->refbList);
  trace->coor = cf_cntLink(trace->cordList);
  trace->chng = cf_cntLink(trace->pchgList);
  trace->auxc = cf_cntLink(trace->achgList);

  if (trace->base > 2) trace->base = 2;
  if (trace->nwbs > 2) trace->nwbs = 2;
  if (trace->rfbs > 2) trace->rfbs = 2;
  if (trace->coor > 2) trace->coor = 2;
  if (trace->chng > 2) trace->chng = 2;
  if (trace->auxc > 2) trace->auxc = 2;

/*  if ( trace->base!=0 && trace->chng==0 && trace->rfbs!=0)  trace->aply = 4;*/
  if ( trace->base!=0 && trace->chng==0)                    trace->aply = 4;
  if ( trace->base==1 && trace->chng==1)                    trace->aply = 1;
  if ( trace->base==1 && trace->chng==2 && trace->auxc!=2 ) trace->aply = 1;
  if ( trace->base==2 && trace->chng==1 && trace->auxc!=2 ) trace->aply = 1;

  if ( trace->aply != 0 ) trace->query &= ~QUERY_APLY;

  if ( trace->query & QUERY_APLY ) {
    printf("\n Choose type of analysis to perform:");
    if ( trace->base==1 && trace->chng==2 && trace->auxc==2 ) {
        printf("\n First, apply each primary change to the base case, then...");
        printf("\n   1.  apply each auxillary change to every 'changed' base case.");
        printf("\n   2.  apply the next auxillary change to the next 'changed' base case.");
    }
    else if ( trace->base==2 && trace->chng==1 && trace->auxc==2 ) {
        printf("\n First, apply the primary change to every base case, then...");
        printf("\n   1.  apply each auxillary change to every 'changed' base case.");
        printf("\n   2.  apply the next auxillary change to the next 'changed' base case.");
    }
    else if ( trace->base==2 && trace->chng==2 && trace->auxc==0 ) {
        printf("\n   1.  Apply each primary change to every base case.");
        printf("\n   3.  Apply the next primary change to the next base case.");
    }
    else if ( trace->base==2 && trace->chng==2 && trace->auxc==1 ) {
        printf("\n First, apply each primary change to every base case, then...");
        printf("\n   1.  apply the auxillary change to every 'changed' base case.");
        printf("\n First, apply the next primary change to the next base case, then...");
        printf("\n   3.  apply the auxillary change to every 'changed' base case.");
    }
    else if ( trace->base==2 && trace->chng==2 && trace->auxc==2 ) {
        printf("\n First, apply each primary change to every base case, then...");
        printf("\n   1.  apply each auxillary change to every 'changed' base case.");
        printf("\n   2.  apply the next auxillary change to the next 'changed' base case.");
        printf("\n First, apply the next primary change to the next base case, then...");
        printf("\n   3.  apply each auxillary change to every 'changed' base case.");
        printf("\n   4.  apply the next auxillary change to the next 'changed' base case.");
    }
    cf_iprompt("\n Choice [%d]:  ", 1, &choice);
    if (choice==1) trace->aply = 1;
    if (choice==2) trace->aply = 2;
    if (choice==3) trace->aply = 3;
    if (choice==4) trace->aply = 4;
  }
  if (trace->aply==0) sprintf(trace->label, "%s", NUN_SSS);
  if (trace->aply==1) sprintf(trace->label, "%s", E2E_E2E);
  if (trace->aply==2) sprintf(trace->label, "%s", E2E_N2N);
  if (trace->aply==3) sprintf(trace->label, "%s", N2N_E2E);
  if (trace->aply==4) sprintf(trace->label, "%s", N2N_N2N);

  printf("\n %s", trace->label);

  if (cf_cntLink(trace->refbList)==0) trace->query &= ~(QUERY_COMP);
  if (trace->query & QUERY_CHEK) {
    printf("\n");
    printf("\n Enter a list of the types of checks that you want performed:");
    printf("\n   0.  No checks - just apply changes - NONE.");
    printf("\n   1.  Check Bus Voltages             - CHECK_VOLTAGE.");
    printf("\n   2.  Check Branch Loading           - CHECK_LOADING.");
    printf("\n   3.  Check Transformer Tap Settings - CHECK_TAPS.   ");
  }
  if (trace->query & QUERY_COMP) {
    printf("\n   4.  Check and Compare Bus Voltages to Reference   - COMPARE_VOLTAGE.");
    printf("\n   5.  Check and Compare Branch Loading to Reference - COMPARE_LOADING.");
    printf("\n   6.  Check Transformer Tap Changes from Reference  - COMPARE_TAPS.");
  }
  if (trace->query & QUERY_OUTG) {
    printf("\n   7.  Perform Outage Simulation (fast outage) - OUTAGE_SIMULATION.");
  }
  if (trace->query & QUERY_CHEK) {
    cf_nprompt("\n > Enter check list, CHECK = [0]: ", "", CF_INBUFSIZE, trace->checks);
    cf_str2list(trace->checks, 2, ' ');
    if (cf_strcon(trace->checks, "1 ")) trace->chek |= CHEK_VOLT;
    if (cf_strcon(trace->checks, "2 ")) trace->chek |= CHEK_LOAD;
    if (cf_strcon(trace->checks, "3 ")) trace->chek |= CHEK_TAPS;
    if (cf_strcon(trace->checks, "4 ")) trace->chek |= CHEK_DVLT;
    if (cf_strcon(trace->checks, "5 ")) trace->chek |= CHEK_DLDG;
    if (cf_strcon(trace->checks, "6 ")) trace->chek |= CHEK_DTAP;
    if (cf_strcon(trace->checks, "7 ")) trace->chek |= CHEK_OUTG;
    printf("\n");
  }
  if ( !(trace->chek & CHEK_VOLT) ) trace->query &= ~(QUERY_VOLT);
  if ( !(trace->chek & CHEK_DVLT) ) trace->query &= ~(QUERY_DVLT);
  if ( !(trace->chek & CHEK_LOAD) ) trace->query &= ~(QUERY_LOAD);
  if ( !(trace->chek & CHEK_DLDG) ) trace->query &= ~(QUERY_DLDG);
  if ( !(trace->chek & (CHEK_LOAD | CHEK_DLDG)) ) trace->query &= ~(QUERY_ZRAT);
  if ( !(trace->chek & (CHEK_LOAD | CHEK_DLDG)) ) trace->query &= ~(QUERY_TXTH);
  if ( !(trace->chek & CHEK_OUTG) ) trace->query &= ~(QUERY_FILE);

  if (trace->query &
        (QUERY_ZRAT|QUERY_TXTH|QUERY_VOLT|QUERY_DVLT|QUERY_LOAD|QUERY_DLDG) ) {
    printf("\n/LIMITS");
  }
  if (trace->query & QUERY_TXTH) {
    printf("\n Transformer loading is checked against bottleneck and emergency ratings.");
    yes = cf_yprompt("\n > Also check thermal? CHECK_TX_THERMAL =  [NO]: ",'N');
    if (yes) trace->chek |= CHEK_TXTH;
  }
  if (trace->query & QUERY_ZRAT) {
    printf("\n Do you want the load reported on branches where ALL ratings are zero?");
    yes = cf_yprompt("\n > Report zero ratings? REPORT_0_RATINGS = [NO]: ",'N');
    if (yes) trace->chek |= CHEK_ZRAT;
    printf("\n");
  }
  if (trace->query & QUERY_VOLT) {
    printf("\n Check Voltage: Enter limits:");
    cf_fprompt("\n > Max 500kV-bus voltage PU, MAX_500_PU=[%5.3f]: ",trace->max500, &trace->max500);
    cf_fprompt("\n > Min 500kV-bus voltage PU, MIN_500_PU=[%5.3f]: ",trace->min500, &trace->min500);
    cf_fprompt("\n > Max sub-500kV voltage PU, MAX_BUS_PU=[%5.3f]: ",trace->maxBus, &trace->maxBus);
    cf_fprompt("\n > Min sub-500kV voltage PU, MIN_BUS_PU=[%5.3f]: ",trace->minBus, &trace->minBus);
    cf_fprompt("\n > Min EH Winter, 500kV-bus, MIN_EH5_PU=[%5.3f]: ",trace->min500EH, &trace->min500EH);
    cf_fprompt("\n > Min EH Winter, sub-500kV, MIN_EHV_PU=[%5.3f]: ",trace->minBusEH, &trace->minBusEH);
    printf("\n");
  }
  if (trace->query & QUERY_DVLT) {
    printf("\n Check Delta Voltage: Enter limits:");
    cf_fprompt("\n > Max 500kV-bus voltage PU, DV_MAX_500_PU=[%5.3f]: ",trace->DVmax500, &trace->DVmax500);
    cf_fprompt("\n > Min 500kV-bus voltage PU, DV_MIN_500_PU=[%5.3f]: ",trace->DVmin500, &trace->DVmin500);
    cf_fprompt("\n > Max sub-500kV voltage PU, DV_MAX_BUS_PU=[%5.3f]: ",trace->DVmaxBus, &trace->DVmaxBus);
    cf_fprompt("\n > Min sub-500kV voltage PU, DV_MIN_BUS_PU=[%5.3f]: ",trace->DVminBus, &trace->DVminBus);
    cf_fprompt("\n > Min EH Winter, 500kV-bus, DV_MIN_EH5_PU=[%5.3f]: ",trace->DVmin500EH, &trace->DVmin500EH);
    cf_fprompt("\n > Min EH Winter, sub-500kV, DV_MIN_EHV_PU=[%5.3f]: ",trace->DVminBusEH, &trace->DVminBusEH);
    cf_fprompt("\n > Max voltage change, pct,  DV_MAX_DELTA =[%5.1f]: ",trace->DVmaxDelta, &trace->DVmaxDelta);
    printf("\n");
  }
  if (trace->query & QUERY_LOAD) {
    printf("\n Check Loading: Enter limits:");
    cf_fprompt("\n > Maximum loading   (%% Rating), MAX_LOAD_PCT=[%5.1f]: ",trace->maxLoad, &trace->maxLoad);
    printf("\n");
  }
  if (trace->query & QUERY_DLDG) {
    printf("\n Check Delta Loading: Enter limits:");
    cf_fprompt("\n > Maximum loading   (%% Rating), DL_MAX_LOAD_PCT=[%5.1f]: ",trace->DLmaxLoad, &trace->DLmaxLoad);
    cf_fprompt("\n > Threshold loading (%% Rating), DL_THR_LOAD_PCT=[%5.1f]: ",trace->DLthrLoad, &trace->DLthrLoad);
    cf_fprompt("\n > Maximum loading change   (%%), DL_MAX_DELTA   =[%5.1f]: ",trace->DLmaxDelta,&trace->DLmaxDelta);
    printf("\n");
  }
  if (trace->query & QUERY_FILE) {
    printf("\n Perform /OUTAGE_SIMULATION: Enter control file:");
    cf_sprompt("\n > Outage sim. control file,   OUTG_CON_FILE = : ", "", trace->conSpec);
    printf("\n");
  }
  if (trace->chek==0 || trace->chek==CHEK_OUTG) {/* no checks or only out_sim */
    trace->query &= ~(QUERY_OWNR | QUERY_ZONE | QUERY_BSKV | QUERY_DATA);
  }
  if (trace->query & QUERY_DATA) {
    printf("\n\n/INCLUDE");
    cf_nprompt("\n > Enter list of files with branch or bus lists: ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
    printf("\n");
  }
  if (trace->query & (QUERY_OWNR | QUERY_ZONE | QUERY_BSKV)) {
    printf("\n/OWNER, /ZONE, /BASE_KV");
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
  if (trace->query & (QUERY_OWNR | QUERY_ZONE | QUERY_BSKV)) {
    printf("\n");
  }
  if ( trace->chng==0 ) trace->query &= ~(QUERY_SAVE | QUERY_SHOW);
  if (trace->query & (QUERY_SAVE | QUERY_SHOW)) {
    printf("\n/REPORT");
  }
  if (trace->query & QUERY_SAVE) {
    trace->save=cf_yprompt("\n > SAVE_NEW_BASECASES with changes? [%c]: ", 'Y');
  }
  if (trace->query & QUERY_SHOW) {
    trace->show=cf_yprompt("\n > SHOW_CHANGES files in report?    [%c]: ", 'Y');
  }
  if (trace->query & (QUERY_SAVE | QUERY_SHOW)) {
    printf("\n");
  }
  if (trace->query & QUERY_COMM) {
    printf("\n/COMMENT");
    cf_nprompt("\n > Enter an 80-character comment string: ","",80,trace->comment);
  }
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

  cf_printList(fp, trace->baseList, CF_oneStyl, "/BASECASE\n");
  cf_printList(fp, trace->pchgList, CF_oneStyl, "/PRI_CHANGE\n");
  cf_printList(fp, trace->achgList, CF_oneStyl, "/AUX_CHANGE\n");
  cf_printList(fp, trace->newbList, CF_oneStyl, "/NEW_BASECASE\n");
  cf_printList(fp, trace->refbList, CF_oneStyl, "/REF_BASECASE\n");
  cf_printList(fp, trace->cordList, CF_oneStyl, "/COORDINATE\n");
  cf_printList(fp, trace->plotList, CF_oneStyl, "/PLOT_NAME\n");

  cf_printList(fp, trace->ownrList, CF_dznStyl, "/OWNER\n");
  cf_printList(fp, trace->zoneList, CF_dznStyl, "/ZONE\n");
  cf_printList(fp, trace->bskvList, CF_sixStyl, "/BASE_KV\n");

  fprintf(fp, "/COMMENT\n");
  if ( trace->comment[0]!='\0' )  fprintf(fp, "  %-.80s\n", trace->comment);

  fprintf(fp, "/REPORT\n");
  fprintf(fp, "  NAME = %s\n", trace->outName);
  if (trace->plot & PLOT_PRNT         ) fprintf(fp,"  PLOT = PRINT\n");
  if (trace->plot & PLOT_SAVE         ) fprintf(fp,"  PLOT = SAVE\n");
  if (trace->plot & PLOT_DIFF         ) fprintf(fp,"  PLOT = DIFFERENCE\n");
  if (trace->plot     ==0             ) fprintf(fp,"  PLOT = NONE\n");
  fprintf(fp, "  APPLY = %d\n", trace->aply);
  if (trace->chek & CHEK_VOLT         ) fprintf(fp,"  CHECK = CHECK_VOLTAGE\n");
  if (trace->chek & CHEK_LOAD         ) fprintf(fp,"  CHECK = CHECK_LOADING\n");
  if (trace->chek & CHEK_TAPS         ) fprintf(fp,"  CHECK = CHECK_TAPS\n");
  if (trace->chek & CHEK_DVLT         ) fprintf(fp,"  CHECK = COMPARE_VOLTAGE\n");
  if (trace->chek & CHEK_DLDG         ) fprintf(fp,"  CHECK = COMPARE_LOADING\n");
  if (trace->chek & CHEK_DTAP         ) fprintf(fp,"  CHECK = COMPARE_TAPS\n");
  if (trace->chek & CHEK_OUTG         ) fprintf(fp,"  CHECK = OUTAGE_SIMULATION\n");
  if (trace->chek     ==0             ) fprintf(fp,"  CHECK = NONE\n");
  if (trace->lis->type==LIST_YES      ) fprintf(fp,"  LIST = YES\n");
  if (trace->lis->type==LIST_NO       ) fprintf(fp,"  LIST = NO\n");
  if (trace->trc->type==TRACE_YES     ) fprintf(fp,"  TRACE = YES\n");
  if (trace->trc->type==TRACE_NO      ) fprintf(fp,"  TRACE = NO\n");
  if (trace->save     ==SAVE_YES      ) fprintf(fp,"  SAVE_NEW_BASECASES = YES\n");
  if (trace->save     ==SAVE_NO       ) fprintf(fp,"  SAVE_NEW_BASECASES = NO\n");
  if (trace->show     ==SHOW_YES      ) fprintf(fp,"  SHOW_CHANGES = YES\n");
  if (trace->show     ==SHOW_NO       ) fprintf(fp,"  SHOW_CHANGES = NO\n");

  fprintf(fp, "/LIMITS\n");
  if ( trace->chek & (CHEK_LOAD | CHEK_DLDG) ) {
    if (  (trace->chek & CHEK_TXTH)   ) fprintf(fp,"  CHECK_TX_THERMAL = YES\n");
    if ( !(trace->chek & CHEK_TXTH)   ) fprintf(fp,"  CHECK_TX_THERMAL = NO\n");
    if (  (trace->chek & CHEK_ZRAT)   ) fprintf(fp,"  REPORT_0_RATINGS = YES\n");
    if ( !(trace->chek & CHEK_ZRAT)   ) fprintf(fp,"  REPORT_0_RATINGS = NO\n");
  }
  if ( trace->chek & CHEK_VOLT ) {
    fprintf(fp, "  MAX_500_PU = %5.3f\n", trace->max500);
    fprintf(fp, "  MIN_500_PU = %5.3f\n", trace->min500);
    fprintf(fp, "  MAX_BUS_PU = %5.3f\n", trace->maxBus);
    fprintf(fp, "  MIN_BUS_PU = %5.3f\n", trace->minBus);
    fprintf(fp, "  MIN_EH5_PU = %5.3f\n", trace->min500EH);
    fprintf(fp, "  MIN_EHV_PU = %5.3f\n", trace->minBusEH);
  }
  if ( trace->chek & CHEK_DVLT ) {
    fprintf(fp, "  DV_MAX_500_PU = %5.3f\n", trace->DVmax500);
    fprintf(fp, "  DV_MIN_500_PU = %5.3f\n", trace->DVmin500);
    fprintf(fp, "  DV_MAX_BUS_PU = %5.3f\n", trace->DVmaxBus);
    fprintf(fp, "  DV_MIN_BUS_PU = %5.3f\n", trace->DVminBus);
    fprintf(fp, "  DV_MIN_EH5_PU = %5.3f\n", trace->DVmin500EH);
    fprintf(fp, "  DV_MIN_EHV_PU = %5.3f\n", trace->DVminBusEH);
    fprintf(fp, "  DV_MAX_DELTA  = %5.1f\n", trace->DVmaxDelta);
  }
  if ( trace->chek & CHEK_LOAD ) {
    fprintf(fp, "  MAX_LOAD_PCT = %5.1f\n", trace->maxLoad);
  }
  if ( trace->chek & CHEK_DLDG ) {
    fprintf(fp, "  DL_MAX_LOAD_PCT = %5.1f\n", trace->DLmaxLoad);
    fprintf(fp, "  DL_THR_LOAD_PCT = %5.1f\n", trace->DLthrLoad);
    fprintf(fp, "  DL_MAX_DELTA    = %5.1f\n", trace->DLmaxDelta);
  }
  if ( trace->chek & CHEK_OUTG ) {
    fprintf(fp, "  OUTG_CON_FILE = %s\n", trace->conSpec);
  }
  if ( trace->solution[0]!=0 ) {
    fprintf(fp, "%s", trace->solution);
  }
  cf_printList(fp, trace->trceList, CF_dotStyl, "./TRACE\n");
  cf_printList(fp, trace->lineList, CF_lisStyl, "/BRANCHES\n.ycown< BUS1 ><V1> < BUS2 ><V2>cs\n");
  cf_printList(fp, trace->xfmrList, CF_lisStyl, "/BRANCHES\n.ycown< BUS1 ><V1> < BUS2 ><V2>cs\n");
  cf_printList(fp, trace->busList,  CF_lisStyl, "/BUS\n.ycown< BUS  ><KV>zn\n");
}
void  checkFileLists(Trace *trace)
{
  checkFileList(trace->baseList);
  checkFileList(trace->pchgList);
  checkFileList(trace->achgList);
  checkFileList(trace->refbList);
  checkFileList(trace->cordList);
}
void  assembleSteps(Trace *trace)
{
  Link *stepLink, *curPlot;
  Link *curChng, *curBase, *curAuxc, *curNwbs, *curRfbs, *curCoor, *bsePtr;
  Step *step;

/* test mimic function : Loop through all cases in base case list */
  if (trace->stepList != NULL) return;
  stepLink= trace->stepList;
  curBase = trace->baseList;
  curChng = trace->pchgList;
  curAuxc = trace->achgList;
  curNwbs = trace->newbList;
  curRfbs = trace->refbList;
  curCoor = trace->cordList;
  curPlot = trace->plotList;
  bsePtr = curBase;
  while (curBase != NULL) {
    stepLink = cf_addLink(stepLink, sizeof(Step));
    step = (Step *) stepLink->data;
    if (trace->stepList==NULL) trace->stepList = stepLink;
    if (curChng!=NULL) {
        if ( trace->aply!=0 && trace->chng!=0 ) {
            step->useChng = 1;
            sprintf(step->priChng, "%s", (char *) curChng->data);
        }
    }
    if (curAuxc!=NULL) {
        if ( trace->aply!=0 && trace->auxc!=0 ) {
            step->useAuxc = 1;
            sprintf(step->auxChng, "%s", (char *) curAuxc->data);
        }
    }
    sprintf(step->oldBase, "%s", (char *) curBase->data);
    if (step->useChng || step->useAuxc) {
        if (curNwbs==NULL || strlen((char *) curNwbs->data)==0) {
            makeNewName(step, &trace->newNameSeed);
        }
        else {
            sprintf(step->newBase, "%s", (char *) curNwbs->data);
        }
        if (curNwbs!=NULL) curNwbs = curNwbs->next;
    }
    if (curRfbs!=NULL) sprintf(step->refBase, "%s", (char *) curRfbs->data);
    if (trace->coor==1 || trace->coor==2) {
        sprintf(step->corFile, "%s", (char *) curCoor->data);
        strcpy(step->ps_File, "mimic.ps");        /* default plot filename */
    }
    if (trace->plot & PLOT_SAVE) {        /* overwrite default plot filename */
        if (strlen(step->ps_File)==0) {
            if (strlen(step->newBase)!=0)
                cf_strsuf(step->ps_File, (char *) step->newBase, '.',".PS");
            else
                cf_strsuf(step->ps_File, (char *) step->oldBase, '.',".PS");
        }
        else if (curPlot!=NULL) {
            cf_strsuf(step->ps_File, (char *) curPlot->data, '.',".PS");
        }
    }
    if (strlen(step->newBase)!=0) {
        cf_strsuf(step->pfoFile, (char *) step->newBase, '.',".PFO");
    }
    else {
        cf_strsuf(step->pfoFile, (char *) step->oldBase, '.',".PFO");
    }
    if (trace->aply==1) { /* p: e2e a: e2e */
        if (curAuxc!=NULL) curAuxc = curAuxc->next;
        if (curAuxc==NULL && curChng!=NULL) curChng = curChng->next;
        if (curChng==NULL) curBase = curBase->next;
        if (curChng==NULL) curChng = trace->pchgList;
        if (curAuxc==NULL) curAuxc = trace->achgList;
    }
    if (trace->aply==2) { /* p: e2e a: o2o */
        if (curChng!=NULL) curChng = curChng->next;
        if (curAuxc!=NULL) curAuxc = curAuxc->next;
        if (curChng==NULL) curBase = curBase->next;
        if (curChng==NULL) curChng = trace->pchgList;
        if (curAuxc==NULL) curAuxc = trace->achgList;
    }
    if (trace->aply==3) { /* p: o2o a: e2e */
        if (curAuxc!=NULL) curAuxc = curAuxc->next;
        if (curAuxc==NULL && curChng!=NULL) curChng = curChng->next;
        if (curAuxc==NULL) curBase = curBase->next;
        if (curChng==NULL) curChng = trace->pchgList;
        if (curAuxc==NULL) curAuxc = trace->achgList;
    }
    if (trace->aply==4) { /* p: o2o a: o2o */
        if (curChng!=NULL) curChng = curChng->next;
        if (curAuxc!=NULL) curAuxc = curAuxc->next;
        curBase = curBase->next;
        if (curChng==NULL) curChng = trace->pchgList;
        if (curAuxc==NULL) curAuxc = trace->achgList;
    }
    if (trace->aply==0) { /* no changes */
        curBase = curBase->next;
    }
    if (bsePtr!=curBase) { /* for ref bases and cor files */
        if (curRfbs!=NULL) curRfbs = curRfbs->next;
        if (curCoor!=NULL) curCoor = curCoor->next;
        if (curPlot!=NULL) curPlot = curPlot->next;
        if (curRfbs==NULL) curRfbs = trace->refbList;
        if (curCoor==NULL) curCoor = trace->cordList;
        if (curPlot==NULL) curPlot = trace->plotList;
        bsePtr = curBase;
    }
  }
}
void queryContinue(Trace *trace)
{
  int  yes;

  if (trace->trc->type==TRACE_YES)
    printf("\nTrace  report written to %s", trace->trc->spec);
  if (CF_logFile != NULL)
    printf("\nError Summary report written to %s", CF_logSpec);
  printf("\n%s\n", DOT_LINE);
  cf_printList(stdout, trace->trceList, CF_oneStyl, "/TRACE\n");
  printf("%s\n", DOT_LINE);
  printf(" Please examine the above /TRACE of MIMIC operations.");
  yes = cf_yprompt("\n > Do you want to continue this run? [%c]: ", 'Y');
  cf_exit(!yes, "Quitting!");
}
void  dispatchEngine(Trace *trace)
{
  Step *step;
  Link *stepLink;
  int   loop = 1;

/**************** BEGINNING OF POWERFLOW DISPATCH ENGINE **********************/

  printf("Processing... \n");
  for (stepLink = trace->stepList; stepLink != NULL; stepLink = stepLink->next){
    step = (Step *) stepLink->data;
    trace->stat = 0;

    cleanUpFiles(trace);

    pf_init();

    de_command(&trace->stat, "/trace, change=on", "");
    de_load_oldbase(&trace->stat, step->oldBase);
    de_comments(&trace->stat, (step->useChng || step->useAuxc), step);
    de_load_changes(&trace->stat, step->useChng, step->priChng, FAIL_PCHG);
    de_load_changes(&trace->stat, step->useAuxc, step->auxChng, FAIL_ACHG);
    de_solution(&trace->stat, (step->useChng || step->useAuxc),trace->solution);
    de_save_newbase(&trace->stat, trace->save, step->newBase);
    de_load_refbase(&trace->stat, trace->chek, trace->plot, step->refBase);
    de_plot(&trace->stat, trace->coor, step->corFile, step->ps_File);
    de_check_plot_queue(&trace->stat, (trace->plot & PLOT_PRNT));
    de_submit_plot(&trace->stat, (trace->plot & PLOT_PRNT), step->ps_File);
    de_bus_list(&trace->stat, trace->chek, trace);
    de_branch_list(&trace->stat, trace->chek, trace);

    checkSolution(trace, step);
    ff_printMainReport(trace->rpt, trace, step);

    de_outage_sim(&trace->stat, trace->chek, trace->conSpec, step->pfoFile);

    if (loop++ == 1) printList(trace);/* only print list after first basecase */

    if (trace->get & GET_BUS_LIS) cf_freeRowList(&trace->busList);
    if (trace->get & GET_BRN_LIS) cf_freeRowList(&trace->lineList);
    if (trace->get & GET_BRN_LIS) cf_freeRowList(&trace->xfmrList);
  }
  return;
/**************** END OF POWERFLOW DISPATCH ENGINE ****************************/
}
void printList(Trace *trace)
{
  FILE *fp;

  if (trace->lis->type==LIST_NO) return;
  fp = trace->lis->file;
  if (fp == NULL) fp = stderr;
  fprintf(fp, ". %s %s %s %s\n", trace->lis->spec, trace->lis->time, trace->userID,
    trace->IPFversion);

  cf_printList(fp, trace->lineList, CF_lisStyl, "/BRANCHES\n.ycown< BUS1 ><V1> < BUS2 ><V2>cs\n");
  cf_printList(fp, trace->xfmrList, CF_lisStyl, "/BRANCHES\n.ycown< BUS1 ><V1> < BUS2 ><V2>cs\n");
  cf_printList(fp, trace->busList,  CF_lisStyl, "/BUS\n.ycown< BUS  ><KV>zn\n");
}
void  finalRemarks(Trace *trace)
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
void checkSolution(Trace *trace, Step *step)
{ /* put data from current solved base in curBus->new and curBranch->new */
  /* put data from loaded reference base in curBus->old and curBranch->old */
  int EHwinter;
  EHwinter = heavy_winter(step->newBase);

  if (trace->stat & FAIL_CRIT) return;
  if (!(trace->chek &
    (CHEK_VOLT|CHEK_DVLT|CHEK_LOAD|CHEK_DLDG|CHEK_TAPS|CHEK_DTAP)) ) return;

  printf("  Checking Solution...\n");
  if (trace->chek & (CHEK_VOLT | CHEK_DVLT)) { /* reset status to 0 */
    stuffBusList(trace->busList, 'N'); /* 'N'=curBus->new, 'O'=curBus->old */
  }
  if (trace->chek & (CHEK_LOAD | CHEK_DLDG)) { /* reset status to 0 */
    stuffBranchList(trace->lineList, 'N', trace->chek);
  }
  if (trace->chek & (CHEK_LOAD | CHEK_DLDG | CHEK_TAPS | CHEK_DTAP)) {
    stuffBranchList(trace->xfmrList, 'N', trace->chek);  /* reset status to 0 */
  }
  if (trace->chek & (CHEK_TAPS | CHEK_DTAP)) {
    getTaps(trace->xfmrList);
  }
  if (trace->chek & CHEK_VOLT) checkVolt(trace, trace->busList, EHwinter);
  if (trace->chek & CHEK_LOAD) checkLoad(trace, trace->lineList);
  if (trace->chek & CHEK_LOAD) checkLoad(trace, trace->xfmrList);
  if (trace->chek & CHEK_TAPS) checkTaps(trace, trace->xfmrList);
  if (trace->chek & (CHEK_DVLT | CHEK_DLDG | CHEK_DTAP)) {
    if (pf_select_base('R')!=0) {
        cf_logErr("checkSolution:  Cannot switch to reference base.\n"); return;
    }
  }
  if (trace->chek & CHEK_DVLT) {
    stuffBusList(trace->busList, 'R');                     /*'N'=new, 'R'=ref */
    checkDeltaVolt(trace, trace->busList, EHwinter);
  }
  if (trace->chek & CHEK_DLDG) {
    stuffBranchList(trace->lineList, 'R', trace->chek);
    checkDeltaLoad(trace, trace->lineList);
  }
  if (trace->chek & (CHEK_DLDG | CHEK_DTAP)) {
    stuffBranchList(trace->xfmrList, 'R', trace->chek);
    checkDeltaLoad(trace, trace->xfmrList);
  }
  if (trace->chek & (CHEK_DTAP)) {
    checkDeltaTaps(trace, trace->xfmrList);
  }
  if (pf_select_base('O')!=0) {
    cf_logErr(" checkSolution: Can't switch to old base.\n");
  }
}
void buildBusList(Trace *trace)
{ /* get buses in zones, owners, kv, but not BD or BM (dc) buses */
  int     e, nRec = 0;
  Link   *idLink, *curRow, *newRow;
  cf_Bus *idData;
  pf_rec  r;

  trace->busList = NULL;
  curRow = NULL;
  for (e = pf_rec_bus(&r, "F"); e == 0; e = pf_rec_bus(&r, "N")) {
    if (hashBcard(&r)==NULL) cf_logErr("hashBcard(&r) failed\n");
    if (!cf_validOwner(trace->ownrList, r.i.ACbus.owner)) continue;
    if (!cf_validZone(trace->zoneList, r.i.ACbus.zone, r.i.ACbus.zone)) continue;
    if (!cf_validBasekv(trace->bskvList, r.i.ACbus.kv, r.i.ACbus.kv)) continue;
    if ( r.i.ACbus.type[1]=='D' || r.i.ACbus.type[1]=='M' ) continue;
    idLink = cf_newLink(sizeof(cf_Bus));
    if (idLink==NULL || idLink->data==NULL) continue;
    idLink->kind = CF_KIND_BUS;
    cf_rec2bus(&r, (cf_Bus *) idLink->data);
    newRow = cf_link2row(idLink);
    curRow = ff_insRow(&trace->busList, curRow, newRow, trace);
    nRec++;
  }
  cf_logErr("Total of %d buses.\n", nRec);
  return;
}
void buildBranchList(Trace *trace)
{
  int        e, nRec = 0;
  Link      *idLink, *curLine, *curXfmr, *newRow;
  cf_Branch *idData;
  char       type;
  pf_rec     r;

  trace->lineList = NULL; curLine = NULL;
  trace->xfmrList = NULL; curXfmr = NULL;

  for (e=pf_rec_branch(&r, "F"); e==OK; e=pf_rec_branch(&r, "N")) {
    type = r.i.branch.type[0];
    if ( type == 'R' ) hashRcard(&r);
    if ( strchr("LET", type) == NULL ) continue;
    if (!cf_validOwner(trace->ownrList, r.i.branch.owner)) continue;
    if (!cf_validBasekv(trace->bskvList,r.i.branch.bus1_kv,r.i.branch.bus2_kv))
        continue;
    idLink = cf_newLink(sizeof(cf_Branch));
    if (idLink==NULL || idLink->data==NULL) continue;
    idLink->kind = CF_KIND_BRN;
    cf_rec_l2h(&r.i.branch);
    cf_rec2branch(&r, (cf_Branch *) idLink->data);
    newRow = cf_link2row(idLink);
    if (type=='T') curXfmr= ff_insRow(&trace->xfmrList, curXfmr, newRow, trace);
    if (type=='L') curLine= ff_insRow(&trace->lineList, curLine, newRow, trace);
    if (type=='E') curLine= ff_insRow(&trace->lineList, curLine, newRow, trace);
    nRec++;
  }
  cf_logErr("Total of %d branches.\n", nRec);
  return;
}
void hashRcard(pf_rec *r)
{
  char net[256], id[256];
  cf_rec_l2h(&r->i.branch);
  pf_rec_b2a(net, r, "I");
  sprintf(id, "R     %12.12s %12.12s", &net[6], &net[19]);
  if (install(id, "\0", r->i.R.num_taps, r->i.R.max_tap, r->i.R.min_tap)==NULL)
    cf_logErr("install(id, 0, num_taps, max_tap, min_tap) failed\n");
/*  printf("install:%s\n", id); */
}
struct nlist *hashBcard(pf_rec *b)
{
  char net[256], id[256];
  struct nlist *b_card;
  float VmagPU;

  b_card = NULL;
  pf_rec_b2a(net, b, "I"); sprintf(id, "B     %12.12s", &net[6]);
  VmagPU = Cf_ratio(b->s.ACbus.Vmag, b->i.ACbus.kv);

  b_card = lookup(id);
  if (b_card==NULL) b_card = install(id, b->i.ACbus.zone, 0, VmagPU,       0);
  if (b_card!=NULL) {
    if (b_card->val1==0) b_card = install(id, b_card->tag1, 0, VmagPU,       0);
    if (b_card->val1!=0) b_card = install(id, b_card->tag1, 0, b_card->val1, 0);
  }
/*
  printf("install:%s", id);
  if (b_card!=NULL)
    printf("Vnew = %5.1f, Vref = %5.1f", b_card->val1, b_card->val2);
  printf("\r");
*/
  return b_card;
}
void getBranchZones(Trace *trace, Link **brchList)
{
  Link      *curRow, *idLink;
  cf_Branch *idData;
  pf_rec     b;

  curRow = *brchList;
  while (curRow != NULL) {
    idLink = (Link *) curRow->data;
    idData = (cf_Branch *) idLink->data;

    pf_init_bus(&b, "B*", idData->bus1_name, idData->bus1_kv);
    getZone(&b, idData->bus1_zone);

    pf_init_bus(&b, "B*", idData->bus2_name, idData->bus2_kv);
    getZone(&b, idData->bus2_zone);

    if ( !cf_validZone(trace->zoneList, idData->bus1_zone, idData->bus2_zone) )
        curRow = cf_delRow(brchList, curRow);
    else
        curRow = curRow->next;
  }
}
void getBranchRatings(Trace *trace, Link **brchList)
{
  Link      *curRow, *idLink;
  cf_Branch *idData;
  pf_rec     b;


  curRow = *brchList;
  while (curRow != NULL) {
    idLink = (Link *) curRow->data;
    idData = (cf_Branch *) idLink->data;
    cf_branch2rec(idData, &b);
    pf_rec_branch(&b, "G");
    cf_rec2branch(&b, idData);
    idData->thermal = b.i.branch.thermal_rating;
    curRow = curRow->next;
  }
}
void getBusZones(Trace *trace, Link **busList)
{
  Link   *curRow, *idLink;
  cf_Bus *idData;
  pf_rec  b;

  curRow = *busList;
  while (curRow != NULL) {
    idLink = (Link *) curRow->data;
    idData = (cf_Bus *) idLink->data;

    if (idData->zone[0]=='\0' || idData->zone[0]==' ') {/* get only if needed */
        pf_init_bus(&b, "B*", idData->name, idData->kv);
        getZone(&b, idData->zone);
    }
    if ( !cf_validZone(trace->zoneList, idData->zone, idData->zone) )
        curRow = cf_delRow(busList, curRow);
    else
        curRow = curRow->next;
  }
}
void getZone(pf_rec *b, char *zone)
{
  char net[256], id[32];
  struct nlist *b_card = NULL;

  pf_rec_b2a(net, b, "I"); sprintf(id, "B     %12.12s", &net[6]);
  b_card = lookup(id);
  if (b_card==NULL || (b_card!=NULL && strlen(b_card->tag1)==0)) {
    if (pf_rec_bus(b, "G")==OK) b_card = hashBcard(b);
  }
  if (b_card!=NULL) strncpy(zone, b_card->tag1, 3);
}
void stuffBranchList(Link *curRow, char rec, int chek)
{ /* get the solution data for the branches */
  int        n;
  Link      *idLink, *newLink;
  char       type, code1, code2;
  float      P, Q, adjBTL1, adjBTL2, rating1, rating2, percent1, percent2;
  cf_Branch *idData;
  ff_soln   *newSoln;
  pf_rec     b, r;

  for (n=0; curRow != NULL; curRow = curRow->next) {
    idLink = (Link *) curRow->data;
    idData = (cf_Branch *) idLink->data;
    cf_branch2rec(idData, &b);
    if ( pf_rec_branch(&b, "O") != OK ) continue;
    if (toupper(rec)=='N') {
        curRow->size = 0;               /* use size as the 'status' indicator */
        newLink = cf_addLink(idLink, sizeof(ff_soln));
    }
    if (toupper(rec)=='R') {
        newLink = cf_addLink(idLink->next, sizeof(ff_soln));
    }
    newSoln = (ff_soln *) newLink->data;
    if (newLink==NULL || newSoln==NULL) continue;
    type = b.i.branch.type[0];
    if (type=='L' || type=='E') {
        newSoln->amps = b.s.branch.crit_line_load_amps;               /* amps */
        rating1 = calcRating(&b, (chek & CHEK_TXTH), &newSoln->code);
        newSoln->percent = 100 * Cf_ratio(newSoln->amps, rating1);
    }
    else if (type=='T') {                                              /* mva */
        P = b.s.branch.Pin;  Q = b.s.branch.Qin;  newSoln->mva1 = sqrt(P*P+Q*Q);
        P = b.s.branch.Pout; Q = b.s.branch.Qout; newSoln->mva2 = sqrt(P*P+Q*Q);
        newSoln->tap1 = b.s.branch.tap1;
        newSoln->tap2 = b.s.branch.tap2;
    }
    if ( toupper(rec)=='N' && (chek & (CHEK_LOAD | CHEK_DLDG)) ) {
        if (type=='T') {   /* use adjusted bottleneck rating for calcRating() */
            pf_init_bus(&r, "B ", b.i.branch.bus1_name, b.i.branch.bus1_kv);
            newSoln->VmagPU1 = getBusKV(&r);
            pf_init_bus(&r, "B ", b.i.branch.bus2_name, b.i.branch.bus2_kv);
            newSoln->VmagPU2 = getBusKV(&r);

            adjBTL1 = idData->bottleneck * newSoln->VmagPU1;
            b.i.branch.bottleneck_rating = adjBTL1;
            rating1 = calcRating(&b, (chek & CHEK_TXTH), &code1);
            percent1 = 100 * Cf_ratio(newSoln->mva1, rating1);

            adjBTL2 = idData->bottleneck * newSoln->VmagPU2;
            b.i.branch.bottleneck_rating = adjBTL2;
            rating2 = calcRating(&b, (chek & CHEK_TXTH), &code2);
            percent2 = 100 * Cf_ratio(newSoln->mva2, rating2);

            newSoln->percent = (percent1 >= percent2) ? percent1 : percent2;
            newSoln->code    = (percent1 >= percent2) ? code1    : code2;
        }
    }
    n++;
  }
  cf_logErr("stuffBranchList(%c): %d branch solutions retrieved.\n", rec, n);
}
float getBusKV(pf_rec *b)
{
  char net[256], id[32];
  struct nlist *b_card = NULL;
  float  VmagPU = 0;

  pf_rec_b2a(net, b, "I"); sprintf(id, "B     %12.12s", &net[6]);
  b_card = lookup(id);
  if (b_card==NULL || (b_card!=NULL && b_card->val1==0)) {
    if (pf_rec_bus(b, "O")==OK) b_card = hashBcard(b);
  }
  if (b_card!=NULL) VmagPU = b_card->val1;
  return VmagPU;
}
void getTaps(Link *xfmrRow)
{
  int        n;
  Link      *idLink;
  cf_Branch *idData;
  ff_soln   *newSoln;
  pf_rec     r;
  char       net[256], id[256];
  struct nlist *r_card;

  for (n=0; xfmrRow != NULL; xfmrRow = xfmrRow->next) {
    idLink = (Link *) xfmrRow->data;
    idData = (cf_Branch *) idLink->data;
    newSoln = (ff_soln *) idLink->next->data;
    cf_branch2rec(idData, &r);
    cf_rec_l2h(&r.i.branch);
    pf_rec_b2a(net, &r, "I");
    sprintf(id, "R     %12.12s %12.12s", &net[6], &net[19]);
/*    printf("lookup :%s\n", id); */
    r_card = lookup(id);
    if (r_card==NULL) continue;
    newSoln->max_tap = r_card->val1;
    newSoln->min_tap = r_card->val2;
    newSoln->num_taps = r_card->num1;
    n++;
  }
  cf_logErr("getTaps: %d tap limits retrieved.\n", n);
}
void  stuffBusList(Link *curRow, char rec)
{ /* get the solution data for the busses */
  int n=1;
  Link    *idLink, *newLink;
  ff_soln          *newSoln;
  pf_rec r;

  for (; curRow != NULL; curRow = curRow->next) {
    idLink = (Link *) curRow->data;
    cf_bus2rec((cf_Bus *) idLink->data, &r);
    if ( pf_rec_bus(&r, "O") != OK) continue;
    if (toupper(rec)=='N' && idLink!=NULL) {
        if (hashBcard(&r)==NULL) cf_logErr("hashBcard(&r) failed\n");
        curRow->size = 0;        /* reset status (size) here, but not on ref */
        newLink = cf_addLink(idLink, sizeof(ff_soln));
    }
    if (toupper(rec)=='R') {
        newLink = cf_addLink(idLink->next, sizeof(ff_soln));
    }
    newSoln = (ff_soln *) newLink->data;
    if (newLink==NULL || newSoln==NULL) continue;
    newSoln->VmagKV = r.s.ACbus.Vmag;
    newSoln->baseKV = r.i.ACbus.kv;
    newSoln->VmagPU = Cf_ratio(r.s.ACbus.Vmag, r.i.ACbus.kv);
    n++;
  }
  cf_logErr("stuffBusList(%c): %d bus solutions retrieved.\n", rec, n);
}
void checkTaps(Trace *trace, Link *xfmrList)
{
  Link      *idLink, *xfmrRow;
  cf_Branch *idData;
  ff_soln   *newSoln;
  for (xfmrRow=xfmrList; xfmrRow != NULL; xfmrRow = xfmrRow->next) {
    idLink = (Link *) xfmrRow->data;
    idData = (cf_Branch *) idLink->data;
    newSoln = (ff_soln *) idLink->next->data;
    if (newSoln->num_taps > 0) xfmrRow->size |= CHEK_TAPS;
  }
}
void checkDeltaTaps(Trace *trace, Link *xfmrList)
{
  Link      *idLink, *xfmrRow;
  cf_Branch *idData;
  ff_soln   *newSoln;
  for (xfmrRow=xfmrList; xfmrRow != NULL; xfmrRow = xfmrRow->next) {
    idLink = (Link *) xfmrRow->data;
    idData = (cf_Branch *) idLink->data;
    newSoln = (ff_soln *) idLink->next->data;
    if (newSoln->num_taps > 0) xfmrRow->size |= CHEK_DTAP;
  }
}
int deltaVoltLimit(Trace *trace, Link *newLink, int EHwinter)
{
  float    mnVoltagePU, mxVoltagePU, mn500, mnLow, dVpct;
  int      is500;                                  /* is500 = 1 if 500 kv bus */
  ff_soln *newSoln, *refSoln;

  newSoln = (ff_soln *) newLink->data;
  refSoln = (ff_soln *) newLink->next->data;
  is500 = ((int) newSoln->baseKV) == 500 ? 1 : 0;
  mn500 = EHwinter ? trace->DVmin500EH   : trace->DVmin500;
  mnLow = EHwinter ? trace->DVminBusEH   : trace->DVminBus;
  mnVoltagePU = is500 ? mn500 : mnLow;
  mxVoltagePU = is500 ? trace->DVmax500  : trace->DVmaxBus;
  if (newSoln->VmagPU < mnVoltagePU) return CHEK_DVLT;
  if (newSoln->VmagPU > mxVoltagePU) return CHEK_DVLT;
  dVpct = 100 * Cf_ratio(newSoln->VmagKV - refSoln->VmagKV, refSoln->VmagKV);
  if (fabs(dVpct) >= fabs(trace->DVmaxDelta)) return CHEK_DVLT;
  return 0;
}
void checkDeltaVolt(Trace *trace, Link *curRow, int EHwinter)
{
  Link *idLink, *newLink;
  while (curRow != NULL) {        /* get volts-per-unit for new and ref cases */
    idLink = (Link *) curRow->data;
    newLink = (Link *) idLink->next;
    curRow->size |= deltaVoltLimit(trace, newLink, EHwinter);
    curRow = curRow->next;
  }
}
void checkVolt(Trace *trace, Link *curRow, int EHwinter)
{
  Link *idLink;
  ff_soln *newSoln;
  while (curRow != NULL) {
    idLink = (Link *) curRow->data;
    newSoln = (ff_soln *) idLink->next->data;
    curRow->size |= voltLimit(trace, newSoln, EHwinter);
    curRow = curRow->next;
  }
}
void checkLoad(Trace *trace, Link *curRow)
{ /* get load_pct for each branch */
  Link *idLink;
  while (curRow != NULL) {
    idLink = (Link *) curRow->data;
    curRow->size |= loadLimit(trace, idLink, trace->chek);
    curRow = curRow->next;
  }
}
int loadLimit(Trace *trace, Link *idLink, int chek)
{
  ff_soln *newSoln;

  newSoln = (ff_soln *) idLink->next->data;
  if (newSoln->percent==0 && (chek & CHEK_ZRAT)) return CHEK_LOAD;
  if (newSoln->percent >= trace->maxLoad) return CHEK_LOAD;
  return 0;
}
int voltLimit(Trace *trace, ff_soln *newSoln, int EHwinter)
{
  float mnVoltagePU, mxVoltagePU, mn500, mnLow;
  int is500;  /* is500 = 1 if 500 kv bus */
  is500 = ((int) newSoln->baseKV) == 500 ? 1 : 0;
  mn500 = EHwinter ? trace->min500EH : trace->min500;
  mnLow = EHwinter ? trace->minBusEH : trace->minBus;
  mnVoltagePU = is500 ? mn500 : mnLow;
  mxVoltagePU = is500 ? trace->max500  : trace->maxBus;
  if (newSoln->VmagPU < mnVoltagePU) return CHEK_VOLT;
  if (newSoln->VmagPU > mxVoltagePU) return CHEK_VOLT;
  return 0;
}
int deltaLoadLimit(Trace *trace, Link *idLink)
{
  ff_soln   *newSoln, *refSoln;
  cf_Branch *idData;
  float      refLow, newLow, deltaPct;

  idData  = (cf_Branch *) idLink->data;
  newSoln = (ff_soln *)   idLink->next->data;
  refSoln = (ff_soln *)   idLink->next->next->data;
  if (newSoln->percent==0 && (trace->chek & CHEK_ZRAT)) return CHEK_DLDG;

  if (newSoln->amps > 0) {               /* if amps > 0 then branch is a line */
    newLow = newSoln->amps;
    refLow = refSoln->amps;
  }
  else  {                                 /* if amps==0 then branch is a xfmr */
    newLow = (idData->bus1_kv<=idData->bus2_kv) ? newSoln->mva1 : newSoln->mva2;
    refLow = (idData->bus1_kv<=idData->bus2_kv) ? refSoln->mva1 : refSoln->mva2;
  }
  deltaPct = 100 * Cf_ratio(newLow-refLow, refLow);     /* calc'd on low side */

  if (newSoln->percent >= trace->DLmaxLoad) return CHEK_DLDG;
  if (newSoln->percent < trace->DLthrLoad) return 0;
  if (fabs(deltaPct) >= fabs(trace->DLmaxDelta)) return CHEK_DLDG;
  return 0;
}
void checkDeltaLoad(Trace *trace, Link *curLink)
{ /* get load_pct for old and new cases */
  while (curLink != NULL) {
    curLink->size |= deltaLoadLimit(trace, (Link *) curLink->data);
    curLink = curLink->next;
  }
}
float calcRating(pf_rec *n, int tx, char *code)
{
  float nom, thm, btl, emr;
  nom = n->i.branch.total_rating;
  thm = n->i.branch.thermal_rating;
  btl = n->i.branch.bottleneck_rating;
  emr = n->i.branch.emergency_rating;
  if (n->i.branch.type[0]!='T') emr = 0; 
  if (thm==0 && btl==0 && emr==0) { *code = 'N'; return nom; }
  if (thm >0 && btl==0 && emr==0) { *code = 'T'; return thm; }
  if (thm==0 && btl >0 && emr==0) { *code = 'B'; return btl; }
  if (thm==0 && btl==0 && emr >0) { *code = 'E'; return emr; }
  if (n->i.branch.type[0]=='T' && tx==0) { /* TX look at btl and emr if tx==0 */
    if (btl==0 && emr >0) { *code = 'E'; return emr; }
    if (btl >0 && emr==0) { *code = 'B'; return btl; }
    if (btl >0 && btl<=emr) { *code = 'B'; return btl; }
    if (emr >0 && emr<=btl) { *code = 'E'; return emr; }
  }
  if (n->i.branch.type[0]=='T' && tx==1) { /* TX look at btl,emr,&thm if tx==1*/
    if (thm >0 && btl >0 && emr >0) {
        if (btl<=thm && btl<=emr) { *code = 'B'; return btl; }
        if (emr<=thm && emr<=btl) { *code = 'E'; return emr; }
        if (thm<=btl && thm<=emr) { *code = 'T'; return thm; }
    }
    if (thm >0 && btl >0 && emr==0) {
        if (btl<=thm) { *code = 'B'; return btl; }
        if (thm<=btl) { *code = 'T'; return thm; }
    } 
    if (thm >0 && btl==0 && emr >0) {
        if (emr<=thm) { *code = 'E'; return emr; }
        if (thm<=emr) { *code = 'T'; return thm; }
    }
    if (thm==0 && btl >0 && emr >0) {
        if (btl<=emr) { *code = 'B'; return btl; }
        if (emr<=btl) { *code = 'E'; return emr; }
    }
  }
  else { /* LINES look at btl and thm, emr=0 */
    if (btl >0 && btl<=thm) { *code = 'B'; return btl; }
    if (thm >0 && thm<=btl) { *code = 'T'; return thm; }
  }
  *code = ' '; return 0;
}
void makeNewName(Step *curStep, int *newNameSeed)
{
  char id[9];
  sprintf(id, "_%03d.bse", (*newNameSeed)++);
  cf_parse(curStep->oldBase, curStep->newBase, 'N');
  strcat(curStep->newBase, id);
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
int heavy_winter(char *bse_spec)
{ /* searches the base case file name for the letters EH (Extra Heavy) */
  char *cp, tmp_spec[FILENAME_MAX];
  strcpy(tmp_spec, bse_spec);
  cf_stoupper(tmp_spec);
  cp = strrchr(tmp_spec, ']');
  if (cp == NULL) cp = tmp_spec;
  cp = strstr(tmp_spec, "EH");
  if (cp != NULL) return 1;
  return 0;
}
void ff_printFailReport(cf_Out *rpt, int stat, Step *step)
{
  int n = 0;
  FILE  *fp = rpt->file;
  cf_eprint(fp, "\n%s\n", PAGE_BREAK); n+=2;
  cf_eprint(fp, "%-57s%s%56s\n", "*", "!!!!! WARNING !!!!!", "*"); n++;
  if (stat & FAIL_BASE) {
    cf_eprint(fp, "%-53s%s%53s\n", "*", "COULD NOT LOAD BASE CASE!!", "*");
    n++;
  }
  if (stat & FAIL_PCHG) {
    cf_eprint(fp, "%-50s%s%50s\n", "*", "COULD NOT LOAD PRIMARY CHANGE!!!", "*");
    n++;
  }
  if (stat & FAIL_ACHG) {
    cf_eprint(fp, "%-50s%s%50s\n", "*", "COULD NOT LOAD AUXILLARY CHANGE!", "*");
    n++;
  }
  if (stat & FAIL_SOLV) {
    cf_eprint(fp, "%-53s%s%53s\n", "*", "COULD NOT SOLVE BASE CASE!", "*");
    n++;
  }
  if (stat & FAIL_SAVE) {
    cf_eprint(fp, "%-53s%s%53s\n", "*", "COULD NOT SAVE BASE CASE!!", "*");
    n++;
  }
  if (stat & FAIL_REFB) {
    cf_eprint(fp, "%-48s%s%48s\n", "*", "COULD NOT LOAD REFERENCE BASE CASE!!", "*");
    n++;
  }
  cf_eprint(fp, "*%130c*\n", ' '); n++;
  cf_eprint(fp, "%s\n", PAGE_BREAK); n++;
  rpt->line += n;
}
void printSubReport(cf_Out *rpt, Trace *trace, Link *list, Step *step, int data, int chek)
{
  Link *curLink, *newLink;
  pf_rec *newID;

  if (trace->stat) return;
  printSubHeader(rpt, step, data);
  for (curLink = list; curLink != NULL; curLink = curLink->next) {
    newLink = (Link *) curLink->data;
    newID = (pf_rec *) newLink->data;
    if (curLink->size & chek) {
        if (rpt->line >= FF_PAGE_LENGTH-2) {
            ff_printTrceFooter(rpt, step);
            printSubHeader(rpt, step, data);
        }
        printSolnData((Link *) curLink->data, rpt->file, data);
        rpt->line++;
    }
  }
  if (rpt->line > FF_PAGE_LENGTH - 9) {      /* make room for new data */
    ff_printBlankLines(rpt, 2);
    ff_printTrceFooter(rpt, step);
  }
  return;
}
void ff_printMainReport(cf_Out *rpt, Trace *trace, Step *step)
{
  ff_printMainHeader(rpt, trace, step);
  ff_printChngRecord(rpt, trace->show, step);

  if (trace->chek & CHEK_VOLT) printSubReport(rpt, trace, trace->busList, step, DATA_VOLT, CHEK_VOLT);
  if (trace->chek & CHEK_DVLT) printSubReport(rpt, trace, trace->busList, step, DATA_DVLT, CHEK_DVLT);
  if (trace->chek & CHEK_LOAD) printSubReport(rpt, trace, trace->xfmrList, step, DATA_XFMR, CHEK_LOAD);
  if (trace->chek & CHEK_LOAD) printSubReport(rpt, trace, trace->lineList, step, DATA_LINE, CHEK_LOAD);
  if (trace->chek & CHEK_DLDG) printSubReport(rpt, trace, trace->xfmrList, step, DATA_DXFM, CHEK_DLDG);
  if (trace->chek & CHEK_DLDG) printSubReport(rpt, trace, trace->lineList, step, DATA_DLIN, CHEK_DLDG);
  if (trace->chek & CHEK_TAPS) printSubReport(rpt, trace, trace->xfmrList, step, DATA_TAPS, CHEK_TAPS);
  if (trace->chek & CHEK_DTAP) printSubReport(rpt, trace, trace->xfmrList, step, DATA_DTAP, CHEK_DTAP);
  if (trace->stat) ff_printFailReport(rpt, trace->stat, step);

  if (rpt->line!=0 && rpt->line!=FF_PAGE_LENGTH) {
    ff_printBlankLines(rpt, 2);
    ff_printTrceFooter(rpt, step);
  }
}
void ff_printChngRecord(cf_Out *rpt, int show, Step *curStep)
{
  int n = 0;
  FILE *fp = rpt->file;
  if (!show) return;
  if (curStep->useChng) {
    cf_eprint(fp, "Primary Change File: %s\n", curStep->priChng); n++;
    cf_eprint(fp, "%s\n", PAGE_MARK); n++;
    n += printContents(curStep->priChng, fp);
    cf_eprint(fp, "%s\n", PAGE_MARK); n++;
  }
  if (curStep->useAuxc) {
    cf_eprint(fp, "Auxillary Change File: %s\n", curStep->auxChng); n++;
    cf_eprint(fp, "%s\n", PAGE_MARK); n++;
    n += printContents(curStep->auxChng, fp);
    cf_eprint(fp, "%s\n", PAGE_MARK); n++;
  }
  rpt->line += n;
  return;
}
int printContents(char *spec, FILE *fp)
{
  FILE *src;
  char s[FF_PAGE_WIDTH];
  int n = 0;
  src = NULL;
  if ( (src=cf_openFile(spec, "r"))==NULL ) return 0;
  if (fp==NULL) return 0;
  while ( fgets(s, FF_PAGE_WIDTH, src) != NULL ) {
    if ( s[strlen(s)-1] == '\n' ) s[strlen(s)-1] = '\0';
    cf_eprint(fp, "%s\n", s); n++;
  }
  fclose(src);
  return n;
}
void ff_printMainHeader(cf_Out *rpt, Trace *trace, Step *curStep)
{
  int n = 0;
  FILE *fp = rpt->file;

  cf_eprint(fp, "%s\n", PAGE_BREAK); n++;

  cf_eprint(fp,   "/BASECASE      | %s\n", curStep->oldBase); n++;
  cf_eprint(fp,   "/PRI_CHANGE    | %s\n", curStep->priChng); n++;
  if (*curStep->priChng!='\0') {
    cf_eprint(fp, "/AUX_CHANGE    | %s\n", curStep->auxChng); n++;
  }
  if (*curStep->newBase!='\0') {
    cf_eprint(fp, "/NEW_BASECASE  | %s\n", curStep->newBase); n++;
  }
  if (*curStep->refBase!='\0') {
    cf_eprint(fp, "/REF_BASECASE  | %s\n", curStep->refBase); n++;
  }
  if (*curStep->corFile!='\0') {
    cf_eprint(fp, "/COORDINATE    | %s\n", curStep->corFile); n++;
  }
  if (*curStep->ps_File!='\0') {
    cf_eprint(fp, "/PLOT_NAME     | %s\n", curStep->ps_File); n++;
  }
  cf_eprint(fp,   "/REPORT        | NAME = %s\n", trace->outName); n++;

  n += cf_printList(fp,trace->ownrList, CF_dznStyl, "/OWNER         |");
  n += cf_printList(fp,trace->zoneList, CF_dznStyl, "/ZONE          |");
  n += cf_printList(fp,trace->bskvList, CF_sixStyl, "/BASE_KV       |");
  cf_printList(stdout, trace->ownrList, CF_dznStyl, "/OWNER         |");
  cf_printList(stdout, trace->zoneList, CF_dznStyl, "/ZONE          |");
  cf_printList(stdout, trace->bskvList, CF_sixStyl, "/BASE_KV       |");

  cf_eprint(fp,   "/COMMENT       | %-.80s\n", trace->comment); n++;
  cf_eprint(fp,   "/REPORT        |");
  cf_eprint(fp,   " TRACE =");
  if (trace->trc->type==TRACE_YES) cf_eprint(fp, " YES");
  if (trace->trc->type==TRACE_NO ) cf_eprint(fp, " NO");
  cf_eprint(fp,   ",   PLOT  =");
  if (  trace->plot & PLOT_PRNT ) cf_eprint(fp, " PRINT");
  if (  trace->plot & PLOT_DIFF ) cf_eprint(fp, " DIFF");
  if (  trace->plot & PLOT_SAVE ) cf_eprint(fp, " SAVE");
  if ( !trace->plot )             cf_eprint(fp, " NONE");
  cf_eprint(fp,   ",   SAVE_NEW_BASECASES =");
  if (  trace->save == SAVE_YES ) cf_eprint(fp, " YES");
  if (  trace->save == SAVE_NO  ) cf_eprint(fp, " NO");
  cf_eprint(fp,   ",   SHOW_CHANGES =");
  if (  trace->show == SHOW_YES ) cf_eprint(fp, " YES");
  if (  trace->show == SHOW_NO  ) cf_eprint(fp, " NO");
  cf_eprint(fp, "\n");  n++;

  cf_eprint(fp,   "/REPORT        | CHECK =");
  if (trace->chek & CHEK_VOLT) cf_eprint(fp, " CHECK_VOLTAGE");
  if (trace->chek & CHEK_LOAD) cf_eprint(fp, " CHECK_LOADING");
  if (trace->chek & CHEK_TAPS) cf_eprint(fp, " CHECK_TAPS");
  if (trace->chek & CHEK_DVLT) cf_eprint(fp, " COMPARE_VOLTAGE");
  if (trace->chek & CHEK_DLDG) cf_eprint(fp, " COMPARE_LOADING");
  if (trace->chek & CHEK_DTAP) cf_eprint(fp, " COMPARE_TAPS");
  if (trace->chek & CHEK_OUTG) cf_eprint(fp, " OUTAGE_SIM");
  if (trace->chek == 0)        cf_eprint(fp, " NONE");
  cf_eprint(fp, "\n"); n++;

  if (trace->chek) { 
    cf_eprint(fp, "/LIMITS        | ");
    cf_eprint(fp, "REPORT_0_RATINGS = ");
    if (  trace->chek & CHEK_ZRAT ) cf_eprint(fp, "YES");
    if (!(trace->chek & CHEK_ZRAT)) cf_eprint(fp, "NO");
    cf_eprint(fp, "   CHECK_TX_THERMAL = ");
    if (  trace->chek & CHEK_TXTH ) cf_eprint(fp, "YES");
    if (!(trace->chek & CHEK_TXTH)) cf_eprint(fp, "NO");
    cf_eprint(fp, "\n"); n++;
  }
  if (trace->chek & CHEK_VOLT) {
    cf_eprint(fp,"   MAX_500_PU = %5.3f,",  trace->max500);
    cf_eprint(fp,"   MIN_500_PU = %5.3f,",  trace->min500);
    cf_eprint(fp,"   MIN_500_EH = %5.3f\n", trace->min500EH); n++;
    cf_eprint(fp,"   MAX_BUS_PU = %5.3f,",  trace->maxBus);
    cf_eprint(fp,"   MIN_BUS_PU = %5.3f,",  trace->minBus);
    cf_eprint(fp,"   MIN_BUS_EH = %5.3f\n", trace->minBusEH); n++;
  }
  if (trace->chek & CHEK_DVLT) {
    cf_eprint(fp,"   DV_MAX_500_PU = %5.3f,",  trace->DVmax500);
    cf_eprint(fp,"   DV_MIN_500_PU = %5.3f,",  trace->DVmin500);
    cf_eprint(fp,"   DV_MIN_500_EH = %5.3f\n", trace->DVmin500EH); n++;
    cf_eprint(fp,"   DV_MAX_BUS_PU = %5.3f,",  trace->DVmaxBus);
    cf_eprint(fp,"   DV_MIN_BUS_PU = %5.3f,",  trace->DVminBus);
    cf_eprint(fp,"   DV_MIN_BUS_EH = %5.3f\n", trace->DVminBusEH); n++;
    cf_eprint(fp,"   DV_MAX_DELTA  = %5.2f\n", trace->DVmaxDelta); n++;
  }
  if (trace->chek & CHEK_LOAD) {
    cf_eprint(fp,"   MAX_LOAD_PCT    = %6.2f\n", trace->maxLoad); n++;
  }
  if (trace->chek & CHEK_DLDG) {
    cf_eprint(fp,"   DL_MAX_LOAD_PCT = %6.2f,",  trace->DLmaxLoad);
    cf_eprint(fp,"   DL_THR_LOAD_PCT = %6.2f,",  trace->DLthrLoad);
    cf_eprint(fp,"   DL_MAX_DELTA    = %5.2f\n", trace->DLmaxDelta); n++;
  }

  cf_eprint(fp, "%s\n", PAGE_BREAK); n++;
  rpt->line += n;
  return;
}
void printStep(cf_Out *rpt, Step *step)
{
  cf_eprint(rpt->file, "   |   |%25.25s|\n", step->newBase);
  rpt->line++;
  return;
}
void ff_printTrceFooter(cf_Out *rpt, Step *step)
{ /* fit as much of the 'trace equation' as possible into 122 characters */
  char s[FF_PAGE_WIDTH+1], form[255];
  int  l, len=0, n=0;

  l = strlen(step->oldBase); len += l; if (l>0) n++;
  l = strlen(step->priChng); len += l; if (l>0) n++;
  l = strlen(step->auxChng); len += l; if (l>0) n++;
  l = strlen(step->newBase); len += l; if (l>0) n++;
  l = strlen(step->refBase); len += l; if (l>0) n++;
  l = strlen(step->corFile); len += l; if (l>0) n++;
  l = strlen(step->ps_File); len += l; if (l>0) n++;

  if (len > 123)
    sprintf(form, "%%s %%.%ds... ",(int) (122.0/n)-6);
  else if (len > 102)
    sprintf(form, "%%s %%.%ds ",(int) (122.0/n)-3);
  else
    strcpy(form, "%s %s ");

  sprintf(s, form, " ", step->oldBase);
  if (step->priChng[0]!='\0') cf_aprint(s, form, "+", step->priChng);
  if (step->auxChng[0]!='\0') cf_aprint(s, form, "+", step->auxChng);
  if (step->newBase[0]!='\0') cf_aprint(s, form, "=", step->newBase);
  if (step->refBase[0]!='\0') cf_aprint(s, form, "-", step->refBase);
  if (step->corFile[0]!='\0') cf_aprint(s, form, "@", step->corFile);
  if (step->ps_File[0]!='\0') cf_aprint(s, form, ">", step->ps_File);

  cf_eprint(rpt->file, CF_PAGE_FOOTER, s, rpt->page);
  rpt->line += 2;
  rpt->page++;
  rpt->line -= FF_PAGE_LENGTH;
  return;
}
void ff_printBlankLines(cf_Out *rpt, int n)
{
  while (rpt->line < FF_PAGE_LENGTH - n) {
    fprintf(rpt->file, "\n");
    rpt->line++;
  }
  return;
}
void printVoltHeader(cf_Out *rpt, Step *step)
{
  FILE *fp = rpt->file;
/*  cf_eprint(fp, "\n%s\n", SUM_OVUV); */
  cf_eprint(fp, "\n");
  if (step->newBase[0]!='\0') cf_eprint(fp,"   |   |%-12.12s|\n",step->newBase);
  else                        cf_eprint(fp,"   |   |%-12.12s|\n",step->oldBase);
  cf_eprint(fp, "   |   |BUS VOLTAGES|  | Voltage|\n");
  cf_eprint(fp, "TYc|OWN|< BUS  ><KV>|ZN| ( PU  )|\n");
  rpt->line += 4;
  return;
}
void printDeltaVoltHeader(cf_Out *rpt, Step *step)
{
  FILE *fp = rpt->file;
/*  cf_eprint(fp, "\n%s\n", SUM_EXDV); */
  cf_eprint(fp, "\n");
  if (step->newBase[0]!='\0') cf_eprint(fp,"   |   |%-12.12s|\n",step->newBase);
  else                        cf_eprint(fp,"   |   |%-12.12s|\n",step->oldBase);
  cf_eprint(fp, "   |   | COMPARISON |  |   Ref  |   New  |  Delta |\n");
  cf_eprint(fp, "   |   |OF  VOLTAGES|  | Voltage| Voltage| Voltage|\n");
  cf_eprint(fp, "TYc|OWN|< BUS  ><KV>|ZN| (  PU )| (  PU )| (    %%)|\n");
  rpt->line += 5;
  return;
}
void printXfmrHeader(cf_Out *rpt, Step *step)
{/* new header: high-side and low-side load */
  FILE *fp = rpt->file;
/*  cf_eprint(fp, "\n%s\n\n", SUM_OVLD_XFMR); */
  cf_eprint(fp, "\n");
  if (step->newBase[0]!='\0') cf_eprint(fp,"   |   |%-25.25s|\n",step->newBase);
  else                        cf_eprint(fp,"   |   |%-25.25s|\n",step->oldBase);
  cf_eprint(fp, "   |   |       SUMMARY  OF       | | |  |  |   | BUS1 | BUS2 |BUS 1|BUS 2|BUS 1|BUS 2|BUS 1|BUS 2|BUS 1|BUS 2| |\n");
  cf_eprint(fp, "   |   |   TRANSFORMER LOADING   | | |  |  |   | LOAD | LOAD | NOM | NOM | THM | THM | EMR | EMR |BTL 1|BTL 2| |\n");
  cf_eprint(fp, "TYc|OWN|< BUS1 ><V1>M< BUS2 ><V2>|C|S|Z1|Z2|MYr|( MVA)|( MVA)|(  %%)|(  %%)|(  %%)|(  %%)|(  %%)|(  %%)|(  %%)|(  %%)|F|\n");
  rpt->line += 5;
  return;
}
void printLineHeader(cf_Out *rpt, Step *step)
{/* new header: high-side and low-side load */
  FILE *fp = rpt->file;
/*  cf_eprint(fp, "\n%s\n\n", SUM_OVLD_LINE); */
  cf_eprint(fp, "\n");
  if (step->newBase[0]!='\0') cf_eprint(fp,"   |   |%-25.25s|\n",step->newBase);
  else                        cf_eprint(fp,"   |   |%-25.25s|\n",step->oldBase);
  cf_eprint(fp, "   |   |       SUMMARY  OF       | | |  |  |   |      |     |     |     | |\n");
  cf_eprint(fp, "   |   |      LINE  LOADING      | | |  |  |   | LOAD | NOM | THM | BTL | |\n");
  cf_eprint(fp, "TYc|OWN|< BUS1 ><V1>M< BUS2 ><V2>|C|S|Z1|Z2|MYr|(AMPS)|(  %%)|(  %%)|(  %%)|F|\n");
  rpt->line += 5;
  return;
}
void printDeltaXfmrHeader(cf_Out *rpt, Step *step)
{
  FILE *fp = rpt->file;
/*  cf_eprint(fp, "\n%s\n\n", SUM_EXDL_XFMR); */
  cf_eprint(fp, "\n");
  if (step->newBase[0]!='\0') cf_eprint(fp,"   |   |%-25.25s|\n",step->newBase);
  else                        cf_eprint(fp,"   |   |%-25.25s|\n",step->oldBase);
  cf_eprint(fp, "   |   |      COMPARISON OF      | | |  |  |   | BUS1 | BUS2 | REF1 | REF2 |BUS 1|BUS 2|BUS 1|BUS 2|BUS 1|BUS 2|BUS 1|BUS 2| |DELTA|\n");
  cf_eprint(fp, "   |   |   TRANSFORMER LOADING   | | |  |  |   | LOAD | LOAD | LOAD | LOAD | NOM | NOM | THM | THM | EMR | EMR |BTL 1|BTL 2| |N-R/R|\n");
  cf_eprint(fp, "TYc|OWN|< BUS1 ><V1>M< BUS2 ><V2>|C|S|Z1|Z2|MYr|( MVA)|( MVA)|( MVA)|( MVA)|(  %%)|(  %%)|(  %%)|(  %%)|(  %%)|(  %%)|(  %%)|(  %%)|F|(  %%)|\n");
  rpt->line += 5;
  return;
}
void printDeltaLineHeader(cf_Out *rpt, Step *step)
{
  FILE *fp = rpt->file;
/*  cf_eprint(fp, "\n%s\n\n", SUM_EXDL_LINE); */
  cf_eprint(fp, "\n");
  if (step->newBase[0]!='\0') cf_eprint(fp,"   |   |%-25.25s|\n",step->newBase);
  else                        cf_eprint(fp,"   |   |%-25.25s|\n",step->oldBase);
  cf_eprint(fp, "   |   |      COMPARISON OF      | | |  |  |   |      | REF  |     |     |     | |DELTA|\n");
  cf_eprint(fp, "   |   |      LINE  LOADING      | | |  |  |   | LOAD | LOAD | NOM | THM | BTL | |N-R/R|\n");
  cf_eprint(fp, "TYc|OWN|< BUS1 ><V1>M< BUS2 ><V2>|C|S|Z1|Z2|MYr|( MVA)|( MVA)|(  %%)|(  %%)|(  %%)|F|(  %%)|\n");
  rpt->line += 5;
  return;
}
void printTapsHeader(cf_Out *rpt, Step *step)
{
  FILE *fp = rpt->file;
/*  cf_eprint(fp, "\n%s\n\n", SUM_XFMR_TAPS); */
  cf_eprint(fp, "\n");
  if (step->newBase[0]!='\0') cf_eprint(fp,"   |   |%-25.25s|\n",step->newBase);
  else                        cf_eprint(fp,"   |   |%-25.25s|\n",step->oldBase);
  cf_eprint(fp, "   |   |SUMMARY OF  TAP POSITIONS| | |  Tap  | Tap| # of|\n");
  cf_eprint(fp, "TYc|OWN|< BUS1 ><V1>M< BUS2 ><V2>|C|S| ( kV )|  # | Taps|\n");
  rpt->line += 4;
  return;
}
void printDeltaTapsHeader(cf_Out *rpt, Step *step)
{
  FILE *fp = rpt->file;
/*  cf_eprint(fp, "\n%s\n\n", SUM_XFMR_DTAP); */
  cf_eprint(fp, "\n");
  if (step->newBase[0]!='\0') cf_eprint(fp,"   |   |%-25.25s|\n",step->newBase);
  else                        cf_eprint(fp,"   |   |%-25.25s|\n",step->oldBase);
  cf_eprint(fp, "   |   |      COMPARISON OF      | | |  New  |  Ref  | New| Ref|     |        |\n");
  cf_eprint(fp, "   |   |      TAP POSITIONS      | | |  Tap  |  Tap  | Tap| Tap| # of| #of Tap|\n");
  cf_eprint(fp, "TYc|OWN|< BUS1 ><V1>M< BUS2 ><V2>|C|S| ( kV )| ( kV )|  # |  # | Taps| Changes|\n");
  rpt->line += 5;
  return;
}
void printSubHeader(cf_Out *rpt, Step *step, int data)
{
  if (data == DATA_VOLT) printVoltHeader(rpt, step);
  if (data == DATA_XFMR) printXfmrHeader(rpt, step);
  if (data == DATA_LINE) printLineHeader(rpt, step);
  if (data == DATA_TAPS) printTapsHeader(rpt, step);
  if (data == DATA_DVLT) printDeltaVoltHeader(rpt, step);
  if (data == DATA_DXFM) printDeltaXfmrHeader(rpt, step);
  if (data == DATA_DLIN) printDeltaLineHeader(rpt, step);
  if (data == DATA_DTAP) printDeltaTapsHeader(rpt, step);
}
void printSolnData(Link *idLink, FILE *fp, int data)
{
  if (data==DATA_VOLT) printVoltData(idLink, fp);
  if (data==DATA_LINE) printLineData(idLink, fp);
  if (data==DATA_XFMR) printXfmrData(idLink, fp);
  if (data==DATA_TAPS) printTapsData(idLink, fp);
  if (data==DATA_DVLT) printDeltaVoltData(idLink, fp);
  if (data==DATA_DLIN) printDeltaLineData(idLink, fp);
  if (data==DATA_DXFM) printDeltaXfmrData(idLink, fp);
  if (data==DATA_DTAP) printDeltaTapsData(idLink, fp);
}
void printVoltData(Link *idLink, FILE *fp)
{
  pf_rec   r;
  char     bus_id[133];
  ff_soln *newSoln;

  cf_bus2rec((cf_Bus *) idLink->data, &r);
  pf_rec_b2a(bus_id, &r, "I");
  cf_eprint(fp, "%2.2s |%3.3s|%12.12s|%2.2s|", &bus_id[0], &bus_id[3],
    &bus_id[6], &bus_id[18]);
  newSoln = (ff_soln *) idLink->next->data;
  cf_eprint(fp, " %7.3f|", newSoln->VmagPU);
  cf_eprint(fp, "\n");
}
void printDeltaVoltData(Link *idLink, FILE *fp)
{
  char     bus_id[133];
  float    dVpct;
  ff_soln *newSoln, *refSoln;
  pf_rec   r;

  cf_bus2rec((cf_Bus *) idLink->data, &r);
  newSoln = (ff_soln *) idLink->next->data;
  refSoln = (ff_soln *) idLink->next->next->data;
  pf_rec_b2a(bus_id, &r, "I");
  cf_eprint(fp, "%2.2s |%3.3s|%12.12s|%2.2s|", &bus_id[0], &bus_id[3],
    &bus_id[6], &bus_id[18]);

  cf_eprint(fp, " %7.3f|", refSoln->VmagPU);
  cf_eprint(fp, " %7.3f|", newSoln->VmagPU);
  dVpct = 100 * Cf_ratio(newSoln->VmagKV - refSoln->VmagKV, refSoln->VmagKV);
  cf_eprint(fp, " %7.2f|", dVpct);
  cf_eprint(fp, "\n");
}
void printDeltaXfmrData(Link *idLink, FILE *fp)
{
  char  xfmrID[133];
  float nom_pct1, nom_pct2, thm_pct1, thm_pct2, emr_pct1, emr_pct2;
  float adj_btl1, adj_btl2, btl_pct1, btl_pct2, delta;
  pf_rec r;
  cf_Branch *idData;
  ff_soln   *newSoln, *refSoln;

  idData = (cf_Branch *) idLink->data;
  newSoln = (ff_soln *) idLink->next->data;
  refSoln = (ff_soln *) idLink->next->next->data;
  cf_branch2rec(idData, &r);
  pf_rec_b2a(xfmrID, &r, "I");
  cf_eprint(fp, "%2.2s |%3.3s|%25.25s|%1.1s|%1.1s|%2.2s|%2.2s|%3.3s|",
    &xfmrID[0], &xfmrID[3], &xfmrID[6], &xfmrID[31], &xfmrID[32],
    idData->bus1_zone, idData->bus2_zone, &xfmrID[74]);

  adj_btl1 = idData->bottleneck * newSoln->VmagPU1;
  adj_btl2 = idData->bottleneck * newSoln->VmagPU2;

  nom_pct1 = 100*Cf_ratio(newSoln->mva1, idData->nominal);
  nom_pct2 = 100*Cf_ratio(newSoln->mva2, idData->nominal);
  thm_pct1 = 100*Cf_ratio(newSoln->mva1, idData->thermal);
  thm_pct2 = 100*Cf_ratio(newSoln->mva2, idData->thermal);
  emr_pct1 = 100*Cf_ratio(newSoln->mva1, idData->emergency);
  emr_pct2 = 100*Cf_ratio(newSoln->mva2, idData->emergency);
  btl_pct1 = 100*Cf_ratio(newSoln->mva1, adj_btl1);
  btl_pct2 = 100*Cf_ratio(newSoln->mva2, adj_btl2);

/* delta calculated on low side */
  delta = ( r.i.branch.bus1_kv <= r.i.branch.bus2_kv ) ?
    100 * Cf_ratio(newSoln->mva1 - refSoln->mva1, refSoln->mva1) :
    100 * Cf_ratio(newSoln->mva2 - refSoln->mva2, refSoln->mva2) ;

  cf_eprint(fp, "%6.1f|", newSoln->mva1);
  cf_eprint(fp, "%6.1f|", newSoln->mva2);
  cf_eprint(fp, "%6.1f|", refSoln->mva1);
  cf_eprint(fp, "%6.1f|", refSoln->mva2);
  cf_eprint(fp, "%5.1f|", nom_pct1);
  cf_eprint(fp, "%5.1f|", nom_pct2);
  cf_eprint(fp, "%5.1f|", thm_pct1);
  cf_eprint(fp, "%5.1f|", thm_pct2);
  cf_eprint(fp, "%5.1f|", emr_pct1);
  cf_eprint(fp, "%5.1f|", emr_pct2);
  cf_eprint(fp, "%5.1f|", btl_pct1);
  cf_eprint(fp, "%5.1f|", btl_pct2);
  cf_eprint(fp, "%1.1c|", newSoln->code);
  if (delta>999.9 || delta<-99.9) cf_eprint(fp, "%5.0f|", delta);
  else                            cf_eprint(fp, "%5.1f|", delta);
  cf_eprint(fp, "\n");
}
void printDeltaLineData(Link *idLink, FILE *fp)
{
  char  lineID[133];
  float nom_pct, thm_pct, btl_pct, delta;
  pf_rec r;
  cf_Branch *idData;
  ff_soln   *newSoln, *refSoln;

  idData = (cf_Branch *) idLink->data;
  newSoln = (ff_soln *) idLink->next->data;
  refSoln = (ff_soln *) idLink->next->next->data;
  cf_branch2rec(idData, &r);
  pf_rec_b2a(lineID, &r, "I");
  cf_eprint(fp, "%2.2s |%3.3s|%25.25s|%1.1s|%1.1s|%2.2s|%2.2s|%3.3s|",
    &lineID[0], &lineID[3], &lineID[6], &lineID[31], &lineID[32],
    idData->bus1_zone, idData->bus2_zone, &lineID[74]);

  nom_pct = 100*Cf_ratio(newSoln->amps, idData->nominal);
  thm_pct = 100*Cf_ratio(newSoln->amps, idData->thermal);
  btl_pct = 100*Cf_ratio(newSoln->amps, idData->bottleneck);
  delta   = 100*Cf_ratio(newSoln->amps - refSoln->amps, refSoln->amps);

  cf_eprint(fp, "%6.1f|", newSoln->amps);
  cf_eprint(fp, "%6.1f|", refSoln->amps);
  cf_eprint(fp, "%5.1f|", nom_pct);
  cf_eprint(fp, "%5.1f|", thm_pct);
  cf_eprint(fp, "%5.1f|", btl_pct);
  cf_eprint(fp, "%1.1c|", newSoln->code);
  if (delta>999.9 || delta<-99.9) cf_eprint(fp, "%5.0f|", delta);
  else                            cf_eprint(fp, "%5.1f|", delta);
  cf_eprint(fp, "\n");
}
void printLineData(Link *idLink, FILE *fp)
{
  char  lineID[133];
  float nom_pct, thm_pct, btl_pct;
  pf_rec r;
  cf_Branch *idData;
  ff_soln   *newSoln;

  idData = (cf_Branch *) idLink->data;
  newSoln = (ff_soln *) idLink->next->data;
  cf_branch2rec(idData, &r);
  pf_rec_b2a(lineID, &r, "I");
  cf_eprint(fp, "%2.2s |%3.3s|%25.25s|%1.1s|%1.1s|%2.2s|%2.2s|%3.3s|",
    &lineID[0], &lineID[3], &lineID[6], &lineID[31], &lineID[32],
    idData->bus1_zone, idData->bus2_zone, &lineID[74]);

  nom_pct = 100*Cf_ratio(newSoln->amps, idData->nominal);
  thm_pct = 100*Cf_ratio(newSoln->amps, idData->thermal);
  btl_pct = 100*Cf_ratio(newSoln->amps, idData->bottleneck);

  cf_eprint(fp, "%6.1f|", newSoln->amps);
  cf_eprint(fp, "%5.1f|", nom_pct);
  cf_eprint(fp, "%5.1f|", thm_pct);
  cf_eprint(fp, "%5.1f|", btl_pct);
  cf_eprint(fp, "%1.1c|", newSoln->code);
  cf_eprint(fp, "\n");
}
void printXfmrData(Link *idLink, FILE *fp)
{
  char  xfmrID[133];
  float nom_pct1, nom_pct2, thm_pct1, thm_pct2, emr_pct1, emr_pct2;
  float adj_btl1, adj_btl2, btl_pct1, btl_pct2;
  pf_rec r;
  cf_Branch *idData;
  ff_soln   *newSoln;

  idData = (cf_Branch *) idLink->data;
  newSoln = (ff_soln *) idLink->next->data;
  cf_branch2rec(idData, &r);
  pf_rec_b2a(xfmrID, &r, "I");
  cf_eprint(fp, "%2.2s |%3.3s|%25.25s|%1.1s|%1.1s|%2.2s|%2.2s|%3.3s|",
    &xfmrID[0], &xfmrID[3], &xfmrID[6], &xfmrID[31], &xfmrID[32],
    idData->bus1_zone, idData->bus2_zone, &xfmrID[74]);

  adj_btl1 = idData->bottleneck * newSoln->VmagPU1;
  adj_btl2 = idData->bottleneck * newSoln->VmagPU2;

  nom_pct1 = 100*Cf_ratio(newSoln->mva1, idData->nominal);
  nom_pct2 = 100*Cf_ratio(newSoln->mva2, idData->nominal);
  thm_pct1 = 100*Cf_ratio(newSoln->mva1, idData->thermal);
  thm_pct2 = 100*Cf_ratio(newSoln->mva2, idData->thermal);
  emr_pct1 = 100*Cf_ratio(newSoln->mva1, idData->emergency);
  emr_pct2 = 100*Cf_ratio(newSoln->mva2, idData->emergency);
  btl_pct1 = 100*Cf_ratio(newSoln->mva1, adj_btl1);
  btl_pct2 = 100*Cf_ratio(newSoln->mva2, adj_btl2);

  cf_eprint(fp, "%6.1f|", newSoln->mva1);
  cf_eprint(fp, "%6.1f|", newSoln->mva2);
  cf_eprint(fp, "%5.1f|", nom_pct1);
  cf_eprint(fp, "%5.1f|", nom_pct2);
  cf_eprint(fp, "%5.1f|", thm_pct1);
  cf_eprint(fp, "%5.1f|", thm_pct2);
  cf_eprint(fp, "%5.1f|", emr_pct1);
  cf_eprint(fp, "%5.1f|", emr_pct2);
  cf_eprint(fp, "%5.1f|", btl_pct1);
  cf_eprint(fp, "%5.1f|", btl_pct2);
  cf_eprint(fp, "%1.1c|", newSoln->code);
  cf_eprint(fp, "\n");
}
void printTapsData(Link *idLink, FILE *fp)
{
  char  xfmrID[133];
  float tapNo, tapkV, tapInc;
  pf_rec r;
  cf_Branch *idData;
  ff_soln   *newSoln;

  idData = (cf_Branch *) idLink->data;
  newSoln = (ff_soln *) idLink->next->data;
  cf_branch2rec(idData, &r);
  pf_rec_b2a(xfmrID, &r, "I");
  cf_eprint(fp, "%2.2s |%3.3s|%25.25s|%1.1s|%1.1s|", &xfmrID[0], &xfmrID[3],
    &xfmrID[6], &xfmrID[31], &xfmrID[32]);

  if ( (newSoln->tap1>=newSoln->min_tap) && (newSoln->tap2<=newSoln->max_tap) )
    tapkV = newSoln->tap1;
  else
    tapkV = newSoln->tap2;
  tapInc = Cf_ratio(newSoln->max_tap - newSoln->min_tap, newSoln->num_taps-1);
  tapNo = Cf_ratio((tapkV - newSoln->min_tap), tapInc) + 1;
  cf_eprint(fp, " %6.2f|", tapkV);
  cf_eprint(fp, " %2.0f |", tapNo);
  cf_eprint(fp, " %3d |", newSoln->num_taps);
  cf_eprint(fp, "\n");
}
void printDeltaTapsData(Link *idLink, FILE *fp)
{
  char  xfmrID[133];
  float newTapNo, refTapNo, newTapkV, refTapkV, tapInc, deltaTap;
  pf_rec r;
  cf_Branch *idData;
  ff_soln   *newSoln, *refSoln;

  idData = (cf_Branch *) idLink->data;
  newSoln = (ff_soln *) idLink->next->data;
  refSoln = (ff_soln *) idLink->next->next->data;
  cf_branch2rec(idData, &r);
  pf_rec_b2a(xfmrID, &r, "I");
  cf_eprint(fp, "%2.2s |%3.3s|%25.25s|%1.1s|%1.1s|", &xfmrID[0], &xfmrID[3],
    &xfmrID[6], &xfmrID[31], &xfmrID[32]);

  if ( (newSoln->tap1>=newSoln->min_tap) && (newSoln->tap1<=newSoln->max_tap) )
    newTapkV = newSoln->tap1;
  else
    newTapkV = newSoln->tap2;
  if ( (refSoln->tap1>=newSoln->min_tap) && (refSoln->tap1<=newSoln->max_tap) )
    refTapkV = refSoln->tap1;
  else
    refTapkV = refSoln->tap2;
  tapInc = Cf_ratio(newSoln->max_tap - newSoln->min_tap, newSoln->num_taps-1);
  newTapNo = Cf_ratio((newTapkV - newSoln->min_tap), tapInc) + 1;
  refTapNo = Cf_ratio((refTapkV - newSoln->min_tap), tapInc) + 1;
  deltaTap = newTapNo - refTapNo;
  cf_eprint(fp, " %6.2f|", newTapkV);
  cf_eprint(fp, " %6.2f|", refTapkV);
  cf_eprint(fp, " %2.0f |", newTapNo);
  cf_eprint(fp, " %2.0f |", refTapNo);
  cf_eprint(fp, " %3d |", newSoln->num_taps);
  cf_eprint(fp, " %6.0f |", deltaTap);
  cf_eprint(fp, "\n");
}
void buildTrace(Trace *trace)
{
  Link *curLink, *stepLink;
  Step *step;
  char  newLine[1024];

  if (trace->trceList!=NULL) return;
  curLink = NULL;
  for (stepLink = trace->stepList; stepLink!=NULL; stepLink = stepLink->next ) {
    step = (Step *) stepLink->data;
    sprintf(newLine, "%s ", step->oldBase);
    if (step->useChng && step->priChng[0]!='\0')
        cf_aprint(newLine, "+ %s ", step->priChng);
    if (step->useAuxc && step->auxChng[0]!='\0')
        cf_aprint(newLine, "+ %s ", step->auxChng);
    if (step->useChng || step->useAuxc)
        cf_aprint(newLine, "= %s ", step->newBase);
    if (step->refBase[0]!='\0')
        cf_aprint(newLine, "- %s ", step->refBase);
    if (step->corFile[0]!='\0')
        cf_aprint(newLine, "@ %s ", step->corFile);
    if (step->ps_File[0]!='\0')
        cf_aprint(newLine, "> %s ", step->ps_File);

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
  Link *list, *link;
  int mode = READ_INC;/* default /INCLUDE */
  while (fgets(str, CF_INBUFSIZE, readMe)!=NULL) { 
    if ( cf_iscomment(str) ) continue;
    strcpy(STR, str);
    cf_str2upper(STR);
    if ( strstr(STR, "/INCLUDE"     )!=NULL ) { mode = READ_INC; }
    if ( strstr(STR, "/BASECASE"    )!=NULL ) { mode = READ_BSE; }
    if ( strstr(STR, "/OWNER"       )!=NULL ) { mode = READ_OWN; }
    if ( strstr(STR, "/ZONE"        )!=NULL ) { mode = READ_ZON; }
    if ( strstr(STR, "/BASE_KV"     )!=NULL ) { mode = READ_BKV; }
    if ( strstr(STR, "/REPORT"      )!=NULL ) { mode = READ_REP; }
    if ( strstr(STR, "/LIMITS"      )!=NULL ) { mode = READ_LIM; }
    if ( strstr(STR, "/TRACE"       )!=NULL ) { mode = READ_TRC; }
    if ( strstr(STR, "/PRI_CHANGE"  )!=NULL ) { mode = READ_PCH; trace->query &= ~QUERY_PCHG; }
    if ( strstr(STR, "/AUX_CHANGE"  )!=NULL ) { mode = READ_ACH; trace->query &= ~QUERY_ACHG; }
    if ( strstr(STR, "/NEW_BASECASE")!=NULL ) { mode = READ_NEW; trace->query &= ~QUERY_NEWB; }
    if ( strstr(STR, "/REF_BASECASE")!=NULL ) { mode = READ_REF; trace->query &= ~QUERY_REFB; }
    if ( strstr(STR, "/COORDINATE"  )!=NULL ) { mode = READ_COR; trace->query &= ~QUERY_CORD; }
    if ( strstr(STR, "/PLOT_NAME"   )!=NULL ) { mode = READ_PLT; trace->query &= ~QUERY_PLTN; }
    if ( strstr(STR, "/COMMENT"     )!=NULL ) { mode = READ_COM; trace->query &= ~QUERY_COMM; }
    if ( strstr(STR, "/SOLUTION"    )!=NULL ) { mode = READ_SOL; trace->query &= ~QUERY_SOLN; }
    if ( strstr(STR, "/BUS"         )!=NULL ) { mode = READ_BUS; }
    if ( strstr(STR, "/BRANCH"      )!=NULL ) { mode = READ_BRN; }
    if ( mode == 0 ) continue;         /* note: mode is defaulted to READ_INC */
    if ( STR[0]=='/' ) {
        if ( (cp=strpbrk(str, "|"))!=NULL ) strcpy(str, (++cp));
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
    if ( mode == READ_ACH ) {
        list = cf_text2List(str);
        cf_appList(&trace->achgList, list);
        trace->query &= ~(QUERY_ACHG);
    }
    if ( mode == READ_NEW ) {
        list = cf_text2List(str);
        cf_appList(&trace->newbList, list);
        trace->query &= ~(QUERY_NEWB);
    }
    if ( mode == READ_REF ) {
        list = cf_text2List(str);
        cf_appList(&trace->refbList, list);
        trace->query &= ~(QUERY_REFB);
    }
    if ( mode == READ_COR ) {
        list = cf_text2List(str);
        cf_appList(&trace->cordList, list);
        trace->query &= ~(QUERY_CORD);
    }
    if ( mode == READ_PLT ) {
        list = cf_text2List(str);
        cf_appList(&trace->plotList, list);
        trace->query &= ~(QUERY_PLTN);
    }
    if ( mode == READ_OWN ) {
        list = cf_text2List(str);
        if (list!=NULL) {
            trace->query &= ~(QUERY_OWNR);
            trace->query &= ~(QUERY_DATA);
        }
        cf_appList(&trace->ownrList, list);
    }
    if ( mode == READ_ZON ) {
        list = cf_text2List(str);
        if (list!=NULL) {
            trace->query &= ~(QUERY_ZONE);
            trace->query &= ~(QUERY_DATA);
        }
        cf_appList(&trace->zoneList, list);
    }
    if ( mode == READ_BKV ) {
        list = cf_text2List(str);
        if (list!=NULL) {
            trace->query &= ~(QUERY_BSKV);
            trace->query &= ~(QUERY_DATA);
        }
        cf_appList(&trace->bskvList, list);
    }
    if ( mode == READ_COM ) {
        sscanf(str, " %80[^\n] ", trace->comment);
        trace->query &= ~(QUERY_COMM);
    }
    if ( mode == READ_SOL ) {
        if (trace->solution[0]=='\0') sprintf(trace->solution, "/SOLUTION\n");
        strcat(trace->solution, str);
        strcat(trace->solution, "\n");
        trace->query &= ~(QUERY_SOLN);
    }
    if ( mode == READ_BUS ) {
/* note: buses appended in input order rather than inserted in sort order */
        link = cf_id2Link(str, 'I');
        list = cf_link2row(link);
        cf_appList(&trace->busList, list);
        if (list!=NULL) {
            trace->query &= ~(QUERY_DATA);
            trace->query &= ~(QUERY_ZONE);
            trace->query &= ~(QUERY_OWNR);
            trace->query &= ~(QUERY_BSKV);
            trace->get   &= ~(GET_BUS_LIS);
            trace->get   |=  (GET_BUS_ZON);
        }
    }
    if ( mode == READ_BRN ) {
/* note: branches appended in input order rather than inserted in sort order */
        link = cf_id2Link(str, 'I');
        list = cf_link2row(link);
        if (str[0]=='L') cf_appList(&trace->lineList, list);
        if (str[0]=='E') cf_appList(&trace->lineList, list);
        if (str[0]=='T') cf_appList(&trace->xfmrList, list);
        if (list!=NULL) {
            trace->query &= ~(QUERY_DATA);
            trace->query &= ~(QUERY_ZONE);
            trace->query &= ~(QUERY_OWNR);
            trace->query &= ~(QUERY_BSKV);
            trace->get   &= ~(GET_BRN_LIS);
            trace->get   |=  (GET_BRN_ZON | GET_BRN_RAT);
        }
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

  stepLink = cf_newLink(sizeof(Step));
  step = (Step *) stepLink->data;
  for (cp=strtok(s, " "), op = ' '; cp != NULL; cp=strtok(NULL, " ")) {
    if (ispunct(*cp)) { op = *cp; continue; }
    if (op == ' ') strcpy(step->oldBase, cp);
    if (op == '+' && *step->priChng=='\0') {
        strcpy(step->priChng, cp); step->useChng = 1;
    }
    else if (op == '+' && *step->auxChng=='\0') {
        strcpy(step->auxChng, cp); step->useAuxc = 1;
    }
    if (op == '=') strcpy(step->newBase, cp);
    if (op == '-') strcpy(step->refBase, cp);
    if (op == '@') strcpy(step->corFile, cp);
    if (op == '>') strcpy(step->ps_File, cp);
  }
  cf_appList(&trace->stepList, stepLink);
  trace->query &=
    ~(QUERY_BASE|QUERY_PCHG|QUERY_ACHG|QUERY_NEWB|QUERY_REFB|QUERY_CORD);
  trace->query &= ~(QUERY_APLY);
  trace->aply = 4;                              /* NEXT TO NEXT, NEXT TO NEXT */

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
  if (*step->auxChng!='\0') {
    trcLink = cf_newLink(strlen(step->auxChng)+1);
    strcpy((char *) trcLink->data, step->auxChng);
    cf_appList(&trace->achgList, trcLink);
  }
  if (*step->newBase!='\0') {
    trcLink = cf_newLink(strlen(step->newBase)+1);
    strcpy((char *) trcLink->data, step->newBase);
    cf_appList(&trace->newbList, trcLink);
  }
  if (*step->refBase!='\0') {
    trcLink = cf_newLink(strlen(step->refBase)+1);
    strcpy((char *) trcLink->data, step->refBase);
    cf_appList(&trace->refbList, trcLink);
  }
  if (*step->corFile!='\0') {
    trcLink = cf_newLink(strlen(step->corFile)+1);
    strcpy((char *) trcLink->data, step->corFile);
    cf_appList(&trace->cordList, trcLink);
  }
  if (*step->ps_File!='\0') {
    trcLink = cf_newLink(strlen(step->ps_File)+1);
    strcpy((char *) trcLink->data, step->ps_File);
    cf_appList(&trace->plotList, trcLink);
  }
  return;
}
void ff_limits(char *s, Trace *trace)
{
  char  *sp, *tp, t[CF_INBUFSIZE];

  for (sp = strtok(s, " ,="); sp!=NULL; sp = strtok(NULL, " ,=")) {
    tp=strtok(NULL, ", ="); if (tp==NULL) return;
    if (strstr(sp, "DL_MAX_DELTA")!=NULL) {
        sscanf(tp, "%f", &trace->DLmaxDelta);
        trace->query &= ~(QUERY_DLDG);
    }
    if (strstr(sp, "DL_MAX_LOAD_PCT")!=NULL) {
        sscanf(tp, "%f", &trace->DLmaxLoad);
        trace->query &= ~(QUERY_DLDG);
    }
    if (strstr(sp, "DL_THR_LOAD_PCT")!=NULL) {
        sscanf(tp, "%f", &trace->DLthrLoad);
        trace->query &= ~(QUERY_DLDG);
    }
    if (strstr(sp, "MAX_LOAD_PCT")!=NULL) {
        sscanf(tp, "%f", &trace->maxLoad);
        trace->query &= ~(QUERY_LOAD);
    }
    if (strstr(sp, "MAX_500_PU")!=NULL) {
        sscanf(tp, "%f", &trace->max500);
        trace->query &= ~(QUERY_VOLT);
    }
    if (strstr(sp, "MIN_500_PU")!=NULL) {
        sscanf(tp, "%f", &trace->min500);
        trace->query &= ~(QUERY_VOLT);
    }
    if (strstr(sp, "MAX_BUS_PU")!=NULL) {
        sscanf(tp, "%f", &trace->maxBus);
        trace->query &= ~(QUERY_VOLT);
    }
    if (strstr(sp, "MIN_BUS_PU")!=NULL) {
        sscanf(tp, "%f", &trace->minBus);
        trace->query &= ~(QUERY_VOLT);
    }
    if (strstr(sp, "MIN_EH5_PU")!=NULL) {
        sscanf(tp, "%f", &trace->min500EH);
        trace->query &= ~(QUERY_VOLT);
    }
    if (strstr(sp, "MIN_EHV_PU")!=NULL) {
        sscanf(tp, "%f", &trace->minBusEH);
        trace->query &= ~(QUERY_VOLT);
    }
    if (strstr(sp, "DV_MAX_DELTA")!=NULL) {
        sscanf(tp, "%f", &trace->DVmaxDelta);
        trace->query &= ~(QUERY_DVLT);
    }
    if (strstr(sp, "DV_MAX_500_PU")!=NULL) {
        sscanf(tp, "%f", &trace->DVmax500);
        trace->query &= ~(QUERY_DVLT);
    }
    if (strstr(sp, "DV_MIN_500_PU")!=NULL) {
        sscanf(tp, "%f", &trace->DVmin500);
        trace->query &= ~(QUERY_DVLT);
    }
    if (strstr(sp, "DV_MAX_BUS_PU")!=NULL) {
        sscanf(tp, "%f", &trace->DVmaxBus);
        trace->query &= ~(QUERY_DVLT);
    }
    if (strstr(sp, "DV_MIN_BUS_PU")!=NULL) {
        sscanf(tp, "%f", &trace->DVminBus);
        trace->query &= ~(QUERY_DVLT);
    }
    if (strstr(sp, "DV_MIN_EH5_PU")!=NULL) {
        sscanf(tp, "%f", &trace->DVmin500EH);
        trace->query &= ~(QUERY_DVLT);
    }
    if (strstr(sp, "DV_MIN_EHV_PU")!=NULL) {
        sscanf(tp, "%f", &trace->DVminBusEH);
        trace->query &= ~(QUERY_DVLT);
    }
    if (strstr(sp, "OUTG_CON_FILE"    )!=NULL) {
        sscanf(tp, "%s", trace->conSpec);
        trace->query &= ~(QUERY_FILE);
    }
    if (strstr(sp, "CHECK_TX_THERMAL")!=NULL) {
        sscanf(tp, "%3s", t);
        if (strstr(t, "NO" )!=NULL) trace->chek &= ~CHEK_TXTH;
        if (strstr(t, "YES")!=NULL) trace->chek |=  CHEK_TXTH;
        trace->query &= ~(QUERY_TXTH);
    }
    if (strstr(sp, "REPORT_0_RATINGS")!=NULL) {
        sscanf(tp, "%3s", t);
        if (strstr(t, "NO" )!=NULL) trace->chek &= ~CHEK_ZRAT;
        if (strstr(t, "YES")!=NULL) trace->chek |=  CHEK_ZRAT;
        trace->query &= ~(QUERY_ZRAT);
    }
  }
}
void ff_report(char *s, Trace *trace)
{
  char *sp, *tp;
  for (sp = strtok(s, " ,="); sp!=NULL; sp = strtok(NULL, " ,=")) {
    tp=strtok(NULL, ", ="); if (tp==NULL) return;
    if (strstr(sp, "NAME")!=NULL) {
        strcpy(trace->outName, tp);
        trace->query &= ~(QUERY_NAME);
        continue;
    }
    if (strstr(sp, "APPLY"           )!=NULL) {
        sscanf(tp, "%d", &trace->aply);
        trace->query &= ~(QUERY_APLY);
    }
    if (strstr(sp, "PLOT")!=NULL) {
        if (strstr(tp, "PRINT")!=NULL) trace->plot |= PLOT_PRNT;
        if (strstr(tp, "SAVE" )!=NULL) trace->plot |= PLOT_SAVE;
        if (strstr(tp, "DIFF" )!=NULL) trace->plot |= PLOT_DIFF;
        if (strstr(tp, "NONE" )!=NULL) trace->plot = 0;
        trace->query &= ~(QUERY_PLOT);
        if (trace->plot==0) trace->query &= ~(QUERY_CORD);
        continue;
    }
    if (strstr(sp, "CHECK"  )!=NULL) {
        while (tp!=NULL) {
            if (strstr(tp, "CHECK_VOLTAGE"  )!=NULL) trace->chek |= CHEK_VOLT;
            if (strstr(tp, "CHECK_LOADING"  )!=NULL) trace->chek |= CHEK_LOAD;
            if (strstr(tp, "CHECK_TAPS"     )!=NULL) trace->chek |= CHEK_TAPS;
            if (strstr(tp, "COMPARE_VOLTAGE")!=NULL) trace->chek |= CHEK_DVLT;
            if (strstr(tp, "COMPARE_LOADING")!=NULL) trace->chek |= CHEK_DLDG;
            if (strstr(tp, "COMPARE_TAPS"   )!=NULL) trace->chek |= CHEK_DTAP;
            if (strstr(tp, "OUTAGE_SIM"     )!=NULL) trace->chek |= CHEK_OUTG;
            if (strstr(tp, "NONE"           )!=NULL) trace->chek =  0;
            tp = strtok(NULL, ", ");
        }
        if ( !(trace->chek & (CHEK_DVLT | CHEK_DLDG | CHEK_DTAP)) )
            trace->query &= ~(QUERY_REFB);
        trace->query &= ~(QUERY_CHEK | QUERY_COMP | QUERY_OUTG);
    }
    if (strstr(sp, "TRACE")!=NULL) {
        if (strstr(tp, "YES")!=NULL) trace->trc->type = TRACE_YES;
        if (strstr(tp, "NO" )!=NULL) trace->trc->type = TRACE_NO;
        continue;
    }
    if (strstr(sp, "LIST" )!=NULL) {
/* currently this option defaults to LIST_NO and must be set in the .trc file */
        if (strstr(tp, "YES")!=NULL) trace->lis->type = LIST_YES;
        if (strstr(tp, "NO" )!=NULL) trace->lis->type = LIST_NO;
        trace->query &= ~(QUERY__LIS);
    }
    if (strstr(sp, "SAVE_NEW_BASECASES"  )!=NULL) {
        if (strstr(tp, "YES")!=NULL) trace->save = SAVE_YES;
        if (strstr(tp, "NO" )!=NULL) trace->save = SAVE_NO;
        if (trace->save == SAVE_NO) trace->query &= ~(QUERY_NEWB);
        trace->query &= ~(QUERY_SAVE);
    }
    if (strstr(sp, "SHOW_CHANGES"  )!=NULL) {
        if (strstr(tp, "YES")!=NULL) trace->show = SHOW_YES;
        if (strstr(tp, "NO" )!=NULL) trace->show = SHOW_NO;
        trace->query &= ~(QUERY_SHOW);
    }
  }
}
int srtRow(Link *newRow, Link *xstRow, Trace *trace)
{
  return srtID((Link *) newRow->data, (Link *) xstRow->data, trace->sort);
}
int srtID(Link *newLink, Link *xstLink, int sort)
{
  pf_rec *newData, *xstData;

  if (newLink==NULL || xstLink==NULL) return 1;
  if (newLink->data==NULL || xstLink->data==NULL) return -1;
  newData = (pf_rec *) newLink->data;
  xstData = (pf_rec *) xstLink->data;
  if (newData->i.branch.type[0]=='B') {                       /* bus u/o volt */
    return srtBus(&newData->i.ACbus, &xstData->i.ACbus, sort);
  }
  else {                                /* branch outage or overloaded branch */
    return srtBranch(&newData->i.branch, &xstData->i.branch, sort);
  }
}
int srtBus(pf_AC_bus *b1, pf_AC_bus *b2, int sort) /* sort bus */
{
  int c;
  float d;
  if ( (c=strncmp(b1->type, b2->type, 1)) != 0 ) return c;     /* [type]-sort */
  if (sort==SORT_BY_ZONE) {                                /* [zone]-alpha-kv */
    if ( (c=strcmp(b1->zone, b2->zone)) != 0 ) return c;
  }
  else if (sort==SORT_BY_OWNER) {                         /* [owner]-alpha-kv */
    if ( (c=strcmp(b1->owner, b2->owner)) != 0 ) return c;
  }
  else if (sort==SORT_BY_BASEKV) {                           /* [kv]-alpha-kv */
    if ( fabs(d = b1->kv - b2->kv) > .001 ) return d;
  }
  return cf_cmpBus(b1, b2);                                        /* [alpha-kv] */
}
int srtBranch(pf_branch *b1, pf_branch *b2, int sort) /* sort branch */
{
  float c;
  if ( c=strncmp(b1->type, b2->type, 1) ) return c>0 ? 1 : -1; /* [type]-sort */
  if (sort==SORT_BY_OWNER) {                              /* [owner]-alpha-kv */
    if ( c=strcmp(b1->owner, b2->owner) ) return c>0 ? 1 : -1;
  }
  else if (sort==SORT_BY_BASEKV) {                           /* [kv]-alpha-kv */
    if ( fabs(c=b1->bus1_kv-b2->bus1_kv) > .001 ) return c>0 ? 1 : -1;
    if ( fabs(c=b1->bus2_kv-b2->bus2_kv) > .001 ) return c>0 ? 1 : -1;
  }
  return cf_cmpBranch(b1, b2);                                  /* [alpha-kv] */
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
/************ documented, common CF_UTIL.H candidates prototypes **************/
/* all documented, CF_UTIL.H candidates shall be designated, cf_name().       */
/******** end documented, common CF_UTIL.H candidates prototypes **************/
/* hash:  form hash value for string s */
unsigned hash(char *s)
{
    unsigned hashval;

    for (hashval = 0; *s != '\0'; s++)
        hashval = *s + 31 * hashval;
    return hashval % HASHSIZE;
}
/* lookup:  look for s in hashtab */
struct nlist *lookup(char *s)
{
    struct nlist *np;

    for (np = hashtab[hash(s)]; np != NULL; np = np->next)
        if (strcmp(s, np->id) == 0)
            return np;  /* found */
    return NULL;        /* not found */
}
/* strrep:  make a replicate of s */ /* formerly duplicate, strdup */
char *strrep(char *s)
{
    char *p;

    p = (char *) malloc(strlen(s)+1); /* +1 for '\0' */
    if (p != NULL)
        strcpy(p, s);
    return p;
}
/* install: put (id, tag1, num1, val1, val2) in hash tab */
struct nlist *install(char *id, char *tag1, int num1, float val1, float val2)
{
    struct nlist *np;
    unsigned hashval;

    if ((np = lookup(id)) == NULL) { /* not found */
        np = (struct nlist *) malloc(sizeof(*np));
        if (np == NULL || (np->id = strrep(id)) == NULL)
            return NULL;
        hashval = hash(id);
        np->next = hashtab[hashval];
        hashtab[hashval] = np;
    }
    strcpy(np->tag1, tag1);
    np->num1 = num1;
    np->val1 = val1;
    np->val2 = val2;
    return np;
}
void  cleanUpFiles(Trace *trace)
{
  char s[CF_INBUFSIZE];

  if ( !(trace->stat & FAIL_CRIT) ) {
    sprintf(s, "%s USER_ANA.SCR", FILE_PURG);  system(s);
    sprintf(s, "%s %s.PFO", FILE_PURG, trace->userID); system(s);
    sprintf(s, "%s %s.PFD", FILE_PURG, trace->userID); system(s);
  }
  if (trace->plot && !(trace->plot & PLOT_SAVE)) {
        sprintf(s, "%s mimic.ps", FILE_PURG); system(s);
  }
  sprintf(s, "%s scratch.1%s", FILE_KILL, FILE_SUFX); system(s); 
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
void de_load_changes(int *stat, int use, char *file, int fail)
{
  if (*stat & FAIL_CRIT) return;
  if (strlen(file)==0) return;
  if (!use) return;
  printf("  Applying Changes... %s\n", file);
  if (pf_load_changes(file)!=0) {
    cf_logErr("Skipping changes:  Cannot use %s\n", file);
    cf_logErr("%s\n", err_buf);
    *stat |= fail;  /* fail = FAIL_PCHG or FAIL_ACHG */
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
  pf_rec_branch(&b, "D");                                /* take outage */
  return;
}
void de_take_como_outg(int *stat, int type, char *comOutg, FILE *comFile)
{
  char s[CF_RECSIZE], cmd[CF_IPCSIZE];
  int  mode;

  if (*stat & FAIL_CRIT) return;
  if (type!=OUTG_COMO) return;
  printf("  Taking Common-mode Outage... %s\n", comOutg);
  mode = 0;                                      /* searching */
  rewind(comFile);
  strcpy(cmd, "/changes, file = *\n");
  while (fgets(s, CF_INBUFSIZE, comFile)!=NULL) {
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
void de_comments(int *stat, int use, Step *step)
{
  pf_comments cr;

  if (*stat & FAIL_CRIT) return;
  if (!use) return;
  memset(&cr, '\0', sizeof(pf_comments));
  strncpy(&cr.case_name[0], step->newBase, 10);
  strncpy(&cr.case_desc[0], step->priChng, 14);
  if (step->useAuxc) strcat(cr.case_desc, "+....");
  pf_rec_comments(&cr, "M");
  return;
}
void de_save_newbase(int *stat, int use, char *file)
{
  if (*stat & FAIL_CRIT) return;
  if (strlen(file)==0) return;
  if (!use) return;
  printf("  Saving New Base... %s\n", file);
  if (pf_save_newbase(file)!=0) {
    cf_logErr("Skipping save base: Cannot save %s\n", file);
    cf_logErr("%s\n", err_buf);
    *stat |= FAIL_SAVE;
  }
}
void de_solution(int *stat, int use, char *solution)
{
  if (*stat & FAIL_CRIT) return;
  if (strlen(solution)==0 && !use) return;
  printf("\n  Solving Basecase...\n");
  if (strlen(solution)!=0) {
    printf("%s\n", solution);
    if (pf_command(solution)!=0) {
        cf_logErr("de_solution:  Cannot solve basecase\n");
        cf_logErr("%s\n", err_buf);
        *stat |= FAIL_SOLV;
        *stat |= FAIL_CRIT;
    }
  }
  else {
    if (pf_solution()!=0) {
        cf_logErr("de_solution:  Cannot solve basecase\n");
        cf_logErr("%s\n", err_buf);
        *stat |= FAIL_SOLV;
        *stat |= FAIL_CRIT;
    }
  }
  return;
}
void de_load_refbase(int *stat, int chek, int plot, char *file)
{
  if (*stat & FAIL_CRIT) return;
  if (strlen(file)==0) return;
  if (!(chek & (CHEK_DVLT|CHEK_DLDG|CHEK_DTAP) || plot & PLOT_DIFF)) return;
  printf("  Loading Reference Base... %s\n", file);
  if (pf_load_refbase(file)!=0) {
    cf_logErr("de_load_refbase: Cannot load reference base %s\n", file);
    *stat |= FAIL_LDRF; /* failed to load reference base */
  }
  return;
}
void de_plot(int *stat, int use, char *corFile, char *ps_File)
{
  if (*stat & FAIL_CRIT) return;
  if (!use) return;
  pf_plot(corFile, ps_File, "");
  return;
}
void de_check_plot_queue(int *stat, int use)
{
  if (*stat & FAIL_CRIT) return;
  if (!use) return;
  if (getenv("CFLOW_PSQ")==NULL) {
    printf("  CFLOW_PSQ undefined!\n");
    printf("--In your login file define something like: CFLOW_PSQ :== PRINT/QUE=PS_EOHQMS\n");
    *stat |= FAIL_CPSQ;
  }
  return;
}
void de_submit_plot(int *stat, int use, char *file)
{
  char s[CF_INBUFSIZE];

  if (*stat & FAIL_CRIT) return;
  if (*stat & FAIL_CPSQ) return;
  if (strlen(file)==0) return;
  if (!use) return;
  printf("Submitting Plot...\n");
  sprintf(s, "CFLOW_PSQ %s", file);
  system(s);
  return;
}
void de_outage_sim(int *stat, int use, char *inFile, char *outFile)
{
  char s[CF_INBUFSIZE];

  if (*stat & FAIL_CRIT) return;
  if (strlen(inFile)==0) return;
  if (strlen(outFile)==0) return;
  if (!(use & CHEK_OUTG)) return;

  sprintf(s, "/OUTAGE_SIMULATION, FILE=%s, OUTFILE=%s\n", inFile, outFile);
  printf(s);
  if (pf_command(s)!=0) {
    cf_logErr(s);
    cf_logErr("de_command:  Command Failed!\n");
    *stat |= FAIL_CMMD;
  }
  return;
}
void de_bus_list(int *stat, int use, Trace *trace)
{
  if (*stat & FAIL_CRIT) return;
  if (!(use & (CHEK_VOLT | CHEK_DVLT))) return;

  printf("  Building Bus List...\n");
  if (trace->get & GET_BUS_LIS) {
    buildBusList(trace);
  }
  if (trace->get & GET_BUS_ZON) {  /* needed once for inputed lists */
    getBusZones(trace, &trace->busList);
  }
  trace->get &= ~(GET_BUS_ZON);
  return;
}
void de_branch_list(int *stat, int use, Trace *trace)
{
  if (*stat & FAIL_CRIT) return;
  if (!(use & (CHEK_LOAD | CHEK_DLDG | CHEK_TAPS | CHEK_DTAP))) return;

  printf("  Building Branch List...\n");
  if (trace->get & GET_BRN_LIS) {
    buildBranchList(trace);
  }
  if (trace->get & GET_BRN_RAT) {
    getBranchRatings(trace, &trace->lineList);
    getBranchRatings(trace, &trace->xfmrList);
  }
  if (trace->get & (GET_BRN_ZON | GET_BRN_LIS)) {  /* once for list */
    getBranchZones(trace, &trace->lineList);
    getBranchZones(trace, &trace->xfmrList);
  }
  trace->get &= ~(GET_BRN_ZON);
  return;
}
