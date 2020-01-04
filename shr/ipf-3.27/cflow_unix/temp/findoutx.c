/******************************************************************************\
UTILITY:    FINDOUT
STRUCTURE:  common CFLOW architecture.
TYPE:       Powerflow (BPF) output report post-processor.
SUMMARY:    Generates a table of outages and corresponding branch overloads or
            bus voltage violations from multiple pfo files. Sorts and screens.
RELATED:    LINEFLOW, MIMIC, CFUSE
SEE ALSO:   TO_DECWRITE.HELP, VMS_TO_PC_TO_EXCEL.HELP 
UPDATED:    April 2, 1997 
LANGUAGE:   Standard C.  CFLOW Library.  cf_util.h 
DEVELOPER:  William D. Rogers, BPA, TOP, 230-3806, wdrogers@bpa.gov
REQUESTER:  IPF User Group (L. Stadler, P. Larguier, K. Kohne) 
USERS:      Kyle Kohne, Larry Stadler, Dorothy Betzing, Kendall Rydell
IPF:        version 317 or above recommended; also works with PTI
PURPOSE:    Works with the .pfo output files of /OUTAGE_SIMULATION runs as a 
            post-processor to screen and sort the results and present them in 
            tabular form.  Tables of 'Outages and Overloads' or 'Outages and 
            Bus Violations' can be produced.  Entries in these tables can be
            screened according to Zone, Owner, Base kV, Loading and Bus Voltage.
            Tables can be sorted by Zone, Owner, Base kV, or alphabetically. The
            idea is to allow the user to automate the creation of a report 
            detailing the results of outages--saving time and reducing errors 
            from the current method of doing so which generally includes cut and
            paste operation with an editor.

            Data fields in the output report table are character delimited to 
            ease importing to MS Excel or DECwrite.
\******************************************************************************/
/******************************* #include *************************************/
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>

#include "cflowlib.h"
/***************************** end #include ***********************************/
/******************************************************************************\
  cf_util.h library
  Developed by William D. Rogers for use with Bonneville Power Administration's
  CFLOW library and Interactive Powerflow (IPF) program.

  Naming conventions: (suggested, note case)
    Cf_macro, Cf_macroName
    cf_function, cf_functionName
    CF_CONSTANT, CF_CONSTANTNAME
    CF_globalVariable
    cf_Structure, Structure
    Cx_, CX, cx,... legacy functions to be retired

  Library organization: (suggested)
    #include
    #define macros
    #define constants
    typedef struct
    global variables
    function prototypes
    functions
\******************************************************************************/

#define Cf_min(A, B) ((A) < (B) ? (A) : (B))
#define Cf_max(A, B) ((A) > (B) ? (A) : (B))
#define Cf_ratio(N, D) ((D) > 0 ? (N) / (D) : 0)
#define Cf_imod(N, D) (int) (fmod((double) N, (double) D))

#define CF_STRSIZE      133
#define CF_INPFOSIZE    135      /* for reading Fortran-generated *.PFO files */
#define CF_INBUFSIZE    250
#define CF_OUTBUFSIZE   250
#define CF_RECSIZE      300     /* maximum length of input or solution record */
#define CF_IPCSIZE     8192                             /* CFLOW_IPC_BUS_SIZE */
#define CF_COMMENT      '.'
#define CF_TRUNCATE     "\n\\"
#define CF_DATATYPES    ".DAT,.TRC,.LIS,.XYC"
#define CF_TOKSEP       ", \t"
#define CF_HISSPEC      "CFPROGS:CFLOW.LOG"       /* CFLOW usage history log */
#define CF_TIMESTAMP    "%d-%b-%Y %H:%M:%S"
#define CF_PAGE_FOOTER  "\n%-122.122s PAGE %4d\n"

#define CF_KIND_LINK      1    /* Link */
#define CF_KIND_STR       2    /* string */
#define CF_KIND_REC       3    /* pf_rec structure */
#define CF_KIND_BUS       4    /* cf_Bus structure */
#define CF_KIND_BRN       5    /* cf_Branch structure */
#define CF_KIND_CHAR      6    /* single character */
#define CF_KIND_INT       7    /* integer */
#define CF_KIND_FLT       8    /* floating point number */
#define CF_KIND_TAG       9    /* tag string for list */
#define CF_KIND_NAME     10    /* cf_Name structure */
#define CF_KIND_GEN      11    /* ff_Gen structure */
#define CF_KIND_SF2      12    /* cf_StrFlt2 (file[80], float, float) structure */
#define CF_KIND_STRSTR   13    /* cf_StrStr (str1[80], str2[80]) structure */

#define CF_TAG_W_LIST     0    /* print tag only if there is a list */
#define CF_TAG_ALWAYS     1    /* always print tag, even with List==NULL */
#define CF_INS_FREE       1    /* discard new links if same as existing links */
#define CF_INS_SKIP       2    /* don't insert new links but keep them */
#define CF_INS_AFTER      3    /* insert new links after current link */
#define CF_INS_BEFORE     4    /* insert new links berfore current link */

typedef struct LinkRecord {
    struct LinkRecord *prev;    /* previous Cell record */
    size_t             size;    /* amount of memory allocated for data */
    int                kind;    /* type of data structure pointed to by *data */
    int                view;    /* code for how to print out data */
    void              *data;    /* pointer to allocated memory */
    struct LinkRecord *next;    /* next Cell record */
} Link; /* cf_Link */

typedef struct ListStyleRecord {
  char   indent[10]; /* leading characters on each data line */
  int    mode;       /* 0 - always print tag, 1 - print only if links present */
  int    limit;      /* number of links to print per line */
} cf_Style;

typedef struct NameIdRecord {
  char   type[3];
  char   name[39];
} cf_Name;

typedef struct BranchIdRecord {
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
} cf_Branch; /* modified form of pf_branch */

typedef struct BusIdRecord {
    char   type[3];
    char   owner[4];
    char   name[9];
    float  kv;
    char   zone[3];
} cf_Bus; /* short form of pf_AC_bus and pf_DC_bus */

typedef struct OutputReportRecord {
  FILE  *file;
  int    line;
  int    page;
  char  *time;
  char   spec[FILENAME_MAX];
  int    type;
  int    form;
  int    wide;
  int    diff;
} cf_Out;

typedef struct GeneratorLimitsRecord {
  float  set;   /* current set value (MW) */
  float  start; /* fixed starting value (MW) */
  float  stop;  /* fixed stopping value (MW) */
  float  step;  /* fixed step size (MW) */
} cf_Gen;

typedef struct StringFloat2 {
  char   file[80];   /* 80 character string (developed for filename) */
  float  flt1;       /* floating point number */
  float  flt2;       /* floating point number */
} cf_StrFlt2;

typedef struct StringString {
  char   str1[80];   /* 80 character string (developed for filename) */
  char   str2[80];   /* 80 character string  */
} cf_StrStr;


FILE   *CF_logFile = NULL;
FILE   *CF_hisFile = NULL;         /* CFLOW usage history file */
char    CF_logSpec[FILENAME_MAX] = "";

/****************** add stuff for query and BAD (aka ZAP) logs *******************/
FILE   *CF_logQFile = NULL;
char    CF_logQSpec[FILENAME_MAX] = "";
FILE   *CF_logZFile = NULL;
char    CF_logZSpec[FILENAME_MAX] = "";
/****************** add stuff for query and BAD logs *******************/

char    CF_inbuf[CF_INBUFSIZE];
long    CF_memCurAlloc = 0;        /* current bytes allocated at present time */
long    CF_memMaxAlloc = 0;        /* maximum bytes allocated at any one time */
long    CF_memTotAlloc = 0;        /* total bytes allocated so far */
long    CF_memTotFreed = 0;        /* total bytes de-allocated (freed) so far */
time_t  CF_time0 = 0;
time_t  CF_time1 = 0;

cf_Style CF_oneStyl = { "  ", CF_TAG_ALWAYS,  1 };  /* filenames, trace data */
cf_Style CF_sixStyl = { "  ", CF_TAG_ALWAYS,  6 };  /* base kv */
cf_Style CF_dznStyl = { "  ", CF_TAG_ALWAYS, 12 };  /* zones, owners */
cf_Style CF_dotStyl = { ". ", CF_TAG_ALWAYS,  1 };  /* comments, trace data */
cf_Style CF_recStyl = { "",   CF_TAG_ALWAYS,  1 };  /* branches, buses, names */
cf_Style CF_lisStyl = { "",   CF_TAG_W_LIST,  1 };  /* branches, buses, masks */

void    cf_cprompt(char *prompt, char dflt, char *reply);
void    cf_sprompt(char *prompt, char *dflt, char  *reply);
void    cf_mprompt(char *prompt, char *dflt, char *mask, char *reply);
void    cf_nprompt(char *prompt, char *dflt, int n, char *reply);
void    cf_kprompt(char *prompt, char dflt, char *reply);
void    cf_fprompt(char *prompt, float dflt, float *reply);
void    cf_dprompt(char *prompt, double dflt, double *reply);
void    cf_iprompt(char *prompt, int   dflt, int   *reply);
int     cf_yprompt(char *prompt, int   dflt);
void    cf_eprint(FILE *fp, char *fmt, ...);
void    cf_aprint(char *target, char *fmt, ...);
void    cf_exit(int error, char *fmt, ...);
void    cf_pfexit(int error, char *fmt, ...);
void    cf_stoupper(char *str);
int     cf_str2list(char *list, int group, char delim);
char   *cf_strsuf(char *dest, char *source, char term, char *suffix);
int     cf_strcon(char *s1, char *s2);
char   *cf_parse(char *spec, char *part, int code);
int     cf_query2spec(char *spec, char *query, char *type);
FILE   *cf_openFile(char *spec, char *mode);

/****************** add stuff here ***************************/
void    cf_logErr(char *fmt, ...);
void   *cf_calloc(size_t nobj, size_t size);
void    cf_free(void *obj, size_t size);
size_t  cf_time(char *s, size_t smax, char *format);
char   *cf_str2upper(char *str);
char   *cf_str2lower(char *str);
int     cf_isblank(char *cp);
int     cf_iscomment(char *str);
int     cf_isDataFile(char *filename);
Link   *cf_newLink(size_t size);
int     cf_cntLink(Link *cur);
Link   *cf_addLink(Link *tar, size_t size);
Link   *cf_delLink(Link **top, Link *del);
Link   *cf_appList(Link **top, Link *src);
Link   *cf_dupList(Link *src);
void    cf_freeList(Link *cur);
Link   *cf_link2row(Link *dat);
void    cf_freeRow(Link *row);
Link   *cf_delRow(Link **rowList, Link *curRow);
int     cf_rec_l2h(pf_branch *r);
int     cf_rec_h2l(pf_branch *r);
int     cf_cmpBus(pf_AC_bus *b1, pf_AC_bus *b2);
int     cf_cmpBranch(pf_branch *b1, pf_branch *b2);
Link   *cf_text2Link(char *s);
Link   *cf_text2List(char *s);
int     cf_validOwner(Link *ownrList, char *owner);
int     cf_validBasekv(Link *bskvList, float bus1_kv, float bus2_kv);
int     cf_validZone(Link *zoneList, char *z1, char *z2);
void   *cf_malloc(size_t size);
void    cf_logUse(char *prog, char *vers, char *user);
char   *cf_strsub(char *str, char *sub, char fill);
void    cf_freeRowList(Link **rowList);
void    cf_bus2rec(cf_Bus *bus, pf_rec *rec);
void    cf_rec2bus(pf_rec *rec, cf_Bus *bus);
void    cf_branch2rec(cf_Branch *branch, pf_rec *rec);
void    cf_rec2branch(pf_rec *rec, cf_Branch *branch);
Link   *cf_id2Link(char *s, char order);
int     cf_cntchr(char *str, char chr);
char   *cf_strdup(char *str);
int     cf_branch_l2h(cf_Branch *b);
Link   *cf_rec2Link(char *s);
cf_Out *cf_initOut(char *fs, char *ts, int ty, int fm);
float   cf_getRating(pf_rec *n, int tx, char *code);
float   cf_getLoad(pf_rec *n);
int     cf_printGroup(FILE *fp, Link *list, cf_Style style, char *tag);
int     cf_printList(FILE *fp, Link *list, cf_Style style, char *tag);
int     cf_printLink(FILE *fp, Link *link);
int     cf_printTag(FILE *fp, Link *list, cf_Style style, char *tag);
char   *cf_link2tagName(Link *tagLink, char *tagName);
Link   *cf_tag2link(char *tag);
void    cf_fillGroup(char *mstrSpec, Link **grpList, char *key);
void    cf_insLink(Link **top, Link **cur, Link *new, long sort, int mode,
            int (*cmp)(Link *, Link *, int));
void    cf_insBefore(Link **cur, Link **new, Link **top);
void    cf_insAfter(Link **cur, Link **new);

int     cx_printList(FILE *fp, Link *list, char *tag, char *indent, char type,
            char *fmt, int n);
int     cx_printRowList(FILE *fp, Link *list, char *list_tag, char *row_tag,
         char type, char *fmt, int n);

/******************************* #define **************************************/

#define  FF_IPF_VERSION   320            /* GPF.EXE_V320 or above recommended */
#define  FF_PAGE_LENGTH   61
#define  DOT_LINE         "..................................................."
#define  FF_WILD          '?'
#define  LOG_SPEC         "findout.log"
#define  OUT_NAME         "findout"
#define  DFLT_CASE_PER_PAGE 2
#define  DFLT_MIN_LOAD_PCT  80.00
#define  DFLT_MAX_DELTA_PCT 0.0
#define  DFLT_MAX_500_PU    1.100
#define  DFLT_MIN_500_PU    1.000
#define  DFLT_MAX_BUS_PU    1.052
#define  DFLT_MIN_BUS_PU    0.950
#define  QUERY_PFO  (int) (1<< 0)  /* prompt for *.pfo files */
#define  QUERY_DATA (int) (1<< 1)  /* prompt for data file listing branch/bus*/
#define  QUERY_SOLN (int) (1<< 2)  /* prompt for including sol'n problem data*/
#define  QUERY_OWNR (int) (1<< 3)  /* prompt for owners of interest */
#define  QUERY_ZONE (int) (1<< 4)  /* prompt for zones of interest */
#define  QUERY_BSKV (int) (1<< 5)  /* prompt for base kV of interest */
#define  QUERY_LOAD (int) (1<< 6)  /* prompt for MAX PCT LOADING */
#define  QUERY_SORT (int) (1<< 7)  /* prompt for sort order */
#define  QUERY_FORM (int) (1<< 8)  /* prompt for report format */
#define  QUERY_NAME (int) (1<< 9)  /* prompt for report name */
#define  QUERY_WDTH (int) (1<<10)  /* prompt for report width in cases/table */
#define  QUERY__TRC (int) (1<<11)  /* prompt for whether or not to make .TRC */
#define  QUERY_CONT (int) (1<<12)  /* prompt for continuation of run */
#define  QUERY_TYPE (int) (1<<13)  /* prompt for continuation of run */
#define  QUERY_VOLT (int) (1<<14)  /* prompt for continuation of run */
#define  QUERY_OUTG (int) (1<<15)  /* prompt for list of branches/busses */
#define  QUERY_COMO (int) (1<<16)  /* prompt for list of branches/busses */
#define  QUERY_OVLD (int) (1<<17)  /* prompt for list of branches/busses */
#define  QUERY_BUSV (int) (1<<18)  /* prompt for list of branches/busses */
#define  QUERY_OW_P (int) (1<<19)  /* prompt for owners of interest */
#define  QUERY_ZN_P (int) (1<<20)  /* prompt for zones of interest */
#define  QUERY_KV_P (int) (1<<21)  /* prompt for base kV of interest */
#define  QUERY_DLTA (int) (1<<22)  /* prompt for max change in load or volt */
#define  QUERY_DIFF (int) (1<<23)  /* prompt for whether to do difference rpt */
#define  QUERY_REDU (int) (1<<24)  /* prompt for removal of redundant outages */
#define  QUERY_VARI (int) (1<<25)  /* prompt for whether to do variance rpt */
#define  QUERY_SPAN (int) (1<<26)  /* prompt for variance span  */
#define  READ_INC        (int)    1 /* found /INCLUDE card in CFLOW data file */
#define  READ_PFO        (int)    2 /* found /PFO     card in CFLOW data file */
#define  READ_OUT        (int)    3 /* found /OUTAGE  card in CFLOW data file */
#define  READ_COM        (int)    4 /* found /COMMON_MODE  in CFLOW data file */
#define  READ_OVR        (int)    5 /* found /OVERLOAD     in CFLOW data file */
#define  READ_BUS        (int)    6 /* found /BUS     card in CFLOW data file */
#define  READ_OWN        (int)    8 /* found /OUTG_OWNER   in CFLOW data file */
#define  READ_ZON        (int)    9 /* found /OUTG_ZONE    in CFLOW data file */
#define  READ_BKV        (int)   10 /* found /OUTG_BASE_KV in CFLOW data file */
#define  READ_LIM        (int)   11 /* found /LIMITS  card in CFLOW data file */
#define  READ_REP        (int)   12 /* found /REPORT  card in CFLOW data file */
#define  READ_OWP        (int)   13 /* found /PROB_OWNER   in CFLOW data file */
#define  READ_ZNP        (int)   14 /* found /PROB_ZONE    in CFLOW data file */
#define  READ_KVP        (int)   15 /* found /PROB_BASE_KV in CFLOW data file */
#define  SORT_BY_ALPHA   (int)    1
#define  SORT_BY_BASEKV  (int)    2
#define  SORT_BY_OWNER   (int)    3
#define  SORT_BY_ZONE    (int)    4
#define  SORT_BY_INPUT   (int)    5
#define  SORT_BY_SOLN    (int)    6
#define  SORT_BY_DIFF    (int)    7
#define  VARI_NO         (int)    0
#define  VARI_YES        (int)    1
#define  TRACE_NO        (int)    0
#define  TRACE_YES       (int)    1
#define  TYPE_NONE       (int)    0
#define  TYPE_TRACE      (int)    1
#define  TYPE_OUTG_OVLD  (int)    1
#define  TYPE_OUTG_BUSV  (int)    2
#define  TYPE_OVLD_OUTG  (int)    3
#define  TYPE_BUSV_OUTG  (int)    4
#define  TYPE_OVLD_BOTH  (int)    5
#define  TYPE_BUSV_BOTH  (int)    6
#define  REDUN_KEEP      (int)    0
#define  REDUN_REMOVE    (int)    1
#define  SKP_SOLN_PROB   (int)    0  /* skip reporting of solution problems */
#define  INC_SOLN_PROB   (int)    1  /* include report of solution problems */
#define  FORM_DATA_FULL  (int)    1
#define  FORM_DATA_BREF  (int)    2  /* BRIEF DATA FORMAT */
#define  FORM_PAGE_WIDE  (int)    3
#define  FORM_PAGE_COMP  (int)    4  /* COMPACT PAGE FORMAT */
#define  HEADER_BREAK_PTI  "................................................................................"
#define  HEADER_COMME_PTI  ".               "
#define  DIS_FAC_FILE_PTI  "DISTRIBUTION FACTOR FILE:"
#define  SUB_DES_FILE_PTI  "SUBSYSTEM DESCRIPTION FILE:"
#define  MON_ELE_FILE_PTI  "MONITORED ELEMENT FILE:"
#define  CON_DES_FILE_PTI  "CONTINGENCY DESCRIPTION FILE:"
#define  CONTIN_EVENT_PTI  "--- C O N T I N G E N C Y   E V E N T S ---"
#define  OPEN_LINE_FR_PTI  "OPEN LINE FROM BUS"
#define  BPA_PF_PROG_VERS  "BPA POWER FLOW PROGRAM VERSION:IPF"
#define  PTI_INTERAC_PSSE  "PTI INTERACTIVE POWER SYSTEM SIMULATOR--PSS/E"
#define  SUM_PROBS_4E_OUT  "Summary of bus and line problems for each outage"
#define  SUM_SYST_SEP_DIV  "Summary of System Separations and Divergences"
#define  SUM_COM_MODE_OUT  "Summary of Common Mode Outages"
#define  OUTG_SIMUL_INPUT  "Outage Simulation Input"
#define  BUS_BUS_VPU_PTI   "X------ BUS -----X  V(PU)"
#define  BUS_VV_A_PU_316   "Bus voltage violation                             Actual    Per unit  Actual / change /(    limits   )"
#define  BUS_VV_A_PU_312   "Bus voltage violation                           actual      per unit actual/(    limits   )"
#define  CONTINGENCY_ANAL  " C O N T I N G E N C Y   A N A L Y S I S "
#define  __O_U_T_A_G_E__   "* * O U T A G E * *"
#define  OVLD_FRO_TO_PTI   "FROM     NAME        TO     NAME    CKT  PRE-CNT  POST-CNT  RATING  PERCENT" 
#define  OVLD_RAT_TY_316   "Overloads                                         Rating    Type    per unit / pst-cont  pre-cont Date in" 
#define  OVLD_RAT_TY_314   "Overloads                                             Rating     Type      per unit / actual"
#define  OVLD_RAT_TY_312   "Overloads                                   Rating     Type      per unit / actual"
#define  OUTG_ID_OWN_314   "--------   Outage  ------------- ID  Owner  Zones  -------  Problem"
#define  OUTG_ID_OWN_312   "--------   Outage  ------------- ID  Owner -------  Problem"
/*********************  stuff to follow powerflow changes **********************/
/* #define  COMO_MOD_NA_312  " %*d%*4c%38[^\n]" */
#define  COMO_MOD_NA_312  " %*d%*3c%38[^\n]"
/*********************  stuff to follow powerflow changes **********************/
#define  TY_CH_OWN_Z_315   "Ty  Ch  Own  Zones  Bus1           Bus2"
#define  TY_CH_OWN_B_312   "Ty  Ch  Own  Bus1           Bus2"
#define  BUSV_FORMAT_PTI   "%*60c%8[^\n]%f%*c%f"
#define  BUSV_FORM_2_PTI   "%*60c%*26c%8[^\n]%f%*c%f"
#define  BUSV_FORM_3_PTI   "%*60c%*26c%*26c%8[^\n]%f%*c%f"
#define  BUSV_FORMAT_316   " %8[^\n]%*c%f%*2c%3[^\n]%*2c%2[^\n]%*24c %f KV %f / %f /( %f, %f)"
#define  BUSV_FORMAT_312   " %8[^\n]%*c%f%*2c%3[^\n]%*2c%2[^\n]%*21c %f KV %f /( %f, %f)" /* extra %f add for alignment */
#define  COMO_FORMAT_314   "%*6c%2[^\n]%*2c%c%*3c%3[^\n]%*2c%2[^\n]%*c%2[^\n]%*2c%*8[^\n]%f %*8[^\n]%f"
#define  COMO_FORMAT_312   "%*6c%2[^\n]%*2c%c%*3c%3[^\n]%1[^\n]%1[^\n]%*8[^\n]%f %*8[^\n]%f"
#define  OVLD_FORMAT_PTI   "%*61c%8[^\n]%f%*1c%*6c%8[^\n]%f%*1c%c %f %f %f %f"
#define  OVLD_FORMAT_316   "%*24c %8[^\n]%f %8[^\n]%f%*1c%c%*1c%d%*2c%3[^\n]%*2c%2[^\n]%*c%2[^\n] %f %*3[^\n]  %c %f / %f %f   %5[^\n]"
#define  OVLD_FORMAT_314   "%*26c%8[^\n]%f %8[^\n]%f%*2c%c%*2c%d%*2c%3[^\n]%*2c%2[^\n]%*c%2[^\n] %f %*3[^\n]  %c %f / %f"
#define  OVLD_FORMAT_312   "%*26c%8[^\n]%f %8[^\n]%f%*2c%c%*2c%d%*2c%3[^\n]%1[^\n]%1[^\n]        %f %*3[^\n]  %c %f / %f"
#define  OUTG_FORMAT_PTI   "%*25c[%8[^\n] %f]%*14c[%8[^\n] %f] CKT %c ---%3[^\n]%*2c%2[^\n]%*1c%2[^\n]"
#define  OUTG_FORMAT_312   " %8[^\n] %f %8[^\n] %f%*2c%c%*2c%3[^\n]%*2c%2[^\n]%*1c%2[^\n]"
#define  SYST_FORMAT_312   " %8[^\n] %f %8[^\n] %f%*2c%c%*4c%3[^\n]%*2c%2[^\n]%*1c%2[^\n]"
#define  MSG_CRTCL     "\n. CRITCL RATING taken from the first non-zero case.\n"
#define  MSG_UNITS     ". Load in AMPS for lines and MVA for transformers.\n"
#define  MSG_FLAGS     ". F Rating flags indicate type of critical rating.\n"
#define  MSG_LWRCS     ". Lowercase flag indicates rating differs from listed value.\n"
#define  MSG_SYMBL     ". T Thermal, N Nominal, B Bottleneck, E Emergency (Loss of Life)\n"
#define  MSG_FALRX     ". FAILED RX   (or RX)   - Failed reactive solution; Solved real power only\n"
#define  MSG_NOSOL     ". NO SOLUTION (or FAIL) - Failed solution\n"
#define  MSG_SEPAR     ". SEPARATION  (or SEPA) - System Separation\n"
#define  SRCHN_4_COMO_SUM  (int) (1<< 0) /* Summary of common mode outages */
#define  SRCHN_4_OUTG_SUM  (int) (1<< 1) /* Sum of bus & line probs 4 ea outg */
#define  SRCHN_4_FAIL_SUM  (int) (1<< 2) /* Sum of system separation & diverg */
#define  READING_COMO_SUM  (int) (1<< 3)
#define  READING_OUTG_SUM  (int) (1<< 4)
#define  READING_FAIL_SUM  (int) (1<< 5)
#define  GET_OVLD          (int) (1<< 6)
#define  GET_BUSV          (int) (1<< 7)
#define  GET_OUTG          (int) (1<< 8)
#define  VALID_DATA        (int) (1<< 9)
#define  DATA_PTI          (int) (1<<10)
#define  SRCHING (int) (SRCHN_4_COMO_SUM | SRCHN_4_OUTG_SUM | SRCHN_4_FAIL_SUM)
#define  READING (int) (READING_COMO_SUM | READING_OUTG_SUM | READING_FAIL_SUM)
#define  HASHSIZE 30
/***************************** end #define ************************************/
/******************************* typedef **************************************/
typedef struct {
  float  actual;  /* post-condition MVA, AMPS, or KV, or absolute (new-ref) */
  float  ratio;   /* PU, per cent, or delta (new-ref/ref) */
  float  min_ref; /* minimum (low limit), or reference */
  float  max_new; /* maximum (high limit), or rating, or new */
  float  pre_con; /* pre-condition MVA, AMPS, or KV */
  char   code;    /* rating code: T, N, E, or B */
  char   date[6]; /* date in MO/YR format */
} ff_soln;

typedef struct traceRecord {
  Link   *pfoList;
  Link   *outgList;
  Link   *comoList;
  Link   *ovldList;
  Link   *busvList;
  Link   *outgMask;
  Link   *comoMask;
  Link   *ovldMask;
  Link   *busvMask;
  Link   *mainTable;
  Link   *rptTable;
  Link   *ownrOutg;
  Link   *zoneOutg;
  Link   *bskvOutg;
  Link   *ownrProb;
  Link   *zoneProb;
  Link   *bskvProb;
  Link   *chkBranchStart;  /* Link to branches in No O/L Log */
  int     query;
  int     sort;       /* sort criteria: i.e. (trace *) or trace->sort */
  int     soln;       /* indicates whether or not to report solution problems */
  int     type;       /* output report type of rpt and aux reports */
  int     redun;
  int     nCases;
  float   minLoadPct;
  float   maxDeltaPct;
  float   varCeiling;
  float   varFloor;
  float   max5;
  float   min5;
  float   maxV;
  float   minV;
  cf_Out *trc;
  cf_Out *rpt;
  cf_Out *aux;
  char    outName[FILENAME_MAX];
  char    timeStamp[CF_STRSIZE];
  char    userID[CF_STRSIZE];
  char    IPFversion[CF_STRSIZE];        /* PRG (program) could be IPF or PTI */
} Trace;

struct nlist {  /* table entry */
    struct nlist *next;  /* next entry in chain */
    char         *name;  /* common-mode outage  */
    int           keep;  /* 1 = report, 0 = ignore */
};
/******************************* end typedef **********************************/

int count=0;
int entry=0;
long total_memory_allocated=0, total_requests=0;

/* top FINDOUT  functions - called by main() **********************************/
void     initializeTrace(Trace *trace);
void     processCommandLine(Trace *trace, int argc, char *argv[]);
void     instructions(Trace *trace);
void     promptUser(Trace *trace);
void     openReport(cf_Out *rpt);
void     printTrace(Trace *trace);
void     queryContinue(Trace *trace);
void     collectData(Trace *trace);
void     buildReport(Trace *trace, cf_Out *rpt);
void     printReport(Trace *trace, cf_Out *rpt);
void     swapReport(Trace *trace);
void     finalRemarks(Trace *trace);
/* end FINDOUT  functions - called by main() **********************************/
      
/* top FINDOUT  functions - report functions **********************************/
void     ff_printBlankLines(cf_Out *rpt, int n);
int      ff_printPageFooter(cf_Out *rpt, int m);
void     pageFooter(cf_Out *rpt, Link *nxRow, Link *nxPfo);
void     tableLegend(cf_Out *rpt);
void     printHeader(cf_Out *rpt, Link *curPfo);
void     printID(cf_Out *rpt, Link *curID, int position);
void     printSoln(cf_Out *rpt, Link *curID, Link *curSoln, int doN);
void     printOvldHeader(cf_Out *rpt, Link *curPfo);
void     printOtBrHeader(cf_Out *rpt, Link *curPfo);
void     printOtBsHeader(cf_Out *rpt, Link *curPfo);
void     printBusvHeader(cf_Out *rpt, Link *curPfo);
void     printTableCases(cf_Out *rpt, Link *curPfo, char *l, char *r, char *d);
void     printTableHeader(cf_Out *rpt, Link *curPfo, char *l, char *r, char *d);
void     printOutg(FILE *fp, Link *outgLink, int form);
void     printOvld(FILE *fp, Link *ovldLink, int form);

void     chkBranch(Trace *trace, Link *curID);             /* No O/L Log support */
void     chkBranchLog(Trace *trace, Link *curPfo);         /* No O/L Log support */

void     printBusv(FILE *fp, Link *busvLink, int form);
void     printLoad(cf_Out *rpt, Link *loadLink, int doN);
void     printVolt(cf_Out *rpt, Link *voltLink, int doN);
void     printProb(cf_Out *rpt, Link *probLink, int doN);
void     printDiff(cf_Out *rpt, Link *solnLink, int doN);
/* end FINDOUT  functions - report functions **********************************/

/* top FINDOUT  functions - support functions *********************************/
int      screenProb(Link *problem, Trace trace);
int      buildMainTable(FILE *pfo_File, Trace *trace);
Link    *getOutgSet(FILE *pfo_File, int *state, Link *problem, Trace *trace);
int      getCoMoRslt(char *s, char *format, Trace trace);
int      getOvldLoad(char *s, char *format, Link *problem);
int      getBusvVolt(char *s, char *format, Link *problem);
Link    *getSystProb(char *s, char *format, Link *problem);
Link    *getBrchOutg(char *s, char *format);
Link    *getCoMoOutg(char *s);
int      identifyInput(char *s);
void     installResult(char *s);
int      ff_srtByInput(Link *newRow, Link *xstRow, void *sort);
int      srtByInput(Link *newRow, Link *xstRow, Trace trace);
int      getIndex(Link *newRow, Link *xstRow, Link *list);
int      validVariance(Trace trace, Link *solnLink);
int      validLoad(Trace trace, Link *solnPtr);
int      validVolt(Trace trace, Link *solnPtr);
int      validSoln(Link *solnPtr);
int      validInput(Link *listPtr, cf_Branch *data);
void     swapRowIdLinks(Link *rowPtr);
int      validMask(Link *maskList, cf_Name *r);
int      ff_srtBus(pf_AC_bus *b1, pf_AC_bus *b2, int sort); /* sort bus */
int      ff_srtSoln(Link *newSoln, Link *xstSoln, int sort);
int      ff_cmpType(Link *newId, Link *xstId);
int      ff_cmpName(cf_Name *b1, cf_Name *b2);
int      ff_cmpID(Link *newLink, Link *xstLink);
int      ff_srtID(Link *newLink, Link *xstLink, int sort);
int      ff_cmpRow(Link *newRow, Link *xstRow, int sort);
/* end FINDOUT  functions - support functions *********************************/

/* top LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/
void     ff_stream2List(FILE *readMe, Trace *trace, Link **expList);
void     ff_report(char *s, Trace *trace);
void     ff_limits(char *s, Trace *trace);
void     ff_expList(Trace *trace, Link *dataList, Link **expList);
void     ff_appSoln2Row(Link *curRow, Link *solnLink, int nCases);
int      ff_srtBranch(cf_Branch *b1, cf_Branch *b2, int sort);
int      ff_wildBranch(pf_branch *b1, pf_branch *b2);
/* end LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/

/* top CF_UTIL.H candidate functions ******************************************/
void date(char *date);
/* end CF_UTIL.H candidate functions ******************************************/

/* top FINDOUT  functions - hash table functions ******************************/
unsigned hash(char *s);
struct nlist *install(char *name, int keep);
struct nlist *lookup(char *s);
/* end FINDOUT  functions - hash table functions ******************************/

/******************************** global variable candidates ******************/
static struct nlist *hashtab[HASHSIZE]; /* pointer table or cm outages */
/**************************** end global variable candidates ******************/

cf_Style FF_mskStyl = { "",   CF_TAG_W_LIST,  1 };

int main(int argc, char *argv[])
{
  Trace trace;

  time(&CF_time0);
  initializeTrace(&trace);
  processCommandLine(&trace, argc, argv);
  instructions(&trace);
  promptUser(&trace);
  printTrace(&trace);
  queryContinue(&trace);
  collectData(&trace);
  trace.chkBranchStart = cf_dupList (trace.ovldList);       /* Link to No O/L Log */
  buildReport(&trace, trace.rpt);
  printReport(&trace, trace.rpt);
  swapReport(&trace);
  printReport(&trace, trace.aux);
  finalRemarks(&trace);
  time(&CF_time1);
  cf_logUse("FindOut", trace.IPFversion, trace.userID);
  return 0;
}
void initializeTrace(Trace *trace)
{ /* special function to do set up/init for FINDOUT */
  memset(trace, '\0', sizeof(Trace));
  trace->query      =  ( QUERY_PFO  | QUERY_DATA | QUERY_DIFF |
                         QUERY_SOLN | QUERY_TYPE | QUERY_NAME |
                         QUERY_LOAD | QUERY_VOLT | QUERY_SORT |
                         QUERY_OWNR | QUERY_ZONE | QUERY_BSKV |
                         QUERY_OW_P | QUERY_ZN_P | QUERY_KV_P |
                         QUERY_FORM | QUERY_WDTH | QUERY__TRC |
                         QUERY_CONT | QUERY_OUTG | QUERY_COMO |
                         QUERY_OVLD | QUERY_BUSV | QUERY_DLTA |
                         QUERY_REDU | QUERY_VARI | QUERY_SPAN );
  trace->varCeiling =  1;
  trace->varFloor   = -1;
  trace->minLoadPct = DFLT_MIN_LOAD_PCT;
  trace->maxDeltaPct= DFLT_MAX_DELTA_PCT;
  trace->max5       = DFLT_MAX_500_PU;
  trace->min5       = DFLT_MIN_500_PU;
  trace->maxV       = DFLT_MAX_BUS_PU;
  trace->minV       = DFLT_MIN_BUS_PU;
  trace->sort       = SORT_BY_ALPHA;
  trace->type       = TYPE_OUTG_OVLD;
  trace->redun      = REDUN_KEEP;
  trace->nCases     = 0;
  trace->IPFversion[0] = '\0';
  cuserid(trace->userID);
  cf_time(trace->timeStamp, CF_STRSIZE, CF_TIMESTAMP);
  strcpy(CF_logSpec, LOG_SPEC);
  strcpy(trace->outName, OUT_NAME);
  trace->trc = cf_initOut("", trace->timeStamp, TRACE_YES, FORM_DATA_FULL);
  trace->rpt = cf_initOut("", trace->timeStamp, TYPE_NONE, FORM_DATA_FULL);
  trace->aux = cf_initOut("", trace->timeStamp, TYPE_NONE, FORM_DATA_FULL);
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
  ff_expList(trace, topLink, &trace->pfoList);
  return;
}
void  instructions(Trace *trace)
{
  if (trace->query==0) return;/* skip if there is not going to be any queries */
  printf("\n                      Welcome to FINDOUT - updated 01-10-98");
  printf("\n");
  printf("\n - Defaults in brackets [].  Press Ctrl-Y & type STOP to abort.");
  printf("\n - Use .trc, .dat, or .lis or append / to designate data files.");
  printf("\n - Powerflow version %d recommended.", FF_IPF_VERSION);
  printf("\n");
}
void promptUser(Trace *trace)
{
  char  query[CF_INBUFSIZE];
  Link *list;

  printf("%s", DOT_LINE);
  if (trace->query & QUERY_PFO) {
    printf("\n\n/PFO, /TRACE, or /INCLUDE");
    cf_nprompt("\n > Enter list of .PFO files (and/or data files): ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->pfoList);
  }
  if (trace->pfoList==NULL) {
    printf("\n\n *** Warning ***, No *.PFO files have been specified!");
    printf("\n\n/PFO, /TRACE, or /INCLUDE");
    cf_nprompt("\n > Enter list of .PFO files (and/or data files): ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->pfoList);
  }
  cf_exit(trace->pfoList==NULL, "No data or *.PFO files!  Quitting!\n");

  if (trace->query == QUERY_CONT) trace->query &= ~(QUERY_CONT);

  if (cf_cntLink(trace->pfoList)==1) {
    trace->query &= ~(QUERY_WDTH);             /* only one *.pfo file present */
  }
  if (trace->query &
        (QUERY_NAME|QUERY_TYPE|QUERY_SORT|QUERY_FORM|QUERY_WDTH|QUERY_REDU)) {
    printf("\n\n/REPORT");
  }
  if (trace->query & QUERY_NAME) {
    cf_sprompt("\n > Enter output files default name, NAME = [%s]: ", trace->outName, trace->outName);
    printf("\n");
  }
  if (trace->query & QUERY_TYPE) {
    printf("\n Specify the type of output report (by number)");
    printf("\n   1. OUTAGEs  causing  OVERLOADs    2. OUTAGEs  causing  BUS Violations");
    printf("\n   3. OVERLOADs caused by OUTAGEs    4. BUS Violations caused by OUTAGES");
    printf("\n   5. OVERLOADs BOTH WAYS (1 & 3)    6. BUS Violations BOTH WAYS (2 & 4)");
    cf_iprompt("\n > Enter report choice, TYPE =[OUTAGE-OVERLOAD]: ", trace->type, &trace->type);
    printf("\n");
  }
  if (trace->type==1) {
    trace->rpt->type = TYPE_OUTG_OVLD;
    trace->aux->type = 0;
  }
  if (trace->type==2) {
    trace->rpt->type = TYPE_OUTG_BUSV;
    trace->aux->type = 0;
  }
  if (trace->type==3) {
    trace->rpt->type = TYPE_OVLD_OUTG;
    trace->aux->type = 0;
  }
  if (trace->type==4) {
    trace->rpt->type = TYPE_BUSV_OUTG;
    trace->aux->type = 0;
  }
  if (trace->type==5) {
    trace->rpt->type = TYPE_OUTG_OVLD;
    trace->aux->type = TYPE_OVLD_OUTG;
  }
  if (trace->type==6) {
    trace->rpt->type = TYPE_OUTG_BUSV;
    trace->aux->type = TYPE_BUSV_OUTG;
  }
  if (cf_cntLink(trace->pfoList)==1) {         /* only one *.pfo file present */
    trace->query &= ~(QUERY_VARI);
    trace->query &= ~(QUERY_SPAN);
    trace->trc->diff = VARI_NO;
  }
  if (trace->query & QUERY_VARI) {
    trace->trc->diff = cf_yprompt("\n > Do a variance report?       VARIANCE = [YES]: ", 'Y');
    printf("\n");
  }
  if (trace->trc->diff==VARI_NO) trace->query &= ~(QUERY_SPAN);
  trace->rpt->diff = trace->trc->diff;
  trace->aux->diff = trace->trc->diff;

  if (trace->query & QUERY_REDU) {
    trace->redun = cf_yprompt("\n > Redundant outages:   REMOVE_REDUNDANT = [NO]: ", 'N');
    printf("\n");
  }
  if (trace->query & QUERY_SORT) {
    printf("\n Specify sort priority (by number)");
    printf("\n   1. ALPHA-base_kv   3. OWNER-alpha-base_kv    5. INPUT order");
    printf("\n   2. BASE_KV-alpha   4.  ZONE-alpha-base_kv    6. SOLUTION");
    cf_iprompt("\n > Enter sort choice,            SORT = [ALPHA]: ", trace->sort, &trace->sort);
    printf("\n");
  }
  if (trace->sort==SORT_BY_SOLN && trace->trc->diff==VARI_YES) {
    trace->sort = SORT_BY_DIFF;
  }
  if (trace->query & QUERY_FORM) {
    printf("\n Specify output report format (by number)");
    printf("\n   1. DATA-FULL (Excel)  2. DATA-BRIEF (Excel)  3. PAGE-WIDE  4. PAGE-COMPACT");
    cf_iprompt("\n > Enter format choice,    FORMAT = [DATA-FULL]: ", trace->trc->form, &trace->trc->form);
    printf("\n");
  }
  trace->rpt->form = trace->trc->form;
  trace->aux->form = trace->trc->form;
  if (trace->rpt->type==TYPE_OUTG_OVLD || trace->rpt->type==TYPE_OVLD_OUTG) {
    if (trace->query & QUERY_WDTH) {   /* get default width */
        if      (trace->trc->form==FORM_DATA_BREF) trace->trc->wide = 9;
        else if (trace->trc->form==FORM_PAGE_COMP) trace->trc->wide = 4;
        else                                       trace->trc->wide = 2;
    }
  }
  else if (trace->rpt->type==TYPE_OUTG_BUSV || trace->rpt->type==TYPE_BUSV_OUTG) {
    if (trace->query & QUERY_WDTH) {                     /* get default width */
        if      (trace->trc->form==FORM_DATA_BREF) trace->trc->wide = 12;
        else if (trace->trc->form==FORM_PAGE_COMP) trace->trc->wide = 4;
        else                                       trace->trc->wide = 3;
    }
  }
  if (trace->query & QUERY_WDTH) {
    printf("\n");
    printf("\n Specify the maximum number of cases per table (%d fit in 132 columns).", trace->trc->wide);
    cf_iprompt("\n > Enter width in cases,  CASES_PER_TABLE = [%d]: ", trace->trc->wide, &trace->trc->wide);
  }
  trace->rpt->wide = trace->trc->wide;
  trace->aux->wide = trace->trc->wide;
  if (trace->query & QUERY_DATA) {
    printf("\n\n/INCLUDE");
    cf_nprompt("\n > Enter list of files with branch or bus lists: ", "", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    ff_expList(trace, list, NULL);
  }
  if (trace->rpt->type==TYPE_OUTG_OVLD || trace->rpt->type==TYPE_OVLD_OUTG)
    trace->query &= ~(QUERY_BUSV | QUERY_VOLT);
  if (trace->rpt->type==TYPE_OUTG_BUSV || trace->rpt->type==TYPE_BUSV_OUTG)
    trace->query &= ~(QUERY_OVLD | QUERY_LOAD);
  if (trace->query & QUERY_OUTG) {
    printf("\n\n/OUTAGE"); 
    printf("\n > Enter outaged branches list       :"); 
    printf("\n   > Tycown< BUS1 ><V1> < BUS2 ><V2>cs");
    do {
        cf_nprompt("\n   > ", "", CF_INBUFSIZE, query);
        if (strchr(query, FF_WILD)==NULL) {
            list = cf_id2Link(query, 'L');
            cf_appList(&trace->outgList, list);
        }
        else {
            list = cf_text2Link(query);
            cf_appList(&trace->outgMask, list);
        }
        if ( list!=NULL && (trace->rpt->type==TYPE_OUTG_OVLD || trace->rpt->type==TYPE_OUTG_BUSV) )
            trace->query &= ~(QUERY_ZONE | QUERY_OWNR | QUERY_BSKV);
    } while (!cf_isblank(query));
  }
  if (trace->query & QUERY_COMO) {
    printf("\n/COMMON_MODE"); 
    printf("\n > Enter common-mode outages list    :");
    do {
        cf_nprompt("\n   > ", "", CF_INBUFSIZE, query);
        if (strchr(query, FF_WILD)==NULL) {
            list = getCoMoOutg(query);
            cf_appList(&trace->comoList, list);
        }
        else {
            list = cf_text2Link(query);
            cf_appList(&trace->comoMask, list);
        }
        if ( list!=NULL && (trace->rpt->type==TYPE_OUTG_OVLD || trace->rpt->type==TYPE_OUTG_BUSV) )
            trace->query&= ~(QUERY_ZONE | QUERY_OWNR | QUERY_BSKV);
    } while (!cf_isblank(query));
    if (list!=NULL && (trace->rpt->type==TYPE_OUTG_OVLD || trace->rpt->type==TYPE_OUTG_BUSV))
        trace->query &= ~(QUERY_ZONE | QUERY_OWNR | QUERY_BSKV);
  }
  if (trace->query & (QUERY_OWNR | QUERY_ZONE | QUERY_BSKV)) {
    printf("\n\n/OUTG_OWNER, /OUTG_ZONE, /OUTG_BASE_KV");
    printf("\n Specify selection of branch OUTAGEs and COMMON_MODE outages");
  }
  if (trace->query & QUERY_OWNR) {
    cf_nprompt("\n > Enter owners of interest..[all owners='%s']: ", "***", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    cf_appList(&trace->ownrOutg, list);
  }
  if (trace->query & QUERY_ZONE) {
    cf_nprompt("\n > Enter zones of interest.....[all zones='%s']: ", "**", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    cf_appList(&trace->zoneOutg, list);
  }
  if (trace->query & QUERY_BSKV) {
    cf_nprompt("\n > Enter base_kv of interest.[all base kV='%s']: ", ">0", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    cf_appList(&trace->bskvOutg, list);
  }
  if (trace->query & QUERY_OVLD) {
    printf("\n\n/OVERLOAD");
    printf("\n > Enter overloaded branches list    :");
    printf("\n   > Tycown< BUS1 ><V1> < BUS2 ><V2>cs");
    do {
        cf_nprompt("\n   > ", "", CF_INBUFSIZE, query);
        if (strchr(query, FF_WILD)==NULL) {
            list = cf_id2Link(query, 'L');
            cf_appList(&trace->ovldList, list);
        }
        else {
            list = cf_text2Link(query);
            cf_appList(&trace->ovldMask, list);
        }
        if ( list!=NULL && (trace->rpt->type==TYPE_OVLD_OUTG || trace->rpt->type==TYPE_BUSV_OUTG) )
            trace->query&= ~(QUERY_ZN_P | QUERY_OW_P | QUERY_KV_P);
    } while (!cf_isblank(query));
    if (list!=NULL && (trace->rpt->type==TYPE_OVLD_OUTG || trace->rpt->type==TYPE_BUSV_OUTG))
        trace->query &= ~(QUERY_ZN_P | QUERY_OW_P | QUERY_KV_P);
  }
  if (trace->query & QUERY_BUSV) {
    printf("\n\n/BUS");
    printf("\n > Enter busses list  :");
    printf("\n   > Tycown< NAME ><KV>");
    do {
        cf_nprompt("\n   > ", "", CF_INBUFSIZE, query);
        if (strchr(query, FF_WILD)==NULL) {
            list = cf_id2Link(query, 'I');
            cf_appList(&trace->busvList, list);
        }
        else {
            list = cf_text2Link(query);
            cf_appList(&trace->busvMask, list);
        }
        if ( list!=NULL && (trace->rpt->type==TYPE_OVLD_OUTG || trace->rpt->type==TYPE_BUSV_OUTG) )
            trace->query &= ~(QUERY_ZN_P | QUERY_OW_P | QUERY_KV_P);
    } while (!cf_isblank(query));
    if (list!=NULL && (trace->rpt->type==TYPE_OVLD_OUTG || trace->rpt->type==TYPE_BUSV_OUTG))
        trace->query &= ~(QUERY_ZN_P | QUERY_OW_P | QUERY_KV_P);
  }
  if (trace->query & (QUERY_OW_P | QUERY_ZN_P | QUERY_KV_P)) {
    printf("\n\n/PROB_OWNER, /PROB_ZONE, /PROB_BASE_KV");
    if (trace->rpt->type==TYPE_OUTG_OVLD || trace->rpt->type==TYPE_OVLD_OUTG)
        printf("\n Specify selection of OVERLOADed branches");
    if (trace->rpt->type==TYPE_OUTG_BUSV || trace->rpt->type==TYPE_BUSV_OUTG)
        printf("\n Specify selection of over- or under-voltage BUSses");
  }
  if (trace->query & QUERY_OW_P) {
    cf_nprompt("\n > Enter owners of interest..[all owners='%s']: ", "***", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    cf_appList(&trace->ownrProb, list);
  }
  if (trace->query & QUERY_ZN_P) {
    cf_nprompt("\n > Enter zones of interest.....[all zones='%s']: ", "**", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    cf_appList(&trace->zoneProb, list);
  }
  if (trace->query & QUERY_KV_P) {
    cf_nprompt("\n > Enter base_kv of interest.[all base kV='%s']: ", ">0", CF_INBUFSIZE, query);
    list = cf_text2List(query);
    cf_appList(&trace->bskvProb, list);
  }
  if (trace->rpt->type==TYPE_OUTG_OVLD || trace->rpt->type==TYPE_OVLD_OUTG) {
    trace->query &= ~(QUERY_VOLT);
  }
  if (trace->rpt->type==TYPE_OUTG_BUSV || trace->rpt->type==TYPE_BUSV_OUTG) {
    trace->query &= ~(QUERY_LOAD);
  }
  if (trace->query & (QUERY_LOAD | QUERY_VOLT | QUERY_SPAN | QUERY_SOLN)) {
    printf("\n\n/LIMITS");
  }
  if (trace->query & QUERY_SPAN) {
    if (trace->rpt->type==TYPE_OUTG_OVLD || trace->rpt->type==TYPE_OVLD_OUTG) {
        cf_fprompt("\n > Enter variance ceiling (%%),VARI_CEILING=[%2.0f]: ", 1.0, &trace->varCeiling);
        cf_fprompt("\n > Enter variance floor (%%),  VARI_FLOOR  =[%2.0f]: ",-1.0, &trace->varFloor);
    }
    if (trace->rpt->type==TYPE_OUTG_BUSV || trace->rpt->type==TYPE_BUSV_OUTG) {
        cf_fprompt("\n > Enter variance ceiling PU, VARI_CEILING =[%3.3f]: ", .01, &trace->varCeiling);
        cf_fprompt("\n > Enter variance floor PU,   VARI_FLOOR   =[%3.3f]: ",-.01, &trace->varFloor);
    }
  }
  if (trace->query & QUERY_LOAD) {
    cf_fprompt("\n > Enter min loading in %%, MIN_LOAD_PCT = [%2.0f%%]: ", trace->minLoadPct, &trace->minLoadPct);
  }
  if (trace->query & QUERY_VOLT) {
    cf_fprompt("\n > Max 500kV-bus voltage PU, MAX_500_PU=[%5.3f]: ", trace->max5, &trace->max5);
    cf_fprompt("\n > Min 500kV-bus voltage PU, MIN_500_PU=[%5.3f]: ", trace->min5, &trace->min5);
    cf_fprompt("\n > Max sub-500kV voltage PU, MAX_BUS_PU=[%5.3f]: ", trace->maxV, &trace->maxV);
    cf_fprompt("\n > Min sub-500kV voltage PU, MIN_BUS_PU=[%5.3f]: ", trace->minV, &trace->minV);
  }
  if (trace->query & QUERY_SOLN) {
    printf("\n");
    trace->soln = cf_yprompt("\n > Include solution problems? INC_SOLN_PROB=[%c]: ", 'Y');
  }
  printf("\n");
  cf_strsuf(trace->rpt->spec, trace->outName, '.', ".rpt");
  cf_strsuf(trace->aux->spec, trace->outName, '.', ".aux");
  cf_strsuf(trace->trc->spec, trace->outName, '.', ".trc");
  printf("%s\n", DOT_LINE);
  return;
}
void openReport(cf_Out *rpt)
{
  rpt->file = cf_openFile(rpt->spec, "w");
  cf_exit(rpt->file==NULL, "Quitting!\n");
}
void printTrace(Trace *trace)
{
  FILE *fp;

  if (trace->trc->type==TYPE_NONE) return;

  openReport(trace->trc);
  fp = trace->trc->file;
  if (fp == NULL) fp = stderr;
  fprintf(fp, ". %s %s %s %s\n", trace->trc->spec, trace->trc->time,
    trace->userID, trace->IPFversion);

  cf_printList(fp, trace->pfoList, CF_oneStyl, "/PFO\n");
  fprintf(fp, "/REPORT\n");
  fprintf(fp, "  NAME = %s\n", trace->outName);
  if (trace->type     ==TYPE_OUTG_OVLD) fprintf(fp,"  TYPE = OUTAGE-OVERLOAD\n");
  if (trace->type     ==TYPE_OUTG_BUSV) fprintf(fp,"  TYPE = OUTAGE-BUS_V\n");
  if (trace->type     ==TYPE_OVLD_OUTG) fprintf(fp,"  TYPE = OVERLOAD-OUTAGE\n");
  if (trace->type     ==TYPE_BUSV_OUTG) fprintf(fp,"  TYPE = BUS_V-OUTAGE\n");
  if (trace->type     ==TYPE_OVLD_BOTH) fprintf(fp,"  TYPE = OVERLOAD-BOTH-WAYS\n");
  if (trace->type     ==TYPE_BUSV_BOTH) fprintf(fp,"  TYPE = BUS_V-BOTH-WAYS\n");
  if (trace->sort     ==SORT_BY_ALPHA ) fprintf(fp,"  SORT = ALPHA\n");
  if (trace->sort     ==SORT_BY_BASEKV) fprintf(fp,"  SORT = BASE_KV\n");
  if (trace->sort     ==SORT_BY_OWNER ) fprintf(fp,"  SORT = OWNER\n");
  if (trace->sort     ==SORT_BY_ZONE  ) fprintf(fp,"  SORT = ZONE\n");
  if (trace->sort     ==SORT_BY_INPUT ) fprintf(fp,"  SORT = INPUT\n");
  if (trace->sort     ==SORT_BY_SOLN  ) fprintf(fp,"  SORT = SOLUTION\n");
  if (trace->sort     ==SORT_BY_DIFF  ) fprintf(fp,"  SORT = SOLUTION\n");
  if (trace->trc->form==FORM_DATA_FULL) fprintf(fp,"  FORMAT = DATA_FULL\n");
  if (trace->trc->form==FORM_DATA_BREF) fprintf(fp,"  FORMAT = DATA_BRIEF\n");
  if (trace->trc->form==FORM_PAGE_WIDE) fprintf(fp,"  FORMAT = PAGE_WIDE\n");
  if (trace->trc->form==FORM_PAGE_COMP) fprintf(fp,"  FORMAT = PAGE_COMPACT\n");
  if (trace->redun    ==REDUN_REMOVE  ) fprintf(fp,"  REMOVE_REDUNDANT = YES\n");
  if (trace->redun    ==REDUN_KEEP    ) fprintf(fp,"  REMOVE_REDUNDANT = NO\n");
  if (trace->trc->type==TYPE_TRACE    ) fprintf(fp,"  TRACE = YES\n");
  if (trace->trc->type!=TYPE_TRACE    ) fprintf(fp,"  TRACE = NO\n");
  if (trace->trc->diff==VARI_YES      ) fprintf(fp,"  VARIANCE = YES\n");
  if (trace->trc->diff==VARI_NO       ) fprintf(fp,"  VARIANCE = NO\n");
  fprintf(fp, "  CASES_PER_TABLE = %d\n", trace->trc->wide);
  fprintf(fp, "/LIMITS\n");
  if (trace->trc->diff==VARI_YES) {
    fprintf(fp, "  VARIANCE_CEILING = %6.4f\n", trace->varCeiling);
    fprintf(fp, "  VARIANCE_FLOOR   = %6.4f\n", trace->varFloor);
  }
  if (trace->rpt->type==TYPE_OUTG_OVLD || trace->rpt->type==TYPE_OVLD_OUTG) {
    fprintf(fp, "  MIN_LOAD_PCT = %6.4f\n", trace->minLoadPct);
  }
  if (trace->rpt->type==TYPE_OUTG_BUSV || trace->rpt->type==TYPE_BUSV_OUTG) {
    fprintf(fp, "  MAX_500_PU = %6.4f\n", trace->max5);
    fprintf(fp, "  MIN_500_PU = %6.4f\n", trace->min5);
    fprintf(fp, "  MAX_BUS_PU = %6.4f\n", trace->maxV);
    fprintf(fp, "  MIN_BUS_PU = %6.4f\n", trace->minV);
  }
  if (trace->soln == INC_SOLN_PROB  ) fprintf(fp,"  INC_SOLN_PROB = YES\n");
  if (trace->soln == SKP_SOLN_PROB  ) fprintf(fp,"  INC_SOLN_PROB = NO\n");

  cf_printList(fp, trace->ownrOutg, CF_dznStyl, "/OUTG_OWNER\n");
  cf_printList(fp, trace->zoneOutg, CF_dznStyl, "/OUTG_ZONE\n");
  cf_printList(fp, trace->bskvOutg, CF_sixStyl, "/OUTG_BASE_KV\n");
  cf_printList(fp, trace->ownrProb, CF_dznStyl, "/PROB_OWNER\n");
  cf_printList(fp, trace->zoneProb, CF_dznStyl, "/PROB_ZONE\n");
  cf_printList(fp, trace->bskvProb, CF_sixStyl, "/PROB_BASE_KV\n");
  cf_printList(fp, trace->outgList, FF_mskStyl, "/OUTAGE\n.ycown< BUS1 ><V1> < BUS2 ><V2>cs\n");
  cf_printList(fp, trace->outgMask, FF_mskStyl, "/OUTAGE\n.ycown< BUS1 ><V1> < BUS2 ><V2>cs\n");
  cf_printList(fp, trace->comoList, FF_mskStyl, "/COMMON_MODE\n");
  cf_printList(fp, trace->comoMask, FF_mskStyl, "/COMMON_MODE\n");
  cf_printList(fp, trace->ovldList, FF_mskStyl, "/OVERLOAD\n.ycown< BUS1 ><V1> < BUS2 ><V2>cs\n");
  cf_printList(fp, trace->ovldMask, FF_mskStyl, "/OVERLOAD\n.ycown< BUS1 ><V1> < BUS2 ><V2>cs\n");
  cf_printList(fp, trace->busvList, FF_mskStyl, "/BUS\n.ycown< BUS  ><KV>zn\n");
  cf_printList(fp, trace->busvMask, FF_mskStyl, "/BUS\n.ycown< BUS  ><KV>zn\n");
}
void queryContinue(Trace *trace)
{
  int  yes;

  if (trace->trc->type==TRACE_YES)
    printf("\nTrace written to %s", trace->trc->spec);
  if (CF_logFile != NULL)
    printf("\nError Summary written to %s", CF_logSpec);
  if ( trace->query & QUERY_CONT) {
    yes= cf_yprompt("\n > Do you want to continue this run?        [%c]: ",'Y');
    cf_exit(!yes, "Quitting!");
  }
}
void collectData(Trace *trace)
{
  FILE *pfo_File;
  Link *curPfo;

  trace->mainTable = NULL;
  curPfo=trace->pfoList;
  while (curPfo!=NULL) {
    if ( (pfo_File = cf_openFile(curPfo->data, "r")) == NULL ) {
        cf_logErr(" Cannot open this *.pfo file: %s\n", curPfo->data);
        curPfo = cf_delLink(&trace->pfoList, curPfo);
        continue;
    }
    fprintf(stdout, " Searching %s\n", curPfo->data);
    if (buildMainTable(pfo_File, trace)==0) {
        printf("   No data in this .pfo file.\n");
        curPfo = cf_delLink(&trace->pfoList, curPfo);
        continue;
    }
    fclose(pfo_File);
    trace->nCases++;
    curPfo=curPfo->next;
  }
}
void  finalRemarks(Trace *trace)
{
  printf("\n");
  printf("\nMemory allocation (bytes): Cur:%ld Max:%ld Alloc:%ld Freed:%ld\n",
    CF_memCurAlloc, CF_memMaxAlloc, CF_memTotAlloc, CF_memTotFreed);
  if (trace->rpt->type!=TYPE_NONE)
    printf("\nOutput report written to %s", trace->rpt->spec);
  if (trace->aux->type!=TYPE_NONE)
    printf("\nOutput report written to %s", trace->aux->spec);
  if (trace->trc->type!=TYPE_NONE)
    printf("\nTrace  report written to %s", trace->trc->spec);
  if (CF_logFile != NULL) printf("\nError  report written to %s", CF_logSpec);
  printf("\n");
  return;
}
void tableLegend(cf_Out *rpt)
{
  fprintf(rpt->file,"%s", MSG_CRTCL);
  fprintf(rpt->file,"%s", MSG_UNITS);
  fprintf(rpt->file,"%s", MSG_FLAGS);
  fprintf(rpt->file,"%s", MSG_LWRCS);
  fprintf(rpt->file,"%s", MSG_SYMBL);
  fprintf(rpt->file,"%s", MSG_FALRX);
  fprintf(rpt->file,"%s", MSG_NOSOL);
  fprintf(rpt->file,"%s", MSG_SEPAR);
  rpt->line += 9;
}
int buildMainTable(FILE *pfo_File, Trace *trace)
{
  int      state;
  Link    *curRow, *problem, *newRow, *newSoln, *newProb;
  state = (SRCHN_4_COMO_SUM | SRCHN_4_OUTG_SUM | SRCHN_4_FAIL_SUM);
  curRow = trace->mainTable;
/*  system("show time"); */
  problem = NULL;
  trace->IPFversion[0] = '\0';
  while ( (problem=getOutgSet(pfo_File, &state, problem, trace)) != NULL ) {
    if (screenProb(problem, *trace)==0) continue;
    newProb = cf_dupList(problem);
    newSoln = newProb->next->next;
    newProb->next->next = NULL;
    newRow = cf_link2row(newProb);
    cf_insLink(&trace->mainTable, &curRow, newRow, (long) SORT_BY_ALPHA,
        CF_INS_FREE, (int (*)(Link*, Link*, int))(ff_cmpRow));
    ff_appSoln2Row(curRow, newSoln, trace->nCases+1);/* add 1 to skip prob id */
  }
/*  system("show time"); */
  printf(" IPF Executible Used: version = %3.3s\n", trace->IPFversion);
  return (state & VALID_DATA) ? 1 : 0;
}
int screenProb(Link *problem, Trace trace)
{ /* return 0 if problem is to be skipped over, else non-zero */
  cf_Branch *outg, *ovld;
  cf_Bus    *busv;
  int        type, vo = 0, vp = 0;
  type = trace.type;
  outg = (cf_Branch *) problem->data;
  if ( outg==NULL) {
    return;
  } else if ( outg->type[0]=='c' ) {                        /* common mode outages */
    if ( validInput(trace.comoList, outg)==1 ) vo = 1;
    if ( validMask(trace.comoMask, (cf_Name *) outg)==1 ) vo = 1;
    if ( trace.zoneOutg!=NULL ||
         trace.ownrOutg!=NULL ||
         trace.bskvOutg!=NULL )
            vo = 1;
  }
  else if ( outg->type[0]!='c' ) {                        /* branch outages */
    cf_branch_l2h(outg);
    if ( validInput(trace.outgList, outg)==1 ) vo = 1;
    if ( validMask(trace.outgMask, (cf_Name *) outg)==1 ) vo = 1;
    if ( cf_validZone(trace.zoneOutg,outg->bus1_zone,outg->bus2_zone)==1 &&
         cf_validOwner(trace.ownrOutg,outg->owner)==1 &&
         cf_validBasekv(trace.bskvOutg,outg->bus1_kv,outg->bus2_kv)==1 )
            vo = 1;
  }
  if (vo==0) return 0;

  if (type==TYPE_OUTG_OVLD || type==TYPE_OVLD_OUTG || type==TYPE_OVLD_BOTH) {
    ovld = (cf_Branch *) problem->next->data;
    if ( ovld->type[0]=='s' ) return trace.soln;     /*include system problems*/
    if ( ovld->type[0]=='B' ) return 0;              /*skip voltage violations*/
    cf_branch_l2h(ovld);
    if ( validInput(trace.ovldList, ovld)==1 ) vp = 1;
    if ( validMask(trace.ovldMask, (cf_Name *) ovld)==1 ) vp = 1;
    if ( cf_validZone(trace.zoneProb,ovld->bus1_zone,ovld->bus2_zone)==1  &&
         cf_validOwner(trace.ownrProb, ovld->owner)==1 && 
         cf_validBasekv(trace.bskvProb, ovld->bus1_kv, ovld->bus2_kv)==1 )
            vp = 1;
  }
  if (type==TYPE_OUTG_BUSV || type==TYPE_BUSV_OUTG || type==TYPE_BUSV_BOTH) {
    busv = (cf_Bus *) problem->next->data;
    if ( busv->type[0]=='s' ) return trace.soln; /*include system problems*/
    if ( busv->type[0]!='B' ) return 0;                     /* skip overloads */
    if ( validInput(trace.busvList, (cf_Branch *) busv)==1 ) vp = 1;
    if ( validMask(trace.busvMask, (cf_Name *) busv)==1 )  vp = 1;
    if ( cf_validZone(trace.zoneProb, busv->zone, busv->zone)==1 &&
         cf_validOwner(trace.ownrProb, busv->owner)==1 && 
         cf_validBasekv(trace.bskvProb, busv->kv, busv->kv)==1 )
            vp = 1;
  }
  return vp;
}
Link *getOutgSet(FILE *pfo_File, int *state, Link *problem, Trace *trace)
{ /* searching the summary of bus and line problems for each outage */
  long  id, pos;
  char  s[CF_INPFOSIZE+1], name[39], *end, *dbg_file = "FINDOUT.DBG", *dbg_mode = "a";
  static char format[132];
  struct nlist *np;
  FILE *dbg_fp;

  entry++;
/*
  dbg_fp = fopen (dbg_file, dbg_mode);
  if (dbg_fp == NULL) {
    fprintf (stderr, "Can't open debug file FINDOUT.DBG\n");
    exit (1);
  }  
*/
  while (1) {
    pos = ftell(pfo_File);      /* needed to get 3 buses per line in PTI data */
    if (fgets(s, CF_INPFOSIZE, pfo_File)==NULL) break;

/*
    fprintf (dbg_fp, " %5d %5d [%s]\n", entry, ++count, s);
*/
    fprintf (stderr, " %5d %5d [%s]\n", entry, ++count, s);
    if (count > 6544) {
      fprintf (stderr, "getOutgSet: Processing record # %10d\n", count);
    }

    if (s[0]=='0' || s[0]=='1') s[0]=' ';
    end = strrchr(s, '\r'); if (end!=NULL) *end = '\0';/* needed for PTI data */
    id = identifyInput(s);
/****************** debug stuff **************************/ 
/*    printf("\n%2d %50.50s", id, s);    		 */
/****************** debug stuff **************************/ 
    if ( id==7 ) continue;

/*********** funky fix stuff to get around funky hacked code ***********/
/*    if ( id==5 ) { sscanf(s, "%*35c %3s", trace->IPFversion); continue; }   */
    if ( id==5 ){sscanf(s, "%*35c %3s", trace->IPFversion); id=identifyInput(&s[5]);}
/*********** funky fix stuff to get around funky hacked code ***********/

    if ( id==6 ) { strcpy(trace->IPFversion,"PTI"); *state|=DATA_PTI; continue;}
    if ( id==60) { strcpy(format, SYST_FORMAT_312); continue; }
    if ( id==50) { strcpy(format, COMO_FORMAT_312); continue; }
    if ( id==51) {
        strcpy(format, COMO_FORMAT_314);
        continue;
    }
    if ( id==20) { strcpy(format, OUTG_FORMAT_312); }
    if ( id==21) { strcpy(format, OUTG_FORMAT_PTI); }
    if ( id==30) { strcpy(format, OVLD_FORMAT_312); }
    if ( id==31) { strcpy(format, OVLD_FORMAT_314); }
    if ( id==32) { strcpy(format, OVLD_FORMAT_316); }
    if ( id==33) { strcpy(format, OVLD_FORMAT_PTI); }
    if ( id==40) { strcpy(format, BUSV_FORMAT_312); }
    if ( id==41) { strcpy(format, BUSV_FORMAT_316); }
    if ( id==42) { strcpy(format, BUSV_FORMAT_PTI); }
    if ( (*state & SRCHN_4_COMO_SUM) && (id==10) ) {
        *state &= ~(SRCHN_4_COMO_SUM);
        *state |= READING_COMO_SUM;
        continue;
    }
    if ( (*state & SRCHN_4_OUTG_SUM) && (id==11 || id==13) ) {
        *state &= ~(SRCHN_4_OUTG_SUM);
        *state |= READING_OUTG_SUM;
        continue;
    }
    if ( (*state & SRCHN_4_FAIL_SUM) && (id==12) ) {
        *state &= ~(GET_OVLD | GET_BUSV);
        *state &= ~(SRCHN_4_FAIL_SUM);
        *state |= READING_FAIL_SUM;
        continue;
    }
    if (id==-1) {
        if ( *state & READING_COMO_SUM ) *state &= ~READING_COMO_SUM;
        if ( *state & READING_OUTG_SUM ) *state &= ~READING_OUTG_SUM;
        if ( *state & READING_FAIL_SUM ) *state &= ~READING_FAIL_SUM;
    }
    if ( !(*state & (READING | SRCHING)) ) {
        return NULL;
    }
    if ( !(*state & READING) ) continue;
    if ( (id==30 || id==31 || id==32 || id==33) && (*state & GET_OUTG) ) {
        *state &= ~GET_BUSV; *state |= GET_OVLD;
    }
    if ( (id==40 || id==41 || id==42) && (*state & GET_OUTG) ) {
        *state &= ~GET_OVLD; *state |= GET_BUSV;
    }
    if ( (id==20 || id==21) && (*state & READING_OUTG_SUM) ) { /* branch/common mode/PTI outage */
        *state &= ~(GET_OVLD | GET_BUSV | GET_OUTG); /* clear all GET_* flags */
        if (*state & DATA_PTI) {
            np = NULL;                    /* no common-mode redundant outages */
        }
        else {              /* only check for redundant outages with IPF data */
            sscanf(s, " %38[^\n]", name);
            np = lookup(name);
        }
        if ( np!=NULL ) {     /* condition satified only if IPF AND redundant */
            if (np->keep == 0) continue;
            if (np->keep == 2) {
                cf_logErr("Redundant Outage Skipped: [%s]\n", name);
                continue;
            }
            cf_freeList(problem);
            problem = getCoMoOutg(s);
        } 
        else {
            cf_freeList(problem);
            problem = getBrchOutg(s, format);
        }
        if (problem!=NULL) *state |= GET_OUTG;/*set only if valid outage found*/
        if (*state & DATA_PTI) {
            *state |= GET_OVLD; strcpy(format, OVLD_FORMAT_PTI);
        }
    }
    if ( (id==0) && (*state & READING_COMO_SUM) ) { /* install: 0-skip, 1-rpt */
        if ( sscanf(s, COMO_MOD_NA_312, name) == 1) {
            if (install(name, 0)==NULL) cf_logErr("install(name, 0) failed\n");
        }
        else if ( getCoMoRslt(s, format, *trace) == 1) {
            if (install(name, 1)==NULL) cf_logErr("install(name, 1) failed\n");
        }
        if (trace->redun==REDUN_REMOVE)
            installResult(s); /* screen out outages already taken by com.mode */
    }            /* default is 0, upgrade to keep=1 if any result yields keep */
    if ( (id==0) && (*state & GET_OVLD) ) {
        cf_freeList(problem->next);
        if ( (getOvldLoad(s, format, problem)) == 1) continue;
        *state |= VALID_DATA;
        return problem;
    }
    if ( (id==0) && (*state & GET_BUSV) ) {
        cf_freeList(problem->next);
        if ( (getBusvVolt(s, format, problem)) == 1) continue;
        *state |= VALID_DATA;
        if (*state & DATA_PTI) {
            if (strcmp(format, BUSV_FORM_3_PTI)==0) {
                strcpy(format, BUSV_FORMAT_PTI);
            }
            else if (strcmp(format, BUSV_FORM_2_PTI)==0) {
                if (pos > 0) {
                    fseek(pfo_File, pos, SEEK_SET);
                    strcpy(format, BUSV_FORM_3_PTI);
                }
            }
            else if (strcmp(format, BUSV_FORMAT_PTI)==0) {
                if (pos > 0) { 
                    fseek(pfo_File, pos, SEEK_SET);
                    strcpy(format, BUSV_FORM_2_PTI);
                }
            }
        }
        return problem;
    }
    if ( (id==0) && (*state & READING_FAIL_SUM) ) {
        if ( (problem=getSystProb(s, format, problem))==NULL) continue; 
        *state |= VALID_DATA;
        return problem;
    }
  }
  return NULL;
}
void installResult(char *s)
{
  int convL, convT;
  char n1[9], v1[6], n2[9], v2[6], ckt, own[4], result[39];

  convL = sscanf(s, " L%*3cD%*3c%3[^\n]%*9c%8[^\n]%*c%5[^\n]%*c%8[^\n]%*c%5[^\n]%*c%1c",
    own, n1, v1, n2, v2, &ckt);
  convT = sscanf(s, " T%*3cD%*3c%3[^\n]%*9c%8[^\n]%*c%5[^\n]%*c%8[^\n]%*c%5[^\n]%*c%1c",
    own, n1, v1, n2, v2, &ckt);
  if (convL<5 && convT<5) return;
  sprintf(result, "%-8.8s %5.5s  %-8.8s %5.5s  %c  %-3.3s",
    n1, v1, n2, v2, ckt, own);
  if (install(result, 2)==NULL)    /* 2 indicates a common-mode outage result */
    cf_logErr("install(result, 2) failed\n");
/*  cf_logErr("Result Installed: [%s]\n", result); */
}
int  getCoMoRslt(char *s, char *format, Trace trace)
{ /* return 1 if keep, else 0 to not report on this outage */
  char type[3], chng, own[4], z1[3], z2[3];
  float v1, v2;
  if ( sscanf(s, format, type, &chng, own, z1, z2, &v1, &v2) != 7 ) return 0;
  if (type[0]!='B' && type[0]!='T' && type[0]!='L') return 0;
  if (type[0]=='B') { strcpy(z2, z1); v2 = v1; }
  if ( cf_validZone(trace.zoneOutg, z1, z2)==0 ) return 0;
  if ( cf_validOwner(trace.ownrOutg, own)==0 ) return 0;
  if ( cf_validBasekv(trace.bskvOutg, v1, v2)==0 ) return 0;
  return 1; /* report */
}
Link *getSystProb(char *s, char *format, Link *problem)
{ /* returns: NULL if not valid, Link * if valid */
  cf_Name b, *systData;
  ff_soln bo, *solnData;
  Link *systLink, *solnLink;
  char name[39];
  struct nlist *np;
  sscanf(s, " %38[^\n]", name);
  np = lookup(name);
  if (np==NULL) problem = getBrchOutg(s, format);
  else if (np->keep==1) problem = getCoMoOutg(s);
  else return NULL;
  systLink = cf_addLink(problem, sizeof(cf_Name));
  systData = (cf_Name *) systLink->data;
  strcpy(b.type, "sp");                                  /* solution problems */
  memcpy(systData, &b, sizeof(cf_Name));
  if (strstr(s, "separation")!=NULL) bo.code = 'S';
  else if (strstr(s, "(Reactive)" )!=NULL) bo.code = 'X';
  else if (strstr(s, "No solution")!=NULL) bo.code = 'F';
  else return NULL;
  solnLink = cf_addLink(systLink, sizeof(ff_soln));
  solnData = (ff_soln *) solnLink->data;
  memcpy(solnData, &bo, sizeof(ff_soln));
  return problem;
}
int  getBusvVolt(char *s, char *format, Link *problem)
{
  cf_Bus b, *busvData;
  ff_soln bo, *solnData;
  Link *busvLink, *solnLink;
  memset(&b, '\0', sizeof(cf_Bus));
  memset(&bo, '\0', sizeof(ff_soln));
  if (strncmp(format, BUSV_FORMAT_PTI, 5)==0) {
    if ( sscanf(s, format, b.name, &b.kv, &bo.ratio) < 3) return 1;
    bo.actual = bo.ratio * b.kv;
  }
  else {
    if ( sscanf(s, format, b.name, &b.kv, b.owner, b.zone, &bo.actual, &bo.ratio,
        &bo.pre_con, &bo.min_ref, &bo.max_new) < 8 ) return 1;
    if (bo.max_new==0) {          /* shift over values for old format, <v.316 */
        bo.max_new = bo.min_ref;  bo.min_ref=bo.pre_con;  bo.pre_con = -9999;
    }
  }
  strcpy(b.type, "B ");
  busvLink = cf_addLink(problem, sizeof(cf_Bus));
  solnLink = cf_addLink(busvLink, sizeof(ff_soln));
  busvData = (cf_Bus *) busvLink->data;
  solnData = (ff_soln *) solnLink->data;
  memcpy(busvData, &b, sizeof(cf_Bus));
  memcpy(solnData, &bo, sizeof(ff_soln));
  return 0;
}
int  getOvldLoad(char *s, char *format, Link *problem)
{
  cf_Branch r, *ovldData;
  ff_soln ro, *solnData;
  Link *ovldLink, *solnLink;
  
  memset(&r, '\0', sizeof(cf_Branch));
  memset(&ro, '\0', sizeof(ff_soln));
  if (strncmp(format, OVLD_FORMAT_PTI, 5)==0) {
    if (sscanf(s, format, r.bus1_name, &r.bus1_kv, r.bus2_name, &r.bus2_kv,
        &r.ckt_id, &ro.pre_con, &ro.actual, &ro.max_new, &ro.ratio)<9) return 1;
  }
  else {
    if (sscanf(s, format, r.bus1_name, &r.bus1_kv, r.bus2_name, &r.bus2_kv,
        &r.ckt_id, &r.section, r.owner, r.bus1_zone, r.bus2_zone, &ro.max_new,
        &ro.code, &ro.ratio, &ro.actual, &ro.pre_con, ro.date) < 13) return 1;
    if (ro.pre_con==0) ro.pre_con = -9999;
    ro.ratio = 100 * ro.ratio; /* per cent = 100 * Per Unit */
    date(&ro.date[0]);
  }
  if (r.bus1_kv==r.bus2_kv) strcpy(r.type, "L ");
  else strcpy(r.type, "T ");
  ovldLink = cf_addLink(problem, sizeof(cf_Branch));
  solnLink = cf_addLink(ovldLink, sizeof(ff_soln));
  ovldData = (cf_Branch *) ovldLink->data;
  solnData = (ff_soln *) solnLink->data;
  memcpy(ovldData, &r, sizeof(cf_Branch));
  memcpy(solnData, &ro, sizeof(ff_soln));
  return 0;
}
Link *getCoMoOutg(char *s)
{
  cf_Name *comoData;
  Link *comoLink;
  char  name[CF_INBUFSIZE];
  if (sscanf(s, " %38[^\n]", name) < 1) return NULL;
  comoLink = cf_newLink(sizeof(cf_Name));
  if (comoLink==NULL || comoLink->data==NULL) return NULL;
  comoLink->kind = CF_KIND_NAME;
  comoData = (cf_Name *) comoLink->data;
  strcpy(comoData->type, "cm");
  sprintf(comoData->name, "%-38s", name);
  return comoLink;
}
Link *getBrchOutg(char *s, char *format)
{
  cf_Branch *brznData, t;
  Link      *brznLink;

  memset(&t, '\0', sizeof(cf_Branch));
  if (sscanf(s, format, t.bus1_name, &t.bus1_kv, t.bus2_name, &t.bus2_kv,
    &t.ckt_id, t.owner, t.bus1_zone, t.bus2_zone) < 8) return NULL;
  brznLink = cf_newLink(sizeof(cf_Branch));
  if ( t.bus1_zone[0]=='-' || t.bus1_zone[1]=='-' ) t.bus1_zone[0] = '\0'; 
  if ( t.bus2_zone[0]=='-' || t.bus2_zone[1]=='-' ) t.bus2_zone[0] = '\0'; 
  if ( t.owner[0]=='-' ) t.owner[0] = '\0'; 
  if (t.bus1_kv==t.bus2_kv) strcpy(t.type, "L ");
  else strcpy(t.type, "T ");
  brznData = (cf_Branch *) brznLink->data;
  memcpy(brznData, &t, sizeof(cf_Branch));
  return brznLink;
}
int identifyInput(char *s)
{
  if (strstr(s, BPA_PF_PROG_VERS) != NULL) return  5; /* 5 = program version  */
  if (strstr(s, PTI_INTERAC_PSSE) != NULL) return  6; /* 6 = PTI program */
  if (strstr(s, HEADER_BREAK_PTI) != NULL) return  7; /* 7 = page break */
  if (strstr(s, HEADER_COMME_PTI) != NULL) return  7; /* 7 = header comment */
  if (strstr(s, DIS_FAC_FILE_PTI) != NULL) return  7; /* 7 = misc. data files */
  if (strstr(s, SUB_DES_FILE_PTI) != NULL) return  7; /* 7 = misc. data files */
  if (strstr(s, MON_ELE_FILE_PTI) != NULL) return  7; /* 7 = misc. data files */
  if (strstr(s, CON_DES_FILE_PTI) != NULL) return  7; /* 7 = misc. data files */
  if (strstr(s, CONTINGENCY_ANAL) != NULL) return  7; /* 7 = page break */
  if (strstr(s, OUTG_SIMUL_INPUT) != NULL) return  7; /* 7 = page break */
  if (strstr(s, __O_U_T_A_G_E__ ) != NULL) return  7; /* 7 = page header */
  if (strstr(s, "Mode  Name    ") != NULL) return  7; /* 7 = old failed sol r */
  if (strstr(s, "Results in:"   ) != NULL) return  7; /* 7 = common mode outg */
  if (strstr(s, " --- "         ) != NULL) return  7; /* 7 = common mode list */
  if (strstr(s, "(MW)   (MVAR) ") != NULL) return  7; /* 7 = common mode list */
  if (strstr(s, " MW     MVAR  ") != NULL) return  7; /* 7 = common mode list */
  if (strlen(s) <= 3)                      return  7; /* 7 = blank line */
  if (strstr(s, SUM_COM_MODE_OUT) != NULL) return 10; /*10 = common mode outg */
/**************** debug Stuff ******************
  if (strstr(s, SUM_COM_MODE_OUT) != NULL) 
     { printf("\n SUM_COM_MODE_OUT has been found \n"); return 10;}
**************** debug Stuff ******************/

  if (strstr(s, SUM_PROBS_4E_OUT) != NULL) return 11; /*11 = outage report */
  if (strstr(s, SUM_SYST_SEP_DIV) != NULL) return 12; /*12 = failed soln rept */
  if (strstr(s, CONTIN_EVENT_PTI) != NULL) return 13; /*13 = outage report,PTI*/
  if (strstr(s, "Summary of"    ) != NULL) return -1; /*-1 = wrong summary */
  if (strstr(s, "- - - - - - - ") != NULL) return 20; /*20 = outage */
  if (strstr(s, OPEN_LINE_FR_PTI) != NULL) return 21; /*21 = outage, PTI */
  if (strstr(s, OVLD_RAT_TY_312 ) != NULL) return 30; /*30 = overload follows */
  if (strstr(s, OVLD_RAT_TY_314 ) != NULL) return 31; /*31 = ovld new format */
  if (strstr(s, OVLD_RAT_TY_316 ) != NULL) return 32; /*32 = ovld newest form*/
  if (strstr(s, OVLD_FRO_TO_PTI ) != NULL) return 33; /*33 = ovld PTI format */
  if (strstr(s, BUS_VV_A_PU_312 ) != NULL) return 40; /*40 = old bus volt form*/
  if (strstr(s, BUS_VV_A_PU_316 ) != NULL) return 41; /*41 = new bus volt form*/
  if (strstr(s, BUS_BUS_VPU_PTI ) != NULL) return 42; /*42 = PTI bus volt form*/
  if (strstr(s, TY_CH_OWN_B_312 ) != NULL) return 50; /*50 = old co mo format */
  if (strstr(s, TY_CH_OWN_Z_315 ) != NULL) return 51; /*51 = new co mo format */
  if (strstr(s, OUTG_ID_OWN_314 ) != NULL) return 60; /*60 = failed soln rept */
  if (strstr(s, OUTG_ID_OWN_312 ) != NULL) return 60; /*60 = old failed sol r */
  return 0;                                           /* 0 = possible data */
}
void buildReport(Trace *trace, cf_Out *rpt)
{ /* build table for output report */
  Link      *curRow, *mainRow, *newRow, *probLink, *outgLink, *solnLink;
  cf_Name   *probData;
  cf_Branch *outgData;
  int        Dbug = 0;

  trace->rptTable = NULL;
  printf(" Building Report Table\n");
  curRow = NULL;
  for(mainRow=trace->mainTable; mainRow!=NULL; mainRow=mainRow->next) {
    outgLink = (Link *) mainRow->data;
    probLink = (outgLink!=NULL) ? outgLink->next : NULL;
    solnLink = (probLink!=NULL) ? probLink->next : NULL;
    probData = (probLink!=NULL) ? (cf_Name *) probLink->data : NULL;

    outgData = (cf_Branch *) outgLink->data;
    if (Dbug==1) {
        printf("\noutg: %s%5.1f %s%5.1f %c",
            outgData->bus1_name, outgData->bus1_kv,
            outgData->bus2_name, outgData->bus2_kv, outgData->ckt_id);
    }
    if (probData==NULL) continue;
    if (probData->type[0]!='s') {
        if (rpt->type==TYPE_OUTG_OVLD || rpt->type==TYPE_OVLD_OUTG) {     /* outg, ovld */
            if ( validLoad(*trace, solnLink)==0 ) continue;
        }
        else if (rpt->type==TYPE_OUTG_BUSV || rpt->type==TYPE_BUSV_OUTG) {/* outg, busv */
            if (solnLink->prev!=probLink) printf("uggg!\n");
            if (probLink->next!=solnLink) printf("bugg!\n");
            if ( validVolt(*trace, solnLink)==0 ) continue;
        }
    }
    else if (validSoln(solnLink)==0) continue;                   /* skip if 0 */
    if (rpt->diff==VARI_YES) {
        if (!validVariance(*trace, solnLink)) continue;
    }
    newRow = cf_link2row(outgLink);
    if (rpt->type==TYPE_OVLD_OUTG || rpt->type==TYPE_BUSV_OUTG)
        swapRowIdLinks((Link *) newRow->data);

    if (trace->sort==SORT_BY_INPUT) {
        cf_insLink(&(trace->rptTable), &curRow, newRow, (long) trace,
            CF_INS_SKIP, (int (*)(Link*, Link*, int))(ff_srtByInput));
    }
    else {
        cf_insLink(&(trace->rptTable), &curRow, newRow, (long) trace->sort,
            CF_INS_SKIP, (int (*)(Link*, Link*, int))(ff_cmpRow));
    }

    if (Dbug==1) {
        printf(" valid: %s%5.1f %s%5.1f %c",
            outgData->bus1_name, outgData->bus1_kv,
            outgData->bus2_name, outgData->bus2_kv, outgData->ckt_id);
    }
  }
  return;
}
void swapReport(Trace *trace)
{ /* build table for output report */
  Link *curRow, *rptRow, *tmpRow, *tmpTable;

  if (trace->aux->type==TYPE_NONE) return;
  printf(" Swapping Report Table\n");

  curRow = NULL;
  rptRow = trace->rptTable;
  tmpTable = NULL;
  while(rptRow!=NULL) {
    tmpRow = rptRow;
    if (rptRow->prev!=NULL) rptRow->prev->next = rptRow->next;
    if (rptRow->next!=NULL) rptRow->next->prev = rptRow->prev;
    rptRow = rptRow->next;

    tmpRow->next = NULL;
    tmpRow->prev = NULL;

    swapRowIdLinks((Link *) tmpRow->data);
    if (trace->sort==SORT_BY_INPUT) {
        cf_insLink(&(tmpTable), &curRow, tmpRow, (long) trace,
            CF_INS_SKIP, (int (*)(Link*, Link*, int))(ff_srtByInput));
    }
    else {
        cf_insLink(&(tmpTable), &curRow, tmpRow, (long) trace->sort,
            CF_INS_SKIP, (int (*)(Link*, Link*, int))(ff_cmpRow));
    }
  }
  trace->rptTable = tmpTable;
  return;
}
void swapRowIdLinks(Link *idLink)
{ /* swaps only the pointers to the Id objects */
  Link *tempLink;
  tempLink = (Link *) idLink->data;
  idLink->data = idLink->next->data;
  idLink->next->data = tempLink;
}
void ff_appSoln2Row(Link *curRow, Link *solnLink, int skip)
{
  Link *linkPtr;
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
int ff_cmpID(Link *newLink, Link *xstLink)
{
  cf_Branch *newIdData, *xstIdData;
  if (newLink==NULL || xstLink==NULL) return 1;
  if (newLink->data==NULL || xstLink->data==NULL) return -1;
  newIdData = (cf_Branch *) newLink->data;
  xstIdData = (cf_Branch *) xstLink->data;
  if (newIdData->type[0]=='c') {                        /* common_mode_outage */
    return ff_cmpName((cf_Name *) newIdData, (cf_Name *) xstIdData);
  }
  else if (newIdData->type[0]=='B') {                         /* bus u/o volt */
    return cf_cmpBus((pf_AC_bus *) newIdData, (pf_AC_bus *) xstIdData);
  }
  else if (newIdData->type[0]=='s') {                       /* system problem */
    return (newIdData->type[0] - xstIdData->type[0]);
  }
  else {                                /* branch outage or overloaded branch */
    return cf_cmpBranch((pf_branch *) newIdData, (pf_branch *) xstIdData);
  }
}
int ff_cmpType(Link *newId, Link *xstId)
{
  cf_Name *newType, *xstType;
  newType = (cf_Name *) newId->data;
  xstType = (cf_Name *) xstId->data;
  if ( newType->type[0] < xstType->type[0] ) return -1;        /* [type]-sort */
  if ( newType->type[0] > xstType->type[0] ) return  1;        /* [type]-sort */
  if ( newType->type[0]=='s' ) return 1;          /* force sort with ff_srtID */
  return 0;
}
int ff_cmpName(cf_Name *b1, cf_Name *b2)
{
  float c;
  if ( c=strncmp(b1->type, b2->type, 1) ) return c>0 ? 1 : -1;
  if ( c=strcmp(b1->name, b2->name) )     return c>0 ? 1 : -1;
  return 0;
}
int ff_cmpRow(Link *newRow, Link *xstRow, int sort)
{
  int srt;
  Link *newId, *xstId;

  newId = (Link *) newRow->data;
  xstId = (Link *) xstRow->data;

  if (sort==SORT_BY_SOLN || sort==SORT_BY_DIFF) {
    if ( (srt=ff_cmpType(newId, xstId)) !=0 ) return srt;
    if ( (srt=ff_srtSoln(newId->next->next, xstId->next->next, sort)) != 0 )
        return srt;
  }
  if ( (srt=ff_srtID(newId, xstId, sort)) != 0 ) return srt;
  else return ff_srtID(newId->next, xstId->next, sort);
}
int ff_srtSoln(Link *newSoln, Link *xstSoln, int sort)
{ /* compares only the solution in the first case for sorting purposes */
  ff_soln *newData, *xstData;
  float    newValu,  xstValu;

  if (newSoln==NULL && xstSoln==NULL) return  0;
  if (newSoln==NULL && xstSoln!=NULL) return  1;
  if (newSoln!=NULL && xstSoln==NULL) return -1;

  if (newSoln->next==NULL && xstSoln->next==NULL) return  0;
  if (newSoln->next==NULL && xstSoln->next!=NULL) return  1;
  if (newSoln->next!=NULL && xstSoln->next==NULL) return -1;
/* also could be called SORT_BY_LAST */
  if (sort==SORT_BY_DIFF) { /* newSoln->refSoln->delSoln: delSoln may be NULL */
    while (newSoln!=NULL && newSoln->next!=NULL) newSoln=newSoln->next;
    while (xstSoln!=NULL && xstSoln->next!=NULL) xstSoln=xstSoln->next;
    newData = (ff_soln *) ( newSoln!=NULL ? newSoln->data : NULL );
    xstData = (ff_soln *) ( xstSoln!=NULL ? xstSoln->data : NULL );
  }
  else {
    newData = (ff_soln *) newSoln->data;
    xstData = (ff_soln *) xstSoln->data;
  }
  newValu = (newData!=NULL) ? newData->ratio : 0;
  xstValu = (xstData!=NULL) ? xstData->ratio : 0;
  if (newValu==0 && xstValu==0) return 0;
  if (newValu < xstValu) return  1;
  if (newValu > xstValu) return -1;
  return 0;
}
int ff_srtID(Link *newLink, Link *xstLink, int sort)
{
  cf_Branch *newIdData, *xstIdData;

  if (newLink==NULL || xstLink==NULL) return 1;
  if (newLink->data==NULL || xstLink->data==NULL) return -1;
  newIdData = (cf_Branch *) newLink->data;
  xstIdData = (cf_Branch *) xstLink->data;
  if (newIdData->type[0]=='c') {                        /* common_mode_outage */
    return ff_cmpName((cf_Name *) newIdData, (cf_Name *) xstIdData);
  }
  else if (newIdData->type[0]=='B') {                         /* bus u/o volt */
    return ff_srtBus((pf_AC_bus *) newIdData, (pf_AC_bus *) xstIdData, sort);
  }
  else if (newIdData->type[0]=='s') {                       /* system problem */
    return (newIdData->type[0] - xstIdData->type[0]);
  }
  else {                                /* branch outage or overloaded branch */
    return ff_srtBranch((cf_Branch *) newIdData, (cf_Branch *) xstIdData, sort);
  }
}
int ff_srtBus(pf_AC_bus *b1, pf_AC_bus *b2, int sort) /* sort bus */
{
  int c;
  float d;
  if ( (c=strcmp(b1->type, b2->type)) != 0 ) return c;         /* [type]-sort */
  if (sort==5) return 1;                                     /* [input order] */
  if (sort==4) {                                           /* [zone]-alpha-kv */
    if ( (c=strcmp(b1->zone, b2->zone)) != 0 ) return c;
  }
  else if (sort==3) {                                     /* [owner]-alpha-kv */
    if ( (c=strcmp(b1->owner, b2->owner)) != 0 ) return c;
  }
  else if (sort==2) {                                        /* [kv]-alpha-kv */
    d = (b1->kv - b2->kv);
    if ( fabs(d) > .001 ) return d;
  }
  return cf_cmpBus(b1, b2);                                        /* [alpha-kv] */
}
int ff_srtByInput(Link *newRow, Link *xstRow, void *sort)
{
  int type;
  int index1, index2;   /* 1 - first Link, 2 - second Link (first Link->next) */
  Link *newRowId, *list1, *list2, *newId1, *newId2, *xstId1, *xstId2;
  cf_Branch *newOutgData, *newProbData;
  Trace *trace;

  trace = (Trace *) sort;
  type = trace->rpt->type;
  if (type==TYPE_OUTG_OVLD || type==TYPE_OUTG_BUSV) {
    newRowId    = (Link *)      newRow->data;
    newOutgData = (cf_Branch *) newRowId->data;
    newProbData = (cf_Branch *) newRowId->next->data;
    list1 = (newOutgData->type[0]=='c') ? trace->comoList : trace->outgList;
    list2 = (newProbData->type[0]=='B') ? trace->busvList : trace->ovldList;
  }
  else if (type==TYPE_OVLD_OUTG || type==TYPE_BUSV_OUTG) {
    newRowId    = (Link *)      newRow->data;
    newProbData = (cf_Branch *) newRowId->data;
    newOutgData = (cf_Branch *) newRowId->next->data;
    list1 = (newProbData->type[0]=='B') ? trace->busvList : trace->ovldList;
    list2 = (newOutgData->type[0]=='c') ? trace->comoList : trace->outgList;
  }
  newId1 = (Link *) newRow->data;
  newId2 = (Link *) newId1->next;
  xstId1 = (Link *) xstRow->data;
  xstId2 = (Link *) xstId1->next;
  index1 = getIndex((Link *) newId1, (Link *) xstId1, list1);
  if (index1!=0) return index1;
  index2 = getIndex((Link *) newId2, (Link *) xstId2, list2);
  return index2;
}
int getIndex(Link *newLink, Link *xstLink, Link *list)
/* determine which branch/bus comes first in the input list */
/* uses a start-at-beginning-and-search-whole-list approach */
{
  if (list==NULL) return ff_cmpID(newLink, xstLink);    /* default [alpha-kv] */
  if (ff_cmpID(newLink, xstLink)==0) return 0;     /* newLink same as xstLink */
  for (; list!=NULL; list=list->next) {
    if (ff_cmpID(list, newLink)==0) return -1;     /* switching return values */
    if (ff_cmpID(list, xstLink)==0) return  1;     /* reverses the sort order */
  }
  return  1;           /* +1 & -1 seem to have same result for return value */
}
void printReport(Trace *trace, cf_Out *rpt)
{
  int      i, doN, skipN, cmp;
  Link    *curRow, *curID, *curSoln, *refID, *pfoPtr, *curPfo;

  if (rpt->type==TYPE_NONE) return;

  openReport(rpt);

  skipN  = 0;
  curPfo = trace->pfoList;
  if (curPfo==NULL || trace->rptTable==NULL) {
    fprintf(rpt->file, "*** Warning: No Data Found! ***");
  }
  for (pfoPtr=curPfo;curPfo!=NULL && trace->rptTable!=NULL;curPfo=pfoPtr) {
    for (doN=0; pfoPtr!=NULL && doN<rpt->wide; pfoPtr=pfoPtr->next) doN++;
    printHeader(rpt, curPfo);
    for (curRow = trace->rptTable; curRow != NULL; curRow=curRow->next) {
        if ( rpt->form!=FORM_DATA_FULL && rpt->line==0 )
            printHeader(rpt, curPfo);
        curID = (Link *) curRow->data;
        curSoln = curID->next->next;
        for (i = skipN; curSoln!=NULL && i-- > 0; curSoln = curSoln->next);
        refID = (curRow->prev!=NULL) ? (Link *) curRow->prev->data : NULL;
        cmp = ff_cmpID(curID, refID);
        if (rpt->form==FORM_DATA_FULL || rpt->form==FORM_DATA_BREF || cmp) {
            printID(rpt, curID, 1);
        }
        if (rpt->form!=FORM_DATA_FULL && rpt->form!=FORM_DATA_BREF && cmp) {
            fprintf(rpt->file, "\n"); rpt->line++;
        }
        if (rpt->form==FORM_PAGE_WIDE) {
            printID(rpt, NULL,  1);
        }
        printID(rpt, curID->next, 2);

        chkBranch (trace, curID);  /* deLink branches with overloads from  No O/L Log */

        printSoln(rpt, curID, curSoln, doN);
        refID = (curRow->next!=NULL) ? (Link *) curRow->next->data : NULL;
        cmp = ff_cmpID(curID, refID);
        if (rpt->form!=FORM_DATA_FULL && rpt->form!=FORM_DATA_BREF && cmp) {
            fprintf(rpt->file, "\n"); rpt->line++;
        }
        if (rpt->form!=FORM_DATA_FULL && rpt->form!=FORM_DATA_BREF) {
            pageFooter(rpt, curRow->next, pfoPtr);
        }
    }

     chkBranchLog (trace, curPfo);  /* send No O/L Log to cf_logErr */       

    skipN += doN;
  }
}

void chkBranch(Trace *trace, Link *curID)
/* deLink branches with overloads from  No O/L Log */
{
  cf_Branch *ovldData, *chkBranchData;        /*  No O/L Log support */
  Link    *chkBranchStep, *chkBranchOvld;     /*  No O/L Log support */
        chkBranchOvld = curID->next;
        ovldData = (cf_Branch *)chkBranchOvld->data;
        chkBranchStep = trace->chkBranchStart;                /* dmstest */
        while (chkBranchStep!=NULL) {
          chkBranchData = (cf_Branch *)chkBranchStep->data;
          if  (ff_cmpID(chkBranchStep, chkBranchOvld) == 0) {
            chkBranchStep = cf_delLink(&trace->chkBranchStart, chkBranchStep);
          }
          else {            
            chkBranchStep = chkBranchStep->next;
          }
        }
  return;
}

void chkBranchLog(Trace *trace, Link *curPfo)
/* send No O/L Log to cf_logErr */

{
  int      cmp;
  cf_Branch *ovldData, *chkBranchData;        /*  No O/L Log support */
  Link    *chkBranchStep, *chkBranchOvld;     /*  No O/L Log support */

        chkBranchStep = trace->chkBranchStart;
        while (chkBranchStep!=NULL) {
          chkBranchData = (cf_Branch *)chkBranchStep->data;
          cf_logErr(" %9s %7.2f %9s %7.2f %c is not overloaded in %s\n",
          chkBranchData->bus1_name, chkBranchData->bus1_kv,
          chkBranchData->bus2_name, chkBranchData->bus2_kv, chkBranchData->ckt_id,
          curPfo->data);
          chkBranchStep = chkBranchStep->next;
        }
  return;
}

void printHeader(cf_Out *rpt, Link *curPfo)
{
       if (rpt->type==TYPE_OUTG_OVLD) printOtBrHeader(rpt, curPfo);
  else if (rpt->type==TYPE_OUTG_BUSV) printOtBsHeader(rpt, curPfo);
  else if (rpt->type==TYPE_OVLD_OUTG) printOvldHeader(rpt, curPfo);
  else if (rpt->type==TYPE_BUSV_OUTG) printBusvHeader(rpt, curPfo);
  return;
}
void printID(cf_Out *rpt, Link *curID, int position)
{
  if (rpt->type==TYPE_OUTG_OVLD) {
    if (position==1) printOutg(rpt->file, curID, rpt->form);
    if (position==2) printOvld(rpt->file, curID, rpt->form);
  }
  if (rpt->type==TYPE_OUTG_BUSV) {
    if (position==1) printOutg(rpt->file, curID, rpt->form);
    if (position==2) printBusv(rpt->file, curID, rpt->form);
  }
  if (rpt->type==TYPE_OVLD_OUTG) {
    if (position==1) printOvld(rpt->file, curID, rpt->form);
    if (position==2) printOutg(rpt->file, curID, rpt->form);
  }
  if (rpt->type==TYPE_BUSV_OUTG) {
    if (position==1) printBusv(rpt->file, curID, rpt->form);
    if (position==2) printOutg(rpt->file, curID, rpt->form);
  }
}
void printSoln(cf_Out *rpt, Link *curID, Link *curSoln, int doN)
{
  cf_Name *data;
       if (rpt->type==TYPE_OUTG_OVLD) data = (cf_Name *) curID->next->data;
  else if (rpt->type==TYPE_OUTG_BUSV) data = (cf_Name *) curID->next->data;
  else if (rpt->type==TYPE_OVLD_OUTG) data = (cf_Name *) curID->data;
  else if (rpt->type==TYPE_BUSV_OUTG) data = (cf_Name *) curID->data;
  if (data->type[0]=='s') {
    printProb(rpt, curSoln, doN);
  }
  else if (rpt->type==TYPE_OUTG_OVLD || rpt->type==TYPE_OVLD_OUTG) {
    printLoad(rpt, curSoln, doN);
  }
  else if (rpt->type==TYPE_OUTG_BUSV || rpt->type==TYPE_BUSV_OUTG) {
    printVolt(rpt, curSoln, doN);
  }
  return;
}
void pageFooter(cf_Out *rpt, Link *nxRow, Link *nxPfo)
{
  if (nxRow!=NULL) {
    ff_printPageFooter(rpt, 3);
  }
  else if (nxRow==NULL && nxPfo!=NULL) {
    ff_printPageFooter(rpt, 9);
    fprintf(rpt->file, "%s\n", DOT_LINE);
    rpt->line++;
    ff_printPageFooter(rpt, FF_PAGE_LENGTH);
  }
  else if (nxRow==NULL && nxPfo==NULL) {
    ff_printPageFooter(rpt, 10);
    tableLegend(rpt);
    ff_printPageFooter(rpt, FF_PAGE_LENGTH);
  }
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
void printOtBrHeader(cf_Out *rpt, Link *pfo)
{ /* COULD USE DEFINE STATEMENTS */
  char *HdLd[] = { /* lead */
    ".|   |OUTAGES CAUSING OVERLOADS| |  |  | |   |      CASE NAME: ->      | | |  |  |    |   |",
    ".|   |                         | |  |  | |   |                         | | |  |  |    |   |",
    ".|   |    Outaged  Branches    | |  |  | |   |   Overloaded Branches   | | |  |  |CRTC|IN |",
    ".|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|Z1|Z2|T|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|S|Z1|Z2|RATG|MYY|",
    " ",
    ".|   |OUTAGES CAUSING OVERLOADS| | |   |      CASE NAME: ->      | |    |",
    ".|   |                         | | |   |                         | |    |",
    ".|   |    Outaged  Branches    | | |   |   Overloaded Branches   | |CRTC|",
    ".|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|T|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|RATG|",
    " ",
    ".|   |OUTAGES CAUSING OVERLOADS| | |  |  |    |   |",
    ".|   | Outage followed by...   | | |  |  |    |   |",
    ".|   |    ...List of Overloads | | |  |  |CRTC|IN |",
    ".|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|S|Z1|Z2|RATG|MYY|",
    " ",
  };
  char *HdRp[] = { /* repeat */
    "%16.16s||||",
    "                ||||",
    " PRE  | POST |POST||",
    " LOAD | LOAD |(%)|F|",
    " ",
    "%4.4s||",
    "    ||",
    "POST||",
    "(%)|F|",
    " ",
  };
  char *HdDf[] = { /* difference */
    "", "", "", "", "",
    "       |       |",
    "POST(%)|POST(%)|",
    "VARIANC|VARIANC|",
    "   UP  |  DOWN |",
    " ",
  };
  int lf, rf, df;
  if (rpt->form==FORM_PAGE_WIDE) lf = 0;
  if (rpt->form==FORM_DATA_FULL) lf = 0;
  if (rpt->form==FORM_DATA_BREF) lf = 5;
  if (rpt->form==FORM_PAGE_COMP) lf = 10;
  rf = (rpt->form==FORM_DATA_BREF) ? 5 : 0;
  df = (rpt->diff==VARI_YES) ? 5 : 0;
  printTableCases(rpt,  pfo, HdLd[0+lf], HdRp[0+rf], HdDf[0+df]);
  printTableHeader(rpt, pfo, HdLd[1+lf], HdRp[1+rf], HdDf[1+df]);
  printTableHeader(rpt, pfo, HdLd[2+lf], HdRp[2+rf], HdDf[2+df]);
  printTableHeader(rpt, pfo, HdLd[3+lf], HdRp[3+rf], HdDf[3+df]);
  printTableHeader(rpt, pfo, HdLd[4+lf], HdRp[4+rf], HdDf[4+df]);
  rpt->line += 5;
  return;
}
void printOtBsHeader(cf_Out *rpt, Link *pfo)
{
  char *HdLd[] = { /* lead */
    ".|   |OUTAGE CAUSING UO VOLTAGE| |  |  | |   |CASE NAME:->|  |     |     |",
    ".|   |                         | |  |  | |   |            |  |LOWER|UPPER|",
    ".|   |     Outaged  Branch     | |  |  | |   |U/O  Voltage|  |LIMIT|LIMIT|",
    ".|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|Z1|Z2|T|OWN|< BUS1 ><V1>|ZN|( PU)|( PU)|",
    " ",
    ".|   |OUTAGE CAUSING UO VOLTAGE| | |   |CASE NAME:->|  |",
    ".|   |                         | | |   |            |  |",
    ".|   |     Outaged  Branch     | | |   |U/O  Voltage|  |",
    ".|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|T|OWN|< BUS1 ><V1>|ZN|",
    " ",
    ".|   |OUTAGE CAUSING UO VOLTAGE| | |  |  |     |     |",
    ".|   | Outage followed by...   | | |  |  |LOWER|UPPER|",
    ".|   |       ...List of Busses | | |  |  |LIMIT|LIMIT|",
    ".|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|S|Z1|Z2|( PU)|( PU)|",
    " ",
  };
  char *HdRp[] = { /* repeat */
    "%16.16s|||",
    "                |||",
    "ACTUA|ACTUA|CHANGE|",
    "( KV)|( PU)|( PU )|",
    " ",
    "%5.5s|",
    "     |",
    "ACTUA|",
    "( PU)|",
    " ",
  };
  char *HdDf[] = { /* difference */
    "", "", "", "", "",
    "       |       |",
    "VARIANC|VARIANC|",
    "  UP   | DOWN  |",
    "(  PU )|(  PU )|",
    " ",
  };
  int lf, rf, df;
  if (rpt->form==FORM_PAGE_WIDE) lf = 0;
  if (rpt->form==FORM_DATA_FULL) lf = 0;
  if (rpt->form==FORM_DATA_BREF) lf = 5;
  if (rpt->form==FORM_PAGE_COMP) lf = 10;
  rf = (rpt->form==FORM_DATA_BREF) ? 5 : 0;
  df = (rpt->diff==VARI_YES) ? 5 : 0;
  printTableCases(rpt,  pfo, HdLd[0+lf], HdRp[0+rf], HdDf[0+df]);
  printTableHeader(rpt, pfo, HdLd[1+lf], HdRp[1+rf], HdDf[1+df]);
  printTableHeader(rpt, pfo, HdLd[2+lf], HdRp[2+rf], HdDf[2+df]);
  printTableHeader(rpt, pfo, HdLd[3+lf], HdRp[3+rf], HdDf[3+df]);
  printTableHeader(rpt, pfo, HdLd[4+lf], HdRp[4+rf], HdDf[4+df]);
  rpt->line += 5;
  return;
}
void printOvldHeader(cf_Out *rpt, Link *pfo)
{ /* COULD USE DEFINE STATEMENTS */
  char *HdLd[] = { /* lead */
    ".|   |OVERLOAD CAUSED BY OUTAGE| | |  |  | |   |      CASE NAME: ->      | |  |  |    |   |",
    ".|   |                         | | |  |  | |   |                         | |  |  |    |   |",
    ".|   |   Overloaded Branches   | | |  |  | |   |    Outaged  Branches    | |  |  |CRTC|IN |",
    ".|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|S|Z1|Z2|T|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|Z1|Z2|RATG|MYY|",
    " ",
    ".|   |OVERLOAD CAUSED BY OUTAGE| | |   |      CASE NAME: ->      | |    |",
    ".|   |                         | | |   |                         | |    |",
    ".|   |   Overloaded Branches   | | |   |    Outaged  Branches    | |CRTC|",
    ".|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|T|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|RATG|",
    " ",
    ".|   |OVERLOAD CAUSED BY OUTAGE| | |  |  |    |   |",
    ".|   | Overload followed by... | | |  |  |    |   |",
    ".|   |      ...List of Outages | | |  |  |CRTC|IN |",
    ".|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|S|Z1|Z2|RATG|MYY|",
    " ",
  };
  char *HdRp[] = { /* repeat */
    "%16.16s||||",
    "                ||||",
    " PRE  | POST |POST||",
    " LOAD | LOAD |(%)|F|",
    " ",
    "%4.4s||",
    "    ||",
    "POST||",
    "(%)|F|",
    " ",
  };
  char *HdDf[] = { /* difference */
    "", "", "", "", "",
    "       |       |",
    "POST(%)|POST(%)|",
    "VARIANC|VARIANC|",
    "   UP  |  DOWN |",
    " ",
  };
  int lf, rf, df;
  if (rpt->form==FORM_PAGE_WIDE) lf = 0;
  if (rpt->form==FORM_DATA_FULL) lf = 0;
  if (rpt->form==FORM_DATA_BREF) lf = 5;
  if (rpt->form==FORM_PAGE_COMP) lf = 10;
  rf = (rpt->form==FORM_DATA_BREF) ? 5 : 0;
  df = (rpt->diff==VARI_YES) ? 5 : 0;
  printTableCases(rpt,  pfo, HdLd[0+lf], HdRp[0+rf], HdDf[0+df]);
  printTableHeader(rpt, pfo, HdLd[1+lf], HdRp[1+rf], HdDf[1+df]);
  printTableHeader(rpt, pfo, HdLd[2+lf], HdRp[2+rf], HdDf[2+df]);
  printTableHeader(rpt, pfo, HdLd[3+lf], HdRp[3+rf], HdDf[3+df]);
  printTableHeader(rpt, pfo, HdLd[4+lf], HdRp[4+rf], HdDf[4+df]);
  rpt->line += 5;
  return;
}
void printBusvHeader(cf_Out *rpt, Link *pfo)
{ /* COULD USE DEFINE STATEMENTS */
  char *HdLd[] = { /*  lead  */
    ".|   |U/O VOLT DUE|  | |   |      CASE NAME: ->      | |  |  |     |     |",
    ".|   | TO  OUTAGE |  | |   |                         | |  |  |LOWER|UPPER|",
    ".|   |U/O  Voltage|  | |   |    Outaged  Branches    | |  |  |LIMIT|LIMIT|",
    ".|OWN|< BUS1 ><V1>|ZN|T|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|Z1|Z2|( PU)|( PU)|",
    " ",
    ".|   |U/O VOLT DUE|  | |   |      CASE NAME: ->      | |",
    ".|   | TO  OUTAGE |  | |   |                         | |",
    ".|   |U/O  Voltage|  | |   |    Outaged  Branches    | |",
    ".|OWN|< BUS1 ><V1>|ZN|T|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|",
    " ",
    ".|   |U/O VOLTAGE DUE TO OUTAGE| | |  |  |     |     |",
    ".|   | Bus followed by...      | | |  |  |LOWER|UPPER|",
    ".|   |      ...List of Outages | | |  |  |LIMIT|LIMIT|",
    ".|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|S|Z1|Z2|( PU)|( PU)|",
    " ",
  };
  char *HdRp[] = { /* repeat */
    "%16.16s|||",
    "                |||",
    "ACTUA|ACTUA|CHANGE|",
    "( KV)|( PU)|( PU )|",
    " ",
    "%5.5s|",
    "     |",
    "ACTUA|",
    "( PU)|",
    " ",
  };
  char *HdDf[] = { /* difference */
    "", "", "", "", "",
    "       |       |",
    "VARIANC|VARIANC|",
    "  UP   | DOWN  |",
    "(  PU )|(  PU )|",
    " ",
  };
  int lf, rf, df;
  if (rpt->form==FORM_PAGE_WIDE) lf = 0;
  if (rpt->form==FORM_DATA_FULL) lf = 0;
  if (rpt->form==FORM_DATA_BREF) lf = 5;
  if (rpt->form==FORM_PAGE_COMP) lf = 10;
  rf = (rpt->form==FORM_DATA_BREF) ? 5 : 0;
  df = (rpt->diff==VARI_YES) ? 5 : 0;
  printTableCases(rpt,  pfo, HdLd[0+lf], HdRp[0+rf], HdDf[0+df]);
  printTableHeader(rpt, pfo, HdLd[1+lf], HdRp[1+rf], HdDf[1+df]);
  printTableHeader(rpt, pfo, HdLd[2+lf], HdRp[2+rf], HdDf[2+df]);
  printTableHeader(rpt, pfo, HdLd[3+lf], HdRp[3+rf], HdDf[3+df]);
  printTableHeader(rpt, pfo, HdLd[4+lf], HdRp[4+rf], HdDf[4+df]);
  rpt->line += 5;
  return;
}
void printTableCases(cf_Out *rpt, Link *curPfo, char *l, char *r, char *d)
{
  int width;
  fprintf(rpt->file, "%s", l);
  for (width=rpt->wide; curPfo!=NULL && width-- > 0; curPfo=curPfo->next) {
    fprintf(rpt->file, r, curPfo->data); /* case name */
  }
  fprintf(rpt->file, "%s", d);
  fprintf(rpt->file, "\n");
}
void printTableHeader(cf_Out *rpt, Link *curPfo, char *l, char *r, char *d)
{
  int width;
  fprintf(rpt->file, "%s", l); /* lead */
  for (width=rpt->wide; curPfo!=NULL && width-- > 0; curPfo=curPfo->next) {
    fprintf(rpt->file, "%s", r); /* repeat */
  }
  fprintf(rpt->file, "%s", d); /* difference */
  fprintf(rpt->file, "\n");
}
void printOutg(FILE *fp, Link *outgLink, int form)
{
  char id[CF_STRSIZE], out[CF_STRSIZE];
  pf_rec d;
  cf_Branch *outgData;
  cf_Name *comoData;
  outgData = (outgLink!=NULL) ? (cf_Branch *) outgLink->data : NULL;
  if (outgData==NULL || outgLink==NULL) {
    if      (form==FORM_PAGE_COMP) sprintf(out, "%41.41s|", "");
    else if (form==FORM_DATA_BREF) sprintf(out, "%32.32s|", "");
    else                           sprintf(out, "%39.39s|", "");
  }
  else if (outgData->type[0]!='c') {
    cf_branch2rec(outgData, &d);
    pf_rec_b2a(id, &d, "I");
    if (form==FORM_PAGE_COMP)
        sprintf(out, "%-1.1s|%-3.3s|%-12.12s %-12.12s|%1.1s| |%-2.2s|%-2.2s|",
            &id[0], &id[3], &id[6], &id[19], &id[31], outgData->bus1_zone,
            outgData->bus2_zone);
    else if (form==FORM_DATA_BREF)
        sprintf(out, "%-1.1s|%-3.3s|%-12.12s %-12.12s|%1.1s|",
            &id[0], &id[3], &id[6], &id[19], &id[31]);
    else
        sprintf(out, "%-1.1s|%-3.3s|%-12.12s %-12.12s|%1.1s|%-2.2s|%-2.2s|",
            &id[0], &id[3], &id[6], &id[19], &id[31], outgData->bus1_zone,
            outgData->bus2_zone);
  }
  else if (outgData->type[0]=='c') { /* common mode */
    comoData = (cf_Name *) outgData;
    if      (form==FORM_PAGE_COMP) sprintf(out,"%-35.35s|||||||",comoData->name);
    else if (form==FORM_DATA_BREF) sprintf(out,"%-30.30s||||",   comoData->name);
    else                           sprintf(out,"%-34.34s||||||", comoData->name);
  }
  fprintf(fp, "%s", out);
}
void printOvld(FILE *fp, Link *ovldLink, int form)
{
  char id[CF_STRSIZE], out[CF_STRSIZE];
  pf_rec d;
  cf_Branch *ovldData;

  ovldData = (ovldLink!=NULL) ? (cf_Branch *) ovldLink->data : NULL;
  if (ovldData==NULL || ovldLink==NULL) {
    if (form==FORM_DATA_BREF) sprintf(out, "%32.32s|", "");
    else                      sprintf(out, "%41.41s|", "");
  }
  else if (ovldData->type[0]!='s') {
    cf_branch2rec(ovldData, &d);
    pf_rec_b2a(id, &d, "I");
    if (form==FORM_DATA_BREF)
        sprintf(out, "%-1.1s|%-3.3s|%-12.12s %-12.12s|%1.1s|",
            &id[0], &id[3], &id[6], &id[19], &id[31]);
    else
        sprintf(out, "%-1.1s|%-3.3s|%-12.12s %-12.12s|%1.1s|%1.1s|%-2.2s|%-2.2s|",
            &id[0], &id[3], &id[6], &id[19], &id[31], &id[32],
            ovldData->bus1_zone, ovldData->bus2_zone);
  }
  else if (ovldData->type[0]=='s') {
    if (form==FORM_DATA_BREF)
        sprintf(out, "      SOLUTION     PROBLEMS   ||||");
    else
        sprintf(out, "      SOLUTION     PROBLEMS        |||||||");
  }
  fprintf(fp, "%s", out);
}
void printBusv(FILE *fp, Link *busvLink, int form)
{
  char id[CF_STRSIZE], out[CF_STRSIZE];
  pf_rec d;
  cf_Bus *busvData;

  busvData = (busvLink!=NULL) ? (cf_Bus *) busvLink->data : NULL;
  if (busvData==NULL || busvLink==NULL) {
    if (form==FORM_DATA_BREF) sprintf(out, "%21.21s|", "");
    else                      sprintf(out, "%21.21s|", "");
  }
  else if (busvData->type[0]=='s') {
    if (form==FORM_PAGE_COMP) /* compact sparse */
        sprintf(out, "        SOLUTION     PROBLEMS    |||||||");
    else if (form==FORM_DATA_BREF)
        sprintf(out," SOLUTION PROBLEMS||||");
    else
        sprintf(out," SOLUTION PROBLEMS||||");
  }
  else if (busvData->type[0]!='s') {
    memset(&d, '\0', sizeof(pf_rec));
    memcpy(&d, busvData, sizeof(cf_Bus));
    pf_rec_b2a(id, &d, "I");
    if (form==FORM_PAGE_COMP) /* compact sparse */
        sprintf(out, "%-1.1s|%-3.3s|%-12.12s %-12.12s|%1.1s| |%-2.2s|%-2.2s|",
            &id[0], &id[3], &id[6], "", "", &id[18], "");
    else if (form==FORM_DATA_BREF)
        sprintf(out, "%-1.1s|%-3.3s|%-12.12s|%-2.2s|",
            &id[0], &id[3], &id[6], &id[18]);
    else
        sprintf(out, "%-1.1s|%-3.3s|%-12.12s|%-2.2s|",
            &id[0], &id[3], &id[6], &id[18]);
  }
  fprintf(fp, "%s", out);
}
void printDiff(cf_Out *rpt, Link *solnLink, int mode)
{
  ff_soln *varData;

  while (solnLink!=NULL && solnLink->next!=NULL) solnLink = solnLink->next;
  if (mode==0 || solnLink==NULL || solnLink->data==NULL) {
    fprintf(rpt->file, "       |       |");
  }
  else {
    varData = (ff_soln *) solnLink->data;
    if (mode==1) {
        fprintf(rpt->file, "%7.1f|%7.1f|", varData->max_new, varData->min_ref);
    }
    if (mode==2) {
        fprintf(rpt->file, "%7.3f|%7.3f|", varData->max_new, varData->min_ref);
    }
  }
  return;
}
void printLoad(cf_Out *rpt, Link *solnLink, int doN)
{
  Link    *curLink, *refLink;
  ff_soln *curSoln, *refSoln;
  char     code;

  refLink=solnLink;                /* find first valid data for CRITCL RATING */
  for (; refLink!=NULL && refLink->data==NULL; refLink=refLink->next);
  if (refLink!=NULL && refLink->data!=NULL) {
    refSoln = (ff_soln *) refLink->data;
    if (rpt->form==FORM_DATA_BREF)
        fprintf(rpt->file, "%4.0f|", refSoln->max_new);
    else
        fprintf(rpt->file, "%4.0f|%3.3s|",refSoln->max_new,refSoln->date);
  }
  else fprintf(rpt->file, "    |   |");
  for (curLink = solnLink; curLink!=NULL; curLink=curLink->next) {
    if (doN-- <= 0) break;
    if (curLink->data==NULL) {
        if (rpt->form==FORM_DATA_BREF) fprintf(rpt->file, "   | |");
        else fprintf(rpt->file, "      |      |   | |");
    }
    else {
        curSoln = (ff_soln *) curLink->data;
        code = curSoln->code;
        if (refLink->data!=NULL && curSoln->max_new != refSoln->max_new) {
            code = tolower(curSoln->code);
        }
        if (rpt->form==FORM_DATA_BREF)
            fprintf(rpt->file, "%3.0f|%1.1s|", curSoln->ratio, &code);
        else {
            if ((int)curSoln->pre_con == -9999) /* no pre_con data found */
                fprintf(rpt->file, "      |%6.1f|%3.0f|%1.1s|",
                    curSoln->actual, curSoln->ratio, &code);
            else
                fprintf(rpt->file, "%6.1f|%6.1f|%3.0f|%1.1s|",
                    curSoln->pre_con, curSoln->actual, curSoln->ratio, &code);
        }
    }
  }
  while (doN-- > 0) {
    if (rpt->form==FORM_DATA_BREF) fprintf(rpt->file, "   | |");
    else fprintf(rpt->file, "      |      |   | |");
  }
  if (rpt->diff==VARI_YES) printDiff(rpt, solnLink, 1);
  fprintf(rpt->file, "\n");
  rpt->line += 1;
  return;
}
void printVolt(cf_Out *rpt, Link *solnLink, int doN)
{
  Link    *curLink, *refLink;
  ff_soln *curSoln, *refSoln;

  refLink=solnLink;                                            /* find LIMITS */
  for (; refLink!=NULL && refLink->data==NULL; refLink=refLink->next);
  if (refLink!=NULL && refLink->data!=NULL) {
    refSoln = (ff_soln *) refLink->data;
    if (rpt->form==FORM_DATA_BREF)
        fprintf(rpt->file, "");
    else
        fprintf(rpt->file, "%5.3f|%5.3f|",refSoln->min_ref, refSoln->max_new);
  }
  else {
    if (rpt->form==FORM_DATA_BREF)
        fprintf(rpt->file, "");
    else
        fprintf(rpt->file, "     |     |      |");
  }
  for (curLink = solnLink; curLink!=NULL; curLink=curLink->next) {
    if (doN-- <= 0) break;
    if (curLink->data==NULL) {
        if (rpt->form==FORM_DATA_BREF)
            fprintf(rpt->file, "     |");
        else
            fprintf(rpt->file, "     |     |      |");
    }
    else {
        curSoln = (ff_soln *) curLink->data;
        if (rpt->form==FORM_DATA_BREF)
            fprintf(rpt->file, "%5.3f|", curSoln->ratio);
        else {
            if ((int)curSoln->pre_con == -9999) /* no change data found */
                fprintf(rpt->file,"%5.1f|%5.3f|      |", curSoln->actual,
                    curSoln->ratio);
            else
                fprintf(rpt->file,"%5.1f|%5.3f|%6.3f|", curSoln->actual,
                    curSoln->ratio, curSoln->pre_con);
        }
    }
  }
  while (doN-- > 0) {
    if (rpt->form==FORM_DATA_BREF) fprintf(rpt->file, "     |");
    else fprintf(rpt->file, "     |     |      |");
  }
  if (rpt->diff==VARI_YES) printDiff(rpt, solnLink, 2);
  fprintf(rpt->file, "\n");
  rpt->line += 1;
  return;
}
void printProb(cf_Out *rpt, Link *probLink, int doN)
{
  char *Problem[] = {
    "    |   |",            /* overload report; blank rating, blank in-date */
    "                 |||", /* overload report; blank solution problem */
    "      %11.11s|||",     /* overload report; format for probData->name */
    "     |     |",         /* bus violation; blank lower-, upper-limit */
    "             ||",      /* bus violation; blank solution problem */
    "      %11.11s||",      /* bus violation; format for probData->name */
    "    |",                /* overload report; blank rating, blank code */
    "    ||",               /* overload report; blank solution problem */
    "%4.4s||",              /* overload report; format for probData->name */
    "",                     /* bus violation; blank lower-, upper-limit */
    "     |",               /* bus violation; blank solution problem */
    "%5.5s|",               /* bus violation; format for probData->name */
  };
  int      offset;
  Link    *curProb;
  ff_soln *probData;
  char     descrip[11];

  offset=(rpt->type==TYPE_OUTG_OVLD || rpt->type==TYPE_OVLD_OUTG) ? 0:3;
  offset += (rpt->form==FORM_DATA_BREF) ? 6 : 0;
  fprintf(rpt->file, Problem[0+offset]);
  for (curProb = probLink; curProb!=NULL; curProb=curProb->next) {
    if (doN-- <= 0) break;
    probData = (ff_soln *) curProb->data;
    if (probData==NULL)
        fprintf(rpt->file, Problem[1+offset]);
    else {
        if (rpt->form!=FORM_DATA_BREF) {
                 if (probData->code=='F') strcpy(descrip, "NO SOLUTION");
            else if (probData->code=='X') strcpy(descrip, "FAILED RX  ");
            else if (probData->code=='S') strcpy(descrip, "SEPARATION ");
            else if (probData->code=='K') strcpy(descrip, "SOLUTION OK");
            else                          strcpy(descrip, "UNKNOWN    ");
        }
        else {
                 if (probData->code=='F') strcpy(descrip, "FAIL");
            else if (probData->code=='X') strcpy(descrip, " RX ");
            else if (probData->code=='S') strcpy(descrip, "SEPA");
            else if (probData->code=='K') strcpy(descrip, " OK ");
            else                          strcpy(descrip, " ?? ");
        }
        fprintf(rpt->file, Problem[2+offset], descrip);
    }
  }
  while (doN-- > 0) fprintf(rpt->file, Problem[1+offset]);
  if (rpt->diff==VARI_YES) printDiff(rpt, probLink, 0);
  fprintf(rpt->file, "\n");
  rpt->line += 1;
  return;
}
int validSoln(Link *solnLink)
{
 ff_soln *solnData;
 while (solnLink!=NULL) {
    solnData = (ff_soln *) solnLink->data;
    if (solnData!=NULL) {
        if ( solnData->code != 'S' )
            return 1;  /* keep if solution failed for other than a separation */
    }
    solnLink = solnLink->next;
 }                                                    
 return 0;
}
int validInput(Link *listPtr, cf_Branch *data)
{ /* uses a start-at-beginning-and-search-whole-list approach */
  cf_Branch *listData;
  int        cmp;

  if (listPtr==NULL) return -1;
  for (cmp=1; listPtr!=NULL; listPtr=listPtr->next) {
    listData = (cf_Branch *) listPtr->data;
    if (listData->type[0] != data->type[0]) continue;
    if (data->type[0]=='c') {                          /* common-mode outages */
        cmp = ff_cmpName((cf_Name *) listData, (cf_Name *) data);
    }
    else if (data->type[0]=='B') {                                   /* buses */
        cmp = cf_cmpBus( (pf_AC_bus *) listData, (pf_AC_bus *) data );
    }
    else {                                    /* branch outages and overloads */
        cmp = ff_wildBranch( (pf_branch *) listData, (pf_branch *) data );
    }
    if (cmp==0) return 1;
  }
  return 0;
}
int validVariance(Trace trace, Link *refLink)
/* return 1 if difference with problem solving, or variation exceeds span */
{
  Link       *variLink, *solnLink;
  ff_soln    *refData, *solnData, *variData;
  float       refRat, solRat, solMax, solMin, varUp, varDown, varSpan;
  int         refCode, solCode;

  refData = (refLink==NULL) ? NULL : (ff_soln *) refLink->data;
  refCode = (refData==NULL) ? ' ' : ((ff_soln *) refLink->data)->code;

/*
   the following loop returns 1 (valid) if not all the cases have data or if
   they failed to solve for different reasons
*/
  for (solnLink=refLink->next; solnLink!=NULL; solnLink=solnLink->next) {
    solCode = (solnLink->data!=NULL) ? ((ff_soln *)solnLink->data)->code : ' ';
    if ((refCode=='S')|(refCode=='X')|(refCode=='F')|(refCode=='K')) {
        if (refCode!=solCode) return 1;
    }
  }

  refRat = (refData==NULL) ? 0 : refData->ratio;
  solMax = refRat;
  solMin = refRat;

  for (solnLink=refLink->next; solnLink!=NULL; solnLink=solnLink->next) {
    solnData = (ff_soln *)solnLink->data;
    solRat = (solnData==NULL) ? 0 : solnData->ratio;
    solMax = Cf_max(solMax, solRat);
    solMin = Cf_min(solMin, solRat);
  }
  varUp   = solMax - refRat;            /* i.e.   3% =  101% - 98% */
  varDown = solMin - refRat;            /* i.e.  -5% =   93% - 98% */
  varSpan = varUp - varDown;

  if (refData==NULL) {
    varUp = varDown = varSpan = 0;
  }
  variLink = cf_newLink(sizeof(ff_soln));
  cf_appList(&refLink, variLink);
  variData = (ff_soln *) variLink->data;

  variData->min_ref = varDown;
  variData->max_new = varUp;
  variData->actual  = varSpan;
  variData->ratio   = (varUp > -1*varDown) ? varUp : varDown;
  variData->code    = ' ';

  if (varUp<trace.varCeiling && varDown>trace.varFloor) return 0;

  return 1;
}
int validLoad(Trace trace, Link *solnLink)
{
  ff_soln *solnData;
  while (solnLink!=NULL) {
    solnData = (ff_soln *) solnLink->data;
    if (solnData!=NULL && solnData->ratio >= trace.minLoadPct) return 1;
    solnLink = solnLink->next;
  }
  return 0;
}
int validVolt(Trace trace, Link *solnLink)
{
  ff_soln *solnData;
  float vpu, basekv;
  cf_Bus  *busvData;
  if (solnLink==NULL || solnLink->prev==NULL || solnLink->prev->data==NULL)
    return 0;
  busvData = (cf_Bus *) solnLink->prev->data;
  basekv = busvData->kv;
  for (; solnLink!=NULL; solnLink = solnLink->next) {
    if ( (solnData = (ff_soln *) solnLink->data) == NULL ) continue;
    vpu = solnData->ratio;
    if      ((int)basekv==500 && (vpu>=trace.max5 || vpu<=trace.min5)) return 1;
    else if ((int)basekv!=500 && (vpu>=trace.maxV || vpu<=trace.minV)) return 1;
  }
  return 0;
}
int ff_wildBranch(pf_branch *b1, pf_branch *b2)
/******************************************************************************\
* ff_wildBranch: compare id data of two pf_branch records with wild card.
* pf_branch *b1 - ptr to branch rec. with optional wild card ckt_id and section.
* pf_branch *b2 - pointer to branch record.
* returns: int - 0 if same, 1 if b1 > b2, -1 if b1 < b2.
* developed by: WDRogers  created: 08-22-96
\******************************************************************************/
{
  float c;
  if ( c=strncmp(b1->type, b2->type, 1) )       return c>0 ? 1 : -1;
  if ( c=strcmp(b1->bus1_name, b2->bus1_name) ) return c>0 ? 1 : -1;
  if ( fabs(c=b1->bus1_kv-b2->bus1_kv) > .001 ) return c>0 ? 1 : -1;
  if ( c=strcmp(b1->bus2_name, b2->bus2_name) ) return c>0 ? 1 : -1;
  if ( fabs(c=b1->bus2_kv-b2->bus2_kv) > .001 ) return c>0 ? 1 : -1;
  if ( b1->ckt_id != ' ' ) {       /* if blank, accept any ckt_id */
    if ( c=b1->ckt_id - b2->ckt_id )            return c>0 ? 1 : -1;
  }
  if ( b1->section != 0 ) {        /* if zero, accept any section */
    if ( c=b1->section - b2->section )          return c>0 ? 1 : -1;
  }
  return 0;
}
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
        if (strcmp(s, np->name) == 0)
            return np;  /* found */
    return NULL;        /* not found */
}
/* install: put (name, keep) in hash tab */
struct nlist *install(char *name, int keep)
{
    struct nlist *np;
    unsigned hashval;

    if ((np = lookup(name)) == NULL) { /* not found */
        np = (struct nlist *) malloc(sizeof(*np));
        if (np == NULL || (np->name = cf_strdup(name)) == NULL)
            return NULL;
        hashval = hash(name);
        np->next = hashtab[hashval];
        hashtab[hashval] = np;
    }
    np->keep = keep;
    return np;
}    
void ff_stream2List(FILE *readMe, Trace *trace, Link **expList)
{
  char str[CF_INBUFSIZE], STR[CF_INBUFSIZE];
  Link *list;
  int mode = READ_INC; /* default /INCLUDE */
  while (fgets(str, CF_INBUFSIZE, readMe)!=NULL) { 
    fprintf (stderr, " %5d %5d [%s]\n", entry, ++count, str);
    if (count > 6545) {
      fprintf (stderr, "ff_stream2List: Processing record # %10d\n", count);
    }


    if ( cf_iscomment(str) ) continue;
    strcpy(STR, str);
    cf_str2upper(STR);
    if ( strstr(STR, "/INCLUDE"     )!=NULL ) { mode = READ_INC; continue; }
    if ( strstr(STR, "/PFO"         )!=NULL ) { mode = READ_PFO; continue; }
    if ( strstr(STR, "/OUTAGE"      )!=NULL ) { mode = READ_OUT; continue; }
    if ( strstr(STR, "/COMMON_MODE" )!=NULL ) { mode = READ_COM; continue; }
    if ( strstr(STR, "/OVERLOAD"    )!=NULL ) { mode = READ_OVR; continue; }
    if ( strstr(STR, "/BUS"         )!=NULL ) { mode = READ_BUS; continue; }
    if ( strstr(STR, "/OUTG_OWNER"  )!=NULL ) { mode = READ_OWN; continue; }
    if ( strstr(STR, "/OUTG_ZONE"   )!=NULL ) { mode = READ_ZON; continue; }
    if ( strstr(STR, "/OUTG_BASE_KV")!=NULL ) { mode = READ_BKV; continue; }
    if ( strstr(STR, "/PROB_OWNER"  )!=NULL ) { mode = READ_OWP; continue; }
    if ( strstr(STR, "/PROB_ZONE"   )!=NULL ) { mode = READ_ZNP; continue; }
    if ( strstr(STR, "/PROB_BASE_KV")!=NULL ) { mode = READ_KVP; continue; }
    if ( strstr(STR, "/LIMITS"      )!=NULL ) { mode = READ_LIM; continue; }
    if ( strstr(STR, "/REPORT"      )!=NULL ) { mode = READ_REP; continue; }
    if ( STR[0]=='/' ) { mode = 0; continue; }     /* unrecognized slash card */
    if ( mode == 0 ) continue;         /* note: mode is defaulted to READ_INC */
    if ( mode == READ_INC ) {
        list = cf_text2List(str);
        ff_expList(trace, list, expList);
    }
    if ( mode == READ_PFO ) {
        list = cf_text2List(str);
        cf_appList(&trace->pfoList, list);
        if (list!=NULL) trace->query &= ~(QUERY_PFO);
    }
    if ( mode == READ_OUT ) {
        if (strchr(STR, FF_WILD)==NULL) {
            list = cf_id2Link(STR, 'L');
            cf_appList(&trace->outgList, list);
        }
        else {
            list = cf_text2Link(STR);
            cf_appList(&trace->outgMask, list);
        }
        if (list!=NULL) trace->query &= ~(QUERY_DATA);
        if (list!=NULL) trace->query &= ~(QUERY_OUTG);
        if (list!=NULL) trace->query &= ~(QUERY_COMO);
        if (list!=NULL) trace->query &= ~(QUERY_ZONE);
        if (list!=NULL) trace->query &= ~(QUERY_OWNR);
        if (list!=NULL) trace->query &= ~(QUERY_BSKV);
    }
    if ( mode == READ_COM ) {
        if (strchr(STR, FF_WILD)==NULL) {
            list = getCoMoOutg(STR);
            cf_appList(&trace->comoList, list);
        }
        else {
            list = cf_text2Link(STR);
            cf_appList(&trace->comoMask, list);
        }
        if (list!=NULL) trace->query &= ~(QUERY_DATA);
        if (list!=NULL) trace->query &= ~(QUERY_OUTG);
        if (list!=NULL) trace->query &= ~(QUERY_COMO);
        if (list!=NULL) trace->query &= ~(QUERY_ZONE);
        if (list!=NULL) trace->query &= ~(QUERY_OWNR);
        if (list!=NULL) trace->query &= ~(QUERY_BSKV);
    }
    if ( mode == READ_OVR ) {
        if (strchr(STR, FF_WILD)==NULL) {
            list = cf_id2Link(STR, 'L');
            cf_appList(&trace->ovldList, list);
        }
        else {
            list = cf_text2Link(STR);
            cf_appList(&trace->ovldMask, list);
        }
        if (list!=NULL) trace->query &= ~(QUERY_DATA);
        if (list!=NULL) trace->query &= ~(QUERY_OVLD);
        if (list!=NULL) trace->query &= ~(QUERY_BUSV);
        if (list!=NULL) trace->query &= ~(QUERY_ZN_P);
        if (list!=NULL) trace->query &= ~(QUERY_OW_P);
        if (list!=NULL) trace->query &= ~(QUERY_KV_P);
    }
    if ( mode == READ_BUS ) {
        if (strchr(STR, FF_WILD)==NULL) {
            list = cf_id2Link(STR, 'I');
            cf_appList(&trace->busvList, list);
        }
        else {
            list = cf_text2Link(STR);
            cf_appList(&trace->busvMask, list);
        }
        if (list!=NULL) trace->query &= ~(QUERY_DATA);
        if (list!=NULL) trace->query &= ~(QUERY_OVLD);
        if (list!=NULL) trace->query &= ~(QUERY_BUSV);
        if (list!=NULL) trace->query &= ~(QUERY_ZN_P);
        if (list!=NULL) trace->query &= ~(QUERY_OW_P);
        if (list!=NULL) trace->query &= ~(QUERY_KV_P);
    }
    if ( mode == READ_OWN ) {
        list = cf_text2List(STR);
        if (list!=NULL) trace->query &= ~(QUERY_OWNR);
        if (list!=NULL) trace->query &= ~(QUERY_DATA);
        if (list!=NULL) trace->query &= ~(QUERY_OUTG);
        if (list!=NULL) trace->query &= ~(QUERY_COMO);
        cf_appList(&trace->ownrOutg, list);
    }
    if ( mode == READ_ZON ) {
        list = cf_text2List(STR);
        if (list!=NULL) trace->query &= ~(QUERY_ZONE);
        if (list!=NULL) trace->query &= ~(QUERY_DATA);
        if (list!=NULL) trace->query &= ~(QUERY_OUTG);
        if (list!=NULL) trace->query &= ~(QUERY_COMO);
        cf_appList(&trace->zoneOutg, list);
    }
    if ( mode == READ_BKV ) {
        list = cf_text2List(STR);
        if (list!=NULL) trace->query &= ~(QUERY_BSKV);
        if (list!=NULL) trace->query &= ~(QUERY_DATA);
        if (list!=NULL) trace->query &= ~(QUERY_OUTG);
        if (list!=NULL) trace->query &= ~(QUERY_COMO);
        cf_appList(&trace->bskvOutg, list);
    }
    if ( mode == READ_OWP ) {
        list = cf_text2List(STR);
        if (list!=NULL) trace->query &= ~(QUERY_OW_P);
        if (list!=NULL) trace->query &= ~(QUERY_DATA);
        if (list!=NULL) trace->query &= ~(QUERY_OVLD);
        if (list!=NULL) trace->query &= ~(QUERY_BUSV);
        cf_appList(&trace->ownrProb, list);
    }
    if ( mode == READ_ZNP ) {
        list = cf_text2List(STR);
        if (list!=NULL) trace->query &= ~(QUERY_ZN_P);
        if (list!=NULL) trace->query &= ~(QUERY_DATA);
        if (list!=NULL) trace->query &= ~(QUERY_OVLD);
        if (list!=NULL) trace->query &= ~(QUERY_BUSV);
        cf_appList(&trace->zoneProb, list);
    }
    if ( mode == READ_KVP ) {
        list = cf_text2List(STR);
        if (list!=NULL) trace->query &= ~(QUERY_KV_P);
        if (list!=NULL) trace->query &= ~(QUERY_DATA);
        if (list!=NULL) trace->query &= ~(QUERY_OVLD);
        if (list!=NULL) trace->query &= ~(QUERY_BUSV);
        cf_appList(&trace->bskvProb, list);
    }
    if ( mode == READ_LIM ) ff_limits(STR, trace);
    if ( mode == READ_REP ) ff_report(str, trace);
  }
  return;
}
void ff_limits(char *s, Trace *trace)
{
  float f;
  if (strstr(s, "VARIANCE_CEILING")!=NULL) {
    sscanf(s, "%*s = %f", &f);
    trace->varCeiling = f;
    trace->query &= ~(QUERY_SPAN);
  }
  if (strstr(s, "VARIANCE_FLOOR")!=NULL) {
    sscanf(s, "%*s = %f", &f);
    trace->varFloor = f;
    trace->query &= ~(QUERY_SPAN);
  }
  if (strstr(s, "MIN_LOAD_PCT")!=NULL) {
    sscanf(s, "%*s = %f", &f);
    trace->minLoadPct = f;
    trace->query &= ~(QUERY_LOAD);
  }
  if (strstr(s, "MAX_500_PU")!=NULL) {
    sscanf(s, "%*s = %f", &f);
    trace->max5 = f;
    trace->query &= ~(QUERY_VOLT);
  }
  if (strstr(s, "MIN_500_PU")!=NULL) {
    sscanf(s, "%*s = %f", &f);
    trace->min5 = f;
    trace->query &= ~(QUERY_VOLT);
  }
  if (strstr(s, "MAX_BUS_PU")!=NULL) {
    sscanf(s, "%*s = %f", &f);
    trace->maxV = f;
    trace->query &= ~(QUERY_VOLT);
  }
  if (strstr(s, "MIN_BUS_PU")!=NULL) {
    sscanf(s, "%*s = %f", &f);
    trace->minV = f;
    trace->query &= ~(QUERY_VOLT);
  }
  if (strstr(s, "INC_SOLN_PROB")!=NULL) {
    if (strstr(s, "NO" )!=NULL) trace->soln = SKP_SOLN_PROB;
    if (strstr(s, "YES")!=NULL) trace->soln = INC_SOLN_PROB;
    trace->query &= ~(QUERY_SOLN);
  }
}
void ff_report(char *s, Trace *trace)
{
  if (strstr(s, "NAME"           )!=NULL) {
    sscanf(s, "%*s = %s", trace->outName);
    trace->query &= ~(QUERY_NAME);
    return;
  }
  if (strstr(s, "TYPE"           )!=NULL) {
    if (strstr(s, "OUTAGE-OVERLOAD" )!=NULL) trace->type = TYPE_OUTG_OVLD;
    if (strstr(s, "OVERLOAD-OUTAGE" )!=NULL) trace->type = TYPE_OVLD_OUTG;
    if (strstr(s, "OVERLOAD-BOTH"   )!=NULL) trace->type = TYPE_OVLD_BOTH;
    if (strstr(s, "OUTAGE-BUS_V"    )!=NULL) trace->type = TYPE_OUTG_BUSV;
    if (strstr(s, "BUS_V-OUTAGE"    )!=NULL) trace->type = TYPE_BUSV_OUTG;
    if (strstr(s, "BUS_V-BOTH"      )!=NULL) trace->type = TYPE_BUSV_BOTH;
    if (trace->type==TYPE_OUTG_OVLD) trace->query &= ~(QUERY_BUSV | QUERY_VOLT);
    if (trace->type==TYPE_OVLD_OUTG) trace->query &= ~(QUERY_BUSV | QUERY_VOLT);
    if (trace->type==TYPE_OVLD_BOTH) trace->query &= ~(QUERY_BUSV | QUERY_VOLT);
    if (trace->type==TYPE_OUTG_BUSV) trace->query &= ~(QUERY_OVLD | QUERY_LOAD);
    if (trace->type==TYPE_BUSV_OUTG) trace->query &= ~(QUERY_OVLD | QUERY_LOAD);
    if (trace->type==TYPE_BUSV_BOTH) trace->query &= ~(QUERY_OVLD | QUERY_LOAD);
    trace->query &= ~(QUERY_TYPE);
    return;
  }
  if (strstr(s, "CASES_PER_TABLE")!=NULL) {
    sscanf(s, "%*s = %d", &trace->trc->wide);
    if (trace->trc->wide<=0) trace->trc->wide = 1;
    trace->query &= ~(QUERY_WDTH);
  }
  if (strstr(s, "FORMAT"    )!=NULL) {
    if (strstr(s, "DATA" )!=NULL) trace->trc->form = FORM_DATA_FULL; /* default  */
    if (strstr(s, "BRIEF")!=NULL) trace->trc->form = FORM_DATA_BREF; /* modifier */
    if (strstr(s, "PAGE" )!=NULL) trace->trc->form = FORM_PAGE_WIDE; /* default  */
    if (strstr(s, "COMP" )!=NULL) trace->trc->form = FORM_PAGE_COMP; /* modifier */
    trace->query &= ~(QUERY_FORM);
  }
  if (strstr(s, "SORT"  )!=NULL) {
    if (strstr(s, "ALPHA"      )!=NULL) trace->sort = SORT_BY_ALPHA;
    if (strstr(s, "BASE_KV"    )!=NULL) trace->sort = SORT_BY_BASEKV;
    if (strstr(s, "OWNER"      )!=NULL) trace->sort = SORT_BY_OWNER;
    if (strstr(s, "ZONE"       )!=NULL) trace->sort = SORT_BY_ZONE;
    if (strstr(s, "INPUT"      )!=NULL) trace->sort = SORT_BY_INPUT;
    if (strstr(s, "SOLUTION"   )!=NULL) trace->sort = SORT_BY_SOLN;
    if (strstr(s, "DIFFERENCE" )!=NULL) trace->sort = SORT_BY_SOLN;
    trace->query &= ~(QUERY_SORT);
  }
  if (strstr(s, "TRACE"   )!=NULL) {
    if (strstr(s, "YES")!=NULL) trace->trc->type = TYPE_TRACE;
    if (strstr(s, "NO" )!=NULL) trace->trc->type = TYPE_NONE;
    trace->query &= ~(QUERY__TRC);
  }
  if (strstr(s, "VARIANCE"   )!=NULL) {
    if (strstr(s, "YES")!=NULL) trace->trc->diff = VARI_YES;
    if (strstr(s, "NO" )!=NULL) trace->trc->diff = VARI_NO;
    trace->query &= ~(QUERY_VARI);
  }
  if (strstr(s, "REMOVE_REDUNDANT")!=NULL) {
    if (strstr(s, "YES")!=NULL) trace->redun = REDUN_REMOVE;
    if (strstr(s, "NO" )!=NULL) trace->redun = REDUN_KEEP;
    trace->query &= ~(QUERY_REDU);
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
int ff_srtBranch(cf_Branch *b1, cf_Branch *b2, int sort) /* sort branch */
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
int validMask(Link *maskList, cf_Name *r)
{
  Link *maskLink;
  char *maskData, netData[CF_STRSIZE], netVal[34], mskVal[34];
  int   n;
  pf_rec t;

  if (maskList==NULL || maskList->data==NULL) return -1;
  for (maskLink=maskList; maskLink!=NULL; maskLink=maskLink->next) {
    maskData = (char *) maskLink->data;
    switch (toupper(r->type[0])) {
        case 'B' :  cf_bus2rec((cf_Bus *) r, &t); 
                    pf_rec_b2a(netData, &t, "I"); break;
        case 'E' :
        case 'L' :
        case 'T' :  cf_branch2rec((cf_Branch *) r, &t); 
                    pf_rec_b2a(netData, &t, "I"); break;
        case 'C' :  strcpy(netData, r->name); break;
        default  :  return 0;                      /* data at *r is not valid */
    }
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
/************ documented, common CF_UTIL.H candidates prototypes **************/
/* all documented, CF_UTIL.H candidates shall be designated, cf_name().       */
void date(char *date)
{
  int mo, yr;
  if (sscanf(date, "%d/%d", &mo, &yr) != 2) return;
  if (mo>12 || mo<1 || yr<0 || yr>99) return;
  if      (mo==10) sprintf(date, "O%02d", yr);
  else if (mo==11) sprintf(date, "N%02d", yr);
  else if (mo==12) sprintf(date, "D%02d", yr);
  else             sprintf(date, "%1d%02d", mo, yr);
}
/******** end documented, common CF_UTIL.H candidates prototypes **************/

/******************************************************************************\
  cf_util.c library
  Developed by William D. Rogers for use with Bonneville Power Administration's
  CFLOW library and Interactive Powerflow (IPF) program.

  Naming conventions: (suggested, note case)
    Cf_macro, Cf_macroName
    cf_function, cf_functionName
    CF_CONSTANT, CF_CONSTANTNAME
    CF_globalVariable
    cf_Structure, Structure
    Cx_, CX, cx,... legacy functions to be retired

  Library organization: (suggested)
    #include
    #define macros
    #define constants
    typedef struct
    global variables
    function prototypes
    functions
\******************************************************************************/

void cf_sprompt(char *prompt, char *dflt, char *reply)
/******************************************************************************\
* cf_sprompt: (s-string, white-space delimited)
* Prompt with default for a white-space delimited string.
* char *prompt  - string directing user response with optional default string
* char *dflt    - default response string if user-reply not convertable
* char *reply   - pointer to destination of response: default or user-reply
* Developed by: WDRogers  Updated: 2-14-95
\******************************************************************************/
{
  int conv = 0;
  printf(prompt, dflt);
  fgets(CF_inbuf, CF_INBUFSIZE, stdin);
  conv = sscanf(CF_inbuf, "%s", reply);
  if ( conv <= 0 )
    strcpy(reply, dflt);
}

void cf_kprompt(char *prompt, char dflt, char *reply)
/******************************************************************************\
* cf_kprompt: (k-character, first non-white)
* Prompt with default for a single non-white-space character.
* char *prompt  - string directing user response with optional default character
* char  dflt    - default response character if user-reply not convertable
* char *reply   - pointer to destination of response: default or user-reply
* Developed by: WDRogers  Updated: 2-14-95
\******************************************************************************/
{
  int conv = 0;
  char temp[2];
  printf(prompt, dflt);
  fgets(CF_inbuf, CF_INBUFSIZE, stdin);
  conv = sscanf(CF_inbuf, "%1s", temp);
  if ( conv <= 0 )
    *reply = dflt;
  else
    *reply = temp[0];
}

void cf_mprompt(char *prompt, char *dflt, char *mask, char *reply)
/******************************************************************************\
* cf_mprompt: (m-mask, sscanf format)
* Prompt with default for a string to be scanned in with specified mask.
* char *prompt  - string directing user response with optional default string
* char *dflt    - default response string if user-reply not convertable
* char *mask    - mask string for sscanf to process user-reply with
* char *reply   - pointer to destination of response: default or user-reply
* Developed by: WDRogers  Updated: 2-14-95
\******************************************************************************/
{
  int conv = 0;
  printf(prompt, dflt);
  fgets(CF_inbuf, CF_INBUFSIZE, stdin);
  conv = sscanf(CF_inbuf, mask, reply);
  if ( conv <= 0 )
    strcpy(reply, dflt);
}

void cf_nprompt(char *prompt, char *dflt, int n, char *reply)
/******************************************************************************\
* cf_nprompt: (n-number, first n characters)
* Prompt with default for a string containing the first n characters.
* char *prompt  - string directing user response with optional default string
* char *dflt    - default response string if user-reply not convertable
* int   n       - max. number of characters to be scanned into response string
* char *reply   - pointer to destination of response: default or user-reply
* Developed by: WDRogers  Updated: 2-14-95
\******************************************************************************/
{
  int conv = 0;
  char mask[16];
  printf(prompt, dflt);
  fgets(CF_inbuf, CF_INBUFSIZE, stdin);
  sprintf(mask, "%%%d[^\n]", n);
  conv = sscanf(CF_inbuf, mask, reply);
  if ( conv <= 0 )
    strcpy(reply, dflt);
}

void cf_cprompt(char *prompt, char dflt, char *reply)
/******************************************************************************\
* cf_cprompt: (c-character, first one)
* Prompt with default for a single character.
* char *prompt  - string directing user response with optional default character
* char  dflt    - default response character if user-reply not convertable
* char *reply   - pointer to destination of response: default or user-reply
* Developed by: WDRogers  Updated: 2-14-95
\******************************************************************************/
{
  int conv = 0;
  printf(prompt, dflt);
  fgets(CF_inbuf, CF_INBUFSIZE, stdin);
  conv = sscanf(CF_inbuf, "%1c", &reply);
  if ( conv <= 0 )
    *reply = dflt;
}

void cf_fprompt(char *prompt, float dflt, float *reply)
/******************************************************************************\
* cf_fprompt: (f-float, real number)
* Prompt with default for a real (floating-point) number.
* char  *prompt  - string directing user response with optional default value
* float  dflt    - default response value if user-reply not convertable
* float *reply   - pointer to destination of response: default or user-reply
* Developed by: WDRogers  Updated: 2-14-95
\******************************************************************************/
{
  int conv = 0;
  printf(prompt, dflt);
  fgets(CF_inbuf, CF_INBUFSIZE, stdin);
  conv = sscanf(CF_inbuf, "%f", reply);
  if ( conv <= 0 )
    *reply = dflt;
}

void cf_dprompt(char *prompt, double dflt, double *reply)
/******************************************************************************\
* cf_dprompt: (d-double, double-precision real number)
* Prompt with default for a real (double-precision floating-point) number.
* char   *prompt  - string directing user response with optional default value
* double  dflt    - default response value if user-reply not convertable
* double *reply   - pointer to destination of response: default or user-reply
* Developed by: WDRogers  10-3-96
\******************************************************************************/
{
  int conv = 0;
  printf(prompt, dflt);
  fgets(CF_inbuf, CF_INBUFSIZE, stdin);
  conv = sscanf(CF_inbuf, "%f", reply);
  if ( conv <= 0 )
    *reply = dflt;
}

void cf_iprompt(char *prompt, int dflt, int *reply)
/******************************************************************************\
* cf_iprompt: (i-integer)
* Prompt with default for an integer.
* char *prompt  - string directing user response with optional default value
* int   dflt    - default response value if user-reply not convertable
* int  *reply   - pointer to destination of response: default or user-reply
* Developed by: WDRogers  Updated: 2-14-95
\******************************************************************************/
{
  int conv = 0;
  printf(prompt, dflt);
  fgets(CF_inbuf, CF_INBUFSIZE, stdin);
  conv = sscanf(CF_inbuf, "%d", reply);
  if ( conv <= 0 )
    *reply = dflt;
}

int cf_yprompt(char *prompt, int dflt)
/******************************************************************************\
* cf_yprompt: (y-yes or no response)
* Prompt with default for a yes (Y) or no (N) response.
* Return 1 for [Y]es, 0 for [N]o, else -1
* char *prompt  - string directing user response with optional default string
* int   dflt    - default response character, 'Y' or 'N'
* Developed by: WDRogers  7-12-95
\******************************************************************************/
{
  char *cp, c;
  printf(prompt, dflt);
  fgets(CF_inbuf, CF_INBUFSIZE, stdin);
  for (cp=CF_inbuf; isspace(*cp); cp++);              /* find first character */
  c = (*cp=='\0') ? toupper(dflt) : toupper(*cp);
  switch (toupper(dflt)) {
    case 'Y': return (c=='Y') ? 1 : 0;
    case 'N': return (c=='N') ? 0 : 1;
    case ' ': return (c=='Y') ? 1 : ((c=='N') ? 0 : -1);
    default : printf("cf_yprompt: invalid default %c", dflt); break;
  }
  return -1;
}

void cf_eprint(FILE *fp, char *fmt, ...)
/******************************************************************************\
* cf_eprint: (e-echo, print to stdout and file)
* prints formated string with arguments to stdout and to specified file
* FILE *fp      - pointer to file to print string to
* char *fmt     - format string for printf function followed by arguments
* Developed by: WDRogers  Updated: 2-14-95
\******************************************************************************/
{ /* print to file and to stdout */
  va_list ap;
  va_start(ap, fmt);
  vfprintf(fp, fmt, ap);
  vfprintf(stdout, fmt, ap);
  va_end(ap);
  return;
}

void cf_aprint(char *target, char *fmt, ...)
/******************************************************************************\
* cf_aprint: (a-append, append sprintf to string)
* appends results of sprintf() to target string
* char *target  - pointer to string to append results of sprintf to
* char *fmt     - format string for sprintf function followed by arguments
* Developed by: WDRogers  Updated: 2-14-95
\******************************************************************************/
{
  va_list ap;
  va_start(ap, fmt);
  vsprintf(CF_inbuf, fmt, ap);
  strcat(target, CF_inbuf);
  va_end(ap);
  return;
} 
void cf_exit(int error, char *fmt, ...)
/******************************************************************************\
* cf_exit:
* on error (error == 1) print messages and exit program, else continue
* int   error - value to be evaluated to determine if exit should occur
* char *fmt   - format of message string for printf() followed by arguments
* Developed by: WDRogers  Updated: 2-14-95
\******************************************************************************/
{ /* error == 0 means OK, no exit */
  va_list ap;
  if (!error) return;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  exit(0);
}

void cf_pfexit(int error, char *fmt, ...)
/******************************************************************************\
* cf_pfexit: (pf - powerflow, break connection with server before exiting)
* on error (error == 1) print messages and exit program, else continue
* int   error - value to be evaluated to determine if exit should occur
* char *fmt   - format of message string for printf() followed by arguments
* Developed by: WDRogers  Updated: 2-14-95
\******************************************************************************/
{ /* error == 0 means OK, no exit */
  va_list ap;
  if (!error) return;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  pf_cflow_exit(); 
  exit(0);
}

void cf_stoupper(char *str)
/******************************************************************************\
* cf_stoupper: (s - string)
* convert all characters in string to upper case.
* char *str   - pointer to string to be converted to upper case
* Developed by: WDRogers  Updated: 2-14-95
\******************************************************************************/
{
  char *cp;
  for( cp = str ; *cp != '\0' ; cp++ ) {
    *cp = toupper(*cp);
  }
}

int cf_str2list(char *list, int group, char delim)
/******************************************************************************\
* cf_str2list:
* Example: cf_str2list("B BCBE+F +I,+N ^%$AD", 2, ' ') => "B  BC BE +F +I +N AD"
* parse list into string of delimited character sequences of max. given length
* char *list  - pointer to string to be parsed and destination of result
* int   group - maximum number of consecutive characters in a sequences
* char  delim - character used to delimit parsed sequences in returned list
* Developed by: WDRogers  Updated: 2-14-95
\******************************************************************************/
{
#define CF_NONSEP_CHARS ".+-_"
  char c, *src, *sp;
  int  i = 0;
  if (*list == '\0') return 0;
  src = strchr(list, '\n');
  if ( src != NULL ) *src = '\0';
  src = calloc(sizeof(char), strlen(list) + 1);
  if ( src == NULL ) return 1;
  sp = src;
  strcpy(src, list);
  while ( (c = *src++) != '\0' ) {
    if (i == group) {
        *list++ = delim;
        i = 0;
    }
    if ( isalnum(c) || (strchr(CF_NONSEP_CHARS,c)!=NULL) ) {
        *list++ = c;
        i++;
    }
    else if ( (i > 0) && (i < group) ) {
        *list++ = delim;
        i++;
    }
  }
  while ( (i > 0) && (i < group) ) {
    *list++ = delim;
    i++;
  }
  *list = '\0';
  free(sp);
  return 0;
}

char *cf_strsuf(char *dest, char *source, char term, char *suffix)
/******************************************************************************\
* cf_strsuf: (suf - suffix)
* append suffix to source at last instance of terminating character or at end
* Example:  cf_strsuf(dest, "FGROVE.BSE", '.', ".NET") ==> dest = "FGROVE.NET"
* char *dest   - pointer to the destination string
* char *source - pointer to the source string
* char  term   - terminating character in source string
* char *suffix - string to be appended to end of source
* Developed by: WDRogers  Updated: 2-14-95
\******************************************************************************/
{
  char *end;
  strcpy(dest, source);
  if ( (end = strrchr(dest, term)) != NULL ) *end = '\0';
  return strcat(dest, suffix);
}

int cf_strcon(char *s1, char *s2)
/******************************************************************************\
* cf_strcon: (con - contains, does s1 contain s2?)
* return 1 if a string contains any occurance of a second string, else return 0
* char *s1  - pointer to string to be searched for occurances of second string
* char *s2  - pointer to string to be searched for in first string
* Developed by: WDRogers  Updated: 2-14-95
\******************************************************************************/
{
  return (strstr(s1, s2) == NULL) ? 0 : 1;
}

char *cf_parse(char *spec, char *part, int code)
/******************************************************************************\
* cf_parse: parse a file spec into its components
* char *spec:  - string containing a valid VMS file spec
* char *part:  - destination string of file spec component, or NULL
* int   code:  - indicates source file spec component, codes are as follows:
*   [S]pec,n[O]de,dis[K],[D]irectory,[N]ame,[T]ype,[V]ersion,[P]ath,[F]ile
* BPA60::DISK1:[BASECASE]FGROVE.DAT;102
* <nOde-><disK><Director><Name><Ty><Ve>
* <--------Path---------><----File---->
* <---------------Spec---------------->
* Return: char pointer to file spec component indicated by code
* Developed by: WDRogers  7-12-95  updated: 08-06-96
\******************************************************************************/
{
  char *sp, *op, *kp, *dp, *np, *tp,*vp, *ep;
  static char item[FILENAME_MAX];

  item[0] = '\0';
/*  cf_stoupper(spec); */
  for (sp = spec; isspace(*sp); sp++);/* skip leading whitespace */
  for (ep = sp;   isgraph(*ep); ep++); *ep='\0';/* remove trailing whitespace */
  if (ep==sp) {
    if (part!=NULL) strcpy(part, item);
    return item; /* strlen = 0 */
  }
  op=strstr(sp, "::"); if (op==NULL) op=sp; else op+=2;
  kp=strrchr(op, ':'); if (kp==NULL) kp=op; else kp++; /* find : or :[ */
  dp=strrchr(kp, ']'); if (dp==NULL) dp=kp; else dp++;
  np=strpbrk(dp,".;"); if (np==NULL) np=ep;
  tp=strrchr(np, ';'); if (tp==NULL) tp=ep;
  vp=strrchr(tp, ';'); if (vp==NULL) vp=ep;

  switch (toupper(code)) {
    case 'S' : strncat(item, sp, Cf_min(ep-sp, FILENAME_MAX-1)); break;
    case 'O' : strncat(item, sp, Cf_min(op-sp, FILENAME_MAX-1)); break;
    case 'K' : strncat(item, op, Cf_min(kp-op, FILENAME_MAX-1)); break;
    case 'D' : strncat(item, kp, Cf_min(dp-kp, FILENAME_MAX-1)); break;
    case 'N' : strncat(item, dp, Cf_min(np-dp, FILENAME_MAX-1)); break;
    case 'T' : strncat(item, np, Cf_min(tp-np, FILENAME_MAX-1)); break;
    case 'V' : strncat(item, tp, Cf_min(ep-vp, FILENAME_MAX-1)); break;
    case 'P' : strncat(item, sp, Cf_min(dp-sp, FILENAME_MAX-1)); break;
    case 'F' : strncat(item, dp, Cf_min(ep-dp, FILENAME_MAX-1)); break;
    default  : strncat(item, sp, Cf_min(ep-sp, FILENAME_MAX-1)); break;
  }
  if (part!=NULL) strcpy(part, item);
  return item;
}
int cf_query2spec(char *spec, char *query, char *type)
/******************************************************************************\
* cf_query2spec: parse query into a file spec and determine if it is a source
                 file or a text file containing a list of source files.
* char *spec   - destination string for a valid file spec
* char *query  - string containing text entered at a prompt
* char *type   - the file type indicating that the file is a text list
* Return type: int  - 0: no file spec, 1: source file, 2: list of source files
* Calls: cf_parse 
* Developed by: WDRogers  7-21-95
\******************************************************************************/
{
  int  len;
  cf_parse(query, spec, 'S');
  if ((len=strlen(spec))==0) return 0;                       /* no input      */
  if ( strcmp(cf_parse(spec, NULL, 'T'), type)==0 ) {        /* type of list  */
    return 2;                                                /* list of files */
  }
  if (spec[len-1]=='/') {                                    /* / for list    */
    spec[len-1] = '\0';                                      /* remove / char */
    return 2;                                                /* list of files */
  }
  return 1;                                                  /* single file   */
}


void cf_logErr(char *fmt, ...)
/******************************************************************************\
* cf_logErr: write a formatted string to a log file or to stderr
* requires CF_logFile, a globally defined file pointer initialized to NULL
* requires CF_logSpec, a globally defined string initialized by a cflow program
* char *fmt     - format string for fprintf function followed by arguments
* Return type: void
* Calls: cf_openFile 
* Developed by: WDRogers  7-21-95
\******************************************************************************/
{
  va_list ap;
  if (CF_logFile==NULL) {
    CF_logFile = stderr;
    CF_logFile = cf_openFile(CF_logSpec, "w");
    if (CF_logFile==NULL) {
        CF_logFile = stderr;                     /* if open fails, use stderr */
        strcpy(CF_logSpec, "stderr");
    }
  }
  va_start(ap, fmt);
  if (CF_logFile != NULL) vfprintf(CF_logFile, fmt, ap); 
  vfprintf(stderr, fmt, ap);
  va_end(ap);
}

/****************** add stuff for query log *******************/
void cf_logQuery(char *fmt, ...)
/******************************************************************************\
* cf_logQuery: write a formatted string to a log file or to stderr
* All strings sent to powerflow via query_pf are recorded here
* requires CF_logQFile, a globally defined file pointer initialized to NULL
* requires CF_logQSpec, a globally defined string initialized by a cflow program
* char *fmt     - format string for fprintf function followed by arguments
* Return type: void
* Calls: cf_openFile
* Developed by: DMStefonek  12-12-97
\******************************************************************************/
{
  va_list ap;
  if (CF_logQFile==NULL) {
    CF_logQFile = stderr;
    CF_logQFile = cf_openFile(CF_logQSpec, "w");
    if (CF_logQFile==NULL) {
        CF_logQFile = stderr;                     /* if open fails, use stderr */
        strcpy(CF_logQSpec, "stderr");
    }
  }
  va_start(ap, fmt);
  if (CF_logQFile != NULL) vfprintf(CF_logQFile, fmt, ap);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
}
/****************** add stuff for query log *******************/

/****************** add stuff for BAD (aka ZAP) log *******************/
void cf_logBAD(char *fmt, ...)
/******************************************************************************\
* cf_logBAD: write a formatted string to a log file or to stderr
* This file is used to advise PVCURVE users where the critical outages exist.
* requires CF_logZFile, a globally defined file pointer initialized to NULL
* requires CF_logZSpec, a globally defined string initialized by a cflow program
* char *fmt     - format string for fprintf function followed by arguments
* Return type: void
* Calls: cf_openFile
* Developed by: DMStefonek  12-12-97
\******************************************************************************/
{
  va_list ap;
  if (CF_logZFile==NULL) {
    CF_logZFile = stderr;
    CF_logZFile = cf_openFile(CF_logZSpec, "w");
    if (CF_logZFile==NULL) {
        CF_logZFile = stderr;                     /* if open fails, use stderr */
        strcpy(CF_logZSpec, "stderr");
    }
  }
 va_start(ap, fmt);
  if (CF_logZFile != NULL) vfprintf(CF_logZFile, fmt, ap);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
}
/****************** add stuff for BAD (aka ZAP) log *******************/



FILE *cf_openFile(char *spec, char *mode)
/******************************************************************************\
* cf_openFile: write a formatted string to a log file or to stderr
* char *spec    - a string containing a file specification
* char *mode    - a string containing a valid file operation mode
* Return type: FILE * - pointer to opened stream or NULL if failed
* Calls: cf_logErr
* Developed by: WDRogers  7-21-95
\******************************************************************************/
{
  FILE *fp;
  if ( (fp = fopen(spec, mode)) == NULL)
    cf_logErr("cf_openFile:  Cannot open file: %s with mode %s\n", spec, mode);
  return fp;
}
size_t cf_time(char *s, size_t smax, char *format)
/******************************************************************************\
* cf_time: writes date and time information into s according to format.
* char *s - destination for time/date string.
* size_t smax - maximum number of characters written to s.
* char *format - character string analogous to a printf format string.
* returns: size_t - number of characters written into s, excluding '\0', or zero
*   if more than smax characters were produced.
* developed by: WDRogers  updated: 01-31-96
\******************************************************************************/
{
  time_t t;
  t = time(NULL);
  return strftime(s, smax, format, localtime(&t));
}
void *cf_calloc(size_t nobj, size_t size)
/******************************************************************************\
* cf_calloc: call calloc() with error messaging; increment CF_memCurAlloc;
* size_t nobj - number of objects in the allocated array
* size_t size - size of each object in the allocated array
* calls: void cf_logErr(char *fmt, ...)
* globals: CF_memCurAlloc, CF_memTotAlloc, CF_memMaxAlloc
* returns: void * - pointer to allocated space and zero initialized space
* developed by: WDRogers  updated: 01-31-96
\******************************************************************************/
{
  void *p;
  p = NULL;
  p = calloc(nobj, size);
  if (p!=NULL) {
    CF_memCurAlloc += nobj*size; 
    if (CF_memCurAlloc < 0) {
      fprintf (stderr, "cf_calloc: memory allocated = %10d %10d %10d %10d %10d\n", 
        total_requests, size, total_memory_allocated, CF_memCurAlloc, CF_memTotAlloc);
    }
    CF_memTotAlloc += nobj*size;
    if (CF_memCurAlloc > CF_memMaxAlloc) CF_memMaxAlloc = CF_memCurAlloc;
  }
  else cf_logErr("cf_calloc:  Can't allocate any more memory!\n");
  return p;
}
void cf_free(void *obj, size_t size)
/******************************************************************************\
* cf_free: call free(); decrement CF_memCurAlloc;
* size_t size - size of object to be free'd
* globals: CF_memCurAlloc, CF_memTotFreed
* returns: void
* developed by: WDRogers  updated: 01-31-96
\******************************************************************************/
{
  free(obj);
  CF_memCurAlloc -= size;  
  CF_memTotFreed += size;
}
int cf_iscomment(char *str)
/******************************************************************************\
* cf_iscomment: determine if line has data on it; truncate at CF_TRUNCATE.
* char *str - pointer to string possibly containing data
* calls: int cf_isblank(char *str)
* returns: int - 1 if str is a comment, otherwise 0
* developed by: WDRogers  updated: 01-31-96
\******************************************************************************/
{
  char *cp;
  if ( str[0]==CF_COMMENT ) return 1;                     /* str is a comment */
  cp = strpbrk(str, CF_TRUNCATE); if (cp!=NULL) *cp='\0';  /* truncate string */
  if (cf_isblank(str)) return 1;  /* str is all white space, treat as comment */
  return 0;
}
char *cf_str2upper(char *str)
/******************************************************************************\
* cf_str2upper: (str - string, 2 - to, upper - uppercase) convert all characters
*   in string to upper case.
* char *str - pointer to string to be converted to upper case
* returns: char * - pointer to uppercase string
* developed by: WDRogers  updated: 01-31-96
\******************************************************************************/
{
  char *cp;
  for( cp = str ; *cp != '\0' ; cp++ ) *cp = toupper(*cp);
  return str;
}
char *cf_str2lower(char *str)
/******************************************************************************\
* cf_str2lower: (str - string, 2 - to, lower - lowercase) convert all characters
*   in string to lower case.
* char *str - pointer to string to be converted to lower case
* returns: char * - pointer to lowercase string
* developed by: WDRogers  created: 08-06-96
\******************************************************************************/
{
  char *cp;
  for( cp = str ; *cp != '\0' ; cp++ ) *cp = tolower(*cp);
  return str;
}
int cf_isblank(char *sp)
/******************************************************************************\
* cf_isblank: examines characters in string starting at cp.
* char *sp - pointer to a string.
* returns: int - 1 if sp is all white space; otherwise 0.
* developed by: WDRogers  updated: 01-31-96
\******************************************************************************/
{
  while (*sp!='\0' && isspace(*sp)) sp++;
  return (*sp=='\0') ? 1 : 0;
}
int cf_isDataFile(char *name)
/******************************************************************************\
* cf_isDataFile: checks file type and post-fix character to see if it is data;
*   removes / from end of name.
* char *name - the name of a file.
* calls: char *cf_parse(char *spec, char *part, int code)
* calls: char *cf_str2upper(char *s)
* returns: int - 1 if file type is in CF_DATATYPES or name appended with '/';
*   otherwise 0.
* developed by: WDRogers  updated: 01-31-96  updated: 08-06-96
\******************************************************************************/
{
  char type[FILENAME_MAX], *cp;
  cp = &name[strlen(name)-1];
  if ( *cp == '/' ) { *cp = '\0'; return 1; }
  cf_parse(name, type, 'T');                    /* get file type, i.e. ".DAT" */
  cf_str2upper(type);
  if ( strstr(CF_DATATYPES, type) != NULL ) return 1;
  return 0;
}
Link *cf_newLink(size_t size)
/******************************************************************************\
* cf_newLink: allocates memory for Link struct and its data.
* size_t size - size of data structure associated with the Link.
* calls: void *cf_malloc(size_t size)
* returns: Link * - pointer to Link or NULL if request fails.
* developed by: WDRogers  updated: 01-31-96
\******************************************************************************/
{
  Link *new;
  new = NULL;
  new = (Link *) cf_malloc(sizeof(Link));
  if (new!=NULL) {
    new->data = (size>0) ? (void *) cf_malloc(size) : NULL;
    new->size = size;
    new->next = NULL;
    new->prev = NULL;
  }
  return new;
}
int cf_cntLink(Link *cur)
/******************************************************************************\
* cf_cntLink: counts the number of links to the end of the list.
* Link *cur - pointer to a Link in a linked list of Link structs.
* returns: int - number of links to the end of the list
* developed by: WDRogers  updated: 01-31-96
\******************************************************************************/
{
  int n;
  for (n=0; cur != NULL; cur=cur->next) n++;
  return n;
}
Link *cf_addLink(Link *tar, size_t size)
/******************************************************************************\
* cf_addLink: add a new Link directly after the target Link.
* Link *tar - pointer to the targeted Link to which to add the new Link.
* size_t size - size of the data area for the new Link.
* calls: Link *cf_newLink(size_t size)
* returns: pointer to the new Link.
* developed by: WDRogers  updated: 01-31-96
\******************************************************************************/
{
  Link *new;
  new = cf_newLink(size);
  if (new!=NULL) new->prev = tar;
  if (tar!=NULL) tar->next = new;
  return (new!=NULL) ? new : tar;
}
Link *cf_delLink(Link **top, Link *del)
/******************************************************************************\
* cf_delLink: removes Link struct from list of Link structs.  Frees memory.
* Link *del - pointer to Link to be deleted from the linked list of Links.
* Link *top - pointer to pointer to top Link in linked list.
* calls: void cf_free(void *obj, size_t size)
* returns: pointer to next Link in list.
* Developed by: WDRogers  Updated: 12-06-95
\******************************************************************************/
{
  Link *prv, *nxt;
  if (del==NULL) return del;
  prv = del->prev;
  nxt = del->next;
  if (del==*top) *top = nxt;
  if (prv!=NULL) prv->next = nxt;
  if (nxt!=NULL) nxt->prev = prv;
  cf_free(del->data, del->size);
  cf_free(del, sizeof(Link));
  return nxt;
}
Link *cf_appList(Link **top, Link *src)
/******************************************************************************\
* cf_appList: append source list of Link structs to destination list.
* Link **top - pointer to pointer to the first link in the destination list.
* Link *src - pointer to the first link in the source list.
* returns: pointer to first Link in the list to which second list was appended.
* developed by: WDRogers  updated: 01-31-96
\******************************************************************************/
{
  Link *end;
  for ( end = *top; end != NULL && end->next != NULL; end = end->next );
  if (*top==NULL) *top = src;
  if (end!=NULL) end->next = src;
  if (src!=NULL) src->prev = end;
  return *top;
}
Link *cf_dupList(Link *src)
/******************************************************************************\
* cf_dupList: duplicate a list.
* Link *src - pointer to top link in list to be copied.
* calls: Link *cf_addLink(Link *tar, size_t size)
* returns: Link * - pointer to new list.
* developed by: WDRogers  updated: 01-31-96
\******************************************************************************/
{
  Link *new, *top;
  top = NULL;
  new = NULL;
  while (src!=NULL) {
    new = cf_addLink(new, src->size);
    if (top==NULL) top = new;
    memcpy(new->data, src->data, src->size);
    src = src->next;
  }
  return top;
}
void cf_freeList(Link *cur)
/******************************************************************************\
* cf_freeList: free current Link and all following Links in a list.
* Link *cur - pointer to the current link in the targeted list.
* calls: void cf_free(void *obj, size_t size)
* returns: void
* developed by: WDRogers  updated: 01-31-96
\******************************************************************************/
{
  Link *del;
  while (cur!=NULL) {
    del = cur;
    cur = cur->next;
    cf_free(del->data, del->size);
    cf_free(del, sizeof(Link));
    del = NULL;
  }
  return;
}
Link *cf_link2row(Link *dat)
/******************************************************************************\
* cf_link2row: allocate space for new Link whose data is another Link.
* Link *dat - pointer to a Link.
* calls: Link *cf_newLink(size_t size)
* returns: Link * - pointer to row link.
* developed by: WDRogers  updated: 01-31-96
\******************************************************************************/
{
  Link *new;
  new = cf_newLink(0);
  if (new!=NULL) new->data = dat;
  if (dat!=NULL) dat->prev = new;
  if (new!=NULL) new->kind = CF_KIND_LINK;
  return new;
}
void cf_freeRow(Link *row)
/******************************************************************************\
* cf_freeRow: free row Link and all data Links in row.
* Link *row - pointer to the targeted row.
* calls: void cf_free(void *obj, size_t size), void cf_freeList(Link *cur)
* returns: void
* developed by: WDRogers  updated: 01-31-96
\******************************************************************************/
{
  Link *dat;
  dat = (row!=NULL) ? (Link *) row->data : NULL;
  cf_freeList(dat);
  cf_free(row, sizeof(Link));
  return;
}
Link *cf_delRow(Link **top, Link *tar)
/******************************************************************************\
* cf_delRow: deletes a row from a linked list of rows.
* Link **top - pointer to pointer to top of list containing target row.
* Link *tar - pointer to target row to be deleted.
* calls: void cf_freeRow(Link *row)
* returns: pointer to next Link after target.
* developed by: WDRogers  updated: 01-31-96
\******************************************************************************/
{ /* removes a row from a row list */
  Link *nxt;
  nxt = tar->next;
  if (tar == *top) *top = tar->next;   
  if (tar->prev!=NULL) tar->prev->next = tar->next;
  if (tar->next!=NULL) tar->next->prev = tar->prev;
  cf_freeRow(tar);
  return nxt;
}
int cf_rec_l2h(pf_branch *r)
/******************************************************************************\
* cf_rec_l2h: rearranges branch data into low-to-hi alpha, hi-to-low numeric.
* pf_branch *r - pointer to a branch.
* returns: int, 1 if already in l2h, otherwise -1.
* developed by: WDRogers  updated: 05-08-96
\******************************************************************************/
{
  int   n;
  float temp_v, v;
  char  temp_n[9];
  n = strcmp(r->bus1_name, r->bus2_name);
  v = r->bus1_kv - r->bus2_kv;
  if ( (v>0) || (v==0 && n<=0) ) return 1;
  strcpy(temp_n, r->bus1_name);
  strcpy(r->bus1_name, r->bus2_name);
  strcpy(r->bus2_name, temp_n);
  temp_v = r->bus1_kv;
  r->bus1_kv = r->bus2_kv;
  r->bus2_kv = temp_v;
  return -1;
}
int cf_rec_h2l(pf_branch *r)
/******************************************************************************\
* cf_rec_h2l: rearranges branch data into hi-to-low alpha, low-to-hi numeric.
* pf_branch *r - pointer to a branch.
* returns: int, 1 if already in h2l, otherwise -1.
* developed by: WDRogers  12-31-96
\******************************************************************************/
{
  int   n;
  float temp_v, v;
  char  temp_n[9];
  n = strcmp(r->bus1_name, r->bus2_name);
  v = r->bus1_kv - r->bus2_kv;
  if ( (v<0) || (v==0 && n>=0) ) return 1;
  strcpy(temp_n, r->bus1_name);
  strcpy(r->bus1_name, r->bus2_name);
  strcpy(r->bus2_name, temp_n);
  temp_v = r->bus1_kv;
  r->bus1_kv = r->bus2_kv;
  r->bus2_kv = temp_v;
  return -1;
}
int cf_cmpBranch(pf_branch *b1, pf_branch *b2)
/******************************************************************************\
* cf_cmpBranch: compare identifying data of two pf_branch records.
* pf_branch *b1 - pointer to branch record.
* pf_branch *b2 - pointer to branch record.
* returns: int - 0 if same, 1 if b1 > b2, -1 if b1 < b2.
* developed by: WDRogers  updated: 02-02-96
\******************************************************************************/
{
  float c;
  if ( (c=strncmp(b1->type, b2->type, 1)) )       return c>0 ? 1 : -1;
  if ( (c=strcmp(b1->bus1_name, b2->bus1_name)) ) return c>0 ? 1 : -1;
  if ( fabs(c=b1->bus1_kv-b2->bus1_kv) > .001 ) return c>0 ? 1 : -1;
  if ( (c=strcmp(b1->bus2_name, b2->bus2_name)) ) return c>0 ? 1 : -1;
  if ( fabs(c=b1->bus2_kv-b2->bus2_kv) > .001 ) return c>0 ? 1 : -1;
  if ( (c=b1->ckt_id - b2->ckt_id) )              return c>0 ? 1 : -1;
  if ( (c=b1->section - b2->section) )            return c>0 ? 1 : -1;
  return 0;
}
int cf_cmpBus(pf_AC_bus *b1, pf_AC_bus *b2)
/******************************************************************************\
* cf_cmpBus: compare identifying data of two pf_AC_bus records.
* pf_AC_bus *b1 - pointer to bus record.
* pf_AC_bus *b2 - pointer to bus record.
* returns: int - 0 if same, 1 if b1 > b2, -1 if b1 < b2.
* developed by: WDRogers  updated: 02-02-96
\******************************************************************************/
{
  float c;
  if ( (c=strncmp(b1->type, b2->type, 1)) ) return c>0 ? 1 : -1;
  if ( (c=strcmp(b1->name, b2->name)) )     return c>0 ? 1 : -1;
  if ( fabs(c=b1->kv - b2->kv) > .001 )   return c>0 ? 1 : -1;
  return 0;
}
Link *cf_text2Link(char *s)
/******************************************************************************\
* cf_text2Link: make string into a Link.
* char *s - string containing text to be made into a Link.
* calls: Link *cf_newLink(size_t);
* returns: Link * - pointer to new Link or NULL if none formed.
* developed by: WDRogers  updated: 02-02-96  updated: 01-16-97
\******************************************************************************/
{
  Link *link;
  if (cf_isblank(s)) return NULL;
  link = cf_newLink(strlen(s)+1);
  if (link==NULL || link->data==NULL) return NULL;
  strcpy(link->data, s);
  link->kind = CF_KIND_STR;
  return link;
}
Link *cf_text2List(char *s)
/******************************************************************************\
* cf_text2List: make each word in string into a Link in a  linked list.
* char *s - string containing words to be broken into Links.
* calls: Link *cf_addLink(Link *, size_t);
* returns: Link * - pointer to top of list or NULL if no list formed.
* developed by: WDRogers  updated: 02-02-96  updated: 01-16-97
\******************************************************************************/
{
  Link *curLink, *topLink;
  char *qp;
  topLink = NULL;
  curLink = NULL;
  for (qp=strtok(s, CF_TOKSEP); qp!=NULL; qp=strtok(NULL, CF_TOKSEP) ) {
    curLink = cf_addLink(curLink, strlen(qp)+1);
    if (curLink==NULL || curLink->data==NULL) continue;
    if (topLink==NULL) topLink = curLink;
    strcpy(curLink->data, qp);
    curLink->kind = CF_KIND_STR;
  }
  return topLink;
}
int cf_validOwner(Link *ownerList, char *owner)
/******************************************************************************\
* cf_validOwner: checks owner against linked list of owner specifications.  Can
*   use: OWN, !OWN, or *** for include, exclude, and wild card, respectively.
* Link *ownerList - top link in linked list of owner specifications.
* char *owner - char string containing an owner code.
* returns: int - 1 if included, 0 if excluded, -1 if not on list;
* developed by: WDRogers  updated: 02-02-96
\******************************************************************************/
{
  Link *ownerLink;
  char *ownerData, listVal[4], maskVal[4];
  int   retval = -1, n;

  if (ownerList==NULL || ownerList->data==NULL) return -1;
  if (memcmp(ownerList->data, "***", 3)==0) return 1;
  for (ownerLink=ownerList; ownerLink!=NULL; ownerLink=ownerLink->next) {
    ownerData = (char *) ownerLink->data;
    n = (ownerData[0]=='!') ? 1 : 0;
    sprintf(listVal, "%-3.3s", &ownerData[n]);
    sprintf(maskVal, "%-3.3s", owner);
    if (listVal[0] == '*') maskVal[0] = '*';
    if (listVal[1] == '*') maskVal[1] = '*';
    if (listVal[2] == '*') maskVal[2] = '*';
    if (ownerData[0]=='!') {
        if (retval==-1) retval = 1;        /* keep, unless explicitly skipped */
        if (strcmp(listVal, maskVal)==0) return 0;         /* skip this owner */
    }
    else {
        if (retval==-1) retval = 0;           /* skip, unless explicitly kept */
        if (strcmp(listVal, maskVal)==0) return 1;         /* keep this owner */
    }
  }
  return (retval == -1) ? 1 : retval;
}
int cf_validBasekv(Link *basekvList, float v1, float v2)
/******************************************************************************\
* cf_validBasekv: checks kv 1 and kv 2 against linked list of kv specifications.
*   Use: 00 (exactly 00), >00 (greater than 00), <00 (less than 00), or
*   00-00 (00 to 00, inclusive).
* Link *basekvList - top link in linked list of base kV specifications.
* float v1 - bus base voltage.
* float v2 - bus base voltage.
* returns: int - 1 if either included, 0 if both excluded, -1 if neither listed.
* developed by: WDRogers  updated: 02-02-96
\******************************************************************************/
{ /* return 1 if base kv are valid, else 0 */
  Link *basekvLink;
  char *basekvData;
  float listVal1, listVal2;
  int   retval = -1;

  if (basekvList==NULL || basekvList->data==NULL) return -1;
  if (memcmp(basekvList->data, ">0", 2)==0) return 1;
  for (basekvLink=basekvList; basekvLink!=NULL; basekvLink=basekvLink->next) {
    basekvData = (char *) basekvLink->data;
    if (basekvData[0]=='>') {
        if (retval==-1) retval = 0;           /* skip, unless explicitly kept */
        sscanf(basekvData, ">%f", &listVal1);
        if (v1 > listVal1 || v2 > listVal1) retval = 1;
    }
    else if (basekvData[0]=='<') {
        if (retval==-1) retval = 0;           /* skip, unless explicitly kept */
        sscanf(basekvData, "<%f", &listVal1);
        if (v1 < listVal1 || v2 < listVal1) retval = 1;
    }
    else if (strchr(basekvData, '-')!=NULL) {
        if (retval==-1) retval = 0;           /* skip, unless explicitly kept */
        sscanf(basekvData, "%f-%f", &listVal1, &listVal2);
        if ( (v1 >= listVal1 || v2 >= listVal1) &&
             (v1 <= listVal2 || v2 <= listVal2) ) retval = 1;/*LO-HI*/
        if ( (v1 >= listVal2 || v2 >= listVal2) &&
             (v1 <= listVal1 || v2 <= listVal1) ) retval = 1;/*HI-LO*/
    }
    else {
        if (retval==-1) retval = 0;           /* skip, unless explicitly kept */
        sscanf(basekvData, "%f", &listVal1);
        if ( fabs(v1-listVal1) < .001 || fabs(v2-listVal1) < .001 )
            retval = 1;
    }
  }
  return (retval == -1) ? 1 : retval;
}
int cf_validZone(Link *zoneList, char *z1, char *z2)
/******************************************************************************\
* cf_validZone: checks zone 1 and zone 2 against linked list of zone specificat-
*   ions. Use: ZN, !ZN, or ** for include, exclude, and wild card, respectively.
* Link *zoneList - top link in linked list of owner specifications.
* char *z1 - char string containing a zone code.
* char *z2 - char string containing a zone code.
* returns: int - 1 if either included, 0 if both excluded, -1 if neither listed.
* developed by: WDRogers  updated: 02-02-96
\******************************************************************************/
{
  Link *zoneLink;
  char *zoneData, listVal[4], maskVal1[4], maskVal2[4];
  int   retval = -1, n;

  if (zoneList==NULL || zoneList->data==NULL) return -1;
  if (memcmp(zoneList->data, "**", 2)==0) return 1;
  for (zoneLink=zoneList; zoneLink!=NULL; zoneLink=zoneLink->next) {
    zoneData = (char *) zoneLink->data;
    n = (zoneData[0]=='!') ? 1 : 0;
    sprintf(listVal, "%2.2s", &zoneData[n]);
    sprintf(maskVal1, "%2.2s", z1);
    sprintf(maskVal2, "%2.2s", z2);
    if (listVal[0] == '*') { maskVal1[0] = '*'; maskVal2[0] = '*'; }
    if (listVal[1] == '*') { maskVal1[1] = '*'; maskVal2[1] = '*'; }
    if (zoneData[0]== '!') {
        if (retval==-1) retval = 1;        /* keep, unless explicitly skipped */
        if ( strcmp(listVal, maskVal1)==0 && strcmp(listVal, maskVal2)==0 )
            return 0;                     /* skip if both zones match listVal */
    }
    else {
        if (retval==-1) retval = 0;           /* skip, unless explicitly kept */
        if ( strcmp(listVal, maskVal1)==0 || strcmp(listVal, maskVal2)==0 )
            return 1;                  /* keep if either zone matches listVal */
    }
  }
  return (retval == -1) ? 1 : retval;
}
void *cf_malloc(size_t size)
/******************************************************************************\
* cf_malloc: call malloc() with error messaging; increment CF_memCurAlloc;
* size_t size - size of each object in the allocated array
* calls: void cf_logErr(char *fmt, ...)
* globals: CF_memCurAlloc, CF_memTotAlloc, CF_memMaxAlloc
* returns: void * - pointer to allocated space and zero initialized space
* developed by: WDRogers  created: 05-23-96
\******************************************************************************/
{
  void *p;
  p = NULL;
  total_memory_allocated += size;
  total_requests++;
  fprintf (stderr, "cf_malloc: memory allocated = %10d %10d %10d %10d %10d\n", 
    total_requests, size, total_memory_allocated, CF_memCurAlloc, CF_memTotAlloc);
  if (total_requests > 19079) {
    fprintf (stderr, "cf_malloc: memory about to be corrupted!\n");
  }
  p = malloc(size);
  if (p!=NULL) {
    CF_memCurAlloc += size; 
    CF_memTotAlloc += size;
    if (CF_memCurAlloc > CF_memMaxAlloc) CF_memMaxAlloc = CF_memCurAlloc;
  } else {
    cf_logErr("cf_malloc:  Can't allocate any more memory!\n");
  }
  return p;
}
void  cf_logUse(char *prog, char *vers, char *user)
/******************************************************************************\
* void  cf_logUse(char *prog, char *vers, char *user);
* cf_logUse: write a formated string to a log file to time stamp program usage
* Requires: CF_hisFile, a globally defined file pointer initialized to NULL
*           CF_HISSPEC, a globally defined string initialized to log file name
*           CF_time0, CF_time1, global variables of type time_t
*           CF_TIMESTAMP, a globally defined string specifying time/date format
*           CF_INBUFSIZE, global integer constant
* char *prog: name of the CFLOW program being run
* char *vers: version of IPF being used
* char *user: ID of user running the CFLOW program
* Return type: void
* Calls: cf_openFile
* Developed by: WDRogers 10-22-96  Updated: WDR 11-25-96
\******************************************************************************/
{
  char    t0[CF_INBUFSIZE], t1[CF_INBUFSIZE];
  double  s, m, h;

  if (CF_time0>0 && CF_time1>0) {
    s = difftime(CF_time1, CF_time0);
    s = 60 * modf(s/60, &m);
    m = 60 * modf(m/60, &h);
    strftime(t0, CF_INBUFSIZE, CF_TIMESTAMP, localtime(&CF_time0));
    strftime(t1, CF_INBUFSIZE, CF_TIMESTAMP, localtime(&CF_time1));
  }
  else {
    h = m = s = 0;
    t0[0] = t1[0] = '\0';
  }
  if (CF_hisFile==NULL) {
    CF_hisFile = cf_openFile(CF_HISSPEC, "a");
  }
  if (CF_hisFile!=NULL) {
/* fprintf(CF_hisFile, "%s %s Elapsed %2.0f:%02.0f:%02.0f (hh:mm:ss)\n",
        t0, t1, h, m, s);  */
    fprintf(CF_hisFile,
	"%-15.15s %-18.18s %-7.7s %-20.20s Elapsed %2.0f:%02.0f:%02.0f\n",
        prog, vers, user, t0, h, m, s);
  }
  fclose(CF_hisFile);
  return;
}
char *cf_strsub(char *str, char *sub, char fill)
/******************************************************************************\
* cf_strsub: (sub - substitute)
* function: replace all occurances of any character in sub with fill character
* returns: char * - pointer to str
* char *str  - pointer to string to be filled
* char *sub  - pointer to string containing characters in str to be filled
* char *fill - character to replaces sub characters with
* Developed by: WDRogers  10-23-96
\******************************************************************************/
{
  char *cp;
  for ( cp = str ; *cp != '\0' ; cp++ ) if (strchr(sub, *cp)!=NULL) *cp = fill;
  return str;
}
void cf_freeRowList(Link **rowList)
/******************************************************************************\
* void cf_freeRowList(Link **rowList);
* cf_freeRowList: free list of row Links and all data Links in each row.
* Link *rowList - pointer to pointer to the top of targeted list of rows.
* calls: void cf_delRow(Link **top, Link *tar)
* returns: void
* developed by: WDRogers  updated: 10-29-96
\******************************************************************************/
{
  Link *curRow = *rowList;
  while (curRow!=NULL) curRow = cf_delRow(rowList, curRow);
  return;
}
void cf_bus2rec(cf_Bus *bus, pf_rec *rec)
/******************************************************************************\
* void cf_bus2rec(cf_Bus *bus, pf_rec *rec);
* cf_bus2rec: translate cf_Bus structure to pf_rec structure
* cf_Bus *bus - structure containing bus identification data
* pf_rec *rec - target structure to contain bus id data
* returns: void
* developed by: WDRogers  updated: 10-31-96
\******************************************************************************/
{
  memset(rec, '\0', sizeof(pf_rec));
  strcpy(rec->i.ACbus.type,  bus->type);
  strcpy(rec->i.ACbus.owner, bus->owner);
  strcpy(rec->i.ACbus.name,  bus->name);
  rec->i.ACbus.kv         =  bus->kv;
  strcpy(rec->i.ACbus.zone,  bus->zone);
}
void cf_rec2bus(pf_rec *rec, cf_Bus *bus)
/******************************************************************************\
* void cf_rec2bus(pf_rec *rec, cf_Bus *bus);
* cf_rec2bus: translate pf_rec structure to cf_Bus structure
* pf_rec *rec - structure containing bus identification data
* cf_Bus *bus - target structure to contain bus id data
* returns: void
* developed by: WDRogers  updated: 10-31-96
\******************************************************************************/
{
  memset(bus, '\0', sizeof(cf_Bus));
  strcpy(bus->type,  rec->i.ACbus.type);
  strcpy(bus->owner, rec->i.ACbus.owner);
  strcpy(bus->name,  rec->i.ACbus.name);
  bus->kv         =  rec->i.ACbus.kv;
  strcpy(bus->zone,  rec->i.ACbus.zone);
}
void cf_branch2rec(cf_Branch *branch, pf_rec *rec)
/******************************************************************************\
* void cf_branch2rec(cf_Branch *branch, pf_rec *rec);
* cf_branch2rec: translate cf_Branch structure to pf_rec structure
* cf_Branch *branch - structure containing branch input data
* pf_rec *rec - target structure to contain branch input data
* returns: void
* developed by: WDRogers  updated: 10-29-96
\******************************************************************************/
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
void cf_rec2branch(pf_rec *rec, cf_Branch *branch)
/******************************************************************************\
* void cf_rec2branch(pf_rec *rec, cf_Branch *branch)
* cf_rec2branch: translate pf_rec structure to cf_Branch structure
* pf_rec *rec - structure containing branch input data
* cf_Branch *branch - target structure to contain branch input data
* returns: void
* developed by: WDRogers  updated: 10-29-96
\******************************************************************************/
{
  memset(branch, '\0', sizeof(cf_Branch));
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
Link *cf_id2Link(char *s, char order)
/******************************************************************************\
* Link *cf_id2Link(char *s, char order);
* cf_id2Link: create Link with cf_Branch or cf_Bus data copied from s
* char *s - string containing bus or branch input data
* char order - character indicating sort order:
*  'H'- h2l; 'L'- l2h; 'I'- input;
* calls: cf_isblank, cf_str2upper, cf_newLink, pf_rec_a2b, cf_rec2bus
* calls: cf_rec2branch, cf_rec_l2h, cf_rec_h2l
* returns: Link * - pointer to Link referencing cf_Branch or cf_Bus data
* developed by: WDRogers  updated: 12-31-96
\******************************************************************************/
{
  Link *link;
  pf_rec r;
  size_t size;
  if (cf_isblank(s)) return NULL;
  cf_str2upper(s);
  size = (s[0]=='B') ? sizeof(cf_Bus) : sizeof(cf_Branch); 
  link = cf_newLink(size);
  if (link==NULL || link->data==NULL) return NULL;
  pf_rec_a2b(s, &r, "I");
  if (s[0]=='B') {
    cf_rec2bus(&r, (cf_Bus *) link->data);
    link->kind = CF_KIND_BUS;
  }
  else {
    switch(toupper(order)) {
        case 'H': cf_rec_h2l((pf_branch *) &r); break;
        case 'L': cf_rec_l2h((pf_branch *) &r); break;
        case 'I': break; /* retain input order */
        default : fprintf(stderr,"cf_id2Link: invalid order code %c\n",order);
    }
    cf_rec2branch(&r, (cf_Branch *) link->data);
    link->kind = CF_KIND_BRN;
  }
  return link;
}
int cf_cntchr(char *str, char chr)
/******************************************************************************\
* cf_cntchr: (cnt - count, chr - character)
* function: count the number of occurances of character chr in string str
* returns: int - number of occurances of chr in str
* char *str  - pointer to string to be examined
* char  chr  - character to be counted
* Developed by: WDRogers  11-12-96
\******************************************************************************/
{
  char *cp;
  int   n;
  n = 0;
  for ( cp = str ; *cp != '\0' ; cp++ ) if (*cp==chr) n++;
  return n;
}
char *cf_strdup(char *str)
/******************************************************************************\
* cf_strdup: (str - string, dup - duplicate)
* function: duplicate the string str
* calls: cf_malloc
* returns: char * - pointer to new occurance of string str
* char *str  - pointer to string to be duplicated
* Developed by: WDRogers  11-7-96
\******************************************************************************/
{
    char *new;
    new = (char *) cf_malloc(strlen(str)+1);      /* +1 to make room for '\0' */
    if (new != NULL) strcpy(new, str);
    return new;
}
int cf_branch_l2h(cf_Branch *b)
/******************************************************************************\
* int cf_branch_l2h(cf_Branch *b);
* cf_branch_l2h: (branch - cf_Branch, l2h - low to high)
* function:  rearranges branch data into low-to-hi alpha, hi-to-low numeric.
* cf_Branch *b - pointer to branch data
* returns: int, 1 if already in l2h, otherwise -1.
* developed by: WDRogers  updated: 11-18-96
\******************************************************************************/
{
  int n;
  float temp_v, v;
  char  temp_n[9], temp_z[3];
  n = strcmp(b->bus1_name, b->bus2_name);
  v = b->bus1_kv - b->bus2_kv;
  if ( (v>0) || (v==0 && n<=0) ) return 1;
  strcpy(temp_n, b->bus1_name);
  strcpy(temp_z, b->bus1_zone);
  temp_v = b->bus1_kv;
  strcpy(b->bus1_name, b->bus2_name);
  strcpy(b->bus2_name, temp_n);
  strcpy(b->bus1_zone, b->bus2_zone);
  strcpy(b->bus2_zone, temp_z);
  b->bus1_kv = b->bus2_kv;
  b->bus2_kv = temp_v;
  return -1;
}
Link *cf_rec2Link(char *s)
/******************************************************************************\
* Link *cf_rec2Link(char *s);
* cf_rec2Link: create Link with pf_rec data taken from s
* char *s - string containing bus or branch input data
* calls: cf_isblank, cf_str2upper, cf_newLink, pf_rec_a2b
* returns: Link * - pointer to Link referencing pf_rec data
* developed by: WDRogers  updated: 12-31-96
\******************************************************************************/
{
  Link *link;

  if (cf_isblank(s)) return NULL;
  cf_str2upper(s);
  link = cf_newLink(sizeof(pf_rec));
  if (link==NULL || link->data==NULL) return NULL;
  pf_rec_a2b(s, (pf_rec *) link->data, "I");
  link->kind = CF_KIND_REC;
  return link;
}
cf_Out *cf_initOut(char *fs, char *ts, int ty, int fm)
/******************************************************************************\
* cf_Out *cf_initOut(char *fs, char *ts, int ty, int fm)
* cf_initOut: allocate and initialize memory for cf_Out struct
* char *fs - file specification
* char *ts - time stamp
* int ty - report type
* int fm - report format
* returns: cf_Out * - pointer to new cf_Out struct
* developed by: WDRogers  updated: 03-14-97
\******************************************************************************/
{
  cf_Out *rpt;
  rpt = (cf_Out *) cf_malloc(sizeof(cf_Out));
  strcpy(rpt->spec, fs);           /* file spec    */
  rpt->file        = NULL;         /* file pointer */
  rpt->line        = 0;            /* line number  */
  rpt->page        = 1;            /* page number  */
  rpt->time        = ts;           /* time stamp   */
  rpt->type        = ty;           /* report type  */
  rpt->form        = fm;           /* format       */
  rpt->wide        = 1;            /* width        */
  rpt->diff        = 0;            /* difference   */
  return rpt;
}
float cf_getRating(pf_rec *n, int tx, char *code)
/******************************************************************************\
* float cf_getRating(pf_rec *n, int tx, char *code)
* cf_getRating: return branch rating based on type and usage code
* pf_rec *n - pointer to branch record
* int tx - use 0 for outage conditions, 1 for basecase conditions
* char *code - pointer to destination for rating code character: N, T, B, or E
* returns: float - branch rating
* developed by: WDRogers  updated: 02-04-97
\******************************************************************************/
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
  *code = ' ';
  return 0;
}
float cf_getLoad(pf_rec *n)
/******************************************************************************\
* float cf_getLoad(pf_rec *n)
* cf_getLoad: return branch total branch loading based on type
* pf_rec *n - pointer to branch record
* returns: float - branch loading in MVA (for Tx) or Amps (for Lines)
* developed by: WDRogers  updated: 02-04-97
\******************************************************************************/
{
  float load;
  if (n->i.branch.type[0]=='T') {
    load = n->s.branch.tot_xfmr_load_mva;
  }
  else {
    load = n->s.branch.tot_line_load_amps;
  }
  return load;
}

int cf_printGroup(FILE *fp, Link *list, cf_Style style, char *tag)
/******************************************************************************\
* int cf_printGroup(FILE *fp, Link *list, cf_Style style, char *tag)
* cf_printGroup: convert and print formated data in a linked list of groups
*  groups could be a linked list of change records (for a common-mode outage) or
*  a branches (for a cut-plane)
* FILE *fp - pointer to output file
* Link *list - pointer to top of list to be printed
* cf_Style style - struct containing control signals for printing link data
* char *tag - character string heading for list of Group
* returns: int - number of carriage returns printed to file fp
* developed by: WDRogers  updated: 03-05-97
\******************************************************************************/
{
  int     n_cr = 0;
  Link   *link;

  if (list==NULL || (list!=NULL && list->kind!=CF_KIND_TAG))
    n_cr = cf_printTag(fp, list, style, tag);

  for (; list!=NULL; list=list->next) {
    if (list->kind==CF_KIND_LINK) {
        n_cr += cf_printList(fp,(Link *) list->data, style, "");
    }      /* modified 3-5-97: cf_printList no longer inherits tag from above */
    else {
        n_cr += cf_printLink(fp, list);
    }
  }
  return n_cr;
}
int cf_printList(FILE *fp, Link *list, cf_Style style, char *tag)
/******************************************************************************\
* int cf_printList(FILE *fp, Link *list, cf_Style style, char *tag)
* cf_printList: convert and print formated data in a linked list
* FILE *fp - pointer to output file
* Link *list - pointer to top of list to be printed
* cf_Style style - struct containing control signals for printing link data
* char *tag - character string heading for list
* returns: int - number of carriage returns printed to file fp
* developed by: WDRogers  updated: 02-04-97
\******************************************************************************/
{
  int     n_cr = 0, n_more;
  Link   *link;

  if (list==NULL || (list!=NULL && list->kind!=CF_KIND_TAG))
    n_cr = cf_printTag(fp, list, style, tag);

  for (n_more = 0; list!=NULL; list=list->next) {
    if (n_more==0) n_more = style.limit;
    if (n_more==style.limit) fprintf(fp, style.indent);
    link = (Link *) ( list->kind==CF_KIND_LINK ? list->data : list );
    n_cr += cf_printLink(fp, link);

    if (list->next!=NULL && --n_more>0) {
        fprintf(fp, ", ");
    }
    else {
        fprintf(fp, "\n");
        n_cr++;
    }
  }
  return n_cr;
}
int cf_printLink(FILE *fp, Link *link)
/******************************************************************************\
* int cf_printLink(FILE *fp, Link *link)
* cf_printLink: convert and print data contents of link according to Link kind
* FILE *fp - pointer to output file
* Link *link - pointer to Link that contains data for printing
* returns: int - number of carriage returns printed to file fp
* developed by: WDRogers  updated: 02-04-97
\******************************************************************************/
{
  char    data[CF_INBUFSIZE];
  int     cr; /* carriage return */
  pf_rec  r;

  cr = 0;
  if (link==NULL) return 0;
  if (link->kind==CF_KIND_STR) {
    cr = cf_cntchr(link->data, '\n');
    fprintf(fp, link->data);
  }
  else if (link->kind==CF_KIND_FLT) {
    sprintf(data, "%f", (float *) link->data);
    fprintf(fp, data);
  }
  else if (link->kind==CF_KIND_INT) {
    sprintf(data, "%d", (int *) link->data);
    fprintf(fp, data);
  }
  else if (link->kind==CF_KIND_BUS) {
    cf_bus2rec((cf_Bus *) link->data, &r);
    pf_rec_b2a(data, &r, "I");
    fprintf(fp, "%20.20s", data);
  }
  else if (link->kind==CF_KIND_BRN) {
    cf_branch2rec((cf_Branch *) link->data, &r);
    pf_rec_b2a(data, &r, "I");
    fprintf(fp, "%33.33s", data);
  }
  else if (link->kind==CF_KIND_REC) {
    pf_rec_b2a(data, (pf_rec *) link->data, "I");
    if (data[0]=='B') fprintf(fp, "%20.20s", data);
    else              fprintf(fp, "%33.33s", data);
  }
  else if (link->kind==CF_KIND_TAG) {
    cr = cf_cntchr(link->data, '\n');
    fprintf(fp, link->data);
  }
  else if (link->kind==CF_KIND_NAME) {
    cr = cf_cntchr(((cf_Name *) link->data)->name, '\n');
    fprintf(fp, ((cf_Name *)link->data)->name);
  }
  else if (link->kind==CF_KIND_GEN) {
    sprintf(data, " %7.1f %7.1f %7.1f", ((cf_Gen *)link->data)->start,
        ((cf_Gen *)link->data)->stop, ((cf_Gen *)link->data)->step);
    fprintf(fp, data);
  }

  else if (link->kind==CF_KIND_SF2) {
    sprintf(data, " %s %7.1f %7.1f", ((cf_StrFlt2 *)link->data)->file,
        ((cf_StrFlt2 *)link->data)->flt1, ((cf_StrFlt2 *)link->data)->flt2);
    fprintf(fp, data);
  }
  else if (link->kind==CF_KIND_STRSTR) {
    sprintf(data, " %s %s", ((cf_StrStr *)link->data)->str1,
        ((cf_StrStr *)link->data)->str2);
    fprintf(fp, data);
  }


  return cr;
}
int cf_printTag(FILE *fp, Link *list, cf_Style style, char *tag)
/******************************************************************************\
* int cf_printTag(FILE *fp, Link *list, cf_Style style, char *tag)
* cf_printTag: print tag string based on value of style and list
* FILE *fp - pointer to output file
* Link *list - pointer to a Link in a list
* cf_Style style - struct containing control signals for printing tag
* char *tag - string to print
* returns: int - number of carriage returns printed to file fp
* developed by: WDRogers  updated: 02-04-97
\******************************************************************************/
{
  int     cr = 0; /* carriage return */

  if (list!=NULL || style.mode==CF_TAG_ALWAYS) {
    cr = cf_cntchr(tag, '\n');
    fprintf(fp, tag);
    if (list==NULL && cr==0 && style.mode==CF_TAG_ALWAYS)
        fprintf(fp, "\n");
  }
  return cr;
}
char *cf_link2tagName(Link *link, char *name)
/******************************************************************************\
* char *cf_link2tagName(Link *link, char *name)
* cf_link2tagName: scan data from Link of kind CF_KIND_TAG into name
* Link *link - pointer to link of kind CF_KIND_TAG
* char *name - destination string scanned contents of Link data
*   expected data format is "> keyword id string" where "id string" is copied
*   into name[].
* returns: Link * - pointer to link data
* developed by: WDRogers  updated: 02-04-97
\******************************************************************************/
{
  sscanf(link->data, ">%*s %[^\n]", name);
  return link->data;
}
Link *cf_tag2link(char *s)
/******************************************************************************\
* Link *cf_tag2link(char *s)
* cf_tag2link: make a new Link of kind CF_KIND_TAG.
* char *s - string to be copied into Link data array.
* calls: Link *cf_newLink(size_t);
* returns: Link * - pointer to new Link or NULL if none formed.
* developed by: WDRogers  updated: 02-04-97
\******************************************************************************/
{
  Link   *link;

  link = cf_newLink(strlen(s)+1);
  if (link==NULL || link->data==NULL) return NULL;
  strcpy(link->data, s);
  link->kind = CF_KIND_TAG;
  return link;
}
void  cf_fillGroup(char *mstrSpec, Link **grpList, char *key)
/******************************************************************************\
* void  cf_fillGroup(char *mstrSpec, Link **grpList, char *key)
* cf_fillGroup: build a list of linked groups from input file
*  groups could be a linked list of change records (for a common-mode outage) or
*  a branches (for a cut-plane)
*  look for input strings with this format: "> groupkey group id name"
* char *mstrSpec - file specification of master data file for group data
* Link **grpList - pointer to pointer to top of list of groups to fill or create
* char *key - string containing group key word identifier
* returns: void
* developed by: WDRogers  updated: 02-04-97
\******************************************************************************/
{
  char s[CF_STRSIZE], curName[CF_STRSIZE];
  Link *curGroup, *curTag, *link, *row, *list;
  FILE *mstrFile;
  int   def = 1, mode = 0;

  if ( strlen(mstrSpec)==0 ) {
    return;
  }
  if ((mstrFile = cf_openFile(mstrSpec, "r"))==NULL) return;

  if (*grpList==NULL) { /* read in all groups */
    rewind(mstrFile);
    while ( fgets(s, CF_STRSIZE, mstrFile) != NULL ) {
        if ( s[0] == '.' ) continue;              /* skip comments, skip data */
        if (cf_isblank(s)) continue;
        cf_str2upper(s);
        cf_strsub(s, "\n", '\0');
        if (strstr(s, key)) { /* key word */
            printf("cf_fillGroup found: %s\n", s);
            link = cf_tag2link(s);
            row = cf_link2row(link);
            cf_appList(grpList, row);
            list = (Link *) row->data;
            continue;
        }
        else {
            if (list==NULL) {
                sprintf(curName, "> %s cut #%d", key, def++);
                link = cf_tag2link(curName);
                row = cf_link2row(link);
                cf_appList(grpList, row);
                list = (Link *) row->data;                
            }
            if (strcmp(key, "CUT-PLANE")==0) link = cf_rec2Link(s);
            else if (strcmp(key, "MODE")==0) link = cf_text2Link(s);
            cf_appList(&list, link);
        }
    }
  }
  else { /* *grpList != NULL */
    printf("  Filling group from master file: %s\n", mstrSpec);
    for (curGroup = *grpList ; curGroup!=NULL; curGroup = curGroup->next ) {
        curTag = (Link *) curGroup->data;
        if (curTag->next!=NULL) continue; /* already filled */
        cf_link2tagName(curTag, curName);
        rewind(mstrFile);
        while (fgets(s, CF_STRSIZE, mstrFile)!=NULL) {
            if ( s[0] == '.' ) continue;                     /* skip comments */
            if (cf_isblank(s)) continue;
            cf_str2upper(s);
            cf_strsub(s, "\n", '\0');
            if (strstr(s, key)) {
                if (strstr(s, curName)!=NULL) {
                    mode = 1;                                /* filling group */
                    printf("cf_fillGroup found: %s\n", s);
                }
                else mode = 0;                               /* wrong group */
                continue;
            }
            else if (mode==1) {
                if (strcmp(key, "CUT-PLANE")==0) link = cf_rec2Link(s);
                else if (strcmp(key, "MODE")==0) link = cf_text2Link(s);
                cf_appList(&curTag, link);
            }
        }
        if (curTag->next==NULL) printf("cf_fillGroup can't find %s\n", curName);
    }
  }
}
void cf_insLink(Link **top, Link **cur, Link *new, long sort, int mode,
    int (*cmp)(Link *, Link *, int))
/******************************************************************************\
* void cf_insLink(Link **top, Link **cur, Link *new, int sort, int mode,
*     int (*cmp)(Link *, Link *, int));
* cf_insLink: insert Link into list in sorted order
* Link **top - pointer to pointer to top Link in list
* Link **cur - pointer to pointer to previously added Link (point of insertion)
* Link *new - pointer to new Link to add to list
* long sort - long int or pointer to sort criteria: (trace *), trace->sort, etc.
* int mode - integer determining what to do when cur and new links are the same
*  for the compare==0 case, one could invision skip, insert, replace, overwrite,
*  or secondary sort as valid responses.
* int (*cmp)(Link *, Link *, int) - pointer to function comparing two Links
* calls: cf_insAfter, cf_insBefore, cf_freeRow
* returns: void
* developed by: WDRogers  updated: 03-27-97
\******************************************************************************/
{
  int comp, last;

  last = 0;
  while (1) {
    if ((*cur)==NULL) {
        (*top) = new;
        break;
    }
    comp = (*cmp)(new, (*cur), sort);

    if ( comp < 0 ) {
        if ((*cur)->prev==NULL || last>0) {
            cf_insBefore(cur, &new, top);  /* insert new before cur or at top */
            break;
        }
        else {
            (*cur) = (*cur)->prev;                           /* move backward */
            last = comp;
        }
    }
    else if (comp==0 ) {
        switch (mode) {
            case CF_INS_FREE   : cf_freeRow(new); new = (*cur); break;
            case CF_INS_AFTER  : cf_insAfter(cur, &new);        break;
            case CF_INS_BEFORE : cf_insBefore(cur, &new, top);  break;
            case CF_INS_SKIP   : new = (*cur);                  break;
            default            : new = (*cur);                  break;
        }
        break;
    }
    else if (comp > 0) {
        if ((*cur)->next==NULL || last<0) {
            cf_insAfter(cur, &new);         /* insert new after cur or at end */
            break;
        }
        else {
            (*cur) = (*cur)->next;                           /* move foreward */
            last = comp;
        }
    }
  }
  (*cur) = new;
}
void cf_insBefore(Link **cur, Link **new, Link **top)
/******************************************************************************\
* void cf_insBefore(Link **cur, Link **new, Link **top)
* cf_insBefore: insert new Link before current Link or at top of list
* Link **cur - pointer to pointer to previously added Link (point of insertion)
* Link **new - pointer to pointer to new Link to add to list
* Link **top - pointer to pointer to top Link in list
* returns: void
* developed by: WDRogers  updated: 03-14-97
\******************************************************************************/
{
  if ((*cur)->prev==NULL) (*top) = (*new);
  (*new)->next = (*cur);
  (*new)->prev = (*cur)->prev;
  if ((*cur)->prev!=NULL) (*cur)->prev->next = (*new);
  (*cur)->prev = (*new);
}
void cf_insAfter(Link **cur, Link **new)
/******************************************************************************\
* void cf_insAfter(Link **cur, Link **new)
* cf_insAfter: insert new Link after current Link or at end of list
* Link **cur - pointer to pointer to previously added Link (point of insertion)
* Link **new - pointer to pointer to new Link to add to list
* returns: void
* developed by: WDRogers  updated: 03-14-97
\******************************************************************************/
{
  (*new)->prev = (*cur);
  (*new)->next = (*cur)->next;
  if ((*cur)->next!=NULL) (*cur)->next->prev = (*new);
  (*cur)->next = (*new);
}
int cx_printList(FILE *fp, Link *list, char *tag, char *indent, char type,
    char *fmt, int n)
/******************************************************************************\
* int cx_printList(FILE *fp, Link *list, char *tag, char *indent, char type,
*     char *fmt, int n);
* cx_printList: convert and print formated data in a linked list
* FILE *f - pointer to file
* Link *list - pointer to top of list to be printed
* char *tag - character string heading for list
* char *indent - indentation string
* char  type - type of data to be printed
*   P,p - primitive(string, float,...), 'P' - list->link->data, 'p' - link->data
*   B,b - cf_Bus                        'B' - list->link->data, 'b' - link->data
*   L,l - cf_Branch                     'L' - list->link->data, 'l' - link->data
*   R,r - pf_rec                        'R' - list->link->data, 'r' - link->data
* char *fmt - format string for printing data
* int   n - fabs(n) is the number of links to print per row;
*   n<0 - if no items in list (list==NULL) then don't print tag, else print tag
*   n>0 - always print tag
* calls: cf_cntchr, cf_bus2rec, cf_branch2rec, pf_rec_b2a
* returns: int - number of lines printed (carriage returns).
* developed by: WDRogers  updated: 10-30-96
\******************************************************************************/
{
  int    i, cr;
  pf_rec r;
  Link  *link;
  char   data[CF_INBUFSIZE];

  if (list==NULL && n<0) return 0;

  cr = cf_cntchr(tag, '\n');
  if (list==NULL) {
    fprintf(fp, tag);
    if (tag[strlen(tag)-1]!='\n') {
        fprintf(fp, "\n");
        cr++;
    }
    return cr;
  }
  else { /* list != NULL */
    fprintf(fp, tag);
  }
  n = (int) fabs(n);
  for (i=n; list!=NULL; list=list->next) {
    if (i==0) i = n;
    if (i==n) fprintf(fp, indent);
    link = (Link *) ( isupper(type) ? list->data : list );
    if (toupper(type)=='P') {     /* 'P' - list->link->data, 'p' - link->data */
        strcpy(data, (char *) link->data);
    }
    if (toupper(type)=='B') {     /* 'B' - list->link->data, 'b' - link->data */
        cf_bus2rec((cf_Bus *) link->data, &r);
        pf_rec_b2a(data, &r, "I");
    }
    if (toupper(type)=='L') {     /* 'L' - list->link->data, 'l' - link->data */
        cf_branch2rec((cf_Branch *) link->data, &r);
        pf_rec_b2a(data, &r, "I");
    }
    if (toupper(type)=='R') {     /* 'R' - list->link->data, 'r' - link->data */
        pf_rec_b2a(data, (pf_rec *) link->data, "I");
    }
    fprintf(fp, fmt, data);
    if (list->next!=NULL && n!=1 && --i>0) {
        fprintf(fp, ", ");
    }
    else {
        fprintf(fp, "\n");
        cr++;
    }
  }
  return cr;
}
int cx_printRowList(FILE *fp, Link *row, char *list_tag, char *row_tag,
    char type, char *fmt, int n)
/******************************************************************************\
* int cx_printRowList(FILE *fp, Link *list, char *list_tag, char *row_tag,
*     char type, char *fmt, int n)
* cx_printRowList: convert and print formated data in a linked list of rows
* FILE *fp - pointer to output file
* Link *row - pointer to top row in list of rows to be printed
* char *list_tag - character string heading for list of rows
* char *row_tag - character string separating each row list
* char  type - type of data to be printed, as follows:
*   P,p - primitive(string, float,...), 'P' - list->link->data, 'p' - link->data
*   B,b - cf_Bus                        'B' - list->link->data, 'b' - link->data
*   L,l - cf_Branch                     'L' - list->link->data, 'l' - link->data
*   R,r - pf_rec                        'R' - list->link->data, 'r' - link->data
* char *fmt - format string for printing data
* int   n - fabs(n) is the number of links to print per row;
*   n<0 - if no items in list (list==NULL) then don't print tag, else print tag
*   n>0 - always print tag
* calls: cf_cntchr, cx_printList
* returns: int - number of lines printed (carriage returns).
* developed by: WDRogers  updated: 12-04-96
\******************************************************************************/
{
  Link *list;
  int   r;

  r = cf_cntchr(list_tag, '\n');
  fprintf(fp, list_tag);
  while (row!=NULL) {
    list = (Link *) row->data;
    r += cx_printList(fp, list, row_tag, "", type, fmt, n);
    row = row->next;
  }
  return r;
}
