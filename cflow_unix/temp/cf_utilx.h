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

