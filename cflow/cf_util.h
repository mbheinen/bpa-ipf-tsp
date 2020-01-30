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

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <stdarg.h>

#define Cf_min(A, B) ((A) < (B) ? (A) : (B))
#define Cf_max(A, B) ((A) > (B) ? (A) : (B))
#define Cf_ratio(N, D) ((D) > 0 ? (N) / (D) : 0)
#define Cf_imod(N, D) (int) (fmod((double) N, (double) D))

#define CF_STRSIZE      133
#define CF_INPFOSIZE    135      /* for reading Fortran-generated *.PFO files */
#define CF_INBUFSIZE    250
#define CF_OUTBUFSIZE   250
#define CF_RECSIZE      300     /* maximum length of input or solution record */
#define CF_IPCSIZE     4096                             /* CFLOW_IPC_BUS_SIZE */
#define CF_COMMENT      '.'
#define CF_TRUNCATE     "\n\\"
#define CF_DATATYPES    ".DAT,.TRC,.LIS,.XYC"
#define CF_TOKSEP       ", \t"
#define CF_HISSPEC      "CFLOW.LOG"       /* CFLOW usage history log */
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
  int    me_flag;  /* Common mode missing element flag added for findout */ 
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
  char *sp, *op, *kp, *dp, *np, *tp,*vp, *ep, item[FILENAME_MAX];
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
    CF_memCurAlloc += nobj*size; CF_memTotAlloc += nobj*size;
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
  CF_memCurAlloc -= size;  CF_memTotFreed += size;
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
  if ( c=strncmp(b1->type, b2->type, 1) )       return c>0 ? 1 : -1;
  if ( c=strcmp(b1->bus1_name, b2->bus1_name) ) return c>0 ? 1 : -1;
  if ( fabs(c=b1->bus1_kv-b2->bus1_kv) > .001 ) return c>0 ? 1 : -1;
  if ( c=strcmp(b1->bus2_name, b2->bus2_name) ) return c>0 ? 1 : -1;
  if ( fabs(c=b1->bus2_kv-b2->bus2_kv) > .001 ) return c>0 ? 1 : -1;
  if ( c=b1->ckt_id - b2->ckt_id )              return c>0 ? 1 : -1;
  if ( c=b1->section - b2->section )            return c>0 ? 1 : -1;
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
  if ( c=strncmp(b1->type, b2->type, 1) ) return c>0 ? 1 : -1;
  if ( c=strcmp(b1->name, b2->name) )     return c>0 ? 1 : -1;
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
  p = malloc(size);
  if (p!=NULL) {
    CF_memCurAlloc += size; CF_memTotAlloc += size;
    if (CF_memCurAlloc > CF_memMaxAlloc) CF_memMaxAlloc = CF_memCurAlloc;
  }
  else cf_logErr("cf_malloc:  Can't allocate any more memory!\n");
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
