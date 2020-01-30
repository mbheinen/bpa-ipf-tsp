/******************************************************************************\
UTILITY:    TSPbatch
STRUCTURE:  common CFLOW architecture.
TYPE:       Transient Stability Program (TSP) pre-processor.
SUMMARY:
RELATED:    FINDOUT, LINEFLOW, MIMIC, CFUSE
SEE ALSO:   
UPDATED:    October 28, 1996 
LANGUAGE:   Standard C.  CFLOW Library.  cf_util.h 
DEVELOPER:  William D. Rogers, BPA, TOP, 230-3806, wdrogers@bpa.gov
REQUESTER:  Gary Keenan 
USERS:      Gary Keenan
TSP:        version 6000 or above recommended; in future may also work with PTI
PURPOSE:
\******************************************************************************/
/******************************* #include *************************************/
#include "cflowlib.h"
#include "cf_util.h"
/***************************** end #include ***********************************/
/******************************* #define **************************************/
#define  OK               0
#define  MAX_IN           135 /* was 150 */
#define  DOT_LINE         "..................................................."
#define  LOG_SPEC         "tspbatch.log"
#define  TRC_SPEC         "tspbatch.trc"
#define  QUERY_CONT (int) (1<< 0)  /* prompt to continue run */
#define  QUERY_BASE (int) (1<< 1)  /* prompt for basename */
#define  QUERY_MSWI (int) (1<< 2)  /* prompt for master swing file */
#define  QUERY_TCOM (int) (1<< 3)  /* prompt for template com file */
#define  QUERY_SERI (int) (1<< 4)  /* prompt for series name */
#define  QUERY_BATC (int) (1<< 5)  /* prompt for batch command */
#define  QUERY_SBUS (int) (1<< 6)  /* prompt for bus list */
#define  QUERY_SDIN (int) (1<< 7)  /* prompt for stability data input file */
#define  QUERY_KEEP (int) (1<< 8)  /* prompt for keeping nth point */
#define  READ_INC        (int)    1 /* found /INCLUDE card in CFLOW data file */
#define  READ_BUS        (int)    2 /* found /BUS     card in CFLOW data file */
#define  READ_SOL        (int)    3 /* found /SOLUTIONcard in CFLOW data file */
#define  READ_REP        (int)    4 /* found /REPORT  card in CFLOW data file */
#define  TRACE_OFF       (int)    0
#define  TRACE_ON        (int)    1
/***************************** end #define ************************************/
/******************************* typedef **************************************/
typedef struct {
  char   type[3];       /* same memory footprint as pf_branch up to 'section' */
  char   owner[4];
  char   bus1_name[9];
  float  bus1_kv;
  int    meter;
  char   bus2_name[9];
  float  bus2_kv;
  char   ckt_id;
  int    section; /* always 0 */
  char   bus1_zone[3];
  char   bus2_zone[3];
} ff_branch;

typedef struct {
  char   type[3];           /* same memory footprint a pf_AC_bus up to 'zone' */
  char   owner[4];
  char   name[9];
  float  kv;
  char   zone[3];
} ff_bus;


typedef struct traceRecord {
  Link  *baseList;
  Link  *sbusList;
  Link  *ownrList;
  Link  *zoneList;
  Link  *bskvList;
  Link  *iswiList;    /* individual swing (*.swi) files */
  Link  *icomList;    /* individual command (*.com) files */
  int    trace;       /* indicates whether or not to produce a report trace   */
  int    query;
  int    keep;        /* collect every nth data point, keep==4, every 4th */
  char   batch[MAX_IN];
  char   series[MAX_IN];
  char   basename[FILENAME_MAX];    /* name of basecase  */
  char   sdiname[FILENAME_MAX];     /* name of sdi file  */
  FILE  *swiFile;                   /* master swing file */
  FILE  *comFile;                   /* template com file */
  char   bseSpec[FILENAME_MAX];     /* name of basecase  */
  char   sdiSpec[FILENAME_MAX];     /* stability data in file */
  char   swiSpec[FILENAME_MAX];     /* master swing file */
  char   comSpec[FILENAME_MAX];     /* template com file */
  char   trcSpec[FILENAME_MAX];
  char   TSPversion[5];
  char   timeStamp[MAX_IN];
  char   userID[MAX_IN];
} Trace;
/******************************* end typedef **********************************/

/* top TSPbatch  functions - called by main() *********************************/
void  initializeTrace(Trace *trace);
void  processCommandLine(Trace *trace, int argc, char *argv[]);
void  instructions(Trace *trace);
void  promptUser(Trace *trace);
void  openReport(Trace *trace);
void  printTrace(Trace *trace);
void  queryContinue(Trace *trace);
void  readBusData(Trace *trace);
void  buildSwingFiles(Trace *trace);
void  buildComFiles(Trace *trace);
void  finalRemarks(Trace *trace);
/* end TSPbatch functions - called by main() **********************************/

/* top TSPbatch functions - report functions **********************************/
/* end TSPbatch functions - report functions **********************************/

/* top TSPbatch functions - support functions *********************************/
int   identifyInput(char *s);
void  makeIcomFile(Trace *trace, char *schemeId, char *icomSpec);
void  batchIcomFiles(Trace *trace);
/* end TSPbatch functions - support functions *********************************/

/* top LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/
void   ff_stream2List(FILE *readMe, Trace *trace, Link **expList);
void   ff_report(char *s, Trace *trace);
void   ff_traceName(Trace *trace);
Link  *ff_id2Link(char *s);
void   ff_expList(Trace *trace, Link *dataList, Link **expList);
void   ff_branch_l2h(ff_branch *brch);
void   mf_fprintList(FILE *fp,Link *link,char *tag, char *tab,char type,int n);
void   ff_rec2branch(pf_rec *rec, ff_branch *branch);
void   ff_branch2rec(ff_branch *branch, pf_rec *rec);
void   ff_bus2rec(ff_bus *bus, pf_rec *rec);
void   ff_rec2bus(pf_rec *rec, ff_bus *bus);
/* end LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/

/* top CF_UTIL.H candidate functions - common to: MIMIC, LINEFLOW, FINDOUT ****/
/*     all documented, CF_UTIL.H candidates shall be designated, vf_name().   */
/* end CF_UTIL.H candidate functions - common to: MIMIC, LINEFLOW, FINDOUT ****/

/******************************** global variable candidates ******************/
/**************************** end global variable candidates ******************/

int main(int argc, char *argv[])
{
  Trace trace;

  time(&CF_time0);
  initializeTrace(&trace);
  processCommandLine(&trace, argc, argv);
  instructions(&trace);
  promptUser(&trace);
  openReport(&trace);
  printTrace(&trace);
  queryContinue(&trace);
  readBusData(&trace);
  buildSwingFiles(&trace);
  buildComFiles(&trace);
  batchIcomFiles(&trace);
  finalRemarks(&trace);
  time(&CF_time1);
  cf_logUse("TSPbatch %s %s", trace.TSPversion, trace.userID);
  return 0;
}
void initializeTrace(Trace *trace)
{ /* special function to do set up/init for TSPbatch */
  memset(trace, '\0', sizeof(Trace));
  trace->iswiList         =   NULL;
  trace->icomList         =   NULL;
  trace->sbusList         =   NULL;
  trace->query            = ( QUERY_CONT | QUERY_BASE | QUERY_MSWI |
                              QUERY_TCOM | QUERY_SERI | QUERY_BATC |
                              QUERY_SDIN | QUERY_KEEP );
  strcpy(trace->series,  "97AUG");
  strcpy(trace->batch,   "SUB41");
  strcpy(trace->swiSpec, "MASTER.SWI");
  strcpy(trace->sdiSpec, "97AUG.SDI");
  strcpy(trace->comSpec, "TEMPLATE.COM");
  strcpy(trace->bseSpec, "97AUG007.BSE");
  strcpy(trace->trcSpec, TRC_SPEC);
  trace->swiFile          =   NULL;
  trace->comFile          =   NULL;
  trace->trace            =   TRACE_ON;
  trace->keep            =    1;
  trace->TSPversion[0]    =  '\0';
  cuserid(trace->userID);
  cf_time(trace->timeStamp, MAX_IN, CF_TIMESTAMP);
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
  ff_expList(trace, topLink, &trace->sbusList);
  return;
}
void  instructions(Trace *trace)
{
  if (trace->query==0) return;/* skip if there is not going to be any queries */
  printf("\n                      Welcome to TSPbatch - updated 10-28-96");
  printf("\n");
  printf("\n - Defaults in brackets [].  Press Ctrl-Y & type STOP to abort.");
  printf("\n - Use .trc, .dat, or .lis or append / to designate data files.");
  printf("\n - Transit Stability Program version 6xxx recommended.");
  printf("\n");
}
void promptUser(Trace *trace)
{
  char  query[MAX_IN];
  Link *list;

  printf("%s", DOT_LINE);

  if (trace->query == QUERY_CONT) trace->query &= ~(QUERY_CONT);

  if (trace->query & (QUERY_BASE|QUERY_MSWI|QUERY_TCOM|QUERY_SERI|QUERY_BATC)) {
    printf("\n\n/REPORT");
  }
  if (trace->query & QUERY_BASE) {
    cf_sprompt("\n > Enter the basecase name,BASECASE = [%s]: ", trace->bseSpec, trace->bseSpec);
  }
  cf_parse(trace->bseSpec, trace->basename, 'N');
  if (trace->query & QUERY_SDIN) {
    cf_sprompt("\n > Enter stability data file,   SDI = [%s]: ", trace->sdiSpec, trace->sdiSpec);
  }
  cf_parse(trace->sdiSpec, trace->sdiname, 'N');
  if (trace->query & QUERY_MSWI) {
    cf_sprompt("\n > Enter master swing file,  MASTER = [%s]: ", trace->swiSpec, trace->swiSpec);
  }
  if (trace->query & QUERY_TCOM) {
    cf_sprompt("\n > Enter template com file,TEMPLATE = [%s]: ", trace->comSpec, trace->comSpec);
  }
  if (trace->query & QUERY_SERI) {
    cf_sprompt("\n > Enter series name,        SERIES = [%s]: ", trace->series, trace->series);
  }
  if (trace->query & QUERY_BATC) {
    cf_sprompt("\n > Enter batch command (0=build only), BATCH = [%s]: ", trace->batch, trace->batch);
  }
  if (trace->query & QUERY_KEEP) {
    cf_iprompt("\n > Keep every nth data point,  KEEP = [%d]: ", trace->keep, &trace->keep);
  }
  printf("%s\n", DOT_LINE);
  return;
}
void openReport(Trace *trace)
{
  trace->swiFile = cf_openFile(trace->swiSpec, "r");
  trace->comFile = cf_openFile(trace->comSpec, "r");
  cf_exit(trace->swiFile==NULL, "Quitting!\n");
  cf_exit(trace->comFile==NULL, "Quitting!\n");
  return;
}
void printTrace(Trace *trace)
{
  FILE *fp;

  if (trace->trace==TRACE_OFF) return;
  fp = cf_openFile(trace->trcSpec, "w");
  if (fp == NULL) fp = stderr;
  fprintf(fp, ". %s %s %s\n", trace->trcSpec, trace->timeStamp, trace->userID);
  fprintf(fp, "/REPORT\n");
  fprintf(fp, "  BASECASE = %s\n", trace->bseSpec);
  fprintf(fp, "  SDI      = %s\n", trace->sdiSpec);
  fprintf(fp, "  MASTER   = %s\n", trace->swiSpec);
  fprintf(fp, "  TEMPLATE = %s\n", trace->comSpec);
  fprintf(fp, "  SERIES   = %s\n", trace->series);
  fprintf(fp, "  BATCH    = %s\n", trace->batch);
  fprintf(fp, "  KEEP     = %d\n", trace->keep);
  if (trace->trace == TRACE_ON      ) fprintf(fp, "  TRACE = ON\n");
  if (trace->trace == TRACE_OFF     ) fprintf(fp, "  TRACE = OFF\n");
}
void queryContinue(Trace *trace)
{
  int  yes;
  if ( trace->query & QUERY_CONT) {
    printf("\n\n Trace  report written to %s", trace->trcSpec);
    yes= cf_yprompt("\n > Do you want to continue this run?        [%c]: ",'Y');
    cf_exit(!yes, "Quitting!");
    printf("\n");
  }
}
void readBusData(Trace *trace)
{
  char  str[MAX_IN], *sp;
  Link *list;

  rewind(trace->swiFile);
  while ( fgets(str, MAX_IN, trace->swiFile)!=NULL ) {
    if ((sp=strchr(str, '\n'))!=NULL) *sp = '\0';
    list = cf_text2Link(str);
    cf_appList(&trace->sbusList, list);
    if (strncmp(str, "99", 2)==0) break;
  }
  cf_exit(cf_cntLink(trace->sbusList)==0, "No bus data found! Quitting!\n");
}
void buildSwingFiles(Trace *trace)
{
  char  s[MAX_IN];
  char  iswiSpec[FILENAME_MAX], schemeId[MAX_IN];
  FILE *iswiFile;
  Link *list;
  int   state, id;

#define  SRCHN_SWIT_SCHEM  (int) 1
#define  FOUND_SWIT_SCHEM  (int) 2
#define  COPYI_SWIT_SCHEM  (int) 3
#define  FOUND_NEXT_SCHEM  (int) 4

  printf("\n");
  state = SRCHN_SWIT_SCHEM;
  while ( fgets(s, MAX_IN, trace->swiFile)!=NULL ) {
    id = identifyInput(s);
    if (id==1) {
        if (state==COPYI_SWIT_SCHEM) state = FOUND_NEXT_SCHEM;
        if (state==FOUND_SWIT_SCHEM) state = FOUND_NEXT_SCHEM;
        if (state==SRCHN_SWIT_SCHEM) state = FOUND_SWIT_SCHEM;
    }
    if (state==COPYI_SWIT_SCHEM) {
        fprintf(iswiFile, s);
        continue;
    }
    if (state==FOUND_NEXT_SCHEM) {
        mf_fprintList(iswiFile, trace->sbusList, "", "", 's', 1);
        state = FOUND_SWIT_SCHEM;
        fclose(iswiFile);
    }
    if (state==FOUND_SWIT_SCHEM) {
        sscanf(s, "*** %s", schemeId);
        sprintf(iswiSpec, "%s%s.swi", trace->series, schemeId);
        printf("Building individual *.swi file... %s\n", iswiSpec);

        list = cf_text2Link(schemeId);        /* previously put iswiSpec here */
        cf_appList(&trace->iswiList, list);

        iswiFile = cf_openFile(iswiSpec, "w");

        fprintf(iswiFile, "CASE %-10.10s %1d  1  1 %-10.10s\n", trace->basename,
            trace->keep, trace->sdiname);
        fprintf(iswiFile, "   CASE %s\n", trace->basename);
        fprintf(iswiFile, "   %s", s);
        state = COPYI_SWIT_SCHEM;
        continue;
    }
  }
  if (state==COPYI_SWIT_SCHEM) {
    mf_fprintList(iswiFile, trace->sbusList, "", "", 's', 1);
    fclose(iswiFile);
  }
}
int identifyInput(char *s)
{
  if (strstr(s, "***"           ) != NULL) return  1; /* 1 = swit. schene hdr */
  return 0;                                           /* 0 = possible data */
}
void  buildComFiles(Trace *trace)
{
  Link *curIswi, *curIcom;
  char  icomSpec[FILENAME_MAX];

  printf("\n");
  for (curIswi=trace->iswiList; curIswi!=NULL; curIswi=curIswi->next) {
    makeIcomFile(trace, (char *) curIswi->data, icomSpec);
    curIcom = cf_text2Link(icomSpec);
    cf_appList(&trace->icomList, curIcom);
  }
}
void makeIcomFile(Trace *trace, char *schemeId, char *icomSpec)
{
  char  str[MAX_IN];
  FILE *icomFile;
  int err;

  sprintf(icomSpec, "%s%s.com", trace->series, schemeId);
  icomFile = cf_openFile(icomSpec, "w");
  if (icomFile==NULL) return;
  printf("Building individual *.com file... %s\n", icomSpec);
  rewind(trace->comFile);

  fprintf(icomFile, "$ BASECASE = \"%s\"\n", trace->bseSpec);
  fprintf(icomFile, "$ SDIFILE = \"%s\"\n", trace->sdiSpec);
  fprintf(icomFile, "$ SERIES = \"%s\"\n", trace->series);
  fprintf(icomFile, "$ SCHEME = \"%s\"\n", schemeId);
  while ( fgets(str, MAX_IN, trace->comFile)!=NULL ) {
    fprintf(icomFile, str);
  }
  err = fclose(icomFile);
}
void  batchIcomFiles(Trace *trace)
{
  Link *curIcom;
  char  str[MAX_IN], icomSpec[FILENAME_MAX];

  printf("\n");
  for (curIcom=trace->icomList; curIcom!=NULL; curIcom=curIcom->next) {
    if (trace->batch[0]!='0') {
        sprintf(str, "%s %s", trace->batch, (char *) curIcom->data);
        printf("%s\n", str);
        system(str);
    }
  }
}
void  finalRemarks(Trace *trace)
{
  printf("\n");
  printf("\nMemory allocation (bytes): Cur:%ld Max:%ld Alloc:%ld Freed:%ld\n",
    CF_memCurAlloc, CF_memMaxAlloc, CF_memTotAlloc, CF_memTotFreed);
  if (trace->trace == TRACE_ON)
    printf("\nTrace  report written to %s", trace->trcSpec);
  if (CF_logFile != NULL) printf("\nError  report written to %s", CF_logSpec);
  printf("\n");
  return;
}
void ff_branch_l2h(ff_branch *brch)      /* basis for a pf_rec_l2h() function */
{ /* force overloaded and outaged branches to be listed low to hi alpha-numer */
  int n, v;
  float temp_v;
  char  temp_n[9], temp_z[3];
  if ( brch->type[0]!='L' && brch->type[0]!='T' ) return;
  n = strcmp(brch->bus1_name, brch->bus2_name);
  v = brch->bus1_kv - brch->bus2_kv;
  if ( (v>0) || (v==0 && n<=0) ) return;
  strcpy(temp_n, brch->bus1_name);
  strcpy(temp_z, brch->bus1_zone);
  temp_v = brch->bus1_kv;
  strcpy(brch->bus1_name, brch->bus2_name);
  strcpy(brch->bus2_name, temp_n);
  strcpy(brch->bus1_zone, brch->bus2_zone);
  strcpy(brch->bus2_zone, temp_z);
  brch->bus1_kv = brch->bus2_kv;
  brch->bus2_kv = temp_v;
  return;
}
char *strrep(char *s) /* make a replicate of s */ /* formerly strdup */
{
    char *p;

    p = (char *) malloc(strlen(s)+1); /* +1 for '\0' */
    if (p != NULL)
        strcpy(p, s);
    return p;
}
void ff_stream2List(FILE *readMe, Trace *trace, Link **expList)
{
  char str[MAX_IN], STR[MAX_IN];
  Link *list;
  int mode = READ_INC; /* default /INCLUDE */
  while (fgets(str, MAX_IN, readMe)!=NULL) { 
    if ( cf_iscomment(str) ) continue;
    strcpy(STR, str);
    cf_str2upper(STR);
    if ( strstr(STR, "/BUS"         )!=NULL ) { mode = READ_BUS; continue; }
    if ( strstr(STR, "/REPORT"      )!=NULL ) { mode = READ_REP; continue; }
    if ( STR[0]=='/' ) { mode = 0; continue; }     /* unrecognized slash card */
    if ( mode == 0 ) continue;         /* note: mode is defaulted to READ_INC */
    if ( mode == READ_INC ) {
        list = cf_text2List(str);
        ff_expList(trace, list, expList);
    }
    if ( mode == READ_BUS ) {
        list = cf_text2Link(str);
        cf_appList(&trace->sbusList, list);
        if (list!=NULL) trace->query &= ~(QUERY_SBUS);
    }
    if ( mode == READ_REP ) ff_report(str, trace);
  }
  return;
}
void ff_report(char *s, Trace *trace)
{
  if (strstr(s, "BASECASE")!=NULL) {
    sscanf(s, "%*s = %s", trace->bseSpec);
    trace->query &= ~(QUERY_BASE);
    return;
  }
  if (strstr(s, "SDI"     )!=NULL) {
    sscanf(s, "%*s = %s", trace->sdiSpec);
    trace->query &= ~(QUERY_SDIN);
    return;
  }
  if (strstr(s, "MASTER"  )!=NULL) {
    sscanf(s, "%*s = %s", trace->swiSpec);
    trace->query &= ~(QUERY_MSWI);
    return;
  }
  if (strstr(s, "MASTER"  )!=NULL) {
    sscanf(s, "%*s = %s", trace->swiSpec);
    trace->query &= ~(QUERY_MSWI);
    return;
  }
  if (strstr(s, "TEMPLATE")!=NULL) {
    sscanf(s, "%*s = %s", trace->comSpec);
    trace->query &= ~(QUERY_TCOM);
    return;
  }
  if (strstr(s, "SERIES"  )!=NULL) {
    sscanf(s, "%*s = %s", trace->series);
    trace->query &= ~(QUERY_SERI);
    return;
  }
  if (strstr(s, "BATCH"   )!=NULL) {
    sscanf(s, "%*s = %s", trace->batch);
    trace->query &= ~(QUERY_BATC);
    return;
  }
  if (strstr(s, "KEEP"    )!=NULL) {
    sscanf(s, "%*s = %d", &trace->keep);
    trace->query &= ~(QUERY_KEEP);
    return;
  }
  if (strstr(s, "TRACE"   )!=NULL) {
    if (strstr(s, "ON" )!=NULL) trace->trace = TRACE_ON;
    if (strstr(s, "OFF")!=NULL) trace->trace = TRACE_OFF;
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
void ff_branch2rec(ff_branch *branch, pf_rec *rec)
{
  memset(rec, '\0', sizeof(pf_rec));
  strcpy(rec->i.branch.type, branch->type);
  strcpy(rec->i.branch.owner, branch->owner);
  strcpy(rec->i.branch.bus1_name, branch->bus1_name);
  strcpy(rec->i.branch.bus2_name, branch->bus2_name);
  rec->i.branch.bus1_kv = branch->bus1_kv;
  rec->i.branch.bus2_kv = branch->bus2_kv;
  rec->i.branch.ckt_id  = branch->ckt_id;
  rec->i.branch.section = branch->section;
}
void ff_rec2branch(pf_rec *rec, ff_branch *branch)
{
  memset(branch, '\0', sizeof(ff_branch));
  memcpy(branch, &rec->i.branch, sizeof(ff_branch));
  branch->bus1_zone[0] = '\0'; branch->bus2_zone[0] = '\0';
/*  branch->rating = 0; branch->code = '\0'; */
}
void mf_fprintList(FILE *fp, Link *link, char *tag, char *tab, char type, int n)
{ /* n is the number of links printed per row */
  int i;
  pf_rec r;
  char net_data[MAX_IN];

  if (tag[strlen(tag)-1]=='\n') {       /* always print tag if ending in '/n' */
    fprintf(fp, "%s", tag);
  }
  if (link==NULL) return;
  if (tag[strlen(tag)-1]!='\n') {/* print tag lacking '/n' only if list!=NULL */
    if (strlen(tag)>0) {         /* don't print null string */
        fprintf(fp, "%s\n", tag);
    }
  }
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
void ff_traceName(Trace *trace)
{
  char s[FILENAME_MAX];
  strcpy(trace->trcSpec, trace->series);
  strcat(trace->trcSpec, ".trc");         /* TYPE */
}
/************ documented, common CF_UTIL.H candidates prototypes **************/
/* all documented, CF_UTIL.H candidates shall be designated, cf_name().       */
/******** end documented, common CF_UTIL.H candidates prototypes **************/
