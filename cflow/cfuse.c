/******************************************************************************\
UTILITY:    CFuse - common CFLOW architecture
TYPE:       Powerflow (IPFSRV) user analysis report generation. 
SUMMARY:    Automate generation of user analysis reports.
RELATED:    LINEFLOW, FINDOUT, MIMIC
UPDATED:    January 09, 1997
LANGUAGE:   Standard C.  CFLOW Libraries.  CF_UTIL.H.
DEVELOPER:  William D. Rogers, BPA, TEOS, 230-3806, wdrogers@bpa.gov
REQUESTER:  Kyle Kohne, BPA, TEOB, 230-4423
USERS:      Kyle Kohne, Kendall Rydell
IPF:        Version 317 or above recommended.
PURPOSE:    Generate user analysis reports given a list of base cases and a list
            of user analysis files.
\******************************************************************************/
/******************************* #include *************************************/
#include "cflowlib.h"
#include "cf_util.h"
/***************************** end #include ***********************************/
/******************************* #define **************************************/
#define  OK               0
#define  MAX_IN           150
#define  FF_IPF_VERSION   317            /* GPF.EXE_V316 or above recommended */
#define  PAGE_LENGTH      61
#define  PAGE_WIDTH       132
#define  DOT_LINE         "..................................................."
#define  LOG_SPEC         "CFuse.LOG"
#define  TRC_SPEC         "CFuse.TRC"
#define  QUERY_BASE (int) (1<< 0)  /* prompt for basecases */
#define  QUERY_USER (int) (1<< 1)  /* prompt for user analysis files */
#define  QUERY_OUTF (int) (1<< 2)  /* prompt for user analysis report names */
#define  QUERY_NAME (int) (1<< 3)  /* prompt for trace file name */
#define  READ_INC        (int)    1 /* found /INCLUDE card in CFLOW data file */
#define  READ_BSE        (int)    2 /* found /BASECAS card in CFLOW data file */
#define  READ_USR        (int)    3 /* found /USER_ANALYSIS   CFLOW data file */
#define  READ_OUT        (int)    5 /* found /OUTPUT_FILE  in CFLOW data file */
#define  READ_REP        (int)    8 /* found /REPORT  card in CFLOW data file */
#define  READ_TRC        (int)   14 /* found /TRACE   card in CFLOW data file */
#define  DO_TRACE        (int)    1
#define  PAGE_BREAK "************************************************************************************************************************************"
#define  PAGE_MARK  "------------------------------------------------------------------------------------------------------------------------------------"
/***************************** end #define ************************************/
/******************************* enum *****************************************/
/***************************** end enum ***************************************/
/******************************* typedef **************************************/

typedef struct processStep {
  char oldBase[FILENAME_MAX];
  char usrFile[FILENAME_MAX];
  char outFile[FILENAME_MAX];
} Step;

typedef struct traceRecord {
  Link  *baseList;
  Link  *userList;
  Link  *outfList;
  Link  *traceList;
  Link  *stepList;
  int    trace;
  int    query;
  char   trcSpec[FILENAME_MAX];
  char   timeStamp[MAX_IN];
  char   userID[MAX_IN];
  char   IPFversion[MAX_IN];
  char   IPFexecute[MAX_IN];
} Trace;
/******************************* end typedef **********************************/

/* top CFuse functions - called by main() *************************************/
void  initializeTrace(Trace *trace);
void  processCommandLine(Trace *trace, int argc, char *argv[]);
void  instructions(Trace *trace);
void  checkIPFversion(Trace *trace);
void  promptUser(Trace *trace);
void  checkFileLists(Trace *trace);
void  assembleSteps(Trace *trace);
void  buildTrace(Trace *trace);
void  printTrace(Trace *trace);
void  queryContinue(Trace *trace);
void  dispatchEngine(Trace *trace);
void  finalRemarks(Trace *trace);
/* end CFuse functions - called by main() *************************************/

/* top CFuse functions - support functions ************************************/
void   makeNewName(Step *curStep);
void   checkFileList(Link *fileList);
int    findFile(Link *curFile, char *path);
/* end CFuse functions - support functions ************************************/

/* top LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/
Link  *ff_stream2List(FILE *readMe, Trace *trace, Link **expList);
void   ff_report(char *s, Trace *trace);
void   ff_traces(char *s, Trace *trace);
void   ff_expList(Trace *trace, Link *dataList, Link **expList);
void   mf_fprintList(FILE *fp,Link *link,char *tag,char *tab, char type, int n);
/* end LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/

/* top CF_UTIL.H candidate functions - common to: MIMIC, LINEFLOW, FINDOUT ****/
/*   all documented, CF_UTIL.H candidates shall be designated, vf_name().     */
/* end CF_UTIL.H candidate functions - common to: MIMIC, LINEFLOW, FINDOUT ****/

main(int argc, char *argv[])
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
  printTrace(&trace);
  queryContinue(&trace);
  pf_cflow_init(argc, argv);      /* initialize cflow connection to powerflow */
  printf("Processing... \n");
  dispatchEngine(&trace);
  pf_cflow_exit();
  time(&CF_time1);
  cf_logUse("CFuse", trace.IPFversion, trace.userID);
  finalRemarks(&trace);
}
/* end of main() */
void  initializeTrace(Trace *trace)
{
  memset(trace, '\0', sizeof(trace));
  trace->baseList         =   NULL;
  trace->userList         =   NULL;
  trace->outfList         =   NULL;
  trace->traceList        =   NULL;
  trace->stepList         =   NULL;
  trace->trace            =   DO_TRACE;
  trace->query            = ( QUERY_BASE | QUERY_USER | QUERY_OUTF | 
                              QUERY_NAME                           );

  strcpy(trace->trcSpec, TRC_SPEC);
  strncpy(trace->IPFexecute, getenv("IPFSRV_CF"), MAX_IN);
  cf_parse(trace->IPFexecute, trace->IPFversion, 'F');
  cuserid(trace->userID);
  cf_time(trace->timeStamp, MAX_IN, "  %d-%b-%Y %H:%M:%S");
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
  ff_expList(trace, topLink, &trace->baseList);
  return;
}
void instructions(Trace *trace)
{
  printf("\n                      Welcome to CFuse - updated 01-09-97");
  printf("\n");
  printf("\n - Defaults in brackets [].  Press Ctrl-Y & type STOP to abort.");
  printf("\n - Use .TRC, .DAT, or .LIS or append / to designate data files.");
  printf("\n - Warning: %s.PFD, and %s.PFO deleted.", trace->userID, trace->userID);
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
  else vp = 0;
  if (vn != FF_IPF_VERSION) {
    printf("\n - - - - - - - - - - - -  Warning  - - - - - - - - - - - -\n");
    printf(" CFuse was tested for use with IPF version %d.\n", FF_IPF_VERSION);
    printf(" You are currently using IPF version %d.\n", vn);
    printf(  " - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n");
  }
}
void promptUser(Trace *trace)
{
  char       s[MAX_IN];
  int        choice, yes;

  char query[MAX_IN];
  Link *list;

  printf("%s", DOT_LINE);
  if (trace->query & QUERY_BASE) {
    printf("\n\n/BASECASE, /TRACE, or /INCLUDE");
    cf_nprompt("\n > Enter list of Basecase or data files: ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->baseList);
  }
  if (trace->baseList==NULL && trace->stepList==NULL) {
    cf_exit(1, "No data or basecase files!  Quitting!\n");
  }
  if (trace->query & QUERY_USER) {
    printf("\n\n/USER_ANALYSIS");
    cf_nprompt("\n > Enter list of User Analysis files   : ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->userList);
  }
  if (trace->userList==NULL && trace->stepList==NULL) {
    cf_exit(1, "No User_Analysis files!  Quitting!\n");
  }
  if (trace->query & QUERY_OUTF) {
    printf("\n\n/OUTPUT_FILES");
    cf_nprompt("\n > Enter list of Analysis Report Names : ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->outfList);
  }

  if (trace->query & QUERY_NAME) {
    printf("\n\n/REPORT");
    cf_sprompt("\n > Enter trace name, NAME = [%s]: ", trace->trcSpec, trace->trcSpec);
  }

  printf("\n%s\n", DOT_LINE);
  return;
}
void printTrace(Trace *trace)
{
  FILE *fp;

  if (trace->trace!=DO_TRACE) return;
  fp = cf_openFile(trace->trcSpec, "w");
  if (fp == NULL) return;
  fprintf(fp, ". %s %s %s %s\n", trace->trcSpec, trace->timeStamp, trace->userID,
    trace->IPFversion);

  mf_fprintList(fp, trace->baseList, "/BASECASE\n",     "  ", 's',  1);
  mf_fprintList(fp, trace->userList, "/USER_ANALYSIS\n","  ", 's',  1);
  mf_fprintList(fp, trace->outfList, "/OUTPUT_FILES\n", "  ", 's',  1);

  fprintf(fp, "/REPORT\n");
  fprintf(fp, "  NAME = %s\n", trace->trcSpec);
  if ( trace->trace == DO_TRACE ) fprintf(fp, "  TRACE = ON\n");
  if ( trace->trace != DO_TRACE ) fprintf(fp, "  TRACE = OFF\n");

  mf_fprintList(fp, trace->traceList, "./TRACE\n",      ". ", 's',  1);
}
void  checkFileLists(Trace *trace)
{
  checkFileList(trace->baseList);
  checkFileList(trace->userList);
}
void  assembleSteps(Trace *trace)
{
  Link *stepLink;
  Link *curUser, *curBase, *curRepo;
  Step *step;

/* test CFuse function : Loop through all cases in base case list */
  if (trace->stepList != NULL) return;
  stepLink= trace->stepList;
  curBase = trace->baseList;
  curUser = trace->userList;
  curRepo = trace->outfList;
  while (curBase != NULL) {
    stepLink = cf_addLink(stepLink, sizeof(Step));
    step = (Step *) stepLink->data;
    if (trace->stepList==NULL) trace->stepList = stepLink;
    if (curUser!=NULL) {
        sprintf(step->usrFile, "%s", (char *) curUser->data);
    }
    sprintf(step->oldBase, "%s", (char *) curBase->data);

    if (curRepo==NULL || strlen((char *) curRepo->data)==0) {
        makeNewName(step);
    }
    else {
        sprintf(step->outFile, "%s", (char *) curRepo->data);
    }
    if (curRepo!=NULL) curRepo = curRepo->next;
    
    if (curUser!=NULL) curUser = curUser->next;
    if (curUser==NULL) curBase = curBase->next;
    if (curUser==NULL) curUser = trace->userList;
  }
}
void queryContinue(Trace *trace)
{
  int  yes;

  if (trace->trace == DO_TRACE)
    printf("\nTrace  report written to %s", trace->trcSpec);
  if (CF_logFile != NULL)
    printf("\nError Summary report written to %s", CF_logSpec);
  printf("\n%s\n", DOT_LINE);
  mf_fprintList(stdout, trace->traceList, "/TRACE\n",      "  ", 's',  1);
  printf("%s\n", DOT_LINE);
  printf(" Please examine the above /TRACE of CFuse operations.");
  yes = cf_yprompt("\n > Do you want to continue this run?[%c]: ", 'Y');
  cf_exit(!yes, "Quitting!");
}
void  dispatchEngine(Trace *trace)
{
  Step *step;
  char  s[MAX_IN], last_base[FILENAME_MAX];
  Link *stepLink;
  last_base[0]='\0'; /* no last base to start with */
/**************** BEGINNING OF POWERFLOW DISPATCH ENGINE **********************/
  for (stepLink = trace->stepList; stepLink != NULL; stepLink = stepLink->next){
    step = (Step *) stepLink->data;
    if (last_base[0]=='\0' || strcmp(last_base, step->oldBase)!=0) {
        if (pf_load_oldbase(step->oldBase)!=0) {
            cf_logErr("Skipping base case: Cannot load %s\n", step->oldBase);
            continue;
        }
    }
    strcpy(last_base, step->oldBase);
                                                  /* apply user-analysis file */
    if (pf_user_report(step->usrFile, step->outFile, 'O')!=0) {
        cf_logErr("Skipping changes:  Cannot use %s\n", step->usrFile);
        cf_logErr("%s\n", err_buf);
        continue;
    }
    printf("Writing User Analysis report to %s\n", step->outFile);
    sprintf(s, "purge %s.PFO", trace->userID); system(s);
    sprintf(s, "purge %s.PFD", trace->userID); system(s);
  }
/**************** END OF POWERFLOW DISPATCH ENGINE ****************************/
}
void  finalRemarks(Trace *trace)
{
  printf("\n");
  printf("\nMemory allocation (bytes): Cur:%d Max:%d Alloc:%d Freed:%d\n",
    CF_memCurAlloc, CF_memMaxAlloc, CF_memTotAlloc, CF_memTotFreed);
  if (trace->trace == DO_TRACE)
    printf("\nTrace  report written to %s", trace->trcSpec);
  if (CF_logFile != NULL) printf("\nError  report written to %s", CF_logSpec);
  printf("\n");
}
void makeNewName(Step *curStep)
{
  char baseName[256], usanName[256];
  cf_parse(curStep->oldBase, baseName, 'N');
  cf_parse(curStep->usrFile, usanName, 'N');
  sprintf(curStep->outFile, "%.20s_%.20s.RPT", baseName, usanName);
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
    cf_logErr(" Warning - Can't find file: %s\n\a", name);
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
  char  newLine[1024];

  if (trace->traceList!=NULL) return;
  curLink = NULL;
  for (stepLink = trace->stepList; stepLink!=NULL; stepLink = stepLink->next ) {
    step = (Step *) stepLink->data;
    sprintf(newLine, "%s ", step->oldBase);
    cf_aprint(newLine, "+ %s ", step->usrFile);
    cf_aprint(newLine, "> %s ", step->outFile);

    curLink = cf_addLink(curLink, (size_t) strlen(newLine) + 1);
    if (curLink!=NULL) strcpy((char *) curLink->data, newLine);
    if (trace->traceList==NULL) trace->traceList = curLink;
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
Link *ff_stream2List(FILE *readMe, Trace *trace, Link **expList)
{
  char str[MAX_IN], *cp;
  Link *list;
  int mode = READ_INC;/* default /INCLUDE */
  while (fgets(str, MAX_IN, readMe)!=NULL) { 
    if ( cf_iscomment(str) ) continue;
    cf_str2upper(str);
    if ( strstr(str, "/INCLUDE"      )!=NULL ) { mode = READ_INC; }
    if ( strstr(str, "/BASECASE"     )!=NULL ) { mode = READ_BSE; }
    if ( strstr(str, "/REPORT"       )!=NULL ) { mode = READ_REP; }
    if ( strstr(str, "/TRACE"        )!=NULL ) { mode = READ_TRC; }
    if ( strstr(str, "/USER_ANALYSIS")!=NULL ) { mode = READ_USR; trace->query &= ~QUERY_USER; }
    if ( strstr(str, "/OUTPUT_FILE"  )!=NULL ) { mode = READ_OUT; trace->query &= ~QUERY_OUTF; }

    if ( mode == 0 ) continue;         /* note: mode is defaulted to READ_INC */
    if ( str[0]=='/' ) {
        if ( (cp=strchr(str, '|'))!=NULL ) strcpy(str, (++cp));
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
    if ( mode == READ_USR ) {
        list = cf_text2List(str);
        cf_appList(&trace->userList, list);
        trace->query &= ~(QUERY_USER);
    }
    if ( mode == READ_OUT ) {
        list = cf_text2List(str);
        cf_appList(&trace->outfList, list);
        trace->query &= ~(QUERY_OUTF);
    }
    if ( mode == READ_REP ) ff_report(str, trace);
    if ( mode == READ_TRC ) ff_traces(str, trace);
  }
  return NULL;
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
    if (op == '+' && *step->usrFile=='\0') {
        strcpy(step->usrFile, cp);
    }
    if (op == '>') strcpy(step->outFile, cp);
  }
  cf_appList(&trace->stepList, stepLink);
  trace->query &= ~(QUERY_BASE|QUERY_USER|QUERY_OUTF);

  if (*step->oldBase!='\0') {
    trcLink = cf_newLink(strlen(step->oldBase)+1);
    strcpy((char *) trcLink->data, step->oldBase);
    cf_appList(&trace->baseList, trcLink);
  }
  if (*step->usrFile!='\0') {
    trcLink = cf_newLink(strlen(step->usrFile)+1);
    strcpy((char *) trcLink->data, step->usrFile);
    cf_appList(&trace->userList, trcLink);
  }
  if (*step->outFile!='\0') {
    trcLink = cf_newLink(strlen(step->outFile)+1);
    strcpy((char *) trcLink->data, step->outFile);
    cf_appList(&trace->outfList, trcLink);
  }
  return;
}
void ff_report(char *s, Trace *trace)
{
  char *sp, *tp;
  for (sp = strtok(s, " ,="); sp!=NULL; sp = strtok(NULL, " ,=")) {
    tp=strtok(NULL, ", ="); if (tp==NULL) return;
    if (strstr(sp, "NAME")!=NULL) {
        strcpy(trace->trcSpec, tp);
        trace->query &= ~(QUERY_NAME);
        continue;
    }
    if (strstr(sp, "TRACE")!=NULL) {
        if (strstr(tp, "ON" )!=NULL) trace->trace =  DO_TRACE;
        if (strstr(tp, "OFF")!=NULL) trace->trace = !DO_TRACE;
        continue;
    }
  }
}
void mf_fprintList(FILE *fp, Link *link, char *tag, char *tab, char type, int n)
{ /* n is the number of links printed per row */
  int i;
  pf_rec r;
  char net_data[MAX_IN];
  fprintf(fp, "%s", tag);
  if (link==NULL) return;
  for (i=n; link!=NULL; link=link->next) {
    if (i==0) i = n;
    if (i==n) fprintf(fp, "%s", tab);
    if (type=='s') fprintf(fp, "%s", link->data);
    if (type=='f') fprintf(fp, "%6.2f", link->data);
    if (link->next!=NULL && n!=1 && --i>0) fprintf(fp, ", ");
    else fprintf(fp, "\n");
  }
}
/************ documented, common CF_UTIL.H candidates prototypes **************/
/* all documented, CF_UTIL.H candidates shall be designated, cf_name().       */
/******** end documented, common CF_UTIL.H candidates prototypes **************/
