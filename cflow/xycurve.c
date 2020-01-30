/******************************************************************************\
UTILITY:    XYcurve
STRUCTURE:  common CFLOW architecture.
TYPE:       Stand-alone PostScript file generator.
SUMMARY:    Generate PostScript X-Y plots from x-y data and setup files.
RELATED:    QVcurve, CAPcurve
SEE ALSO:   
UPDATED:    January 9, 1997
LANGUAGE:   Standard C.  CFLOW Libraries.  CF_UTIL.H.
DEVELOPER:  William D. Rogers, BPA, TOP, 230-3806, wdrogers@bpa.gov
REQUESTER:  Scott Kinney, Kyle Kohne,...
USERS:      Scott Kinney, Kyle Kohne, Melvin Rodrigues, Gordon Comegys
            Berhanu Tesema
IPF:        N/A
PURPOSE:
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
#define  MAX_CURVES       6
#define  DOT_LINE         "..................................................."
#define  LOG_SPEC         "xycurve.log"
#define  SET_SPEC         "xycurve.xyc"
#define  QUERY_LMTS  (int) (1<< 0) /* prompt for limits */
#define  QUERY_XYDT  (int) (1<< 1) /* prompt for /OUTPUT file names */
#define  QUERY_SETU  (int) (1<< 2) /* prompt for /SETUP file names */
#define  QUERY_SUPT  (int) (1<< 3) /* prompt for SUPER_TITLE */
#define  QUERY_SUBT  (int) (1<< 4) /* prompt for SUB_TITLE */
#define  QUERY_XTTL  (int) (1<< 5) /* prompt for X_TITLE */
#define  QUERY_YTTL  (int) (1<< 6) /* prompt for Y_TITLE */
#define  QUERY_MARK  (int) (1<< 7) /* prompt for MARK */
#define  QUERY_CONT  (int) (1<< 8) /* prompt for whether to continue run */
#define  READ_INC    (int)      1  /* found /INCLUDE card in CFLOW data file */
#define  READ_LIM    (int)      2  /* found /LIMITS  card in CFLOW data file */
#define  READ_REP    (int)      3  /* found /REPORT  card in CFLOW data file */
#define  READ_SUP    (int)      4  /* found /SUPER_TITLE  in CFLOW data file */
#define  READ_SUB    (int)      5  /* found /SUB_TITLE    in CFLOW data file */
#define  READ_XTL    (int)      6  /* found /X_TITLE card in CFLOW data file */
#define  READ_YTL    (int)      7  /* found /Y_TITLE card in CFLOW data file */
#define  READ_SET    (int)      8  /* found /SETUP   card in CFLOW data file */
#define  READ_XYD    (int)      9  /* found /XY_DATA card in CFLOW data file */
#define  TRACE_NO    (int)      0
#define  TRACE_YES   (int)      1
#define  PRINT_NO    (int)      0
#define  PRINT_YES   (int)      1
#define  SAVE_NO     (int)      0
#define  SAVE_YES    (int)      1
#define  PAGE_BREAK "************************************************************************************************************************************"
#define  PAGE_MARK  "------------------------------------------------------------------------------------------------------------------------------------"
/***************************** end #define ************************************/
/******************************* enum *****************************************/
/***************************** end enum ***************************************/
/******************************* typedef **************************************/

typedef struct processStep {
  int                    type;     /* 0 = no outage, 1 = branch, 2 = change */
  char  oldBase[FILENAME_MAX];     /* load this base case */
  char  priChng[FILENAME_MAX];     /* primary change file */
  void              *outgLink;     /* outage: branch, change, or common_mode */
  pf_rec                busID;     /* do QV at this bus */
  char  qvcRept[FILENAME_MAX];     /* save QV curve with this name */
  char  usrRept[FILENAME_MAX];     /* save user analysis with this name */
  float                  Qmin;
  float               VatQmin;
  float            dVdQatQmin;
  float                  Vmin;
  float               QatVmin;
} Step;

typedef struct xyPlotSetup {
  char     setName[FILENAME_MAX];     /* setup file name */
  int             numberOfCurves;     /* up to 5 allowed */
  char        superTitle[MAX_IN];     /* super title for plot */
  char  qvpFile[6][FILENAME_MAX];     /* array of six qvpt point files */
  char       subTitle[6][MAX_IN];     /* array of six subtitles */
  char         legend[6][MAX_IN];     /* array of six legends */
  char        xAxisTitle[MAX_IN];     /* x axis title */
  char        yAxisTitle[MAX_IN];     /* y axis title */
} Plot;

typedef struct traceRecord {
  Link  *xydtList;
  Link  *trceList;
  Link  *stepList;
  Link  *plotList;
  Link  *setuList;
  Link  *subtList;
  int    trace;
  int    print;
  int    save;
  long   query;
  FILE  *ps_File;
  FILE  *setFile;
  char   setSpec[FILENAME_MAX];
  char   ps_Spec[FILENAME_MAX];
  char   superTitle[MAX_IN];
  char   xTitle[MAX_IN];
  char   yTitle[MAX_IN];
  float  xMax;
  float  xMin;
  float  xInc;
  float  yMax;
  float  yMin;
  float  yInc;
  int    mark;
  char   timeStamp[MAX_IN];
  char   userID[MAX_IN];
} Trace;

/******************************* end typedef **********************************/

/* top MIMIC functions - called by main() *************************************/
void  initializeTrace(Trace *trace);
void  processCommandLine(Trace *trace, int argc, char *argv[]);
void  instructions(Trace *trace);
void  promptUser(Trace *trace);
void  printTrace(Trace *trace);
void  queryContinue(Trace *trace);
void  openReport(Trace *trace);
void  buildPostScript(Trace *trace);
void  printPostScript(Trace *trace);
void  insertXYfile(FILE *ps, FILE *xy, char *legend, int mark);
void  finalRemarks(Trace *trace);
/* end MIMIC functions - called by main() *************************************/

/* top MIMIC functions - report functions *************************************/
/* end MIMIC functions - report functions *************************************/

/* top MIMIC functions - support functions ************************************/
void   makeNewName(Step *curStep, int *outgNum);
void   ff_scale(float min, float max, float *first, float *last, float inc);
/* end MIMIC functions - support functions ************************************/

/* top LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/
void   ff_stream2List(FILE *readMe, Trace *trace, Link **expList);
void   ff_report(char *s, Trace *trace);
void   ff_limits(char *s, Trace *trace);
void   ff_traceName(Trace *trace);
Link  *ff_insRow(Link **table, Link *rowPtr, Link *newRow, Trace *trace);
void   ff_expList(Trace *trace, Link *xydtList, Link **expList);
int    ff_getList(Trace *trace, char *dataFile, Link **expList);
void   ff_freeRowList(Link **rowList);
void   xf_fprintList(FILE *fp,Link *link,char *tag,char *tab, char type, int n);
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
  promptUser(&trace);
  ff_traceName(&trace);
  printTrace(&trace);
  queryContinue(&trace);
  openReport(&trace);
  buildPostScript(&trace);
  printPostScript(&trace);
  finalRemarks(&trace);
  time(&CF_time1);
  cf_logUse("XYcurve", "", trace.userID);
  return 0;
}
/* end of main() */
void  initializeTrace(Trace *trace)
{
  memset(trace, '\0', sizeof(trace));
  trace->trceList         =   NULL;
  trace->stepList         =   NULL;
  trace->xydtList         =   NULL;
  trace->subtList         =   NULL;
  trace->trace            =   TRACE_NO;
  trace->print            =   PRINT_YES;
  trace->save             =   SAVE_NO;
  trace->query            = ( QUERY_SETU | QUERY_LMTS | QUERY_SUBT |
                              QUERY_SUPT | QUERY_XTTL | QUERY_YTTL |
                              QUERY_XYDT | QUERY_MARK | QUERY_CONT );
  trace->xMax            = 1.10;
  trace->xMin            = 0.90;
  trace->xInc            = 0.02;
  trace->yMin            = -500;
  trace->yMax            =  500;
  trace->yInc            =  100;
  trace->mark            =    1;
  strcpy(trace->setSpec, SET_SPEC);
  cuserid(trace->userID);
  cf_time(trace->timeStamp, MAX_IN, "%d-%b-%Y %H:%M:%S");
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
  ff_expList(trace, topLink, NULL);
  return;
}
void instructions(Trace *trace)
{
  if (trace->query==QUERY_CONT) {
    trace->query = 0;
    return;
  }
  printf("\n                      Welcome to XYcurve - updated 01-09-97");
  printf("\n");
  printf("\n - Defaults in brackets [].  Press Ctrl-Y & type STOP to abort.");
  printf("\n - Use .setup to designate preference settings files.");
  printf("\n");
}
void promptUser(Trace *trace)
{
  char query[MAX_IN];
  Link *list;

  if (trace->query==0) return;
  printf("%s", DOT_LINE);
  if (trace->query & QUERY_SETU) {
    printf("\n\n/SETUP");
    cf_sprompt("\n > Enter setup file name   [%s]: ", trace->setSpec, trace->setSpec);
    trace->trace = ff_getList(trace, trace->setSpec, NULL);
  }
  if (trace->query & QUERY_XYDT) {
    printf("\n\n/XY_DATA");
    cf_nprompt("\n > Enter a list of or XY data files : ", "", MAX_IN, query);
    list = cf_text2List(query);
    ff_expList(trace, list, &trace->xydtList);
  }
  if (trace->xydtList==NULL) {
    cf_exit(1, "Quitting!\n");
  }
  if (trace->query & QUERY_LMTS) {
    printf("\n\n/LIMITS");
    printf("\n For the X-Y plot, enter axis limits:");
    cf_fprompt("\n > XMAX = [%5.3f]: ", trace->xMax, &trace->xMax);
    cf_fprompt("\n > XMIN = [%5.3f]: ", trace->xMin, &trace->xMin);
    cf_fprompt("\n > XINC = [%5.3f]: ", trace->xInc, &trace->xInc);
    cf_fprompt("\n > YMAX = [%5.3f]: ", trace->yMax, &trace->yMax);
    cf_fprompt("\n > YMIN = [%5.3f]: ", trace->yMin, &trace->yMin);
    cf_fprompt("\n > YINC = [%5.3f]: ", trace->yInc, &trace->yInc);
    cf_iprompt("\n > MARK =     [%1d]: ", trace->mark, &trace->mark);
  }
  printf("\n");
  printf("\n%s\n", DOT_LINE);
  return;
}
void printTrace(Trace *trace)
{
  FILE *fp;

  if (trace->trace==TRACE_NO) return;
  fp = cf_openFile(trace->setSpec, "w");
  if (fp == NULL) fp = stderr;
  fprintf(fp, ". %s %s %s\n", trace->setSpec, trace->timeStamp, trace->userID);

  fprintf(fp, "/LIMITS\n");
  fprintf(fp, "  XMAX = %5.3f\n", trace->xMax);
  fprintf(fp, "  XMIN = %5.3f\n", trace->xMin);
  fprintf(fp, "  XINC = %5.3f\n", trace->xInc);
  fprintf(fp, "  YMAX = %5.3f\n", trace->yMax);
  fprintf(fp, "  YMIN = %5.3f\n", trace->yMin);
  fprintf(fp, "  YINC = %5.3f\n", trace->yInc);
  fprintf(fp, "  MARK = %d\n", trace->mark);

  xf_fprintList(fp, trace->xydtList, "/XY_FILES\n",     "",   's',  1);
  fclose(fp);
}
void queryContinue(Trace *trace)
{
  int yes;

  if (CF_logFile != NULL)
    printf("\nError Summary written to %s", CF_logSpec);
  if ( trace->query & QUERY_CONT) {
    printf("\n%s\n", DOT_LINE);
    yes = cf_yprompt("\n > Do you want to continue this run? [%c]: ", 'Y');
    cf_exit(!yes, "Quitting!");
  }
}
void openReport(Trace *trace)
{
  if (trace->trace==TRACE_NO) return;
  trace->setFile = cf_openFile(trace->setSpec, "w");
  cf_exit(trace->setFile==NULL, "Quitting!\n");
  return;
}
void  buildPostScript(Trace *trace)
{
  FILE *ps, *xy;
  Link *subtLink, *xydtLink;
  char  s[MAX_IN], data[MAX_IN];

  sprintf(s, "copy cfprogs:qvmaster.post %s", trace->ps_Spec);
  system(s);
  ps = cf_openFile(trace->ps_Spec, "a");
  if (ps==NULL) return;

  fprintf(ps, "%f %f %f (%s) qvc_yAxis\n",
    trace->yMin, trace->yMax, trace->yInc, trace->yTitle);
  fprintf(ps, "%f %f %f (%s) qvc_xAxis\n",
    trace->xMin, trace->xMax, trace->xInc, trace->xTitle);
  fprintf(ps, "(%s) SuperTitle\n", trace->superTitle);

  for (subtLink = trace->subtList; subtLink != NULL; subtLink = subtLink->next){
  	fprintf(ps, "(%s) SubTitle\n", subtLink->data);
  }
  for (xydtLink = trace->xydtList; xydtLink != NULL; xydtLink = xydtLink->next){
        xy = cf_openFile(xydtLink->data, "r");
        if (xy==NULL) continue;
	insertXYfile(ps, xy, xydtLink->data, trace->mark);
  }
  fprintf(ps, "(%s) (%s) qvc_Date\n", trace->setSpec, trace->timeStamp);

  fprintf(ps, " 0 0 998 pPlot\n");
  fclose(ps);
}
void  printPostScript(Trace *trace)
{
  char  s[MAX_IN];

  if (trace->print==PRINT_NO) return;

  if (getenv("CFLOW_PSQ")==NULL) {
    printf("CFLOW_PSQ undefined!\n");
    printf("--In your login file define something like: CFLOW_PSQ :== PRINT/QUE=PS_EOFQMS\n");
  }
  else {
    printf("Submitting Plot...\n");
    sprintf(s, "CFLOW_PSQ %s", trace->ps_Spec);
    system(s);
  }
}
void insertXYfile(FILE *ps, FILE *xy, char *legend, int mark)
{
  char str[MAX_IN], comment[MAX_IN];
  int  lgnd, titl, comm;
  double x, y;

  lgnd = 0; titl = 0;
  while (fgets(str, MAX_IN, xy)!=NULL) { 
    if (str[0]=='C') { /* may be other types of comment characters */
        if (titl==0 && str[1]=='1') { /* subtitle */
            sscanf(str, "C1 %[^\n]", comment);
            fprintf(ps, "(%s) SubTitle\n", comment);
            titl = 1;
        }
        if (lgnd==0 && str[1]=='2') { /* legend */
            sscanf(str, "C2 %[^\n]", comment);
            fprintf(ps, "(%s) %d qvc_Case\n", comment, mark);
            lgnd = 1;
        }
    }
    else {
        if (titl==0) {
            fprintf(ps, "(%s) subTitle\n", legend);
            titl = 1;
        }
        if (lgnd==0) {
            cf_parse(legend, comment, 'N');
            comm = (strlen(comment) > 12) ? strlen(comment)-12 : 0;
            fprintf(ps, "(%s) %d qvc_Case\n", &comment[comm], mark);
            lgnd = 1;
        }
        sscanf(str, "%lf %lf", &x, &y);
        fprintf(ps, "%.10f %.10f qvc_Dat\n", x, y);
    }
  }
}
void  finalRemarks(Trace *trace)
{
/*
  printf("\nMemory allocation (bytes): Cur:%d Max:%d Alloc:%d Freed:%d\n",
    cf_memCurAlloc, cf_memMaxAlloc, cf_memTotAlloc, cf_memTotFreed);
*/
  printf("PostScript file written to %s\n", trace->ps_Spec);
  if (trace->trace == TRACE_YES)
    printf("Setup File written to %s\n", trace->setSpec);
  if (CF_logFile != NULL) printf("Error  report written to %s\n", CF_logSpec);
}
/* end function definitions */
/* proposed CF_UTIL.H functions */
void ff_expList(Trace *trace, Link *xydtList, Link **expList)
{ /* expand double-linked list */
  Link *curLink, *newList;
  FILE *readMe;

  curLink = xydtList;
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
    curLink = cf_delLink(&xydtList, curLink);
  }
}
int ff_getList(Trace *trace, char *dataFile, Link **expList)
{ /* version of ff_expList for use with only one file name */
  FILE *readMe;

  readMe = cf_openFile(dataFile, "r");/* it's okay if readMe==NULL */
  if (readMe==NULL) return TRACE_YES;
  ff_stream2List(readMe, trace, expList);
  fclose(readMe);
  return TRACE_NO;
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
    if ( strstr(STR, "/SUPER_TITLE" )!=NULL ) { mode = READ_SUP; }
    if ( strstr(STR, "/SUB_TITLE"   )!=NULL ) { mode = READ_SUB; trace->query &= ~(QUERY_SUBT); }
    if ( strstr(STR, "/X_TITLE"     )!=NULL ) { mode = READ_XTL; }
    if ( strstr(STR, "/Y_TITLE"     )!=NULL ) { mode = READ_YTL; }
    if ( strstr(STR, "/XY_DATA"     )!=NULL ) { mode = READ_XYD; }
    if ( strstr(STR, "/LIMITS"      )!=NULL ) { mode = READ_LIM; }
    if ( mode == 0 ) continue;         /* note: mode is defaulted to READ_INC */
    if ( STR[0]=='/' ) {
        if ( (cp=strpbrk(str, "|,"))!=NULL ) strcpy(str, (++cp));
        else continue;
    }
    trace->query &= ~(QUERY_SETU);
    if ( mode == READ_INC ) {
        list = cf_text2List(str);
        ff_expList(trace, list, expList);
    }
    if ( mode == READ_XYD ) {
        list = cf_text2List(str);
        if (list!=NULL) trace->query &= ~(QUERY_XYDT);
        cf_appList(&trace->xydtList, list);
    }
    if ( mode == READ_SUP ) {
        strcpy(trace->superTitle, str);
        trace->query &= ~(QUERY_SUPT);
    }
    if ( mode == READ_SUB ) {
        list = cf_text2Link(str);
        cf_appList(&trace->subtList, list);
        trace->query &= ~(QUERY_SUBT);
    }
    if ( mode == READ_XTL ) {
        strcpy(trace->xTitle, str);
        trace->query &= ~(QUERY_XTTL);
    }
    if ( mode == READ_YTL ) {
        strcpy(trace->yTitle, str);
        trace->query &= ~(QUERY_YTTL);
    }
    if ( mode == READ_REP ) ff_report(str, trace);
    if ( mode == READ_LIM ) ff_limits(str, trace);
  }
  return;
}
void ff_report(char *s, Trace *trace)
{
  char *sp, *tp;
  for (sp = strtok(s, " ,="); sp!=NULL; sp = strtok(NULL, " ,=")) {
    tp=strtok(NULL, ", =");
    if (strstr(sp, "TITLE"        )!=NULL) {
        if (tp==NULL) return;
        sscanf(trace->superTitle, "(%[^)\n]", tp);
        trace->query &= ~(QUERY_SUPT);
        continue;
    }
    if (tp==NULL) return;
  }
  return;
}
void ff_limits(char *s, Trace *trace)
{
  char  *sp, *tp, t[MAX_IN];

  for (sp = strtok(s, " ,="); sp!=NULL; sp = strtok(NULL, " ,=")) {
    tp=strtok(NULL, ", ="); if (tp==NULL) return;
    if (strstr(sp, "XMAX"        )!=NULL) {
        sscanf(tp, "%f", &trace->xMax);
        trace->query &= ~(QUERY_LMTS);
    }
    if (strstr(sp, "YMAX"        )!=NULL) {
        sscanf(tp, "%f", &trace->yMax);
        trace->query &= ~(QUERY_LMTS);
    }
    if (strstr(sp, "XMIN"        )!=NULL) {
        sscanf(tp, "%f", &trace->xMin);
        trace->query &= ~(QUERY_LMTS);
    }
    if (strstr(sp, "YMIN"        )!=NULL) {
        sscanf(tp, "%f", &trace->yMin);
        trace->query &= ~(QUERY_LMTS);
    }
    if (strstr(sp, "XINC"        )!=NULL) {
        sscanf(tp, "%f", &trace->xInc);
        trace->query &= ~(QUERY_LMTS);
    }
    if (strstr(sp, "YINC"        )!=NULL) {
        sscanf(tp, "%f", &trace->yInc);
        trace->query &= ~(QUERY_LMTS);
    }
    if (strstr(sp, "MARK"        )!=NULL) {
        sscanf(tp, "%d", &trace->mark);
        trace->query &= ~(QUERY_MARK);
        trace->query &= ~(QUERY_LMTS);
    }
  }
}
void ff_traceName(Trace *trace)
{
  char s[FILENAME_MAX];
  cf_parse(trace->setSpec, s, 'P');  /* PATH */
  strcpy(trace->ps_Spec, s);
  cf_parse(trace->setSpec, s, 'N');  /* NAME */
  strcat(trace->ps_Spec, s);
  strcat(trace->ps_Spec, ".ps");    /* TYPE */
}
void xf_fprintList(FILE *fp, Link *link, char *tag, char *tab, char type, int n)
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
    if (link->next!=NULL && n!=1 && --i>0) fprintf(fp, ", ");
    else fprintf(fp, "\n");
  }
}
/************ documented, common CF_UTIL.H candidates prototypes **************/
/* all documented, CF_UTIL.H candidates shall be designated, cf_name().       */
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
  for (*upper =      0; *upper < max; *upper += inc);
  for (*lower = *upper; *lower > min; *lower -= inc);
}
