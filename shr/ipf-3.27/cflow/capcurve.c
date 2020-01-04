/******************************************************************************\
UTILITY:    CAPcurve
STRUCTURE:  common CFLOW architecture.
TYPE:       
SUMMARY:    Generate capacitor reactance-voltage curves.
RELATED:    LINEFLOW, FINDOUT, CFUSE, MIMIC, QVCURVE
SEE ALSO:   MODES.TXT
UPDATED:    February 19, 1997
LANGUAGE:   Standard C.  CFLOW Libraries.  CF_UTIL.H.
DEVELOPER:  William D. Rogers, BPA, TEOS, 230-3806, wdrogers@bpa.gov
REQUESTER:  Scott Kinney, BPA, TEOC, 230-3905
USERS:      Scott Kinney, Kyle Kohne, Melvin Rodrigues, Gordon Comegys
IPF:        Version 317 or above recommended.
PURPOSE:    
\******************************************************************************/
/******************************* #include *************************************/
#include "cflowlib.h"
#include "cf_util.h"
/***************************** end #include ***********************************/
/******************************* #define **************************************/
#define  OK               0
#define  DOT_LINE         "..................................................."
#define  LOG_SPEC         "capcurve.log"
#define  OUT_NAME         "capcurve"
#define  QUERY_MVAR  (int) (1<< 0) /* prompt for capacitor mvar */
#define  QUERY_VOLT  (int) (1<< 1) /* prompt for capacitor voltage */
#define  QUERY_NAME  (int) (1<< 2) /* prompt for output report file name */
#define  QUERY_LMTS  (int) (1<< 3) /* prompt for limits */
#define  QUERY_CONT  (int) (1<< 4) /* prompt for whether to continue run */
#define  READ_INC    (int)      1  /* found /INCLUDE card in CFLOW data file */
#define  READ_REP    (int)      2  /* found /REPORT  card in CFLOW data file */
#define  READ_LIM    (int)      3  /* found /LIMITS  card in CFLOW data file */
#define  TRACE_NO    (int)      0
#define  TRACE_YES   (int)      1
#define  PAGE_BREAK "************************************************************************************************************************************"
#define  PAGE_MARK  "------------------------------------------------------------------------------------------------------------------------------------"
#define  FORM_DATA_FULL  (int)    1
#define  TYPE_NONE   (int)      0
/***************************** end #define ************************************/
/******************************* typedef **************************************/

typedef struct xyPlotSetup {
  char     setName[FILENAME_MAX];     /* setup file name */
  int             numberOfCurves;     /* up to 5 allowed */
  char    superTitle[CF_STRSIZE];     /* super title for plot */
  char  qvpFile[6][FILENAME_MAX];     /* array of six qvpt point files */
  char   subTitle[6][CF_STRSIZE];     /* array of six subtitles */
  char     legend[6][CF_STRSIZE];     /* array of six legends */
  char    xAxisTitle[CF_STRSIZE];     /* x axis title */
  char    yAxisTitle[CF_STRSIZE];     /* y axis title */
} Plot;

typedef struct traceRecord {
  int    trace;
  int    fail;
  long   query;
  cf_Out *trc;
  cf_Out *out;
  char   outName[FILENAME_MAX];
  double capMVAR;
  double capVolt;
  double Vmax;
  double Vmin;
  double dV;
  char   timeStamp[CF_STRSIZE];
  char   userID[CF_STRSIZE];
} Trace;

/******************************* end typedef **********************************/

/* top CAPCURVE functions - called by main() **********************************/
void  initializeTrace(Trace *trace);
void  processCommandLine(Trace *trace, int argc, char *argv[]);
void  instructions(Trace *trace);
void  promptUser(Trace *trace);
void  printTrace(Trace *trace);
void  openReport(cf_Out *rpt);
void  queryContinue(Trace *trace);
void  makeCurve(Trace *trace);
void  finalRemarks(Trace *trace);
/* end CAPCURVE functions - called by main() **********************************/

/* top LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/
void   ff_stream2List(FILE *readMe, Trace *trace, Link **expList);
void   ff_report(char *s, Trace *trace);
void   ff_limits(char *s, Trace *trace);
void   ff_expList(Trace *trace, Link *dataList, Link **expList);
/* end LINEFLOW, FINDOUT, MIMIC - similar functions ***************************/

int main(int argc, char *argv[])
{
  Trace trace;

  time(&CF_time0);
  initializeTrace(&trace);
  processCommandLine(&trace, argc, argv);
  instructions(&trace);
  promptUser(&trace);
  openReport(trace.trc);
  printTrace(&trace);
  queryContinue(&trace);
  openReport(trace.out);
  makeCurve(&trace);
  finalRemarks(&trace);
  time(&CF_time1);
  cf_logUse("CAPcurve", "", trace.userID);
  return 0;
}
/* end of main() */
void  initializeTrace(Trace *trace)
{
  memset(trace, '\0', sizeof(trace));
  trace->trace           =   TRACE_YES;
  trace->fail            =   0;
  trace->query           = ( QUERY_NAME | QUERY_LMTS | QUERY_CONT |
                             QUERY_MVAR | QUERY_VOLT );
  trace->capMVAR         = 100.;
  trace->capVolt         = 1.0;
  trace->Vmax            = 1.10;
  trace->Vmin            = 0.90;
  trace->dV              = 0.05;
  cuserid(trace->userID);
  cf_time(trace->timeStamp, CF_STRSIZE, "  %d-%b-%Y %H:%M:%S");
  strcpy(CF_logSpec, LOG_SPEC);
  strcpy(trace->outName, OUT_NAME);
  trace->trc = cf_initOut("", trace->timeStamp, TRACE_YES, FORM_DATA_FULL);
  trace->out = cf_initOut("", trace->timeStamp, TYPE_NONE, FORM_DATA_FULL);
/*  trace->trc = cf_initOut("", NULL, 0, 1, trace->timeStamp);
  trace->out = cf_initOut("", NULL, 0, 1, trace->timeStamp); */
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
  printf("\n                      Welcome to CAPcurve - updated 02-19-97");
  printf("\n");
  printf("\n - Defaults in brackets [].  Press Ctrl-Y & type STOP to abort.");
  printf("\n");
}
void promptUser(Trace *trace)
{
  char query[CF_INBUFSIZE];
  Link *list;

  printf("%s", DOT_LINE);
  if (trace->query & (QUERY_MVAR | QUERY_VOLT | QUERY_LMTS)) {
    printf("\n\n/LIMITS");
    printf("\n To calculate the Capacitor Reactance-Voltage curve,");
    printf("\n give the reactance (MVAR) at a chosen voltage (PU).");
  }
  if (trace->query & QUERY_MVAR) {
    cf_dprompt("\n > Enter MVAR at chosen voltage,  CAP_MVAR = [%5.1f]: ", trace->capMVAR, &trace->capMVAR);
  }
  if (trace->query & QUERY_VOLT) {
    cf_dprompt("\n > Enter voltage at given MVAR,   CAP_VOLT = [%5.3f]: ", trace->capVolt, &trace->capVolt);
  }
  if (trace->query & QUERY_LMTS) {
    printf("\n\n For Capacitor Q-V curve, enter Min, Max, and Delta Voltage:");
    cf_dprompt("\n > Enter starting voltage in PU, VX_MAX_PU = [%5.3f]: ",trace->Vmax, &trace->Vmax);
    cf_dprompt("\n > Enter ending voltage in PU,   VX_MIN_PU = [%5.3f]: ",trace->Vmin, &trace->Vmin);
    cf_dprompt("\n > Enter voltage change in PU, DELTA_VX_PU = [%5.3f]: ",trace->dV,   &trace->dV  );
  }
  if (trace->query & QUERY_NAME) {
    printf("\n\n/REPORT");
    cf_sprompt("\n > Enter output file default name, NAME = [%s]: ", trace->outName, trace->outName);
  }

  if (trace->query == QUERY_CONT) trace->query &= ~(QUERY_CONT);

  printf("\n");
  cf_strsuf(trace->trc->spec, trace->outName, '.', ".trc");
  cf_strsuf(trace->out->spec, trace->outName, '.', ".out");
  printf("\n%s\n", DOT_LINE);
  return;
}
void printTrace(Trace *trace)
{
  FILE *fp;

  if (trace->trace==TRACE_NO) return;
  fp = trace->trc->file;
  if (fp == NULL) fp = stderr;
  fprintf(fp, ". %s %s %s\n", trace->trc->spec, trace->trc->time, trace->userID);

  fprintf(fp, "/REPORT\n");
  fprintf(fp, "  NAME = %s\n", trace->outName);
  if ( trace->trace == TRACE_YES) fprintf(fp, "  TRACE = YES\n");
  if ( trace->trace == TRACE_NO ) fprintf(fp, "  TRACE = NO\n");

  fprintf(fp, "/LIMITS\n");
  fprintf(fp, "  CAP_MVAR    = %5.3f\n", trace->capMVAR);
  fprintf(fp, "  CAP_VOLT    = %5.3f\n", trace->capVolt);
  fprintf(fp, "  VX_MAX_PU   = %5.3f\n", trace->Vmax);
  fprintf(fp, "  VX_MIN_PU   = %5.3f\n", trace->Vmin);
  fprintf(fp, "  DELTA_VX_PU = %5.3f\n", trace->dV);

  fclose(fp);
}
void openReport(cf_Out *rpt)
{
  rpt->file = cf_openFile(rpt->spec, "w");
  cf_exit(rpt->file==NULL, "Quitting!\n");
}
void queryContinue(Trace *trace)
{
  int  yes;

  if (trace->trace == TRACE_YES)
    printf("\nTrace written to %s", trace->trc->spec);
  if (CF_logFile != NULL)
    printf("\nError Summary written to %s", CF_logSpec);
  printf("\n%s\n", DOT_LINE);
  if ( trace->query & QUERY_CONT) {
    yes = cf_yprompt("\n > Do you want to continue this run? [%c]: ", 'Y');
    cf_exit(!yes, "Quitting!");
  }
}
void  makeCurve(Trace *trace)
{
  double v, q;

  for (v =trace->Vmax; v >=trace->Vmin; v -=trace->dV) {
    q =  trace->capMVAR * ( (v*v) / ( trace->capVolt*trace->capVolt) );
    cf_eprint(trace->out->file, "%.10f %.10f\n", v, q);
  }
  return;
}
void  finalRemarks(Trace *trace)
{
  printf("\nOutput data written to %s", trace->out->spec);
  if (trace->trace == TRACE_YES)
    printf("\nTrace  report written to %s", trace->trc->spec);
  if (CF_logFile != NULL) printf("\nError  report written to %s", CF_logSpec);
  printf("\n");
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
    if ( strstr(STR, "/REPORT"      )!=NULL ) { mode = READ_REP; }
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
    if ( mode == READ_REP ) ff_report(str, trace);
    if ( mode == READ_LIM ) ff_limits(str, trace);
  }
  return;
}
void ff_limits(char *s, Trace *trace)
{
  char  *sp, *tp, t[CF_INBUFSIZE];
  float f;

  for (sp = strtok(s, " ,="); sp!=NULL; sp = strtok(NULL, " ,=")) {
    tp=strtok(NULL, ", ="); if (tp==NULL) return;
    if (strstr(sp, "VX_MAX_PU"   )!=NULL) {
        sscanf(tp, "%lf", &trace->Vmax);
        trace->query &= ~(QUERY_LMTS);
    }
    if (strstr(sp, "VX_MIN_PU"   )!=NULL) {
        sscanf(tp, "%lf", &trace->Vmin);
        trace->query &= ~(QUERY_LMTS);
    }
    if (strstr(sp, "DELTA_VX_PU" )!=NULL) {
        sscanf(tp, "%lf", &trace->dV);
        trace->query &= ~(QUERY_LMTS);
    }
    if (strstr(sp, "CAP_MVAR"    )!=NULL) {
        sscanf(tp, "%lf", &trace->capMVAR);
        trace->query &= ~(QUERY_MVAR);
    }
    if (strstr(sp, "CAP_VOLT"    )!=NULL) {
        sscanf(tp, "%lf", &trace->capVolt);
        trace->query &= ~(QUERY_VOLT);
    }
  }
}
void ff_report(char *s, Trace *trace)
{
  char *sp, *tp;
  for (sp = strtok(s, " ,="); sp!=NULL; sp = strtok(NULL, " ,=")) {
    tp=strtok(NULL, ", =");
    if (tp==NULL) return;
    if (strstr(sp, "NAME")!=NULL) {
        trace->query &= ~(QUERY_NAME);
        strcpy(trace->outName, tp);
        continue;
    }
    if (strstr(sp, "TRACE")!=NULL) {
        if (strstr(tp, "YES")!=NULL) trace->trace = TRACE_YES;
        if (strstr(tp, "NO" )!=NULL) trace->trace = TRACE_NO;
        continue;
    }
  }
  return;
}
