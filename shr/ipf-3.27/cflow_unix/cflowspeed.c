/******************************************************************************\
UTILITY:    CFLOWSPEED
TYPE:       Powerflow (IPFSRV v. 209 or later) data extraction.
SUMMARY:    Read records from a case as fast as possible and time how long it
            takes.
RELATED:
SEE ALSO:
START DATE: November 14, 1994
UPDATED:    February 15, 1995
LANGUAGE:   Standard C.  CFLOW Libraries.
DEVELOPER:  WDRogers, BPA, TEOS, x3806
REQUESTER:  JGColeman, WDRogers, BPA, TEOS
USERS:      Will Rogers
PURPOSE:    Serve as a benchmark for the exchange of powerflow data between 
            Powerflow and a CFLOW program.  Test the speed of retrieving bus and
            branch records.
\******************************************************************************/

/* #includes */
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
/* #include <math.h> */
#include <time.h>
#include "cflowlib.h"
/* end #includes */

/* #defines */
#define OK                     0
#define NOTOK                 -1
#define TESTLENGTH             3
#define PROCESSLOOP   (int) (400000 * TESTLENGTH)
#define QUERYLOOP     (int)   (1000 * TESTLENGTH)
#define BUSEXISTSLOOP (int)   (1000 * TESTLENGTH)
#define BUSB2ALOOP    (int)   (2000 * TESTLENGTH)
#define B2ALOOP       (int)    (400 * TESTLENGTH)
#define FIRSTBUSLOOP  (int)   (1000 * TESTLENGTH)
#define SEPCHARS "\n"
#define RPT_SPEC               "cflowspeed.rpt"
#define BSE_SPEC               "J95EH163.BSE"
#define MAXHEADER            400
#define MAXLINE              133
/* end #defines */

/* typedefs */
typedef struct statistics {
    int    loops;
    double delay;
    double loopsPerSecond;
    double delayPerLoop;
} TimeStats;

typedef struct measurements {
    time_t start;
    time_t stop;
} StopWatch;
/* end typedefs */

/* declare global variables */
char FirstBus[]=   "/get_data, type=cflow_input, table=bus,action=f";
char Eom[]=        "*[EOM]";
int  error, ipf_status;
char RtnStat[]=    "return status: ";
char action[]=     "F";
char      data[] = "Test data for the process delay loop to time sprintf.\n";

FILE      *rpt_file;
char      rpt_spec[FILENAME_MAX], bse_spec[FILENAME_MAX];
int       count, status;
StopWatch Timer;
char      header[MAXHEADER], temp[MAXHEADER], b_data[MAXLINE], title[MAXLINE];
pf_rec    b;
/* end declare global variables */

/* function prototypes */
int  processDelay(TimeStats *Stats);
int  busB2aDelay(TimeStats *Stats);
int  branchB2aDelay(TimeStats *Stats);
int  busExistsDelay(TimeStats *Stats);
int  queryDelay(TimeStats *Stats);
int  firstBusDelay(TimeStats *Stats);
int  nextBusDelay(TimeStats *Stats);
int  nextBranchDelay(TimeStats *Stats);
int  nextAreaDelay(TimeStats *Stats);
int  nextCbusDelay(TimeStats *Stats);
int  nextItieDelay(TimeStats *Stats);
int  nextXdataDelay(TimeStats *Stats);
void printStats(TimeStats *Stats, FILE *fp, char *header);
void computeStats(StopWatch *Timer, TimeStats *Stats, int count);
int  query_pf(char *cmd, char *data);
void reportStats(StopWatch *Tmr, TimeStats *Sts, int cnt, FILE *fp, char *hdr);
int get_pf_rec(pf_rec *b, char *code, char *type, char *template, char *header);
void get_spec(char *spec, char *default_spec);
/* end of function prototypes */

main(int argc, char *argv[])
{
  TimeStats ProcessStats, BusB2aStats, BranchB2aStats, BusExistsStats;
  TimeStats QueryStats, FirstBusStats, NextBusStats, NextBranchStats;
  TimeStats NextAreaStats, NextCbusStats, NextItieStats, NextXdataStats;

/********************* initialize benchmark ***********************************/
  pf_cflow_init(argc, argv);

/* get test case file */
  printf("\n > Enter test case file spec [%s]: ", BSE_SPEC);
  get_spec(bse_spec, BSE_SPEC);

  if ( (rpt_file = fopen(RPT_SPEC, "w")) == NULL ) {
    fprintf(stderr, "Cannot open report file: %s\n", RPT_SPEC);
    return;
  }
  printf("\n > Enter report title: ");
  gets(title);
  fprintf(rpt_file, "%s\n", title);
  fprintf(stdout, "\n%s\n", title);

  if ( time(&Timer.start) == -1 ) {                /* is time availble? */
    fprintf(stderr, "WARNING:  TIME NOT AVAILABLE\n");
    return;
  }
  status = pf_load_oldbase(bse_spec);
  if ( status != OK ) {
    fprintf(stderr, "WARNING:  TEST CASE NOT FOUND %s\n", bse_spec);
    pf_cflow_exit();
    return;
  }
/********************** benchmark initialized *********************************/
  processDelay(&ProcessStats);
  busB2aDelay(&BusB2aStats);
  branchB2aDelay(&BranchB2aStats);
  busExistsDelay(&BusExistsStats);
  queryDelay(&QueryStats);
  firstBusDelay(&FirstBusStats);
  nextBusDelay(&NextBusStats);
  nextAreaDelay(&NextAreaStats);
  nextCbusDelay(&NextCbusStats);
  nextItieDelay(&NextItieStats);
  nextXdataDelay(&NextXdataStats);
  nextBranchDelay(&NextBranchStats);

  fclose(rpt_file);
  printf("\n Report written to file: %s\n", RPT_SPEC);
  pf_cflow_exit();
}

int processDelay(TimeStats *Stats)
{
  sprintf(header, "\n******* processSpeed test using %d loops\n",PROCESSLOOP);

  time(&Timer.start);
  for (count = 0; count < PROCESSLOOP; count++ ) {
    sprintf(b_data, "%s", data);
    switch (count) {
        case -1: break;
        default: break;
    }
  }
  time(&Timer.stop);
  reportStats(&Timer, Stats, count, rpt_file, header);
  return 0;
}

int busB2aDelay(TimeStats *Stats)
{
  sprintf(header, "\n******* busB2aSpeed test using %d loops\n", BUSB2ALOOP);
  if ( get_pf_rec(&b,"F","Bus", "Converted bus: %18.18s\n", header) != 0 )
    return -1;

  time(&Timer.start);
  for ( count = 0;  count < BUSB2ALOOP; count++ ) {
    pf_rec_b2a(b_data, &b, "I");
  }
  time(&Timer.stop);

  reportStats(&Timer, Stats, count, rpt_file, header);
  return 0;
}

int branchB2aDelay(TimeStats *Stats)
{
  sprintf(header, "\n******* branchB2aSpeed test using %d loops\n", B2ALOOP);
  if ( get_pf_rec(&b,"F","Branch", "Converted branch: %33.33s\n", header) != 0 )
    return -1;

  time(&Timer.start);
  for ( count = 0; count < B2ALOOP; count++ ) {
    status = pf_rec_branch(&b, "G");
  }
  time(&Timer.stop);
  reportStats(&Timer, Stats, count, rpt_file, header);
  return 0;
}

int busExistsDelay(TimeStats *Stats)
{
  sprintf(header, "\n******* busExistsSpeed test using %d loops\n",
    BUSEXISTSLOOP);
  if ( get_pf_rec(&b,"F","Bus","Tested bus: %18.18s\n", header) != 0) return -1;

  time(&Timer.start);
  for ( count = 0; count < BUSEXISTSLOOP; count++ ) {
    pf_bus_exists(b.i.ACbus.name, b.i.ACbus.kv);
  }
  time(&Timer.stop);

  reportStats(&Timer, Stats, count, rpt_file, header);
  return 0;
}

int queryDelay(TimeStats *Stats)
{
  sprintf(header, "\n******* querySpeed test using %d loops\n", QUERYLOOP);

  time(&Timer.start);
  for ( count = 0; count < QUERYLOOP; count++ ) {
    switch(toupper(action[0])) {
        case 'F': status = query_pf(FirstBus, "");
                  if (status!=0) printf("ouch\n");
                  break;
        default:  fprintf(stderr,"queryDelay: invalid action code %s\n",action);
                  return -1;
    }
  }
  time(&Timer.stop);
  reportStats(&Timer, Stats, count, rpt_file, header);
  return 0;
}

int firstBusDelay(TimeStats *Stats)
{
  sprintf(header, "\n******* firstBusSpeed test using %d loops\n",
    FIRSTBUSLOOP);
  if ( get_pf_rec(&b,"F","Bus","First bus: %18.18s\n", header) != 0 ) return -1;

  time(&Timer.start);
  for ( count = 0;  count < FIRSTBUSLOOP; count++ ) {
    status = pf_rec_bus(&b, "F");
  }
  time(&Timer.stop);

  reportStats(&Timer, Stats, count, rpt_file, header);
  return 0;
}

int nextBusDelay(TimeStats *Stats)
{
  sprintf(header, "\n******* nextBusSpeed test\n");
  if ( get_pf_rec(&b,"F","Bus","First bus: %18.18s\n", header) != 0 ) return -1;

  time(&Timer.start);
  status = OK;
  for ( count = 0; (status == OK) && (count < 1630) ; count++ ) {
    status = pf_rec_bus(&b, "N");
  }
  time(&Timer.stop);

  if ( get_pf_rec(&b,"G","Bus","Last  bus: %18.18s\n", header) != 0 ) return -1;
  reportStats(&Timer, Stats, count, rpt_file, header);
  return 0;
}

int nextAreaDelay(TimeStats *Stats)
{
  int iter;
  sprintf(header, "\n******* nextAreaSpeed test\n");
  if ( get_pf_rec(&b,"F","Area","First area: %26.26s\n", header) !=0) return -1;

  count = 0;
  time(&Timer.start);
  status = OK;
  for ( iter = 0; iter < 100; iter++ ) {
  status = pf_rec_area(&b, "F"); count++;
    while ( status == OK ) {
        status = pf_rec_area(&b, "N");
	count++;
    }
  }
  time(&Timer.stop);

  if ( get_pf_rec(&b,"G","Area","Last  area: %26.26s\n", header) !=0) return -1;
  reportStats(&Timer, Stats, count, rpt_file, header);
  return 0;
}

int nextCbusDelay(TimeStats *Stats)
{
  int iter;
  sprintf(header, "\n******* nextCbusSpeed test\n");
  if ( get_pf_rec(&b,"F","Cbus","First Cbus: %18.18s\n", header) !=0) return -1;

  count = 0;
  time(&Timer.start);
  status = OK;
  for ( iter = 0; iter < 4; iter++ ) {
  status = pf_rec_cbus(&b, "F"); count++;
    while ( status == OK ) {
        status = pf_rec_cbus(&b, "N");
	count++;
    }
  }
  time(&Timer.stop);

  if ( get_pf_rec(&b,"G","Cbus","Last  Cbus: %18.18s\n", header) !=0) return -1;
  reportStats(&Timer, Stats, count, rpt_file, header);
  return 0;
}

int nextItieDelay(TimeStats *Stats)
{
  int iter;
  sprintf(header, "\n******* nextItieSpeed test\n");
  if ( get_pf_rec(&b,"F","Itie","First Itie: %24.24s\n", header) !=0) return -1;

  count = 0;
  time(&Timer.start);
  status = OK;
  for ( iter = 0; iter < 30; iter++ ) {
  status = pf_rec_itie(&b, "F"); count++;
    while ( status == OK ) {
        status = pf_rec_itie(&b, "N");
	count++;
    }
  }
  time(&Timer.stop);

  if ( get_pf_rec(&b,"G","Itie","Last  Itie: %24.24s\n", header) !=0) return -1;
  reportStats(&Timer, Stats, count, rpt_file, header);
  return 0;
}

int nextXdataDelay(TimeStats *Stats)
{
  int iter;
  sprintf(header, "\n******* nextXdataSpeed test\n");
  if ( get_pf_rec(&b,"F","Xdata","First Xdata: %32.32s\n", header) !=0) return -1;

  count = 0;
  time(&Timer.start);
  status = OK;
  for ( iter = 0; iter < 40; iter++ ) {
  status = pf_rec_xdata(&b, "F"); count++;
    while ( status == OK ) {
        status = pf_rec_xdata(&b, "N");
	count++;
    }
  }
  time(&Timer.stop);

  if ( get_pf_rec(&b,"G","Xdata","Last  Xdata: %32.32s\n", header) !=0) return -1;
  reportStats(&Timer, Stats, count, rpt_file, header);
  return 0;
}

int nextBranchDelay(TimeStats *Stats)
{
  sprintf(header, "\n******* nextBranchSpeed test\n");
  if ( get_pf_rec(&b,"F","Branch","First branch: %33.33s\n", header) != 0 )
    return -1;

  time(&Timer.start);
  status = OK;
  for ( count = 0; (status == OK) && (count < 1140) ; count++ ) {
    status = pf_rec_branch(&b, "N");
  }
  time(&Timer.stop);

  if ( get_pf_rec(&b,"G","Branch", "Last  branch: %33.33s\n", header) != 0 )
    return -1;
  reportStats(&Timer, Stats, count, rpt_file, header);
  return 0;
}

void printStats(TimeStats *Stats, FILE *fp, char *header)
{
  fprintf(fp, "%s", header);
  if (Stats->loops >= 100000)
    fprintf(fp, "%7.1e loops, ", (double) Stats->loops );
  else
    fprintf(fp, "%7d loops, ", Stats->loops);
  fprintf(fp, "%6.1f seconds, ", Stats->delay);
  fprintf(fp, "%9.3g loops/second, ", Stats->loopsPerSecond);
  fprintf(fp, "%6.3f mS per loop\n", 1000 * Stats->delayPerLoop);
}

void computeStats(StopWatch *Timer, TimeStats *Stats, int count)
{
  Stats->loops = count;
  Stats->delay = difftime(Timer->stop, Timer->start);
  if ( Stats->delay > 0 )
    Stats->loopsPerSecond = (double) ((double)Stats->loops / Stats->delay);
  else
    Stats->loopsPerSecond = -1;
  if ( Stats->loops > 0 )
    Stats->delayPerLoop  = (double) (Stats->delay / (double)Stats->loops); 
  else
    Stats->delayPerLoop = 0;
}

int query_pf(char *cmd, char *data) {
  char *line,*stat;
  err_buf[0]='\0';
  reply_pf[0]='\0';
  if (strlen(data) > 0) sprintf(pf_cflow_outbuf,"%s\n%s\n%s\n",cmd,data,Eom);
  else sprintf(pf_cflow_outbuf,"%s\n%s\n",cmd,Eom);
  if (cf_debug) {
    printf("************cflow_outbuf=\n%s************\n",pf_cflow_outbuf);
    fflush(stdout);
  }
  if (error=pf_cflow_ipc()) return error;
  if (cf_debug) {
    printf("************cflow_inbuf=\n%s************\n",pf_cflow_inbuf);
    fflush(stdout);
  }
  if ((stat=strstr(pf_cflow_inbuf,RtnStat)) != NULL) {
    ipf_status = (int)atol(stat + strlen(RtnStat));
  } else ipf_status = -1;
  line=strtok(pf_cflow_inbuf,SEPCHARS);
  while(line != NULL) {
    if (line[0] == '/') { /* echo of command */
    } else if ((line[0] == '*') || ((line[1] == '*') && (line[2] == '*'))
                                || ((line[1] == '*') && (line[2] == 'I'))) {
    /* check if line is error and if it is append to error buffer */
      strcat(err_buf,line);
      strcat(err_buf,"\n");
    } else {
      strcat(reply_pf,line); /* otherwise send reply back to cflow user */
      strcat(reply_pf,"\n");
    }
    line=strtok(NULL,SEPCHARS);
  }
  return ipf_status;
}

void reportStats(StopWatch *Timer, TimeStats *Stats, int count, FILE *rpt_file,
                 char *header)
{
  computeStats(Timer, Stats, count);
  printStats(Stats, rpt_file, header);
  printStats(Stats, stdout, header);
}

int get_pf_rec_(pf_rec *b, char *code, char *type, char *template, char *header)
{
  char temp[MAXLINE], b_data[MAXLINE];
  int  stat;
  if ( strcmp("F", code) == 0 ) {
    stat = (strcmp("Bus",type)==0) ? pf_rec_bus(b, "F") : pf_rec_branch(b, "F");
    if ( stat != 0 ) {
        fprintf(stderr, "%s not found\n", type);
        return -1;
    }
  }
  stat = (strcmp("Bus",type)==0) ? 
    pf_rec_b2a(b_data, b, "I") : pf_rec_b2a(b_data, b, "I");
  sprintf(temp, template, b_data);
  strcat(header, temp);
  return stat;
}

int get_pf_rec(pf_rec *b, char *code, char *type, char *template, char *header)
{
  char temp[MAXLINE], b_data[MAXLINE];
  int  stat;
  if ( strcmp("F", code) == 0 ) {
    if ( strcmp("Bus", type) == 0 ) {
        stat = pf_rec_bus(b, "F");
    }
    else if ( strcmp("Area", type) == 0 ) {
        stat = pf_rec_area(b, "F");
    }
    else if ( strcmp("Cbus", type) == 0 ) {
        stat = pf_rec_cbus(b, "F");
    }
    else if ( strcmp("Itie", type) == 0 ) {
        stat = pf_rec_itie(b, "F");
    }
    else if ( strcmp("Xdata", type) == 0 ) {
        stat = pf_rec_xdata(b, "F");
    }
    else if ( strcmp("Branch", type) == 0 ) {
        stat = pf_rec_branch(b, "F");
    }
    else {
        stat = -1;
    }
    if ( stat != 0 ) {
        fprintf(stderr, "%s not found\n", type);
        return -1;
    }
  }
  stat = pf_rec_b2a(b_data, b, "I");
  if ( stat != 0 ) {
    fprintf(stderr, " pf_rec_b2a error.\n");
    return -1;
  }
  sprintf(temp, template, b_data);
  strcat(header, temp);
  return stat;
}

void get_spec(char *spec, char *default_spec)
{
  char s[MAXLINE], *sp;
  if ( (sp = gets(s)) == NULL ) {
    strcpy(spec, default_spec);
    return;
  }
  while (isspace(*sp)) sp++;                /* find first non-space character */
  strcpy(spec, (*sp == '\0') ? default_spec : sp);
}
