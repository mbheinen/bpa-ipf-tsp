/******************************************************************************\
UTILITY:    DXLOSS
TYPE:       Powerflow (IPFSRV) user_analysis.
SUMMARY:    Generates a table of losses versus reactance for a specified line.
SEE ALSO:   IPF Advanced User's Guide, USER_ANALYSIS PCL Commands
            Randy's Plotter Commands and Notes
UPDATED:    January 18, 1995
LANGUAGE:   Standard C.  CFLOW Libraries.
DEVELOPER:  William D. Rogers, BPA, TEOH, 230-3806
REQUESTER:  Walt Powell, BPA, TEOS, 230-3234
PURPOSE:
Generate a table of values showing Owner, Zone, Area, and System losses versus
the reactance of a branch for a given basecase, minimum and maximum values,
number of points, owner, zone, and area.  Format the table for use with Randy's
Plotter.
\******************************************************************************/

/* #includes */
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include "cflowlib.h"
/* end #includes */

/* #defines */
#define BASE_SPEC "J01CY94.BSE"
#define RPT_SPEC  "DXLOSS.TABLE"
#define OWNER     "BPA"
#define ZONE      "ND"
#define AREA      "NORTHWEST"
#define MAX_INPUT 132
#define MIN_PCT   0.5
#define MAX_PCT   1.0
#define STEPS     10
/* end #defines */

/* typedefs */
/* end typedefs */

/* declare global variables */
/* end declare global variables */

/* function prototypes */
void get_spec(char *spec, char *default_spec);
void cf_exit();
/* end of function prototypes */

main(int argc, char *argv[])
{
  int    rtn_cd, steps, conv;
  FILE   *rpt_file;
  float  pct, min_pct, max_pct, step_size, o1, z1, a1, s1, x;
  char   user_def[1000], base_spec[FILENAME_MAX], rpt_spec[FILENAME_MAX];
  char   own[4], area[11], zone[3], out[132], temp_def[132];
  pf_rec r;
  char   input[MAX_INPUT], branch[MAX_INPUT];
  char   type[3], owner[4], bus1[9], bus2[9], ckt_id;
  int    section = 0;
  float  kv1, kv2;

/* get file specs */
  printf("\n Default values shown in brackets [].\n");
  printf("\n > Enter base case file spec [%-12.12s]: ", BASE_SPEC);
  get_spec(base_spec, BASE_SPEC);
  printf("\n > Enter output file spec    [%-12.12s]: ", RPT_SPEC);
  get_spec(rpt_spec, RPT_SPEC);
/* get branch to work on */
  printf("\n > Enter branch to modify (align to format guide):");
  printf("\nTY OWN< BUS1 ><V1> < BUS2 ><V2>CS\n");
  gets(branch); strcat(branch,"                                 ");
/* parse branch identifying info */
  conv = sscanf(branch, "%2[^\n]%*c%3[^\n]%8[^\n]%f%*c%8[^\n]%f%c%d",
    type, owner, bus1, &kv1, bus2, &kv2, &ckt_id, &section);
  if ( conv < 7 ) {
    printf("Incomplete branch specification.\n");
    exit(EXIT_FAILURE);
  }
  pf_init_branch(&r, type, bus1, kv1, bus2, kv2, ckt_id, section);
/* get parameter variation info */
  printf(  " > Enter parameter variation relative to normal value (1.0=100%%)");
  printf("\n   Minimum [%4.2f]: ", min_pct = MIN_PCT);
  gets(input); sscanf(input, "%f", &min_pct);
  printf("\n   Maximum [%4.2f]: ", max_pct = MAX_PCT);
  gets(input); sscanf(input, "%f", &max_pct);
  printf("\n   Points  [%4.0d]: ", steps = STEPS);
  gets(input); sscanf(input, "%d", &steps);
  if ( (steps-1) < 0 ) exit(0);
  if ( (steps-1) == 0) { step_size = 0; }
  else { step_size = (max_pct - min_pct)/(steps-1); }
/* get owner, zone, and area */
  printf("\n > Enter owner name for loss report [%-3.3s]: ", OWNER);
  get_spec(own, OWNER);
  printf("\n > Enter zone  name for loss report [%-2.2s]: ", ZONE);
  get_spec(zone, ZONE);
  printf("\n > Enter area  name for loss report [%-10.10s]: ", AREA);
  get_spec(area, AREA);

/* initialize cflow connection to powerflow */
  pf_cflow_init(argc, argv);

  if ( (rpt_file = fopen(rpt_spec, "w")) == NULL ) {
    fprintf(stderr, " Cannot open output file: %s\n", rpt_spec);
    cf_exit();
  }

  sprintf(out, "!\n{ Loss by Category Versus Line Reactance\n{ %s\n", branch);
  fprintf(rpt_file, "%s", out); fprintf(stdout, "%s", out);
  sprintf(out, "{ %s\n", base_spec);
  fprintf(rpt_file, "%s", out); fprintf(stdout, "%s", out);
  sprintf(out, "{ OWNER = %s, ZONE = %s, AREA = %s\n", own, zone, area);
  fprintf(rpt_file, "%s", out); fprintf(stdout, "%s", out);
  sprintf(out, "!\nTIMEBASE=\"Reactance (Per Unit)\"\n");
  fprintf(rpt_file, "%s", out); fprintf(stdout, "%s", out);
  sprintf(out, "! Number of signals:\n6\n!\n", branch);
  fprintf(rpt_file, "%s", out); fprintf(stdout, "%s", out); 
  sprintf(out,
 "BRANCH_X(PU)  PCT_NOM(%%)   OWNER(MW)    ZONE(MW)    AREA(MW)  SYSTEM(MW)\n");
  fprintf(rpt_file, "%s", out); fprintf(stdout, "%s", out); 

  sprintf(user_def, "> DEFINE_TYPE  OWNER_LOSS, LET O1 = %s\n", own);
  sprintf(temp_def, "> DEFINE_TYPE   ZONE_LOSS, LET Z1 = %s\n", zone);
  strcat(user_def, temp_def);
  sprintf(temp_def, "> DEFINE_TYPE   AREA_LOSS, LET A1 = %s\n", area);
  strcat(user_def, temp_def);
  strcat(user_def,  "> DEFINE_TYPE SYSTEM_LOSS, LET S1 = SYSTEM\n");
  strcat(user_def,"C    $O1/F8.2    $Z1/F8.2    $A1/F8.2    $S1/F8.2\n");

  if ( pf_init() != 0 ) cf_exit();                        /* get a flat start */
  if ( pf_load_oldbase(base_spec)!=0 ) cf_exit();
  if ( pf_rec_branch(&r, "G") != 0 ) cf_exit();
  x = r.i.branch.x;

  for (pct = min_pct; steps-- > 0; pct += step_size) {
    r.i.branch.x = x * pct;
    if ( pf_rec_branch(&r, "M") != 0 ) cf_exit();
    if ( pf_solution() != 0 ) cf_exit();
    pf_user_init_def();
    pf_user_load_def(user_def);
    pf_user_sub_def("base");
    sscanf(reply_pf, "%f %f %f %f", &o1, &z1, &a1, &s1);
    sprintf(out, "%12.6f%12.2f%12.2f%12.2f%12.2f%12.2f\n",r.i.branch.x, 100*pct,
        o1, z1, a1, s1);
    fprintf(rpt_file, "%s", out); fprintf(stdout, "%s", out); 
    if ( pf_load_oldbase(base_spec) != 0) cf_exit();
  }
  fclose(rpt_file);
  printf("\n Report written to file: %s\n", rpt_spec);
  pf_cflow_exit();
}

void get_spec(char *spec, char *default_spec)
{
  char s[MAX_INPUT], *sp;
  if ( (sp = gets(s)) == NULL ) {
    strcpy(spec, default_spec);
    return;
  }
  while (isspace(*sp)) sp++;                /* find first non-space character */
  strcpy(spec, (*sp == '\0') ? default_spec : sp);
}

void cf_exit()
{
  pf_cflow_exit();
  exit(0);
}
