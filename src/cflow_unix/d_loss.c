/******************************************************************************\
UTILITY:    D_LOSS
TYPE:       Powerflow (IPFSRV v. 209 or later) user_analysis.
SUMMARY:    Generates a table of losses versus parameter for a specified line.
RELATED:    DXLOSS, QVP
SEE ALSO:   IPF Advanced User's Guide, USER_ANALYSIS PCL Commands
            Randy's Plotter Commands and Notes
UPDATED:    February 16, 1995
LANGUAGE:   Standard C.  CFLOW Libraries. CF_UTIL.H.
DEVELOPER:  William D. Rogers, BPA, TEOH, 230-3806
REQUESTER:  Walt Powell, BPA, TEOS, 230-3234
USERS:
PURPOSE:    Generate a table of values showing Owner, Zone, Area, and System 
            losses versus selected branch parameter for a given base case, 
            minimum and maximum values, number of points, owner, zone, and area.
            Format the table for use with Randy's Plotter.
\******************************************************************************/

/* #includes */
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include "cflowlib.h"
#include "cf_util.h"
/* end #includes */

/* #defines */
#define BASE_SPEC "J01CY94.BSE"
#define RPT_SPEC  "D_LOSS.TABLE"
#define OWNER     "BPA"
#define ZONE      "ND"
#define AREA      "NORTHWEST"
#define PARAM     'X'
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
/* end of function prototypes */

main(int argc, char *argv[])
{
  int    steps;
  FILE   *rpt_file;
  float  pct, min_pct, max_pct, step_size, o1, z1, a1, s1, val, cur_val;
  char   user_def[1000], base_spec[FILENAME_MAX], rpt_spec[FILENAME_MAX];
  char   owner[4], area[11], zone[3], branch[MAX_INPUT], param;
  pf_rec r;

/* get file specs */
  printf("\n Default values shown in brackets [].\n");
  cf_sprompt("\n > Enter base case file spec [%-12.12s]: ",BASE_SPEC,base_spec);
  cf_sprompt("\n > Enter output file spec    [%-12.12s]: ", RPT_SPEC, rpt_spec);
/* get branch to work on */
  printf("\n > Enter branch (L, T, or TP) to modify (align to format guide):");
  printf("\nTY OWN< BUS1 ><V1> < BUS2 ><V2>CS\n");
  gets(branch);
  pf_rec_a2b(branch, &r, "I");
/* enter parameter to vary */
  cf_kprompt("\n > Enter parameter to vary (R X G B) [%c]: ", PARAM, &param);
  param = toupper(param);
/* get parameter variation info */
  printf(  " > Enter parameter variation relative to normal value (1.0=100%%)");
  cf_fprompt("\n   Minimum [%4.2f]: ", min_pct = MIN_PCT, &min_pct);
  cf_fprompt("\n   Maximum [%4.2f]: ", max_pct = MAX_PCT, &max_pct);
  cf_iprompt("\n   Points  [%4.0d]: ", steps = STEPS, &steps);
  cf_exit( (--steps < 0), " illegal number of steps: %d\n", steps); 
  step_size = steps ? (max_pct - min_pct)/(steps) : 0;
/* get owner, zone, and area */
  cf_sprompt("\n > Enter owner name for loss report [%-3.3s]: ", OWNER, owner);
  cf_sprompt("\n > Enter zone  name for loss report [%-2.2s]: ",  ZONE, zone);
  cf_sprompt("\n > Enter area  name for loss report [%-10.10s]: ",AREA, area);

/* initialize cflow connection to powerflow */
  pf_cflow_init(argc, argv);

  rpt_file = fopen(rpt_spec, "w");
  cf_pfexit( (rpt_file==NULL) , " Cannot open output file: %s\n", rpt_spec);

  cf_eprint(rpt_file, "!\n{ Loss by Category Versus Branch Parameter\n");
  cf_eprint(rpt_file, "{ %s\n", branch);
  cf_eprint(rpt_file, "{ %s\n", base_spec);
  cf_eprint(rpt_file, "{ OWNER = %s, ZONE = %s, AREA = %s\n",owner, zone, area);
  cf_eprint(rpt_file, "!\nTIMEBASE=\"%c (Per Unit)\"\n", param);
  cf_eprint(rpt_file, "! Number of signals:\n6\n!\n");
  cf_eprint(rpt_file, "BRANCH_%c(PU)  PCT_NOM(%%)", param);
  cf_eprint(rpt_file, "   OWNER(MW)    ZONE(MW)    AREA(MW)  SYSTEM(MW)\n");

  sprintf(user_def,   "> DEFINE_TYPE  OWNER_LOSS, LET O1 = %s\n", owner);
  cf_aprint(user_def, "> DEFINE_TYPE   ZONE_LOSS, LET Z1 = %s\n", zone);
  cf_aprint(user_def, "> DEFINE_TYPE   AREA_LOSS, LET A1 = %s\n", area);
  cf_aprint(user_def, "> DEFINE_TYPE SYSTEM_LOSS, LET S1 = SYSTEM\n");
  cf_aprint(user_def, "C    $O1/F8.2    $Z1/F8.2    $A1/F8.2    $S1/F8.2\n");

  cf_pfexit( pf_init() , " pf_init failed.\n");           /* get a flat start */
  cf_pfexit( pf_load_oldbase(base_spec) , " pf_load_oldbase failed.\n");
  cf_pfexit( pf_rec_branch(&r, "G") , " pf_rec_branch failed: G\n");
  switch (param) {
    case 'R': val = r.i.branch.r; break;
    case 'G': val = r.i.branch.g; break;
    case 'B': val = r.i.branch.b; break;
    default : val = r.i.branch.x; break;
  }
  for (pct = min_pct; steps-- >= 0; pct += step_size) {
    cur_val = val * pct;
    switch (param) {
        case 'R': r.i.branch.r = cur_val; break;
        case 'G': r.i.branch.g = cur_val; break;
        case 'B': r.i.branch.b = cur_val; break;
        default : r.i.branch.x = cur_val; break;
    }
    cf_pfexit( pf_rec_branch(&r, "M") , " pf_rec_branch failed: M\n");
    cf_pfexit( pf_solution() , " pf_solution failed.");
    pf_user_init_def();
    pf_user_load_def(user_def);
    pf_user_sub_def("base");
    sscanf(reply_pf, "%f %f %f %f", &o1, &z1, &a1, &s1);
    cf_eprint(rpt_file,
        "%12.6f%12.2f%12.2f%12.2f%12.2f%12.2f\n",cur_val,100*pct,o1,z1,a1,s1);
    cf_pfexit( pf_load_oldbase(base_spec) , " pf_load_oldbase failed.\n");
  }
  fclose(rpt_file);
  printf("\n Report written to file: %s\n", rpt_spec);
  pf_cflow_exit();
}
