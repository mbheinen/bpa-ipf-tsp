/******************************************************************************\
UTILITY:    QVP
TYPE:       Powerflow (IPFSRV version 209 or later) user_analysis.  
SUMMARY:    Generates a table of quantity versus parameter for a given branch.
RELATED:    DXLOSS, D_LOSS
SEE ALSO:   IPF Advanced User's Guide, USER_ANALYSIS PCL Commands
            Randy's Plotter Commands and Notes
UPDATED:    February 16, 1995
LANGUAGE:   Standard C.  CFLOW Libraries.  cf_util.h functions.
DEVELOPER:  William D. Rogers, BPA, TEOH, 230-3806
REQUESTER:  Walt Powell, BPA, TEOS, 230-3234
PURPOSE:    Generate a table of values of any user_analysis quantity as a 
            function of a selected branch parameter as it varies over a 
            specified range.  Format the table for use with  Randy's Plotter.
\******************************************************************************/

/* #includes */
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "cflowlib.h"
#include "cf_util.h"
/* end #includes */

/* #defines */
#define BASE_SPEC "J01CY94.BSE"
#define RPT_SPEC  "QVP.TABLE"
#define PARAM     'X'
#define MAX_INPUT 132
#define MIN_PCT   0.5
#define MAX_PCT   1.0
#define STEPS     10
/* end #defines */

/* typedefs */ /* end typedefs */
/* declare global variables */ /* end declare global variables */
/* function prototypes */ /* end of function prototypes */

main(int argc, char *argv[])
{
  int    steps, fpad, spad;
  FILE   *rpt_file;
  float  pct, min_pct, max_pct, step_size, q1, cur_val, nom_val;
  char   base_spec[FILENAME_MAX], rpt_spec[FILENAME_MAX];
  char   branch[MAX_INPUT], param, col1[13], col2[13];
  char   qtype[32], isufx[8], qid[128], *cp;
  pf_rec r, b;

/* get file specs */
  printf("\n Default values shown in brackets [].\n");
  cf_sprompt("\n > Enter base case file spec [%-12.12s]: ",BASE_SPEC,base_spec);
  cf_sprompt("\n > Enter output file spec    [%-12.12s]: ", RPT_SPEC, rpt_spec);
/* get branch to work on */
  printf("\n > Enter branch (E L T TP) to modify (align to format guide):\n");
  cf_nprompt("TY OWN< BUS1 ><V1> < BUS2 ><V2>CS\n", "", 33, branch);
  cf_stoupper(branch);
  pf_rec_a2b(branch, &r, "I");
/* enter parameter to vary */
  printf("\n Choose a parameter to vary appropriate for the branch type:\n");
  printf("  [R] RESISTANCE [X] REACTANCE  [G] ADMITTANCE [B] SUSCEPTANCE\n");
  printf("  [M] MILES      [1] TAP 1      [2] TAP 2      [P] PHASE SHIFT\n");
  printf("  [3] G @ BUS1   [4] B @ BUS1   [5] G @ BUS1   [6] B @ BUS2   \n");
  cf_kprompt("\n > Enter parameter designator [%c]: ", PARAM, &param);
  param = toupper(param);
/* get parameter variation info */
  printf(  " > Enter parameter variation relative to normal value (1.0=100%%)");
  cf_fprompt("\n   Minimum [%4.2f]: ", min_pct = MIN_PCT, &min_pct);
  cf_fprompt("\n   Maximum [%4.2f]: ", max_pct = MAX_PCT, &max_pct);
  cf_iprompt("\n   Points  [%4.0d]: ", steps = STEPS, &steps);
  cf_exit( (--steps < 0), " illegal number of steps: %d\n", steps); 
  step_size = steps ? (max_pct - min_pct)/(steps) : 0;
/* get quantity type, index suffix, and quantity id */
  printf("\n\n Specify a user_analysis quantity to monitor.");
  cf_sprompt("\n > Enter quantity type: ", "", qtype);
  cf_stoupper(qtype);
  if (strstr(qtype, "INDEX") != NULL) {
    cf_sprompt("\n > Enter index suffix : ", "", isufx);
    cf_stoupper(isufx);
  }
  else isufx[0] = '\0';
  cf_nprompt("\n > Enter quantity id  : ", "", 50, qid);
  cf_stoupper(qid);
  spad = 12 - (strlen(qtype) + strlen(isufx)); if (spad<0) spad=0;
  fpad = (strlen(qtype) + strlen(isufx)) - 12; if (fpad<0) fpad=0;
  printf("\n\n");
/* initialize cflow connection to powerflow */
  pf_cflow_init(argc, argv);

  rpt_file = fopen(rpt_spec, "w");
  cf_pfexit( (rpt_file==NULL) , " Cannot open output file: %s\n", rpt_spec);
  switch (param) {
    case 'R': strcpy(col1,"R(PU)");       strcpy(col2, "R(%NOM)");       break;
    case 'X': strcpy(col1,"X(PU)");       strcpy(col2, "X(%NOM)");       break;
    case 'G': strcpy(col1,"G(PU)");       strcpy(col2, "G(%NOM)");       break;
    case 'B': strcpy(col1,"B(PU)");       strcpy(col2, "B(%NOM)");       break;
    case 'M': strcpy(col1,"LENGTH(MI)");  strcpy(col2, "LENGTH(%NOM)");  break;
    case '1': strcpy(col1,"TAP1(KV)");    strcpy(col2, "TAP1(%NOM)");    break;
    case '2': strcpy(col1,"TAP2(KV)");    strcpy(col2, "TAP2(%NOM)");    break;
    case 'P': strcpy(col1,"PHAS_SH(DEG)");strcpy(col2, "PHAS_S(%NOM)");  break;
    case '3': strcpy(col1,"G1(PU)");      strcpy(col2, "G1(%NOM)");      break;
    case '4': strcpy(col1,"B1(PU)");      strcpy(col2, "B1(%NOM)");      break;
    case '5': strcpy(col1,"G2(PU)");      strcpy(col2, "G2(%NOM)");      break;
    case '6': strcpy(col1,"B2(PU)");      strcpy(col2, "B2(%NOM)");      break;
    default : strcpy(col1,"X(PU)");       strcpy(col2, "X(%NOM)");       break;
  }
  cf_eprint(rpt_file, "!\n{ User_analysis Quantity Versus Branch Parameter\n");
  cf_eprint(rpt_file, "{ %s%s Versus %s\n", qtype, isufx, col1);
  cf_eprint(rpt_file, "{ Quantity ID: %s\n", qid);
  cf_eprint(rpt_file, "{ Branch   ID: %s\n", branch);
  cf_eprint(rpt_file, "{ %s\n", base_spec);
  cf_eprint(rpt_file, "!\nTIMEBASE=\"%s\"\n", col1);
  cf_eprint(rpt_file, "! Number of signals:\n3\n!\n");
  cf_eprint(rpt_file, "%12.12s  %12.12s", col1, col2);
  cf_eprint(rpt_file, "  %*s%s%s\n", spad, "", qtype, isufx);

  cf_pfexit( pf_load_oldbase(base_spec) , " pf_load_oldbase failed.\n");
  cf_pfexit( pf_rec_branch(&r, "G") , " pf_rec_branch failed: G\n");
  switch (param) {
    case 'R': nom_val = (r.i.branch.r); break;
    case 'X': nom_val = (r.i.branch.x); break;
    case 'G': nom_val = (r.i.branch.g); break;
    case 'B': nom_val = (r.i.branch.b); break;
    case 'M': nom_val = (r.i.L.miles); break;
    case '1': nom_val = (r.i.T.tap1); break;
    case '2': nom_val = (r.i.T.tap2); break;
    case 'P': nom_val = (r.i.TP.phase_shift_deg); break;
    case '3': nom_val = (r.i.E.g1); break;
    case '4': nom_val = (r.i.E.b1); break;
    case '5': nom_val = (r.i.E.g2); break;
    case '6': nom_val = (r.i.E.b2); break;
    default : nom_val = (r.i.branch.x); break;
  }
  for (pct = min_pct; steps-- >= 0; pct += step_size) {
    cur_val = pct * nom_val;
    switch (param) {
        case 'R': r.i.branch.r = cur_val; break;
        case 'X': r.i.branch.x = cur_val; break;
        case 'G': r.i.branch.g = cur_val; break;
        case 'B': r.i.branch.b = cur_val; break;
        case 'M': r.i.L.miles = cur_val; break;
        case '1': r.i.T.tap1 = cur_val; break;
        case '2': r.i.T.tap2 = cur_val; break;
        case 'P': r.i.TP.phase_shift_deg = cur_val; break;
        case '3': r.i.E.g1 = cur_val; break;
        case '4': r.i.E.b1 = cur_val; break;
        case '5': r.i.E.g2 = cur_val; break;
        case '6': r.i.E.b2 = cur_val; break;
        default : r.i.branch.x = cur_val; break;
    }
    cf_pfexit( pf_rec_branch(&r, "M") , " pf_rec_branch failed: M\n");
    cf_pfexit( pf_solution() , " pf_solution failed.");

    pf_user_init_def();
    pf_user_define("Q1", qid, qtype);
    pf_user_comment("Q1", isufx, "/F15.7");
    pf_user_sub_def("base");
    pf_user_quantity("Q1", isufx, &q1);
    cf_eprint(rpt_file, "%12.6f  %12.2f", cur_val, pct*100);
    cf_eprint(rpt_file, "  %*s%12.4f\n", fpad, "", q1);
  }
  fclose(rpt_file);
  printf("\n Report written to file: %s\n", rpt_spec);
  pf_cflow_exit();
}
