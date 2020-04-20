 /* increm.c
 This CFLOW procedure creates an incremental line flow listing of
 selected lines, sorted by areas. The incremental flows are computed
 as the change in flows from the base case to the incremental case.
 Several incremental cases can be submitted. The limit here is 10,
 compared with 64 for the INCREM COPE procedure for IPS. However, this
 limit can be extended with attendent changes in the incremental
 storage arrays and in the output reports.
 Two aspects makes this CFLOW procedure more complicated than the COPE
 equivalent.
 1. IPF uses area intertie "I" records to define the net area export.
 If these records are present, an interarea transfer is effected
 only by changing the scheduled interarea export. If "I" records exist
 but the particular Area1-Area2 "I" record does not exist, then a
 new "I" record must be added for Area1-Area2 with an export value
 of the desired transfer. If no "I" records exist, then the interarea
 transfer is effected by the ordinary means, namely, by increasing the
 area1 export and decreasing the area2 export.
 2. Process INCREM does not exist in IPF. Consequently, the sorting
 and listing of branches was implemented entirely within this CFLOW
 procedure.
 The code is intentionally batch. It could be made interactive by
 prompting for file names and bus names. All file names and bus names
 are hard coded. Changing these requires re-editing, recompiling, and
 relinking the program. Fortunately, these steps can be performed in
 a short time.
 The program's execution plan is as follows.
 1. Load in base case history file.
 2. Open branch data file, bus data file, and output report file.
 3. Process the branch data file. For each branch, obtain the base case
 line flow.
 4. Process the bus data file to identify each transfer pair of buses:
 "busname1" and "busname2". The "busname1" is a hard-coded global
 variable.
 a. For each bus pair, perturb busname1's generation +100MW (and its
 associated areaname1's export +100MW) and busname2's generation
 -100MW (and its associated areaname2 export -100MW).
 b. Solve the case.
 c. Loop through the monitored branches, obtaining the line flows
 for the perturbed case.
 5. Print the output report.
 a. Use a branch index array "keysrt" in conjunction with a
 user-written compare routine (to be used with qsort) to obtain a
 double-entry list of monitored branches sorted by the following
 fields: area1, bus1, area2, bus2, id, and section.
 b. Print out the monitored lines flows using the sort index.
 */
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "../cflow/cflowlib.h"
#include "../cflow/ft.h"
#define NULLC '\0'
#define MAXFLOWS 1000
#define MAXCASES 10
#define TRUE 1
#define FALSE 0
#define LINES_PER_PAGE 64
/* Declare global variables */
struct FLOWS {
 char bus1[13]; /* bus1, base1 field */
 char bus2[13]; /* bus2, base2 field */
 char id; /* id field */
 int section; /* section field */
 char area1[11]; /* area name corresponding to bus1 */
 char area2[11]; /* area name corresponding to bus2 */
 float Pin[MAXCASES+1]; /* Tie line flows in MWs */
 float Pout[MAXCASES+1]; /* Tie line flows in MWs */
 /* Declare hard-coded files and bus names */
char *progname = "increm";
char *basefile = "../cflow_progs/98hs3inc.bse";
char *branchfile = "../cflow_progs/line.dat";
char *busname1 = "GADSBY 313.8";
char *busfile = "../cflow_progs/name2.dat";
char *outfile = "98hs3inc.out";
/* declare function prototypes */
FILE *efopen (char *, char *);
int compare( const void *, const void * );
/* static int compare( const void *key1, const void *key2 ); */
int increment (char *, char *, float, int);
int get_bus_area (char *, char *);
int mod_bus_gen (char *, float);
int mod_area_export (char *, float);
int mod_itie_export (char *, char *, float);
void title (int *, int *, FILE *, pf_comments *, int, char *);
FILE *efopen (char *file, char *mode) /* fopen file, return message */
 /* if can't */
{
 FILE *fp;
 if ((fp = fopen(file, mode)) != NULL)
 return fp;
 fprintf (stderr, " %s: can't open file %s mode %s\n",
 progname, file, mode);
 return NULL;
}
main(int argc, char *argv[])
{
 pf_comments c;
 pf_rec br, itie;
 int i, j, k, status, numcases, keysrt[2*MAXFLOWS], numbr, lineno = 0,
 pageno = 0, intertie_flag = FALSE, section;
 FILE *fp_busfile, *fp_branchfile, *fp_outfile;
 char id, base[5], busname2[13], oldarea[11], string[133], areaname1[11];
 pf_cflow_init( argc, argv);
 /* Load the history data file */
 status = pf_load_oldbase (basefile);
 if (status) {
 fprintf (stderr, " Unable to open history file %s status %d",
 basefile, status);
 pf_cflow_exit();
 return 1;
 }
 /* Determine if any Area Intertie "I" records exist in the base case */
 pf_rec_init (&itie, PF_REC); /* Initialize structure prior
 to calling pf_rec_itie.
 PF_REC is in header file "ft.h" */
 status = pf_rec_itie (&itie, "F");
 if (status == 0) intertie_flag = TRUE; /* If success, "I" records exist */
 /* Obtain case comments */
 pf_rec_init (&c, PF_REC); /* Initialize structure prior
 to calling pf_rec_comments.
 PF_REC is in header file "ft.h" */
 status = pf_rec_comments (&c, "G");
 /* Open the monitored branch data file for read only */
 fp_branchfile = efopen(branchfile, "r");
 if (fp_branchfile == NULL) {
 fprintf (stderr, " Unable to open monitored branch file %s \n",
 branchfile);
 pf_cflow_exit();
 return 1;
 }
 /* Open the bus data file for read only */
 fp_busfile = efopen(busfile, "r");
 if (fp_busfile == NULL) {
 fprintf (stderr, " Unable to open buses list file %s \n",
 busfile);
 pf_cflow_exit();
 return 1;
 }
 /* Open the output report file for write only */
 fp_outfile = efopen(outfile, "w");
 if (fp_outfile == NULL) {
 fprintf (stderr, " Unable to open INCREM output report file %s \n",
 outfile);
 pf_cflow_exit();
 return 1;
 }
 fprintf (fp_outfile, " Base case from history file %s \n", basefile);
 for (i=0; i < 3; i++) {
 if (strlen (c.h[i]) > 0)
 fprintf (fp_outfile, " %s \n", &c.h[i][1]);
 }
 /*
 Process each record in the monitored branch file. The procedure invokes
 the following steps.
 1. Parse the ID fields of each branch entity into the branch data
 structure.
2. A call to routine "pf_rec_branch" with appropriate arguments will
 obtain the branch output quantitys Pin and Pout.
 3. Parse the ID fields again of each branch entity into arrays used
 for sorting and printing the output report. The "area1" and "area2"
 arrays are obtained indirectly through the zone. First the zone is
 obtained from the bus data structure of each terminal bus. From each
 zone, the corresponding the area name is obtained via the call
 "pf_area_of_zone".
 4. An slightly unusual convention is employed here for subscrip "numbr".
 Normally, a C-arrays begins with subscript "0". Here, it begins with
 subscript "1". The reason is that it is necessary to distinquish
 normal branches from transposed branches in the "keysrt" array, which
 will be built after all lines have been read in and all incremental
 cases have been processed. The distinction is done by positive
 and negative subscripts: +n denotes the normal orientation, -n its
 transpose. (C cannot distinquish between +0 and -0.)
 */
 fprintf (stderr, " Reading in monitored line data \n");
 numbr = 0;
 while ( fgets (string, 132, fp_branchfile) != NULL) {
 /*
 Parse the branch data id fields from "string" into structure
 "br.i.branch" and call "pf_rec_branch" with the "O" option to
 retrieve the base case flow.
 Copy the parsed branch data id fields into a second structure
 "br_flow[]" for storing the flow results.
 Note that the strncpy command requires that the strings be
 explicitly null-terminated.
 */
 pf_rec_init (&br, PF_REC); /* Initialize structure prior
 to calling pf_rec_branch.
 PF_REC is in header file "ft.h" */
 strcpy (br.i.branch.type, "L ");
 strncpy (br.i.branch.bus1_name, &string[6], 8);
 br.i.branch.bus1_name[8] = NULLC;
 strncpy (base, &string[14], 4);
 base[4] = NULLC;
 br.i.branch.bus1_kv = atof (base);
 strncpy (br.i.branch.bus2_name, &string[19], 8);
 br.i.branch.bus2_name[8] = NULLC;
 strncpy (base, &string[27], 4);
 base[4] = NULLC;
 br.i.branch.bus2_kv = atof (base);
 id = string[31];
 if (id == '\n' || id == '\t' || id == '\0') id = ' ';
 br.i.branch.ckt_id = id;
 if (strlen (string) > 32) {
 base[0] = string[32];
 base[1] = NULLC;
 section = atoi (base);
 } else {
 section = 0;
 }
 br.i.branch.section = section;
 /*
 The branch data is now loaded in structure "branch". A call to
 "pf_rec_branch" will retrieve the desired information.
 */
 status = pf_rec_branch (&br, "O");
 if (!status) {
 numbr++;
 strcpy (br_flow[numbr].bus1, &string[6]);
 br_flow[numbr].bus1[12] = NULLC;
 strcpy (br_flow[numbr].bus2, &string[19]);
 br_flow[numbr].bus2[12] = NULLC;
 br_flow[numbr].id = br.i.branch.ckt_id;
 br_flow[numbr].section = br.i.branch.section;
 br_flow[numbr].Pin[0] = br.s.branch.Pin;
 br_flow[numbr].Pout[0] = br.s.branch.Pout;
 /* Get the area name associated with bus1 indirectly through the
 zone of bus1 */
 status = get_bus_area (br_flow[numbr].bus1, br_flow[numbr].area1);
 if (status) {
 fprintf (stderr, " Unable to retrieve area associated with bus1 of \
monitored branch %s %s \n", br_flow[numbr].bus1, br_flow[numbr].bus2);
pf_cflow_exit();
return 1;
 }
 /* Get the area name associated with bus2 indirectly through the
 zone of bus2 */
 status = get_bus_area (br_flow[numbr].bus2, br_flow[numbr].area2);
 if (status) {
 fprintf (stderr, " Unable to retrieve area associated with bus2 of \
monitored branch %s %s \n", br_flow[numbr].bus1, br_flow[numbr].bus2);
pf_cflow_exit();
return 1;
 }
 }
 }
 /* Begin the incremental bus loop. This is the list of buses in
 testdc3.bdat, processed one-by-one. */
 while ( fgets (string, 80, fp_busfile) != NULL) {
 strncpy (busname2, string, 12);
 busname2[12] = NULLC;
 status = increment (busname1, busname2, 100.0, intertie_flag);
 if (!status) {
     numcases++;
 /* Solve the modified case */
 status = pf_solution ();
 /* Write change case comments */
 if (status) {
fprintf (fp_outfile, "\n Incremental case number %d failed \n",
 numcases);
fprintf (fp_outfile, " 100 MW scheduled from \"%s\" to \"%s\"\n",
 busname1, busname2);
fprintf (stderr, " Incremental case number %d failed \n",
 numcases);
for (i = 1; i <= numbr; i++) {
 br_flow[i].Pin[numcases] = 0.0;
 br_flow[i].Pout[numcases] = 0.0;
 }
 } else {
fprintf (fp_outfile, "\n Incremental case number %d \n", numcases);
fprintf (fp_outfile, " 100 MW scheduled from \"%s\" to \"%s\"\n",
 busname1, busname2);
fprintf (stderr, "\n Incremental case number %d \n", numcases);
fprintf (stderr, " 100 MW scheduled from \"%s\" to \"%s\"\n",
 busname1, busname2);
for (i = 1; i <= numbr; i++) {
 pf_rec_init (&br, PF_REC); /* Initialize structure prior
 to calling pf_rec_branch.
 PF_REC is in header file "ft.h" */
 strcpy (br.i.branch.type, "L ");
 strncpy (br.i.branch.bus1_name, br_flow[i].bus1, 8);
 br.i.branch.bus1_name[8] = NULLC;
 br.i.branch.bus1_kv = atof (&br_flow[i].bus1[8]);
 strncpy (br.i.branch.bus2_name, br_flow[i].bus2, 8);
 br.i.branch.bus2_name[8] = NULLC;
 br.i.branch.bus2_kv = atof (&br_flow[i].bus2[8]);
 br.i.branch.ckt_id = br_flow[i].id;
 status = pf_rec_branch (&br, "O");
 br_flow[i].Pin[numcases] = br.s.branch.Pin;
 br_flow[i].Pout[numcases] = br.s.branch.Pout;
}
 }
 }
 }
 /* Obtain sorted double entry index "keysrt" for branch flows
 keysrt[i] > 0 means "br_flow" is processed as is
 < 0 means "br_flow" is processed transposed
 */
 for (i = 0; i < numbr; i++) {
 keysrt[2*i] = i + 1;
 keysrt[2*i+1] = -keysrt[2*i];
 }
 qsort ( keysrt, 2*numbr, sizeof (keysrt[0]), compare );
 title (&lineno, &pageno, fp_outfile, &c, numcases, "");
 oldarea[0] = NULLC;
 for (i = 0; i < 2*numbr; i++) {
 j = keysrt[i];
 if (j > 0) {
 strcpy (areaname1, br_flow[j].area1);
 } else {
 strcpy (areaname1, br_flow[-j].area2);
 }
 if (strcmp (areaname1, oldarea) != 0) {
 if (lineno+4 > LINES_PER_PAGE) {
 lineno = 0;
 title (&lineno, &pageno, fp_outfile, &c, numcases, areaname1);
 } else {
 fprintf (fp_outfile, "\n From area %s \n\n", areaname1);
 lineno += 3;
 }
 strcpy (oldarea, areaname1);
 }
 if (lineno+1 > LINES_PER_PAGE) {
 lineno = 0;
 title (&lineno, &pageno, fp_outfile, &c, numcases, areaname1);
 }
 if (j > 0) {
 fprintf (fp_outfile, " %s %s %c %s %8.2f ", br_flow[j].bus1,
br_flow[j].bus2, br_flow[j].id, br_flow[j].area2,
br_flow[j].Pin[0]);
 for (k = 1; k <= numcases; k++) {
fprintf (fp_outfile, " %7.2f", br_flow[j].Pin[k] - br_flow[j].Pin[0]);
 }
 } else {
 fprintf (fp_outfile, " %s %s %c %s %8.2f ", br_flow[-j].bus2,
br_flow[-j].bus1, br_flow[-j].id, br_flow[-j].area1,
-br_flow[-j].Pout[0]);
 for (k = 1; k <= numcases; k++) {
fprintf (fp_outfile, " %7.2f",
 -br_flow[-j].Pout[k] + br_flow[-j].Pout[0]);
 }
 }
 fprintf (fp_outfile, "\n");
 lineno++;
 }
 pf_cflow_exit();
 fclose (fp_busfile);
 fclose (fp_branchfile);
 fclose (fp_outfile);
 if (!status) {
 exit (0);
 } else {
 fprintf (stderr, " %s aborted with errors \n", progname);
  exit (1);
 }
}
int get_bus_area (char *name, char *area)
/* This routine returns the area name given the bus name. It is obtained
 indirectly through the bus bus data structure. First, essential information
 is inserted into the bus data structure and the remaining data in the
 structure is obtained after calling "pf_rec_bus". From the zone name in
 the bus structure, the area name is obtained by calling "pf_area_of_zone".
 */
{
 pf_rec bus;
 char base[5], zone[3];
 int len, status;
 pf_rec_init (&bus, PF_REC); /* Initialize structure prior
 to calling pf_rec_bus.
 PF_REC is in header file "ft.h" */
 strcpy (bus.i.ACbus.type, "B ");
 strncpy (bus.i.ACbus.name, name, 8);
 bus.i.ACbus.name[8] = NULLC;
 strcpy (base, &name[8]);
 bus.i.ACbus.kv = atof (base);
 status = pf_rec_bus (&bus, "G");
 if (status) {
 fprintf (stderr, " Bus %s is not in history file %s \n", name, basefile);
 return 1;
 }
 strncpy (zone, bus.i.ACbus.zone, 2);
 zone[2] = NULLC;
 status = pf_area_of_zone (area, zone);
 if (status) {
 fprintf (stderr, " No area data in history file %s \n", basefile);
 return 1;
 }
 return 0;
}
int increment (char *busname1, char *busname2, float delta_gen, int flag)
/* This routine applies +/- delta_gen to pairs of buses, areas, and/or
 intertie records to effect the desired transfer. */
{
 pf_rec b, a, itie;
 char areaname1[11], areaname2[11];
 int status;
 /* The following are declared "static" such that they can be initialized
 to NULL, are local, and are not volatile, i.e., they retain their value
 after the routine is executed. In FORTRANese, they are similar to local
 variables, initialized with a DATA statement, and retained with a
 SAVE statement.
  */
 static char oldbus1[13] = "", oldarea1[11] = "", oldbus2[13] = "",
 oldarea2[13] = "";
 /* Get areaname1 associated with busname1 */
 status = get_bus_area (busname1, areaname1);
 if (status) {
 fprintf (stderr, " Unable to retrieve area associated with bus %s \n",
 busname1);
 pf_cflow_exit();
 return 1;
 }
 /* Get areaname2 associated with busname2 */
 status = get_bus_area (busname2, areaname2);
 if (status) {
 fprintf (stderr, " Unable to retrieve area associated with bus %s \n",
 busname2);
 pf_cflow_exit();
 return 1;
 }
 /* Restore original Pgen on "oldbus1" and modify Pgen on "busname1".
 In this case, restoring the original Pgen on "oldbus1" is not
 necessary because it never changes its name once it is assigned.
 The same is true for area "oldarea1" and for intertie "oldarea1-oldarea2".
 This extra logic (two lines in each instance) is added for generality.
 */
 if (strcmp (busname1, oldbus1) != 0) {
 if (strlen (oldbus1) > 0)
 status = mod_bus_gen (oldbus1, -delta_gen);
 status = mod_bus_gen (busname1, delta_gen);
 }
 /* Restore original Pgen on oldbus2 and modify Pgen on busname2 */
 if (strcmp (busname2, oldbus2) != 0) {
 if (strlen (oldbus2) > 0)
 status = mod_bus_gen (oldbus2, delta_gen);
 status = mod_bus_gen (busname2, -delta_gen);
 }
 if (flag) {
 /* Area Intertie "I" records exist and these records define the net
 area interchange export. Restore the original export on intertie
 "oldarea1-oldarea2" and modify the export on intertie
 "areaname1-areaname2" */
 if (strlen (oldarea1) > 0 && strlen (oldarea2) > 0)
 status = mod_itie_export (oldarea1, oldarea2, -delta_gen);
 status = mod_itie_export (areaname1, areaname2, delta_gen);
 } else {
     /* Area intertie "I" records do not exist. Work directly on the
 area records. Modify export on each area "areaname1" and
 "areaname2" */
 if (strlen (oldarea1) > 0)
 status = mod_area_export (oldarea1, -delta_gen);
 status = mod_area_export (areaname1, delta_gen);
 if (strlen (oldarea2) > 0)
 status = mod_area_export (oldarea2, delta_gen);
 status = mod_area_export (areaname2, -delta_gen);
 }
 strcpy (oldbus1, busname1);
 strcpy (oldbus2, busname2);
 strcpy (oldarea1, areaname1);
 strcpy (oldarea2, areaname2);
 return status;
}
int mod_bus_gen (char *busname, float delta_gen)
/* This routine changes Pgen on bus "busname" an amount "delta_pgen" */
{
 pf_rec b;
 int status;
 pf_rec_init (&b, PF_REC); /* Initialize structure prior
 to calling pf_rec_bus.
 PF_REC is in header file "ft.h" */
 strcpy (b.i.ACbus.type, "B ");
 strncpy (b.i.ACbus.name, busname, 8);
 b.i.ACbus.name[8] = NULLC;
 b.i.ACbus.kv = atof (&busname[8]);
 status = pf_rec_bus (&b, "G");
 if (status) {
 fprintf (stderr, " Bus %s is not in history file %s \n",
 busname, basefile);
 pf_cflow_exit();
 return 1;
 }
 /* Add "delta_gen" to bus generation */
 b.i.ACbus.Pgen = b.i.ACbus.Pgen + delta_gen;
 status = pf_rec_bus (&b, "M");
 return status;
}
int mod_area_export (char *areaname1, float delta_export)
/* This routine changes Export on Area "areaname1" an amount "delta_export" */
{
 pf_rec a;
 int status;
  pf_rec_init (&a, PF_REC); /* Initialize structure prior
 to calling pf_rec_bus.
 PF_REC is in header file "ft.h" */
 strcpy (a.i.area.type, "A ");
 strcpy (a.i.area.name, areaname1);
 status = pf_rec_area (&a, "G");
 if (status) {
 fprintf (stderr, " Area %s is not in history file %s \n",
 a.i.area.name, basefile);
 pf_cflow_exit();
 return 1;
 }
 /* Add "delta_export" to area export */
 a.i.area.sched_export = a.i.area.sched_export + delta_export;
 status = pf_rec_area (&a, "M");
 return status;
}
int mod_itie_export (char *areaname1, char *areaname2, float delta_export)
/* This routine changes Export on area intertie "areaname1-areaname2" and
 amount "delta_export" */
{
 pf_rec itie;
 int status;
 pf_rec_init (&itie, PF_REC); /* Initialize structure prior
 to calling pf_rec_itie.
 PF_REC is in header file "ft.h" */
 if (strcmp (areaname1, areaname2) == 0) {
 return 0;
 } else {
 strcpy (itie.i.itie.type, "I ");
 strcpy (itie.i.itie.area1_name, areaname1);
 strcpy (itie.i.itie.area2_name, areaname2);
 status = pf_rec_itie (&itie, "G");
 if (status) {
 itie.i.itie.sched_export = delta_export;
 status = pf_rec_itie (&itie, "A"); /* Add a new "I" record */
 } else {
 itie.i.itie.sched_export = itie.i.itie.sched_export + delta_export;
 status = pf_rec_itie (&itie, "M"); /* Modify existing "I" record */
 }
 return status;
 }
}
int compare( const void *key1, const void *key2 )
/* static int compare( const void *key1, const void *key2 ) */
/* This comparing function obtains the relative sort order of *key1 and
 *key2 upon the following fields:
 1. area1.
 2. bus1.
 3. area2.
 4. bus2.
 5. id.
 */
{
 int i = *((int *) key1), j = *((int *) key2), comp;
 char *area11, *area12, *area21, *area22, *bus11, *bus12, *bus21, *bus22,
 id1, id2;
 int sect1, sect2;
 if (i == j) {
 return 0;
 } else {
 if (i > 0) {
 area11 = br_flow[i].area1;
 area12 = br_flow[i].area2;
 bus11 = br_flow[i].bus1;
 bus12 = br_flow[i].bus2;
 id1 = br_flow[i].id;
 sect1 = br_flow[i].section;
 } else {
 area11 = br_flow[-i].area2;
 area12 = br_flow[-i].area1;
 bus11 = br_flow[-i].bus2;
 bus12 = br_flow[-i].bus1;
 id1 = br_flow[-i].id;
 sect1 = br_flow[-i].section;
 }
 if (j > 0) {
 area21 = br_flow[j].area1;
 area22 = br_flow[j].area2;
 bus21 = br_flow[j].bus1;
 bus22 = br_flow[j].bus2;
 id2 = br_flow[j].id;
 sect2 = br_flow[j].section;
 } else {
 area21 = br_flow[-j].area2;
 area22 = br_flow[-j].area1;
 bus21 = br_flow[-j].bus2;
 bus22 = br_flow[-j].bus1;
 id2 = br_flow[-j].id;
 sect2 = br_flow[-j].section;
 }
 comp = strcmp (area11, area21);
 if (comp == 0) comp = strcmp (bus11, bus21);
 if (comp == 0) comp = strcmp (area12, area22);
 if (comp == 0) comp = strcmp (bus12, bus22);
 if (comp == 0) comp = (unsigned int)id1 - (unsigned int)id2;
 if (comp == 0) comp = sect1 - sect2;
 return comp;
  }
}
void title ( int *lineno, int *pageno, FILE *fp_outfile, pf_comments *c,
 int numcases, char *areaname)
{
 /* Write base case comments */
 int i;
 fprintf (fp_outfile, "\f\n\n");
 (*pageno)++;
 for (i=0; i < 3; i++) {
 if (strlen (c->h[i]) > 0)
 fprintf (fp_outfile, " %s \n", &c->h[i][1]);
 (*lineno)++;
 }
 fprintf (fp_outfile, "\n INCREMENTAL report (DELTA) line flows in MW \
page No. %d \n\n", *pageno);
 fprintf (fp_outfile, " From bus To bus cir To area Base Flow ");
 for (i = 1; i <= numcases; i++) {
 fprintf (fp_outfile, "Incr %2d ", i);
 }
 (*lineno) += 6;
 if (strlen (areaname) > 0) {
 fprintf (fp_outfile, "\n From area %s \n\n", areaname);
 (*lineno) += 3;
 }
}
