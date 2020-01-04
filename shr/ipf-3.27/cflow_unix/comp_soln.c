/******************************************************************************\
UTILITY:    COMP_SOLN
TYPE:       Powerflow (IPFSRV v. 209 or later) data extraction.
SUMMARY:    Compares bus solution voltages of two solved cases.
RELATED:    ALT_LOAD
SEE ALSO:
UPDATED:    February 14, 1995
LANGUAGE:   Standard C.  CFLOW libraries.
DEVELOPER:  William D. Rogers, BPA, TEOH, 230-3806
REQUESTER:  WDRogers - used for testing ALT_LOAD results.
USERS:      WDRogers
PURPOSE:    Compares the solution voltage magnitude and angle of selected buses
            in two separated base cases.
\******************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "cflowlib.h"

#define CFLOWOK 0
#define SOLOK 0
#define BASECASEOK 0
#define SAVEOK 0
#define LOADOK 0
#define GETRECOK 0
#define GETSOLOK 0
#define YES 1
#define NO 0
#define TYPE_B "B "
#define B_CARDS "B  BC BE BG BQ BT BV BX BS"   /* bus types to be modified */
#define FILESPEC 256     /* 256 characters for filespec */
#define INPUT 132         /* 132 characters in input string for a number */
#define DEF_ERR 0.101  /* default minimum transformer MVA */
#define SEP " ,;:-/|=_`~><!@#$%^&*(){}[]\t\\\"\'\?"
#define MAX 8000
 

main( int argc, char *argv[] )
{
  typedef struct changed_node {
	char oldnode[9];
	char oldtype[3];
	float oldkv;
	float oldvmag;
	float oldvdeg;
  } Node_data;                   /* (Node_data) same as (struct changed_node) */
  Node_data node_list[MAX];    /* array of nodes changed by the program */

  pf_rec b;         /* structure of type pf_rec for holding B-card data */
  pf_rec n;         /* structure of type pf_rec for holding B-card data */

  char oldbase[FILESPEC]; /* holds name of old base input file */
  char newbase[FILESPEC]; /* holds name of old base input file */
  int get_data;
  int rtncde;       /* holds return code for powerflow function calls */

  char s[INPUT];    /* holds an input string */
  char *sp;         /* string pointer */
  float mxerr;

  int i, j, k;      /* counters */
  char c;           /* holds characters from input string s[] for examination */
  char zones[INPUT];/* INPUT number of zones, 2 chars + '\0' per zone */
  char types[INPUT];/* string to hold bus types */
  int kvs[INPUT];   /* array to hold bus voltages to search */
  int valid_zone;
  int valid_type;
  int valid_kv;

  int num_mod;      /* counts the number of nodes modified */
  int search;       /* counter */
  int rtn_cd;       /* holds return code value for pf_rec_branch() function */

  char type[3], zone[3], owner[4], name[9];
  float kv;

  float newvmag, newvdeg, vmagdiff, vdegdiff;
  char result[120]; /* string to hold compared solution data */

  memset(&b, '\0', sizeof(b));
  memset(&n, '\0', sizeof(n));

  pf_cflow_init(argc, argv);
  printf("pf_cflow_init\n");

/* Prompt for OLD_BASE and NEW_BASE input files */
  printf("\n > Enter OLD_BASE file spec: "); gets(oldbase);
  printf("\n > Enter NEW_BASE file spec: "); gets(newbase);

/*  Prompt for acceptable error.  Use default if none given.  */
  printf("\n > Enter maximum acceptable error for kV and ° [%0.3f]: ",DEF_ERR);
  gets(s);
  mxerr = ( (s[0] == '\0') ? DEF_ERR : atof(s) ); 
  printf("   MAX ERROR = %6.5f\n", mxerr);

/*  Prompt for bus kv list.  Default is 115 kv */
  printf("\n > Enter bus voltage list (69, 115,...) [115.0]: ");
  gets(s); i = 0;
  if ( s[0] != '\0' ) {
	kvs[i++] = (int) (10 * atof(strtok(s, SEP)));
	while ( (sp=strtok(NULL, SEP)) != NULL )
		kvs[i++]=(int) (10*atof(sp));
  }
  else kvs[i++] = 10 * 115;
  kvs[i] = 0; printf("  ");
  for ( i = 0; kvs[i] != 0; printf("%6.1f ", (float) kvs[i++]/10) );
  printf("\n");

/*  Prompt for zones.  Default is ALL */
  strcpy(zones, "ALL");
  printf("\n > Enter zones (XX XX,XX...) [ALL ZONES]: ");
  gets(s);
  i = j = k = 0;
  while ( (c = s[j++]) != '\0' && k < INPUT) {
        if (i == 2) { zones[k++] = ' '; i = 0; }		
	if ( isalnum(c) ) { zones[k++] = toupper(c); i++; }
	else if ( !isalnum(c) && i == 1 ) { zones[k++] = ' '; i++; }
  }
  if ( s[0] != '\0' ) {
	if (i == 1) zones[k++] = ' ';
	zones[k] = '\0'; }
  printf("   Zones selected: %s\n", zones);

/*  Prompt for bus types.  Default is "B  BC BE BG BQ BT BV BX" */
  strcpy(types, B_CARDS);
  printf("\n > Enter bus types [%s]: ", B_CARDS);
  gets(s);
  i = j = k = 0;
  while ( (c = s[j++]) != '\0'  && k < INPUT ) {
        if (i == 2) { types[k++] = ' '; i = 0; }		
	if ( isalpha(c) ) { types[k++] = toupper(c); i++; }
	else if ( !isalpha(c) && i == 1 ) { types[k++] = ' '; i++; }
  }
  if ( s[0] != '\0' ) {
  	if (i == 1) types[k++] = ' ';
  	types[k] = '\0'; }
  printf("   Bus types selected: %s\n", types);

/* Load old base case */
  printf("\n   Loading   %s   ...\n", oldbase);
  if ( pf_load_oldbase(oldbase) == BASECASEOK )
	printf("   The old base is loaded.\n");
  else {printf("\n   The old base did not load.\n"); pf_cflow_exit(); return; }

/* Solve case */
  printf("   Solving case.  Please wait.\n");
  if ( pf_solution() == SOLOK ) printf("   Case solved.\n");
  else { pf_cflow_exit(); return; }

/* Need to record a list of wanted buses along with their solution data */
/* find 115kv B_ buses in specified zones with Zload > 0 */

  printf("   Getting solution values.\n");
  num_mod = 0; search = 0;
  rtn_cd = pf_rec_bus(&b, "F");
  while ( rtn_cd == GETRECOK && num_mod < MAX ) {
	strcpy(type, b.i.ACbus.type);
	strcpy(zone, b.i.ACbus.zone);
	strcpy(name, b.i.ACbus.name);
	kv = b.i.ACbus.kv;

	/* search list of acceptable Bus cards, B_CARDS, for current type */
	valid_type = (strstr(types, type) != NULL) ? YES : NO;
	/* search list of zones for current zone */
	valid_zone = (strstr(zones,"ALL")!=NULL) || (strstr(zones,zone)!=NULL);
	/* search kv list for bus kv */
	for (valid_kv = i = 0; valid_kv == NO && kvs[i] != 0; i++)
		valid_kv = ( kvs[i] == (int) (10*kv) );

	if ( valid_type && valid_zone && valid_kv ) {
		rtncde = pf_rec_bus(&b, "O");
		if (rtncde == GETSOLOK ) {
			strcpy(node_list[num_mod].oldnode, name);
			strcpy(node_list[num_mod].oldtype, type);
			node_list[num_mod].oldkv = kv;
			node_list[num_mod].oldvmag = b.s.ACbus.Vmag;
			node_list[num_mod].oldvdeg = b.s.ACbus.Vdeg;
			num_mod++;
		}
	}
	search++;
 	rtn_cd = pf_rec_bus(&b,"N");
  }
  if ( num_mod >= MAX ) printf("   Maximum number of nodes examined.\n");

/* Load NEW_BASE */
  printf("\n   Loading   %s   ...\n", newbase);
  if ( pf_load_oldbase(newbase) == BASECASEOK )
	printf("   The new base is loaded.\n");
  else {printf("\n   The new base did not load.\n"); pf_cflow_exit(); return; }

/* Solve case */
  printf("   Solving case.  Please wait.\n");
  if ( pf_solution() == SOLOK ) printf("   Case solved.\n");
  else{ printf("   Modified case not solved.\n"); pf_cflow_exit(); return; }

/* check modified case against old base case */
  printf("   Now checking new base case solution against old base case.\n");
  i = num_mod; j = 0; k = 0;
  printf("{ty}[  name  ](  kv  ) (old sol val)   (new sol val)   ");
  printf("(difference)\n");
  while ( i-- > 0 ) {
	strcpy(name, node_list[i].oldnode);
	strcpy(type, node_list[i].oldtype);
	kv = node_list[i].oldkv;
	pf_init_bus(&b, type, name, kv);
	rtncde = pf_rec_bus(&b, "O");
	j = sprintf(result, "{%s}[%s](%6.1f) ", type, name, kv);
	j += sprintf(&result[j], "(%5.1f,", node_list[i].oldvmag);
	j += sprintf(&result[j], "%5.1f) ", node_list[i].oldvdeg);
	j += sprintf(&result[j], "- (%5.1f,", newvmag = b.s.ACbus.Vmag);
	j += sprintf(&result[j], "%5.1f) ", newvdeg = b.s.ACbus.Vdeg);
	vmagdiff = fabs(node_list[i].oldvmag - newvmag);
	vdegdiff = fabs(node_list[i].oldvdeg - newvdeg);

	if ( (vmagdiff > mxerr) || (vdegdiff > mxerr) )
		printf("%s= (%4.2f, %4.2f) BAD!\n",result,vmagdiff,vdegdiff);
	else k++;
/*	printf("%s - (%4.2f, %4.2f) ¡OK!\n",result,vmagdiff,vdegdiff);
*/
  }
  printf("   %d records checked.  %d records OK.\n", num_mod, k);
  pf_cflow_exit();
}	
