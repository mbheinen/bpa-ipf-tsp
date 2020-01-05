/* SEE_NODES.C */
/* William D. Rogers */
/* August 5, 1994 */

/* Program will be interactive with Powerflow.
 * - prompt for oldbase and newbase
 * - solve oldbase
 * - find all 115 kv buses in the specified zones in oldbase
 * - get Vmag and Vdeg solution data for selected nodes
 * - solve newbase
 * - get Vmag and Vdeg solution data for selected nodes from oldbase
 * - print differences in solution
*/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "cflowlib.h"

#define CFLOWOK 0
#define SOLOK 0
#define BASECASEOK 0
#define GETRECOK 0
#define B_CARDS "B  BC BE BG BQ BT BV BX"         /* bus types to be modified */
#define B_ 0
#define NOTB_ 1
#define NOCIRID " "
#define NOSECTID (int)0
#define FILESPEC 196     /* 196 characters for filespec */
#define INPUT 40         /* 40 characters in input string for a number */
/* #define DEF_OLD_BASE "WILL::DISK15:[CFLOW.CASES]J95EH163.BSE" */
#define DEF_OLD_BASE "WILL::DISK15:[CFLOW.CASES]TEST_NET.BSE"
#define DEF_NET_DATA "WILL::DISK15:[CFLOW.CASES]TEST_NET.NET"
#define MAX 16000

/********** added stuff ********************/
/*int in_zones(char *, char *);*/
int in_zones(char zones[INPUT][3], char zone[3]);
/********** added stuff ********************/


main( int argc, char *argv[] )
{
/* ----- comp_base.c ------------------------------------------------------ */
  typedef struct changed_node {
	char oldnode[9];
	char oldtype[3];
	float oldkv;
	float oldvmag;
	float oldvdeg;
  } Node_data;                   /* (Node_data) same as (struct changed_node) */

  Node_data node_list[MAX/2];    /* array of nodes changed by the program */
/* ----- comp_base.c ------------------------------------------------------ */

  pf_rec b;         /* structure of type pf_rec for holding B-card data */

  char oldbase[FILESPEC]; /* holds name of old base input file */
  char newbase[FILESPEC]; /* holds name of new base input file */
  int rtncde;       /* holds return code for powerflow function calls */

  char s[INPUT];    /* holds an input string */

  int i;            /* counter */
  int k;            /* counter */
  int j;            /* counter */
  int search;
  char c;           /* holds characters from input string s[] for examination */
  char zones[INPUT][3];  /* INPUT number of zones, 2 chars + '\0' per zone */

  int rtn_cd;       /* holds return code value for pf_rec_branch() function */

  char type[3];
  char zone[3];
  char name[9];
  float kv;
  float vmagdiff;
  float vdegdiff;
  float vtotdiff;
 
  int card_type;    /* holds value from comparing card type */


  memset(&b, '\0', sizeof(b));

  pf_cflow_init(argc, argv);
  printf("pf_cflow_init\n");

/*  Prompt for OLD_BASE file spec.  Use default file if none given.  */
  printf("\n > Enter OLD_BASE file spec: "); 
  gets(oldbase);
  printf("\n   Loading   %s   ...\n", oldbase);
  rtncde = pf_load_oldbase(oldbase);
  if ( rtncde == BASECASEOK )
	printf("   The old base is loaded.\n");
  else {
	printf("\n   The old base did not load.\n");
	pf_cflow_exit();
	return;
  }
/*  Prompt for NEW_BASE file spec.  Use default file if none given.  */
  printf("\n > Enter NEW_BASE file spec: "); 
  gets(newbase);

/* ----- comp_base.c ------------------------------------------------------ */
/* Solve case */
  if ( pf_solution() != SOLOK ) {
	pf_cflow_exit();
	return;
  }
  else
	printf("   Old base solved.\n");
/* ----- comp_base.c ------------------------------------------------------ */

/*  Prompt for zones.  Default is ALL */
  printf("\n > Enter zones (XX,XX XX;XX,...) [ALL ZONES]: ");
  gets(s);
  i = j = k = 0;
  while ( (c = s[j++]) != '\0'  && k < INPUT) {
        if (i == 2) {
		zones[k++][i] = '\0';
		i = 0;
	}		
	if ( isalnum(c) )
		zones[k][i++] = toupper(c);
	else if ( !isalnum(c) && i == 1 )
		zones[k][i++] = ' ';
  }
  if (i == 1)
	zones[k][i] = ' ';
  zones[k][2] = '\0';

/*  Print list of selected zones */
  printf("   Zones selected: ");
  if ( zones[0][0] == '\0' )
	printf("ALL");
  else
	for (k = 0; zones[k][0] != '\0'; printf("%s,", &zones[k++][0]));
  printf("\n");

/* find 115kv B_ buses in specified zones with Zload > 0 */

  search = 0;
  rtn_cd = pf_rec_bus(&b, "F");
  while ( rtn_cd == GETRECOK && search < MAX ) {
	strcpy(type, b.i.ACbus.type);
	strcpy(zone, b.i.ACbus.zone);
	strcpy(name, b.i.ACbus.name);
	kv = b.i.ACbus.kv;

	/* search list of acceptable Bus cards, B_CARDS, for current type */
	card_type = (strstr(B_CARDS, type) != NULL) ? B_ : NOTB_;
	if ( card_type == B_ && in_zones(zones, zone) && kv==115 ) {
		pf_rec_bus(&b, "O");
		strcpy(node_list[search].oldnode, name);
		strcpy(node_list[search].oldtype, type);
		node_list[search].oldkv = kv;
		node_list[search].oldvmag = b.s.ACbus.Vmag;
		node_list[search].oldvdeg = b.s.ACbus.Vdeg;
		printf("<%5d> Node: {%s}[%s]", search, type, name);
		printf("(%3.0f) Soln: (%7.3f kV", kv, b.s.ACbus.Vmag);
		printf(", %5.3f °)\n", b.s.ACbus.Vdeg);
		search++;
	}
	rtn_cd = pf_rec_bus(&b, "N");
  }
/* load newbase and compare solution with oldbase */
  rtncde = pf_load_oldbase(newbase);
  if ( rtncde == BASECASEOK )
	printf("   The new base is loaded.\n");
  else {
	printf("\n   The new base did not load.\n");
	pf_cflow_exit();
	return;
  }

/* Solve newbase */
  if ( pf_solution() != SOLOK ) {
	pf_cflow_exit();
	return;
  }
  else
	printf("   New base solved.\n");

/* find buses in node_list */

  vtotdiff = (float) 0;
  while ( search-- > 0 ) {
	strcpy(type, node_list[search].oldtype);
	strcpy(name, node_list[search].oldnode);
	kv = node_list[search].oldkv;

	pf_init_bus(&b, type, name, kv);
	pf_rec_bus(&b, "O");

	vmagdiff = b.s.ACbus.Vmag - node_list[search].oldvmag;
	vdegdiff = b.s.ACbus.Vdeg - node_list[search].oldvdeg;

	vtotdiff += vmagdiff + vdegdiff;
	printf("<%5d> Node: {%s}[%s] ", search, type, name);
	printf("diff:(%7.4f kV,%7.4f °)", vmagdiff, vdegdiff);
	if ( vtotdiff != 0 ) {
		printf("  -------------- \n");
		pf_cflow_exit();
		return;
	}
	else
		printf("\n");
  }
  printf("Solutions are different by: %10.8f\n", vtotdiff);
  pf_cflow_exit();
}	

int in_zones(char zones[INPUT][3], char zone[3])
{
  int k=0, match=0;
  char s[3], t[3];

  if( zones[0][0] == '\0' )
	match = 1;
  while ( !match && zones[k][0] != '\0' ) {
	strncpy(s, &zones[k++][0], 3);
	strncpy(t, &zone[0], 3);
	if ( strcmp(s, t) == 0 )
		match = 1;
  }
  return match;
}
