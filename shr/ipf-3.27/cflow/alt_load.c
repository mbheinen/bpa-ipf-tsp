/******************************************************************************\
UTILITY:    ALT_LOAD
TYPE:       Powerflow (IPFSRV v. 209 or later) data modification.
SUMMARY:    Separate bus loads from transformer losses.
RELATED:    COMP_SOLN
SEE ALSO:
UPDATED:    February 14, 1995
LANGUAGE:   Standard C.  CFLOW libraries.  CF_UTIL.H.
DEVELOPER:  WDRogers, BPA, TEOH, 230-3806
REQUESTER:  Gordon L. Comegys, BPA, TOT, 418-2335, x02335 from HQ
USERS:      WDRogers, GLComegys
PURPOSE:    Generate a new basecase with loads at specified buses separated from
            transformer losses.  Add new buses with the bus specific-loads and
            add transformers with the transformer-specific loss.  Add R cards
            to hold the new bus voltage to that of the old bus.  Buses are 
            selected from a binary base case file based on base kV, type, and
            zone.
\******************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "cflowlib.h"
#include "cf_util.h"

#define PFOK 0
#define YES 1
#define NO 0
#define B_CARDS "B  BC BE BG BQ BT BV BX +F +I +N + "   /* bus types modified */
#define NOCIRID ' '
#define NOSECTID (int)0
#define FILESPEC FILENAME_MAX
#define INPUT 132         /* 132 characters in input string for a number */
#define DEF_MVA 1.0  /* default minimum transformer MVA */
#define SEP " ,;:/|=_`~><!@#$%^&*(){}[]\t\\\"\'\?"
#define MAX 4000
#define CCODES "FIN "                       /* Valid continuation-card types. */

int make_new_name(char *new_name, char *name, float kv);

char name_list[MAX*9];     /* string to hold new_names */

main( int argc, char *argv[] )
{
  typedef struct changed_node {
	char oldnode[9];
	char oldtype[3];
        float oldkv;
	char newnode[9];
	float vholdpu;
  } Node_data;                  /* (Node_data) same as (struct changed_node) */
  Node_data node_list[MAX];     /* array of nodes changed by the program */

  pf_rec b, n, t, r, u, m;
  FILE *change_file, *append_file;
  char appends[FILESPEC],oldbase[FILESPEC],newbase[FILESPEC],changes[FILESPEC];
  char zones[INPUT], types[INPUT], name[9], new_name[9];
  char net_data[120], s[INPUT], *sp;
  int i, j, k, kvs[INPUT];   /* array to hold bus voltages to search */
  int valid_zone, valid_type, valid_kv, new_bus;
  int rtn_cd, num_mod, search;
  float vholdpu, kv, p, q, z, q_new, mva_new, x_new, ztot, zdiff, xtot;
  float newvmag, newvdeg, vmagdiff, vdegdiff, txmva;
  name_list[0] = '\0';

/* Prompt for OLD_BASE input file */
  cf_sprompt("\n > Enter base case file spec [%s]: ", "FGROVE.BSE", oldbase);
  printf("   Base case file: %s\n", oldbase);
  cf_strsuf(newbase, oldbase, ';', "_NEW");
  cf_strsuf(changes, oldbase, '.', ".CHG");
  cf_strsuf(appends, oldbase, '.', ".APP");

/*  Prompt for bus kv list.  Default is 115 kv */
  printf("\n > Enter bus voltage list (69, 115,...) [115.0]: ");
  gets(s);
  i = 0;
  if ( s[0] != '\0' ) {
	kvs[i++] = (int) (10 * atof(strtok(s, SEP)));
	while ( (sp=strtok(NULL, SEP)) != NULL )
		kvs[i++]=(int) (10*atof(sp));
  }
  else kvs[i++] = 10 * 115;
  kvs[i] = 0;
  printf("   Base kVs selected: ");
  for ( i = 0; kvs[i] != 0; printf("%6.1f ", (float) kvs[i++]/10) );
  printf("\n");

/*  Prompt for minimum transformer MVA.  Use default if none given.  */
  cf_fprompt("\n > Enter minimum transformer nameplate rating [%3.1f MVA]: ",
    DEF_MVA, &txmva);
  printf("   MIN TX MVA = %6.3f\n", txmva);

/*  Prompt for bus types.  Default is "B  BC BE BG BQ BT BV BX +N +I +F + " */
/*  -- always get all B cards -- */
  cf_nprompt("\n > Enter bus types [%s]: ", B_CARDS, INPUT, types);
  cf_stoupper(types);
  cf_str2list(types, 2, ' ');
  printf("   Bus types selected: %s\n", types);

/*  Prompt for zones.  Default is ALL */
  cf_nprompt("\n > Enter zones [%s]: ","ALL ZONES", INPUT, zones);
  cf_stoupper(zones);
  if ( !cf_strcon(zones, "ALL") )
  	cf_str2list(zones, 2, ' ');
  printf("   Zones selected: %s\n\n", zones);

/* Connect to Powerflow */
  pf_cflow_init(argc, argv);
  printf("pf_cflow_init\n");

/* Load old base.  Open changes file. */
  printf("\n   Loading   %s   ...  ", oldbase);
  cf_pfexit(pf_load_oldbase(oldbase), "   Cannot load old base: %s\n", oldbase);
  printf("   Old base loaded.\n");

  change_file = fopen(changes, "w");
  fprintf(change_file, "/ changes, file = *\n");

/* Modify and add records */
  printf("   Modifying buses.  ");
  num_mod = 0; search = 0;
  rtn_cd = pf_rec_bus(&b, "F");
  for (; (rtn_cd==PFOK) && (num_mod<MAX); rtn_cd = pf_rec_bus(&b,"N") ){
    pf_rec_bus(&b, "G");
    valid_type = cf_strcon(types, b.i.ACbus.type);
    valid_zone = cf_strcon(zones, "ALL") || cf_strcon(zones, b.i.ACbus.zone);
    for (valid_kv = i = 0; valid_kv == NO && kvs[i] != 0; i++)
        valid_kv = ( kvs[i] == (int) (10*b.i.ACbus.kv) );
    if ( valid_type && valid_zone && valid_kv )
        new_bus = make_new_name(new_name, b.i.ACbus.name, b.i.ACbus.kv);
    else new_bus = NO;

    if ( new_bus ) {
        strcpy(node_list[num_mod].oldnode, b.i.ACbus.name);
        strcpy(node_list[num_mod].oldtype, b.i.ACbus.type);
        strcpy(node_list[num_mod].newnode, new_name);
        node_list[num_mod].oldkv = b.i.ACbus.kv;

    /* move all the + cards to new bus card */
        memcpy(&u, &b, sizeof(b));
        rtn_cd = pf_rec_cbus(&u, "F1");
        xtot = 0;
        for ( ztot = 0 ; rtn_cd == PFOK ; rtn_cd = pf_rec_cbus(&u,"N1") ) {
            if ( !cf_strcon(types, u.i.cbus.type) ) continue;
            pf_rec_cbus(&u, "G");
            p = u.i.cbus.Pload;
            q = u.i.cbus.Qload;
            z = (float)sqrt( (float)(p*p) + (float)(q*q) );
            if ( z <= 0 ) continue;
            q_new = q - 0.10*z;
            ztot += z;
        /* delete original + bus card */
            if ( pf_rec_b2a(net_data, &u, "D") == PFOK )
                fprintf(change_file, "%s\n", net_data);
            else printf("Error deleting + card\n");
        /* create new + bus card */
            pf_init_bus(&m, u.i.cbus.type, new_name, u.i.cbus.kv);
            strcpy(m.i.cbus.owner, u.i.cbus.owner);
            strcpy(m.i.cbus.code_year, u.i.cbus.code_year);
            m.i.cbus.Pload = p;
            m.i.cbus.Qload = q_new;
            if ( pf_rec_b2a(net_data, &m, "A") != PFOK )
                printf("Error adding + card\n");
            else fprintf(change_file, "%s\n", net_data);
            search++;
        }
        p = b.i.ACbus.Pload;
        q = b.i.ACbus.Qload;
        z = (float)sqrt( (float)(p*p) + (float)(q*q) );
        ztot += z;
        if ( ztot <= 0 ) continue;
        zdiff = (ztot < txmva) ? (txmva - ztot) : 0;
        if (zdiff > 0) printf("   Zdiff = %4.4f\n", zdiff);
        mva_new = ztot + zdiff;
        x_new  = 10.0/mva_new;
        q_new = q - 0.10*(z + zdiff);

    /* modify original B  bus card */
        b.i.ACbus.Pload = 0;
        b.i.ACbus.Qload = 0;
        if ( pf_rec_b2a(net_data, &b, "M") != PFOK )
            printf("Error modifying B card\n");
        else fprintf(change_file, "%s\n", net_data);

    /* create new B_ bus card */
        pf_init_bus(&n, "B ", new_name, b.i.ACbus.kv);
        strcpy(n.i.ACbus.owner, b.i.ACbus.owner);
        strcpy(n.i.ACbus.zone, b.i.ACbus.zone);
        n.i.ACbus.Pload = p;
        n.i.ACbus.Qload = q_new;
        if ( pf_rec_b2a(net_data, &n, "A") != PFOK )
            printf("Error adding B card\n");
        else fprintf(change_file, "%s\n", net_data);

    /* create T transformer card */
        pf_init_branch(&t, "T ", b.i.ACbus.name, b.i.ACbus.kv, 
            n.i.ACbus.name, n.i.ACbus.kv, NOCIRID, NOSECTID);
        t.i.T.x = x_new;
        t.i.T.tap1 = b.i.ACbus.kv;
        t.i.T.tap2 = n.i.ACbus.kv;
        t.i.T.total_rating = mva_new;
        strcpy(t.i.T.owner, b.i.ACbus.owner);
        if ( pf_rec_b2a(net_data, &t, "A") != PFOK )
            printf("Error adding T card\n");
        else fprintf(change_file, "%s\n", net_data);
        num_mod++;
    }
    search++;
  }
  printf("   %d buses searched.   %d buses modified.\n", search, num_mod);
  if ( num_mod >= MAX ) printf("   Maximum number of buses processed.\n");
  fclose(change_file);

  printf("   Loading changes.  ");
  cf_pfexit(pf_load_changes(changes), "   Cannot load changes: %s\n", changes);
  printf("   Changes loaded.\n");

  printf("   Solving case....  ");
  cf_pfexit(pf_solution(), "   Cannot solve modified case.\n");
  printf("   Modified case solved.\n");

/* find buses in name_list[], get vholdpu for new BT buses */
  printf("   Getting VHOLDPU.  ");
  i = num_mod;
  while ( i-- > 0 ) {
        pf_init_bus(&b, "B ", node_list[i].newnode, node_list[i].oldkv);
        if ( pf_rec_bus(&b, "O") == PFOK ) {
            node_list[i].vholdpu = b.s.ACbus.Vmag/node_list[i].oldkv;
        }
        else {
            printf("   Error getting VHOLDPU\n"); 
            node_list[i].vholdpu = 1.0;
        }
  }

/* Open new change file */
  change_file = fopen(appends, "w");
  fprintf(change_file, "/ changes, file = *\n");

/* find buses in name_list[], fix vholdpu on new BT buses */
  printf("   Modifying VHOLDPU values.\n");
  i = num_mod;
  while ( i-- > 0 ) {
    strcpy(new_name, node_list[i].newnode);
    strcpy(name, node_list[i].oldnode);
    kv = node_list[i].oldkv;

  /* modify new B card to be a BT card with VHOLDPU */
    pf_init_bus(&n, "B ", new_name, kv);
    pf_rec_bus(&n, "G");
    strcpy(n.i.ACbus.type, "BT");
    n.i.ACbus.Vhold_Vmax = node_list[i].vholdpu;
    if ( pf_rec_b2a(net_data, &n, "M") == PFOK ) 
        fprintf(change_file, "%s\n", net_data);
    else printf("Error modifying B card\n");

  /* create R transformer card */
    pf_init_branch(&r, (char *)"R ",name,kv,new_name,kv,NOCIRID,NOSECTID);
    r.i.R.rmt_bus_kv = kv;
    r.i.R.max_tap = 1.1 * kv;
    r.i.R.min_tap = 0.9 * kv;
    strcpy(r.i.R.rmt_bus_name, new_name);
    strcpy(r.i.R.owner, n.i.ACbus.owner);
    if ( pf_rec_b2a(net_data, &r, "A") == PFOK )
        fprintf(change_file, "%s\n", net_data);
    else printf("Error adding R card\n");
  }

  fclose(change_file);

  printf("   Loading changes.  ");
  cf_pfexit(pf_load_changes(appends), "   Cannot load changes: %s\n", appends);
  printf("   Changes loaded.\n");

  printf("   Solving case....  ");
  cf_pfexit(pf_solution(), "   Cannot solve modified case.\n");
  printf("   Modified case solved.\n");

  printf("   Saving case.....  ");
  cf_pfexit(pf_save_newbase(newbase), "   Cannot save modified case.\n");
  printf("   Modified case saved.\n");

/* Append change file 2 to change file 1 */
  change_file = fopen(changes, "a");
  append_file = fopen(appends, "r");
  while ( (char) (k = fgetc(append_file)) != '\n' );
  for (j=0; (k=fgetc(append_file))!=EOF && j!=EOF; j=fputc(k, change_file));
  fclose(change_file);
  remove(appends);
  printf("   Changes  file saved as:   %s\n", changes);

  pf_cflow_exit();
}	

int make_new_name(char *new_name, char *name, float kv) {
  int unique = NO, change = 7;
  char c = '0';
  strcpy(new_name, name);
  while (unique == NO) {
  	new_name[change] = c;
	unique = pf_bus_exists(new_name, kv);
	if ( (unique = pf_bus_exists(new_name, kv)) == YES )
		unique = (strstr(name_list, new_name) == NULL) ? YES : NO;
	else unique = NO; 
 	if (unique == NO) {
		/* printf(" - This node already exists! %s\n", new_name); */
		if ( c == '9' && change-- > 0 )	c = '0';
		else if ( c == '9' && change <= 0 ) {
			printf("\nCould not generate new bus for %s.\n", name);
			return 0; }
		else c++;
	}
  }
  strncat(name_list, new_name, 9);
  return 1;
}
