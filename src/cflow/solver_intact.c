/******************************************************************************\
UTILITY:    SOLVER_INTACT
TYPE:       Powerflow (IPFSRV) parameter optimization.
SUMMARY:    Find the power generated at two specified buses which results in the
            100% loading of the line connecting the two buses.
RELATED:
SEE ALSO:
UPDATED:    November 28, 1994
LANGUAGE:   Standard C.  CFLOW libraries.
DEVELOPER:  KDShephard
REQUESTER:
USERS:
PURPOSE:    Find the power generated at two specified buses which results in the
            100% loading of the line connecting the two buses.
\******************************************************************************/

/* Program will be interactive with PSAP.  The user will enter in the  	*/
/* buses that the power generation will modify and the line that it	*/ 
/* wants to decrease the % overloading.  User will aslo specify the	*/
/* amount the power generation changes for each bus.  After each 	*/
/* modification, the program wll resolve, evaluate the % load and	*/ 
/* determine if it has reached 100% load on the line.  At 100%, the 	*/
/* program will display the power generated at the buses. 		*/

/* Until the CFLOW liberary is fixed, PCT_COMP actually gives the value */
/* for % LOAD.  The correct value calculated for PCT_COMP has not yet	*/ 
/* been included into the liberary. */


#include <stdio.h>
#include <string.h>
#include "cflowlib.h"

#define SOLVED 0
#define MINOVLD -9999
#define CFLOWOK 0
#define TYPELINE "L "   
#define TYPE1 "B* "     
#define TYPE2 "B*"      
#define NOSECTID (int)0
/****************** debug stuff *********************/
/* #define NOTYPE "  "     */
/* #define CIRIDALL "*"    */

#define NOTYPE '  '     
#define CIRIDALL '*'    
/* #define NOSECTID (int)0 */ 
/************************* stuff to make it go **********************/
/* DEFINE prototypes */ 
void get_branch_overload_pct(pf_rec *r);
/* DEFINE prototypes */ 
  float oldpctload, max_pct_ovld; 
  int rtncde;
  int err;                      

/* main (int argc, char *argv[]) */  
int main (int argc, char *argv[])  
/************************* stuff to make it go **********************/
{
  char *p1,*p2,*p3,*p4;
  char bus1[120], bus2[120];
  char branch1[120], branch2[120];
  float bus_kv1,bus_kv2;
  float branch_kv1, branch_kv2;
  float pgeninit,pgenfac,newpgen_bus1, newpgen_bus2;
/************************* stuff to make it go **********************/
/*  float oldpctload, max_pct_ovld; */
/*  int rtncde;                     */
/*  int i,err;                      */
  int i;
/************************* stuff to make it go **********************/
  int rtnsol;
  int loop=0;
  short toupper(short c); 

  pf_rec r;				/* Line */
  pf_rec br1;				/* Bus1 */
  pf_rec br2;				/* Bus2*/
  pf_case_stats case_info;

  memset(&r, '\0', sizeof(r));

/*  INITIALIZATION  */

  pf_cflow_init(argc, argv);
  printf("pf_cflow_init\n");

  rtncde = pf_load_oldbase("CUTNW.BSE");
  if (rtncde != CFLOWOK)  {
    printf("The oldbase did not load\n");
    pf_cflow_exit();  }	


/*  When the PF_CASE_INFO is included into the CFLOW liberary,		*/
/*  insert the following code to eliminate the initial solution		*/
/*  since the base case has already been solved. Would also have to	*/
/*  modify While loop							*/

/*  rtncde = pf_case_info(&case_info);					*/
/*  if(case_info.case_soln_status != 1)  {				*/
/*    printf("Base case did not contain a successful solution\n");	*/
/*    pf_cflow_exit();  }						*/

  rtnsol = pf_solution();
  if (rtnsol != SOLVED)  {
    printf("Did not reach a successful solution\n");
    pf_cflow_exit();  }

  printf("For the desired line, enter in Bus1\n");
   gets(branch1);
  printf("For the desired line, enter in Bus2\n");
   gets(branch2);
  printf("Enter in the bus name whose Power Generation will decrease\n");
   gets(bus1);
  printf("Enter in the bus name whose Power Generation will increase\n");
   gets(bus2);
  printf("Enter in the voltages for the desired line\n");
   scanf("%f %f",&branch_kv1,&branch_kv2);
  printf("Enter in the voltages for the two busses that are modifiesd\n");
   scanf("%f %f",&bus_kv1,&bus_kv2);
  printf("Enter in an initial value for the Power Generation\n");
   scanf("%f",&pgeninit);
  printf("Enter in a delta change for the Power Generation\n");
   scanf("%f",&pgenfac);

  newpgen_bus1 = -pgeninit;		/* Initializing Power Generation Bus1 */
  newpgen_bus2 = pgeninit;		/* Initializing Power Generation Bus2 */

/* Converts the string BRANCH1 in capital letters */
  for (p1=branch1; *p1!='\0'; p1++)  {
    *p1=(char)toupper((short)*p1);  }

/* Deletes any characters beyond the 8th place */
  for (i=strlen(branch1); i<8;  i++)  {
    branch1[i]=' ';  }

/* Places a null character into the 9th spot of the string */
  branch1[8]='\0';

/* Converts the string BRANCH2 in capital letters */
  for (p2=branch2; *p2!='\0'; p2++)  {
    *p2=(char)toupper((short)*p2);  }

/* Deletes any characters beyond the 8th place */
  for (i=strlen(branch2); i<8;  i++)  {
    branch2[i]=' ';  }

/* Places a null character into the 9th spot of the string */
  branch2[8]='\0';

/* Converts the string BUS1 in capital letters */
  for (p3=bus1; *p3!='\0'; p3++)  {
    *p3=(char)toupper((short)*p3);  }

/* Deletes any characters beyond the 8th place */
  for (i=strlen(bus1); i<8;  i++)  {
    bus1[i]=' ';  }

/* Places a null character into the 9th spot of the string */
  bus1[8]='\0';

/* Converts the string BUS2 in capital letters */
  for (p4=bus2; *p4!='\0'; p4++)  {
    *p4=(char)toupper((short)*p4);  }

/* Deletes any characters beyond the 8th place */
  for (i=strlen(bus2); i<8;  i++)  {
    bus2[i]=' ';  }

/* Places a null character into the 9th spot of the string */
  bus2[8]='\0';

/* Selects Lexingtn Longview 230 line */

    pf_init_branch(&r,TYPELINE,branch1,branch_kv1,branch2,branch_kv2,
			CIRIDALL,NOSECTID);

/* Accesses Subroutine to get the maximum % overloading on the */
/* Lexingtn Longview 230 line */

    get_branch_overload_pct(&r);

/* If after 20 iteration the % load is not under 100%, program terminates */

/*********** may need check for Line/Transformer ************/
/*  while ((rtnsol == SOLVED)&&(r.s.branch.pct_comp > 100)&&(loop < 20))  */
  while ((rtnsol == SOLVED)&&(r.s.branch.tot_line_load_pct > 100)&&(loop < 20))  
/*********** may need check for Line/Transformer ************/
  {
/* Selects busses */

    pf_init_bus(&br1,TYPE1,bus1,bus_kv1);
    pf_init_bus(&br2,TYPE2,bus2,bus_kv2);

    pf_rec_bus(&br1,"g");  
    pf_rec_bus(&br2,"g");  

/* Verifies the loop counter */
/*	printf("Loop # %d\n",loop);				*/
      loop += 1;

/* The variable OLDPCTLOAD will be used if their is a failed solution */
/*********** may need check for Line/Transformer ************/
/*    oldpctload = r.s.branch.pct_comp; */
    oldpctload = r.s.branch.tot_line_load_pct;
/*********** may need check for Line/Transformer ************/

      if(loop == 1)  {
  	newpgen_bus1 = -pgeninit;
  	newpgen_bus2 = pgeninit;  }      

      else  {
/* Verifies the variable oldpctload */
/*	  printf("OLD_PCT_LOAD = %4.1f%%\n",oldpctload);	*/
  	newpgen_bus1 += - pgenfac;
  	newpgen_bus2 +=   pgenfac;  } 

/* Verifies new power generation 				*/
/*      printf("NEWPGEN_BUS1 = %6.1f\n",newpgen_bus1);		*/
/*	printf("NEWPGEN_BUS2 = %6.1f\n",newpgen_bus2);		*/

/* Modifies the Pgen for the specified busses 			*/
 
      br1.i.ACbus.Pgen = newpgen_bus1;
      br2.i.ACbus.Pgen = newpgen_bus2;
        pf_rec_bus(&br1,"m");
        pf_rec_bus(&br2,"m");

/* Submits record changes to be resolved			*/

    rtnsol = pf_solution();
/* Verifies successful solution - The return code value is one	*/
/* iteration behind.						*/
/*     printf("rtnsol = %d\n",rtnsol); 				*/

  if (rtnsol != SOLVED)  
  {
   printf("Did not reach a successful solution\n");
   printf("The lowest percent for line overloading on the %s %6.1f  %s %6.1f\n",
	r.i.branch.bus1_name,r.i.branch.bus1_kv,r.i.branch.bus2_name,
	r.i.branch.bus2_kv);
   printf("was %4.1f%%\n",oldpctload);
   printf("	Power Generation for %s %6.1f  MW\n",br1.i.ACbus.name,
			br1.i.ACbus.Pgen);
    printf("	Power Generation for %s %6.1f  MW\n",br2.i.ACbus.name,
			br2.i.ACbus.Pgen);
  pf_cflow_exit();  
}	/* Closes IF (!= SOLVED) */	    

/* Accesses Subroutine to get the maximum % overloading on the */
/* Lexingtn Longview 230 line */

    get_branch_overload_pct(&r);
/*********** may need check for Line/Transformer ************/
/*    if(r.s.branch.pct_comp > 100)  { */
    if(r.s.branch.tot_line_load_pct > 100)  {
/*********** may need check for Line/Transformer ************/

    pf_init_bus(&br1,TYPE1,bus1,bus_kv1);
    pf_init_bus(&br2,TYPE2,bus2,bus_kv2);

    pf_rec_bus(&br1,"g");  
    pf_rec_bus(&br2,"g");  

      printf("\n%s %6.1f     %s %6.1f\n",r.i.branch.bus1_name,
		r.i.branch.bus1_kv,r.i.branch.bus2_name,r.i.branch.bus2_kv);
      printf("\n     Percent load	     Rating Code\n");
/*********** may need check for Line/Transformer ************/
/*      printf("         %4.1f%%		     %c\n",r.s.branch.pct_comp, */
/*		&r.s.branch.rating_code);                                       */
      printf("         %4.1f%%		     %c\n",r.s.branch.tot_line_load_pct,
		&r.s.branch.crit_line_rat_code);
/*********** may need check for Line/Transformer ************/
      printf("\n	Power Generation for %s %6.1f  MW\n",br1.i.ACbus.name,
		br1.i.ACbus.Pgen);
      printf("	Power Generation for %s %6.1f  MW\n",br2.i.ACbus.name,
		br2.i.ACbus.Pgen);  
}	/* Closes IF (% load > 100) */
}	/* Closes While (% load > 100) */
/*********** may need check for Line/Transformer ************/
/*  if ((rtnsol == SOLVED)&&(r.s.branch.pct_comp < 100)) */  
  if ((rtnsol == SOLVED)&&(r.s.branch.tot_line_load_pct < 100))  
/*********** may need check for Line/Transformer ************/
  {	
  printf("\nThe %s %6.1f  %s %6.1f line IS NOT overloaded\n",
	r.i.branch.bus1_name,r.i.branch.bus1_kv,r.i.branch.bus2_name,
	r.i.branch.bus2_kv);
/*********** may need check for Line/Transformer ************/
  printf("The line overload was %4.1f%%\n",r.s.branch.tot_line_load_pct); 
/*  printf("The line overload was %4.1f%%\n",r.s.branch.pct_comp); */
/*********** may need check for Line/Transformer ************/
  printf("	The Power Generation for %s was %6.1f MW\n",br1.i.ACbus.name,
		br1.i.ACbus.Pgen);
  printf("	The Power Generation for %s was %6.1f MW\n",br2.i.ACbus.name,
		br2.i.ACbus.Pgen);
  }	/* Closes IF (% load < 100) */


/* Selects Bus records */

    pf_init_bus(&br1,TYPE1,bus1,bus_kv1);
    pf_init_bus(&br2,TYPE2,bus2,bus_kv2);

    pf_rec_bus(&br1,"g");  
    pf_rec_bus(&br2,"g");  

/* Verifies branch record ouput and bus record input 	*/
/*  print_branch_soln(&r);				*/
/*  print_bus1_input(&br1);				*/
/*  print_bus2_input(&br2);				*/
 
 pf_cflow_exit();

}	/* CLOSES MAIN */



/* SUBROUTINE TO GET THE % OVERLOAD ON THE LEXINGTN LONGVIEW LINE */
/*************** stuff to make it compile ********************/
void get_branch_overload_pct(pf_rec *r)
/* get_branch_overload_pct(pf_rec *r, int rtncde, int err, float max_pct_ovld) */
/*************** stuff to make it compile ********************/
{
  rtncde = pf_rec_branch(r,"o");
  if(rtncde != CFLOWOK)  {
    printf("Warning - branch data not found\n");
    pf_cflow_exit();  }
  
  max_pct_ovld = MINOVLD;

/* Grabs rest of input data on the Lexingtn Longview 230 line that */
/* was not specified in the PF_INIT_BRANCH statment */

  for (err=pf_rec_branch(r,"f2"); err==0; err=pf_rec_branch(r,"n2"))
    {
/*********** may need check for Line/Transformer ************/
/*    if(r->s.branch.pct_comp > max_pct_ovld)  { */
/*      max_pct_ovld = r->s.branch.pct_comp;       */
    if(r->s.branch.tot_line_load_pct > max_pct_ovld)  {
      max_pct_ovld = r->s.branch.tot_line_load_pct;  
/*********** may need check for Line/Transformer ************/

  }	/* Closes IF (Max % load) */
 }	/* Closes FOR (branch data) */
}	/* Closes Subroutine get_branch_overload_pct */


/* SUBROUTINE TO DISPLAY PF_BRANCH_SOLN */

print_branch_soln(pf_rec *r)
 {
  printf("\nPF_BRANCH_SOLN\n");
  printf("Type = %s\n",r->s.branch.type);
  printf("Num_ckts = %d\n",r->s.branch.num_ckts);
  printf("Pin = %6.1f\n",r->s.branch.Pin);
  printf("Qin = %6.1f\n",r->s.branch.Qin);
  printf("Pout = %6.1f\n",r->s.branch.Pout);
  printf("Qout = %6.1f\n",r->s.branch.Qout);
  printf("Ploss = %6.1f\n",r->s.branch.Ploss);
  printf("Qloss = %6.1f\n",r->s.branch.Qloss);
/*********** may need check for Line/Transformer ************/
/*  printf("Max_flow_amps = %6.1f\n",r->s.branch.max_flow_amps); */
/*  printf("Max_flow_mva = %6.1f\n",r->s.branch.max_flow_mva);   */
/*  printf("Rating_code = %c\n",&r->s.branch.rating_code); */
/*  printf("Percent load = %6.1f\n",r->s.branch.pct_load); */
/*  printf("Rating = %6.1f\n",r->s.branch.rating);         */
/*  printf("Percent_comp = %6.1f\n",r->s.branch.pct_comp); */
  printf("Max_flow_amps = %6.1f\n",r->s.branch.tot_line_load_amps);
  printf("Max_flow_mva = %6.1f\n",r->s.branch.tot_xfmr_load_mva);
  printf("Rating_code = %c\n",&r->s.branch.crit_line_rat_code);
  printf("Percent load = %6.1f\n",r->s.branch.tot_line_load_pct);
  printf("Rating = %6.1f\n",r->s.branch.crit_line_rat_amps);
  printf("Percent_comp = %6.1f\n",r->s.branch.tap1); /* NOTE: Line/Tx shared value */
/*********** may need check for Line/Transformer ************/
  printf("Tap1 = %6.1f\n",r->s.branch.tap1);
  printf("Tap2 = %6.1f\n",r->s.branch.tap2);
 }		/* Closes print_branch_soln Subroutine */

/* SUBROUTINE TO DISPLAY BUS INPUT */

print_bus1_input(pf_rec *br1)
 {
  printf("\nPF_AC_BUS\n");
  printf("Type = 		%s\n",br1->i.ACbus.type);
  printf("Owner = 	%s\n",br1->i.ACbus.owner);
  printf("Name = 		%s\n",br1->i.ACbus.name);
  printf("KV = 		 %6.1f\n",br1->i.ACbus.kv);
  printf("Zone =		%s\n",br1->i.ACbus.zone);
  printf("Pload =		%6.1f\n",br1->i.ACbus.Pload);
  printf("Qload = 	%6.1f\n",br1->i.ACbus.Qload);
  printf("Pshunt = 	%6.1f\n",br1->i.ACbus.Pshunt);
  printf("Qshunt = 	%6.1f\n",br1->i.ACbus.Qshunt);
  printf("Pmax = 		%6.1f\n",br1->i.ACbus.Pmax);
  printf("Pgen = 		%6.1f\n",br1->i.ACbus.Pgen);
  printf("Qsch_Qmax = 	%6.1f\n",br1->i.ACbus.Qsch_Qmax);
  printf("Qmin = 		%6.1f\n",br1->i.ACbus.Qmin);
  printf("Vhold_Vmax =    %6.1f\n",br1->i.ACbus.Vhold_Vmax);
 }  	/* Closes print_bus1_input Subroutine */ 

/* SUBROUTINE TO DISPLAY BUS2 INPUT */

print_bus2_input(pf_rec *br2)
 {
  printf("\nPF_AC_BUS\n");
  printf("Type = 		%s\n",br2->i.ACbus.type);
  printf("Owner = 	%s\n",br2->i.ACbus.owner);
  printf("Name = 		%s\n",br2->i.ACbus.name);
  printf("KV = 		 %6.1f\n",br2->i.ACbus.kv);
  printf("Zone =		%s\n",br2->i.ACbus.zone);
  printf("Pload =		%6.1f\n",br2->i.ACbus.Pload);
  printf("Qload = 	%6.1f\n",br2->i.ACbus.Qload);
  printf("Pshunt = 	%6.1f\n",br2->i.ACbus.Pshunt);
  printf("Qshunt = 	%6.1f\n",br2->i.ACbus.Qshunt);
  printf("Pmax = 		%6.1f\n",br2->i.ACbus.Pmax);
  printf("Pgen = 		%6.1f\n",br2->i.ACbus.Pgen);
  printf("Qsch_Qmax = 	%6.1f\n",br2->i.ACbus.Qsch_Qmax);
  printf("Qmin = 		%6.1f\n",br2->i.ACbus.Qmin);
  printf("Vhold_Vmax =    %6.1f\n",br2->i.ACbus.Vhold_Vmax);
 }  	/* Closes print_bus2_input Subroutine */ 

