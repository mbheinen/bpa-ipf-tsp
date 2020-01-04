/***********************************************************************
* file: base_data.h
* purpose: define the layout of the base data from cards
* 
* author: Dan Clark	December 12, 1991
*
* note:
*  all fields are defined as characters
*
************************************************************************
*/
typedef struct PFBus {
  char	type[1];		/* of record		*/
  char	subtype[1];		/* of record		*/
  char	chgcde[1];		/* add, delete or modify	*/
  char	owner[3];		/* id of bus	*/
  char	name[8];		/* of bus	*/
  char	base[4];		/* kv of bus */
  char	zone[2];		/* bus is located */
  char	data[63];		/* character data	*/
  char	eol[1];			/* end of line		*/
} PFBUS;


typedef struct PFBranch *PFBranch;

typedef struct PFBranch {
  char	type[1];		/* f "L" or "T"  of record		*/
  char	subtype[1];		/* f  of record		*/
  char	chgcde[1];		/* o add, delete or modify	*/
  char	owner[3];		/* o id of bus	*/
  char	name1[8];		/* f name of first bus	*/
  char	base1[4];		/* f base kv of first bus	*/
  char	meter[1];		/* o tie line metering point flag */
  char	name2[8];		/* f name of second bus	*/
  char	base2[4];		/* f base kv of second bus	*/
  char	circuit[1];		/* f cirquit id		*/
  char	section[1];		/* f section id		*/
  char	eol[1];			/* end of line		*/
} PFBRANCH;

/* "B " bus has no voltage control since Q is held constant */
typedef struct {
  char	type[1];		/* r "B" of record		*/
  char	subtype[1];		/* r " " of record		*/
  char	chgcde[1];		/* o add, delete or modify	*/
  char	owner[3];		/* o id of bus	*/
  char	name[8];		/* r of bus	*/
  char	base[4];		/* r kv of bus */
  char	zone[2];		/* r bus is located */
  char	Pload[5];		/* o MW real power held constant solution */
  char	Qload[5];		/* o MVAR reactive power constant solution */
  char	shunt_load[4];		/* o MW */
  char	shunt_cap[4];		/* o MVAR Capacitance = + Reactance = - */
  char	Pmax[4];		/* o MW maximum power */
  char	Pgen[5];		/* o MW generation power */
  char	Qsched[5];		/* o MVAR scheduled reactive power */
  char	Qmin[5];		/* o MVAR minimum reactive */
  char  unused[22];
  char	eol[1];			/* end of line		*/
} PFBUS_B; 

/* "BE" bus hold the voltage to specified value regardless of reactive */
typedef struct {
  char	type[1];		/* r "B" of record		*/
  char	subtype[1];		/* r "E" of record		*/
  char	chgcde[1];		/* o add, delete or modify	*/
  char	owner[3];		/* o id of bus	*/
  char	name[8];		/* r of bus	*/
  char	base[4];		/* r kv of bus */
  char	zone[2];		/* r bus is located */
  char	Pload[5];		/* o MW real power held constant solution */
  char	Qload[5];		/* o MVAR reactive power constant solution */
  char	shunt_load[4];		/* o MW */
  char	shunt_cap[4];		/* o MVAR Capacitance = + Reactance = - */
  char	Pmax[4];		/* o MW maximum power */
  char	Pgen[5];		/* o MW generation power */
  char	Qmax[5];		/* o MVAR scheduled reactive power */
  char	Qmin[5];		/* o MVAR minimum reactive */
  char	Vhold[4];		/* r MVAR minimum reactive */
  char	unused[20];
  char	eol[1];			/* end of line		*/
} PFBUS_BE; 

/* "BQ" bus hold the voltage to specified value within reactive limits */
typedef struct {
  char	type[1];		/* r "B" of record		*/
  char	subtype[1];		/* r "Q" of record		*/
  char	chgcde[1];		/* o add, delete or modify	*/
  char	owner[3];		/* o id of bus	*/
  char	name[8];		/* r of bus	*/
  char	base[4];		/* r kv of bus */
  char	zone[2];		/* r bus is located */
  char	Pload[5];		/* o MW real power held constant solution */
  char	Qload[5];		/* o MVAR reactive power constant solution */
  char	shunt_load[4];		/* o MW */
  char	shunt_cap[4];		/* o MVAR Capacitance = + Reactance = - */
  char	Pmax[4];		/* o MW maximum power */
  char	Pgen[5];		/* o MW generation power */
  char	Qmax[5];		/* r MVAR scheduled reactive power */
  char	Qmin[5];		/* r MVAR minimum reactive */
  char	Vhold[4];		/* r MVAR minimum reactive */
  char	unused[20];
  char	eol[1];			/* end of line		*/
} PFBUS_BQ; 

/* "BS" bus system swing or slack bus */
typedef struct {
  char	type[1];		/* r "B" of record		*/
  char	subtype[1];		/* r "S" of record		*/
  char	chgcde[1];		/* o add, delete or modify	*/
  char	owner[3];		/* o id of bus	*/
  char	name[8];		/* r of bus	*/
  char	base[4];		/* r kv of bus */
  char	zone[2];		/* r bus is located */
  char	Pload[5];		/* o MW real power held constant solution */
  char	Qload[5];		/* o MVAR reactive power constant solution */
  char	shunt_load[4];		/* o MW */
  char	shunt_cap[4];		/* o MVAR Capacitance = + Reactance = - */
  char	Pmax[4];		/* o MW maximum power */
  char	unused1[5];		/* */
  char	Qmax[5];		/* o MVAR scheduled or max reactive power */
  char	Qmin[5];		/* o MVAR minimum reactive */
  char	Vhold[4];		/* r MVAR minimum reactive */
  char	Vang[4];		/* r degrees of angle for swing bus */
				/* note: implied decimal xxx.x */
  char	unused[14];
  char	eol[1];			/* end of line		*/
} PFBUS_BS;

/* Line data */
typedef struct {
  char	type[1];		/* f "T" of record		*/
  char	subtype[1];		/* f " " of record		*/
  char	chgcde[1];		/* o add, delete or modify	*/
  char	owner[3];		/* o id of bus	*/
  char	name1[4];		/* f name of first bus	*/
  char	base1[4];		/* f base kv of first bus	*/
  char	meter[1];		/* o tie line metering point flag */
  char	name2[4];		/* f name of second bus	*/
  char	base2[4];		/* f base kv of second bus	*/
  char	circuit[1];		/* f cirquit id		*/
  char	section[1];		/* f section id		*/
  char	rating[4];		/* o MVA total rating	*/
  char	ckt_cnt[1];		/* o number of parallel transformer banks */
  char	ZpiR[6];		/* o Total per unit impedance */
  char	ZpiX[6];		/* o Total per unit impedance */
  char	YpiG[6];		/* o Per unit G of iron losses and mag curr */
  char	YpiB[6];		/* o per unit B of losses */
  char	tap1[5];		/* o fixed bus 1 tap or phase shift in deg */
  char	tap2[5];		/* o fixed bus 2 tap of fixed phase shift */
  char	unused1[2];	
  char	datein_mth[1];		/* o energization date (1,2..9,O,N,D) */
  char	datein_yr[2];
  char	dateout_mth[1];		/* o de-energization date */
  char	dateout_yr[2];
  char	thermal_rate[4];	/* o MVA thermal rating */
  char	loss_rate[4];		/* o MVA loss of life */
  char	bttl_neck_rate[4];	/* o MVA bottleneck rating */
  char	eol[1];			/* end of line		*/
} PFLINE;

/* transformer data */
typedef struct {
  char	type[1];		/* f "T" of record		*/
  char	subtype[1];		/* f " " of record		*/
  char	chgcde[1];		/* o add, delete or modify	*/
  char	owner[3];		/* o id of bus	*/
  char	name1[4];		/* f name of first bus	*/
  char	base1[4];		/* f base kv of first bus	*/
  char	meter[1];		/* o tie line metering point flag */
  char	name2[4];		/* f name of second bus	*/
  char	base2[4];		/* f base kv of second bus	*/
  char	circuit[1];		/* f cirquit id		*/
  char	section[1];		/* f section id		*/
  char	rating[4];		/* o MVA total rating	*/
  char	ckt_cnt[1];		/* o number of parallel transformer banks */
  char	ZpiR[6];		/* o Total per unit impedance */
  char	ZpiX[6];		/* o Total per unit impedance */
  char	YpiG[6];		/* o Per unit G of iron losses and mag curr */
  char	YpiB[6];		/* o per unit B of losses */
  char	tap1[5];		/* o fixed bus 1 tap or phase shift in deg */
  char	tap2[5];		/* o fixed bus 2 tap of fixed phase shift */
  char	unused1[2];	
  char	datein_mth[1];		/* o energization date (1,2..9,O,N,D) */
  char	datein_yr[2];
  char	dateout_mth[1];		/* o de-energization date */
  char	dateout_yr[2];
  char	thermal_rate[4];	/* o MVA thermal rating */
  char	loss_rate[4];		/* o MVA loss of life */
  char	bttl_neck_rate[4];	/* o MVA bottleneck rating */
  char	eol[1];			/* end of line		*/
} PFTRANSFORMER;

typedef union {
  char	type[1];
  PFBUS	bus;
  PFBUS_B	bus_b;
  PFBUS_BQ	bus_bq;
  PFBUS_BE	bus_be;
  PFBUS_BS	bus_bs;
  PFBRANCH	branch;
  PFLINE	line;
  PFTRANSFORMER	transformer;
  char any[256];
} PFBASERECORD;

static PFBRANCH		*pbranch;

enum BSEKEY
{ 
  BSE_NONE,  BSE_TYPE, 
  BSE_NAME1_BASE1,
  BSE_NAME2_BASE2,
  BSE_TYPE_NAME1_BASE1,
  BSE_BRANCH
};

extern DBID base_db;
