/***********************************************************************\
* file: ai_data.h
* purpose: define the layout of the area data from cards
* 
* author: Bill Rogers	August 04, 1992
*
************************************************************************/

typedef struct Area {
  char	type[1];		/* "A" of record	*/
  char	subtype[1];		/* of record		*/
  char	chgcde[1];		/* add, delete or modify*/
  char	area[10];		/* area interchange name*/
  char	bus[8];			/* of bus		*/
  char	base[4];		/* kv of bus 		*/
  char  unused1[1];
  char	export[8];		/* scheduled export 	*/
  struct Zone {
    char unusedz[1];
    char id[2];
  } zone[10];
  char  unused2[8];
  char	vmax[4];		/* max p.u. volt 	*/
  char	vmin[4];		/* min p.u. volt 	*/
  char	eol[1];			/* end of line		*/
} AREA;


typedef struct Intertie {
  char	type[1];		/* f "I" of record	*/
  char	unusedi1[1];
  char	chgcde[1];		/* add, delete or modify*/
  char	name1[10];		/* interchange area name 1*/
  char	unusedi2[1];
  char	name2[10];		/* interchange area name 2*/
  char	unusedi3[2];
  char	export[8];		/* sched export for sched interchange */ 
} INTERTIE;

typedef union {
  char     type[1];
  AREA     arec;
  INTERTIE irec;
  char     any[256];
} AIREC;

