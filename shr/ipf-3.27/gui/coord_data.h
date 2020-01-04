/***********************************************************************\
* Purpose: define BPA coordinate file template
*
* Note:
*  all fields are defined as characters but it may be better to
*  perform initial conversion to integers and floats for the
*  values.
*
*
* Referenced by: base_data.c
*                coord_data.c	(obsolete)
*                edge.c
*                graphbase.c	(obsolete)
*                graphcor.c	(obsolete)
*                graphdata.c
*                graphpscor.c
*                guilib.m
*                libgui.m
*                printopts.c
*                pscordat.c
*                toolbox.c
*
* Author: Dan Clark	December 12, 1991
\***********************************************************************/

#define PSIDRECORD	  "[ID COORD"
#define PSENDRECORD 	  "(*EOR)"
#define PSKEY_BUS 	  "B"
#define PSKEY_TRANSFORMER "T"
#define PSKEY_LINE 	  "L"
#define PSKEY_OPTION 	  "O"

#define PSKEY_DRAW	  "D"
#define PSKEY_COMMENTS 	  "C"
#define PSKEY_DEFINES 	  ">"
#define PSKEY_LEGEND 	  "OP LG"
#define PSKEY_LABEL_BOX	  "OP BX"

#define  CoordOnly      0
#define  CoordAndBase   1


typedef struct {
  char	x[6];
  char	y[6];
} PSPosition;

typedef struct {
  char	x[6];
  char	y[6];
  char  up_dn;
} PSPen_pts;

static enum PsKey {
  psNoOp,
  psID,				/* [ID COORD - beginning of coord records */
  psOptions,			/* O - options for coord drawing/printing */
  psPostScript,			/* P - identifies PostScript record 	  */
  psDefine,			/* >DEFINE record 			  */
  psComment, 			/* C - comments 			  */ 
  psDraw,			/* D - drawing record 			  */
  psBus,			/* B - bus record 			  */
  psBranch,			/* L|T - branch record 			  */
  psArea,			/* A - area record			  */
  psIntertie,			/* I - intertie record			  */
  psTrailer 			/* 9|(*EOR) - end of coord file 	  */
} PSKey;

typedef struct {
  long idx;
  char	key[1];			/* constant B				*/
  char	display[1];		/* display flag				*/
				/* 0 or blank - display bus		*/
				/* 1 - bus not displayed name is	*/
  char	name[8];		/* name of bus shown in powerflow	*/
  char	base[4];		/* base kv of bus			*/
  char	abbr[8];		/* name of bus displayed (abbreviation)	*/
				/* if blank no name OR voltage display	*/
  char	v_flag[1];		/* voltage position relative to bus	*/
				/* 1 over name				*/
				/* 2 right of name			*/
				/* 3 below name				*/
				/* 4 left of name			*/
				/* 5 do not print volt			*/
				/* 6 print volt no name			*/
  PSPosition symbol;		/* x,y of bus symbol 			*/
  PSPosition namepos;		/* x,y coordinate of bus name		*/
				/* x or default to X(bus) + 0.015	*/
				/* y or default to Y(bus) + 0.018	*/
  char	generator_ang[3];	/* angle of generator			*/
  char	reactor_ang[3];		/* angle of reactor 			*/
  char	symbol_shape[2];	/* bus symbol shape identifier 		*/
				/* "  " round symbol 			*/
				/* "HB" Horizontal Bar length = 2 x radius*/
				/* "VB" Vertical Bar length = 2 x radius*/
				/* "??" user defined 			*/
  char	symbol_radius[4];	/* bus symbol shape identifier 		*/
  char  eol[1];
} PSBusRecord; 

#define PSMAXBENDS 5
#define PSMAXPENS 6
typedef struct {
  long	idx;
  char	key[1];			/* constant C		*/
  char	subkey[1];
  char	x[6];			/* X coordinate of comment @	*/
  char	y[6];			/* Y coordinate of comment @	*/
		  		/* @ => blank coordinates either:	*/
				/*    print under previous comment or */
				/* or print starting at x,y or COORd header */
  char	text[76];		/* comment text		*/
} PSCommentRecord;

typedef struct {
  long idx;
  char	key[1];			/* constant L			*/
  char	blank[1];		/* primary/secondary circuit identifier */
  char	name1[8];		/* name of first bus		*/
  char	base1[4];		/* base kv of first bus		*/
  char	name2[8];		/* name of second bus		*/
  char	base2[4];		/* base kv of second bus	*/
  char	circuit_id[1];		/* circuit identifier 		*/
  char	section_id[1];		/* section identifier 		*/
  char	segment[2];		/* segment for annotation with flow */ 
  PSPosition bend[PSMAXBENDS];
  char  eol[1];
} PSBranchRecord;
 
typedef struct {
  long idx;
  char	key[1];			/* constant D				*/
  char	blank[1];		/* r */
  PSPen_pts pt[PSMAXPENS];
} PSDrawRecord;

typedef struct psAnyRecord {
    long idx;
    char key[1];
    char subkey[1];
    char data[88];
    char eol[1];
} PSAnyRecord;

typedef union {
  PSAnyRecord cor;
  PSCommentRecord comment;
  PSBusRecord bus;
  PSBranchRecord branch;
  PSDrawRecord draw;
} PSCoordFileRecord;

static enum COORDKEY 
{ 
  COORD_NONE,  
  COORD_KEY, 
  COORD_NAME1_BASE1,
  COORD_NAME2_BASE2,
  COORD_KEY_NAME1_BASE1,
  COORD_IDX, 
  COORD_KEY_IDX,
  COORD_NAME1_BASE1_NAME2_BASE2
};

extern DBID pscor_db;
extern PSCoordFileRecord *coord_db_branch_search(char *busname1,char *busname2);
extern PSCoordFileRecord *coord_db_bus_search( char *busname );
extern void display_bus_coord_data( char *busname );
extern Boolean psopenCoordFile( char *filename );
extern int psreadCoordData( );
extern Boolean pscloseCoordFile( );
extern Boolean pscloseCoordFile( );
extern int pswriteCoordFile (char *coord_output_file_name);
extern int psbendCount( PSBranchRecord *rec );
extern int pscreatecoordtbl( );
extern int psfindfirstcoordbusrec( PSBusRecord **nextbusrec );


#ifdef REFERENCE_ONLY
/*********************** OLD FORMAT *********************/
/* 
* Note: the structures below define the layout of the old
* coordinate file supported by BPA.  These layouts are often
* useful for supporting new features available to the printing
* subsytem but not yet supported by the graphics system
* at BPA.
*/
typedef struct {
  char	key[1];			/* key field		*/
  char	key_ext[1];		/* key extension field	*/
  char	data[77];		/* character data	*/
  char  eol[1];
} PFANYRECORD;

typedef struct {
  char	key[1];			/* constant C		*/
  char	key_ext[4];		/* constant  OORD	*/
  char  filler1[5];
  char	file_id[9];		/* permits multiple coordinate in file	*/
  char  filler2[1];
  char	width[4];		/* X corner of border (blank for no border) */
  char	height[4];		/* Y corner of border (blank for no border) */
  char	comment_x[4];		/* X corner of comment (blank for none) */
  char	comment_y[4];		/* Y corner of comment (blank for none) */
  char	xscale[4];		/* X-axis scale factor & number height	*/
				/* 0.075 * y_scale = number height	*/
  char	yscale[4];		/* Y-axis scale factor & letter height	*/
				/* 0.075 * x_scale = letter height	*/
} COORDRECORD;

typedef struct {	/* NOT USED */
  char	key[1];			/* constant B		*/
  char	key_ext[5];		/* constant  UBBLE	*/
  char	x_upper_right[4];	/* X corner of border (blank for no border) */
  char	y_upper_right[4];	/* Y corner of border (blank for no border) */
  char	x_comment[4];		/* X corner of comment (blank for none) */
  char	y_comment[4];		/* Y corner of comment (blank for none) */
  char	x_scale[4];		/* X-axis scale factor & number height	*/
				/* 0.085 * y_scale = number height	*/
  char	y_scale[4];		/* Y-axis scale factor & letter height	*/
				/* 0.085 * x_scale = letter height	*/
} BUBBLERECORD;

typedef struct {
  char	key[1];			/* constant C		*/
  char	x[4];			/* X coordinate of comment @	*/
  char	y[4];			/* Y coordinate of comment @	*/
		  		/* @ => blank coordinates either:	*/
				/*    print under previous comment or */
				/* or print starting at x,y or COORd header */
  char	text[70];		/* comment text		*/
} COMMENTRECORD;

typedef struct { 
  char	key[1];			/* constant *		*/
  char	key_ext[7];		/* constant  DEFINE_	*/
  char	type[18];		/* type of quantity to be printed */
				/* OWNER	loss by ownership	*/
				/* AREA		loss by area		*/
				/* ZONE		loss by zone		*/
				/* SYSTEM	total system loss	*/
				/* BRANCH	total branch MW flow	*/
				/* INTERTIE	total computed flow	*/
				/* SCHEDULED_INTERTIE	tot sched flow	*/
  char	field[53];		/* string of values (see table A)	*/
} DEFINERECORD; 

typedef struct {
  char	x[4];			/* X coord of point on line	*/
  char	y[4];			/* Y coord of point on line	*/
  char	pen[1];			/* position of drawing pen (up or down)	*/
				/* 0 Ignore this point	*/
				/* 1 Move to this point with pen down	*/
				/* 2 Move to this point with pen up	*/
} POINTPOSITION;  

typedef struct {
  char	key[1];			/* constant D		*/
  POINTPOSITION	pt[8];	/* x, y and pen style of point	*/
} DRAWRECORD; 

typedef struct {
  char	key[1];			/* constant B		*/
  char	display[1];		/* display flag		*/
				/* 0 or blank - display bus	*/
				/* 1 - bus not displayed name is	*/
  char	name[8];		/* name of bus shown in powerflow	*/
  char	base[4];		/* base kv of bus	*/
  char	abbr[8];		/* name of bus displayed (abbreviation)	*/
				/* if blank no name OR voltage display	*/
  char	v_flag[1];	/* voltage position relative to bus	*/
				/* 1 over name		*/
				/* 2 right of name	*/
				/* 3 below name		*/
				/* 4 left of name	*/
				/* 5 do not print volt	*/
				/* 6 print volt no name	*/
  char	blank[1];		/* unused space */
  char	symbol_x[4];		/* x coordinate of center of bus	*/
  char	symbol_y[4];		/* y coordinate of center of bus	*/
  char	gen_angle[3];		/* angle of generator	*/
  char	name_x[4];		/* x coordinate of bus name	*/
				/* or default to X(bus) + 0.015	*/
  char	name_y[4];		/* y coordinate of bus name	*/
				/* or default to Y(bus) + 0.018	*/
  char	unused[12];
  char	reac_x[4];		/* x coordinate of reactance	*/
  char	reac_y[4];		/* y coordinate of reactance	*/
  char	reac_angle[3];		/* angle of reactance		*/
} BUSRECORD; 

typedef struct {
  char	x[4];
  char	y[4];
} TURNINGPOINT;

typedef struct {
  char	key[1];			/* constant L		*/
  char	style[1];		/* style or type of line	*/
				/* 0 solid line (230KV)		*/
				/* 1 dashed line (115KV)	*/
				/* 2 heavy line (500KV)		*/
  char	name1[8];		/* name of first bus		*/
  char	base1[4];		/* base kv of first bus		*/
  char	name2[8];		/* name of second bus		*/
  char	base2[4];		/* base kv of second bus	*/
  char	arrow_x[4];		/* x coordinate of arrow tip	*/ 
  char	arrow_y[4];		/* y coordinate of arrow tip	*/ 
  char	arrow_ang[3];		/* angle of arrow indicating power flow	*/
  char	flow_x[4];		/* x coord of power flow value	*/ 
				/* if x = -1 do not print value	*/
  char	flow_y[4];		/* y coord of power flow value	*/ 
  char	flow_ang[3];		/* angle to print powerflow value	*/
  TURNINGPOINT	bend[4];
} LINERECORD;

typedef struct { 
  char	key[1];			/* constant T		*/
  char	style[1];		/* style or type of line	*/
				/* 0 solid line (230KV)		*/
				/* 1 dashed line (115KV)	*/
				/* 2 heavy line (500KV)		*/
  char	name1[8];		/* name of first bus		*/
  char	base1[4];		/* base kv of first bus		*/
  char	name2[8];		/* name of second bus		*/
  char	base2[4];		/* base kv of second bus	*/
  char	arrow_x[4];		/* x coordinate of arrow tip	*/ 
  char	arrow_y[4];		/* y coordinate of arrow tip	*/ 
  char	arrow_ang[3];		/* angle of arrow indicating power flow	*/
  char	flow_x[4];		/* x coord of power flow value	*/ 
				/* if x = -1 do not print value	*/
  char	flow_y[4];		/* y coord of power flow value	*/ 
  char	flow_ang[3];		/* angle to print powerflow value	*/
  TURNINGPOINT	bend[2];	/* bends for transformer 	*/
  char	tsymbol_x[4];		/* x coordinate of transformer symbol	*/ 
  char	tsymbol_y[4];		/* y coordinate of transformer symbol	*/ 
  char	symbol_ang[3];		/* angle transformer symbol is from bus	*/
} TRANSFORMERRECORD;

typedef struct { 
  char key[1];
  char filler3[79];
} AREARECORD;

typedef struct {
  char	key[1];
  char	filler4[79];
} INTERTIERECORD;


typedef struct BranchRecord * pBranchRecord;
typedef struct BranchRecord {
    char  key[1];
    char  display[1];
    char  name1[8];               /* name of first bus            */
    char  base1[4];               /* base kv of first bus         */
    char  name2[8];               /* name of second bus           */
    char  base2[4];               /* base kv of second bus        */
} BRANCHRECORD;

static BRANCHRECORD pcbranch;

typedef union {
  char key[1];
  PFANYRECORD pfplot;
  COORDRECORD coord;
  BUBBLERECORD bubble;
  COMMENTRECORD comment;
  DEFINERECORD define;
  DRAWRECORD draw;
  BUSRECORD bus;
  LINERECORD line;
  TRANSFORMERRECORD transformer;
  AREARECORD area;
  INTERTIERECORD intertie;
  BRANCHRECORD branch;
} COORDFILERECORD;
#endif /* REFERENCE ONLY */
