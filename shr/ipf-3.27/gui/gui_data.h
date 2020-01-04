/***********************************************************************
* file: gui_data.h
* author: Dan Clark	December 20, 1991
*
************************************************************************
*/
#include <stddef.h>
typedef struct {
  char  type[1];                /* of record            */
  char  subtype[1];             /* of record            */
  char  chgcde[1];              /* add, delete or modify        */
  char  owner[3];               /* id of bus    */
  char  name[8];                /* of bus       */
  char	base[4];                /* kv of bus */
  char  zone[2];                /* bus is located */
  int	x;		/* x coordinate of center of bus	*/
  int	y;		/* y coordinate of center of bus	*/
  float base;                /* kv of bus */
  char	display[1];		/* display flag		*/
				/* 0 or blank - display bus	*/
				/* 1 - bus not displayed name is	*/
  char	abbr[8];		/* name of bus displayed (abbreviation)	*/
				/* if blank no name OR voltage display	*/
} GUIBUSRECORD;

typedef struct {
  int	x1;			/* x position of branch start */
  int	y1;			/* y position of branch start */
  int	x2;			/* x postition of branch end */
  int	y2;			/* y postition of branch end */
  char  type[1];                /* (L, G, C, R...) of record */
  char  subtype[1];             /* of record         */
  char  owner[3];               /* id of bus  */
  char  name1[4];               /* name of first bus  */
  char  base1[4];               /* base kv of first bus       */
  char  name2[4];               /* name of second bus */
  char  base2[4];               /* base kv of second bus      */
  char  circuit[1];             /* cirquit id         */
  char  section[1];             /* section id         */
  float	base1;
  float	base2;
  char	style[1];		/* style or type of line	*/
				/* 0 solid line (230KV)		*/
				/* 1 dashed line (115KV)	*/
				/* 2 heavy line (500KV)		*/
} GUIBRANCHRECORD;

enum GUIKEY 
{ 
  GUI_BUS_NONE, 
  GUI_BUS_NAME1_BASE1,
  GUI_BUS_X, GUI_BUS_Y,
  GUI_BUS_ZONE, GUI_BUS_OWNER,
  GUI_BUS_NAME1, GUI_BUS_BASE1,
  GUI_BUS_NAME2, GUI_BUS_BASE2
};

static SCHEMA coord_sch[] =
{
  { 'n', 'c', sizeof(COORDFILERECORD), 0 },	/* size of record stored */
  { 'd', 'c', 1, 0},	/* COORD_KEY first character in any record */
			/* d - dup keys, c - char, 1 length, 0 start pos */
  { 'd', 'c', 12, 2}, 	/* COORD_NAME1_BASE1 key */
  /* { 'd', 'c', 8, 2}, */ 	/* COORD_NAME1 key */
  /* { 'd', 'c', 4, 10}, */ 	/* COORD_BASE1 key */
};
DBID coord_db;
