/***********************************************************************
* file: substation.h
* purpose:
* author: Dan Clark	January 03, 1992
*
************************************************************************
*/
#include <stddef.h>
typedef struct {
  char	blank[1];		/* blank field		*/
  char	data[79];		/* character data	*/
} SUBANYRECORD; 

typedef struct {
  char	blank_0[1];
  char	index[3];
  char	blank_1[1];
  char	x[14];
  char	blank_2[2];
  char	y[14];
  char	blank_3[2];
  char	code[4];
  char	blank_4[2];
  char	name[25];
  char	blank_5[2];
} SUBRECORD;

typedef struct Substation *Substationptr;

typedef struct Substation {
  char	code[4];
  char	name[25];
  int	index;
  double	x;
  double	y;
  /* char	filler[1]; */
} SUBSTATIONRECORD;

SUBSTATIONRECORD	*psubrec;

  SUBANYRECORD gen;
typedef union {
  SUBANYRECORD	any;
  SUBSTATIONRECORD	sub;
} SUBSTATIONSCHEMARECORD;

enum COORDKEY 
{ 
  SUB_NONE,
  SUB_CODE, SUB_NAME,
  SUB_X, SUB_Y
};

static SCHEMA sub_schema[] =
{
  { 'n', 'c', sizeof(SUBSTATIONRECORD), 0 },/* size of record stored */
  { 'u', 'c', 4, offsetof(SUBSTATIONRECORD, code)}, /* SUB_CODE 4 char code of substaton name */
			/* u - no dup keys, c - char, 4 length, 0 start pos */
  { 'd', 'c', 25, offsetof(SUBSTATIONRECORD, name)}, 	/* SUB_NAME */
  { 'd', 'd', sizeof(psubrec->x), offsetof(SUBSTATIONRECORD, x )}, /* SUB_X */
  { 'd', 'd', sizeof(psubrec->y), offsetof(SUBSTATIONRECORD, y)}, /* SUB_Y */
};
#define SUB_KEY_CNT 5	/* number of keys entries inj SCHEMA definition */
#define SUB_PARTITIONS 0 

DBID sub_db;
