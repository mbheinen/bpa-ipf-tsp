#define CHGMAXKEY 64
#define CHGMAXDATA 128

typedef struct ChgAny {
 long idx;
 long cde;
 char key[CHGMAXKEY];
 char data[CHGMAXDATA];
} CHGANY;

typedef struct ChgXY {
  long idx;
  long cde;
  char key[CHGMAXKEY];
  int oldx;
  int oldy;
  int newx;
  int newy;
} CHGXY;

typedef union Changes {
  CHGANY chgany;
  CHGXY  chgxy;
} CHANGES;
  

enum AIKEY {
  CHG_NONE,
  CHG_IDX
};

enum Code {
  CHGCDE_NONE,
  CHGCDE_SYMBOL_XY,
  CHGCDE_NAME_XY
};
