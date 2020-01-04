#include  <X11/Intrinsic.h>
#include  <X11/StringDefs.h>
#include  <Xm/Xm.h>

#define PGFMAX 40    /* maximum number of paragraphs in catalog */
#define FONTMAX 40   /* maximum number of fonts in catalog */
#define CONDMAX 10   /* maximum entries in condition catalog */
#define PAGEMAX 100   /* maximum pages in a single file */
#define TRECMAX 1000   /* maximum number of text rectangles per file */
#define MKRMAX 200   /* maximum number of markers in a file */

/* mif conditions */
typedef struct {
  char   CTag[20];
  int    CState;
  int    CStyle;
  int    CSeparation;
} Condition;

/* mif fonts */
typedef struct {
  char   tag[20];
  Font   fontid;
  XFontStruct *fontstruct;
  char   FFamily[20];
  char   FVar[20];
  char   FWeight[20];
  char   FAngle[20];
  double FSize;
  int    FSeparation;
  unsigned FPlain:    1;
  unsigned FBold:     1;
  unsigned FItalic:   1;
  unsigned FUnderline:1;
  unsigned FOverline: 1;
  unsigned FStrike:   1;
  unsigned FSupScript:1;
  unsigned FSubScript:1;
  unsigned FChangeBar:1;
  unsigned FOutline:  1;
  unsigned FShadow:   1;
  unsigned FPairKern: 1;
  unsigned FDoubleUnderline: 1;
  unsigned FNumericUnderline:1;
  int    FDX;
  int    FDY;
  int    FDW;
} MifFont;

typedef struct {
  double i1,i2,i3,i4;
} fmtCOORD;

/* mif text rectangles */
typedef struct {
  int    ID;
  int    pgid;
  int    Pen;
  int    Fill;
  double PenWidth;
  int    Separation;
  int    Angle;
  fmtCOORD  BRect;
  int    TRNext;
} TRECT;

/* mif text markers */
typedef struct {
  int    MType; 
  char   MText[255]; 
  int    MCurrPage;
  fmtCOORD  MRect;
} Marker;   

/* mif pages */
typedef struct {
  int    PageType;
  char   PageNum[10];
  char   PageTag[20];
  fmtCOORD  PageSize;
  int    PageOrientation;
  int    PageBackground;
} Page;

/* mif tab stops */
typedef struct {
  int    TSX;
  int    TSType;
  char   TSLeaderStr[20];
  int    TSDecimalChar;
} TSTOP;

/* mif paragraphs */
typedef struct {
  char   tag[20];
  char   PgfTopSeparator[20];  /* frame above paragraph */
  char   PgfBotSeparator[20];  /* frame below paragraph */
  char   PgfNumFormat[20];
  char   PgfNumberFont[20];
  char   PgfNextTag[20];       /* following pgf tag  */
  unsigned PgfUseNextTag:  1;  /* following pgf flag */
  unsigned PgfWithPrev:    1; 
  unsigned PgfWithNext:    1; 
  unsigned PgfAutoNum:     1; 
  unsigned PgfNumAtEnd:    1; 
  unsigned PgfHyphenate:   1; 
  unsigned PgfLetterSpace: 1; 
  unsigned PgfCellLMarginFixed: 1;
  unsigned PgfCellTMarginFixed: 1;
  unsigned PgfCellRMarginFixed: 1;
  unsigned PgfCellBMarginFixed: 1;
  int    PgfPlacement;
  int    PgfBlockSize;
  int    PgfAlignment;         /* LEFT,CENTER,RIGHT,LEFTRIGHT */
  double PgfFIndent;	       /* first line left margin */
  double PgfLIndent;           /* indent from left of page */
  double PgfRIndent;           /* indent from right of page */
  double PgfSpBefore;          /* space before paragraph    */
  double PgfSpAfter;           /* space after  paragraph    */
  double PgfLeading;           /* space between lines */
  int    PgfFontSize;          /* font size */
  int    PgfFontWeight;        /* PLAIN,BOLD,ITALIC */
  int    PgfFont;              /* font */
  int    PgfLineSpacing;
  int    PgfNumTabs;
  int    HyphenMaxLines;
  int    HyphenMinPrefix;
  int    HyphenMinSuffix;
  int    HyphenMinWord;  
  int    PgfMinWordSpace;
  int    PgfOptWordSpace;
  int    PgfMaxWordSpace;
  int    PgfLanguage;
  int    PgfCellAlignment;
  fmtCOORD  PgfCellMargins;
  TSTOP  *tabs;
} Pgf;
