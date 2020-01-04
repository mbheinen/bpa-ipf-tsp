#include <Xm/Xm.h>
typedef enum {
  SlopePos,
  SlopeNeg
} EdgeSlope;
#define DrawBorder 10
#define MINDIMENSION 7
#define MAXLINETHICKNESS 3 
typedef struct {
  int x1;
  int y1;
  int x2;
  int y2;
  GC gc;
} EdgePoint;

typedef struct {
  Position x;
  Position y;
  Dimension width;
  Dimension height;
  EdgeSlope slope;
} EdgeBox;

int line2rect(EdgePoint *pedgept, EdgeBox *pedgebox);
