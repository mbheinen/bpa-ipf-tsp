typedef enum err_type { FATAL=(1L<<0),INFO=(1L<<1),WARNING=(1L<<2) }ErrType;

typedef struct {
  char *file;   /* name of file where error occurred */
  long line;    /* line number where error occurred */
  char *msg;    /* error message to display in dialog box */
  char *link;   /* hyperteext name, this is used if additional info is req */
  ErrType type;    /* error type from err_type list */
  char *ident;  /* SCCS identifier */
} emERROR;

#define EM_LINE __LINE__
#define em_init static emERROR err={__FILE__,0,"","",0,""};

extern void em_show( emERROR * );

static char errmsg[1024];
