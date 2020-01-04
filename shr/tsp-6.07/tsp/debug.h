/*             debug.h 
 
  -  Include file for debug file writeout routines.  
      */

/* -  FILE * variable must be declared in main routine */
/* extern FILE *dbg_fl;  */

/*  -  prototypes of debug routines  */
void dbgopen (char *debug_name);
int dbgclose (void);
void cdbgeko (char *text);
void cdbgeko2 (char *text1, char *text2);
void cdbgwri (char *descrip, int ival);
void cdbgwrf (char *descrip, float fval);
void cdbgwrs (char *descrip, char *strval);
void cdbgwrp (char *descrip, void *vptr);

/*  -  end of debug.h           */
