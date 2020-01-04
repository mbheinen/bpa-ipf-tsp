/*                swginutil.h
 
  -  Include file for small utility programs to go with the SWGINIT program 
          */
void dpause(void);
char *trim (char *string);
char *blank_fill (char *string,int strg_len);
char *squeeze_blk (char *string);
int ibelong2 (int test, int val1, int val2); 
int ibelong3 (int test, int val1, int val2, int val3); 
int ibelong4 (int test, int val1, int val2, int val3, int val4); 
void fiche_codes (char *date_code, char *usernam, char *account);
long jpinfo (ulong *cputim, ulong *pgflts, ulong *wksetsz, char *usernam, 
             char *account); 
char *date (char *today);

/*  -  end of swginutil.h                  */
