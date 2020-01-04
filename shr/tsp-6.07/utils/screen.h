/*        screen.h 

  -  include file for screen output and cursor control using the 
     CURSES package  */

/* -  cursor stor/recall storage   */
#ifndef position_def
#define position_def
typedef struct { int row; int col; } POSITION;
#endif

/*                                     */
void curs_init (int rowin, int colin);
void curs_to (int rowin, int colin);
void curs_change (int posnum, int rowin, int colin);
void curs_store (int posnum, int rowin, int colin);
void curs_recall (int posnum);
void curs_copy (int destc, int origc);
void clrscr(void);
void wrt_txt (int color, char *text);
int good_format (int *key_p, char fmt_c);
int edit_fld (int srow, int scol, int fwid, char* format_p, char* text_p);
int sho_fld (int srow, int scol, int fwid, char* text_p);
int sho_desc (int srow, int scol, char* text_p);
int sho_bold (int srow, int scol, char* text_p);

/*  -  end of screen.h                 */
