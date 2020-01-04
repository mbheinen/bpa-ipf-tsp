/*                         SCREEN.C

  -  set of routines for screen output and cursor control using the
     CURSES package  */
#include <curses.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "screen.h"
#include "dem.h"
#include "keystroke.h"
#include "swginutil.h"
#include "debug.h"

/*  -  global variable for cursor stor/recall storage   */
extern POSITION curs_hold[4]; 
/*  -  global var for editing modes (ins/over)  */
extern int edit_mode;

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

int edit_fld (int srow, int scol, int fwid, char* format_p, char* text_p)  {
  /*  -   funct to edit data field.  It shows up in inverse.  The first
          letter starts at (srow,scol).  Format_p holds the editing format.
          Note: format_p may be enlarged to have the same length
          as the text field.  */
  /*  POSITION curs_hold[4];    declared globally  */
  int keyin, curpos, endpos, quitting;
  int fld_color = _REVERSE;         /*  reverse   */
  int norm_color = 0;               /*  normal   */
  char cha, chb, ccount[5];
  int lent, lenf, la, lb, lc;
  char *cpa, *cpb, *curtxt_p, *curfmt_p;
  int insert = 1;
  int debug = 1;
  /*  begin debug  */
  if (debug)  { 
    cdbgeko ("edit_fld - at start - incoming parameters:"); 
    cdbgwrs ("  text   = ",text_p);  
    cdbgwrs ("  format = ",format_p);  
    cdbgwri ("  srow   = ",srow);  
    cdbgwri ("  scol   = ",scol);  
    cdbgwri ("  fwid   = ",fwid);  }
  /* curs_to (23,10);  wrt_txt (norm_color,format_p);
     curs_to (23,30);  wrt_txt (norm_color,text_p);  */
  /*  end debug  */
  /*  -  add trailing blanks to fill field */
  if (debug)  cdbgeko ("  blank filling text");
  blank_fill (text_p,fwid);
  /*  -  force format string to be no shorter than text string  */
  lenf = strlen (format_p);
  if (lenf < fwid)  {
    /*  -  must 'x' fill format  */
    for (la = lenf; la < lent; la++)  format_p[la] = 'x';
    format_p[la] = '\0';  }
  /*  -  set up cursor storage  */
  if (debug)  cdbgeko ("  setting up cursor storage");
  curs_to (srow,scol);
  curs_store (0,srow,scol);       /* start of input field */
  curs_store (1,srow,scol);       /* movable position in input field */
  wrt_txt (fld_color,text_p);
  /*  -  modify field  */
  curs_recall (0);
  curpos = 0;                     /* rel current pos of cursor */
  curtxt_p = text_p;              /* pointer to cursor's postn in text */
  curfmt_p = format_p;            /* pointer to position in format */
  endpos = strlen (text_p) - 1;   /* pos of last character  */
  quitting = false;
  la = -1;
  if (debug)  cdbgeko ("  beginning loop for each character");
  repeat  {
    curs_recall (1);
    wrt_txt (fld_color,curtxt_p);
    curs_recall (1);
    keyin = getkey ();
    if (keyin == esc_key)  keyin = get_esc_code();
    /*  -  test value of key & perform operation  */
    if (keyin >= 32 && keyin < 127) {  /* insert w/ a printable char */
      if (good_format (&keyin, *curfmt_p))  {  /* ins char if it fits format  */
        if (curpos < endpos)  {   /* insert if not at end char */
          if ((edit_mode && 1) == insert)  {
            cpb = text_p; cpb += endpos;
            cpa = cpb;  cpa--;
            cha = *curtxt_p;      /* temp hold first old char */
            *curtxt_p = '\0';     /* set pos to insrt at = '\0' and back-fill */
            for (; (*cpa); cpa--, cpb--)  *cpb = *cpa;
            *cpb = cha;  }  
          *curtxt_p = keyin;
          curs_recall (1);
          wrt_txt (fld_color,curtxt_p);
          /* advance all pointers */
          curpos++;
          curtxt_p++; curfmt_p++;
          curs_change (1,0,1);  }
        else  {     /* just overstrike insert if at end char */
          *curtxt_p = keyin;  }  }
      else { }  /* do nothing if keyin doesn't fit format */
      restart_loop;  }
    if (keyin == kpcom_key)  {     /* shift rest of chars to left  */
      cpa = curtxt_p;
      cpb = cpa; cpb++;
      for (; (*cpb); cpa++, cpb++)  {
        *cpa = *cpb; }
      *cpa = ' ';
      restart_loop; }
    if (keyin == del_key)  {     /* shift this & rest of chars to left  */
      if (curpos > 0)  {
        cpa = curtxt_p;
        cpb = cpa; cpa--;
        for (; (*cpb); cpa++, cpb++)  {
          *cpa = *cpb; }
        *cpa = ' ';
        /* retreat all pointers  */
        curpos--;
        curtxt_p--;  curfmt_p--;
        curs_change (1,0,-1);  }
      restart_loop; }
    if (keyin == lf_key)  {  /* move cursor one char left  */
      if (curpos > 0)  {
        curpos--;  curtxt_p--;  curfmt_p--;  
        curs_change (1,0,-1);  }
      restart_loop;  }
    if (keyin == rt_key)  {  /* move cursor one char right */
      if (curpos < endpos)  {
        curpos++;  curtxt_p++;  curfmt_p++; 
        curs_change (1,0,1);  }
      restart_loop;  }
    if (keyin == kp1_key)  {  /* move cursor to start position */
      curpos = 0;
      curs_recall (0);  curs_copy (1,0);
      curtxt_p = text_p; curfmt_p = format_p;
      restart_loop;  }
    if (keyin == kp2_key)  {  /* move cursor to end position */
      curpos = endpos;
      curs_copy (1,0);  curs_change (1,0,endpos); 
      curtxt_p = text_p;  curtxt_p += endpos;
      curfmt_p = format_p;  curfmt_p += endpos;
      restart_loop;  }
    if (keyin == kp0_key)  {  /*  toggle insert/overstrike  */
      edit_mode ^= insert; 
      if ((edit_mode && 1) == insert)  {
        curs_to (23,30);  wrt_txt (norm_color,"[insert]    ");  }
      else  {
        curs_to (23,30);  wrt_txt (norm_color,"[overstrike]");  }
      restart_loop;  }

    if (ibelong4 (keyin,tab_key,bksp_key,up_key,dn_key)) { 
      /* quit doing mods */
      quitting = true;
      quit_loop; }
    if (ibelong3 (keyin,rtn_key,kpmin_key,ctl_y_key)) { 
      /* quit doing mods */
      quitting = true;
      quit_loop; }
    if (ibelong3 (keyin,pf2_key,pf3_key,pf4_key)) { 
      /* quit doing mods */
      quitting = true;
      quit_loop; }
    /*  -  merely loop back if key is not an allowed one */
    } aslongas (! quitting);
    /*  -  end of key reading loop.  */
  /*  -  get rid of trailing blanks */
  trim (text_p);
  return keyin;
  }  /*  -  end of edit_fld ()   */

/*  -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=-    -=-   */

void wrt_txt (int color, char *text)  {
  /*  -  Displays text starting at (row,col) with specified color attrib.
         Color is set by ANSI esccape sequence.
         At end, cursor is one column past last char written  */
  /*                  */
  setattr (color);        /*  set character attrib */
  addstr (text);
  refresh (); 
  clrattr (color);
  return;
  }  /* end of wrt_txt */

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

int sho_fld (int srow, int scol, int fwid, char* text_p)  {
  /*  -   funct to display a data field with no user mod allowed.  It 
          shows up in inverse.  Right-side blanks are shown if the text
          is shorter than the field width. 
      -   Note:  no refreshing is done, since this routine will be 
          called many times in a row.  */
  int la, lna;
  /*                  */
  setattr (_REVERSE);
  move (srow,scol);
  lna = strlen (text_p);
  addstr (text_p);
  if (lna >= fwid)  {
    clrattr (_REVERSE);  return 1;  }
  for (la = lna; la < fwid; la++) addch (' ');
  clrattr (_REVERSE);
  return 0;  }

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

int sho_desc (int srow, int scol, char* text_p)  {
  /*  -   funct to display a field descrip.  It shows up in normal.  
      -   Note:  no refreshing is done, since this routine will be 
          called many times in a row.  */
  int la, lna;
  /*                  */
  setattr (0);
  move (srow,scol);
  addstr (text_p);
  return 0;  }

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

int sho_bold (int srow, int scol, char* text_p)  {
  /*  -   funct to display a data field with no user mod allowed.  It 
          shows up in inverse.  Right-side blanks are shown if the text
          is shorter than the field width. 
      -   Note:  no refreshing is done, since this routine will be 
          called many times in a row.  */
  int la, lna;
  /*                  */
  setattr (_BOLD);
  move (srow,scol);
  addstr (text_p);
  clrattr (_BOLD);
  return 0;  }

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

int good_format (int *key_p, char fmt_c)  {
  /*  -  Checks of the key input at this position can match the format of
         that position.  Certain matches may cause the key to be modified.
            */
  char cha, chb;
  /*                  */
  cha = *key_p;
  /* - 'x' allows any char */
  if (fmt_c == 'x' || fmt_c == 'X') return 1;
  /* - '!' allows any char but forces to upper case */
  if (fmt_c == '!') {
    if ((cha >= 'a') && (cha <= 'z')) (*key_p) -= 32;
    return 1; }
  /* - '9' allows only digits  */
  if (fmt_c == '9')  {
    if ((cha >= '0') && (cha <= '9'))  return 1; }
  /* - here if format violated */
  return 0;
  }  /*  end of good_format  */


/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

void clrscr(void)  {
  /*  -  clears the screen and puts cursor at home postion  */
  clear ();  return;  }

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

void curs_copy (int destc, int origc)  {
  /*  -  copies one saved cursor position to another slot */
  if (destc < 0)  destc = 0;
  if (destc > 3)  destc = 3;
  if (origc < 0)  origc = 0;
  if (origc > 3)  origc = 3;
  curs_hold[destc].row =  curs_hold[origc].row;
  curs_hold[destc].col =  curs_hold[origc].col;
  return;  }

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

void curs_init (int rowin, int colin)  {
  /*  -  INITIALIZER: sets all four storable cursor positions to the
         input values.  */
  int la;
  /*         */
  for (la = 0; la <= 3; la++)  {
    curs_hold[la].col = colin;
    curs_hold[la].row = rowin;  }
  return;  }

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

void curs_to (int rowin, int colin)  {
  /*  -  moves cursor to specified row & column in text window */
  /* printf ("%c[%d;%dF",0x1b,rowin,colin);  */
  move (rowin,colin);  refresh(); 
  return;  }

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

void curs_change (int posnum, int rowby, int colby)  {
  /* - recalls saved cursor position, adds the offset row & column,
  /*   stores back new position, then moves cursor there  */
  if (posnum < 0)  posnum = 0;
  if (posnum > 3)  posnum = 3;
  curs_hold[posnum].col += colby;
  curs_hold[posnum].row += rowby;
  curs_to (curs_hold[posnum].row, curs_hold[posnum].col);
  return;  }

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

void curs_store (int posnum, int rowin, int colin)  {
  /*  -  Stores given cursor position into one of the curs_hold slots.
         Slot numbers allowed are 0 to 3.  If pos_num is out-of-range, the
         nearest limit is used.    */
  if (posnum < 0)  posnum = 0;
  if (posnum > 3)  posnum = 3;
  curs_hold[posnum].col = colin;
  curs_hold[posnum].row = rowin;
  return;  }

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

void curs_recall (int posnum)  {
  /*  -  Recalls saved cursor position from one of the curs_hold slots.
         Slot numbers allowed are 0 to 3.  If pos_num is out-of-range, the
         nearest limit is used.    */
  if (posnum < 0)  posnum = 0;
  if (posnum > 3)  posnum = 3;
  curs_to (curs_hold[posnum].row, curs_hold[posnum].col);
  return;  }

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */
