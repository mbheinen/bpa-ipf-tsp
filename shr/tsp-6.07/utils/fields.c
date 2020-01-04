/*     fields  |
---------------+

  -  Set of subroutines to display and operate on data entry display fields.
                     */
#include <curses.h>
#include <stdio.h> 
#include <stdlib.h>
#include <unixlib.h>
#include <string.h>
/*  -  keystroke.h (in local directory) contains structure templates, system 
       includes and key code defs for reading individual keystrokes  */
#include "keystroke.h"
#include "fields.h"

#define NEVER 0
#define ALWAYS 1
#define TRUE 1
#define FALSE 0
#define loop_when while
#define repeat do
#define aslongas while
#define restart_loop continue
#define quit_loop break
typedef unsigned long ulong;
typedef unsigned short ushort;
/*                             */
/* Various video attributes
#define A_STANDOUT      0x80000000
#define A_UNDERLINE     0x40000000
#define A_REVERSE       0x20000000
#define A_BLINK         0x10000000
#define A_DIM           0x08000000
#define A_BOLD          0x04000000
                                    */
/*  -  key definitions in keystroke.h */

/* - Field structure from fields.h * /
  typedef struct {
    char label[32], value[80], editmask[80], dbase_id[12];
    int lab_len, val_dat_len, val_sho_len, val_sho_start;
    int labr, labc, valr, valc;     / * row & column positions * /
    } FIELD;      */

/* -  global variables for cursor stor/recall functions   */
extern POSITION curs_hold[4];

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void movetorc (int row, int col)  {
  /*  moves text cursor to specific row & column  (1,1 is NW corner) */
   move (row-1,col-1);  return;  
  }

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void movebyrc (int rowby, int colby)  {
  /*  moves text cursor by a specific row & column amount.  */
  int ir,ic;
  getyx (stdscr,ir,ic);
  move (ir + rowby, ic + colby);
  return;
  }

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void normal_color () {
    /*   attrset (color);  return;  }            UNIX version  */
  clrattr (_BOLD | _REVERSE | _BLINK | _UNDERLINE); 
  return;  }                                   /*  VAX version  */
  
void label_color () {
  setattr (_BOLD);
  clrattr (_REVERSE | _BLINK | _UNDERLINE);
  return;
  } 

void value_sho_color ()  {
  setattr (_UNDERLINE);
  clrattr (_BOLD | _REVERSE | _BLINK);
  return;
  } 

void value_edit_color () {
  setattr (_REVERSE);
  clrattr (_BOLD | _BLINK | _UNDERLINE);
  return;
  } 

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

FIELD *NewField (char *ulabel, int lbl_row, int lbl_col, char *edt_mask,
  int val_max_len, int val_disp_len, int val_row, int val_col)  {
  /*  -  Constructs a new field using heap memory  */
  FIELD *newfld;   int ilen, la;
  char *cpa, *cpb;
  /*    */
  newfld = malloc (sizeof (FIELD));
  strcpy (newfld->label,ulabel);
  newfld->lab_len = strlen (ulabel);
  newfld->labr = lbl_row;
  newfld->labc = lbl_col;
  newfld->valr = val_row;
  newfld->valc = val_col;
  /* - Set dbase_id to blank (may remove from struct eventually)  */
  strcpy (newfld->dbase_id,"           ");
  /*            */
  if (val_max_len > 79) val_max_len = 79;
  if (val_max_len < 1) val_max_len = 1;
  newfld->val_dat_len = val_max_len;
  newfld->val_sho_len = (val_disp_len < val_max_len) ? val_disp_len :
      val_max_len;
  strncpy (newfld->editmask,edt_mask,val_max_len);
  newfld->editmask[val_max_len] = '\0';
  ilen = strlen (edt_mask);
  /* - if edit mask is shorter than its declared length, then
       extend last mask char to rest of length */
  if (ilen < val_max_len)  {
    cpb = newfld->editmask + ilen;
    cpa = cpb - 1;
    for (la = ilen; la < val_max_len; la++)  {
      *cpb = *cpa;  cpb++;
      }
    }
  /* - Initially set value to all blanks  */
  cpa = newfld->value;
  for (la = 0;  la < val_max_len; la++)  {
    *cpa = ' ';  cpa++;
    }
  *cpa = '\0';
  newfld->val_sho_start = 0;   /* offset of first displayed char */
  return newfld;
  }  /*  end of NewField  */

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

char *GetFieldValue (FIELD *field_p, char *dest)  {
  /*  -  Puts the value of the field into the destination string.
         Return value is dest, so function can be used
         as a printable object  */
  dest[field_p->val_dat_len] = '\0';
  return strncpy (dest,field_p->value,field_p->val_dat_len);
  }

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

int SetFieldValue (FIELD *field_p, char *src)  {
  /* - Sets the current field value with the source string.
       Value is truncated or blank-filled to length val_dat_len
       (in structure).
       */
  int ia, ib, la, vlen;
  char *cpa,*cpb,*fpval;
  /*      */
  vlen = field_p->val_dat_len;
  fpval = field_p->value;
  strncpy (fpval,src,vlen);
  fpval[vlen] =  (char) 0;    /* - truncation */
  ia = strlen (fpval);
  if (ia == vlen) return 0;   /* - no blank-fill needed */
  cpa = fpval + ia;
  for (la = ia; la < vlen; la++)  {
    *cpa = ' ';  cpa++;
    }
  return 0;
  }  /*  end of SetFieldValue  */

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

int ShowFldValue (FIELD *field_p)  {
  /* - Displays the field value as read-only, but does not redisplay the
       label.  */
  int ia, ib, la, lb;  char *cpa;
  /*      */
  value_sho_color ();
  movetorc (field_p->valr,field_p->valc);
  /* - display at offset if short display length */
  cpa = field_p->value + field_p->val_sho_start;
  for (la = field_p->val_sho_len; la; la--)  {
    addch (*cpa);  cpa++;
    }
  normal_color ();
  return 0;
  }

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

int ShowField (FIELD *field_p)  {
  /* - Displays the field's label and value as read-only  */
  label_color ();
  movetorc (field_p->labr,field_p->labc);
  addstr (field_p->label); 
  ShowFldValue (field_p);
  return 0;
  }

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

int EditField (FIELD *field_p, int *mode_p, char *new_val)  {
  /* - Lets user edit the value part of a field.  Allowed moves are
       right & left one char, and right and left ends.  Allowed edits
       are insert a char + current and previous character delete.
       Overstrike mode not supported.  The mask is enforced at each new
       character position.  Quit chars are [rtn] [enter] [up] [down] [tab] 
       [bak_tab] [kp-] [pf2] [^Y] and [^C].  [kp3] restores the pre-edit value 
       but does not quit from editing.  Returns user's quit char.  Code 
       borrowed fieldinp.c .  Allows value text to slide in display area if
       it is shorter than allowed value length.  Mode_p indicates if editing
       mode is insert or overstrike.  
            */
  void StoreCursor (int posnum);
  void RecallCursor (int posnum);
  int ibelong6 (int test, int val1, int val2, int val3, int val4,
                int val5, int val6);
  int GoodFormat (int *key_p, char fmt_c);
  int last_non_blk_pl1 (char *text);
  /* ^--  these 5 known only to this subrtn */
  char *curtxt_p, *curfmt_p, *cpa, *cpb, *cpdisp;
  char cha, old_val[80];
  int ia, la, lb, keyin, errrow, errcol, quitting, sho_start;
  int curvapos, endvapos, curdipos, enddipos;  /* val & disp positions */
  /*     */
  quitting = FALSE;
  errrow = 24;  errcol = 3;
  label_color ();
  movetorc (field_p->labr,field_p->labc);
  addstr (field_p->label);  
  value_edit_color ();
  movetorc (field_p->valr,field_p->valc);
  refresh ();
  StoreCursor (0);                 /* start of display */
  StoreCursor (2);
  /* - display at offset if short display length */
  cpdisp = field_p->value + field_p->val_sho_start;
  for (la = field_p->val_sho_len; la; la--)  {
    addch (*cpdisp);  cpdisp++;
    }
  /* - begin editing  */
  strcpy (old_val,field_p->value);
  movetorc (errrow, errcol);
  StoreCursor (1);                 /* start of error status */
  movetorc (24,31);
  normal_color ();
  if (*mode_p == INSERT)  {     /*  display mode  */
    addstr ("  [insert]  ");  }
  else if (*mode_p == OVERSTRIKE)  {
    addstr ("[overstrike]");  }
  refresh ();
  value_edit_color ();
  /*  -  modify field  */
  sho_start = field_p->val_sho_start;
  RecallCursor (0);
  curdipos = 0;                    /* current display pos of cursor */
  curvapos =  curdipos + sho_start;
                                   /* current value pos of cursor */
  curtxt_p = field_p->value + curvapos;
                                   /* pointer to cursor's postn in text */
  curfmt_p = field_p->editmask + curvapos;
                                   /* pointer to position in format */
  endvapos = field_p->val_dat_len - 1;   /* pos of last val character  */
  enddipos = field_p->val_sho_len - 1;   /* pos of last disp character  */
  /* - temp debug by showing mask */
  /* value_sho_color ();
     RecallCursor (0);  movebyrc (1,0);
     cpdisp = field_p->editmask + field_p->val_sho_start;
     for (la = curdipos - 1; la < enddipos; la++)  {
       addch (*cpdisp);  cpdisp++;
       }
     value_edit_color ();   */
  /*    */
  repeat  {
    /* - Determine if value must be shifted in a short display area.  If
         so, start position and cursor position must be recalculated.  */
    if (curdipos < 0)  {    /* must shift left */
      curdipos = 0;
      sho_start = curvapos;         /* first value char displayed */
      RecallCursor (0);
      refresh ();
      StoreCursor (2);
      }
    if (curdipos >= enddipos)  {    /* must shift right */
      curdipos = enddipos;
      sho_start = curvapos - enddipos;
      if (sho_start < 0) {
        sho_start = 0;
        curdipos = curvapos;
        }
      RecallCursor (0);
      cpdisp = field_p->value + sho_start;
      value_edit_color ();
      for (la = curdipos; la; la--)  {
        addch (*cpdisp);  cpdisp++;
        }
      refresh ();
      StoreCursor (2);
      }
    /* - Show rest of value from cursor position */
    value_edit_color ();
    RecallCursor (2);
    cpdisp = curtxt_p;
    for (la = curdipos - 1; la < enddipos; la++)  {
      addch (*cpdisp);  cpdisp++;
      }
    refresh ();
    /* - debug position numbers for display & text 
      label_color ();
      RecallCursor (1);
      printw ("<%02d:%02d>",curdipos,curvapos); 
      value_edit_color ();
        - end debug */
    RecallCursor (2);
    refresh ();
    keyin = getkey ();
    if (keyin == esc_key)  keyin = get_esc_code();
    /* - debug value of input key  
      label_color ();
      RecallCursor (1);  movebyrc (-1,0);
      printw ("<%d>    ",keyin);
      refresh (); 
      value_edit_color ();
       - end debug */
    /*  -  test value of key & perform operation  */
    if (keyin >= 32 && keyin < 127) {  /* insert w/ a printable char */
      ia = GoodFormat (&keyin, *curfmt_p);
      /*                  */
      if (GoodFormat (&keyin, *curfmt_p))  {  /* ins char if it fits format  */
        if (curvapos < endvapos)  {  /*  not at end char  */  
          if (*mode_p == INSERT)  {   /* in insert mode */
            cpb = field_p->value + endvapos;
            cpa = cpb;  cpa--;
            cha = *curtxt_p;      /* temp hold first old char */
            *curtxt_p = '\0';     /* set pos to insrt at = '\0' and back-fill */
            for (; (*cpa); cpa--, cpb--)  *cpb = *cpa;
            *cpb = cha;
            }
          *curtxt_p = keyin;
          RecallCursor (2);
          value_edit_color ();
          cpdisp = curtxt_p;
          /* advance all pointers */
          addch (*cpdisp);  cpdisp++;
          curvapos++;  curdipos++;
          curtxt_p++; curfmt_p++;
          RecallCursor (2);  movebyrc (0,1);    
          refresh ();  
          StoreCursor (2); }
        else  {     /* just overstrike if at end char */
          *curtxt_p = keyin;  }  }
      else { }  /* do nothing if keyin doesn't fit format */
      restart_loop;  }
    if (keyin == kpcom_key)  {     /* shift rest of chars to left  */
      cpa = curtxt_p;
      cpb = cpa; cpb++;
      for (; (*cpb); cpa++, cpb++)  {
        *cpa = *cpb; }
      *cpa = ' ';                 /* turn empty last char to a blank */
      restart_loop; }
    if (keyin == del_key)  {     /* shift this & rest of chars to left  */
      if (curvapos > 0)  {
        cpa = curtxt_p;
        cpb = cpa; cpa--;
        for (; (*cpb); cpa++, cpb++)  {
          *cpa = *cpb; }
        *cpa = ' ';
        /* retreat all pointers  */
        curvapos--;  curdipos--;
        curtxt_p--;  curfmt_p--;
        RecallCursor (2);  movebyrc (0,-1);  
        refresh ();
        StoreCursor (2); 
        }
      restart_loop; 
      }
    if (keyin == lf_key)  {  /* move cursor one char left  */
      if (curvapos > 0)  {
        curvapos--;  curdipos--;
        curtxt_p--;  curfmt_p--;
        RecallCursor (2);  movebyrc (0,-1);  
        refresh ();
        StoreCursor (2); }
      restart_loop;  }
    if (keyin == rt_key)  {  /* move cursor one char right */
      if (curvapos < endvapos)  {
        curvapos++;  curdipos++;
        curtxt_p++;  curfmt_p++;
        RecallCursor (2);  movebyrc (0,1);  
        refresh ();
        StoreCursor (2); }
      restart_loop;  }
    if (keyin == kp1_key)  {  /* move cursor to start position */
      curvapos = 0;  curdipos = 0;
      RecallCursor (0);  
      refresh ();
      StoreCursor (2);
      curtxt_p = field_p->value;
      curfmt_p = field_p->editmask;
      restart_loop;  }
    if (keyin == kp2_key)  {      /* move cursor to end position */
      curvapos = last_non_blk_pl1 (field_p->value);
      if (curvapos > endvapos) curvapos = endvapos;
      curdipos = enddipos + 1;
      RecallCursor (0);  movebyrc (0,curvapos);  
      refresh ();
      StoreCursor (2);
      curtxt_p = field_p->value + curvapos;
      curfmt_p = field_p->editmask + curvapos;
      restart_loop;  }
    if (keyin == kp3_key)  {  /*  restore orig value  */
      strcpy (field_p->value,old_val);
      curvapos = 0;  curdipos = 0;
      RecallCursor (0);  
      refresh ();
      StoreCursor (2);
      curtxt_p = field_p->value;
      curfmt_p = field_p->editmask;
      restart_loop;
      }
    if (keyin == kp0_key)  {  /*  toggle insert/overstrike modes */ 
      movetorc (24,31); 
      normal_color ();
      if (*mode_p == INSERT)  { 
        *mode_p = OVERSTRIKE; 
        addstr ("[overstrike]");
        }
      else if (*mode_p == OVERSTRIKE)  {
        *mode_p = INSERT; 
        addstr ("  [insert]  ");
         }
      restart_loop;
      }
    if (keyin == rtn_key ||
        keyin == up_key ||
        keyin == dn_key ||
        keyin == tab_key ||
        keyin == bksp_key ||
        keyin == rtn_key ||
        keyin == kpent_key ||
        keyin == kpmin_key ||
        keyin == pf2_key ||
        keyin == pf3_key ||
        keyin == pf4_key ||
        keyin == 25 ||
        keyin == 3)  {
      quitting = TRUE;
      quit_loop; }
    /*  -  here if key is not an allowed one */
    RecallCursor (1);
    normal_color ();
    printw ("%3d ",keyin); 
    } aslongas (! quitting);
  field_p->val_sho_start = sho_start;
  /* - Return new data to input string, blank-filled & zero-terminated  */
  strncpy (new_val,field_p->value,field_p->val_dat_len);   
  new_val[field_p->val_dat_len] = '\0'; 
  ShowFldValue (field_p);
  return keyin;
  }  /*  -  end of key reading loop */

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void StoreCursor (int posnum)  {
  /*  -  Stores current cursor position into one of the curs_hold slots.
         Slot numbers allowed are 0 to 3.  If pos_num is out-of-range, the
         nearest limit is used.    */
  int irow, icol;
  if (posnum < 0)  posnum = 0;
  if (posnum > 3)  posnum = 3; 
  getyx (stdscr,irow,icol);
  curs_hold[posnum].row = irow;
  curs_hold[posnum].col = icol; 
  return;  }

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

void RecallCursor (int posnum)  {
  /*  -  Recalls saved cursor position from one of the curs_hold slots.
         Slot numbers allowed are 0 to 3.  If pos_num is out-of-range, the
         nearest limit is used.    */
  if (posnum < 0)  posnum = 0;
  if (posnum > 3)  posnum = 3;
  move (curs_hold[posnum].row, curs_hold[posnum].col);
  return;  }

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

int ibelong6 (int test, int val1, int val2, int val3, int val4,
              int val5, int val6)  {
  /*  -  determines of the test integer is one the 6 values.  Returns
         true if it is, false if not  */
  if (test == val1) return TRUE;
  if (test == val2) return TRUE;
  if (test == val3) return TRUE;
  if (test == val4) return TRUE;
  if (test == val5) return TRUE;
  if (test == val6) return TRUE;
  return FALSE;  }

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

int GoodFormat (int *key_p, char fmt_c)  {
  /*  -  Checks of the key input at this position can match the format of
         that position.  Certain matches may cause the key to be modified.
            */
  void RecallCursor (int posnum);
  char cha, chb;
  /*                  */
  cha = *key_p & 0xff;
  /* - 'x' allows any char */
  if (fmt_c == 'x' || fmt_c == 'X') return TRUE;
  /* - '!' allows any char but forces to upper case */
  if (fmt_c == '!') {
    if ((cha >= 'a') && (cha <= 'z'))  *key_p -= 32;
    return TRUE; }
  /* - '9' allows only digits  */
  if (fmt_c == '9')  {
    if ((cha >= '0') && (cha <= '9'))  return TRUE; }
  /* - here if format violated */
  return FALSE;
  }  /*  end of GoodFormat  */

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

int last_non_blk_pl1 (char *text)  {
  /* Returns the offset of one past the last non-blank in text.  If the
     char before the terminating null is non-blank, then fctn returns the
     offset of the null.  If the string is all blanks, the fctn returns
     0.  */
  int ia;  char *cpa;
  /*    */
  ia = strlen (text);
  cpa = text + ia - 1;
  for ( ; ia; ia--, cpa--)  {
    if (*cpa != ' ')  quit_loop;
    }
  return ia;
  }  /*  end of last_non_blk_pl1  */

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */
/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */
