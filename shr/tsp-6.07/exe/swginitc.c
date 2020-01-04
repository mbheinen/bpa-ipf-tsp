/*   SWGINITC.C  |
-----------------+
  -  Remake of the SWGINIT program using C language and CURSES screen
     manipulation routines.  
  -  Screen coords are (1,1) to (24,80)
  -  Arguments: 1) version of swing program 
                2) hard copy plot device
                3) run-type specs 
  -  Note: for Alpha VMS, must compile with option /STANDARD=RELAXED_ANSI89,
           or will get compilation failures in ttdef.h 
  -  Revs:
     *  Nov/17/94 - DEM - Restructure .COM file to add program execution,
                          printing, plotting, and purging old files. 
                          Separate .CM2 file no longer built.
     *  Nov/14/94 - DEM - Restructure to support new FIELD objects.
     *  Oct/29/93 - DEM - Add assignment to .COM and .CM2 files for logical 
        name DFLT so it is available with batch runs.  
*/
#include <curses.h>
#include <stdio.h>
#include <stdlib.h>
#include <unixlib.h>
#include <string.h>
#include <ttdef.h>
#include <tt2def.h>
#include <signal.h>
#include <stat.h>
#include <ctype.h>
#include "dem.h"
#include "keystroke.h"
#include "fields.h"

#define CASE_NAM 0
#define REPLOT 1
#define PRT_KEEP 2
#define FICHE_FLG 3
#define FICHE_CPY 4
#define DFLTDIR 5
#define SWI_IN 6
#define SAV_IN 7
#define PF_BASE 8
#define PST_IN  9
#define SOL_IN 10
#define SOL_OUT 11
#define SAV_OUT 12
#define RPT_OUT 13
#define PLT_OUT 14
#define AUX_OUT 15

#define NUM_CASE_FLDS 6
#define NUM_FILE_FLDS 10
#define NUM_FLDS 16

#include "swginutil.h"

/*  -  global variables for direct keybard input  */

io_stat_blk stat_blk;
short chan;
int readfunc;
ulong add_modes;

typedef struct  {
  char tclass;
  char ttype;
  ushort pg_wid;
  ulong bas_modes;
  ulong ext_modes;  }  TERM_MODES;
TERM_MODES term_modes;

#include "screen.h"
#define _NORMAL 0 

/* -  global storage for cursor stor/recall functions   */

POSITION curs_hold[4]; 

#include "debug.h"

#define CASE_SCR 0
#define FILE_SCR 1
#define NUM_SCRS 2

/* -  global variable for debug routines  */

FILE *dbg_fl;

/* - global var to hold edit_mode (insert/over)  */

int edit_mode;

typedef struct {
   char prognam[56];
   char casenam[14];
   char replot[2];
   char dflt_dir[60];
   char make_fiche[2];
   char prt_disp[2];
   char plot_dev[20];
   char plot_meth[12];
   int fiche_cnt;  
   char run_type[12];
   char sbm_que[16];
   char bat_time[8];
   char notif_flag[12];
   }  CASE_DAT;
CASE_DAT this_case;

typedef struct {
  char ctlin[56];
  char savin[56];
  char bsein[56];
  char pstin[56];
  char solin[56];
  char solot[56];
  char savot[56];
  char rptot[56];
  char pdfot[56];
  char pltot[56];
  char auxot[56];  
  char fchot[56];  }  FILE_SET;
FILE_SET these_files;

typedef struct {
  char datecode[7];
  char user[13];
  char account[16];  }  FICHE_HDR;
FICHE_HDR this_fiche;

/*                     */

void bld_blank_flds (FIELD **fld_arr);
void wrt_scr_perm (void);
void wrt_case_const (CASE_DAT *cased);
void wrt_file_const (void);
void wrt_help (void);
char case_entry (CASE_DAT *ccase, FIELD *fields[]);
char file_entry (FILE_SET *ffile, FIELD *fields[]);
int find_dir_fl (FILE **dirlis_flp, char *dir_fil_nam); 
int set_dirs (FILE_SET *file_set, CASE_DAT *ccase, FILE *dirlis_fl, 
              char *err_msg); 
void erase_files (FILE_SET *file_set);
void sho_dat (CASE_DAT *cased, FILE_SET *files, FICHE_HDR *fiched); 
int build_files (CASE_DAT *cased, FILE_SET *files, FICHE_HDR *fiched, 
                 char *bad_flnam);
int chek_infiles (FILE_SET *files); 
int set_plt_method (CASE_DAT *cased);
void send_quit_msg();
char *strupper (char *strin);
int SYS$ASSIGN ();
int SYS$QIOW ();
void LIB$STOP ();
int SYS$DASSGN ();

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

int main (int argc, char* argv[])  {
  char inkey, prt_char, char_buff, user_stat;
  int curpos, endpos;
  int errrow, errcol, crow, ccol, begcol;
  int norm_color = _NORMAL;     
  int rvrs_color = _REVERSE;    
  int bold_color = _BOLD;     
  char dir_file_nm[56], bad_file_nm[56];
  char swprog_file_nm[56];
  int disp_scr = CASE_SCR;
  int debug = false;
  FILE *dirlis_fl;

  /* - Array of pointers to field structures with names */

  FIELD *fields[20], **field_p;
  char descrip[10];
  char fld_val[20];
  char format[20];
  char kv_str[20], stra[20];
  char cha, *cpa, *cpb, *desc_p, *text_p, *curtxt_p;
  char *format_p, *curfmt_p;
  char err_buf[81];
  int keyin, syserr, go_on, find_err, file_err, bld_err;
  int any_err;
  int la, lb, lc;
  register qiostat;
  $DESCRIPTOR (terminal,"SYS$COMMAND");

  /*            */
  /*  -    Begin    Begin    Begin    Begin    Begin    Begin  */

  if (getenv ("PLOT_COM") == NULL) {
    fprintf(stderr, "Fatal error - Environment variable PLOT_COM is not \
defined [swginitc.c]\n");
    exit (1);
  }
  if (find_dir_fl (&dirlis_fl, dir_file_nm)) {
    fprintf(stderr, "Fatal error - directory assignment file SWISEAR.DAT \
could not be located [swginitc.c] ");
    exit (1);
  }
  if (debug) dbgopen ("swginitc.dbo"); 

  /*  - Show arguments if debugging  */

  if (debug) {
    cdbgwri ("swginitc - # of args = ",argc);
    for (la = 0; la < argc; la++)  {
      cdbgwri ("  for la = ",la);
      cdbgwrs ("    argv[la] = ",argv[la]);
    }
  }

  /* -  TSP program version  */

  memset(this_case.prognam, strlen(this_case.prognam), ' ');
  memset(this_case.casenam, strlen(this_case.casenam), ' ');
  memset(this_case.replot, strlen(this_case.replot), ' ');
  memset(this_case.dflt_dir, strlen(this_case.dflt_dir), ' ');
  memset(this_case.make_fiche, strlen(this_case.make_fiche), ' ');
  memset(this_case.prt_disp, strlen(this_case.prt_disp), ' ');
  memset(this_case.plot_dev, strlen(this_case.plot_dev), ' ');
  memset(this_case.plot_meth, strlen(this_case.plot_meth), ' ');
  memset(this_case.run_type, strlen(this_case.run_type), ' ');
  memset(this_case.sbm_que, strlen(this_case.sbm_que), ' ');
  memset(this_case.bat_time, strlen(this_case.bat_time), ' ');
  memset(this_case.notif_flag, strlen(this_case.notif_flag), ' ');

  if (argc > 1)  {
    strcpy (this_case.prognam,argv[1]);  }
  else  {
    strcpy (this_case.prognam,"UNKNOWN");  }

  /* -  plotting device  */

  if (argc > 2)  {
    strcpy (this_case.plot_dev,argv[2]);  }
  else  {
    strcpy (this_case.plot_dev,"NONE"); 
  }

  /*  -  run type specs  */

  if (argc > 3)  {
    cpa = argv[3];
    if (tolower (*cpa) == 'b')  {   /* batch - has | delim fields */
      for (cpb = this_case.run_type; *cpa != '|' && *cpa != '\0'; cpa++, cpb++) 
        *cpb = *cpa;
      *cpb = '\0';
      if (*cpa != '\0') cpa++;
      for (cpb = this_case.sbm_que; *cpa != '|' && *cpa != '\0'; cpa++, cpb++)
        *cpb = *cpa;
      *cpb = '\0';
      if (*cpa != '\0') cpa++;
      for (cpb = this_case.bat_time; *cpa != '|' && *cpa != '\0'; cpa++, cpb++)
        *cpb = *cpa;
      *cpb = '\0';
      if (*cpa != '\0') cpa++;
      cpb = this_case.notif_flag;
      strcpy (cpb,cpa); 
      }  /* end batch specs */
    else  {  /*  not batch  */
      strcpy (this_case.run_type,argv[3]);
    }
  }  /* end if have 4th arg  */
  else { 
    strcpy (this_case.run_type,"UNKNOWN");
  }  

  /*  -  Use program name to determine plotting method  */

    any_err = set_plt_method (&this_case);

  /*  -  initialize stdscr for direct output    */

  initscr ();

  /*  -  modify terminal driver for immediate keystroke capture      */

  qiostat = SYS$ASSIGN (&terminal, &chan, 0,0);
  if ( !(qiostat & 1)) LIB$STOP (qiostat);

  /*  -  initialize terminal for direct keyboard input  */

  qiostat = SYS$QIOW (0,chan,IO$_SENSEMODE,&stat_blk,0,0,&term_modes,12,
            0,0,0,0);
  if ( !(qiostat & 1)) LIB$STOP (qiostat);
  add_modes = TT$M_NOBRDCST; 
  term_modes.bas_modes |= add_modes; 
  add_modes = TT2$M_PASTHRU; 
  term_modes.ext_modes |= add_modes; 
  qiostat = SYS$QIOW (0,chan,IO$_SETMODE,&stat_blk,0,0,&term_modes,12,
            0,0,0,0);
  if ( !(qiostat & 1)) LIB$STOP (qiostat);

  /*                              */

  readfunc = IO$_READVBLK | IO$M_NOECHO;

  /*  -  Begin data entry  */

  edit_mode = OVERSTRIKE;    /* overstrike editing mode at start */
  bld_blank_flds (&fields[0]);
  go_on = true;   

  /* Loop until all data OK by user */

  repeat {
    wrt_scr_perm ();    /*  -  Write permanent screen constants */
    if (disp_scr == CASE_SCR) {   /* case screen  */
      /*  -  Erase variable portion of screen */
      normal_color ();
      for (la = 3; la < 20; la++) {
        movetorc (la,1); 
        addstr ("                                        ");
        addstr ("                                        ");
      }
      refresh ();
      movetorc (22,1);  clrtoeol();
      movetorc (23,1);  clrtoeol();
      wrt_case_const (&this_case); 
      field_p = &fields[0];
      for (la = 0; la < NUM_CASE_FLDS; la++)  {
        ShowField (*field_p);    field_p++;  
      }    

      /*  -  Enter case  name stuff (5 fields) */

      if (debug) cdbgeko ("swginitc - calling case_entry");
      user_stat = case_entry (&this_case, fields);
      if (debug) {
        cdbgwrs ("swginitc - after calling case_entry, dflt is ",
            this_case.dflt_dir);
      }

      /*  -  Stop program here, and tell PSAP that user has quit with no
             stab case to be run  */

      if (user_stat == 'q')  {
        send_quit_msg();
        go_on = FALSE;
        quit_loop;  } 

      /*  -  Find file with directory lists */

      find_err = find_dir_fl (&dirlis_fl, dir_file_nm);
      if (find_err)  {
        movetorc (8,5);
        addstr (" *** Couldn't find directory assignment file SWISEAR.DAT **");
        go_on = FALSE;
        dpause ();
        quit_loop;  }
      if ((! find_err) && debug) 
        cdbgwrs ("swginitc -  using directory list from file = ",dir_file_nm); 

      /*  -  Do searches through directories  */

      if (debug) {
        cdbgwrs ("swginitc - before calling set_dirs, dflt is ",
          this_case.dflt_dir);
      }
      file_err = set_dirs (&these_files,&this_case,dirlis_fl,err_buf);
  
      /*  -  if a .swi or .srp file not found, display message & restart loop */

      movetorc (21,5); 
      if (file_err == 1)  {
        addstr (err_buf); 
        restart_loop;  }
      else  {
        clrtoeol ();  
      }

      /*  -  If other input files not found, display message but let user
             supply correction on file name form  */

      if (file_err > 1)  {
        movetorc (21,5); 
        addstr (err_buf);
        sleep (2);      /*  2-sec pause  */
      }

      /* - Next can go to file screen  */

      disp_scr = FILE_SCR;
    } 

    if (disp_scr == FILE_SCR)  {  /*  file screen  */

      /*  -  Erase variable portion of screen */

      normal_color ();
      for (la = 3; la < 24; la++) {
        movetorc (la,1);  
        addstr ("                                        ");
        addstr ("                                        ");
      }
      refresh ();

      /*  -  Write screen constants for file name entry */

      wrt_file_const ();
      field_p = &fields[NUM_CASE_FLDS];
      for (la = 0; la < NUM_FILE_FLDS; la++)  {
        ShowField (*field_p);    field_p++;  
      }    

      /*  -  Enter file name stuff */ 

      if (debug) cdbgeko ("swginitc - calling file_entry");
      user_stat = file_entry (&these_files, fields);

      /*  -  If user wants to change case, switch to case screen  
             & restart loop */

      if (user_stat == 'n')  {
        if (debug) cdbgeko ("swginitc - calling erase_files");
        disp_scr = CASE_SCR; 
        restart_loop; }
    } 
    if (user_stat == 'q')  {
      send_quit_msg();
      go_on = FALSE;
      quit_loop;  } 
    if (user_stat == 'c') quit_loop; 

    /*  -  end of project loop  */

  } aslongas (go_on);
  if (go_on)  {

    /*  -  get fiche header info  */ 

    fiche_codes (this_fiche.datecode,this_fiche.user,this_fiche.account);
    if (debug) {   /*  sho data in program  */
      cdbgeko ("swginitc - calling sho_dat");
      sho_dat (&this_case, &these_files, &this_fiche);
      dbgclose ();  }  

    /*  -  Write out .com, .cm2 and .swf files  */

    bld_err = build_files (&this_case, &these_files, &this_fiche, bad_file_nm);

    /* bld_err = FALSE; */

    if (bld_err)  {
      movetorc (24,1); 
      addstr ("   ***  Error - Unable to build file ");
      addstr (bad_file_nm);  
      addstr ("  ***         ");
      refresh ();
      dpause (); }  
    }  

  /*  -  cleanup.  destroy windows and reset terminal  */

  if (debug) cdbgeko ("swginitc - begin clean-up");
  endwin (); 
  add_modes = TT$M_NOBRDCST; 
  term_modes.bas_modes -= add_modes; 
  add_modes = TT2$M_PASTHRU; 
  term_modes.ext_modes -= add_modes; 
  qiostat = SYS$QIOW (0,chan,IO$_SETMODE,&stat_blk,0,0,&term_modes,12,
            0,0,0,0);
  if ( !(qiostat & 1)) LIB$STOP (qiostat);
  qiostat = SYS$DASSGN (chan);
  if ( !(qiostat & 1)) LIB$STOP (qiostat);

  /*  syserr = system ("set term /nopasthru /broad");  */

  return;

  }  /*  end of swginitc.main  */

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void wrt_scr_perm (void)  { 
  /*  -  writes permanent text to the screen  */ 
  int bold_color = _BOLD;     
  int norm_color = _NORMAL;     
  movetorc (1,15);
  addstr ("SWING PROGRAM FILE ASSIGNMENTS     ");
  addstr ("SWGINITC (V 2.0)"); 
  refresh ();
  return; 

}  /*  end of wrt_scr_perm  *.

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void wrt_case_const (CASE_DAT *ccase)  { 

  /*  -  writes text constant to the case screen  */

  movetorc (11,12);
  addstr ("* Press [RETURN] when done with heading information  *");
  movetorc (12,18);
  addstr ("(List of files will display next)");
  movetorc (14,2);
  addstr ("Run type: ");
  addstr (ccase->run_type); 
  if (tolower (*ccase->run_type) == 'b')  {
    addstr (" to queue ");
    addstr (ccase->sbm_que);
    addstr (" at ");
    addstr (ccase->bat_time);
    addstr ("; ");
    addstr (ccase->notif_flag);
  }
  movetorc (15,2);
  addstr ("Plotting method: ");  addstr (ccase->plot_meth);
  addstr (" to ");  addstr (ccase->plot_dev);
  movetorc (16,2); 
  addstr ("Stab Program: ");  
  addstr (ccase->prognam);
  movetorc (23,16);
  addstr ("[PF2]/Help, [KP-]/Quit without submitting");
  refresh ();
  return;

}  /*  end of wrt_scr_perm  *.

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void wrt_file_const (void)  {

  /*  -  writes screen constants for the file name entry phase  */

  movetorc (3,35);   
  addstr ("INPUT FILES");
  movetorc (11,34);   
  addstr ("OUTPUT FILES");
  movetorc (19,15);   
  addstr ("* Press RETURN to submit or run using these files. *");
  movetorc (23,10);   
  addstr (" [PF2]/Help, [PF3]/Change case, [KP-]/Quit without submitting");
  refresh();
  }  /*  -  end of wrt_file_const  */

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */ 

void wrt_help (void)  {
  /*  -  writes the help screen  */
  WINDOW *helpwin;
  int box_ok, keyin;
  int bold_color = _BOLD;     

  /*                */

  helpwin = newwin(19,60,2,8);
  wsetattr (helpwin,_REVERSE);
  box_ok = box (helpwin,' ',' ');
  wclrattr (helpwin,_REVERSE);
  if (! box_ok)  { 
    wmove (helpwin,1,1);
    waddstr (helpwin,"ERROR DRAWING BOX FOR HELP WINDOW");
    dpause();
    exit (2);  }
  wmove (helpwin,1,2);
  waddstr (helpwin,"            SWGINIT  -  HELP FOR DATA ENTRY SCREEN");
  wmove (helpwin,3,2);
  waddstr (helpwin,"[RTN]    Finish data entry and submit or run");
  wmove (helpwin,4,2);
  waddstr (helpwin,"[KP-]    Abort data entry and return to PSAP  (also [^Y])");
  wmove (helpwin,5,2);
  waddstr (helpwin,"[PF3]    Change case name");
  wmove (helpwin,6,2);
  waddstr (helpwin,"[PF4]    Check existence of input files");
  wmove (helpwin,7,2);
  waddstr (helpwin,"[DOWN]   Move to next field (also [tab])");
  wmove (helpwin,8,2);
  waddstr (helpwin,"[UP]     Move to previous field (also [bksp] or [^H])");
  wmove (helpwin,9,2);
  waddstr (helpwin,"[KP1]    Move to start of this field");
  wmove (helpwin,10,2);
  waddstr (helpwin,"[KP2]    Move to end of this field");
  wmove (helpwin,11,2);
  waddstr (helpwin,"[DEL]    Delete char at left");
  wmove (helpwin,12,2);
  waddstr (helpwin,"[KP,]    Delete char under cursor");
  wmove (helpwin,13,2);
  waddstr (helpwin,"[KP0]    Toggle insert / overstrike mode");
  wmove (helpwin,14,2);
  waddstr (helpwin,"[KP3]    Restore older data in this field");
  wmove (helpwin,16,2);
  waddstr (helpwin,"[PF2]    Show this help menu");
  wmove (helpwin,17,10);
  wsetattr (helpwin,_BOLD);
  waddstr (helpwin,"Press any key to get back to data screen");
  wsetattr (helpwin,0);
  wrefresh (helpwin);
  keyin = getkey ();
  if (keyin == esc_key)  keyin = get_esc_code();
  delwin (helpwin);
  refresh ();
  keyin = 0;
  return;
  }  /*  -  end of wrt_help  */
  
/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

char case_entry (CASE_DAT *ccase, FIELD *fields[])  {  

  /*  -  picks up the user's input in the case entry phase  */

  static int cecalls = 0;
  int wh_field, max_flds;
  int la, lb, ka, kb;
  int end_key;
  char format[36];      /*  must be one more than largest field  */
  char fcnt[2];
  int rvrs_color = _REVERSE;
  int norm_color = _NORMAL;
  int debug = 1;

  /*                   */

  if (debug)  { 
    cdbgeko ("case_entry - at start of function");
    cdbgwri ("  cecalls = ",cecalls);  
  }
  max_flds = NUM_CASE_FLDS;

  /*  -  inititalization for first pass only  */ 

  if (cecalls == 0)  {
    strcpy (ccase->casenam,"          ");
    strcpy (ccase->replot,"N");
    getcwd (ccase->dflt_dir,36);                  /* deflt dir  */
    trim (ccase->dflt_dir);                       /* remove trailing blanks */
    strcpy (ccase->prt_disp,"K");
    strcpy (ccase->make_fiche,"N");
    ccase->fiche_cnt = 0;
    SetFieldValue (fields[CASE_NAM],ccase->casenam);
    SetFieldValue (fields[REPLOT],ccase->replot);
    SetFieldValue (fields[PRT_KEEP],ccase->prt_disp);
    SetFieldValue (fields[FICHE_FLG],ccase->make_fiche);
    SetFieldValue (fields[DFLTDIR],ccase->dflt_dir);
  } 
  cecalls++;
  sprintf (fcnt,"%1d",ccase->fiche_cnt);
  SetFieldValue (fields[FICHE_CPY],fcnt);
  if (! *ccase->dflt_dir)  { 
    getcwd (ccase->dflt_dir,36);
    trim (ccase->dflt_dir);
  }
  SetFieldValue (fields[DFLTDIR],ccase->dflt_dir);
  strcpy (format,  "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");  
  end_key = 0;

  /*                   */

  if (debug)  { 
     cdbgeko ("case_entry - about to write input fields");
     cdbgwrs ("  casenam   = ",ccase->casenam);
     cdbgwrs ("  dflt dir  = ",ccase->dflt_dir);
     cdbgwrs ("  replot    = ",ccase->replot);
     cdbgwrs ("  make_fich = ",ccase->make_fiche);
     cdbgwrs ("  prt_disp  = ",ccase->prt_disp);
     cdbgwrs ("  fiche cnt = ",fcnt);  }

  /*  -  Display edit mode at screen bottom  */

  if (edit_mode == INSERT)  {
    movetorc (24,31);  wrt_txt (norm_color,"[insert]    ");  }
  else  {
    movetorc (24,31);  wrt_txt (norm_color,"[overstrike]");  }

  /*  -  Write out input fields before first update  */

  for (la = 0; la < NUM_CASE_FLDS; la++)  {
    ShowFldValue (fields[la]);  
  }
  if (debug)  cdbgeko ("case_entry - about to edit first input field");

  /*           */

  wh_field = 0;
  repeat {
    switch (wh_field) {
    case 0: 
      end_key = EditField (fields[CASE_NAM], &edit_mode, ccase->casenam);
      break;
    case 1: 
      end_key = EditField (fields[REPLOT], &edit_mode, ccase->replot);  
      if (ccase->replot[0] != 'Y') ccase->replot[0] = 'N';
      SetFieldValue (fields[REPLOT], ccase->replot);
      ShowFldValue (fields[REPLOT]);
      break;
    case 2: 
      end_key = EditField (fields[PRT_KEEP], &edit_mode, ccase->prt_disp);  
      if (ccase->prt_disp[0] != 'P') ccase->prt_disp[0] = 'K';
      SetFieldValue (fields[PRT_KEEP], ccase->prt_disp);
      ShowFldValue (fields[PRT_KEEP]);
      break;
    case 3: 
      end_key = EditField (fields[FICHE_FLG], &edit_mode, ccase->make_fiche);  
      SetFieldValue (fields[FICHE_FLG], ccase->make_fiche);
      ShowFldValue (fields[FICHE_FLG]);
      break;
    case 4: 
      sprintf (fcnt,"%1d",ccase->fiche_cnt);
      end_key = EditField (fields[FICHE_CPY], &edit_mode, fcnt);  
      if (ccase->make_fiche[0] != 'Y')  fcnt[0] = '0';
      sscanf (fcnt,"%1d",&ccase->fiche_cnt);
      SetFieldValue (fields[FICHE_CPY], fcnt);
      ShowFldValue (fields[FICHE_CPY]);
      if (debug)  {
        cdbgeko ("case_entry - just after entering # of fiche");
        cdbgwrs ("  fcnt             = ",fcnt);
        cdbgwri ("  ccase->fiche_cnt = ",ccase->fiche_cnt);  }
      break;
    case 5: 
      end_key = EditField (fields[DFLTDIR], &edit_mode, ccase->dflt_dir);  
      trim (ccase->dflt_dir);
      if (! *ccase->dflt_dir)  {   /* restore dflt if all blank */
        getcwd (ccase->dflt_dir,36);
        trim  (ccase->dflt_dir);
      } 
      if (! strchr (ccase->dflt_dir,':'))  {  /*  needs a colon */
        strcat (ccase->dflt_dir,":"); 
      }
      SetFieldValue (fields[DFLTDIR], ccase->dflt_dir);
      ShowFldValue (fields[DFLTDIR]);
    }  /*  -  end of wh_field switch  */
    if (end_key == kpmin_key || end_key == ctl_y_key) return 'q';  /* quit */
    if (end_key == ctl_c_key) return 'q';  /* quit */
    if (end_key == pf2_key)  {             /* sho help  */
      wrt_help(); restart_loop; } 
    if (end_key == up_key || end_key == bksp_key) wh_field--;
    if (end_key == dn_key || end_key == tab_key)  wh_field++;
    if (wh_field >= max_flds) wh_field -= max_flds;
    if (wh_field < 0)  wh_field += max_flds;
  }  aslongas (end_key != rtn_key);

  /* - clear out err message line & leave cursor there  */

  movetorc (8,5);
  addstr ("                                                  ");
  refresh(); 
  return 'c';   /*  user continue  */
}  /*  end of case_entry ()  */

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

char file_entry (FILE_SET *ffile, FIELD *fields[])  {  

  /*  -  allows user to modify pre-selected files in the file entry phase  */

  int rvrs_color = _REVERSE;
  int norm_color = _NORMAL;
  int wh_field, max_flds;
  int la, lb, ka, kb;
  int end_key, any_error;
  char format [56]; 

  /*                   */

  strcpy (format,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
  max_flds = NUM_FILE_FLDS;
  end_key = 0;

  /*  -  display all fields before modifying any one of them   */

  SetFieldValue (fields[SWI_IN], ffile->ctlin);
  ShowField (fields[SWI_IN]);

  SetFieldValue (fields[SAV_IN], ffile->savin);
  ShowField (fields[SAV_IN]);
  if (strchr (ffile->savin,'?'))  {
    movetorc (6,71),  wrt_txt (norm_color,"Not found");  }

  SetFieldValue (fields[PF_BASE], ffile->bsein);
  ShowField (fields[PF_BASE]);
  if (strchr (ffile->bsein,'?'))  {
    movetorc (7,71),  wrt_txt (norm_color,"Not found");  }

  SetFieldValue (fields[PST_IN], ffile->pstin);
  ShowField (fields[PST_IN]);
  if (strchr (ffile->pstin,'?'))  {
    movetorc (8,71),  wrt_txt (norm_color,"Not found");  }

  SetFieldValue (fields[SOL_IN], ffile->solin);
  ShowField (fields[SOL_IN]);
  if (strchr (ffile->solin,'?'))  {
    movetorc (9,71),  wrt_txt (norm_color,"Not found");  }

  SetFieldValue (fields[SOL_OUT], ffile->solot);
  ShowField (fields[SOL_OUT]);

  SetFieldValue (fields[SAV_OUT], ffile->savot);
  ShowField (fields[SAV_OUT]);

  SetFieldValue (fields[RPT_OUT], ffile->rptot);
  ShowField (fields[RPT_OUT]);

  SetFieldValue (fields[PLT_OUT], ffile->pltot);
  ShowField (fields[PLT_OUT]);

  SetFieldValue (fields[AUX_OUT], ffile->auxot);
  ShowField (fields[AUX_OUT]);

  /*         */  

  wh_field = 0;
  repeat {
    switch (wh_field) {
    case 0: 
      end_key = EditField (fields[SWI_IN],&edit_mode,ffile->ctlin);
      break;
    case 1: 
      end_key = EditField (fields[SAV_IN],&edit_mode,ffile->savin);
      break;
    case 2: 
      end_key = EditField (fields[PF_BASE],&edit_mode,ffile->bsein);
      break;
    case 3: 
      end_key = EditField (fields[SOL_IN],&edit_mode,ffile->solin);
      break;
    case 4: 
      end_key = EditField (fields[SOL_OUT],&edit_mode,ffile->solot);
      break;
    case 5: 
      end_key = EditField (fields[SAV_OUT],&edit_mode,ffile->savot);
      break;
    case 6: 
      end_key = EditField (fields[RPT_OUT],&edit_mode,ffile->rptot);
      break;
    case 7: 
      end_key = EditField (fields[AUX_OUT],&edit_mode,ffile->auxot);
      break;
    }  
    if (end_key == kpmin_key || end_key == ctl_y_key) return 'q';  /* quit */
    if (end_key == ctl_c_key) return 'q';    /* quit */
    if (end_key == pf3_key) return 'n';      /* user new case */
    if (end_key == pf2_key)  {               /* sho help */
      wrt_help(); restart_loop; } 
    if (end_key == pf4_key)  {               /* check input files  */
      any_error = chek_infiles (ffile); 
      restart_loop; } 
    if (end_key == rtn_key)  {     /* check input files, finish if OK */
      any_error = chek_infiles (ffile); 
      if (any_error)  {
        end_key = pf4_key;  
        restart_loop; }
      } 
    if (end_key == up_key || end_key == bksp_key) wh_field--;
    if (end_key == dn_key || end_key == tab_key)  wh_field++;
    if (wh_field >= max_flds) wh_field -= max_flds;
    if (wh_field < 0)  wh_field += max_flds;
  }  aslongas (end_key != rtn_key);
  return 'c';   /*  user continue  */
}  /*  end of file_entry ()  */

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

int find_dir_fl (FILE **dirlis_flp, char *dir_fil_nam)  {

  /*  -  Looks for a file that contains a list of search directories.

         Look for file SWISEAR.DAT in (1) current dir, (2) home dir,
         (3) SWG_EXE: dir.
         Returns  0 if successful
                  1 if no file found in any of the 3 directories
         */
  char full_nam[128];
  /*             */
  strcpy (dir_fil_nam,"SWISEAR.DAT");
  getcwd (full_nam,60);                  /* current  */
  trim (full_nam);
  strcat (full_nam,dir_fil_nam);
  *dirlis_flp = fopen (full_nam,"r");
  if (*dirlis_flp)  { 
    strcpy (dir_fil_nam,full_nam);
    return 0;  }
  strcpy (full_nam,"SYS$LOGIN:");        /*  home  */
  strcat (full_nam,dir_fil_nam);
  *dirlis_flp = fopen (full_nam,"r");
  if (*dirlis_flp)  {
    strcpy (dir_fil_nam,full_nam);
    return 0;  }
  strcpy (full_nam,"SWG_EXE:");          /* preset */
  strcat (full_nam,dir_fil_nam);
  *dirlis_flp = fopen (full_nam,"r");
  if (*dirlis_flp)  {
    strcpy (dir_fil_nam,full_nam);
    return 0;  }
  /*  -  here if no file found in any of above directories  */
  return 1;
}  /*  end of find_dir_fl () 

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

int set_dirs (FILE_SET *file_set, CASE_DAT *ccase, FILE *dirlis_fl, 
              char *err_msg)  { 
  /*  -  Does searches for input files using the found directory list file.
         Also determines full names for output files.  
      -  *dirlis_fl is a file with a list of directories to search through
         *badfile is a file (name + ext) that couldn't be found in any of 
            the search directories  
      -  Returns 0 if all files found 
                 1 if a .SWI or .SRP file or directory not found 
                 2 if any other input file not found 
                         */
  stat_t file_stat;
  char srpdir[4][36], swidir[4][36], bsedir[8][36], sdidir[4][36];
  char pstdir[4][36];
  char soldir[36], outdir[36], sdodir[36], auxdir[36], fchdir[36];  
  char pltdir[36];
  char pf_id[11], swi_id[11], sdi_id[11], sdo_id[11];
  int la,lb,lc,ka,kb,kc,ina,inb;
  char *cpa, *cpb, *cpc;
  FILE *filea, *swifil;
  char inline[81], card_typ[5], case_line[80];
  char fullnam[128], stra[81];
  int ksrp, kswi, kbse, ksdi, kpst; 
  int ksdo, ksol, kout, kplt, kaux, kfch;
  int staterr, rtncode;
  int debug = 1;

  /*  -  Initialize character pointers  */
  for (la = 0; la < 4; la++) *srpdir[la] = '\0';
  for (la = 0; la < 4; la++) *swidir[la] = '\0';
  for (la = 0; la < 8; la++) *bsedir[la] = '\0';
  for (la = 0; la < 4; la++) *sdidir[la] = '\0';
  for (la = 0; la < 4; la++) *pstdir[la] = '\0';
  *soldir = '\0';
  *outdir = '\0';
  *sdodir = '\0';
  *pltdir = '\0';
  *auxdir = '\0';
  *fchdir = '\0';
  ksrp = 0;
  kswi = 0;
  kbse = 0;
  ksdi = 0;
  kpst = 0;
  ksdo = 0;
  ksol = 0;
  kout = 0;
  kplt = 0;
  kaux = 0;
  kfch = 0;
  swifil = NULL;
  rtncode = 0;
   /*  -  Build directory list in memory  */
  cpc = fgets (inline,80,dirlis_fl);
  /*  -  read up to end of file  */
  loopwhen (cpc)  {   /*  for each line  */
    /*  -  Separate directory type from direct name  */
    for (cpa = inline,  cpb = card_typ; *cpa != ' '; cpa++, cpb++) *cpb = *cpa;
    *cpb = '\0'; 
    for (; *cpa == ' '; cpa++);  
    /*  -  Here, cpa points to start of dir name  */
    trim (cpa);
    /*  -  Change keyword here to actual default directory name  */
    if (debug) {
      cdbgwrs ("set_dirs - for DFLT: the dir is ",ccase->dflt_dir);
      }
    if (strcmp (cpa,"DFLT:") == 0)  strcpy (cpa,ccase->dflt_dir);
    /*  -  Distribute dir names to proper arrays  */
    if      ((ina = strncmp (card_typ,"SRP",3) == 0) && ksrp < 4)  {
      strcpy (srpdir[ksrp],cpa);  
      ksrp++;  }
    else if ((ina = strncmp (card_typ,"SWI",3) == 0) && kswi < 4)  {
      strcpy (swidir[kswi],cpa);  
      if (debug)  {
        cdbgwri ("set_dirs - kswi = ",kswi);
        cdbgwrs ("  swidir[kswi] = ",swidir[kswi]); }
      kswi++;  }
    else if ((ina = strncmp (card_typ,"BSE",3) == 0) && kbse < 8)  {
      strcpy (bsedir[kbse],cpa);  
      kbse++;  }
    else if ((ina = strncmp (card_typ,"SDI",3) == 0) && ksdi < 4)  {
      strcpy (sdidir[ksdi],cpa);  
      ksdi++;  }
    else if ((ina = strncmp (card_typ,"POST",4) == 0) && kpst < 4)  {
      strcpy (pstdir[kpst],cpa);  
      kpst++;  }
    else if ((ina = strncmp (card_typ,"SOL",3) == 0) && ksol < 1)  {
      strcpy (soldir,cpa);  
      ksol++;  }
    else if ((ina = strncmp (card_typ,"OUT",3) == 0) && kout < 1)  {
      strcpy (outdir,cpa);  
      kout++;  }
    else if ((ina = strncmp (card_typ,"PLT",3) == 0) && kplt < 1)  {
      strcpy (pltdir,cpa);  
      kplt++;  }
    else if ((ina = strncmp (card_typ,"SDO",3) == 0) && ksdo < 1)  {
      strcpy (sdodir,cpa);  
      ksdo++;  }
    else if ((ina = strncmp (card_typ,"AUX",3) == 0) && kaux < 1)  {
      strcpy (auxdir,cpa);  
      kaux++;  }
    else if ((ina = strncmp (card_typ,"FCH",3) == 0) && kfch < 1)  {
      strcpy (fchdir,cpa);  
      kfch++;  }
    cpc = fgets (inline,80,dirlis_fl);
    }  /*  end of file reading loop   */
  fclose (dirlis_fl);

  if (debug)  {
    cdbgeko ("set_dirs - just after reading direct list file");
    cdbgwri ("  ksrp = ", ksrp);
    cdbgwri ("  kswi = ", kswi);
    cdbgwri ("  ksdi = ", ksdi);
    cdbgwri ("  kbse = ", kbse); 
    cdbgwri ("  kpst = ", kpst); }

  /*  -  Now have all direct names from list file  */
  /*  -  from case name, develop file names to search by  */
  strcpy (swi_id,ccase->casenam);
  trim (swi_id); 

  /*  -  Look for replot file if replotting  */
  if (ccase->replot[0] == 'Y')  { 
    strcpy (stra,swi_id);
    strcat (stra,".SRP");
    for (la = 0; la < ksrp; la++)  {
      strcpy (fullnam,srpdir[la]);
      strcat (fullnam,stra);           /*  full file name  */
      staterr = stat (fullnam,&file_stat);
      if (! staterr) quit_loop;          /* got the right file  */
      } 
    if (staterr) { 
      sprintf (err_msg,"*** Couldn't find file %s anywhere ***",stra);
      return 1;  }
    strcpy (file_set->ctlin,fullnam);     /*  cntrl file is .SRP  */
    }     

  /*  -  Look for simulation (.SWI) file  */
  strcpy (stra,swi_id);
  strcat (stra,".SWI");
  for (la = 0; la < kswi; la++)  {
    strcpy (fullnam,swidir[la]);
    strcat (fullnam,stra);           /*  full file name  */
    if (debug) cdbgwrs ("set_dirs - looking for .SWI file = ",fullnam);
    staterr = stat (fullnam,&file_stat);
    if (! staterr) {
      swifil = fopen (fullnam,"r");  
      quit_loop;  }        /* got the right file  */
    } 
  if (staterr) { 
    sprintf (err_msg,"*** Couldn't find file %s anywhere ***",stra);
    return 1;  }
  /*  -  Set  cntrl file to be .SWI if not replotting  */
  if (ccase->replot[0] != 'Y')  strcpy (file_set->ctlin,fullnam);   

  /*  -  read the first few lines of the .SWI file to get the other 
         file names  */
  *pf_id = '\0'; 
  /*  -  debug to find out why prog bombs here  */
  if (debug)  {
    cdbgeko ("set_dirs - just before reading _.SWI_ file");
    cdbgwrp ("  char ptr case_line = ",case_line);
    cdbgeko ("set_dirs - in loop reading _.SWI_ file");
    }
  for (la = 0; la < 10; la++)  {
    if (debug) cdbgwri ("  la = ",la);
    fgets (case_line, 80, swifil); 
    if (debug)  cdbgwrs ("    case_line = ",case_line); 
    if ( strncmp (case_line,"CASE",4) == 0)  {   /*  have CASE card  */ 
      /*  -  blank-fill to at least 48 chars  */
      blank_fill (case_line,48);
      /*  -  fetch id's  */
      strncpy (pf_id,&case_line[5],10);
      pf_id[10] = '\0';
      trim (pf_id); 
      strncpy (sdi_id,&case_line[24],10);
      sdi_id[10] = '\0';
      trim (sdi_id); 
      strncpy (sdo_id,&case_line[34],10);
      sdo_id[10] = '\0';
      trim (sdo_id); 
      quit_loop;
      } 
    } 
  fclose (swifil);
  /*  -  display id's fetched from CASE card  */
  if (debug)  {
    cdbgeko ("setdirs - case ID's found on CASE card");
    cdbgwrs ("  pf_id  = ", pf_id);
    cdbgwrs ("  sdi_id = ",sdi_id); 
    cdbgwrs ("  sdo_id = ",sdo_id);  }
  if (! *pf_id)  {    /*  no pf case means no case card  */
    strcpy (err_msg,"*** .SWI file has no CASE card ***");
    return 1;  }

  /*  -  look for powerflow base file  */  
  strcpy (stra,pf_id);
  strcat (stra,".BSE");
  for (la = 0; la < kbse; la++)  {
    strcpy (fullnam,bsedir[la]);
    strcat (fullnam,stra);           /*  full file name  */
    staterr = stat (fullnam,&file_stat);
    if (! staterr) quit_loop;          /* got the right file  */
    } 
  if (staterr) { 
    sprintf (err_msg,"*** Couldn't find file %s anywhere ***",stra);
    strcpy (fullnam,"?:");
    strcat (fullnam,stra);
    rtncode = 2;  }
  else  {
    strcpy (file_set->bsein,fullnam);
  }

  /*  -  look for input machine data base file (if used)  */  
  /*  -  No sdi id means all data from ctl file.  Replotting doesn't use an
         sdi file  */
  if (! *sdi_id || ccase->replot[0] == 'Y')  {  
    strcpy (file_set->savin,"--_not_used_--");  }
  else  {  /*  search for an input gen data base file  */
    strcpy (stra,sdi_id);
    strcat (stra,".SDI");
    for (la = 0; la < ksdi; la++)  {
      strcpy (fullnam,sdidir[la]);
      strcat (fullnam,stra);           /*  full file name  */
      staterr = stat (fullnam,&file_stat); 
      if (! staterr)  quit_loop;          /* got the right file  */
      }
    if (staterr) {
      sprintf (err_msg,"*** Couldn't find file %s anywhere ***",stra);
      strcpy (fullnam,"?:");
      strcat (fullnam,stra);
      rtncode = 2;  }
    else  {
      strcpy (file_set->savin,fullnam);
    }
  } 

  /*  -  look for postscript header file  */  
  strcpy (stra,"TSPMASTER.POST");
  for (la = 0; la < kpst; la++)  {
    strcpy (fullnam,pstdir[la]);
    strcat (fullnam,stra);           /*  full file name  */
    staterr = stat (fullnam,&file_stat);
    if (! staterr) quit_loop;          /* got the right file  */
    } 
  if (staterr) { 
    sprintf (err_msg,"*** Couldn't find file %s anywhere ***",stra);
    strcpy (fullnam,"?:");
    strcat (fullnam,stra);
    rtncode = 2;  } 
  else {
    strcpy (file_set->pstin,fullnam);
  }

  /*  -  Assign output files with default directories   */
  /*  -  Build solution file name  */
  if (! soldir)  { 
    strcpy (err_msg,"*** No solution directory in directory list ***");
    return 1;  }
  strcpy (fullnam,soldir);
  strcat (fullnam,swi_id);
  strcat (fullnam,".SOL");   
  /*  -  solution file might be input or output */
  if (ccase->replot[0] != 'Y')  {  /* output */ 
    strcpy (file_set->solot,fullnam);
    strcpy (file_set->solin,"--_not_used_--"); }
  else  {                       /* input */
    staterr = stat (fullnam, &file_stat);
    if (staterr) {
      sprintf (err_msg,"*** Couldn't find file %s anywhere ***",stra);
      strcpy (fullnam,"?:");
      strcat (fullnam,swi_id);
      strcat (fullnam,".SOL");   
      rtncode = 2;  }
    strcpy (file_set->solin,fullnam);
    strcpy (file_set->solot,"--_not_used_--"); }

  /*  -  Build printout file name  */
  if (! *outdir)  { 
    strcpy (err_msg,"*** No report output directory in directory list ***");
    return 1;  }
  strcpy (fullnam,outdir);
  strcat (fullnam,swi_id);
  if (ccase->replot[0] != 'Y')  {   
    strcat (fullnam,".OUT");  }
  else  {
    strcat (fullnam,".SRO");  }
  strcpy (file_set->rptot,fullnam);

  /*  -  Build plot file names  */
  if (! *pltdir)  { 
    strcpy (err_msg,"*** No plot_file output directory in directory list ***");
    return 1;  }
  strcpy (fullnam,pltdir);
  strcat (fullnam,swi_id);
  strcpy (file_set->pdfot,fullnam);
  strcat (file_set->pdfot,".PDF");  
  strcpy (file_set->pltot,fullnam);
  strcat (file_set->pltot,".PLT");  

  /*  -  Build auxiliary file name  */
  if (! *auxdir)  { 
    strcpy (err_msg,"*** No auxiliary output directory in directory list ***");
    return 1;  }
  strcpy (fullnam,auxdir);
  strcat (fullnam,swi_id);
  strcat (fullnam,".SWX"); 
  strcpy (file_set->auxot,fullnam);

  /*  -  Build m-fiche file name  */
  if (! *fchdir)  { 
    strcpy (err_msg,"*** No fiche output directory in directory list ***");
    return 1;  }
  strcpy (fullnam,fchdir);
  strcat (fullnam,swi_id);
  strcat (fullnam,".SWF"); 
  strcpy (file_set->fchot,fullnam);

  /*  -  Build new gen data base file name (if used) */
  if (*sdo_id)  {  /* new sdi file used  */
    if (! *sdodir)  { 
      strcpy (err_msg,"*** No saved data output directory in dirctry list ***");
      return 1;  }
    strcpy (fullnam,sdodir);
    strcat (fullnam,sdo_id);
    strcat (fullnam,".SDI"); 
    strcpy (file_set->savot,fullnam);  }
  else  {
    strcpy (file_set->savot,"--_not_used_--");
    }
  /*  -  Now have all in/out file names  */
  return 0;
  }  /*  end of int set_dirs ()  */

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void erase_files (FILE_SET *file_set)  { 
  /*  - sets all file names back to blanks */
  /*  typedef struct {
        char ctlin[56];
        char savin[56];
        char bsein[56];
        char pstin[56];
        char solin[56];
        char solot[56];
        char savot[56];
        char rptot[56];
        char pdfot[56];
        char pltot[56];
        char auxot[56];
        char fchot[56];  }  FILE_SET;      */
  char blank[] = " ";
  /*                    */
  strcpy (file_set->ctlin,blank);
  strcpy (file_set->savin,blank);
  strcpy (file_set->bsein,blank);
  strcpy (file_set->solin,blank);
  strcpy (file_set->solot,blank);
  strcpy (file_set->savot,blank);
  strcpy (file_set->rptot,blank);
  strcpy (file_set->pdfot,blank);
  strcpy (file_set->pltot,blank);
  strcpy (file_set->auxot,blank);
  strcpy (file_set->fchot,blank);
  }  /*  end of erase_files()  */

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void sho_dat (CASE_DAT *cased, FILE_SET *files, FICHE_HDR *fiched)  {
  /*  -  dumps to debug file selected data so far.  Decision to use this is
         by calling program.  */
  /* typedef struct {
       char prognam[56];
       char casenam[14];
       char replot[2];
       char dflt_dir[36];
       char make_fiche[2];
       char prt_disp[2];
       char plot_dev[20];
       char plot_meth[8];
       int fiche_cnt;  
       char run_type[12];
       char sbm_que[16];
       char bat_time[8];
       char notif_flag[12];
       }  CASE_DAT;
    typedef struct {
      char ctlin[56];
      char savin[56];
      char bsein[56];
      char pstin[56];
      char solin[56];
      char solot[56];
      char savot[56];
      char rptot[56];
      char pdfot[56];
      char pltot[56];
      char auxot[56];
      char fchot[56];  }  FILE_SET;          */
  /*                */
  cdbgeko ("shodat - at start, dump of data structures");
  cdbgeko ("  CASE_DAT structure:");
  cdbgwrs ("    prognam    = ",cased->prognam);
  cdbgwrs ("    casenam    = ",cased->casenam);
  cdbgwrs ("    replot     = ",cased->replot);
  cdbgwrs ("    dflt_dir   = ",cased->dflt_dir);
  cdbgwrs ("    make_fiche = ",cased->make_fiche);
  cdbgwrs ("    prt_disp   = ",cased->prt_disp);
  cdbgwrs ("    plot_dev   = ",cased->plot_dev);
  cdbgwrs ("    plot_meth  = ",cased->plot_meth );
  cdbgwri ("    fiche_cnt  = ",cased->fiche_cnt);
  cdbgeko ("  FILE_SET structure:");
  cdbgwrs ("    ctlin = ",files->ctlin);
  cdbgwrs ("    savin = ",files->savin);
  cdbgwrs ("    bsein = ",files->bsein);
  cdbgwrs ("    pstin = ",files->pstin);
  cdbgwrs ("    solin = ",files->solin);
  cdbgwrs ("    solot = ",files->solot);
  cdbgwrs ("    savot = ",files->savot);
  cdbgwrs ("    rptot = ",files->rptot);
  cdbgwrs ("    pdfot = ",files->pdfot);
  cdbgwrs ("    pltot = ",files->pltot);
  cdbgwrs ("    auxot = ",files->auxot); 
  cdbgwrs ("    fchot = ",files->fchot); 
  cdbgeko ("  FICHE_HDR structure");
  cdbgwrs ("    datecode = ",fiched->datecode);
  cdbgwrs ("    username = ",fiched->user);
  cdbgwrs ("    account  = ",fiched->account);
  return;  
  }  /*  end of sho_dat()  */

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

int build_files (CASE_DAT *cased, FILE_SET *files, FICHE_HDR *fiched, 
                 char *bad_flnam)  {
  /*  -  actually writes out the .com, .cm2 and (optionally) .swf files
         to interface with the BSW batch swing procedure.  */ 
  int la, lb, ka, kb;
  char *cpa, *cpb;
  char pffname[128], here_dir[60], otname[128], today[10];
  char plcom_dir[60];
  FILE *otfile;
  char assign[] = "$ assign ";
  char purge[] = "$ purge/keep=2/log ";
  char set_prot[] = "$ set prot=(s:wred,o:wred,g:wred,w:wred)/log ";
  int ok = 0, bad = 1;
  /*                        */
  getcwd (here_dir,60);                      /* default dir  */
  trim (here_dir);
  trim (cased->casenam);
  strcpy (plcom_dir,getenv ("PLOT_COM"));
  /* -  generate .com file  */
  strcpy (otname,here_dir);
  strcat (otname,cased->casenam);
  strcat (otname,".COM");
  otfile = fopen (otname,"w","mrs=133","rat=cr","rfm=var");
  if (! otfile)  { 
    strcpy(bad_flnam,otname);
    return bad;  }

  /*  -  Write data to .com file  */
  /*  -  ASSIGN statements        */

  fprintf (otfile,"%s%s\n","$!  -    .COM file for stab case ",cased->casenam);
  fprintf (otfile,"%s%s %s\n",assign,cased->dflt_dir," DFLT "); 
  trim (files->ctlin);
  fprintf (otfile,"%s%s%s\n",assign,files->ctlin,"  FOR005");
  fprintf (otfile,"%s%s%s\n",assign,files->bsein,"  FOR003");
  fprintf (otfile,"%s%s%s\n",assign,files->pstin,"  FOR023");
  if (*files->savin != '-')  {
    fprintf (otfile,"%s%s%s\n",assign,files->savin,"  FOR009"); } 
  if (cased->replot[0] != 'Y')  {
    fprintf (otfile,"%s%s%s\n",assign,files->solot,"  FOR008"); }
  else  {
    fprintf (otfile,"%s%s%s\n",assign,files->solin,"  FOR008"); }
  if (*files->savot != '-')  {
    fprintf (otfile,"%s%s%s\n",assign,files->savot,"  FOR015"); } 
  fprintf (otfile,"%s%s%s\n",assign,files->rptot,"  FOR006"); 
  fprintf (otfile,"%s%s%s\n",assign,files->pltot,"  PLOT$FILE"); 
  fprintf (otfile,"%s%s%s\n",assign,files->pltot,"  FOR022"); 
  fprintf (otfile,"%s%s%s\n",assign,files->auxot,"  FOR011");
  if (cased->make_fiche[0] == 'Y')  { 
    fprintf (otfile,"%s%s%s\n",assign,files->fchot,"  FOR055");  }

  /*  -  RUN statement         */

  fprintf (otfile,"$ run /nodebug  %s\n",cased->prognam); 

  /*  -  PLOT statement        */

  if (! strcmp (cased->plot_meth,"GKS")) {
    fprintf (otfile,"%s%s%s  %s  %s\n","$ @",plcom_dir,"plot_pdf", 
        files->pltot, cased->plot_dev,"11");  }
  else  {
    fprintf (otfile,"%s%s%s  %s  %s\n","$ @",plcom_dir,"plot_ps", 
        files->pltot, cased->plot_dev);  }
  fprintf (otfile,"%s %s\n",purge,files->rptot); 

  /*  -  SET PROT statements        */

  fprintf (otfile,"$ set noon \n");
  if (cased->replot[0] != 'Y')  {
    fprintf (otfile,"%s%s\n",set_prot,files->solot); }
  else  {
    fprintf (otfile,"%s%s\n",set_prot,files->solin); }
  if (*files->savot != '-')  {
    fprintf (otfile,"%s%s\n",set_prot,files->savot); } 
  fprintf (otfile,"%s%s\n",set_prot,files->rptot); 
  fprintf (otfile,"%s%s\n",set_prot,files->pltot); 
  fprintf (otfile,"%s%s\n",set_prot,files->auxot);
  if (cased->make_fiche[0] == 'Y')  { 
    fprintf (otfile,"%s%s\n",set_prot,files->fchot);  }

  /*  -  PURGE statements        */

  fprintf (otfile,"%s %s\n",purge,files->pltot); 
  fprintf (otfile,"%s %s\n",purge,files->auxot); 
  if (cased->replot[0] != 'Y')  {
    fprintf (otfile,"%s %s\n",purge,files->solot);  }
  if (*files->savot != '-')  {
    fprintf (otfile,"%s %s\n",purge,files->savot);  }

  /*  -  PRINT statements        */

  if (cased->prt_disp[0] == 'P')  {
    fprintf (otfile,"$ PRINT /feed /flag /delete %s\n",files->rptot); } 
  if (cased->make_fiche[0] == 'Y')  {
    strcpy (pffname,files->fchot);
    cpa = strstr(pffname,".SWF");
    strncpy (cpa,".PFF",4);
    fprintf (otfile,"$ APPEND %s %s\n",files->rptot,files->fchot);
    fprintf (otfile,"$ RENAME %s %s\n",files->fchot,pffname);  
    fprintf (otfile,"%s%s\n",set_prot,pffname); }
  fclose (otfile);
  /*  -  build fiche file if any fiche to be made  */
  if (cased->make_fiche[0] == 'Y')  {
    strcpy (otname,files->fchot);
    otfile = fopen (otname,"w","mrs=133","rat=cr","rfm=var");
    if (! otfile)  {
      strcpy(bad_flnam,otname);
      return bad;  }
    fprintf (otfile,"$MFLB     ");
    fprintf (otfile,"%6s  ",fiched->datecode);
    fprintf (otfile,"%-12s",fiched->user);
    fprintf (otfile,"%1d COPIES, ",cased->fiche_cnt);
    fprintf (otfile," ACCOUNT NUM: %s\n",fiched->account);
    fprintf (otfile,"$MFHD     ");
    fprintf (otfile,"SWING Case %-20s",cased->casenam);
    date (today); 
    fprintf (otfile,"%s\n",today);
    fclose (otfile);
    }  /*  done with fiche header output  */
  /*  -  build temp file to pass submit data back to PSAP  */
  strcpy (otname,"temp.dat");
  otfile = fopen (otname,"w");
  if (! otfile) {
    strcpy(bad_flnam,otname);
    return bad;  }
  fprintf (otfile,"%s\n",cased->casenam);
  fprintf (otfile,"%s\n",strupper (trim (cased->sbm_que)));
  /* fprintf (otfile,"%s %s %s",cased->prognam,cased->sbm_que,
     cased->casenam);  */
  fclose (otfile);
  /*         */
  return 0;
  }  /*  end of build_files()   */

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void send_quit_msg(void)  {
  /*  -  builds file that tells PSAP that user has aborted swing run 
         operation  ("QUIT" in file "temp.dat")  */
  char otname[128];
  FILE *otfile;
  /*                   */
  strcpy (otname,"temp.dat");
  otfile = fopen (otname,"w");
  if (! otfile) {
    fprintf (stderr,"SWGINITC [f] - unable to quit properly\n");
    exit (1);  }
  fprintf (otfile,"QUIT\n");
  fprintf (otfile,"QUIT\n");
  fclose (otfile);
  /*         */
  return;
  }  /*  end of send_quit_msg()   */

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

int chek_infiles (FILE_SET *files)  { 
  /*  -  Checks the existence of named input files.  If the filename begins
         with a '-', it is not checked  */  
  int not_found, stat_err, rtn_code, norm_color = 0;     
  FILE *filea; 
  stat_t stat_buff;

  rtn_code = 0;
  movetorc (5,71),  wrt_txt (norm_color,"         ");  
  movetorc (6,71),  wrt_txt (norm_color,"         ");  
  movetorc (7,71),  wrt_txt (norm_color,"         ");  
  movetorc (8,71),  wrt_txt (norm_color,"         ");
  movetorc (9,71),  wrt_txt (norm_color,"         ");

  if (*files->ctlin != '-')  {
    stat_err = stat (files->ctlin,&stat_buff);
    if (stat_err)  { 
      movetorc (5,71),  wrt_txt (norm_color,"Not found");  
      rtn_code = 1; 
    }
  }

  if (*files->savin != '-')  {
    stat_err = stat (files->savin,&stat_buff);
    if (stat_err)  { 
      movetorc (6,71),  wrt_txt (norm_color,"Not found");  
      rtn_code = 1; 
    }
  }

  if (*files->bsein != '-')  {
    stat_err = stat (files->bsein,&stat_buff);
    if (stat_err)  { 
      movetorc (7,71),  wrt_txt (norm_color,"Not found"); 
      rtn_code = 1; 
    }
  }

  if (*files->pstin != '-')  {
    stat_err = stat (files->pstin,&stat_buff);
    if (stat_err)  { 
      movetorc (8,71),  wrt_txt (norm_color,"Not found"); 
      rtn_code = 1; 
    }
  }

  if (*files->solin != '-')  {
    stat_err = stat (files->solin,&stat_buff);
    if (stat_err)  { 
      movetorc (9,71),  wrt_txt (norm_color,"Not found"); 
      rtn_code = 1; 
    }
  }  

  return rtn_code;
  }  /*  end of chek_infiles  */

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void bld_blank_flds (FIELD *fld_arr[])  {
  /* - Builds all the field structures & fills then with blank data  */
  fld_arr[CASE_NAM] = NewField ("Case name:",3,2, "!!!!!!!!!!", 12, 12, 
      3, 14);
  SetFieldValue (fld_arr[CASE_NAM],"          ");
  fld_arr[REPLOT] = NewField ("Replot (Y/N)?",4,2, "!",1,1,4,16);
  SetFieldValue (fld_arr[REPLOT]," ");
  fld_arr[PRT_KEEP] = NewField ("Printout ([K]eep or [P]rint):",5,2,
      "!",1,1,5,32);
  SetFieldValue (fld_arr[PRT_KEEP]," ");
  fld_arr[FICHE_FLG] = NewField ("Any fiche?",6,2,"!",1,1,6,19);
  SetFieldValue (fld_arr[FICHE_FLG]," ");
  fld_arr[FICHE_CPY] = NewField ("# Copies:",6,24,"9",1,1,6,34);
  SetFieldValue (fld_arr[FICHE_CPY]," ");
  fld_arr[DFLTDIR] = NewField ("Default directory:",8,2,"!!!!",35,35,8,21);
  SetFieldValue (fld_arr[DFLTDIR]," ");
  fld_arr[SWI_IN] =  NewField (" CONTROL INPUT",5,1,"!!!!",55,50,5,18);
  SetFieldValue (fld_arr[SWI_IN]," ");
  fld_arr[SAV_IN] =  NewField ("     SAVE DATA",6,1,"!!!!",55,50,6,18);
  SetFieldValue (fld_arr[SAV_IN]," ");
  fld_arr[PF_BASE] = NewField ("POWERFLOW BASE",7,1,"!!!!",55,50,7,18);
  SetFieldValue (fld_arr[PF_BASE]," ");
  fld_arr[PST_IN] =  NewField (" POSTSCRIP HDR",8,1,"!!!!",55,50,8,18);
  SetFieldValue (fld_arr[PST_IN]," ");
  fld_arr[SOL_IN] =  NewField (" STAB SOLUTION",9,1,"!!!!",55,50,9,18);
  SetFieldValue (fld_arr[SOL_IN]," ");
  fld_arr[SOL_OUT] = NewField (" STAB SOLUTION",13,1,"!!!!",55,50,13,18);
  SetFieldValue (fld_arr[SOL_OUT]," ");
  fld_arr[SAV_OUT] = NewField ("     SAVE DATA",14,1,"!!!!",55,50,14,18);
  SetFieldValue (fld_arr[SAV_OUT]," ");
  fld_arr[RPT_OUT] = NewField ("        REPORT",15,1,"!!!!",55,50,15,18);
  SetFieldValue (fld_arr[RPT_OUT]," ");
  fld_arr[PLT_OUT] = NewField ("          PLOT",16,1,"!!!!",55,50,16,18);
  SetFieldValue (fld_arr[PLT_OUT]," ");
  fld_arr[AUX_OUT] = NewField ("     AUXILIARY",17,1,"!!!!",55,50,17,18);
  SetFieldValue (fld_arr[AUX_OUT]," ");
  return; 
}  /*  end of   bld_blank_flds  */

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

int set_plt_method (CASE_DAT *ccase)  {
  /*  - Determines the method of plotting (PostScript vs. GKS) based on
        the name of the TSP executable.  */
  char *cpa, *cpb, *cpcolon, *cpbrak, *cpdig;

  strcpy (ccase->plot_meth,"GKS");        /*  default  */
  cpa = ccase->prognam;
  cpcolon = strchr (cpa,':');             /* VMS end of logical name  */
  if (! cpcolon) cpcolon = cpa;
  cpbrak = strchr (cpcolon,']');          /* VMS end of dir name */
  if (! cpbrak) cpbrak = cpcolon; 
  /*  Find first digit  */
  cpdig = cpbrak;
  cpdig += strcspn (cpdig,"0123456789"); 
  if (! *cpdig) return 0;                /* No digit in exec name  */
  if (*cpdig >= '6')   strcpy (ccase->plot_meth,"POSTSCRIPT"); 
  return 0;
}

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */
  
char *strupper (char *strin)  {
  /*  -  Converts a whole null-term'd string to upper case  */
  char *cpa;

  for (cpa = strin; *cpa; cpa++)  *cpa = toupper (*cpa); 
  return strin; 
}

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */
