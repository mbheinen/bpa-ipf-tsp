/*                     SWGINUTIL.C  
  -  Package of small utility programs to go with the SWGINIT program 

      */
#include <curses.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <jpidef.h>
#include "debug.h"
#include "dem.h"
#include "swginutil.h"

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void dpause ()  {
  /*  -  stops the program for a couple of seconds  */
  int la,lb,lc;
  int ka,kb; 
  /*         */
  ka = 1000;  kb = 1000; 
  /*         */
  for (la = 1; la <= ka; la ++) 
    for (lb = 1; lb <= kb; lb ++)  lc = lc * lc;
  return; 
  }  /*  -  end of dpause  */
 
/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

char *trim (char *string)  {
  /*  -  shortens a string to just before last blank or newline */
  char *cpa;
  int la;
  /*                   */
  la = strlen (string) - 1; 
  cpa = &string[la];
  for (; la >= 0; la--, cpa--)  {
    if (*cpa == ' ' || *cpa == '\n')  restart_loop;
    cpa++;  *cpa = '\0';    /* here if found latest non-blank */
    return string;  }
  /*  -  here if string was completely blank  */
  *string = '\0';
  return string;  }

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

char *blank_fill (char *string,int strg_len)  {
  /*  -  fills a string with blanks from its current null terminator
         to a new one at the strg_len - 1st position.
      -  Objective is so that strlen(string) will = strg_len  */
  int la, lb, ka, kb;
  char *cpa;
  /*            */
  ka = strlen (string);
  if (ka >= strg_len) return string;
  cpa = &string[ka];
  for (la = ka; la < strg_len; la++, cpa++)  *cpa = ' ';
  *cpa = '\0';
  return string;
  }  /*  end of blank_fill  */

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

int ibelong2 (int test, int val1, int val2)  {
  /*  -  determines of the test integer is one the 2 values.  Returns
         true if it is, false if not  */
  if (test == val1) return true;
  if (test == val2) return true;
  return false;  }

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

int ibelong3 (int test, int val1, int val2, int val3)  {
  /*  -  determines of the test integer is one the 3 values.  Returns
         true if it is, false if not  */
  if (test == val1) return true;
  if (test == val2) return true;
  if (test == val3) return true;
  return false;  }

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

int ibelong4 (int test, int val1, int val2, int val3, int val4)  {
  /*  -  determines of the test integer is one the 4 values.  Returns
         true if it is, false if not  */
  if (test == val1) return true;
  if (test == val2) return true;
  if (test == val3) return true;
  if (test == val4) return true;
  return false;  }

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

char *squeeze_blk (char *string)  {  
  /*  -  removes blanks from the middle of a char string.  If any are
         present, the string will end up with a shorter length  */
  char *cpa, *cpb;
  /*               */
  cpa = string;  cpb = string;
  for (; *cpa; cpa++)   {
    if (*cpa != ' ')  {  *cpb = *cpa;  cpb++;  }  }
  *cpb = '\0';
  return string;
  }      /*  end of squeeze_blk()  */

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

void fiche_codes (char *date_code, char *usernam, char *account)  {
  /*  -  produces the job date code and account strings that appear on
         a microfiche label  */ 
  char cd1[] = "0ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz?%&*+@=";
  char month[] = "123456789OND";
  time_t sec_time;
  struct tm *time_dat;       /* tm struct has dynamic alloc in localtime() */
  int ia;
  ulong jpstat, cputim, pgflts, wksetsz; 
  /*  -  required lengths:  char date_code[7], usernam[13], account[9]  */
  int debug = 1;
  /*               */
  time (&sec_time);
  time_dat = localtime (&sec_time);     /*  parts of date in integer form */
  date_code[0] = month[time_dat->tm_mon];
  date_code[1] = cd1[time_dat->tm_mday];
  date_code[2] = cd1[time_dat->tm_hour];
  date_code[3] = cd1[time_dat->tm_min];
  date_code[4] = cd1[time_dat->tm_sec];
  jpstat = jpinfo (&cputim, &pgflts, &wksetsz, usernam, account);
  date_code[5] = cd1[cputim % 60];
  date_code[6] = '\0';
  if (debug)  {
    cdbgeko ("fiche_codes - at end");
    cdbgwrs ("  date_code = ",date_code); 
    cdbgwrs ("  account   = ",account);  }
  }  /*  end of fiche_codes()  */  

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

long jpinfo (ulong *cputim, ulong *pgflts, ulong *wksetsz, 
            char *usernam, char *account)  { 
  /*  returns process information for the SYS$GETJPI VMS call.  */

  int SYS$GETJPIW ();

  typedef struct { 
    ushort buff_len;
    ushort code;
    char *buffer;
    ushort *rtn_len_p; }  ITEM_DESCRPTR; 

  ITEM_DESCRPTR item[7];
  short rlen[7]; 
  char procnam[16];
  ulong stat_blk[2], getjp_stat;
  ulong zero;
  int ia, ib;
  int debug = 1;
  int lngsz; 
  /*  required string lengths:  char account[9], usernam[13]  */

  lngsz = sizeof(long);
  /*  -  set input values for requested items  */
  item[0].code = JPI$_ACCOUNT;
  item[0].buffer = account;
  item[0].buff_len = 8;
  item[0].rtn_len_p = (ushort *) &rlen[0];

  item[1].code = JPI$_CPUTIM;
  item[1].buffer = (char *) cputim;
  item[1].buff_len = lngsz;
  item[1].rtn_len_p = (ushort *) &rlen[1];

  item[2].code = JPI$_PAGEFLTS;
  item[2].buffer = (char *) pgflts;
  item[2].buff_len = lngsz;
  item[2].rtn_len_p = (ushort *) &rlen[2];

  item[3].code = JPI$_PRCNAM;
  item[3].buffer = procnam;
  item[3].buff_len = 15;
  item[3].rtn_len_p = (ushort *) &rlen[3];

  item[4].code = JPI$_USERNAME;
  item[4].buffer = usernam;
  item[4].buff_len = 12;
  item[4].rtn_len_p = (ushort *) &rlen[4];

  item[5].code = JPI$_WSSIZE;
  item[5].buffer = (char *) wksetsz;
  item[5].buff_len = lngsz;
  item[5].rtn_len_p = (ushort *) &rlen[5];

  item[6].code = 0; 
  item[6].buffer = NULL;
  item[6].buff_len = 0;
  item[6].rtn_len_p = NULL;

  getjp_stat = SYS$GETJPIW (0,0,0,item,&stat_blk,0,0); 

  /*  -  put term null on strings  */
  
  account[rlen[0]] = '\0';
  procnam[rlen[3]] = '\0';
  usernam[rlen[4]] = '\0'; 
  
  /*  -  copy numeric data to integer vars  */
  memcpy (cputim,item[1].buffer,lngsz);
  memcpy (pgflts,item[2].buffer,lngsz);
  memcpy (wksetsz,item[5].buffer,lngsz);

  if (debug)  {
    cdbgeko ("jpinfo - at end of function");
    cdbgwrs ("  username     = ",usernam);
    cdbgwrs ("  procname     = ",procnam);
    cdbgwrs ("  account      = ",account);
    cdbgwri ("  cpusec(*100) = ",(int) *cputim);
    cdbgwri ("  page flts    = ",(int) *pgflts);
    cdbgwri ("  work set siz = ",(int) *wksetsz); } 
  return getjp_stat; 
  }  /*  end of jpinfo  */ 

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

char *date (char *today)  {  
  /*  -  returns today's date as a 9 char string of the form
         dd-mmm-yy  
      -  requires calling routine to set size today[10]   
                 */
  char month_set[] = "JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC";
  char month[4];
  int la, lb, ka, kb;
  time_t sec_time;
  struct tm *time_dat;       /* tm struct has dynamic alloc in localtime() */
  /*                */
  time (&sec_time);
  time_dat = localtime (&sec_time);
  strncpy (month,&month_set[3 * time_dat->tm_mon],3);  
  month[3] = '\0';
  sprintf (today,"%2d-%3s-%2d",time_dat->tm_mday,month,time_dat->tm_year);
  return today;  
  }  /*  end of date()  */

/*   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */
