/*                     KEYSTROKE.C 

    -  Set of routines for reading a key from the keyboard  */

#include "dem.h"
#include "keystroke.h"

/*  -  global variables for direct keybard input  */
extern io_stat_blk stat_blk;
extern short chan;
extern int readfunc;

/* -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=- */

int get_esc_code()  {
  /*  -  picks up following one or two characters after an escape key was
         read from the keyboard, indicative of a keypad or function key,
         and returns an integer corresponding to the codes up above  
      -  Note: these codes are tied to DEC keyboards of VT220 or later 
               vintage.  
                                     */
  int key1, key2; 
  /*                             */
  key1 = getkey();
  if (key1 == 91)  {    /* start of arrow key series */ 
    key2 = getkey(); 
    if (key2 == 67) return rt_key;
    if (key2 == 68) return lf_key;
    if (key2 == 65) return up_key;
    if (key2 == 66) return dn_key;  }
  if (key1 == 79)  {    /* start of keypad series */ 
    key2 = getkey(); 
    if (key2 == 67) return rt_key;
    if (key2 == 68) return lf_key;
    if (key2 == 65) return up_key;
    if (key2 == 66) return dn_key; 
    if (key2 == 112) return kp0_key;
    if (key2 == 113) return kp1_key;
    if (key2 == 114) return kp2_key;   
    if (key2 == 115) return kp3_key;   
    if (key2 == 116) return kp4_key;   
    if (key2 == 117) return kp5_key;   
    if (key2 == 118) return kp6_key;   
    if (key2 == 119) return kp7_key;   
    if (key2 == 120) return kp8_key;   
    if (key2 == 121) return kp9_key;   
    if (key2 == 109) return kpmin_key;
    if (key2 == 108) return kpcom_key;
    if (key2 == 110) return kpdot_key;
    if (key2 == 77) return kpent_key;   
    if (key2 == 80) return pf1_key;   
    if (key2 == 81) return pf2_key;   
    if (key2 == 82) return pf3_key;   
    if (key2 == 83) return pf4_key;  }
  /*  -  here if unregistered key  */
  return 0;
  }  /* end of get_esc_code  */

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */

int getkey (void)  {
  /*  -  function to get a single char from the keyboard  */
  /* typedef struct {
       short cond_value;
       short count;
       int info;  }  io_stat_blk;
     io_stat_blk stat_blk;     */
  /*                   */
  int SYS$QIOW ();
  void LIB$STOP ();
  int qiostat, fullkey, inkey;
  /*                   */
  qiostat = SYS$QIOW (0, chan, readfunc, &stat_blk, 0,0, &fullkey,1,0,0,0,0);
  if ( !(qiostat & 1)) LIB$STOP (qiostat); 
  inkey = fullkey & 0xff;          /*  supress leading bits  */
  return inkey;   
  }   

/*  -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-  -=-  */
