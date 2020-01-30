/*          keystroke.h 

    -  include file for reading a key from the keyboard  */

/*  -  for SYS$QIOW call  */
#include <iodef.h>
#include <ssdef.h>
#include <descrip.h>
#include <processes.h>

/*      A simple macro to construct a string descriptor (from descrip.h):
#define $DESCRIPTOR(name,string)
struct dsc$descriptor_s name = { sizeof(string)-1, DSC$K_DTYPE_T,
   DSC$K_CLASS_S, string }  */

/*  -  io-status structure for direct keybard input  */
typedef struct {
  short cond_value;
  short count;
  int info;  }  io_stat_blk;

/*  -  global variables to be declared above main routine  */
/* extern io_stat_blk stat_blk;
   extern short chan;
   extern int readfunc;      */

/*  -  key definitions  */
#define lf_key 260
#define rt_key 261
#define up_key 259
#define dn_key 258
#define del_key 127
#define tab_key 9
#define bksp_key 8
#define rtn_key 13
#define esc_key 27
#define ctl_y_key 25
#define ctl_c_key  3
#define kpmin_key -6
#define kpcom_key -7
#define kpent_key -8
#define kpdot_key -9
#define kp0_key -10
#define kp1_key -11
#define kp2_key -12
#define kp3_key -13
#define kp4_key -14
#define kp5_key -15
#define kp6_key -16
#define kp7_key -17
#define kp8_key -18
#define kp9_key -19
#define pf1_key 265
#define pf2_key 266
#define pf3_key 267
#define pf4_key 268
/*  -  end of key definitions  */

int getkey (void);
int get_esc_code(void); 

/*  -  end of keystroke.h      */
