/*                     fields.h

  ]  Contains type defs and subroutine prototypes for using FIELD
     structures  
             */
/* - Field structure  */
typedef struct {
  char label[32], value[80], editmask[80], dbase_id[12];
  int lab_len, val_dat_len, val_sho_len, val_sho_start;
  int labr, labc, valr, valc;     /* row & column positions */
  } FIELD;

/* -  global variables for cursor stor/recall functions   */
#ifndef position_def
#define position_def
typedef struct { int row; int col; } POSITION;
#endif

#define INSERT 0
#define OVERSTRIKE 1

FIELD *NewField (char *ulabel, int lbl_row, int lbl_col, char *edt_mask,
  int val_max_len, int val_disp_len, int val_row, int val_col);
    /*  ^-- similar to C++ constructor  */
int ShowField (FIELD *field_p);
int ShowFldValue (FIELD *field_p);
int EditField (FIELD *field_p, int *mode_p, char *new_val);
  /*  ^--  returns code with type of user exit  */
char *GetFieldValue (FIELD *field_p, char *rcv_str);
int SetFieldValue (FIELD *field_p, char *repl_val);
void normal_color (void);
void label_color (void);
void value_sho_color (void);
void value_edit_color (void);
int ibelong6 (int test, int val1, int val2, int val3, int val4,
              int val5, int val6);
int GoodFormat (int *key_p, char fmt_c);
void movetorc (int row, int col);
/*  -               end of fields.h              */
