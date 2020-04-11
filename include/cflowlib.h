/*****************************************************************************
 * file:  cflowlib.h
 * author: Jay Coleman   May 1993
 * purpose: This is the header file used both by the CFLOW library and all
 *          CFLOW programs to define structures, constants, etc.
 *****************************************************************************
 */


/*****************************************************************************
 *   This is the set of cflow global buffers and assiciated data used by
 *   the cflow to powerflow inter-process communication
 */

#define CFLOW_IPC_BUFF_SIZE 4096

#ifdef CFLOW_GLOBAL_DATA
            char  pf_cflow_inbuf[CFLOW_IPC_BUFF_SIZE];
            char  pf_cflow_outbuf[CFLOW_IPC_BUFF_SIZE];
            char  err_buf[CFLOW_IPC_BUFF_SIZE];
            char  reply_pf[CFLOW_IPC_BUFF_SIZE];
            int   pf_cflow_socket;
            int   cf_debug=0;
#else
   extern   char  pf_cflow_inbuf[];
   extern   char  pf_cflow_outbuf[];
   extern   char  err_buf[];
   extern   char  reply_pf[];
   extern   int   pf_cflow_socket;
   extern   int   cf_debug;
#endif

/*****************************************************************************/


/*****************************************************************************
 *   This is the set of cflow constants used in pf_get_list
 */

enum  { AREA_LIST, BUS_LIST, KV_LIST, OWNER_LIST, REC_TYPE_LIST, ZONE_LIST };

/*****************************************************************************/


/*****************************************************************************
 *   This is the set of cflow structures used by the "pf_rec_..." functions
 */

/** @addtogroup input_data
 * 
 * The following structures are used for input to powerflow.
 * @{ */
typedef struct {      /* pf_AC_bus */
   char   type[3];
   char   owner[4];
   char   name[9];
   float  kv;
   char   zone[3];
   int    dummy1;
   float  Pload;
   float  Qload;
   float  Pshunt;
   float  Qshunt;
   float  Pmax;
   float  Pgen;
   float  Qsch_Qmax;
   float  Qmin;
   float  Vhold_Vmax;
   float  Vmin_Vdeg;
   char   rmt_name[9];
   float  rmt_kv;
   char   dummy2;
   float  pct_vars;
                                   } pf_AC_bus;

typedef struct {  /* pf_DC_bus */
   char   type[3];
   char   owner[4];
   char   name[9];
   float  kv;
   char   zone[3];
   int    bridges_per_ckt;
   float  smooth_rx_mh;
   float  alpha_min_deg;
   float  alpha_stop_deg;
   float  valve_drop_per_bridge_volts;
   float  bridge_current_rating_amps;
   float  alpha_gamma_N_deg;
   float  gamma_0_deg;      /* Changed from gamma_o_deg */
   float  P_sched;
   float  V_sched;
   float  dummy1;                  
   char   commutating_bus_name[9];
   float  commutating_bus_kv;
   char   converter_code;
   float  dummy2;
                                   } pf_DC_bus;

typedef struct {        /* pf_branch  -- generic branch record */
   char   type[3];
   char   owner[4];
   char   bus1_name[9];
   float  bus1_kv;
   int    meter;
   char   bus2_name[9];
   float  bus2_kv;
   char   ckt_id;
   int    section;
   float  total_rating;
   int    num_ckts;
   float  r;
   float  x;
   float  g;
   float  b;
   float  tap1;
   float  tap2;
   float  alpha_N_deg;
   float  gamma_0_deg;      /* Changed from gamma_o_deg */
   char   descrip[9];
   char   date_in[4];
   char   date_out[4];
   float  thermal_rating;
   float  bottleneck_rating;
   float  emergency_rating;
                                   } pf_branch;

typedef struct {  /* pf_LD */
   char   type[3];
   char   owner[4];
   char   bus1_name[9];
   float  bus1_kv;
   int    meter;
   char   bus2_name[9];
   float  bus2_kv;
   char   I_or_R_control;
   int    dummy1;
   float  total_rating;
   int    dummy2;
   float  R;
   float  L_mh;
   float  C_uf;
   float  P_sched;
   float  V_sched;
   float  miles;
   float  alpha_N_deg;
   float  gamma_0_deg;      /* Changed from gamma_o_deg */
   char   dummy3[9];
   char   dummy4[4];
   char   dummy5[4];
   float  thermal_rating;
   float  bottleneck_rating;
   float  dummy6;
                                   } pf_LD;

typedef struct {  /* pf_LM */
   char   type[3];
   char   owner[4];
   char   bus1_name[9];
   float  bus1_kv;
   int    meter;
   char   bus2_name[9];
   float  bus2_kv;
   char   dummy1;
   int    dummy2;
   float  total_rating;
   int    dummy3;
   float  R;
   float  L_mh;
   float  C_uf;
   float  dummy4;
   float  dummy5;
   float  miles;
   float  dummy6;
   float  dummy7;
   char   dummy8[9];
   char   date_in[4];
   char   date_out[4];
   float  thermal_rating;
   float  bottleneck_rating;
   float  dummy9;
                                   } pf_LM;

typedef struct {   /* pf_E */
   char   type[3];
   char   owner[4];
   char   bus1_name[9];
   float  bus1_kv;
   int    meter;
   char   bus2_name[9];
   float  bus2_kv;
   char   ckt_id;
   int    section;
   float  total_rating;
   int    num_ckts;
   float  r;
   float  x;
   float  g1;
   float  b1;
   float  g2;
   float  b2;
   float  dummy1;
   float  dummy2;
   char   dummy3[9];
   char   date_in[4];
   char   date_out[4];
   float  thermal_rating;
   float  bottleneck_rating;
   float  dummy4;
                                   } pf_E;

typedef struct {      /* pf_L */
   char   type[3];
   char   owner[4];
   char   bus1_name[9];
   float  bus1_kv;
   int    meter;
   char   bus2_name[9];
   float  bus2_kv;
   char   ckt_id;
   int    section;
   float  total_rating;
   int    num_ckts;
   float  r;
   float  x;
   float  g;
   float  b;
   float  miles;
   float  dummy1;
   float  dummy2;
   float  dummy3;
   char   descrip[9];
   char   date_in[4];
   char   date_out[4];
   float  thermal_rating;
   float  bottleneck_rating;
   float  dummy4;
                                   } pf_L;

typedef struct {    /* pf_T */
   char   type[3];
   char   owner[4];
   char   bus1_name[9];
   float  bus1_kv;
   int    meter;
   char   bus2_name[9];
   float  bus2_kv;
   char   ckt_id;
   int    section;
   float  total_rating;
   int    num_ckts;
   float  r;
   float  x;
   float  g;
   float  b;
   float  tap1;
   float  tap2;
   float  dummy1;
   float  dummy2;
   char   dummy3[9];
   char   date_in[4];
   char   date_out[4];
   float  thermal_rating;
   float  bottleneck_rating;
   float  emergency_rating;
                                   } pf_T;

typedef struct {    /* pf_TP */
   char   type[3];
   char   owner[4];
   char   bus1_name[9];
   float  bus1_kv;
   int    meter;
   char   bus2_name[9];
   float  bus2_kv;
   char   ckt_id;
   int    section;
   float  total_rating;
   int    num_ckts;
   float  r;
   float  x;
   float  g;
   float  b;
   float  phase_shift_deg;
   float  tap2;
   float  dummy1;
   float  dummy2;
   char   dummy3[9];
   char   date_in[4];
   char   date_out[4];
   float  thermal_rating;
   float  bottleneck_rating;
   float  emergency_rating;
                                   } pf_TP;

typedef struct {           /* pf_R  --  for R, RN, RQ, RV */
   char   type[3];
   char   owner[4];
   char   bus1_name[9];
   float  bus1_kv;
   int    var_tap_side;
   char   bus2_name[9];
   float  bus2_kv;
   char   dummy1;
   int    dummy2;
   float  dummy3;
   int    num_taps;
   float  dummy4;
   float  dummy5;
   float  dummy6;
   float  dummy7;
   float  max_tap;
   float  min_tap;
   float  dummy8;
   float  dummy9;
   char   rmt_bus_name[9];
   char   date_in[4];
   char   date_out[4];
   float  rmt_bus_kv;
   float  Qmax;
   float  Qmin;
                                   } pf_R;

typedef struct {     /* pf_RM  --  for RM, RP */
   char   type[3];
   char   owner[4];
   char   bus1_name[9];
   float  bus1_kv;
   int    var_tap_side;
   char   bus2_name[9];
   float  bus2_kv;
   char   dummy1;
   int    dummy2;
   float  dummy3;
   int    num_taps;
   float  dummy4;
   float  dummy5;
   float  dummy6;
   float  dummy7;
   float  max_phase_shift_deg;
   float  min_phase_shift_deg;
   float  dummy8;
   float  dummy9;
   char   rmt_bus_name[9];
   char   date_in[4];
   char   date_out[4];
   float  rmt_bus_kv;
   float  Pmax;
   float  Pmin;
                                   } pf_RM;

typedef struct {     /* pf_RZ */
   char   type[3];
   char   owner[4];
   char   bus1_name[9];
   float  bus1_kv;
   int    var_tap_side;
   char   bus2_name[9];
   float  bus2_kv;
   char   ckt_id;
   int    section;
   float  I_rate;
   int    rani_type;
   float  Pc_max;
   float  Pc_min;
   float  Xij_max;
   float  Xij_min;
   float  Bis_max;
   float  Bis_min;
   float  dummy1;
   float  dummy2;
   char   dummy3[9];
   char   dummy4[4];
   char   dummy5[4];
   float  dummy6;
   float  dummy7;
   float  dummy8;
                                   } pf_RZ;

typedef struct {   /* pf_area */
   char   type[3];
   char   name[11];
   char   sbus_name[9];
   float  sbus_kv;
   float  sched_export;
   char   zone0[3];
   char   zone1[3];
   char   zone2[3];
   char   zone3[3];
   char   zone4[3];
   char   zone5[3];
   char   zone6[3];
   char   zone7[3];
   char   zone8[3];
   char   zone9[3];
   float  max_Vpu;
   float  min_Vpu;
                                   } pf_area;

typedef struct {   /* pf_itie */
   char   type[3];
   char   area1_name[11];
   char   area2_name[11];
   float  sched_export;
                                   } pf_itie;


typedef struct {   /* pf_cbus */
   char   type[3];
   char   owner[4];
   char   name[9];
   float  kv;
   char   code_year[3];
   float  Pload;
   float  Qload;
   float  Gshunt;
   float  Bshunt;
   float  Pgen;
   float  Qgen_Qmax;
   float  Qmin;
                                   } pf_cbus;

typedef struct {   /* pf_qcurve */
   char   type[3];
   char   PU_code[3];
   char   active;
   char   bus_name[9];
   float  bus_kv;
   float  Pgen0;    /* Note: value is Qmin0 for QN cards, Qmax0 for QM cards, */
   float  Pgen1;    /* and Pgen0 for QP cards, but is always named Pgen0, etc.*/
   float  Pgen2;
   float  Pgen3;
   float  Pgen4;
   float  Pgen5;
   float  Pgen6;
   float  Pgen7;
   float  Pgen8;
   float  Pgen9;
                                   } pf_qcurve;

typedef struct {   /* pf_xdata */
   char   type[3];
   char   owner[4];
   char   bus_name[9];
   float  bus_kv;
   char   rmt_name[9];
   float  rmt_kv;
   int    seg1_num_steps;
   float  seg1_delta_mva;
   int    seg2_num_steps;
   float  seg2_delta_mva;
   int    seg3_num_steps;
   float  seg3_delta_mva;
   int    seg4_num_steps;
   float  seg4_delta_mva;
   int    seg5_num_steps;
   float  seg5_delta_mva;
   int    seg6_num_steps;
   float  seg6_delta_mva;
   int    seg7_num_steps;
   float  seg7_delta_mva;
   int    seg8_num_steps;
   float  seg8_delta_mva;
                                   } pf_xdata;

 /** @} */

/**@addtogroup solution_data
 *
 * The following structures are used for reaading solution (output) from powerflow 
 * @{ */

typedef struct {      /* pf_bus_AC_soln */
   char   type[3];
   float  Pgen;
   float  Qgen;
   float  Vmag;
   float  Vdeg;
   float  Pload;
   float  Qload;
   float  Bshunt_used;
   float  Bshunt_sch;
   float  Bshunt_used_cap;
   float  Bshunt_sch_cap;
   float  Bshunt_used_rx;
   float  Bshunt_sch_rx;
   float  Qunsch;
                                   } pf_bus_AC_soln;

typedef struct {      /* pf_bus_DC_soln */
   char   type[3];
   float  P_DC;
   float  Q_DC;
   float  V_DC;
   float  converter_deg;
   float  P_valve_losses;
   float  Q_valve_losses;
   float  dummy1;
   float  dummy2;
   float  dummy3;
   float  dummy4;
   float  dummy5;
   float  dummy6;
   float  dummy7;
} pf_bus_DC_soln;
                                   
typedef struct {      /* pf_branch_soln */
   char   type[3];
   int    num_ckts;     /* If ckt id was *, contains number of parallels */
   float  Pin;
   float  Qin;
   float  Pout;
   float  Qout;
   float  Ploss;
   float  Qloss;
   float  crit_line_load_amps;
   float  crit_line_rat_amps; 
   char   crit_line_rat_code;       
   int    crit_line_load_term;
   float  crit_xfmr_load_mva; 
   float  crit_xfmr_rat_mva;  
   char   crit_xfmr_rat_code;       
   int    crit_xfmr_load_term; 
   float  tot_line_load_pct;  
   float  tot_line_load_amps; 
   float  tot_xfmr_load_pct;  
   float  tot_xfmr_load_mva;  
   float  tap1;
   float  tap2;
} pf_branch_soln;

typedef struct {   /* pf_area_soln */
   char   type[3];
   float  Pgen;
   float  Pload;
   float  Ploss;
   float  Pexport;
} pf_area_soln;

typedef struct {   /* pf_itie_soln */
   char   type[3];
   float  Pexport;
   float  Pcirc;
   int    input_exists;  /* 0= no input record (internally generated itie)
                            1=    input data is from input record         */
} pf_itie_soln;

typedef struct {   /* pf_cbus_soln */
   char   type[3];
   float  Pgen;
   float  Qgen;
   float  Pload;
   float  Qload;
   float  Gshunt;
   float  Bshunt;
} pf_cbus_soln;

typedef struct {   /* pf_qcurve_soln */
   char   type[3];
   float  Pgen;
   float  Qgen;
} pf_qcurve_soln;

typedef struct {   /* pf_xdata_soln */
   char   type[3];
   int    seg1_sch_units;
   int    seg1_used_units;
   float  seg1_mvar_per_unit;
   int    seg2_sch_units;
   int    seg2_used_units;
   float  seg2_mvar_per_unit;
   int    seg3_sch_units;
   int    seg3_used_units;
   float  seg3_mvar_per_unit;
   int    seg4_sch_units;
   int    seg4_used_units;
   float  seg4_mvar_per_unit;
   int    seg5_sch_units;
   int    seg5_used_units;
   float  seg5_mvar_per_unit;
   int    seg6_sch_units;
   int    seg6_used_units;
   float  seg6_mvar_per_unit;
   int    seg7_sch_units;
   int    seg7_used_units;
   float  seg7_mvar_per_unit;
   int    seg8_sch_units;
   int    seg8_used_units;
   float  seg8_mvar_per_unit;
} pf_xdata_soln;

typedef pf_R  pf_RN;
typedef pf_R  pf_RQ;
typedef pf_R  pf_RV;
typedef pf_RM pf_RP;
/** @} */

typedef union {                /* input data */
  pf_AC_bus           ACbus;
  pf_DC_bus           DCbus;
  pf_branch          branch;
  pf_LD                  LD;
  pf_LM                  LM;
  pf_E                    E;
  pf_L                    L;
  pf_T                    T;
  pf_TP                  TP;
  pf_R                    R;
  pf_RN                  RN;
  pf_RQ                  RQ;
  pf_RV                  RV;
  pf_RP                  RP;
  pf_RM                  RM;
  pf_RZ                  RZ;
  pf_area              area;
  pf_itie              itie;
  pf_cbus              cbus;
  pf_qcurve          qcurve;
  pf_xdata            xdata;
  char            cmnt[120];
}  input_data;

typedef union {              /* solution data */
  pf_bus_AC_soln      ACbus;
  pf_bus_DC_soln      DCbus;
  pf_branch_soln     branch;
  pf_area_soln         area;
  pf_itie_soln         itie;
  pf_cbus_soln         cbus;
  pf_qcurve_soln     qcurve;
  pf_xdata_soln       xdata;
} solution_data;

typedef struct {   /* pf_rec */
  input_data    i;
  solution_data s;
} pf_rec;


typedef struct {   /* pf_comments */
   char   case_name[11];
   char   case_desc[21];   /* Changed from project_title */
   char   h[3][133];
   char   c[20][121];
} pf_comments;

/* Miscellaneous case data */
typedef struct {
   char   PF_version[11];         /**< Ten character string containing Powerflow version information. */
   float  base_mva;               /**< Base MVA of the base case (normally 100.0). */
   int    num_DC_systems;         /**< An integer count of the number of DC systems in the case. */
   int    num_areas;              /**< An integer count of the number of areas in the case. */
   int    num_ities;              /**< An integer count of the number of interties in the case. */
   int    num_zones;              /**< An integer count of the number of zones in the case. */
   int    num_owners;             /**< An integer count of the number of owners in the case. */
   int    num_buses;              /**< An integer count of the number of buses in the case (both AC and DC) */
   int    num_area_slack_buses;   /**< An integer count of the number of area slack buses in the case. */
   int    num_DC_buses;           /**< An integer count of the number of dc buses in the case. */
   int    num_AGC_buses;          /**< An integer count of the number of buses with AGC control in the case. */
   int    num_BX_buses;           /**< An integer count of the number of BX (constant V using switched Q) buses in the case. */
   int    num_adjustable_buses;   /**< An integer count of the number of adjustable buses in the case. */
   int    num_pct_var_ctrl_buses; /**< An integer count of the number of buses with percent VAR control in the case. */
   int    num_branches;           /**< An integer count of the number of branches in the case. */
   int    num_circuits;           /**< An integer count of the number of circuits in the case. All parallel lines count as one circuit. */
   int    num_DC_lines;           /**< An integer count of the number of DC linse in the case. */
   int    num_LTC_xfmrs;          /**< An integer count of the number of LTC transformers in the case. */
   int    num_phase_shifters;     /**< An integer count of the number of phase shifters in the case. */
   int    case_soln_status;       /**< An integer containing the solution status. Corresponds to enumerated variables as follows: 
                                    1 = NO_CASE  (no case data loaded)
                                    2 = UNSOLVED (netdata loaded)
                                    5 = SOLVED  (successful solution, or solved case loaded)
                                    6 = SAVED  (solved case has been saved) 
                                    7 = DIVERGED (unsuccessful solution - diverged) */
   int    num_diff_kv;            /**< An integer count of the number of unique kVs in the case. */
   int    num_rec_types;          /**< An integer count of the number of unique record types in the case. */
} pf_case_stats;

char *cfu_next_err_msg(void);

/** Close the data link to the powerﬂow engine (ipfsrv)
 *
 * Call this in to “disconnect” properly from ipfsrv.
 *
 * @return Has no return; it calls the exit function.
 *
 * @example pf_cflow_init_and_exit.c */
void pf_cflow_exit(void);
void pf_cflow_init( int argc, char *argv[] );
int pf_cflow_ipc(void);
int pf_del_area(char *area);
int pf_del_zone(char *zone);
int pf_rename_area(char *oldname, char *newname);
int pf_rename_bus(char *oldname, float oldkv, char *newname, float newkv);
int pf_rename_zone(char *oldname, char *newname);
int pf_load_changes(char *filename);
int pf_load_netdata(char *filename);
int pf_load_oldbase(char *filename);
int pf_put_inrec(char *record);
int pf_rec_area(pf_rec *r, char *action);
int pf_rec_branch(pf_rec *r, char *action);
int pf_rec_bus(pf_rec *r, char *action);
int pf_rec_cbus(pf_rec *r, char *action);
int pf_rec_comments(pf_comments *r, char *action);
int pf_rec_itie(pf_rec *r, char *action);
int pf_rec_qcurve(pf_rec *r, char *action);
int pf_rec_xdata(pf_rec *r, char *action);
int pf_save_changes(char *filename);
int pf_save_netdata(char *filename, char *dialect, char *ratings, int size);
int pf_save_newbase(char *filename);
int pf_save_wscc_stab_data(char *filename, char *type);
int pf_solution();
int pf_init();
int pf_get_list( char *list, int listlen, int type, char *data );

/** Finds the name of the area that a zone is in.
 *
 * @param[out] area      A pointer to an array of 11 characters in which the area name is returned.
 * @param[in]  zone_name A string holding a zone name.
 *
 * @return Returns 0 if it is successful; otherwise, it returns 1.
 *
 * @example pf_area_of_zone.c */
int pf_area_of_zone(char *area, char *zone);

/**  Retrieve case info
 * 
 * Retrieves data from a Powerflow base case and puts it in the info structure.
 *
 * @param[out] pf_case_stats A pointer to a structure of type pf_case_stats.
 *
 * @return Returns 0 if it is successful; otherwise, it returns 1.
 *
 * @example pf_case_info.c */
int pf_case_info(pf_case_stats *r);
void pf_init_rec(void *r, int rtype);
void pf_init_qcurve(pf_rec *r, char *type, char *name, float kv);
void pf_init_itie(pf_rec *r, char *type, char *area1, char *area2);
void pf_init_cbus(pf_rec *r, char *type, char *owner, char *name, float kv, char *year);
void pf_init_bus(pf_rec *r, char *type, char *name, float kv);
void pf_init_branch(pf_rec *r, char *type, char *name1, float kv1, char *name2, float kv2, char cid, int sid);
void pf_init_area(pf_rec *r, char *type, char *name);

/** Seee if a bus exists
 * 
 * @author William D. Rogers
 * @date 8-17-1994
 *
 * @param[in] name A pointer to a string containing the name.
 * @param[in] kv   A real value representing the bus base kV.
 * 
 * @return Returns 0 if the bus exists; otherwise, it returns 1.
 *
 * @example pf_bus_exists.c
 */
int pf_bus_exists(char *name, float kv);
int pf_b2a_bus(char *net_data, pf_rec *r, char *action); /* WDRogers */
int pf_b2a_branch(char *net_data, pf_rec *r, char *action); /* WDRogers */
int pf_rec_a2b(char *net_data, pf_rec *r, char *action); /* WDRogers */
int pf_rec_b2a(char *net_data, pf_rec *r, char *action); /* WDRogers 1-23-95 */
int pf_user_init_def(); /* wdr 1-6-95 */
int pf_user_load_def(char *definitions); /* wdr 1-6-95 */
int pf_user_sub_def(char *base); /* wdr 1-6-95 */
int pf_user_report(char *filename, char *output, char action); /* wdr 1-6-95, 5-21-96 */
int pf_user_define(char *symbol, char *id, char *type); /* wdr 1-30-95 */
int pf_user_comment(char *symbol, char *suffix, char *format); /* wdr 1-30-95 */
int pf_user_quantity(char *symbol, char *suffix, float *quantity);/*wdr1-30-95*/
int pf_user_string(char *symbol, int length, char *info); /* wdr 1-31-95 */
int pf_user_branch(char *symbol, pf_rec *r, char *type); /* wdr 1-31-95 */
int pf_user_itie(char *symbol, pf_rec *r, char *type);  /* wdr 1-31-95 */
int pf_user_bus(char *symbol, pf_rec *r, char *suffix); /* wdr 1-31-95 */
int pf_plot(char *cor_filename, char *ps_filename, char *options);/*wdr 7-6-95*/
int pf_load_refbase(char *filename); /* wdr 7-7-95 */
int pf_select_base(char base); /* wdr 7-21-95 */
int pf_solve_area(char base); /* wdr 3-6-96 */
int pf_command(char *command); /* wdr 5-6-96 */
