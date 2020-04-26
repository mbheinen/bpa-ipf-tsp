/**
 * @file  cflowlib.h
 * 
 * This is the header file used both by the CFLOW library and all CFLOW 
 * programs to define structures, constants, etc. in the libcflow library. */

/* This is the set of cflow global buffers and assiciated data used by
 * the cflow to powerflow inter-process communication */

 /** This is the byte size of the in and out buffers used by CLFOW */
#define CFLOW_IPC_BUFF_SIZE 4096

#ifdef CFLOW_GLOBAL_DATA
   char  pf_cflow_inbuf[CFLOW_IPC_BUFF_SIZE];  /**< A buffer of the data most recently received from the ipfsrv "engine". */
   char  pf_cflow_outbuf[CFLOW_IPC_BUFF_SIZE]; /**< A buffer of the data most recently sent to the ipfsrv "engine". */
   char  err_buf[CFLOW_IPC_BUFF_SIZE];         /**< A buffer containing a null terminated list of error messages from the most recent ipfsrv "engine" communication. */
   char  reply_pf[CFLOW_IPC_BUFF_SIZE];        /**< A buffer containing a null terminated list of data from the most recent ipfsrv "engine" communication.*/
   int   pf_cflow_socket;                      /**< Gets set during internals of call to pf_cflow_init(). Contains the socket number in use by the libcflow API. */
   int   cf_debug=0;                           /**< Users of the API should set this to non-zero value if they which to have libcflow library perform extra debug logging of internals. */
#else
   extern   char  pf_cflow_inbuf[];
   extern   char  pf_cflow_outbuf[];
   extern   char  err_buf[];
   extern   char  reply_pf[];
   extern   int   pf_cflow_socket;
   extern   int   cf_debug;
#endif


/** This is an enumeration of options that you can use in calls to pf_get_list() */
enum pf_list_type { 
   AREA_LIST,     /**< A list of the different area names in the case. String length 11. Max size 50. */
   BUS_LIST,      /**< A list of the different bus names in the case. */
   KV_LIST,       /**< A list of the different bus kV's in the case. String length 4. Max size 150. */
   OWNER_LIST,    /**< A list of the different owners in the case. String length 4. Max size 450. */
   REC_TYPE_LIST, /**< A list of the different record types in the case. String length 3. Max size 50. */
   ZONE_LIST      /**< A list of the different zones in the case.. String length 3. Max size 150. */
};

/** @addtogroup input_data
 * 
 * The following structures are used for input to powerflow. This is the set of
 * CFLOW structures used by the "pf_rec_..." functions
 * @{ */

/** This structure holds power flow input data for an AC bus. The fields type, 
 * owner, name, kV, and zone are the same for both AC and DC buses. */
typedef struct {
   char   type[3];     /**< Two character bus record type, for example, B, BS, BQ, etc. */
   char   owner[4];    /**< Three character bus owner. */
   char   name[9];     /**< Eight character bus name. */
   float  kv;          /**< Base kV of the bus. */
   char   zone[3];     /**< Two character zone name. */
   int    dummy1;      /**< Ignore. This is a dummy field used for alignment purposes. */
   float  Pload;       /**< Real load in MW. */
   float  Qload;       /**< Reactive load in MVAR. */
   float  Pshunt;      /**< Real shunt in MW. */
   float  Qshunt;      /**< Reactive shunt in MVAR. */
   float  Pmax;        /**< Maximum real load in MW. */
   float  Pgen;        /**< Scheduled real power in MW. */
   float  Qsch_Qmax;   /**< Scheduled reactive load in MVAR (Qsch) or a real number designating maximum reactive power in MVAR (Qmax).*/
   float  Qmin;        /**< Minimum reactive power in MVAR.*/
   float  Vhold_Vmax;  /**< Voltage to hold in per unit (Vhold) or a real number designating a maximum voltage limit in per unit (Vmax), depending on the bus type.*/
   float  Vmin_Vdeg;   /**< Minimum voltage limit in per unit, or voltage angle for the BS bus*/
   char   rmt_name[9]; /**< Eight character remote bus name. */
   float  rmt_kv;      /**< Base kV of a remote bus. */
   char   dummy2;      /**< Ignore. This is a dummy field used for alignment purposes. */
   float  pct_vars;    /**< Percent vars supplied for control of remote bus. */
} pf_AC_bus;

/** This structure holds power flow input data for a DC bus. The fields type, 
 * owner, name, kV, and zone are the same for both AC and DC buses. */
typedef struct {
   char   type[3];                     /**< Two character bus record type. */
   char   owner[4];                    /**< Three character bus owner. */
   char   name[9];                     /**< Eight character bus name. */
   float  kv;                          /**< Base kV of the bus. */
   char   zone[3];                     /**< Two character zone name. */
   int    bridges_per_ckt;             /**< Number of dc bridges per circuit. */
   float  smooth_rx_mh;                /**< Smoothing reactance in millihenries (mH). */
   float  alpha_min_deg;               /**< Alpha_min in degrees. */
   float  alpha_stop_deg;              /**< Alpha_stop in degrees. */
   float  valve_drop_per_bridge_volts; /**< Voltage drop per valve. */
   float  bridge_current_rating_amps;  /**< DC current rating. */
   float  alpha_gamma_N_deg;           /**< Alpha_N or gamma_N in degrees. */
   float  gamma_0_deg;                 /**< Gamma_0 in degrees. */
   float  P_sched;                     /**< Scheduled power MW. */
   float  V_sched;                     /**< Scheduled voltage kV. */
   float  dummy1;                      /**< Ignore. This is a dummy field used for alignment purposes.  */
   char   commutating_bus_name[9];     /**< Eight character commutating bus name. */
   float  commutating_bus_kv;          /**< Commutating bus kV. */
   char   converter_code;              /**< One character converter code. */
   float  dummy2;                      /**< Ignore. This is a dummy field used for alignment purposes. */
} pf_DC_bus;

/** This structure holds power flow input data for a branch record. */
typedef struct {
   char   type[3];           /**< */
   char   owner[4];          /**< */
   char   bus1_name[9];      /**< */
   float  bus1_kv;           /**< */
   int    meter;             /**< */
   char   bus2_name[9];      /**< */
   float  bus2_kv;           /**< */
   char   ckt_id;            /**< */
   int    section;           /**< */
   float  total_rating;      /**< */
   int    num_ckts;          /**< */
   float  r;                 /**< */
   float  x;                 /**< */
   float  g;                 /**< */
   float  b;                 /**< */
   float  tap1;              /**< */
   float  tap2;              /**< */
   float  alpha_N_deg;       /**< */
   float  gamma_0_deg;       /**< */
   char   descrip[9];        /**< */
   char   date_in[4];        /**< */
   char   date_out[4];       /**< */
   float  thermal_rating;    /**< */
   float  bottleneck_rating; /**< */
   float  emergency_rating;  /**< */
} pf_branch;

/** This structure holds power flow input data for a two terminal DC line record. */
typedef struct {
   char   type[3];           /**< */
   char   owner[4];          /**< */
   char   bus1_name[9];      /**< */
   float  bus1_kv;           /**< */
   int    meter;             /**< */
   char   bus2_name[9];      /**< */
   float  bus2_kv;           /**< */
   char   I_or_R_control;    /**< */
   int    dummy1;            /**< */
   float  total_rating;      /**< */
   int    dummy2;            /**< */
   float  R;                 /**< */
   float  L_mh;              /**< */
   float  C_uf;              /**< */
   float  P_sched;           /**< */
   float  V_sched;           /**< */
   float  miles;             /**< */
   float  alpha_N_deg;       /**< */
   float  gamma_0_deg;       /**< */
   char   dummy3[9];         /**< */
   char   dummy4[4];         /**< */
   char   dummy5[4];         /**< */
   float  thermal_rating;    /**< */
   float  bottleneck_rating; /**< */
   float  dummy6;            /**< */
} pf_LD;

/** This structure holds power flow input data for a multiterminal DC line record. */
typedef struct {
   char   type[3];           /**< */
   char   owner[4];          /**< */
   char   bus1_name[9];      /**< */
   float  bus1_kv;           /**< */
   int    meter;             /**< */
   char   bus2_name[9];      /**< */
   float  bus2_kv;           /**< */
   char   dummy1;            /**< */
   int    dummy2;            /**< */
   float  total_rating;      /**< */
   int    dummy3;            /**< */
   float  R;                 /**< */
   float  L_mh;              /**< */
   float  C_uf;              /**< */
   float  dummy4;            /**< */
   float  dummy5;            /**< */
   float  miles;             /**< */
   float  dummy6;            /**< */
   float  dummy7;            /**< */
   char   dummy8[9];         /**< */
   char   date_in[4];        /**< */
   char   date_out[4];       /**< */
   float  thermal_rating;    /**< */
   float  bottleneck_rating; /**< */
   float  dummy9;
} pf_LM;

/** This structure holds power flow input data for a equivalent transmission 
line record (asymmetric pi representation). */
typedef struct {
   char   type[3];           /**< */
   char   owner[4];          /**< */
   char   bus1_name[9];      /**< */
   float  bus1_kv;           /**< */
   int    meter;             /**< */
   char   bus2_name[9];      /**< */
   float  bus2_kv;           /**< */
   char   ckt_id;            /**< */
   int    section;           /**< */
   float  total_rating;      /**< */
   int    num_ckts;          /**< */
   float  r;                 /**< */
   float  x;                 /**< */
   float  g1;                /**< */
   float  b1;                /**< */
   float  g2;                /**< */
   float  b2;                /**< */
   float  dummy1;            /**< */
   float  dummy2;            /**< */
   char   dummy3[9];         /**< */
   char   date_in[4];        /**< */
   char   date_out[4];       /**< */
   float  thermal_rating;    /**< */
   float  bottleneck_rating; /**< */
   float  dummy4;            /**< */
} pf_E;

/** This structure holds power flow input data for an AC line record 
(symmetric pi representation). */
typedef struct {
   char   type[3];           /**< */
   char   owner[4];          /**< */
   char   bus1_name[9];      /**< */
   float  bus1_kv;           /**< */
   int    meter;             /**< */
   char   bus2_name[9];      /**< */
   float  bus2_kv;           /**< */
   char   ckt_id;            /**< */
   int    section;           /**< */
   float  total_rating;      /**< */
   int    num_ckts;          /**< */
   float  r;                 /**< */
   float  x;                 /**< */
   float  g;                 /**< */
   float  b;                 /**< */
   float  miles;             /**< */
   float  dummy1;            /**< */
   float  dummy2;            /**< */
   float  dummy3;            /**< */
   char   descrip[9];        /**< */
   char   date_in[4];        /**< */
   char   date_out[4];       /**< */
   float  thermal_rating;    /**< */
   float  bottleneck_rating; /**< */
   float  dummy4;            /**< */
} pf_L;

/** This structure holds power flow input data for transformer record. */
typedef struct {
   char   type[3];           /**< */
   char   owner[4];          /**< */
   char   bus1_name[9];      /**< */
   float  bus1_kv;           /**< */
   int    meter;             /**< */
   char   bus2_name[9];      /**< */
   float  bus2_kv;           /**< */
   char   ckt_id;            /**< */
   int    section;           /**< */
   float  total_rating;      /**< */
   int    num_ckts;          /**< */
   float  r;                 /**< */
   float  x;                 /**< */
   float  g;                 /**< */
   float  b;                 /**< */
   float  tap1;              /**< */
   float  tap2;              /**< */
   float  dummy1;            /**< */
   float  dummy2;            /**< */
   char   dummy3[9];         /**< */
   char   date_in[4];        /**< */
   char   date_out[4];       /**< */
   float  thermal_rating;    /**< */
   float  bottleneck_rating; /**< */
   float  emergency_rating;  /**< */
} pf_T;

/** This structure holds power flow input data for phase shifting transformer record. */
typedef struct {
   char   type[3];           /**< */
   char   owner[4];          /**< */
   char   bus1_name[9];      /**< */
   float  bus1_kv;           /**< */
   int    meter;             /**< */
   char   bus2_name[9];      /**< */
   float  bus2_kv;           /**< */
   char   ckt_id;            /**< */
   int    section;           /**< */
   float  total_rating;      /**< */
   int    num_ckts;          /**< */
   float  r;                 /**< */
   float  x;                 /**< */
   float  g;                 /**< */
   float  b;                 /**< */
   float  phase_shift_deg;   /**< */
   float  tap2;              /**< */
   float  dummy1;            /**< */
   float  dummy2;            /**< */
   char   dummy3[9];         /**< */
   char   date_in[4];        /**< */
   char   date_out[4];       /**< */
   float  thermal_rating;    /**< */
   float  bottleneck_rating; /**< */
   float  emergency_rating;  /**< */
} pf_TP;

/** This structure holds power flow input data for regulating transformer record. Struct for R, RN, RQ, RV type records */
typedef struct {
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

/** Struct for RM, RP record types */
typedef struct {
   char   type[3];             /**< */
   char   owner[4];            /**< */
   char   bus1_name[9];        /**< */
   float  bus1_kv;             /**< */
   int    var_tap_side;        /**< */
   char   bus2_name[9];        /**< */
   float  bus2_kv;             /**< */
   char   dummy1;              /**< */
   int    dummy2;              /**< */
   float  dummy3;              /**< */
   int    num_taps;            /**< */
   float  dummy4;              /**< */
   float  dummy5;              /**< */
   float  dummy6;              /**< */
   float  dummy7;              /**< */
   float  max_phase_shift_deg; /**< */
   float  min_phase_shift_deg; /**< */
   float  dummy8;              /**< */
   float  dummy9;              /**< */
   char   rmt_bus_name[9];     /**< */
   char   date_in[4];          /**< */
   char   date_out[4];         /**< */
   float  rmt_bus_kv;          /**< */
   float  Pmax;                /**< */
   float  Pmin;                /**< */
} pf_RM;

typedef struct {
   char   type[3];      /**< */
   char   owner[4];     /**< */
   char   bus1_name[9]; /**< */
   float  bus1_kv;      /**< */
   int    var_tap_side; /**< */
   char   bus2_name[9]; /**< */
   float  bus2_kv;      /**< */
   char   ckt_id;       /**< */
   int    section;      /**< */
   float  I_rate;       /**< */
   int    rani_type;    /**< */
   float  Pc_max;       /**< */
   float  Pc_min;       /**< */
   float  Xij_max;      /**< */
   float  Xij_min;      /**< */
   float  Bis_max;      /**< */
   float  Bis_min;      /**< */
   float  dummy1;       /**< */
   float  dummy2;       /**< */
   char   dummy3[9];    /**< */
   char   dummy4[4];    /**< */
   char   dummy5[4];    /**< */
   float  dummy6;       /**< */
   float  dummy7;       /**< */
   float  dummy8;       /**< */
} pf_RZ;

typedef struct {
   char   type[3];      /**< Record type; here "A" for area record. */
   char   name[11];     /**< Ten character area name. */
   char   sbus_name[9]; /**< Eight character area slack bus name. */
   float  sbus_kv;      /**< Base kV of the area slack bus. */
   float  sched_export; /**< Scheduled export power from an area. */
   char   zone0[3];     /**< Two character zone defined to be in an area. */
   char   zone1[3];     /**< Two character zone defined to be in an area. */
   char   zone2[3];     /**< Two character zone defined to be in an area. */
   char   zone3[3];     /**< Two character zone defined to be in an area. */
   char   zone4[3];     /**< Two character zone defined to be in an area. */
   char   zone5[3];     /**< Two character zone defined to be in an area. */
   char   zone6[3];     /**< Two character zone defined to be in an area. */
   char   zone7[3];     /**< Two character zone defined to be in an area. */
   char   zone8[3];     /**< Two character zone defined to be in an area. */
   char   zone9[3];     /**< Two character zone defined to be in an area. */
   float  max_Vpu;      /**< Maximum per unit voltage. */
   float  min_Vpu;      /**< Minimum per unit voltage. */
} pf_area;

typedef struct {
   char   type[3];        /**< */
   char   area1_name[11]; /**< */
   char   area2_name[11]; /**< */
   float  sched_export;   /**< */
} pf_itie;


typedef struct {
   char   type[3];      /**< */
   char   owner[4];     /**< */
   char   name[9];      /**< */
   float  kv;           /**< */
   char   code_year[3]; /**< */
   float  Pload;        /**< */
   float  Qload;        /**< */
   float  Gshunt;       /**< */
   float  Bshunt;       /**< */
   float  Pgen;         /**< */
   float  Qgen_Qmax;    /**< */
   float  Qmin;         /**< */
} pf_cbus;

typedef struct {
   char   type[3];     /**< */
   char   PU_code[3];  /**< */
   char   active;      /**< */
   char   bus_name[9]; /**< */
   float  bus_kv;      /**< */
   float  Pgen0;       /**< Value is Qmin0 for QN records, Qmax0 for QM cards, and Pgen0 for QP cards, but is always named Pgen0, etc. */
   float  Pgen1;       /**< */
   float  Pgen2;       /**< */
   float  Pgen3;       /**< */
   float  Pgen4;       /**< */
   float  Pgen5;       /**< */
   float  Pgen6;       /**< */
   float  Pgen7;       /**< */
   float  Pgen8;       /**< */
   float  Pgen9;       /**< */
} pf_qcurve;

typedef struct {
   char   type[3];        /**< */
   char   owner[4];       /**< */
   char   bus_name[9];    /**< */
   float  bus_kv;         /**< */
   char   rmt_name[9];    /**< */
   float  rmt_kv;         /**< */
   int    seg1_num_steps; /**< */
   float  seg1_delta_mva; /**< */
   int    seg2_num_steps; /**< */
   float  seg2_delta_mva; /**< */
   int    seg3_num_steps; /**< */
   float  seg3_delta_mva; /**< */
   int    seg4_num_steps; /**< */
   float  seg4_delta_mva; /**< */
   int    seg5_num_steps; /**< */
   float  seg5_delta_mva; /**< */
   int    seg6_num_steps; /**< */
   float  seg6_delta_mva; /**< */
   int    seg7_num_steps; /**< */
   float  seg7_delta_mva; /**< */
   int    seg8_num_steps; /**< */
   float  seg8_delta_mva; /**< */
} pf_xdata;
/** @} */

/** @addtogroup solution_data
 *
 * The following structures are used for reaading solution (output) from powerflow 
 * @{ */

/** This struct stores power flow solution values for an AC bus */
typedef struct {
   char   type[3];         /**< Two character bus record type, for example, B, BS, BQ, etc. */
   float  Pgen;            /**< Solved real power generation in MW.*/
   float  Qgen;            /**< Solved reactive power in MVAR. */
   float  Vmag;            /**< Solved voltage magnitude in per unit. */
   float  Vdeg;            /**< Solved voltage angle in degrees. */
   float  Pload;           /**< Solved real load in MW. */
   float  Qload;           /**< Solved reactive load in MVAR. */
   float  Bshunt_used;     /**< Total shunt used, net of capacitors (+) and reactors (-). */
   float  Bshunt_sch;      /**< Total shunt available, net of capacitors and reactors. */
   float  Bshunt_used_cap; /**< Capacitive shunt used, MVAR. */
   float  Bshunt_sch_cap;  /**< Capacitive shunt available, MVAR. */
   float  Bshunt_used_rx;  /**< Reactive shunt used, MVAR. */
   float  Bshunt_sch_rx;   /**< Reactive shunt available, MVAR. */
   float  Qunsch;          /**< MVARs produced, on a type BS or BE bus. */
} pf_bus_AC_soln;

/** This struct stores power flow solution values for a DC bus */
typedef struct {
   char   type[3];        /**< Two character bus record type. */
   float  P_DC;           /**< AC real power into the DC bus, positive at the rectifier and negative at the inverter. */
   float  Q_DC;           /**< AC reactive power into the DC bus, returned as a positive number. */
   float  V_DC;           /**< DC terminal voltage (final voltage at commutating bus,  in kV).*/
   float  converter_deg;  /**< Converter angle, @math alpha for rectifier, @math gamma for inverter, in degrees*/
   float  P_valve_losses; /**< Difference between AC power and DC power. */
   float  Q_valve_losses; /**< Difference between AC power and DC power (same as Q_DC). */
   float  dummy1;         /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy2;         /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy3;         /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy4;         /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy5;         /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy6;         /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy7;         /**< Ignore. This is a dummy field used for alignment purposes. */
} pf_bus_DC_soln;
                                   
typedef struct {
   char   type[3];             /**< Two character record type. */
   int    num_ckts;            /**< If ckt id was "*"", contains number of parallels */
   float  Pin;                 /**< Real power flow at bus1, in MW. Positive indicates flow from bus1 toward bus2. */
   float  Qin;                 /**< Reactive power flow at bus1, in MVAR. Positive indicates flow from bus1 toward bus2.  */
   float  Pout;                /**< Real power flow at bus2, in MW. Negative indicates flow from bus1 to bus2 (i.e., without losses, Pout = -Pin). */
   float  Qout;                /**< Reactive power flow at bus2, in MVAR.  Negative indicates flow from bus1 to bus2. */
   float  Ploss;               /**< Real losses, in MW (Pin + Pout). */
   float  Qloss;               /**< Reactive losses, in Mvar (Qin + Qout). */
   float  crit_line_load_amps; /**< Largest current in any section of a line (amps). */
   float  crit_line_rat_amps;  /**< Actual value of rating used (amps). */
   char   crit_line_rat_code;  /**< Type of rating used – N, B, T, or E. */
   int    crit_line_load_term; /**< */
   float  crit_xfmr_load_mva;  /**< Largest flow in a transformer (MVA). */
   float  crit_xfmr_rat_mva;   /**< Actual value of rating used (MVA). */
   char   crit_xfmr_rat_code;  /**< Type of rating used – N, B, T, or E. */
   int    crit_xfmr_load_term; /**< */
   float  tot_line_load_pct;   /**< Percent line compensation (total negative reactance divided by total positive reactance). */
   float  tot_line_load_amps;  /**< */
   float  tot_xfmr_load_pct;   /**< Percent loading on the transformer, using the indicated rating*/
   float  tot_xfmr_load_mva;   /**< */
   float  tap1;                /**< Final tap at bus1 of transformer, in kV for normal tap, in degrees for phase shifter. */
   float  tap2;                /**< Final tap at bus2 of transformer, in kV. */
} pf_branch_soln;

typedef struct {
   char   type[3]; /**< Two character record type. */
   float  Pgen;    /**< Total area generation. */
   float  Pload;   /**< Total area load. */
   float  Ploss;   /**< Total area losses. */
   float  Pexport; /**< Total actual export. */
} pf_area_soln;

typedef struct {
   char   type[3];      /**< Two character record type. */
   float  Pexport;
   float  Pcirc;
   int    input_exists; /**< 0= no input record (internally generated itie) <br>
                             1=    input data is from input record         */
} pf_itie_soln;

typedef struct {
   char   type[3]; /**< */
   float  Pgen;    /**< */
   float  Qgen;    /**< */
   float  Pload;   /**< */
   float  Qload;   /**< */
   float  Gshunt;  /**< */
   float  Bshunt;  /**< */
} pf_cbus_soln;

typedef struct {
   char   type[3]; /**< */
   float  Pgen;    /**< */
   float  Qgen;    /**< */
} pf_qcurve_soln;

typedef struct {
   char   type[3];            /**< */
   int    seg1_sch_units;     /**< */
   int    seg1_used_units;    /**< */
   float  seg1_mvar_per_unit; /**< */
   int    seg2_sch_units;     /**< */
   int    seg2_used_units;    /**< */
   float  seg2_mvar_per_unit; /**< */
   int    seg3_sch_units;     /**< */
   int    seg3_used_units;    /**< */
   float  seg3_mvar_per_unit; /**< */
   int    seg4_sch_units;     /**< */
   int    seg4_used_units;    /**< */
   float  seg4_mvar_per_unit; /**< */
   int    seg5_sch_units;     /**< */
   int    seg5_used_units;    /**< */
   float  seg5_mvar_per_unit; /**< */
   int    seg6_sch_units;     /**< */
   int    seg6_used_units;    /**< */
   float  seg6_mvar_per_unit; /**< */
   int    seg7_sch_units;     /**< */
   int    seg7_used_units;    /**< */
   float  seg7_mvar_per_unit; /**< */
   int    seg8_sch_units;     /**< */
   int    seg8_used_units;    /**< */
   float  seg8_mvar_per_unit; /**< */
} pf_xdata_soln;

typedef pf_R  pf_RN;
typedef pf_R  pf_RQ;
typedef pf_R  pf_RV;
typedef pf_RM pf_RP;
/** @} */

/** Input data union. */
typedef union {
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
} input_data;

/** Solution (output) data union. */
typedef union {
  pf_bus_AC_soln      ACbus;
  pf_bus_DC_soln      DCbus;
  pf_branch_soln     branch;
  pf_area_soln         area;
  pf_itie_soln         itie;
  pf_cbus_soln         cbus;
  pf_qcurve_soln     qcurve;
  pf_xdata_soln       xdata;
} solution_data;

/** Record data */
typedef struct {
  input_data    i; /**< Used to access power flow input data. */
  solution_data s; /**< Used to access power flow solution (output) data. */
} pf_rec;

/** Comment card data */
typedef struct {
   char   case_name[11];   /**< Case name */
   char   case_desc[21];   /**< Case description */
   char   h[3][133];       /**< Case header information */
   char   c[20][121];      /**< Case comments */
} pf_comments;

/** Miscellaneous case data */
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
   int    case_soln_status;       /**< An integer containing the solution status. Corresponds to enumerated variables as follows: <br>
                                    1 = NO_CASE  (no case data loaded) <br>
                                    2 = UNSOLVED (netdata loaded) <br>
                                    5 = SOLVED  (successful solution, or solved case loaded) <br>
                                    6 = SAVED  (solved case has been saved) <br>
                                    7 = DIVERGED (unsuccessful solution - diverged) */
   int    num_diff_kv;            /**< An integer count of the number of unique kVs in the case. */
   int    num_rec_types;          /**< An integer count of the number of unique record types in the case. */
} pf_case_stats;

char *cfu_next_err_msg(void);

/** Close the data link to the powerﬂow engine (ipfsrv)
 *
 * Call this in to “disconnect” properly from ipfsrv.
 *
 * @return Has no return; it calls the exit function. */
void pf_cflow_exit(void);
/** @example pf_main.c **/

/** Initialize the data link to the powerﬂow engine (ipfsrv)
 *
 * Establishes a socket connection with the Powerflow process (ipfsrv). Other 
 * command line arguments that have been collected by the argv mechanism may be
 * used by the CFLOW program. The command line arguments are "shifted left" 
 * such that *argv[1] contains the first command line argument intended for 
 * the CFLOW program and argc is updated to reflect the count of those 
 * arguments only.
 *
 * @return Returns 0 if it is successful; otherwise, it calls the exit function.*/
void pf_cflow_init(int argc, char *argv[]);
 /** @example pf_main.c */

/** Buffer interface to powerﬂow
 *
 * A low-level interface to the interprocess communication that uses two global
 * buffers pf_cflow_inbuf and pf_cflow_outbuf. This routine is used by most of 
 * the other pf functions; however, you can also use it directly. You put valid 
 * PCL commands and associated WSCC-formatted data records into 
 * pf_cflow_outbuf, call pf_cflow_ipc, then look for the results in 
 * pf_cflow_inbuf. 
 * 
 * A list and description of the valid PCL commands is in the documentation. 
 *
 * @return Returns 0 if it is successful; otherwise, it calls the exit function. */
int pf_cflow_ipc(void);

/** Delete area by name
 *
 * Delete an area along will all buses and all associated branches in the area.
 *
 * @param[in] area A string representing an area name. 
 *
 * @return Returns 0 (zero) if it is successful; otherwise, it returns 1.
 *
 * @see pf_del_zone
 *
 * @code error = pf_del_area("ARIZONA"); @endcode */
int pf_del_area(char *area);

/** Delete zone by name
 *
 * Delete a zone along will all buses and all associated branches in the zone.
 *
 * @param[in] zone  A string representing a zone name. 
 *
 * @return Returns 0 (zero) if it is successful; otherwise, it returns 1.
 *
 * @see pf_del_area
 *
 * @code error = pf_del_zone("NA"); @endcode */
int pf_del_zone(char *zone);

/** 
 *
 * @param 
 * 
 * @return 
 *
 * @example */
int pf_rename_area(char *oldname, char *newname);

/** 
 *
 * @param
 * 
 * @return 
 *
 * @example */
int pf_rename_bus(char *oldname, float oldkv, char *newname, float newkv);

/** 
 *
 * @param
 * 
 * @return */
int pf_rename_zone(char *oldname, char *newname);

/** Load changes into case
 *
 * Passes an ASCII change file name to ipfsrv so that it can read and interpret
 * the file or @p filename contains an "*\n" followed by change records which 
 * are to be processed by ipfsrv. The records must be separated by "\n".
 *
 * @param[in] filename A string representing a file name, or  "*\n", followed by valid change records.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_load_changes(char *filename);

/** Load network data
 *
 * Passes a network data file name to the ipfsrv process so that it can read 
 * and interpret the network data file. Or contains an "*\n" followed by bus 
 * and branch records which are to be processed by ipfsrv. The records must be
 * separated by "\n". If a case is currently loaded, it is overwritten and the
 * data is lost. The case loaded is not usable by GUI after CFLOW has 
 * completed.
 *
 * @param[in] filename A string representing a file name or an "*\n" followed 
 *                     by bus and branch records.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_load_netdata(char *filename);

/** Load a base case
 *
 * Passes a base case filename to the ipfsrv process so that it can read and 
 * interpret the file as an "oldbase". If a case is currently loaded, it is 
 * overwritten and the data is lost.
 *
 * @param[in] filename A string representing a file name followed by an optional, "rebuild = ON|OFF".

 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_load_oldbase(char *filename);

/** @example pf_load_functions.c */

/** Send WSCC change record to powerﬂow
 *
 * Changes, adds, or deletes an input data record for Powerflow. See Record 
 * Format section for a description of input record types and rules for adding,
 * changing, and deleting. This function provides a means of inputting records
 * for which there is not a specific function such as factor change (P), 
 * although it can be used for inputting any WSCC input record
 *
 * @param record A string containing WSCC formatted data for a Powerflow input 
 *               data record.
 * 
 * @return Returns 0 (zero) if it is successful; otherwise, it returns 1. */
int pf_put_inrec(char *record);
 
/** @example pf_put_inrec.c 
 * The following program uses pf_put_inrec to change an input data record in 
 * Powerflow and then outputs a success or failure message to the screen. */

/** @addtogroup pf_rec 
 * Read, write, and modify powerﬂow records
 *
 * The pf_rec_ functions allow powerflow input (network data) and output 
 * (solution) data to be retrieved, as well as allowing input data (network 
 * data) to be added, modified, or deleted.
 *
 * @{ */

/** Retrieves, modifies, adds, or deletes area data.
 *
 * @param[in] r      A pointer to a structure of type pf_rec supplied by the 
 *                   calling routine.
 * @param[in] action A string designating the action to be performed on an area
 *                   record. See below for the codes and their meanings. 
 *                   Either upper or lower case is acceptable.<br>
 *                     "F" Retrieves the first area record.<br>
 *                     "N" Retrieves the next area record. (Area name must be valid.)<br>
 *                     "G" Retrieves the rest of the area record. (Area name must be valid.)<br>
 *                     "D" Deletes an area record. (Area name must be valid.)<br>
 *                     "A" Adds an area record. (All required data fields must be valid. See the IPF Batch User’s Guide .)<br>
 *                     "M" Modifies an area record. (All required data fields must be valid. See the IPF Batch User’s Guide .)<br>
 *                     "E" Eliminates all area records. (area_rec is ignored and can be set to zero. This code does not delete any zones, buses, etc. It places all zones in area "blank.") <br>
 *                     "O" Retrieves the solution output data. (The case must be solved and the area name must be valid.)<br>
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_rec_area(pf_rec *r, char *action);

/** Retrieves, modifies, adds, or deletes branch records.
 *
 * @param[in|out] r      A pointer to a structure of type pf_rec, supplied by the calling routine.
 * @param[in]     action A string designating the action to be performed on a 
 *                   branch record. See below for the codes and their meanings.
 *                   Either upper or lower case is acceptable.<br>
 *                     "F3" Retrieves the first branch record associated with bus1, bus2, and circuit ID. bus1_name, bus1_kV, bus2_name, bus2_kV, and ckt_id  must be valid.<br>
 *                     "N3" Retrieves the next branch record associated with bus1, bus2, and circuit ID. See notes below.<br>
 *                     "F2" Retrieves the first branch record associated with bus1 and bus2. bus1_name, bus1_kV, bus2_name, bus2_kV must be valid.<br>
 *                     "N2" Retrieves the next branch record associated with bus1 and bus2. See notes below.<br>
 *                     "F1" Retrieves the first branch record associated with bus1. bus1_name and bus1_kV must be valid.<br>
 *                     "N1" Retrieves the next branch record associated with bus1. See notes below.<br>
 *                     "F" Retrieves the first branch record disregarding bus association. All id fields may be null or zero.<br>
 *                     "N" Retrieves the next branch record disregarding bus association. See notes below.<br>
 *                     "G" Retrieves the rest of the branch record. All id fields must be valid to get a specific record.<br>
 *                     "D" Deletes a branch record. All id fields must be valid.<br>
 *                     "A" Adds a branch record. All fields appropriate for the branch type must be valid<br>
 *                     "M" Modifies a branch record. All fields appropriate for the branch type must be valid.<br>
 *                     "O" Retrieves the solution output data.<br>
 *                     Notes:<br>
 *                     1) with a "wildcard" circuit ID of " " or "*" and a section code of zero (or blank) -"G" and "F3" are the same as "F2".<br>
 *                     2) with a valid (non-"wildcard") circuit ID and a section code of zero (or blank) -"G" is the same as "F3".<br>
 *                     3) codes "F", "N", "N1", "N2", and "N3"  do not need any data specified in the pf_rec , however the first use of "N", "N1", "N2", and "N3" relies on initialization with a "F", "F1", "F2", "F3", or "G" code on a previous call.<br>
 *
 * To access individual fields of the records use the union member name, for 
 * example, using a declaration of pf_rec br; then br.i.branch.tap1 would 
 * reference the input tap1 field for a transformer. If you were looking at an 
 * E-type line instead, the variable br.i.branch.tap1 would contain the g2 
 * value, but to make it more obvious what you were actually doing, you would
 * probably want to use the union member name br.i.E.g2, instead. All of the 
 * structures and unions are declared in cflowlib.h.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1.*/
int pf_rec_branch(pf_rec *r, char *action);

/** Retrieves, modifies, adds, or deletes bus records.
 *
 * pf_rec is a union of both input data (i) and solution data (s). To access
 * individual fields of the records use the union member name, for example, 
 * using a declaration of pf_rec b; then b.i.ACbus.Pload would contain the MW
 * load for an AC bus, and the smoothing reactance for a DC bus, but to make 
 * it more obvious what you were actually doing, you would probably want to 
 * use the union member name b.i.DCbus.smooth_rx_mh instead, when dealing with 
 * a DC bus. If you are retrieving all buses, you can use the ACbus designation
 * for the type, owner, name, kv, and zone fields; the contents of these fields
 * is the same regardless of the bus type.

 * @param[in|out] r      A pointer to a structure of type pf_rec supplied by the calling routine.
 * @param[in]     action A string designating the action to be performed on an 
 *                       intertie record. See below for the codes and their 
 *                       meanings. Either upper or lower case is acceptable
 *                         "F" Retrieves the first bus record.
 *                         "N" Retrieves the next bus record. (Name and kV must be valid.)
 *                         "G" Retrieves the rest of the bus record. (Name and kV must be valid.)
 *                         "D" Deletes a bus record. (Name and kV must be valid.)
 *                         "A" Adds a bus record. (All data fields must be valid. See the Record Formats section.)
 *                         "M" Modifies a bus record. (All data fields must be valid. See the Record Formats section.)
 *                         "O" Retrieves the solution output data. (The case must be solved and name and kV must be valid.)
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_rec_bus(pf_rec *r, char *action);

/** 

 *
 * @param[in|out] r      A pointer to a structure of type pf_rec, supplied by 
 *                       the calling routine.
 * @param[in]     action A string designating the action to be performed on a 
                         continuation bus record. See the table below for the codes and their meanings. Either upper or lower case is acceptable.
 * 
 * @return */
int pf_rec_cbus(pf_rec *r, char *action);

/** 
 *
 * @param
 * 
 * @return  */
int pf_rec_comments(pf_comments *r, char *action);

/** 
 *
 * @param
 * 
 * @return */
int pf_rec_itie(pf_rec *r, char *action);

/** 
 *
 * @param
 * 
 * @return  */
int pf_rec_qcurve(pf_rec *r, char *action);

/** 
 *
 * @param
 * 
 * @return */
int pf_rec_xdata(pf_rec *r, char *action);

/** Converts an ASCII record in WSCC format to a pf_rec.
 * 
 * @param[in] net_data A pointer to a source string of network or output data.
 * @param[in] r        A pointer to a structure of type pf_rec.
 * @param[in] action   A pointer to a string designating the action to be 
 *                     performed. "I" Converts network data string to binary 
 *                     input data. "O" Converts output data string to binary 
 *                     solution data.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns -1. */
int pf_rec_a2b(char *net_data, pf_rec *r, char *action);
/** @example pf_rec.c */

/** Converts a pf_rec to an ASCII record in WSCC format. 
 *
 * All data fields in pf_rec must be valid.
 * 
 * @author William D. Rogers
 * @date 1-23-1995
 *
 * @param[out] net_data A pointer to a destination string for network data.
 * @param[in]  r        A pointer to a structure of type pf_rec.
 * @param[in]  action   A pointer to a string designating the action to be 
 *                      performed on a bus record. See below <br>
 *                       "I" Writes network data record.<br>
 *                       "D" Writes change record to delete.<br>
 *                       "A" Writes change record to add.<br>
 *                       "M" Writes change record to modify.<br>
 *                       "O" Writes solution record.  (The case must be solved.)<br>
 * 
 * @return Returns 0 if it is successful; otherwise, it returns -1. */
int pf_rec_b2a(char *net_data, pf_rec *r, char *action);

/** @} */

/** 
 *
 * @param
 * 
 * @return  */
int pf_save_changes(char *filename);

/** 
 *
 * @param
 * 
 * @return */
int pf_save_netdata(char *filename, char *dialect, char *ratings, int size);

/** 
 *
 * @param
 * 
 * @return */
int pf_save_newbase(char *filename);

/** 
 *
 * @param
 * 
 * @return */
int pf_save_wscc_stab_data(char *filename, char *type);

/** 
 *
 * @param
 * 
 * @return */
int pf_solution();

/** 
 *
 * @param
 * 
 * @return */
int pf_init();

/** Retrieve various lists: owners, areas, etc.
 * 
 * char *list should be a two dimensional character array. For example, for 
 * AREA_LIST, the array might be declared as char area_names[20][11] if you 
 * know that there are not more than 20 area names in the case. pf_case_info
 * gives information on the number of areas, zones, owners, etc. The listlen 
 * argument prevents the routine from exceeding the bounds of your array.
 *
 * @param[in] list    A pointer to a two dimensional character array of appropriate size.
 * @param[in] listlen An integer specifying the maximum number of list elements.
 * @param[in] type    The type of list.
 *
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_get_list( char *list, int listlen, enum pf_list_type type, char *data );
/** @example pf_get_list.c */

/** Finds the name of the area that a zone is in.
 *
 * @param[out] area      A pointer to an array of 11 characters in which the area name is returned.
 * @param[in]  zone_name A string holding a zone name.
 *
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_area_of_zone(char *area, char *zone);
/** @example pf_area_of_zone.c */

/** Retrieve case info
 * 
 * Retrieves data from a Powerflow base case and puts it in the info structure.
 *
 * @param[out] pf_case_stats A pointer to a structure of type pf_case_stats.
 *
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_case_info(pf_case_stats *r);
/** @example pf_case_info.c */

/** Initialize pf_rec data  
 *
 * Initializes a data buffer of type pf_rec to blanks and zeros, in order to 
 * clear out old data before calling one of the pf_rec routines to store new 
 * data in it. Its use is not necessary, but is recommended.
 *
 * @param r     A pointer to a structure of type pf_rec, supplied by the calling routine.
 * @param rtype An enumerated variable defined in ft.h */
void pf_init_rec(void *r, int rtype);

/** Initialize all P-Q curve data fields
 *
 * Initializes all P-Q curve data fields to 0 except ID fields that are 
 * initialized to the values passed as parameters. This function is used to
 * store ID fields for a specific bus before calling pf_rec_qcurve() to 
 * retrieve the generator capability curve values for that bus.
 *
 * @param[out] r   A pointer to a structure of type pf_rec supplied by the calling routine.
 * @param[in] type A string that specifies the record type (must be "QP").
 * @param[in] name A string that contains the bus name.
 * @param[in] kv   A floating point value representing the base kV. */
void pf_init_qcurve(pf_rec *r, char *type, char *name, float kv);

/** Initialize all intertie data fields
 *
 * Initializes all intertie data fields to 0 except ID fields that are 
 * initialized to the values passed as parameters.  This function is used to 
 * store ID fields for a specific tie line before calling pf_rec_itie() to 
 * retrieve the values for that line.
 *
 * @param r     A pointer to a structure of type pf_rec, supplied by the calling routine.
 * @param type  A string that specifies the record type (must be "I").
 * @param area1 A string which contains the area 1 name.
 * @param area2 A string which contains the area 2 name. */
void pf_init_itie(pf_rec *r, char *type, char *area1, char *area2);

/** Initialize all continuation bus fields
 *
 * Initializes all continuation bus data fields to 0 except ID fields which are
 * initialized to the values passed as parameters. This function is used to 
 * store ID fields for a specific bus before calling pf_rec_cbus() to retrieve
 * the continuation record values for that bus.
 *
 * @param[out] r     A pointer to a structure of type pf_rec, supplied by the calling routine.
 * @param[in] type   A string that specifies the record type (must be "+").
 * @param[in] owner  A string that contains the owner name.
 * @param[in] name   A string that contains the bus name.
 * @param[in] kv     A floating point value representing the base kV.
 * @param[in] year   A two character string that contains the code year. */
void pf_init_cbus(pf_rec *r, char *type, char *owner, char *name, float kv, char *year);

/** Initialize all bus data fields
 *
 * Initializes all bus data fields to 0 except ID fields which are initialized 
 * to the values passed as parameters. This function is used to store ID fields
 * for a specific bus before calling pf_rec_bus to retrieve the values for that
 * bus.
 *
 * @param[out] r   A pointer to a structure of type pf_rec, supplied by the calling routine.
 * @param[in] type A string that specifies the record type (either B or any bus type is legal).
 * @param[in] name A string that contains the bus name.
 * @param[in] kv   A floating point value representing the base kV. */
void pf_init_bus(pf_rec *r, char *type, char *name, float kv);

/** Initialize all branch data fields
 *
 * Initializes all branch data fields to 0 except ID fields which are 
 * initialized to the values passed as parameters. This function is used to 
 * store ID fields for a specific branch before calling pf_rec_branch to 
 * retrieve the values for that branch.
 *
 * @param[out] r      A pointer to a structure of type pf_rec, supplied by the calling routine.
 * @param[in]  type   A string which specifies the record type (L, T, E, or specific R-type).
 * @param[in]  name1  A string that contains the bus 1 name.
 * @param[in]  kv1    A floating point value representing the base kV for bus 1.
 * @param[in]  name2  A string that contains the bus 2 name.
 * @param[in]  kv2    A floating point value representing the base kV for bus 2.
 * @param[in]  cid    A string that contains the circuit ID. For solution data, '*' will retrieve the sum of all parallel circuits.
 * @param[in]  sid    A integer value representing the section ID. For solution data, a value of 0 will retrieve the total equivalent line. */
void pf_init_branch(pf_rec *r, char *type, char *name1, float kv1, char *name2, float kv2, char cid, int sid);

/** Initialize all area data fields 
 *
 * Initializes all area data fields to 0 except ID fields which are initialized
 * to the values passed as parameters. This function is used to store ID fields
 * for a specific area before calling pf_rec_area to retrieve the values for 
 * that area.
 *
 * @param[in] r    A pointer to a structure of type pf_rec, supplied by the calling routine.
 * @param[in] type A string that specifies the record type (must be A).
 * @param[in] name A string that contains the area name. */
void pf_init_area(pf_rec *r, char *type, char *name);

/** @example pf_init_functions.c */

/** Seee if a bus exists
 * 
 * @author William D. Rogers
 * @date 8-17-1994
 *
 * @param[in] name A pointer to a string containing the name.
 * @param[in] kv   A real value representing the bus base kV.
 * 
 * @return Returns 0 if the bus exists; otherwise, it returns 1. */
int pf_bus_exists(char *name, float kv);

 /** @example pf_bus_exists.c */

/** 
 * 
 * @author William D. Rogers
 * @date 8-17-1994
 *
 * @param
 * 
 * @return */
int pf_b2a_bus(char *net_data, pf_rec *r, char *action);

/** 
 * 
 * @author William D. Rogers
 * @date 8-17-1994
 *
 * @param
 * 
 * @return */
int pf_b2a_branch(char *net_data, pf_rec *r, char *action);

/** 
 * 
 * @author William D. Rogers
 * @date 1-6-1995
 *
 * @param
 * 
 * @return */
int pf_user_init_def();

/** 
 * 
 * @author William D. Rogers
 * @date 1-6-1995
 *
 * @param
 * 
 * @return */
int pf_user_load_def(char *definitions);

/** 
 * 
 * @author William D. Rogers
 * @date 1-6-1995
 *
 * @param
 * 
 * @return */
int pf_user_sub_def(char *base);

/** 
 * 
 * @author William D. Rogers
 * @date 1-6-1995
 *
 * @param
 * 
 * @return */
int pf_user_report(char *filename, char *output, char action);

/** 
 * 
 * @author William D. Rogers
 * @date 1-30-1995
 *
 * @param
 * 
 * @return */
int pf_user_define(char *symbol, char *id, char *type);

/** 
 * 
 * @author William D. Rogers
 * @date 1-30-1995
 *
 * @param
 * 
 * @return */
int pf_user_comment(char *symbol, char *suffix, char *format);

/** 
 * 
 * @author William D. Rogers
 * @date 1-30-1995
 *
 * @param
 * 
 * @return */
int pf_user_quantity(char *symbol, char *suffix, float *quantity);

/** 
 * 
 * @author William D. Rogers
 * @date 1-31-1995
 *
 * @param
 * 
 * @return  */
int pf_user_string(char *symbol, int length, char *info);

/** 
 * 
 * @author William D. Rogers
 * @date 1-31-1995
 *
 * @param
 * 
 * @return */
int pf_user_branch(char *symbol, pf_rec *r, char *type);

/** 
 * 
 * @author William D. Rogers
 * @date 1-31-1995
 *
 * @param
 * 
 * @return 
 *
 * @example */
int pf_user_itie(char *symbol, pf_rec *r, char *type);

/** 
 * 
 * @author William D. Rogers
 * @date 1-31-1995
 *
 * @param
 * 
 * @return */
int pf_user_bus(char *symbol, pf_rec *r, char *suffix);

/** Create a plot
 * 
 * Causes powerflow (IPF) to generate a plot. Difference plots may be made by
 * first loading an reference (alternate) base case with pf_load_refbase() and
 * providing a difference plot coordinate file. pf_plot sends a command 
 * constructed as follows:
 * 
 @code
 /plot
 <cor_filename> 
 <ps_filename>
 <options>
 @endcode
 *
 * @author William D. Rogers
 * @date 7-6-1995
 *
 * @param[in] cor_filename A string representing the name of a coordinate file.
 * @param[in] ps_filename  A string representing the name of the postscript 
  *                        file to be created.
 * @param[in] options      An optional string (may be NULL), representing a list
 *                         of comments and options, separated by newline ("\n"). 
 *                         Each option must begin with an "@"" character.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_plot(char *cor_filename, char *ps_filename, char *options);

/** @example pf_plot.c */

/** Load a reference base case
 *
 * Passes a base case filename to the ipfsrv process so that it can read and 
 * interpret the file as a "reference base" (also referred to as an "alternate
 * base"). This is done prior to requesting difference plots or comparison 
 * (difference) reports. If a reference case is currently loaded, it is 
 * overwritten and the data is lost.
 * 
 * @author William D. Rogers
 * @date 7-7-1995
 *
 * @param[in] filename A string representing a file name.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_load_refbase(char *filename);

/** 
 * 
 * @author William D. Rogers
 * @date 7-21-1995
 *
 * @param
 * 
 * @return */
int pf_select_base(char base);

/** 
 * 
 * @author William D. Rogers
 * @date 3-6-1996
 *
 * @param
 * 
 * @return */
int pf_solve_area(char base);

/** Run PCL commands
 *
 * Passes an ASCII control file name to ipfsrv so that it can read and 
 * interpret the file.
 * 
 * @author William D. Rogers
 * @date 5-6-1996
 *
 * @param[in] command A string representing PCL commands [and data].
 * 
 * @return Returns 0 if it is successful; otherwise, it returns -1. */
int pf_command(char *command);
