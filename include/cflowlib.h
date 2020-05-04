
/** This is the byte size of the in and out buffers used by CLFOW. */
#define CFLOW_IPC_BUFF_SIZE 4096

#ifdef CFLOW_GLOBAL_DATA
   char  pf_cflow_inbuf[CFLOW_IPC_BUFF_SIZE];  /**< A buffer of the data most recently received from the ipfsrv "engine". Used for cflow to powerflow inter-process communication. */
   char  pf_cflow_outbuf[CFLOW_IPC_BUFF_SIZE]; /**< A buffer of the data most recently sent to the ipfsrv "engine". Used for cflow to powerflow inter-process communication. */
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


/** This is an enumeration of options that you can use in calls to pf_get_list(). */
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
   char   type[3];           /**< Two character branch record type. */
   char   owner[4];          /**< Three character branch owner. */
   char   bus1_name[9];      /**< Bus 1 name. */
   float  bus1_kv;           /**< Base kV for bus 1. */
   int    meter;             /**< The line metering point for area tie lines.<br>
                                   1 = bus 1 end.<br>
                                   2 = bus 2 end. <br>
                                  If blank, metering point will be identified (1) by 
                                  location where line ownership differs from bus ownership
                                  or (2) when buses at end of tie line have same ownership,
                                  then the bus name 1 will be the metering point. */
   char   bus2_name[9];      /**< Bus 2 name. */
   float  bus2_kv;           /**< Base kV for bus 2. */
   char   ckt_id;            /**< Circuit identification if more than one parallel branch exists. */
   int    section;           /**< Section number for making an equivalent for series elements 
                                 (numeric). Program assembles series elements in numerical order of 
                                 section numbers (need not be consecutive). */
   float  total_rating;      /**< Total ampere rating for all lines. */
   int    num_ckts;          /**< Number of parallel circuits represented by this record, for 
                                  information purposes only. The equivalent impedance is entered 
                                  in r, x, g, b for lines with equal legs. */
   float  r;                 /**< Per unit resistance at base kV and base MVA. */
   float  x;                 /**< Per unit reactance at base kV and base MVA. */
   float  g;                 /**< Per unit conductance G_pi/2 at base kV and MVA. This format is for balanced
                                  lines when Y_pi sending equals Y_pi receiving and only Y_pi needs
                                  to be input. */
   float  b;                 /**< Per unit susceptance B_pi/2 at base kV and MVA. */
   float  tap1;              /**< Tap at bus1 of transformer, in kV for normal tap, in degrees for phase shifter. */
   float  tap2;              /**< Tap at bus2 of transformer, in kV. */
   float  alpha_N_deg;       /**< Initial firing angle in degrees at rectiﬁer (DC lines). */
   float  gamma_0_deg;       /**< Minimum margin angle in degrees at inverter (DC lines). */
   char   descrip[9];        /**< Unknown. */
   char   date_in[4];        /**< Energization Date in format "MYY". Where M = {1,2,3,4,5,6,7,8,9,O,N,D} and YY = last two digits of year. */
   char   date_out[4];       /**< De-nergization Date in format "MYY". Where M = {1,2,3,4,5,6,7,8,9,O,N,D} and YY = last two digits of year. */
   float  thermal_rating;    /**< Thermal rating in Amps. */
   float  bottleneck_rating; /**< Bottleneck rating in Amps. */
   float  emergency_rating;  /**< Emergency rating in MVA. */
} pf_branch;

/** This structure holds power flow input data for a two terminal DC line record. */
typedef struct {
   char   type[3];           /**< Two character record type "LD". */
   char   owner[4];          /**< Three character owner. */
   char   bus1_name[9];      /**< Converter bus 1 name. */
   float  bus1_kv;           /**< Base kV for bus 1. */
   int    meter;             /**< The line metering point for area tie lines.<br>
                                   1 = bus 1 end.<br>
                                   2 = bus 2 end. <br>
                                  If blank, metering point will be identified (1) by 
                                  location where line ownership differs from bus ownership
                                  or (2) when buses at end of tie line have same ownership,
                                  then the bus name 1 will be the metering point. */
   char   bus2_name[9];      /**< Converter bus 2 name. */
   float  bus2_kv;           /**< Base kV for bus 2. */
   char   I_or_R_control;    /**< Inverter or rectiﬁer control - Enter 'R' for rectiﬁier
                                  control or 'I' for inverter control (point of DC line in
                                  which scheduled power is measured). */
   int    dummy1;            /**< Ignore. This is a dummy field used for alignment purposes. */
   float  total_rating;      /**< Total ampere rating for all lines. */
   int    dummy2;            /**< Ignore. This is a dummy field used for alignment purposes. */
   float  R;                 /**< DC Line resistance, ohms. */
   float  L_mh;              /**< DC line inductance, millihenries. */
   float  C_uf;              /**< DC line capacitance, microfarads. */
   float  P_sched;           /**< Schedule DC power (MW) - Scheduled DC power in megawatts
                                  from converter 1 to 2 metered at the end indicated by I 
                                  or R in I_or_R_control. */
   float  V_sched;           /**< Schedule DC line volts (kV) - at rectiﬁer end of DC line. */
   float  miles;             /**< How many miles long the line is. */
   float  alpha_N_deg;       /**< Initial firing angle in degrees at rectiﬁer. */
   float  gamma_0_deg;       /**< Minimum margin angle in degrees at inverter. */
   char   dummy3[9];         /**< Ignore. This is a dummy field used for alignment purposes. */
   char   dummy4[4];         /**< Ignore. This is a dummy field used for alignment purposes. */
   char   dummy5[4];         /**< Ignore. This is a dummy field used for alignment purposes. */
   float  thermal_rating;    /**< Thermal rating in Amps. */
   float  bottleneck_rating; /**< Bottleneck rating in Amps. */
   float  dummy6;            /**< Ignore. This is a dummy field used for alignment purposes. */
} pf_LD;

/** This structure holds power flow input data for a multiterminal DC line record. */
typedef struct {
   char   type[3];           /**< Two character record type "LM". */
   char   owner[4];          /**< Three character owner. */
   char   bus1_name[9];      /**< Converter bus 1 name. */
   float  bus1_kv;           /**< Base kV for bus 1. */
   int    meter;             /**< The line metering point for area tie lines.<br>
                                   1 = bus 1 end.<br>
                                   2 = bus 2 end. <br>
                                  If blank, metering point will be identified (1) by 
                                  location where line ownership differs from bus ownership
                                  or (2) when buses at end of tie line have same ownership,
                                  then the bus name 1 will be the metering point. */
   char   bus2_name[9];      /**< Converter bus 2 name. */
   float  bus2_kv;           /**< Base kV for bus 2. */
   char   dummy1;            /**< Ignore. This is a dummy field used for alignment purposes. */
   int    dummy2;            /**< Ignore. This is a dummy field used for alignment purposes. */
   float  total_rating;      /**< Total Current (I) Rating in Amps - Maximum DC line current in ampere. */
   int    dummy3;            /**< Ignore. This is a dummy field used for alignment purposes. */
   float  R;                 /**< DC Line resistance, ohms. */
   float  L_mh;              /**< DC line inductance, millihenries. */
   float  C_uf;              /**< DC line capacitance, microfarads. */
   float  dummy4;            /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy5;            /**< Ignore. This is a dummy field used for alignment purposes. */
   float  miles;             /**< Length of the line in miles - information only. */
   float  dummy6;            /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy7;            /**< Ignore. This is a dummy field used for alignment purposes. */
   char   dummy8[9];         /**< Ignore. This is a dummy field used for alignment purposes. */
   char   date_in[4];        /**< Energization Date in format "MYY". Where M = {1,2,3,4,5,6,7,8,9,O,N,D} and YY = last two digits of year. */
   char   date_out[4];       /**< De-nergization Date in format "MYY". Where M = {1,2,3,4,5,6,7,8,9,O,N,D} and YY = last two digits of year. */
   float  thermal_rating;    /**< Thermal rating in Amps. */
   float  bottleneck_rating; /**< Bottleneck rating in Amps. */
   float  dummy9;            /**< Ignore. This is a dummy field used for alignment purposes. */
} pf_LM;

/** This structure holds power flow input data for a equivalent transmission 
line record (asymmetric pi representation). */
typedef struct {
   char   type[3];           /**< Two character record type "E". */
   char   owner[4];          /**< Three character owner. */
   char   bus1_name[9];      /**< Bus 1 name. */
   float  bus1_kv;           /**< Base kV for bus 1. */
   int    meter;             /**< The line metering point for area tie lines.<br>
                                   1 = bus 1 end.<br>
                                   2 = bus 2 end. <br>
                                  If blank, metering point will be identified (1) by 
                                  location where line ownership differs from bus ownership
                                  or (2) when buses at end of tie line have same ownership,
                                  then the bus name 1 will be the metering point. */
   char   bus2_name[9];      /**< Bus 2 name. */
   float  bus2_kv;           /**< Base kV for bus 2. */
   char   ckt_id;            /**< Circuit identification if more than one parallel line exists. */
   int    section;           /**< Section number for making an equivalent for series elements 
                                 (numeric). Program assembles series elements in numerical order of 
                                 section numbers (need not be consecutive). */
   float  total_rating;      /**< Total ampere rating for all lines. */
   int    num_ckts;          /**< Number of parallel circuits represented by this record, for 
                                  information purposes only. The impedance is entered in r, x, 
                                  g1, b1, g2, b2. */
   float  r;                 /**< Per unit resistance at base kV and base MVA. */
   float  x;                 /**< Per unit reactance at base kV and base MVA. */
   float  g1;                /**< Per unit conductance at base kV and MVA for bus 1 end of line. */
   float  b1;                /**< Per unit susceptance at base kV and MVA for bus 1 end of line. */
   float  g2;                /**< Per unit conductance at base kV and MVA for bus 2 end of line. */
   float  b2;                /**< Per unit susceptance at base kV and MVA for bus 2 end of line.. */
   float  dummy1;            /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy2;            /**< Ignore. This is a dummy field used for alignment purposes. */
   char   dummy3[9];         /**< Ignore. This is a dummy field used for alignment purposes. */
   char   date_in[4];        /**< Energization Date in format "MYY". Where M = {1,2,3,4,5,6,7,8,9,O,N,D} and YY = last two digits of year. */
   char   date_out[4];       /**< De-nergization Date in format "MYY". Where M = {1,2,3,4,5,6,7,8,9,O,N,D} and YY = last two digits of year. */
   float  thermal_rating;    /**< Thermal rating in Amps. */
   float  bottleneck_rating; /**< Bottleneck rating in Amps. */
   float  dummy4;            /**< Ignore. This is a dummy field used for alignment purposes. */
} pf_E;

/** This structure holds power flow input data for an AC line record 
(symmetric pi representation). */
typedef struct {
   char   type[3];           /**< Two character record type "L". */
   char   owner[4];          /**< Three character owner. */
   char   bus1_name[9];      /**< Bus 1 name. */
   float  bus1_kv;           /**< Base kV for bus 1. */
   int    meter;             /**< The line metering point for area tie lines.<br>
                                   1 = bus 1 end.<br>
                                   2 = bus 2 end. <br>
                                  If blank, metering point will be identified (1) by 
                                  location where line ownership differs from bus ownership
                                  or (2) when buses at end of tie line have same ownership,
                                  then the bus name 1 will be the metering point. */
   char   bus2_name[9];      /**< Bus 2 name. */
   float  bus2_kv;           /**< Base kV for bus 2. */
   char   ckt_id;            /**< Circuit identification if more than one parallel branch exists. */
   int    section;           /**< Section number for making an equivalent for series elements 
                                 (numeric). Program assembles series elements in numerical order of 
                                 section numbers (need not be consecutive). */
   float  total_rating;      /**< Total ampere rating for all lines. */
   int    num_ckts;          /**< Number of parallel circuits represented by this record, for 
                                  information purposes only. */
   float  r;                 /**< Per unit resistance at base kV and base MVA. */
   float  x;                 /**< Per unit reactance at base kV and base MVA. */
   float  g;                 /**< Per unit conductance at base kV and MVA. */
   float  b;                 /**< Per unit susceptance at base kV and MVA. */
   float  miles;             /**< Length of the line in miles - information only. */
   float  dummy1;            /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy2;            /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy3;            /**< Ignore. This is a dummy field used for alignment purposes. */
   char   descrip[9];        /**< Information only. */
   char   date_in[4];        /**< Energization Date in format "MYY". Where M = {1,2,3,4,5,6,7,8,9,O,N,D} and YY = last two digits of year. */
   char   date_out[4];       /**< De-nergization Date in format "MYY". Where M = {1,2,3,4,5,6,7,8,9,O,N,D} and YY = last two digits of year. */
   float  thermal_rating;    /**< Thermal rating in Amps. */
   float  bottleneck_rating; /**< Bottleneck rating in Amps. */
   float  dummy4;            /**< Ignore. This is a dummy field used for alignment purposes. */
} pf_L;

/** This structure holds power flow input data for transformer record. */
typedef struct {
   char   type[3];           /**< Two character record type "T". */
   char   owner[4];          /**< Three character owner. */
   char   bus1_name[9];      /**< Bus 1 name. */
   float  bus1_kv;           /**< Base kV for bus 1. */
   int    meter;             /**< Metering point.<br>
                                   1 = bus 1 end.<br>
                                   2 = bus 2 end. <br>
                                  If blank, metering point will be identified (1) by 
                                  location where line ownership differs from bus ownership
                                  or (2) when buses at end of tie line have same ownership,
                                  then the bus name 1 will be the metering point. */
   char   bus2_name[9];      /**< Bus 2 name. */
   float  bus2_kv;           /**< Base kV for bus 2. */
   char   ckt_id;            /**< Circuit identification if more than one parallel transformer exists. */
   int    section;           /**< Section number for making an equivalent for series elements 
                                 (numeric). Program assembles series elements in numerical order of 
                                 section numbers (need not be consecutive). */
   float  total_rating;      /**< Total ampere rating. */
   int    num_ckts;          /**< Number of parallel circuits represented by this record, for 
                                  information purposes only. */
   float  r;                 /**< Per unit resistance at base kV and base MVA. */
   float  x;                 /**< Per unit reactance at base kV and base MVA. */
   float  g;                 /**< Per unit conductance at base kV and MVA. */
   float  b;                 /**< Per unit susceptance at base kV and MVA. */
   float  tap1;              /**< Fixed bus 1 TAP which describe bus 1 relative to bus 2. */
   float  tap2;              /**< Fixed bus 2 TAP. */
   float  dummy1;            /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy2;            /**< Ignore. This is a dummy field used for alignment purposes. */
   char   dummy3[9];         /**< Ignore. This is a dummy field used for alignment purposes. */
   char   date_in[4];        /**< Energization Date in format "MYY". Where M = {1,2,3,4,5,6,7,8,9,O,N,D} and YY = last two digits of year. */
   char   date_out[4];       /**< De-nergization Date in format "MYY". Where M = {1,2,3,4,5,6,7,8,9,O,N,D} and YY = last two digits of year. */
   float  thermal_rating;    /**< Thermal rating in Amps. */
   float  bottleneck_rating; /**< Bottleneck rating in Amps. */
   float  emergency_rating;  /**< Emergency rating in MVA. */
} pf_T;

/** This structure holds power flow input data for phase shifting transformer record. */
typedef struct {
   char   type[3];           /**< Two character record type "TP". */
   char   owner[4];          /**< Three character owner. */
   char   bus1_name[9];      /**< Bus 1 name. */
   float  bus1_kv;           /**< Base kV for bus 1. */
   int    meter;             /**< The line metering point for area tie lines.<br>
                                   1 = bus 1 end.<br>
                                   2 = bus 2 end. <br>
                                  If blank, metering point will be identified (1) by 
                                  location where line ownership differs from bus ownership
                                  or (2) when buses at end of tie line have same ownership,
                                  then the bus name 1 will be the metering point. */
   char   bus2_name[9];      /**< Bus 2 name. */
   float  bus2_kv;           /**< Base kV for bus 2. */
   char   ckt_id;            /**< Circuit identification if more than one parallel transformer exists. */
   int    section;           /**< Section number for making an equivalent for series elements 
                                 (numeric). Program assembles series elements in numerical order of 
                                 section numbers (need not be consecutive). */
   float  total_rating;      /**< Total ampere rating. */
   int    num_ckts;          /**< Number of parallel circuits represented by this record, for 
                                  information purposes only. */
   float  r;                 /**< Per unit resistance at base kV and base MVA. */
   float  x;                 /**< Per unit reactance at base kV and base MVA. */
   float  g;                 /**< Per unit conductance at base kV and MVA. */
   float  b;                 /**< Per unit susceptance at base kV and MVA. */
   float  phase_shift_deg;   /**< Fixed bus 1 TAP or ﬁxed phase shift in degrees which describe bus 1 relative to bus 2. */
   float  tap2;              /**< Fixed bus 2 TAP or blank for ﬁxed phase shifter. */
   float  dummy1;            /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy2;            /**< Ignore. This is a dummy field used for alignment purposes. */
   char   dummy3[9];         /**< Ignore. This is a dummy field used for alignment purposes. */
   char   date_in[4];        /**< Energization Date in format "MYY". Where M = {1,2,3,4,5,6,7,8,9,O,N,D} and YY = last two digits of year. */
   char   date_out[4];       /**< De-nergization Date in format "MYY". Where M = {1,2,3,4,5,6,7,8,9,O,N,D} and YY = last two digits of year. */
   float  thermal_rating;    /**< Thermal rating in Amps. */
   float  bottleneck_rating; /**< Bottleneck rating in Amps. */
   float  emergency_rating;  /**< Emergency rating in MVA. */
} pf_TP;

/** This structure holds power flow input data for regulating transformer record.
Struct for R, RN, RQ, RV type records. */
typedef struct {
   char   type[3];         /**< Two character record type "R". */
   char   owner[4];        /**< Three character owner. */
   char   bus1_name[9];    /**< Bus 1 name. */
   float  bus1_kv;         /**< Base kV for bus 1. */
   int    var_tap_side;    /**< Variable tap side if T_max and T_min cannot orient T_x.<br>
                                  0 - Low alpha is ﬁxed<br>
                                  1 - Bus 1 is variable<br>
                                  2 - Bus 2 is variable */
   char   bus2_name[9];    /**< Bus 2 name. */
   float  bus2_kv;         /**< Base kV for bus 2. */
   char   dummy1;          /**< Ignore. This is a dummy field used for alignment purposes. */
   int    dummy2;          /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy3;          /**< Ignore. This is a dummy field used for alignment purposes. */
   int    num_taps;        /**< Total number of LTC taps. If blank, assumes continuous action. */
   float  dummy4;          /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy5;          /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy6;          /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy7;          /**< Ignore. This is a dummy field used for alignment purposes. */
   float  max_tap;         /**< Maximum kV tap. */
   float  min_tap;         /**< Minimum kV taps. */
   float  dummy8;          /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy9;          /**< Ignore. This is a dummy field used for alignment purposes. */
   char   rmt_bus_name[9]; /**< Controlled bus name. */
   char   date_in[4];      /**< Energization Date in format "MYY". Where M = {1,2,3,4,5,6,7,8,9,O,N,D} and YY = last two digits of year. */
   char   date_out[4];     /**< De-nergization Date in format "MYY". Where M = {1,2,3,4,5,6,7,8,9,O,N,D} and YY = last two digits of year. */
   float  rmt_bus_kv;      /**< Controlled bus kV. */
   float  Qmax;            /**< Maximum reactive (MVAR) flow through the transformer. */
   float  Qmin;            /**< Minimum reactive (MVAR) flow through the transformer. */
} pf_R;

/** This structure holds power flow input data for regulating transformer 
RM, RP record types. */
typedef struct {
   char   type[3];             /**< Two character record type "RM". */
   char   owner[4];            /**< Three character owner. */
   char   bus1_name[9];        /**< Bus 1 name. */
   float  bus1_kv;             /**< Base kV for bus 1. */
   int    var_tap_side;        /**< Variable tap side if T_max and T_min cannot orient T_x.<br>
                                      0 - Low alpha is ﬁxed<br>
                                      1 - Bus 1 is variable<br>
                                      2 - Bus 2 is variable */
   char   bus2_name[9];        /**< Bus 2 name. */
   float  bus2_kv;             /**< Base kV for bus 2. */
   char   dummy1;              /**< Ignore. This is a dummy field used for alignment purposes. */
   int    dummy2;              /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy3;              /**< Ignore. This is a dummy field used for alignment purposes. */
   int    num_taps;            /**< Total number of LTC taps. */
   float  dummy4;              /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy5;              /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy6;              /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy7;              /**< Ignore. This is a dummy field used for alignment purposes. */
   float  max_phase_shift_deg; /**< Maximum angle in degrees. */
   float  min_phase_shift_deg; /**< Minimum angle in degrees. */
   float  dummy8;              /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy9;              /**< Ignore. This is a dummy field used for alignment purposes. */
   char   rmt_bus_name[9];     /**< Controlled bus name. */
   char   date_in[4];          /**< Energization Date in format "MYY". Where M = {1,2,3,4,5,6,7,8,9,O,N,D} and YY = last two digits of year. */
   char   date_out[4];         /**< De-nergization Date in format "MYY". Where M = {1,2,3,4,5,6,7,8,9,O,N,D} and YY = last two digits of year. */
   float  rmt_bus_kv;          /**< Controlled bus kV. */
   float  Pmax;                /**< Maximum real power (MW) flow through the transformer. */
   float  Pmin;                /**< Maximum real power (MW) flow through the transformer. */
} pf_RM;

/** This structure holds power flow input data for Rapid Adjustment of Network 
Impedance (RANI) device. It represents a series connected thyristor which 
changes its series impedance to control power or voltage. */
typedef struct {
   char   type[3];      /**< Two character record type "RZ". */
   char   owner[4];     /**< Three character owner. */
   char   bus1_name[9]; /**< Bus 1 name. */
   float  bus1_kv;      /**< Base kV for bus 1. */
   int    var_tap_side; /**< Variable tap side if T_max and T_min cannot orient T_x.<br>
                                  0 - Low alpha is ﬁxed<br>
                                  1 - Bus 1 is variable<br>
                                  2 - Bus 2 is variable */
   char   bus2_name[9]; /**< Bus 2 name. */
   float  bus2_kv;      /**< Base kV for bus 2. */
   char   ckt_id;       /**< Circuit identification if more than one parallel transformer exists. */
   int    section;      /**< Section number for making an equivalent for series elements 
                             (numeric). Program assembles series elements in numerical order of 
                             section numbers (need not be consecutive). */
   float  I_rate;       /**< Rated current. */
   int    rani_type;    /**< TYPE 1, 2, or 3<br>
                               TYPE 1 - Control Pc using Xij
                               TYPE 2 - Control V using Xij
                               TYPE 3 - Control V using Bis */
   float  Pc_max;       /**< Pc_max in MW */
   float  Pc_min;       /**< Pc_min in MW */
   float  Xij_max;      /**< Per unit maximum reactance. */
   float  Xij_min;      /**< Per unit minimum reactance. */
   float  Bis_max;      /**< Per unit minimum reactance. */
   float  Bis_min;      /**< Per unit minimum reactance. */
   float  dummy1;       /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy2;       /**< Ignore. This is a dummy field used for alignment purposes. */
   char   dummy3[9];    /**< Ignore. This is a dummy field used for alignment purposes. */
   char   dummy4[4];    /**< Ignore. This is a dummy field used for alignment purposes. */
   char   dummy5[4];    /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy6;       /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy7;       /**< Ignore. This is a dummy field used for alignment purposes. */
   float  dummy8;       /**< Ignore. This is a dummy field used for alignment purposes. */
} pf_RZ;

/** This structure holds power flow input data for control areas which can be
made up of multiple zones. */
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

/** This structure holds power flow input data for intertie records. It 
indicates the two areas between which interchange must be scheduled. */
typedef struct {
   char   type[3];        /**< Two character record type, here "I" for intertie record. */
   char   area1_name[11]; /**< Ten character area1 name. */
   char   area2_name[11]; /**< Ten character area2 name. */
   float  sched_export;   /**< Scheduled export power. */
} pf_itie;

/** This structure holds power flow input data for continuation bus (+) records. */
typedef struct {
   char   type[3];      /**< Two character bus record type, for example, "+"", "+A", etc.*/
   char   owner[4];     /**< Three character bus owner. */
   char   name[9];      /**< Eight character bus name. */
   float  kv;           /**< Base kV of the bus. */
   char   code_year[3]; /**< Two character extension of type. */
   float  Pload;        /**< Real load in MW belonging to this owner. */
   float  Qload;        /**< Reactive load in MVAR belonging to this owner. */
   float  Gshunt;       /**< Fixed real shunt in MW. */
   float  Bshunt;       /**< Fixed reactive shunt in MVAR. */
   float  Pgen;         /**< Scheduled real power in MW for this owner. */
   float  Qgen_Qmax;    /**< Scheduled reactive power in MVAR (Qgen) or  maximum reactive power in MVAR (Qmax). */
   float  Qmin;         /**< Minimum reactive power in MVAR.*/
} pf_cbus;

/** This structure holds power flow input data for reactive power capability. */
typedef struct {
   char   type[3];     /**< Two character record type - here "QP", "QM", or "QN". */
   char   PU_code[3];  /**< Two character code – PU for per unit or blank for kV values. */
   char   active;      /**< One character code – "A" for active or "*"" for inactive. */
   char   bus_name[9]; /**< Eight character bus name. */
   float  bus_kv;      /**< Base kV of the bus. */
   float  Pgen0;       /**< Real power levels in MW, for the reactive capability curve. Value is Qmin0 for QN records, Qmax0 for QM cards, and Pgen0 for QP cards, but is always named Pgen0, etc. */
   float  Pgen1;       /**< Real power levels in MW, for the reactive capability curve. */
   float  Pgen2;       /**< Real power levels in MW, for the reactive capability curve. */
   float  Pgen3;       /**< Real power levels in MW, for the reactive capability curve. */
   float  Pgen4;       /**< Real power levels in MW, for the reactive capability curve. */
   float  Pgen5;       /**< Real power levels in MW, for the reactive capability curve. */
   float  Pgen6;       /**< Real power levels in MW, for the reactive capability curve. */
   float  Pgen7;       /**< Real power levels in MW, for the reactive capability curve. */
   float  Pgen8;       /**< Real power levels in MW, for the reactive capability curve. */
   float  Pgen9;       /**< Real power levels in MW, for the reactive capability curve. */
} pf_qcurve;

/** This structure holds power flow input data for switched reactance devices
(capacitor and reactor banks). */
typedef struct {
   char   type[3];        /**< Two character array designating the record type — here "X" for switched reactance record. */
   char   owner[4];       /**< Three character array designating an owner. */
   char   bus_name[9];    /**< Eght character bus name. */
   float  bus_kv;         /**< Base kV of the BX bus.*/
   char   rmt_name[9];    /**< Eight character remote bus name. */
   float  rmt_kv;         /**< Remote bus base kV.*/
   int    seg1_num_steps; /**< Number of each reactance value available (integer). */
   float  seg1_delta_mva; /**< Magnitude of reactance values available. */
   int    seg2_num_steps; /**< Number of each reactance value available (integer). */
   float  seg2_delta_mva; /**< Magnitude of reactance values available. */
   int    seg3_num_steps; /**< Number of each reactance value available (integer). */
   float  seg3_delta_mva; /**< Magnitude of reactance values available. */
   int    seg4_num_steps; /**< Number of each reactance value available (integer). */
   float  seg4_delta_mva; /**< Magnitude of reactance values available. */
   int    seg5_num_steps; /**< Number of each reactance value available (integer). */
   float  seg5_delta_mva; /**< Magnitude of reactance values available. */
   int    seg6_num_steps; /**< Number of each reactance value available (integer). */
   float  seg6_delta_mva; /**< Magnitude of reactance values available. */
   int    seg7_num_steps; /**< Number of each reactance value available (integer). */
   float  seg7_delta_mva; /**< Magnitude of reactance values available. */
   int    seg8_num_steps; /**< Number of each reactance value available (integer). */
   float  seg8_delta_mva; /**< Magnitude of reactance values available. */
} pf_xdata;
/** @} */

/** This struct stores power flow solution values for an AC bus. */
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

/** This struct stores power flow solution values for a DC bus. */
typedef struct {
   char   type[3];        /**< Two character bus record type. */
   float  P_DC;           /**< AC real power into the DC bus, positive at the rectifier and negative at the inverter. */
   float  Q_DC;           /**< AC reactive power into the DC bus, returned as a positive number. */
   float  V_DC;           /**< DC terminal voltage (final voltage at commutating bus,  in kV).*/
   float  converter_deg;  /**< Converter angle, alpha for rectifier, gamma for inverter, in degrees*/
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

/** This structure holds power flow solution data for branch records. */                       
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
   int    crit_line_load_term; /**< Largest flow in a transformer terminal (MVA). */
   float  crit_xfmr_load_mva;  /**< Largest flow in a transformer (MVA). */
   float  crit_xfmr_rat_mva;   /**< Actual value of rating used (MVA). */
   char   crit_xfmr_rat_code;  /**< Type of rating used – N, B, T, or E. */
   int    crit_xfmr_load_term; /**< Largest flow in a transformer terminal (MVA). */
   float  tot_line_load_pct;   /**< Percent line compensation (total negative reactance divided by total positive reactance). */
   float  tot_line_load_amps;  /**< Line compensation in amps. */
   float  tot_xfmr_load_pct;   /**< Percent loading on the transformer, using the indicated rating*/
   float  tot_xfmr_load_mva;   /**< Transformer load in apparent power (MVA). */
   float  tap1;                /**< Final tap at bus1 of transformer, in kV for normal tap, in degrees for phase shifter. */
   float  tap2;                /**< Final tap at bus2 of transformer, in kV. */
} pf_branch_soln;

/** This structure holds power flow solution (output) data for control areas which can be
made up of multiple zones. */
typedef struct {
   char   type[3]; /**< Two character record type. */
   float  Pgen;    /**< Total area generation. */
   float  Pload;   /**< Total area load. */
   float  Ploss;   /**< Total area losses. */
   float  Pexport; /**< Total actual export. */
} pf_area_soln;

/** This structure holds power flow solution (output) data for interties which can be. */
typedef struct {
   char   type[3];      /**< Two character record type. */
   float  Pexport;      /**< Solution export power. */
   float  Pcirc;        /**< Solution circulating current. */
   int    input_exists; /**< An integer indicating whether intertie values are internally or externally generated.<br>
                               0= no input record (internally generated itie)<br>
                               1= input data is from input record. */
} pf_itie_soln;

/** This structure holds power flow solution (output) data for continuation bus (+) records. */
typedef struct {
   char   type[3]; /**< Two character bus record type, for example, "+"", "+A", etc. */
   float  Pgen;    /**< Solved real power generation in MW. */
   float  Qgen;    /**< Solved reactive power generation in MVAR. */
   float  Pload;   /**< Solved real load (same as input). */
   float  Qload;   /**< Solved reactive load (same as input). */
   float  Gshunt;  /**< Solved real shunt. */
   float  Bshunt;  /**< Solved reactive shunt. */
} pf_cbus_soln;

/** This structure holds power flow solution (output) data for reactive power cability curves. */
typedef struct {
   char   type[3]; /**< Two character record type – here "QP", "QM", or "QN". */
   float  Pgen;    /**< Real power output level in MW. */
   float  Qgen;    /**< Reactive power output level in MVAR. */
} pf_qcurve_soln;

/** This structure holds power flow solution (output) data for switched reactance (capacitor and
reactor banks) devices. */
typedef struct {
   char   type[3];            /**< Two character array designating the record type — here “X” for switched reactance record. */
   int    seg1_sch_units;     /**< Scheduled number of steps. */
   int    seg1_used_units;    /**< Actual number of steps used. */
   float  seg1_mvar_per_unit; /**< Actual reactance amounts used per step. */
   int    seg2_sch_units;     /**< Scheduled number of steps. */
   int    seg2_used_units;    /**< Actual number of steps used. */
   float  seg2_mvar_per_unit; /**< Actual reactance amounts used per step. */
   int    seg3_sch_units;     /**< Scheduled number of steps. */
   int    seg3_used_units;    /**< Actual number of steps used. */
   float  seg3_mvar_per_unit; /**< Actual reactance amounts used per step. */
   int    seg4_sch_units;     /**< Scheduled number of steps. */
   int    seg4_used_units;    /**< Actual number of steps used. */
   float  seg4_mvar_per_unit; /**< Actual reactance amounts used per step. */
   int    seg5_sch_units;     /**< Scheduled number of steps. */
   int    seg5_used_units;    /**< Actual number of steps used. */
   float  seg5_mvar_per_unit; /**< Actual reactance amounts used per step. */
   int    seg6_sch_units;     /**< Scheduled number of steps. */
   int    seg6_used_units;    /**< Actual number of steps used. */
   float  seg6_mvar_per_unit; /**< Actual reactance amounts used per step. */
   int    seg7_sch_units;     /**< Scheduled number of steps. */
   int    seg7_used_units;    /**< Actual number of steps used. */
   float  seg7_mvar_per_unit; /**< Actual reactance amounts used per step. */
   int    seg8_sch_units;     /**< Scheduled number of steps. */
   int    seg8_used_units;    /**< Actual number of steps used. */
   float  seg8_mvar_per_unit; /**< Actual reactance amounts used per step. */
} pf_xdata_soln;

typedef pf_R  pf_RN;
typedef pf_R  pf_RQ;
typedef pf_R  pf_RV;
typedef pf_RM pf_RP;

/** Input data union. */
typedef union {
  pf_AC_bus           ACbus; /**< Use for accessing AC bus input data. */
  pf_DC_bus           DCbus; /**< Use for accessing DC bus input data. */
  pf_branch          branch; /**< Use for accessing branch input data. */
  pf_LD                  LD; /**< Use for accessing Two Terminal DC Line input data. */
  pf_LM                  LM; /**< Use for accessing Multiterminal DC Line input data. */
  pf_E                    E; /**< Use for accessing Equivalent Transmission Line Branch input data. */
  pf_L                    L; /**< Use for accessing Balanced Transmission Line input data. */
  pf_T                    T; /**< Use for accessing Transformer input data. */
  pf_TP                  TP; /**< Use for accessing Phase Shifting Transformer input data. */
  pf_R                    R; /**< Use for accessing Regulating Transformer (LTC voltage control) input data. */
  pf_RN                  RN; /**< Use for accessing Regulating Transformer (Contraints on VAR flow via change to RQ type of limits are violated) input data. */
  pf_RQ                  RQ; /**< Use for accessing Regulating Transformer (LTC VAR control) input data. */
  pf_RV                  RV; /**< Use for accessing Regulating Transformer (Contraints on VAR flow via change to RP type of limits are violated) input data. */
  pf_RP                  RP; /**< Use for accessing Regulating Transformer (LTC phase shifter) input data. */
  pf_RM                  RM; /**< Use for accessing Regulating Transformer input data. */
  pf_RZ                  RZ; /**< Use for accessing Series Compensated Rani Model input data. */
  pf_area              area; /**< Use for accessing area input data. */
  pf_itie              itie; /**< Use for accessing intertie input data. */
  pf_cbus              cbus; /**< Use for accessing continuation bus input data. */
  pf_qcurve          qcurve; /**< Use for accessing reactive power capability curve input data. */
  pf_xdata            xdata; /**< Use for accessing switched reactance input data. */
  char            cmnt[120]; /**< Use for accessing comment. */
} input_data;

/** Solution (output) data union. */
typedef union {
  pf_bus_AC_soln      ACbus; /**< Use for accessing AC bus solution (output) data. */
  pf_bus_DC_soln      DCbus; /**< Use for accessing DC bus solution (output) data. */
  pf_branch_soln     branch; /**< Use for accessing branch solution (output) data. */
  pf_area_soln         area; /**< Use for accessing area solution (output) data. */
  pf_itie_soln         itie; /**< Use for accessing intertie solution (output) data. */
  pf_cbus_soln         cbus; /**< Use for accessing continuatino bus solution (output) data. */
  pf_qcurve_soln     qcurve; /**< Use for accessing reactive capability curve solution (output) data. */
  pf_xdata_soln       xdata; /**< Use for accessing switched reactance solution (output) data. */
} solution_data;

/** Record data. */
typedef struct {
  input_data    i; /**< Used to access power flow input data. */
  solution_data s; /**< Used to access power flow solution (output) data. */
} pf_rec;

/** Comment card data. */
typedef struct {
   char   case_name[11];   /**< Ten character string containing caseid. */
   char   case_desc[21];   /**< Twenty character string containing case description. */
   char   h[3][133];       /**< A character array containing case headers. The first one, h[0], 
                                is generated by IPF, and contains the program version, the caseid
                                and description, and the date of the run. The other two are user-specified. */
   char   c[20][121];      /**< A character array containing case comments. */
} pf_comments;

/** Miscellaneous case data. */
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

/** Close the data link to the powerﬂow engine (ipfsrv).
 *
 * Call this in to “disconnect” properly from ipfsrv.
 *
 * @return Has no return; it calls the exit function. */
void pf_cflow_exit(void);

/** Initialize the data link to the powerﬂow engine (ipfsrv).
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

/** Buffer interface to powerﬂow.
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

/** Delete area by name.
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

/** Delete zone by name.
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

/** Rename an area.
 *
 * Utilizes the powerflow change records of type Z.
 *
 * @param[in] oldname A string representing an area name to be changed.
 * @param[in] newname A string representing an area name that will become the new name.
 *
 * @see pf_rename_zone pf_rename_bus
 * 
 * @return Returns 0 (zero) if it is successful; otherwise, it returns 1. */
int pf_rename_area(char *oldname, char *newname);

/** Rename a bus and re-map all associated data to the new name.
 *
 * Utilizes the powerflow change records of type Z.
 *
 * @param[in] oldname A string representing a bus name to be changed.
 * @param[in] oldkv   A floating point number representing the base kV of the bus.
 * @param[in] newname A string representing an area name that will become the new name.
 * @param[in] newkv   A floating point number representing the new base kV.
 *
 * @see pf_rename_zone pf_rename_area
 * 
 * @return Returns 0 (zero) if it is successful; otherwise, it returns 1. */
int pf_rename_bus(char *oldname, float oldkv, char *newname, float newkv);

/** Rename a zone.
 *
 * All zone fields for all records in a zone are updated. If the new zone name 
 * already exists, a combined zone results if adjacency permits; otherwise, it
 * is an error.
 *
 * Utilizes the powerflow change records of type Z.
 *
 * @param[in] oldname A string representing a zone name to be replaced.
 * @param[in] newname A string representing the new zone name.
 * 
 * @return Returns 0 (zero) if it is successful; otherwise, it returns 1. */
int pf_rename_zone(char *oldname, char *newname);
/** @example pf_rename.c */

/** Load changes into case.
 *
 * Passes an ASCII change file name to ipfsrv so that it can read and interpret
 * the file or @p filename contains an "*\n" followed by change records which 
 * are to be processed by ipfsrv. The records must be separated by "\n".
 *
 * @param[in] filename A string representing a file name, or  "*\n", followed by valid change records.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_load_changes(char *filename);

/** Load network data.
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

/** Load a base case.
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

/** Send WSCC change record to powerﬂow.
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
 * Read, write, and modify powerﬂow records.
 *
 * The pf_rec_ functions allow powerflow input (network data) and output 
 * (solution) data to be retrieved, as well as allowing input data (network 
 * data) to be added, modified, or deleted.
 *
 * @{ */

/** Retrieves, modifies, adds, or deletes area data.
 *
 * @param[in,out] r      A pointer to a structure of type pf_rec supplied by the 
 *                      calling routine.
 * @param[in]     action A string designating the action to be performed on an area
 *                       record. See below for the codes and their meanings. 
 *                       Either upper or lower case is acceptable.<br>
 *                         "F" Retrieves the first area record.<br>
 *                         "N" Retrieves the next area record. (Area name must be valid.)<br>
 *                         "G" Retrieves the rest of the area record. (Area name must be valid.)<br>
 *                         "D" Deletes an area record. (Area name must be valid.)<br>
 *                         "A" Adds an area record. (All required data fields must be valid. See the IPF Batch User’s Guide .)<br>
 *                         "M" Modifies an area record. (All required data fields must be valid. See the IPF Batch User’s Guide .)<br>
 *                         "E" Eliminates all area records. (area_rec is ignored and can be set to zero. This code does not delete any zones, buses, etc. It places all zones in area "blank.") <br>
 *                         "O" Retrieves the solution output data. (The case must be solved and the area name must be valid.)<br>
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_rec_area(pf_rec *r, char *action);

/** Retrieves, modifies, adds, or deletes branch records.
 *
 * @param[in,out] r      A pointer to a structure of type pf_rec, supplied by the calling routine.
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

 * @param[in,out] r      A pointer to a structure of type pf_rec supplied by the calling routine.
 * @param[in]     action A string designating the action to be performed on an 
 *                       intertie record. See below for the codes and their 
 *                       meanings. Either upper or lower case is acceptable.<br>
 *                         "F" Retrieves the first bus record.<br>
 *                         "N" Retrieves the next bus record. (Name and kV must be valid.)<br>
 *                         "G" Retrieves the rest of the bus record. (Name and kV must be valid.)<br>
 *                         "D" Deletes a bus record. (Name and kV must be valid.)<br>
 *                         "A" Adds a bus record. (All data fields must be valid. See the Record Formats section.)<br>
 *                         "M" Modifies a bus record. (All data fields must be valid. See the Record Formats section.)<br>
 *                         "O" Retrieves the solution output data. (The case must be solved and name and kV must be valid.)
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_rec_bus(pf_rec *r, char *action);

/** Retrieves, modifies, adds, or deletes continuation bus (+) data.  Note that 
* cbus data is always associated with particular buses.
 *
 * @param[in,out] r      A pointer to a structure of type pf_rec, supplied by 
 *                       the calling routine.
 * @param[in]     action A string designating the action to be performed on a 
 *                       continuation bus record. See the table below for the 
 *                       codes and their meanings. Either upper or lower case
 *                       is acceptable.<br>
 *                         "F1" Retrieves the first continuation bus record associated with a given bus (name, kV).<br>
 *                         "N1" Retrieves the next cbus record associated with a given bus. (All ID fields must be valid. See the Record Formats section.)<br>
 *                         "G" Retrieves the rest of the cbus record. (All ID fields must be valid. See the Record Formats section.)<br>
 *                         "D" Deletes a cbus record. (All ID fields must be valid. See the Record Formats section.)<br>
 *                         "A" Adds a cbus record. (All data fields must be valid. See the Record Formats section.)<br>
 *                         "M" Modifies a cbus record. (All data fields must be valid. See the Record Formats section.)<br>
 *                         "O" Retrieves the output data. (The case must be solved; all ID fields must be valid. See the Record Formats section.)
 *
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_rec_cbus(pf_rec *r, char *action);

/** Retrieves or modifies the case name, project title, and case comments.
 *
 * @param[in] r      A pointer to a structure of type pf_comments.
 * @param[in] action A string designating the action to be performed. See 
                     below for the codes and their meanings. Either upper or 
                     lower case is acceptable.<br>
 *                     "G" Retrieves the case comments.<br>
 *                     "M" Modifies the case comments. All data is updated 
 *                     with the contents of the record.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_rec_comments(pf_comments *r, char *action);

/** Retrieves, adds, modifies, and deletes intertie data.
 *
 * @param[in,out] r      A pointer to a structure of type pf_rec, supplied by the calling routine.
 * @param[in]     action A string designating the action to be performed on an intertie record. 
 *                       See below for the codes and their meanings. Either upper or lower case is
 *                       acceptable.<br>
 *                         "F" Retrieves the first intertie record.<br>
 *                         "N" Retrieves the next intertie record. (Name1 and Name2 must be valid.)<br>
 *                         "G" Retrieves the rest of the intertie record. (Name1 and Name2 must be valid.)<br>
 *                         "D" Deletes an intertie record. (Name1 and Name2 must be valid.)<br>
 *                         "A" Adds an intertie record. (All data fields must be valid. See the Record Formats section.)<br>
 *                         "M" Modifies an intertie record. (All data fields must be valid. See the Record Formats section.)<br>
 *                         "O" Retrieves the solution output. (The case must be solved and name1 and name2 must be valid.)
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_rec_itie(pf_rec *r, char *action);

/** Retrieves, modifies, adds, or deletes reactive power capability curve data.
 *
 * @param[in,out] r      A pointer to a calling routine-supplied structure of type pf_rec.
 * @param[in]     action A string designating the action to be performed on 
 *                       qcurve record. See below for the codes and their meanings. 
 *                       Either upper or lower case is acceptable.<br>
 *                         "G" Retrieves the rest of the Q curve records associated with a given bus. (Name and kV must be valid. See the Record Formats section.)<br>
 *                         "D" Deletes a Q curve record. (Name and kV must be valid. See the Record Formats section.)<br>
 *                         "M" Modifies a Q curve record. (Valid only for activation or inactivation. See the Record Formats section.)
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_rec_qcurve(pf_rec *r, char *action);

/** Retrieves, modifies, and adds switched reactance (X) data.
 *
 * The delete function is handled by changing the BX bus to another bus type or deleting the BX bus.
 *
 * @param[in,out] r      A pointer to a structure of type pf_rec, supplied by the calling routine.
 * @param[in]     action A string designating the action to be performed on a switched reactance 
 *                       record. See below for the codes and their meanings. Either upper or 
 *                       lower case is acceptable.<br>
 *                         "F" Retrieves the first xdata record in a case.<br>
 *                         "N" Retrieves the next xdata record in a case. (Name and kV must be valid. See the Record Formats section.)<br>
 *                         "G" Retrieves the xdata record associated with bus_name and bus_kV.<br>
 *                         "A" Adds an xdata record. (All required data must be valid. See the Record Formats section.)<br>
 *                         "M" Modifies an xdata record. (All required data must be valid. See the Record Formats section.)<br>
 *                         "O" Retrieves the output data. (The case must be solved; all id fields must be valid. See the Record Formats section.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1.*/
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

/** Save a CHANGES file.
 *
 * Saves to a change file the input data changes you have made to the currently
 * resident base case data.
 *
 * @param[in] filename A string representing a file name.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1.*/
int pf_save_changes(char *filename);

/** Save network data.
 *
 * Save network data from a powerflow base case in ASCII format.
 *
 * @param[in] filename A string representing a file name.
 * @param[in] dialect  A string having the following possible values: "BPA", 
 *                     "WSCC", "WSCC1", or "PTI". These refer to different 
 *                     forms that the output file can take. See ?? for the 
 *                     differences in dialects.
 * @param[in] ratings  A string having the following possible values: 
 *                     "EXTENDED", "NOMINAL", or "MIN_EXTENDED". See ?? for a
 *                     description of these options for ratings.
 * @param[in] size     An integer representing the size of output records - 
 *                     either 80 or 120. The choice of 120 is valid only with 
 *                     the BPA dialect.
 *
 * @return Returns 0 if it is successful; otherwise, it returns 1.*/
int pf_save_netdata(char *filename, char *dialect, char *ratings, int size);

/** Save new base case.
 *
 * Saves the currently resident base case in its current state to the specified 
 * filename.
 *
 * @param[in] filename A string representing a file name
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_save_newbase(char *filename);

/** Save WSCC Stability Data.
 *
 * Saves the power flow data required for input to the WSCC Stability program 
 * in either ASCII or binary form depending on the type argument value.
 *
 * @param[in] filename A string representing a file name.
 * @param[in] type     A string representing the type of file format of the 
 *                     saved file. The type values are either "ASCII" or "BINARY"
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_save_wscc_stab_data(char *filename, char *type);
/** @example pf_save.c */

/** Solve the current case.
 *
 * Causes the powerflow process (`ipfsrv`) to initiate a solution on the 
 * currently resident base case data.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns -1. */
int pf_solution(void);

/** Start up and initialize the powerﬂow engine.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_init(void);

/** Retrieve list of owners, areas, bus kV's, record types, or zones.
 *
 * The @p list argument should be a two dimensional character array with the 
 * first dimension being the number of areas, and the second dimension being
 * 11 to store area names of up to 10 characters in length. For example, for 
 * AREA_LIST, the array might be declared as @code char area_names[20][11] 
 * @endcode if you know that there are not more than 20 area names in the case.
 * pf_case_info gives information on the number of areas, zones, owners, etc. 
 * The listlen argument prevents the routine from exceeding the bounds of your
 * array.
 *
 * @param[out] list    A pointer to a two dimensional character array of size [area size][11].
 * @param[in]  listlen An integer specifying the maximum number of list elements.
 * @param[in]  type    The type of list.
 * @param[in]  data    Any additional data needed for Program Control Language query. Use "" if none.
 *
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_get_list( char *list, int listlen, enum pf_list_type type, char *data );
/** @example pf_get_list.c */

/** Finds the name of the area that a zone is in.
 *
 * @param[out] area      A pointer to an array of 11 characters in which the area name is returned.
 * @param[in]  zone      A string holding a zone name.
 *
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_area_of_zone(char *area, char *zone);
/** @example pf_area_of_zone.c */

/** Retrieve case info.
 * 
 * Retrieves data from a Powerflow base case and puts it in the info structure.
 *
 * @param[out] r A pointer to a structure of type pf_case_stats.
 *
 * @return Returns 0 if it is successful; otherwise, it returns 1. */
int pf_case_info(pf_case_stats *r);
/** @example pf_case_info.c */

/** Initialize pf_rec data.
 *
 * Initializes a data buffer of type pf_rec to blanks and zeros, in order to 
 * clear out old data before calling one of the pf_rec routines to store new 
 * data in it. Its use is not necessary, but is recommended.
 *
 * @param r     A pointer to a structure of type pf_rec, supplied by the calling routine.
 * @param rtype An enumerated variable defined in ft.h */
void pf_init_rec(void *r, int rtype);

/** Initialize all P-Q curve data fields.
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

/** Initialize all intertie data fields.
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

/** Initialize all continuation bus fields.
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

/** Initialize all bus data fields.
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

/** Initialize all branch data fields.
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

/** Initialize all area data fields.
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

/** Seee if a bus exists.
 * 
 * @author William D. Rogers
 * @date 8-17-1994
 *
 * @param[in] name A pointer to a string containing the bus name.
 * @param[in] kv   A real value representing the bus base kV.
 * 
 * @return Returns 0 if the bus exists; otherwise, it returns 1. */
int pf_bus_exists(char *name, float kv);
/** @example pf_bus_exists.c */

/** @addtogroup pf_user
  The pf_user_ functions provide a means of using the User Analysis features of
  the powerflow.

@{ */

/** Initialize User Analysis definitions.
 * 
 * Initializes the user analysis arrays in powerflow (IPF). It should be called
 * prior to other user analysis functions. It sends the command /INITDEF to ipfsrv.
 *
 * @author William D. Rogers
 * @date 1-6-1995
 * 
 * @return Returns 0 if it is successful; otherwise, it returns -1. */
int pf_user_init_def(void);

/** Load User Analysis definitions into IPF.
 *
 * Loads the user analysis arrays in powerflow (IPF) with the specified symbol 
 * definitions. It sends the command @code/LOADDEF @endcode, followed by a 
 * newline ( @code '\n' @endcode ) separated list of definitions, to powerflow.
 * 
 * @author William D. Rogers
 * @date 1-6-1995
 *
 * @param[in] definitions A pointer to a string containing User Analysis Define statements.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns -1. */
int pf_user_load_def(char *definitions);

/** Substitute User Analysis definitions.
 * 
 * Performs character string substitutions using computed base case quantities
 * upon the tokens defined with the >DEFINE statements within comment records 
 * sent to powerflow either through the pf_user_load_def function or a User 
 * Analysis file. The return message is available in the global buffer reply_pf.
 * It sends the command @code /SUBDEF, SOURCE=<name> @endcode, where name is 
 * either "BASE" or "ALTERNATE_BASE" ("ALT").
 *
 * @author William D. Rogers
 * @date 1-6-1995
 *
 * @param[in] base   A pointer to a string designating the source base case as 
 *                   "BASE" or "ALTERNATE_BASE"  (may be abreviated to "ALT").
 * 
 * @return Returns 0 if it is successfull; otherwise, it returns -1. */
int pf_user_sub_def(char *base);

/** Load a User Analysis file for customized analysis listings.
 * 
 * Loads a user analysis file for generating customized analysis listings. The 
 * requested report is appended to the output file, which is created if it 
 * doesn't already exist.
 *
 * @author William D. Rogers
 * @date 1-6-1995
 *
 * @param[in] filename A character string representing a user analysis file name.
 * @param[in] output   A character string representing an output report file name.
 * @param[in] action   A string designating the action to be performed.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns -1. */
int pf_user_report(char *filename, char *output, char action);

/** Define User Analysis Symbol.
 * 
 * Builds a symbol definition and loads it into the user-analysis arrays in
 * powerflow (IPF).  It sends a command constructed as follows:
 @code
 /LOADDEF 
 > DEFINE_TYPE <symbol_type> 
 LET <symbol_name> = <id_of_computed_quantity>
 @endcode
 * If blanks are part of the quantity id, substitute them with pound signs (#).
 * All data is case insensitive. Symbol names are limited to six characters. 
 * Use blanks or commas to separate identity items.
 *
 * @author William D. Rogers
 * @date 1-30-1995
 *
 * @param symbol A pointer to a string containing a symbol name.
 * @param id     A pointer to a string containing quantity identity.
 * @param type   A pointer to a string containing symbol type.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns -1. */
int pf_user_define(char *symbol, char *id, char *type);

/** Define User Analysis comment record.
 * 
 * Builds a comment record and loads it into the user-analysis arrays in 
 * powerflow (IPF). It sends a command constructed as follows:
 @code
 /LOADDEF 
 C <symbol_name><symbol_suffix> = $<symbol_name><symbol_suffix><format>
 @endcode
 * All data is case insensitive, however, the case is preserved.  Symbol names
 * are limited to six characters. The suffix is optional and is used for 
 * BUS_INDEX and ZONE_INDEX data types. Use a null string ("") if the suffix is
 * not applicable. The format obeys the Fortran convention and can be either 
 * floating point (i. e. /F8.3) or text (i. e. /A7). The default is /F6.0. The
 * format string must include the slash (/).
 *
 * Comment cards constructed by pf_user_comment are designed to be processed by
 * either pf_user_quantity to retrieve floating point values or pf_user_string 
 * to retrieve textual information.
 *
 * @author William D. Rogers
 * @date 1-30-1995
 *
 * @param[in] symbol A pointer to a string containing a symbol name.
 * @param[in] suffix A pointer to a string containing an index suffix.
 * @param[in] format A pointer to a string containing format code.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns -1. */
int pf_user_comment(char *symbol, char *suffix, char *format);

/** Retrieve a User Analysis quantity.
 * 
 * Searches the reply_pf buffer after a pf_user_sub_def function is called for 
 * "<symbol_name><index_suffix> = ", where the suffix is optional, and scans in
 * the floating point value that immediately follows. Comment cards can be 
 * built with pf_user_comment or any other applicable method. The case of the 
 * symbol and suffix must match. The suffix is optional and is used for 
 * BUS_INDEX and ZONE_INDEX data types. Use a null string ("") for the suffux, 
 * if the data type does not use a suffix. The suffix must include the period 
 * (i.e. ".VK").
 *
 * @author William D. Rogers
 * @date 1-30-1995
 *
 * @param[in] symbol    A pointer to a string containing a symbol name.
 * @param[in] suffix    A pointer to a string containing an index suffix.
 * @param[in]  quantity A pointer to a float to hold the retrieved quantity.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns -1. */
int pf_user_quantity(char *symbol, char *suffix, float *quantity);

/** Retrieve for User Analysis string.
 * 
 * Searches the reply_pf buffer after a pf_user_sub_def function is called for 
 * "<symbol_name> = " and scans into @p info the number of immediately 
 * following characters specified by length. Comment cards can be built with 
 * pf_user_comment or any other applicable method. The case of the symbol name 
 * must match. Where applicable include the suffix in the symbol name string.
 *
 * @author William D. Rogers
 * @date 1-31-1995
 *
 * @param[in] symbol A pointer to a string containing a symbol name.
 * @param[in] length An integer specifying the number of characters to scan.
 * @param[in] info   A pointer to a destination string for the scanned data.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns -1. */
int pf_user_string(char *symbol, int length, char *info);

/** Define User Analysis Branch Index.
 * 
 * Builds a symbol definition and corresponding comment card based on the data 
 * in a pf_rec structure and the supplied symbol and type and loads them into 
 * the user-analysis arrays in powerflow (IPF). It sends a command constructed
 * as follows:
 @code
 /LOADDEF 
 > DEFINE_TYPE <symbol_type> 
 LET <symbol_name> = <bus1_name> <bus1_kv>[*] <bus2_name> <bus2_kv>[*] 
 C <symbol_name> = $<symbol_name>/F15.7
 @endcode
 * Bus names and voltages are derived from the pf_rec branch structure.  Blanks
 * in the bus names are replaced by pound signs (#). An asterisk (*) determines 
 * at which terminal the line flow is computed. If the metering point in the 
 * branch data is 0 or 1, the first bus is selected, if 2 then the second. Symbol
 * names are limited to six characters and are case insensitive, but retain case 
 * for the comment card.
 *
 * @author William D. Rogers
 * @date 1-31-1995
 *
 * @param[in] symbol A pointer to a string containing a symbol name.
 * @param[in] r      A pointer to a structure of type pf_rec.
 * @param[in] type   A pointer to a string indicating the quantity type.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns -1. */
int pf_user_branch(char *symbol, pf_rec *r, char *type);

/** Define User Analysis intertie symbol.
 *
 * Builds a symbol definition and corresponding comment card based on the data 
 * in a pf_rec structure and the supplied symbol and type and loads them into 
 * the user-analysis arrays in powerflow (IPF). It sends a command constructed 
 * as follows:
 @code
 /LOADDEF 
 > DEFINE_TYPE <symbol_type> 
 LET <symbol_name> = <area1_name> <area2_name> 
 C <symbol_name> = $<symbol_name>/F15.7
 @endcode
 * Area names are derived from the pf_rec intertie structure.  Blanks in the 
 * area names are replaced by pound signs (#).  Symbol names are limited to
 * six characters and are case insensitive, but retain case for the  comment
 * card.
 *
 * @author William D. Rogers
 * @date 1-31-1995
 *
 * @param[in] symbol A pointer to a string containing a symbol name.
 * @param[in] r      A pointer to a structure of type pf_rec.
 * @param[in] type   A pointer to a string indicating the quantity type.<br>
 *                     'P' = INTERTIE_P<br>
 *                     'Q' = INTERTIE_Q<br>
 *                     'S' = INTERTIE_P_SCHEDULED
 * 
 * @return Returns 0 if it is successful; otherwise, it returns -1. */
int pf_user_itie(char *symbol, pf_rec *r, char *type);

/** Define User Analysis Bus Index.
 * 
 * Builds a symbol definition and corresponding comment card based on the data in a 
 * pf_rec structure and the supplied symbol and suffix and loads them into the 
 * user-analysis arrays in powerflow (IPF). It sends a command constructed as 
 * follows :
 @code
 /LOADDEF 
 > DEFINE_TYPE  BUS_INDEX 
 LET <symbol_name> = <bus_name> <bus_kv> 
 C <symbol_name><index_suffix> = $<symbol_name><index_suffix>/F15.7
 @endcode
 * Bus name and voltage is derived from the pf_rec bus structure. Blanks in the
 * bus names are replaced by pound signs (#).  Symbol names are limited to six 
 * characters and are case insensitive, but retain case for the  comment card. 
 * The suffix must contain the period (i. e. ".VK").
 *
 * @author William D. Rogers
 * @date 1-31-1995
 *
 * @param[in] symbol A pointer to a string containing a symbol name.
 * @param[in] r      A pointer to a structure of type pf_rec.
 * @param[in] suffix A pointer to a string containing the BUS_INDEX suffix.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns -1. */
int pf_user_bus(char *symbol, pf_rec *r, char *suffix);
/** @} */

/** Create a plot.
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

/** Load a reference base case.
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

/** Select primary base base or reference base base records.
 *
 * Allows the pf_rec functions to access the input and solution data in either 
 * the primary base case (OLDBASE) loaded with pf_load_oldbase or the reference 
 * (alternate) base case loaded with pf_load_refbase. The accessed base case 
 * initially defaults to the OLDBASE data.
 *
 * @author William D. Rogers
 * @date 7-21-1995
 *
 * @param[in] base A character indicating which set of data other commands act
 *                  upon: 'O' OLDBASE data or 'R' REFBASE data.
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1.*/
int pf_select_base(char base);

/** Get the area interchange for an area.
 * 
 * Run the solution to obtain the area interchange amount. Can run for either 
 * the old base case or the reference base case.
 *
 * @author William D. Rogers
 * @date 3-6-1996
 *
 * @param[in] base A character indicating the base case for which to 
 *                 calculate area interchange.<br>
 *                   'O' = Old base case
 *                   'R' = Reference base case
 * 
 * @return Returns 0 if it is successful; otherwise, it returns 1.*/
int pf_solve_area(char base);

/** Run PCL commands.
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
