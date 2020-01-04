
float a2f(char *str, int size, int dec);
long a2l(char *str, int size);
char *l2a(long l, int size);
char *f2a(float f, int size, int dec);
int put_fld_a(void *arec, char *buf, int fld);
int put_fld_b(void *arec, char *buf, int fld);
int get_fld_a(char *buf, void *arec, int fld);
int cv_rec_a2b(void *brec, void *arec, int rec);
int cv_rec_b2a(void *arec, void *brec, int rec);
int cv_pf_a2b(void *brec, void *arec);
int cv_pf_b2a(void *arec, void *brec);
int cv_pfs_a2b(void *brec, void *arec);
int cv_pfs_b2a(void *arec, void *brec);

struct fld_table {
  char bintype;
  int boffset;
  int bsize;
  int aoffset;
  int asize;
  int adec;
};

#define MAXRECNUM   27           /* PREVIOUSLY 25 */
#define MAXFIELDNUM 26           /* PREVIOUSLY 24 */
#define CV_INVALID_DIGIT 1
#define CV_NUMERIC_OVERFLOW 2
#define CV_TRUNCATED 3

enum {PF_REC=0,AC_BUS=1,LD_LINE,LM_LINE,E_LINE,L_LINE,T_LINE,TP_LINE,
      R_LINE,RN_LINE=R_LINE,RQ_LINE=R_LINE,RV_LINE=R_LINE,
      RM_LINE,RP_LINE=RM_LINE,
      RZ_LINE,AC_SOLN,DC_SOLN,DC_BUS,BRANCH_SOLN,BRANCH,
      AREA_SOLN,AREA,ITIE_SOLN,ITIE,CBUS_SOLN,CBUS,
      QCURVE,XDATA_SOLN,XDATA,COMM};  /* assign record numbers */
enum {AC_BUS_TYPE=AC_BUS*256 +1,
      AC_BUS_OWNER,      AC_BUS_NAME,      AC_BUS_KV,        AC_BUS_ZONE,
      AC_BUS_PLOAD,      AC_BUS_QLOAD,     AC_BUS_PSHUNT,    AC_BUS_QSHUNT,
      AC_BUS_PMAX,       AC_BUS_PGEN,      AC_BUS_QSCH_QMAX, AC_BUS_QMIN,
      AC_BUS_VHOLD_VMAX, AC_BUS_VMIN_VDEG, AC_BUS_RMT_NAME,  AC_BUS_RMT_KV, 
      AC_BUS_PCT_VARS};
enum {LD_TYPE=LD_LINE*256 +1,
      LD_OWNER,          LD_BUS1_NAME,     LD_BUS1_KV,       LD_METER,
      LD_BUS2_NAME,      LD_BUS2_KV,       LD_I_OR_R_CONTROL,LD_TOTAL_RATING,
      LD_R,              LD_L_MH,          LD_C_UF,          LD_P_SCHED,
      LD_V_SCHED,        LD_MILES,         LD_ALPHA_N_DEG,   LD_GAMMA_O_DEG,
      LD_THERMAL_RATING, LD_BOTTLENECK_RATING};
enum  {LM_TYPE=LM_LINE*256 +1,
       LM_OWNER,          LM_BUS1_NAME,     LM_BUS1_KV,       LM_METER,
       LM_BUS2_NAME,      LM_BUS2_KV,       LM_TOTAL_RATING,  LM_R,
       LM_L_MH,           LM_C_UF,          LM_MILES,         LM_DATE_IN,
       LM_DATE_OUT,       LM_THERMAL_RATING,LM_BOTTLENECK_RATING};
enum   {E_TYPE=E_LINE*256 +1,
        E_OWNER,           E_BUS1_NAME,      E_BUS1_KV,        E_METER,
        E_BUS2_NAME,       E_BUS2_KV,        E_CKT_ID,         E_SECTION,
        E_TOTAL_RATING,    E_NUM_CKTS,       E_R,              E_X,
        E_GL,              E_B1,             E_G2,             E_B2,
        E_DATE_IN,         E_DATE_OUT,       E_THERMAL_RATING,
        E_BOTTLENECK_RATING};
enum   {L_TYPE=L_LINE*256 +1,
        L_OWNER,           L_BUS1_NAME,      L_BUS1_KV,        L_METER,
        L_BUS2_NAME,       L_BUS2_KV,        L_CKT_ID,         L_SECTION,
        L_TOTAL_RATING,    L_NUM_CKTS,       L_R,              L_X,
        L_G,               L_B,              L_MILES,          L_DESCRIP,
        L_DATE_IN,         L_DATE_OUT,       L_THERMAL_RATING, 
        L_BOTTLENECK_RATING};
enum   {T_TYPE=T_LINE*256 +1,
        T_OWNER,           T_BUS1_NAME,      T_BUS1_KV,        T_METER,
        T_BUS2_NAME,       T_BUS2_KV,        T_CKT_ID,         T_SECTION,
        T_TOTAL_RATING,    T_NUM_CKTS,       T_R,              T_X,
        T_G,               T_B,              T_TAP1,           T_TAP2,
        T_DATE_IN,         T_DATE_OUT,       T_THERMAL_RATING, 
        T_BOTTLENECK_RATING, T_EMERGENCY_RATING};
enum   {TP_TYPE=TP_LINE*256 +1,
        TP_OWNER,          TP_BUS1_NAME,      TP_BUS1_KV,    TP_METER,
        TP_BUS2_NAME,      TP_BUS2_KV,        TP_CKT_ID,     TP_SECTION,
        TP_TOTAL_RATING,   TP_NUM_CKTS,       TP_R,          TP_X,
        TP_G,              TP_B,              TP_PHASE_SHIFT_DEG,
        TP_TAP2,           TP_DATE_IN,        TP_DATE_OUT,   TP_THERMAL_RATING,
        TP_BOTTLENECK_RATING,TP_EMERGENCY_RATING};
enum   {R_TYPE=R_LINE*256 +1,
        R_OWNER,            R_BUS1_NAME,       R_BUS1_KV,    R_VAR_TAP_SIDE,
        R_BUS2_NAME,        R_BUS2_KV,         R_NUM_TAPS,   R_MAX_TAP,
        R_MIN_TAP,          R_RMT_BUS_NAME,    R_DATE_IN,    R_DATE_OUT,
        R_RMT_BUS_KV,       R_QMAX,            R_QMIN};
enum   {RM_TYPE=RM_LINE*256 +1,
        RM_OWNER,           RM_BUS1_NAME,      RM_BUS1_KV,   RM_VAR_TAP_SIDE,
        RM_BUS2_NAME,       RM_BUS2_KV,        RM_NUM_TAPS,  
        RM_MAX_PHASE_SHIFT_DEG,                RM_MIN_PHASE_SHIFT_DEG,
        RM_RMT_BUS_NAME,    RM_DATE_IN,        RM_DATE_OUT,  RM_RMT_BUS,
        RM_PMAX,            RM_PMIN};
enum   {RZ_TYPE=RZ_LINE*256 +1,
        RZ_OWNER,          RZ_BUS1_NAME,       RZ_BUS1_KV,   RZ_VAR_TAP_SIDE,
        RZ_BUS2_NAME,      RZ_BUS2_KV,         RZ_CKT_ID,    RZ_SECTION,
        RZ_I_RATE,         RZ_RANI_TYPE,       RZ_PC_MAX,    RZ_PC_MIN,
        RZ_XIJ_MAX,        RZ_XIJ_MIN,         RZ_BIS_MAX,   RZ_BIS_MIN};
enum   {AC_SOLN_PGEN=AC_SOLN*256 +1,
        AC_SOLN_QGEN,     AC_SOLN_VMAG,        AC_SOLN_VDEG, AC_SOLN_PLOAD,
        AC_SOLN_QLOAD,    AC_SOLN_BSHUNT_USED, AC_SOLN_BSHUNT_SCH,
        AC_SOLN_BSHUNT__USED_CAP,              AC_SOLN_BSHUNT_CAP,
        AC_SOLN_BSHUNT_USED_RX,                AC_SOLN_BSHUNT_SCH_RX,
        AC_SOLN_QUNSCH};
enum   {DC_SOLN_P_DC=DC_SOLN*256 +1,
        DC_SOLN_Q_DC,    DC_SOLN_V_DC,        DC_SOLN_CONVERTER_DEG,
        DC_SOLN_P_VALVE_LOSSES,               DC_SOLN_Q_VALVE_LOSSES};
enum   {DC_BUS_TYPE=DC_BUS*256 +1,
        DC_BUS_OWNER,     DC_BUS_NAME,        DC_BUS_KV,     DC_BUS_ZONE,
        DC_BUS_BRIDGES_PER_CKT,               DC_BUS_SMOOTH_RX_MH,
        DC_BUS_ALPHA_MIN_DEG,                 DC_BUS_ALPHA_STOP_DEG,
        DC_BUS_VALVE_DROP_PER_BRIDGE_VOLTS,   DC_BUS_BRIDGE_CURENT_RATING_AMPS,
        DC_BUS_ALPHA_GAMMA_N_DEG,             DC_BUS_GAMMA_O_DEG,
        DC_BUS_P_SCHED,   DC_BUS_V_SCHED,     DC_BUS_COMMUTATING_BUS_NAME,
        DC_BUS_COMMUTATING_BUS_KV,            DC_BUS_CONVERTER_CODE};
enum   {BRANCH_SOLN_NUM_CKT=BRANCH_SOLN*256 +1,
        BRANCH_SOLN_PIN,   BRANCH_SOLN_QIN,   BRANCH_SOLN_POUT,
        BRANCH_SOLN_QOUT,  BRANCH_SOLN_PLOSS, BRANCH_SOLN_QLOSS,
	BRANCH_SOLN_CRIT_LINE_LOAD_AMPS,      BRANCH_SOLN_CRIT_LINE_RAT_AMPS,
	BRANCH_SOLN_CRIT_LINE_RAT_CODE,       BRANCH_SOLN_CRIT_LINE_LOAD_TERM,
	BRANCH_SOLN_CRIT_XFMR_LOAD_MVA,       BRANCH_SOLN_CRIT_XFMR_RAT_MVA,
	BRANCH_SOLN_CRIT_XFMR_RAT_CODE,       BRANCH_SOLN_CRIT_XFMR_LOAD_TERM,
	BRANCH_SOLN_TOT_LINE_LOAD_PCT,        BRANCH_SOLN_TOT_LINE_LOAD_AMPS,
	BRANCH_SOLN_TOT_XFMR_LOAD_PCT,        BRANCH_SOLN_TOT_XFMR_LOAD_MVA,
        BRANCH_SOLN_TAP1,                     BRANCH_SOLN_TAP2};
enum   {BRANCH_TYPE=BRANCH*256 +1,
        BRANCH_OWNER,      BRANCH_BUS1_NAME,  BRANCH_BUS1_KV,
        BRANCH_METER,      BRANCH_BUS2_NAME,  BRANCH_BUS2_KV,
        BRANCH_CKT_ID,     BRANCH_SECTION,    BRANCH_TOTAL_RATING,
        BRANCH_NUM_CKTS,   BRANCH_R,          BRANCH_X,
        BRANCH_G,          BRANCH_B,          BRANCH_DESCRIP,
        BRANCH_DATE_IN,    BRANCH_DATE_OUT,   BRANCH_THERMAL_RATING,
        BRANCH_EMERGENCY_RATING};
enum   {AREA_SOLN_PGEN=AREA_SOLN*256 +1,
        AREA_SOLN_PLOAD,   AREA_SOLN_PLOSS,   AREA_SOLN_PEXPORT};
enum   {AREA_TYPE=AREA*256 +1,
        AREA_NAME,         AREA_SBUS_NAME,    AREA_SBUS_KV,
        AREA_SCHED_EXPORT, AREA_ZONE0,        AREA_ZONE1,
	AREA_ZONE2,        AREA_ZONE3,        AREA_ZONE4,
	AREA_ZONE5,        AREA_ZONE6,        AREA_ZONE7,
	AREA_ZONE8,        AREA_ZONE9,        AREA_MAX_VPU,
        AREA_MIN_VPU};
enum   {ITIE_SOLN_PEXPORT=ITIE_SOLN*256 +1,
        ITIE_SOLN_PCIRC,   ITIE_SOLN_INPUT_EXISTS};
enum   {ITIE_TYPE=ITIE*256 +1,
        ITIE_AREA1_NAME,   ITIE_AREA2_NAME,   ITIE_SCHED_EXPORT};
enum   {CBUS_SOLN_PGEN=CBUS_SOLN*256 +1,    
	CBUS_SOLN_QGEN,    CBUS_SOLN_PLOAD,   CBUS_SOLN_QLOAD,
        CBUS_SOLN_GSHUNT,  CBUS_SOLN_BSHUNT};
enum   {CBUS_TYPE=CBUS*256 +1,
        CBUS_OWNER,        CBUS_NAME,         CBUS_KV,
        CBUS_CODE_YEAR,    CBUS_PLOAD,        CBUS_QLOAD,
        CBUS_GSHUNT,       CBUS_BSHUNT,       CBUS_PGEN,
        CBUS_QGEN_QMAX,    CBUS_QMIN};
enum   {QCURVE_TYPE=QCURVE*256 +1,
        QCURVE_PU_CODE,    QCURVE_ACTIVE,     QCURVE_BUS_NAME,
        QCURVE_BUS_KV,     QCURVE_PGEN0,      QCURVE_PGEN1,
        QCURVE_PGEN2,      QCURVE_PGEN3,      QCURVE_PGEN4,
        QCURVE_PGEN5,      QCURVE_PGEN6,      QCURVE_PGEN7,
        QCURVE_PGEN8,      QCURVE_PGEN9};
enum   {XDATA_SOLN_SEG1_SCH_UNITS=XDATA_SOLN*256 +1,
        XDATA_SOLN_SEG1_USED_UNITS,             XDATA_SOLN_SEG1_MVAR_PER_UNIT,
	XDATA_SOLN_SEG2_SCH_UNITS,
        XDATA_SOLN_SEG2_USED_UNITS,             XDATA_SOLN_SEG2_MVAR_PER_UNIT,
	XDATA_SOLN_SEG3_SCH_UNITS,
        XDATA_SOLN_SEG3_USED_UNITS,             XDATA_SOLN_SEG3_MVAR_PER_UNIT,
	XDATA_SOLN_SEG4_SCH_UNITS,
        XDATA_SOLN_SEG4_USED_UNITS,             XDATA_SOLN_SEG4_MVAR_PER_UNIT,
	XDATA_SOLN_SEG5_SCH_UNITS,
        XDATA_SOLN_SEG5_USED_UNITS,             XDATA_SOLN_SEG5_MVAR_PER_UNIT,
	XDATA_SOLN_SEG6_SCH_UNITS,
        XDATA_SOLN_SEG6_USED_UNITS,             XDATA_SOLN_SEG6_MVAR_PER_UNIT,
	XDATA_SOLN_SEG7_SCH_UNITS,
        XDATA_SOLN_SEG7_USED_UNITS,             XDATA_SOLN_SEG7_MVAR_PER_UNIT,
	XDATA_SOLN_SEG8_SCH_UNITS,
        XDATA_SOLN_SEG8_USED_UNITS,             XDATA_SOLN_SEG8_MVAR_PER_UNIT};
enum   {XDATA_TYPE=XDATA*256 +1,
        XDATA_OWNER,        XDATA_BUS_NAME,     XDATA_BUS_KV,
        XDATA_RMT_NAME,     XDATA_RMT_KV,
        XDATA_SEG1_NUM_STEPS,                   XDATA_SEG1_DELTA_MVA,
        XDATA_SEG2_NUM_STEPS,                   XDATA_SEG2_DELTA_MVA,
        XDATA_SEG3_NUM_STEPS,                   XDATA_SEG3_DELTA_MVA,
        XDATA_SEG4_NUM_STEPS,                   XDATA_SEG4_DELTA_MVA,
        XDATA_SEG5_NUM_STEPS,                   XDATA_SEG5_DELTA_MVA,
        XDATA_SEG6_NUM_STEPS,                   XDATA_SEG6_DELTA_MVA,
        XDATA_SEG7_NUM_STEPS,                   XDATA_SEG7_DELTA_MVA,
        XDATA_SEG8_NUM_STEPS,                   XDATA_SEG8_DELTA_MVA};
            
