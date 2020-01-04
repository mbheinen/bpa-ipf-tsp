typedef struct pf_busOut{
  char  code[1];
  char  type[1];                /* of record            */
  char  not_used[1];             /* of record            */
  char  owner[3];               /* id of bus    */
  char  name[8];                /* of bus       */
  char  base[4];                /* kv of bus */
  char  zone[2];                /* bus is located */
  char  data[63];               /* character data       */
  char  eol[1];                 /* end of line          */
} pfBusOut;

typedef struct pf_busACOut{
  char  code[1];
  char  subtype[1];                /* of record            */
  char  not_used[1];             /* of record            */
  char  owner[3];               /* id of bus    */
  char  name[8];                /* of bus       */
  char  base[4];                /* kv of bus */
  char  zone[2];                /* bus is located */
  char  P_gen[15];	/* gen in MW */
  char  Q_gen[15];	/* gen in MVAR */
  char  voltage[15];
  char  angle[15];
  char  P_load[15];
  char  Q_load[15];
  char  B_shunt_used[15];
  char  B_shunt_sched[15];
  char  B_shunt_cap_used[15];
  char  B_shunt_cap_sched[15];
  char  B_shunt_react_used[15];
  char  B_shunt_react_sched[15];
  char  Q_unscheduled[15];
  char  eol[1];                 /* end of line          */
} pfBusACOut;

typedef struct pf_busDCOut{
  char  code[1];
  char  subtype[1];                /* of record            */
  char  not_used[1];             /* of record            */
  char  owner[3];               /* id of bus    */
  char  name[8];                /* of bus       */
  char  base[4];                /* kv of bus */
  char  zone[2];                /* bus is located */
  char  P_DC[15];	/* gen in MW */
  char  Q_DC[15];	/* gen in MVAR */
  char  voltage_DC[15];
  char  angle[15];
  char  P_valve_loss[15]; /* MW */
  char  Q_valve_loss[15]; /* MVAR */
  char  eol[1];                 /* end of line          */
} pfBusDCOut;

typedef struct pf_reactanceStep {
  char  units_sched[1];
  char  units_unsched[1];
  char  reactance[15];
} pfReactanceStep; 

typedef struct pf_busXOut{
  char  code[1];
  char  subtype[1];                /* of record            */
  char  not_used[1];             /* of record            */
  char  owner[3];               /* id of bus    */
  char  name[8];                /* of bus       */
  char  base[4];                /* kv of bus */
  char  zone[2];                /* bus is located */
  pfReactanceStep step[8];
  char  eol[1];                 /* end of line          */
} pfBusXOut;

typedef struct pf_outBranch{
  char  code[1];                /* f "L" or "T"  of record */
  char  subtype[1];             /* f  of record         */
  char  not_used[1];              /* o add, delete or modify      */
  char  owner[3];               /* o id of bus  */
  char  name1[8];               /* f name of first bus  */
  char  base1[4];               /* f base kv of first bus       */
  char  meter[1];               /* o tie line metering point flag */
  char  name2[8];               /* f name of second bus */
  char  base2[4];               /* f base kv of second bus      */
  char  circuit[1];             /* f * for all parallels cirquit id         */
  char  section[1];             /* f section id         */
  char  P_in[15];	/* input power in MW */
  char  Q_in[15];	/* input power in MVAR */
  char  P_out[15];	/* output power in MW */
  char  Q_out[15];	/* output power in MVAR */
  char  P_loss[15];	/* loss in MW */
  char  Q_loss[15];	/* loss in MVAR */
  char  critical_line_load[15];	/* (max_current) maximum current for line */
  char  critical_line_rating[8];
  char  critical_line_rating_code[1];	/* (N,T,B) */
  char  critical_line_loading_terminal[1];	/* (0,1,2) */
  char  critical_transformer_load[15];	/* maximum current for line */
  char  critical_transformer_rating[8];
  char  critical_transformer_rating_code[1]; /* (N,T,E,B) */
  char  critical_transformer_loading_terminal[1];	/* (0,1,2) */
  char  total_line_load_pct[15];	/* percent line loading */
  char  total_line_loading[15];	/* total line loading */
  char  total_transformer_load_pct[15];	/* percent line loading */
  char  total_transformer_loading[15];	/* total line loading */
  char  tap1kv_pctcomp[8]; /* tap1 kv for type T or TP */
			/* Percent Compensation for type L or E */
  char  tap2kv[8];	/* tap2 kv for type T or TP */
  char  plot_diff[7];
  char  eol[1];                 /* end of line          */
} pfOutBranch;

typedef union {
  char code[1];
  pfBusOut     bus;
  pfBusACOut   acbus;
  pfBusDCOut   dcbus;
  pfOutBranch  line;
  pfBusXOut    xbus;
} pfOutRecord;
