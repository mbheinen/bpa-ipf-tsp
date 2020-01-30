
c     Set up common block for storage of sorted items:
c     a scalar for type of sort and vectors for 
c     ovtx_table_pointer, tfrmr_branch_pointer,
c     txbus_1_tap, bus1_solved_volt, bus1_solve/tap_ratio, 
c     paper_list_flag, and fiche_list_flag. 

      common /overex/
     &       jsort,                    ! sort type switch
     &       iotxptr(2*MAXOLT),        ! ptr to this data (jsort order)
     &       itxbdptr(2*MAXOLT),       ! ptr to the tx branch data
     &       txtap1(2*MAXOLT),         ! tx tap1 setting
     &       bus1pu(2*MAXOLT),         ! solved voltage at bus1 (pu)
     &       txvratio(2*MAXOLT),       ! tx voltage/tap ratio
     &       txmvaflo(2*MAXOLT),       ! tx mvaflow max
     &       itxrptr(2*MAXOLT)         ! ptr to the Reg data if any

      common /overex_c/
     &       txpaperl(2*MAXOLT),       ! paper list option
     &       txfichel(2*MAXOLT)        ! fiche list option
      character txpaperl * 1, txfichel * 1

