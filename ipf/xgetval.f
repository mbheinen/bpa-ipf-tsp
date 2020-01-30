C    @(#)xgetval.f	20.6 1/7/99
        real function xgetval (ind, type, fn_code, error)

        integer type, error
        character * (*) fn_code
C       
C       This function optains bus, branch, intertie, and area   
C       interchange data for specified items.   
C       
C       Dictionary of index, types, and codes:  
C       
C       TYPE 1 = System Index   
C       TYPE 2 = Bus Index  
C       TYPE 3 = Line/branch Index 
C       TYPE 4 = Area Interchange Index 
C       TYPE 5 = Area Index 
C       TYPE 6 = Zone Index 
C       TYPE 7 = Owner Index
C       
C       TYPE  INDEX  CODE  Quantity 
C       
C        1     -      B    Base MVA 

C        2     IND    PL   P_load in MW 
C        2     IND    QL   Q_load in MVAR   
C        2     IND    PG   P_gen in MW  
C        2     IND    PM   P_max in MW  
C        2     IND    QG   Q_gen in MVAR
C        2     IND    QM   Q_max in MVAR
C        2     IND    QN   Q_min in MVAR
C        2     IND    V    V in per unit
C        2     IND    VR   V in per unit, real component
C        2     IND    VI   V in per unit, imaginary component   
C        2     IND    VK   V in kV  
C        2     IND    VM   V_max in per unit
C        2     IND    VN   V_min in per unit
C        2     IND    C    Q_caps used in MVAR  
C        2     IND    CM   Q_caps scheduled in MVAR 
C        2     IND    R    Q_reactors used in MVAR  
C        2     IND    RM   Q_reactors scheduled in MVAR 
C        2     IND    QU   Q_unscheduled in MVAR
C        2     IND    DVQ  dV/dQ in kV/MVAR 
C        2     IND    DVP  dV/dP in kV/MW 
C        2     IND    RKK  Thevenin's driving point impedance   
C        2     IND    XKK  Thevenin's driving point impedance   

C        3     IND    TAP1 Tap1 in kV or degrees
C        3     IND    TAP2 Tap2 in kV (or 0 if phase shifter)

C        5     IND    PG   P_gen in MW  
C        5     IND    QG   Q_gen in MVAR
C        5     IND    PL   P_load in MW 
C        5     IND    QL   Q_load in MVAR   
C        5     IND    PLS  P_loss in MW 
C        5     IND    QLS  Q_loss in MW 
C        5     IND    PSH  P_shunt in MW
C        5     IND    QSH  Q_shunt in MW
C        6     IND    PG   P_gen in MW  
C        6     IND    QG   Q_gen in MVAR
C        6     IND    PL   P_load in MW 
C        6     IND    QL   Q_load in MVAR   
C        6     IND    PLS  P_loss in MW 
C        6     IND    QLS  Q_loss in MW 
C        6     IND    PSH  P_shunt in MW
C        6     IND    QSH  Q_shunt in MW

C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
c	Global variables used:
c		None
        include 'ipfinc/alt_case.inc'
c	Global variables used:
c		oinp2opt, ozsum, obrnch, obrtype, obrnch_ptr,
c		opnetu, oqnetu, oploadu, oqloadu, olde, oldf
        include 'ipfinc/com007.inc'
c	Global variables used:
c		None
        include 'ipfinc/coment.inc'
c	Global variables used:
c		None
        include 'ipfinc/lfiles.inc'
c	Global variables used:
c		None
        include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf
C     
        real array(30) 
        integer q
        
        error = 0   
        xgetval = 0.0

C       If TYPE equals AREA

        if (type .eq. 5) then   
           write (errbuf(1), 10) type   
   10      format ('Unrecognized TYPE has been fixed(', i1, ')')
           call prterx ('W',1)  
           if (fn_code .eq. 'PG') then 
           else if (fn_code .eq. 'QG') then
           else if (fn_code .eq. 'PL') then
           else if (fn_code .eq. 'QL') then
           else if (fn_code .eq. 'PLS') then   
           else if (fn_code .eq. 'QLS') then   
           else if (fn_code .eq. 'PSH') then   
           else if (fn_code .eq. 'QSH') then   
           else 
              write (errbuf(1), 20) fn_code
  20          format ('Unrecognized CODE (', a, ') for AREA symbol')
              call prterx ('W',1)   
              error = 1 
           endif
C       
C       If TYPE equals ZONE 
C       
        elseif (type .eq. 6) then   
        
           if (fn_code .eq. 'PG') then 
              xgetval = ozsum(1,ind)  
           else if (fn_code .eq. 'QG') then
              xgetval = ozsum(2,ind)  
           else if (fn_code .eq. 'PL') then
              xgetval = ozsum(3,ind)  
           else if (fn_code .eq. 'QL') then
              xgetval = ozsum(4,ind)  
           else if (fn_code .eq. 'PLS') then   
              xgetval = ozsum(5,ind)  
           else if (fn_code .eq. 'QLS') then   
              xgetval = ozsum(6,ind)  
           else if (fn_code .eq. 'PSH') then   
              xgetval = ozsum(7,ind)  
           else if (fn_code .eq. 'QSH') then   
              xgetval = ozsum(8,ind)  
           else 
              write (errbuf(1), 30) fn_code
  30          format ('Unrecognized CODE (', a, ') for ZONE symbol')
              call prterx ('W',1)   
              error = 1 
           endif
C       
C       If TYPE equals BUS  
C       
        elseif (type .eq. 2) then   
        
           kt = oinp2opt(ind)  
           pgen = opnetu(kt) + oploadu(kt)                    
           qgen = oqnetu(kt) + oqloadu(kt)                    
           call xbusqua (ind, pgen, qgen, array) 

           if (fn_code .eq. 'V') then  
              xgetval = sqrt (olde(kt) ** 2 + oldf(kt) ** 2)   
           else if (fn_code .eq. 'VR') then
              xgetval = olde(kt)
           else if (fn_code .eq. 'VI') then
              xgetval = oldf(kt)
           else if (fn_code .eq. 'VK') then
              vmag = sqrt (olde(kt) ** 2 + oldf(kt) ** 2)
              xgetval = vmag * oldbase(ind)   
           else if (fn_code .eq. 'PG') then
              xgetval = array(1)
           else if (fn_code .eq. 'PL') then
              xgetval = array(3)
           else if (fn_code .eq. 'PM') then
              xgetval = array(5)
           else if (fn_code .eq. 'QG') then
              call xallocq (ind, qk, qgen, qgnmax, qgnmin, qld, totcap,
     1                     usecap, totrek, userek, unsked, qerr)
              xgetval = qgen 
           else if (fn_code .eq. 'QM') then
              xgetval = array(6)
           else if (fn_code .eq. 'QN') then
              xgetval = array(8)
           else if (fn_code .eq. 'QL') then
              xgetval = array(4)
           else if (fn_code .eq. 'S') then 
              call xallocq (ind, qk, qgen, qgnmax, qgnmin, qld, totcap, 
     1                     usecap, totrek, userek, unsked, qerr)
              xgetval = usecap + userek  
           else if (fn_code .eq. 'SM') then 
              call xallocq (ind, qk, qgen, qgnmax, qgnmin, qld, totcap,
     1                     usecap, totrek, userek, unsked, qerr)
              xgetval = totcap + totrek  
           else if (fn_code .eq. 'C') then 
              call xallocq (ind, qk, qgen, qgnmax, qgnmin, qld, totcap,
     1                     usecap, totrek, userek, unsked, qerr)
              xgetval = usecap   
           else if (fn_code .eq. 'CM') then
              call xallocq (ind, qk, qgen, qgnmax, qgnmin, qld, totcap, 
     1                     usecap, totrek, userek, unsked, qerr)
              xgetval = totcap   
           else if (fn_code .eq. 'R') then 
              call xallocq (ind, qk, qgen, qgnmax, qgnmin, qld, totcap, 
     1                     usecap, totrek, userek, unsked, qerr)
              xgetval = userek   
           else if (fn_code .eq. 'RM') then
              call xallocq (ind, qk, qgen, qgnmax, qgnmin, qld, totcap, 
     1                     usecap, totrek, userek, unsked, qerr)
              xgetval = totrek   
           else if (fn_code .eq. 'QU') then
              call xallocq (ind, qk, qgen, qgnmax, qgnmin, qld, totcap, 
     1                     usecap, totrek, userek, unsked, qerr)
              xgetval = unsked   
           else 
              write (errbuf(1), 100) fn_code   
  100         format ('Unrecognized CODE (', a, ') for BUS symbol') 
              call prterx ('W',1)   
              error = 1 
           endif

C       If TYPE equals Line or Branch

        else if (type .eq. 3) then

           q = obrnch_ptr(ind)
           if (fn_code .eq. 'TAP1' .and. obrtype(ind) .eq. 5) then 
              if (q .gt. 0) then
                 xgetval = obrnch(9,q)
              else 
                 q = -q
                 xgetval = obrnch(10,q)
              endif                 
           else if (fn_code .eq. 'TAP1' .and. obrtype(ind) .eq. 6) then 
              if (q .gt. 0) then
                 xgetval = obrnch(9,q)
              else 
                 q = -q
                 xgetval = -obrnch(9,q)
              endif                 
           else if (fn_code .eq. 'TAP2' .and. obrtype(ind) .eq. 5) then 
              if (q .gt. 0) then
                 xgetval = obrnch(10,q)
              else 
                 q = -q
                 xgetval = obrnch(9,q)
              endif                 
           else
              if (q .gt. 0) then
                 xgetval = obrnch(10,q)
              else 
                 q = -q
                 xgetval = obrnch(10,q)
              endif                 
           endif

        else
           write (errbuf(1), 110) type  
  110      format ('Unrecognized TYPE (', i1, ')')  
           call prterx ('W',1)  
           error = 1
        endif   
        return  
        end 
