C    @(#)getval.f	20.10 3/29/99
        real function getval (ind, type, fn_code, error)

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
C       TYPE 3 = Line/Branch Index 
C       TYPE 4 = Area Interchange Index 
C       TYPE 5 = Area Index 
C       TYPE 6 = Zone Index 
C       TYPE 7 = Owner Index
C       TYPE 8 = Transfer Index
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
C        2     IND    VA   Voltage angle in degrees
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
C        2     IND    DCA  D-C firing angle (rectifier) or extinction
C                          delay angle (inverter) in degrees
C        2     IND    DCP  D-C Power (+ for rectifier; - for inverter)
C                          in MWs.
C        2     IND    DCV  D-C Voltage in KV.
C
C        3     IND    TAP1 Tap1 in kV or degrees
C        3     IND    TAP2 Tap2 in kV (or 0 if phase shifter)
C        3     IND    TAP  Actual tap number for LTC transformers
C        3     IND    TAPS Total taps for LTC transformers
C
C        5     IND    PG   P_gen in MW  
C        5     IND    QG   Q_gen in MVAR
C        5     IND    PL   P_load in MW 
C        5     IND    QL   Q_load in MVAR   
C        5     IND    PLS  P_loss in MW 
C        5     IND    QLS  Q_loss in MW 
C        5     IND    PSH  P_shunt in MW
C        5     IND    QSH  Q_shunt in MW
C
C        6     IND    PG   P_gen in MW  
C        6     IND    QG   Q_gen in MVAR
C        6     IND    PL   P_load in MW 
C        6     IND    QL   Q_load in MVAR   
C        6     IND    PLS  P_loss in MW 
C        6     IND    QLS  Q_loss in MW 
C        6     IND    PSH  P_shunt in MW
C        6     IND    QSH  Q_shunt in MW
C        6     IND    SCAP Q_cap scheduled in MVAR
C        6     IND    SREK Q_reactors scheduled in MVAR
C        6     IND    UCAP Q_cap used in MVAR
C        6     IND    UREK Q_reactors used in MVAR
C
C        7     IND    PG   P_gen in MW  
C        7     IND    QG   Q_gen in MVAR
C        7     IND    PL   P_load in MW 
C        7     IND    QL   Q_load in MVAR   
C        7     IND    PLS  P_loss in MW 
C        7     IND    QLS  Q_loss in MW 
C        7     IND    PSH  P_shunt in MW
C        7     IND    QSH  Q_shunt in MW
C        7     IND    QCAP Q_cap scheduled in MVAR
C        7     IND    QREK Q_reactors scheduled in MVAR
C        7     IND    SCAP Q_cap scheduled in MVAR
C        7     IND    SREK Q_reactors scheduled in MVAR
C        7     IND    UCAP Q_cap used in MVAR
C        7     IND    UREK Q_reactors used in MVAR
C
C        8     IND    RKM  Thevenin's Transfer Impedance
C        8     IND    XKM  Thevenin's Transfer Impedance
*----------------------------------------------------------------------*
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/anlys.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/busanl.inc'
      include 'ipfinc/com007.inc'
      include 'ipfinc/coment.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/snput.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
C     
      common /oldval/ oldkt, oldzkk, oldmt
      integer oldkt, oldmt
      complex oldzkk

      real array(30) 
      integer p, q, ptr, qptr, aq, orienttx, shift
      logical found
      complex zkk   
      character rsbtyp  *1
        
        error = 0   
        getval = 0.0

C       If TYPE equals AREA

        if (type .eq. 5) then   

           write (errbuf(1), 10) type   
   10      format ('Unrecognized TYPE has been fixed (', i1, ')')
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
        else if (type .eq. 6) then   
        
           if (fn_code .eq. 'PG') then 
              getval = zsum(1,ind)  
           else if (fn_code .eq. 'QG') then
              getval = zsum(2,ind)  
           else if (fn_code .eq. 'PL') then
              getval = zsum(3,ind)  
           else if (fn_code .eq. 'QL') then
              getval = zsum(4,ind)  
           else if (fn_code .eq. 'PLS') then   
              getval = zsum(5,ind)  
           else if (fn_code .eq. 'QLS') then   
              getval = zsum(6,ind)  
           else if (fn_code .eq. 'PSH') then   
              getval = zsum(7,ind)  
           else if (fn_code .eq. 'QSH') then   
              getval = zsum(8,ind)  
           else if (fn_code .eq. 'QREK' .or. fn_code .eq. 'SREK') then
              getval = zsum(18,ind)  
           else if (fn_code .eq. 'QCAP' .or. fn_code .eq. 'SCAP') then
              getval = zsum(15,ind)  
           else if (fn_code .eq. 'UREK') then   
              getval = zsum(19,ind)  
           else if (fn_code .eq. 'UCAP') then   
              getval = zsum(16,ind)  
           else 
              write (errbuf(1), 30) fn_code
  30          format ('Unrecognized CODE (', a, ') for ZONE symbol')
              call prterx ('W',1)   
              error = 1 
           endif
C       
C       If TYPE equals OWNER 
C       
        else if (type .eq. 7) then   
        
           if (fn_code .eq. 'PG') then 
              getval = syst(1,ind)  
           else if (fn_code .eq. 'QG') then
              getval = syst(2,ind)  
           else if (fn_code .eq. 'PL') then
              getval = syst(3,ind)  
           else if (fn_code .eq. 'QL') then
              getval = syst(4,ind)  
           else if (fn_code .eq. 'PSH') then
              getval = syst(5,ind)  
           else if (fn_code .eq. 'QSH') then
              getval = syst(6,ind)  
           else if (fn_code .eq. 'QREK' .or. fn_code .eq. 'SREK') then
              getval = syst(7,ind)  
           else if (fn_code .eq. 'QCAP' .or. fn_code .eq. 'SCAP') then
              getval = syst(8,ind)  
           else if (fn_code .eq. 'UREK') then   
              getval = syst_used(1,ind)  
           else if (fn_code .eq. 'UCAP') then   
              getval = syst_used(2,ind)  
           else if (fn_code .eq. 'PLS') then   
              getval = syst(9,ind)  
           else if (fn_code .eq. 'QLS') then   
              getval = syst(10,ind)  
           else 
              write (errbuf(1), 40) fn_code
  40          format ('Unrecognized CODE (', a, ') for OWNER symbol')
              call prterx ('W',1)   
              error = 1 
           endif
C       
C       If TYPE equals BUS  
C       
        else if (type .eq. 2) then   
        
           if (ordvlt .eq. 1) then
              kt = ind
           else
              kt = inp2opt(ind)
           endif
           call nrpqv (kt, pk, dpk, qk, dqk, vk) 
           pgen = pnetu(kt) + ploadu(kt)                    
           qgen = qnetu(kt) + qloadu(kt)                    
           call busqua (ind, pgen, qgen, array) 

           if (fn_code .eq. 'V') then  
              getval = dsqrt (e(kt) ** 2 + f(kt) ** 2)   
           else if (fn_code .eq. 'VA') then
              getval = 57.2957795 * atan2 (f(kt), e(kt))
           else if (fn_code .eq. 'VR') then
              getval = e(kt)
           else if (fn_code .eq. 'VI') then
              getval = f(kt)
           else if (fn_code .eq. 'VK') then
              getval = dsqrt (e(kt) ** 2 + f(kt) ** 2) * base(ind)   
           else if (fn_code .eq. 'VM') then
              getval = vlimx(kt)                  
           else if (fn_code .eq. 'VN') then
              getval = vlimn(kt)                  
           else if (fn_code .eq. 'PG') then
              getval = array(1)
           else if (fn_code .eq. 'PL') then
              getval = array(3)
           else if (fn_code .eq. 'PM') then
              getval = array(5)
           else if (fn_code .eq. 'QG') then
              call allocq (ind, qk, qgen, qgnmax, qgnmin, qld, totcap,  
     1                     usecap, totrek, userek, unsked, qerr)
              getval = qgen 
           else if (fn_code .eq. 'QM') then
              getval = array(6)
           else if (fn_code .eq. 'QN') then
              getval = array(8)
           else if (fn_code .eq. 'QL') then
              getval = array(4)
           else if (fn_code .eq. 'S') then 
              call allocq (ind, qk, qgen, qgnmax, qgnmin, qld, totcap,  
     1                     usecap, totrek, userek, unsked, qerr)
              getval = usecap + userek  
           else if (fn_code .eq. 'SM') then 
              call allocq (ind, qk, qgen, qgnmax, qgnmin, qld, totcap,  
     1                     usecap, totrek, userek, unsked, qerr)
              getval = totcap + totrek  
           else if (fn_code .eq. 'C') then 
              call allocq (ind, qk, qgen, qgnmax, qgnmin, qld, totcap,  
     1                     usecap, totrek, userek, unsked, qerr)
              getval = usecap   
           else if (fn_code .eq. 'CM') then
              call allocq (ind, qk, qgen, qgnmax, qgnmin, qld, totcap,  
     1                     usecap, totrek, userek, unsked, qerr)
              getval = totcap   
           else if (fn_code .eq. 'R') then 
              call allocq (ind, qk, qgen, qgnmax, qgnmin, qld, totcap,  
     1                     usecap, totrek, userek, unsked, qerr)
              getval = userek   
           else if (fn_code .eq. 'RM') then
              call allocq (ind, qk, qgen, qgnmax, qgnmin, qld, totcap,  
     1                     usecap, totrek, userek, unsked, qerr)
              getval = totrek   
           else if (fn_code .eq. 'QU') then
              call allocq (ind, qk, qgen, qgnmax, qgnmin, qld, totcap,  
     1                     usecap, totrek, userek, unsked, qerr)
              getval = unsked   
           else if (fn_code .eq. 'DVQ') then   
C       
C             Set options for bus sensitivity dQ/dV:
C       
C             KSPARE(19) = LTC option   
C             KSPARE(20) = Area Interchange option  
C             KSPARE(39) = BQ shunt is adjusable
C             KSPARE(40) = BG Q-generation is adjustable
C       
              kspare(19) = iopton(16)   
              kspare(20) = iopton(17)   
              kspare(39) = 0
              kspare(40) = 0
C       
C             Temporarily change bus type to PQ.
C       
              ktemp = kvolt(kt) 
              kvolt(kt) = 0 
C       
C             Compute bus sensitivity dQ/dV.
C       
              call senfac   
              do i = 1, ntot + ntota 
                 dpt(1,i) = 0.0 
                 dpt(2,i) = 0.0 
              enddo
              dpt(2,kt+ntota) = 1.0 
              call baksen (0)   
        
              if (dpt(2,kt+ntota) .ne. 0.0) then
C       
C                Compute the sensitivity dV/dQ. 
C       
                 vk = dsqrt (e(kt) ** 2 + f(kt) ** 2)
                 getval = vk * dpt(2,kt+ntota) * intbas(kt) / bmva  
              else  
                 getval = 0.0   
              endif 
C       
C             Restore bus type  
C       
              kvolt(kt) = ktemp 

           else if (fn_code .eq. 'DVP') then   
C       
C             Set options for bus sensitivity dP/dV:
C       
C             KSPARE(19) = LTC option   
C             KSPARE(20) = Area Interchange option  
C             KSPARE(39) = BQ shunt is adjusable
C             KSPARE(40) = BG Q-generation is adjustable
C       
              kspare(19) = iopton(16)   
              kspare(20) = iopton(17)   
              kspare(39) = 0
              kspare(40) = 0
C       
C             Temporarily change bus type to PQ.
C       
              ktemp = kvolt(kt) 
              kvolt(kt) = 0 
C       
C             Compute bus sensitivity dP/dV.
C       
              call senfac   
              do i = 1, ntot + ntota 
                 dpt(1,i) = 0.0 
                 dpt(2,i) = 0.0 
              enddo
              dpt(1,kt+ntota) = 1.0 
              call baksen (0)   
        
              if (dpt(2,kt+ntota) .ne. 0.0) then
C       
C                Compute the sensitivity dV/dP. 
C       
                 vk = dsqrt (e(kt) ** 2 + f(kt) ** 2)
                 getval = vk * dpt(2,kt+ntota) * intbas(kt) / bmva  
              else  
                 getval = 0.0   
              endif 
C       
C             Restore bus type  
C       
              kvolt(kt) = ktemp 

           else if (fn_code .eq. 'RKK') then   
C       
C             Compute the driving point impedance Zkk.  
C       
              if (kt .eq. oldkt) then   
                 zkk = oldzkk   
              else  
                 call getzkk (kt, 0, zkk, error)   
                 oldzkk = zkk   
                 oldkt = kt 
              endif 
              getval = real(zkk)
           else if (fn_code .eq. 'XKK') then   
C       
C             Compute the driving point impedance Zkk.  
C       
              if (kt .eq. oldkt) then   
                 zkk = oldzkk   
              else  
                 call getzkk (kt, 0, zkk, error)   
                 oldzkk = zkk   
                 oldkt = kt 
              endif 
              getval = aimag(zkk)   
           else if (fn_code(1:2) .eq. 'DC') then   
              if (orddc .eq. 1) then
                k1 = ind
              else
                k1 = kt
              endif
              if (ntypu(kt) .eq. 5) then
                found = .false.
                i = 1
                do while (i .le. kdtot .and. .not. found)
                  if (dc2t(1,i) .eq. k1 .or. dc2t(3,i) .eq. k1) then
                    found = .true.
                  else
                    i = i + 1
                  endif
                enddo
                if (found) then
                  if (dc2t(1,i) .eq. k1) then
                    if (fn_code .eq. 'DCA') then   
                      getval = 57.2957795 * dc2t(22,i)
                    else if (fn_code .eq. 'DCP') then
                      getval = 0.001 * dc2t(40,i) * dc2t(39,i)
                    else if (fn_code .eq. 'DCV') then
                      getval = dc2t(40,i)
                    endif
                  else
                    if (fn_code .eq. 'DCA') then   
                      getval = 57.2957795 * dc2t(26,i)
                    else if (fn_code .eq. 'DCP') then
                      getval = -0.001 * dc2t(41,i) * dc2t(39,i)
                    else if (fn_code .eq. 'DCV') then
                      getval = dc2t(41,i)
                    endif
                  endif 
                endif
              else if (ntypu(kt) .eq. 12) then
                found = .false.
                i = 1
                do while (i .le. mtdcbs .and. .not. found)
                  if (dcmtbs(1,i) .eq. k1) then
                    found = .true.
                  else
                    i = i + 1
                  endif
                enddo
                if (found) then
                  if (fn_code .eq. 'DCA') then   
                    getval = 57.2957795 * dcmtbs(13,i)
                  else if (fn_code .eq. 'DCP') then
                    getval = dcmtbs(19,i)
                  else if (fn_code .eq. 'DCV') then
                    getval = dcmtbs(20,i)
                  endif
                endif
              else
                write (errbuf(1), 10050) fn_code, bus(ind), base(ind)
10050           format ('Invalid CODE (', a, ') for non-BD/BM bus ',
     &            a8, f7.1)
                call prterx ('W',1)   
                error = 1 
                getval = 0.0
              endif
           else 
              write (errbuf(1), 50) fn_code   
   50         format ('Unrecognized CODE (', a, ') for BUS symbol') 
              call prterx ('W',1)   
              error = 1 
           endif

C       If TYPE equals Line/Branch

        else if (type .eq. 3) then

           q = brnch_ptr(ind)
           if (fn_code .eq. 'TAP1' .and. brtype(ind) .eq. 5) then 
              if (q .gt. 0) then
                 getval = brnch(9,q)
              else 
                 q = -q
                 getval = brnch(10,q)
              endif                 
           else if (fn_code .eq. 'TAP1' .and. brtype(ind) .eq. 6) then 
              if (q .gt. 0) then
                 getval = brnch(9,q)
              else 
                 q = -q
                 getval = -brnch(9,q)
              endif                 
           else if (fn_code .eq. 'TAP2' .and. brtype(ind) .eq. 5) then 
              if (q .gt. 0) then
                 getval = brnch(10,q)
              else 
                 q = -q
                 getval = brnch(9,q)
              endif                 
           else if (fn_code .eq. 'TAP2' .and. brtype(ind) .eq. 6) then
              if (q .gt. 0) then
                 getval = brnch(10,q)
              else 
                 q = -q
                 getval = brnch(10,q)
              endif                 
           else if (fn_code(1:3) .eq. 'TAP') then
c
c             Retrieve pointer to "R" record
c
              ptr = numbrn (kx(ind), ky(ind), '*', 0)
              if (brtype(ptr) .eq. 4) then
                 qptr = brnch_ptr(ptr)
                 aq = iabs(qptr)
                 call getchr (1, rsbtyp, kbrnch(3,aq))
                 tapmax = brnch(6,aq)
                 tapmin = brnch(7,aq)
                 taps = brnch(8,aq)
c
c                Function "orienttx" returns k1x = fixed tap, 
c                kx2 = variable tap.
c
                 krmap = orienttx (ptr, ind, k1x, k2x, tap1, tap2)
                 if (rsbtyp .eq. 'P' .or. rsbtyp .eq. 'M') then
                    if ((krmap .eq. 1 .and. qptr .lt. 0) .or.
     &                  (krmap .eq. 2 .and. qptr. gt. 0)) then
                       temp = tapmax
                       tapmax = -tapmin
                       tapmin = -temp
                    endif
                 endif
                 if (taps .gt. 1.0) then 
                    tdisc = (tapmax - tapmin) / (taps - 1.0) 
                    if (tdisc .gt. 0.0) then  
                       tsteps = (tap2 - tapmin) / tdisc + 1.01
                    else  
                       tsteps = 0.0   
                    endif 
                 else 
                    tsteps = 0.0  
                 endif
                 idisc = tsteps
                 itaps = taps 
              else
                 idisc = 0
                 itaps = 0
              endif
              if (fn_code(1:4) .eq. 'TAPS') then
                 getval = itaps
              else
                 getval = idisc
              endif
           else 
              write (errbuf(1), 60) fn_code   
   60         format ('Unrecognized CODE (', a, ') for BRANCH symbol') 
              call prterx ('W',1)   
              error = 1 
           endif
C       
C       If TYPE equals TRANSFER
C       
        else if (type .eq. 8) then   
        
           k1 = shift(ind, -16)
           k2 = shift(shift(ind, 16), -16)
           if (ordvlt .eq. 1) then
              kt = k1
              mt = k2
           else
              kt = inp2opt(k1)
              mt = inp2opt(k2)
           endif
           if (fn_code .eq. 'RKM') then   
C       
C             Compute the transfer impedance Zkk.  
C       
              if (kt .eq. oldkt .and. mt .eq. oldmt) then   
                 zkk = oldzkk
              else  
                 call getzkk (kt, mt, zkk, error)   
                 oldzkk = zkk   
                 oldkt = kt 
                 oldmt = mt 
              endif 
              getval = real(zkk)
           else if (fn_code .eq. 'XKM') then   
C       
C             Compute the driving point impedance Zkk.  
C       
              if (kt .eq. oldkt .and. mt .eq. oldmt) then   
                 zkk = oldzkk   
              else  
                 call getzkk (kt, mt, zkk, error)   
                 oldzkk = zkk   
                 oldkt = kt 
                 oldmt = mt 
              endif 
              getval = aimag(zkk)   
           else if (fn_code .eq. 'DVQ') then   
C       
C             Set options for bus sensitivity dQ/dV:
C       
C             KSPARE(19) = LTC option   
C             KSPARE(20) = Area Interchange option  
C             KSPARE(39) = BQ shunt is adjusable
C             KSPARE(40) = BG Q-generation is adjustable
C       
              kspare(19) = iopton(16)   
              kspare(20) = iopton(17)   
              kspare(39) = 0
              kspare(40) = 0
C       
C             Temporarily change bus type to PQ.
C       
              ktemp1 = kvolt(kt) 
              kvolt(kt) = 0 
              ktemp2 = kvolt(mt) 
              kvolt(mt) = 0 
C       
C             Compute bus sensitivity dQ/dV.
C       
              call senfac   
              do i = 1, ntot + ntota 
                 dpt(1,i) = 0.0 
                 dpt(2,i) = 0.0 
              enddo
              dpt(2,kt+ntota) = 1.0 
              dpt(2,mt+ntota) = -1.0 
              call baksen (0)   
        
              if (dpt(2,kt+ntota) .ne. 0.0 .and.
     &            dpt(2,mt+ntota) .ne. 0.0) then
C       
C                Compute the sensitivity dV/dQ. 
C       
                 vk = dsqrt (e(kt) ** 2 + f(kt) ** 2)
                 vm = dsqrt (e(mt) ** 2 + f(mt) ** 2)
                 getval = vk * dpt(2,kt+ntota) * intbas(kt) / bmva  
     &                  - vm * dpt(2,mt+ntota) * intbas(mt) / bmva  
              else  
                 getval = 0.0   
              endif 
C       
C             Restore bus type  
C       
              kvolt(kt) = ktemp1
              kvolt(mt) = ktemp2

           else if (fn_code .eq. 'DVP') then   
C       
C             Set options for bus sensitivity dP/dV:
C       
C             KSPARE(19) = LTC option   
C             KSPARE(20) = Area Interchange option  
C             KSPARE(39) = BQ shunt is adjusable
C             KSPARE(40) = BG Q-generation is adjustable
C       
              kspare(19) = iopton(16)   
              kspare(20) = iopton(17)   
              kspare(39) = 0
              kspare(40) = 0
C       
C             Temporarily change bus type to PQ.
C       
              ktemp1 = kvolt(kt) 
              kvolt(kt) = 0 
              ktemp2 = kvolt(mt) 
              kvolt(mt) = 0 
C       
C             Compute bus sensitivity dP/dV.
C       
              call senfac   
              do i = 1, ntot + ntota 
                 dpt(1,i) = 0.0 
                 dpt(2,i) = 0.0 
              enddo
              dpt(1,kt+ntota) = 1.0 
              dpt(1,mt+ntota) = -1.0 
              call baksen (0)   
        
              if (dpt(2,kt+ntota) .ne. 0.0 .and. 
     &            dpt(2,mt+ntota) .ne. 0.0) then
C       
C                Compute the sensitivity dV/dP. 
C       
                 vk = dsqrt (e(kt) ** 2 + f(kt) ** 2)
                 vm = dsqrt (e(mt) ** 2 + f(mt) ** 2)
                 getval = vk * dpt(2,kt+ntota) * intbas(kt) / bmva  
     &                  - vm * dpt(2,mt+ntota) * intbas(mt) / bmva
              else  
                 getval = 0.0   
              endif 
C       
C             Restore bus type  
C       
              kvolt(kt) = ktemp1 
              kvolt(mt) = ktemp2 

           else if (fn_code .eq. 'DTP') then   
C       
C             Set options for bus sensitivity dT/dP:
C       
C             KSPARE(19) = LTC option   
C             KSPARE(20) = Area Interchange option  
C             KSPARE(39) = BQ shunt is adjusable
C             KSPARE(40) = BG Q-generation is adjustable
C       
              kspare(19) = iopton(16)   
              kspare(20) = iopton(17)   
              kspare(39) = 0
              kspare(40) = 0
C       
C             Compute bus sensitivity dP/dT.
C       
              call senfac   
              do i = 1, ntot + ntota 
                 dpt(1,i) = 0.0 
                 dpt(2,i) = 0.0 
              enddo
              dpt(1,kt+ntota) = 1.0 
              dpt(1,mt+ntota) = -1.0 
              call baksen (0)   
        
              if (dpt(1,kt+ntota) .ne. 0.0 .and. 
     &            dpt(1,mt+ntota) .ne. 0.0) then
C       
C                Compute the sensitivity dV/dP. 
C       
                 getval = dpt(1,kt+ntota) * 57.2957795 / bmva  
     &                  - dpt(1,mt+ntota) * 57.2957795 / bmva
              else  
                 getval = 0.0   
              endif 

           else 
              write (errbuf(1), 70) fn_code   
   70         format ('Unrecognized CODE (', a, ') for TRANSFER symbol')
              call prterx ('W',1)   
              error = 1 
           endif
        else
           write (errbuf(1), 110) type  
  110      format ('Unrecognized TYPE (', i1, ')')  
           call prterx ('W',1)  
           error = 1
        endif   
        return  
        end 
