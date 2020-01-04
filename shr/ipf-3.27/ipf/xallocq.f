C    @(#)xallocq.f	20.4 2/13/96
C****************************************************************
C
C   File: xallocq.f
C   Purpose: Routine to obtain reactive allocation based upon the
c            final solution values and the state of the bus, using
C            the data from alternate base case.  
C
C   Author: Walt Powell  Date: 26 July 1993
C                        Modified: 
C   Called by:
C
C****************************************************************
        subroutine xallocq (nb, qk, qgen, qgnmax, qgnmin, qload, totcap,
     1                     usecap, totrek, userek, unsked, qerr)
C
C       This subroutine allocates Q to load, generation, shunt,
C       unscheduled, and Q_error according to Qk.
C
C     Input paramters:
C
C     NB     - external bus number.
C     QK     - computed net reactive injection.
C
C     Output paramters:
C
C     QGEN   - reactive generation in MVAR.
C     QLOAD  - reactive load in MVAR.
C     QGNMAX - maximum reactive generation in MVAR.
C     QGNMIN - minimum reactive generation in MVAR.
C     TOTCAP - total capacitive shunt in MVAR.
C     TOTREK - total reactive shunt in MVAR.
C     USECAP - used capacitive shunt in MVAR.
C     USEREK - used capacitive shunt in MVAR.
C     UNSKED - reactive shunt unscheduled in MVAR.
C     QERR   - reactive shunt mismatched in MVAR.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/alt_case.inc'
      include 'ipfinc/alt_flag.inc'

      common /scratch/ scratch(22*MAXBUS)
      integer ptrtbx(MAXBUS)
      equivalence (ptrtbx, scratch(12*MAXBUS+1))
C
c***      logical xallocq_loaded
      logical capsw
      character kode2 * 1, kodeyr * 2, kownr2 * 3

      if (.not. tbx_flag) then
         do i = 1, ontot
            ptrtbx(i)  = 0
         enddo
         do jt = 1, ontotb
            j = oltbx(2,jt)
            ptrtbx(j) = jt
         enddo
         tbx_flag = .true.
      endif                
 
      kt = oinp2opt(nb)
      qkmva = qk * bmva
      vksq = olde(kt) ** 2 + oldf(kt) ** 2
      vk = sqrt (vksq)
C
C     Obtain total susceptance.  Note: BX bus type store initial
C     value on B record.  The total is in array XDATA.
C
      ktyp = okbsdta(1,nb)
C
      if (ktyp .eq. 11) then
C
         jtbx = ptrtbx(nb)
         if (jtbx .gt. 0) then
            nt = oltbx(5,jtbx)
            totrek = oxdata(3,nt) * vksq
            totcap = oxdata(4,nt) * vksq
         else
            call erexit
         endif
      else if (ktyp .eq. 5 .or. ktyp .eq. 12) then
         qgen = qkmva 
         qload  = 0.0
         qgnmax = qgen
         qgnmin = qgen
         totcap = 0.0
         totrek = 0.0
         usecap = 0.0
         userek = 0.0
         unsked = 0.0
         go to 1280
      else
         sksusp = obusdta(6,nb) * vksq
         totcap = amax1 (sksusp, 0.0)
         totrek = amin1 (sksusp, 0.0)
      endif
 
      qload = obusdta(4,nb)
      if (ktyp .eq. 1 .or. ktyp .eq. 4 .or. ktyp .eq. 10) then
         if (obusdta(10,nb) .lt. 0.0) then
            qgnmax = 0.0
            qgnmin = 0.0
         else
            qgnmax = obusdta(9,nb)
            qgnmin = obusdta(9,nb)
         endif
      else
         qgnmax = obusdta(9,nb)
         qgnmin = obusdta(10,nb)
      endif
      bfixed = 0.0
      ncb = okbsdta(15,nb)
      do while (ncb .gt. 0) 
         qload2 = obctbl(3,ncb)
         sksus2 = obctbl(5,ncb) * vksq
         if (ktyp .eq. 1 .or. ktyp .eq. 4 .or. ktyp .eq. 10) then
            if (obctbl(12,ncb) .lt. 0.0) then
            else
               qgnmax = qgnmax + obctbl(11,ncb)
               qgnmin = qgnmin + obctbl(11,ncb)
            endif
         else
            qgnmax = qgnmax + obctbl(11,ncb)
            qgnmin = qgnmin + obctbl(12,ncb)
         endif
         call getchr(1,kode2,okbctbl(8,ncb))
         call getchr(2,kodeyr,okbctbl(9,ncb))
         call getchr(3,kownr2,okbctbl(10,ncb))
         if (kode2 .eq. 'A') then
            if (kodeyr .eq. '01' .or. kodeyr .eq. '02') then
               bfixed = bfixed + sksus2
               sksus2 = 0.0
            else
               bfixed = bfixed + sksus2
               sksus2 = 0.0
            endif
            if (kodeyr .eq. '01') then

C              This load is constant current. It appears on the
C              RHS in the Qk equation. Transfer it to the LHS.

               qload2 = qload2 * sqrt (vksq)
               qkmva = qkmva - qload2
            endif
         else if (kodeyr .eq. '*P') then
            bfixed = bfixed + sksus2
            sksus2 = 0.0
         else if (kodeyr .eq. '*I') then
            bfixed = bfixed + sksus2
            sksus2 = 0.0

C           This load is constant current. It appears on the
C           RHS in the Qk equation. Transfer it to the LHS.

            qload2 = qload2 * sqrt (vksq)
            qkmva = qkmva - qload2
         endif
         qload = qload + qload2
         totcap = totcap + amax1 (sksus2, 0.0)
         totrek = totrek + amin1 (sksus2, 0.0)
         ncb = obctbl_nxt(ncb)
      enddo

      totcap = totcap + amax1( bfixed, 0.0 )
      totrek = totrek + amin1( bfixed, 0.0 )
      qgen = qkmva + qload + oineti(kt) * vk * bmva
C
C     Remove switched shunt from BQ busses.
C
C     "CAPSW" = .TRUE.  - shunt susceptance is fixed.
C               .FALSE. - shunt susceptance is adjustable.
C
      usecap = totcap
      userek = totrek
      capsw = .true.
      if (ktyp .eq. 1 .or. ktyp .eq. 4 .or. ktyp .eq. 10) then
      else if (usecap .gt. 0.0 .or. userek .lt. 0.0) then
 
         if (ktyp .eq. 7) then
 
            jtbx = ptrtbx(nb)
            if (jtbx .gt. 0) then
               ltyp = oltbx(1,jtbx)
               ityp = oltbx(7,jtbx)
               if (ityp .eq. 3) then
C
C                 Q_min limit hit. Remove all capacitors.
C
                  usecap = amax1( 0.0, bfixed )
C
               else if (ityp .eq. 4) then
C
C                 Q_max limit hit. Remove all reactors.
C
                  userek = amin1( 0.0, bfixed )
C
               else
                  capsw = .false.
               endif
            else
               call erexit
            endif
C
  130       continue
C
C       "BG" shunt is held constant. CAPSW does not need to be set
C        to .TRUE., which implies fixed Qshunt, since .TRUE. is its
C        default value.
C
         else if (ktyp .eq. 11) then
C
C           "BX" shunt is explicitly defined.
C
            jtbx = ptrtbx(nb)
            if (jtbx .gt. 0) then
               nt = oltbx(5,jtbx)
               userek = oxdata(5,nt) * vksq + amin1 (0.0, bfixed)
               usecap = oxdata(6,nt) * vksq + amax1 (0.0, bfixed)
            else
               call erexit
            endif
C
  150       continue
 
         else if (ktyp .eq. 2 .or. ktyp .eq. 3) then
C
C           "BE" and "BS" have allocatable shunt also.
C
            capsw = .false.
         endif
      endif
C
C     Remove "ficticious generation" from BQ, BE, and BS busses.
C     This ficticious generation compensates any unused shunt.
C
      if (.not. capsw) then
         xusecp = usecap - amax1 (0.0, bfixed)
         xuserk = userek - amin1 (0.0, bfixed)
         if (qgen * xusecp .lt. 0.0) then
            if (abs(qgen) .le. abs(xusecp) + 0.5) then
               usecap = usecap + qgen
               qkmva = qkmva - qgen
               qgen = 0.0
            else
               qgen = qgen + xusecp
               qkmva = qkmva + xusecp
               usecap = usecap - xusecp
            endif
         else if (qgen * xuserk .lt. 0.0) then
            if ( abs(qgen) .le. abs(xuserk) + 0.5 ) then
               userek = userek + qgen
               qkmva = qkmva - qgen
               qgen = 0.0
            else
               qgen = qgen + xuserk
               qkmva = qkmva + xuserk
               userek = userek - xuserk
            endif
         endif
      endif
 
 1280 qgen = qgen - dim( qgen, qgnmax ) + dim( qgnmin, qgen )
 
      if ( dim( qkmva, qgnmax - qload ) .gt. 0.5 ) then
         unsked = dim( qkmva, qgnmax - qload )
      else if ( dim ( qgnmin - qload, qkmva ) .gt. 0.5 ) then
         unsked = -dim( qgnmin - qload, qkmva )
      else
         unsked = 0.0
      end if
      qerr = 0.0
 
      return
      end
