C    @(#)allocq.f	20.6 9/10/96
        subroutine allocq (nb, qk, qgen, qgnmax, qgnmin, qload, totcap,
     1                     usecap, totrek, userek, unsked, qerr)
C
c	May want to change program arguments to double precision.
c		
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

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/changr.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/xdata.inc'
C
      logical capsw
      character kode2 * 1, kodeyr * 2, kownr2 * 3

      if (tbx_loaded .ne. ordtbx) then
         do i = 1, ntot
            ptrtbx(i)  = 0
         enddo
         do i = 1, ntotb
            j = tbx(2,i)
            if (ordtbx .eq. 2) j = opt2inp(i)
            ptrtbx(j) = i
         enddo
         tbx_loaded = ordtbx
      endif
                
      if (ordvlt .eq. 1) then
         kt = nb
      else 
         kt = inp2opt(nb)
      endif
      jtbx = ptrtbx(nb)
      vk = dsqrt( e(kt) ** 2 + f(kt) ** 2 )
      vksq = vk ** 2
      qkmva = qk * bmva
C
C     Obtain total susceptance.  Note: BX bus type store initial
C     value on B record.  The total is in array XDATA.
C
      ktyp = kbsdta(1,nb)
C
      if (ktyp .eq. 11) then
C
         if (jtbx .gt. 0) then
            nt = tbx(5,jtbx)
            totrek = xdata(3,nt) * vksq 
            totcap = xdata(4,nt) * vksq 
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
         qerr   = 0.0
         go to 1280
      else
         sksusp = busdta(6,nb) * vksq
         totcap = amax1 (sksusp, 0.0)
         totrek = amin1 (sksusp, 0.0)
      endif
 
      qload = busdta(4,nb)
      if (ktyp .eq. 1 .or. ktyp .eq. 4 .or. ktyp .eq. 10) then
         if (busdta(10,nb) .lt. 0.0) then
            qgnmax = 0.0
            qgnmin = 0.0
         else
            qgnmax = busdta(9,nb)
            qgnmin = busdta(9,nb)
         endif
      else
         qgnmax = busdta(9,nb)
         qgnmin = busdta(10,nb)
      endif
      bfixed = 0.0
      ncb = kbsdta(15,nb)
      do while (ncb .gt. 0) 
         qload2 = bctbl(3,ncb)
         sksus2 = bctbl(5,ncb) * vksq
         if (ktyp .eq. 1 .or. ktyp .eq. 4 .or. ktyp .eq. 10) then
            if (bctbl(12,ncb) .lt. 0.0) then
            else
               qgnmax = qgnmax + bctbl(11,ncb)
               qgnmin = qgnmin + bctbl(11,ncb)
            endif
         else
            qgnmax = qgnmax + bctbl(11,ncb)
            qgnmin = qgnmin + bctbl(12,ncb)
         endif
         call getchr(1,kode2,kbctbl(8,ncb))
         call getchr(2,kodeyr,kbctbl(9,ncb))
         call getchr(3,kownr2,kbctbl(10,ncb))
         if (kode2 .eq. 'A') then
            if (kodeyr .eq. '01' .or. kodeyr .eq. '02') then
               bfixed = bfixed + sksus2
               sksus2 = 0.0
            else
               bfixed = bfixed + sksus2
               sksus2 = 0.0
            endif
            if (kodeyr .eq. '01') then
               qload2 = qload2 * sqrt(vksq)
            endif
         else if (kodeyr .eq. '*P') then
            bfixed = bfixed + sksus2
            sksus2 = 0.0
         else if (kodeyr .eq. '*I') then
            bfixed = bfixed + sksus2
            sksus2 = 0.0
            qload2 = qload2 * sqrt(vksq)
         endif
         qload = qload + qload2
         totcap = totcap + amax1 (sksus2, 0.0)
         totrek = totrek + amin1 (sksus2, 0.0)
         ncb = bctbl_nxt(ncb)
      enddo

      totcap = totcap + amax1( bfixed, 0.0 )
      totrek = totrek + amin1( bfixed, 0.0 )
      qgen = qkmva + qload + ineti(kt) * vk * bmva
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
 
            if (jtbx .gt. 0) then
               ltyp = tbx(1,jtbx)
               ityp = tbx(7,jtbx)
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
            if (jtbx .gt. 0) then
               nt = tbx(5,jtbx)
               userek = xdata(5,nt) * vksq + amin1 (0.0, bfixed)
               usecap = xdata(6,nt) * vksq + amax1 (0.0, bfixed)
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
