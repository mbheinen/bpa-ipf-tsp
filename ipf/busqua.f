C    @(#)busqua.f	20.5 7/29/96
      subroutine busqua (nb, pk, qk, array)
      dimension array(*)
C
C     This subroutine determines the bus quantities corresponding to
C     load, generation, shunt, unscheduled, and error according to Pk
C     and Qk.
C
C     Input paramters:
C
C     NB     - external bus number.
C     PK     - computed active generation.
C     QK     - computed reactive generation (includes ficticious
C              generation from adjustable shunts).
C
C     Output paramters:
C
C     ARRAY(1): PGEN   - active generation in MW.
C          (2): QGEN   - reactive generation in MVAR.
C          (3): PLOAD  - active load in MW.
C          (4): QLOAD  - reactive load in MVAR.
C          (5): PGNMAX - maximum active generation in MW.
C          (6): QGNMAX - maximum reactive generation in MVAR.
C          (7): PGNMIN - minimum active generation in MW.
C          (8): QGNMIN - minimum reactive generation in MVAR.
C          (9): SKCOND - total active shunt in MW.
C         (10): TOTCAP - total capacitive shunt in MVAR.
C         (11): (not used -- negative conductance has no significance.)
C         (12): TOTREK - total reactive shunt in MVAR.
C         (13): SKCOND - used active shunt in MW.
C         (14): USECAP - used capacitive shunt in MVAR.
C         (15): (not used -- negative conductance has no significance.)
C         (16): USEREK - used capacitive shunt in MVAR.
C         (17): PVLV - d-c converter losses in MWs.
C         (18): QVLV - d-c converter losses in MVARs.
C         (19): GEQUIV - equivalent shunt impedance in MWs.
C         (20): BEQUIV - equivalent shunt impedance in MVARs.
C         (21): (not used -- there is no unscheduled conductance.)
C         (22): UNSKED - reactive shunt unscheduled in MVAR.
C         (23): PERR   - active shunt mismatched in MW.
C         (24): QERR   - reactive shunt mismatched in MVAR.
C         (25): PLOAD  - Constant MVA load in MWs.
C         (26): QLOAD  - Constant MVA load in MVARs.
C         (27): PLOAD  - Constant current load in MWs.
C         (28): QLOAD  - Constant current load in MVARs.
C         (29): PLOAD  - Constant impedance load in MWs.
C         (30): QLOAD  - Constant impedance load in MVARs.
C
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/lodtyp.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/xdata.inc'
C
C     Local Variables
C 
      integer j1, j2, pointr
      logical capsw
      character kode2 * 1, kodeyr * 2, kowner * 3
 
      if (tbx_loaded .ne. ordtbx) then
         do i = 1, ntot
            ptrtbx(i)  = 0
         enddo
         do jt = 1, ntotb
            nb = tbx(2,jt)
            if (ordtbx .eq. 2) nb = opt2inp(nb)
            ptrtbx(nb) = jt
         enddo
         tbx_loaded = ordtbx
      endif                
C
C     Initialize composite load quantities
C
      do i = 1, 6
         tottyp(i) = 0.0
      enddo
      gequiv = 0.0
      bequiv = 0.0
      bfixed = 0.0

      ktyp = kbsdta(1,nb)
      if (ktyp .eq. 5) then
C
C        Compute bus quantities for type BD bus.
C
         do 90 i = 1, 26
   90    array(i) = 0.0
         do 92 idc = 1,kdtot
            if (dc2t(1,idc) .eq. nb) then
               pdc=real(dc2t(42,idc))
               qdc=real(dc2t(44,idc))
               vdc=real(dc2t(40,idc))
               pvlv=abs(pdc-0.001*real(dc2t(39,idc))*real(dc2t(40,idc)))
               qvlv=qdc
               adc=real(dc2t(39,idc))
               array(17) = pvlv
               array(18) = qvlv
               go to 94
            else if (dc2t(3,idc) .eq. nb) then
               pdc=real(dc2t(43,idc))
               qdc=real(dc2t(45,idc))
               vdc=real(dc2t(41,idc))
               pvlv=abs(pdc+0.001*real(dc2t(39,idc))*real(dc2t(41,idc)))
               qvlv=qdc
               adc=-real(dc2t(39,idc))
               array(17) = pvlv
               array(18) = qvlv
               go to 94
            else
            endif
   92    continue
         call erexit
   94    continue
         pgen = 0.0
         qgen = 0.0
         pload = 0.0
         qload = 0.0
         pgnmax = 0.0
         qgnmax = 0.0
         pgnmin = 0.0
         qgnmin = 0.0
         skcond = 0.0
         sksusp = 0.0
         totcap = 0.0
         totrek = 0.0
 
      else if (ktyp .eq. 12) then
C
C        Compute bus quantities for type BM bus.
C
         do 96 idc = 1,mtdcbs
            if (dcmtbs(1,idc) .eq. nb) then
               pdc = real(dcmtbs(25,idc))
               qdc = real(dcmtbs(26,idc))
               vdc = real(dcmtbs(20,idc))
               pvlv = abs(pdc - real(dcmtbs(19,idc)))
               qvlv = qdc
               if (dcmtbs(20,idc) .eq. 0.0) then
                  adc = 0.0
               else
                  adc = 1000.0 * real(dcmtbs(19,idc)) / 
     &                  real(dcmtbs(20,idc))
               endif
               array(17) = pvlv
               array(18) = qvlv
               go to 98
            endif
   96    continue
         call erexit
   98    continue
         pgen = 0.0
         qgen = 0.0
         pload = 0.0
         qload = 0.0
         pgnmax = 0.0
         qgnmax = 0.0
         pgnmin = 0.0
         qgnmin = 0.0
         skcond = 0.0
         sksusp = 0.0
         totcap = 0.0
         totrek = 0.0
 
      else
         array(17) = 0.0
         array(18) = 0.0
         pgnmax = busdta(7,nb)
         pgnmin = 0.0
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
C
C        Compute bus quantities for all other type buses.
C
         kt = inp2opt(nb)
         pgen = pk * bmva
         qgen = qk * bmva
         vk = real( dsqrt(e(kt) ** 2 + f(kt) ** 2) )
         pload = busdta(3,nb)
         qload = busdta(4,nb)
         jtbx = ptrtbx(nb)
C
C        Obtain total susceptance.  Note: BX bus type store initial
C        value on B record.  The total is in array XDATA.
C
         if (ktyp .eq. 11) then
            if (jtbx .gt. 0) then
               nt = tbx(5,jtbx)
               totrek = xdata(3,nt) * vk ** 2
               totcap = xdata(4,nt) * vk ** 2
            else
               call erexit
            endif
         else
            skcond = busdta(5,nb) * vk ** 2
            sksusp = busdta(6,nb) * vk ** 2
            totcap = amax1 (sksusp, 0.0)
            totrek = amin1 (sksusp, 0.0)
C
         endif
 
      endif
   
      tottyp(1) = pload
      tottyp(2) = qload
 
      ncb = kbsdta(15,nb)
      do while (ncb .gt. 0)
         pload2 = bctbl(2,ncb)
         qload2 = bctbl(3,ncb)
         skcon2 = bctbl(4,ncb) * vk ** 2
         sksus2 = bctbl(5,ncb) * vk ** 2
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
         call getchr(3,kowner,kbctbl(10,ncb))
C
C        Convert constant current and constant admittance
C        loads back to constant MVA.
C
         do 220 i = 1, 6
  220    lodtyp(i) = 0.0
         if (kode2 .eq. 'A') then
            if (kodeyr .eq. '01' .or. kodeyr .eq. '*I') then
               pload2 = pload2 * vk
               qload2 = qload2 * vk
               lodtyp(3) = pload2
               lodtyp(4) = qload2
               if (kowner .eq. '***') then
                  gequiv = gequiv + skcon2
                  bequiv = bequiv + sksus2
                  skcon2 = 0.0
                  sksus2 = 0.0
               else
                  lodtyp(5) = skcon2
                  lodtyp(6) = -sksus2
                  pload2 = pload2 + skcon2
                  qload2 = qload2 - sksus2
                  skcon2 = 0.0
                  sksus2 = 0.0
               endif
            else if (kodeyr .eq. '02' .or. kodeyr .eq. '*P') then
               lodtyp(1) = pload2
               lodtyp(2) = qload2
               if (kowner .eq. '***') then
                  gequiv = gequiv + skcon2
                  bequiv = bequiv + sksus2
                  skcon2 = 0.0
                  sksus2 = 0.0
               else
                  lodtyp(5) = skcon2
                  lodtyp(6) = -sksus2
                  pload2 = pload2 + skcon2
                  qload2 = qload2 - sksus2
                  skcon2 = 0.0
                  sksus2 = 0.0
               endif
            else
               lodtyp(1) = pload2
               lodtyp(2) = qload2
               bfixed = bfixed + sksus2
               sksus2 = 0.0
            endif
         else if (kodeyr .eq. '*I') then
            pload2 = pload2*vk
            qload2 = qload2*vk
            lodtyp(3) = pload2
            lodtyp(4) = qload2
            if (kowner .eq. '***') then
               gequiv = gequiv + skcon2
               bequiv = bequiv + sksus2
               skcon2 = 0.0
               sksus2 = 0.0
            else
               lodtyp(5) = skcon2
               lodtyp(6) = -sksus2
               pload2 = pload2 + skcon2
               qload2 = qload2 - sksus2
               skcon2 = 0.0
               sksus2 = 0.0
            endif
         else if (kodeyr .eq. '*P') then
            lodtyp(1) = pload2
            lodtyp(2) = qload2
            if (kowner .eq. '***') then
               gequiv = gequiv + skcon2
               bequiv = bequiv + sksus2
               skcon2 = 0.0
               sksus2 = 0.0
            else
               lodtyp(5) = skcon2
               lodtyp(6) = -sksus2
               pload2 = pload2 + skcon2
               qload2 = qload2 - sksus2
               skcon2 = 0.0
               sksus2 = 0.0
            endif
         else
            lodtyp(1) = pload2
            lodtyp(2) = qload2
         endif
         pload = pload + pload2
         qload = qload + qload2
         skcond = skcond + skcon2
         sksusp = sksusp + sksus2
         totcap = totcap + amax1 (sksus2, 0.0)
         totrek = totrek + amin1 (sksus2, 0.0)
         do 99 i = 1, 6
   99    tottyp(i) = tottyp(i) + lodtyp(i)
         ncb = bctbl_nxt(ncb)
      enddo
      totcap = totcap + amax1( bfixed, 0.0 )
      totrek = totrek + amin1( bfixed, 0.0 )
C
C     Remove switched shunt from BQ busses.
C
C     "CAPSW" = .TRUE./.FALSE.
C                  (shunt susceptance is fixed/adjustable)
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
C           "BG" shunt is held constant. CAPSW does not need to be 
C           set to .TRUE., which implies fixed Qshunt, since .TRUE. 
C           is its default value.
C
         else if (ktyp .eq. 11) then
C
C           "BX" shunt is explicitly defined.
C
            if (jtbx .gt. 0) then
               nt = tbx(5,jtbx)
               userek = xdata(5,nt) * vk ** 2 + amin1 (0.0, bfixed)
               usecap = xdata(6,nt) * vk ** 2 + amax1 (0.0, bfixed)
            else
               call erexit
            endif
 
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
         if (qgen * usecap .lt. 0.0) then
            if (abs(qgen) .le. abs(usecap) + 0.5) then
               usecap = usecap + qgen
               qgen = 0.0
            else
               qgen = qgen + usecap
               usecap = 0.0
            endif
         else if (qgen * userek .lt. 0.0) then
            if ( abs( qgen ) .le. abs( userek ) + 0.5 ) then
               userek = userek + qgen
               qgen = 0.0
            else
               qgen = qgen + userek
               userek = 0.0
            endif
         endif
      endif
 
 1280 unsked = dim (qgen, qgnmax) - dim(qgnmin, qgen)
      qgen = qgen - unsked
      if (abs(unsked) .lt. 0.5) unsked = 0.0
 
      perr = 0.0
      qerr = 0.0
 
      array(1) = pgen
      array(2) = qgen
      array(3) = pload
      array(4) = qload
      array(5) = pgnmax
      array(6) = qgnmax
      array(7) = pgnmin
      array(8) = qgnmin
      array(9) = skcond
      array(10) = totcap
      array(11) = 0.0
      array(12) = totrek
      array(13) = skcond
      array(14) = usecap
      array(15) = 0.0
      array(16) = userek
      array(19) = gequiv
      array(20) = bequiv
      array(21) = 0.0
      array(22) = unsked
      array(23) = perr
      array(24) = qerr
      array(25) = tottyp(1)
      array(26) = tottyp(2)
      array(27) = tottyp(3)
      array(28) = tottyp(4)
      array(29) = tottyp(5)
      array(30) = tottyp(6)
      return
      end
