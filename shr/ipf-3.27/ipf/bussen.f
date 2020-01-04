C    @(#)bussen.f	20.3 2/13/96
      subroutine bussen
C
C     This subroutine computes the sensitivities DTheta/DP and
C     Dvolt/DQ.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha2.inc'
c	Global variables used:
c		kvolt
      include 'ipfinc/amtrx.inc'
c	Global variables used:
c		dpt(r*8)
      include 'ipfinc/beta2.inc'
c	Global variables used:
c		None
      include 'ipfinc/blank.inc'
c	Global variables used:
c		ntota, bmva, kspare
      include 'ipfinc/bus.inc'
c	Global variables used:
c		f(r*8), e(r*8)
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		ntotx ,nsen
      include 'ipfinc/gamma.inc'
c	Global variables used:
c		None
      include 'ipfinc/intbus.inc'
c	Global variables used:
c		intbus, intbas
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		None
      include 'ipfinc/optim1.inc'
c	Global variables used:
c		ksen, sen
      include 'ipfinc/prt.inc'
c	Global variables used:
c		outbuf
      include 'ipfinc/sensit.inc'
c	Global variables used:
c		senmax, maxsen
      include 'ipfinc/slnopt.inc'
c	Global variables used:
c		None
      include 'ipfinc/tran.inc'
c	Global variables used:
c		tap(r*8), ltran(i*4)
 
      character*1 tag(3)
      integer senchk
      external kompsv, swapsv
C
        call forbtm
        call fortop
        write (outbuf,100)
  100   format(' BUS_SENSITIVITIES COMPUTED ',
     &         'WITH THE FOLLOWING CONTROLS:')
        call prtout (1)
        call space(1)
        if (kspare(19) .eq. 0) then
           tag(1) = 'X'
           tag(2) = ' '
           tag(3) = ' '
        else if (kspare(19) .eq. 1) then
           tag(1) = ' '
           tag(2) = ' '
           tag(3) = 'X'
        else
           tag(1) = ' '
           tag(2) = 'X'
           tag(3) = ' '
        endif
        write (outbuf,120) tag(1)
120     format(t18,'LTC CONTROL',t43,'(',a,')',3x,'OFF')
        call prtout (1)
 
        write (outbuf,130) tag(2)
130     format(t18,'--- -------',t43,'(',a,')',3x,
     &         'ON (FULL CONTROL)')
        call prtout (1)
 
        write (outbuf,140) tag(3)
140     format(t43,'(',a,')',3x,'ON (NO VOLTAGE CONTROL)')
        call prtout (1)
 
        call space (1)
 
        if (kspare(20) .eq. 0) then
           tag(1) = 'X'
           tag(2) = ' '
           tag(3) = ' '
        else if (kspare(20) .eq. 1) then
           tag(1) = ' '
           tag(2) = 'X'
           tag(3) = ' '
        else
           tag(1) = ' '
           tag(2) = ' '
           tag(3) = 'X'
        endif
        write (outbuf,150) tag(1)
150     format(t18,'AI CONTROL',t43,'(',a,')',3x,'OFF')
        call prtout (1)
 
        write (outbuf,160) tag(2)
160     format(t18,'-- -------',t43,'(',a,')',3x,
     &        'CONTROL (DEFAULT)')
        call prtout (1)
 
        write (outbuf,170) tag(3)
170     format(t43,'(',a,')',3x,'MONITOR')
        call prtout (1)
 
        call space(1)
 
        if (kspare(39) .eq. 0) then
           tag(1) = 'X'
           tag(2) = ' '
        else
           tag(1) = ' '
           tag(2) = 'X'
        endif
        write (outbuf,180) tag(1)
180     format(t18,'Q_SHUNT CONTROL',t43,'(',a,')',3x,'ADJUSTABLE')
        call prtout (1)
 
        write (outbuf,190) tag(2)
190     format(t18,'------- -------',t43,'(',a,')',3x,'FIXED')
        call prtout (1)
 
        call space (1)
 
        if (kspare(40) .eq. 0) then
           tag(1) = 'X'
           tag(2) = ' '
        else
           tag(1) = ' '
           tag(2) = 'X'
        endif
        write (outbuf,200) tag(1)
200     format(t18,'Q_GENERATION CONTROL',t43,'(',a,')',3x,'ADJUSTABLE')
        call prtout (1)
 
        write (outbuf,210) tag(2)
210     format(t18,'------------ -------',t43,'(',a,')',3x,'FIXED')
        call prtout (1)
 
        call space (2)
C
C     Recompute Jacobian matrix. The previous Jacobian matrix cannot
C     be reused because
C         1.  only the upper-diagonal portion is stored and
C         2.  common /AMTRX/ is not physically large enought to
C             accomodate both upper and lower factors in double
C             precision.
C     To circumvert the second obstacle, the Jacobian is refactored in
C     single precision, which reduces the physical storage requirements
C     by 50%.
C
      call senfac
C
C     BEGIN BUS_SENSITIVITY LOOP
C
      do 1670 n = 1,nsen
 
      do 1510 k = 1,ntotx
         dpt(1,k) = 0.0d0
         dpt(2,k) = 0.0d0
 1510 continue
 
      kt = ksen(1,n)
      if (ksen(2,n) .eq. 1) then
         dpzz = -sen(3,n)
         ix = 1
         iy = 1
      else if (ksen(2,n) .eq. 2) then
         dpzz = -sen(3,n)
         ix = 2
         iy = 2
      else if (ksen(2,n) .eq. 3) then
         dpzz = -sen(3,n) * (e(kt)**2 + f(kt)**2)
         ix = 1
         iy = 1
      else if (ksen(2,n) .eq. 4) then
         dpzz = sen(3,n) * (e(kt)**2 + f(kt)**2)
         ix = 2
         iy = 2
      else if (ksen(2,n) .eq. 5) then
         dpzz = sen(3,n)
         ix = 1
         iy = 1
      else if (ksen(2,n) .eq. 6) then
         dpzz = sen(3,n)
         ix = 2
         iy = 2
      else if (ksen(2,n) .eq. 7) then
         dpzz = -sen(3,n)
         ix = 1
         iy = 2
      else if (ksen(2,n) .eq. 8) then
         dpzz = -sen(3,n) * (e(kt)**2 + f(kt)**2)
         ix = 1
         iy = 2
      else if (ksen(2,n) .eq. 9) then
         dpzz = sen(3,n)
         ix = 1
         iy = 2
      endif
      dpt(ix,kt+ntota) = 1.0d0
 
      call baksen (0)
C
C     FIND 10 LARGEST SENSITIVITIES
C
      nt = 0
      dplo = 1.0e+20
      do 1530 k = 1,ntota
      dp = dpt(1,k)
      if (dp.eq.0.0) go to 1530
      ityp = mod (ltran(10,k),100)
      if (ix .eq. 1) then
         if (ityp .ne. 3) go to 1530
      else
         if (ityp .eq. 3) go to 1530
      endif
      if (nt.lt.10) then
         nt = nt + 1
         if ( (nt.eq.1) .or. (abs(dp).le.dplo) ) then
            dplo = abs(dp)
            ntlo = nt
         endif
         maxsen(nt) = k
         senmax(nt) = dp
      else
         if (abs(dp) .gt. dplo) then
            maxsen(ntlo) = k
            senmax(ntlo) = dp
            dplo = abs(dp)
            do 1520 j = 1,10
            if (abs(senmax(j)) .le. dplo) then
               dplo = abs(senmax(j))
               ntlo = j
            endif
 1520       continue
         endif
      endif
 1530 continue
      do 1550 k = ntota+1,ntotx-1
      dp = dpt(iy,k)
      if (dp.eq.0.0) go to 1550
      if (nt.lt.10) then
         nt = nt + 1
         if ( (nt.eq.1) .or. (abs(dp).le.dplo) ) then
            dplo = abs(dp)
            ntlo = nt
         endif
         maxsen(nt) = k
         senmax(nt) = dp
      else
         if (abs(dp) .gt. dplo) then
            maxsen(ntlo) = k
            senmax(ntlo) = dp
            dplo = abs(dp)
            do 1540 j = 1,10
            if (abs(senmax(j)) .le. dplo) then
               dplo = abs(senmax(j))
               ntlo = j
            endif
 1540       continue
         endif
      endif
 1550 continue
      do 1560 j = 1,nt
 1560 maxsrt(j) = j
      if (nt .gt. 1) then
         call qiksrt (1,nt,kompsv,swapsv)
      endif
      kt = ksen(1,n)
      if (ix .eq. 1 .and. ksen(2,n) .le. 5) then
         if (ksen(2,n) .eq. 1) then
            write (outbuf,1580) intbus(kt),intbas(kt),sen(3,n)
 1580       format('0 ',a8,f7.1,' A ',f6.1,
     &             ' MW CHANGE IN LOAD AT THIS BUS WILL CAUSE ',
     &             'THE FOLLOWING ANGLE CHANGES')
         else if (ksen(2,n) .eq. 3) then
            write (outbuf,1582) intbus(kt),intbas(kt),sen(3,n)
 1582       format('0 ',a8,f7.1,' A ',f6.1,
     &             ' MW CHANGE IN SHUNT AT THIS BUS WILL CAUSE ',
     &             'THE FOLLOWING ANGLE CHANGES')
         else
            write (outbuf,1584) intbus(kt),intbas(kt),sen(3,n)
 1584       format('0 ',a8,f7.1,' A ',f6.1,
     &             ' MW CHANGE IN GENERATION AT THIS BUS WILL CAUSE ',
     &             'THE FOLLOWING ANGLE CHANGES')
         endif
         call prtout (1)
         call space (1)
         write (outbuf,1586)
 1586    format('0',t9,'BUS       BASE',
     1              t26,'BUS       BASE',
     2              t45,'------ DTHEAT/DP -----',
     3              t69,'------ BASE VOLTAGE -------',
     4              t100,'------ NEW VOLTAGE -------')
         call prtout (1)
         write (outbuf,1588)
 1588    format(t45,'(RADIANS/P.U.) (DEGREES/MW)',
     1          t69,'(RADIANS)        (DEGREES) ',
     2          t100,'(RADIANS)      (DEGREES)' )
         call prtout (1)
      else if (ix .eq. 1 .and. ksen(2,n) .ge. 7) then
         if (ksen(2,n) .eq. 7) then
            write (outbuf,10580) intbus(kt),intbas(kt),sen(3,n)
10580       format('0 ',a8,f7.1,' A ',f6.1,' MW CHANGE IN LOAD AT THIS B
     1us will cause the following voltage changes')
         else if (ksen(2,n) .eq. 8) then
            write (outbuf,10582) intbus(kt),intbas(kt),sen(3,n)
10582       format('0 ',a8,f7.1,' A ',f6.1,' MW CHANGE IN SHUNT AT THIS
     1 bus will cause the following voltage changes')
         else
            write (outbuf,10584) intbus(kt),intbas(kt),sen(3,n)
10584       format('0 ',a8,f7.1,' A ',f6.1,' MW CHANGE IN GENERATION AT
     1 this bus will cause the following voltage changes')
         endif
         call prtout (1)
         call space (1)
         write (outbuf,10586)
10586    format('0',t9,'BUS       BASE',
     1              t26,'BUS       BASE',
     2              t45,'------ DVOLT/DP -----',
     3              t69,'------ BASE VOLTAGE -------',
     4              t100,'------ NEW VOLTAGE -------')
         call prtout (1)
         write (outbuf,10588)
10588    format(t45,'(P.U./P.U.)   (KV/MW)',
     1          t69,'(P.U. KV)          (KV) ',
     2          t100,'(P.U. KV)         (KV) ' )
         call prtout (1)
      else
         if (ksen(2,n) .eq. 2) then
            write (outbuf,1590) intbus(kt),intbas(kt),sen(3,n)
 1590       format('0 ',a8,f7.1,' A ',f6.1,
     &             ' MVAR CHANGE IN LOAD AT THIS BUS WILL CAUSE ',
     &             'THE FOLLOWING VOLTAGE CHANGES')
         else if (ksen(2,n) .eq. 4) then
            write (outbuf,1592) intbus(kt),intbas(kt),sen(3,n)
 1592       format('0 ',a8,f7.1,' A ',f6.1,
     &             ' MVAR CHANGE IN SHUNT AT THIS BUS WILL CAUSE ',
     &             'THE FOLLOWING VOLTAGE CHANGES')
         else
            write (outbuf,1594) intbus(kt),intbas(kt),sen(3,n)
 1594       format('0 ',a8,f7.1,' A ',f6.1,
     &             ' MVAR CHANGE IN GENERATION AT THIS BUS WILL CAUSE ',
     &             'THE FOLLOWING VOLTAGE CHANGES')
         endif
         call prtout (1)
         call space (1)
         write (outbuf,1596)
 1596    format('0',t9,'BUS       BASE',
     1              t27,'BUS       BASE',
     2              t45,'----- DVOLT/DQ  -----',
     3              t69,'------ BASE VOLTAGE -------',
     4              t100,'------ NEW VOLTAGE -------')
         call prtout (1)
         write (outbuf,1598)
 1598    format(t45,'(P.U./P.U.)   (KV/MVAR)',
     1          t69,'(P.U. KV)             (KV)',
     2          t100,'(P.U.KV)            (KV)')
         call prtout (1)
C
C        CHECK FOR PQ OR PV BUS.  IF PV BUS, THEN 1.0 PERTURBATION
C        CORRESPONDS TO 1.0 P.U. VOLTAGE PERTURBATION!  FUNCTION
C        "SENSDQ" COMPUTES THE CORRESPONDING Q-PERTURBATION.
C
      endif
        cx = 1.0
      if (ksen(2,n) .eq. 2 .or. ksen(2,n) .eq. 4 .or.
     1    ksen(2,n) .eq. 6) then
         if (kvolt(kt) .eq. 0) then
         else if (senchk(kt) .eq. 0) then
         else
            cx = 1.0 / sensdq(kt,dpt)
         endif
      endif
      if (ksen(2,n) .eq. 3) then
         cx = 1.0/(1.0 + 2.0 * cx * dpt(1,kt+ntota) * dpzz / bmva)
      else if (ksen(2,n) .eq. 4) then
         cx = 1.0/(1.0 - 2.0 * cx * dpt(2,kt+ntota) * dpzz / bmva)
      else if (ksen(2,n) .eq. 8) then
         cx = 1.0/(1.0 + 2.0 * cx * dpt(1,kt+ntota) * dpzz / bmva)
      endif
      do 1660 j = 1,nt
      m = maxsrt(j)
      kt = maxsen(m)
      if (ix .eq. 1 .and. ksen(2,n) .le. 5) then
         if (kt .gt. ntota) then
            mt = kt - ntota
            vold = datan2(f(mt),e(mt))
            voldxx = 57.2957795 * vold
            dv = cx * senmax(m)
            dvxx = 57.2957795 * dv / bmva
            vnew = vold + dv * dpzz / bmva
            vnewxx = voldxx + dvxx * dpzz
            write (outbuf,1630) intbus(mt),intbas(mt),dv,dvxx,
     1         vold,voldxx,vnew,vnewxx
            call prtout (1)
 1630       format(8x,a8,f6.1,21x,f8.4,f12.4,4x,f9.3,f18.2,4x,
     1       f7.3,f18.2)
         else
            kx = ltran(1,kt)
            mx = ltran(9,kt)
            told = tap(kt)
            toldxx = told * 57.2957795
            dt = cx * senmax(m)
            dtxx = 57.2957795 * dt / bmva
            tnew = told + dt * dpzz / bmva
            tnewxx = toldxx + dtxx * dpzz
            write (outbuf,1640) intbus(kx),intbas(kx),intbus(mx),
     1         intbas(mx),dt,dtxx,told,toldxx,tnew,tnewxx
 1640       format(8x,a8,f6.1,2x,a8,f6.1,5x,f8.4,f12.4,4x,f9.3,f18.2,
     1        4x,f7.3,f18.2)
            call prtout (1)
         endif
      else if (ix .eq. 1 .and. ksen(2,n) .ge. 7) then
         if (kt .gt. ntota) then
            mt = kt - ntota
            vold = dsqrt(e(mt)**2 + f(mt)**2)
            voldxx = intbas(mt) * vold
            dv = cx * senmax(m)
            dvxx = intbas(mt) * dv * vold / bmva
            vnew = vold + dv * dpzz / bmva
            vnewxx = voldxx + dvxx * dpzz
            write (outbuf,1630) intbus(mt),intbas(mt),dv,dvxx,
     1         vold,voldxx,vnew,vnewxx
            call prtout (1)
         else
            kx = ltran(1,kt)
            mx = ltran(9,kt)
            told = tap(kt)
            toldxx = told * 57.2957795
            dt = cx * senmax(m)
            dtxx = 57.2957795 * dt / bmva
            tnew = told + dt * dpzz / bmva
            tnewxx = toldxx + dtxx * dpzz
            write (outbuf,1640) intbus(kx),intbas(kx),intbus(mx),
     1         intbas(mx),dt,dtxx,told,toldxx,tnew,tnewxx
            call prtout (1)
         endif
      else
         if (kt .gt. ntota) then
            mt = kt - ntota
            vold = dsqrt(e(mt)**2 + f(mt)**2)
            voldxx = intbas(mt) * vold
            dv = cx * senmax(m)
            dvxx = intbas(mt) * dv * vold / bmva
            vnew = vold + dv * dpzz / bmva
            vnewxx = voldxx + dvxx * dpzz
            write (outbuf,1630) intbus(mt),intbas(mt),dv,dvxx,
     1         vold,voldxx,vnew,vnewxx
            call prtout (1)
         else
            kx = ltran(1,kt)
            mx = ltran(9,kt)
            told = tap(kt)
            toldxx = tran(6,kt) * intbas(mx) / tap(kt)
            dt = cx * senmax(m)
            dtxx = -toldxx * dt / bmva
            tnew = told + dt * dpzz / bmva
            tnewxx = toldxx + dtxx * dpzz
            write (outbuf,1640) intbus(kx),intbas(kx),intbus(mx),
     1         intbas(mx),dt,dtxx,told,toldxx,tnew,tnewxx
             call prtout (1)
          endif
      endif
 1660 continue
 1670 continue
      return
      end
