C    @(#)vsen.f	20.3 2/13/96
      subroutine vsen (n,k1,m1,dydx,deltax)   ! compute the voltage
C                      sensitivities dVi/dBkl where Bkl is a
C                      pi-equivalent susceptance defined by branch
C                      K1-M1.
      complex * 8 dydx

      include 'ipfinc/parametr.inc'

      include 'ipfinc/amtrx.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/gamma.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/lnsen.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/sensit.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tran.inc'

      character id1 * 1
      external kompsv,swapsv
C                           FIND 20 LARGEST SENSITIVITIES
      nt = 0
      dplo = 1.0e+20
      do 1530 k = 1,ntota
      dp = dpt(1,k)
      if (dp.eq.0.0) go to 1530
      itype = mod (ltran(10,k),100)
      if (itype .eq. 3) go to 1530
      if (nt.lt.20) then
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
            do 1520 j = 1,20
            if (abs(senmax(j)) .le. dplo) then
               dplo = abs(senmax(j))
               ntlo = j
            endif
 1520       continue
         endif
      endif
 1530 continue
      do 1550 k = ntota+1,ntotx-1
      dp = dpt(2,k)
      if (dp.eq.0.0) go to 1550
      if (nt.lt.20) then
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
            do 1540 j = 1,20
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
 
      write (outbuf,1570)
 1570 format('0Sensitivity type',
     1         t23,'Branch immitance sensitivity',
     2         t60,'--- Original immitance (p.u.) --',
     3         t96,'- Compensated immitance (p.u.) -')
      call prtout (1)
 
      id1 = char(lsen(4,n))
      ksect1 = lsen(5,n)
 
      write (outbuf,1572)
 1572 format(t60,'---  R + jX ---  ---  G + jB ---',
     1       t96,'---  R + jX ---  ---  G + jB ---')
      call prtout (1)
      call space (1)
      write (outbuf,1574) intbus(k1),intbas(k1),intbus(m1),
     1      intbas(m1),id1,ksect1,(yzold(i,n),i=1,4),
     2      (yznew(i,n),i=1,4)
 1574 format (t2,'dVi/dX or dVi/dB',
     1        t23,a8,f7.1,1x,a8,f7.1,1x,a1,i2,
     2        t60,2f8.5,1x,2f8.5,
     3        t96,2f8.5,1x,2f8.5)
      call prtout (1)
      call space (1)
 
      if (ysen(7,n) .eq. 0.0 .and. ysen(8,n) .ne. 0.0) then
         itype = 2
         write (outbuf,1576) 'dVi/dB','dB'
 1576    format(t23,'Node i',
     1          t60,a,
     2          t75,a,
     3          t90,'Voltage',
     4          t105,'Pertubed V ',
     5          t120,'New Voltage')
         call prtout (1)
      else
         itype = 1
         write (outbuf,1576) 'dVi/dX','dX'
         call prtout (1)
      endif
      write (outbuf,1578)
 1578 format(
     1         t60,'(p.u./p.u.) ',
     2         t75,'   (p.u.) ',
     3         t90,'   (p.u.)    ',
     4         t105,'   (p.u.)    ',
     5         t120,'   (p.u.)   ')
      call prtout (1)
      call space (1)
      cx = 1.0
      do 1660 j = 1,nt
      m = maxsrt(j)
      kt = maxsen(m)
      if (kt .gt. ntota) then
         mt = kt - ntota
         vold = dsqrt(e(mt)**2 + f(mt)**2)
         dv = cx * senmax(m)
         deltav = dv * deltax
         vnew = vold + deltav
         write (outbuf,1580) intbus(mt),intbas(mt),
     1      dv,deltax,vold,deltav,vnew
 1580    format (t23,a8,f7.1,
     1           t60,e12.5,
     2           t75,f12.5,
     3           t90,f10.4,
     4           t105,f10.4,
     5           t120,f10.4)
         call prtout (1)
      else
         kx = ltran(1,kt)
         mx = ltran(9,kt)
         told = tran(6,kt) * intbas(mx) / tap(kt)
         dt = - cx * tap(kt) / told
         deltat = dt * deltax
         tnew = told + deltav
         write (outbuf,1590) intbus(kx),intbas(kx),intbus(mx),
     1      intbas(mx),dt,deltax,told,deltat,tnew
 1590    format (t23,a8,f7.1,1x,a8,f7.1,
     1           t60,e12.5,
     2           t75,f12.5,
     3           t90,f10.2,
     4           t105,f10.2,
     5           t120,f10.2)
         call prtout (1)
      endif
 1660 continue
 1670 continue
 
      return
      end
