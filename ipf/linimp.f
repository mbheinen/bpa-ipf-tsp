C    @(#)linimp.f	20.6 11/11/97
      subroutine  linimp ( num, dist, units, basmva, basekv, freq,
     1   iphase, skin, resis, diam, horiz, vtower, vmid, separ, alpha, 
     2   numbnd, zout, ierr )  
      implicit real*8 (a-h, o-z),  integer*4 (i-n)

      include 'ipfinc/deck25.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/miscfile.inc'

      common /is_batch / is_batch

      parameter( MAXCND = 36 )    ! N
      parameter( MAXTRI = 666)    ! N*(N+1)/2
      parameter( MAXSQR = 1296)   ! N*N
      parameter( MAXSQ2 = 2592)   ! 2*N*N
      parameter( MAXHAF = 171)    ! N/2*(N/2+1)/2

      dimension  iphase(MAXCND), skin(MAXCND), resis(MAXCND)
      dimension  diam(MAXCND), horiz(MAXCND)
      dimension  itbic(MAXCND), tbtb2(MAXCND), tbr(MAXCND)
      dimension  tbd(MAXCND), tbx(MAXCND), vtower(MAXCND)
      dimension  vmid(MAXCND), separ(MAXCND), alpha(MAXCND) 
      dimension  numbnd(MAXCND),  zout(6)
      dimension  ic(MAXCND), r(MAXCND), d(MAXCND), gmd(MAXCND)
      dimension  x(MAXCND), y(MAXCND), tb2(MAXCND), itb3(MAXCND)
      dimension  tbg(MAXCND), tby(MAXCND), itbtb3(MAXCND)
      dimension  workr1(MAXTRI), workr2(MAXTRI)
      dimension  p(MAXTRI), z(MAXTRI)                  
      dimension  gd(MAXHAF), bd(MAXHAF), yd (MAXHAF)    
      dimension  bcars(30), ccars(30),  dcars(30)

      character*7   units
      integer    status, open_file

      integer    iblank, iblanks(2)
      real*8     xblank
      equivalence (xblank, iblanks)

      i = ichar(' ')
      iblank = i + 256*i + 256*256*i + 256*256*256*i
      iblanks(1) = iblank
      iblanks(2) = iblank

      ll0 = 0
      valu1    =  5 584 596.2   d0
      valu2    =   .000 856 465 41  d0
      valu3    =   .000 643 738 88  d0
      valu4    =   .615 931 52  d0
      valu6    =   .000 500 000 0   d0
      onehaf   =  1. / 2. d0
      onetrd   =  1. / 3. d0
      sqrt3    =  dsqrt (3.0 d0)
      valu7    =  onehaf * sqrt3
      valu8    =   .004 044 730 6   d0
      valu9    =   .211 392 17  d0
      valu10   =   .577 215 664 901 532 860 6   d0
      valu11   =   .392 699 1   d0
      valu12   =   .398 942 280 401 433 d0
      valu13   =  1.253 314 137 315 5   d0
      aaa1     =   .785 398 163 397 448 d0
      aaa2     =   .318 309 886 183 791 d0
      sqrt2    =  1.414 213 562 373 095 048 8   d0
      twopi    =  6.283 185 307 179 586 476 92  d0
      pi = twopi * onehaf
      pidiv4 = pi * 0.25 d0
      tenm6 = 1. d-6
      unity = 1. d0
      ccars(2) =  1.365 931 515 658 412 448 8   d0
      nbundl = 0
      ierr = 0

      rearth = 100. d0
      lphase = MAXCND    
      lphpl1 = MAXCND + 1   
      nthalf = MAXHAF  

C     IPRSUP = 4       !  Diagnostic output control
      lunit4 = lunscr1 !  Scratch file
      lunit6 = dbug    !  Output file

      call close_file (lunit4)
      status = open_file (lunit4, ' ', 'U', 'W', iostat)
      rewind lunit4
      finpcm = unity / 2.54d0
      ftpm = 100. * finpcm / 12.0
      fmipkm = ftpm * 1000.0 / 5280.0
      d8 = 0.3048d0
      fspac = .024d0 * twopi * dlog ( d8 )
      bcars(1) = sqrt2 / 6.0
      bcars(2) = unity / 16.0
      dcars(2) = bcars(2) * pidiv4
      do 6603  i=3, 30
      isn = (-1) ** ( (i - 1 ) / 2 )
      bcars(i) = -bcars(i-2) / ( i * i + 2.0 * i ) * isn
 6603 dcars(i) = bcars(i) * pidiv4
      ccars(1) = unity / sqrt2
      ccars(3) = ccars(1)
      ccars(5) = 3.0 * sqrt2 * onehaf
      ccars(7) = -45.0 * onehaf * sqrt2
      do 3964  i=1, 29, 2
      if ( i .gt. num ) ccars(i) = 0.0
 3964 dcars(i) = ccars(i)
      dcars(3) = -dcars(3)
      dcars(7) = -dcars(7)
      do 3846  i=4, 30, 2
 3846 ccars(i) = ccars(i-2) + unity / i  +  unity / ( i + 2.0 )
      fbe(1) = 16.0
      fbed(1) = -4.0
      d1 = unity - valu10
      fke(1) = fbe(1) * d1
      do 1811  i=2, 14
      isn= (-1)**i
      fbe(i) = fbe(i-1) * ( 16.0 / (i*i) ) * (-isn)
      fbed(i) = fbe(i) / ( 2.0 * i + 2.0 ) * isn
      d1 = d1 + unity / i
      fke(i) = fbe(i) * d1
 1811 fked(i-1) = fke(i) * i / 32.0
      fked(14) = 0.0
      fbe(15) =   .011 048 60 d0
      fbe(16) =   0.0
      fbe(17) =  -.000 090 60 d0
      fbe(18) =  -.000 025 20 d0
      fbe(19) =  -.000 003 40 d0
      fbe(20) =   .000 000 60 d0
      fbed(15) = -.011 048 50 d0
      fbed(16) = -.000 976 50 d0
      fbed(17) = -.000 090 10 d0
      fbed(18) =   0.0
      fbed(19) =  .000 005 10 d0
      fbed(20) =  .000 001 90 d0
      fke(15) =  -.062 500 10 d0
      fke(16) =  -.001 381 30 d0
      fke(17) =   .000 000 50 d0
      fke(18) =   .000 034 60 d0
      fke(19) =   .000 011 70 d0
      fke(20) =   .000 001 60 d0
      fked(15) = -.000 000 10 d0
      fked(16) =  .001 381 10 d0
      fked(17) =  .000 245 20 d0
      fked(18) =  .000 033 80 d0
      fked(19) = -.000 002 40 d0
      fked(20) = -.000 003 20 d0
      zero = 0.0

      j = 0
      do 1021  kk = 1, num  ! # OF CONDUCTOR CARDS
      if ( numbnd(kk) .eq. 0 )  j = j + 1
      if ( numbnd(kk) .ne. 0 )  j = j + numbnd(kk)
 1021 continue
      num = j     !  TOTAL NUMBER OF CONDUCTORS
      write (lunit6, '(a, i4)') ' Total # of conductors =', num

      if ( units(1:6) .ne. 'METRIC' ) go to 1258
      metrik = 1
      go to 4239
 1258 metrik = 0

 4239 i = 1
      j = 1      ! needed for counting bundled conductos
      go to 4245
 4241 i = i + 1
      j = j + 1

 4245 itbtb3(j) = 4
      tbg(j) = 0.
      itbic(j) = iphase(i)
      tbtb2(j) = skin(i)
      tbr(j) = resis(i)
      tbd(j) = diam(i)
      tbx(j) = horiz(i)
      h1 = vtower(i)
      h2 = vmid(i)
      d8 = separ(i)
      d9 = alpha(i)
      i3 = numbnd(i)
c
c     The following statement exploit the equivalence (xblanks, iblanks)
c     to test for blanks
c
      if ( i .eq. 1 )  go to 4320
      if ( itbic(j)  .eq. iblank ) itbic(j) = itbic(j-1)
      if ( tbtb2(j)  .eq. xblank ) tbtb2(j) = tbtb2(j-1)
      if ( tbr(j)    .eq. xblank ) tbr(j) = tbr(j-1)
      if ( itbtb3(j) .eq. iblank ) itbtb3(j) = itbtb3(j-1)
      if ( tbg(j)    .eq. xblank ) tbg(j) = tbg(j-1)
      if ( tbd(j)    .eq. xblank ) tbd(j) = tbd(j-1)
 4320 if ( vtower(j) .eq. xblank ) h1 = vmid(i)
      if ( vmid(j)   .eq. xblank ) h2 = vtower(i)
      tby(j) = ( h1 + h2 + h2 ) * onetrd
      if ( i3 .le. 1 )   go to 2405  ! NOT AUTO. BUNDLING
      nbundl = 1
      i4 = 1
      xx = tbx(j)
      yy = tby(j)
      dangl = twopi / i3
      angl = ( pi - dangl ) * onehaf
      radius = d8 / ( 24.0 * dcos ( angl ) )
      if ( metrik .eq. 1 ) radius = radius * 0.12d0
      d9r = d9 * twopi / 360.
 4713 d22 =  d9r - dangl * i4
      tbx(j) = xx + radius * dcos ( d22 )
      tby(j) = yy + radius * dsin ( d22 )
      if ( i4 .eq. i3 )  go to 2405
      i4 = i4 + 1
      j = j + 1
      itbic(j) = itbic(j-1)
      tbtb2(j) = tbtb2(j-1)
      tbr(j) = tbr(j-1)
      itbtb3(j) = itbtb3(j-1)
      tbg(j) = tbg(j-1)
      tbd(j) = tbd(j-1)
      go to 4713
 2405 if ( j .lt. num  )  go to 4241   
      if ( iprsup .ge. 1 )
     1 write (lunit6, 3625)  ( itbic(k), itbtb3(k),  tbtb2(k),  tbr(k),
     2                         tbg(k), tbd(k), tbx(k), tby(k),  k=1, j )
 3625 format ( / '   itbic  itbtb3', 12x, 'tbtb2',  14x, 'tbr',  14x,
     1          'tbg',  14x, 'tbd',  14x, 'tbx'  ,14x, 'tby'
     1         , /,  ( 2i8, 6e17.8 )  )
      if ( metrik .ne. 1 ) go to 5981
      do 2982  k = 1, num
      tbd(k) = tbd(k) * finpcm
      tbx(k) = tbx (k) * ftpm
      tby(k) = tby(k) * ftpm
      tbr(k) = tbr(k) / fmipkm
 2982 continue
 5981 do  1248  i = 1, lphase
 1248 ic (i) = 0
      ktot = 0
      kcirct = 0
      do 2810  k=1, num           
      i1 = itbic(k)
      if ( i1 .eq. 0 )  go to 4012
      if ( i1 .lt. 0 ) go to 2810
      if ( i1 .gt. kcirct ) kcirct = i1
      i = lphpl1 - i1
      if ( ic(i) .eq. 0 ) go to 4813
 4012 ktot = ktot + 1
      i = ktot
      if ( i1 .eq. 0 ) go to 4813
      ip = ktot
 2918 j = i - 1
      if ( j .eq. 0  .or.
     1     ic(j) .gt. 0 ) go to 4813
      i = j
      ic(ip) = ic(i)
      tb2(ip) = tb2(i)
      itb3(ip) = itb3(i)
      r(ip) = r(i)
      d(ip) = d(i)
      gmd(ip) = gmd(i)
      x(ip) = x(i)
      y(ip) = y(i)
      ip = ip - 1
      go to 2918

 3527 ierr = 1
      write (errbuf(1), 281) kfull
  281 format ('Overflow - KFULL (', i6, ') is larger than MAXCND. Paramet
     1er maxcnd needs be increased.')
      write (errbuf(2), 282) '280', 'LINIMP'
  282 format ('Error called from statemnt ', a, ' in subroutine ', a)
      if (is_batch .eq. 0) then
         call prterx ('E',2)
      else
         call prterx ('F',2)
      endif
      go to 9800
 4813 kfull = ktot + kcirct
      if ( kfull .gt. lphase )  go to 3527
      ic(i) = i1
      tb2(i) = tbtb2(k)
      itb3(i) = itbtb3(k)
      r(i) = tbr(k)
      d(i) = tbd(k)
      gmd(i) = tbg(k)
      x(i) = tbx(k)
      y(i) = tby(k)
 2810 continue
 2715 rewind lunit4

      j5 = 1
      j9 = 1
      j14 = 1
      j16 = 1
      corr = tenm6
      if ( metrik .eq. 1 )  dist = dist * fmipkm
      omega = twopi * freq
      idist = 1
      if ( kcirct .eq. 0 )  go to 4271
      if ( kcirct .lt. lphase/2 )  go to 4275
 4271 idist = 0
 4275 j56 = j5  + idist
      ip = 0
      k = 0
      f1 = unity / omega
      f1 = f1 * valu1
      f2 = f1 * 2.0
 1020 k = k + 1
      if ( k .gt. kfull ) go to 1730
      if ( k .le. kcirct ) go to 2328
      j = k - kcirct
 3621 x1 = x(j)
      y1 = y(j)
      d1 = d(j)
      if ( tb2(j) .gt. 0.0 ) go to 5224

      ierr = 1
      write (errbuf(1), 371) j
  371 format ('Illegal null SKIN data for conductor No. ', i6)
      write (errbuf(2), 282) '371', 'LINIMP'
      call prterx('W', 2)      
      go to 9800
 5224 gmd(j) = d(j)
      do 4123  i=1, k-1
      ip = ip + 1
      j = i - kcirct
      if ( i .le. kcirct )  j = lphpl1 - i
      dx = ( x(j) - x1 )**2
      h1 = y(j) - y1
      h2 = y(j) + y1
      if ( dx .eq. 0.0  .and.
     1     h1 .eq. 0.0 ) go to 3026
      d21 = ( dx + h2 * h2 ) / ( dx + h1 * h1 )
      r1 = dlog ( d21 )
      p(ip) = r1 * f1
      z(ip) = r1
 4123 continue
      ip = ip + 1
      p(ip) = dlog ( 48.0 * y1 / d1 ) * f2
      go to 1020

 3026 ierr = 1
      write (errbuf(1), 401) k, j
  401 format ('Illegal duplicate (x,y) coordinates for (sorted) conducto
     1rs ', i6, ' and ', i6)
      write (errbuf(2), 282) '400', 'LINIMP'
      if (is_batch .eq. 0) then
         call prterx ('E',2)
      else
         call prterx ('F',2)
      endif
      go to 9800
 2328 j = lphpl1 - k
      if ( ic(j) .ne. 0 )  go to 3621

      ierr = 1
      write (errbuf(1), 411) k
  411 format ('Missing conductor data for circuit ', i6)
      write (errbuf(2), 282) '411', 'LINIMP'
      if (is_batch .eq. 0) then
         call prterx ('E',2)
      else
         call prterx ('F',2)
      endif
      go to 9800
 1730     write (lunit4) ( z(i), i=1, ip )
      if ( iprsup .ge. 2 )
     1 write (lunit6, 4427)  ( z(i), i=1, ip )
 4427 format ( /, 1p,  ' Z(1:IP) follow ....'  ,/,  ( 1x, 8e16.7 ) )
      if ( iprsup .ge. 2 )
     1 write (lunit6,  1327)  ( p(i), i=1, ip )
 1327 format (    1p,  ' P(1:IP) follow ....'  ,/,  ( 1x, 8e16.7 ) )
      if ( iprsup .ge. 1 )
     1 write (lunit6, 4428)  j1, j4, kcirct, j2, j3, j56, kfull
 4428 format ( ' J1, J4, KCIRCT, J2, J3, J56, KFULL =',  7i10  )
      switch = -unity
 1449 k = kcirct
 2150 k = k + 1
      if ( k .gt. kfull ) go to 4156
      i = k - kcirct
      i = ic(i)
      if ( i .eq. 0 ) go to 2150
      i2 = i * ( i - 1 ) / 2
      k2 = k * ( k - 1 ) / 2
      ndx1 = k2 + i
      h1 = p(ndx1)
      h2 = z(ndx1)
      ndx1 = k2 + k
      l = 0
 1851 if ( l .lt. i ) go to 1154
      i2 = i2 + l
 3652 if ( l .lt. k ) go to 3355
      k2 = k2 + l
 2453 l = l + 1
      p(k2) = p(k2) - p(i2)
      if ( switch .gt. 0.0 ) z(k2) = z(k2) - z(i2)
      if ( l .ne. kfull ) go to 1851
      p(ndx1) = p(ndx1) - h1
      if ( switch .gt. 0.0 ) z(ndx1) = z(ndx1) - h2
      go to 2150
 1154 i2 = i2 + 1
      go to 3652
 3355 k2 = k2 + 1
      go to 2453
 4156 kp = kcirct * ( kcirct + 1 ) / 2
      if ( switch .gt. 0.0 ) go to 5202
      call reduction ( p(1), workr1(1), kfull, kcirct, ierr, iprsup,
     &                 lunit6 )
      if ( ierr .eq. 0 )  go to 4839
      go to 9800
 2980 switch = +unity
      j56 = 2     
      rewind lunit4
      f1 = valu2 * dsqrt ( freq / rearth )
      f2 = omega * valu3
      imaxlc = 30
      if ( imaxlc .gt. 31 )  imaxlc = 30
      read (lunit4)  ( p(i), i=1, ip )
      rewind lunit4
      ip = 0
      k = 0
 2120 k = k + 1
      if ( k .gt. kfull ) go to 5200
      if ( k .le. kcirct ) go to 1129
      j = k - kcirct
      go to 5121
 1129 j = lphpl1 - k
 5121 x1 = x(j)
      y1 = y(j)
      r1 = r(j)
      h1 = tb2(j)
      h2 = unity - 2.0 * h1
      if ( h1 .gt. 0.0 )
     1 call lcskin ( h2, r1, r1, h1, freq )
      g1 = gmd(j)
      ix = itb3(j)
      izero = 1
      xm = 0.0
      xs = dlog ( 24.0 * y1 / g1 ) * onehaf
 4122  d17 = dlog ( d(j) * onehaf / g1 )
      xs = xs + h1 / f2 - onehaf * d17
      i = 0
 3123 i = i + 1
      ip = ip + 1
      if ( i .eq. k ) go to 1124
      j = i - kcirct
      if ( i .le. kcirct )
     1 j = lphpl1 - i
      rm = 0.0
      if ( izero .eq. 0 ) go to 5126
      xm = p(ip) / 4.0
      if ( imaxlc .lt. 0 ) go to 5126
      dx = abs ( x(j) - x1 )
      h2 = y(j) + y1
      s = dsqrt ( h2 * h2 + dx * dx )
      z1 = s * f1
      if ( z1 .gt. 5.0 )  go to 4151
      rm = twopi / 16.0
      zl = dlog ( z1 )
      xm = xm  +  ( valu4 - zl ) * onehaf
      if ( imaxlc .eq. 0 ) go to 5126
      s1 = dx / s
      c1 = h2 / s
      cs = c1 * z1
      sn = s1 * z1
      if ( imaxlc .gt. 1 )   phi = dasin ( s1 )
      m5 = 0
      error = 0.0
 1110 k5 = 0
      i5 = m5 * 4
 1111 k5 = k5 + 1
      i5 = i5 + 1
      if ( k5 .eq. 1  .or.
     1     k5 .eq. 3 ) go to 1113
      deltap = ( ( ccars(i5) - zl ) * cs + phi * sn ) * bcars(i5)
      h1 = -dcars(i5) * cs
      if ( k5 .eq. 4 ) go to 1114
      deltaq = h1
 1004 rm = rm + deltap
      xm = xm + deltaq
      if ( i5 .eq. imaxlc ) go to 5126
      if ( dabs ( deltap ) .lt. corr  .and.
     1     dabs ( deltaq ) .lt. corr )  go to 1115
      error = 0.0
 1005 h1 = sn * c1 + cs * s1
      cs = ( cs * c1 - sn * s1 ) * z1
      sn = h1 * z1
      if ( k5 .lt. 4 ) go to 1111
      m5 = m5 + 1
      go to 1110
 1113 deltaq = bcars(i5) * cs
      deltap = deltaq
      if ( k5 .eq. 1 ) deltap = -deltap
      go to 1004
 1114 deltaq = -deltap
      deltap = h1
      go to 1004
 1115 if ( error .gt. onehaf ) go to 5126
      error = unity
      go to 1005
 4151 s = s * z1
      sn = dx / s
      cs = h2 / s
      s2 = sn * cs * 2.0
      c2 = cs * cs - sn * sn
      rm = -c2
      do 1006  i5=1, 7, 2
      rm = rm + ccars(i5) * cs
      xm = xm + dcars(i5) * cs
      h1 = cs * s2 + sn * c2
      cs = cs * c2 - sn * s2
 1006 sn = h1
 5126 p(ip) = rm * f2
      z(ip) = xm * f2
      if ( iprsup .ge. 4 )
     1 write (lunit6, 4439)  i, k, kfull, kcirct, i5, ip, p(ip), z(ip)
 4439 format ( ' I, K, KFULL, KCIRCT, I5, IP, =',  6i6,  2e25.16 )
      go to 3123
 1124 if ( imaxlc .lt. 0 ) go to 4128
      r1 = r1 / f2
      z1 = y1 * f1 * 2.0
      if ( z1 .gt. 5.0 ) go to 5152
      r1 = r1 + twopi / 16.0
      zl = dlog ( z1 )
      xs = xs  +  ( valu4 - zl ) * onehaf
      if ( imaxlc .eq. 0 ) go to 127
      m5 = 0
      cs = z1
      error = 0.0
 2110 k5 = 0
      i5 = m5 * 4
 2111 k5 = k5 + 1
      i5 = i5 + 1
      if ( k5 .eq. 1  .or.
     1     k5 .eq. 3 ) go to 2113
      deltap = ( ccars(i5) - zl ) * bcars(i5) * cs
      h1 = -dcars(i5) * cs
      if ( k5 .eq. 4 ) go to 2114
      deltaq = h1
 2004 r1 = r1 + deltap
      xs = xs + deltaq
      if ( i5 .eq. imaxlc ) go to 127
      if ( dabs ( deltap ) .lt. corr  .and.
     1     dabs ( deltaq ) .lt. corr )  go to 2115
      error = 0.0
 2005 cs = cs * z1
      if ( k5 .lt. 4 ) go to 2111
      m5 = m5 + 1
      go to 2110
 2113 deltaq = bcars(i5) * cs
      deltap = deltaq
      if ( k5 .eq. 1 ) deltap = -deltap
      go to 2004
 2114 deltaq = -deltap
      deltap = h1
      go to 2004
 2115 if ( error .gt. onehaf ) go to 127
      error = unity
      go to 2005
 5152 cs = unity / z1
      c2 = cs * cs
      r1 = r1 - c2
      do 2006  i5=1, 7, 2
      r1 = r1 + ccars(i5) * cs
      xs = xs + dcars(i5) * cs
 2006 cs = cs * c2
  127 r1 = r1 * f2
 4128 z(ip) = xs * f2
      p(ip) = r1
      if ( iprsup .ge. 4 )
     1 write (lunit6, 4439)  i, k, kfull, kcirct, i5, ip, p(ip), z(ip)
      go to 2120
 5200       if ( iprsup .ge. 1 )
     1 write (lunit6, 4434)  j7, j10, j56, kcirct, j8, j11, j12,
     2                       idist, j9, kp, j2, j3, j5, j6
 4434 format ( /,  ' at 4234.   misc. integers.'   ,/, ( 1x, 10i12 ) )
      go to 1449
 5202 call cxredu ( p(1), z(1),  workr1(1), workr2(1),  kfull,  kcirct )
 8202 if ( iprsup .ge. 1 )
     1 write (lunit6, 3202)   dist
 3202 format ( '  DIST =',   e25.16  )
 3203 j56 = 1
      do 1204  i=1, kp
      p(i) = -p(i)
 1204 z(i) = -z(i)
      call cxredu ( p(1), z(1), workr1(1), workr2(1), kcirct, ll0 )
      do 3206  i=1, kp
      gd(i) = p(i)
 3206 bd(i) = z(i)
 3207 go to 5600
 4839 i2 = 3
      i2 = i2 + 1
      do 2391  i=1, kp
 2391 p(i) = -p(i)
      call reduction ( p(1), workr1(1), kcirct, ll0, ierr, iprsup,
     &                 lunit6 )
      if ( iprsup .lt. 1 )  go to 5124
      write (lunit6, '(a, 6e12.5)') ' Ready for [C] avg.  P(1:6) =',
     1                                ( p(i), i=1, 6 )
      write (lunit6, '(a, 6e12.5)') ' Begin averaging of capacitance.'
      write (lunit6, '(a, 6e12.5)') ' 1st diagonal,  P(1) =',  p(1)
      write (lunit6, '(a, 6e12.5)') ' 1st off-diagonal,  P(2) =',  p(2)
      write (lunit6, '(a, 6i10)') ' =======  , KCIRCT, freq =',
     1                        kcirct, freq
 5124 call mover ( p(1), yd(1), kp )
      go to 2980
 5600 if ( metrik .eq. 1 )  go to 2610
      go to 4612
 2610 distm = dist / fmipkm
 4612 if ( idist .eq. 1 ) go to 1603
      go to 9800                
 1603 ip = 0
      d1 = 1.d+20
      do 2604  k=1, kcirct
      ip = ip + k
      h1 = valu6 * bd(ip) / yd(ip)
      if ( dabs ( h1 ) .lt. d1 ) d1 = dabs ( h1 )
 2604 continue
      d1 = dsqrt ( d1 * 2.0 )
      i1 = dist / d1 + onehaf
      i2 = 1
      do 4606  isec=1, 34
      if ( i2 .gt. i1 )  go to 5608
 4606 i2 = i2 * 2
      go to 9800
 5608 x1 = i2
      i1 = isec - 1
      deltad = dist / x1
 3615 f1 = unity / deltad
      f2 = deltad * onehaf
      kp = kcirct * ( kcirct + 1 ) / 2
      kcir2 = 2 * kcirct
      if ( i1 .eq. 0 )  go to 2670
      do 4621  i=1, kp
      r1 = f1 * gd(i)
      x1 = f1 * bd(i)
      g1 = f2 * yd(i)
      d1 = x1 + g1
      p(i) = r1
      z(i) = d1
      gd(i) = r1 + r1
 4621 bd(i) = x1 + d1
      icount = 0
 2622 icount = icount + 1
      if ( icount .eq. isec )  go to 1650
      ip = 0
      i3 = kp
      do 3623  k=1, kcirct
      i2 = i3 + kcirct
      l4 = kp + k
      do 2624  i=1, k
      l1 = ip + i
      l2 = i2 + i
      l3 = i3 + i
      r1 = p(l1)
      x1 = z(l1)
      h1 = r1 - gd(l1)
      h2 = x1 - bd(l1)
      p(l2) = r1 * 2.0
      z(l2) = x1 * 2.0
      p(l3) = h1
      z(l3) = h2
      gd(l1) = r1
      bd(l1) = x1
      if ( i .eq. k ) go to 4625
      p(l4) = h1
      z(l4) = h2
      l4 = l4 + i + kcirct
 2624 continue
 4625 ip = ip + k
      i3 = i2 + k
 3623 continue
      call cxredu ( p(1), z(1),  workr1(1), workr2(1),  kcir2,  kcirct )
      go to 2622
 1650 do 3651  i=1, kp
      gd(i) = gd(i) - p(i)
      bd(i) = bd(i) - z(i)
      p(i) = ( p(i) - gd(i) ) * 2.0
 3651 z(i) = ( z(i) - bd(i) ) * 2.0
 4673 go to 1660
 2652 call lcsym1 ( gd(1), bd(1) )
      if ( iprsup .ge. 1)
     &   write (lunit6, '(a, 6e12.5)') ' GD(1:6) AND BD(1:6) =', 
     &     (gd(i),bd(i), i=1,6)
      call lcsym1 ( p(1), z(1) )
      if ( iprsup .ge. 1)
     &   write (lunit6, '(a, 6e12.5)') ' P(1:6) AND Z(1:6)=',
     &    (p(i),z(i), i=1,6)
      d99 = basekv ** 2 / basmva
      zout(3) = p(5) * d99              ! half of pos-seq shunt adm.,
      zout(4) = z(5) * d99              ! in p.u., real & imag. parts

      temp1 = p(5)     !  Shunt admittance in mhos
      temp2 = z(5)     !  

 5654 do 2655  i=1, kp
      n1 = 2 * nthalf + 1 + i
      n2 = i + nthalf
      gd(i) = -p(n2)
      bd(i) = -z(n2)
      p(i) = -p(n1)
 2655 z(i) = -z(n1)
      call cxredu ( gd(1), bd(1), workr1(1), workr2(1), kcirct, ll0 )
      call cxredu ( p(1), z(1), workr1(1), workr2(1), kcirct, ll0 )
      call lcsym1 ( gd(1), bd(1) )
      d89 = basmva / (basekv ** 2)
      if ( iprsup .ge. 1) then
         write (lunit6,'(a, 6e12.5)') ' GD(1:6) AND BD(1:6) =', 
     &     (gd(i),bd(i), i=1,6)
      endif
      zout(1) = gd(5) * d89           ! pos-seq transfer imp, in
      zout(2) = bd(5) * d89           ! p.u., real and imag. parts
      zout(5) = gd(1) * d89           ! zerp-seq transfer imp, in
      zout(6) = bd(1) * d89           ! p.u., real and imag. parts
      write (lunit6, '(a, 6f10.3)') ' ZOUT(1:6) IN PHYSICAL UNIT:' ,
     1   gd(5), bd(5), temp1, temp2, gd(1), bd(1)
      write (lunit6, '(a, 6f10.5)') ' ZOUT(1:6) IN P.U. ARE:', 
     &   ( zout(i), i=1,6)
      go to 9800
 1660 i = nthalf + 1
      call mover ( gd(1), p(i), kp )
      call mover ( bd(1), z(i), kp )
      i = 2 * i
      call mover ( p(1), p(i), kp )
      call mover ( z(1), z(i), kp )
      go to 2652
 2670 f2 = f2 * 2.0
      do 3671  i=1, kp
      p(i) = 0.0
      z(i) = f2 * yd(i)
      gd(i) = f1 * gd(i)
 3671 bd(i) = f1 * bd(i)
      go to 4673
 9800 if ( iprsup .ge. 1 )
     1 write (lunit6, 9818)   ierr
 9818 format ( ' Exit  "LINIMP".   IERR =',  i8  )              
      return
      end
