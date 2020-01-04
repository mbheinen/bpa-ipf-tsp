C    @(#)revert.f	20.3 2/13/96
      subroutine revert
 
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/smallp.inc'
 
 
      double precision abdif, best, hold, t, tb, tc, xrk, geta
 
 9000 format (1h ,20x,'REINVERTED AT ITERATION ',i6)
 
      ir = ir + 1
      ithold = itr
      inrev = 1
      hold = small
      small = 0.0
      tol8 = tol(8)
      do 20 k = 1,size
   10 if (xbasis(k) .le. n) go to 20
      i = xbasis(k) - n
      l = iseff(i)
      if (k .eq. l) go to 20
      xbasis(k) = xbasis(l)
      xbasis(l) = i + n
      j = xbasis(k)
      if (j .gt. n) go to 10
   20 continue
      do 40 k = 1,size
      if (xbasis(k) .le. n) xbasis(k) = -xbasis(k)
      do 30 l = 1,size
   30 inv(k,l) = 0.0
   40 inv(k,k) = 1.0
      do 50 j = 1,n
      if (inbase(j) .ne. -1) inbase(j) = 0
   50 continue
      do 90 k = 1,size
      newx = -xbasis(k)
      if (newx .lt. 0 .or. newx .gt. n) go to 90
   60 call newvec
      newy = 0
      do 80 kk = 1,size
      if (xbasis(kk) .gt. 0) go to 80
      abdif = abs(gr(kk))
      if (abdif .lt. tol8) go to 80
      if (newy .ne. 0) go to 70
      best = abs(1.0 - abdif)
      newy = kk
      go to 80

   70 abdif = abs(1.0 - abdif)
      if (abdif .ge. best) go to 80
      best = abdif
      newy = kk

   80 continue
      if (newy .eq. 0) go to 90
      ihold = -xbasis(newy)
      if (ihold .eq. newx) ihold = 0
      call chbsis
      xbasis(newy) = newx
      if (ihold .eq. 0) go to 90
      newx = ihold
      go to 60

  90  continue
      numslk = 0
      do 110 k = 1,size
      j = xbasis(k)
      if (j .gt. 0) go to 100
      i = ybasis(k)
      xbasis(k) = n + i
      numslk = numslk + 1
      go to 110

 100  if (j .le. n) inbase(j) = k
      if (j.gt.n) numslk = numslk + 1
 110  continue
      small = hold
      do 120 k = 1,size
      xr(k) = 0.0
 120  yr(k) = 0.0
      do 140 k = 1,size
      i = ybasis(k)
      j = xbasis(k)
      tc = 0.0
      if (j .le. n) tc = c(j)
      tb = b(i)
      do 130 jj = 1,n
         if (inbase(jj) .ne. -1) go to 130
         tb = tb - bound(jj) * geta(i,jj)
 130  continue
      do 140 l = 1,size
         xr(l) = xr(l) + tb * inv(l,k)
         yr(l) = yr(l) + tc * inv(k,l)
         if (abs(yr(l)).le. small) yr(l) = 0.0
         if (abs(xr(l)) .le. small) xr(l) = 0.0
 140  continue
      neginv = 0
      t = 0.0
      do 180 k = 1,size
      xrk = xr(k)
      j = xbasis(k)
      if (j .gt. n) go to 160
      if (isbnd .eq. 0) go to 150
      if (bound(j) .eq. -1.0) go to 150
      if (xrk .gt. bound(j)) xrk = bound(j) - xr(k)
 150  if (xrk .ge. t) go to 180
      go to 170
 160  i = j - n
      if (s(i) .ne. 0.0 .and. xrk * s(i) .ge. t .or. s(i) .eq. 0.0 .and.
     1   abs(xrk) * (-1.0) .ge. t) go to 180
 170  t = -1.0 * abs(xrk)
      neginv = k
      driver = 1.0
      if (xr(k) .gt. 0.0) driver = -1.0
 180  continue
      if (numslk.ge.1) call reduce
      call chslck
      call isopt
      itr = ithold
      inrev = 0
      obj = 0.0
      do 190 j = 1,n
         if (inbase(j) .eq. 0) go to 190
         obj = obj + x(j) * c(j)
 190  continue
      write (dbug,9000) itr
      return
      end
