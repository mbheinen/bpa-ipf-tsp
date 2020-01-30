C    @(#)chbsis.f	20.3 2/13/96
      subroutine chbsis
 
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/smallp.inc'

      double precision boundj, dritem, rl, rr, si, xofneg, xxx
 
      itr = itr + 1
      mosneg = 0
      howneg = 0.0
      xofneg = 0.0
      dritem = 0.0
      tol1 = tol(1)
      if (inrev.eq.1) go to 90
      if (r.eq.0.0) go to 40
      do 30 k = 1,size
      xr(k) = xr(k) - r * gr(k) * xkpos
      xxx = xr(k)
      if (abs(xxx).le.tol1) xr(k) = 0.0
      j = xbasis(k)
      if (j.le.n) go to 10
      xxx = xr(k)
      i = j - n
      si = s(i)
      if (si .eq. 0.0 .and. xxx .gt. 0.0 .or. si .eq. -1.0) xxx = -xxx
      go to 20

   10 boundj = bound(j)
      if (abs(boundj-xxx).le.tol1) xr(k) = boundj
      xxx = xr(k)
      if (xxx .le. boundj. or. boundj .eq. -1.0) go to 20
      xxx = boundj - xxx

   20 if (k.eq.neginv) xofneg = xxx
      if (xxx.ge.howneg.or.k.eq.neginv) go to 30
      mosneg = k
      dritem = 1.0
      if (xr(k).ge.0.0) dritem =-1.0
      howneg = xxx

   30 continue
      if (newy.ne.-1) go to 40
      it = inbase(newx)
      inbase(newx) = -1
      if (it.eq.-1) inbase(newx) = 0
      ixout = newx
      obj = obj - r * yaminc
      go to 120

   40 ixout = xbasis(newy)
      if (ixout.gt.n) go to 50
      inbase(ixout) = 0
      if (gr(newy) * xkpos.lt.0.0.and.newy.ne.neginv) inbase(ixout) = -1
      if (newy.eq.neginv.and.xr(newy).gt.0.0) inbase(ixout) = -1
   50 if (newx.gt.n) go to 60
      ihold = inbase(newx)
      inbase(newx) = newy
   60 xbasis(newy) = newx
      if (newx.gt.n) numslk = numslk + 1
      if (ixout.gt.n) numslk = numslk - 1
      xr(newy) = r
      if (newx.le.n) go to 70
      i = newx - n
      if (s(i).eq.-1.0) xr(newy) = -r
      go to 80

   70 if (ihold.eq.-1) xr(newy) = bound(newx) - r
   80 obj = obj - r * yaminc
   90 rr = 1.0/ gr(newy)
      do 110 l = 1,size
         if (abs(inv(newy,l)).lt.small) go to 110
         rl = inv(newy,l) * rr
         do 100 k = 1,size
            inv(k,l) = inv(k,l) - rl * gr(k)
  100    continue
         inv(newy,l) = rl
         if (inrev.ne.1) yr(l) = yr(l) - rl * yaminc * xkpos
  110 continue

  120 if (r.eq.0.0 .or. xofneg.lt.0.0 .and. newy.ne.neginv) go to 130
      neginv = mosneg
      driver = dritem

  130 return
      end
