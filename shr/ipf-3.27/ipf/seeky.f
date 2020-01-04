C    @(#)seeky.f	20.3 2/13/96
      subroutine seeky
 
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/aref.inc'
      include 'ipfinc/smallp.inc'
 
 
      double precision boundj, gi, gk, rtry, si, slacki, t,
     1                 xrneg, xx
 
      si = 1.0
      boundj = -1.0
      r = big
      newy = 0
      tol5 = tol(5)
      if (isbnd.eq.0.or.newx.gt.n) go to 10
      if (bound(newx).eq.-1.0) go to 10
      r = bound(newx)
      newy = -1
  10  if (neginv.eq.0) go to 30
      xrneg = xr(neginv)
      j = xbasis(neginv)
      if (j .eq. 0 .or. j.gt.n) go to 20
      boundj = bound(j)
      if (boundj.ge.xrneg.or.boundj.eq.-1.0) go to 20
      xrneg = xrneg - boundj
  20  if (gr(neginv) .ne. 0.0) then
         rtry = xrneg / (xkpos * gr(neginv))
         if (rtry.gt.r) go to 30
         r = rtry
         if (r.le.small) r = 0.0
         newy = neginv
         if (r.eq.0.0) go to 140
      endif
  30  do 90 k = 1,size
      if (k.eq.neginv) go to 90
      gk = gr(k) * xkpos
      if (abs(gk).le.tol5) go to 90
      j = xbasis(k)
      if (j.gt.n) si = s(j-n)
      if (j.le.n) boundj = bound(j)
      xx = xr(k)
      if (gk.le.0.0) go to 70
      if (xx.lt.0.0) go to 90
      if (j.le.n.and.boundj.eq.-1.0.or.j.le.n.and.xx.le.boundj) go to 40
      if (j.gt.n.and.si.eq.1.0) go to 40
      go to 90

  40  if (xx.ge.gk * r) go to 90
  50  r = xx / gk
  60  if (r.le.small) r = 0.0
      newy = k
      if (r.eq.0.0) go to 140
      go to 90

  70  if (j.gt.n) go to 80
      if (boundj.eq.-1.0.or.xx.lt.0.0.or.xx.gt.boundj) go to 90
      if ((xx - gk * r) .le. boundj) go to 90
      r = (boundj - xx) / (-1.0 * gk)
      go to 60

  80  if (xx.ge.0.0.or.s(j-n).ge.0.0) go to 90
      if ((xx - gk * r) .le. 0.0) go to 90
      go to 50

  90  continue
      do 130 i = 1,mnow
      if (iseff(i).eq.0) go to 100
      g(i) = 0.0
      go to 130

 100  slacki = slack(i)
      si = s(i)
      gi = 0.0
      istart = irow(i)
      last = irow(i+1) - 1
      do 120 look = istart,last
      j = jcol(look)
      inj = inbase(j)
      if (inj.le.0) go to 110
      gi = gi - aa(look ) * gr(inj)
      go to 120

 110  if (j.eq.newx) gi = gi + aa(look)

 120  continue
      g(i) = gi
      if (abs(gi).le.tol5) go to 130
      if (si.eq.0.0.and.slacki.ne.0.0) go to 130
      if (si * slacki .lt. 0.0) go to 130
      gi = gi * xkpos
      t = slacki - gi * r
      if (t.ge.0.0 .and. si.eq.1.0 .or. t.le.0.0 .and. si.eq.-1.0)
     1   go to 130
      r = slacki / gi
      if (r.le.small) r = 0.0
      newy = size + i
      gr(size1) = gi * xkpos
      if (r.eq.0.0) go to 140
 130  continue

 140  return
      end
