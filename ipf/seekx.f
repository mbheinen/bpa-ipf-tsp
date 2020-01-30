C    @(#)seekx.f	20.3 2/13/96
      subroutine seekx
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/aref.inc'
      include 'ipfinc/smallp.inc'
 
 
      double precision  pivot, pivmax, func, bespiv, ratio,
     1                  aij, rinvl, si, sj, yacp, yi
 
      newx = 0
      r = -big
      pivmax = 0.0
      jmaxp = 0
      bespiv = 0.0
      tol3 = tol(3)
      tol4 = tol(4)
      tol5 = tol(5)
      do 10 j = 1,n
  10  piv(j) = 0.0
      do 40 l = 1,size
      i = ybasis(l)
      si = s(i)
      yi = yr(l) * si
      if (abs(yi).lt.tol3) yi = 0.0
      rinvl = inv(neginv,l)
      istart = irow(i)
      last = irow(i+1) - 1
      do 20 look = istart,last
      j = jcol(look)
      if (inbase(j) .ge. 1 .or. bound(j) .eq. 0.0) go to 20
      aij = aa(look)
      piv(j) = piv(j) + aij * rinvl
  20  continue
      if (si .eq. 0.0) go to 40
      pivot = rinvl * si * driver
      if (pivot .ge. -tol5 .or. pivot .ge. -0.5 .and. newx .ne. 0
     1     .and. yi .lt. 0.0) go to 40
      if (pivot .ge. -0.5 .and. yi .lt. 0.0) go to 30
      ratio = yi / pivot
      if (ratio .lt. r .and. newx .ne. 0) go to 40
      if (ratio .eq. 0.0 .and. pivot .ge. bespiv) go to 40
      if (ratio .eq. 0.0) bespiv = pivot
      r = ratio
      yaminc = yi
      newx = n + i
      go to 40

  30  if (pivot .ge. pivmax) go to 40
      yacp = yi
      jmaxp = n + i
      pivmax = pivot

  40  continue
      do 60 j = 1,n
      inj = inbase(j)
      if (inj .ge. 1 .or. bound(j) .eq. 0.0) go to 60
      sj = 1.0
      if (inj .eq. -1) sj = -1.0
      func = yac(j) * sj
      if (abs(func) .lt. tol4) func = 0.0
      pivot = piv(j) * sj * driver
      if (pivot .ge. -tol5 .or. pivot .ge. -0.5 .and. newx .ne. 0
     1     .and. func .lt. 0.0) go to 60
      if (pivot .ge. -0.5 .and. func .lt. 0.0) go to 50
      ratio = func / pivot
      if (ratio .lt. r .and. newx .ne. 0) go to 60
      if (ratio .eq. 0.0 .and. pivot .ge. bespiv) go to 60
      if (ratio .eq. 0.0) bespiv = pivot
      r = ratio
      yaminc = func
      newx = j
      go to 60

  50  if (pivot .ge. pivmax) go to 60
      pivmax = pivot
      yacp = func
      jmaxp = j

  60  continue
      if (newx .ne. 0) go to 70
      newx = jmaxp
      yaminc = yacp
      if (newx .ne. 0) r = yaminc / pivmax

  70  return
      end
