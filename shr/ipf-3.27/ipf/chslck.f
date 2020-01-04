C    @(#)chslck.f	20.3 2/13/96
      subroutine chslck
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/aref.inc'
      include 'ipfinc/smallp.inc'
 
 
        double precision abslki, aij, si, slki, xj, yacj, yi, geta
 
      if (r .ne. 0.0) negrow = 0
      howneg = 0.0
      do 100 j = 1, n
         yacj = 0.0
         k = inbase(j)
         if (k .le. 0) yacj = -c(j)
         yac(j) = yacj
         xj = 0.0
         if (k .eq. -1) xj = bound(j)
         if (k .gt. 0) xj = xr(k)
         x(j) = xj
  100 continue
 
      tol2 = tol(2)
 
      do 160 i = 1, mnow
         l = iseff(i)
         y(i) = 0.0
         if (l .eq. 0) go to 120
         yi = yr(l)
         y(i) = yi
         slack(i) = 0.0
         last = irow(i+1) - 1
         istart = irow(i)
 
         do 110 look = istart, last
            j = jcol (look)
            if (inbase(j) .gt. 0) go to 110
            aij = aa(look)
            yac(j) = yac(j) + yi * aij
  110    continue
         go to 160

  120    if (inrev .ne. 1) go to 140
         slki = b(i)
         do 130 j = 1, n
            if (inbase(j) .eq. 0) go to 130
            slki = slki - geta(i,j) * x(j)
  130    continue
         go to 150

  140    if (r .eq. 0.0) go to 160
         slki = slack(i) - r * g(i) * xkpos
  150    if (abs(slki) .le. tol2) slki = 0.0
         slack(i) = slki
         if (neginv .ne. 0) go to 160
         si = s(i)
         abslki = abs(slki)
         if (si .ne. 0.0 .and. si*slki .ge. howneg .or. si .eq. 0.0
     1      .and. -abslki .ge. howneg) go to 160
         howneg = -abslki
         negrow = i
  160 continue
      inrev = 0
      if (marki .ne. 0) slack(marki) = xr(markk)
      return
      end
