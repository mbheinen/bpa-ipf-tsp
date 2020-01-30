C    @(#)reduce.f	20.3 2/13/96
      subroutine reduce
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/smallp.inc'


      double precision si
 
      marki = 0
      markk = 0
      if (numslk.eq.0) go to 80
      it = size
      do 60 k = 1,it
   10 if (size.le.1) go to 70
      j = xbasis(k)
      if (j.le.n) go to 60
      i = j - n
      si = s(i)
      if (si * xr(k).lt.0.0.or.si.eq.0.0.and.xr(k).ne.0.0) go to 60
      if (k.eq.size) go to 30
      do 20 l = 1,size
   20 inv(k,l) = inv(size,l)
      j = xbasis(size)
      xbasis(k) = j
      if (j.le.n) inbase(j) = k
   30 slack(i) = xr(k)
      xr(k) = xr(size)
      if (neginv.eq.size) neginv = k
      l = iseff(i)
      iseff(i) = 0
      if (l.eq.size) go to 50
      do 40 kk = 1,size
   40 inv(kk,l) = inv(kk,size)
      yr(l) = yr(size)
      ybasis(l) = ybasis(size)
      i = ybasis(size)
      iseff(i) = l
   50 xbasis(size) = 0
      ybasis(size) = 0
      size = size - 1
      size1 = size1 - 1
      numslk = numslk - 1
      go to 10

   60 continue
   70 if (size.lt.2.and.xbasis(1).gt.n) mark = 1
      if (neginv.eq.0.and.markk.eq.0) go to 80
      j = 0
      if (neginv.ne.0) j = xbasis(neginv)
      if (j.gt.n) markk = neginv
      if (markk.eq.0) go to 80
      marki = xbasis(markk) - n
   80 return
      end
