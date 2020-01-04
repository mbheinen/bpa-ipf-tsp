C    @(#)isopt.f	20.3 2/13/96
      subroutine isopt
 
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/smallp.inc'

      double precision si, t, yrl
 
      yaminc = - tol(3)
      newx = 0
      do 10 l = 1,size
         i = ybasis(l)
         si = s(i)
         if (si.eq.0.0) go to 10
         yrl = yr(l) * si
         if (yrl.ge.yaminc) go to 10
         yaminc = yrl
         newx = i + n
   10 continue
      tol4 = tol(4)
      do 20 j = 1,n
         inbj = inbase(j)
         if (inbj .gt. 0 .or. bound(j) .eq. 0.0) go to 20
         t = yac(j)
         if (abs(t).le.tol4) t = 0.0
         yac(j) = t
         if (inbj.eq.-1) t = -t
         if (t.ge.yaminc) go to 20
         yaminc = t
         newx = j
   20 continue
      return
      end
