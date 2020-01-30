C    @(#)swpred.f	20.3 2/13/96
      subroutine swpred (m,n)
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/bus.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/red7.inc'
      include 'ipfinc/sortbs.inc'
 
      double precision xk

      go to (90,100,100,120) key
 
   90 i = nbsort(m)
      nbsort(m) = nbsort(n)
      nbsort(n) = i
      go to 900
 
  100 do 110 i = 1,3
         xk = yred(i,m)
         yred(i,m) = yred(i,n)
         yred(i,n) = xk
  110 continue
      go to 900
 
  120 do 130 i = 1,3
      xk = ymtrx(i,m)
      ymtrx(i,m) = ymtrx(i,n)
  130 ymtrx(i,n) = xk
 
  900 continue
      return
      end
