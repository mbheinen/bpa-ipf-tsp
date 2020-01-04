C    @(#)spxsrt.f	20.3 2/13/96
      subroutine spxsrt (m,n)

C     This subroutine swaps indices for in the branch overload 
C     auxiliary index array.

      include 'ipfinc/parametr.inc'
      include 'ipfinc/apcom.inc'
 
      i = xsort(m)
      xsort(m) = xsort(n)
      xsort(n) = i
 
      return
      end
