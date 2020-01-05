C    @(#)spx_cmde2.f	20.1 8/20/98
      subroutine spx_cmde2 (m,n)

C     This subroutine swaps indices for in the branch overload 
C     auxiliary index array.

      include 'ipfinc/parametr.inc'
      include 'ipfinc/cmde_com.inc'
 
      i = xsort(m)
      xsort(m) = xsort(n)
      xsort(n) = i
 
      return
      end
