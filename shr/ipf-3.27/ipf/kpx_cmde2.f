C    @(#)kpx_cmde2.f	20.4 5/27/99
      function kpx_cmde2 (m,n)

C     This function performs bus sort in the branch overload auxiliary 
C     index array.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/cmde_com.inc'
 
      if (m .eq. n) then
         kpx_cmde2 = 0
      else
         i = xsort(m)
         j = xsort(n)
         ix = ibrolp(1,i)
         jx = ibrolp(1,j)
         kpx_cmde2 = ix - jx
      endif
      return
      end
