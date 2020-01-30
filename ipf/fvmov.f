C    @(#)fvmov.f	20.3 2/13/96
      subroutine fvmov (a,i,c,k,n)
      real a(*),c(*)

      ia = 1
      ic = 1
      do 100 l=1,n
         c(ic) = a(ia)
         ia = ia + i
         ic = ic + k
  100 continue
      return
      end
