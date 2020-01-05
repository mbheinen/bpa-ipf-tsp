C    @(#)fvdiv.f	20.3 2/13/96
      subroutine fvdiv (a,i,b,j,c,k,n)
      real a(*),b(*),c(*)

      ia = 1
      ib = 1
      ic = 1
      do 100 l=1,n
         c(ic) = b(ib) / a(ia)
         ia = ia + i
         ib = ib + j
         ic = ic + k
  100 continue
      return
      end
