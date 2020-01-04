C    @(#)fcvcmb.f	20.3 2/13/96
      subroutine fcvcmb (a,i,b,j,c,k,n)

      real a(*),b(*),c(*)

      ia = 1
      ib = 1
      ic = 1
      do 100 l=1,n
         c(ic) = a(ia)
         c(ic+1) = b(ib)
         ia = ia + i
         ib = ib + j
         ic = ic + k
  100 continue
      return
      end
