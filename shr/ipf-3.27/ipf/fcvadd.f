C    @(#)fcvadd.f	20.3 2/13/96
      subroutine fcvadd (a,i,b,j,c,k,n)

      real a(*),b(*),c(*)

      ia = 1
      ib = 1
      ic = 1
      do 100 l=1,n
         c(ic) = a(ia) + b(ib)
         c(ic+1) = a(ia+1) + b(ib+1)
         ia = ia + i
         ib = ib + j
        ic = ic + k
  100 continue
      return
      end
