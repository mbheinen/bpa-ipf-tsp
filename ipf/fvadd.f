C    @(#)fvadd.f	20.3 2/13/96
      subroutine fvadd (a,i,b,j,c,k,n)
      real a(*),b(*),c(*)
C
C     This subroutine adds the elements of A to the elements of B
C     and store the result in vector C.
C
      ia = 1
      ib = 1
      ic = 1
      do 100 l=1,n
         c(ic) = a(ia) + b(ib)
         ia = ia + i
         ib = ib + j
         ic = ic + k
  100 continue
      return
      end
