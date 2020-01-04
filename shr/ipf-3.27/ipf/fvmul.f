C    @(#)fvmul.f	20.3 2/13/96
      subroutine fvmul (a,i,b,j,c,k,n)
      real a(*),b(*),c(*)
C
C     This subroutine multiplies the Ith element of A and the
c     Ith element of B and store the result in the Ith element
c     of C

      ia = 1
      ib = 1
      ic = 1
      do 100 l=1,n
         c(ic) = a(ia) * b(ib)
         ia = ia + i
         ib = ib + j
         ic = ic + k
  100 continue
      return
      end
