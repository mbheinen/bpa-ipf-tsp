C    @(#)fvfill.f	20.3 2/13/96
      subroutine fvfill(a,c,k,n)

      dimension c(1)

      ic = 1
      do 100 l=1,n
         c(ic) = a
         ic = ic + k
  100 continue
      return
      end
