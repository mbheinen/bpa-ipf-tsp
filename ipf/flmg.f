C    @(#)flmg.f	20.3 2/13/96
      subroutine flmg (a,i,b,j,c,k,n)

      integer b(*)
      real a(*),c(*)

      ia = 1
      ib = 1
      ic = 1
      do 100 m=1,n
         if (b(ib) .ne. 0) then
            c(ic) = a(ia)
         else
            c(ic) = 0.0
         endif
         ia = ia + i
         ib = ib + j
         ic = ic + k
  100 continue
      return
      end
