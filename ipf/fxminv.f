C    @(#)fxminv.f	20.3 2/13/96
      subroutine fxminv(a,i,c,n)
      real a(*),c(*)

      c(1) = a(1)
      c(2) = 1.0
      ia = 1
      do 100 l=1,n
         x = a(ia)
         if (x .le. c(1)) then
            c(1) = x
            c(2) = float(l)
         endif
         ia = ia + i
  100 continue
      return
      end
