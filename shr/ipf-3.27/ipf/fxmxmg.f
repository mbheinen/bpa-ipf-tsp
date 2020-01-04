C    @(#)fxmxmg.f	20.3 2/13/96
      subroutine fxmxmg (a,i,c,n)
      real a(*),c(*)

      ia = 1
      c(1) = 0.
      c(2) = 0.
      do 100 l=1,n
         x = abs(a(ia))
         if (x .gt. c(1)) then
            c(1) = x
            c(2) = float(l)
         endif
         ia = ia + i
  100 continue
      return
      end
