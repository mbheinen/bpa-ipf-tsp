C    @(#)mover.f	20.3 2/13/96
      subroutine mover ( from, to, n17 )
      implicit real * 8 (a-h, o-z), integer * 4 (i-n)
      dimension from(*), to(*)

      do 100 j = 1, n17
  100 to(j) = from(j)
      return
      end
 
