C    @(#)swpepb.f	20.3 2/13/96
      subroutine swpepb (i, j)
C
      include 'ipfinc/epridc.inc'
 
      character temp * 80
 
      temp = epbus(i)
      epbus(i) = epbus(j)
      epbus(j) = temp
      return
      end
