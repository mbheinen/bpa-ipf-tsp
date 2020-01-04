C    @(#)swpepc.f	20.3 2/13/96
      subroutine swpepc (i, j)
C
      include 'ipfinc/epridc.inc'
 
      character temp * 120
 
      temp = epctl(i)
      epctl(i) = epctl(j)
      epctl(j) = temp
      return
      end
