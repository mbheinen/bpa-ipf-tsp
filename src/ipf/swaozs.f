C    @(#)swaozs.f	20.3 2/13/96
      subroutine swaozs (m,n)
C
C     swap OUTPUT SORT ORDER BY ZONES
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/asort.inc'
      include 'ipfinc/bus.inc'
C
      i=sorto(m)
      sorto(m)=sorto(n)
      sorto(n)=i
C
      i=barea(m)
      barea(m)=barea(n)
      barea(n)=i

      return
      end
