C    @(#)kmpgd1.f	20.3 2/13/96
      function kmpgd1 (m,n)
C
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/bus.inc'
      include 'ipfinc/gendrp.inc'
C
      kmpgd1 = gndpno(m) - gndpno(n)
      return
      end
