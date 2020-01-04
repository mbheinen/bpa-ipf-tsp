C    @(#)kmpgd2.f	20.3 2/13/96
      function kmpgd2 (m,n)
C
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/bus.inc'
      include 'ipfinc/gendrp.inc'
C
      kmpgd2 = gennum(m) - gennum(n)
      return
      end
