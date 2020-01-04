C    @(#)kmpkol.f	20.3 2/13/96
      function kmpkol (i,j)
C
      include 'ipfinc/parametr.inc'
      include 'ipfinc/renum.inc'
C
      kmpkol = kolum1(i) - kolum1(j)
      return
      end
