C    @(#)swpkol.f	20.3 2/13/96
      subroutine swpkol (i,j)
C
      include 'ipfinc/parametr.inc'
      include 'ipfinc/renum.inc'
C
      itemp = kolum1(i)
      kolum1(i) = kolum1(j)
      kolum1(j) = itemp
      return
      end
