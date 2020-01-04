C    @(#)swpgd2.f	20.3 2/13/96
      subroutine swpgd2 (m,n)
C
      include 'ipfinc/parametr.inc'
      include 'ipfinc/gendrp.inc'
C
      itemp = gennum(m)
      gennum(m) = gennum(n)
      gennum(n) = itemp
 
      itemp = gentyp(m)
      gentyp(m) = gentyp(n)
      gentyp(n) = itemp
 
      temp = genpol(m)
      genpol(m) = genpol(n)
      genpol(n) = temp
 
      return
      end
