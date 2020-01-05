C    @(#)swpgd1.f	20.3 2/13/96
      subroutine swpgd1(m,n)
C
      include 'ipfinc/parametr.inc'
      include 'ipfinc/gendrp.inc'
C
      itemp = gndpno(m)
      gndpno(m) = gndpno(n)
      gndpno(n) = itemp
 
      itemp = gndpty(m)
      gndpty(m) = gndpty(n)
      gndpty(n) = itemp
 
      temp = gndpol(m)
      gndpol(m) = gndpol(n)
      gndpol(n) = temp
 
      temp = gndpmn(m)
      gndpmn(m) = gndpmn(n)
      gndpmn(n) = temp
 
      temp = gndpmx(m)
      gndpmx(m) = gndpmx(n)
      gndpmx(n) = temp
 
      return
      end
