C    @(#)swpclx.f	20.3 2/13/96
      subroutine swpclx (m,n)
C
      include 'ipfinc/parametr.inc'

      common /scratch/ kolum(MAXBUS), net(2,2*MAXREI), mtrx(MAXBUS)
C
      itemp = mtrx(m)
      mtrx(m) = mtrx(n)
      mtrx(n) = itemp
 
      return
      end
