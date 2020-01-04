C    @(#)swpmtx.f	20.3 2/13/96
        subroutine swpmtx (m,n)
C
      include 'ipfinc/parametr.inc'
 
        common /mtrx/ mtrx(2,MAXBUS)
 
        do 100 i = 1,2
           itemp = mtrx(i,m)
           mtrx(i,m) = mtrx(i,n)
           mtrx(i,n) = itemp
  100   continue
        return
        end
