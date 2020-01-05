C    @(#)kmpmtx.f	20.3 2/13/96
        function kmpmtx (m,n)
C
      include 'ipfinc/parametr.inc'
 
        common /mtrx/ mtrx(2,MAXBUS)
 
        kmpmtx = mtrx(2,m) - mtrx(2,n)
        if( kmpmtx .eq. 0 ) kmpmtx = mtrx(1,m) - mtrx(1,n)
        return
        end
