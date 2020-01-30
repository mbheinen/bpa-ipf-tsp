C    @(#)kmpmps.f	20.3 2/13/96
        function kmpmps (i,j)
 
        include 'ipfinc/cut.inc'

        kmpmps = mpsort(1,i) - mpsort(1,j)
        if (kmpmps .eq. 0) kmpmps = mpsort(2,i) - mpsort(2,j)
        return
        end
