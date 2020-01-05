C    @(#)kp_outcmd2.f	20.1 8/20/98
        function kp_outcmd2 (m,n)

C       This function performs ownership sort in the branch outage 
C       index array.
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/cmde_com.inc'
        include 'ipfinc/comm_mode.inc'
 
        character id1*1, id2*1, own1*3, own2*3
 
        kp_outcmd2 = isort(m) - isort(n)

        return
        end
