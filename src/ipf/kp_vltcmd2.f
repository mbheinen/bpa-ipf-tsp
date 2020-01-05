C    @(#)kp_vltcmd2.f	20.1 8/20/98
        function kp_vltcmd2 (m,n)

C       Compare ownership in the over/undervoltage index array.

        include 'ipfinc/parametr.inc'

        include 'ipfinc/cmde_com.inc'
        include 'ipfinc/bus.inc'
 
        if (m .eq. n) then
           kp_vltcmd2 = 0
        else
           i = isort(m)
           j = isort(n)
           if (kompr (owner(i), owner(j), kp_vltcmd2) .eq. 0) then
              kp_vltcmd2 = inp2alf(i) - inp2alf(j)
           endif
        endif
        return
        end
