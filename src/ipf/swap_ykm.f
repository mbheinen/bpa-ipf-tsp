C    @(#)swap_ykm.f	20.3 2/13/96
        subroutine swap_ykm (m,n)
C
        include 'ipfinc/parametr.inc'
        include 'ipfinc/alpha.inc'
C
        itemp = ikmu(m)
        ikmu(m) = ikmu(n)
        ikmu(n) = itemp

        temp = gkmu(m)
        gkmu(m) = gkmu(n)
        gkmu(n) = temp

        temp = bkmu(m)
        bkmu(m) = bkmu(n)
        bkmu(n) = temp

        return
        end
