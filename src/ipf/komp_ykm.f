C    @(#)komp_ykm.f	20.3 2/13/96
        integer function komp_ykm (m,n)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/alpha.inc'

        komp_ykm = ikmu(m) - ikmu(n)
        return
        end
