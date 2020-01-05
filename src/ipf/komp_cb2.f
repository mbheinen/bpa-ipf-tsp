C    @(#)komp_cb2.f	20.3 2/13/96
        integer function komp_cb2 (i,j)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/cbsorc.inc'

        komp_cb2 = kompr (cbkey(i), cbkey(j), komp_cb2)
        return
        end
