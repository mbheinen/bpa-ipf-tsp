C    @(#)kmpvar.f	20.6 7/18/96
        function kmpvar (m,n)
C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/bus.inc'
        include 'ipfinc/pctvr2.inc'
C
        i = opt2inp(kpctvr(1,m))
        j = opt2inp(kpctvr(1,n))
        kmpvar = inp2alf(i) - inp2alf(j)
        return
        end
