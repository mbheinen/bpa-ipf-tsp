C    @(#)kmpymx.f	20.3 2/13/96
        function  kmpymx (m,n)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/red7.inc'

        kmpymx = ymtrx(1,m) - ymtrx(1,n)
        return
        end
