C    @(#)kmprat.f	20.3 2/13/96
        function kmprat (i,j)
C
C       compare the default ratings in array ZRAT.
C
        include 'ipfinc/dflrat.inc'
C
        xkomp = zrat(1,i) - zrat(1,j)
        if (xkomp.eq.0) xkomp = zrat(2,i) - zrat(2,j)
        kmprat = 100.0*xkomp
        return
        end
