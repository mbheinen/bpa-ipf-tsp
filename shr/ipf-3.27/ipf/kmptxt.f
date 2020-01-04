C    @(#)kmptxt.f	20.3 2/13/96
        function kmptxt(m,n)
C
        include 'ipfinc/parametr.inc'
        include 'ipfinc/optim1.inc'
C
C       Sort keys:
C
C       1. LTC index: 0, +i, -i
C       2. K1
C       3. K2
C
        if (txtie(7,m) .eq. 0) then
           i1 = 0
        else if (txtie(7,m) .gt. 0) then
           i1 = 1
        else
           i1 = 2
        endif
        if (txtie(7,n) .eq. 0) then
           i2 = 0
        else if (txtie(7,n) .gt. 0) then
           i2 = 1
        else
           i2 = 2
        endif
        kmptxt = i1 - i2
        if (kmptxt .eq. 0) kmptxt = txtie(1,m) - txtie(1,n)
        if (kmptxt .eq. 0) kmptxt = txtie(2,m) - txtie(2,n)
        return
        end
