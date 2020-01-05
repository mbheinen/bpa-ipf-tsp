C    @(#)linkcbus.f	20.3 2/13/96
        subroutine linkcbus (indx, error)
        integer indx, error

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'

        integer k1, p

        k1 = kbctbl(1,indx)
        p = kbsdta(15,k1)
        kbsdta(15,k1) = indx

        bctbl_nxt(indx) = p

        return
        end
