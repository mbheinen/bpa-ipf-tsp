C    @(#)lkbrdata.f	20.3 2/13/96
        subroutine lkbrdata (indx, k1, k2, bptr1, bptr2, error)

        integer indx, k1, k2, bptr1, bptr2, error

        include 'ipfinc/parametr.inc'

        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'

        integer p

        p = kbsdta(16,k1)
        ltot2 = ltot2 + 1
        brnch_ptr(ltot2) = indx
        brnch_nxt(ltot2) = p
        kbsdta(16,k1) = ltot2
        bptr1 = ltot2

        p = kbsdta(16,k2)
        ltot2 = ltot2 + 1
        brnch_ptr(ltot2) = -indx
        brnch_nxt(ltot2) = p
        kbsdta(16,k2) = ltot2
        bptr2 = ltot2

        return
        end
