C    @(#)swpvlimit.f	20.3 2/13/96
        subroutine swpvlimit (i, j)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/sortuvov.inc'

        temp = v_max(i)
        v_max(i) = v_max(j)
        v_max(j) = temp

        temp = v_min(i)
        v_min(i) = v_min(j)
        v_min(j) = temp

        temp = v_range(1,i)
        v_range(1,i) = v_range(1,j)
        v_range(1,j) = temp

        temp = v_range(2,i)
        v_range(2,i) = v_range(2,j)
        v_range(2,j) = temp

        return
        end
