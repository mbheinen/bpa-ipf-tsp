C    @(#)swp_ptiz.f	20.5 10/13/99
        subroutine swp_ptiz (i,j)

        include 'ipfinc/parametr.inc'

        common /scratch/ nbr, array(4,MAXBUS)
        integer array, p, q

        do p = 1, 4
           q = array(p,i)
           array(p,i) = array(p,j)
           array(p,j) = q
        end do
        return
        end
