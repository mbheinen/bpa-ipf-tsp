C    @(#)swap_br2.f	20.3 2/13/96
        subroutine swap_br2 (i,j)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/branch.inc'
        include 'ipfinc/qsdup.inc'
 
        common /scratch/ nbr, array(2,MAXBRN)
        integer array

        integer p, q
        real r

        do p = 1, 2
           q = array(p,i)
           array(p,i) = array(p,j)
           array(p,j) = q
        end do

        do p = 1, 18
           q = kbrnch(p,i)
           kbrnch(p,i) = kbrnch(p,j)
           kbrnch(p,j) = q
        enddo

        do p = 1, 3
           r = rateln(p,i)
           rateln(p,i) = rateln(p,j)
           rateln(p,j) = r
        enddo

        return
        end
