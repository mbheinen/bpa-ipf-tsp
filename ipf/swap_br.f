C    @(#)swap_br.f	20.3 2/13/96
        subroutine swap_br (i,j)

        common /scratch/ nbr, array(2,100)
        integer array, p, q

        do p = 1, 2
           q = array(p,i)
           array(p,i) = array(p,j)
           array(p,j) = q
        end do
        return
        end
