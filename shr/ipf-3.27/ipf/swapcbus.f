C    @(#)swapcbus.f	20.3 2/13/96
        subroutine swapcbus (i,j)

        common /scratch/ ncbs, array(100), array_c(100)
        character array_c * 8, temp * 8
        integer array, p

        p = array(i)
        array(i) = array(j)
        array(j) = p

        temp = array_c(i)
        array_c(i) = array_c(j)
        array_c(j) = temp

        return
        end
