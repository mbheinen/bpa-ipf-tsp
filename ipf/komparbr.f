C    @(#)komparbr.f	20.3 2/13/96
        integer function komparbr (i,j)

        include 'ipfinc/qsdup.inc'
 
        common /scratch/ nbr, array(2,100)
        integer array

        komparbr = array(2,i) - array(2,j)
        if ((komparbr .eq. 0) .and. (i .ne. j)) dupsw = .true.

        return
        end
