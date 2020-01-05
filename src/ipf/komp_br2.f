C    @(#)komp_br2.f	20.3 2/13/96
        integer function komp_br2 (i,j)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/qsdup.inc'
 
        common /scratch/ nbr, array(2,MAXBRN)
        integer array

        komp_br2 = array(1,i) - array(1,j)
        if (komp_br2 .eq. 0) komp_br2 = array(2,i) - array(2,j)
        if ((komp_br2 .eq. 0) .and. (i .ne. j)) dupsw = .true.

        return
        end
