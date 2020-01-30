C    @(#)kompai.f	20.3 2/13/96
        integer function kompai (i,j)
 
C       This subroutine determines the relative sorting order of two
C       "I" intertie entities ARCINT(*,I) and ARCINT(*,J)
 
        include 'ipfinc/parametr.inc'
        include 'ipfinc/arcntl.inc'
 
        if ( i .eq. j) then
           kompai = 0
        else
           kompai = kompr (arcint(1,i),arcint(1,j),junk)
           if (kompai .eq. 0) then
              kompai = kompr (arcint(2,i),arcint(2,j),junk)
           endif
        endif
        return
        end
