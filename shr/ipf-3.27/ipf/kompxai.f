C    @(#)kompxai.f	20.3 2/13/96
        integer function kompxai (i,j)
 
C       This subroutine determines the relative sorting order of two
C       "I" intertie entities OARCINT(*,I) and OARCINT(*,J)
 
        include 'ipfinc/parametr.inc'
        include 'ipfinc/alt_case.inc'
 
        if ( i .eq. j) then
           kompxai = 0
        else
           kompxai = kompr (oarcint(1,i), oarcint(1,j), junk)
           if (kompxai .eq. 0) then
              kompxai = kompr (oarcint(2,i), oarcint(2,j), junk)
           endif
        endif
        return
        end
