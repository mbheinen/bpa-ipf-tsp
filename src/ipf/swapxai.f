C    @(#)swapxai.f	20.3 2/13/96
        subroutine swapxai (i,j)
 
C       This subroutine exchanges two "I" intertie entities OARCINT(*,I) 
c       and OARCINT(*,J).
 
        include 'ipfinc/parametr.inc'
        include 'ipfinc/alt_case.inc'
 
        character tempc*10
 
        tempc = oarcint(1,i)
        oarcint(1,i) = oarcint(1,j)
        oarcint(1,j) = tempc
        tempc = oarcint(2,i)
        oarcint(2,i) = oarcint(2,j)
        oarcint(2,j) = tempc
        temp = oarcinp(i)
        oarcinp(i) = oarcinp(j)
        oarcinp(j) = temp
 
        return
        end
