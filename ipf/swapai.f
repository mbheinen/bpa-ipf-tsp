C    @(#)swapai.f	20.3 2/13/96
        subroutine swapai (i,j)
 
C       This subroutine exchanges two "I" intertie entities ARCINT(*,I) 
c       and ARCINT(*,J).
 
        include 'ipfinc/parametr.inc'
        include 'ipfinc/arcntl.inc'
 
        character tempc*10
 
        tempc = arcint(1,i)
        arcint(1,i) = arcint(1,j)
        arcint(1,j) = tempc
        tempc = arcint(2,i)
        arcint(2,i) = arcint(2,j)
        arcint(2,j) = tempc
        temp = arcinp(i)
        arcinp(i) = arcinp(j)
        arcinp(j) = temp
 
        return
        end
