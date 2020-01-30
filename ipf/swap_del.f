C    @(#)swap_del.f	20.3 2/13/96
        subroutine swap_del (i1,i2) 
c
C       swap delete() records 

        include 'ipfinc/parametr.inc'
        include 'ipfinc/delete.inc'

        character chgtmp*120
c
        chgtmp = delete(i1)
        delete(i1) = delete(i2)
        delete(i2) = chgtmp

        return
        end
