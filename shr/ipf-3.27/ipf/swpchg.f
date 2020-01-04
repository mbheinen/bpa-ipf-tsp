C    @(#)swpchg.f	20.3 2/13/96
        subroutine swpchg (i1,i2) 

C       swap change records 

        include 'ipfinc/parametr.inc'
        include 'ipfinc/changr.inc'

        common /scratch/ array(MAXCHG), newarray(MAXCHG)
        integer array

        character chgtmp*126
c
        chgtmp = chgcrd(i1)
        chgcrd(i1) = chgcrd(i2)
        chgcrd(i2) = chgtmp

        itemp = array(i1)
        array(i1) = array(i2)
        array(i2) = itemp

        return
        end
