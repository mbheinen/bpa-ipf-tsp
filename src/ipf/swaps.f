C    @(#)swaps.f	20.3 2/13/96
        subroutine swaps (m,n)
C
      include 'ipfinc/snput.inc'
C
        character zontmp*2
        zontmp = zondat(m)
        zondat(m) = zondat(n)
        zondat(n) = zontmp
        return
        end
