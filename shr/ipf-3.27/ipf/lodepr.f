C    @(#)lodepr.f	20.7 11/12/98
        subroutine lodepr (zonex, num)
 
C       This subroutine processes EPRI 1964 input.
C       The only  /COMMON/  block is /EPRIDC/.
C
        include 'ipfinc/dcparm.inc'

        include 'ipfinc/epridc.inc'
        include 'ipfinc/grlf01.inc'

        character zonex(*) * (*)
        character zones(100) * 4
        integer izones(100)
        equivalence (zones, izones)
        integer errcon, errlin, errbrk, errpgm, num,    errcc
C
C       Process EPRI_1964 data.
C
        if (nepbus .gt. 0) then
           call readdc (nepbus, ndccon, ndclin, ndcbrk, errcon,
     1                  errlin, errbrk, errpgm)
           call zonedc (izones, num)
C
C          Convert I4 ZONES to C2 ZONEX.
C
           do 100 i = 1, num
  100      zonex(i) = zones(i)
        endif
        if (nepctl .gt. 0) call csread (nepctl, errcc)
        if (nepbus + nepctl .gt. 0) then
           call getare
        endif
 
        return
        end
