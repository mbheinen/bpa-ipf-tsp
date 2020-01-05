C    @(#)glbvlt.f	20.3 2/13/96
        subroutine glbvlt(nb,vmin,vmax)
C
C       THIS SUBROUTINE OBTAINS GLOBAL VOLTAGE LIMITS
C       ONLY WHICH ARE APPLICABLE TO BUS "NB".
C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
C
        character zn*2

        vmax = 0.0
        vmin = 0.0
        do 150 i=1,nvlim
           if (base(nb) .ge. vlimit(1,i) .and. 
     &         base(nb) .le. vlimit(2,i)) then
              do 140 j=1,10
                 zn = vlimzn(j,i)
                 if (zn .eq. ' ') then
                    if (j .eq. 1) go to 170
                    go to 150
                 endif
                 if (zone(nb) .eq. zn) go to 170
  140         continue
           endif
  150   continue
        i = nvlim
  170   vmax = vlimit(4,i)
        vmin = vlimit(3,i)
        return
        end
