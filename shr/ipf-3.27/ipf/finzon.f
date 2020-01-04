C    @(#)finzon.f	20.3 2/13/96
        subroutine finzon (zone, base, outage, overld)
        character zone*2
        logical outage, overld
C
C       This subroutine determines whether a bus residing in ZONE
C       and BASE is a candidancy OUTAGE (=.TRUE.) or OVERLD (=.TRUE.).
C       If BASE is zero, only the zone is considered.
C
        include 'ipfinc/cont.inc'
        include 'ipfinc/zbdata.inc'
 
        outage = .false.
        overld = .false.
 
        if (nc .gt. 0) then
           do 100 i=1,nc
           if (zone .eq. znc(i)) then
              if (base .ge. vcl .and. base .le. vch) outage = .true.
              if (base .eq. 0.0) outage = .true.
              go to 120
           endif
  100      continue
        else
           if (base .ge. vcl .and. base .le. vch) outage = .true.
           if (base .eq. 0.0) outage = .true.
        endif
  120   continue
 
        if (no .gt. 0) then
           do 130 i=1,no
           if (zone .eq. zno(i)) then
              if (base .ge. vol .and. base .le. voh) overld = .true.
              if (base .eq. 0.0) overld = .true.
              go to 140
           endif
  130      continue
        else
           if (base .ge. vol .and. base .le. voh) overld=.true.
           if (base .eq. 0.0) overld = .true.
        endif
  140   continue
        return
        end
