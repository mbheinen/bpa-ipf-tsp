C    %W% %G%
C
C****************************************************************
C
C      File: chkfltro.f
C
C      Purpose: Routine to apply a filter to owner "own" 
C
C      Author: Walt Powell  Date: 22 Feb 1995
C      Called by: ownintchg.f
C
C****************************************************************
C
        logical function chkfltro (own) 
        character own * 3

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/zonehash.inc'
        include 'ipfinc/ownhash.inc'
        include 'ipfinc/bsekvhsh.inc'
        include 'ipfinc/zbo.inc'

        integer f1, f2, f3, f4, p, ownsch, find_zon, find_base
        logical accept(6)
	external ownsch, find_zon
c
c       Apply filters
c
        do j = 1, 4
           if (filter(j) .eq. 0) then
               accept(j) = .true.
           else
               accept(j) = .false.
           endif
        enddo
c
c       Check owner filter via areas (through zones)
c
        io = ownsch (own)
        if (io .le. 0) go to 900

        f1 = 1
        do while (f1 .le. filter(1) .and. .not. accept(1))
c
c          Check area filters through zones and then through
c          zn_ow()
c
           iz = 1
           do while (iz .le. nztot .and. .not. accept(1))
              if (arcnam(acznum(iz)) .eq. area_filter(f1)) then
                 p = zn_ow(iz)
                 do while (p .gt. 0 .and. (zbo(p) .lt. io))
                    p = zbo_nxt(p)
                 enddo
                 if (p .gt. 0 .and. (zbo(p) .eq. io)) then
                    accept(1) = .true.
                 endif
              endif
              iz = iz + 1
           enddo
           f1 = f1 + 1
        enddo
c
c       Check zone filters through zn_ow()
c
        f2 = 1
        do while (f2 .le. filter(2) .and. .not. accept(2))
           iz = find_zon (zone_filter(f2))
           p = zn_ow(iz)
           do while (p .gt. 0 .and. (zbo(p) .lt. io))
              p = zbo_nxt(p)
           enddo
           if (p .gt. 0 .and. (zbo(p) .eq. io)) then
              accept(2) = .true.
           endif
           f2 = f2 + 1
        enddo
c
c       Check owner filters directly
c
        f3 = 1
        do while (f3 .le. filter(3) .and. .not. accept(3))
           if (owner_filter(f3) .eq. own) then
               accept(3) = .true.
           endif
           f3 = f3 + 1
        enddo
c
c       Check base kv filters through bs_ow()
c
        f4 = 1
        do while (f4 .le. filter(4) .and. .not. accept(4))
           iz = find_base (voltage_filter(1,f4))
           p = bs_ow(iz)
           do while (p .gt. 0 .and. (zbo(p) .lt. io))
              p = zbo_nxt(p)
           enddo
           if (p .gt. 0 .and. (zbo(p) .eq. io)) then
              accept(4) = .true.
           endif
           f4 = f4 + 1
        enddo

  900   chkfltro = accept(1) .and. accept(2) .and. accept(3) .and.
     &             accept(4)
        return
        end

