C    %W% %G%
C****************************************************************
C
C      File: chkfltrz.f
C
C      Purpose: Routine to apply a filter to zone "zn" 
C
C      Author: Walt Powell  Date: 22 Feb 1995
C      Called by: ownintchg.f
C
C****************************************************************
C
        logical function chkfltrz (zn) 
        character zn * 2

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

	external ownsch, find_zon
        integer  ownsch, find_zon, find_base

        integer f1, f2, f3, f4, p
        logical accept(6)
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
        iz = find_zon (zn)
        if (iz .le. 0) go to 900

        f1 = 1
        do while (f1 .le. filter(1) .and. .not. accept(1))
c
c          Check area filters through zones 
c
           if (arcnam(acznum(iz)) .eq. area_filter(f1)) then
              accept(1) = .true.
           else
              f1 = f1 + 1
           endif
        enddo
c
c       Check zone filters directly
c
        f2 = 1
        do while (f2 .le. filter(2) .and. .not. accept(2))
           if (zone_filter(f2) .eq. zn) then
               accept(2) = .true.
           endif
           f2 = f2 + 1
        enddo
c
c       Check owner filters through zone ow_zn
c
        f3 = 1
        do while (f3 .le. filter(3) .and. .not. accept(3))
           io = ownsch (owner_filter(f3))
           p = ow_zn(io)
           do while (p .gt. 0 .and. zbo(p) .lt. iz)
              p = zbo_nxt(p)
           enddo
           if (p .gt. 0 .and. zbo(p) .eq. iz) then
              accept(3) = .true.
           endif
           f3 = f3 + 1
        enddo
c
c       Check base kv filters through bs_zn()
c
        f4 = 1
        do while (f4 .le. filter(4) .and. .not. accept(4))
           ib = find_base (voltage_filter(1,f4))
           p = bs_zn(ib)
           do while (p .gt. 0 .and. zbo(p) .lt. iz)
              p = zbo_nxt(p)
           enddo
           if (p .gt. 0 .and. zbo(p) .eq. iz) then
              accept(4) = .true.
           endif
           f4 = f4 + 1
        enddo

  900   chkfltrz = accept(1) .and. accept(2) .and. accept(3) .and.
     &             accept(4)
        return
        end
