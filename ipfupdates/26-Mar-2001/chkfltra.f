C    %W% %G%
C
C****************************************************************
C
C      File: chkfltra.f
C
C      Purpose: Routine to apply a filter to area "area" 
C
C      Author: Walt Powell  Date: 18 May 1992
C      Called by: p_gtdata.f
C
C****************************************************************
C
        logical function chkfltra (areaname, type)
        character areaname*10, type*2

        include 'ipfinc/parametr.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/zonehash.inc'
        include 'ipfinc/ownhash.inc'
        include 'ipfinc/bsekvhsh.inc'
        include 'ipfinc/zbo.inc'

	external ownsch, find_zon, find_ara
        integer  ownsch, find_zon, find_base, find_ara

        integer f1, f2, f3, f4, f5, f6, p
        character zn*2
        logical accept(6)
c
c       Apply filters
c
        do j = 1, 6
           if (filter(j) .eq. 0) then
               accept(j) = .true.
           else
               accept(j) = .false.
           endif
        enddo

        if (areaname .eq. '**********') then
           if (filter(1) .gt. 0) accept(1) = .true.
        else
           ia = find_ara (areaname)
           f1 = 1
           do while (f1 .le. filter(1) .and. .not. accept(1))
              if (area_filter(f1) .eq. areaname) accept(1) = .true.
              f1 = f1 + 1
           enddo
c
c          Check zone filters
c
           ix = 1
           do while (ix .le. MAXCAZ .and. filter(2) .gt. 0 .and.
     &               .not. accept(2))
              zn = arczns(ix,ia)                                    
              if (zn .eq. '  ' .and. ix .gt. 1) then               
                 ix = MAXCAZ + 1
              else 
                 f2 = 1
                 do while (f2 .le. filter(2) .and.
     &                     .not. accept(2))
                    if (zone_filter(f2) .eq. zn) then
                       accept(2) = .true.
                    else
                       f2 = f2 + 1
                    endif
                 enddo
              endif
              ix = ix + 1
           enddo
c
c          Check owner filters via zones
c
           ix = 1
           do while (ix .le. MAXCAZ .and. filter(3) .gt. 0 .and.
     &               .not. accept(3))
              zn = arczns(ix,ia)                                    
              if (zn .eq. '  ' .and. ix .gt. 1) then               
                 ix = MAXCAZ + 1
              else 
                 iz = find_zon (zn)
                 f3 = 1
                 do while (f3 .le. filter(3) .and.
     &                    .not. accept(3))
                    io = ownsch (owner_filter(f3))
                    p = ow_zn(io)
                    do while (p .gt. 0 .and. (zbo(p) .lt. iz))
                       p = zbo_nxt(p)
                    enddo
                    if (p .gt. 0 .and. (zbo(p) .eq. iz)) then
                       accept(3) = .true.
                    endif
                    f3 = f3 + 1
                 enddo
              endif
              ix = ix + 1
           enddo
c
c          Check base kv filters via zones
c
           ix = 1
           do while (ix .le. MAXCAZ .and. filter(4) .gt. 0 .and.
     &               .not. accept(4))
              zn = arczns(ix,ia)                                    
              if (zn .eq. '  ' .and. ix .gt. 1) then               
                 ix = MAXCAZ + 1
              else 
                 iz = find_zon(zn)
                 if (iz .gt. 0) then
                    f4 = 1
                    do while (f4 .le. filter(4) .and.
     &                        .not. accept(4))
                       nb = find_base(voltage_filter(1,f4))
                       if (nb .gt. 0) then
                          p = bs_zn(nb)
                          do while (p .gt. 0 .and. (zbo(p) .lt. iz))
                             p = zbo_nxt(p)
                          enddo
                          if (p .gt. 0 .and. (zbo(p) .eq. iz)) then
                             accept(4) = .true.
                          endif
                       endif
                       f4 = f4 + 1
                    enddo
                 endif
              endif
              ix = ix + 1
           enddo
c
c          Check type filters
c
           f5 = 1
           do while (f5 .le. filter(5) .and. .not. accept(5))
              if (type_filter(f5) .eq. '*' .or.
     &            type_filter(f5) .eq. type .or.
     &            type .eq. '*') then
                 accept(5) = .true.
              endif
              f5 = f5 + 1
           enddo
c
c          Check bus filters
c
           f6 = 1
           do while (f6 .le. filter(6) .and. .not. accept(6))
              if (jarzn(bus_filter(f6)) .eq. ia) accept(6) = .true.
              f6 = f6 + 1
           enddo

        endif

        chkfltra = accept(1) .and. accept(2) .and. accept(3) .and.
     &             accept(4) .and. accept(5) .and. accept(6)
        return
        end

