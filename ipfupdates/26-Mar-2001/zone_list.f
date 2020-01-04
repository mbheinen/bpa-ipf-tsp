C    %W% %G%
        subroutine zone_list (in_buffer, out_buffer, error)
C
C****************************************************************
C
C      File: zone_list.f
C
C      Purpose: Routine to obtain a zone list.
C
C      Author: Walt Powell  Date: 18 May 1992
C      Called by: p_gtdata.f
C
C****************************************************************
C
        character in_buffer * (*), out_buffer * (*)
        integer error
c
c       This subroutine returns the list of system zones in out_buffer.
c       Output parameter:
c
c       in_buffer - a character string specifying desired data
c       out_buffer - a character string for storing data
c       error      - warning switch (0 means ignore errors,
c                                    1 means observe errors)
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/zonehash.inc'
        include 'ipfinc/ownhash.inc'
        include 'ipfinc/bsekvhsh.inc'
        include 'ipfinc/zbo.inc'

        character  null * 1, linefeed * 1, zn * 2
        integer bldzone, kmpzone, o2, p, findstr,
     &          f1, f2, f3, f4, f5, f6, find_zon, areasr, ownsch
        logical accept(6), gtfltr, found, change_f, finished
        external bldzone, kmpzone, swapzone
 
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        max_buf = len(out_buffer) - 500

        call updzon()    ! Conditionally rebuild cross-reference
c                        ! and loss arrays
c
c       Determine if filtered list is requested
c
        last = index (in_buffer, null)
        ix = findstr (in_buffer(1:last), 'WHERE')
        if (ix .gt. 0) go to 400

        nb = 1
        o2 = index (out_buffer,null)
        do while (nb .le. nztot .and. o2 + 2 .lt. max_buf)
           write (out_buffer(o2:o2+2), 310) acznam(nb), 
     1        linefeed
  310      format (a2, a)
           o2 = o2 + 3
           nb = nb + 1
        enddo
        go to 900

  400   found = .false.
        if (ix .gt. 0) then
           ix = ix + len('WHERE')
           change_f = gtfltr (in_buffer(ix:))
           found = .true.
        endif
        if (.not .found) then
           do i = 1, 7
              filter(i) = 0
           enddo
        endif

        nb = 1
        o2 = index (out_buffer,null)

        if (filter(1) .gt. 0) then
c
c          Expand area filters into equivalent zone filters
c
           f1 = 1
           do while (f1 .le. filter(1))
              ia = areasr (area_filter(f1))
              found = .false.
              j = 1
              do while (j .le. MAXCAZ .and. .not. found)
                 zn = arczns(j,ia)                                    
                 if (zn .eq. '  ' .and. j .gt. 1) then               
                    found = .true.
                 else 
                    iz = find_zon(zn)
                    if (iz .gt. 0) then
                       finished = .false.
                       f2 = 1
                       do while (f2 .le. filter(2) .and.
     &                             .not. finished)
                          if (zone_filter(f2) .eq. zn) then
                             finished = .true.
                          else
                             f2 = f2 + 1
                          endif
                       enddo
                       if (.not. finished) then
                          f2 = filter(2) + 1
                          filter(2) = f2
                          zone_filter(f2) = zn
                       endif
                    endif
                    j = j + 1
                 endif
              enddo
              f1 = f1 + 1
           enddo
        endif

        do while (nb .le. nztot .and. o2 + 2 .lt. max_buf)
c
c          Apply filters
c
           if (filter(1) .eq. 0) then
              accept(1) = .true.
           else
c
c             Apply generated zone_filter in lieu of area_filter
c
              accept(1) = .false.
              f2 = 1
              do while (f2 .le. filter(2) .and. .not. accept(1))
                 iz = find_zon(zone_filter(f2))
                 if (iz .eq. nb) accept(1) = .true.
                 f2 = f2 + 1
              enddo
           endif
           if (filter(3) .eq. 0) then
              accept(3) = .true.
           else
              accept(3) = .false.
              f3 = 1
              do while (f3 .le. filter(3) .and. .not. accept(3))
                 io = ownsch (owner_filter(f3))
                 p = zn_ow(nb)
                 do while (p .gt. 0 .and. (zbo(p) .lt. io))
                    p = zbo_nxt(p)
                 enddo
                 if (p .gt. 0 .and. (zbo(p) .eq. io)) accept(3) = .true.
                 f3 = f3 + 1
              enddo
           endif
           if (filter(4) .eq. 0) then
              accept(4) = .true.
           else
              accept(4) = .false.
              f4 = 1
              accept(4) = .false.
              do while (f4 .le. filter(4) .and. .not. accept(4))
                 found = .false.
                 ib = 1
                 do while (ib .le. numbases .and. .not. found)
                    if (basekvs(ib) .eq. voltage_filter(1,f4)) then
                       found = .true.
                    else
                       ib = ib + 1
                    endif
                 enddo
                 if (found) then
                     p = zn_bs(nb)
                     do while (p .gt. 0 .and. (zbo(p) .lt. ib))
                        p = zbo_nxt(p)
                     enddo
                     if (p .gt. 0 .and. (zbo(p) .eq. ib)) 
     &                  accept(4) = .true.
                 endif
                 f4 = f4 + 1
              enddo
           endif
           if (accept(1) .and. accept(3) .and. accept(4)) then
              write (out_buffer(o2:o2+2), 310) acznam(nb), 
     1           linefeed
              o2 = o2 + 3
           endif
           nb = nb + 1
        enddo

  900   if (nb .le. nztot) then
           o2 = o2 - 9
           write (out_buffer(o2:o2+9), 320) linefeed, null
  320      format (a, '*[MORE]', a)
        else
           if (o2 .gt. 1) o2 = o2 - 1
           if (out_buffer(o2:o2) .ne. linefeed) o2 = o2 + 1
           out_buffer(o2:o2) = null
        endif 
        return
        end
