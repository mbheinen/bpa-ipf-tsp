C    %W% %G%
C****************************************************************
C
C      File: bsekvlst.f
C
C      Purpose: Routine to obtain a basekv list.
C
C      Author: Walt Powell  Date: 18 May 1992
C      Called by: p_gtdata.f
C
C****************************************************************
C
        subroutine bsekvlst (in_buffer, out_buffer, error)
        character in_buffer * (*), out_buffer * (*)
        integer error
c
c       This subroutine returns the list of system basekvs in out_buffer.
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
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/zonehash.inc'
        include 'ipfinc/ownhash.inc'
        include 'ipfinc/bsekvhsh.inc'
        include 'ipfinc/zbo.inc'

        character  null * 1, linefeed * 1, zn * 2
        integer kmpbsekv, o2, p, bsekvhsh, findstr,
     &          f1, f2, f3, f4, f5, f6, find_zon, areasr
        logical accept(6), gtfltr, found, change_f, finished
        external bsekvhsh, kmpbsekv, swapbsekv
 
        max_buf = len ( out_buffer ) - 500
        null = char(0)
        linefeed = char(10)

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)

        call updzon()    ! Conditionally rebuild cross-reference
c                        ! and loss arrays
c
c       Determine if filtered list is requested
c
        last = index (in_buffer, null)
        ix = findstr (in_buffer(1:last), 'WHERE')
        if (ix .gt. 0) go to 400

        nb = 1
        do while (nb .le. numbases .and. o2 + 6 .lt. max_buf)
           write (out_buffer(o2:o2+6), 310) 
     1        basekvs(nb), linefeed
  310      format (f6.1, a)
           o2 = o2 + 7
           nb = nb + 1
        enddo
        go to 900

  400   found = .false.
        if (ix .gt. 0) then
           ix = ix + len('WHERE')
           change_f = gtfltr(in_buffer(ix:))
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

        do while (nb .le. numbases .and. o2 + 6 .lt. max_buf)
c
c          Apply filters
c
           if (filter(2) .eq. 0) then
              accept(2) = .true.
           else
              accept(2) = .false.
              f2 = 1
              do while (f2 .le. filter(2) .and. .not. accept(2))
                 iz = find_zon(zone_filter(f2))
                 p = bs_zn(nb)
                 do while (p .gt. 0 .and. (zbo(p) .lt. iz))
                    p = zbo_nxt(p)
                 enddo
                 if (p .gt. 0 .and. (zbo(p) .eq. iz)) accept(2) = .true.
                 f2 = f2 + 1
              enddo
           endif
           if (filter(3) .eq. 0) then
              accept(3) = .true.
           else
              accept(3) = .false.
              f3 = 1
              do while (f3 .le. filter(3) .and. .not. accept(3))
                 found = .false.
                 io = 1
                 do while (io .le. numown .and. .not. found)
                    if (owner_o(io) .eq. owner_filter(f3)) then
                       found = .true.
                    else
                       io = io + 1
                    endif
                 enddo
                 if (found) then
                     p = bs_ow(nb)
                     do while (p .gt. 0 .and. (zbo(p) .lt. io))
                        p = zbo_nxt(p)
                     enddo
                     if (p .gt. 0 .and. (zbo(p) .eq. io)) 
     &                  accept(3) = .true.
                 endif
                 f3 = f3 + 1
              enddo
           endif
           if (accept(2) .and. accept(3)) then
              write (out_buffer(o2:o2+6), 310) basekvs(nb), linefeed
              o2 = o2 + 7
           endif
           nb = nb + 1
        enddo

  900   if (nb .le. numbases) then
           o2 = o2 - 14
           write (out_buffer(o2:o2+9), 320) linefeed, null
  320      format (a, '*[MORE]', a)
        else
           if (o2 .gt. 1) o2 = o2 - 1
           if (out_buffer(o2:o2) .ne. linefeed) o2 = o2 + 1
           out_buffer(o2:o2) = null
        endif 
        return
        end
