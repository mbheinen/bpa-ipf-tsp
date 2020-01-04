C    %W% %G%
C
        subroutine owner_list (in_buffer, out_buffer, error)
C
C****************************************************************
C
C      File: owner_list.f
C
C      Purpose: Routine to obtain a owner list.
C
C      Author: Walt Powell  Date: 18 May 1992
C      Called by: p_gtdata.f
C
C****************************************************************
C
        character in_buffer *(*), out_buffer *(*)
        integer error, o2
c
c       This subroutine returns the list of system owners in out_buffer.
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
        integer kmpowner, p, owner_hash, findstr,
     &          f1, f2, f3, f4, f5, f6, find_zon, areasr
        logical accept(6), gtfltr, found, change_f, finished
        external owner_hash, kmpowner, swapowner

        max_buf = len( out_buffer ) - 50 
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

        ib = 1
        do while (ib .le. numown .and. o2 .lt. max_buf)
           write (out_buffer(o2:o2+3), 310) 
     1        owner_o(alf2own(ib)), linefeed
  310      format (a3, a)
           o2 = o2 + 4
           ib = ib + 1
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

        ib = 1
        nb = alf2own(ib)

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

        do while (ib .le. numown .and. o2 .lt. max_buf)
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
                 p = ow_zn(nb)
                 do while (p .gt. 0 .and. (zbo(p) .lt. iz))
                    p = zbo_nxt(p)
                 enddo
                 if (p .gt. 0 .and. (zbo(p) .eq. iz)) accept(2) = .true.
                 f2 = f2 + 1
              enddo
           endif
           if (filter(4) .eq. 0) then
              accept(4) = .true.
           else
              accept(4) = .false.
              f4 = 1
              do while (f4 .le. filter(4) .and. .not. accept(4))
                 found = .false.
                 ix = 1
                 do while (ix .le. numbases .and. .not. found)
                    if (basekvs(ix) .eq. voltage_filter(1,f4)) then
                       found = .true.
                    else
                       ix = ix + 1
                    endif
                 enddo
                 if (found) then
                     p = ow_bs(nb)
                     do while (p .gt. 0 .and. (zbo(p) .lt. ix))
                        p = zbo_nxt(p)
                     enddo
                     if (p .gt. 0 .and. (zbo(p) .eq. ix)) 
     &                  accept(4) = .true.
                 endif
                 f4 = f4 + 1
              enddo
           endif
           if (accept(2) .and. accept(4)) then
              write (out_buffer(o2:o2+3), 310) owner_o(nb), linefeed
              o2 = o2 + 4
           endif
           ib = ib + 1
           nb = alf2own(ib)
        enddo

  900   if (ib .le. numown) then
           o2 = o2 - 8
           write (out_buffer(o2:o2+9), 320) linefeed, null
  320      format (a, '*[MORE]', a)
        else
           if (o2 .gt. 1) o2 = o2 - 1
           if (out_buffer(o2:o2) .ne. linefeed) o2 = o2 + 1
           out_buffer(o2:o2) = null
        endif 
        return
        end
