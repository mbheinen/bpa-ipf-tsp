C    %W% %G%
C****************************************************************
C
C   File: area_list.f
C
C   Purpose: Routine to obtain an area list.
C
C   Author: Walt Powell  Date: 18 May 1992
C   Called by: p_gtdata.f
C
C****************************************************************
C
        subroutine area_list (in_buffer, out_buffer, error)
        character in_buffer * (*), out_buffer * (*)
        integer error
c
c       This subroutine returns the list of system buses in out_buffer.
c       Output parameter:
c
c       in_buffer - a character string specifying desired data
c       out_buffer - a character string for storing data
c       error      - warning switch (0 means ignore errors,
c                                    1 means observe errors)
c
C       This routine obtains and applies a filter for a bus input 
C       listing.
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/zonehash.inc'
        include 'ipfinc/ownhash.inc'
        include 'ipfinc/bsekvhsh.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/zbo.inc'

        character  null*1, linefeed*1, zn*2, zn1*2, own1*3
        integer findstr, o2, find_zon,
     &          f1, f2, f3, f4, f5, f6, p
        logical accept(6), gtfltr, found, change_f
 
        null = char(0)
        linefeed = char(10)

        out_buffer(1:1) = null
        o2 = 1
        max_buf = len(out_buffer) - 500
c
c       Determine if filtered list is requested
c
        last = index (in_buffer, null)
        ix = findstr (in_buffer(1:last), 'WHERE')
        if (ix .gt. 0) go to 400

        nb = 1
        do while (nb .le. ntotc .and. o2 .lt. max_buf)
           write (out_buffer(o2:o2+10), 310) arcnam(nb), 
     1        linefeed
  310      format (a10, a)
           o2 = o2 + 11
           nb = nb + 1
        enddo
        go to 900

  400   call updzon()    ! Conditionally rebuild cross-reference
c                        ! and loss arrays
        found = .false.
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
        do while (nb .le. ntotc .and. o2 .lt. max_buf)
c
c          Apply filters
c
           if (filter(2) .eq. 0) then
              accept(2) = .true.
           else
              accept(2) = .false.
              f2 = 1
              do while (f2 .le. filter(2) .and. .not. accept(2))
                 zn1 = zone_filter(f2)
                 j = 1
                 found = .false.
                 do while (j .le. MAXCAZ .and. .not. found)
                    zn = arczns(j,nb)
                    if (zn .eq. '  ' .and. j .gt. 1) then               
                       found = .true.
                    else 
                       if (zn1 .eq. zn) then
                          found = .true.
                          accept(2) = .true.
                       else
                          j = j + 1
                       endif
                    endif
                 enddo
                 f2 = f2 + 1
              enddo
           endif
           if (filter(3) .eq. 0) then
              accept(3) = .true.
           else
              accept(3) = .false.
              f3 = 1
              do while (f3 .le. filter(3) .and. .not. accept(3))
                 own1 = owner_filter(f3)
                 j = 1
                 found = .false.
                 do while (j .le. MAXCAZ .and. .not. found)
                    zn = arczns(j,nb)
                    if (zn .eq. '  ' .and. j .gt. 1) then               
                       found = .true.
                    else 
                       ix = find_zon(zn)
                       p = zn_ow(ix)
                       do while (p .gt. 0 .and. 
     &                          (owner_o(zbo(p)) .ne. own1))
                          p = zbo_nxt(p)
                       enddo
                       if (p .gt. 0 .and. 
     &                    (owner_o(zbo(p)) .eq. own1)) then
                          accept(3) = .true.
                          found = .true.
                       else
                          j = j + 1
                       endif
                    endif
                 enddo
                 f3 = f3 + 1
              enddo
           endif
           if (filter(4) .eq. 0) then
              accept(4) = .true.
           else
              accept(4) = .false.
              f4 = 1
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
                    j = 1
                    found = .false.
                    do while (j .le. MAXCAZ .and. .not. found)
                       zn = arczns(j,nb)
                       if (zn .eq. '  ' .and. j .gt. 1) then
                          found = .true.
                       else 
                          ix = find_zon(zn)
                          p = zn_bs(ix)
                          do while (p .gt. 0 .and. (zbo(p) .lt. ib))
                             p = zbo_nxt(p)
                          enddo
                          if (p .gt. 0 .and. (zbo(p) .eq. ib)) then
                             accept(4) = .true.
                             found = .true.
                          else
                             j = j + 1
                          endif
                       endif
                    enddo
                 endif
                 f4 = f4 + 1
              enddo
           endif
           if (accept(2) .and. accept(3) .and. accept(4)) then
              write (out_buffer(o2:o2+10), 310) arcnam(nb), linefeed
              o2 = o2 + 11
           endif
           nb = nb + 1
        enddo

  900   if (nb .le. ntotc) then
           o2 = o2 - 11
           write (out_buffer(o2:o2+9), 320) linefeed, null
  320      format (a, '*[MORE]', a)
        else
           if (o2 .gt. 1) o2 = o2 - 1
           if (out_buffer(o2:o2) .ne. linefeed) o2 = o2 + 1
           out_buffer(o2:o2) = null
        endif 
        return
        end
