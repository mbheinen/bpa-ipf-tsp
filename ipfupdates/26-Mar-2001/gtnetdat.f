C    %W% %G%
C****************************************************************
C
C       File: gtnetdat.f
C
C       Purpose: Re-entrany subroutine to obtain filtered network 
C                input data. Array lastloop() flags last valid
C                encoded entity from previous call. Rentry is
C                invoked with the IPF command
C
C                /GET_DATA, TYPE=NETWORK_DATA, CONTINUE
C
C                Premature termination occurs whenever the encoded
C                output fills out_buffer. The last record is flagged
C                with the terminator
C
C                *[MORE]
C
C                It is the GUI's task to utilize this information
C                to request continuation calls to retrieve the entire
C                data set for the specified filter.
C
C       Author: Walt Powell  Date: 1 May 1993
C       Called by: p_gtdata.f
C
C****************************************************************
C
        subroutine gtnetdat (in_buffer, out_buffer)
        character in_buffer *(*), out_buffer *(*)
c
c       This subroutine returns WSCC-formated input data records.
c       Output parameter:
c
c       in_buffer - a character string specifying desired data
c       out_buffer - a character string for storing data
c       error      - warning switch (0 means ignore errors,
c                                    1 means observe errors)
c
C       This routine obtains and applies a filter for entire input 
C       network data.
 
        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/pqcurves.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/xdata.inc'

        character  text*120, bustyp*2, null*1, linefeed*1, 
     &             own*3, brntyp*2, type*2
        logical gtfltr, chkfltr, chkfltra, found, change_f, found_bus, 
     &          found_area
        external  gtfltr, chkfltr, bustyp, brntyp
        integer o2, apdoutbuf, findstr, loop(8), lastloop(8),
     &          ptr, count

        save

        maxbuf_out = len( out_buffer ) - 400
        if ( maxbuf_out .lt. 1 ) maxbuf_out = len( out_buffer ) - 150
        null = char(0)
        linefeed = char(10)
        last = index (in_buffer, null)
c
c       Check for re-entry and continue
c
        if (findstr (in_buffer(1:last), 'CONTINUE') .ne. 0) then
           do i = 1, 8
              lastloop(i) = loop(i)
           enddo
        else
           do i = 1, 8
              lastloop(i) = 0
           enddo
c
c          Search and align to "WHERE" ...
c
           ix = findstr (in_buffer(1:last), 'WHERE') 
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
        endif

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        lasto2 = o2

        do i = 1, 8
           loop(i) = 0
        enddo
c
c       Process "A" or "I" records
c
        nb = lastloop(1)
        if (nb .eq. 0) nb = 1
c
c       Negate retrieval of area data if bus filter is on.
c
        loop(1) = nb
        do while (nb .le. ntotc .and. o2 .lt. maxbuf_out)
           found_area = chkfltra( arcnam(nb), 'A*') 
           if (found_area) then
              if (nb .gt. lastloop(1)) then
                 if (chkfltra(arcnam(nb), 'A?')) then
                    call bcdarc (nb, text)
                    length = apdoutbuf(o2, text(1:80), out_buffer(o2:))
                    if (length .eq. 0) go to 800
                    lasto2 = o2
                    o2 = o2 + length
                 endif
              endif
              if (o2 .lt. maxbuf_out) then
                 loop(1) = nb
                 loop(2) = 0
                 loop(3) = 0
              endif
              j1 = 1
              do while ( j1 .lt. MAXCAZ / MAXCAZR          .and.
     &                   arczns(j1*MAXCAZR+1,nb) .ne. ' '  .and.
     &                   o2 .lt. maxbuf_out                )
                 if (j1 .gt. lastloop(2)) then
                    call bcdarc2 (nb, j1, text)
                    length = apdoutbuf(o2, text(1:80), 
     &                                 out_buffer(o2:))
                    if (length .eq. 0) go to 800
                    lasto2 = o2
                    o2 = o2 + length
                 endif
                 if (o2 .lt. maxbuf_out) loop(2) = j1
                 j1 = j1 + 1
              enddo
              nc = lastloop(3)
              if (nc .eq. 0) nc = 1
              do while (nc .le. ntotic .and. 
     &                  o2 .lt. maxbuf_out)
                 if (arcint(1,nc) .eq. arcnam(nb)) then
                    if (chkfltra (arcnam(nb), 'I ')) then
                       if (nc .gt. lastloop(3)) then
                          call bcdari (nc, text)
                          length = apdoutbuf(o2, text(1:80), 
     &                                       out_buffer(o2:))
                          if (length .eq. 0) go to 800
                          lasto2 = o2
                          o2 = o2 + length
                       endif
                    endif
                 endif
                 if (o2 .lt. maxbuf_out) loop(3) = nc
                 nc = nc + 1
              enddo
           else
              if (o2 .lt. maxbuf_out) then
                 loop(1) = nb
                 loop(2) = 0
                 loop(3) = 0
              endif
           endif
           nb = nb + 1
           lastloop(2) = 0   ! A1 - A9 loop
           lastloop(3) = 0   ! I data loop
        enddo
c
c       Process "B", "+", "X", "Q", "L", "R", "E", or "T" records
c
        ib = lastloop(4)
        if (ib .eq. 0) ib = 1
        do while (ib .le. ntot_alf .and. o2 .lt. maxbuf_out)
           nb = alf2inp(ib)
           found_bus = chkfltr (arcnam(jarzn(nb)), zone(nb), '***',
     &                          base(nb), 'B*', nb)
           if (found_bus) then
              if (ib .gt. lastloop(4)) then
                 type = bustyp(nb)
                 if (chkfltr (arcnam(jarzn(nb)), zone(nb), owner(nb),
     &                        base(nb), type, nb)) then
                    call bcdbus (nb, text)
                    length = apdoutbuf(o2, text(1:80), 
     &                                 out_buffer(o2:))
                    if (length .eq. 0) go to 800
                    lasto2 = o2
                    o2 = o2 + length
                 endif
              endif
              if (o2 .lt. maxbuf_out) then
                 loop(4) = ib
                 loop(5) = 0
                 loop(6) = 0
                 loop(7) = 0
                 loop(8) = 0
              endif

              if (chkfltr (arcnam(jarzn(nb)), zone(nb), '***',
     &                     base(nb), '+ ', nb)) then
c
c                Loop through continuation buses
c
                 ncb = kbsdta(15,nb)
                 count = 0
                 do while (ncb .gt. 0 .and. o2 .lt. maxbuf_out)
                    count = count + 1
                    call getchr (3, own, kbctbl(10,ncb))
                    found = chkfltr (arcnam(jarzn(nb)), zone(nb), own,
     &                               base(nb), '+ ', nb)
                    if (found) then
                       if (count .gt. lastloop(5)) then
                          call bcdcbs (ncb, text)
                          length = apdoutbuf(o2, text(1:80), 
     &                                          out_buffer(o2:))
                          if (length .eq. 0) go to 800
                          lasto2 = o2
                          o2 = o2 + length
                       endif
                    endif
                    if (o2 .lt. maxbuf_out) loop(5) = count
                    ncb = bctbl_nxt(ncb)
                 enddo
              endif

              if (chkfltr (arcnam(jarzn(nb)), zone(nb), '   ', base(nb),
     &                     'X ', nb)) then
c
c                Process "X" record
c
                 kxd = busxdtptr(nb)
                 if (kxd .gt. 0) then
                    if (kxd .gt. lastloop(6)) then
                       call bcdxdt (kxd, text)
                       length = apdoutbuf(o2, text(1:80), 
     &                                    out_buffer(o2:))
                       if (length .eq. 0) go to 800
                       lasto2 = o2
                       o2 = o2 + length
                    endif
                    if (o2 .lt. maxbuf_out) loop(6) = kxd
                 endif
              endif

              if (chkfltr (arcnam(jarzn(nb)), zone(nb), '   ', base(nb),
     &                     'Q ', nb)) then
c
c                Loop through pqcurve data 
c
                 kpqd = buspqptr(nb)
                 if (kpqd .gt. 0) then
                    if (1 .gt. lastloop(7)) then
                       call bcdqpd (kpqd, text)
                       length = apdoutbuf(o2, text(1:120), 
     &                                    out_buffer(o2:))
                       if (length .eq. 0) go to 800
                       lasto2 = o2
                       o2 = o2 + length
                    endif
                    if (o2 .lt. maxbuf_out) loop(7) = 1
                    if (2 .gt. lastloop(7)) then
                       call bcdqxd (kpqd, text)
                       length = apdoutbuf(o2, text(1:120), 
     &                                    out_buffer(o2:))
                       if (length .eq. 0) go to 800
                       lasto2 = o2
                       o2 = o2 + length
                    endif
                    if (o2 .lt. maxbuf_out) loop(7) = 2
                    if (3 .gt. lastloop(7)) then
                       call bcdqnd (kpqd, text)
                       length = apdoutbuf(o2, text(1:120), 
     &                                    out_buffer(o2:))
                       if (length .eq. 0) go to 800
                       lasto2 = o2
                       o2 = o2 + length
                    endif
                    if (o2 .lt. maxbuf_out) loop(7) = 3
                 endif
              endif

              if (chkfltr (arcnam(jarzn(nb)), zone(nb), '***', base(nb),
     &                     'L*', nb)) then
c
c                Loop through branch data
c
                 ptr = kbsdta(16,nb)
                 count = 0
                 do while (ptr .gt. 0 .and. o2 .lt. maxbuf_out)
                    count = count + 1
                    nbr = iabs(brnch_ptr(ptr))
                    call getchr (3, own, kbrnch(3,nbr))
                    type = brntyp(brtype(ptr))
                    found = chkfltr (arcnam(jarzn(nb)), zone(nb), own,
     &                               base(nb), type, nb)
                    if (found .and. brtype(ptr) .ne. 1) then
                       if (count .gt. lastloop(8)) then
                          call bcdbrn (ptr, text)
                          length = apdoutbuf(o2, text(1:120), 
     &                                       out_buffer(o2:))
                          if (length .eq. 0) go to 800
                          lasto2 = o2
                          o2 = o2 + length
                       endif
                    endif
                    if (o2 .lt. maxbuf_out) loop(8) = count
                    ptr = brnch_nxt(ptr)
                 enddo
              endif
           else
              if (o2 .lt. maxbuf_out) then
                 loop(4) = ib
                 loop(5) = 0    ! + data loop
                 loop(6) = 0    ! X data loop
                 loop(7) = 0    ! QP-curve data loop
                 loop(8) = 0    ! branch loop
              endif
           endif
           ib = ib + 1
           lastloop(5) = 0    ! + data loop
           lastloop(6) = 0    ! X data loop
           lastloop(7) = 0    ! QP-curve data loop
           lastloop(8) = 0    ! branch loop
        enddo
  800   continue

c*** remember maxbuf_out is really 400 less than the real buffer size
        if (o2 .ge. maxbuf_out) then
           write (out_buffer(o2:o2+8), 820) linefeed, null
  820      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif
        return
        end
