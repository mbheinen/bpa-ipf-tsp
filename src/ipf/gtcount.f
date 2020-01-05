C    %W% %G%
C****************************************************************
C
C       File: gtcount.f
C
C       Purpose: Integer function to obtain the count of filtered 
C                network input data which will be retrieved in a 
C                subsequent call using the same filter.
C
C                /GET_DATA, TYPE=NETWORK_DATA
C
C       Author: Walt Powell  Date: 1 May 1993
C       Called by: p_gtdata.f
C
C****************************************************************
C
        integer function gtcount (in_buffer, out_buffer)
        character in_buffer *(*), out_buffer *(*)
c
c       in_buffer - a character string specifying desired data
c       out_buffer - a character string for storing data
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

        character  bustyp*2, null*1, linefeed*1, own*3, 
     &             brntyp*2, text*120, type*2
        logical gtfltr, chkfltr, chkfltra, found, change_f, found_bus, 
     &          found_area
        external  gtfltr, chkfltr, bustyp, brntyp
        integer o2, findstr, ptr, count, apdoutbuf

        save

        gtcount = 0
        null = char(0)
        linefeed = char(10)
        last = index (in_buffer, null)
c
c       Search and align to "WHERE" ...
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

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        count = 0
c
c       Process "A" or "I" records
c
        nb = 1
        do while (nb .lt. ntotc)
           found_area = chkfltra( arcnam(nb), 'A*')
           if (found_area) then
              if (chkfltra(arcnam(nb), 'A?')) then
                 count = count + 1
              endif
              j1 = 2
              do while (j1 .le. 9 .and.
     &                 (arczns(j1*MAXCAZR-9,nb) .ne. ' '))
                 count = count + 1
                 j1 = j1 + 1
              enddo
              nc = 1
              do while (nc .le. ntotic)
                 if (arcint(1,nc) .eq. arcnam(nb)) then
                    if (chkfltra(arcnam(nb), 'I ')) then
                       count = count + 1
                    endif
                 endif
                 nc = nc + 1
              enddo
           endif
           nb = nb + 1
        enddo
c
c       Process "B", "+", "X", "Q", "L", "R", "E", or "T" records
c
        ib = 1
        do while (ib .le. ntot_alf)
           nb = alf2inp(ib)
           found_bus = chkfltr( arcnam(jarzn(nb)), zone(nb), '***',
     &                          base(nb), 'B*', nb)
           if (found_bus) then
              type = bustyp(nb)
              if (chkfltr( arcnam(jarzn(nb)), zone(nb), owner(nb),
     &                     base(nb), type, nb)) then
                 count = count + 1
              endif

              if (chkfltr( arcnam(jarzn(nb)), zone(nb), '***',
     &                     base(nb), '+ ', 0)) then
c
c                Loop through continuation buses
c
                 ncb = kbsdta(15,nb)
                 do while (ncb .gt. 0)
                    call getchr (3, own, kbctbl(10,ncb))
                    if (chkfltr( arcnam(jarzn(nb)), zone(nb), own,
     &                           base(nb), '+ ', nb)) then
                       count = count + 1
                    endif
                    ncb = bctbl_nxt(ncb)
                 enddo
              endif

              if (chkfltr( arcnam(jarzn(nb)), zone(nb), '   ', 
     &                     base(nb), 'X ', nb)) then
c
c                Process "X" record
c
                 kxd = busxdtptr(nb)
                 if (kxd .gt. 0) count = count + 1
              endif

              if (chkfltr( arcnam(jarzn(nb)), zone(nb), '   ', base(nb),
     &                     'Q ', nb)) then
c
c                Loop through pqcurve data 
c
                 kpqd = buspqptr(nb)
                 if (kpqd .gt. 0) count = count + 3
              endif

              if (chkfltr( arcnam(jarzn(nb)), zone(nb), '***', base(nb),
     &                     'L*', nb)) then
c
c                Loop through branch data
c
                 ptr = kbsdta(16,nb)
                 do while (ptr .gt. 0)
                    nbr = iabs(brnch_ptr(ptr))
                    call getchr (3, own, kbrnch(3,nbr))
                    type = brntyp(brtype(ptr))
                    found = chkfltr( arcnam(jarzn(nb)), zone(nb), own,
     &                               base(nb), type, nb)
                    if (found .and. brtype(ptr) .ne. 1) then
                       count = count + 1
                    endif
                    ptr = brnch_nxt(ptr)
                 enddo
              endif
           endif
           ib = ib + 1
        enddo

        write (text, 820) count
  820   format (1x, ' count = ', i6)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        return
        end
