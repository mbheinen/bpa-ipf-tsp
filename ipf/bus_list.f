C    %W% %G%
C****************************************************************
C
C   File: bus_list.f
C
C   Purpose: Routine to obtain a bus list.
C
C   Author: Walt Powell  Date: 18 May 1992
C   Called by: pf_gtdata.f
C
C****************************************************************
C
        subroutine bus_list (in_buffer, out_buffer, error)
        character in_buffer *(*), out_buffer *(*)
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
      include 'ipfinc/bus.inc'
      include 'ipfinc/owncom.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/sortuvov.inc'

        character  null*1, linefeed*1, bustyp*2,
     &             code*4, basekv*4
        logical change_f, found, gtfltr, chkfltr
        integer o2, lastloop, findstr
 
        save

        maxbuf_out = len( out_buffer ) - 400
        null = char(0)
        linefeed = char(10)
c
c       Check for re-entry and continue
c
        last = index (in_buffer, null)
        if (findstr (in_buffer(1:last), 'CONTINUE') .eq. 0) then
           lastloop = 0
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
           else if (filter(6) .gt. 0) then

C             Align bus pointer nb to last bus for beginning 

              nb = bus_filter(filter(6))  
              ib = inp2alf(nb)
              lastloop = ib
           endif
        endif
        
        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        ib = lastloop + 1
        do while (ib .le. ntot_alf .and. o2 .lt. maxbuf_out)
           nb = alf2inp(ib)
           if ( chkfltr( arcnam(jarzn(nb)), zone(nb), owner(nb), 
     &                   base(nb), bustyp(nb), 0 )  ) then
              basekv = code(base(nb), 4, 0)
              write (out_buffer(o2:o2+12), 310) bus(nb), basekv, 
     &           linefeed
  310         format (a8, a4, a)
              o2 = o2 + 13
           endif
           ib = ib + 1
        enddo
        lastloop = ib - 1

c*** remember maxbuf_out is really 400 less than the real buffer size
        if (o2 .gt. maxbuf_out) then
           write ( out_buffer(o2:o2+6), 320)
  320      format ('*[MORE]')
           o2 = o2 + 7
        endif 
        out_buffer(o2:o2) = null
        return
        end
