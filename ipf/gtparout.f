C    @(#)gtparout.f	20.3 2/13/96
C****************************************************************
C
C   File: gtoutput.f
C   Purpose: This routine computes the output quantities for all
C            parallels between named bus_1 and bus_2.
C
C   Author: Walt Powell  Date: 23 August 1993
C                        Modified:
C   Called by:
C
C****************************************************************
C
        subroutine gtparout (in_buffer, out_buffer)
        character in_buffer *(*), out_buffer *(*)
c
c       This subroutine returns WSCC-formated input data records.
c       Output parameter:
c
c       out_buffer - a character string for storing data
c
        include 'ipfinc/parametr.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/xdata.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/prt.inc'
 
        parameter (MAXDATAREC = 258)
        character bus_1 * 8, bus_2 * 8, text * 120, id * 1
        character null * 1, linefeed * 1, record * (MAXDATAREC)
        logical eof, found
        integer find_bus, count, sect, o2, apdoutbuf, ptr, flag(50)
c
c       Read branch bus data from in_buffer
c
        null = char(0)
        linefeed = char(10)
 
        i1 = 1
        i2 = index (in_buffer, null)
        if (i1 .lt. i2 .and. (in_buffer(1:1) .eq. '/')) then
           i1 = nxt_term(in_buffer)
        endif
        if (in_buffer(i1:i1) .eq. linefeed) i1 = i1 + 1
 
        out_buffer(1:1) = null
        o2 = index (out_buffer, null)
 
        text = '( end )'
        count = 0
 
        do while (i1 .lt. i2)
           next_i1 = nxt_term(in_buffer(i1+1:)) + i1
           text = in_buffer(i1:next_i1-1)
           if (index ('LERT', text(1:1)) .ne. 0) then
c
c             Obtain types "L", "E", "R", or "T" records.
c
              read (text, 170, err=200)  bus_1, base_1, bus_2,
     &            base_2
  170         format (bz, t7, a8, f4.0, 1x, a8, f4.0)
              k1 = find_bus (bus_1,base_1)
              if (k1 .le. 0) then
                 call ermisbus (bus_1, base_1, text)
              else
                 k2 = find_bus (bus_2,base_2)
                 if (k2 .le. 0) then
                    call ermisbus (bus_2, base_2, text)
                 else
                    ptr = kbsdta(16,k1)
                    do while (ptr .gt. 0 .and.
     1                       (inp2alf(ky(ptr)) .lt. inp2alf(k2)))
                       ptr = brnch_nxt(ptr)
                    enddo
                    do while (ptr .gt. 0 .and.
     1                       (k2 .eq. ky(ptr)))
                       if (brtype(ptr) .eq. 4) then
                          ptr = brnch_nxt(ptr)
                       else
                          id = brid(ptr)
                          call out_brn (ptr, id, 0, record)
                          length = apdoutbuf(o2, record,
     1                                       out_buffer(o2:))
 
                          if (length .eq. 0) then
                             write (errbuf(1), 116) bus(k1), base(k1),
     1                                              bus(k2), base(k2)
  116                        format (' Output buffer overflowed at branc
     &h ', a8, f6.1, 1x, a8, f6.1)
                             call prterx ('W', 1)
                             go to 900
                          endif
                          o2 = o2 + length
                          count = count + 1
                          do while (ptr .gt. 0 .and.
     1                             (k2 .eq. ky(ptr)  .and.
     &                              id .eq. brid(ptr)))
                             ptr = brnch_nxt(ptr)
                          enddo
                       endif
                    enddo
                    if (count .eq. 0) then
                       write (errbuf(1), 180) text(1:33)
  180                  format ('Branch record is not in system ',
     1                     '(', a, '). Adjacent records follow.')
                       n = 1
                       ptr = kbsdta(16,k1)
                       do while (ptr .gt. 0 .and. n .lt. 9)
                          call bcdbrn (ptr, record)
                          n = n + 1
                          write (errbuf(n), 130) record(1:80)
  130                     format (a)
                          ptr = brnch_nxt(ptr)
                       enddo
                       call prterx ('W', n)
                    endif
                 endif
              endif
           endif
           go to 220
 
  200      write (errbuf(1), 210) text(1:80)
  210      format (' Illegal data in numeric field (', a, ')')
           call prterx ('W', 1)
 
  220      continue
           i1 = next_i1
           if (in_buffer(i1:i1) .eq. linefeed) i1 = i1 + 1
 
        enddo
 
  900   continue
        buf = text
        return
        end
