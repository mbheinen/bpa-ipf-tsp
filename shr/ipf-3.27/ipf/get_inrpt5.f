C    @(#)get_inrpt5.f	20.3 1/7/99
C****************************************************************
C
C       File: get_inrpt5.f
C       Purpose: Routine to generate a filtered report of data 
C       sanity checks using IPF command
C
C                / REPORTS, SELECT DATA_SANITY
C                  WHERE AREAS = "AREA 1", ...
C                (END)
C
C       Author: Walt Powell  Date: 4 October 1995
C                            Modified: 
C       Called by:
C
C****************************************************************
C
        subroutine get_inrpt5 (scrfil)
        integer scrfil
 
C       This routine obtains and applies a filter for a system-wide
C       data sanity check listing.
 
      	include 'ipfinc/parametr.inc'
      	include 'ipfinc/blank.inc'
      	include 'ipfinc/arcntl.inc'
      	include 'ipfinc/bus.inc'
      	include 'ipfinc/ownhash.inc' 
      	include 'ipfinc/sortuvov.inc' 

        character in_buffer*(MAXBUFFER), out_buffer*(MAXBUFFER), 
     &            null*1, linefeed*1, header(5)*132, capital*1,
     &            text*2, temp*10
        logical load_fltr, change_f, finished, no_more
        integer findstr, o1, o2, apdoutbuf

        change_f = .true.      ! Change filters
        null = char(0)
        linefeed = char(10)
        linespage = 22

        call getenvir  ('NO_MORE', temp)
        no_more = (temp(1:7) .eq. 'NO_MORE')

        do while (change_f)
           in_buffer(1:1) = null
           i1 = index (in_buffer, null)
           length = apdoutbuf (i1, '/REPORTS, SELECT DATA_SANITY ',
     &                         in_buffer(i1:))
           i1 = i1 + length
           out_buffer(1:1) = null
           change_f = load_fltr (in_buffer, out_buffer, i1)
           if (change_f) then
              ix = findstr (in_buffer, 'DATA_SANITY')
     &           + lastch ('DATA_SANITY')
              call dtasanrpt(in_buffer(ix:), out_buffer, scrfil)
              o1 = 1
              do i = 1, 5
                o2 = nxt_term(out_buffer(o1+1:)) + o1
                header(i) = out_buffer(o1:o2-1)
                o1 = o2
                if (out_buffer(o1:o1) .eq. linefeed) o1 = o1 + 1
              enddo
              o1 = 1
              numlin = 1
              finished = .false.
              do while (.not. finished)
                 do while (o1 .lt. MAXBUFFER .and. 
     &                     out_buffer(o1:o1) .ne. null)
                    o2 = nxt_term(out_buffer(o1+1:)) + o1
                    if (out_buffer(o1:o1) .eq. '/') then
                       o2 = MAXBUFFER + 1
                    else 
                       if (out_buffer(o1:o1+6) .eq. '*[MORE]' .and.
     &                    .not. no_more) then
                          in_buffer(1:1) = null
                          i1 = index (in_buffer, null)
                          length = apdoutbuf (i1, 
     &                         '/REPORTS, SELECT DATA_SANITY CONTINUE',
     &                         in_buffer(i1:))
                          i1 = i1 + length
                          out_buffer(1:1) = null
                          call dtasanrpt(in_buffer, out_buffer, 
     &                       scrfil)
                          o1 = 1
                          o2 = nxt_term(out_buffer(o1+1:)) + o1
                       endif

                       if (no_more) then
                          finished = .true.
                          go to 130
                       else if (numlin .ge. linespage+1) then
                          write(*, 100)
  100                     format (' --More-- ')
                          read (*, 120) text(1:1)
  120                     format (a)
                          text(1:1) = capital (text(1:1))
                          numlin = 0
                          if (text(1:1) .eq. 'Q') then
                             finished = .true.
                             go to 130
                          endif
                          do i = 1, 5
                            last = min0 (lastch (header(i)), 79)
                            write (*, 110) header(i)(1:last)
  110                       format (a)
                            numlin = numlin + 1
                          enddo
                       endif                    
                       numlin = numlin + 1
                       last = o1 + min0(lastch(out_buffer(o1:o2-1)),78)
                       write (*, 110) out_buffer(o1:last)
                    endif
                    o1 = o2 
                    if (out_buffer(o1:o1) .eq. linefeed) o1 = o1 + 1
                 enddo
  130            finished = .true.
              enddo
           endif
        enddo
 
        return
        end
