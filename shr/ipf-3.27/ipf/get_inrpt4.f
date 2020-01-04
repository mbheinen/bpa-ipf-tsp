C    @(#)get_inrpt4.f	20.5 1/7/99
C****************************************************************
C
C       File: get_inrpt4.f
C       Purpose: Routine to generate filtered bus input reports
C                using IPF command
C
C                / REPORTS, SELECT BUS_INPUT
C                  WHERE AREAS = "AREA 1", ...
C                (END)
C
C       Author: Walt Powell  Date: 22 July 1992
C                            Modified: 22 July 1992
C       Called by:
C
C****************************************************************
C
        subroutine get_inrpt4 (scrfil)
        integer scrfil
 
C       This routine obtains and applies a filter for a bus input 
C       listing.
 
      	include 'ipfinc/parametr.inc'
      	include 'ipfinc/blank.inc'
      	include 'ipfinc/arcntl.inc'
      	include 'ipfinc/bus.inc'
      	include 'ipfinc/ownhash.inc' 
      	include 'ipfinc/sortuvov.inc' 

        character in_buffer*(MAXBUFFER), out_buffer*(MAXBUFFER), 
     &            null * 1, linefeed * 1, header(4) * 132, capital * 1,
     &            text * 2
        logical load_fltr, change_f, finished
        integer findstr, o1, o2, apdoutbuf

        change_f = .true.      ! Change filters
        null = char(0)
        linefeed = char(10)
        linespage = 22

        do while (change_f)
           in_buffer(1:1) = null
           i1 = index (in_buffer, null)
           length = apdoutbuf (i1, '/REPORTS, SELECT BUS_INPUT ',
     &                         in_buffer(i1:))
           i1 = i1 + length
           out_buffer(1:1) = null
           change_f = load_fltr (in_buffer, out_buffer, i1)
           if (change_f) then
              ix = findstr (in_buffer, 'BUS_INPUT')
     1           + lastch ('BUS_INPUT')
              call businrpt(in_buffer(ix:), out_buffer, scrfil)
              o1 = 1
              o2 = nxt_term(out_buffer(o1+1:)) + o1
              header(1) = out_buffer(o1:o2-1)
              numlin = 1
              finished = .false.
              do while (.not. finished)
                 do while (o1 .lt. MAXBUFFER .and. 
     &                     out_buffer(o1:o1) .ne. null)
                    o2 = nxt_term(out_buffer(o1+1:)) + o1
                    if (out_buffer(o1:o1) .eq. '/') then
                       o2 = MAXBUFFER + 1
                    else 
                       if (out_buffer(o1:o1+6) .eq. '*[MORE]') then
                          in_buffer(1:1) = null
                          i1 = index (in_buffer, null)
                          length = apdoutbuf (i1, 
     &                         '/REPORTS, SELECT BUS_INPUT CONTINUE',
     &                         in_buffer(i1:))
                          i1 = i1 + length
                          out_buffer(1:1) = null
                          call businrpt(in_buffer, out_buffer, 
     &                       scrfil)
                          o1 = 1
                          o2 = nxt_term(out_buffer(o1+1:)) + o1
                       endif
                       if (numlin .ge. linespage+1) then
                          write(*, 100)
  100                     format (' --More-- ')
                          read (*, 120) text(1:1)
  120                     format (a)
                          text(1:1) = capital (text(1:1))
                          numlin = 1
                          if (text(1:1) .eq. 'Q') then
                             finished = .true.
                             go to 130
                          endif
                          write (*, 110) header(1)
                       endif                    
                       numlin = numlin + 1
                       write (*, 110) out_buffer(o1:o2-1)
  110                  format (1x, a)
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
