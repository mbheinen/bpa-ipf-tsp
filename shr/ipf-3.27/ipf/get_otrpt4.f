C    @(#)get_otrpt4.f	20.5 1/7/99
C****************************************************************
C
C       File: get_otrpt4.f
C       Purpose: Routine to generate bus under/over voltage output 
C                reports using IPF command
C
C                / REPORTS, SELECT BUS_UVOV
C                  WHERE BUS = "<busname>"
C                (END)
C
C       Author: Walt Powell  Date: 22 July 1992
C                            Modified: 22 July 1992
C       Called by:
C
C****************************************************************
C
	subroutine get_otrpt4 (scrfil)
        integer scrfil

	include 'ipfinc/parametr.inc'	
	include 'ipfinc/lfiles.inc'
	include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/sortuvov.inc'

        character in_buffer*(MAXBUFFER), out_buffer*(MAXBUFFER), 
     &            null*1, linefeed*1, header(4)*132, capital*1,
     &            text*2, temp*10
	integer apdoutbuf, o1, o2, findstr
        logical change_v, change_f, get_defv, load_fltr, finished,
     &          no_more

        null = char(0)
        linefeed = char(10)
        linespage = 22
        call getenvir  ('NO_MORE', temp)
        no_more = (temp(1:7) .eq. 'NO_MORE')
c
c       Copy default limits into arrays.
c
        num_limit = nvlim - 1
        do i = 1, num_limit
           v_range(1,i) = vlimit(1,i)
           v_range(2,i) = vlimit(2,i)
           v_min(i) = vlimit(3,i)
           v_max(i) = vlimit(4,i)
        enddo

        change_f = .true.      ! Change filters

        do while (change_f)
           in_buffer(1:1) = null
           i1 = index (in_buffer, null)
           length = apdoutbuf(i1, '/REPORTS, SELECT BUS_UVOV ',
     &                        in_buffer(i1:))
           i1 = i1 + length
           change_f = load_fltr(in_buffer, out_buffer, i1)
           if (change_f) then
              change_v = .true.
              do while (change_v)
                 change_v = get_defv()
                 out_buffer(1:1) = null
                 ix = findstr (in_buffer, 'BUS_UVOV')
     &                 + lastch ('BUS_UVOV')
                 call busuvovrpt(in_buffer(ix:), out_buffer, scrfil)
                 o1 = 1
                 do i = 1, 4
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
     &                        out_buffer(o1:o1) .ne. null)
                       o2 = nxt_term(out_buffer(o1+1:)) + o1
                       if (out_buffer(o1:o1) .eq. '/') then
                          o2 = MAXBUFFER + 1
                       else 
                          if (out_buffer(o1:o1+6) .eq. '*[MORE]' .and.
     &                       .not. no_more) then
                             in_buffer(1:1) = null
                             i1 = index (in_buffer, null)
                             length = apdoutbuf (i1, 
     &                         '/REPORTS, SELECT BUS_UVOV CONTINUE',
     &                          in_buffer(i1:))
                             i1 = i1 + length
                             out_buffer(1:1) = null
                             call busuvovrpt(in_buffer, out_buffer, 
     &                            scrfil)
                             o1 = 1
                             do i = 1, 4
                                o2 = nxt_term(out_buffer(o1+1:)) + o1
                                o1 = o2
                                if (out_buffer(o1:o1) .eq. linefeed) 
     &                             o1 = o1 + 1
                             enddo
                             o2 = nxt_term(out_buffer(o1+1:)) + o1
                          endif
                          if (no_more) then
                             finished = .true.
                             go to 130
                          else if (numlin .ge. linespage+1) then
                             numlin = 0
                             write(*, 100)
  100                        format (' --More-- ')
                             read (*, 120) text(1:1)
  120                        format (a)
                             text(1:1) = capital (text(1:1))
                             if (text(1:1) .eq. 'Q') then
                                finished = .true.
                                go to 130
                             endif
                             do i = 1, 4
                                write (*, 110) header(i)
                                numlin = numlin + 1
                             enddo
                          endif                    
                          numlin = numlin + 1
                          write (*, 110) out_buffer(o1:o2-1)
  110                     format (1x, a)
                       endif
                       o1 = o2 
                       if (out_buffer(o1:o1) .eq. linefeed) o1 = o1 + 1
                    enddo
  130               finished = .true.
                 enddo
              enddo
           endif
        enddo
        return
        end
