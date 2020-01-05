C    @(#)get_otrpt6.f	20.5 1/7/99
C****************************************************************
C
C       File: get_otrpt6.f
C       Purpose: Routine to generate overloaded transformer reports
C                using IPF command
C
C                / REPORTS, SELECT OVERLOADED_TXS
C                  WHERE AREAS = ...
C                (END)
C
C       Author: Walt Powell  Date: 22 July 1992
C                            Modified: 22 July 1992
C       Called by:
C
C****************************************************************
C
	subroutine get_otrpt6 (scrfil)
        integer scrfil

	include 'ipfinc/parametr.inc'
        include 'ipfinc/sortuvov.inc'

        character in_buffer*(MAXBUFFER), out_buffer*(MAXBUFFER), 
     &            null*1, linefeed*1, text*10, fmt*10, header(4)*132, 
     &            capital*1, temp*10

	integer apdoutbuf, o1, o2, findstr
        logical change_f, load_fltr, finished, no_more
        real loading

        null = char(0)
        linefeed = char(10)
        linespage = 22
        call getenvir  ('NO_MORE', temp)
        no_more = (temp(1:7) .eq. 'NO_MORE')

        change_f = .true.      ! Change filters

        do while (change_f)
           in_buffer(1:1) = null
           i1 = index (in_buffer, null)
           length = apdoutbuf(i1, '/REPORTS, SELECT OVERLOADED_TXS ',
     &                        in_buffer(i1:))
           i1 = i1 + length
           change_f = load_fltr(in_buffer, out_buffer, i1)
           if (change_f) then
              loading = 1.0
              do while (loading .gt. 0.0)
                 write (*, 90)
   90            format (' > Enter % transformer loading: ')
                 read (*, 120) text
  120            format (a)
                 last = lastch (text)
                 write (fmt, '(''(f'', i2, ''.0)'')') last
                 read (text, fmt, end=140) loading
                 if (loading .gt. 0.0) then
                    out_buffer(1:1) = null
c
c                   Append "AND LOADING = " to in_buffer
c
                    i1 = findstr (in_buffer, linefeed // ' AND LOADING')
                    if (i1 .eq. 0) then
                       i1 = findstr (in_buffer, linefeed // '(END)')
                    endif
                    if (i1 .eq. 0) i1 = 1
                    in_buffer(i1:i1) = null
                    last = lastch (text)
                    length = apdoutbuf(i1, ' AND LOADING = ' 
     &                   // text(1:last) // ' ' // text(1:last) 
     &                   // linefeed // '(END)',
     &                   in_buffer(i1:))
                    ix = findstr (in_buffer, 'OVERLOADED_TXS')
     &                 + lastch ('OVERLOADED_TXS')
                    call ovldtxsrpt(in_buffer(ix:), out_buffer, scrfil)
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
     &                           out_buffer(o1:o1) .ne. null)
                          o2 = nxt_term(out_buffer(o1+1:)) + o1
                          if (out_buffer(o1:o1) .eq. '/') then
                             o2 = MAXBUFFER + 1
                          else 
                             if (out_buffer(o1:o1+6) .eq. '*[MORE]'
     &                          .and. .not. no_more) then
                                in_buffer(1:1) = null
                                i1 = index (in_buffer, null)
                                length = apdoutbuf (i1, 
     &                   '/REPORTS, SELECT OVERLOADED_TXS CONTINUE',
     &                             in_buffer(i1:))
                                i1 = i1 + length
                                out_buffer(1:1) = null
                                call ovldtxsrpt(in_buffer, out_buffer, 
     &                             scrfil)
                                o1 = 1
                                do i = 1, 4
                                   o2 = nxt_term(out_buffer(o1+1:)) + o1
                                   o1 = o2
                                   if (out_buffer(o1:o1) .eq. linefeed) 
     &                                o1 = o1 + 1
                                enddo
                                o2 = nxt_term(out_buffer(o1+1:)) + o1
                             endif
                             if (no_more) then
                                finished = .true.
                                go to 130
                             else if (numlin .ge. linespage+1) then
                                numlin = 0
                                write(*, 100)
  100                           format (' --More-- ')
                                read (*, 120) text(1:1)
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
  110                        format (1x, a)
                          endif
                          o1 = o2 
                          if (out_buffer(o1:o1) .eq. linefeed) 
     &                       o1 = o1 + 1
                       enddo
  130                  finished = .true.
                    enddo
                 endif
              enddo
  140         continue
           endif
        enddo
        return
        end
