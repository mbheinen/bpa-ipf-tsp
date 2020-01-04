C    @(#)get_orpt19.f	20.8 1/7/99
C****************************************************************
C
C       File: get_orpt19.f
C       Purpose: Routine to generate various base case comparison
C                reports using IPF commands
C
C                / REPORTS, SELECT OWNER_INT_DETAIL
C                  WHERE BUS = "<busname>"
C                (END)
C       or
C                / REPORTS, SELECT OWNER_INT_BRIEF
C                  WHERE BUS = "<busname>"
C                (END)
C
C       Author: Walt Powell  Date: 14 Feb 1995
C                            Modified: 14 Feb 1995
C       Called by:
C
C****************************************************************
C
	subroutine get_orpt19 (scrfil)
        integer scrfil

	include 'ipfinc/parametr.inc'	
	include 'ipfinc/blank.inc'
	include 'ipfinc/lfiles.inc'
	include 'ipfinc/dtaiop.inc'
	include 'ipfinc/bus.inc'
	include 'ipfinc/arcntl.inc'
	include 'ipfinc/ownhash.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/update.inc'

        common /file_name/ filename
        character filename*60

        character in_buffer*(MAXBUFFER), out_buffer*(MAXBUFFER), 
     &            null*1, linefeed*1, header(4)*132, 
     &            text*10, fmt*10, xquery*2,
     &            query*2, ljstfy*2, capital*2, tempc*2, temp*10,
     &            reports(2)*20, unit_diff(3,11)*10
	integer apdoutbuf, o1, o2, findstr, status, ownintchg,
     &          tempfile, open_file, bldownint
        logical change_v, change_f, get_defv, load_fltr, finished,
     &          loop1, loop2, no_more
        real loading

        data (reports(i), i=1,2) 
     &                / 'OWNER_INT_BRIEF', 
     &                  'OWNER_INT_DETAIL' /

        null = char(0)
        linefeed = char(10)
        linespage = 22
        call dbgprt(0)
        call getenvir  ('NO_MORE', temp)
        no_more = (temp(1:7) .eq. 'NO_MORE')
c
c       Build ownership interchange arrays
c
        status = bldownint(status)
        update(1) = 0 

        in_buffer(1:1) = null
        i1 = index (in_buffer, null)
        out_buffer(1:1) = null
c
c       Select level of detail
c
   95   loop1 = .true.
        do while (loop1)
           write (*, 98)
   98      format (' * Detail level > 1) Summary only ',
     &        /,   '                  2) Full detail ',
     &        /,   '                  Q) Quit ', 
     &        /,   ' > Enter selection : ',$)

           read (*, 99) query
   99      format (a)
           query = capital (query)
           query = ljstfy (query)

           if (query .eq. 'Q') go to 900
           iquery = 0
           if (query(2:2) .eq. ' ') then
              tempc = ' ' // query
              query = tempc
           endif
           read (query, '(i2)', err=101) iquery
  101      if (iquery .eq. 1 .or. iquery .eq. 2) then
              loop1 = .false.
              in_buffer(1:1) = null
              i1 = index (in_buffer, null)
              out_buffer(1:1) = null
              last = lastch (reports(iquery))
              length = apdoutbuf(i1, 
     &                   '/REPORTS, SELECT ' // 
     &                   reports(iquery)(1:last), 
     &                   in_buffer(i1:))
              i1 = i1 + length
           else
              write (*, 118)
  118         format (' * Illegal value - reenter selection :',$)
           endif
        enddo

        change_f = .true.      ! Change filters
        do while (change_f)
           in_buffer(1:1) = null
           i1 = index (in_buffer, null)
           out_buffer(1:1) = null
           last = lastch (reports(iquery))
           length = apdoutbuf(i1, 
     &                  '/REPORTS, SELECT ' // reports(iquery)(1:last), 
     &                   in_buffer(i1:))
           i1 = i1 + length
c
c          Align to "WHERE " in in_buffer
c
           ix = findstr (in_buffer, ' WHERE ')
           if (ix .gt. 0) then
              i1 = ix
           endif
           in_buffer(i1:i1) = null
           change_f = load_fltr(in_buffer, out_buffer, i1)
           if (change_f) then
              lastx = lastch (reports(iquery))
              ix = findstr (in_buffer, 
     &                    reports(iquery)(1:lastx)) 
              status = ownintchg(in_buffer(ix:), out_buffer, scrfil)
              o1 = 1
              do i = 1, 3
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
     &                     .not. no_more) then
                          in_buffer(1:1) = null
                          i1 = index (in_buffer, null)
                          lastx = lastch (reports(iquery))
                          length = apdoutbuf (i1, 
     &                              '/REPORTS, SELECT ' // 
     &                              reports(iquery)(1:lastx) //
     &                              ' CONTINUE ', in_buffer(i1:))
                          i1 = i1 + length
                          out_buffer(1:1) = null
                          status = ownintchg(in_buffer(ix:), 
     &                                       out_buffer, scrfil)
                          o1 = 1
                          do i = 1, 3
                             o2 = nxt_term(out_buffer(o1+1:)) + o1 
                             o1 = o2
                             if (out_buffer(o1:o1) .eq. linefeed) 
     &                          o1 = o1 + 1
                          enddo
                          o2 = nxt_term(out_buffer(o1+1:)) + o1
                       endif
                       if (no_more) then
                          finished = .true.
                          go to 130
                       else if (numlin .ge. linespage+1) then
                          numlin = 0
                          write(*, 100)
  100                     format (' --More-- ')
                          read (*, 120) text(1:1)
  120                     format (a)
                          text(1:1) = capital (text(1:1))
                          if (text(1:1) .eq. 'Q') then
                             finished = .true.
                             go to 130
                          endif
                          do i = 1, 3
                             write (*, 110) header(i)
                             numlin = numlin + 1
                          enddo
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
        go to 95
  900   return
        end
