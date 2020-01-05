C    @(#)get_outrpt.f	20.6 1/7/99
C****************************************************************
C
C       File: get_outrpt.f
C       Purpose: Routine to generate the following output reports
C                reports using IPF commands:
C
C                / REPORTS, SELECT BUS_OV
C                  WHERE AREA = "<areaname>"
C                (END)
C                
C                ENTRY: busovrpt
C       or
C                / REPORTS, SELECT BUS_UV
C                  WHERE AREA = "<areaname>"
C                (END)
C                
C                ENTRY: busuvrpt
C       or
C                / REPORTS, SELECT FILT_OUTPUT
C                  WHERE AREA = "<areaname>"
C                (END)
C                
C                ENTRY: filtoutrpt
C       or
C                / REPORTS, SELECT GEN_SUMMARY
C                  WHERE AREA = "<areaname>"
C                (END)
C                
C                ENTRY: genrpt
C       or
C                / REPORTS, SELECT REG_XFMR
C                  WHERE AREA = "<areaname>"
C                (END)
C                
C                ENTRY: regxrpt
C
C       Author: Walt Powell  Date: 14 Feb 1995
C                            Modified: 14 Feb 1995
C       Called by:
C
C****************************************************************
C
	subroutine get_outrpt (scrfil)
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
     &            query*2, ljstfy*2, capital*2, tempc*2,
     &            reports(5)*20, temp*10
	integer apdoutbuf, o1, o2, findstr, status, report_type,
     &          tempfile, open_file, regxrpt
        logical change_f, load_fltr, finished, loop1, loop2, no_more
        real loading

        data (reports(i), i=1,5) 
     &                / 'BUS_OV', 
     &                  'BUS_UV',
     &                  'FILT_OUTPUT',
     &                  'REG_XFMR',
     &                  'GEN_SUMMARY' /

        entry get_otrpt2 (scrfil)     ! BUS_OV
        report_type = 1
        go to 90

        entry get_otrpt3 (scrfil)     ! BUS_UV
        report_type = 2
        go to 90

        entry get_otrpt7 (scrfil)     ! FILT_OUTPUT
        report_type = 3
        go to 90

        entry get_otrpt8 (scrfil)     ! REG_XFMR
        report_type = 4
        go to 90

        entry get_orpt21 (scrfil)     ! GEN_SUMMARY
        report_type = 5

   90   null = char(0)
        linefeed = char(10)
        linespage = 22
        call dbgprt(0)

        call getenvir  ('NO_MORE', temp)
        no_more = (temp(1:7) .eq. 'NO_MORE')

        in_buffer(1:1) = null
        i1 = index (in_buffer, null)
        out_buffer(1:1) = null

        change_f = .true.      ! Change filters
        do while (change_f)
           in_buffer(1:1) = null
           i1 = index (in_buffer, null)
           out_buffer(1:1) = null
           last = lastch (reports(report_type))
           length = apdoutbuf(i1, 
     &           '/REPORTS, SELECT ' // reports(report_type)(1:last), 
     &           in_buffer(i1:))
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
           if (report_type .le. 2) then
              loading = 0.0
              write (*, 96)
   96         format (' > Enter p.u. voltage violation: ')
              read (*, 120) text
              last = lastch (text)
              write (fmt, '(''(f'', i2, ''.0)'')') last
              read (text, fmt, end=98) loading
   98         if (loading .gt. 0.0) then
                 out_buffer(1:1) = null
c
c                Append "AND LOADING = " to in_buffer
c
                 i1 = findstr (in_buffer, linefeed // ' AND LOADING')
                 if (i1 .eq. 0) then
                    i1 = findstr (in_buffer, linefeed // '(END)')
                 endif
                 if (i1 .eq. 0) i1 = 1
                 in_buffer(i1:i1) = null
                 last = lastch (text)
                 length = apdoutbuf(i1, ' AND LOADING = ' 
     &                // text(1:last) // ' ' // text(1:last) 
     &                // linefeed // '(END)',
     &                in_buffer(i1:))
              endif
              if (.not. change_f .and. loading .gt. 0.0) 
     &           change_f = .true.
           endif
           if (change_f) then
              lastx = lastch (reports(report_type))
              ix = findstr (in_buffer, 
     &                    reports(report_type)(1:lastx)) 
              if (report_type .eq. 1) then
                 call busovrpt (in_buffer(ix:), out_buffer, scrfil)
              else if (report_type .eq. 2) then
                 call busuvrpt (in_buffer(ix:), out_buffer, scrfil)
              else if (report_type .eq. 3) then
                 status = regxrpt (in_buffer(ix:), out_buffer, scrfil)
              else if (report_type .eq. 4) then
                 call filtoutrpt (in_buffer(ix:), out_buffer, scrfil)
              else if (report_type .eq. 5) then
                 call genrpt (in_buffer(ix:), out_buffer, scrfil)
              endif
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
                          lastx = lastch (reports(report_type))
                          length = apdoutbuf (i1, 
     &                              '/REPORTS, SELECT ' // 
     &                              reports(report_type)(1:lastx) //
     &                              ' CONTINUE ', in_buffer(i1:))
                          i1 = i1 + length
                          out_buffer(1:1) = null
                          if (report_type .eq. 1) then
                             call busovrpt (in_buffer(ix:), out_buffer, 
     &                                      scrfil)
                          else if (report_type .eq. 2) then
                             call busuvrpt (in_buffer(ix:), out_buffer, 
     &                                      scrfil)
                          else if (report_type .eq. 3) then
                             status = regxrpt (in_buffer(ix:), 
     &                                         out_buffer, scrfil)
                          else if (report_type .eq. 4) then
                             call filtoutrpt (in_buffer(ix:), 
     &                                        out_buffer, scrfil)
                          else if (report_type .eq. 5) then
                             call genrpt (in_buffer(ix:), out_buffer, 
     &                                    scrfil)
                          endif
                          o1 = 1
                          if (out_buffer(o1:o1) .eq. linefeed)
     &                       o1 = o1 + 1
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

  900   return
        end
