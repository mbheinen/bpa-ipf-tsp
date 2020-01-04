C    @(#)get_orpt16.f	20.5 1/7/99
C****************************************************************
C
C       File: get_orpt15.f
C       Purpose: Routine to generate bone pile output 
C                reports using IPF command
C
C                / REPORTS, SELECT CAPACITOR_COMP
C                  WHERE BUS = "<busname>"
C                (END)
C
C       Author: Walt Powell  Date: 14 July 1994
C                            Modified: 14 July 1994
C       Called by:
C
C****************************************************************
C
	subroutine get_orpt16 (scrfil)
        integer scrfil

	include 'ipfinc/parametr.inc'	
	include 'ipfinc/blank.inc'
	include 'ipfinc/lfiles.inc'
	include 'ipfinc/dtaiop.inc'
	include 'ipfinc/bus.inc'
	include 'ipfinc/arcntl.inc'
	include 'ipfinc/ownhash.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/alt_case.inc'

        character in_buffer*(MAXBUFFER), out_buffer*(MAXBUFFER), 
     &            null*1, linefeed*1, header(4)*132, capital*1,
     &            text*10, filename*60, fmt*10, temp*10
	integer apdoutbuf, o1, o2, findstr, status, capdifrpt,
     &          tempfile, open_file
        logical change_v, change_f, get_defv, load_fltr, finished,
     &          is_loaded, no_more
        real loading

        null = char(0)
        linefeed = char(10)
        linespage = 22
        call getenvir  ('NO_MORE', temp)
        no_more = (temp(1:7) .eq. 'NO_MORE')

        change_f = .true.      ! Change filters
C
C	Get second Powerflow file name
C
        in_buffer(1:1) = null
        i1 = index (in_buffer, null)
        out_buffer(1:1) = null
        length = apdoutbuf(i1, '/REPORTS, SELECT CAPACITOR_COMP',
     &                     in_buffer(i1:))
        i1 = i1 + length

        is_loaded = .false.
        do while (.not. is_loaded)
           if (ofilename .eq. ' ' .or. ofilename(1:1) .eq. null) then
              write(*, 80)
   80         format(' > Enter altername BASE file name or <RETURN> to q
     &uit: ')
	      read (*, 90) filename
   90	      format (a)

              if (filename .eq. ' ' .or. filename(1:1) .eq. null) 
     &           go to 900
           else
              last = lastch(ofilename)
              write(*, 92) ofilename(1:last)
   92         format(' > Enter altername BASE file name, ', /
     &               '         <RETURN> to reuse BASE file ', a, /
     &               '         or "Q" to quit: ')
	      read (*, 90) filename

              if (filename .eq. ' ' .or. filename(1:1) .eq. null) then
                 is_loaded = .true.
              else if (filename .eq. 'q' .or. filename .eq. 'Q') then
                 go to 900
              endif
           endif
           if (.not. is_loaded) then
              tempfile = 20           
              status = open_file (tempfile, filename, 'B', 'R', ierr)
              if (status .ne. 0) then
                 last = lastch(filename)
                 write (*, 94) filename(1:last)
   94            format(' File ', a, ' cannot be opened ')
              else
                 is_loaded = .true.
                 call close_file (tempfile)
                 last = lastch(filename)
                 length = apdoutbuf(i1, ' FILE = ' // filename(1:last),
     &                              in_buffer(i1:))
                 i1 = i1 + length
              endif
           endif
        enddo

        do while (change_f)
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
              loading = 1.0
              do while (loading .gt. 0.0)
                 write (*, 96)
   96            format (' > Enter capacitor difference (MVAR): ')
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
                    ix = findstr (in_buffer, 'CAPACITOR_COMP')
     &                 + lastch ('CAPACITOR_COMP')
                    status = capdifrpt(in_buffer(ix:), out_buffer, 
     &                                 scrfil)
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
     &                   '/REPORTS, SELECT CAPACITOR_COMP CONTINUE',
     &                             in_buffer(i1:))
                                i1 = i1 + length
                                out_buffer(1:1) = null
                                status = capdifrpt(in_buffer, 
     &                              out_buffer, scrfil)
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
                                do i = 1, 3
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
  900   return
        end
