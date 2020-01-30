C    @(#)get_orpt18.f	20.13 1/7/99
C****************************************************************
C
C       File: get_orpt18.f
C       Purpose: Routine to generate various base case comparison
C                reports using IPF commands
C
C                / REPORTS, SELECT CAPACITOR_COMP
C                  WHERE BUS = "<busname>"
C                (END)
C       or
C                / REPORTS, SELECT VOLTAGE_COMP
C                  WHERE BUS = "<busname>"
C                (END)
C       or
C                / REPORTS, SELECT LINE_COMP
C                  WHERE BUS = "<busname>"
C                (END)
C       or
C                / REPORTS, SELECT AR_COMP_XXX
C                  WHERE BUS = "<busname>"
C                (END)
C       or
C                / REPORTS, SELECT BUS_COMP_XXX
C                  WHERE BUS = "<busname>"
C                (END)
C       or
C                / REPORTS, SELECT BR_COMP_XXX
C                  WHERE BUS = "<busname>"
C                (END)
C       or
C
C                / REPORTS, SELECT REACTIVE_UTIL
C                  WHERE ZONES = "<zones>"
C                (END)
C
C       Author: Walt Powell  Date: 14 July 1994
C                            Modified: 6 Nov 1995
C       Called by:
C
C****************************************************************
C
	subroutine get_orpt18 (scrfil)
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
        include 'ipfinc/pfstates.inc'

        common /file_name/ filename
        character filename*60

        character in_buffer*(MAXBUFFER), out_buffer*(MAXBUFFER), 
     &            null*1, linefeed*1, header(4)*132, text*20, 
     &            xquery*2, query*2, ljstfy*2, capital*2, tempc*2,
     &            reports(3,17)*20, unit_diff(3,17)*20, temp*10
	integer apdoutbuf, o1, o2, findstr, status, capdifrpt,
     &          tempfile, vltdifrpt, lfodifrpt, busdifrpt,
     &          aredifrpt, lindifrpt, loaded, qutdifrpt
        logical change_f, load_fltr, finished, no_more, 
     &          is_loaded, loop1, loop2, loading_f
        real loading

        data (reports(1,i), i=1,10) 
     &                / 'AR_COMP_EXPORT_MW', 
     &                  'AR_COMP_EXPORT_MVAR',
     &                  'AR_COMP_FLOW_MW',
     &                  'AR_COMP_FLOW_MVAR',
     &                  'AR_COMP_GEN_MW',
     &                  'AR_COMP_GEN_MVAR',
     &                  'AR_COMP_LOAD_MW',
     &                  'AR_COMP_LOAD_MVAR',
     &                  'AR_COMP_LOSS_MW',
     &                  'AR_COMP_LOSS_MVAR' /

        data (reports(2,i), i=1,7) 
     &                / 'VOLTAGE_COMP', 
     &                  'CAPACITOR_COMP',
     &                  'BUS_COMP_PGEN', 
     &                  'BUS_COMP_QGEN', 
     &                  'BUS_COMP_PLOAD',
     &                  'BUS_COMP_QLOAD', 
     &                  'REACTIVE_UTIL' /

        data (reports(3,i), i=1,17) 
     &                 / 'LINE_COMP',
     &                   'BR_COMP_FLOW_MW',
     &                   'BR_COMP_FLOW_MVAR',
     &                   'BR_COMP_LOSS_MW',
     &                   'BR_COMP_LOSS_MVAR',
     &                   'BR_COMP_R',
     &                   'BR_COMP_X',
     &                   'BR_COMP_TAP1',
     &                   'BR_COMP_TAP2',
     &                   'BR_COMP_G',
     &                   'BR_COMP_B',
     &                   'BR_COMP_RATE',
     &                   'BR_COMP_RATE1',
     &                   'BR_COMP_RATE2',
     &                   'BR_COMP_RATE3',
     &                   'BR_COMP_RATE*',
     &                   'BR_COMP_Z*'  /

        data (unit_diff(1,i), i=1,10) 
     &    / '(MW)' , '(MVAR)', '(MW)', '(MVAR)', '(MW)', '(MVAR)',
     &      '(MW)' , '(MVAR)', '(MW)', '(MVAR)' /
        data (unit_diff(2,i), i=1,7)
     &    / '(p.u.)' , '(MVAR)', '(MW)', '(MVAR)', '(MW)', '(MVAR)',
     &      '(MVAR)' /
        data (unit_diff(3,i), i=1,17)
     &    / '(percent)', '(MW)', '(MVAR)', '(MW)', '(MVAR)', '(p.u.)', 
     &       '(p.u.)', '(KV)', '(KV)', '(p.u.)', '(p.u.)', '(RATE)',
     &       '(RATE1)', '(RATE2)', '(RATE3)', '(RATE*)', 
     &       '(p.u.), (KV)' /

        null = char(0)
        linefeed = char(10)
        linespage = 22
        call getenvir  ('NO_MORE', temp)
        no_more = (temp(1:7) .eq. 'NO_MORE')

        in_buffer(1:1) = null
        i1 = index (in_buffer, null)
        out_buffer(1:1) = null
C
C	Get second Powerflow file name
C
        is_loaded = .false.
        do while (.not. is_loaded)
           if (ofilename .eq. ' ' .or. ofilename(1:1) .eq. null) then
              write(*, 80)
   80         format(' > Enter altername BASE file name or <RETURN> to q
     &uit: ')
	      read (*, 120) filename
  120         format (a)

              if (filename .eq. ' ' .or. filename(1:1) .eq. null) 
     &           go to 900
           else
              last = lastch(ofilename)
              write(*, 92) ofilename(1:last)
   92         format(' > Enter altername BASE file name, ', /
     &               '         <RETURN> to reuse BASE file ', a, /
     &               '         or "Q" to quit: ')
	      read (*, 120) filename

              if (filename .eq. ' ' .or. filename(1:1) .eq. null) then
                 is_loaded = .true.
              else if (filename .eq. 'q' .or. filename .eq. 'Q') then
                 go to 900
              endif
           endif
           if (.not. is_loaded) then
              tempfile = 20           
              ierr = 99    ! Indicate that the input file is binary 
c                          ! formatted
              call opnfil (tempfile, filename, ierr)
              if (ierr .ne. 0) then
                 write (*, 10092)
10092            format(' * Alternate base case cannot be opened ')
                 write (*, 10094) filename
10094            format(' * File ', a)
              else
c
c                Function ldaltbse (load alternate base) defines
c                "ocase" and "ofilename"
c
                 ocase = ' '
                 write (*, 10100)
10100            format (' * Loading alternate base file - this will tak
     &e a minute.')
                 status = ldaltbse (tempfile, filename, ocase, loaded)
                 if (status .ne. 0) then
                    write (*, 10102)
10102               format(' * Alternate base case cannot be read. ',/
     &                     ' * This may not be an IPF base case.')
                    write (*, 10104) filename
10104               format(' * File ', a)
                 else
                    is_loaded = .true.
                 endif
              endif
           endif
        enddo
c
c       Select type of case difference
c
   95   loop1 = .true.
        do while (loop1)
           write (*, 98)
   98      format (' * Differences > 1) Area interchange ',
     &        /,   '                 2) Bus data ',
     &        /,   '                 3) Line data ',
     &        /,   '                 Q) Quit ', 
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
  101      if (iquery .eq. 1) then
c
c             Select type of area difference
c
              loop2 = .true.
              do while (loop2)
                 write (*, 111)
  111            format (' * Area differences > 1) Export MW',
     &              /,   '                      2) Export MVAR ',
     &              /,   '                      3) Tie line MW ',
     &              /,   '                      4) Tie line MVAR ',
     &              /,   '                      5) Generation MW',
     &              /,   '                      6) Generation MVAR ',
     &              /,   '                      7) Load MW ',
     &              /,   '                      8) Load MVAR',
     &              /,   '                      9) Losses MW',
     &              /,   '                     10) Losses MVAR',
     &              /,   '                      Q) Revert ', 
     &              /,   ' > Enter selection : ',$)

                 read (*, 99) xquery
                 xquery = capital (xquery)
                 xquery = ljstfy (xquery)

                 if (xquery .eq. 'Q') go to 119
                 jquery = 0
                 if (xquery(2:2) .eq. ' ') then
                    tempc = ' ' // xquery 
                    xquery = tempc
                 endif
                 read (xquery, '(i2)', err=112) jquery
  112            if (jquery .ge. 1 .and. jquery .le. 10) then
                    loop1 = .false.
                    loop2 = .false.
                    in_buffer(1:1) = null
                    i1 = index (in_buffer, null)
                    out_buffer(1:1) = null
                    last = lastch (reports(iquery,jquery))
                    length = apdoutbuf(i1, 
     &                  '/REPORTS, ' // reports(iquery,jquery)(1:last), 
     &                  in_buffer(i1:))
                    i1 = i1 + length
                 else
                    write (*, 118)
                 endif
              enddo
           else if (iquery .eq. 2) then
c
c             Select type of bus difference
c
              loop2 = .true.
              do while (loop2)
                 write (*, 113)
  113            format (' * Bus differences > 1) Voltage ',
     &              /,   '                     2) Installed shunt ',
     &              /,   '                     3) P generation ',
     &              /,   '                     4) Q generation ',
     &              /,   '                     5) P load ',
     &              /,   '                     6) Q load ',
     &              /,   '                     7) Reactive utilization',
     &              /,   '                     Q) Revert ', 
     &              /,   ' > Enter selection : ',$)

                 read (*, 99) xquery
                 xquery = capital (xquery)
                 xquery = ljstfy (xquery)

                 if (xquery .eq. 'Q') go to 119
                 jquery = 0
                 if (xquery(2:2) .eq. ' ') then
                    tempc = ' ' // xquery
                    xquery = tempc
                 endif
                 read (xquery, '(i2)', err=114) jquery
  114            if (jquery .ge. 1 .and. jquery .le. 7) then
                    loop1 = .false.
                    loop2 = .false.
                    in_buffer(1:1) = null
                    i1 = index (in_buffer, null)
                    out_buffer(1:1) = null
                    last = lastch (reports(iquery,jquery))
                    length = apdoutbuf(i1, 
     &                  '/REPORTS, ' // reports(iquery,jquery)(1:last), 
     &                  in_buffer(i1:))
                    i1 = i1 + length
                    if (filename .ne. ofilename .and. 
     &                  filename .ne. ' ') then
                       last = lastch(filename)
                       length = apdoutbuf(i1, 
     &                     ' FILE = ' // filename(1:last),
     &                     in_buffer(i1:))
                       i1 = i1 + length
                    endif
                 else
                    write (*, 118)
                 endif
              enddo
           else if (iquery .eq. 3) then
c
c             Select type of branch difference
c
              loop2 = .true.
              do while (loop2)
                 write (*, 115)
  115            format (' * Line differences > 1) Loading ',
     &              /,   '                      2) Flows MW ',
     &              /,   '                      3) Flows MVAR ',
     &              /,   '                      4) Losses MW ',
     &              /,   '                      5) Losses MVAR ',
     &              /,   '                      6) R ',
     &              /,   '                      7) X ',
     &              /,   '                      8) Tap1 ',
     &              /,   '                      9) Tap2 ',
     &              /,   '                     10) G ',
     &              /,   '                     11) B ', 
     &              /,   '                     12) Rate (nom)', 
     &              /,   '                     13) Rate1 ', 
     &              /,   '                     14) Rate2 ', 
     &              /,   '                     15) Rate3 ', 
     &              /,   '                     16) Rate* ', 
     &              /,   '                     17) Z* ', 
     &              /,   '                      Q) Quit ', 
     &              /,   ' > Enter selection : ',$)

                 read (*, 99) xquery
                 xquery = capital (xquery)
                 xquery = ljstfy (xquery)

                 if (xquery .eq. 'Q') go to 119
                 jquery = 0
                 if (xquery(2:2) .eq. ' ') then
                    tempc = ' ' // xquery
                    xquery = tempc
                 endif
                 read (xquery, '(i2)', err=116) jquery
  116            if (jquery .ge. 1 .and. jquery .le. 17) then
                    loop1 = .false.
                    loop2 = .false.
                    in_buffer(1:1) = null
                    i1 = index (in_buffer, null)
                    out_buffer(1:1) = null
                    last = lastch (reports(iquery,jquery))
                    length = apdoutbuf(i1, 
     &                  '/REPORTS, ' // reports(iquery,jquery)(1:last), 
     &                   in_buffer(i1:))
                    i1 = i1 + length
                    if (filename .ne. ofilename .and. 
     &                  filename .ne. ' ') then
                       last = lastch(filename)
                       length = apdoutbuf(i1, 
     &                     ' FILE = ' // filename(1:last),
     &                     in_buffer(i1:))
                       i1 = i1 + length
                    endif
                 else
                    write (*, 118)
                 endif
              enddo
           else
              write (*, 118)
  118         format (' * Illegal value - reenter selection :',$)
           endif
  119      continue
        enddo

        change_f = .true.      ! Change filters
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
              loading_f = .true.
              do while (loading_f)
                 last = lastch (unit_diff(iquery,jquery))
                 write (*, 96) unit_diff(iquery,jquery)(1:last)
   96            format (' > Enter difference ', a, 
     &                   ' or <RETURN> to terminate : ')
                 read (*, fmt=120, end=140) text
                 ix = lastch (text)
                 jx = index (text, ' ') - 1
                 kx = index (text, ',') - 1
                 if (jx .le. 0) jx = ix
                 if (kx .le. 0) kx = ix
                 ix = min0 (ix, jx, kx)
                 if (ix .gt. 0) then
                    loading = ftn_atof (text(1:ix))
                 else
                    loading = 0.0
                 endif
                 if (text .eq. ' ' .or. loading .lt. 0.0) then
                    loading_f = .false.
                 else
                    out_buffer(1:1) = null
c
c                   Append "AND LOADING = " to in_buffer
c
                    ix = findstr (in_buffer(1:i1), 
     &                            linefeed // ' CONTINUE')
                    if (ix .eq. 0) ix = findstr (in_buffer(1:i1), 
     &                                  linefeed // ' AND LOADING')
                    if (ix .eq. 0) ix = findstr (in_buffer(1:i1), 
     &                                  linefeed // '(END)')
                    i1 = ix
                    if (i1 .eq. 0) i1 = 1
                    in_buffer(i1:i1) = null
                    last = lastch (text)
                    length = apdoutbuf(i1, ' AND LOADING = ' 
     &                    // text(1:last) // ' ' // text(1:last) 
     &                    // linefeed // '(END)',
     &                    in_buffer(i1:))
                    i1 = i1 + length
                    if (iquery .eq. 1) then
                       ix = findstr (in_buffer, 'AR_COMP_')
                       status = aredifrpt(in_buffer, out_buffer, 
     &                                 scrfil)
                    else if (iquery .eq. 2 .and. jquery .eq. 1) then
                       lastx = lastch (reports(iquery,jquery))
                       ix = findstr (in_buffer, 
     &                    reports(iquery,jquery)(1:lastx)) + lastx
                       status = vltdifrpt(in_buffer(ix:), out_buffer, 
     &                                 scrfil)
                    else if (iquery .eq. 2 .and. jquery .eq. 2) then
                       lastx = lastch (reports(iquery,jquery))
                       ix = findstr (in_buffer, 
     &                    reports(iquery,jquery)(1:lastx)) + lastx
                       status = capdifrpt(in_buffer(ix:), out_buffer, 
     &                                 scrfil)
                    else if (iquery .eq. 2 .and. jquery .gt. 2 .and.
     &                       jquery .lt. 7) then
                       ix = findstr (in_buffer, 'BUS_COMP_')
                       status = busdifrpt(in_buffer, out_buffer, 
     &                                 scrfil)
                    else if (iquery .eq. 2 .and. jquery .eq. 7) then
                       lastx = lastch (reports(iquery,jquery))
                       ix = findstr (in_buffer, 
     &                    reports(iquery,jquery)(1:lastx)) + lastx
                       status = qutdifrpt(in_buffer(ix:), out_buffer, 
     &                                 scrfil)
                    else if (iquery .eq. 3 .and. jquery .eq. 1) then
                       lastx = lastch (reports(iquery,jquery))
                       ix = findstr (in_buffer, 
     &                    reports(iquery,jquery)(1:lastx)) + lastx
                       status = lfodifrpt(in_buffer(ix:), out_buffer, 
     &                                 scrfil)
                    else if (iquery .eq. 3 .and. jquery .gt. 1) then
                       ix = findstr (in_buffer, 'BR_COMP_')
                       status = lindifrpt(in_buffer, out_buffer, 
     &                                 scrfil)
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
     &                           out_buffer(o1:o1) .ne. null)
                          o2 = nxt_term(out_buffer(o1+1:)) + o1
                          if (out_buffer(o1:o1) .eq. '/') then
                             o2 = MAXBUFFER + 1
                          else 
                             if (out_buffer(o1:o1+6) .eq. '*[MORE]'
     &                          .and. .not. no_more) then
                                in_buffer(1:1) = null
                                i1 = index (in_buffer, null)
                                lastx = lastch (reports(iquery,jquery))
                                length = apdoutbuf (i1, 
     &                                 '/REPORTS, SELECT ' // 
     &                               reports(iquery,jquery)(1:lastx) //
     &                               linefeed // ' CONTINUE ', 
     &                               in_buffer(i1:))
                                i1 = i1 + length
                                out_buffer(1:1) = null
                                if (iquery .eq. 1) then
                                   status = aredifrpt(in_buffer(ix:), 
     &                                        out_buffer, scrfil)
                                else if (iquery .eq. 2 .and. 
     &                                   jquery .eq. 1) then
                                   status = vltdifrpt(in_buffer(ix:), 
     &                                        out_buffer, scrfil)
                                else if (iquery .eq. 2 .and. 
     &                                   jquery .eq. 2) then
                                   status = capdifrpt(in_buffer(ix:), 
     &                                        out_buffer, scrfil)
                                else if (iquery .eq. 2 .and. 
     &                                   jquery .gt. 2 .and.
     &                                   jquery .lt. 7) then
                                   status = busdifrpt(in_buffer(ix:), 
     &                                        out_buffer, scrfil)
                                else if (iquery .eq. 2 .and. 
     &                                   jquery .eq. 7) then
                                   status = qutdifrpt(in_buffer(ix:), 
     &                                        out_buffer, scrfil)
                                else if (iquery .eq. 3 .and. 
     &                                   jquery .eq. 1) then
                                   status = lfodifrpt(in_buffer(ix:), 
     &                                         out_buffer, scrfil)
                                else if (iquery .eq. 3 .and. 
     &                                   jquery .gt. 1) then
                                   status = lindifrpt(in_buffer(ix:), 
     &                                         out_buffer, scrfil)
                                endif
                                o1 = 1
                                if (out_buffer(o1:o1) .eq. linefeed) 
     &                             o1 = o1 + 1
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
        go to 95
  900   return
        end
