C    %W% %G%
C****************************************************************
C
C   File: p_report.f
C   Purpose: IPF shell program to process /REPORTS commands
C
C   Author: Walt Powell  
C
C****************************************************************
C
	integer function p_report (in_buffer, out_buffer) 
        character in_buffer *(*), out_buffer *(*)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/errorsw.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/errorx.inc'
        include 'ipfinc/update.inc'
        include 'ipfinc/usranl.inc'

        character null * 1, linefeed * 1, capital * 40, 
     &            text * 80, word(50) * 60, out_filename * 60
        integer apdoutbuf, o1, o2, status, vltdifrpt, lfodifrpt,
     &          areaintrpt, chglisrpt, inttierpt, phshftrpt,
     &          findstr, scrfil, open_file, open_apnd, capdifrpt,
     &          usranlrpt, busdifrpt, aredifrpt, lindifrpt, ownintchg,
     &          zonintchg, arsummrpt, genrpt, regxrpt, areatotrpt,
     &          qutdifrpt
        logical apndfil, opened

        p_report = 0     ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        null = char(0)
        linefeed = char(10)
        max_buf = len( out_buffer )
        out_buffer(1:1) = null
        apndfil = .true.
        opened = .false.
        out_filename = ' '

        i1 = 1
        i2 = index (in_buffer, null)
        num_word = 0

c*************** Now done by command parser ****************************
c***c
c***c       Replace "*[EOM]" with "(END)"
c***c
c***        last = index (in_buffer, null)
c***        ix = findstr (in_buffer(1:last), '*[EOM]')
c***        if (ix .gt. 0) then
c***           in_buffer(ix:ix+5) = '(END)' // null
c***        endif
c***
c**********************************************************************

        next = nxt_term(in_buffer) 
        if (in_buffer(next:next) .eq. linefeed) then
           next = nxt_term(in_buffer(next+1:)) + next 
        endif
        inrcd = in_buffer(1:next-1)
        call uscan(in_buffer(2:next-1), word, nwrd, '=',' ,'//linefeed)
c
c       Capitalize all word() except <filename>
c
        i = 0
        do while ( i .lt. nwrd )
           i = i + 1
           if ( word(i) .eq. '=' ) i = i + 1
           word(i) = capital(word(i))
           if ( word(i) .eq. 'OUTPUT' ) word(i) = 'APPEND'
           if ( word(i) .eq. 'OVERWRITE'  .or. 
     &          word(i) .eq. 'APPEND' ) then
              i = i + 1
              if ( word(i) .eq. '=' ) i = i + 1
           endif
        enddo

        out_filename = ' '
        scrfil = 0
        isave_lprt = 0
        iwrd = 1
        do while (iwrd .le. nwrd)
           if ( word(iwrd) .eq. 'OVERWRITE'  .or.
     &          word(iwrd) .eq. 'APPEND'           ) then
              if ( word(iwrd) .eq. 'OVERWRITE' ) apndfil = .false.
              iwrd = iwrd + 1
              if (word(iwrd) .eq. '=') iwrd = iwrd + 1
              out_filename = word(iwrd)
              if (out_filename .ne. ' ') then

c                temporarily borrow "lprt" so reports can use "prtpkg"

                 if (lprt .eq. wscfil) then
                    write (errbuf(1),11)
  11                format(' programmer error in P_REPORT -- ',
     &                 'logical units are improperly assigned')
                    call prterx ('F',1)
                    goto 300
                 endif
                 isave_lprt = lprt
                 lprt = wscfil
                 scrfil = wscfil
                 call close_file (scrfil)
                 if ( apndfil ) then
                    status = open_apnd( scrfil, out_filename )
                 else
                    status = open_file(scrfil,out_filename, 'F', 'W', 
     &                                 iost)
                 endif
                 if (status .ne. 0) then
                    scrfil = 0
                    lc = lastch( out_filename )
                    if ( lc .eq. 0 ) lc = 1
                    write( errbuf(1), 111 ) out_filename(1:lc)
  111               format(' Error opening file: ', a )
                    call prterx ('E', 1)
                    write( out_buffer, 113 ) out_filename(1:lc), null
  113               format(' !!! ERROR OPENING FILE :  ',a,' !!!',a)
                    p_report = 1
                    goto 300
                 else
                    opened = .true.
                 endif
              endif
           endif
           iwrd = iwrd + 1
        enddo

	iwrd = 1
        do while (iwrd .le. nwrd)
           if (word(iwrd)(1:6) .eq. 'SELECT') then
              iwrd = iwrd + 1

              if (word(iwrd)(1:9) .eq. 'BUS_INPUT') then
                 itext = findstr (in_buffer, 'BUS_INPUT')
     &                   + lastch (word(iwrd))
                 call businrpt(in_buffer(itext:), out_buffer, scrfil)
                 iwrd = nwrd + 1

              else if (word(iwrd)(1:12) .eq. 'BUS_BR_INPUT') then
                 itext = findstr (in_buffer, 'BUS_BR_INPUT')
     &                   + lastch (word(iwrd))
                 call busbrinrpt(in_buffer(itext:), out_buffer, scrfil)
                 iwrd = nwrd + 1

              else if (word(iwrd)(1:13) .eq. 'BUS_BR_OUTPUT') then
                 itext = findstr (in_buffer, 'BUS_BR_OUTPUT')
     &                   + lastch (word(iwrd))
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
  144               format(' Request rejected - unsolved base data in',
     &                     ' residence')
                    call prterx ('W', 1)
                 else
                    call busbrotrpt(in_buffer(itext:), out_buffer, 
     &                              scrfil)
                 endif

              else if (word(iwrd)(1:8) .eq. 'LINEFLOW') then
                 itext = findstr (in_buffer, 'LINEFLOW')
     &                   + lastch (word(iwrd))
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    call lineflowxl(in_buffer(itext:), out_buffer, 
     &                              scrfil)
                 endif

              else if (word(iwrd)(1:11) .eq. 'AREA_TOTALS') then
                 itext = findstr (in_buffer, 'AREA_TOTALS')
     1                 + lastch (word(iwrd))
                 iwrd = nwrd + 1
                 if (ostates .le. 4) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    call updzon
                    status = areatotrpt(in_buffer(itext:), out_buffer,
     &                                  scrfil)
                 endif

              else if (word(iwrd)(1:16) .eq. 'OVERLOADED_LINES') then
                 itext = findstr (in_buffer, 'OVERLOADED_LINES')
     &                   + lastch (word(iwrd))
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    call ovldlnsrpt(in_buffer(itext:), out_buffer,
     &                              scrfil)
                 endif

              else if (word(iwrd)(1:14) .eq. 'OVERLOADED_TXS') then
                 itext = findstr (in_buffer, 'OVERLOADED_TXS')
     &                   + lastch (word(iwrd))
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    call ovldtxsrpt(in_buffer(itext:), out_buffer,
     &                              scrfil)
                 endif

              else if (word(iwrd)(1:8) .eq. 'BUS_UVOV') then
                 itext = findstr (in_buffer, 'BUS_UVOV')
     &                   + lastch (word(iwrd))
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    call busuvovrpt(in_buffer(itext:), out_buffer,
     &                              scrfil)
                 endif

              else if (word(iwrd)(1:9) .eq. 'VOLTAGE_C') then
                 itext = findstr (in_buffer, 'VOLTAGE_C')
     &                   + lastch (word(iwrd))
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    status = vltdifrpt(in_buffer(itext:), out_buffer,
     &                                 scrfil)
                 endif

              else if (word(iwrd)(1:9) .eq. 'CAPACITOR_C') then
                 itext = findstr (in_buffer, 'CAPACITOR_C')
     &                   + lastch (word(iwrd))
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    status = capdifrpt(in_buffer(itext:), out_buffer,
     &                                 scrfil)
                 endif

              else if (word(iwrd)(1:6) .eq. 'LINE_C') then
                 itext = findstr (in_buffer, 'LINE_C')
     &                   + lastch (word(iwrd))
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    status = lfodifrpt(in_buffer(itext:), out_buffer,
     &                                 scrfil)
                 endif

              else if (word(iwrd)(1:13) .eq. 'FILT_OUTPUT') then
                 itext = findstr (in_buffer, 'FILT_OUTPUT')
     1                 + lastch (word(iwrd))
                 iwrd = nwrd + 1
                 if (ostates .le. 4) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    call filtoutrpt(in_buffer(itext:), out_buffer,
     &                              scrfil)
                 endif

              else if (word(iwrd)(1:10) .eq. 'AI_SUMMARY') then
                 itext = findstr (in_buffer, 'AI_SUMMARY')
     &                   + lastch (word(iwrd))
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    call updzon
                    status = areaintrpt(in_buffer(itext:), out_buffer,
     &                                  scrfil)
                 endif

              else if (word(iwrd)(1:9) .eq. 'AREA_SUMM') then
                 itext = findstr (in_buffer, 'AREA_SUMM')
     1                 + lastch (word(iwrd))
                 iwrd = nwrd + 1
                 if (ostates .le. 4) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    call updzon
                    status = arsummrpt(in_buffer(itext:), out_buffer,
     &                                 scrfil)
                 endif

              else if (word(iwrd)(1:8) .eq. 'TIE_LINE') then
                 itext = findstr (in_buffer, 'TIE_LINE')
     &                   + lastch (word(iwrd))
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    call updzon
                    status = inttierpt(in_buffer(itext:), out_buffer,
     &                                 scrfil)
                 endif

              else if (word(iwrd)(1:9) .eq. 'AREA_SUMM') then
                 itext = findstr (in_buffer, 'AREA_SUMM')
     &                   + lastch (word(iwrd))
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    call updzon
                    status = arsummrpt(in_buffer(itext:), out_buffer,
     &                                 scrfil)
                 endif

              else if (word(iwrd)(1:11) .eq. 'NETWORK_CHA') then
                 itext = findstr (in_buffer, 'NETWORK_CHA')
     &                   + lastch (word(iwrd))
                 status = chglisrpt(in_buffer(itext:), out_buffer,
     &                              scrfil)
                 iwrd = nwrd + 1

              else if (word(iwrd)(1:6) .eq. 'BUS_OV') then
                 itext = findstr (in_buffer, 'BUS_OV')
     1                 + lastch (word(iwrd))
                 iwrd = nwrd + 1
                 if (ostates .le. 4) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    call busovrpt(in_buffer(itext:), out_buffer,
     &                              scrfil)
                 endif
 
              else if (word(iwrd)(1:6) .eq. 'BUS_UV') then
                 itext = findstr (in_buffer, 'BUS_UV')
     1                 + lastch (word(iwrd))
                 iwrd = nwrd + 1
                 if (ostates .le. 4) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    call busuvrpt(in_buffer(itext:), out_buffer,
     &                              scrfil)
                 endif

              else if (word(iwrd)(1:8) .eq. 'GEN_SUMM') then
                 itext = findstr (in_buffer, 'GEN_SUMM')
     1                 + lastch (word(iwrd))
                 iwrd = nwrd + 1
                 if (ostates .le. 4) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    status = genrpt(in_buffer(itext:), out_buffer,
     &                                 scrfil)
                 endif

              else if (word(iwrd)(1:11) .eq. 'PHASE_SHIFT') then
                 itext = findstr (in_buffer, 'PHASE_SHIFT')
     &                   + lastch (word(iwrd))
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    status = phshftrpt(in_buffer(itext:), out_buffer,
     &                                 scrfil)
                 endif

              else if (word(iwrd)(1:8) .eq. 'REG_XFMR') then
                 itext = findstr (in_buffer, 'REG_XFMR')
     1                 + lastch (word(iwrd))
                 iwrd = nwrd + 1
                 if (ostates .le. 4) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    status = regxrpt(in_buffer(itext:), out_buffer,
     &                                 scrfil)
                 endif
 
              else if (word(iwrd)(1:13) .eq. 'USER_ANALYSIS') then
                 itext = findstr (in_buffer, 'USER_ANALYSIS')
     &                   + lastch (word(iwrd))
                 iwrd = nwrd + 1
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
c
c                   Any prior opened USER_ANALYSIS output must be
C                   closed
c
                    if (opened) then
                      call close_file (26)
                    endif
                    status = usranlrpt(in_buffer(itext:), out_buffer,
     &                                 scrfil)
                    if (opened) then
                      usrfil(numusr) = out_filename
                      call close_file (scrfil)
                    endif
                 endif

              else if (word(iwrd)(1:7) .eq. 'AR_COMP') then
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    status = aredifrpt(in_buffer, out_buffer,
     &                                 scrfil)
                 endif

              else if (word(iwrd)(1:8) .eq. 'BUS_COMP') then
                 itext = findstr (in_buffer, 'BUS_COMP')
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    status = busdifrpt(in_buffer(itext:), out_buffer,
     &                                 scrfil)
                 endif

              else if (word(iwrd)(1:7) .eq. 'BR_COMP') then
                 itext = findstr (in_buffer, 'BR_COMP')
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    status = lindifrpt(in_buffer(itext:), out_buffer,
     &                                 scrfil)
                 endif

              else if (word(iwrd)(1:9) .eq. 'OWNER_INT') then
                 itext = findstr (in_buffer, 'OWNER_INT')
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    status = ownintchg(in_buffer(itext:), out_buffer,
     &                                 scrfil)
                 endif

              else if (word(iwrd)(1:8) .eq. 'ZONE_INT') then
                 itext = findstr (in_buffer, 'ZONE_INT')
                 iwrd = nwrd + 1
c                 if (ostates .le. 4) then
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    status = zonintchg(in_buffer(itext:), out_buffer,
     &                                 scrfil)
                 endif

              else if (word(iwrd)(1:13) .eq. 'REACTIVE_UTIL') then
                 itext = findstr (in_buffer, 'REACTIVE_UTIL')
                 iwrd = nwrd + 1
                 if ( lskp .eq. 0 ) then
                    p_report = 1
                    write (errbuf(1), 144)
                    call prterx ('W', 1)
                 else
                    status = qutdifrpt(in_buffer(itext:), out_buffer,
     &                                 scrfil)
                 endif

              else
                 itext = findstr (in_buffer, 'SELECT')
     &                   + len ('SELECT')
                 call outreport(in_buffer(itext:), out_buffer, scrfil)
                 iwrd = nwrd + 1
              endif
           else
              iwrd = iwrd + 1
           endif
        enddo

        call prtime('REPORTS')

  300   continue

        if (isave_lprt .ne. 0) lprt = isave_lprt

        if (scrfil .gt. 0) then
           call close_file (scrfil)
           scrfil = 0
        endif

        inrcd = buf
c
c       Append error messages to buffer
c
        j = 1 
        length = 1
        o2 = index (out_buffer,null)
        do while (j .le. numerr .and. length .gt. 0)
           length = apdoutbuf(o2, errm(j), out_buffer(o2:))
           o2 = o2 + length
           j = j + 1
        enddo
c
c 	Append summary
c
        if ( o2 .gt. max_buf - 50 ) then
           o2 = max_buf - 50
           do while ( o2 .gt. 1  .and.
     &                out_buffer(o2:o2) .ne. linefeed )
              o2 = o2 - 1
           enddo
           out_buffer(o2:o2) = null
        endif
        write (text, 340) 'p_report.f', p_report, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
c
c       Debug printout invoked with IPF command /TRACE, CHANGE = ON
c
        if (kase1(27) .ne. 0) then
           o1 = 1
           do while (o1 .lt. max_buf .and. 
     &              (out_buffer(o1:o1) .ne. null))
              next = nxt_term (out_buffer(o1+1:)) + o1
              o2 = min (o1+131, next)
              write (dbug, 830) out_buffer(o1:o2)             
  830         format (1x, a)
              o1 = next
              if (out_buffer(o1:o1) .eq. linefeed) o1 = o1 + 1
           enddo
        endif
        return
	end
