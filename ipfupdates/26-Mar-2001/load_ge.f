C    %W% %G%
C****************************************************************
C
C     File: load_ge.f
C
C     Purpose: Routine to load GE data into IPF data base
C
C     / LOAD_GE, FILE = <filename>, -
C                REFFILE = <filename>, -
C                TRANSFILE = <filename>, -
C                VERSION = <number>, -
C                RATINGS = (TX=AABC, LN=AAC), -
C                RELAX_BG_VOLTAGES = ON -
C                LTC = AVE
C                      HIGH
C                      LOW
C                      RANGE
C
C     or
C
C     / LOAD_GE, FILE = *, -
C                REFFILE = <filename>, -
C                TRANSFILE = <filename>, -
C                VERSION = <number>, 
C                RATINGS = (TX=AABC, LN=AAC), -
C                RELAX_BG_VOLTAGES = ON -
C                LTC = AVE
C                      HIGH
C                      LOW
C                      RANGE
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: ctlpow.f
C
C****************************************************************
      integer function load_ge (error)
      integer error

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/alt_case.inc'
 
      integer MAXWYEDELTA
      parameter (MAXWYEDELTA = 500)
      common /wye_delta/ num_wye_delta, lwye_delta(3,MAXWYEDELTA),
     &                   wye_delta(MAXWYEDELTA)

      common /num_dcbuses/ count_dcbus_no, dcbus_no(2,MAXMDC),
     &                     highest_bus_no
      integer count_dcbus_no, dcbus_no, highest_bus_no

      character word(50)*60, bigbuf*512, file(3)*60, xbuf*512, 
     &          tempc*120, tempd*120, capital*20, version*10
      logical finished, found, finished_data_segment
      integer findstr, firstxstr, status, ld_gebus, numver, ftn_atoi,
     &        ld_gebrn, ld_geltc, ld_gegen, ld_geld, ld_geshn, 
     &        ld_gesvd, ld_geare, ld_gezon, ld_gebdc, ld_geldc, 
     &        ld_gecdc, ld_geait, ld_geown, ld_gezdt, ld_gemtr, start, 
     &        hsh_ptib, options(10), count, chk_ptib,
     &        open_ge_file, read_ge_file, close_ge_file, ld_gezones
 
      load_ge = 0         ! set default return status = successful
      error = 0           ! initialize error count
      version = '1.0'     ! default GE version number
      call ptihinit ()    ! initialize all PTI hash tables...
      num_wye_delta = 0   ! initialize wye_delta counter

      if (findstr(inrcd,'LOAD_GE') .ne. 0 .or. 
     &    findstr(inrcd,'LOADGE') .ne. 0) then
C
C        / LOAD_GE, FILE = <filename>, -
C                   REFFILE = <filename>, -
C                   VERSION = <number>
C       
        bigbuf = inrcd
        file(1) = ' '
        file(2) = ' '
        file(3) = ' '
c
c       Set default options
c
        options(1) = 0        ! Default TX Nominal = RateA
        options(2) = 1        ! Default TX Thermal = RateA
        options(3) = 2        ! Default TX Emergency = RateB
        options(4) = 3        ! Default TX Bottleneck = RateC
        options(5) = 1        ! Default Ln Nominal = RateA
        options(6) = 1        ! Default Ln Thermal = RateA
        options(7) = 3        ! Default Ln Bottleneck = RateC
        options(8) = 3        ! Default BT/BC voltage to Vmax/Vmin
        options(9) = 0        ! Use/Expand default BG/BX voltage limits
        options(10) = 0       ! Use Translation file zone code
C       
C       Check for and concatenate continuation records.
C       
        last = lastch (bigbuf) 
        do while (bigbuf(last:last) .eq. '-') 
          read (inp, fmt='(a)', end=900) xbuf
          call space (1)  
          write (outbuf, 10000) xbuf(1:80)   
10000     format (' LOAD_GE text (', a, ')')  
          call prtout (1) 
          bigbuf(last+1:) = xbuf
          last = lastch (bigbuf) 
        enddo
        call uscan (bigbuf(1:last), word, nwrd, '=()',  ' ,')   
        iwrd = 1
        do while (iwrd .le. nwrd)
          if ( firstxstr(word(iwrd), 'FILE') .ne. 0)  then
            iwrd = iwrd + 1
            if (word(iwrd) .eq. '=') iwrd = iwrd + 1
            file(1) = word(iwrd)
          else if ( firstxstr(word(iwrd), 'REF*') .ne. 0)  then
            iwrd = iwrd + 1
            if (word(iwrd) .eq. '=') iwrd = iwrd + 1
            file(2) = word(iwrd)
          else if ( firstxstr(word(iwrd), 'TRANS*') .ne. 0)  then
            iwrd = iwrd + 1
            if (word(iwrd) .eq. '=') iwrd = iwrd + 1
            file(3) = word(iwrd)
            options(10) = 1
          else if ( firstxstr(word(iwrd), 'VER*') .ne. 0)  then
            iwrd = iwrd + 1
            if (word(iwrd) .eq. '=') iwrd = iwrd + 1
            numver = ftn_atoi (word(iwrd))
          else if ( firstxstr(word(iwrd), 'RAT*') .ne. 0)  then
            iwrd = iwrd + 1
            if (word(iwrd) .eq. '=') iwrd = iwrd + 1
            found = .true.
            do while (found)
              if ( firstxstr(word(iwrd), 'T*') .ne. 0)  then
                found = .true.  
                iwrd = iwrd + 1
                if (word(iwrd) .eq. '(') iwrd = iwrd + 1
                tempc(1:10) = capital(word(iwrd)(1:10))
                last = lastch (tempc)
                do i = 1, last
                  options(i) = index ('ABC', tempc(i:i))
                enddo
                iwrd = iwrd + 1
              else if ( firstxstr(word(iwrd), 'L*') .ne. 0)  then
                found = .true.  
                iwrd = iwrd + 1
                if (word(iwrd) .eq. '(') iwrd = iwrd + 1
                tempc(1:10) = capital(word(iwrd)(1:10))
                last = lastch (tempc)
                do i = 1, last
                  options(4+i) = index ('ABC', tempc(i:i))
                enddo
                iwrd = iwrd + 1
              else
                found = .false.
              endif
            enddo              
            if (word(iwrd) .eq. ')') iwrd = iwrd + 1
          else if ( firstxstr(word(iwrd), 'LTC*') .ne. 0)  then
            iwrd = iwrd + 1
            if (word(iwrd) .eq. '=') iwrd = iwrd + 1
            if (firstxstr(word(iwrd), 'AV*') .ne. 0)  then
              options(8) = 1
            else if (firstxstr(word(iwrd), 'HI*') .ne. 0)  then
              options(8) = 2
            else if (firstxstr(word(iwrd), 'LO*') .ne. 0)  then
              options(8) = 3
            else if (firstxstr(word(iwrd), 'RA*') .ne. 0)  then
              options(8) = 4
            endif
          else if ( firstxstr(word(iwrd), 'RELAX*') .ne. 0)  then
            iwrd = iwrd + 1
            if (word(iwrd) .eq. '=') iwrd = iwrd + 1
            if (firstxstr(word(iwrd), 'ON*') .ne. 0)  then
              options(9) = 1
            else
              options(9) = 0
            endif
          endif
          iwrd = iwrd + 1
        enddo
c
c       PTI data files description        logical unit   file name
c
c       file(1)        GE EPC data        busbrn         <file=...>
c       file(2)        Reference file     busfil         <reffile=...>
c       file(3)        Reference file     busfil         <tranfile=...>
c
c       Process Reference file
c
        if (file(2) .ne. ' ') then
          call close_file (busfil)
          ierr = 99       ! Indicate that input file is binary formatted
          call opnfil (busfil, file(2), ierr)
          if (ierr .ne. 0) then
            last = lastch( file(2) )
            if ( lc .eq. 0 ) lc = 1
            write( errbuf(1), 10010) file(2)(1:last)
10010       format('Reference base file ', a, ' cannot be read. This may
     & not be an IPF base case.')
            call prterx ('E', 1)
            outbuf = errbuf(1)
            call prtout (1)
            load_ge = 1
            error = 1
            goto 900
          endif
c
c         Function ldaltbse (load alternate base) defines
c         "ocase" and "ofilename"
c
          ocase = ' '
          last = lastch (file(2))
          write (*, 10020)
10020     format (' * Loading reference base case -- this will take a mi
     &nute')
          status = ldaltbse (busfil, file(2)(1:last), ocase, loaded)
          if (status .ne. 0) then
            write( errbuf(1), 10010) file(2)(1:last)
          else
            call close_file(busfil)
          endif
        endif
c
c       If a Translation file is specified, load and hash these
c       zone numbers and names.  They will supersede any matching 
c       zone names later read. 
c
        status = ld_gezones (options, file(3))

        call prt_gehd (options)
c
c       Process raw data file
c
        if (file(1) .eq. '*') then
          write (errbuf(1), 10022) 
10022     format ('Illegal "FILE=*" option on / LOAD_GE command')
          call prterx ('E', 1)
          go to 790
        else if (file(1) .ne. ' ') then
          call close_file (busbrn)
          last = lastch (file(1)) + 1
          file(1)(last:last) = char(0)
          status = open_ge_file (0, file(1)(1:last), 'r')
          if (status .ne. 0) then
            last = lastch( file(1) )
            if ( lc .eq. 0 ) lc = 1
            write( errbuf(1), 10010) file(1)(1:last)
            call prterx ('E', 1)
            outbuf = errbuf(1)
            call prtout (1)
            load_ge = 1
            error = 1
            goto 900
          endif
        endif

        write (*, 10030)
10030   format (' * Loading GE *.epc data file - this will take a few mi
     &nutes')
c
c       Process # records, comment records, title records, and solution

c       records
C
        numrec = 0
        error = 0
        if (file(1) .ne. ' ') then
          finished = .false.
          do while (.not. finished)
            xbuf = ' '
            last = read_ge_file (0, xbuf)
            if (last .eq. 0) go to 300
            numrec = numrec + 1
            call uscan (xbuf(1:last), word, nwrd, '~',  ' ')   
            if (xbuf(1:1) .eq. '#') then
              tempc = capital (word(2))
              if (tempc .eq. 'VERSION') version = word(3)
            else
              tempc = capital (word(1))
              if (tempc .eq. 'TITLE') then
                num_header = 0
                do while (xbuf(1:1) .ne. '!')
                  xbuf = ' '
                  last = read_ge_file (0, xbuf)
                  if (last .eq. 0) go to 300
                  numrec = numrec + 1
c
c                 Process case info
c
                  if (xbuf(1:1) .ne. '!') then
                    num_header = num_header + 1
                    if (num_header .lt. 10) then
                      write (tempc, 10032) num_header, xbuf(1:115)
10032                 format ('.#H', i1, 1x, a)
                      call casetxt (tempc)
                    endif
                  endif
                enddo
              else if (tempc .eq. 'COMMENTS') then
                num_header = 0
                do while (xbuf(1:1) .ne. '!')
                  xbuf = ' '
                  last = read_ge_file (0, xbuf)
                  if (last .eq. 0) go to 300
                  numrec = numrec + 1
c
c                 Process case info
c
                  if (xbuf(1:1) .ne. '!') then
                    num_header = num_header + 1
                    if (num_header .lt. 10) then
                      write (tempc, 10034) num_header, xbuf(1:113)
10034                 format ('.#C', i3.3, 1x, a)
                      call casetxt (tempc)
                    endif
                  endif
                enddo
              else if (tempc .eq. 'SOLUTION') then
                do while (xbuf(1:1) .ne. '!')
                  xbuf = ' '
                  last = read_ge_file (0, xbuf)
                  if (last .eq. 0) go to 300
                  numrec = numrec + 1
c
c                 Process case info
c
                  if (xbuf(1:1) .ne. '!') then
                    tempc = '.# Solution ' // xbuf
                    call casetxt (tempc)
                  endif
                enddo
              else
                finished = .true.
              endif 
            endif
          enddo

          go to 320

  300     write (errbuf(1), 10040) 
10040     format ('E-O-F encountered processing title records in raw dat
     &a file ')
          call prterx ('W', 1)
          go to 790

  320     continue
c
c         Main loop to process all data segments
c
          finished_data_segment = .false.
          do while (.not. finished_data_segment)
            call uscan (xbuf(1:last), word, nwrd, '[]',  ' ')   
            tempc = capital (word(1))
            if (nwrd .gt. 1) then
              tempd = capital (word(2))
              if (tempd .ne. 'DATA') then
                last = lastch (tempc)
                tempc(last+2:) = tempd
              endif
            endif

            if (tempc .eq. 'BUS') then
c
c             Process Bus data in raw data file
C
              count = ftn_atoi(word(4))
              start = numrec
              finished = .false.
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 340
                numrec = numrec + 1
                status = ld_gebus (xbuf, 0, options, ierror)
                error = error + ierror
                if (status .ne. 0) then
                   finished = .true.
                endif
              enddo
              write (*, 10070) numrec-start-1
10070         format (' * Loaded  ', i5, ' bus records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10080) count, numrec-start-1
10080           format ('Actual count of Bus Data records (', i5, 
     &            ') does not agree with prescribed count (', i5, ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 360

  340         write (errbuf(1), 10090) 
10090         format ('E-O-F encountered processing Bus records in raw d
     &ata file ')
              call prterx ('W', 1)
              go to 790

  360         continue

            else if (tempc .eq. 'BRANCH') then
c
c             Process Branch data in raw data file
C
              count = ftn_atoi(word(4))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 370
                numrec = numrec + 1
                status = ld_gebrn (xbuf, 0, options, ierror)
                error = error + ierror
                if (status .ne. 0) then
                   finished = .true.
                endif
              enddo
              write (*, 10120) 2*(numrec-start-1)
10120         format (' * Loaded  ', i5, ' branch records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10130) count, numrec-start-1
10130           format ('Actual count of Branch Data records (', i5, 
     &            ') does not agree with prescribed count (', i5, ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 380

  370         write (errbuf(1), 10140) 
10140         format ('E-O-F encountered processing Branch data records 
     &in raw data file ')
              call prterx ('W', 1)
              go to 790

  380         continue

           else if (tempc .eq. 'TRANSFORMER') then
c 
c             Process Transformer data in raw data file
C
              count = ftn_atoi(word(4))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 390
                numrec = numrec + 1
                status = ld_geltc (xbuf, 0, options, ierror)
                error = error + ierror
                if (status .ne. 0) then
                   finished = .true.
                endif
              enddo
              write (*, 10170) 3*(numrec-start-1)
10170         format (' * Loaded  ', i5, ' transformer records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10180) count, numrec-start-1
10180           format ('Actual count of Transformer Data records (', 
     &            i5, ') does not agree with prescribed count (', i5, 
     &            ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 400

  390         write (errbuf(1), 10190) 
10190         format ('E-O-F encountered processing transformer data rec
     &ords in raw data file ')
              call prterx ('W', 1)
              go to 790

  400         continue

            else if (tempc .eq. 'GENERATOR') then
c
c             Process Generator data in raw data file
C
              count = ftn_atoi(word(4))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 410
                numrec = numrec + 1
                status = ld_gegen (xbuf, 0, options, ierror)
                error = error + ierror
                if (status .ne. 0) then
                   finished = .true.
                endif
              enddo
              write (*, 10220) 2*(numrec-start-1)
10220         format (' * Loaded  ', i5, ' generator records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10230) count, numrec-start-1
10230           format ('Actual count of Generator Data records (', i5, 
     &            ') does not agree with prescribed count (', i5, ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 420

  410         write (errbuf(1), 10240) 
10240         format ('E-O-F encountered processing Generator records in
     & raw data file ')
              call prterx ('W', 1)
              go to 790

  420         continue

            else if (tempc .eq. 'LOAD') then
c
c             Process Load data in raw data file
C
              count = ftn_atoi(word(4))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 430
                numrec = numrec + 1
                status = ld_geld (xbuf, 0, options, ierror)
                error = error + ierror
                if (status .ne. 0) then
                   finished = .true.
                endif
              enddo
              write (*, 10270) numrec-start-1
10270         format (' * Loaded  ', i5, ' load records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10280) count, numrec-start-1
10280           format ('Actual count of Load Data records (', i5, 
     &            ') does not agree with prescribed count (', i5, ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 440

  430         write (errbuf(1), 10290) 
10290         format ('E-O-F encountered processing Load records in raw 
     &data file ')
              call prterx ('W', 1)
              go to 790

  440         continue

            else if (tempc .eq. 'SHUNT') then
c
c             Process Shunt data in raw data file
C
              count = ftn_atoi(word(4))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 450
                numrec = numrec + 1
                status = ld_geshn (xbuf, 0, options, ierror)
                error = error + ierror
                if (status .ne. 0) then
                   finished = .true.
                endif
              enddo
              write (*, 10320) numrec-start-1
10320         format (' * Loaded  ', i5, ' shunt records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10330) count, numrec-start-1
10330           format ('Actual count of Shunt Data records (', i5, 
     &            ') does not agree with prescribed count (', i5, ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 460

  450         write (errbuf(1), 10340) 
10340         format ('E-O-F encountered processing Shunt records in raw
     & data file ')
              call prterx ('W', 1)
              go to 790

  460         continue

            else if (tempc .eq. 'SVD') then
c
c             Process SVD data in raw data file
c
              count = ftn_atoi(word(4))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 470
                numrec = numrec + 1
                status = ld_gesvd (xbuf, 0, options, ierror)
                error = error + ierror
                if (status .ne. 0) then
                   finished = .true.
                endif
              enddo
              write (*, 10370) 2*(numrec-start-1)
10370         format (' * Loaded  ', i5, ' SVD records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10380) count, numrec-start-1
10380           format ('Actual count of SVD Data records (', i5, 
     &            ') does not agree with prescribed count (', i5, ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 480

  470         write (errbuf(1), 10390) 
10390         format ('E-O-F encountered processing SVD records in raw d
     &ata file ')
              call prterx ('W', 1)
              go to 790

  480         continue

            else if (tempc .eq. 'AREA') then
c
c             Process Area data in raw data file
C
              count = ftn_atoi(word(4))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 490
                numrec = numrec + 1
                status = ld_geare (xbuf, 0, options, ierror)
                error = error + ierror
                if (status .ne. 0) then
                   finished = .true.
                endif
              enddo
              write (*, 10420) numrec-start-1
10420         format (' * Loaded  ', i5, ' area records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10430) count, numrec-start-1
10430           format ('Actual count of Area Data records (', i5, 
     &            ') does not agree with prescribed count (', i5, ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 500

  490         write (errbuf(1), 10440) 
10440         format ('E-O-F encountered processing Area records in raw 
     &data file ')
              call prterx ('W', 1)
              go to 790

  500         continue

            else if (tempc .eq. 'ZONE') then
c
c             Process Zone data in raw data file
C
              count = ftn_atoi(word(4))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 510
                numrec = numrec + 1
                status = ld_gezon (xbuf, 0, options, ierror)
                error = error + ierror
                if (status .ne. 0) then
                   finished = .true.
                endif
              enddo
              write (*, 10470) numrec-start-1
10470         format (' * Loaded  ', i5, ' zone records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10480) count, numrec-start-1
10480           format ('Actual count of Zone Data records (', i5, 
     &            ') does not agree with prescribed count (', i5, ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 520

  510         write (errbuf(1), 10490) 
10490         format ('E-O-F encountered processing Zone records in raw 
     &data file ')
              call prterx ('W', 1)
              go to 790

  520         continue

            else if (tempc .eq. 'INTERFACE') then
c
c             Skip Interface data in raw data file
C
              count = ftn_atoi(word(4))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 530
                numrec = numrec + 1
                call uscan (xbuf(1:last), word, nwrd, '[]',  ' ')   
                if (ftn_atoi(word(1)) .ne. 0) then
                else
                   finished = .true.
                endif
              enddo
              write (*, 10510) numrec-start-1
10510         format (' * Skipped ', i5, ' interface records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10520) count, numrec-start-1
10520           format ('Actual count of Interface Data records (', i5, 
     &            ') does not agree with prescribed count (', i5, ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 540
 
  530         write (errbuf(1), 10530) 
10530         format ('E-O-F encountered processing Interface records in
     & raw data file ')
              call prterx ('W', 1)
              go to 790

  540         continue

            else if (tempc .eq. 'INTERFACE BRANCH') then
c
c             Skip Interface Branch data in raw data file
C
              count = ftn_atoi(word(5))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 550
                numrec = numrec + 1
                call uscan (xbuf(1:last), word, nwrd, '[]',  ' ')   
                if (ftn_atoi(word(1)) .ne. 0) then
                else if (word(1)(1:1) .ne. '0') then
                   finished = .true.
                endif
              enddo
              write (*, 10550) numrec-start-1
10550         format (' * Skipped ', i5, ' interface branch records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10560) count, numrec-start-1
10560           format ('Actual count of Interface Branch Data records (
     &', i5, ') does not agree with prescribed count (', i5, ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 560

  550         write (errbuf(1), 10570) 
10570         format ('E-O-F encountered processing Interface Branch rec
     &ords in raw data file ')
              call prterx ('W', 1)
              go to 790

  560         continue

            else if (tempc .eq. 'DC BUS') then
c
c             Process d-c bus data in raw data file
C
c             Preliminary - d-c buses require new bus numbers
c
              highest_bus_no = 0
              do i = 1, num_hashn
                highest_bus_no = max0 (pti_num(i), highest_bus_no)
              enddo

              count = ftn_atoi(word(5))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 570
                numrec = numrec + 1
                status = ld_gebdc (xbuf, 0, options, ierror)
                error = error + ierror
                if (status .ne. 0) then
                   finished = .true.
                endif
              enddo
              write (*, 10590) numrec-start-1
10590         format (' * Loaded  ', i5, ' bus dc records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10600) count, numrec-start-1
10600           format ('Actual count of DC Bus Data records (', i5, 
     &            ') does not agree with prescribed count (', i5, ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 580

  570         write (errbuf(1), 10610) 
10610         format ('E-O-F encountered processing DC Bus records in ra
     &wdata file ')
              call prterx ('W', 1)
              go to 790

  580         continue

            else if (tempc .eq. 'DC LINE') then
c
c             Process d-c line data in raw data file
C
              count = ftn_atoi(word(5))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 590
                numrec = numrec + 1
                status = ld_geldc (xbuf, 0, options, ierror)
                error = error + ierror
                if (status .ne. 0) then
                  finished = .true.
                endif
              enddo
              write (*, 10630) 2*(numrec-start-1)
10630         format (' * Loaded  ', i5, ' dc line records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10640) count, numrec-start-1
10640           format ('Actual count of DC Line Data records (', i5, 
     &            ') does not agree with prescribed count (', i5, ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 600

  590         write (errbuf(1), 10650) 
10650         format ('E-O-F encountered processing DC Line records in r
     &aw data file ')
              call prterx ('W', 1)
              go to 790

  600         continue

            else if (tempc .eq. 'DC CONVERTER') then
c
c             Process d-c converter data in raw data file
C
              count = ftn_atoi(word(5))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 610
                numrec = numrec + 1
                status = ld_gecdc (xbuf, 0, options, ierror)
                error = error + ierror
                if (status .ne. 0) then
                  finished = .true.
                endif
              enddo
              write (*, 10680) 5*(numrec-start-1)
10680         format (' * Loaded  ', i5, ' dc converter records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10690) count, numrec-start-1
10690           format ('Actual count of DC Converter Data records (', 
     &            i5, ') does not agree with prescribed count (', i5, 
     &            ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 620

  610         write (errbuf(1), 10700) 
10700         format ('E-O-F encountered processing DC Converter records
     & in raw data file ')
              call prterx ('W', 1)
              go to 790

  620         continue

            else if (tempc .eq. 'Z TABLE') then
c
c             Store transformer Z data in raw data file
C
              count = ftn_atoi(word(5))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 622
                numrec = numrec + 1
                status = ld_gezdt (xbuf, 0, options, ierror)
                error = error + ierror
                if (status .ne. 0) then
                   finished = .true.
                endif
              enddo
              write (*, 10704) numrec-start-1
10704         format (' * Loaded  ', i5, ' z data records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10706) count, numrec-start-1
10706           format ('Actual count of Z Data records (', i5, 
     &            ') does not agree with prescribed count (', i5, ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 624

  622         write (errbuf(1), 10708) 
10708         format ('E-O-F encountered processing Z Data records in ra
     &wdata file ')
              call prterx ('W', 1)
              go to 790

  624         continue

            else if (tempc .eq. 'PED') then
c
c             Skip ped data in raw data file
C
              count = ftn_atoi(word(4))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 630
                numrec = numrec + 1
                call uscan (xbuf(1:last), word, nwrd, '[]',  ' ')   
                if (ftn_atoi(word(1)) .ne. 0) then
                else
                   finished = .true.
                endif
              enddo
              write (*, 10720) numrec-start-1
10720         format (' * Skipped ', i5, ' ped records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10730) count, numrec-start-1
10730           format ('Actual count of PED Data records (', i5, 
     &            ') does not agree with prescribed count (', i5, ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 640

  630         write (errbuf(1), 10740) 
10740         format ('E-O-F encountered skipping PED records in raw dat
     &a file ')
              call prterx ('W', 1)
              go to 790
 
  640         continue

            else if (tempc .eq. 'TRANSACTION') then
c
c             Process area transaction data in raw data file
C
              count = ftn_atoi(word(4))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 650
                numrec = numrec + 1
                status = ld_geait (xbuf, 0, options, ierror)
                error = error + ierror
                if (status .ne. 0) then
                   finished = .true.
                endif
              enddo
              write (*, 10760) numrec-start-1
10760         format (' * Loaded  ', i5, ' transaction records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10770) count, numrec-start-1
10770           format ('Actual count of Transaction Data records (', 
     &            i5, ') does not agree with prescribed count (', i5, 
     &            ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 660

  650         write (errbuf(1), 10780) 
10780         format ('E-O-F encountered processing transaction records 
     &in raw data file ')
              call prterx ('W', 1)
              go to 790

  660         continue

            else if (tempc .eq. 'OWNER') then
c
c             Process owner data in raw data file
C
              count = ftn_atoi(word(4))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 670
                numrec = numrec + 1
                status = ld_geown (xbuf, 0, options, ierror)
                error = error + ierror
                if (status .ne. 0) then
                   finished = .true.
                endif
              enddo
              write (*, 10800) numrec-start-1
10800         format (' * Loaded  ', i5, ' owner records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10810) count, numrec-start-1
10810           format ('Actual count of Owner Data records (', i5, 
     &            ') does not agree with prescribed count (', i5, ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 680

  670         write (errbuf(1), 10820) 
10820         format ('E-O-F encountered processing owner records in raw
     & data file ')
              call prterx ('W', 1)
              go to 790

  680         continue

            else if (tempc .eq. 'MOTOR') then
c
c             Process Motor data in raw data file
C
              count = ftn_atoi(word(4))
              finished = .false.
              start = numrec
              do while (.not. finished)
                xbuf = ' '
                last = read_ge_file (0, xbuf)
                if (last .eq. 0) go to 690
                numrec = numrec + 1
                status = ld_gemtr (xbuf, 0, options, ierror)
                error = error + ierror
                if (status .ne. 0) then
                   finished = .true.
                endif
              enddo
              write (*, 10840) numrec-start-1
10840         format (' * Loaded  ', i5, ' Motor records')
              if (count .ne. numrec-start-1) then
                write (errbuf(1), 10850) count, numrec-start-1
10850           format ('Actual count of Motor Data records (', i5, 
     &            ') does not agree with prescribed count (', i5, ')')
                call prterx ('W', 1)
                error = error + 1
              endif
              go to 790

  690         write (errbuf(1), 10860) 
10860         format ('E-O-F encountered processing Motor records in raw
     & data file ')
              call prterx ('W', 1)
              go to 790

            else if (tempc .eq. 'END') then

              finished_data_segment = .true.

            else
              write (errbuf(1), 10862) xbuf(1:60)
10862         format ('Unrecognized EPC data segment [', a, ']')
              call prterx ('W', 1)
              go to 790
            endif
          enddo
        endif
      endif

  790 continue

      last = lastch (file(1))
      write (outbuf, 10870) numrec, error, file(1)(1:last)
10870 format (1x, i5, ' records, ', i5, ' warnings in file ', a)
      call prtout (1)
c
c     Hash the bus names, identify any duplicates, and generate 
c     an alpha cross reference
c
      status = chk_ptib()
      status = hsh_ptib()

      call clnup_ge (error)

  900 continue

      status = close_ge_file (0)
      read (inp, fmt='(a)', end=900) inrcd
      call space (1)  
      write (outbuf, 10000) xbuf(1:80)   
      call prtout (1) 

      return
      end
