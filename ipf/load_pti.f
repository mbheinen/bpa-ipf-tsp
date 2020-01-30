C    @(#)load_pti.f	20.17 5/3/00
C****************************************************************  
C  
C     File: load_pti.f  
C  
C     Purpose: Routine to load PTI data into IPF data base  
C  
C     / LOAD_PTI, FILE = <filename>, -  
C                 REFFILE = <filename>, -  
C                 VERSION = <number>, -  
C                 RATINGS = (TX=AABC, LN=AAC), -  
C                 LTC = AVE  
C                       HIGH  
C                       LOW  
C                       RANGE, -  
C                 RENAME_ZONES = AREA_NUMBER  
C  
C     or  
C  
C     / LOAD_PTI, FILE = *, -  
C                 REFFILE = <filename>, -  
C                 VERSION = <number>,   
C                 RATINGS = (TX=AABC, LN=AAC), -  
C                 LTC = AVE  
C                       HIGH  
C                       LOW  
C                       RANGE, -  
C                 RENAME_ZONES = AREA_NUMBER  
C  
C     Author: Walt Powell  Date: 21 May 1996  
C     Called by: ctlpow.f  
C  
C****************************************************************  
      integer function load_pti (error)  
      integer error  
  
      include 'ipfinc/parametr.inc'  
  
      include 'ipfinc/blank.inc'  
      include 'ipfinc/pti_data.inc'  
      include 'ipfinc/filnam.inc'  
      include 'ipfinc/jobctl.inc'  
      include 'ipfinc/lfiles.inc'  
      include 'ipfinc/prt.inc'  
      include 'ipfinc/alt_case.inc'  
      include 'ipfinc/errorx.inc'  
   
      character word(20)*60, bigbuf*512, file(2)*60, xbuf*240,   
     &          tempc*240, capital*10  
      logical finished, found  
      integer findstr, firstxstr, status, ldptibus, numver, ftn_atoi,  
     &        ldptigen, ldptibrn, ldptiltc, ldptiare, ldpti2dc,   
     &        ldptixdt, ldptimdc, ldptisec, ldptizon, chk_ptib,  
     &        start, ldptiint, tempfile, open_file, hsh_ptib,  
     &        ldptiown, options(10), initial_errcnt(5)  
   
      load_pti = 0        ! set default return status = successful  
      error = 0           ! initialize error count  
      numver = 23         ! default PSS/E version number  
      call ptihinit ()    ! initialize all PTI hash tables...  
  
      if (findstr(inrcd,'LOAD_PTI') .ne. 0 .or.   
     &    findstr(inrcd,'LOADPTI') .ne. 0) then  
C  
C        / LOAD_PTI, FILE = <filename>, -  
C                    REFFILE = <filename>, -  
C                    VERSION = <number>  
C         
        bigbuf = inrcd  
        file(1) = ' '  
        file(2) = ' '  
c  
c       Set default options  
c  
        options(1) = 1        ! Default TX Nominal = RateA  
        options(2) = 1        ! Default TX Thermal = RateA  
        options(3) = 2        ! Default TX Emergency = RateB  
        options(4) = 3        ! Default TX Bottleneck = RateC  
        options(5) = 1        ! Default Ln Nominal = RateA  
        options(6) = 1        ! Default Ln Thermal = RateA  
        options(7) = 3        ! Default Ln Bottleneck = RateC  
        options(8) = 1        ! Default BT/BC voltage to average  
        options(9) = 0        ! Default Zone names  
        options(10) = 0       ! Unused  
C         
C       Check for and concatenate continuation records.  
C         
        last = lastch (bigbuf)   
        do while (bigbuf(last:last) .eq. '-')   
          read (inp, fmt='(a)', end=900) inrcd  
          call space (1)    
          write (outbuf, 100) xbuf(1:80)     
  100     format (' LOAD_PTI text (', a, ')')    
          call prtout (1)   
          bigbuf(last+1:) = inrcd  
        enddo  
        call uscan (bigbuf, word, nwrd, '=()',  ' ,')     
  
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
          else if ( firstxstr(word(iwrd), 'RENAME*') .ne. 0)  then  
            iwrd = iwrd + 1  
            if (word(iwrd) .eq. '=') iwrd = iwrd + 1  
            options(9) = 1  
          endif  
          iwrd = iwrd + 1  
        enddo  
c  
c       PTI data files description        logical unit   file name  
c  
c       file(1)        PTI raw data       busbrn         <file=...>  
c       file(2)        Reference file     busfil         <tranfile=...>  
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
            write( errbuf(1), 110 ) file(2)(1:last)  
  110       format('Reference base file ', a, ' cannot be read. This may  
     & not be an IPF base case.')  
            call prterx ('E', 1)  
            outbuf = errbuf(1)  
            call prtout (1)  
            load_pti = 1  
            error = 1  
            goto 900  
          endif  
c  
c         Function ldaltbse (load alternate base) defines  
c         "ocase" and "ofilename"  
c  
          ocase = ' '  
          last = lastch (file(2))  
          write (*, 90)  
   90     format (' * Loading reference base case -- this will take a mi  
     &nute')  
          status = ldaltbse (busfil, file(2)(1:last), ocase, loaded)  
          if (status .ne. 0) then  
            write( errbuf(1), 110) file(2)(1:last)  
          else  
            call close_file(busfil)  
          endif  
        endif  
  
        do i = 1, 5  
          initial_errcnt(i) = errcnt(i)  
        enddo  
  
        call prtptihd (options)  
c  
c       Process raw data file  
c  
        if (file(1) .eq. '*') then  
          tempfile = inp  
        else if (file(1) .ne. ' ') then  
          tempfile = busbrn  
          call close_file (busbrn)  
          status = open_file (tempfile, file(1), 'F', 'R', iost)  
          if (status .ne. 0) then  
            last = lastch( file(1) )  
            if ( lc .eq. 0 ) lc = 1  
            write( errbuf(1), 110 ) file(1)(1:last)  
            call prterx ('E', 1)  
            outbuf = errbuf(1)  
            call prtout (1)  
            load_pti = 1  
            error = 1  
            goto 900  
          endif  
        endif  
c  
c       Read in Title records  
C  
        numrec = 0  
        error = 0  
        if (file(1) .ne. ' ') then  
          finished = .false.  
          do i = 1, 3  
            read (tempfile, fmt='(a)', end=300) xbuf  
            numrec = numrec + 1  
c  
c           Process " .# " case info  
c  
            tempc = '.#' // xbuf  
            call casetxt (tempc)  
          enddo  
  
          go to 320  
  
  300     write (errbuf(1), 310)   
  310     format ('E-O-F encountered processing title records in raw dat  
     &a file ')  
          call prterx ('W', 1)  
          go to 790  
  
  320     continue  
c  
c         Read in and process Bus data in raw data file  
C  
          write (*, 322)  
  322     format (' * Loading PTI raw data file - this will take a few m  
     &inutes')  
          start = numrec  
          finished = .false.  
          do while (.not. finished)  
            read (tempfile, fmt='(a)', end=340) xbuf  
            numrec = numrec + 1  
            status = ldptibus(xbuf, options, numver, ierror)  
            error = error + ierror  
            if (status .ne. 0) then  
               finished = .true.  
            endif  
          enddo  
          write (*, 334) numrec-start-1  
  334     format (' * Loaded ', i5, ' bus records')  
          go to 360  
  
  340     write (errbuf(1), 350)   
  350     format ('E-O-F encountered processing Bus records in raw data  
     &file ')  
          call prterx ('W', 1)  
          go to 790  
  
  360     continue  
c  
c         Read in and process Load data in raw data file  
c  
          finished = (numver .le. 23)  
          start = numrec  
          do while (.not. finished)  
            read (tempfile, fmt='(a)', end=364) xbuf  
            numrec = numrec + 1  
            status = ldptild (xbuf, options, numver, ierror)  
            error = error + ierror  
            if (status .ne. 0) then  
               finished = .true.  
            endif  
          enddo  
          write (*, 362) numrec-start-1  
  362     format (' * Loaded ', i5, ' Load records')  
          go to 368  
  
  364     write (errbuf(1), 366)   
  366     format ('E-O-F encountered processing Load records in raw data  
     & file ')  
          call prterx ('W', 1)  
          go to 790  
  
  368     continue  
c  
c         Read in and process Generator data in raw data file  
c  
          finished = .false.  
          start = numrec  
          do while (.not. finished)  
            read (tempfile, fmt='(a)', end=394) xbuf  
            numrec = numrec + 1  
            status = ldptigen (xbuf, options, numver, ierror)  
            error = error + ierror  
            if (status .ne. 0) then  
               finished = .true.  
            endif  
          enddo  
          write (*, 392) numrec-start-1  
  392     format (' * Loaded ', i5, ' generator records')  
          go to 398  
  
  394     write (errbuf(1), 396)   
  396     format ('E-O-F encountered processing Generator records in raw  
     & data file ')  
          call prterx ('W', 1)  
          go to 790  
  
  398     continue  
c  
c         Read in and process Branch data in raw data file  
C  
          finished = .false.  
          start = numrec  
          do while (.not. finished)  
            read (tempfile, fmt='(a)', end=420) xbuf  
            numrec = numrec + 1  
            status = ldptibrn (xbuf, options, numver, ierror)  
            error = error + ierror  
            if (status .ne. 0) then  
               finished = .true.  
            endif  
          enddo  
          write (*, 412) numrec-start-1  
  412     format (' * Loaded ', i5, ' branch records')  
          go to 440  
  
  420     write (errbuf(1), 430)   
  430     format ('E-O-F encountered processing Branch data records in r  
     &aw data file ')  
          call prterx ('W', 1)  
          go to 790  
  
  440     continue  
c  
c         Read in and process LTC data in raw data file  
C  
          finished = .false.  
          start = numrec  
          do while (.not. finished)  
            read (tempfile, fmt='(a)', end=460) xbuf  
            numrec = numrec + 1  
            status = ldptiltc (xbuf, options, numver, ierror)  
            error = error + ierror  
            if (status .ne. 0) then  
               finished = .true.  
            endif  
          enddo  
          write (*, 452) numrec-start-1  
  452     format (' * Loaded ', i5, ' LTC records')  
          go to 480  
  
  460     write (errbuf(1), 470)   
  470     format ('E-O-F encountered processing LTC data records in raw  
     & data file ')  
          call prterx ('W', 1)  
          go to 790  
  
  480     continue  
c  
c         Read in and process Area Interchange data in raw data file  
C  
          finished = .false.  
          start = numrec  
          do while (.not. finished)  
            read (tempfile, fmt='(a)', end=500) xbuf  
            numrec = numrec + 1  
            status = ldptiare (xbuf, options, numver, ierror)  
            error = error + ierror  
            if (status .ne. 0) then  
               finished = .true.  
            endif  
          enddo  
          write (*, 492) numrec-start-1  
  492     format (' * Loaded ', i5, ' area interchange records')  
          go to 520  
  
  500     write (errbuf(1), 510)   
  510     format ('E-O-F encountered processing Area Interchange data re  
     &cords in raw data file ')  
          call prterx ('W', 1)  
          go to 790  
  
  520     continue  
c  
c         Read in and process 2-terminal d-c data in raw data file in  
c         blocks of three records  
C  
          finished = .false.  
          last = 0  
          start = numrec  
          do while (.not. finished)  
            read (tempfile, fmt='(a)', end=540) xbuf  
            numrec = numrec + 1  
            status = ldpti2dc (xbuf, tempfile, options, numver, ierror)  
            error = error + ierror  
            if (status .ne. 0) then  
               finished = .true.  
            else  
               numrec = numrec + 2  
            endif  
          enddo  
          write (*, 532) numrec-start-1  
  532     format (' * Loaded ', i5, ' 2-terminal d-c records')  
          go to 560  
  
  540     write (errbuf(1), 550)   
  550     format ('E-O-F encountered processing 2-terminal d-c data reco  
     &rds in raw data file ')  
          call prterx ('W', 1)  
          go to 790  
  
  560     continue  
c  
c         Read in and process Switched Reactance data in raw data file  
C  
          finished = .false.  
          start = numrec  
          do while (.not. finished)  
            read (tempfile, fmt='(a)', end=580) xbuf  
            numrec = numrec + 1  
            status = ldptixdt (xbuf, options, numver, ierror)  
            error = error + ierror  
            if (status .ne. 0) then  
               finished = .true.  
            endif  
          enddo  
          write (*, 572) numrec-start-1  
  572     format (' * Loaded ', i5, ' switched reactance records')  
          go to 600  
  
  580     write (errbuf(1), 590)   
  590     format ('E-O-F encountered processing Switched Reactance data  
     &records in raw data file ')  
          call prterx ('W', 1)  
          go to 790  
  
  600     continue  
c  
c         Skip Transformer Impedance Correction data in raw data file  
C  
          finished = .false.  
          start = numrec  
          do while (.not. finished)  
            read (tempfile, fmt='(a)', end=610) xbuf  
            numrec = numrec + 1  
            if (xbuf(1:2) .eq. ' 0' .or. xbuf(1:2) .eq. '0 ')   
     &         finished = .true.  
          enddo  
          write (*, 602) numrec-start-1  
  602     format (' * Skipped', i5, ' transformer impedance correction   
  
     &records')  
          go to 630  
  
  610     write (errbuf(1), 620)   
  620     format ('E-O-F encountered processing Transformer Impedance Co  
     &rrections data records in raw data file ')  
          call prterx ('W', 1)  
          go to 790  
  
  630     continue  
c  
c         Read in and process Multi-terminal d-c data in raw data file  
C  
          finished = .false.  
          start = numrec  
          do while (.not. finished)  
            read (tempfile, fmt='(a)', end=650) xbuf  
            status = ldptimdc (xbuf, tempfile, options, numver, inum,   
     &                         ierror)  
            numrec = numrec + inum  
            error = error + ierror  
            if (status .ne. 0) then  
               finished = .true.  
            endif  
          enddo  
          write (*, 642) numrec-start-1  
  642     format (' * Loaded ', i5, ' multi-terminal d-c records')  
          go to 670  
  
  650     write (errbuf(1), 660)   
  660     format ('E-O-F encountered processing Multi-terminal d-c data  
     &records in raw data file ')  
          call prterx ('W', 1)  
          go to 790  
  
  670     continue  
c  
c         Read in and process Multi-section data in raw data file  
C  
          finished = .false.  
          start = numrec  
          do while (.not. finished)  
            read (tempfile, fmt='(a)', end=690) xbuf  
            numrec = numrec + 1  
            status = ldptisec (xbuf, options, numver, ierror)  
            error = error + ierror  
            if (status .ne. 0) then  
               finished = .true.  
            endif  
          enddo  
          write (*, 682) numrec-start-1  
  682     format (' * Loaded ', i5, ' line section records')  
          go to 710  
  
  690     write (errbuf(1), 700)   
  700     format ('E-O-F encountered processing Multi-section data recor  
     &ds in raw data file ')  
          call prterx ('W', 1)  
          go to 790  
  
  710     continue  
c  
c         Read in and process Zone data in raw data file  
c  
          finished = .false.  
          start = numrec  
          do while (.not. finished)  
            read (tempfile, fmt='(a)', end=730) xbuf  
            numrec = numrec + 1  
            status = ldptizon (xbuf, options, numver, ierror)  
            error = error + ierror  
            if (status .ne. 0) then  
               finished = .true.  
            endif  
          enddo  
          write (*, 722) numrec-start-1  
  722     format (' * Loaded ', i5, ' zone records')  
          go to 750  
  
  730     write (errbuf(1), 740)   
  740     format ('E-O-F encountered processing Zone data records in raw  
     & data file ')  
          call prterx ('W', 1)  
          go to 790  
  
  750     continue  
c  
c         Read in and process Area Transaction data in raw data file  
c  
          finished = .false.  
          start = numrec  
          do while (.not. finished)  
            read (tempfile, fmt='(a)', end=770) xbuf  
            numrec = numrec + 1  
            status = ldptiint (xbuf, options, numver, ierror)  
            error = error + ierror  
            if (status .ne. 0) then  
               finished = .true.  
            endif  
          enddo  
          write (*, 760) numrec-start-1  
  760     format (' * Loaded ', i5, ' Area Transaction records')  
          go to 790  
  
  770     write (errbuf(1), 780)   
  780     format ('E-O-F encountered processing Area Transaction records  
     & in raw data file ')  
          call prterx ('W', 1)  
  
  790     continue  
c  
c         Read in and process Owner data in raw data file  
c  
          finished = (numver .le. 23)  
          start = numrec  
          do while (.not. finished)  
            read (tempfile, fmt='(a)', end=810) xbuf  
            numrec = numrec + 1  
            status = ldptiown (xbuf, options, numver, ierror)  
            error = error + ierror  
            if (status .ne. 0) then  
               finished = .true.  
            endif  
          enddo  
          write (*, 800) numrec-start-1  
  800     format (' * Loaded ', i5, ' owner records')  
          go to 830  
  
  810     write (errbuf(1), 820)   
  820     format ('E-O-F encountered processing Owner data records in ra  
     &w data file ')  
          call prterx ('W', 1)  
          go to 830  
  
  830     continue  
c  
          last = lastch (file(1))  
          write (outbuf, 840) numrec, file(1)(1:last)  
  840     format (1x, i5, ' records in file ', a)  
          call prtout (1)  
          call close_file(busfil)  
  
          call space(1)  
          write (outbuf, 850) errcnt(1)-initial_errcnt(1)  
  850     format (1x, i5, ' Type "I" INFORMATIONAL messages')  
          call prtout (1)  
          write (outbuf, 860) errcnt(2)-initial_errcnt(2)  
  860     format (1x, i5, ' Type "W" WARNING messages')  
          call prtout (1)  
          write (outbuf, 870) errcnt(3)-initial_errcnt(3)  
  870     format (1x, i5, ' Type "E" ERROR messages')  
          call prtout (1)  
          write (outbuf, 880) errcnt(4)-initial_errcnt(4)  
  880     format (1x, i5, ' Type "F" FATAL messages')  
          call prtout (1)  
          write (outbuf, 890) errcnt(5)-initial_errcnt(5)  
  890     format (1x, i5, ' Type "A" ABORT messages')  
          call prtout (1)  
          call space(1)  
   
        endif  
      endif  
c  
c     Hash the bus names, identify any duplicates, and generate   
c     an alpha cross reference  
c  
      status = chk_ptib()  
      status = hsh_ptib()  
    
      call clnuppti(options, error)  
  
  900 continue  
  
      read (inp, fmt='(a)', end=910) inrcd  
      call space (1)    
      write (outbuf, 100) xbuf(1:80)     
      call prtout (1)   
  
  910 continue  
      return  
      end  
