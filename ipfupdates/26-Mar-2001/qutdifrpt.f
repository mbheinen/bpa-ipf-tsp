C****************************************************************
C
C   File: qutdifrpt.f
C
C   Purpose: Routine to obtain a filtered bus reactive utilizatin 
C            comparison report.
C
C   Author: Walt Powell  Date: 15 July 1994
C                        Modified: 6 Nov 1995
C   Called by: p_report.f
C
C****************************************************************
C
        integer function qutdifrpt (in_buffer, out_buffer, scrfil)
        character in_buffer *(*), out_buffer *(*)
        integer scrfil

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/qksrt.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/alt_case.inc'
        include 'ipfinc/ordsta.inc'

        common /file_name/ filename
        character filename*60

	character type * 2, bus_type * 10, obus_type * 10,
     &            capital * 80, null * 1, linefeed * 1, 
     &            text * 80, word(20) * 60, type1 * 2,
     &            type2 * 2
        logical found, finished, repeat, gtfltr, chkfltr, change_f
        integer kmpuvov, apdoutbuf, tempfile, 
     &          ldaltbse, status, kmpvltdif, findstr,
     &          lastloop(3), loop(3), o2, bus_match, loaded,
     &          scrfilx
	external kmpuvov, swapuvov, kmpvltdif, swpvltdif, bus_type,
     &           obus_type
        real total(6,2)
c
        save

        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        maxbuf_out = len( out_buffer ) - 400

        qutdifrpt = 0
c
c       Check for re-entry and continue
c
        last = index( in_buffer, null )
        i = last
        if (findstr (in_buffer(1:i), 'CONTINUE') .ne. 0) then
           repeat = .true.
           scrfilx = 0
           do i = 1, 3
              lastloop(i) = loop(i)
           enddo
           go to 122
        endif

        repeat = .false.
        scrfilx = scrfil
        do i = 1, 3
           lastloop(i) = 0
        enddo
        do i = 1, 6
           total(i,1) = 0.0
           total(i,2) = 0.0
        enddo
c
c       Search and align to "WHERE" ...
c
        ix = findstr(in_buffer, 'WHERE') ! findstr is a case-insensitive
c                                        ! version of index
        found = .false.
        if (ix .gt. 0) then
           ix = ix + len('WHERE')
           change_f = gtfltr(in_buffer(ix:))
           found = .true.
        endif
        if (.not .found) then
           do i = 1, 7
              filter(i) = 0
           enddo
           range_filter(1) = 0.0
        endif
        voltdiff = range_filter(1)

        ix = 1
        i1 = 1
        i2 = index (in_buffer, null)
        call uscan (in_buffer(i1:i2), word, nwrd, '=', ', ' // linefeed)
        iwrd = 1
        do while (iwrd .lt. nwrd .and. 
     &            (capital(word(iwrd)) .ne. 'FILE'))
           iwrd = iwrd + 1
        enddo
        if (iwrd .ge. nwrd .and. 
     &     (ofilename .eq. ' ' .or. ofilename(1:1) .eq. null)) then
           write (errbuf(1), 90)
   90      format(' No alternate base case loaded ')
           call prterx ('W', 1)
           qutdifrpt = 1
           filename = ' '
           go to 900
        else if (capital(word(iwrd)) .ne. 'FILE') then
           filename = ofilename
           go to 110
        endif
        iwrd = iwrd + 1
        if (word(iwrd) .eq. '=') iwrd = iwrd + 1
        filename = word(iwrd)
        tempfile = 20           
        ierr = 99    ! Indicate that input file is binary formatted
        call opnfil (tempfile, filename, ierr)
        if (ierr .ne. 0) then
           write (errbuf(1), 92)
   92      format(' Alternate base case cannot be opened ')
           write (errbuf(2), 94) filename
   94      format(' File ', a)
           call prterx ('W', 2)
           qutdifrpt = 1
           go to 900
        endif
c
c       Function ldaltbse (load alternate base) installs "ocase"
c       and "ofilename"
c
        ocase = ' '
        write (*, 100)
  100	format (' * Loading alternate base file - this will take a minu 
     &e.')
        status = ldaltbse (tempfile, filename, ocase, loaded)
        if (status .ne. 0) then
           write (errbuf(1), 102)
  102      format(' Alternate base case cannot be loaded ')
           write (errbuf(2), 104) filename
  104      format(' File ', a)
           call prterx ('W', 2)
           qutdifrpt = 1
           go to 900
        endif
c
c       Align in_buffer past <filename>
c
        last = lastch(word(iwrd))
        ix = index (in_buffer, word(iwrd)(1:last)) + last
  110   continue
c
c       Search in_buffer and align past "WHERE" ...
c
        if (scrfilx. gt. 0) then
           outbuf = 'SHUNT Reactive Differences'
           call rpnlod
    
           write (outbuf, 111) chase1(1), chase1(34), chase1(35)
  111      format('Case: ', a10, ' Project: ', 2a10)
           call hedlod

        endif

        write (outbuf, 112) cspare(30), ocase
  112   format (' SHUNT reactive differences', t36, 'CASE 1:', a, 
     &          t55, 'CASE 2:', a)
        length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
        o2 = o2 + length
        if (scrfilx .gt. 0) call shdlod(1)

        write (outbuf, 114)
  114   format ('0', t5, 'Bus ', t20, 'Zone', T26, 'Owner', t33, 
     &          'Type State ', 
     &    t44, '      Vk', 
     &    t54, '      Qk', 
     &    t68, '/--------- Capacitors  -------/',
     &    t102, '/--------- Reactors  ---------/')
        length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
        o2 = o2 + length
        if (scrfilx .gt. 0) call shdlod(2)

        write (outbuf, 116)
  116   format (t68, 'Scheduled     Used  Unscheduled',
     1         t102, 'Scheduled     Used  Unscheduled')
        length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
        o2 = o2 + length
        if (scrfilx .gt. 0) call shdlod(3)

        if (scrfilx .gt. 0) then
           outbuf = ' '
           call shdlod(4)
           call shdlod(5)
           call comlod(1)
           call comlod(2)
           call forbtm()
           call fortop()
           call prnt_fltr (in_buffer(ix:))
        endif

        bus(ntot+1) = srtlst
        base(ntot+1) = 9999.0
        oldbus(ontot+1) = srtlst
        oldbase(ontot+1) = 9999.0

  122   o2 = index (out_buffer,null)

        if (lastloop(1) .eq. 0) then
           inb1 = 0
           inb2 = 0
           ksw = 1
        else 
           inb1 = iabs (lastloop(1))
           inb2 = iabs (lastloop(2))
           ksw = lastloop(3)
        endif

        type = 'B '
        type1 = 'B '
        type2 = 'B '
c
c       ksw assignments: 1 - normal
c                        2 - e-o-f inb1
c                        3 - e-o-f inb2
c                        4 - e-o-f inb1 and inb2
c
        ksw = inck1vdif (inb1, nb1, ksw)
        ksw = inck2vdif (inb2, nb2, ksw)

  124   if (ksw .eq. 1) then  ! Search both nb1 and nb2
           komp = kompr(bus(nb1), oldbus(nb2), junk)
           if (komp .eq. 0) komp = 100.0 * (base(nb1) - oldbase(nb2))
           bus_match = komp
        else if (ksw .eq. 2) then
           bus_match = 1
        else if (ksw .eq. 3) then
           bus_match = -1
        else
           outbuf(1:) = ' '
           if (o2 .lt. maxbuf_out) then
              length = apdoutbuf(o2, outbuf(1:1), out_buffer(o2:))
              o2 = o2 + length
           endif
           if (scrfilx .gt. 0) then
              call chkbtm(4)
              call prtout(1)
           endif
           last = lastch(cspare(30))
           write (outbuf, 10126) '1:', cspare(30)(1:last),
     &                           (total(j,1), j=1,6)
10126      format (1x, a, t6, 'Subtotal (', a, ')',
     &         t68, f8.1, f10.1, f10.1,
     &        t102, f8.1, f10.1, f10.1)
           if (o2 .lt. maxbuf_out) then
              length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
              o2 = o2 + length
           endif
           if (scrfilx .gt. 0) then
              call prtout(1)
           endif
           last = lastch(ocase)
           write (outbuf, 10126) '2:', ocase(1:last),
     &                           (total(j,2), j=1,6)
           if (o2 .lt. maxbuf_out) then
              length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
              o2 = o2 + length
           endif
           if (scrfilx .gt. 0) then
              call prtout(1)
           endif
           go to 140
        endif

        if (bus_match .eq. 0) then

           if (ordvlt .eq. 1) then
              kt = nb1
           else
              kt = inp2opt(nb1)
           endif
           ntyp = ntypu(kt)
           call typno( type1(2:2), ntyp )
           if (chkfltr (arcnam(jarzn(nb1)), zone(nb1), owner(nb1), 
     &                  base(nb1), type1, 0)) then
              volt1 = dsqrt (e(kt) ** 2 + f(kt) ** 2)
              qk1 = qnetu(kt)
              call allocq (nb1, qk1, qgen1, qgnmax1, qgnmin1, qld1, 
     &                     totcap1, usecap1, totrek1, userek1, unsked1, 
     &                     qerr2)

              kt = oinp2opt(nb2)
              ntyp = ntypu(kt)
              call typno( type2(2:2), ntyp )
              qk2 = oqnetu(kt)
              volt2 = sqrt (olde(kt) ** 2 + oldf(kt) ** 2)
              call xallocq (nb2, qk2, qgen2, qgnmax2, qgnmin2, qld2, 
     &                      totcap2, usecap2, totrek2, userek2, unsked2,
     &                      qerr2)

              thrshld = amax1(totcap1, -totrek1, abs(unsked1),
     &                        totcap2, -totrek2, abs(unsked2))
              if (thrshld .ge. voltdiff) then

                 if (.not. repeat) then
                    total(1,1) = total(1,1) + totcap1
                    total(2,1) = total(2,1) + usecap1
                    total(3,1) = total(3,1) + amax1(unsked1, 0.0)
                    total(4,1) = total(4,1) + totrek1
                    total(5,1) = total(5,1) + userek1
                    total(6,1) = total(6,1) + amin1(unsked1, 0.0)
                    total(1,2) = total(1,2) + totcap2
                    total(2,2) = total(2,2) + usecap2
                    total(3,2) = total(3,2) + amax1(unsked2, 0.0)
                    total(4,2) = total(4,2) + totrek2
                    total(5,2) = total(5,2) + userek2
                    total(6,2) = total(6,2) + amin1(unsked2, 0.0)
                 endif

                 text = bus_type(nb1)

                 outbuf = ' '
                 if (o2 .lt. maxbuf_out) then
                    length = apdoutbuf(o2, outbuf(1:1), out_buffer(o2:))
                    o2 = o2 + length
                 endif
                 if (scrfilx .gt. 0) then
                    call chkbtm(4)
                    call prtout(1)
                 endif

                 unskedcap = amax1 (unsked1, 0.0)
                 unskedrek = amin1 (unsked1, 0.0)
                 write (outbuf, 126) '1:', bus(nb1), base(nb1), 
     &              zone(nb1), owner(nb1), text(1:2), 
     &              text(3:8), volt1, qgen1, totcap1, usecap1,
     &              unskedcap, totrek1, userek1, unskedrek
  126            format (1x, a, t5, a8, f6.1, t21, a, t27, a, t34, a, 
     &              t39, a, t44, f8.3, t54, f8.2, t68, f8.1, f10.1, 
     &              f10.1, t102, f8.1, f10.1, f10.1)

                 if (o2 .lt. maxbuf_out) then
                    length = apdoutbuf(o2, outbuf(1:132), 
     &                                 out_buffer(o2:))
                    o2 = o2 + length
                    loop(1) = inb1
                    loop(3) = ksw
                 else if (repeat) then
                    finished = .true.
                 endif
                 if (scrfilx .gt. 0) call prtout(1)

                 unskedcap = amax1 (unsked2, 0.0)
                 unskedrek = amin1 (unsked2, 0.0)
                 text = obus_type(nb2)
                 write (outbuf, 126) '2:', oldbus(nb2), oldbase(nb2), 
     &              oldzone(nb2), oldowner(nb2), text(1:2), 
     &              text(3:8), volt2, qgen2, totcap2, usecap2,
     &              unskedcap, totrek2, userek2, unskedrek

                 if (o2 .lt. maxbuf_out) then
                    length = apdoutbuf(o2, outbuf(1:132), 
     &                                 out_buffer(o2:))
                    o2 = o2 + length
                    loop(2) = inb2
                    loop(3) = ksw
                 else if (repeat) then
                    finished = .true.
                 endif
                 if (scrfilx .gt. 0) call prtout(1)
              else
                 if (o2 .lt. maxbuf_out) then
                    loop(1) = inb1
                    loop(2) = inb2
                    loop(3) = ksw
                 else if (repeat) then
                    finished = .true.
                 endif
              endif

           else
              if (o2 .lt. maxbuf_out) then
                 loop(1) = inb1
                 loop(2) = inb2
                 loop(3) = ksw
              else if (repeat) then
                 finished = .true.
              endif
           endif
c
c          Increment buses inb1 and inb2
c
           ksw = inck1vdif (inb1, nb1, ksw)
           ksw = inck2vdif (inb2, nb2, ksw)
           go to 124

        else if (bus_match .lt. 0) then ! E-O-I ptr2 - search ptr1

           if (ordvlt .eq. 1) then
              kt = nb1
           else
              kt = inp2opt(nb1)
           endif
           ntyp = ntypu(kt)
           call typno( type1(2:2), ntyp )
           if (chkfltr (arcnam(jarzn(nb1)), zone(nb1), owner(nb1), 
     &                  base(nb1), type1, 0)) then

              if (ordvlt .eq. 1) then
                 kt = nb1
              else
                 kt = inp2opt(nb1)
              endif
              ntyp = ntypu(kt)
              call typno( type, ntyp )
              qk1 = qnetu(kt)
              volt1 = dsqrt (e(kt) ** 2 + f(kt) ** 2)
              call allocq (nb1, qk1, qgen1, qgnmax1, qgnmin1, qld1, 
     &                     totcap1, usecap1, totrek1, userek1, unsked1, 
     &                     qerr2)

              thrshld = amax1(totcap1, -totrek1, abs(unsked1))
              if (thrshld .ge. voltdiff) then

                 if (.not. repeat) then
                    total(1,1) = total(1,1) + totcap1
                    total(2,1) = total(2,1) + usecap1
                    total(3,1) = total(3,1) + amax1(unsked1, 0.0)
                    total(4,1) = total(4,1) + totrek1
                    total(5,1) = total(5,1) + userek1
                    total(6,1) = total(6,1) + amin1(unsked1, 0.0)
                 endif

                 outbuf = ' '
                 if (o2 .lt. maxbuf_out) then
                    length = apdoutbuf(o2, outbuf(1:1), out_buffer(o2:))
                    o2 = o2 + length
                 endif
                 if (scrfilx .gt. 0) call prtout(1)

                 unskedcap = amax1 (unsked1, 0.0)
                 unskedrek = amin1 (unsked1, 0.0)
                 text = bus_type(nb1)
                 write (outbuf, 126) '1:', bus(nb1), base(nb1), 
     &              zone(nb1), owner(nb1), text(1:2), 
     &              text(3:8), volt1, qgen1, totcap1, usecap1,
     &              unskedcap, totrek1, userek1, unskedrek
                 if (o2 .lt. maxbuf_out) then
                    length = apdoutbuf(o2, outbuf(1:132), 
     &                                 out_buffer(o2:))
                    o2 = o2 + length
                    loop(1) = inb1
                    loop(3) = ksw
                 else if (repeat) then
                    finished = .true.
                 endif
                 if (scrfilx .gt. 0) call prtout(1)
              else
                 if (o2 .lt. maxbuf_out) then
                    loop(1) = inb1
                    loop(3) = ksw
                 else if (repeat) then
                    finished = .true.
                 endif
              endif
           else
              if (o2 .lt. maxbuf_out) then
                 loop(1) = inb1
                 loop(3) = ksw
              else if (repeat) then
                 finished = .true.
              endif
           endif
c
c          Increment bus inb1
c
           ksw = inck1vdif (inb1, nb1, ksw)
           go to 124

        else if (bus_match .gt. 0) then ! E-O-F ptr1 - search ptr2

           kt = oinp2opt(nb2)
           ntyp = ntypu(kt)
           call typno( type2(2:2), ntyp )
           if (chkfltr (oarcnam(oarzn(nb2)), oldzone(nb2), 
     &                  oldowner(nb2), oldbase(nb2), type2, 0)) then
              qk2 = oqnetu(kt)
              volt2 = sqrt (olde(kt) ** 2 + oldf(kt) ** 2)
              call xallocq (nb2, qk2, qgen2, qgnmax2, qgnmin2, qld2, 
     &                      totcap2, usecap2, totrek2, userek2, 
     &                      unsked2, qerr2)

              thrshld = amax1(totcap2, -totrek2, abs(unsked2))
              if (thrshld .ge. voltdiff) then

                 if (.not. repeat) then
                    total(1,2) = total(1,2) + totcap2
                    total(2,2) = total(2,2) + usecap2
                    total(3,2) = total(3,2) + amax1(unsked2, 0.0)
                    total(4,2) = total(4,2) + totrek2
                    total(5,2) = total(5,2) + userek2
                    total(6,2) = total(6,2) + amin1(unsked2, 0.0)
                 endif

                 outbuf = ' '
                 if (o2 .lt. maxbuf_out) then
                    length = apdoutbuf(o2, outbuf(1:1), out_buffer(o2:))
                    o2 = o2 + length
                 endif
                 if (scrfilx .gt. 0) call prtout(1)

                 unskedcap = amax1 (unsked2, 0.0)
                 unskedrek = amin1 (unsked2, 0.0)
                 text = obus_type(nb2)
                 write (outbuf, 126) '2:', oldbus(nb2), oldbase(nb2), 
     &              oldzone(nb2), oldowner(nb2), text(1:2), 
     &              text(3:8), volt2, qgen2, totcap2, usecap2,
     &              unskedcap, totrek2, userek2, unskedrek

                 if (o2 .lt. maxbuf_out) then
                    length = apdoutbuf(o2, outbuf(1:132), 
     &                                 out_buffer(o2:))
                    o2 = o2 + length
                    loop(2) = inb2
                    loop(3) = ksw
                 else if (repeat) then
                    finished = .true.
                 endif
                 if (scrfilx .gt. 0) call prtout(1)
              else
                 if (o2 .lt. maxbuf_out) then
                    loop(2) = inb2
                    loop(3) = ksw
                 else if (repeat) then
                    finished = .true.
                 endif
              endif
           else
              if (o2 .lt. maxbuf_out) then
                 loop(2) = inb2
                 loop(3) = ksw
              else if (repeat) then
                 finished = .true.
              endif
           endif
c
c          Increment bus inb2
c
           ksw = inck2vdif (inb2, nb2, ksw)
           go to 124

        endif

c*** remember maxbuf_out is really 400 less than the real buffer size
  140   if (o2 .gt. maxbuf_out) then
           write (out_buffer(o2:o2+8), 190) linefeed, null
  190      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif

  900   continue
        return
        end

