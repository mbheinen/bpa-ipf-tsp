C    %W% %G%
C****************************************************************
C
C   File: vltdifrpt.f
C
C   Purpose: Routine to obtain a filtered bus voltage comparison 
C            report.
C
C   Author: Walt Powell  Date: 14 December 1992
C   Called by: p_report.f
C
C****************************************************************
C
        integer function vltdifrpt (in_buffer, out_buffer, scrfil)
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
        include 'ipfinc/ordsta.inc'
        include 'ipfinc/alt_case.inc'

        common /file_name/ filename
c
        character filename*60
c
        common /scratch/ numdiff, ixref(MAXBUS), kdiff(2,MAXBUS), 
     &                   fdiff(6,MAXBUS), cdiff(2,MAXBUS)
c
        character cdiff * 1
c
	character type * 2, relvlt * 3, bustyp * 2, 
     &            capital * 80, null * 1, linefeed * 1, header(4) * 80,
     &            text * 80, word(20) * 60, type1 * 2,
     &            type2 * 2, bus_name * 8, bus_owner * 3, bus_zone * 2
c
c
	external kmpuvov, swapuvov, kmpvltdif, swpvltdif
c 
        logical found, finished, repeat, gtfltr, chkfltr, change_f
        integer o2, okt, bus_match, loaded, kmpuvov, apdoutbuf, 
     &          tempfile, ldaltbse, status, kmpvltdif, 
     &          findstr, scrfilx
c
        save
c
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        maxbuf_out = len( out_buffer ) - 400

        vltdifrpt = 0
c
c       Check for re-entry and continue
c
        last = index( in_buffer, null )
        i = last
        if (findstr (in_buffer(1:i), 'CONTINUE') .ne. 0) then
           repeat = .true.
           scrfilx = 0
           lastloop = loop
           go to 170
        endif

        repeat = .false.
        scrfilx = scrfil
        lastloop = 0

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
           vltdifrpt = 1
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
           vltdifrpt = 1
           go to 900
        endif
c
c       Function ldaltbse (load alternate base) installs "ocase"
c       and "ofilename"
c
        ocase = ' '
        write (*, 100)
  100	format (' * Loading alternate base file - this will take a minute.
     &')
        status = ldaltbse (tempfile, filename, ocase, loaded)
        if (status .ne. 0) then
           write (errbuf(1), 102)
  102      format(' Alternate base case cannot be loaded ')
           write (errbuf(2), 104) filename
  104      format(' File ', a)
           call prterx ('W', 2)
           vltdifrpt = 1
           go to 900
        endif
c
c       Align in_buffer past <filename>
c
        last = lastch(word(iwrd))
        ix = index (in_buffer, word(iwrd)(1:last)) + last
c
c       Search in_buffer and align past "WHERE" ...
c
  110   found = .false.
        i = findstr (in_buffer(ix:), 'WHERE') ! findstr is a case-
c                                             ! insensitive version of
c                                             ! index
        if (i .gt. 0) then
           ix = ix + i + len('WHERE') - 1
           change_f = gtfltr(in_buffer(ix:))
           found = .true.
        endif
        if (.not .found) then
           do i = 1, 7
              filter(i) = 0
           enddo
        endif
        voltdiff = range_filter(1)

        numdiff = 0

        inb1 = 0
        inb2 = 0
        ksw = 1
        bus(ntot+1) = srtlst
        base(ntot+1) = 9999.0
        oldbus(ontot+1) = srtlst
        oldbase(ontot+1) = 9999.0
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

  120   if (ksw .eq. 1) then  ! Search both nb1 and nb2
           komp = kompr(bus(nb1), oldbus(nb2), junk)
           if (komp .eq. 0) komp = 100.0 * (base(nb1) - oldbase(nb2))
           bus_match = komp
        else if (ksw .eq. 2) then
           bus_match = 1
        else if (ksw .eq. 3) then
           bus_match = -1
        else
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
              volt1 = dsqrt(e(kt) ** 2 + f(kt) ** 2)

              okt = oinp2opt(nb2)
              ntyp = ontypu(okt)
              call typno( type2(2:2), ntyp )
              volt2 = sqrt (olde(okt) ** 2 + oldf(okt) ** 2)
              idv = 1000.0 * (volt1 - volt2) 

              if (abs (idv) .ge. int(1000.0 * voltdiff)) then
                 numdiff = numdiff + 1
                 kdiff(1,numdiff) = nb1
                 kdiff(2,numdiff) = nb2
                 fdiff(1,numdiff) = iabs(idv) 
                 fdiff(2,numdiff) = volt1
                 fdiff(3,numdiff) = volt2
                 cdiff(1,numdiff) = type1
                 cdiff(2,numdiff) = type2
              endif
           endif
c
c          Increment buses inb1 and inb2
c
           ksw = inck1vdif (inb1, nb1, ksw)
           ksw = inck2vdif (inb2, nb2, ksw)
           go to 120

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

              volt1 = dsqrt(e(kt) ** 2 + f(kt) ** 2)

              idv = 1000.0
              if (abs (idv) .ge. int(1000.0 * voltdiff)) then
                 numdiff = numdiff + 1
                 kdiff(1,numdiff) = nb1
                 kdiff(2,numdiff) = 0
                 fdiff(1,numdiff) = iabs(idv) 
                 fdiff(2,numdiff) = volt1
                 fdiff(3,numdiff) = 0.0
                 cdiff(1,numdiff) = type1
                 cdiff(2,numdiff) = ' '
              endif

           endif
c
c          Increment bus inb1
c
           ksw = inck1vdif (inb1, nb1, ksw)
           go to 120

        else if (bus_match .gt. 0) then ! E-O-F ptr1 - search ptr2

           kt = oinp2opt(nb2)
           ntyp = ontypu(kt)
           call typno( type2(2:2), ntyp )
           if (chkfltr (oarcnam(oarzn(nb2)), oldzone(nb2), 
     &                  oldowner(nb2), oldbase(nb2), type2, 0)) then

              idv = -1000.0

              if (abs (idv) .ge. int(1000.0 * voltdiff)) then
                 numdiff = numdiff + 1
                 kdiff(1,numdiff) = 0
                 kdiff(2,numdiff) = nb2
                 fdiff(1,numdiff) = iabs(idv) 
                 fdiff(2,numdiff) = 0.0
                 fdiff(3,numdiff) = volt2
                 cdiff(1,numdiff) = ' '
                 cdiff(2,numdiff) = type2
              endif
           endif
c
c          Increment bus inb2
c
           ksw = inck2vdif (inb2, nb2, ksw)
           go to 120

        endif

  140   if (numdiff .gt. 0) then
           do i = 1, numdiff
              ixref(i) = i
           enddo
c
c          Set "key" = 1 to inform kmpvltdif that kdiff(1,*) and
c          kdiff(2,*) are bus indices.
c
           write (*, 10140)
10140      format (' * Enter sort option - 1 = Alphabetical', /,
     &             ' *                     2 = High to low' , /,
     &             ' > Enter option ', $)
           read (*, '(a)') text
           if (text(1:1) .eq. '2') then

              key = 1
              call qiksrt (1, numdiff, kmpvltdif, swpvltdif)

           endif

           write (header(1), 142) cspare(30), ocase
  142      format (' Voltage difference report', t34, a, 3x, a)
           write (header(2), 144)
  144      format (' Ty Own Bus1           Zone     Voltage    Voltage
     & difference ')
           write (header(3), 146)
  146      format ('                                 (p.u.)     (p.u.)
     &  (p.u.)')

           do i = 1, 3
              if (scrfilx .gt. 0) write (scrfilx, '(a)') header(i)
              length = apdoutbuf(o2, header(i), out_buffer(o2:))
              o2 = o2 + length
           enddo
        endif

  170   ix = lastloop
        if (ix .eq. 0) ix = 1
        loop = ix

        finished = .false.
        do while (ix .le. numdiff .and. .not. finished)
           i = ixref(ix)
           nb1 = kdiff(1,i)           
           if (nb1 .gt. 0) then
              if (ordvlt .eq. 1) then
                 kt = nb1
              else
                 kt = inp2opt(nb1)
              endif
              type = bustyp (nb1)
              volt1 = fdiff(2,i)
              ntyp = ntypu(kt)
              call typno( type(2:2), ntyp )
              bus_name = bus(nb1)
              bus_base = base(nb1)
              bus_owner = owner(nb1)
              bus_zone = zone(nb1)
           else
              volt1 = 0.0
           endif
           nb2 = kdiff(2,i)            
           if (nb2 .gt. 0) then
              okt = oinp2opt(nb2)
              volt2 = fdiff(3,i)
              ntyp = ontypu(okt)
              call typno( type(2:2), ntyp )
              bus_name = oldbus(nb2)
              bus_base = oldbase(nb2)
              bus_owner = oldowner(nb2)
              bus_zone = oldzone(nb2)
           else
              volt2 = 0.0
           endif
           if (nb1 .ne. 0 .and. nb2 .ne. 0) then
              relvlt = ' '
           else if (nb1 .gt. 0) then
              relvlt = '(1)'
           else
              relvlt = '(2)'
           endif
           write (text, 180) type, bus_owner, bus_name, bus_base,
     &                       bus_zone, volt1, volt2, volt1 - volt2, 
     &                       relvlt
  180      format(1x, a2, 1x, a3, 1x, a8, f6.1, 2x, a2, 4x, f8.3, 2x, 
     &            f8.3, 2x, f8.3, 2x, a3)
           if (o2 .lt. maxbuf_out) then
              length = apdoutbuf(o2, text(1:80), out_buffer(o2:))
              o2 = o2 + length
              loop = ix
           else if (repeat) then
              finished = .true.
           endif
           if (scrfilx .gt. 0) write (scrfilx, '(a)') text
           ix = ix + 1
        enddo

c*** remember maxbuf_out is really 400 less than the real buffer size
        if (o2 .gt. maxbuf_out) then
           write (out_buffer(o2:o2+8), 190) linefeed, null
  190      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif

  900   continue
        return
        end
