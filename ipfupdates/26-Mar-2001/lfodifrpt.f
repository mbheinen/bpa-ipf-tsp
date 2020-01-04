C    %W% %G%
C****************************************************************
C
C   File: lfodifrpt.f
C
C   Purpose: Routine to obtain a filtered line flow comparison 
C            report.
c
C   Author: Walt Powell  Date: 14 December 1992
C   Called by: p_report.f
C
C****************************************************************
C
        integer function lfodifrpt (in_buffer, out_buffer, scrfil)
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

        common /file_name/ filename
        character filename*60

        common /scratch/ numdiff, ixref(MAXBUS), kdiff(2,MAXBUS), 
     &                   fdiff(6,MAXBUS), cdiff(2,MAXBUS)
        character cdiff * 1

	character type * 2, relvlt * 3,
     &            capital * 80, null * 1, linefeed * 1, header(4) * 80, 
     &            text * 80, word(20) * 60
        integer o2, rating1, rating2, loading1, loading2, sect, ptr1,
     &          ptr2, ptr1x, ptr2x, bus_match, br_match, findstr
        logical found, chkfltr

	external kmpvltdif, swpvltdif
        logical gtfltr, change_f, repeat, finished
        integer apdoutbuf, tempfile, ldaltbse, status, kmpvltdif,
     &          scrfilx
        character tag1 * 1, tag2 * 1, id * 1, brntyp * 2, 
     &            own * 3, code * 4, base_1c * 4, base_2c * 4,
     &            bus_1c * 8, bus_2c * 8, own1 * 3, own2 * 3

        save

        maxbuf_out = len( out_buffer ) - 400
        null = char(0)
        linefeed = char(10)

        lfodifrpt = 0
c
c       Check for re-entry and continue
c
        last = index( in_buffer, null )
        i = last
        if (findstr (in_buffer(1:i), 'CONTINUE') .ne. 0) then
           repeat = .true.
           scrfilx = 0
           lastloop = loop
           go to 250
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
           lfodifrpt = 1
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
           write (errbuf(1), 100)
  100      format(' Alternate base case cannot be opened ')
           write (errbuf(2), 102) filename
  102      format(' File ', a)
           call prterx ('W', 2)
           lfodifrpt = 1
           go to 900
        endif
c
c       Function ldaltbse (load alternate base) installs "ocase"
c       and "ofilename"
c
        ocase = ' '
        write (*, 104)
  104	format (' * Loading alternate base file - this will take a minute.
     &')
        status = ldaltbse (tempfile, filename, ocase, loaded)
        if (status .ne. 0) then
           write (errbuf(1), 106)
  106      format(' Alternate base case cannot be loaded ')
           write (errbuf(2), 108) filename
  108      format(' File ', a)
           call prterx ('W', 2)
           lfodifrpt = 1
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
        i = findstr (in_buffer(ix:), 'WHERE') ! findstr is a 
c                                             ! case-insensitive
c                                             ! version of index
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
        flowdiff = range_filter(1)
        numdiff = 0
        out_buffer(1:1) = null   
        o2 = index (out_buffer,null)

        inb1 = 0
        inb2 = 0
        ksw = 1
        bus(ntot+1) = srtlst
        base(ntot+1) = 9999.0
        oldbus(ontot+1) = srtlst
        oldbase(ontot+1) = 9999.0
c
c       ksw assignments: 1 - normal
c                        2 - e-o-f inb1
c                        3 - e-o-f inb2
c                        4 - e-o-f inb1 and inb2
c
        ksw = inck1vdif (inb1, nb1, ksw)
        ksw = inck2vdif (inb2, nb2, ksw)

  170   if (numdiff .ge. MAXBUS) go to 230

        if (ksw .eq. 1) then  ! Search both nb1 and nb2
           komp = kompr(bus(nb1), oldbus(nb2), junk)
           if (komp .eq. 0) komp = 100.0 * (base(nb1) - oldbase(nb2))
           bus_match = komp
        else if (ksw .eq. 2) then
           bus_match = 1
        else if (ksw .eq. 3) then
           bus_match = -1
        else
           go to 230
        endif

        if (bus_match .eq. 0) then

           if (.not. chkfltr (arcnam(jarzn(nb1)), zone(nb1), '***', 
     &                        base(nb1), '**', 0)) then
c
c             Increment buses inb1 and inb2
c
              ksw = inck1vdif (inb1, nb1, ksw)
              ksw = inck2vdif (inb2, nb2, ksw)
              go to 170

           endif

           ptr1 = kbsdta(16,nb1)
           if (ptr1 .gt. 0) then
              m1 = ky(ptr1)
              lsw = 1
           else
              lsw = 2
           endif
           ptr2 = okbsdta(16,nb2)
           if (ptr2 .gt. 0) then
              m2 = oky(ptr2)
           else
              lsw = lsw + 2
           endif
c
c          lsw assignments: 1 - normal
c                           2 - e-o-f inb1
c                           3 - e-o-f inb2
c                           4 - e-o-f inb1 and inb2
c
           do while (lsw .ne. 4)
              if (lsw .eq. 1) then ! Normal - search ptr1 and ptr2
                 komp = kompr(bus(m1), oldbus(m2), junk)
                 if (komp .eq. 0) komp = 100.0*(base(m1)-oldbase(m2))
                 br_match = komp
              else if (lsw .eq. 2) then
                 br_match = 1
              else if (lsw .eq. 3) then
                 br_match = -1
              endif
              
              if (br_match .eq. 0) then

                 nbr1 = iabs(brnch_ptr(ptr1))
                 nbr2 = iabs(obrnch_ptr(ptr2))
                 call getchr (3, own1, kbrnch(3,nbr1))
                 if (inp2alf(nb1) .lt. inp2alf(m1) .and.
     &               chkfltr (arcnam(jarzn(m1)), zone(m1), own1, 
     &                        base(m1), '**', 0)) then

                    call getmxld (ptr1, ptr1x, lastptr1, loading1,
     &                            rating1, tag1)
                    call ogetmxld (ptr2, ptr2x, lastptr2, loading2,
     &                             rating2, tag2)
                    if (loading1 .gt. 0 .and. loading2 .gt. 0) then
                       idv = 100 * (loading1 - loading2) 
                       if (iabs (idv) .ge. int(100.0 * flowdiff)) then
                          numdiff = numdiff + 1
                          kdiff(1,numdiff) = ptr1x
                          kdiff(2,numdiff) = ptr2x
                          fdiff(1,numdiff) = iabs(idv) 
                          fdiff(2,numdiff) = loading1
                          fdiff(3,numdiff) = rating1
                          fdiff(4,numdiff) = loading2
                          fdiff(5,numdiff) = rating2
                          fdiff(6,numdiff) = idv
                          cdiff(1,numdiff) = tag1
                          cdiff(2,numdiff) = tag2
                       endif
                    endif
                 endif
c
c                Increment branches for m1 and m2
c
                 lsw = incm1vdif (ptr1, nb1, m1, lsw)
                 lsw = incm2vdif (ptr2, nb2, m2, lsw)

              else if (br_match .lt. 0) then

                 nbr1 = iabs(brnch_ptr(ptr1))
                 call getchr (3, own1, kbrnch(3,nbr1))
                 if (inp2alf(nb1) .lt. inp2alf(m1) .and.
     &               chkfltr (arcnam(jarzn(m1)), zone(m1), own1, 
     &                        base(m1), '**', 0)) then

                    call getmxld (ptr1, ptr1x, lastptr1, loading1,
     &                            rating1, tag1)
                    if (loading1 .gt. 0) then
                       idv = 100.0 * loading1 
                       if (abs (idv) .ge. int(100.0 * flowdiff)) then
                          numdiff = numdiff + 1
                          kdiff(1,numdiff) = ptr1x
                          kdiff(2,numdiff) = 0
                          fdiff(1,numdiff) = iabs(idv) 
                          fdiff(2,numdiff) = loading1
                          fdiff(3,numdiff) = rating1
                          fdiff(4,numdiff) = 0.0
                          fdiff(5,numdiff) = 0.0
                          fdiff(6,numdiff) = idv
                          cdiff(1,numdiff) = tag1
                          cdiff(2,numdiff) = ' '
                       endif
                    endif
                 endif
c
c                Increment branches for m1
c
                 lsw = incm1vdif (ptr1, nb1, m1, lsw)

              else

                 nbr2 = iabs(obrnch_ptr(ptr2))
                 call getchr (3, own2, okbrnch(3,nbr2))
                 if (oinp2alf(nb2) .le. oinp2alf(m2) .and.
     &               chkfltr (oarcnam(oarzn(m1)), oldzone(m2), own2, 
     &                        oldbase(m2), '**', 0)) then

                    call ogetmxld (ptr2, ptr2x, lastptr2, loading2,
     &                             rating2, tag2)
                    if (loading2 .gt. 0) then
                       idv = 100.0 * loading2 
                       if (iabs (idv) .ge. int(100.0 * flowdiff)) then
                          numdiff = numdiff + 1
                          kdiff(1,numdiff) = 0
                          kdiff(2,numdiff) = ptr2x
                          fdiff(1,numdiff) = iabs(idv) 
                          fdiff(2,numdiff) = 0.0
                          fdiff(3,numdiff) = 0.0
                          fdiff(4,numdiff) = loading2
                          fdiff(5,numdiff) = rating2
                          fdiff(6,numdiff) = idv
                          cdiff(1,numdiff) = ' '
                          cdiff(2,numdiff) = tag2
                       endif
                    endif
                 endif
c
c                Increment branches for m2
c
                 lsw = incm2vdif (ptr2, nb2, m2, lsw)

              endif
           enddo
c
c          Increment buses inb1 and inb2
c
           ksw = inck1vdif (inb1, nb1, ksw)
           ksw = inck2vdif (inb2, nb2, ksw)
           go to 170

        else if (bus_match .lt. 0) then ! E-O-I ptr2 - search ptr1

           if (.not. chkfltr (arcnam(jarzn(nb1)), zone(nb1), '***', 
     &                        base(nb1), '**', 0)) then
c
c             Increment bus inb1
c
              ksw = inck1vdif (inb1, nb1, ksw)
              go to 170
           endif

           ptr1 = kbsdta(16,nb1)
           if (ptr1 .gt. 0) then
              m1 = ky(ptr1)
              lsw = 1
           else
              lsw = 2
           endif
c
c          lsw assignments: 1 - normal
c                           2 - e-o-f inb1
c                           3 - e-o-f inb2
c                           4 - e-o-f inb1 and inb2
c
           do while (lsw .ne. 2)

              nbr1 = iabs(brnch_ptr(ptr1))
              call getchr (3, own1, kbrnch(3,nbr1))
              if (inp2alf(nb1) .lt. inp2alf(m1) .and.
     &            chkfltr (arcnam(jarzn(m1)), zone(m1), own1, 
     &                     base(m1), '**', 0)) then
c
                 call getmxld (ptr1, ptr1x, lastptr1, loading1,
     &                       rating1, tag1)
                 if (loading1 .gt. 0) then
                    idv = 100.0 * loading1 
                    if (abs (idv) .ge. int(100.0 * flowdiff)) then
                       numdiff = numdiff + 1
                       kdiff(1,numdiff) = ptr1x
                       kdiff(2,numdiff) = 0
                       fdiff(1,numdiff) = iabs(idv) 
                       fdiff(2,numdiff) = loading1
                       fdiff(3,numdiff) = rating1
                       fdiff(4,numdiff) = 0.0
                       fdiff(5,numdiff) = 0.0
                       fdiff(6,numdiff) = idv
                       cdiff(1,numdiff) = tag1
                       cdiff(2,numdiff) = ' '
                    endif
                 endif
              endif
c
c             Increment branches for m1
c
              lsw = incm1vdif (ptr1, nb1, m1, lsw)

           enddo
c
c          Increment bus inb1
c
           ksw = inck1vdif (inb1, nb1, ksw)
           go to 170

        else if (bus_match .gt. 0) then ! E-O-F ptr1 - search ptr2

           if (.not. chkfltr (oarcnam(oarzn(nb2)), oldzone(nb2), '***', 
     &                        oldbase(nb2), '**', 0)) then
c
c             Increment bus inb2
c
              ksw = inck2vdif (inb2, nb2, ksw)
              go to 170

           endif

           ptr2 = okbsdta(16,nb2)
           if (ptr2 .gt. 0) then
              m2 = oky(ptr2)
              lsw = 1
           else
              lsw = lsw + 2
           endif
c
c          lsw assignments: 1 - normal
c                           2 - e-o-f inb1
c                           3 - e-o-f inb2
c                           4 - e-o-f inb1 and inb2
c
           do while (lsw .ne. 3)

              nbr2 = iabs(obrnch_ptr(ptr2))
              call getchr (3, own2, okbrnch(3,nbr2))
              if (oinp2alf(nb2) .lt. oinp2alf(m2) .and.
     &            chkfltr (oarcnam(oarzn(m2)), oldzone(m2), own2, 
     &                     base(m1), '**', 0)) then
 
                 call ogetmxld (ptr2, ptr2x, lastptr2, loading2,
     &                          rating2, tag2)
                 if (loading2 .gt. 0) then
                    idv = 100.0 * loading2 
                    if (abs (idv) .ge. int(100.0 * flowdiff)) then
                       numdiff = numdiff + 1
                       kdiff(1,numdiff) = 0
                       kdiff(2,numdiff) = ptr2x
                       fdiff(1,numdiff) = iabs(idv) 
                       fdiff(2,numdiff) = 0.0
                       fdiff(3,numdiff) = 0.0
                       fdiff(4,numdiff) = loading2
                       fdiff(5,numdiff) = rating2
                       fdiff(6,numdiff) = idv
                       cdiff(1,numdiff) = ' '
                       cdiff(2,numdiff) = tag2
                    endif
                 endif
              endif
c
c             Increment branches for m2
c
              lsw = incm2vdif (ptr2, nb2, m2, lsw)

           enddo
c
c          Increment bus inb2
c
           ksw = inck2vdif (inb2, nb2, ksw)
           go to 170

        endif

  230   if (numdiff .gt. 0) then
           do i = 1, numdiff
              ixref(i) = i
           enddo
c
c          Set "key" = 2 to inform kmpvltdif that kdiff(1,*) and
c          kdiff(2,*) are branch indices.
c
           key = 2
           call qiksrt (1, numdiff, kmpvltdif, swpvltdif)

           write (header(1), 240) cspare(30), ocase
  240      format (' Line loading difference report',  t39, a, t57, a)
           header(2) = ' Ty Own Bus1         Bus2         IS  Case Ratin
     &g load  case rating load  rel'
           header(3) = '                                      amps amps 
     &    %   amps amps     %  (1/2)'
           header(4) = '                                      MVA  MVA  
     &        mva  mva'

           do i = 1, 4
              if (scrfil .gt. 0) write (scrfil, '(a)') header(i)
              length = apdoutbuf(o2, header(i), out_buffer(o2:))
              o2 = o2 + length
           enddo
        endif

  250   ix = lastloop
        if (ix .eq. 0) ix = 1
        loop = ix

        finished = .false.
        do while (ix .le. numdiff .and. .not. finished)
           i = ixref(ix)
           ptr1 = kdiff(1,i)           
           ptr2 = kdiff(2,i)         
           loading1 = fdiff(2,i)
           rating1 = abs (fdiff(3,i))
           loading2 = fdiff(4,i)
           rating2 = abs (fdiff(5,i))
           tag1 = cdiff(1,i)
           tag2 = cdiff(2,i)
           icase1 = 0.01 * fdiff(2,i) * abs(fdiff(3,i))
           icase2 = 0.01 * fdiff(4,i) * abs(fdiff(5,i))
           if (ptr1 .gt. 0 .and. ptr2 .gt. 0) then
              relvlt = ' '
              k1 = kx(ptr1)
              k2 = ky(ptr1)
              id = brid(ptr1)
              sect = brsect(ptr1)
              type = brntyp(brtype(ptr1))
              nbr = iabs(brnch_ptr(ptr1))
              call getchr (3, own, kbrnch(3,nbr))
              bus_1c =  bus(k1)
              bus_2c =  bus(k2)
              base_1c = code (base(k1), 4, 0)
              base_2c = code (base(k2), 4, 0)
           else if (ptr1 .gt. 0) then
              relvlt = '(1)'
              k1 = kx(ptr1)
              k2 = ky(ptr1)
              id = brid(ptr1)
              sect = brsect(ptr1)
              type = brntyp(brtype(ptr1))
              nbr = iabs(brnch_ptr(ptr1))
              call getchr (3, own, kbrnch(3,nbr))
              bus_1c =  bus(k1)
              bus_2c =  bus(k2)
              base_1c = code (base(k1), 4, 0)
              base_2c = code (base(k2), 4, 0)
           else 
              relvlt = '(2)'
              k1 = okx(ptr2)
              k2 = oky(ptr2)
              id = obrid(ptr2)
              sect = obrsect(ptr2)
              type = brntyp(obrtype(ptr2))
              nbr = iabs(obrnch_ptr(ptr2))
              call getchr (3, own, okbrnch(3,nbr))
              bus_1c =  oldbus(k1)
              bus_2c =  oldbus(k2)
              base_1c = code (oldbase(k1), 4, 0)
              base_2c = code (oldbase(k2), 4, 0)
           endif
           ivrel = fdiff(6,i) / 100.0
           write (text, 290) type, own, bus_1c, base_1c,
     &                       bus_2c, base_2c, id, sect,
     &                       icase1, rating1, tag1, loading1,
     &                       icase2, rating2, tag2, loading2,
     &                       ivrel, relvlt
  290      format(1x, a2, 1x, a3, 1x, a8, a4, 1x, a8, a4, 1x, a1,
     &            i1, i6, i5, 1x, a1, i4, i7, i5, 1x, a1, i4, 1x,
     &            i4, 1x, a3)
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
           write (out_buffer(o2:o2+8), 820) linefeed, null
  820      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif

  900   continue
        return
        end
