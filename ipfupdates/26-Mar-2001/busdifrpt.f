C    %W% %G%
C****************************************************************
C
C   File: busdifrpt.f
C
C   Purpose: Routine to obtain a filtered bus quantity comparison 
C            report.
C
C   Invoked by:
C              / REPORTS, SELECT BUS_COMP_PGEN
C              / REPORTS, SELECT BUS_COMP_QGEN
C              / REPORTS, SELECT BUS_COMP_PLOAD
C              / REPORTS, SELECT BUS_COMP_QLOAD
C
C   Author: Walt Powell  Date: 15 July 1994
C   Called by: p_report.f
C
C****************************************************************
C
        integer function busdifrpt (in_buffer, out_buffer, scrfil)
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
        include 'ipfinc/ordsta.inc'
        include 'ipfinc/qksrt.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/alt_case.inc'
c
        common /file_name/ filename
        character filename*60

        common /scratch/ numdiff, ixref(MAXBUS), kdiff(2,MAXBUS), 
     &                   fdiff(6,MAXBUS), cdiff(2,MAXBUS)
c
c***kln Conversion to double precision
c
	double precision volt1, quan1, qk

        character cdiff * 1

	character type*2, relvlt*3, 
     &            capital*80, null*1, linefeed*1, header(4)*80, 
     &            text*80, word(20)*60, type1*2,
     &            type2*2, bus_name*8, bus_owner*3, bus_zone*2,
     &            dict(6)*10, unit_diff(3,10)*10
        integer o2, okt, bus_match, scrfilx, loaded
c
        logical found, finished, repeat
c
	external kmpuvov, swapuvov, kmpvltdif, swpvltdif
c
        logical gtfltr, chkfltr, change_f
c
        integer kmpuvov, apdoutbuf, tempfile,
     &          ldaltbse, status, kmpvltdif, findstr,
     &          rpt_type

        real totcap
c
        save 

        data dict / '$', '$', 'PGEN', 'QGEN', 'PLOAD', 'QLOAD' /

        data (unit_diff(1,i), i=1,9) / 9 * ' ' /
        data (unit_diff(2,i), i=1,6)
     &    / '(p.u.)' , '(MVAR)', '(MW)', '(MVAR)', '(MW)', '(MVAR)' /
        data (unit_diff(3,i), i=1,10)
     &    / '(percent)', '(MW)', '(MVAR)', '(MW)', '(MVAR)', '(p.u.)', 
     &       '(p.u.)', '(KV)', '(p.u.)', '(p.u.)' /

        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        maxbuf_in = len( in_buffer )
        maxbuf_out = len( out_buffer ) - 400

        busdifrpt = 0
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
        system_total_1 = 0.0
        system_total_2 = 0.0
        filter_total_1 = 0.0
        filter_total_2 = 0.0
c
c       Search for subtype of report
c
        ix = 1
        i1 = 1
        i2 = index (in_buffer, null)
        call uscan (in_buffer(i1:i2), word, nwrd, '=', ', ' // linefeed)
c
c       Search for subtype of report
c
        iwrd = 1
        do while (iwrd .le. nwrd .and. 
     &            (findstr(word(iwrd), 'BUS_COMP_') .eq. 0))
           iwrd = iwrd + 1
        enddo
        found = .false.
        if (iwrd .le. nwrd) then
           i = findstr(word(iwrd), 'BUS_COMP_') 
           text = capital (word(iwrd)(i+9:))
           rpt_type = 3
           do while (rpt_type .le. 6 .and. .not. found)
              if (dict(rpt_type) .eq. text) then
                 found = .true.
              else
                 rpt_type = rpt_type + 1
              endif
           enddo
        endif
        if (.not. found) go to 900
c
c       Search for alternate file
c
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
           busdifrpt = 1
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
           busdifrpt = 1
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
           busdifrpt = 1
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

        o2 = index (out_buffer,null)
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
           if (chkfltr( arcnam(jarzn(nb1)), zone(nb1), owner(nb1), 
     &                  base(nb1), type1, 0)) then
              volt1 = e(kt) ** 2 + f(kt) ** 2
              qk = qnetu(kt)
              call allocq (nb1, sngl(qk), qgen, qgnmax, qgnmin, qld,
     &                     totcap, usecap, totrek, userek, unsked, qerr)

              if (rpt_type .eq. 3) then
                 if (ntyp .ne. 5 .and. ntyp .ne. 12) then
                    quan1 = (pnetu(kt) + ploadu(kt)) * bmva
                 else
                    quan1 = 0.0
                 endif
              else if (rpt_type .eq. 4) then
                 if (ntyp .ne. 5 .and. ntyp .ne. 12) then
                    quan1 = qgen
                 else
                    quan1 = 0.0
                 endif
              else if (rpt_type .eq. 5) then
                 if (ntyp .ne. 5 .and. ntyp .ne. 12) then
                    quan1 = ploadu(kt) * bmva
                 else
                    quan1 = 0.0
                 endif
              else if (rpt_type .eq. 6) then
                 if (ntyp .ne. 5 .and. ntyp .ne. 12) then
                    quan1 = qld
                 else
                    quan1 = 0.0
                 endif
              else
                 quan1 = 0.0d0
              endif

              kt = oinp2opt(nb2)
              ntyp = ontypu(kt)
              call typno( type2(2:2), ntyp )
              qk = oqnetu(kt)
              volt2 = olde(kt) ** 2 + oldf(kt) ** 2
              call xallocq (nb2, sngl(qk), xqgen, qgnmax, qgnmin,
     &                      xqld, totcap, usecap, totrek, userek,
     &                      xunsked, qerr)

              if (rpt_type .eq. 3) then
                 if (ntyp .ne. 5 .and. ntyp .ne. 12) then
                    quan2 = (opnetu(kt) + oploadu(kt)) * bmva
                 else
                    quan2 = 0.0
                 endif
              else if (rpt_type .eq. 4) then
                 if (ntyp .ne. 5 .and. ntyp .ne. 12) then
                    quan2 = xqgen
                 else
                    quan2 = 0.0
                 endif
              else if (rpt_type .eq. 5) then
                 if (ntyp .ne. 5 .and. ntyp .ne. 12) then
                    quan2 = oploadu(kt) * bmva
                 else
                    quan2 = 0.0
                 endif
              else if (rpt_type .eq. 6) then
                 if (ntyp .ne. 5 .and. ntyp .ne. 12) then
                    quan2 = xqld
                 else
                    quan2 = 0.0
                 endif
              else
                 quan2 = 0.0
              endif

              system_total_1 = system_total_1 + quan1
              system_total_2 = system_total_2 + quan2
              idv = 100.0 * ((sngl(quan1)) - quan2)

              if (abs (idv) .ge. int(100.0 * voltdiff) .and.
     &            abs (quan1) + abs (quan2) .gt. 1.0e-3) then
                 numdiff = numdiff + 1
                 kdiff(1,numdiff) = nb1
                 kdiff(2,numdiff) = nb2
                 fdiff(1,numdiff) = iabs(idv) 
                 fdiff(2,numdiff) = sngl(quan1)
                 fdiff(3,numdiff) = quan2
                 fdiff(4,numdiff) = 0.0
                 fdiff(5,numdiff) = 0.0
                 fdiff(6,numdiff) = 0.0
                 cdiff(1,numdiff) = type1(2:2)
                 cdiff(2,numdiff) = type2(2:2)
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
           if (chkfltr( arcnam(jarzn(nb1)), zone(nb1), owner(nb1), 
     &                  base(nb1), type1, 0)) then

              qk = qnetu(kt)
              volt1 = e(kt) ** 2 + f(kt) ** 2
              call allocq (nb1, sngl(qk), qgen, qgnmax, qgnmin, qld,
     &                     totcap, usecap, totrek, userek, unsked, qerr)
              if (rpt_type .eq. 3) then
                 if (ntyp .ne. 5 .and. ntyp .ne. 12) then
                    quan1 = (pnetu(kt) + ploadu(kt)) * bmva
                 else
                    quan1 = 0.0
                 endif
              else if (rpt_type .eq. 4) then
                 if (ntyp .ne. 5 .and. ntyp .ne. 12) then
                    quan1 = qgen
                 else
                    quan1 = 0.0
                 endif
              else if (rpt_type .eq. 5) then
                 if (ntyp .ne. 5 .and. ntyp .ne. 12) then
                    quan1 = ploadu(kt) * bmva
                 else
                    quan1 = 0.0
                 endif
              else if (rpt_type .eq. 6) then
                 if (ntyp .ne. 5 .and. ntyp .ne. 12) then
                    quan1 = qld
                 else
                    quan1 = 0.0
                 endif
              else
                 quan1 = 0.0d0
              endif

              system_total_1 = system_total_1 + quan1
              idv = 100.0 * (sngl(quan1)) 

              if (abs (idv) .ge. int(100.0 * voltdiff) .and.
     &            abs (quan1) .gt. 1.0e-3) then
                 numdiff = numdiff + 1
                 kdiff(1,numdiff) = nb1
                 kdiff(2,numdiff) = 0
                 fdiff(1,numdiff) = iabs(idv) 
                 fdiff(2,numdiff) = sngl(quan1)
                 fdiff(3,numdiff) = 0.0
                 fdiff(4,numdiff) = 0.0
                 fdiff(5,numdiff) = 0.0
                 fdiff(6,numdiff) = 0.0
                 cdiff(1,numdiff) = type1(2:2)
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
           if (chkfltr( oarcnam(oarzn(nb2)), oldzone(nb2), 
     &                  oldowner(nb2), oldbase(nb2), type2, 0)) then
              qk = oqnetu(kt)
              volt2 = olde(kt) ** 2 + oldf(kt) ** 2
              call xallocq (nb2, sngl(qk), xqgen, qgnmax, qgnmin, xqld,
     &                     totcap, usecap, totrek, userek, unsked, qerr)
              if (rpt_type .eq. 3) then
                 if (ntyp .ne. 5 .and. ntyp .ne. 12) then
                    quan2 = (opnetu(kt) + oploadu(kt)) * bmva
                 else
                    quan2 = 0.0
                 endif
              else if (rpt_type .eq. 4) then
                 if (ntyp .ne. 5 .and. ntyp .ne. 12) then
                    quan2 = xqgen
                 else
                    quan2 = 0.0
                 endif
              else if (rpt_type .eq. 5) then
                 if (ntyp .ne. 5 .and. ntyp .ne. 12) then
                    quan2 = oploadu(kt) * bmva
                 else
                    quan2 = 0.0
                 endif
              else if (rpt_type .eq. 6) then
                 if (ntyp .ne. 5 .and. ntyp .ne. 12) then
                    quan2 = xqld
                 else
                    quan2 = 0.0
                 endif
              else
                 quan2 = 0.0
              endif

              system_total_2 = system_total_2 + quan2
              idv = -100.0 * quan2

              if (abs (idv) .ge. int(100.0 * voltdiff) .and.
     &            abs (quan2) .gt. 1.0e-3) then
                 numdiff = numdiff + 1
                 kdiff(1,numdiff) = 0
                 kdiff(2,numdiff) = nb2
                 fdiff(1,numdiff) = iabs(idv) 
                 fdiff(2,numdiff) = 0.0
                 fdiff(3,numdiff) = quan2
                 fdiff(4,numdiff) = 0.0
                 fdiff(5,numdiff) = 0.0
                 fdiff(6,numdiff) = 0.0
                 cdiff(1,numdiff) = ' '
                 cdiff(2,numdiff) = type2(2:2)
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

           last = lastch (dict(rpt_type))
           write (header(1), 142) dict(rpt_type)(1:last), cspare(30), 
     &        ocase
  142      format (' Bus difference report ', t24, a, t36, a, t53, a)
           write (header(2), 144) dict(rpt_type)(1:last),
     &                            dict(rpt_type)(1:last)
  144      format ( ' Ty Own Bus1           Zone      ', t36, a, t53, 
     &              a, t63, 'dif')
           last = lastch (unit_diff(2,rpt_type))
           write (header(3), 146) unit_diff(2,rpt_type)(1:last),
     &                            unit_diff(2,rpt_type)(1:last),
     &                            unit_diff(2,rpt_type)(1:last)
  146      format ( t36, a, t53, a, t63, a)

           out_buffer(1:1) = null
           o2 = index (out_buffer,null)
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
              quan1 = fdiff(2,i)
              ntyp = ntypu(kt)
              call typno( type(2:2), ntyp )
              bus_name = bus(nb1)
              bus_base = base(nb1)
              bus_owner = owner(nb1)
              bus_zone = zone(nb1)
           else
              quan1 = 0.0d0
           endif
           nb2 = kdiff(2,i)            
           if (nb2 .gt. 0) then
              okt = oinp2opt(nb2)
              quan2 = fdiff(3,i)
              ntyp = ontypu(okt)
              call typno( type(2:2), ntyp )
              bus_name = oldbus(nb2)
              bus_base = oldbase(nb2)
              bus_owner = oldowner(nb2)
              bus_zone = oldzone(nb2)
           else
              quan2 = 0.0
           endif
           if (nb1 .ne. 0 .and. nb2 .ne. 0) then
              relvlt = ' '
           else if (nb1 .gt. 0) then
              relvlt = '(1)'
           else
             relvlt = '(2)'
           endif
           qdiff = (sngl(quan1)) - quan2
           filter_total_1 = filter_total_1 + quan1
           filter_total_2 = filter_total_2 + quan2
           write (text, 180) type, bus_owner, bus_name, bus_base,
     &                       bus_zone, quan1, quan2, qdiff, 
     &                       relvlt
  180      format(1x, a2, 1x, a3, 1x, a8, f6.1, 2x, a2, t34, f8.1, t49,
     &            f8.1, t60, f7.1, 2x, a3)
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

        write (text, 182) voltdiff, filter_total_1, 
     &        filter_total_2
  182   format(1x, 'Total (Filter = ', f6.1, ')', t34, f8.1, t49, 
     &        f8.1)
        if (o2 .lt. maxbuf_out) then
           length = apdoutbuf(o2, text(1:80), out_buffer(o2:))
           o2 = o2 + length
           loop = numdiff + 1
        endif
        if (scrfilx .gt. 0) write (scrfilx, '(a)') text
        write (text, 184) system_total_1, system_total_2
  184   format(1x, 'Total', t34, f8.1, t49, f8.1)
        if (o2 .lt. maxbuf_out) then
           length = apdoutbuf(o2, text(1:80), out_buffer(o2:))
           o2 = o2 + length
           loop = numdiff + 2
        endif
        if (scrfilx .gt. 0) write (scrfilx, '(a)') text

c       Remember maxbuf_out is really 400 less than the real buffer size

        if (o2 .gt. maxbuf_out) then
           write (out_buffer(o2:o2+8), 190) linefeed, null
  190      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif

  900   continue
        return
        end
