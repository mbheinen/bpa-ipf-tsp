C    %W% %G%
C****************************************************************
C
C   File: lindifrpt.f
C
C   Purpose: Routine to obtain a filtered line quantity comparison 
C            report.
c
C   Invoked by:
C              / REPORTS, SELECT BR_COMP_FLOW_MW
C              / REPORTS, SELECT BR_COMP_FLOW_MVAR
C              / REPORTS, SELECT BR_COMP_LOSS_MW
C              / REPORTS, SELECT BR_COMP_LOSS_MVAR
C              / REPORTS, SELECT BR_COMP_R
C              / REPORTS, SELECT BR_COMP_X
C              / REPORTS, SELECT BR_COMP_TAP1
C              / REPORTS, SELECT BR_COMP_TAP2
C              / REPORTS, SELECT BR_COMP_G
C              / REPORTS, SELECT BR_COMP_B
C              / REPORTS, SELECT BR_COMP_RATE
C              / REPORTS, SELECT BR_COMP_RATE1
C              / REPORTS, SELECT BR_COMP_RATE2
C              / REPORTS, SELECT BR_COMP_RATE3
C              / REPORTS, SELECT BR_COMP_RATE*
C              / REPORTS, SELECT BR_COMP_Z*
C
C   Author: Walt Powell  Date: 14 December 1992
C   Called by: p_report.f
C
C****************************************************************
C
        integer function lindifrpt (in_buffer, out_buffer, scrfil)
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

	character type*2, relvlt*3, capital*80, null*1, linefeed*1, 
     &            header(4)*132, text*132, word(20)*60, text1*24,
     &            text2*24, dict(17)*10, unit_diff(3,17)*10, 
     &            ratec*10, ratecx*10, subhdr(4)*132, tag1*1, tag2*1, 
     &            id*1, brntyp*2, bus_1c*8, bus_2c*8, own*3, code*4, 
     &            base_1c*4, base_2c*4, own1*3, own2*3, subtxt(4)*30,
     &            id1*1, id2*1
        integer o2, sect, ptr1, ptr2, ptr1x, ptr2x, bus_match, 
     &          br_match, findstr, rpt_type, whichend1, whichend2, 
     &          whichend1x, whichend2x, scrfilx, loaded, apdoutbuf, 
     &          tempfile, ldaltbse, status, kmpvltdif, loop(2), 
     &          lastloop(2)
        logical found, chkfltr, repeat, finished, gtfltr, change_f
	external kmpvltdif, swpvltdif

        save

        data dict / '$$$', 'FLOW_MW' , 'FLOW_MVAR', 'LOSS_MW', 
     &              'LOSS_MVAR', 'R', 'X', 'TAP1', 'TAP2', 'G', 'B',
     &              'RATE', 'RATE1', 'RATE2', 'RATE3', 'RATE*', 'Z*' /

        data (unit_diff(1,i), i=1,9) / 9 * ' ' /
        data (unit_diff(2,i), i=1,6)
     &    / '(p.u.)' , '(MVAR)', '(MW)', '(MVAR)', '(MW)', '(MVAR)' /
        data (unit_diff(3,i), i=1,17)
     &    / '(percent)', '(MW)', '(MVAR)', '(MW)', '(MVAR)', '(p.u.)', 
     &       '(p.u.)', '(KV)', '(KV)', '(p.u.)', '(p.u.)', '(RATE)',
     &       '(RATE1)', '(RATE2)', '(RATE3)', '(RATE*)', '(p.u.)' /

        data subhdr(1) / ' Ty Own Bus1         Bus2         IS       R,X
     &       G/2,B/2   Miles     R,X       G/2,B/2   Miles     Dif' /
        data subhdr(2) / ' Ty Own Bus1         Bus2         IS       R,X
     &       G1,B1     G2,B2     R,X       G1,B1     G2,B2     Dif' /
        data subhdr(3) / ' Ty Own Bus1         Bus2         IS       R,X
     &       G,B       Tap1,Tap2 R,X       G,B       Tap1,Tap2 Dif' /
        data subhdr(4) / ' Ty Own Bus1         Bus2         IS       R,L
     &       C,Pdc     Vdc,Th    R,L       C,Pdc     Vdc,Th    Dif' /

        maxbuf_out = len( out_buffer ) - 400
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        o2 = index (out_buffer,null)

        lindifrpt = 0
c
c       Check for re-entry and continue
c
        last = index( in_buffer, null )
        i = last
        if (findstr (in_buffer(1:i), 'CONTINUE') .ne. 0) then
           repeat = .true.
           scrfilx = 0
           lastloop(1) = loop(1)         ! ix data loop
           lastloop(2) = loop(2)         ! iy subtitle
           go to 250
        endif

        repeat = .false.
        scrfilx = scrfil
        lastloop(1) = 0
        lastloop(2) = 0
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
     &            (findstr(word(iwrd), 'BR_COMP_') .eq. 0))
           iwrd = iwrd + 1
        enddo
        found = .false.
        if (iwrd .le. nwrd) then
           i = findstr(word(iwrd), 'BR_COMP_') 
           text = capital (word(iwrd)(i+8:))
           rpt_type = 2
           do while (rpt_type .le. 17 .and. .not. found)
              if (dict(rpt_type) .eq. text) then
                 found = .true.
              else
                 rpt_type = rpt_type + 1
              endif
           enddo
        endif
        if (.not. found) go to 900

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
           lindifrpt = 1
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
           lindifrpt = 1
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
           lindifrpt = 1
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
        quan1diff = range_filter(1)
        if (rpt_type .eq. 17) then
          quan2diff = range_filter(2)
        else
          quan2diff = 1.0
        endif
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

  170   if (numdiff .ge. MAXBUS-50) go to 230

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
           found = .false.
           do while (ptr1 .gt. 0 .and. .not. found) 
              if (brtype(ptr1) .eq. 4) then
                 ptr1 = brnch_nxt(ptr1)
              else
                 m1 = ky(ptr1)
                 lsw = 1
                 found = .true.
              endif
           enddo
           if (.not. found) lsw = 2

           ptr2 = okbsdta(16,nb2)
           found = .false.
           do while (ptr2 .gt. 0 .and. .not. found) 
              if (obrtype(ptr2) .eq. 4) then
                 ptr2 = obrnch_nxt(ptr2)
              else
                 m2 = oky(ptr2)
                 found = .true.
              endif
           enddo
           if (.not. found) lsw = lsw + 2
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
                 if (komp .eq. 0) then
                   id1 = brid(ptr1)
                   id2 = obrid(ptr2)
                   if (id1 .eq. '0') id1 = ' '
                   if (id2 .eq. '0') id2 = ' '
                   komp = kompr(id1, id2, junk)
                 endif
                 if (komp .eq. 0) komp = brsect(ptr1) - obrsect(ptr2)
                 if (komp .eq. 0) komp = brtype(ptr1) - obrtype(ptr2)
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
                 if (brtype(ptr1) .eq. 4) then
                 else if (rpt_type .ge. 2 .and. rpt_type .le. 5 .and.
     &                    brsect(ptr1) .gt. 0) then
                 else if (rpt_type .ge. 6 .and. 
     &                    brtype(ptr1) .eq. 1) then
                 else if (inp2alf(nb1) .lt. inp2alf(m1) .and.
     &               chkfltr (arcnam(jarzn(m1)), zone(m1), own1, 
     &                        base(m1), '**', 0)) then

                    call gtlfq (ptr1, pin, qin, ploss, qloss,
     &                          ovld, ratec, actual_amps,
     &                          whichend1, actual_mva, whichend2)

                    call xgtlfq (ptr2, pinx, qinx, plossx, qlossx,
     &                          ovldx, ratecx, actual_ampsx,
     &                          whichend1x, actual_mvax, whichend2x)

                    quanx = 0.0
                    quany = 0.0
                    if (rpt_type .eq. 2) then
                       quan1 = pin 
                       quan2 = pinx
                    else if (rpt_type .eq. 3) then
                       quan1 = qin
                       quan2 = qinx
                    else if (rpt_type .eq. 4) then
                       quan1 = ploss
                       quan2 = plossx
                    else if (rpt_type .eq. 5) then
                       quan1 = qloss
                       quan2 = qlossx
                    else if (rpt_type .eq. 6) then
                       if (brtype(ptr1) .eq. 3 .or. 
     &                     brtype(ptr1) .eq. 5 .or.
     &                     brtype(ptr1) .eq. 6 .or.
     &                     brtype(ptr1) .eq. 8) then
                          quan1 = brnch(5,nbr1) 
                          quan2 = obrnch(5,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 7) then
                       if (brtype(ptr1) .eq. 3 .or. 
     &                     brtype(ptr1) .eq. 5 .or.
     &                     brtype(ptr1) .eq. 6 .or.
     &                     brtype(ptr1) .eq. 8) then
                         quan1 = brnch(6,nbr1) 
                         quan2 = obrnch(6,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 8) then
                       if (brtype(ptr1) .eq. 5) then
                          nbr1 = brnch_ptr(ptr1)
                          if (nbr1 .gt. 0) then
                             quan1 = brnch(9,nbr1) 
                          else
                             quan1 = brnch(10,-nbr1) 
                          endif
                          nbr2 = obrnch_ptr(ptr2)
                          if (nbr2 .gt. 0) then
                             quan2 = obrnch(9,nbr2) 
                          else
                             quan2 = obrnch(10,-nbr2) 
                          endif
                       else if (brtype(ptr1) .eq. 6) then
                          nbr1 = brnch_ptr(ptr1)
                          if (nbr1 .gt. 0) then
                             quan1 = brnch(9,nbr1) 
                          else
                             quan1 = -brnch(9,-nbr1) 
                          endif
                          nbr2 = obrnch_ptr(ptr2)
                          if (nbr2 .gt. 0) then
                             quan2 = obrnch(9,nbr2) 
                          else
                             quan2 = -obrnch(9,-nbr2) 
                          endif
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 9) then
                       if (brtype(ptr1) .eq. 5) then
                          nbr1 = brnch_ptr(ptr1)
                          if (nbr1 .gt. 0) then
                             quan1 = brnch(10,nbr1) 
                          else
                             quan1 = brnch(9,-nbr1) 
                          endif
                          nbr2 = obrnch_ptr(ptr2)
                          if (nbr2 .gt. 0) then
                             quan2 = obrnch(10,nbr2) 
                          else
                             quan2 = obrnch(9,-nbr2) 
                          endif
                       else if (brtype(ptr1) .eq. 6) then
                          nbr1 = brnch_ptr(ptr1)
                          if (nbr1 .gt. 0) then
                             quan1 = brnch(10,nbr1) 
                          else
                             quan1 = brnch(10,-nbr1) 
                          endif
                          nbr2 = obrnch_ptr(ptr2)
                          if (nbr2 .gt. 0) then
                             quan2 = obrnch(10,nbr2) 
                          else
                             quan2 = obrnch(10,-nbr2) 
                          endif
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 10) then
                       if (brtype(ptr1) .eq. 3 .or. 
     &                     brtype(ptr1) .eq. 5 .or.
     &                     brtype(ptr1) .eq. 6 .or.
     &                     brtype(ptr1) .eq. 8) then
                         quan1 = brnch(7,nbr1) 
                         quan2 = obrnch(7,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 11) then
                       if (brtype(ptr1) .eq. 3 .or. 
     &                     brtype(ptr1) .eq. 5 .or.
     &                     brtype(ptr1) .eq. 6 .or.
     &                     brtype(ptr1) .eq. 8) then
                         quan1 = brnch(8,nbr1) 
                         quan2 = obrnch(8,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 12) then
                       if (brtype(ptr1) .eq. 2 .or. 
     &                     brtype(ptr1) .eq. 3 .or.
     &                     brtype(ptr1) .eq. 5 .or.
     &                     brtype(ptr1) .eq. 6 .or.
     &                     brtype(ptr1) .eq. 7 .or.
     &                     brtype(ptr1) .eq. 8) then
                         quan1 = brnch(4,nbr1) 
                         quan2 = obrnch(4,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 13) then
                       if (brtype(ptr1) .eq. 2 .or. 
     &                     brtype(ptr1) .eq. 3 .or.
     &                     brtype(ptr1) .eq. 5 .or.
     &                     brtype(ptr1) .eq. 6 .or.
     &                     brtype(ptr1) .eq. 7 .or.
     &                     brtype(ptr1) .eq. 8) then
                         quan1 = rateln(1,nbr1)
                         quan2 = orateln(1,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 14) then
                       if (brtype(ptr1) .eq. 2 .or. 
     &                     brtype(ptr1) .eq. 3 .or.
     &                     brtype(ptr1) .eq. 5 .or.
     &                     brtype(ptr1) .eq. 6 .or.
     &                     brtype(ptr1) .eq. 7 .or.
     &                     brtype(ptr1) .eq. 8) then
                         quan1 = rateln(2,nbr1)
                         quan2 = orateln(2,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 15) then
                       if (brtype(ptr1) .eq. 5 .or. 
     &                     brtype(ptr1) .eq. 6) then
                         quan1 = rateln(3,nbr1)
                         quan2 = orateln(3,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 16) then
                       if (brtype(ptr1) .eq. 5 .or. 
     &                     brtype(ptr1) .eq. 6) then
                         quan1 = rateln(1,nbr1) + rateln(2,nbr1) 
     &                         + rateln(3,nbr1) + brnch(4,nbr1) 
                         quan2 = orateln(1,nbr2) + orateln(2,nbr2) 
     &                         + orateln(3,nbr2) + obrnch(4,nbr2) 
                       else
                         quan1 = rateln(1,nbr1) + rateln(2,nbr1) 
     &                         + brnch(4,nbr1) 
                         quan2 = orateln(1,nbr2) + orateln(2,nbr2) 
     &                         + obrnch(4,nbr2) 
                       endif
                    else if (rpt_type .eq. 17) then
                       if (brtype(ptr1) .eq. 3) then
                         quan1 = brnch(5,nbr1) + brnch(6,nbr1)
     &                         + brnch(7,nbr1) + brnch(8,nbr1) 
                         quanx = brnch(9,nbr1)
                         quan2 = obrnch(5,nbr2) + obrnch(6,nbr2)
     &                         + obrnch(7,nbr2) + obrnch(8,nbr2) 
                         quany = obrnch(9,nbr2)
                       else if (brtype(ptr1) .eq. 5 .or. 
     &                          brtype(ptr1) .eq. 6) then
                         quan1 = brnch(5,nbr1) + brnch(6,nbr1)
     &                         + brnch(7,nbr1) + brnch(8,nbr1) 
                         quanx = brnch(9,nbr1) + brnch(10,nbr1)
                         quan2 = obrnch(5,nbr2) + obrnch(6,nbr2)
     &                         + obrnch(7,nbr2) + obrnch(8,nbr2) 
                         quany = obrnch(9,nbr2) + obrnch(10,nbr2)
                       else if (brtype(ptr1) .eq. 8) then
                         quan1 = brnch(5,nbr1) + brnch(6,nbr1)
     &                         + brnch(7,nbr1) + brnch(8,nbr1) 
     &                         + brnch(9,nbr1) + brnch(10,nbr1)
                         quan2 = obrnch(5,nbr2) + obrnch(6,nbr2)
     &                         + obrnch(7,nbr2) + obrnch(8,nbr2) 
     &                         + obrnch(9,nbr2) + obrnch(10,nbr2)
                       else
                         quan1 = 0.0
                         quanx = brnch(5,nbr1) + brnch(6,nbr1)
     &                         + brnch(7,nbr1) + brnch(8,nbr1) 
     &                         + brnch(9,nbr1) + brnch(10,nbr1)
                         quan2 = 0.0
                         quany = obrnch(5,nbr2) + obrnch(6,nbr2) 
     &                         + obrnch(7,nbr2) + obrnch(8,nbr2) 
     &                         + obrnch(9,nbr2) + obrnch(10,nbr2)
                       endif
                    else
                       quan1 = 0.0
                       quan2 = 0.0
                    endif

                    if (rpt_type .eq. 6 .or. rpt_type .eq. 7 .or.
     &                  rpt_type .eq. 10 .or. rpt_type .eq. 11 .or.
     &                  rpt_type .eq. 17) then
                       xmult = 10000.0
                    else
                       xmult = 100.0
                    endif
                    idv = int(xmult * (quan1 - quan2))
                    idy = int(100.0 * (quanx - quany))
                    if (iabs (idv) .gt. int(xmult * quan1diff) .or.
     &                  iabs (idy) .gt. int(100.0 * quan2diff)) then
                       numdiff = numdiff + 1
                       kdiff(1,numdiff) = ptr1
                       kdiff(2,numdiff) = ptr2
                       fdiff(1,numdiff) = iabs(idv) + iabs(idy) 
                       fdiff(2,numdiff) = quan1
                       fdiff(3,numdiff) = quan2
                       fdiff(4,numdiff) = 0.0
                       fdiff(5,numdiff) = 0.0
                       fdiff(6,numdiff) = idv
                       cdiff(1,numdiff) = tag1
                       cdiff(2,numdiff) = tag2
                    endif
                 endif
c
c                Increment branches for m1 and m2
c
                 found = .false.
                 do while (ptr1 .gt. 0 .and. .not. found) 
                    lsw = incm1vdif (ptr1, nb1, m1, lsw)
                    if (brtype(ptr1) .ne. 4) found = .true.
                 enddo

                 found = .false.
                 do while (ptr2 .gt. 0 .and. .not. found) 
                    lsw = incm2vdif (ptr2, nb2, m2, lsw)
                    if (obrtype(ptr2) .ne. 4) found = .true.
                 enddo

              else if (br_match .lt. 0) then

                 nbr1 = iabs(brnch_ptr(ptr1))
                 call getchr (3, own1, kbrnch(3,nbr1))
                 if (brtype(ptr1) .eq. 4) then
                 else if (rpt_type .ge. 2 .and. rpt_type .le. 5 .and.
     &                    brsect(ptr1) .gt. 0) then
                 else if (rpt_type .ge. 6 .and. 
     &                    brtype(ptr1) .eq. 1) then
                 else if (inp2alf(nb1) .lt. inp2alf(m1) .and.
     &                    chkfltr (arcnam(jarzn(m1)), zone(m1), own1, 
     &                             base(m1), '**', 0)) then

                    call gtlfq (ptr1, pin, qin, ploss, qloss,
     &                          ovld, ratec, actual_amps,
     &                          whichend1, actual_mva, whichend2)

                    quanx = 0.0
                    quany = 0.0
                    if (rpt_type .eq. 2) then
                       quan1 = pin 
                       quan2 = 0.0
                    else if (rpt_type .eq. 3) then
                       quan1 = qin
                       quan2 = 0.0
                    else if (rpt_type .eq. 4) then
                       quan1 = ploss
                       quan2 = 0.0
                    else if (rpt_type .eq. 5) then
                       quan1 = qloss
                       quan2 = 0.0
                    else if (rpt_type .eq. 6) then
                       if (brtype(ptr1) .eq. 3 .or. 
     &                     brtype(ptr1) .eq. 5  .or.
     &                     brtype(ptr1) .eq. 6 .or.
     &                     brtype(ptr1) .eq. 8) then
                          quan1 = brnch(5,nbr1) 
                          quan2 = 0.0
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 7) then
                       if (brtype(ptr1) .eq. 3 .or. 
     &                     brtype(ptr1) .eq. 5 .or.
     &                     brtype(ptr1) .eq. 6 .or.
     &                     brtype(ptr1) .eq. 8) then
                         quan1 = brnch(6,nbr1) 
                         quan2 = 0.0
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 8) then
                       if (brtype(ptr1) .eq. 5) then
                          nbr1 = brnch_ptr(ptr1)
                          if (nbr1 .gt. 0) then
                             quan1 = brnch(9,nbr1) 
                          else
                             quan1 = brnch(10,-nbr1) 
                          endif
                          quan2 = 0.0
                       else if (brtype(ptr1) .eq. 6) then
                          nbr1 = brnch_ptr(ptr1)
                          if (nbr1 .gt. 0) then
                             quan1 = brnch(9,nbr1) 
                          else
                             quan1 = -brnch(9,-nbr1) 
                          endif
                          quan2 = 0.0
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 9) then
                       if (brtype(ptr1) .eq. 5) then
                          nbr1 = brnch_ptr(ptr1)
                          if (nbr1 .gt. 0) then
                             quan1 = brnch(10,nbr1) 
                          else
                             quan1 = brnch(9,-nbr1) 
                          endif
                          quan2 = 0.0
                       else if (brtype(ptr1) .eq. 6) then
                          nbr1 = brnch_ptr(ptr1)
                          if (nbr1 .gt. 0) then
                             quan1 = brnch(10,nbr1) 
                          else
                             quan1 = brnch(10,-nbr1) 
                          endif
                          quan2 = 0.0
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 10) then
                       if (brtype(ptr1) .eq. 3 .or. 
     &                     brtype(ptr1) .eq. 5 .or.
     &                     brtype(ptr1) .eq. 6 .or.
     &                     brtype(ptr1) .eq. 8) then
                         quan1 = brnch(7,nbr1) 
                         quan2 = 0.0
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 11) then
                       if (brtype(ptr1) .eq. 3 .or. 
     &                     brtype(ptr1) .eq. 5 .or.
     &                     brtype(ptr1) .eq. 6 .or.
     &                     brtype(ptr1) .eq. 8) then
                         quan1 = brnch(8,nbr1) 
                         quan2 = 0.0
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 12) then
                       if (brtype(ptr1) .eq. 2 .or. 
     &                     brtype(ptr1) .eq. 3 .or.
     &                     brtype(ptr1) .eq. 5 .or.
     &                     brtype(ptr1) .eq. 6 .or.
     &                     brtype(ptr1) .eq. 7 .or.
     &                     brtype(ptr1) .eq. 8) then
                         quan1 = brnch(4,nbr1) 
                         quan2 = 0.0
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 13) then
                       if (brtype(ptr1) .eq. 2 .or. 
     &                     brtype(ptr1) .eq. 3 .or.
     &                     brtype(ptr1) .eq. 5 .or.
     &                     brtype(ptr1) .eq. 6 .or.
     &                     brtype(ptr1) .eq. 7 .or.
     &                     brtype(ptr1) .eq. 8) then
                         quan1 = rateln(1,nbr1)
                         quan2 = 0.0
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 14) then
                       if (brtype(ptr1) .eq. 2 .or. 
     &                     brtype(ptr1) .eq. 3 .or.
     &                     brtype(ptr1) .eq. 5 .or.
     &                     brtype(ptr1) .eq. 6 .or.
     &                     brtype(ptr1) .eq. 7 .or.
     &                     brtype(ptr1) .eq. 8) then
                         quan1 = rateln(2,nbr1)
                         quan2 = 0.0
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 15) then
                       if (brtype(ptr1) .eq. 5 .or. 
     &                     brtype(ptr1) .eq. 6) then
                         quan1 = rateln(3,nbr1)
                         quan2 = 0.0
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 16) then
                       if (brtype(ptr1) .eq. 5 .or. 
     &                     brtype(ptr1) .eq. 6) then
                         quan1 = rateln(1,nbr1) + rateln(2,nbr1) 
     &                         + rateln(3,nbr1) + brnch(4,nbr1) 
                         quan2 = 0.0
                       else
                         quan1 = rateln(1,nbr1) + rateln(2,nbr1) 
     &                         + brnch(4,nbr1) 
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 17) then
                       if (brtype(ptr1) .eq. 3) then
                         quan1 = brnch(5,nbr1) + brnch(6,nbr1)
     &                         + brnch(7,nbr1) + brnch(8,nbr1) 
                         quanx = brnch(9,nbr1)
                         quan2 = 0.0
                       else if (brtype(ptr1) .eq. 5 .or. 
     &                          brtype(ptr1) .eq. 6) then
                         quan1 = brnch(5,nbr1) + brnch(6,nbr1)
     &                         + brnch(7,nbr1) + brnch(8,nbr1) 
                         quanx = brnch(9,nbr1) + brnch(10,nbr1)
                         quan2 = 0.0
                       else if (brtype(ptr1) .eq. 8) then
                         quan1 = brnch(5,nbr1) + brnch(6,nbr1)
     &                         + brnch(7,nbr1) + brnch(8,nbr1) 
                         quanx = brnch(9,nbr1) + brnch(10,nbr1)
                         quan2 = 0.0
                       else
                         quan1 = 0.0
                         quanx = brnch(5,nbr1) + brnch(6,nbr1) 
     &                         + brnch(7,nbr1) + brnch(8,nbr1) 
     &                         + brnch(9,nbr1) + brnch(10,nbr1)
                         quan2 = 0.0
                       endif
                    else
                       quan1 = 0.0
                       quan2 = 0.0
                    endif

                    if (rpt_type .eq. 6 .or. rpt_type .eq. 7 .or.
     &                  rpt_type .eq. 10 .or. rpt_type .eq. 11 .or.
     &                  rpt_type .eq. 17) then
                       xmult = 10000.0
                    else
                       xmult = 100.0
                    endif
                    idv = int(xmult * (quan1 - quan2))
                    idy = int(100.0 * (quanx - quany))
                    if (iabs (idv) .gt. int(xmult * quan1diff) .or.
     &                  iabs (idy) .gt. int(100.0 * quan2diff)) then
                       numdiff = numdiff + 1
                       kdiff(1,numdiff) = ptr1
                       kdiff(2,numdiff) = 0
                       fdiff(1,numdiff) = iabs(idv) + iabs(idy) 
                       fdiff(2,numdiff) = quan1
                       fdiff(3,numdiff) = 0.0
                       fdiff(4,numdiff) = 0.0
                       fdiff(5,numdiff) = 0.0
                       fdiff(6,numdiff) = idv
                       cdiff(1,numdiff) = tag1
                       cdiff(2,numdiff) = ' '
                    endif
                 endif
c
c                Increment branches for m1
c
                 found = .false.
                 do while (ptr1 .gt. 0 .and. .not. found) 
                    lsw = incm1vdif (ptr1, nb1, m1, lsw)
                    if (brtype(ptr1) .ne. 4) found = .true.
                 enddo

              else

                 nbr2 = iabs(obrnch_ptr(ptr2))
                 call getchr (3, own2, okbrnch(3,nbr2))
                 if (obrtype(ptr2) .eq. 4) then
                 else if (rpt_type .ge. 2 .and. rpt_type .le. 5 .and.
     &                    brsect(ptr2) .gt. 0) then
                 else if (rpt_type .ge. 6 .and. 
     &                    obrtype(ptr2) .eq. 1) then
                 else if (oinp2alf(nb2) .le. oinp2alf(m2) .and.
     &               chkfltr (oarcnam(oarzn(m2)), oldzone(m2), own2, 
     &                        oldbase(m2), '**', 0)) then

                    call xgtlfq (ptr2, pinx, qinx, plossx, qlossx,
     &                          ovldx, ratecx, actual_ampsx,
     &                          whichend1x, actual_mvax, whichend2x)

                    quanx = 0.0
                    quany = 0.0
                    if (rpt_type .eq. 2) then
                       quan1 = 0.0
                       quan2 = pinx
                    else if (rpt_type .eq. 3) then
                       quan1 = 0.0
                       quan2 = qinx
                    else if (rpt_type .eq. 4) then
                       quan1 = 0.0
                       quan2 = plossx
                    else if (rpt_type .eq. 5) then
                       quan1 = 0.0
                       quan2 = qlossx
                    else if (rpt_type .eq. 6) then
                       if (obrtype(ptr2) .eq. 3 .or. 
     &                     obrtype(ptr2) .eq. 5 .or.
     &                     obrtype(ptr2) .eq. 6 .or.
     &                     obrtype(ptr2) .eq. 8) then
                          quan1 = 0.0
                          quan2 = obrnch(5,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 7) then
                       if (obrtype(ptr2) .eq. 3 .or. 
     &                     obrtype(ptr2) .eq. 5 .or.
     &                     obrtype(ptr2) .eq. 6 .or.
     &                     obrtype(ptr2) .eq. 8) then
                         quan1 = 0.0
                         quan2 = obrnch(6,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 8) then
                       if (obrtype(ptr2) .eq. 5) then
                          quan1 = 0.0
                          nbr2 = obrnch_ptr(ptr2)
                          if (nbr2 .gt. 0) then
                             quan2 = obrnch(9,nbr2) 
                          else
                             quan2 = obrnch(10,-nbr2) 
                          endif
                       else if (obrtype(ptr2) .eq. 6) then
                          quan1 = 0.0
                          nbr2 = obrnch_ptr(ptr2)
                          if (nbr2 .gt. 0) then
                             quan2 = obrnch(9,nbr2) 
                          else
                             quan2 = -obrnch(9,-nbr2) 
                          endif
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 9) then
                       if (obrtype(ptr2) .eq. 5) then
                          quan1 = 0.0
                          nbr2 = obrnch_ptr(ptr2)
                          if (nbr2 .gt. 0) then
                             quan2 = obrnch(10,nbr2) 
                          else
                             quan2 = obrnch(9,-nbr2) 
                          endif
                       else if (obrtype(ptr2) .eq. 6) then
                          quan1 = 0.0
                          nbr2 = obrnch_ptr(ptr2)
                          if (nbr2 .gt. 0) then
                             quan2 = obrnch(10,nbr2) 
                          else
                             quan2 = obrnch(10,-nbr2) 
                          endif
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 10) then
                       if (obrtype(ptr2) .eq. 3 .or. 
     &                     obrtype(ptr2) .eq. 5 .or.
     &                     obrtype(ptr2) .eq. 6 .or.
     &                     obrtype(ptr2) .eq. 8) then
                         quan1 = 0.0
                         quan2 = obrnch(7,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 11) then
                       if (obrtype(ptr2) .eq. 3 .or. 
     &                     obrtype(ptr2) .eq. 5 .or.
     &                     obrtype(ptr2) .eq. 6 .or.
     &                     obrtype(ptr2) .eq. 8) then
                         quan1 = 0.0
                         quan2 = obrnch(8,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 12) then
                       if (obrtype(ptr2) .eq. 2 .or. 
     &                     obrtype(ptr2) .eq. 3 .or.
     &                     obrtype(ptr2) .eq. 5 .or.
     &                     obrtype(ptr2) .eq. 6 .or.
     &                     obrtype(ptr2) .eq. 7 .or.
     &                     obrtype(ptr2) .eq. 8) then
                         quan1 = 0.0
                         quan2 = obrnch(4,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 13) then
                       if (obrtype(ptr2) .eq. 2 .or. 
     &                     obrtype(ptr2) .eq. 3 .or.
     &                     obrtype(ptr2) .eq. 5 .or.
     &                     obrtype(ptr2) .eq. 6 .or.
     &                     obrtype(ptr2) .eq. 7 .or.
     &                     obrtype(ptr2) .eq. 8) then
                         quan1 = 0.0
                         quan2 = orateln(1,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 14) then
                       if (obrtype(ptr2) .eq. 2 .or. 
     &                     obrtype(ptr2) .eq. 3 .or.
     &                     obrtype(ptr2) .eq. 5 .or.
     &                     obrtype(ptr2) .eq. 6 .or.
     &                     obrtype(ptr2) .eq. 7 .or.
     &                     obrtype(ptr2) .eq. 8) then
                         quan1 = 0.0
                         quan2 = orateln(2,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 15) then
                       if (obrtype(ptr2) .eq. 5 .or. 
     &                     obrtype(ptr2) .eq. 6) then
                         quan1 = 0.0
                         quan2 = orateln(3,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                       endif
                    else if (rpt_type .eq. 16) then
                       if (obrtype(ptr2) .eq. 5 .or. 
     &                     obrtype(ptr2) .eq. 6) then
                         quan1 = 0.0
                         quan2 = orateln(1,nbr2) + orateln(2,nbr2) 
     &                         + orateln(3,nbr2) + obrnch(4,nbr2) 
                       else
                         quan1 = 0.0
                         quan2 = orateln(1,nbr2) + orateln(2,nbr2) 
     &                         + obrnch(4,nbr2) 
                       endif
                    else if (rpt_type .eq. 17) then
                       if (obrtype(ptr2) .eq. 3) then
                         quan1 = 0.0
                         quan2 = obrnch(5,nbr2) + obrnch(6,nbr2)
     &                         + obrnch(7,nbr2) + obrnch(8,nbr2) 
                         quany = obrnch(9,nbr2)
                       else if (obrtype(ptr2) .eq. 5 .or. 
     &                          obrtype(ptr2) .eq. 6) then
                         quan1 = 0.0
                         quan2 = obrnch(5,nbr2) + obrnch(6,nbr2)
     &                         + obrnch(7,nbr2) + obrnch(8,nbr2) 
                         quany = obrnch(9,nbr2) + obrnch(10,nbr2)
                       else if (obrtype(ptr2) .eq. 8) then
                         quan1 = 0.0
                         quan2 = obrnch(5,nbr2) + obrnch(6,nbr2)
     &                         + obrnch(7,nbr2) + obrnch(8,nbr2) 
     &                         + obrnch(9,nbr2) + obrnch(10,nbr2)
                       else
                         quan1 = 0.0
                         quan2 = 0.0
                         quany = obrnch(5,nbr2) + obrnch(6,nbr2) 
     &                         + obrnch(7,nbr2) + obrnch(8,nbr2) 
     &                         + obrnch(9,nbr2) + obrnch(10,nbr2)
                       endif
                    else
                       quan1 = 0.0
                       quan2 = 0.0
                    endif

                    if (rpt_type .eq. 6 .or. rpt_type .eq. 7 .or.
     &                  rpt_type .eq. 10 .or. rpt_type .eq. 11 .or.
     &                  rpt_type .eq. 17) then
                       xmult = 10000.0
                    else
                       xmult = 100.0
                    endif
                    idv = int(xmult * (quan1 - quan2))
                    idy = int(100.0 * (quanx - quany))
                    if (iabs (idv) .gt. int(xmult * quan1diff) .or.
     &                  iabs (idy) .gt. int(100.0 * quan2diff)) then
                       numdiff = numdiff + 1
                       kdiff(1,numdiff) = 0
                       kdiff(2,numdiff) = ptr2
                       fdiff(1,numdiff) = iabs(idv) + iabs(idy) 
                       fdiff(2,numdiff) = 0.0
                       fdiff(3,numdiff) = quan2
                       fdiff(4,numdiff) = 0.0
                       fdiff(5,numdiff) = 0.0
                       fdiff(6,numdiff) = idv
                       cdiff(1,numdiff) = ' '
                       cdiff(2,numdiff) = tag2
                    endif
                 endif
c
c                Increment branches for m2
c
                 found = .false.
                 do while (ptr2 .gt. 0 .and. .not. found) 
                    lsw = incm2vdif (ptr2, nb2, m2, lsw)
                    if (obrtype(ptr2) .ne. 4) found = .true.
                 enddo

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
              if (brtype(ptr1) .eq. 4) then
              else if (rpt_type .ge. 2 .and. rpt_type .le. 5 .and.
     &                 brsect(ptr1) .gt. 0) then
              else if (rpt_type .ge. 6 .and. 
     &                 brtype(ptr1) .eq. 1) then
              else if (inp2alf(nb1) .lt. inp2alf(m1) .and.
     &            chkfltr (arcnam(jarzn(m1)), zone(m1), own1, 
     &                     base(m1), '**', 0)) then
c
                 call gtlfq (ptr1, pin, qin, ploss, qloss,
     &                       ovld, ratec, actual_amps,
     &                       whichend1, actual_mva, whichend2)

                 quanx = 0.0
                 quany = 0.0
                 if (rpt_type .eq. 2) then
                    quan1 = pin 
                    quan2 = 0.0
                 else if (rpt_type .eq. 3) then
                    quan1 = qin
                    quan2 = 0.0
                 else if (rpt_type .eq. 4) then
                    quan1 = ploss
                    quan2 = 0.0
                 else if (rpt_type .eq. 5) then
                    quan1 = qloss
                    quan2 = 0.0
                 else if (rpt_type .eq. 6) then
                    if (brtype(ptr1) .eq. 3 .or. 
     &                  brtype(ptr1) .eq. 5 .or.
     &                  brtype(ptr1) .eq. 6 .or.
     &                  brtype(ptr1) .eq. 8) then
                       quan1 = brnch(5,nbr1) 
                       quan2 = 0.0
                    else
                       quan1 = 0.0
                       quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 7) then
                    if (brtype(ptr1) .eq. 3 .or. 
     &                  brtype(ptr1) .eq. 5 .or.
     &                  brtype(ptr1) .eq. 6 .or.
     &                  brtype(ptr1) .eq. 8) then
                       quan1 = brnch(6,nbr1) 
                       quan2 = 0.0
                    else
                       quan1 = 0.0
                       quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 8) then
                    if (brtype(ptr1) .eq. 5) then
                       nbr1 = brnch_ptr(ptr1)
                       if (nbr1 .gt. 0) then
                          quan1 = brnch(9,nbr1) 
                       else
                          quan1 = brnch(10,-nbr1) 
                       endif
                       quan2 = 0.0
                    else if (brtype(ptr1) .eq. 6) then
                       nbr1 = brnch_ptr(ptr1)
                       if (nbr1 .gt. 0) then
                          quan1 = brnch(9,nbr1) 
                       else
                          quan1 = -brnch(9,-nbr1) 
                       endif
                       quan2 = 0.0
                    else
                      quan1 = 0.0
                      quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 9) then
                    if (brtype(ptr1) .eq. 5) then
                       nbr1 = brnch_ptr(ptr1)
                       if (nbr1 .gt. 0) then
                          quan1 = brnch(10,nbr1) 
                       else
                          quan1 = brnch(9,-nbr1) 
                       endif
                       quan2 = 0.0
                    else if (brtype(ptr1) .eq. 6) then
                       nbr1 = brnch_ptr(ptr1)
                       if (nbr1 .gt. 0) then
                          quan1 = brnch(10,nbr1) 
                       else
                          quan1 = brnch(10,-nbr1) 
                       endif
                       quan2 = 0.0
                    else
                      quan1 = 0.0
                      quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 10) then
                    if (brtype(ptr1) .eq. 3 .or. 
     &                  brtype(ptr1) .eq. 5 .or.
     &                  brtype(ptr1) .eq. 6 .or.
     &                  brtype(ptr1) .eq. 8) then
                       quan1 = brnch(7,nbr1) 
                       quan2 = 0.0
                    else
                       quan1 = 0.0
                       quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 11) then
                    if (brtype(ptr1) .eq. 3 .or. 
     &                  brtype(ptr1) .eq. 5 .or.
     &                  brtype(ptr1) .eq. 6 .or.
     &                  brtype(ptr1) .eq. 8) then
                       quan1 = brnch(8,nbr1) 
                       quan2 = 0.0
                    else
                       quan1 = 0.0
                       quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 12) then
                    if (brtype(ptr1) .eq. 2 .or. 
     &                  brtype(ptr1) .eq. 3 .or.
     &                  brtype(ptr1) .eq. 5 .or.
     &                  brtype(ptr1) .eq. 6 .or.
     &                  brtype(ptr1) .eq. 7 .or.
     &                  brtype(ptr1) .eq. 8) then
                      quan1 = brnch(4,nbr1) 
                      quan2 = 0.0
                    else
                      quan1 = 0.0
                      quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 13) then
                    if (brtype(ptr1) .eq. 2 .or. 
     &                  brtype(ptr1) .eq. 3 .or.
     &                  brtype(ptr1) .eq. 5 .or.
     &                  brtype(ptr1) .eq. 6 .or.
     &                  brtype(ptr1) .eq. 7 .or.
     &                  brtype(ptr1) .eq. 8) then
                      quan1 = rateln(1,nbr1)
                      quan2 = 0.0
                    else
                      quan1 = 0.0
                      quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 14) then
                    if (brtype(ptr1) .eq. 2 .or. 
     &                  brtype(ptr1) .eq. 3 .or.
     &                  brtype(ptr1) .eq. 5 .or.
     &                  brtype(ptr1) .eq. 6 .or.
     &                  brtype(ptr1) .eq. 7 .or.
     &                  brtype(ptr1) .eq. 8) then
                      quan1 = rateln(2,nbr1)
                      quan2 = 0.0
                    else
                      quan1 = 0.0
                      quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 15) then
                    if (brtype(ptr1) .eq. 5 .or. 
     &                  brtype(ptr1) .eq. 6) then
                      quan1 = rateln(3,nbr1)
                      quan2 = 0.0
                    else
                      quan1 = 0.0
                      quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 16) then
                    if (brtype(ptr1) .eq. 5 .or. 
     &                  brtype(ptr1) .eq. 6) then
                      quan1 = rateln(1,nbr1) + rateln(2,nbr1) 
     &                      + rateln(3,nbr1) + brnch(4,nbr1) 
                      quan2 = 0.0
                    else
                      quan1 = rateln(1,nbr1) + rateln(2,nbr1) 
     &                      + brnch(4,nbr1) 
                      quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 17) then
                    if (brtype(ptr1) .eq. 3) then
                      quan1 = brnch(5,nbr1) + brnch(6,nbr1)
     &                      + brnch(7,nbr1) + brnch(8,nbr1) 
                      quanx = brnch(9,nbr1)
                      quan2 = 0.0
                    else if (brtype(ptr1) .eq. 5 .or. 
     &                       brtype(ptr1) .eq. 6) then
                      quan1 = brnch(5,nbr1) + brnch(6,nbr1)
     &                      + brnch(7,nbr1) + brnch(8,nbr1) 

                      quanx = brnch(9,nbr1) + brnch(10,nbr1)
                      quan2 = 0.0
                    else if (brtype(ptr1) .eq. 8) then
                      quan1 = brnch(5,nbr1) + brnch(6,nbr1)
     &                      + brnch(7,nbr1) + brnch(8,nbr1) 
     &                      + brnch(9,nbr1) + brnch(10,nbr1)
                      quan2 = 0.0
                    else
                      quan1 = 0.0
                      quanx = brnch(5,nbr1) + brnch(6,nbr1) 
     &                      + brnch(7,nbr1) + brnch(8,nbr1) 
     &                      + brnch(9,nbr1) + brnch(10,nbr1)
                      quan2 = 0.0
                    endif
                 else
                    quan1 = 0.0
                    quan2 = 0.0
                 endif
                 if (quan1 .gt. 0) then
                    if (rpt_type .eq. 6 .or. rpt_type .eq. 7 .or.
     &                  rpt_type .eq. 10 .or. rpt_type .eq. 11 .or.
     &                  rpt_type .eq. 17) then
                       xmult = 10000.0
                    else
                       xmult = 100.0
                    endif
                    idv = int(xmult * (quan1 - quan2))
                    idy = int(100.0 * (quanx - quany))
                    if (iabs (idv) .gt. int(xmult * quan1diff) .or.
     &                  iabs (idy) .gt. int(100.0 * quan2diff)) then
                       numdiff = numdiff + 1
                       kdiff(1,numdiff) = ptr1
                       kdiff(2,numdiff) = 0
                       fdiff(1,numdiff) = iabs(idv) + iabs(idy) 
                       fdiff(2,numdiff) = quan1
                       fdiff(3,numdiff) = 0.0
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
              found = .false.
              do while (ptr1 .gt. 0 .and. .not. found) 
                 lsw = incm1vdif (ptr1, nb1, m1, lsw)
                 if (brtype(ptr1) .ne. 4) found = .true.
              enddo

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
              if (obrtype(ptr2) .eq. 4) then
              else if (rpt_type .ge. 2 .and. rpt_type .le. 5 .and.
     &                 obrsect(ptr2) .gt. 0) then
              else if (rpt_type .ge. 6 .and. 
     &                 obrtype(ptr2) .eq. 1) then
              else if (oinp2alf(nb2) .lt. oinp2alf(m2) .and.
     &            chkfltr (oarcnam(oarzn(m2)), oldzone(m2), own2, 
     &                     base(m2), '**', 0)) then
 
                 call xgtlfq (ptr2, pinx, qinx, plossx, qlossx,
     &                        ovldx, ratecx, actual_ampsx,
     &                        whichend1x, actual_mvax, whichend2x)

                 quanx = 0.0
                 quany = 0.0
                 if (rpt_type .eq. 2) then
                    quan1 = 0.0
                    quan2 = pinx
                 else if (rpt_type .eq. 3) then
                    quan1 = 0.0
                    quan2 = qinx
                 else if (rpt_type .eq. 4) then
                    quan1 = 0.0
                    quan2 = plossx
                 else if (rpt_type .eq. 5) then
                    quan1 = 0.0
                    quan2 = qlossx
                 else if (rpt_type .eq. 6) then
                    if (obrtype(ptr2) .eq. 3 .or. 
     &                  obrtype(ptr2) .eq. 5 .or.
     &                  obrtype(ptr2) .eq. 6 .or.
     &                  obrtype(ptr2) .eq. 8) then
                       quan1 = 0.0
                       quan2 = obrnch(5,nbr2) 
                    else
                      quan1 = 0.0
                      quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 7) then
                    if (obrtype(ptr2) .eq. 3 .or. 
     &                  obrtype(ptr2) .eq. 5 .or.
     &                  obrtype(ptr2) .eq. 6 .or.
     &                  obrtype(ptr2) .eq. 8) then
                       quan1 = 0.0
                       quan2 = obrnch(6,nbr2) 
                    else
                       quan1 = 0.0
                       quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 8) then
                    if (obrtype(ptr2) .eq. 5) then
                       quan1 = 0.0
                       nbr2 = obrnch_ptr(ptr2)
                       if (nbr2 .gt. 0) then
                          quan2 = obrnch(9,nbr2) 
                       else
                          quan2 = obrnch(10,-nbr2) 
                       endif
                    else if (obrtype(ptr2) .eq. 6) then
                       quan1 = 0.0
                       nbr2 = obrnch_ptr(ptr2)
                       if (nbr2 .gt. 0) then
                          quan2 = obrnch(9,nbr2) 
                       else
                          quan2 = -obrnch(9,-nbr2) 
                       endif
                    else
                      quan1 = 0.0
                      quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 10) then
                    if (obrtype(ptr2) .eq. 3 .or. 
     &                  obrtype(ptr2) .eq. 5 .or.
     &                  obrtype(ptr2) .eq. 6 .or.
     &                  obrtype(ptr2) .eq. 8) then
                      quan1 = 0.0
                      quan2 = obrnch(7,nbr2) 
                    else
                      quan1 = 0.0
                      quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 11) then
                    if (obrtype(ptr2) .eq. 3 .or. 
     &                  obrtype(ptr2) .eq. 5 .or.
     &                  obrtype(ptr2) .eq. 6 .or.
     &                  obrtype(ptr2) .eq. 8) then
                      quan1 = 0.0
                      quan2 = obrnch(8,nbr2) 
                    else
                      quan1 = 0.0
                      quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 12) then
                    if (obrtype(ptr2) .eq. 2 .or. 
     &                  obrtype(ptr2) .eq. 3 .or.
     &                  obrtype(ptr2) .eq. 5 .or.
     &                  obrtype(ptr2) .eq. 6 .or.
     &                  obrtype(ptr2) .eq. 7 .or.
     &                  obrtype(ptr2) .eq. 8) then
                      quan1 = 0.0
                      quan2 = obrnch(4,nbr2) 
                    else
                      quan1 = 0.0
                      quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 13) then
                    if (obrtype(ptr2) .eq. 2 .or. 
     &                  obrtype(ptr2) .eq. 3 .or.
     &                  obrtype(ptr2) .eq. 5 .or.
     &                  obrtype(ptr2) .eq. 6 .or.
     &                  obrtype(ptr2) .eq. 7 .or.
     &                  obrtype(ptr2) .eq. 8) then
                      quan1 = 0.0
                      quan2 = orateln(1,nbr2) 
                    else
                      quan1 = 0.0
                      quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 14) then
                    if (obrtype(ptr2) .eq. 2 .or. 
     &                  obrtype(ptr2) .eq. 3 .or.
     &                  obrtype(ptr2) .eq. 5 .or.
     &                  obrtype(ptr2) .eq. 6 .or.
     &                  obrtype(ptr2) .eq. 7 .or.
     &                  obrtype(ptr2) .eq. 8) then
                      quan1 = 0.0
                      quan2 = orateln(2,nbr2) 
                    else
                      quan1 = 0.0
                      quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 15) then
                    if (obrtype(ptr2) .eq. 5 .or. 
     &                  obrtype(ptr2) .eq. 6) then
                      quan1 = 0.0
                      quan2 = orateln(3,nbr2) 
                    else
                      quan1 = 0.0
                      quan2 = 0.0
                    endif
                 else if (rpt_type .eq. 16) then
                    if (obrtype(ptr2) .eq. 5 .or. 
     &                  obrtype(ptr2) .eq. 6) then
                      quan1 = 0.0
                      quan2 = orateln(1,nbr2) + orateln(2,nbr2) 
     &                      + orateln(3,nbr2) + obrnch(4,nbr2) 
                    else
                      quan1 = 0.0
                      quan2 = orateln(1,nbr2) + orateln(2,nbr2) 
     &                      + obrnch(4,nbr2) 
                    endif
                 else if (rpt_type .eq. 17) then
                    if (obrtype(ptr2) .eq. 3) then
                      quan1 = 0.0
                      quan2 = obrnch(5,nbr2) + obrnch(6,nbr2)
     &                      + obrnch(7,nbr2) + obrnch(8,nbr2) 
                      quany = obrnch(9,nbr2)
                    else if (obrtype(ptr2) .eq. 5 .or. 
     &                       obrtype(ptr2) .eq. 6) then
                      quan1 = 0.0
                      quan2 = obrnch(5,nbr2) + obrnch(6,nbr2)
     &                      + obrnch(7,nbr2) + obrnch(8,nbr2) 
                      quany = obrnch(9,nbr2) + obrnch(10,nbr2)
                    else if (obrtype(ptr2) .eq. 8) then
                      quan1 = 0.0
                      quan2 = obrnch(5,nbr2) + obrnch(6,nbr2)
     &                      + obrnch(7,nbr2) + obrnch(8,nbr2) 
     &                      + obrnch(9,nbr2) + obrnch(10,nbr2)
                    else
                      quan1 = 0.0
                      quan2 = 0.0
                      quany = obrnch(5,nbr2) + obrnch(6,nbr2) 
     &                      + obrnch(7,nbr2) + obrnch(8,nbr2) 
     &                      + obrnch(9,nbr2) + obrnch(10,nbr2)
                    endif
                 else
                    quan1 = 0.0
                    quan2 = 0.0
                 endif

                 if (rpt_type .eq. 6 .or. rpt_type .eq. 7 .or.
     &               rpt_type .eq. 10 .or. rpt_type .eq. 11 .or.
     &               rpt_type .eq. 17) then
                    xmult = 10000.0
                 else
                    xmult = 100.0
                 endif
                 idv = int(xmult * (quan1 - quan2))
                 idy = int(100.0 * (quanx - quany))
                 if (iabs (idv) .gt. int(xmult * quan1diff) .or.
     &               iabs (idy) .gt. int(100.0 * quan2diff)) then
                    numdiff = numdiff + 1
                    kdiff(1,numdiff) = 0
                    kdiff(2,numdiff) = ptr2
                    fdiff(1,numdiff) = iabs(idv) + iabs(idy) 
                    fdiff(2,numdiff) = 0.0
                    fdiff(3,numdiff) = quan2
                    fdiff(4,numdiff) = 0.0
                    fdiff(5,numdiff) = 0.0
                    fdiff(6,numdiff) = idv
                    cdiff(1,numdiff) = ' '
                    cdiff(2,numdiff) = tag2
                 endif
              endif
c
c             Increment branches for m2
c
              found = .false.
              do while (ptr2 .gt. 0 .and. .not. found) 
                 lsw = incm2vdif (ptr2, nb2, m2, lsw)
                 if (obrtype(ptr2) .ne. 4) found = .true.
              enddo

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
           write (*, 10140)
10140      format (' * Enter sort option - 1 = Alphabetical', /,
     &             ' *                     2 = High to low' , /,
     &             ' > Enter option ', $)
           read (*, '(a)') text

           if (text(1:1) .eq. '2') then

              key = 2
              call qiksrt (1, numdiff, kmpvltdif, swpvltdif)

           endif

           if (rpt_type .lt. 16) then
              last = lastch (dict(rpt_type))
              write (header(1), 240) dict(rpt_type)(1:last), cspare(30),
     &           ocase
  240         format (' Line difference report ',  a, t44, a, t59, a)
              write (header(2), 242) dict(rpt_type)(1:last), 
     &                               dict(rpt_type)(1:last)
  242         format (' Ty Own Bus1         Bus2         IS  ', t44, a, 
     &             t59, a, t70, 'dif')
 
              last = lastch (unit_diff(3,rpt_type))
              write (header(3), 244) unit_diff(3,rpt_type)(1:last), 
     &                               unit_diff(3,rpt_type)(1:last), 
     &                               unit_diff(3,rpt_type)(1:last)
  244         format (t44, a, t59, a, t70, a)
           else if (rpt_type .eq. 16) then
              last = lastch (dict(rpt_type))
              write (header(1), 245) dict(rpt_type)(1:last), cspare(30),
     &           ocase
  245         format (' Line difference report ',  a, t44, a, t70, a)
              write (header(2), 246) 
  246         format (' Ty Own Bus1         Bus2         IS  ', t44, 
     &          'NOM   THRM  EMG   BOTL', t70, 'NOM   THRM  EMG   BOTL',
     &           t96, 'dif')
 
              last = lastch (unit_diff(3,rpt_type))
              write (header(3), 247) unit_diff(3,rpt_type)(1:last), 
     &                               unit_diff(3,rpt_type)(1:last), 
     &                               unit_diff(3,rpt_type)(1:last)
  247         format (t44, a, t70, a, t96, a)
           else
              last = lastch (dict(rpt_type))
              write (header(1), 248) dict(rpt_type)(1:last), cspare(30),
     &           ocase
  248         format (' Line difference report ',  a, t44, a, t70, a)
              last = lastch (unit_diff(3,rpt_type))
              write (header(2), 249) unit_diff(3,rpt_type)(1:last), 
     &                               unit_diff(3,rpt_type)(1:last)
  249         format (' Ty Own Bus1         Bus2         IS  ', 
     &            t44, a, t74, a, t96, 'dif')
 
              write (header(3), 10249)
10249         format (' *')
           endif
           do i = 1, 3
              last = lastch(header(i))
              if (scrfil .gt. 0) write (scrfil, '(a)') header(i)(1:last)
              length = apdoutbuf(o2, header(i)(1:last), out_buffer(o2:))
              o2 = o2 + length
           enddo
        endif

  250   ix = lastloop(1)
        iy = lastloop(2)
        if (ix .eq. 0) then
           ix = 1
           iy = 0
        endif
        loop(1) = ix
        loop(2) = iy

        finished = .false.
        do while (ix .le. numdiff .and. .not. finished)
           i = ixref(ix)
           ptr1 = kdiff(1,i)           
           ptr2 = kdiff(2,i)         
           quan1 = fdiff(2,i)
           quan2 = fdiff(3,i)
           tag1 = cdiff(1,i)
           tag2 = cdiff(2,i)
           if (ptr1 .gt. 0 .and. ptr2 .gt. 0) then
              relvlt = ' '
              k1 = kx(ptr1)
              k2 = ky(ptr1)
              id = brid(ptr1)
              sect = brsect(ptr1)
              type = brntyp(brtype(ptr1))
              nbr = iabs(brnch_ptr(ptr1))
              if (brtype(ptr1) .eq. 1) then
                 ptr1x = brnch_nxt(ptr1)
                 nbrx = iabs (brnch_ptr(ptr1x))
                 call getchr (3, own, kbrnch(3,nbrx))
              else
                 call getchr (3, own, kbrnch(3,nbr))
              endif
              bus_1c = bus(k1)
              bus_2c = bus(k2)
              base_1c = code (base(k1), 4, 0)
              base_2c = code (base(k2), 4, 0)
              if (rpt_type .eq. 16) then
                 nbr1 = iabs (brnch_ptr(ptr1))
                 irate0 = brnch(4,nbr1)
                 irate1 = rateln(1,nbr1)
                 irate2 = rateln(2,nbr1)
                 irate3 = rateln(3,nbr1)
                 write (text1, 251) irate0, irate1, irate2, irate3
  251            format (i4, 2x, i4, 2x, i4, 2x, i4)
                 nbr2 = iabs (obrnch_ptr(ptr2))
                 irate0 = obrnch(4,nbr2)
                 irate1 = orateln(1,nbr2)
                 irate2 = orateln(2,nbr2)
                 irate3 = orateln(3,nbr2)
                 write (text2, 251) irate0, irate1, irate2, irate3
              else if (rpt_type .eq. 17) then
                 nbr1 = iabs (brnch_ptr(ptr1))
                 if (brtype(ptr1) .eq. 3) then
                    write (subtxt(1), 10251) brnch(5,nbr1), 
     &                brnch(7,nbr1), brnch(9,nbr1)
10251               format (f8.5, f10.5, f10.1)
                    write (subtxt(2), 10252) brnch(6,nbr1), 
     &                brnch(8,nbr1)
10252               format (f8.5, f10.5)
                    isubhdr = 1
                 else if (brtype(ptr1) .eq. 8) then
                    write (subtxt(1), 10253) brnch(5,nbr1), 
     &                brnch(7,nbr1), brnch(9,nbr1)
10253               format (f8.5, f10.5, f10.5)
                    write (subtxt(2), 10253) brnch(6,nbr1), 
     &                brnch(8,nbr1), brnch(10,nbr1)
                    isubhdr = 2
                 else if (brtype(ptr1) .eq. 5 .or. brtype(ptr1) .eq. 6) 
     &              then
                    write (subtxt(1), 10254) brnch(5,nbr1), 
     &                brnch(7,nbr1), brnch(9,nbr1)
10254               format (f8.5, f10.5, f10.2)
                    write (subtxt(2), 10254) brnch(6,nbr1), 
     &                brnch(8,nbr1), brnch(10,nbr1)
                    isubhdr = 3
                 else
                    write (subtxt(1), 10255) brnch(5,nbr1), 
     &                brnch(7,nbr1), brnch(9,nbr1)
10255               format (f8.2, f10.2, f10.2)
                    write (subtxt(2), 10255) brnch(6,nbr1), 
     &                brnch(8,nbr1), brnch(10,nbr1)
                    isubhdr = 4
                 endif
                 nbr2 = iabs (obrnch_ptr(ptr2))
                 if (obrtype(ptr2) .eq. 3) then
                    write (subtxt(3), 10251) obrnch(5,nbr2), 
     &                obrnch(7,nbr2), obrnch(9,nbr2)
                    write (subtxt(4), 10252) obrnch(6,nbr2), 
     &                obrnch(8,nbr2)
                 else if (obrtype(ptr2) .eq. 8) then
                    write (subtxt(3), 10253) obrnch(5,nbr2), 
     &                obrnch(7,nbr2), obrnch(9,nbr2)
                    write (subtxt(4), 10253) obrnch(6,nbr2), 
     &                obrnch(8,nbr2), obrnch(10,nbr2)
                 else if (obrtype(ptr2) .eq. 5 .or. 
     &                    obrtype(ptr2) .eq. 6) then
                    write (subtxt(3), 10254) obrnch(5,nbr2), 
     &                obrnch(7,nbr2), obrnch(9,nbr2)
                    write (subtxt(4), 10254) obrnch(6,nbr2), 
     &                obrnch(8,nbr2), obrnch(10,nbr2)
                 else
                    write (subtxt(3), 10255) obrnch(5,nbr2), 
     &                obrnch(7,nbr2), obrnch(9,nbr2)
                    write (subtxt(4), 10255) obrnch(6,nbr2), 
     &                obrnch(8,nbr2), obrnch(10,nbr2)
                 endif
              endif
              lasttype = brtype(ptr1)
           else if (ptr1 .gt. 0) then
              relvlt = '(1)'
              k1 = kx(ptr1)
              k2 = ky(ptr1)
              id = brid(ptr1)
              sect = brsect(ptr1)
              type = brntyp(brtype(ptr1))
              nbr = iabs(brnch_ptr(ptr1))
              if (brtype(ptr1) .eq. 1) then
                 ptr1x = brnch_nxt(ptr1)
                 nbrx = iabs (brnch_ptr(ptr1x))
                 call getchr (3, own, kbrnch(3,nbrx))
              else
                 call getchr (3, own, kbrnch(3,nbr))
              endif
              bus_1c = bus(k1)
              bus_2c = bus(k2)
              base_1c = code (base(k1), 4, 0)
              base_2c = code (base(k2), 4, 0)
              if (rpt_type .eq. 16) then
                 nbr1 = iabs (brnch_ptr(ptr1))
                 irate0 = brnch(4,nbr1)
                 irate1 = rateln(1,nbr1)
                 irate2 = rateln(2,nbr1)
                 irate3 = rateln(3,nbr1)
                 write (text1, 251) irate0, irate1, irate2, irate3
                 text2 = ' '
              else if (rpt_type .eq. 17) then
                 nbr1 = iabs (brnch_ptr(ptr1))
                 if (brtype(ptr1) .eq. 3) then
                    write (subtxt(1), 10251) brnch(5,nbr1), 
     &                brnch(7,nbr1), brnch(9,nbr1)
                    write (subtxt(2), 10252) brnch(6,nbr1), 
     &                brnch(8,nbr1)
                    isubhdr = 1
                 else if (brtype(ptr1) .eq. 8) then
                    write (subtxt(1), 10253) brnch(5,nbr1), 
     &                brnch(7,nbr1), brnch(9,nbr1)
                    write (subtxt(2), 10253) brnch(6,nbr1), 
     &                brnch(8,nbr1), brnch(10,nbr1)
                    isubhdr = 2
                 else if (brtype(ptr1) .eq. 5 .or. brtype(ptr1) .eq. 6) 
     &              then
                    write (subtxt(1), 10254) brnch(5,nbr1), 
     &                brnch(7,nbr1), brnch(9,nbr1)
                    write (subtxt(2), 10254) brnch(6,nbr1), 
     &                brnch(8,nbr1), brnch(10,nbr1)
                    isubhdr = 3
                 else
                    write (subtxt(1), 10255) brnch(5,nbr1), 
     &                brnch(7,nbr1), brnch(9,nbr1)
                    write (subtxt(2), 10255) brnch(6,nbr1), 
     &                brnch(8,nbr1), brnch(10,nbr1)
                    isubhdr = 4
                 endif
                 subtxt(3) = ' '
                 subtxt(4) = ' '
              endif
              lasttype = brtype(ptr1)
           else 
              relvlt = '(2)'
              k1 = okx(ptr2)
              k2 = oky(ptr2)
              id = obrid(ptr2)
              sect = obrsect(ptr2)
              type = brntyp(obrtype(ptr2))
              nbr = iabs(obrnch_ptr(ptr2))
              if (obrtype(ptr2) .eq. 1) then
                 ptr2x = obrnch_nxt(ptr2)
                 nbrx = iabs (obrnch_ptr(ptr2x))
                 call getchr (3, own, obrnch(3,nbrx))
              else
                 call getchr (3, own, okbrnch(3,nbr))
              endif
              bus_1c = oldbus(k1)
              bus_2c = oldbus(k2)
              base_1c = code (oldbase(k1), 4, 0)
              base_2c = code (oldbase(k2), 4, 0)
              if (rpt_type .eq. 16) then
                 text1 = ' '
                 nbr2 = iabs (obrnch_ptr(ptr2))
                 irate0 = obrnch(4,nbr2)
                 irate1 = orateln(1,nbr2)
                 irate2 = orateln(2,nbr2)
                 irate3 = orateln(3,nbr2)
                 write (text2, 251) irate0, irate1, irate2, irate3
              else if (rpt_type .eq. 17) then
                 subtxt(1) = ' '
                 subtxt(2) = ' '
                 nbr2 = iabs (obrnch_ptr(ptr2))
                 if (obrtype(ptr2) .eq. 3) then
                    write (subtxt(3), 10251) obrnch(5,nbr2), 
     &                obrnch(7,nbr2), obrnch(9,nbr2)
                    write (subtxt(4), 10252) obrnch(6,nbr2), 
     &                obrnch(8,nbr2)
                    isubhdr = 1
                 else if (obrtype(ptr2) .eq. 8) then
                    write (subtxt(3), 10253) obrnch(5,nbr2), 
     &                obrnch(7,nbr2), obrnch(9,nbr2)
                    write (subtxt(4), 10253) obrnch(6,nbr2), 
     &                obrnch(8,nbr2), obrnch(10,nbr2)
                    isubhdr = 2
                 else if (obrtype(ptr2) .eq. 5 .or. 
     &                    obrtype(ptr2) .eq. 6) then
                    write (subtxt(3), 10254) obrnch(5,nbr2), 
     &                obrnch(7,nbr2), obrnch(9,nbr2)
                    write (subtxt(4), 10254) obrnch(6,nbr2), 
     &                obrnch(8,nbr2), obrnch(10,nbr2)
                    isubhdr = 3
                 else
                    write (subtxt(3), 10255) obrnch(5,nbr2), 
     &                obrnch(7,nbr2), obrnch(9,nbr2)
                    write (subtxt(4), 10255) obrnch(6,nbr2), 
     &                obrnch(8,nbr2), obrnch(10,nbr2)
                    isubhdr = 4
                 endif
              endif
              lasttype = obrtype(ptr2)
           endif
           if (rpt_type .eq. 8 .or. rpt_type .eq. 9 .or.
     &        (rpt_type .gt. 11 .and. rpt_type .lt. 16)) then
              write (text, 290) type, own, bus_1c, base_1c,
     &                          bus_2c, base_2c, id, sect,
     &                          quan1, quan2, quan1-quan2, relvlt
  290         format(1x, a2, 1x, a3, 1x, a8, a4, 1x, a8, a4, 1x, a1,
     &               i1, t43, f8.2, t57, f8.2, t66, f10.2, 1x, a3)
           else if (rpt_type .eq. 16) then
              write (text, 291) type, own, bus_1c, base_1c,
     &                          bus_2c, base_2c, id, sect,
     &                          text1, text2, quan1-quan2, relvlt
  291         format(1x, a2, 1x, a3, 1x, a8, a4, 1x, a8, a4, 1x, a1,
     &               i1, t44, a, t70, a, t96, f10.2, 1x, a3)
           else if (rpt_type .lt. 6) then 
              write (text, 292) type, own, bus_1c, base_1c,
     &                          bus_2c, base_2c, id, sect,
     &                          quan1, quan2, quan1-quan2, relvlt
  292         format(1x, a2, 1x, a3, 1x, a8, a4, 1x, a8, a4, 1x, a1,
     &               i1, t43, f8.1, t57, f8.1, t66, f10.1, 1x, a3)
           else if (rpt_type .ne. 17) then
              write (text, 294) type, own, bus_1c, base_1c,
     &                          bus_2c, base_2c, id, sect,
     &                          quan1, quan2, quan1-quan2, relvlt
  294         format(1x, a2, 1x, a3, 1x, a8, a4, 1x, a8, a4, 1x, a1,
     &               i1, t43, f8.5, t57, f8.5, t66, f10.5, 1x, a3)
           endif
           if (rpt_type .eq. 17) then
              if (loop(2) .eq. 0) then
                 if (o2 .lt. maxbuf_out) then
                    last = lastch(subhdr(isubhdr))
                    length = apdoutbuf(o2, subhdr(isubhdr)(1:last), 
     &                                 out_buffer(o2:))
                    o2 = o2 + length
                    loop(1) = ix
                    loop(2) = 1
                 else if (repeat) then
                    finished = .true.
                 endif
              endif
              if (scrfilx .gt. 0) then
                 last = lastch(subhdr(isubhdr))
                 write (scrfilx, '(a)') subhdr(isubhdr)(1:last)
              endif
              iy = 1
              write (text, 10291) type, own, bus_1c, base_1c,
     &                          bus_2c, base_2c, id, sect,
     &                          subtxt(1), subtxt(3), quan1-quan2, 
     &                          relvlt
10291         format(1x, a2, 1x, a3, 1x, a8, a4, 1x, a8, a4, 1x, a1,
     &               i1, t43, a, t73, a, t104, f10.2, 1x, a3)
              if (loop(2) .eq. 1) then
                 if (o2 .lt. maxbuf_out) then
                    last = lastch(text)
                    length = apdoutbuf(o2, text(1:last), 
     &                                 out_buffer(o2:))
                    o2 = o2 + length
                    loop(1) = ix
                    loop(2) = 2
                 else if (repeat) then
                    finished = .true.
                 endif
              endif
              if (scrfilx .gt. 0) then
                 last = lastch(text)
                 write (scrfilx, '(a)') text(1:last)
              endif
              iy = 2
              write (text, 10292) subtxt(2), subtxt(4)
10292         format(t43, a, t73, a)
              if (loop(2) .eq. 2) then
                 if (o2 .lt. maxbuf_out) then
                    last = lastch(text)
                    length = apdoutbuf(o2, text(1:last), 
     &                                 out_buffer(o2:))
                    o2 = o2 + length
                    loop(1) = ix
                    loop(2) = 0
                 else if (repeat) then
                    finished = .true.
                 endif
              endif
              if (scrfilx .gt. 0) then
                 last = lastch(text)
                 write (scrfilx, '(a)') text(1:last)
              endif
              iy = 3
           else
              if (o2 .lt. maxbuf_out) then
                 last = lastch(text)
                 length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = ix
                 loop(2) = 0
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) then
                 last = lastch(text)
                 write (scrfilx, '(a)') text(1:last)
              endif
           endif
           ix = ix + 1
           iy = 0
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
