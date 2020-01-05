C****************************************************************
C
C       File: zonintchg.f
C       Purpose: Routine to generate zone interchange analysis reports
C
C       Invoked by:
C              / REPORTS, SELECT ZONE_INT_DETAIL
C              / REPORTS, SELECT ZONE_INT_BRIEF
C                  WHERE AREAS = ...
C                (END)
C          
C       Author: Walt Powell  Date: 17 Feb 1995
C                            Modified: 17 Feb 1995
C       Called by:
C
C****************************************************************
	integer function zonintchg (in_buffer, out_buffer, scrfil) 

        character in_buffer * (*)
        character out_buffer * (*)
        integer scrfil

        include 'ipfinc/parametr.inc'
  
        include 'ipfinc/anlys.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/busanl.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/zonehash.inc'
        include 'ipfinc/ownhash.inc'
        include 'ipfinc/bsekvhsh.inc'
      	include 'ipfinc/zbo.inc'
      	include 'ipfinc/owntie.inc'

        common /basecase/ casename(2), filename(2)
        character casename * 10, filename * 60

        integer p, q, blankowner, apdoutbuf, o2, findstr, 
     &          ptr, scrfilx, loop(5), lastloop(5)
        character lown * 3, type * 2, null*1, linefeed*1, 
     &            brntyp * 2, flag1 * 1, flag2 * 1, own1 * 3, own2 * 3
        logical change_f, finished, 
     &          found, gtfltr, chkfltr, chkfltrz, repeat, detailed,
     &          found1, found2

        real subtot(8), totexp(8), last_subtot(8), last_totexp(8)

        save

        zonintchg = 0

        null = char(0)
        linefeed = char(10)
        linespage = 22

        change_f = .true.      ! Change filters

        out_buffer(1:1) = null
        o2 = index (out_buffer, null)
        maxbuf_in = len( in_buffer )
        maxbuf_out = len( out_buffer ) - 400
        last = index( in_buffer, null )
        if ( last .eq. 0 ) last = maxbuf_in
        ibuf_ful = 0
c
c       Check for re-entry and continue
c
        i = last
        if ( i .gt. 50 ) i = 50
        if (findstr (in_buffer(1:i), 'CONTINUE') .ne. 0) then
           repeat = .true.
           scrfilx = 0
           do i = 1, 5
              lastloop(i) = loop(i)
           enddo
           do i = 1, 8
              last_totexp(i) = totexp(i)
              last_subtot(i) = subtot(i)
           enddo
           if (detailed) go to 532
           go to 432
        endif

        do i = 1, 5
           lastloop(i) = 0
        enddo
        do i = 1, 8
           last_totexp(i) = 0.0
           last_subtot(i) = 0.0
        enddo
        scrfilx = scrfil
c
c       Search for type of report
c
        detailed = (findstr(in_buffer(1:last), 
     &                'ZONE_INT_DETAIL') .gt. 0) 
c
c       Search and align to "WHERE" ...
c
        ix = findstr(in_buffer, 'WHERE') ! findstr is a case-insensitive
c                                        ! version of index
        if (ix .gt. 0) then
           ix = ix + len('WHERE')
           change_f = gtfltr(in_buffer(ix:))
        else
           do i = 1, 7
              filter(i) = 0
           enddo
        endif
C                                                                      *
C       Brief zone interchange summary
C                                                                      *
        if (detailed) go to 500

c       Set up the page header

        if (scrfilx. gt. 0) then
           outbuf = 'Zone Interchange'
           call rpnlod
    
           write (outbuf, 100) chase1(1), chase1(34), chase1(35)
  100      format('Case: ', a10, ' Project: ', 2a10)
           call hedlod

        endif

        write (outbuf, 410)
  410   format (t2, 'ZN1', t7, '->', t11, 'ZN2', t26,
     &'/------------------------- EXPORT (+) -------------------------/'
     &)
        length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
        o2 = o2 + length
        if (scrfilx .gt. 0) call shdlod(1)
        write (outbuf(1:132), 420)
  420   format (t26, '/---- FLOW ----//---- LOAD ----//--- SHUNT ----//-
     & GENERATION -/')
        length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
        o2 = o2 + length
        if (scrfilx .gt. 0) call shdlod(2)
        write (outbuf, 430)
  430   format (t26, '      MW    MVAR      MW    MVAR      MW    MVAR  
     &    MW    MVAR')
        length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
        o2 = o2 + length
        outbuf = ' '
        length = apdoutbuf(o2, outbuf(1:1), out_buffer(o2:))
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

  432   continue
C
C       Temporarily overwrite any blank ownership with '(.)'. 
C       Restore at end of run.
C
        blankowner = 0
        i = 1
        do while (i .le. numown .and. blankowner .eq. 0)
           if (owner_o(i) .eq. ' ') then 
              owner_o(i) = '(.)'
              blankowner = i
           else
              i = i + 1
           endif
        enddo 

        do i = 1, 5
           loop(i) = 0
        enddo

        iz = lastloop(1)
        if (iz .eq. 0) iz = 1

        finished = .false.
        do while (iz .le. nztot .and. .not. finished)
           found = chkfltrz (acznam(iz))
           if (.not. found) go to 496
           if (lastloop(3) .eq. 0) then
              do i = 1, 8
                 totexp(i) = 0.0
              enddo
              p = own1_inx(iz)
           else
              do i = 1, 8
                 totexp(i) = last_totexp(i)
              enddo
              p = own1_nxt(lastloop(3))
              lastloop(3) = 0
           endif
           do while (p .gt. 0 .and. .not. finished)
              jo = own1_own2(p)
C                                                                      *
C             Find total ZONE1 - ZONE2 interchange
C                                                                      *
              do i = 1, 8
                 subtot(i) = 0.0
              enddo
              q = own1_ref(p)
              do while (q .ne. 0)
                 i = own1_tie_ptr(q)
                 ix = iabs (i)
                 ptr = lowntie(5,ix)
                 nbr = iabs (brnch_ptr(ptr))
                 call getchr (3, lown, kbrnch(3,nbr)) 
                 type = brntyp(brtype(ptr))
                 k1 = kx(ptr)
                 k2 = ky(ptr)
                 if ((i .gt. 0 .and. lowntie(6,ix) .eq. 1) .or.
     &               (i .lt. 0 .and. lowntie(6,ix) .eq. 2)) then
                    own1 = owner(k1)     
                    own2 = lown
                 else
                    own1 = lown
                    own2 = owner(k1)     
                 endif
                 found1 = chkfltr (arcnam(jarzn(k1)), zone(k1), own1, 
     &                             base(k1), type, k1)
                 found2 = chkfltr (arcnam(jarzn(k2)), zone(k2), own2, 
     &                             base(k2), type, k2)
                 found = (found1 .or. found2)
                 if (found) then
                    if (i .lt. 0) then
                       subtot(1) = subtot(1) - owntie(1,ix)
                       subtot(2) = subtot(2) - owntie(2,ix)
                    else
                       subtot(1) = subtot(1) + owntie(1,ix)
                       subtot(2) = subtot(2) + owntie(2,ix)
                    endif
                 endif
                 q = own1_tie_nxt(q)
              enddo
              write (outbuf, 450) acznam(iz), acznam(jo), subtot
  450         format (t2, a3, t7, '->', t11, a3, t16, 'SUBTOTAL', t26, 
     &                8f8.1)

              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = iz
                 loop(2) = 0
                 loop(3) = p
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
              do i = 1, 8
                 totexp(i) = totexp(i) + subtot(i)
              enddo
  459         p = own1_nxt(p)
           enddo

           if (scrfilx .gt. 0) call prtout(1)
           outbuf = ' '
           if (o2 .lt. maxbuf_out) then
              length = apdoutbuf(o2, outbuf(1:1), out_buffer(o2:))
              o2 = o2 + length
              loop(1) = iz
              loop(2) = 2
              loop(3) = 0
           else if (repeat) then
              finished = .true.
           endif
           if (scrfilx .gt. 0) call prtout(1)
           if (lastloop(2) .lt. 1) then
              write (outbuf, 460) acznam(iz)
  460         format (t2, a3, t7, 'ZONE TOTAL', t26, '    MW    MVAR')
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = iz
                 loop(2) = 1
                 loop(3) = 0
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
           endif
           if (lastloop(2) .lt. 2) then
              write (outbuf, 470) zsum(1,iz), zsum(2,iz)
  470         format (t7, 'GENERATION', t26, 2f8.1)
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = iz
                 loop(2) = 2
                 loop(3) = 0
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
           endif
           if (lastloop(2) .lt. 3) then
              write (outbuf, 480) zsum(3,iz), zsum(4,iz)
  480         format (t7, 'LOAD', t26, 2f8.1)
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = iz
                 loop(2) = 3
                 loop(3) = 0
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
           endif
           if (lastloop(2) .lt. 4) then
              write (outbuf, 490) zsum(5,iz), zsum(6,iz)
  490         format (t7, 'LOSSES', t26, 2f8.1)
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = iz
                 loop(2) = 4
                 loop(3) = 0
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
           endif
           if (lastloop(2) .lt. 5) then
              write (outbuf, 492) zsum(7,iz), zsum(8,iz)
  492         format (t7, 'SHUNT', t26, 2f8.1)
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = iz
                 loop(2) = 5
                 loop(3) = 0
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
           endif
           if (lastloop(2) .lt. 6) then
              ptot = zsum(1,iz) - zsum(3,iz) - zsum(5,iz) - zsum(7,iz)
              qtot = zsum(2,iz) - zsum(4,iz) - zsum(6,iz) + zsum(8,iz)
              write (outbuf, 494) ptot, qtot
  494         format (t7, 'EXPORT', t26, 2f8.1)
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = iz
                 loop(2) = 6
                 loop(3) = 0
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
           endif
           if (lastloop(2) .lt. 7) then
              write (outbuf, 495) totexp(1), totexp(2)
  495         format (t7, 'FILTERED EXPORT', t26, 2f8.1)
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = iz
                 loop(2) = 7
                 loop(3) = 0
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
           endif
           outbuf = ' '
           if (o2 .lt. maxbuf_out) then
              length = apdoutbuf(o2, outbuf(1:1), out_buffer(o2:))
              o2 = o2 + length
              loop(1) = iz
              loop(2) = 2
              loop(3) = 0
           else if (repeat) then
              finished = .true.
           endif
           if (scrfilx .gt. 0) call prtout(1)
  496      iz = iz + 1
           lastloop(1) = 0
           lastloop(2) = 0
           lastloop(3) = 0
        enddo
        go to 920
C                                                                      *
C       Detailed Zone Interchange Listing

c       Set up the page header

  500   if (scrfilx. gt. 0) then
           outbuf = 'Zone Interchange'
           call rpnlod
    
           write (outbuf, 100) chase1(1), chase1(34), chase1(35)
           call hedlod

        endif

        write (outbuf, 510)
  510   format (t2, 'ZN1', t7, '->', t11, 'ZN2', t16,
     &          '/--------------- ZONE INTERCHANGE ------------------//-
     &-------------------------- EXPORT (+) -----------------------/')
        length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
        o2 = o2 + length
        if (scrfilx .gt. 0) call shdlod(1)
        write (outbuf, 520)
  520   format (t69, '/---- FLOW ----//---- LOAD ----//--- SHUNT ----//-
     & GENERATION -/')
        length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
        o2 = o2 + length
        if (scrfilx .gt. 0) call shdlod(2)
        write (outbuf, 530)
  530   format (t69, '     MW    MVAR     MW    MVAR       MW    MVAR   
     &   MW    MVAR')
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

  532   continue
C
C       Temporarily overwrite any blank ownership with '(.)'. 
C       Restore at end of run.
C
        blankowner = 0
        i = 1
        do while (i .le. numown .and. blankowner .eq. 0)
           if (owner_o(i) .eq. ' ') then 
              owner_o(i) = '(.)'
              blankowner = i
           else
              i = i + 1
           endif
        enddo 
C                                                                      *
C       Detailed Zone interchange
C                                                                      *
        iz = lastloop(1)
        if (iz .eq. 0) iz = 1

        finished = .false.
        do while (iz .le. nztot .and. .not. finished)
           found = chkfltrz (acznam(iz))
           if (.not. found) go to 650

           if (lastloop(3) .eq. 0) then
              do i = 1, 8
                 totexp(i) = 0.0
              enddo
              p = own1_inx(iz)
           else if (lastloop(5) .eq. 0) then
              do i = 1, 8
                 totexp(i) = last_totexp(i)
              enddo
              p = own1_nxt(lastloop(3))
           else
              p = lastloop(3)
              lastloop(3) = 0
           endif
           do while (p .gt. 0 .and. .not. finished)
              jo = own1_own2(p)
C                                                                      *
C             Find total ZONE1 - ZONE2 interchange
C                                                                      *
              if (lastloop(5) .eq. 0) then
                 do i = 1, 8
                    subtot(i) = 0.0
                 enddo
              else
                 do i = 1, 8
                    subtot(i) = last_subtot(i)
                 enddo
              endif
              if (lastloop(5) .eq. 0) then
                 q = own1_ref(p)
              else
                 q = own1_tie_nxt(lastloop(5))
              endif
              do while (q .ne. 0)
                 i = own1_tie_ptr(q)
                 ix = iabs (i)
                 ptr = lowntie(5,ix)
                 nbr = iabs (brnch_ptr(ptr))
                 call getchr (3, lown, kbrnch(3,nbr)) 
                 type = brntyp(brtype(ptr))
                 k1 = kx(ptr)
                 k2 = ky(ptr)
                 if ((i .gt. 0 .and. lowntie(6,ix) .eq. 1) .or.
     &               (i .lt. 0 .and. lowntie(6,ix) .eq. 2)) then
                    own1 = owner(k1)     
                    own2 = lown
                 else
                    own1 = lown
                    own2 = owner(k1)     
                 endif
                 found1 = chkfltr (arcnam(jarzn(k1)), zone(k1), own1, 
     &                             base(k1), type, k1)
                 found2 = chkfltr (arcnam(jarzn(k2)), zone(k2), own2, 
     &                             base(k2), type, k2)
                 found = (found1 .or. found2)
                 if (found) then
                    if (i .lt. 0) then
                       subtot(1) = subtot(1) - owntie(1,ix)
                       subtot(2) = subtot(2) - owntie(2,ix)
                    else
                       subtot(1) = subtot(1) + owntie(1,ix)
                       subtot(2) = subtot(2) + owntie(2,ix)
                    endif
                 endif
                 q = own1_tie_nxt(q)
              enddo
              outbuf = ' '
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:1), out_buffer(o2:))
                 o2 = o2 + length
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
              write (outbuf, 540) acznam(iz), acznam(jo), subtot
  540         format (t2, a3, t7, '->', t11, a3, t16, 
     &                'SUBTOTAL (of following list)', t69, 8f8.1)
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = iz
                 loop(2) = 0
                 loop(3) = p
                 loop(4) = 0
                 loop(5) = 0
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
              outbuf = ' '
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:1), out_buffer(o2:))
                 o2 = o2 + length
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)

              do i = 1, 8
                 totexp(i) = totexp(i) + subtot(i)
              enddo
c
c             Now repeat with detailed summary
c
              if (lastloop(5) .eq. 0) then
                 q = own1_ref(p)
              else
                 q = own1_tie_nxt(lastloop(5))
                 lastloop(5) = 0
              endif
              do while (q .ne. 0 .and. .not. finished)
                 i = own1_tie_ptr(q)
                 ix = iabs (i)
                 ptr = lowntie(5,ix)
                 nbr = iabs (brnch_ptr(ptr))
                 call getchr (3, lown, kbrnch(3,nbr)) 
                 type = brntyp(brtype(ptr))
                 k1 = kx(ptr)
                 k2 = ky(ptr)
                 if ((i .gt. 0 .and. lowntie(6,ix) .eq. 1) .or.
     &               (i .lt. 0 .and. lowntie(6,ix) .eq. 2)) then
                    own1 = owner(k1)     
                    own2 = lown
                 else
                    own1 = lown
                    own2 = owner(k1)     
                 endif
                 found1 = chkfltr (arcnam(jarzn(k1)), zone(k1), own1, 
     &                             base(k1), type, k1)
                 found2 = chkfltr (arcnam(jarzn(k2)), zone(k2), own2, 
     &                             base(k2), type, k2)
                 found = (found1 .or. found2)
                 if (found) then
                    if (base(kx(ptr)) .eq. base(ky(ptr))) then
                       type = 'L'
                    else
                       type = 'T'
                    endif
                    if (i .lt. 0) then
                       if (lowntie(6,ix) .eq. 1) then
                          flag1 = ' '
                          flag2 = '*'
                       else
                          flag1 = '*'
                          flag2 = ' '
                       endif
                       write (outbuf, 570) type(1:1), 
     &                    zone(ky(ptr)), lown, flag1, 
     &                    bus(ky(ptr)), base(ky(ptr)), brid(ptr), 
     &                    zone(kx(ptr)), owner(kx(ptr)), flag2,
     &                    bus(kx(ptr)), base(kx(ptr)), 
     &                    -owntie(1,ix), -owntie(2,ix)
  570                  format (t8, a, 2x, a2, 1x, a3, 1x, a1, a8, 
     &                         f6.1, 1x, a1, t38, ' -> B ', 1x, a2, 
     &                         1x, a3, 1x, a1, a8, f6.1, t69, 2f8.1)
                    else
                       if (lowntie(6,ix) .eq. 1) then
                          flag1 = '*'
                          flag2 = ' '
                       else
                          flag1 = ' '
                          flag2 = '*'
                       endif
                       write (outbuf, 580) zone(kx(ptr)), 
     &                    owner(kx(ptr)), flag1, bus(kx(ptr)), 
     &                    base(kx(ptr)), type(1:1), 
     &                    zone(ky(ptr)), lown, flag2,
     &                    bus(ky(ptr)), 
     &                    base(ky(ptr)), brid(ptr), 
     &                    owntie(1,ix), owntie(2,ix)
  580                  format (t8,  'B ', 1x, a2, 1x, a3, 1x, a1, a8,
     &                         f6.1, t38, ' ->', 1x, a1, 2x, a2, 1x,
     &                         a3, 1x, a1, a8, 
     &                         f6.1, 1x, a1, t69, 2f8.1)
                    endif
                 endif
                 if (found) then
                    if (o2 .lt. maxbuf_out) then
                       length = apdoutbuf(o2, outbuf(1:132), 
     &                                    out_buffer(o2:))
                       o2 = o2 + length
                       loop(1) = iz
                       loop(2) = 0
                       loop(3) = p
                       loop(4) = 0
                       loop(5) = q
                    else if (repeat) then
                       finished = .true.
                    endif
                    if (scrfilx .gt. 0) call prtout(1)
                 endif
                 q = own1_tie_nxt(q)
              enddo
              p = own1_nxt(p)
           enddo
           outbuf = ' '
           if (o2 .lt. maxbuf_out) then
              length = apdoutbuf(o2, outbuf(1:1), out_buffer(o2:))
              o2 = o2 + length
              loop(1) = iz
              loop(2) = 1
              loop(3) = 0
              loop(4) = 0
              loop(5) = 0
           else if (repeat) then
              finished = .true.
           endif
           if (scrfilx .gt. 0) call prtout(1)
           outbuf = ' '
           if (o2 .lt. maxbuf_out) then
              length = apdoutbuf(o2, outbuf(1:1), out_buffer(o2:))
              o2 = o2 + length
              loop(1) = iz
              loop(2) = 2
              loop(3) = 0
              loop(4) = 0
              loop(5) = 0
           else if (repeat) then
              finished = .true.
           endif
           if (scrfilx .gt. 0) call prtout(1)
           if (lastloop(2) .lt. 1) then
              write (outbuf, 590) acznam(iz)
  590         format (t2, a3, t16, 'ZONE TOTAL', t69, '    MW    MVAR')
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = iz
                 loop(2) = 1
                 loop(3) = 0
                 loop(4) = 0
                 loop(5) = 0
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
           endif
           if (lastloop(2) .lt. 2) then
              write (outbuf, 600) zsum(1,iz), zsum(2,iz)
  600         format (t16, 'GENERATION', t69, 2f8.1)
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = iz
                 loop(2) = 2
                 loop(3) = 0
                 loop(4) = 0
                 loop(5) = 0
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
           endif
           if (lastloop(2) .lt. 3) then
              write (outbuf, 610) zsum(3,iz), zsum(4,iz)
  610         format (t16, 'LOAD', t69, 2f8.1)
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = iz
                 loop(2) = 3
                 loop(3) = 0
                 loop(4) = 0
                 loop(5) = 0
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
           endif
           if (lastloop(2) .lt. 4) then
              write (outbuf, 620) zsum(5,iz), zsum(6,iz)
  620         format (t16, 'LOSSES', t69, 2f8.1)
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = iz
                 loop(2) = 4
                 loop(3) = 0
                 loop(4) = 0
                 loop(5) = 0
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
           endif
           if (lastloop(2) .lt. 5) then
              write (outbuf, 630) zsum(7,iz), zsum(8,iz)
  630         format (t16, 'SHUNT', t69, 2f8.1)
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = iz
                 loop(2) = 5
                 loop(3) = 0
                 loop(4) = 0
                 loop(5) = 0
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
           endif
           if (lastloop(2) .lt. 6) then
              ptot = zsum(1,iz) - zsum(3,iz) - zsum(5,iz) - zsum(7,iz)
              qtot = zsum(2,iz) - zsum(4,iz) - zsum(6,iz) + zsum(8,iz)
              write (outbuf, 640) ptot, qtot
  640         format (t16, 'EXPORT', t69, 2f8.1)
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = iz
                 loop(2) = 6
                 loop(3) = 0
                 loop(4) = 0
                 loop(5) = 0
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
           endif
           if (lastloop(2) .lt. 7) then
              write (outbuf, 642) totexp(1), totexp(2)
  642         format (t16, 'FILTERED EXPORT', t69, 2f8.1)
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = iz
                 loop(2) = 7
                 loop(3) = 0
                 loop(4) = 0
                 loop(5) = 0
              else if (repeat) then
                 finished = .true.
              endif
              if (scrfilx .gt. 0) call prtout(1)
           endif
           outbuf = ' '
           if (o2 .lt. maxbuf_out) then
              length = apdoutbuf(o2, outbuf(1:1), out_buffer(o2:))
              o2 = o2 + length
              loop(1) = iz
              loop(2) = 0
              loop(3) = 0
              loop(4) = 0
              loop(5) = 0
           else if (repeat) then
              finished = .true.
           endif
           if (scrfilx .gt. 0) call prtout(1)
  650      iz = iz + 1
           lastloop(1) = 0
           lastloop(2) = 0
           lastloop(3) = 0
           lastloop(4) = 0
           lastloop(5) = 0
        enddo

c*** remember maxbuf_out is really 400 less than the real buffer size
        if (o2 .gt. maxbuf_out) then
           length = apdoutbuf(o2, '*[MORE]' , out_buffer(o2:))
           o2 = o2 + length
        endif

  920   continue
C
C       Restore any blank ownership overwritten with '(.)'
C
        if (blankowner .gt. 0) owner_o(blankowner) = ' '

        return
        end
