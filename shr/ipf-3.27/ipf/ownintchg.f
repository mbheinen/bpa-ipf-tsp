C****************************************************************
C
C       File: ownintchg.f
C       Purpose: Routine to generate ownership analysis reports
C
C       Invoked by:
C              / REPORTS, SELECT OWNER_INT_DETAIL
C              / REPORTS, SELECT OWNER_INT_BRIEF
C                  WHERE AREAS = ...
C                (END)
C          
C       Author: Walt Powell  Date: 17 Feb 1995
C                            Modified: 17 Feb 1995
C       Called by:
C
C****************************************************************
	integer function ownintchg (in_buffer, out_buffer, scrfil) 

        character in_buffer * (*)
        character out_buffer * (*)
        integer scrfil

        include 'ipfinc/parametr.inc'
  
        include 'ipfinc/anlys.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/prt.inc'
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
     &          ptr, scrfilx, loop(5), lastloop(5), ptr1
        character lown * 3, cbtype * 1, cbown * 3, cbyear * 2,
     &            type * 2, type2 * 2, null*1, linefeed*1, 
     &            brntyp * 2, flag1 * 1, flag2 * 1, type1 * 2
        logical change_f, finished,
     &          found, gtfltr, chkfltr, chkfltro, repeat, detailed,
     &          found1, found2

        real subtot(8), totexp(8), last_subtot(8), last_totexp(8)

        save

        ownintchg = 0

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
     &                'OWNER_INT_DETAIL') .gt. 0) 
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
C       Brief ownership interchange summary
C                                                                      *
        if (detailed) go to 500

c       Set up the page header

        if (scrfilx. gt. 0) then
           outbuf = 'Ownership Interchange'
           call rpnlod
    
           write (outbuf, 100) chase1(1), chase1(34), chase1(35)
  100      format('Case: ', a10, ' Project: ', 2a10)
           call hedlod

        endif

        write (outbuf, 410)
  410   format (t2, 'OWN1', t7, '->', t11, 'OWN2', t26,
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

        io = lastloop(1)
        if (io .eq. 0) io = 1

        finished = .false.
        do while (io .le. numown .and. .not. finished)
           found = chkfltro (owner_o(alf2own(io)))
           if (.not. found) go to 461
           if (lastloop(3) .eq. 0) then
              do i = 1, 8
                 totexp(i) = 0.0
              enddo
              p = own1_inx(io)
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
C             Find total OWN1 - OWN2 interchange
C                                                                      *
              do i = 1, 8
                 subtot(i) = 0.0
              enddo
              q = own1_ref(p)
              do while (q .ne. 0)
                 i = own1_tie_ptr(q)
                 ix = iabs (i)
                 if (lowntie(1,ix) .eq. 1) then
                    ncb = lowntie(5,ix)
                    call getchr (1, cbtype, kbctbl(8,ncb))
                    call getchr (3, cbown, kbctbl(10,ncb))
                    call getchr (2, cbyear, kbctbl(9,ncb))
                    nb = kbctbl(1,ncb)
                    type = '+' // cbtype
                    found = chkfltr (arcnam(jarzn(nb)), zone(nb), '***',
     &                               base(nb), type, nb)
                    if (found) then
                       kt = inp2opt(nb)
                       vk = sqrt (e(kt) ** 2 + f(kt) ** 2)
                       pload2 = bctbl(2, ncb)
                       qload2 = bctbl(3, ncb)
                       skcon2 = bctbl(4, ncb) * vk ** 2
                       sksus2 = bctbl(5, ncb) * vk ** 2
                       pgen2 = bctbl(6, ncb)
                       qgen2 = bctbl(11, ncb)

C                      Convert constant current and constant admittance 
c                      loads back to constant MVA.

                       if (cbtype .eq. 'A') then
                          if (cbyear .eq. '01' .or. cbyear .eq. '*I') 
     &                       then
                             pload2 = pload2 * vk + skcon2
                             qload2 = qload2 * vk - sksus2
                             skcon2 = 0.0
                             sksus2 = 0.0
                          else if (cbyear .ne. '02' .and. 
     &                             cbyear .ne. '*P') then
                          else
                             pload2 = pload2 + skcon2
                             qload2 = qload2 - sksus2
                             skcon2 = 0.0
                             sksus2 = 0.0
                          endif
                       else if (cbyear .eq. '*I') then
                          pload2 = pload2 * vk + skcon2
                          qload2 = qload2 * vk - sksus2
                          skcon2 = 0.0
                          sksus2 = 0.0
                       else if (cbyear .eq. '*P') then
                          pload2 = pload2 + skcon2
                          qload2 = qload2 - sksus2
                          skcon2 = 0.0
                          sksus2 = 0.0
                       endif
                       if (i .lt. 0) then
                          subtot(3) = subtot(3) - pload2
                          subtot(4) = subtot(4) - qload2
                          subtot(5) = subtot(5) - skcon2
                          subtot(6) = subtot(6) - sksus2
                          subtot(7) = subtot(7) + pgen2
                          subtot(8) = subtot(8) + qgen2
                       else
                          subtot(3) = subtot(3) + pload2
                          subtot(4) = subtot(4) + qload2
                          subtot(5) = subtot(5) + skcon2
                          subtot(6) = subtot(6) + sksus2
                          subtot(7) = subtot(7) - pgen2
                          subtot(8) = subtot(8) - qgen2
                       endif
                    endif
                 else if (lowntie(1,ix) .eq. 2) then
                    ptr = lowntie(5,ix)
                    nbr = iabs (brnch_ptr(ptr))
                    call getchr (3, lown, kbrnch(3,nbr)) 
                    type = brntyp(brtype(ptr))
                    k1 = kx(ptr)
                    k2 = ky(ptr)
                    found1 = chkfltr (arcnam(jarzn(k1)), zone(k1), 
     &                                '***', base(k1), type, k1)
                    found2 = chkfltr (arcnam(jarzn(k2)), zone(k2), 
     &                                '***', base(k2), type, k2)
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
                 else
                    ptr = lowntie(4,ix)
                    nbr = iabs (brnch_ptr(ptr))
                    call getchr (3, lown, kbrnch(3,nbr)) 
                    type = brntyp(brtype(ptr))
                    k1 = kx(ptr)
                    found1 = chkfltr (arcnam(jarzn(k1)), zone(k1), 
     &                                '***', base(k1), type, k1)
                    ptr = lowntie(5,ix)
                    nbr = iabs (brnch_ptr(ptr))
                    call getchr (3, lown, kbrnch(3,nbr)) 
                    type = brntyp(brtype(ptr))
                    k2 = ky(ptr)
                    found2 = chkfltr (arcnam(jarzn(k2)), zone(k2), 
     &                                '***', base(k2), type, k2)
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
                 endif
                 q = own1_tie_nxt(q)
              enddo
              write (outbuf, 450) owner_o(alf2own(io)), 
     &                          owner_o(alf2own(jo)), subtot
  450         format (t2, a3, t7, '->', t11, a3, t16, 'SUBTOTAL', t26, 
     &                8f8.1)

              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = io
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
           write (outbuf, 460) owner_o(alf2own(io)), totexp
  460      format (t2, a3, t16, 'TOTAL', t26, 8f8.1)
           if (o2 .lt. maxbuf_out) then
              length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
              o2 = o2 + length
              loop(1) = io
              loop(2) = 1
              loop(3) = 0
           else if (repeat) then
              finished = .true.
           endif
           if (scrfilx .gt. 0) call prtout(1)
           outbuf = ' '
           if (o2 .lt. maxbuf_out) then
              length = apdoutbuf(o2, outbuf(1:1), out_buffer(o2:))
              o2 = o2 + length
              loop(1) = io
              loop(2) = 2
              loop(3) = 0
           else if (repeat) then
              finished = .true.
           endif
           if (scrfilx .gt. 0) call prtout(1)
           if (o2 .lt. maxbuf_out) then
              loop(1) = io
              loop(2) = 3
              loop(3) = 0
           else if (repeat) then
              finished = .true.
           endif
  461      io = io + 1
        enddo
        go to 920
C                                                                      *
C       Detailed Ownership Interchange Listing
C                                                                      *
c       Set up the page header

c       Set up the page header

  500   if (scrfilx. gt. 0) then
           outbuf = 'Ownership Interchange'
           call rpnlod
    
           write (outbuf, 100) chase1(1), chase1(34), chase1(35)
           call hedlod

        endif

        write (outbuf, 510)
  510   format (t2, 'OWN1', t7, '->', t11, 'OWN2', t16,
     &          '/-------------- OWNER INTERCHANGE ------------------//-
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
C       Detailed ownership interchange
C                                                                      *
        io = lastloop(1)
        if (io .eq. 0) io = 1

        finished = .false.
        do while (io .le. numown .and. .not. finished)
           found = chkfltro (owner_o(alf2own(io)))
           if (.not. found) go to 890

           if (lastloop(3) .eq. 0) then
              do i = 1, 8
                 totexp(i) = 0.0
              enddo
              p = own1_inx(io)
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
C             Find total OWN1 - OWN2 interchange
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
                 if (lowntie(1,ix) .eq. 1) then
                    ncb = lowntie(5,ix)
                    call getchr (1, cbtype, kbctbl(8,ncb))
                    call getchr (3, cbown, kbctbl(10,ncb))
                    call getchr (2, cbyear, kbctbl(9,ncb))
                    nb = kbctbl(1,ncb)
                    kt = inp2opt(nb)
                    vk = sqrt (e(kt) ** 2 + f(kt) ** 2)
                    type = '+' // cbtype
                    found = chkfltr (arcnam(jarzn(nb)), zone(nb), '***',
     &                               base(nb), type, nb)
                    if (found) then
                       pload2 = bctbl(2, ncb)
                       qload2 = bctbl(3, ncb)
                       skcon2 = bctbl(4, ncb) * vk ** 2
                       sksus2 = bctbl(5, ncb) * vk ** 2
                       pgen2 = bctbl(6, ncb)
                       qgen2 = bctbl(11, ncb)

C                      Convert constant current and constant admittance 
c                      loads back to constant MVA.

                       if (cbtype .eq. 'A') then
                          if (cbyear .eq. '01' .or. cbyear .eq. '*I') 
     &                       then
                             pload2 = pload2 * vk + skcon2
                             qload2 = qload2 * vk - sksus2
                             skcon2 = 0.0
                             sksus2 = 0.0
                          else if (cbyear .ne. '02' .and. 
     &                             cbyear .ne. '*P') then
                          else
                             pload2 = pload2 + skcon2
                             qload2 = qload2 - sksus2
                             skcon2 = 0.0
                             sksus2 = 0.0
                          endif
                       else if (cbyear .eq. '*I') then
                          pload2 = pload2 * vk + skcon2
                          qload2 = qload2 * vk - sksus2
                          skcon2 = 0.0
                          sksus2 = 0.0
                       else if (cbyear .eq. '*P') then
                          pload2 = pload2 + skcon2
                          qload2 = qload2 - sksus2
                          skcon2 = 0.0
                          sksus2 = 0.0
                       endif
                       if (i .lt. 0) then
                          subtot(3) = subtot(3) - pload2
                          subtot(4) = subtot(4) - qload2
                          subtot(5) = subtot(5) - skcon2
                          subtot(6) = subtot(6) - sksus2
                          subtot(7) = subtot(7) + pgen2
                          subtot(8) = subtot(8) + qgen2
                       else
                          subtot(3) = subtot(3) + pload2
                          subtot(4) = subtot(4) + qload2
                          subtot(5) = subtot(5) + skcon2
                          subtot(6) = subtot(6) + sksus2
                          subtot(7) = subtot(7) - pgen2
                          subtot(8) = subtot(8) - qgen2
                       endif
                    endif
                 else if (lowntie(1,ix) .eq. 2) then
                    ptr = lowntie(5,ix)
                    nbr = iabs (brnch_ptr(ptr))
                    call getchr (3, lown, kbrnch(3,nbr)) 
                    type = brntyp(brtype(ptr))
                    k1 = kx(ptr)
                    k2 = ky(ptr)
                    found1 = chkfltr (arcnam(jarzn(k1)), zone(k1), 
     &                                '***', base(k1), type, k1)
                    found2 = chkfltr (arcnam(jarzn(k2)), zone(k2), 
     &                                '***', base(k2), type, k2)
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
                 else
                    ptr = lowntie(4,ix)
                    nbr = iabs (brnch_ptr(ptr))
                    call getchr (3, lown, kbrnch(3,nbr)) 
                    type = brntyp(brtype(ptr))
                    k1 = kx(ptr)
                    k2 = ky(ptr)
                    found1 = chkfltr (arcnam(jarzn(k1)), zone(k1), 
     &                                '***', base(k1), type, k1)
                    ptr = lowntie(5,ix)
                    nbr = iabs (brnch_ptr(ptr))
                    call getchr (3, lown, kbrnch(3,nbr)) 
                    type = brntyp(brtype(ptr))
                    found2 = chkfltr (arcnam(jarzn(k2)), zone(k2), 
     &                                '***', base(k2), type, k2)
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
              write (outbuf, 540) owner_o(alf2own(io)), 
     &                          owner_o(alf2own(jo)), subtot
  540         format (t2, a3, t7, '->', t11, a3, t16, 
     &                'SUBTOTAL (of following list)', t69, 8f8.1)
              if (o2 .lt. maxbuf_out) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = io
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
                 if (lowntie(1,ix) .eq. 1) then
                    ncb = lowntie(5,ix)
                    call getchr (1, cbtype, kbctbl(8,ncb))
                    call getchr (3, cbown, kbctbl(10,ncb))
                    call getchr (2, cbyear, kbctbl(9,ncb))
                    nb = kbctbl(1,ncb)
                    type = '+' // cbtype
                    found = chkfltr (arcnam(jarzn(nb)), zone(nb), '***',
     &                               base(nb), type, nb)
                    if (found) then
                       kt = inp2opt(nb)
                       vk = sqrt (e(kt) ** 2 + f(kt) ** 2)
                       pload2 = bctbl(2, ncb)
                       qload2 = bctbl(3, ncb)
                       skcon2 = bctbl(4, ncb) * vk ** 2
                       sksus2 = bctbl(5, ncb) * vk ** 2
                       pgen2 = bctbl(6, ncb)
                       qgen2 = bctbl(11, ncb)

C                      Convert constant current and constant admittance 
c                      loads back to constant MVA.

                       if (cbtype .eq. 'A') then
                          if (cbyear .eq. '01' .or. cbyear .eq. '*I') 
     &                       then
                             pload2 = pload2 * vk + skcon2
                             qload2 = qload2 * vk - sksus2
                             skcon2 = 0.0
                             sksus2 = 0.0
                          else if (cbyear .ne. '02' .and. 
     &                             cbyear .ne. '*P') then
                          else
                             pload2 = pload2 + skcon2
                             qload2 = qload2 - sksus2
                             skcon2 = 0.0
                             sksus2 = 0.0
                          endif
                       else if (cbyear .eq. '*I') then
                          pload2 = pload2 * vk + skcon2
                          qload2 = qload2 * vk - sksus2
                          skcon2 = 0.0
                          sksus2 = 0.0
                       else if (cbyear .eq. '*P') then
                          pload2 = pload2 + skcon2
                          qload2 = qload2 - sksus2
                          skcon2 = 0.0
                          sksus2 = 0.0
                       endif
                       if (i .lt. 0) then
                          write (outbuf, 550) cbtype, 
     &                       zone(nb), owner_o(alf2own(io)), ' ',
     &                       bus(nb), base(nb), zone(nb), 
     &                       owner_o(alf2own(jo)), ' ', bus(nb), 
     &                       base(nb), -pload2, 
     &                       -qload2, -skcon2, sksus2, pgen2, qgen2
  550                     format (t7, '+', a, 1x, a2, 1x, a3, 1x, a1,
     &                            a8, f6.1, 
     &                            t36, ' -> B ', 1x, a2, 1x, a3, 1x, a1,
     &                            a8, f6.1, t85, 6f8.1)
                       else
                          write (outbuf, 560) zone(nb),  
     &                       owner_o(alf2own(io)), ' ', bus(nb), 
     &                       base(nb), cbtype, zone(nb), 
     &                       owner_o(alf2own(jo)), ' ',
     &                       bus(nb), base(nb), pload2, qload2, 
     &                       skcon2, sksus2, -pgen2, -qgen2
  560                      format (t7, 'B', 2x, a2, 1x, a3, 1x, a1, a8,
     &                        f6.1, t36, ' -> +', a, 1x, a2, 1x, a3, 1x,
     &                        a1, a8, f6.1, t85, 6f8.1)
                       endif
                    endif
                 else if (lowntie(1,ix) .eq. 2) then
                    ptr = lowntie(5,ix)
                    nbr = iabs (brnch_ptr(ptr))
                    call getchr (3, lown, kbrnch(3,nbr)) 
                    type = brntyp(brtype(ptr))
                    k1 = kx(ptr)
                    k2 = ky(ptr)
                    found1 = chkfltr (arcnam(jarzn(k1)), zone(k1), 
     &                                '***', base(k1), type, k1)
                    found2 = chkfltr (arcnam(jarzn(k2)), zone(k2), 
     &                                '***', base(k2), type, k2)
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
     &                       zone(ky(ptr)), owner_o(alf2own(io)), 
     &                       flag1, bus(ky(ptr)), base(ky(ptr)), 
     &                       brid(ptr), brsect(ptr), 
     &                       zone(kx(ptr)), owner_o(alf2own(jo)), flag2,
     &                       bus(kx(ptr)), base(kx(ptr)), 
     &                       -owntie(1,ix), -owntie(2,ix)
  570                     format (t7, a, 2x, a2, 1x, a3, 1x, a1, a8, 
     &                            f6.1, 1x, a1, 1x, i1, t36, ' -> B ', 
     &                            1x, a2, 1x, a3, 1x, a1, a8, f6.1, t69,
     &                            2f8.1)
                       else
                          if (lowntie(6,ix) .eq. 1) then
                             flag1 = '*'
                             flag2 = ' '
                          else
                             flag1 = ' '
                             flag2 = '*'
                          endif
                          write (outbuf, 580) zone(kx(ptr)), 
     &                       owner_o(alf2own(io)), flag1, bus(kx(ptr)), 
     &                       base(kx(ptr)), type(1:1), 
     &                       zone(ky(ptr)), owner_o(alf2own(jo)), flag2,
     &                       bus(ky(ptr)), 
     &                       base(ky(ptr)), brid(ptr), brsect(ptr),
     &                       owntie(1,ix), owntie(2,ix)
  580                     format (t7,  'B ', 1x, a2, 1x, a3, 1x, a1, a8,
     &                            f6.1, t36, ' ->', 1x, a1, 2x, a2, 1x,
     &                            a3, 1x, a1, a8, 
     &                            f6.1, 1x, a1, 1x, i1, t69, 2f8.1)
                       endif
                    endif
                 else
                    ptr1 = lowntie(4,ix)
                    nbr = iabs (brnch_ptr(ptr1))
                    call getchr (3, lown, kbrnch(3,nbr)) 
                    type1 = brntyp(brtype(ptr1))
                    k1 = kx(ptr1)
                    k2 = ky(ptr1)
                    found1 = chkfltr (arcnam(jarzn(k1)), zone(k1), 
     &                                '***', base(k1), type, k1)
                    ptr2 = lowntie(5,ix)
                    nbr = iabs (brnch_ptr(ptr2))
                    call getchr (3, lown, kbrnch(3,nbr)) 
                    type2 = brntyp(brtype(ptr2))
                    found2 = chkfltr (arcnam(jarzn(k2)), zone(k2), 
     &                                '***', base(k2), type, k2)
                    found = (found1 .or. found2)
                    if (found) then
                       if (i .lt. 0) then
                          if (lowntie(6,ix) .eq. 1) then
                             flag1 = ' '
                             flag2 = '*'
                          else
                             flag1 = '*'
                             flag2 = ' '
                          endif
                          write (outbuf, 582) type2(1:1), 
     &                       zone(ky(ptr1)), owner_o(alf2own(io)), 
     &                       flag1, bus(ky(ptr1)), base(ky(ptr1)), 
     &                       brid(ptr1), brsect(ptr2), type1(1:1), 
     &                       zone(kx(ptr1)), owner_o(alf2own(jo)), 
     &                       flag2, bus(kx(ptr1)), base(kx(ptr1)), 
     &                       brid(ptr1), brsect(ptr1), -owntie(1,ix), 
     &                       -owntie(2,ix)
  582                     format (t7, a, 2x, a2, 1x, a3, 1x, a1, a8, 
     &                            f6.1, 1x, a1, 1x, i1, t36, ' -> ', a, 
     &                            2x, a2, 1x, a3, 1x, a1, a8, f6.1, 1x, 
     &                            a1, 1x, i1, t69, 2f8.1)
                       else
                          if (lowntie(6,ix) .eq. 1) then
                             flag1 = '*'
                             flag2 = ' '
                          else
                             flag1 = ' '
                             flag2 = '*'
                          endif
                          write (outbuf, 584) type1(1:1), 
     &                       zone(kx(ptr1)), owner_o(alf2own(io)), 
     &                       flag1, bus(kx(ptr1)), base(kx(ptr1)), 
     &                       brid(ptr1), brsect(ptr1), type2(1:1), 
     &                       zone(ky(ptr1)), owner_o(alf2own(jo)), 
     &                       flag2, bus(ky(ptr1)), base(ky(ptr1)), 
     &                       brid(ptr2), brsect(ptr2), owntie(1,ix), 
     &                       owntie(2,ix)
  584                     format (t7,  a1, 2x, a2, 1x, a3, 1x, a1, a8,
     &                            f6.1, 1x, a1, 1x, i1, t36, ' ->', 1x, 
     &                            a1, 2x, a2, 1x, a3, 1x, a1, a8, f6.1, 
     &                            1x, a1, 1x, i1, t69, 2f8.1)
                       endif
                    endif
                 endif
                 if (found) then
                    if (o2 .lt. maxbuf_out) then
                       length = apdoutbuf(o2, outbuf(1:132), 
     &                                    out_buffer(o2:))
                       o2 = o2 + length
                       loop(1) = io
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
              loop(1) = io
              loop(2) = 1
              loop(3) = 0
              loop(4) = 0
              loop(5) = 0
           else if (repeat) then
              finished = .true.
           endif
           if (scrfilx .gt. 0) call prtout(1)
           write (outbuf, 590) owner_o(alf2own(io)), totexp
  590      format (t2, a3, t16, 'TOTAL', t69, 8f8.1)
           if (o2 .lt. maxbuf_out) then
              length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
              o2 = o2 + length
              loop(1) = io
              loop(2) = 2
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
              loop(1) = io
              loop(2) = 3
              loop(3) = 0
              loop(4) = 0
              loop(5) = 0
           else if (repeat) then
              finished = .true.
           endif
           if (scrfilx .gt. 0) call prtout(1)
  890      io = io + 1
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
