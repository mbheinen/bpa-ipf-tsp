C    %W% %G%
C****************************************************************
C
C   File: inttierpt.f
C
C   Purpose: Routine to obtain an area intertie line report
c
C   Author: Walt Powell  Date: 14 December 1992
C   Called by: p_report.f
C
C****************************************************************
C
        integer function inttierpt (in_buffer, out_buffer, scrfil)
        character in_buffer *(*), out_buffer *(*)
        integer scrfil,scrfilx

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/anlys.inc'
        include 'ipfinc/dc2t.inc'
        include 'ipfinc/dcmt.inc'
        include 'ipfinc/intchg.inc'
        include 'ipfinc/slnopt.inc'
        include 'ipfinc/busanl.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/ordsta.inc'

        integer MAXTIESORT
        parameter (MAXTIESORT = 200)
        common /tiesrt/ tiesrt(MAXTIESORT)
        integer tiesrt, o2

        common /is_batch / is_batch

	character  null * 1, linefeed * 1,
     &            header(4) * 80, zn * 2, arnam1 * 10, 
     &            arnam2 * 10, j1 * 1, j2 * 1
        logical found, finished, repeat

	external kmptie, swptie
        logical gtfltr, chkfltra, change_f
        integer kmptie, apdoutbuf, find_zon, loop(3),
     &          lastloop(3), findstr

        save

        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        o2 = index (out_buffer,null)

        inttierpt = 0
        maxbuf_out = len( out_buffer ) - 400
        last = index( in_buffer, null )
        if ( last .eq. 0 ) last = maxbuf_out
        ibuf_ful = 0

        i = last
        if ( i .gt. 50 ) i = 50
        repeat = ( findstr( in_buffer(1:i), 'CONTINUE') .ne. 0) 
        scrfilx = scrfil
        if ( repeat ) then
           scrfilx = 0
           do i = 1, 3
              lastloop(i) = loop(i)
           enddo
           go to 140
        endif
        do i = 1, 3
           lastloop(i) = 0
        enddo
c
c       Search and align to "WHERE" ...
c
        ix = findstr (in_buffer, 'WHERE') ! findstr is a 
c                                         ! case-insensitive
c                                         ! version of index
        if (ix .gt. 0) then
           ix = ix + len('WHERE')
           change_f = gtfltr(in_buffer(ix:))
        else
           do i = 1, 7
              filter(i) = 0
           enddo
        endif

c       Set up the page header

        if (scrfilx. gt. 0) then
           outbuf = 'Tie Line Summary'
           call rpnlod
    
           write (outbuf, 90) chase1(1), chase1(34), chase1(35)
   90      format('Case: ', a10, ' Project: ', 2a10)
           call hedlod
        endif

        write (header(1), 100) cspare(30), dte
  100   format (' Tie line summary of area interchange case ', a10, 
     &         ' date ', a10)
        write (header(2), 110) 
  110   format (' Area 1     Area 2     /-------------- Intertie branch 
     &-------------/   intertie')
        write (header(3), 120) 
  120   format ('                       Zone Bus 1         Zone Bus 2   
     &          line     flow')
        header(4) = '                                                   
     &              (mw)     (mw)'

        do i = 1, 4
           length = apdoutbuf(o2, header(i), out_buffer(o2:))
           o2 = o2 + length
           if (scrfilx .gt. 0) then
              outbuf = header(i)
              call shdlod(i)
           endif
        enddo

        if (scrfilx .gt. 0) then
           outbuf = ' '
           call shdlod(5)
           call comlod(1)
           call comlod(2)
           call forbtm()
           call fortop()
           call prnt_fltr (in_buffer(ix:))
        endif

        do jt = 1, jtie

C          Calculate all tie line interchanges
 
           k1 = tie(1,jt)
           k2 = tie(7,jt)
           ka1 = tie(2,jt)
           ka2 = tie(8,jt)
           kdc = tie(9,jt)
   
           if (kdc .gt. 0) then
c
c             It is known only that tie(jt) is a d-c line. Search
c             first in the multi-terminal d-c array and last in
c             the 2-terminal d-c array.
c
              found = .false.
              mdc = 1
              do while (mdc .le. mtdcbs .and. .not. found)
                 m1 = dcmtln(1,mdc)
                 m2 = dcmtln(2,mdc)
                 if (min0(m1,m2) .ne. min0(k1,k2)) then
                    mdc = mdc + 1
                 else if (max0(m1,m2) .ne. max0(k1,k2)) then
                    mdc = mdc + 1
                 else
                    found = .true.
                 endif
              enddo
              if (found) then
 
                 if (m1 .eq. k1) then
                    l1 = dcmtln(8,mdc)
                    l2 = dcmtln(9,mdc)
                 else
                    l1 = dcmtln(9,mdc)
                    l2 = dcmtln(8,mdc)
                 endif
 
                 v1 = dcmtbs(20,l1)
                 v2 = dcmtbs(20,l2)
                 pin = v1 * (v1-v2) / dcmtln(4,mdc)
              else
                 mdc = 1
                 do while (mdc .le. kdtot .and. .not. found)
                    if (dc2t(1,mdc) .eq. k1 .and. 
     &                  dc2t(3,mdc) .eq. k2) then
                       found = .true.
                       pin = 0.001 * dc2t(39,mdc) * dc2t(40,mdc)
                    else if (dc2t(1,mdc) .eq. k2 .and. 
     &                       dc2t(3,mdc) .eq. k1) then
                       found = .true.
                       pin = -0.001 * dc2t(39,mdc) * dc2t(41,mdc)
                    else
                       mdc = mdc + 1
                    endif
                 enddo
                 if (.not. found) then
                    write (errbuf(1), 130)
  130               format (' D-C tie line not found in d-c arrays')
                    if (is_batch .eq. 0) then
                       call prterx ('E',1)
                    else
                       call prterx ('F',1)
                    endif
                    go to 900 
                 endif
              endif
           else
 
              if (ordvlt .eq. 1) then
                 kt = k1
                 mt = k2
              else
                 kt = inp2opt(k1)
                 mt = inp2opt(k2)
              endif
              ek = e(kt)
              fk = f(kt)
              em = e(mt)
              fm = f(mt)
              gk11 = tie(3,jt)
              bk11 = tie(4,jt)
              gk12 = tie(5,jt)
              bk12 = tie(6,jt)
              aik = ek * gk11 - fk * bk11 + em * gk12 - fm * bk12
              bik = ek * bk11 + fk * gk11 + em * bk12 + fm * gk12
              pin = (ek*aik+fk*bik) * bmva
           endif
           tiesum(1,jt) = pin
        enddo

  140   continue

        ix = lastloop(1)
        if (ix .eq. 0) ix = 1
        if (jtie .eq. 0) ix = ntotc + 1

        finished = .false.
        do while ( ix .le. ntotc .and. .not. finished)
           loop(1) = ix
           if (chkfltra( arcnam(ix), 'A*')) then
C                                             
C          This area was requested
C                                             
              arnam1 = arcnam(ix)
              totexp = 0.0
              ntie = 0
 
c          Find all ties to this area, and sort by connected area
 
              do i = 1, jtie
                 if (tie(2,i) .eq. ix) then
                    ntie = ntie + 1
                    tiesrt(ntie) = i
                 else if (tie(8,i) .eq. ix) then
                    ntie = ntie + 1
                    tiesrt(ntie) = -i
                 endif
              enddo
              call qiksrt (1, ntie, kmptie, swptie)
C          ntie is total number of ties to this area
              jx = 1
              do while (jx .le. ntie)
                 jt = iabs(tiesrt(jx))
                 if (tie(2,jt) .eq. ix) then
                    ka2 = tie(8,jt)
                 else
                    ka2 = tie(2,jt)
                 endif
                 arnam2 = arcnam(ka2)
C  
C          Process AREA 1 - AREA 2 interties
C
c                subtot is total AREA 1 - AREA 2 interchange
                 subtot = 0.0
                 j = jx 
                 jxx = jx - 1
                 finished = .false.
                 do while (j. le. ntie .and. .not. finished)
                    lt = iabs(tiesrt(j))
                    if (tie(2,lt) .eq. ix .and. 
     &                  tie(8,lt) .eq. ka2) then
                       subtot = subtot + tiesum(1,lt)
                       jxx = j
                       j = j + 1
                    else if (tie(2,lt) .eq. ka2 .and. 
     &                       tie(8,lt) .eq. ix) then
                       subtot = subtot - tiesum(1,lt)
                       jxx = j
                       j = j + 1
                    else
                       finished = .true.
                    endif
                 enddo
C                                                                 
C                Search for scheduled intertie flow  AREA 1 - AREA 2
C 
                 sched = 0.0
                 i = 1
                 found = .false.
                 do while (i .le. ntotic .and. .not. found)
                    if (arcint(1,i) .eq. arnam1 .and.
     &                  arcint(2,i) .eq. arnam2) then
                       sched = arcinp(i)
                       found = .true.
                    else
                       i = i + 1
                    endif
                 enddo
                 circul = subtot - sched
 
C   Report AREA 1 - AREA 2 total and individual tie lines
 
                 ksw = 0
                 j = jx
                 do while (j .le. jxx)
                    lt = iabs(tiesrt(j))
                    if (tie(2,lt) .eq. ix) then
                       k = tie(1,lt)
                       m = tie(7,lt)
                       j1 = '*'
                       j2 = ' '
                       export = tiesum(1,lt)
                    else
                       k = tie(7,lt)
                       m = tie(1,lt)
                       j1 = ' '
                       j2 = '*'
                       export = -tiesum(1,lt)
                    endif
c          Write report
                    if (ksw .eq. 0) then
c              This is the first line, with area names and total
c                   Accumulate total export for AREA 1
                    totexp = totexp + subtot
                       write (outbuf, 650) arnam1, arnam2, zone(k), 
     &                    j1, bus(k), base(k), zone(m), j2, bus(m), 
     &                    base(m), export, subtot
  650                  format ('0', t2, a10, t13, a10, t24, a2, 1x, 
     &                    a1, a8, f6.1, t43, a2, 1x, a1, a8, f6.1, 
     &                    t61, f9.1, f9.1)
                       if (scrfilx .gt. 0) call prtout (1)
                       if (j .gt. lastloop(2) .and.
     &                     o2 .lt. maxbuf_out ) then
                          length = apdoutbuf(o2, outbuf(1:132), 
     &                                       out_buffer(o2:))
                          o2 = o2 + length
                       elseif ( repeat ) then
                          finished = .true.
                          ibuf_ful = 1
                          go to 812
                       elseif ( ibuf_ful .eq. 0 ) then
                          ibuf_ful = 1
                       endif
                       arnam1 = ' '
                       arnam2 = ' '
                       ksw = 1
                    else if (ksw .eq. 1) then
c             This is for rest of tie lines AREA 1 - AREA 2
                       write (outbuf, 660) zone(k), j1, bus(k), 
     &                    base(k), zone(m), j2, bus(m), base(m), 
     &                    export
  660                  format (t24, a2, 1x, a1, a8, f6.1, t43, 
     &                    a2, 1x, a1, a8, f6.1, t61, f9.1)
                       if (scrfilx .gt. 0) call prtout (1)
                       if (j .gt. lastloop(2) .and.
     &                     o2 .lt. maxbuf_out ) then
                          length = apdoutbuf(o2, outbuf(1:132), 
     &                                       out_buffer(o2:))
                          o2 = o2 + length
                       elseif ( repeat ) then
                          finished = .true.
                          ibuf_ful = 1
                          go to 812
                       elseif ( ibuf_ful .eq. 0 ) then
                          ibuf_ful = 1
                       endif
                    endif                          
                    if (o2 .lt. maxbuf_out) loop(2) = j
                    jx = jx + 1
                    j = j + 1
                 enddo
              enddo
              if (1 .gt. lastloop(3)) then
                 write (outbuf, 690)
  690            format (t13, 'Area total', t37, '(MW)      (MVAR)')
                 if (scrfilx .gt. 0) call prtout (1)
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
              endif
              if (o2 .lt. maxbuf_out) loop(3) = 1
 
              if (nztot .gt. 0) then
                 do i = 1, 8
                    z(i) = 0.0
                 enddo
                 ii = 0
                 finished = .false.
                 i = 1
                 do while (i .le. MAXCAZ .and. .not. finished)
                    zn = arczns(i,ix)
                    if (zn .eq. '  ') then
                       if (i .eq. 1) then
                          ii = ii + 1
                          zname(ii) = zn
                       else
                          finished = .true.
                       endif
                    else
                       ii = ii + 1
                       zname(ii) = zn
                    endif
                    i = i + 1
                 enddo
                 do i = 1, ii
                    ij = find_zon(zname(i))
                    if (ij .gt. 0) then
                       do jj = 1, 8
                          z(jj) = z(jj) + zsum(jj,ij)
                       enddo
                    endif
                 enddo
              endif
              if (2 .gt. lastloop(3)) then
                 write (outbuf, 750) z(1), z(2)
  750            format (t24, 'Generation', t35, f9.1, f9.1)
                 if (scrfilx .gt. 0) call prtout (1)
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
              endif
              if (o2 .lt. maxbuf_out) loop(3) = 2
              if (3 .gt. lastloop(3)) then
                 write (outbuf, 760) z(3), z(4)
  760            format (t24, 'Load', t35, f9.1, f9.1)
                 if (scrfilx .gt. 0) call prtout (1)
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
              endif
              if (o2 .lt. maxbuf_out) loop(3) = 3
              if (4 .gt. lastloop(3)) then
                 write (outbuf, 770) z(5), z(6)
  770            format (t24, 'Losses', t35, f9.1, f9.1)
                 if (scrfilx .gt. 0) call prtout (1)
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
              endif
              if (o2 .lt. maxbuf_out) loop(3) = 4
              if (5 .gt. lastloop(3)) then
                 write (outbuf, 780) z(7), z(8)
  780            format (t24, 'Shunt', t35, f9.1, f9.1)
                 if (scrfilx .gt. 0) call prtout (1)
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
              endif
              if (o2 .lt. maxbuf_out) loop(3) = 5
              pcheck = z(1) - z(3) - z(5) - z(7)
              qcheck = z(2) - z(4) - z(6) + z(8)
              if (6 .gt. lastloop(3)) then
                 write (outbuf, 790) pcheck, qcheck
  790            format (t24, 'Net Export', t35, f9.1, f9.1)
                 if (scrfilx .gt. 0) call prtout (1)
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
              endif
              if (o2 .lt. maxbuf_out) loop(3) = 6
              pcheck = pcheck - totexp
              if (abs(pcheck) .gt. option(5)*bmva .and.
     &            7 .gt. lastloop(3)) then
                 write (outbuf,800) arcnam(ix), pcheck
  800            format (' Error: Area ', a10,
     1              ' Gen - Load - Loss - Shunt - Export = ', f10.1)
                 if (scrfilx .gt. 0) call prtout (1)
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
              endif
              if (o2 .lt. maxbuf_out) loop(3) = 7
  810         continue
           else
              if (o2 .lt. maxbuf_out) then
                 loop(1) = ix
                 loop(2) = 0
                 loop(3) = 0
              endif
           endif
           ix = ix + 1
           lastloop(2) = 0   ! Tie line loop
           lastloop(3) = 0   ! Summary loop
  812      continue
        enddo
        areaintrpt = 0

c*** remember maxbuf_out is really 400 less than the real buffer size
        if (o2 .gt. maxbuf_out) then
           write (out_buffer(o2:o2+8), 820) linefeed, null
  820      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif

        if (ntotc .eq. 0 .or. jtie .eq. 0) then
           write (errbuf(1), 830)
  830      format(' No area interchange data in network ')
           call prterx ('W', 1)
           areaintrpt = 1
        endif

  900   continue
        return
        end
