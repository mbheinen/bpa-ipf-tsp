C    %W% %G%
C****************************************************************
C
C   File: arsummrpt.f
C
C   Purpose: Routine to obtain an IPS-style area summary
c
C   Author: Merilyn George  Oct. 1994
C   Called by: p_report.f
C
***  Printed report (such as it is) is produced OK.
***  When it tries to write to the screen, the window abruptly
***  shrinks to 1/2 inch wide, and nothing is in it.
C****************************************************************
C
        integer function arsummrpt (in_buffer, out_buffer, scrfil)
        character in_buffer *(*), out_buffer *(*)
        integer scrfil,scrfilx

        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/blank.inc'
        include 'ipfinc/header.inc'
        include 'ipfinc/coment.inc'
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

        integer MAXTIESORT
        parameter (MAXTIESORT = 200)
        common /tiesrt/ tiesrt(MAXTIESORT)
        integer tiesrt, o2

        common /is_batch / is_batch

	character  null * 1, linefeed * 1, title(4) * 80,
     &            head(4) * 80, text * 80, zn * 2, arnam1 * 10,
     &            arnam2 * 10, arbus1 * 10, j1 * 1, j2 * 1
        logical found, finished, repeat

	external kmptie, swptie
        logical gtfltr, chkfltra, change_f
        integer kmptie, apdoutbuf, find_zon, find_bus,
     &          loop(3),lastloop(3), findstr

        real subtot(4),export(4),totexp(4)
        save

        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        o2 = index (out_buffer,null)

        arsummrpt = 0
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

*** Compose 3-line title for printed report only
 
      if (scrfilx .gt. 0) then
        write (title(1),100) chase1(1), chase1(34),chase1(35), dte
  100   format ('1CaseID: ',a10, '  Case Description: ',2a10,'  Date: ',
     &     a)
        title(2) = coment(1)
        title(3) = coment(2)
        title(4) = ' '
        do i = 1, 4
           write (scrfilx, '(a)') title(i)
        enddo
      endif

        do jt = 1, jtie

C          Calculate all tie line interchanges
 
           k1 = tie(1,jt)   ! bus no. of metered end
           k2 = tie(7,jt)   ! ditto, non-metered
           ka1 = tie(2,jt)  ! area no. of metered end
           ka2 = tie(8,jt)  ! ditto, non-metered
           kdc = tie(9,jt)  ! dc index
   
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
 
              kt = inp2opt(k1)
              mt = inp2opt(k2)
              ek = e(kt)
              fk = f(kt)
              em = e(mt)
              fm = f(mt)
              gk11 = tie(3,jt)     ! real y11
              bk11 = tie(4,jt)     ! imag y11
              gk12 = tie(5,jt)     ! real y12
              bk12 = tie(6,jt)     ! imag y12
              gm22 = 0.0           ! real y22    these need to
              bm22 = 0.0           ! imag y22    call pieqiv
              gm21 = 0.0           ! real y21
              bm21 = 0.0           ! imag y21
              aik = ek * gk11 - fk * bk11 + em * gk12 - fm * bk12
              bik = ek * bk11 + fk * gk11 + em * bk12 + fm * gk12
              pin = (ek*aik+fk*bik) * bmva
              qin = (-ek*bik+fk*aik) * bmva
              aim = em * gm22 - fm * bm22 + ek * gm21 - fk * bm21
              bim = em * bm22 + fm * gm22 + ek * bm21 + fk * gm21
              pout = (em*aim+fm*bim) * bmva
              qout = (-em*bim+fm*aim) * bmva
 
           endif
           tiesum(1,jt) = pin            ! MW flow
           tiesum(2,jt) = qin            ! Mvar flow
           tiesum(3,jt) = pin + pout     ! MW loss
           tiesum(4,jt) = qin + qout     ! Mvar loss
        enddo

  140   continue

        ix = lastloop(1)
        if (ix .eq. 0) ix = 1
        if (jtie .eq. 0) ix = ntotc + 1

c   Find requested area - more than one allowed???
 
        finished = .false.
        do while (  ix .le. ntotc. and. .not. finished)
           loop(1) = ix
           if (chkfltra(arcnam(ix), 'A*')) then
C                                             
C   This is the area requested
C                                             
              arnam1 = arcnam(ix)
              arbus1 = arcbus(ix)
              arbse1 = arcbas(ix)
              ixx = find_bus (arbus1,arbse1)
              ntie = 0
              if (ntotic .eq. 0) sched = arcnet(ix) * bmva
              do i=1,4
                 totexp(i) = 0.0
              enddo
 
              write (head(1), 150) arnam1,chase1(1)
  150         format (17x,'Area Summary for Area ', a10,' Case ', a10)
              if (scrfilx .gt. 0) write (scrfilx, '(a)') head(1)
              length = apdoutbuf(o2, head(1), out_buffer(o2:))
              o2 = o2 + length
              head(1) = ' '
 
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
 
        write (head(2), 160) arnam2
  160   format (18x,'Summary of Tie Line Flows from Area ',a10)
        write (head(3), 170)
  170   format ('    From Bus        To Bus          Flow at Meter
     &          losses')
        write (head(4), 180)
  180   format(' ',t37,'(MW)   (MVAR)',t58,'(MW)   (MVAR)')
           do i = 1, 4
              if (scrfilx .gt. 0) write (scrfilx, '(a)') head(i)
              length = apdoutbuf(o2, head(i), out_buffer(o2:))
              o2 = o2 + length
           enddo
C  
C          Process AREA 1 - AREA 2 interties
c              subtot is total AREA 1 - AREA 2 interchange
c
                 do i=1,4
                    subtot(i) = 0.0
                 enddo
                 j = jx 
                 jxx = jx - 1
                 finished = .false.
                 do while (j. le. ntie .and. .not. finished)
                    lt = iabs(tiesrt(j))
                    if (tie(2,lt) .eq. ix .and. 
     &                  tie(8,lt) .eq. ka2) then
                       subtot(1) = subtot(1) + tiesum(1,lt)  !MW
                       subtot(2) = subtot(2) + tiesum(2,lt)  !Mvar
                       subtot(3) = subtot(3) + tiesum(3,lt)  !Rloss
                       subtot(4) = subtot(4) + tiesum(4,lt)  !Xloss
                       jxx = j
                       j = j + 1
                    else if (tie(2,lt) .eq. ka2 .and. 
     &                       tie(8,lt) .eq. ix) then
                       subtot(1) = subtot(1) - tiesum(1,lt)  !MW
                       subtot(2) = subtot(2) - tiesum(2,lt)  !Mvar
                       subtot(3) = subtot(3) + tiesum(3,lt)  !Rloss
                       subtot(4) = subtot(4) + tiesum(4,lt)  !Xloss
                       jxx = j
                       j = j + 1
                    else
                       finished = .true.
                    endif
                 enddo
C                                                                 
C                Search for scheduled intertie flow  AREA 1 - AREA 2
C 
                 if (ntotic .ne. 0) then
                     sched = 0.0
                     i = 1
                     found = .false.
                     do while (i .le. ntotic .and. .not. found)
                        if (arcint(1,i) .eq. arnam1 .and.
     &                     arcint(2,i) .eq. arnam2) then
                             sched = arcinp(i)
                             found = .true.
                        else
                             i = i + 1
                        endif
                     enddo
                     circul = subtot(1) - sched
                  endif
 
C   Report individual tie lines and AREA 1 - AREA 2 total
 
                 j = jx
                 do while (j .le. jxx)
                    lt = iabs(tiesrt(j))
                    if (tie(2,lt) .eq. ix) then
                       k = tie(1,lt)
                       m = tie(7,lt)
                       j1 = '*'
                       j2 = ' '
                       do i=1,4
                         export(i) = tiesum(i,lt)
                       enddo
                    else
                       k = tie(7,lt)
                       m = tie(1,lt)
                       j1 = ' '
                       j2 = '*'
                       export(1) = -tiesum(1,lt)
                       export(2) = -tiesum(2,lt)
                       export(3) = tiesum(3,lt)
                       export(4) = tiesum(4,lt)
                    endif
c          Write out tie line
                       write (text, 650)
     &                    bus(k), base(k), j1, bus(m),base(m),j2,
     &                    (export(i),i=1,4)
  650                  format ('0', t2, a8, f6.1, a1,t18,a8, f6.1,
     &                    a1, 2f9.2, t54,2f9.2)
                       if (scrfilx .gt. 0) write (scrfilx, '(a)') 
     &                    text
                       if (j .gt. lastloop(2) .and.
     &                     o2 .lt. maxbuf_out ) then
                          length = apdoutbuf(o2, text, 
     &                                       out_buffer(o2:))
                          o2 = o2 + length
                       elseif ( repeat ) then
                          finished = .true.
                          ibuf_ful = 1
                          go to 812
                       elseif ( ibuf_ful .eq. 0 ) then
                          ibuf_ful = 1
                       endif
                    if (o2 .lt. maxbuf_out) loop(2) = j
                    jx = jx + 1
                    j = j + 1
                 enddo   ! End loop (j .le. jxx)
 
c                   Accumulate total export for AREA 1
                    do i=1,4
                       totexp(i) = totexp(i) + subtot(i)
                    enddo
 
c  Print totals, AREA 1 - AREA 2
c   Write to file only, for now - figure out how to get to screen
c   later (maybe?)
 
      if (scrfilx .gt. 0) then
         write (scrfilx,660)
  660    format (t34,'  -------  -------',t54,'  -------  -------')
         write (scrfilx,670) arnam2,arnam1, (subtot(i),i=1,4)
  670    format (' Total ',a10,'to ',a10,t34,2f9.2,t54,2f9.2)
      endif
 
              enddo  ! End loop (jx .le. ntie)
 
c  Print Area Totals  !! Need slack generator data, generators in area,
c    overloads, and under/overvoltage (compare to figures on A card).
 
           if (1 .gt. lastloop(3)) then
                 if (scrfilx .gt. 0) then
                     write (scrfilx,'(a)') title(4)
                     write (scrfilx,680) (totexp(i),i=1,4)
  680                format ('0Total Export and Tie Line Loss ',
     &                t34,2f9.2,t54,2f9.2)
                     write (scrfilx,681) sched
  681                format (' Scheduled Export ', t34,f9.2)
                     write (scrfilx,'(a)') title(4)
                     write (scrfilx,682)
  682                format ('0Slack Generator   Scheduled Generation ',
     &                  '   Actual Generation    Maximum Generation')
                     write (scrfilx,683) arbus1,arbse1, busdta(8,ixx),
     &                     area(7,ka2),busdta(7,ixx)
  683                format (' ',a8,f6.1,t19,f12.2,'MW',t43,f12.2,'MW',
     &                  t65,f12.2,'MW')
                     write (scrfilx,'(a)') title(4)
                  endif
 
*** summary of generator data goes here
 
 
                     write (text, 690)
  690            format (t13, 'Area Totals', t39, '(MW)    (MVAR)')
                     if (scrfilx .gt. 0) write (scrfilx, '(a)') text
                     length = apdoutbuf(o2, text, out_buffer(o2:))
                     o2 = o2 + length
              endif
 
              if (o2 .lt. maxbuf_out) loop(3) = 1
 
              if (nztot .gt. 0) then
                 do i = 1, 9
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
                       do jj = 1, 9
                          z(jj) = z(jj) + zsum(jj,ij)
                       enddo
                    endif
                 enddo
              endif
              if (2 .gt. lastloop(3)) then
                 write (text, 750) z(1), z(2)
  750            format (t24, 'Generation', t35, f9.1, f9.1)
                 if (scrfilx .gt. 0) write (scrfilx, '(a)') text
                 length = apdoutbuf(o2, text, out_buffer(o2:))
                 o2 = o2 + length
              endif
              if (o2 .lt. maxbuf_out) loop(3) = 2
              if (3 .gt. lastloop(3)) then
                 write (text, 760) z(3), z(4)
  760            format (t24, 'Load', t35, f9.1, f9.1)
                 if (scrfilx .gt. 0) write (scrfilx, '(a)') text
                 length = apdoutbuf(o2, text, out_buffer(o2:))
                 o2 = o2 + length
              endif
              if (o2 .lt. maxbuf_out) loop(3) = 3
              if (4 .gt. lastloop(3)) then
                 write (text, 770) z(5), z(6)
  770            format (t24, 'Losses', t35, f9.1, f9.1)
                 if (scrfilx .gt. 0) write (scrfilx, '(a)') text
                 length = apdoutbuf(o2, text, out_buffer(o2:))
                 o2 = o2 + length
              endif
              if (o2 .lt. maxbuf_out) loop(3) = 4
              if (5 .gt. lastloop(3)) then
                 write (text, 780) z(7), z(8)
  780            format (t24, 'Shunt', t35, f9.1, f9.1)
                 if (scrfilx .gt. 0) write (scrfilx, '(a)') text
                 length = apdoutbuf(o2, text, out_buffer(o2:))
                 o2 = o2 + length
              endif
              if (o2 .lt. maxbuf_out) loop(3) = 5
              pcheck = z(1) - z(3) - z(5) - z(7)
              qcheck = z(2) - z(4) - z(6) + z(8)
              if (6 .gt. lastloop(3)) then
                 write (text, 790) pcheck, qcheck
  790            format (t24, 'Net Export', t35, f9.1, f9.1)
                 if (scrfilx .gt. 0) write (scrfilx, '(a)') text
                 length = apdoutbuf(o2, text, out_buffer(o2:))
                 o2 = o2 + length
              endif
              if (o2 .lt. maxbuf_out) loop(3) = 6
              pavail = z(9) - z(1)
              if (7 .gt. lastloop(3)) then
                 write (text, 795) pavail
  795            format (t24, 'PMax - PGen', t35, f9.1)
                 if (scrfilx .gt. 0) write (scrfilx, '(a)') text
                 length = apdoutbuf(o2, text, out_buffer(o2:))
                 o2 = o2 + length
              endif
              if (o2 .lt. maxbuf_out) loop(3) = 7
              pcheck = pcheck - totexp(1)
              if (abs(pcheck) .gt. option(5)*bmva .and.
     &            8 .gt. lastloop(3)) then
                 write (text,800) arnam1, pcheck
  800            format (' Error: Area ', a10,
     1              ' Gen - Load - Loss - Shunt - Export = ', f10.1)
                 if (scrfilx .gt. 0) write (scrfilx, '(a)') text
                 length = apdoutbuf(o2, text, out_buffer(o2:))
                 o2 = o2 + length
              endif
              if (o2 .lt. maxbuf_out) loop(3) = 8
  810         continue
           else
              if (o2 .lt. maxbuf_out) then
                 loop(1) = ix
                 loop(2) = 0
                 loop(3) = 0
              endif
 
**** Over/Under Voltage
**** Overloaded Lines
**** Overloaded Transformers
**** Regulating Transformers
 
           endif       !  End of if (chkfltra  ...
           ix = ix + 1
           lastloop(2) = 0   ! Tie line loop
           lastloop(3) = 0   ! Summary loop
  812      continue
        enddo        ! End do while (ix .le. ntotc)
 
        arsummrpt = 0

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
