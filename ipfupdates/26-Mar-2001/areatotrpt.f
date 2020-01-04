C    %W% %G%
C
C   File: areatotrpt.f
C
C   Purpose: Routine to obtain area totals for multiple areas.
***
*** Report all areas if no filter, or area(s) requested in
*** the filter.
 
*** Two lines per area?
*** AREANAME    Load(MW)  Losses(MW)  Shunt(MW)  Generation  Export(MW)
***              (MVAR)     (MVAR)      (MVAR)     (MVAR)      (MVAR)
 
C   Author: Merilyn George  Dec. 1994
C   Called by: p_report.f
C
C****************************************************************
C
        integer function areatotrpt (in_buffer, out_buffer, scrfil)
        character in_buffer *(*), out_buffer *(*)
        integer scrfil, o2

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/anlys.inc'
        include 'ipfinc/busanl.inc'
        include 'ipfinc/intchg.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/sortuvov.inc'

        character null * 1, linefeed * 1, text * 80, zn * 2
        integer  apdoutbuf, findstr, find_zon
        logical gtfltr, chkfltr, change_f, finished

        save

        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        max_buf = len( out_buffer ) - 150

        areatotrpt = 0
        maxbuf_out = len( out_buffer ) - 80
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

        write (text, 100) chase1(1), dte
  100   format (' Area Totals Summary - Case: ', a10, ' Date ',
     &            a10)
        if (scrfil .gt. 0) write (scrfil, '(a)') text
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length

        ix = 1

        do while ( ix .le. ntotc .and. jtie .gt. 0 )

           if (o2 .lt. maxbuf_out) then
              loop = ix
           endif
           if (chkfltr(arcnam(ix), '**', '***', 0.0, '**', 0)) then
 
*** For generation total use code from genrpt.f, but do not print
 
        nb = 0
        nx = 0
        finished = .false.
        totpgen = 0.0
        totqgen = 0.0
 
        do while ( nx .le. ntot_alf .and. .not. finished)
           nb = nb + 1
c  This is doing it direct from the bus array. Could define type*2,
c    type = bustyp(nb) -- (bustyp is a function which returns the 
c                          2-char. bus type)
c    then check the alpha type instead of numeric.
           type = kbsdta(1,nb)
 
c  Omit DC buses
           if (type .ne. 5 .and. type .ne. 12) then
c  Apply area filter
           if (jarzn(nb) .eq. ix) then
c  Sum only buses with generation
            if (busdta(8,nb) .ne. 0.0) then
              kt = inp2opt(nb)
              pgen = bmva * (pnetu(kt) + ploadu(kt))
              qgen = bmva * (qnetu(kt) + qloadu(kt))
              totpgen = totpgen + pgen
              totqgen = totqgen + qgen
 
            endif   !busdta(8) not 0.0  (has generation)
           endif    !jarzn eq ix (bus in area)
           endif    !type ne 5 or 12   (is AC bus)
           nx = nx + 1
        enddo
 
c  Print Area Totals

        write (text, 690) arcnam(ix)
  690   format (t6,'Totals for ',a10, t38, '(MW)    (MVAR)')
        if (scrfil .gt. 0) write (scrfil, '(//a)') text
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + 80
 
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
              endif    !(nztot gt 0)
 
              totngen = totpgen - z(1)
              totnvar = totqgen - z(2)
              write (text, 700) totpgen, totqgen
 700          format (t15, 'Total Generation', t35, 2f9.1)
              if (scrfil .gt. 0) write (scrfil, '(a)') text
              length = apdoutbuf(o2, text, out_buffer(o2:))
              o2 = o2 + 80
              write (text, 710) z(1), z(2)
 710          format (t15, 'Positive Pgen',t35,2f9.1)
              if (scrfil .gt. 0) write (scrfil, '(a)') text
              length = apdoutbuf(o2, text, out_buffer(o2:))
              o2 = o2 + 80
              write (text, 720) totngen, totnvar
 720          format (t15, 'Negative Pgen',t35,2f9.1)
              if (scrfil .gt. 0) write (scrfil, '(a)') text
              length = apdoutbuf(o2, text, out_buffer(o2:))
              o2 = o2 + 80
              write (text, 730) z(3)+totngen, z(4)+totnvar
 730          format (t15, 'Total Load', t35,2f9.1)
              if (scrfil .gt. 0) write (scrfil, '(a)') text
              length = apdoutbuf(o2, text, out_buffer(o2:))
              o2 = o2 + 80
              write (text, 740) z(3), z(4)
 740          format (t15, 'Load plus Neg. Pgen', t35,2f9.1)
              if (scrfil .gt. 0) write (scrfil, '(a)') text
              length = apdoutbuf(o2, text, out_buffer(o2:))
              o2 = o2 + 80
              write (text, 750) z(5),z(6)
 750          format (t15, 'Losses', t35, f9.1, f9.1)
              if (scrfil .gt. 0) write (scrfil, '(a)') text
              length = apdoutbuf(o2, text, out_buffer(o2:))
              o2 = o2 + 80
              write (text, 760) z(7), z(8)
 760          format (t15, 'Shunt', t35, f9.1, f9.1)
              if (scrfil .gt. 0) write (scrfil, '(a)') text
              length = apdoutbuf(o2, text, out_buffer(o2:))
              o2 = o2 + 80
              pcheck = z(1) - z(3) - z(5) - z(7)
              qcheck = z(2) - z(4) - z(6) + z(8)
              write (text, 770) pcheck, qcheck
 770          format (t15, 'Net Export', t35, f9.1, f9.1)
              if (scrfil .gt. 0) write (scrfil, '(a)') text
              length = apdoutbuf(o2, text, out_buffer(o2:))
              o2 = o2 + 80
              pavail = z(9) - z(1)
              write (text, 780) pavail
 780          format (t15, 'PMax - PGen', t35, f9.1,
     &           '  (without negative Pgen)')
              if (scrfil .gt. 0) write (scrfil, '(a)') text
              length = apdoutbuf(o2, text, out_buffer(o2:))
              o2 = o2 + 80
  810         continue
           endif   ! chfltr
  150      continue
           ix = ix + 1
        enddo
        areatotrpt = 0

        if (o2 .gt. max_buf) then
           o2 = max_buf - 80
           write (out_buffer(o2:o2+36), 820) linefeed, null
  820      format (a, '*[MORE DATA THAN CAN BE DISPLAYED]', a)
           o2 = o2 + 37
        endif
        if (ntotc .eq. 0) then
           write (errbuf(1), 830)
  830      format(' No area data in network ')
           call prterx ('W', 1)
           areatotrpt = 1
        endif

  900   continue
        return
        end
