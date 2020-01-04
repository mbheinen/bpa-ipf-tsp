C    %W% %G%
C****************************************************************
C
C       File: dtasanrpt.f
C
C       Purpose: Re-entrany subroutine to obtain filtered report
C                of data sanity checks. Array lastloop() flags last valid
C                encoded entity from previous call. Rentry is
C                invoked with the IPF command
C
C                /GET_DATA, TYPE=SANITY_CHECK, CONTINUE
C
C                Premature termination occurs whenever the encoded
C                output fills out_buffer. The last record is flagged
C                with the terminator
C
C                *[MORE]
C
C                It is the GUI's task to utilize this information
C                to request continuation calls to retrieve the entire
C                data set for the specified filter.
C
C       Author: Walt Powell  Date: 1 May 1993
C       Called by: p_gtdata.f
C
C****************************************************************
C
        subroutine dtasanrpt (in_buffer, out_buffer, scrfil)
        character in_buffer *(*), out_buffer *(*)
        integer scrfil
c
c       This subroutine returns WSCC-formated input data records.
c       Output parameter:
c
c       in_buffer - a character string specifying desired data
c       out_buffer - a character string for storing data
c       error      - warning switch (0 means ignore errors,
c                                    1 means observe errors)
c
C       This routine obtains and applies a filter for entire input 
C       network data.
 
        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/header.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/pqcurves.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/xdata.inc'

        character  text*120, bustyp*2, null*1, linefeed*1, 
     &             own*3, brntyp*2, type*2, temp_out(10)*120,
     &             headrx(16)*120
        logical gtfltr, chkfltr, chkfltra, found, change_f, found_bus, 
     &          found_area, repeat, finished
        external  gtfltr, chkfltr, bustyp, brntyp
        integer o2, apdoutbuf, findstr, loop(14), lastloop(14),
     &          ptr, count, status, count_ncb, 
     &          chk_aredta, chk_busdta, chk_cbsdta, 
     &          chk_xdtdta, chk_brndta, chk_pqdt, scrfilx

        save

        data headrx(1) / 'Btc<O>< Bus  ><KV>ZN<Pld><Qld>Psh>Qsh><Pm>Pge
     1n>qmax>qmin><vh><vl><rembus><kv>%q>' /

        data headrx(2) / 'BDc<O>< Bus  ><KV>ZN< >Br<mH >Amin>Astp>Vdrp>
     1amps><combus><kv>' /

        data headrx(3) / 'BMc<O>< Bus  ><KV>ZN< >Br<mH >Amin>Astp>Vdrp>
     1amps><combus><kv>c<an<gm<pskd><vdc>' /
        data headrx(4) / 'L c<O>< Bus1 ><V1>M< Bus2 ><V2>CS<RT>N< R  ><
     1 x  ><g/2 ><b/2 ><mi><      >in>ou><th><bn>' /
        data headrx(5) / 'LDc<O>< Bus1 ><V1> < Bus2 ><V2>  <RT>< R  >< 
     1l  >< c  >d<psk><vdc>ast>gmn><mi>  <th><bn>' /
        data headrx(6) / 'LMc<O>< Bus1 ><V1> < Bus2 ><V2>  <RT>< R  >< 
     1l  >< c  >               <mi>in>ou><th><bn>' /
        data headrx(7) / 'T c<O>< Bus1 ><V1>M< Bus2 ><V2>CS<RT>N<  R ><
     1  x ><  g ><  b ><tp1><tp2><>in>ou><th><em><bn>' /
        data headrx(8) / 'R c<O>< Bus1 ><V1> < Bus2 ><V2>  < BusC ><Vc>
     1taph>tapl>#t<-------->' /
        data headrx(9) / 'RMc<O>< Bus1 ><V1> < Bus2 ><V2>  < BusC ><Vc>
     1angh>angl>#p<mwh><mwl>' /
        data headrx(10) / 'E c<O>< Bus1 ><V1>M< Bus2 ><V2>CS<RT>N< R  >
     1< x  >< g1 >< b1 >< g2 >< b2 >in>ou><th><bn>' /
        data headrx(11) / 'X c<O>< Bus1 ><V1>--< CBus ><CV>#<Inc>#<Inc>
     1#<inc>#<inc>' /
        data headrx(12) / 'I C< Area 1 >x< Area 2 >xx<MW 1-2> ' /
        data headrx(13) / 'A c< Area   ><Slack ><kV>x<Export>xZ1xZ2xZ3x
     1z4x' /
        data headrx(14) / '+tc<O>< Bus  ><KV>KY<Pld><Qld>Psh>Qsh>....Pg
     1en>qmax>qmin>' /
        data headrx(15) / 'TPc<O>< Bus1 ><V1>M< Bus2 ><V2>CS<RT>N<  R ><
     1  x ><  g ><  b ><ang><tp2><>in>ou><th><em><bn>' /
        data headrx(16) / 'QtcIdA< Bus  ><KV>un<MMVA<Pmax<  1 ><  2 ><  
     &3 ><  4 ><  5 ><  6 ><  7 ><  8 ><  9 >< 10 >< 11 >< 12 >< 13 >< 1
     &4 >< 15>' /

        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        maxbuf_out = len( out_buffer ) - 400
        if ( maxbuf_out .lt. 1 ) maxbuf_out = len( out_buffer ) - 150
        last = index (in_buffer, null)
c
c       Check for re-entry and continue
c
        if (findstr (in_buffer(1:last), 'CONTINUE') .ne. 0) then
           repeat = .true.
           scrfilx = 0
           do i = 1, 14
              lastloop(i) = loop(i)
           enddo
        else
           repeat = .false.
           scrfilx = scrfil
           do i = 1, 14
              lastloop(i) = 0
           enddo
c
c          Search and align to "WHERE" ...
c
           ix = findstr (in_buffer(1:last), 'WHERE') 
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
           endif
           if (scrfilx .gt. 0) then
              write (*, 100)
  100	      format (' * Creating sanity check file - this will take a mi
     &nute.')
           else
              repeat = .true.
           endif
        endif

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        lasto2 = o2

c       loop(1) = A record loop
c       loop(2) = A record subloop
c       loop(3) = I record loop
c       loop(4) = I record subloop
c       loop(5) = B record loop
c       loop(6) = B record subloop
c       loop(7) = + record loop
c       loop(8) = + record subloop
c       loop(9) = X record loop
c       loop(10) = X record subloop
c       loop(11) = Q record loop
c       loop(12) = Q record subloop
c       loop(13) = L record loop
c       loop(14) = L record subloop

        do i = 1, 12
           loop(i) = 0
        enddo
c
c       Add header
c
        if (scrfilx. gt. 0) then
           outbuf = 'Data Sanity Report'
           call rpnlod
    
           write (outbuf, 111) chase1(1), chase1(34), chase1(35)
  111      format('Case: ', a10, ' Project: ', 2a10)
           call hedlod

           outbuf = ' ' // headrx(1)
           call shdlod(1)
           last = min0 (lastch(outbuf), 80)
           length = apdoutbuf(o2, outbuf(1:last), out_buffer(o2:))
           lasto2 = o2
           o2 = o2 + length

           outbuf = ' ' // headrx(4)
           call shdlod(2)
           last = min0 (lastch (outbuf), 80)
           length = apdoutbuf(o2, outbuf(1:last), out_buffer(o2:))
           lasto2 = o2
           o2 = o2 + length

           outbuf = ' ' // headrx(7)
           call shdlod(3)
           last = min0 (lastch (outbuf), 80)
           length = apdoutbuf(o2, outbuf(1:last), out_buffer(o2:))
           lasto2 = o2
           o2 = o2 + length

           outbuf = ' ' // headrx(10)
           call shdlod(4)
           last = min0 (lastch (outbuf), 80)
           length = apdoutbuf(o2, outbuf(1:last), out_buffer(o2:))
           lasto2 = o2
           o2 = o2 + length

           outbuf = ' ' // headrx(8)
           call shdlod(5)
           last = min0 (lastch (outbuf), 80)
           length = apdoutbuf(o2, outbuf(1:last), out_buffer(o2:))
           lasto2 = o2
           o2 = o2 + length

           coment(1) = ' '
           coment(2) = ' '

           call fortop ()
           call prnt_fltr (in_buffer(ix:))
        endif
c
c       Process "A" or "I" records
c
        finished = .false.
        nb = lastloop(1)
        if (nb .eq. 0) nb = 1
c
c       Negate retrieval of area data if bus filter is on.
c
        loop(1) = nb
        do while (nb .le. ntotc .and. .not. finished)
           found_area = chkfltra (arcnam(nb), 'A*') 
           if (found_area) then
              if (nb .gt. lastloop(1)) then
                 if (chkfltra( arcnam(nb), 'A?')) then
                    count = 1
                    status = chk_aredta (nb, 0, count, temp_out)
                    if (status .ne. 0) then
                      call bcdarc (nb, text)
                      last = min0 (lastch (text), 80)
                      temp_out(1) = ' ' // text(1:last)
                      do i = 1, count
                        if (i .gt. lastloop(2)) then
                          outbuf = temp_out(i)
                          last = min0 (lastch (outbuf), 80)
                          if (o2 .lt. maxbuf_out) then
                             length = apdoutbuf(o2, outbuf(1:last), 
     &                                          out_buffer(o2:))
                             lasto2 = o2
                             o2 = o2 + length
                             loop(1) = nb
                             loop(2) = i
                          else if (repeat) then
                             finished = .true.
                          endif
                          if (scrfilx .gt. 0) call prtout(1)
                        endif
                      enddo
                    endif
                 endif
              endif
              if (o2 .lt. maxbuf_out) then
                 loop(1) = nb
                 loop(2) = 0
                 loop(3) = 0
                 loop(4) = 0
              endif
              nc = lastloop(3)
              if (nc .eq. 0) nc = 1
              do while (nc .le. ntotic .and. .not. finished)
                 if (arcint(1,nc) .eq. arcnam(nb)) then
                    if (chkfltra( arcnam(nb), 'I ')) then
                       if (nc .gt. lastloop(3)) then
                          count = 1
                          status = chk_aredta (nc, 0, count, temp_out)
                          if (status .ne. 0) then
                            call bcdari (nc, text)
                            last = min0 (lastch (text), 80)
                            temp_out(1) = ' ' // text(1:last)
                            do i = 1, count
                              if (i .gt. lastloop(4)) then
                                outbuf = temp_out(i)
                                last = min0 (lastch (outbuf), 80)
                                if (o2 .lt. maxbuf_out) then
                                   length = apdoutbuf(o2, 
     &                                                outbuf(1:last), 
     &                                                out_buffer(o2:))
                                   lasto2 = o2
                                   o2 = o2 + length
                                   loop(3) = nc
                                   loop(4) = i
                                else if (repeat) then
                                   finished = .true.
                                endif
                                if (scrfilx .gt. 0) call prtout(1)
                              endif
                            enddo
                          endif
                       endif
                    endif
                 endif
                 if (o2 .lt. maxbuf_out) then
                   loop(3) = nc
                   loop(4) = 0
                 endif
                 nc = nc + 1
              enddo
           else
              if (o2 .lt. maxbuf_out) then
                 loop(1) = nb
                 do i = 2, 4
                   loop(i) = 0
                 enddo
              endif
           endif
           nb = nb + 1
           do i = 2, 4
             lastloop(i) = 0
           enddo
        enddo
c
c       Process "B", "+", "X", "Q", "L", "R", "E", or "T" records
c
        ib = lastloop(5)
        if (ib .eq. 0) ib = 1
        do while (ib .le. ntot_alf .and. .not. finished)
           nb = alf2inp(ib)
           found_bus = chkfltr(arcnam(jarzn(nb)), zone(nb), '***',
     &                         base(nb), 'B*', nb)
           if (found_bus) then
              if (ib .gt. lastloop(5)) then
                 type = bustyp(nb)
                 if (chkfltr( arcnam(jarzn(nb)), zone(nb), owner(nb),
     &                        base(nb), type, nb)) then
                    count = 1
                    status = chk_busdta (nb, 0, count, temp_out)
                    if (status .ne. 0) then
                      call bcdbus (nb, text)
                      last = min0 (lastch (text), 80)
                      temp_out(1) = ' ' // text(1:last)
                      do i = 1, count
                        if (i .gt. lastloop(6)) then
                          outbuf = temp_out(i)
                          last = min0 (lastch (outbuf), 80)
                          if (o2 .lt. maxbuf_out) then
                             length = apdoutbuf(o2, 
     &                                          outbuf(1:last), 
     &                                          out_buffer(o2:))
                             lasto2 = o2
                             o2 = o2 + length
                             loop(5) = ib
                             loop(6) = i
                          else if (repeat) then
                             finished = .true.
                          endif
                          if (scrfilx .gt. 0) call prtout(1)
                        endif
                      enddo
                    endif
                 endif
              endif
              if (o2 .lt. maxbuf_out) then
                 loop(5) = ib  ! B record loop
                 loop(6) = 0   ! B record subloop
              endif

              if (chkfltr( arcnam(jarzn(nb)), zone(nb), '***',
     &                     base(nb), '+ ', nb)) then
c
c                Loop through continuation buses
c
                 ncb = kbsdta(15,nb)
                 count_ncb = 0
                 do while (ncb .gt. 0 .and. .not. finished)
                    count_ncb = count_ncb + 1
                    call getchr (3, own, kbctbl(10,ncb))
                    found = chkfltr( arcnam(jarzn(nb)), zone(nb), own,
     &                               base(nb), '+ ', nb)
                    if (found) then
                       if (count_ncb .gt. lastloop(7)) then
                          count = 1
                          status = chk_cbsdta (ncb, 0, count, temp_out)
                          if (status .ne. 0) then
                             call bcdcbs (ncb, text)
                             last = min0 (lastch (text), 80)
                             temp_out(1) = ' ' // text(1:last)
                             do i = 1, count
                                if (i .gt. lastloop(8)) then
                                  outbuf = temp_out(i)
                                  last = min0 (lastch (outbuf), 80)
                                  if (o2 .lt. maxbuf_out) then
                                     length = apdoutbuf(o2, 
     &                                                outbuf(1:last), 
     &                                                out_buffer(o2:))
                                     lasto2 = o2
                                     o2 = o2 + length
                                     loop(7) = count_ncb
                                     loop(8) = i
                                  else if (repeat) then
                                     finished = .true.
                                  endif
                                  if (scrfilx .gt. 0) call prtout(1)
                                endif
                             enddo
                          endif
                       endif
                    endif
                    if (o2 .lt. maxbuf_out) then
                       loop(7) = count_ncb
                       loop(8) = 0
                    endif
                    ncb = bctbl_nxt(ncb)
                 enddo
              endif

              if (chkfltr( arcnam(jarzn(nb)), zone(nb), '   ', 
     &                     base(nb), 'X ', nb)) then
c
c                Process "X" record
c
                 kxd = busxdtptr(nb)
                 if (kxd .gt. 0) then
                    if (kxd .gt. lastloop(9)) then
                       count = 1
                       status = chk_xdtdta (kxd, 0, count, temp_out)
                       if (status .ne. 0) then
                          call bcdxdt (kxd, text)
                          last = min0 (lastch (text), 80)
                          temp_out(1) = ' ' // text(1:last)
                          do i = 1, count
                             if (i .gt. lastloop(10)) then
                               outbuf = temp_out(i)
                               last = min0 (lastch (outbuf), 80)
                               if (o2 .lt. maxbuf_out) then
                                  length = apdoutbuf(o2, 
     &                                               outbuf(1:last), 
     &                                               out_buffer(o2:))
                                  lasto2 = o2
                                  o2 = o2 + length
                                  loop(9) = kxd
                                  loop(10) = i
                               else if (repeat) then
                                  finished = .true.
                               endif
                               if (scrfilx .gt. 0) call prtout(1)
                             endif
                          enddo
                       endif
                    endif
                    if (o2 .lt. maxbuf_out) then
                       loop(9) = kxd
                       loop(10) = 0
                    endif
                 endif
              endif

              if (chkfltr( arcnam(jarzn(nb)), zone(nb), '   ', 
     &                     base(nb), 'Q*', nb)) then
c
c                Process "P/Q records
c
                 kpq = buspqptr(nb)
                 if (kpq .gt. 0) then
                    if (kxd .gt. lastloop(11)) then
                       count = 1
                       status = chk_pqdt (kpq, 0, count, temp_out)
                       if (status .ne. 0) then
                          call bcdqpd (kpq, text)
                          last = min0 (lastch (text), 120)
                          temp_out(1) = ' ' // text(1:last)
                          do i = 1, count
                             if (i .gt. lastloop(12)) then
                               outbuf = temp_out(i)
                               last = min0 (lastch (outbuf), 120)
                               if (o2 .lt. maxbuf_out) then
                                  length = apdoutbuf(o2, 
     &                                               outbuf(1:last), 
     &                                               out_buffer(o2:))
                                  lasto2 = o2
                                  o2 = o2 + length
                                  loop(11) = kpq
                                  loop(12) = i
                               else if (repeat) then
                                  finished = .true.
                               endif
                               if (scrfilx .gt. 0) call prtout(1)
                             endif
                          enddo
                       endif
                    endif
                    if (o2 .lt. maxbuf_out) then
                       loop(11) = kpq
                       loop(12) = 0
                    endif
                 endif
              endif

              if (chkfltr( arcnam(jarzn(nb)), zone(nb), '***', 
     &                     base(nb), 'L*', nb)) then
c
c                Loop through branch data
c
                 ptr = kbsdta(16,nb)
                 count_nbr = 0
                 do while (ptr .gt. 0 .and. .not. finished)
                    count_nbr = count_nbr + 1
                    nbr = iabs(brnch_ptr(ptr))
                    call getchr (3, own, kbrnch(3,nbr))
                    type = brntyp(brtype(ptr))
                    found = chkfltr( arcnam(jarzn(nb)), zone(nb), own,
     &                               base(nb), type, nb)
                    if (found .and. brtype(ptr) .ne. 1) then
                       if (count_nbr .gt. lastloop(13)) then
                          count = 1
                          status = chk_brndta (ptr, 0, count, temp_out)
                          if (status .ne. 0) then
                             call bcdbrn (ptr, text)
                             last = min0 (lastch (text), 80)
                             temp_out(1) = ' ' // text(1:last)
                             do i = 1, count
                               if (i .gt. lastloop(14)) then
                                 outbuf = temp_out(i)
                                 last = min0 (lastch (outbuf), 80)
                                 if (o2 .lt. maxbuf_out) then
                                    length = apdoutbuf(o2, 
     &                                                 outbuf(1:last), 
     &                                                 out_buffer(o2:))
                                    lasto2 = o2
                                    o2 = o2 + length
                                    loop(13) = count_nbr
                                    loop(14) = i
                                 else if (repeat) then
                                   finished = .true.
                                 endif
                                 if (scrfilx .gt. 0) call prtout(1)
                               endif
                             enddo
                          endif
                       endif
                    endif
                    if (o2 .lt. maxbuf_out) then
                       loop(13) = count_nbr
                       loop(14) = 0
                    endif
                    ptr = brnch_nxt(ptr)
                 enddo
              endif
           else
              if (o2 .lt. maxbuf_out) then
                 loop(5) = ib   ! B record loop
                 do i = 6, 12
                   loop(i) = 0
                 enddo
              endif
           endif
           lastloop(5) = ib
           ib = ib + 1
           do i = 6, 12
              lastloop(i) = 0
           enddo
        enddo
  800   continue

c       Remember maxbuf_out is really 400 less than the real buffer size

        if (o2 .ge. maxbuf_out) then
           write (out_buffer(o2:o2+8), 820) linefeed, null
  820      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif
        return
        end
