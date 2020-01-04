C    %W% %G%
C****************************************************************
C
C       File: ovldtxsrpt.f
C
C       Purpose: Routine to obtain a filtered list of transformers and
c                phase shifters which are loaded above a prescribed 
c                level.
c
C       Author: Walt Powell  Date: 14 December 1992
C       Called by: p_report.f
C
C****************************************************************
C
	subroutine ovldtxsrpt (in_buf, out_buf, scrfil) 

        character in_buf * (*)
        character out_buf * (*)
        integer scrfil

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/basval.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/oldbus.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/lfiles.inc'

	character null * 1, linefeed * 1, type*2,
     &            header(4) * 80, brnown * 3, text * 80, brntyp * 2
        integer o2, ptr, ptrold, kmpuvov, apdoutbuf, findstr,
     &          scrfilx, loop(2), lastloop(2), count
        logical gtfltr, chkfltr, change_f, repeat, finished
        real lnload, loading
	external kmpuvov, swapuvov

        save

        null = char(0)
        linefeed = char(10)
        out_buf(1:1) = null
        o2 = index (out_buf,null)
        maxbuf_in = len( in_buf )
        maxbuf_out = len( out_buf ) - 400
        last = index( in_buf, null )
        if ( last .eq. 0 ) last = maxbuf_in
        ibuf_ful = 0

        i = last
        if ( i .gt. 50 ) i = 50
        repeat = ( findstr( in_buf(1:i), 'CONTINUE') .ne. 0) 
        scrfilx = scrfil
        if ( repeat ) then
           scrfilx = 0
           do i = 1, 2
              lastloop(i) = loop(i)
           enddo
        else
           do i = 1, 2
              lastloop(i) = 0
           enddo
c
c          Search and align to "WHERE" ...
c
           ix = findstr (in_buf, 'WHERE') ! findstr is a 
c                                         ! case-insensitive
c                                         ! version of index
           if (ix .gt. 0) then
              ix = ix + len('WHERE')
              change_f = gtfltr(in_buf(ix:))
           else
              do i = 1, 7
                 filter(i) = 0
              enddo
           endif
C                                                                       
C          sortsw: 0 - Sorted by bus names.                      
C                  1 - Sorted by area-bus names.              
C                                                                       
           if (sortsw .eq. 0) then
              do i = 1, ntot
                 vltsrt(i) = i
              enddo
              if (ntotc .gt. 0) call qiksrt (1, ntot, kmpuvov, swapuvov)
           endif
                                                                        
           if (basval(4) .ne. null) then
              text(1:10) = basval(4)
           else if (cspare(30) .ne. null) then
              text(1:10) = cspare(30)
           else
              text(1:10) = ' '
           endif

           if (basval(5) .ne. null) then
              text(11:20) = basval(5) 
           else if (cspare(31) .ne. null) then
              text(11:20) = cspare(31)
           else
              text(11:20) = ' '
           endif

           write (header(1), 90) text(1:10), text(11:20)
   90   format (' Transformer Loading - Case ', a, ' Date ', a)
        header(2) =  ' Own Zn Bus1    Base Zn Bus2    Base IS Case -----
     1---------- Ratings -----------'
        header(3) =  '                                              Ther
     &mal  Emergcy  Bottlnk  Nominal'
        header(4) =  '                                         MVA  MVA 
     &  %  MVA   %  MVA   %  MVA   %'

           o2 = index (out_buf,null)
           do i = 1, 4
              if (scrfilx .gt. 0) write (scrfilx, '(a)') header(i)
              length = apdoutbuf(o2, header(i), 
     1                            out_buf(o2:))
              o2 = o2 + length
           enddo

           loading = range_filter(1)

        endif

        ix = lastloop(1)
        if (ix .eq. 0) ix = 1

c*** Fix to allow full report to the file

        finished = .false.
        do while ( ix .le. ntot_alf .and. .not. finished)
           nb = vltsrt(ix)                                           
           if (inp2alf(nb) .gt. ntot_alf) then
           else if (chkfltr (arcnam(jarzn(nb)), zone(nb), '***', 
     &                       base(nb), '**', 0)) then
              if (o2 .lt. maxbuf_out) then
                 loop(1) = ix
                 loop(2) = 0
              endif
              count = 0
              ptr = kbsdta(16,nb)
              do while (ptr .gt. 0)
                 count = count + 1
                 k2 = ky(ptr)
                 if (inp2alf(nb) .lt. inp2alf(k2)) then
                 else if (chkfltr (arcnam(jarzn(k2)), zone(k2), '***', 
     1                             base(k2), '**', 0)) then
                    go to 140
                 endif
                 ltyp = brtype(ptr)
                 if (ltyp .eq. 1) then
                    call gtlnload (ptr, caseld, lnload, ratthr, 
     1                               ratbtl, ratllf, ratnom)
                    ptrold = ptr
                    j = brnch_nxt(ptr)
                    do while (j .gt. 0 .and. 
     &                       (ky(j) .eq. k2 .and.
     &                        brid(j) .eq. brid(ptr)))
                       nbr = iabs(brnch_ptr(j))
                       call getchr (3, brnown, kbrnch(3,nbr))
                       type = brntyp(brtype(j))
                       if (chkfltr ('**********', '**', brnown, 0.0, 
     &                              type, 0)) then
                          ltyp = brtype(j)
                          if (ltyp .eq. 5 .or. ltyp .eq. 6) then
                             call gtlnload (j, caseld, lnload, ratthr, 
     1                                           ratbtl, ratllf, ratnom)
                             if (lnload .gt. loading) then
                                call bcdbrn (j, buf)
                                if (ratthr .gt. 0) then
                                   iovthr = 100.0 * caseld / ratthr
                                else
                                   iovthr = 0.0
                                endif
                                if (ratbtl .gt. 0) then
                                   iovbtl = 100.0 * caseld / ratbtl
                                else
                                   iovbtl = 0.0
                                endif
                                if (ratllf .gt. 0) then
                                   iovllf = 100.0 * caseld / ratllf
                                else
                                   iovllf = 0.0
                                endif
                                if (ratnom .gt. 0) then
                                   iovnom = 100.0 * caseld / ratnom
                                else
                                   iovnom = 0.0
                                endif

                                icaseld = caseld
                                iratthr = ratthr
                                iratbtl = ratbtl
                                iratllf = ratllf
                                iratnom = ratnom
 
                                write (text, 110) buf(4:6), zone(nb), 
     &                             buf(7:18), zone(k2), buf(20:31), 
     &                             buf(32:32), buf(33:33),
     &                             icaseld, iratthr, iovthr, iratllf, 
     &                             iovllf, iratbtl, iovbtl, iratnom, 
     &                             iovnom
  110 		                format (t2, a, 1x, a, 1x, a, 1x, a, 1x, 
     1                              a, 1x, a, a, i5, 4(i5,i4)) 
                                if (scrfilx .gt. 0) then
                                   write (scrfilx, '(a)') text
                                endif
                                if (count .gt. lastloop(2) .and.
     &                              o2 .lt. maxbuf_out ) then
                                   length = apdoutbuf(o2, text, 
     &                                                out_buf(o2:))
                                   o2 = o2 + length
                                elseif ( repeat ) then
                                   finished = .true.
                                   ibuf_ful = 1
                                   go to 150
                                elseif ( ibuf_ful .eq. 0 ) then
                                   ibuf_ful = 1
                                endif
                             endif
                          endif
                       endif
                       ptrold = j
                       j = brnch_nxt(j)
                    enddo
                    ptr = ptrold
                 else if (ltyp .eq. 5 .or. ltyp .eq. 6) then
                    nbr = iabs(brnch_ptr(ptr))
                    call getchr (3, brnown, kbrnch(3,nbr))
                    type = brntyp(brtype(ptr))
                    if (chkfltr ('**********', '**', brnown, 0.0, type,
     1                           0)) then
                       call bcdbrn (ptr, buf)
                       call gtlnload (ptr, caseld, lnload, ratthr, 
     1                                  ratbtl, ratllf, ratnom)
                       if (lnload .gt. loading) then
                          if (ratthr .gt. 0) then
                             iovthr = 100.0 * caseld / ratthr
                           else
                             iovthr = 0.0
                          endif
                          if (ratbtl .gt. 0) then
                             iovbtl = 100.0 * caseld / ratbtl
                          else
                             iovbtl = 0.0
                          endif
                          if (ratllf .gt. 0) then
                             iovllf = 100.0 * caseld / ratllf
                          else
                             iovllf = 0.0
                          endif
                          if (ratnom .gt. 0) then
                             iovnom = 100.0 * caseld / ratnom
                          else
                             iovnom = 0.0
                          endif

                          icaseld = caseld
                          iratthr = ratthr
                          iratbtl = ratbtl
                          iratllf = ratllf
                          iratnom = ratnom

                          write (text, 110) buf(4:6), zone(nb), 
     &                           buf(7:18), zone(k2), buf(20:31), 
     &                           buf(32:32), buf(33:33),
     &                           icaseld, iratthr, iovthr, iratllf, 
     &                           iovllf,iratbtl, iovbtl, iratnom, 
     &                           iovnom
                          if (scrfilx .gt. 0) then
                             write (scrfilx, '(a)') text
                          endif
                          if (count .gt. lastloop(2) .and.
     &                        o2 .lt. maxbuf_out ) then
                             length = apdoutbuf(o2, text, 
     &                                          out_buf(o2:))
                             o2 = o2 + length
                          elseif ( repeat ) then
                             finished = .true.
                             ibuf_ful = 1
                             go to 150
                          elseif ( ibuf_ful .eq. 0 ) then
                             ibuf_ful = 1
                          endif
                       endif
                    endif
                 endif
  140            ptr = brnch_nxt(ptr)
                 if (o2 .lt. maxbuf_out) loop(2) = count
              enddo
           endif
  150      continue
           ix = ix + 1
           lastloop(2) = 0    ! branch data loop
        enddo

c*** remember maxbuf_out is really 400 less than the real buffer size
        if (o2 .gt. maxbuf_out) then
           length = apdoutbuf(o2, '*[MORE]' , out_buf(o2:))
           o2 = o2 + length
        endif

        return
        end 
