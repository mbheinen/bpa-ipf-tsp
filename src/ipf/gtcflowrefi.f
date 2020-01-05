C    @(#)gtcflowrefi.f	20.9 1/4/99
C****************************************************************
C
C   File: gtcflowrefi.f
C   Purpose: Routine to obtain BCD image of CFLOW-requested data --
c            for the ref base (alternate base)
c            handles "GET_DATA,TYPE=CFLOW_REF_INPUT,TABLE=xxxx,ACTION=xx"
C
C   Author: Walt Powell     Date: 2 June 1993
c   Author2: Jay Coleman    Date: 14 January 1994
C
C****************************************************************
C
        integer function gtcflowrefi (in_buffer, out_buffer)

        implicit none

        character in_buffer *(*), out_buffer *(*)

c***********************************************************************
c***********************************************************************
c****************  temp declarations to get clean compile  *************
c***********************  with "implicit none"  ************************
c***********************************************************************

c****************  need "alt case" versions of subroutines *************
c
c   ermisare, bcdarc, bcdarc2, bcdari, bcdqpd, bcdqxd, bcdqnd
c
c****************  need "alt case" versions of below       *************

        integer find_xara   ! function

c***********************************************************************
c****************  end temp declarations for clean compile  ************
c***********************************************************************
c***********************************************************************

c
c       This subroutine returns WSCC-formated input data records.
c       Output parameter:
c
c       in_buffer  - a character string for storing data
c       out_buffer - a character string for storing data
c
c       Action codes have the following interpretation:
c
c       F  - get the first record in table
c       F1 - get the first record in table associated with bus1
c       F2 - get the first record in table associated with bus1 and bus2
c       F3 - get the first record in table associated with bus1 and bus2
c                                                     and the circuit ID
c       N  - get the next record in table
c       N1 - get the next record in table associated with bus1
c       N2 - get the next record in table associated with bus1 and bus2
c       N3 - get the next record in table associated with bus1 and bus2
c                                                    and the circuit ID
c       G  - get the specified record
c
c       Notes:
c
c         with a "wildcard" circuit ID of " " or "*" and
c            a section code of zero (or blank) --
c              "G" and "F3" are the same as "F2"
c
c         with a non-"wildcard" circuit ID and
c            a section code of zero (or blank) --
c              "G" is the same as "F3"
c
c         if bus1 or bus2 is not specified on a following text record,
c            bus1 or bus2 is used from the previous call.
c
c         codes "F", "N", "N1", "N2", and "N3"  do not take a text record
c            but rely on initialization with a "F", "F1", "F2", "F3",
c            or "G" code on a previous call
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/prt.inc'
        include 'ipfinc/alt_flag.inc'
        include 'ipfinc/alt_case.inc'
        include 'ipfinc/pqcurves.inc'

        character bus_1*8, bus_2*8, text*120, record*120,
     &            null*1, linefeed*1, word(100)*20, action*2,
     &            table*4, capital*20, save_brid_sec*2,
     &            save_br_type*2
        integer findoldbus, apdoutbuf, next_brp_alt, frst_brp_alt, 
     &          lastch, nxt_term, ptr, o2, loop_ai(3), loop_b1, loop_cb,
     &          loop_q, loop_b2, loop_brp, nb, i, ieq, kxd, next, nwrd, 
     &          iwrd, i1, lctext, k1, k2, nc, ncb, kpqd, ib, length, jb,
     &          ic, ftn_atoi
        logical found, cf_err, area_continue
        real base_1, base_2

        save

c***    if (ostates .lt. 2) then
        if (alt_base_loaded .eq. 0) then
           write (errbuf(1), 11)
   11      format(' Request rejected - no base data loaded' )
           goto 800
        endif

        gtcflowrefi = 0  ! Set default return status "successful"

c       *******************************************************
c       set up pointers to any P/Q curve data
c
c       Store X-data pointer in obusxdtptr() and P/Q curve
c       data pointer in buspqptr()
c
c***********************************************************************
c***********************************************************************
c*******  comment out until converted for "ref" (alt) base  ************
c***********************************************************************
c***********************************************************************
c
c        if (.not. pq_flag) then
c           do nb = 1, ontot
c              buspqptr(nb) = 0
c           enddo
c
c           do i = 1, numcurv
c              ieq = pqbusptr(i)
c              if (ieq .gt. 0) buspqptr(ieq) = i
c           enddo
c           pq_flag = .true.
c        endif
c
c***********************************************************************
c***********************************************************************
c*******                   END OF                           ************
c*******  comment out until converted for "ref" (alt) base  ************
c***********************************************************************
c***********************************************************************
c
        if (.not. oxdt_flag) then
           do nb = 1, ontot
              obusxdtptr(nb)  = 0
           enddo
           do i = 1, okxtot
              kxd = oxdata(1,i)
              if (kxd .gt. 0) obusxdtptr(kxd) = i
           enddo
           oxdt_flag = .true.
        endif

        null = char(0)
        linefeed = char(10)
c
c       Parse first record in in_buffer
c
        next = nxt_term(in_buffer)
        text = in_buffer(1:next-1)
        call uscan(text(2:next-1), word, nwrd, '=',' ,'//linefeed)

        do i = 1, nwrd
           word(i) = capital(word(i))
        enddo

        out_buffer(1:1) = null
        o2 = index (out_buffer, null)

        iwrd = 1
        table = ' '
        action = ' '
        cf_err = .false.
        do while (iwrd .le. nwrd)
           if (word(iwrd) .eq. 'TABLE') then
              iwrd = iwrd + 1
              if (word(iwrd) .eq. '=') iwrd = iwrd + 1
              if (iwrd .le. nwrd) then
                 if ( index( 'AREA$BRAN$BUS $CBUS$ITIE$QCUR$XDAT',
     &                word(iwrd)(1:4) ) .ne. 0 ) then
                    table = word(iwrd)
                    iwrd = iwrd + 1
                 else
                    cf_err = .true.
                 endif
              else
                 cf_err = .true.
              endif
              if ( cf_err ) then
                 write (errbuf(1), 100) text(1:60)
  100            format (' Syntax error - illegal "TABLE" :', a)
                 go to 800
              endif
           elseif (word(iwrd) .eq. 'ACTION') then
              iwrd = iwrd + 1
              if (word(iwrd) .eq. '=') iwrd = iwrd + 1
              if (iwrd .le. nwrd) then
                 if ( index( 'F $F1$F2$F3$N $N1$N2$N3$G ',
     &                word(iwrd)(1:2) ) .ne. 0 ) then
                    action = word(iwrd)
                    iwrd = iwrd + 1
                 else
                    cf_err = .true.
                 endif
              else
                 cf_err = .true.
              endif
              if ( cf_err ) then
                 write (errbuf(1), 110) text(1:60)
  110            format (' Syntax error - illegal "ACTION" :', a)
                 go to 800
              endif
           endif
        enddo
        if (action .eq. ' ') then
           write (errbuf(1), 120) text(1:60)
  120      format (' Syntax error - "ACTION" missing :', a)
           call prterx ('W', 1)
           gtcflowrefi = 1
           go to 900
        endif
        if (table .eq. ' ') then
           write (errbuf(1), 130) text(1:60)
  130      format (' Syntax error - "TABLE" missing :', a)
           call prterx ('W', 1)
           gtcflowrefi = 1
           go to 900
        endif

c***********************************************************************
c***********************************************************************
c******************  temp patch until and QCUR            **************
c******************  work with "ref" (alt) base           **************
c***********************************************************************
c***********************************************************************


        if ( index( '$QCUR', table ) .ne. 0 ) then
           write (errbuf(1), 135) 
  135      format (' QCURVE data currently cannot be accessed ', 
     &        'with the REF (ALT) base')
           call prterx ('W', 1)
           gtcflowrefi = 1
           go to 900
        endif

c***********************************************************************
c***********************************************************************
c******************           END OF                      **************
c******************  temp patch until QCUR                **************
c******************  work with "ref" (alt) base           **************
c***********************************************************************
c***********************************************************************


        if ( action(1:1) .eq. 'N'  .or.  action .eq. 'F ' ) goto 200

        i1 = next
        next = nxt_term( in_buffer(i1+1:) ) + i1
        if ( next - i1  .lt.  2 ) then
           text = ' '
        else
           text = in_buffer(i1+1:next-1)
        endif

        lctext = lastch(text)
        if ( lctext .lt. 4 ) goto 200
c
c       Locate array indices of any text records provided
c
        if ( table .eq. 'AREA'  .or.  table .eq. 'ITIE'  ) then
           k1 = find_xara (text(4:13))
           if (k1 .le. 0) then
              call ermisare (text(4:13), text)
              gtcflowrefi = 1
              go to 900
           endif
           loop_ai(1) = k1
           do i = 2, 3
              loop_ai(i) = 0
           enddo
           if (text(1:1) .eq. 'A') then
              loop_ai(2) = ftn_atoi(text(2:2))
           elseif ( table .eq. 'ITIE' ) then
              k2 = find_xara (text(15:24))
              if (k2 .le. 0) then
                 call ermisare (text(15:24), text)
                 gtcflowrefi = 1
                 go to 900
              endif
              nc = 1
              do while (nc .le. ontotic .and. loop_ai(3) .eq. 0)
                 if (oarcint(1,nc) .eq. oarcnam(k1) .and.
     &               oarcint(2,nc) .eq. oarcnam(k2)) then
                    loop_ai(3) = nc
                 endif
                 nc = nc + 1
              enddo
           endif
        else
           read (text, 150, err=700) bus_1, base_1
  150      format (bz, t7, a8, f4.0)
           k1 = findoldbus (bus_1,base_1)
           if (k1 .le. 0) then
              call oermisbus (bus_1, base_1, text)
              gtcflowrefi = 1
              go to 900
           endif
           loop_b1 = oinp2alf(k1)
           loop_cb = 0
           loop_q = 0
           loop_b2 = 0
           loop_brp = 0

           if ( table .eq. 'CBUS' ) then
              if ( action .eq. 'F1' ) goto 200
              ncb = okbsdta(15,k1)
              loop_cb = 0
              do while (ncb .gt. 0 .and. loop_cb .eq. 0)
                 call obcdcbs (ncb, record)
c
c                Check type, ownership, and code-year
c
                 if ( text(2:2) .eq. record(2:2)  .and.
     &                text(4:6) .eq. record(4:6)  .and.
     &                text(19:20) .eq. record(19:20)    )
     &              loop_cb = ncb
                 ncb = obctbl_nxt(ncb)
              enddo
           elseif ( table .eq. 'XDAT' ) then
              kxd = obusxdtptr(k1)
           elseif ( table .eq. 'QCUR' ) then
              kpqd = buspqptr(k1)
              if (kpqd .gt. 0) then
                 if (text(1:2) .eq. 'QP') then
                    loop_q = 1
                 elseif (text(1:2) .eq. 'QX') then
                    loop_q = 2
                 elseif (text(1:2) .eq. 'QN') then
                    loop_q = 3
                 endif
              endif
           elseif ( table .eq. 'BRAN' ) then
              if ( action(1:2) .eq. 'F1' ) goto 200
              read (text, 160, err=700)  bus_2, base_2
  160         format (bz, t20, a8, f4.0)
              k2 = findoldbus (bus_2,base_2)
              if (k2 .le. 0) then
                 call oermisbus (bus_2, base_2, text)
                 gtcflowrefi = 1
                 go to 900
              endif
              loop_b2 = oinp2alf(k2)
c
              if ( text(32:32) .eq. '*' ) text(32:32) = ' '
              if ( text(33:33) .eq. '0' ) text(33:33) = ' '
              if ( text(1:2) .eq. 'L*' ) text(1:2) = '  '
c
              if ( action(1:1) .eq. 'F' ) goto 200
c
c          Check matching  bus2, cktid (or brid), section,
c                          and record type (LERT)
c
              ptr = frst_brp_alt( okbsdta(16,k1) )
              loop_brp = 0
              save_brid_sec = text(32:33)
              save_br_type = text(1:2)
              do while (ptr .gt. 0 .and. loop_brp .eq. 0)
                 if ( oky(ptr) .eq. k2 ) then
                    call obcdbrn (ptr, record)
                    if (text(32:32).eq.' ') text(32:32) = record(32:32)
                    if (text(33:33).eq.' ') text(33:33) = record(33:33)
                    if (text(1:2).eq.'  ') text(1:2) = record(1:2)
                    if ( text(32:33) .eq. record(32:33)  .and.
     &                   text(1:2) .eq. record(1:2) ) loop_brp = ptr
                    text(32:33) = save_brid_sec
                    text(1:2) = save_br_type
                 endif
                 ptr = next_brp_alt(ptr)
              enddo
           endif
        endif
c
c       Obtain data according to table
c
  200   continue
        if (table .eq. 'AREA') then
           if (action(1:1) .eq. 'F') then
              ib = 1
              do i = 2, 3
                 loop_ai(i) = 0
              enddo
           elseif (action(1:1) .eq. 'N') then
              area_continue = .false.
              if (loop_ai(1) .gt. 0) then
                 ib = loop_ai(1) 
                 if (loop_ai(2) .lt. 9) then
                    ic = (loop_ai(2) + 1) * MAXCAZR + 1
                    if (oarczns(ic,ib) .ne. ' ') then
                       area_continue = .true.
                       loop_ai(2) = loop_ai(2) + 1
                    endif
                 endif
              endif
              if (.not. area_continue) then                 
                 ib = loop_ai(1) + 1
                 do i = 2, 3
                    loop_ai(i) = 0
                 enddo
              endif
           elseif (action(1:1) .eq. 'G') then
              ib = loop_ai(1)
           else
              ib = 0
           endif
           loop_ai(1) = ib
           if ( ib .gt. 0  .and.  ib .le. ontotc ) then
              if (loop_ai(2) .eq. 0) then
                 call bcdarc (ib, record)
              else
                 call bcdarc2 (ib, loop_ai(2), record)
              endif
              length = apdoutbuf(o2, record(1:80), out_buffer(o2:))
              if (length .eq. 0) then
                 write (errbuf(1), 250) record(1:33)
  250            format (' Output buffer overflowed at text ', a)
                 call prterx ('W', 1)
                 gtcflowrefi = 1
                 go to 900
              endif
              o2 = o2 + length
           else
              gtcflowrefi = 1
              go to 900
           endif
        elseif (table .eq. 'ITIE') then
           if (action(1:1) .eq. 'F') then
              ib = 1
              do i = 1, 3
                 loop_ai(i) = 0
              enddo
           elseif (action(1:1) .eq. 'N') then
              ib = loop_ai(3) + 1
           elseif (action(1:1) .eq. 'G') then
              ib = loop_ai(3)
           else
              ib = 0
           endif
           loop_ai(3) = ib
           if ( ib .gt. 0  .and.  ib .le. ontotic ) then
              call bcdari (ib, record)
              length = apdoutbuf(o2, record(1:80), out_buffer(o2:))
              if (length .eq. 0) then
                 write (errbuf(1), 250) record(1:33)
                 call prterx ('W', 1)
                 gtcflowrefi = 1
                 go to 900
              endif
              o2 = o2 + length
           else
              gtcflowrefi = 1
              go to 900
           endif
        elseif (table .eq. 'BUS ') then
           if (action .eq. 'F') then
              ib = 1
              loop_cb = 0
              loop_q = 0
              loop_b2 = 0
              loop_brp = 0
           elseif (action .eq. 'N') then
              ib = loop_b1 + 1
              loop_cb = 0
              loop_q = 0
              loop_b2 = 0
              loop_brp = 0
           elseif (action .eq. 'G') then
              ib = loop_b1
           else
              ib = 0
           endif
           loop_b1 = ib
           if ( ib .gt. 0  .and.  ib .le. ontot_alf ) then
              k1 = oalf2inp(ib)
              call obcdbus (k1, record)
              length = apdoutbuf(o2, record(1:80), out_buffer(o2:))
              if (length .eq. 0) then
                 write (errbuf(1), 250) record(1:33)
                 call prterx ('W', 1)
                 gtcflowrefi = 1
                 go to 900
              endif
              o2 = o2 + length
           else
              gtcflowrefi = 1
              go to 900
           endif
        elseif (table .eq. 'CBUS') then
           ib = loop_b1
           ncb = 0
           found = .false.
           if ( ( action .eq. 'F'  .or.  action .eq. 'N' )  .and.
     &            ontot2 .gt. 0  ) then
              if ( action .eq. 'F' ) then
                 ib = 1
                 loop_q = 0
                 loop_b2 = 0
                 loop_brp = 0
              else
                 ncb = loop_cb
                 if (ncb .gt. 0) ncb = obctbl_nxt(ncb)
                 if (ncb .gt. 0) then
                    found = .true.
                 else
                    ib = ib + 1
                 endif
              endif
              do while ( ib .le. ontot_alf  .and.  .not. found ) 
                 k1 = oalf2inp(ib)
                 ncb = okbsdta(15,k1)
                 if (ncb .gt. 0) then
                    found = .true.
                    loop_q = 0
                    loop_b2 = 0
                    loop_brp = 0
                 else
                    ib = ib + 1
                 endif
              enddo
           elseif (action .eq. 'F1') then
              ncb = 0
              if ( ib .gt. 0  .and.  ib .le. ontot_alf ) then
                 k1 = oalf2inp(ib)
                 ncb = okbsdta(15,k1)
              endif
           elseif (action .eq. 'N1') then
              ncb = loop_cb
              if (ncb .gt. 0) then
                 ncb = obctbl_nxt(ncb)
              endif
           elseif (action .eq. 'G') then
              ncb = loop_cb
           else
              ncb = 0
           endif
           loop_b1 = ib
           loop_cb = ncb
           if (ncb .gt. 0) then
              call obcdcbs (ncb, record)
              length = apdoutbuf(o2, record(1:80), out_buffer(o2:))
              if (length .eq. 0) then
                 write (errbuf(1), 250) record(1:33)
                 call prterx ('W', 1)
                 gtcflowrefi = 1
                 go to 900
              endif
              o2 = o2 + length
           else
              gtcflowrefi = 1
              go to 900
           endif
        elseif (table .eq. 'XDAT') then
           ib = loop_b1
           kxd = 0
           if ( action .eq. 'F'  .or. action .eq. 'N' ) then
              if ( action .eq. 'F' ) then
                 ib = 1
              else
                 ib = ib + 1
              endif
              found = .false.
              do while ( ib .le. ontot_alf  .and.  .not. found ) 
                 k1 = oalf2inp(ib)
                 kxd = obusxdtptr(k1)
                 if (kxd .gt. 0) then
                    found = .true.
                 else
                    ib = ib + 1
                 endif
              enddo
           elseif (action .eq. 'G') then
              if ( ib .gt. 0  .and.  ib .le. ontot_alf ) then
                 k1 = oalf2inp(ib)
                 kxd = obusxdtptr(k1)
              endif
           endif
           loop_b1 = ib
           if ( kxd .gt. 0 ) then
              call obcdxdt (kxd, record)
              length = apdoutbuf(o2, record(1:80), out_buffer(o2:))
              if (length .eq. 0) then
                 write (errbuf(1), 250) record(1:33)
                 call prterx ('W', 1)
                 gtcflowrefi = 1
                 go to 900
              endif
              o2 = o2 + length
           else
              gtcflowrefi = 1
              go to 900
           endif
        elseif (table .eq. 'QCUR') then
           ib = loop_b1
           kpqd = 0
           jb = 0
           if (action .eq. 'F') then
              if ( ib .gt. 0  .and.  ib .le. ontot_alf ) then
                 k1 = oalf2inp(ib)
                 kpqd = buspqptr(k1)
                 if (kpqd .gt. 0) jb = 1
              endif
           elseif (action .eq. 'N') then
              if ( ib .gt. 0  .and.  ib .le. ontot_alf ) then
                 k1 = oalf2inp(ib)
                 kpqd = buspqptr(k1)
                 if ( kpqd .gt. 0  .and.  loop_q .lt. 3 ) then
                    jb = loop_q + 1
                 endif
              endif
           else
              if ( ib .gt. 0  .and.  ib .le. ontot_alf ) then
                 k1 = oalf2inp(ib)
                 kpqd = buspqptr(k1)
                 if ( kpqd .gt. 0  .and.  loop_q .gt. 0 ) then
                    jb = loop_q
                 endif
              endif
           endif
           loop_q = jb
           if ( kpqd .gt. 0  .and.  jb .gt. 0 ) then
              if (loop_q .eq. 1) then
                 call bcdqpd (kpqd, record)
              elseif (loop_q .eq. 2) then
                 call bcdqxd (kpqd, record)
              else
                 call bcdqnd (kpqd, record)
              endif
              length = apdoutbuf(o2, record(1:120), out_buffer(o2:))
              if (length .eq. 0) then
                 write (errbuf(1), 250) record(1:33)
                 call prterx ('W', 1)
                 gtcflowrefi = 1
                 go to 900
              endif
              o2 = o2 + length
           else
              gtcflowrefi = 1
              go to 900
           endif
        elseif (table .eq. 'BRAN') then
           ib = loop_b1
           ptr = 0
           if (action .eq. 'F') then
              ib = 1
              loop_cb = 0
              loop_q = 0
              loop_b2 = 0
              loop_brp = 0
              k1 = oalf2inp(ib)
              ptr = frst_brp_alt( okbsdta(16,k1) )
           elseif (action .eq. 'F1') then
              if ( ib .gt. 0  .and.  ib .le. ontot_alf ) then
                 k1 = oalf2inp(ib)
                 ptr = frst_brp_alt( okbsdta(16,k1) )
              endif
           elseif ( action .eq. 'F2'  .or.  ( action .eq. 'F3'
     &              .and.  text(32:32) .eq. ' ' ) ) then
              if ( ib .gt. 0  .and.  ib .le. ontot_alf ) then
                 k1 = oalf2inp(ib)
                 k2 = oalf2inp( loop_b2 )
                 ptr = frst_brp_alt( okbsdta(16,k1) )
                 found = .false.
                 do while ( ptr .gt. 0  .and.  .not. found )
                    if (oky(ptr) .eq. k2) then
                       found = .true.
                    else
                       ptr = next_brp_alt(ptr)
                    endif
                 enddo
              endif
           elseif ( action .eq. 'F3' ) then
              if ( ib .gt. 0  .and.  ib .le. ontot_alf ) then
                 k1 = oalf2inp(ib)
                 k2 = oalf2inp( loop_b2 )
                 ptr = frst_brp_alt( okbsdta(16,k1) )
                 found = .false.
                 do while ( ptr .gt. 0  .and.  .not. found )
                    if ( oky(ptr) .eq. k2 ) then
                       call obcdbrn (ptr, record)
                       if ( text(32:32) .eq. record(32:32) ) then
                          found = .true.
                       else
                          ptr = next_brp_alt(ptr)
                       endif
                    endif
                 enddo
              endif
           elseif (action .eq. 'N') then
              ptr = loop_brp
              ptr = next_brp_alt(ptr)
              if (ptr .eq. 0) then
                 ib = ib + 1
                 if ( ib .le. ontot_alf ) then
                    k1 = oalf2inp(ib)
                    ptr = frst_brp_alt( okbsdta(16,k1) )
                 endif
                 loop_cb = 0
                 loop_q = 0
              endif
           elseif (action .eq. 'N1') then
              ptr = loop_brp
              ptr = next_brp_alt(ptr)
           elseif (action .eq. 'N2') then
              k2 = oalf2inp( loop_b2 )
              ptr = loop_brp
              ptr = next_brp_alt(ptr)
              if (ptr .gt. 0) then
                 if ( oky(ptr) .ne. k2 ) ptr = 0
              endif
           elseif (action .eq. 'N3') then
              k2 = oalf2inp( loop_b2 )
              ptr = loop_brp
              ptr = next_brp_alt(ptr)
              if (ptr .gt. 0) then
                 call obcdbrn (ptr, record)
                 if ( oky(ptr) .ne. k2  .or.
     &                text(32:32) .ne. record(32:32) ) ptr = 0
              endif
           elseif (action .eq. 'G') then
              ptr = loop_brp
           endif
           loop_b1 = ib
           loop_brp = ptr
           if (ptr .gt. 0) then
              call obcdbrn (ptr, record)
              length = apdoutbuf(o2, record(1:92), out_buffer(o2:))
              if (length .eq. 0) then
                 write (errbuf(1), 250) record(1:33)
                 call prterx ('W', 1)
                 gtcflowrefi = 1
                 go to 900
              endif
              o2 = o2 + length
           else
              gtcflowrefi = 1
              go to 900
           endif
        endif
        call buf_load_alt( text )
        go to 900

  700   write (errbuf(1), 710) text(1:33)
  710   format (' Error decoding record (', a, ')')

  800   call prterx ('W', 1)
        gtcflowrefi = 1

  900   continue
        return
        end
