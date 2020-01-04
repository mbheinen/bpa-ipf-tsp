C    @(#)gtaltopt.f	20.7 7/18/96
C****************************************************************
C
C   File: gtaltopt.f
C   Purpose: Routine to obtain BCD image of INPUT/OUTPUT/SYSTEM 
C            data from alternate base case
C
C   Author: Walt Powell  Date: 26 July 1993
C                        Modified: 
C   Called by: p_gtdata
C
C****************************************************************
C
        subroutine gtaltopt (in_buffer, out_buffer)
        character in_buffer *(*), out_buffer *(*)
c
c       This subroutine returns WSCC-formated input data records.
c       Output parameter:
c
c       out_buffer - a character string for storing data
c
        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/alt_case.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
 
        parameter (MAXDATAREC = 258)
        character bus_1*8, bus_2*8, text*120, id*1, zn*2, null*1, 
     &            linefeed*1, record*(MAXDATAREC)
        integer findoldbus, count, sect, o2, apdoutbuf, ptr,
     &          find_xara, status, xcalcintfl
c
c       Read bus data from in_buffer
c
        null = char(0)
        linefeed = char(10)

        i1 = 1
        i2 = index (in_buffer, null)
        if (i1 .lt. i2 .and. (in_buffer(1:1) .eq. '/')) then
           i1 = nxt_term(in_buffer)
        endif
        if (in_buffer(i1:i1) .eq. linefeed) i1 = i1 + 1

        out_buffer(1:1) = null
        o2 = index (out_buffer, null)

        text = '( end )'
        count = 0

        do while (i1 .lt. i2)
           next_i1 = nxt_term(in_buffer(i1+1:)) + i1
           text = in_buffer(i1:next_i1-1)
           if (text(1:1) .eq. 'A') then
c
c             Obtain output data associated with type "A" record.
c
              k1 = find_xara (text(4:13))
              if (k1 .le. 0) then
                 call ermisare (text(4:13), text)
              else

                 gentot = 0.0
                 lodtot = 0.0
                 lostot = 0.0
 
                 export = oarcnet(k1)*bmva
 
                 do 130 j = 1,MAXCAZ
                    zn = oarczns(j,k1)
                    if (j .eq. 1 .or. zn .ne. ' ') then
                       do k = 1, onztot
                          if (oacznam(k) .eq. zn) then
                             gentot = gentot + ozsum(1,k)
                             lodtot = lodtot + ozsum(3,k)
                             lostot = lostot + ozsum(5,k)
                             go to 130
                          endif
                       enddo
                    endif
  130            continue
C
C                Write area "A" quantities to output
C
                 write (text, 140) oarcnam(k1), gentot, lodtot, lostot, 
     &                             export
  140            format('A ', a10, 1x, 4e15.7)
                 length = apdoutbuf(o2, text, out_buffer(o2:))
                 o2 = o2 + length
                 if (length .eq. 0) then
                    write (errbuf(1), 142) oarcnam(k1)
  142               format (' Output buffer overflowed at area ', a10)
                    call prterx ('W', 1)
                    go to 900
                 endif
              endif
           else if (text(1:1) .eq. 'I') then
c
c             Obtain output data associated with type "I" record.
c
              k1 = find_xara (text(4:13))
              if (k1 .le. 0) then
                 call ermisare (text(4:13), text)
              else
                 k2 = find_xara (text(15:24))
                 if (k2 .le. 0) then
                    call ermisare (text(15:24), text)
                 else
                    status = xcalcintfl (k1, k2, schedflo, actualflo,
     &                                   int_data)
C
C                   Write area "I" quantities to output
C
                    write (text, 144) oarcnam(k1), oarcnam(k2), 
     &                 schedflo, actualflo, schedflo-actualflo, 
     &                 int_data
  144               format('I ', a10, 1x, a10, t25, e15.7, t40, e15.7,
     &                 t55, e15.7, t71, i1)
                    length = apdoutbuf(o2, text, out_buffer(o2:))
                    o2 = o2 + length
                    if (length .eq. 0) then
                       write (errbuf(1), 142) oarcnam(k1)
                       call prterx ('W', 1)
                       go to 900
                    endif
                 endif
              endif
           else if (text(1:1) .eq. 'B') then
c
c             Obtain all data associated with type "B" record.
c
              read (text, 110, err=200)  bus_1, base_1
  110         format (bz, t7, a8, f4.0)
              k1 = findoldbus (bus_1,base_1)
              if (k1 .le. 0) then
                 call oermisbus (bus_1, base_1, text)
              else
                 call xout_bus (k1, record)
                 length = apdoutbuf(o2, record, 
     1                                  out_buffer(o2:))
                 o2 = o2 + length
                 if (length .eq. 0) then
                    write (errbuf(1), 112) oldbus(k1), oldbase(k1)
  112               format (' Output buffer overflowed at bus ', a8,
     1                 f6.1)
                    call prterx ('W', 1)
                    go to 900
                 endif
                 ptr = okbsdta(16,k1)
                 do while (ptr .gt. 0)
                    k2 = oky(ptr)
                    call xout_brn (ptr, '*', 0, record)
                    length = apdoutbuf(o2, record, 
     1                                     out_buffer(o2:))
                    o2 = o2 + length
                    if (length .eq. 0) then
                       write (errbuf(1), 116) oldbus(k1), oldbase(k1),
     1                                        oldbus(k2), oldbase(k2)
  116                  format (' Output buffer overflowed at branch ',
     1                    a8, f6.1, 1x, a8, f6.1)
                       call prterx ('W', 1)
                       go to 900
                    endif
                    do while (ptr .gt. 0 .and. (oky(ptr) .eq. k2))
                       ptr = obrnch_nxt(ptr)
                    enddo
                 enddo
              endif
           else if (index ('LERT', text(1:1)) .ne. 0) then
c
c             Obtain types "L", "E", "R", or "T" records. Note that
c             blank identifier fields (parallel and section) default
c             as wild card.
c
              read (text, 170, err=200)  bus_1, base_1, bus_2, 
     &            base_2, id, sect
  170         format (bz, t7, a8, f4.0, 1x, a8, f4.0, a1, i1)
              k1 = findoldbus (bus_1,base_1)
              if (k1 .le. 0) then
                 call oermisbus (bus_1, base_1, text)
              else
                 k2 = findoldbus (bus_2,base_2)
                 if (k2 .le. 0) then
                    call oermisbus (bus_2, base_2, text)
                 else
                    ptr = okbsdta(16,k1)
                    do while (ptr .gt. 0 .and.
     1                       (oinp2alf(oky(ptr)) .lt. oinp2alf(k2)))
                       ptr = obrnch_nxt(ptr)
                    enddo
                    do while (ptr .gt. 0 .and.
     1                       (k2 .eq. oky(ptr)))
                       call xout_brn (ptr, id, sect, record)
                       length = apdoutbuf(o2, record, 
     1                                        out_buffer(o2:))
                       o2 = o2 + length
                       count = count + 1
                       if (length .eq. 0) then
                          write (errbuf(1), 116) oldbus(k1), 
     1                       oldbase(k1), oldbus(k2), oldbase(k2)
                          call prterx ('W', 1)
                          go to 900
                       endif
                       do while (ptr .gt. 0 .and.
     1                          (k2 .eq. oky(ptr)))
                          ptr = obrnch_nxt(ptr)
                       enddo
                    enddo
                    if (count .eq. 0) then
                       write (errbuf(1), 180) text(1:33)
  180                  format ('Branch record is not in system ',
     1                     '(', a, '). Adjacent records follow.')
                       n = 1
                       ptr = okbsdta(16,k1)
                       do while (ptr .gt. 0 .and. n .lt. 9)
                          call obcdbrn (ptr, record)
                          n = n + 1
                          write (errbuf(n), 190) record(1:80)
  190                     format (a)
                          ptr = obrnch_nxt(ptr)
                       enddo
                       call prterx ('W', n)
                    endif
                 endif
              endif
           endif
           go to 220

  200      write (errbuf(1), 210) text(1:80)
  210      format (' Illegal data in numeric field (', a, ')')
           call prterx ('W', 1)

  220      continue
           i1 = next_i1
           if (in_buffer(i1:i1) .eq. linefeed) i1 = i1 + 1

        enddo

  900   continue
        return 
        end
