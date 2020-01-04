C    @(#)gtoutput.f	20.8 11/12/98
C****************************************************************
C
C   File: gtoutput.f
C   Purpose: Routine to obtain BCD image of INPUT/OUTPUT/SYSTEM 
C            data 
C
C   Author: Walt Powell  Date: 20 February 1992
C                        Modified: 20 February 1992
C   Called by:
C
C****************************************************************
C
        subroutine gtoutput (in_buffer, out_buffer)
        character in_buffer *(*), out_buffer *(*)
c
c       This subroutine returns WSCC-formated input data records.
c       Output parameter:
c
c       out_buffer - a character string for storing data
c
        include 'ipfinc/parametr.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/xdata.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/pqcurves.inc'
        include 'ipfinc/anlys.inc'
        include 'ipfinc/busanl.inc'
        include 'ipfinc/ordsta.inc'
 
        parameter (MAXDATAREC = 258)
        character bus_1*8, bus_2*8, text*120, id*1,
     &            null*1, linefeed*1, record*(MAXDATAREC), 
     &            zn*2
        integer find_bus, count, sect, o2, apdoutbuf, ptr, flag(50),
     &          find_ara, status, calcintflo, int_data
        real lodtot, lostot, schedflo, actualflo
c
c       *******************************************************
c       set up pointers to any P/Q curve data
c
c       Store X-data pointer in busxdtptr() and P/Q curve
c       data pointer in buspqptr()
c
        if (.not. pq_flag) then
           do nb = 1, ntot
              buspqptr(nb) = 0
           enddo

           do i = 1, numcurv
              ieq = pqbusptr(i)
              if (ieq .gt. 0) buspqptr(ieq) = i
           enddo
           pq_flag = .true.
        endif

        if (.not. xdt_flag) then
           do nb = 1, ntot
              busxdtptr(nb)  = 0
           enddo
           do i = 1, kxtot
              kxd = xdata(1,i)
              if (kxd .gt. 0) busxdtptr(kxd) = i
           enddo
           xdt_flag = .true.
        endif
C
C       Set error flag -> 0 = /NOWARNINGS 
C                         1 = /WARNINGS
        error = 1
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
              k1 = find_ara (text(4:13))
              if (k1 .le. 0) then
                 call ermisare (text(4:13), text)
              else

                 gentot = 0.0
                 lodtot = 0.0
                 lostot = 0.0
 
                 export = arcnet(k1)*bmva
 
                 do 130 j = 1,MAXCAZ
                    zn = arczns(j,k1)
                    if (j .eq. 1 .or. zn .ne. ' ') then
                       do k = 1,nztot
                          if (acznam(k) .eq. zn) then
                             gentot = gentot + zsum(1,k)
                             lodtot = lodtot + zsum(3,k)
                             lostot = lostot + zsum(5,k)
                             go to 130
                          endif
                       enddo
                    endif
  130            continue
C
C                Write area "A" quantities to output
C
                 write (text, 140) arcnam(k1), gentot, lodtot, lostot, 
     &                             export
  140            format('A ', a10, 1x, 4e15.7)
                 length = apdoutbuf(o2, text, out_buffer(o2:))
                 o2 = o2 + length
                 if (length .eq. 0) then
                    write (errbuf(1), 142) arcnam(k1)
  142               format (' Output buffer overflowed at area ', a10)
                    call prterx ('W', 1)
                    go to 900
                 endif
              endif
           else if (text(1:1) .eq. 'I') then
c
c             Obtain output data associated with type "I" record.
c
              k1 = find_ara (text(4:13))
              if (k1 .le. 0) then
                 call ermisare (text(4:13), text)
              else
                 k2 = find_ara (text(15:24))
                 if (k2 .le. 0) then
                    call ermisare (text(15:24), text)
                 else
                    status = calcintflo (k1, k2, schedflo, actualflo,
     &                                   int_data)
C
C                   Write area "I" quantities to output
C
                    write (text, 144) arcnam(k1), arcnam(k2), schedflo,
     &                 actualflo, schedflo-actualflo, int_data
  144               format('I ', a10, 1x, a10, t25, e15.7, t40, e15.7,
     &                 t55, e15.7, t71, i1)
                    length = apdoutbuf(o2, text, out_buffer(o2:))
                    o2 = o2 + length
                    if (length .eq. 0) then
                       write (errbuf(1), 142) arcnam(k1)
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
              k1 = find_bus (bus_1,base_1)
              if (k1 .le. 0) then
                 call ermisbus (bus_1, base_1, text)
              else
                 call out_bus (k1, record)
                 length = apdoutbuf(o2, record, 
     1                                  out_buffer(o2:))
                 o2 = o2 + length
                 if (length .eq. 0) then
                    write (errbuf(1), 112) bus(k1), base(k1)
  112               format (' Output buffer overflowed at bus ', a8,
     1                 f6.1)
                    call prterx ('W', 1)
                    go to 900
                 endif

                 ptr = kbsdta(15,k1)
                 do while (ptr .gt. 0)
                    call out_cbs (ptr, record)
                    length = apdoutbuf(o2, record, out_buffer(o2:))
                    o2 = o2 + length
                    ptr = bctbl_nxt(ptr)
                 enddo
 
                 if (kbsdta(1,k1) .eq. 11) then
                    kxd = busxdtptr(k1) 
                    if (kxd .gt. 0) then
                       call out_xbs (k1, record)
                       length = apdoutbuf(o2, record, out_buffer(o2:))
                       o2 = o2 + length
                    endif
                 endif

                 ptr = kbsdta(16,k1)
                 do while (ptr .gt. 0)
                    k2 = ky(ptr)
                    call out_brn (ptr, '*', 0, record)
                    length = apdoutbuf(o2, record, 
     1                                     out_buffer(o2:))
                    o2 = o2 + length
                    if (length .eq. 0) then
                       write (errbuf(1), 116) bus(k1), base(k1),
     1                                        bus(k2), base(k2)
  116                  format (' Output buffer overflowed at branch ',
     1                    a8, f6.1, 1x, a8, f6.1)
                       call prterx ('W', 1)
                       go to 900
                    endif
                    do while (ptr .gt. 0 .and. (ky(ptr) .eq. k2))
                       ptr = brnch_nxt(ptr)
                    enddo
                 enddo
              endif

           else if (text(1:1) .eq. 'X') then
c
c             Obtain type "X" records.
c
              read (text, 110)  bus_1, base_1
              k1 = find_bus (bus_1,base_1)
              if (k1 .le. 0) then
                 call ermisbus (bus_1, base_1, text)
              else
                 if (kbsdta(1,k1) .eq. 11) then
                    kxd = busxdtptr(k1) 
                    if (kxd .gt. 0) then
                       call out_xbs (k1, record)
                       length = apdoutbuf(o2, record, out_buffer(o2:))
                       o2 = o2 + length
                    endif
                 else
                    if (error .ne. 0) then
                       write (errbuf(1), 150) bus(k1),base(k1)
  150                  format (' Bus ', a8, f6.1, ' has no associated "x
     &" records.')
                    endif
                 endif
              endif
 
           else if (text(1:1) .eq. '+') then
c
c             Obtain type "+" records. Note that blank identifier
c             fields (type, owner, and code-year) default as wild
c             card.
c
              read (text, 110)  bus_1, base_1
              k1 = find_bus (bus_1,base_1)
              if (k1 .le. 0) then
                 call ermisbus (bus_1, base_1, text)
              else
                 ptr = kbsdta(15,k1)
                 nflag = 0
                 do while (ptr .gt. 0)
                    nflag = nflag + 1
                    flag(nflag) = ptr
                    call out_cbs (ptr, record)
c
c                   Check type, ownership, and code-year
c
                    if (text(2:2) .ne. ' ' .and.
     1                  text(2:2) .ne. record(2:2)) then
                       flag(nflag) = -ptr
                    else if (text(3:6) .ne. ' ' .and.
     1                       text(3:6) .ne. record(3:6)) then
                       flag(nflag) = -ptr
                    else if (text(19:20) .ne. ' ' .and.
     1                       text(19:20) .ne. record(19:20)) then
                       flag(nflag) = -ptr
                    endif
                    ptr = bctbl_nxt(ptr)
                 enddo
                 count = 0
                 do i = 1, nflag
                    ptr = flag(i)
                    if (ptr .gt. 0) then
                       call out_cbs (ptr, record)
                       length = apdoutbuf(o2, record, 
     1                                   out_buffer(o2:))
                       o2 = o2 + length
                       count = count + 1
                    endif
                 enddo
                 if (count .eq. 0 .and. error .ne. 0) then
                    write (errbuf(1), 160) text(1:20)
  160               format (1x, a, ' not found. Adjacent records follow.
     &')
                    ierr = 1
                    do i = 1, nflag
                       ptr = -flag(i)
                       if (ptr .gt. 0) then
                          call bcdcbs (ptr, record)
                          if (ierr .lt. 10) then
                             ierr = ierr + 1
                             errbuf(ierr) = record
                          endif
                       endif
                    enddo
                    call prterx ('W', ierr)
                 endif
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
              k1 = find_bus (bus_1,base_1)
              if (k1 .le. 0) then
                 call ermisbus (bus_1, base_1, text)
              else
                 k2 = find_bus (bus_2,base_2)
                 if (k2 .le. 0) then
                    call ermisbus (bus_2, base_2, text)
                 else
                    ptr = kbsdta(16,k1)
                    do while (ptr .gt. 0 .and.
     1                       (inp2alf(ky(ptr)) .lt. inp2alf(k2)))
                       ptr = brnch_nxt(ptr)
                    enddo
                    do while (ptr .gt. 0 .and.
     1                       (k2 .eq. ky(ptr)))
                       call out_brn (ptr, id, sect, record)
                       length = apdoutbuf(o2, record, 
     1                                        out_buffer(o2:))
                       o2 = o2 + length
                       count = count + 1
                       if (length .eq. 0) then
                          write (errbuf(1), 116) bus(k1), base(k1),
     1                                           bus(k2), base(k2)
                          call prterx ('W', 1)
                          go to 900
                       endif
                       do while (ptr .gt. 0 .and.
     1                          (k2 .eq. ky(ptr)))
                          ptr = brnch_nxt(ptr)
                       enddo
                    enddo
                    if (count .eq. 0) then
                       write (errbuf(1), 180) text(1:33)
  180                  format ('Branch record is not in system ',
     1                     '(', a, '). Adjacent records follow.')
                       n = 1
                       ptr = kbsdta(16,k1)
                       do while (ptr .gt. 0 .and. n .lt. 9)
                          call bcdbrn (ptr, record)
                          n = n + 1
                          write (errbuf(n), 190) record(1:80)
  190                     format (a)
                          ptr = brnch_nxt(ptr)
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
        buf = text
        return 
        end
