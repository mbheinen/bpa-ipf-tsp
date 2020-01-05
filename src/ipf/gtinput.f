C    @(#)gtinput.f	20.4 11/12/98
C****************************************************************
C
C   File: gtinput.f
C   Purpose: Routine to obtain BCD image of INPUT/OUTPUT/SYSTEM 
C            data 
C
C   Author: Walt Powell  Date: 20 February 1992
C                        Modified: 20 February 1992
C   Called by:
C
C****************************************************************
C
        subroutine gtinput (in_buffer, out_buffer)
        character in_buffer *(*), out_buffer *(*)
c
c       This subroutine returns WSCC-formated input data records.
c       Output parameter:
c
c       out_buffer - a character string for storing data
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/pqcurves.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/xdata.inc'

        character bus_1*8, bus_2*8, text*120, record*120,
     &            null*1, linefeed*1
        logical eof, found
        integer find_bus, flag(50), count, o2, apdoutbuf, ptr

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
c
c       Read bus data from in_buffer
c
        null = char(0)
        linefeed = char(10)

        i1 = nxt_term(in_buffer)
        if (in_buffer(i1:i1) .eq. linefeed) i1 = i1 + 1
        i2 = index (in_buffer, null)

        out_buffer(1:1) = null
        o2 = index (out_buffer, null)
        text = '( end )'

        do while (i1 .lt. i2)
           next_i1 = nxt_term(in_buffer(i1+1:)) + i1
           text = in_buffer(i1:next_i1-1)
           if (text(1:1) .eq. 'B') then
c
c             Obtain all data associated with type "B" record.
c
              read (text, 110, err=200) bus_1, base_1
  110         format (bz, t7, a8, f4.0)
              k1 = find_bus (bus_1,base_1)
              if (k1 .le. 0) then
                 call ermisbus (bus_1, base_1, text)
              else
                 call bcdbus (k1, record)
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
c
c                Obtain all continuation bus data associated with 
c                bus record.
c
                 ncb = kbsdta(15,k1)
                 do while (ncb .gt. 0 .and. ncb .le. ntot2)
                    call bcdcbs (ncb, record)
                    length = apdoutbuf(o2, record, 
     1                                 out_buffer(o2:))
                    o2 = o2 + length
                    if (length .eq. 0) then
                       write (errbuf(1), 114) bus(k1), base(k1)
  114                  format (' Output buffer overflowed at +bus ', 
     1                    a8, f6.1) 
                       call prterx ('W', 1)
                       go to 900
                    endif
                    ncb = bctbl_nxt(ncb)
                 enddo
c
c                Obtain all x-data associated with bus record.
c
                 if (kbsdta(1,k1) .eq. 11) then
                    kxd = busxdtptr(k1)
                    if (kxd .gt. 0) then
                       call bcdxdt (kxd, record)
                       length = apdoutbuf(o2, record, 
     1                                 out_buffer(o2:))
                       o2 = o2 + length
                       if (length .eq. 0) then
                          write (errbuf(1), 116) bus(k1), base(k1)
  116                     format (' Output buffer overflowed at Xbus ', 
     1                       a8, f6.1) 
                          call prterx ('W', 1)
                          go to 900
                       endif
                    endif
                 endif
c
c                Obtain all pqcurve data associated with bus record.
c
                 kpqd = buspqptr(k1)
                 if (kpqd .gt. 0) then
                    do i = 1, 3
                       if (i .eq. 1) then
                          call bcdqpd (kpqd, record)
                       else if (i .eq. 2) then
                          call bcdqxd (kpqd, record)
                       else
                          call bcdqnd (kpqd, record)
                       endif
                       length = apdoutbuf(o2, record, 
     1                                 out_buffer(o2:))
                       o2 = o2 + length
                       if (length .eq. 0) then
                          write (errbuf(1), 116) bus(k1), base(k1)
                          call prterx ('W', 1)
                          go to 900
                       endif
                    enddo
                 endif
c
c                Obtain all branch data associated with bus record.
c
                 ptr = kbsdta(16,k1)
                 do while (ptr .gt. 0)
                    ltyp = brtype(ptr)
                    if (ltyp .ne. 1) then
                       call bcdbrn (ptr, record)
                       length = apdoutbuf(o2, record, 
     1                                        out_buffer(o2:))
                       o2 = o2 + length
                       if (length .eq. 0) then
                          k2 = ky(ptr)
                          write (errbuf(1), 118) bus(k1), base(k1),
     1                                           bus(k2), base(k2)
  118                     format (' Output buffer overflowed at branch '
     1                       , a8, f6.1, 1x, a8, f6.1) 
                          call prterx ('W', 1)
                          go to 900
                       endif
                    endif
                    ptr = brnch_nxt(ptr)
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
                 call bcdbus (k1, record)
                 if (kbsdta(1,k1) .eq. 11) then
                    kxd = busxdtptr(k1)
                 else
                    kxd = 0
                    write (errbuf(1), 150) bus(k1),base(k1)
  150               format (' Bus ', a8, f6.1, ' has no associated "X"
     1records.')
                    call prterx ('W', 1)
                 endif
                 if (kxd .gt. 0) then
                    call bcdxdt (kxd, record)
                    length = apdoutbuf(o2, record, 
     1                                 out_buffer(o2:))
                    o2 = o2 + length
                    if (length .eq. 0) then
                       write (errbuf(1), 116) bus(k1), base(k1)
                       call prterx ('W', 1)
                       go to 900
                    endif
                 endif
              endif
           else if (text(1:2) .eq. 'QP' .or. 
     &              text(1:2) .eq. 'QX' .or. 
     &              text(1:2) .eq. 'QN') then
c
c             Obtain type "QP", "QX", or "QN" records.
c
              read (text, 110)  bus_1, base_1
              k1 = find_bus (bus_1,base_1)
              if (k1 .le. 0) then
                 call ermisbus (bus_1, base_1, text)
              else
                 call bcdbus (k1, record)
                 kpqd = buspqptr(k1)
                 if (kpqd .gt. 0) then
                    if (text(1:2) .eq. 'QP') then
                       call bcdqpd (kpqd, record)
                    else if (text(1:2) .eq. 'QX') then
                       call bcdqxd (kpqd, record)
                    else if (text(1:2) .eq. 'QN') then
                       call bcdqnd (kpqd, record)
                    endif
                    length = apdoutbuf(o2, record, 
     1                                 out_buffer(o2:))
                    o2 = o2 + length
                    if (length .eq. 0) then
                       write (errbuf(1), 116) bus(k1), base(k1)
                       call prterx ('W', 1)
                       go to 900
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
                 ncb = kbsdta(15,k1)
                 nflag = 0
                 do while (ncb .gt. 0 .and. ncb .le. ntot2)
                    nflag = nflag + 1
                    flag(nflag) = -ncb
                    call bcdcbs (ncb, record)
c
c                   Check type, ownership, and code-year
c
                    if (text(2:2) .eq. '*' .or.
     &                  text(2:2) .eq. record(2:2)) then
                       if (text(4:6) .eq. '***' .or.
     &                     text(4:6) .ne. record(4:6)) then
                          if (text(19:20) .ne. '**' .or.
     &                        text(19:20) .ne. record(19:20)) then
                             flag(nflag) = ncb
                          endif
                       endif
                    endif
                    ncb = bctbl_nxt(ncb)
                 enddo
                 count = 0
                 do i = 1, nflag
                    ncb = flag(i)
                    if (ncb .gt. 0) then
                       call bcdcbs (ncb, record)
                       length = apdoutbuf(o2, record, 
     1                                        out_buffer(o2:))
                       o2 = o2 + length
                       if (length .eq. 0) then
                          write (errbuf(1), 114) bus(k1), base(k1)
                          call prterx ('W', 1)
                          go to 900
                       endif
                       count = count + 1
                    endif
                 enddo
                 if (count .eq. 0) then
                    write (errbuf(1), 160) text(1:20)
  160               format (1x, a, ' not found. Adjacent records follow
     1.')
                    ierr = 1
                    do i = 1, nflag
                       ncb = -flag(i)
                       if (ncb .gt. 0) then
                          call bcdcbs (ncb, record)
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
              read (text, 170, err=200)  bus_1, base_1, bus_2, base_2
  170         format (bz, t7, a8, f4.0, 1x, a8, f4.0)
              k1 = find_bus (bus_1,base_1)
              if (k1 .le. 0) then
                 call ermisbus (bus_1, base_1, text)
              else
                 k2 = find_bus (bus_2,base_2)
                 if (k2 .le. 0) then
                    call ermisbus (bus_2, base_2, text)
                 else
                    nflag = 0
                    ptr = kbsdta(16,k1)
                    do while (ptr .gt. 0)
                       ltyp = brtype(ptr)
                       if (ltyp .ne. 1) then
                          call bcdbrn (ptr, record)
                          nflag = nflag + 1
                          flag(nflag) = -ptr
c
c                         Check type, ID and section
c
                          if (ky(ptr) .eq. k2) then
                             if (text(32:32) .eq. '*') then
                                flag(nflag) = ptr
                             else if (text(32:32) .eq. record(32:32)) 
     &                          then
                                if (text(33:33) .eq. '0') then
                                   flag(nflag) = ptr
                                else if (text(33:33) .eq. record(33:33))
     &                             then
                                   flag(nflag) = ptr
                                endif
                             endif
                          endif
                       endif
                       ptr = brnch_nxt(ptr)
                    enddo
                    count = 0
                    do i = 1, nflag
                       ptr = flag(i)
                       if (ptr .gt. 0) then
                          call bcdbrn (ptr, record)
                          length = apdoutbuf(o2, record, 
     1                                           out_buffer(o2:))
                          o2 = o2 + length
                          count = count + 1
                          if (length .eq. 0) then
                             k2 = ky(ptr)
                             write (errbuf(1), 118) bus(k1), base(k1),
     1                                              bus(k2), base(k2)
                             call prterx ('W', 1)
                             go to 900
                          endif
                       endif
                    enddo
                    if (count .eq. 0) then
                       write (errbuf(1), 180) text(1:33)
  180                  format ('Branch is not in system ',
     1                    '(', a, '). Adjacent records follow.')
                       ierr = 1
                       i = 1
                       do while (i .le. nflag .and. ierr .lt. 9)
                          ptr = -flag(i)
                          if (ptr .gt. 0) then
                             call bcdbrn (ptr, record)
                             if (ierr .lt. 10) then
                                ierr = ierr + 1
                                write (errbuf(ierr), 130) record(1:80)
  130                           format (a)
                             endif
                          endif
                          i = i + 1
                       enddo
                       call prterx ('W', ierr)
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
        buf = text

  900   continue
        return 
        end
