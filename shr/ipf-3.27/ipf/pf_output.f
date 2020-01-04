C    @(#)pf_output.f	20.4 11/12/98
C****************************************************************
C
C   File: pf_output.f
C   Purpose: Routine to obtain BCD image of INPUT/OUTPUT/SYSTEM 
C            data 
C
C   Author: Walt Powell  Date: 28 April 1992
C   Called by:
C
C****************************************************************
C
        subroutine pf_output (in_buffer, out_buffer, error)
 
        character in_buffer * (*), out_buffer * (*)
        integer error
c
c       This subroutine returns WSCC-formated input data records.
c       Output parameter:
c
c       in_buffer - a character string specifying desired data
c       out_buffer - a character string for storing data
c       error      - warning switch (0 means ignore errors,
c                                    1 means observe errors)
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
        include 'ipfinc/prt.inc'
        include 'ipfinc/xdata.inc'
        include 'ipfinc/pqcurves.inc'

        parameter (MAXDATAREC = 258)
        character null * 1, linefeed * 1, record * (MAXDATAREC), 
     &            bus_1 * 8, bus_2 * 8, text * 120, id * 1
        integer find_bus, flag(50), count, apdoutbuf, o2,
     &          sect, ptr

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
c       Read bus data
c
        null = char(0)
        linefeed = char(10)
 
        out_buffer(1:1) = null
        i1 = 1
        o2 = index (out_buffer, null)
        do while (i1 .lt. index (in_buffer, null))
           i2 = nxt_term(in_buffer(i1+1:)) + i1
           text = in_buffer(i1:i2-1)

           i1 = i2 
           if (in_buffer(i1:i1) .eq. linefeed) i1 = i1 + 1
           if (text(1:1) .eq. 'B') then
c
c             Obtain all data associated with type "B" record.
c
              read (text, 110)  bus_1, base_1
  110         format (t7, a8, f4.0)
              k1 = find_bus (bus_1,base_1)
              if (k1 .gt. 0) then
                 call out_bus (k1, record)
                 length = apdoutbuf(o2, record, out_buffer(o2:))
                 o2 = o2 + length
 
                 ncb = kbsdta(15,k1)
                 do while (ncb .gt. 0)
                    call out_cbs (ncb, record)
                    length = apdoutbuf(o2, record, out_buffer(o2:))
                    o2 = o2 + length
                    ncb = bctbl_nxt(ncb)
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
     1                                 out_buffer(o2:))
                    o2 = o2 + length
                    if (length .eq. 0) then
                       write (errbuf(1), 116) bus(k1), base(k1),
     1                                        bus(k2), base(k2)
  116                  format (' Output buffer overflowed at branch '
     1                       ,a8, f6.1, 1x, a8, f6.1)
                       call prterx ('W', 1)
                       go to 900
                    endif
                    do while (ptr .gt. 0 .and. (ky(ptr) .eq. k2))
                       ptr = brnch_nxt(ptr)
                    enddo
                 enddo
              else
                 if (error .ne. 0) then
                    write (errbuf(1), 140) bus_1, base_1
  140               format (' Bus (', a, f7.1, ') is not in system. ')
                    i = iabs (k1)
                    ierr = 1
                    do j = i-1,i+1
                       if (j .gt. 0 .and. j .le. ntot) then
                          ierr = ierr + 1
                          write (errbuf(ierr), 142) bus(j),base(j)
  142                     format (' Adjacent bus names > ', a8, f6.1)
                       endif
                    enddo
                    call prterx ('W', ierr)
                 endif
              endif
 
           else if (text(1:1) .eq. 'X') then
c
c             Obtain type "X" records.
c
              read (text, 110)  bus_1, base_1
              k1 = find_bus (bus_1,base_1)
              if (k1 .gt. 0) then
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
              else
                 if (error .ne. 0) then
                    i = iabs (k1)
                    do j = i-1,i+1
                       if (j .gt. 0 .and. j .le. ntot) then
                           write (errbuf(1), 140) bus(j),base(j)
                       endif
                    enddo
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
              if (k1 .gt. 0) then
                 ncb = kbsdta(15,k1)
                 nflag = 0
                 do while (ncb .gt. 0)
                    nflag = nflag + 1
                    flag(nflag) = ncb
                    call out_cbs (ncb, record)
c
c                   Check type, ownership, and code-year
c
                    if (text(2:2) .ne. ' ' .and.
     1                  text(2:2) .ne. record(2:2)) then
                       flag(nflag) = -ncb
                    else if (text(3:6) .ne. ' ' .and.
     1                       text(3:6) .ne. record(3:6)) then
                       flag(nflag) = -ncb
                    else if (text(19:20) .ne. ' ' .and.
     1                       text(19:20) .ne. record(19:20)) then
                       flag(nflag) = -ncb
                    endif
                    ncb = bctbl_nxt(ncb)
                 enddo
                 count = 0
                 do i = 1, nflag
                    ncb = flag(i)
                    if (ncb .gt. 0) then
                       call out_cbs (ncb, record)
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
              else
                 if (error .ne. 0) then
                    write (errbuf(1), 140) bus_1, base_1
                    ierr = 1
                    do j = i-1,i+1
                       if (j .gt. 0 .and. j .le. ntot) then
                          ierr = ierr + 1
                          write (errbuf(ierr), 142) bus(j),base(j)
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
              read (text, 170)  bus_1, base_1, bus_2, base_2, id,
     &              sect
  170         format (t7, a8, f4.0, 1x, a8, f4.0, a1, i1)
              k1 = find_bus (bus_1,base_1)
              if (k1 .gt. 0) then
                 k2 = find_bus (bus_2,base_2)
                 if (k2 .gt. 0) then
                    nflag = 0
                    ptr = kbsdta(16,k1)
                    do while (ptr .gt. 0 .and.
     &                       (inp2alf(ky(ptr)) .lt. inp2alf(k2)))
                       ptr = brnch_nxt(ptr)
                    enddo
                    do while (ptr .gt. 0 .and.
     1                       (k2 .eq. ky(ptr)))
                       ltyp = brtype(ptr)
                       if (ltyp .ne. 1) then
                          nflag = nflag + 1
                          flag(nflag) = ptr
c
c                         Check type, ID and section
c
                          if (text(32:32) .ne. ' ' .and.
     1                        text(32:32) .ne. record(32:32)) then
                              flag(nflag) = -ptr
                          else if (text(33:33) .ne. ' ' .and.
     1                             text(33:33) .ne. record(33:33)) 
     2                        then
                              flag(nflag) = -ptr
                          endif
                       endif
                       do while (ptr .gt. 0 .and. (k2 .eq. ky(ptr)))
                         ptr = brnch_nxt(ptr)
                       enddo
                    enddo
 
                    count = 0
                    do i = 1, nflag
                       ptr = flag(i)
                       if (ptr .gt. 0) then
                          count = count + 1
                          call out_brn (ptr, id, sect, record)
                          length = apdoutbuf(o2, record, 
     1                                      out_buffer(o2:))
                          o2 = o2 + length
                          if (length .eq. 0) then
                             write (errbuf(1), 116) bus(k1), 
     1                          base(k1), bus(k2), base(k2)
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
                                write (errbuf(ierr), 130) 
     1                             record(1:80)
  130                           format (a)
                             endif
                          endif
                          i = i + 1
                       enddo
                       call prterx ('W', ierr)
                    endif
                 else
                    if (error .gt. 0) then
                       write (errbuf(1), 190) text(1:33)
  190                  format ('Bus 2 on branch record is not in system 
     &(', a, ')')
                       ierr = 1
                       i = iabs(k2)
                       do j = i-1,i+1
                          if (j .gt. 0 .and. j .le. ntot) then
                             ierr = ierr + 1
                             write (errbuf(ierr), 142) bus(j),base(j)
                          endif
                       enddo
                       call prterx ('W', ierr)
                    endif
                 endif
              else
                 if (error .gt. 0) then
                    i = iabs (k1)
                    write (errbuf(1), 194) text(1:33)
  194               format ('Bus 1 on branch record is not in system (',
     & a, ')')
                    ierr = 1
                    do j = i-1,i+1
                       if (j .gt. 0 .and. j .le. ntot) then
                          ierr = ierr + 1
                          write (errbuf(ierr), 142) bus(j),base(j)
                       endif
                    enddo
                    call prterx ('W', ierr)
                 endif
              endif
           endif
        enddo
  900   continue
        return 
        end
