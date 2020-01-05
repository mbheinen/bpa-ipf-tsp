C    @(#)chchek.f	20.7 11/12/98
        subroutine chchek  
c
c       Checks all change records 
c 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/pqcurves.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/xdata.inc'
 
        common /is_batch / is_batch
 
c       temporary scratch storage

        common /scratch/ scratch( 22*MAXBUS )
        integer array(MAXCHG)
        equivalence (array, scratch(1))
c
c	Record typed as character for call to bdcqpd
c
        character xbuf*126, busn*8, txpose*126, record*126
        integer thstyp, find_bus, sec1, sec2, ptr
        external kmpchg, swpchg
c
        if (numchg .le. 1) go to 1040
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

        next = numchg
C
C       Append bus deletion changes
C
        do 120 i = 1, numchg
        if (chgcrd(i)(1:1).eq.'B' .and. chgcrd(i)(3:3).eq.'D') then
           if (chgcrd(i)(126:126) .eq. ' ') then
              busn = chgcrd(i)(7:14)
              read (chgcrd(i)(15:19),610,err=118) basen
              nb = find_bus (busn,basen)
              if (nb .le. 0) then
                 if (chgcrd(i)(126:126) .eq. ' ') then
                   read (chgcrd(i)(122:125), '(bz, i4)') nc
                   write (errbuf(1), 10120) chgcrd(i)(1:33), nc
10120              format( ' Bus1 not found for change ', a, ' No. ',i5)
                   if (is_batch .eq. 0) then
                     call prterx ('E',1)
                   else
                     call prterx ('F',1)
                   endif
                 endif
                 chgcrd(i)(126:126) = 'E'
              else

                 kxd = busxdtptr(nb)
                 if (kxd .gt. 0) then
                    call bcdxdt (kxd, xbuf)
                    xbuf(3:3) = 'D'
                    if (next .lt. MAXCHG) then
                       next = next + 1
                       write (chgcrd(next),90) xbuf, i
   90                  format (a120,'P',i4)
                    else
                       write (errbuf(1), 92) MAXCHG
   92                  format (' More than ', i5, ' change records. Over
     &flow occurred as pseudo-record for following change ')
                       write (errbuf(2), 94) xbuf(1:60)
   94                  format (' (', a, ')')
                       call prterx ('F',2)
                       goto 1040
                    endif
                 endif

                 ncb = kbsdta(15,nb)
                 do while (ncb .gt. 0)
                    call bcdcbs (ncb, xbuf)
                    xbuf(3:3) = 'D'
                    if (next .lt. MAXCHG) then
                       next = next + 1
                       write (chgcrd(next),90) xbuf, i
                       ncb = bctbl_nxt(ncb)
                    else
                       write (errbuf(1), 92) MAXCHG
                       write (errbuf(2), 94) xbuf(1:60)
                       call prterx ('F',2)
                       goto 1040
                    endif
                 enddo
c
c                Obtain all pqcurve data associated with bus record.
c
                 kpqd = buspqptr(nb)
                 if (kpqd .gt. 0) then
                    do j = 1, 3
                       if (j .eq. 1) then
                          call bcdqpd (kpqd, record)
                       else if (j .eq. 2) then
                          call bcdqxd (kpqd, record)
                       else
                          call bcdqnd (kpqd, record)
                       endif
                       xbuf(3:3) = 'D'
                       next = next + 1
                       write (chgcrd(next),90) xbuf, i
                    enddo
                 endif

                 ptr = kbsdta(16,nb)
                 do while (ptr .gt. 0)
                    call bcdbrn (ptr,xbuf)
                    xbuf(3:3) = 'D'
                    if (next .lt. MAXCHG) then
                       next = next + 1
                       write (chgcrd(next),90) xbuf, i
                       ptr = brnch_nxt(ptr)
                    else
                       write (errbuf(1), 92) MAXCHG
                       write (errbuf(2), 94) xbuf(1:60)
                       call prterx ('F',2)
                       goto 1040
                    endif
                 enddo

              endif
           endif
        endif
        go to 120
  118   write (errbuf(1), 119)
  119   format (' Illegal data in a numeric field ')
        write (errbuf(2), 320) chgcrd(i)(122:125), chgcrd(i)(1:80)
        call prterx ('W',2)
  120   continue
        numchg = next
c
c       append branch transposes
c
        do i = 1,numchg
           if (index ('LERT',chgcrd(i)(1:1)) .ne. 0) then
              if (next .lt. MAXCHG) then
                 next = next + 1
                 read (chgcrd(i)(122:125),181) j
  181            format (i4)
                 xbuf = txpose (chgcrd(i))
                 if (chgcrd(i)(121:121) .eq. 'P') then
                    write (chgcrd(next), 182) xbuf, 'U', j
  182               format (a120, a1, i4)
                 else
                    write (chgcrd(next), 182) xbuf, 'T', j
                 endif
              else
                 write (errbuf(1), 92) MAXCHG
                 write (errbuf(2), 94) xbuf(1:60)
                 call prterx ('F',2)
                 goto 1040
              endif
           endif
        enddo
        numchg = next
c
c       sort change array; initialize CHGCRD cross-reference array
c
        do i = 1, numchg
           array(i) = i
        enddo
        call qiksrt (1, numchg, kmpchg, swpchg)
c
c       Update cross-references ! Note: Disabled 1-Mar-1993 WLP
c
c       do i = 1, numchg
c          read (chgcrd(i)(122:125), '(bz, i4)') nc
c          write (chgcrd(i)(122:125), '(i4)') array(nc)
c       enddo

c       Debug printout

        if (kase1(27) .ne. 0) then
           write (dbug, 186)
  186      format ('0 Dump of CHGCRD array '/
     1         t2, 'Num', t7, 'Record', t127, 'P', t129, 'Num', /)
           do i = 1, numchg
              write (dbug, 187) i, chgcrd(i)
  187         format (t2, i4, t7, a)
           enddo
        endif
c
c       Check for duplicates
c
        lsttyp = index ('DAPZB+XQLER$TI',chgcrd(1)(1:1))
        if (chgcrd(1)(1:2) .eq. 'RZ') lsttyp = lsttyp + 1
        do 1020 i = 2, numchg
        thstyp = index ('DAPZB+XQLER$TI',chgcrd(i)(1:1))
        if (chgcrd(i)(1:2) .eq. 'RZ') thstyp = thstyp + 1
        if (thstyp .eq. 0) go to 1020
        if (thstyp .ne. lsttyp) go to 1020
c
c       exempt pseudo changes
c
        if (chgcrd(i-1)(121:121) .eq. 'P' .and.
     1      chgcrd(i)(121:121) .eq. 'P') go to 1020
c
c              d   a   p   z   b   +   x   q,  l    e   r   rz   t   
c              i
        go to (200,300,400,500,600,700,800,810,1000,1000,900,910,1000,
     &         340) thstyp
c
c       Check "D" changes
c
  200   if (chgcrd(i-1)(2:2) .eq. chgcrd(i)(2:2) .and.
     1      chgcrd(i-1)(4:13) .eq. chgcrd(i)(4:13)) then
           if (chgcrd(i-1)(3:3) .eq. chgcrd(i)(3:3)) then
              write (errbuf(1),310)
              write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                              chgcrd(i-1)(1:80)
              write (errbuf(3),320) chgcrd(i)(122:125), chgcrd(i)(1:80)
              call prterx ('W',3)
           else
              write (errbuf(1),330)
              write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                              chgcrd(i-1)(1:80)
              write (errbuf(3),320) chgcrd(i)(122:125), chgcrd(i)(1:80)
              call prterx ('W',3)
            endif
        endif
        goto 1020
c
c       Check "A"  changes
c
  300   if (chgcrd(i-1)(1:2) .eq. chgcrd(i)(1:2) .and.
     &      chgcrd(i-1)(4:13) .eq. chgcrd(i)(4:13)) then
           if (chgcrd(i-1)(3:3) .eq. chgcrd(i)(3:3)) then
              write (errbuf(1),310)
  310         format (' Duplicate change records :')
              write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                              chgcrd(i-1)(1:80)
  320         format ('   Change card no. ',a4,' (',a80,')')
              write (errbuf(3),320) chgcrd(i)(122:125),chgcrd(i)(1:80)
              call prterx ('W',3)
           else if (index(' RMD', chgcrd(i-1)(3:3)) .lt. 
     1              index(' RMD', chgcrd(i)(3:3))) then
              write (errbuf(1),332)
  332         format (' Multiple changes combined:')
              write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                              chgcrd(i-1)(1:80)
              write (errbuf(3),320) chgcrd(i)(122:125), chgcrd(i)(1:80)
              call prterx ('W',3)
           else if (chgcrd(i-1)(3:3) .eq. 'M' .and.
     1              chgcrd(i)(3:3) .eq. 'M') then
              write (errbuf(1),332)
              write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                              chgcrd(i-1)(1:80)
              write (errbuf(3),320) chgcrd(i)(122:125), chgcrd(i)(1:80)
              call prterx ('W',3)
           else
              write (errbuf(1),330)
  330         format (' Incompatible change records :')
              write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                              chgcrd(i-1)(1:80)
              write (errbuf(3),320) chgcrd(i)(122:125),
     &                              chgcrd(i)(1:80)
              call prterx ('W',3)
           endif
        endif
        go to 1020
c
c       Check "I"  changes
c
  340   if (chgcrd(i-1)(4:13).eq.chgcrd(i)(4:13)) then
           if (chgcrd(i-1)(15:24).eq.chgcrd(i)(15:24)) then
              if (chgcrd(i-1)(3:3).eq.chgcrd(i)(3:3)) then
                 write (errbuf(1),310)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     1                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              else if (index(' RMD', chgcrd(i-1)(3:3)) .lt. 
     1                 index(' RMD', chgcrd(i)(3:3))) then
                 write (errbuf(1),332)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     &                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              else if (chgcrd(i-1)(3:3) .eq. 'M' .and.
     1                 chgcrd(i)(3:3) .eq. 'M') then
                 write (errbuf(1),332)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     &                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              else if (chgcrd(i-1)(126:126) .ne. 'P' .and.
     &                 chgcrd(i)(126:126)   .ne. 'P') then
                 write (errbuf(1),330)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1              chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     1              chgcrd(i)(1:80)
                 call prterx ('W',3)
              endif
           endif
        endif
        go to 1020
c
c       Check "P" changes
c
  400   if (chgcrd(i-1)(2:2) .eq. chgcrd(i)(2:2) .and.
     1      chgcrd(i-1)(4:6) .eq. chgcrd(i)(4:6) .and.
     2      chgcrd(i-1)(35:80) .eq. chgcrd(i)(35:80)) then
           write (errbuf(1),310)
           write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                           chgcrd(i-1)(1:80)
           write (errbuf(3),320) chgcrd(i)(122:125), chgcrd(i)(1:80)
           call prterx ('W',3)
        endif
        go to 1020
c
c       Check "Z" changes
c
  500   continue
        go to 1020
c
c       Check "B"  changes
c
  600   if (chgcrd(i-1)(7:14) .eq. chgcrd(i)(7:14)) then
           read (chgcrd(i-1)(15:19),610,err=1018) base1
  610      format (f4.0)
           read (chgcrd(i)(15:19),610,err=1018) base2
           if (base1 .eq. base2) then
              if (chgcrd(i-1)(3:3) .eq. chgcrd(i)(3:3) .and.
     &            chgcrd(i-1)(126:126) .ne. 'P' .and.
     &            chgcrd(i)(126:126)   .ne. 'P' .and.
     &            chgcrd(i-1)(121:121) .ne. 'P' .and.
     &            chgcrd(i-1)(121:121) .ne. 'U' .and.
     &            chgcrd(i)(121:121)   .ne. 'P' .and.
     &            chgcrd(i)(121:121)   .ne. 'U') then
                 write (errbuf(1),310)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     1                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              else if (index(' RMD', chgcrd(i-1)(3:3)) .lt. 
     &                 index(' RMD', chgcrd(i)(3:3)) .and.
     &                 chgcrd(i-1)(126:126) .ne. 'P' .and.
     &                 chgcrd(i)(126:126)   .ne. 'P' .and.
     &                 chgcrd(i-1)(121:121) .ne. 'P' .and.
     &                 chgcrd(i-1)(121:121) .ne. 'U' .and.
     &                 chgcrd(i)(121:121)   .ne. 'P' .and.
     &                 chgcrd(i)(121:121)   .ne. 'U') then
                 write (errbuf(1),332)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     &                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              else if (chgcrd(i-1)(3:3) .eq. 'M' .and.
     1                 chgcrd(i)(3:3) .eq. 'M') then
                 write (errbuf(1),332)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     &                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              else if (chgcrd(i-1)(126:126) .ne. 'P' .and.
     &                 chgcrd(i)(126:126)   .ne. 'P' .and.
     &                 chgcrd(i-1)(121:121) .ne. 'P' .and.
     &                 chgcrd(i-1)(121:121) .ne. 'U' .and.
     &                 chgcrd(i)(121:121)   .ne. 'P' .and.
     &                 chgcrd(i)(121:121)   .ne. 'U') then
                 write (errbuf(1),330)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     1                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              endif
           endif
        endif
        go to 1020
c
c       Check "+"  changes
c
  700   if (chgcrd(i-1)(7:14) .eq. chgcrd(i)(7:14)) then
           read (chgcrd(i-1)(15:19),610,err=1018) base1
           read (chgcrd(i)(15:19),610,err=1018) base2
           if (base1 .eq. base2) then
              if (chgcrd(i-1)(2:2) .eq. chgcrd(i)(2:2) .and.
     1            chgcrd(i-1)(4:6) .eq. chgcrd(i)(4:6) .and.
     2            chgcrd(i-1)(19:20) .eq. chgcrd(i)(19:20)) then
                 if (chgcrd(i-1)(3:3) .eq. chgcrd(i)(3:3) .and.
     &               chgcrd(i-1)(126:126) .ne. 'P' .and.
     &               chgcrd(i)(126:126)   .ne. 'P' .and.
     &               chgcrd(i-1)(121:121) .ne. 'P' .and.
     &               chgcrd(i-1)(121:121) .ne. 'U' .and.
     &               chgcrd(i)(121:121)   .ne. 'P' .and.
     &               chgcrd(i)(121:121)   .ne. 'U') then
                    write (errbuf(1),310)
                    write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                    chgcrd(i-1)(1:80)
                    write (errbuf(3),320) chgcrd(i)(122:125),
     1                                    chgcrd(i)(1:80)
                    call prterx ('W',3)
                 else if (index(' RMD', chgcrd(i-1)(3:3)) .lt. 
     &                    index(' RMD', chgcrd(i)(3:3)) .and.
     &                    chgcrd(i-1)(126:126) .ne. 'P' .and.
     &                    chgcrd(i)(126:126)   .ne. 'P' .and.
     &                    chgcrd(i-1)(121:121) .ne. 'P' .and.
     &                    chgcrd(i-1)(121:121) .ne. 'U' .and.
     &                    chgcrd(i)(121:121)   .ne. 'P' .and.
     &                    chgcrd(i)(121:121)   .ne. 'U') then
                    write (errbuf(1),332)
                    write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                    chgcrd(i-1)(1:80)
                    write (errbuf(3),320) chgcrd(i)(122:125),
     1                                    chgcrd(i)(1:80)
                    call prterx ('W',3)
                 else if (chgcrd(i-1)(3:3) .eq. 'M' .and.
     1                    chgcrd(i)(3:3) .eq. 'M') then
                    write (errbuf(1),332)
                    write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                    chgcrd(i-1)(1:80)
                    write (errbuf(3),320) chgcrd(i)(122:125),
     &                                    chgcrd(i)(1:80)
                    call prterx ('W',3)
                 else if (chgcrd(i-1)(126:126) .ne. 'P' .and.
     &                    chgcrd(i)(126:126)   .ne. 'P' .and.
     &                    chgcrd(i-1)(121:121) .ne. 'P' .and.
     &                    chgcrd(i-1)(121:121) .ne. 'U' .and.
     &                    chgcrd(i)(121:121)   .ne. 'P' .and.
     &                    chgcrd(i)(121:121)   .ne. 'U') then
                    write (errbuf(1),330)
                    write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                    chgcrd(i-1)(1:80)
                    write (errbuf(3),320) chgcrd(i)(122:125),
     1                                    chgcrd(i)(1:80)
                    call prterx ('W',3)
                 endif
              endif
           endif
        endif
        go to 1020
C
c       Check "X"  changes
c
  800   if (chgcrd(i-1)(7:14) .eq. chgcrd(i)(7:14)) then
           read (chgcrd(i-1)(15:19),610,err=1018) base1
           read (chgcrd(i)(15:19),610,err=1018) base2
           if (base1 .eq. base2) then
              if (chgcrd(i-1)(3:3) .eq. chgcrd(i)(3:3) .and.
     &            chgcrd(i-1)(126:126) .ne. 'P' .and.
     &            chgcrd(i)(126:126)   .ne. 'P' .and.
     &            chgcrd(i-1)(121:121) .ne. 'P' .and.
     &            chgcrd(i-1)(121:121) .ne. 'U' .and.
     &            chgcrd(i)(121:121)   .ne. 'P' .and.
     &            chgcrd(i)(121:121)   .ne. 'U') then
                 write (errbuf(1),310)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     1                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              else if (index(' RMD', chgcrd(i-1)(3:3)) .lt. 
     &                 index(' RMD', chgcrd(i)(3:3)) .and.
     &                 chgcrd(i-1)(126:126) .ne. 'P' .and.
     &                 chgcrd(i)(126:126)   .ne. 'P' .and.
     &                 chgcrd(i-1)(121:121) .ne. 'P' .and.
     &                 chgcrd(i-1)(121:121) .ne. 'U' .and.
     &                 chgcrd(i)(121:121)   .ne. 'P' .and.
     &                 chgcrd(i)(121:121)   .ne. 'U') then
                 write (errbuf(1),332)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     &                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              else if (chgcrd(i-1)(3:3) .eq. 'M' .and.
     1                 chgcrd(i)(3:3) .eq. 'M') then
                 write (errbuf(1),332)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     &                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              else if (chgcrd(i-1)(126:126) .ne. 'P' .and.
     &                 chgcrd(i)(126:126)   .ne. 'P' .and.
     &                 chgcrd(i-1)(121:121) .ne. 'P' .and.
     &                 chgcrd(i-1)(121:121) .ne. 'U' .and.
     &                 chgcrd(i)(121:121)   .ne. 'P' .and.
     &                 chgcrd(i)(121:121)   .ne. 'U') then
                 write (errbuf(1),330)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     1                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              endif
           endif
        endif
        go to 1020
C
c       Check "QP", "QX", and "QN" changes
c
  810   if (chgcrd(i-1)(7:14) .eq. chgcrd(i)(7:14)) then
           read (chgcrd(i-1)(15:19),610,err=1018) base1
           read (chgcrd(i)(15:19),610,err=1018) base2
           if (base1 .eq. base2) then
              if (chgcrd(i)(1:2) .eq. chgcrd(i-1)(1:2)) then
                 if (chgcrd(i-1)(3:3) .eq. chgcrd(i)(3:3) .and.
     &               chgcrd(i-1)(126:126) .ne. 'P' .and.
     &               chgcrd(i)(126:126)   .ne. 'P' .and.
     &               chgcrd(i-1)(121:121) .ne. 'P' .and.
     &               chgcrd(i-1)(121:121) .ne. 'U' .and.
     &               chgcrd(i)(121:121)   .ne. 'P' .and.
     &               chgcrd(i)(121:121)   .ne. 'U') then
                    write (errbuf(1),310)
                    write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                    chgcrd(i-1)(1:80)
                    write (errbuf(3),320) chgcrd(i)(122:125),
     1                                    chgcrd(i)(1:80)
                    call prterx ('W',3)
                 else if (index(' RMD', chgcrd(i-1)(3:3)) .lt. 
     &                    index(' RMD', chgcrd(i)(3:3)) .and.
     &                    chgcrd(i-1)(126:126) .ne. 'P' .and.
     &                    chgcrd(i)(126:126)   .ne. 'P' .and.
     &                    chgcrd(i-1)(121:121) .ne. 'P' .and.
     &                    chgcrd(i-1)(121:121) .ne. 'U' .and.
     &                    chgcrd(i)(121:121)   .ne. 'P' .and.
     &                    chgcrd(i)(121:121)   .ne. 'U') then
                    write (errbuf(1),332)
                    write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                    chgcrd(i-1)(1:80)
                    write (errbuf(3),320) chgcrd(i)(122:125),
     1                                    chgcrd(i)(1:80)
                    call prterx ('W',3)
                 else if (chgcrd(i-1)(3:3) .eq. 'M' .and.
     1                    chgcrd(i)(3:3) .eq. 'M') then
                    write (errbuf(1),332)
                    write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                    chgcrd(i-1)(1:80)
                    write (errbuf(3),320) chgcrd(i)(122:125),
     &                                    chgcrd(i)(1:80)
                    call prterx ('W',3)
                 else if (chgcrd(i-1)(126:126) .ne. 'P' .and.
     &                    chgcrd(i)(126:126)   .ne. 'P' .and.
     &                    chgcrd(i-1)(121:121) .ne. 'P' .and.
     &                    chgcrd(i-1)(121:121) .ne. 'U' .and.
     &                    chgcrd(i)(121:121)   .ne. 'P' .and.
     &                    chgcrd(i)(121:121)   .ne. 'U') then
                    write (errbuf(1),330)
                    write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                    chgcrd(i-1)(1:80)
                    write (errbuf(3),320) chgcrd(i)(122:125),
     1                                    chgcrd(i)(1:80)
                    call prterx ('W',3)
                 endif
              endif
           endif
        endif
        go to 1020
c
c       Check "R"  changes
c
  900   if (chgcrd(i-1)(7:14) .eq. chgcrd(i)(7:14) .and.
     1      chgcrd(i-1)(20:27) .eq. chgcrd(i)(20:27)) then
           read (chgcrd(i-1)(15:19),610,err=1018) base1
           read (chgcrd(i)(15:19),610,err=1018) base2
           read (chgcrd(i-1)(28:31),610,err=1018) base3
           read (chgcrd(i)(28:31),610,err=1018) base4
           if (base1 .eq. base2 .and. base3 .eq. base4) then
              if (chgcrd(i-1)(3:3) .eq. chgcrd(i)(3:3) .and.
     &            chgcrd(i-1)(126:126) .ne. 'P' .and.
     &            chgcrd(i)(126:126)   .ne. 'P' .and.
     &            chgcrd(i-1)(121:121) .ne. 'P' .and.
     &            chgcrd(i-1)(121:121) .ne. 'U' .and.
     &            chgcrd(i)(121:121)   .ne. 'P' .and.
     &            chgcrd(i)(121:121)   .ne. 'U' .and.
     &            chgcrd(i-1)(121:121) .eq. ' ') then
                 write (errbuf(1),310)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     1                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              else if (index(' RMD', chgcrd(i-1)(3:3)) .lt. 
     &                 index(' RMD', chgcrd(i)(3:3)) .and.
     &                 chgcrd(i-1)(126:126) .ne. 'P' .and.
     &                 chgcrd(i)(126:126)   .ne. 'P' .and.
     &                 chgcrd(i-1)(121:121) .ne. 'P' .and.
     &                 chgcrd(i-1)(121:121) .ne. 'U' .and.
     &                 chgcrd(i)(121:121)   .ne. 'P' .and.
     &                 chgcrd(i)(121:121)   .ne. 'U') then
                 write (errbuf(1),332)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     &                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              else if (chgcrd(i-1)(3:3) .eq. 'M' .and.
     1                 chgcrd(i)(3:3) .eq. 'M') then
                 write (errbuf(1),332)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     &                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              else if (chgcrd(i-1)(126:126) .ne. 'P' .and.
     &                 chgcrd(i)(126:126)   .ne. 'P' .and.
     &                 chgcrd(i-1)(121:121) .ne. 'P' .and.
     &                 chgcrd(i-1)(121:121) .ne. 'U' .and.
     &                 chgcrd(i)(121:121)   .ne. 'P' .and.
     &                 chgcrd(i)(121:121)   .ne. 'U') then
                 write (errbuf(1),330)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     1                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              endif
           endif
        endif
        go to 1020
c
c       Check "RZ"  changes
c
  910   if (chgcrd(i-1)(7:14) .eq. chgcrd(i)(7:14) .and.
     1      chgcrd(i-1)(20:27) .eq. chgcrd(i)(20:27)) then
           read (chgcrd(i-1)(15:19),610,err=1018) base1
           read (chgcrd(i)(15:19),610,err=1018) base2
           read (chgcrd(i-1)(28:31),610,err=1018) base3
           read (chgcrd(i)(28:31),610,err=1018) base4
           if (base1 .eq. base2 .and. base3 .eq. base4) then
              read (chgcrd(i-1)(33:33),1010) sec1
              read (chgcrd(i)(33:33),1010) sec2
              if (chgcrd(i-1)(32:32) .eq. chgcrd(i)(32:32) .and.
     1           sec1 .eq. sec2) then
                 if (chgcrd(i-1)(3:3) .eq. chgcrd(i)(3:3) .and.
     &               chgcrd(i-1)(126:126) .ne. 'P' .and.
     &               chgcrd(i)(126:126)   .ne. 'P' .and.
     &               chgcrd(i-1)(121:121) .ne. 'P' .and.
     &               chgcrd(i-1)(121:121) .ne. 'U' .and.
     &               chgcrd(i)(121:121)   .ne. 'P' .and.
     &               chgcrd(i)(121:121)   .ne. 'U' .and.
     &               chgcrd(i-1)(121:121) .eq. ' ') then
                    write (errbuf(1),310)
                    write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                    chgcrd(i-1)(1:80)
                    write (errbuf(3),320) chgcrd(i)(122:125),
     1                                    chgcrd(i)(1:80)
                    call prterx ('W',3)
                 else if (index(' RMD', chgcrd(i-1)(3:3)) .lt. 
     &                    index(' RMD', chgcrd(i)(3:3)) .and.
     &                    chgcrd(i-1)(126:126) .ne. 'P' .and.
     &                    chgcrd(i)(126:126)   .ne. 'P' .and.
     &                    chgcrd(i-1)(121:121) .ne. 'P' .and.
     &                    chgcrd(i-1)(121:121) .ne. 'U' .and.
     &                    chgcrd(i)(121:121)   .ne. 'P' .and.
     &                    chgcrd(i)(121:121)   .ne. 'U') then
                    write (errbuf(1),332)
                    write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                    chgcrd(i-1)(1:80)
                    write (errbuf(3),320) chgcrd(i)(122:125),
     1                                    chgcrd(i)(1:80)
                    call prterx ('W',3)
                 else if (chgcrd(i-1)(3:3) .eq. 'M' .and.
     1                    chgcrd(i)(3:3) .eq. 'M') then
                    write (errbuf(1),332)
                    write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                    chgcrd(i-1)(1:80)
                    write (errbuf(3),320) chgcrd(i)(122:125),
     &                                    chgcrd(i)(1:80)
                    call prterx ('W',3)
                 else if (chgcrd(i-1)(126:126) .ne. 'P' .and.
     &                    chgcrd(i)(126:126)   .ne. 'P' .and.
     &                    chgcrd(i-1)(121:121) .ne. 'P' .and.
     &                    chgcrd(i-1)(121:121) .ne. 'U' .and.
     &                    chgcrd(i)(121:121)   .ne. 'P' .and.
     &                    chgcrd(i)(121:121)   .ne. 'U') then
                    write (errbuf(1),330)
                    write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                    chgcrd(i-1)(1:80)
                    write (errbuf(3),320) chgcrd(i)(122:125),
     1                                    chgcrd(i)(1:80)
                    call prterx ('W',3)
                 endif
              endif
           endif
        endif
        go to 1020
c
c       Check "LET"  changes
c
 1000   if (chgcrd(i-1)(7:14) .eq. chgcrd(i)(7:14) .and.
     1      chgcrd(i-1)(20:27) .eq. chgcrd(i)(20:27)) then
           read (chgcrd(i-1)(15:19),610,err=1018) base1
           read (chgcrd(i)(15:19),610,err=1018) base2
           read (chgcrd(i-1)(28:31),610,err=1018) base3
           read (chgcrd(i)(28:31),610,err=1018) base4
           read (chgcrd(i-1)(33:33),1010) sec1
 1010      format (i1)
           read (chgcrd(i)(33:33),1010) sec2
           if (base1 .eq. base2 .and. base3 .eq. base4 .and.
     1         chgcrd(i-1)(32:32) .eq. chgcrd(i)(32:32) .and.
     2         sec1 .eq. sec2) then
              if (chgcrd(i-1)(3:3) .eq. chgcrd(i)(3:3) .and.
     &            chgcrd(i-1)(126:126) .ne. 'P' .and.
     &            chgcrd(i)(126:126)   .ne. 'P' .and.
     &            chgcrd(i-1)(121:121) .ne. 'P' .and.
     &            chgcrd(i-1)(121:121) .ne. 'U' .and.
     &            chgcrd(i)(121:121)   .ne. 'P' .and.
     &            chgcrd(i)(121:121)   .ne. 'U') then
                 write (errbuf(1),310)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     1                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              else if (index(' RMD', chgcrd(i-1)(3:3)) .lt. 
     &                 index(' RMD', chgcrd(i)(3:3)) .and.
     &                 chgcrd(i-1)(126:126) .ne. 'P' .and.
     &                 chgcrd(i)(126:126)   .ne. 'P' .and.
     &                 chgcrd(i-1)(121:121) .ne. 'P' .and.
     &                 chgcrd(i-1)(121:121) .ne. 'U' .and.
     &                 chgcrd(i)(121:121)   .ne. 'P' .and.
     &                 chgcrd(i)(121:121)   .ne. 'U') then
                 write (errbuf(1),332)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     &                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              else if (chgcrd(i-1)(3:3) .eq. 'M' .and.
     1                 chgcrd(i)(3:3) .eq. 'M') then
                 write (errbuf(1),332)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     &                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              else if (chgcrd(i-1)(126:126) .ne. 'P' .and.
     &                 chgcrd(i)(126:126)   .ne. 'P' .and.
     &                 chgcrd(i-1)(121:121) .ne. 'P' .and.
     &                 chgcrd(i-1)(121:121) .ne. 'U' .and.
     &                 chgcrd(i)(121:121)   .ne. 'P' .and.
     &                 chgcrd(i)(121:121)   .ne. 'U') then
                 write (errbuf(1),330)
                 write (errbuf(2),320) chgcrd(i-1)(122:125),
     1                                 chgcrd(i-1)(1:80)
                 write (errbuf(3),320) chgcrd(i)(122:125),
     1                                 chgcrd(i)(1:80)
                 call prterx ('W',3)
              endif
           endif
        endif
        go to 1020

 1018   write (errbuf(1), 1019)
 1019   format (' Illegal data in a numeric field ')
        write (errbuf(2), 320) chgcrd(i)(122:125), chgcrd(i)(1:80)
        call prterx ('W',2)
 1020   lsttyp = thstyp
 1040   continue

        return
        end
