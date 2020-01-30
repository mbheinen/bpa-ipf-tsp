C    @(#)chadd.f	20.8 11/12/98
        subroutine chadd
 
c       change subroutine no. 5
c       process all add/restore data
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/pqcurves.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/xdata.inc'
 
        common /scratch/ scratch( 22*MAXBUS )
        integer array(MAXCHG), pointer(MAXCHG)
        equivalence (array, scratch(1)), 
     &              (pointer, scratch(MAXCHG+1))

        common /is_batch / is_batch

        character cbtype*1, cbyear*2, cbown*3, id*1, bus1*8, bus2*8,
     &            txpose*126, chg_temp*126
        integer pold, ptr, sect, compare, gtbrtype, find_bus
        logical found, debug, sections

        if (nadd .eq. 0) go to 280

        debug = (kase1(27) .ne. 0) 
c
c       *******************************************************
c       set up pointers to any P/Q curve data
c
c       Store X-data pointer in busxdtptr() and P/Q curve data pointer 
c       in buspqptr()
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

        do i = 1, numchg
           pointer(i) = 0
           array(i) = 0
        enddo

        do 260 ic = 1, numchg
 
        if (index ('B+XQLRET', chgcrd(ic)(1:1)) .ne. 0 .and.
     &     (chgcrd(ic)(3:3) .eq. ' ' .or. chgcrd(ic)(3:3) .eq. 'R') 
     &     .and. chgcrd(ic)(126:126) .eq. ' ') then

           read (chgcrd(ic)(122:125), '(bz, i4)') nc
           bus1 = chgcrd(ic)(7:14)
           read (chgcrd(ic)(15:18), '(bz, f4.0)') base1
           ibus = find_bus (bus1, base1)
           array(ic) = ibus

           if (ibus .le. 0) then
              write (errbuf(1), 110) chgcrd(ic)(1:33), nc
  110         format( ' Bus1 not found for change ', a, ' No. ',i5)
              if (is_batch .eq. 0) then
                 call prterx ('E',1)
              else
                 call prterx ('F',1)
              endif
              chgcrd(ic)(126:126) = 'E'
              goto 260
           endif

           if (chgcrd(ic)(1:1) .eq. 'B') then
c
c             Add bus additions
c
              call mvchbu(ic, ibus)
              chgcrd(ic)(126:126) = 'P'  ! Set process flag
              goto 260

           else if (chgcrd(ic)(1:1) .eq. '+') then
c
c             Add +bus additions
c
              pold = 0
              icb = 0
              ncb = kbsdta(15,ibus)  
              do while (ncb .gt. 0 .and. icb .eq. 0)
                 call getchr (1, cbtype, kbctbl(8,ncb))
                 call getchr (3, cbown, kbctbl(10,ncb))
                 call getchr (2, cbyear, kbctbl(9,ncb)) 
                 compare = kompr (chgcrd(ic)(2:2) //
     &                            chgcrd(ic)(4:6) // 
     &                            chgcrd(ic)(19:20), 
     &                            cbtype // cbown // cbyear, junk)
                 if (compare .lt. 0) then
c
c                   Insert current entity between pold and ncb
c
                    if (ntot2 + 1 .ge. MAXCBS) then
                       write (errbuf(1), 120) chgcrd(ic)(1:33), nc
  120                  format(' Too many +buses added, record ',a,
     &                    ' no. ',i5)
                       if (is_batch .eq. 0) then
                          call prterx ('E',1)
                       else
                          call prterx ('F',1)
                       endif
                       chgcrd(ic)(126:126) = 'E'
                       go to 260
                    endif
                    ntot2 = ntot2 + 1
                    icb = ntot2
                    bctbl_nxt(ntot2) = ncb
                    if (pold .eq. 0) then
                       kbsdta(15,ibus) = ntot2
                    else
                       bctbl_nxt(pold) = ntot2
                    endif
                 else if (compare .eq. 0) then
                    write (errbuf(1), 130) chgcrd(ic)(1:33), nc
  130               format( ' +Bus already in system, record ', a, 
     &                  ' no. ',i5)
                    if (is_batch .eq. 0) then
                       call prterx ('E',1)
                    else
                       call prterx ('F',1)
                    endif
                    chgcrd(ic)(126:126) = 'E'
                    go to 260
                 else
                    pold = ncb
                    ncb = bctbl_nxt(ncb)
                 endif
              enddo
              if (icb .eq. 0) then
                 if (ntot2 + 1 .ge. MAXCBS) then
                    write (errbuf(1), 120) chgcrd(ic)(1:33), nc
                    if (is_batch .eq. 0) then
                       call prterx ('E',1)
                    else
                       call prterx ('F',1)
                    endif
                    chgcrd(ic)(126:126) = 'E'
                    go to 260
                 endif
                 call mvchcb(ic, ntot2+1, ibus)
                 if (chgcrd(ic)(126:126) .ne. 'E') then
                    chgcrd(ic)(126:126) = 'P'  ! Set process flag
                    ntot2 = ntot2 + 1
                    icb = ntot2
                    bctbl_nxt(ntot2) = ncb
                    if (pold .eq. 0) then
                       kbsdta(15,ibus) = ntot2
                    else
                       bctbl_nxt(pold) = ntot2
                    endif
                 endif
              else
                 call mvchcb(ic, icb, ibus)
                 chgcrd(ic)(126:126) = 'P'  ! Set process flag
              endif 
 
           else if (chgcrd(ic)(1:1) .eq. 'X') then
c
c             Add Xbus additions at end of xdata and sort later
c
              kxd = busxdtptr(ibus)
              if (kxd .gt. 0) then
                 write (errbuf(1), 140) chgcrd(ic)(1:33), nc
  140            format( ' X-data record is already in system, ', a, 
     &                  ' no. ',i5)
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
                 chgcrd(ic)(126:126) = 'E'
                 go to 260
              else if (kxtot+1 .ge. MAXXDT) then
                 write (errbuf(1), 150) chgcrd(ic)(1:33), nc
  150            format(' Too many X-data records added, ',a,
     &                    ' no. ',i5)
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
                 chgcrd(ic)(126:126) = 'E'
                 go to 260
              endif
              kxtot = kxtot + 1 
              ilx = kxtot
              call mvchxd(ic, ilx, ibus)
              totrek = 0.0
              totcap = 0.0
 
              do k = 7, 21, 2
                 xtot = xdata(k,ilx) * xdata(k+1,ilx)
                 totrek = totrek + amin1(0.0,xtot)
                 totcap = totcap + amax1(0.0,xtot)
              enddo
 
              xdata(3,ilx) = totrek
              xdata(4,ilx) = totcap
              xdata(5,ilx) = totrek
              xdata(6,ilx) = totcap
              chgcrd(ic)(126:126) = 'P'  ! Set process flag

           else if (chgcrd(ic)(1:1) .eq. 'Q') then
c
c             Add pqcurves data to end of pqpgen(), etc. 
c
              kpqd = buspqptr(ibus)
              if (kpqd .eq. 0 .and. chgcrd(ic)(1:2) .eq. 'QP') then
                 if(numcurv+1 .ge. MAXCRV) then
                 write (errbuf(1), 154) chgcrd(ic)(1:33), nc
  154            format(' Too many P/Q curve records added, ', a,
     &                    ' no. ',i5)
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
                 chgcrd(ic)(126:126) = 'E'
                 go to 260
              endif
                 numcurv = numcurv + 1
                 kpqd = numcurv 
                 buspqptr(ibus) = kpqd
           endif
              call mvchpq(ic, chgcrd(ic)(1:2), ibus, kpqd)
              chgcrd(ic)(126:126) = 'P'  ! Set process flag

           else if (index ('LERT', chgcrd(ic)(1:1)) .ne. 0) then
c
c             Add line additions
c
              bus2 = chgcrd(ic)(20:27)
              read (chgcrd(ic)(28:31), '(bz, f4.0)') base2
              jbus = find_bus (bus2, base2)

              if (jbus .le. 0) then
                 write (errbuf(1), 160) chgcrd(ic)(1:33), nc
  160            format( ' Bus2 not found for change ', a, ' No. ',i5)
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
                 chgcrd(ic)(126:126) = 'E'
                 goto 260
              endif

C             Determine branch pointers for insertion: 
c             pold -> ltot2 -> ptr.
 
              found = .false.
              sections = .false.
              id = chgcrd(ic)(32:32)
              read (chgcrd(ic)(33:33), '(bz, i1)') ksect
              pold = 0
              ptr = kbsdta(16,ibus)
              do while (ptr .gt. 0 .and. .not. found)
                 compare = kompr(bus2, bus(ky(ptr)), junk)
                 if (compare .eq. 0) then
                    compare = 100.0 * (base2 - base(ky(ptr)))
                 endif
                 if (compare .lt. 0) then
                    found = .true.
                 else if (compare .gt. 0) then
                    pold = ptr
                    ptr = brnch_nxt(ptr)
                 else 
C
C                   The following branch types have no parallel id:
C                   LM, LD, and R
C
                    if (chgcrd(ic)(1:2) .eq. 'LM' .or. 
     &                  chgcrd(ic)(1:2) .eq. 'LD') then
c
c                      d-c line
c 
                       write (errbuf(1), 170) chgcrd(ic)(1:33), nc
  170                  format(' D-c line is already in system for change
     & ', a, ' No. ',i5)
                       if (is_batch .eq. 0) then
                          call prterx ('E',1)
                       else
                          call prterx ('F',1)
                       endif
                       chgcrd(ic)(126:126) = 'E'
                       goto 260
                    else if (chgcrd(ic)(1:2) .eq. 'RZ') then
                       if (brtype(ptr) .eq. 9) then
                          write (errbuf(1), 180) chgcrd(ic)(1:33), nc
  180                     format(' R record is already in system, change
     & (', a, ' No. ',i5)
                          if (is_batch .eq. 0) then
                             call prterx ('E',1)
                          else
                             call prterx ('F',1)
                          endif
                          chgcrd(ic)(126:126) = 'E'
                          go to 260
                       else if (brtype(ptr) .eq. 1) then
                          write (errbuf(1), 190) chgcrd(ic)(1:33), nc
  190                     format(' Illegal Regulator added to line secti
     &ons, change ', a, ' No. ',i5)
                          if (is_batch .eq. 0) then
                             call prterx ('E',1)
                          else
                             call prterx ('F',1)
                          endif
                          chgcrd(ic)(126:126) = 'E'
                          go to 260
                       else if (brtype(ptr) .ne. 5 .and.
     &                          brtype(ptr) .ne. 6) then
                          write (errbuf(1), 200) chgcrd(ic)(1:33), nc
  200                     format(' Illegal Regulator added to non-transf
     &ormer, change ', a, ' No. ',i5)
                          if (is_batch .eq. 0) then
                             call prterx ('E',1)
                          else
                             call prterx ('F',1)
                          endif
                          chgcrd(ic)(126:126) = 'E'
                          go to 260
                       else
                          found = .true.
                       endif
                    else if (chgcrd(ic)(1:1) .eq. 'R') then
                       if (brtype(ptr) .eq. 4) then
                          write (errbuf(1), 180) chgcrd(ic)(1:33), nc
                          if (is_batch .eq. 0) then
                             call prterx ('E',1)
                          else
                             call prterx ('F',1)
                          endif
                          chgcrd(ic)(126:126) = 'E'
                          go to 260
                       else if (brtype(ptr) .ne. 5 .and.
     &                          brtype(ptr) .ne. 6) then
                          write (errbuf(1), 200) chgcrd(ic)(1:33), nc
                          if (is_batch .eq. 0) then
                             call prterx ('E',1)
                          else
                             call prterx ('F',1)
                          endif
                          chgcrd(ic)(126:126) = 'E'
                          go to 260
                       else
                          found = .true.
                       endif
                    else
C
C                      Check parallel ID
C
                       compare = kompr (chgcrd(ic)(32:32), brid(ptr),
     &                                  junk)
                       if (compare .lt. 0) then
                          found = .true.
                       else if (compare .gt. 0) then
                          pold = ptr
                          ptr = brnch_nxt(ptr)
                       else if (compare .eq. 0) then
                          if (brtype(ptr) .eq. 1) then
                             sections = .true.
C
C                            Check 0 and non-0 section mix.
C
                             if (ksect .eq. 0) then
                                write (errbuf(1), 210) chgcrd(ic)(1:33),
     &                             nc
  210                           format(' Line is missing section number 
     &for change ', a, ' No. ',i5)
                                if (is_batch .eq. 0) then
                                   call prterx ('E',1)
                                else
                                   call prterx ('F',1)
                                endif
                                chgcrd(ic)(126:126) = 'E'
                                go to 260
                             else 
                                pold = ptr
                                ptr = brnch_nxt(ptr)
                             endif
                          else if (brtype(ptr) .eq. 4) then
                             if (chgcrd(ic)(1:1) .eq. 'T') then
                                pold = ptr
                                ptr = brnch_nxt(ptr)
                             else if (chgcrd(ic)(1:1) .eq. 'L' .or.
     &                                chgcrd(ic)(1:1) .eq. 'E') then
                                write (errbuf(1), 212) chgcrd(ic)(1:33),
     &                             nc
  212                           format(' Illegal line added to a regulat
     &or, change ', a, ' No. ',i5)
                                if (is_batch .eq. 0) then
                                   call prterx ('E',1)
                                else
                                   call prterx ('F',1)
                                endif
                                chgcrd(ic)(126:126) = 'E'
                                go to 260
                             else
                                write (errbuf(1), 180) chgcrd(ic)(1:33),
     &                             nc
                                if (is_batch .eq. 0) then
                                   call prterx ('E',1)
                                else
                                   call prterx ('F',1)
                                endif
                                chgcrd(ic)(126:126) = 'E'
                                go to 260
                             endif
                          else if (brsect(ptr) .eq. 0) then
                             if (ksect .ne. 0) then
                                write (errbuf(1), 220) chgcrd(ic)(1:33),
     &                             nc
  220                           format(' Line has an illegal section num
     &ber for change ', a, ' No. ',i5)
                                if (is_batch .eq. 0) then
                                   call prterx ('E',1)
                                else
                                   call prterx ('F',1)
                                endif
                                chgcrd(ic)(126:126) = 'E'
                                go to 260
                             else
                                write (errbuf(1), 230) chgcrd(ic)(1:33),
     &                             nc
  230                           format(' Line is already in system for c
     &hange ', a, ' No. ',i5)
                                if (is_batch .eq. 0) then
                                   call prterx ('E',1)
                                else
                                   call prterx ('F',1)
                                endif
                                chgcrd(ic)(126:126) = 'E'
                                go to 260
                             endif
                          else 
                             if (ksect .eq. 0) then
                                write (errbuf(1), 240) chgcrd(ic)(1:33),
     &                             nc
  240                           format(' Line is missing section number 
     &for change ', a, ' No. ',i5)
                                if (is_batch .eq. 0) then
                                   call prterx ('E',1)
                                else
                                   call prterx ('F',1)
                                endif
                                chgcrd(ic)(126:126) = 'E'
                                go to 260
                             endif
c
c                            Check sections (sect > 0 means ascending,
c                                                 > 0 means decending)
c
                             sect = sign (ptr, brnch_ptr(ptr))
                             if (sect .lt. 0) then
                                compare = -(ksect - brsect(ptr))
                             else
                                compare = ksect - brsect(ptr)
                             endif
                             if (compare .lt. 0) then
                                found = .true.
                             else if (compare .gt. 0) then
                                pold = ptr
                                ptr = brnch_nxt(ptr)
                             else
c
c                               An "RZ" - E pair is the only legal
c                               combination at this point.
c
                                if (brtype(ptr) .eq. 9 .and. 
     &                              chgcrd(ic)(1:2) .eq. 'RZ') then
                                else if (brtype(ptr) .eq. 9 .and. 
     &                              chgcrd(ic)(1:1) .ne. 'R') then    
                                   compare = +1
                                   pold = ptr
                                   ptr = brnch_nxt(ptr)
                                else if (brtype(ptr) .ne. 9 .and. 
     &                              chgcrd(ic)(1:1) .eq. 'R') then    
                                   compare = -1
                                   found = .true.
                                endif
                                if (compare .eq. 0) then
                                   write (errbuf(1), 230) 
     &                               chgcrd(ic)(1:33), nc
                                   if (is_batch .eq. 0) then
                                      call prterx ('E',1)
                                   else
                                      call prterx ('F',1)
                                   endif
                                   chgcrd(ic)(126:126) = 'E'
                                   go to 260
                                endif
                             endif
                          endif
                       endif
                    endif
                 endif
              enddo
c
c             Search for original branch entity
c
              chg_temp = txpose (chgcrd(ic))
              found = .false.
              ix = 1
              do while (ix .le. numchg .and. .not. found)
                 if (ix .eq. ic) then
                    ix = ix + 1
                 else if (pointer(ix) .gt. 0 .and. 
     &                    index ('LERT', chgcrd(ix)(1:1)) .ne. 0) then
                    if (chg_temp(1:2) .eq. chgcrd(ix)(1:2) .and.
     &                  chg_temp(7:33) .eq. chgcrd(ix)(7:33)) then
                       found = .true.
                    else
                       ix = ix + 1
                    endif
                 else
                    ix = ix + 1
                 endif
              enddo
              if (found) then
                 il = -pointer(ix)
              else
                 il = ltot + 1
                 if (ltot + 1 .ge. MAXBRN) then
                    write (errbuf(1), 250) chgcrd(ic)(1:33), nc
  250               format(' Too many branches added at change ', a, 
     &                  ' No. ',i5)
                    if (is_batch .eq. 0) then
                       call prterx ('E',1)
                    else
                       call prterx ('F',1)
                    endif
                    chgcrd(ic)(126:126) = 'E'
                    ltot = 1
                    ltot2 = 1
                    go to 260
                 endif
                 ltot = ltot + 1
                 il = ltot
              endif
              pointer(ic) = il
c
c             Insert current entity between pold and ptr
c
              ltot2 = ltot2 + 1
              brnch_ptr(ltot2) = il
              brnch_nxt(ltot2) = ptr
              if (pold .eq. 0) then
                 kbsdta(16,ibus) = ltot2
              else
                 brnch_nxt(pold) = ltot2
              endif
              kx(ltot2) = ibus
              ky(ltot2) = jbus
              brid(ltot2) = id
              brsect(ltot2) = ksect
              brtype(ltot2) = gtbrtype (chgcrd(ic)(1:2))
              if (il .gt. 0) call mvchbr (ic, il)
 
C             Determine whether pi-equivalent must be added with 
c             sections
 
              if (chgcrd(ic)(1:2) .eq. 'LM') then
              else if (chgcrd(ic)(1:2) .eq. 'LD') then
c
c                Special processing is required here.  The section
c                actually stores the "I" or "R" control field for
c                LD records and brsect() must be updated.
c
                 if (chgcrd(ic)(56:56) .eq. 'R' .or.
     &               chgcrd(ic)(56:56) .eq. 'S') then
                    brsect(ltot2) = 1
                 else if (chgcrd(ic)(56:56) .eq. 'I' .or.
     &                    chgcrd(ic)(56:56) .eq. 'J') then
                    brsect(ltot2) = 2
                 endif

              else if (chgcrd(ic)(1:1) .eq. 'R' .and. 
     &                 chgcrd(ic)(2:2) .ne. 'Z') then
              else if (ksect .eq. 0) then
              else if (.not. sections) then
c
c                Insert pi-equivalent section before current entity
c
                 if (il .gt. 0) then
                    if (ltot + 1 .ge. MAXBRN) then
                       write (errbuf(1), 250) chgcrd(ic)(1:33), nc
                       if (is_batch .eq. 0) then
                          call prterx ('E',1)
                       else
                          call prterx ('F',1)
                       endif
                       chgcrd(ic)(126:126) = 'E'
                       ltot = 1
                       ltot2 = 1
                       go to 260
                    else 
                       ltot = ltot + 1
                       il = ltot
c
c                      Initialize equivalent pi data
c
                       do i = 1, 18
                          kbrnch(i,il) = 0
                       enddo
                       do i = 1, 3
                          rateln(i,il) = 0.0
                       enddo
                       call putchr(3, chgcrd(ic)(3:6), kbrnch(3,il))
                    endif

                 else
c
c                   Determine pointer "il" to transpose pi-equivalent
c                   branch. A direct search is necessary.
                    
                    iptr = kbsdta(16,jbus)
                    il = 0
                    do while (iptr .gt. 0 .and. il .eq. 0)
                       if (ky(iptr) .eq. ibus .and. 
     &                     brid(iptr) .eq. chgcrd(ic)(32:32) .and.
     &                     brtype(iptr) .eq. 1) then
                          il = -brnch_ptr(iptr)
                       else
                          iptr = brnch_nxt(iptr)
                       endif
                    enddo
                    if (iptr .eq. 0) then
                       write (errbuf(1), 252) chgcrd(ic)(1:33), nc
  252                  format( ' Pointer error for section equivalent ',
     &                     a, ' No. ',i5)
                       if (is_batch .eq. 0) then
                          call prterx ('E',1)
                       else
                          call prterx ('F',1)
                       endif
                       chgcrd(ic)(126:126) = 'E'
                       goto 260
                     endif
                 endif

                 ltot2 = ltot2 + 1
                 kx(ltot2) = ibus
                 ky(ltot2) = jbus
                 brid(ltot2) = id
                 brsect(ltot2) = 0
                 brtype(ltot2) = 1
                 brnch_nxt(ltot2) = ltot2-1
                 if (pold .eq. 0) then
                    kbsdta(16,ibus) = ltot2
                 else
                    brnch_nxt(pold) = ltot2
                 endif
                 brnch_ptr(ltot2) = il
              endif
              if (chgcrd(ic)(126:126) .eq. ' ') 
     &           chgcrd(ic)(126:126) = 'P'  ! Set process flag

              if (debug) call chkbrlnk (ibus, ic, 'CHADD')

           endif
        endif
  260   continue
c
c       Test for added buses without branches
c
        do ic = 1, numchg
 
           if (chgcrd(ic)(1:1) .eq. 'B' .and. array(ic) .ne. 0 .and.
     &        (chgcrd(ic)(3:3) .eq. ' ' .or. chgcrd(ic)(3:3) .eq. 'R'))
     &        then
              ibus = array(ic)
              if (kbsdta(16,ibus) .eq. 0) then
                 if (batch) then
                    write (errbuf(1), 270) chgcrd(ic)(1:33), nc
  270               format(' No branches added for bus ',a, ' No. ', i5)
                    if (is_batch .eq. 0) then
                       call prterx ('E',1)
                    else
                       call prterx ('F',1)
                    endif
                 else
                    write (errbuf(1), 270) chgcrd(ic)(1:33), nc
                    call prterx ('W',1)
                endif
              endif
           endif
        enddo
  280   continue
        return
        end
