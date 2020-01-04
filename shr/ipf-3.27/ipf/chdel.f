C    @(#)chdel.f	20.9 11/12/98
        subroutine chdel
 
c       change subroutine no. 3: process all change deletes
c
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/agc.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/delete.inc'
        include 'ipfinc/oldtbx.inc'
        include 'ipfinc/pqcurves.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/xdata.inc'
 
        common /scratch/ scratch( 22*MAXBUS )
        integer array(MAXCHG), pointer(MAXCHG)
        equivalence (array, scratch(1)), (pointer, scratch(MAXCHG+1))

        common /is_batch / is_batch

        integer find_bus, compare, pold, ptr, sect
        character cbtype*1, cbown*3, cbyear*2, id*1, bus1*8, bus2*8,
     &            bus1d*8, bus2d*8, sectc*1
        logical found, sections
        external komp_del, swap_del
 
        if (numchg .le. 1) go to 800
c
c       *******************************************************
c       set up pointers to any P/Q curve data
c
c       Store temporary X-data pointer in busxdtptr() and P/Q curve
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
                                                                                
        ntoto = ntot
        nbsdel = 0
        nxddel = 0
        ncbdel = 0
        nbrdel = 0
c
c       Preliminary cleanup - delete all bonepile entities which
c       will be superseded with wild-card deletions, e.g., bus
c       deletions, branch deletions with ID = * or section = 0.
c
        do ic = 1,numchg
c
c          The first test pertains to bus deletions.
c 
           if (chgcrd(ic)(1:1) .eq. 'B' .and.
     &         chgcrd(ic)(3:3) .eq. 'D' .and.
     &         chgcrd(ic)(126:126) .eq. ' ') then

              bus1 = chgcrd(ic)(7:14)
              read (chgcrd(ic)(15:18), '(bz, f4.0)') base1
 
              do i = 1, ndelete
                 if (index ('B+XQ', delete(i)(1:1)) .ne. 0 .and.
     &               delete(i)(3:3) .ne. '*') then
                    bus1d = delete(i)(7:14)
                    read (delete(i)(15:18), '(bz, f4.0)') base1d
                    if (bus1 .eq. bus1d .and. base1 .eq. base1d) then
                       delete(i)(3:3) = '*'   ! Nullify delete record
                    endif
                 else if (index ('LRET', delete(i)(1:1)) .ne. 0
     &                .and. delete(i)(3:3) .ne. '*') then
                    bus1d = delete(i)(7:14)
                    read (delete(i)(15:18), '(bz, f4.0)') base1d
                    bus2d = delete(i)(20:27)
                    read (delete(i)(28:31), '(bz, f4.0)') base2d
                    if ((bus1 .eq. bus1d .and. base1 .eq. base1d) .or.
     &                  (bus1 .eq. bus2d .and. base1 .eq. base2d)) then
                       delete(i)(3:3) = '*'   ! Nullify delete record
                    endif
                 endif
              enddo
c
c          The second test pertains to wild-card branch deletions.
c 
           else if (index ('LRET', chgcrd(ic)(1:1)) .ne. 0 .and.
     &              chgcrd(ic)(3:3) .eq. 'D' .and.
     &             (chgcrd(ic)(32:32) .eq. '*' .or.
     &              chgcrd(ic)(33:33) .eq. '0') .and.
     &              chgcrd(ic)(126:126) .eq. ' ') then

              bus1 = chgcrd(ic)(7:14)
              read (chgcrd(ic)(15:18), '(bz, f4.0)') base1
              bus2 = chgcrd(ic)(20:27)
              read (chgcrd(ic)(28:31), '(bz, f4.0)') base2
              id = chgcrd(ic)(32:32)
              sectc = chgcrd(ic)(33:33)
  
              do i = 1, ndelete
                 if (index ('LRET', delete(i)(1:1)) .ne. 0 .and. 
     &               delete(i)(3:3) .ne. '*') then
                    bus1d = delete(i)(7:14)
                    read (delete(i)(15:18), '(bz, f4.0)') base1d
                    bus2d = delete(i)(20:27)
                    read (delete(i)(28:31), '(bz, f4.0)') base2d
                    if ((bus1 .eq. bus1d .and. base1 .eq. base1d .and.
     &                   bus2 .eq. bus2d .and. base2 .eq. base2d) .or.
     &                  (bus1 .eq. bus2d .and. base1 .eq. base2d .and.
     &                   bus2 .eq. bus1d .and. base1 .eq. base2d)) then
                       if (id .eq. '*') then
                          delete(i)(3:3) = '*'   ! Nullify delete record
                       else if (id .eq. delete(i)(32:32) .and. 
     &                          sectc .eq. '0') then
                          delete(i)(3:3) = '*'   ! Nullify delete record
                       endif
                    endif
                 endif
              enddo
           endif
        enddo
C
C       Compress deleted items in change()
C
        jc = 0
        do i = 1, ndelete
C
C          Check for deleted items
C
           if (delete(i)(3:3) .ne. '*') then
              jc = jc + 1
              if (jc .ne. i) then
                 delete(jc) = delete(i)
              endif
           endif
        enddo
        ndelete = jc
        if (ndelete .gt. 0) call qiksrt (1, ndelete, komp_del, swap_del)
c
c       First pass - delete all non-bus entities. (If the bus is 
c       deleted before other deletions are processed, those other
c       items cannot be found in the network data!)
c
        do 580 ic = 1,numchg
 
        if (index ('+XQLRET', chgcrd(ic)(1:1)) .ne. 0 .and.
     &      chgcrd(ic)(3:3) .eq. 'D' .and.
     &      chgcrd(ic)(126:126) .eq. ' ') then

           read (chgcrd(ic)(122:125), '(bz, i4)') nc
           bus1 = chgcrd(ic)(7:14)
           read (chgcrd(ic)(15:18), '(bz, f4.0)') base1
           ibus = find_bus (bus1, base1)

           if (ibus .le. 0) then
              write (errbuf(1),120) chgcrd(ic)(1:33), nc
  120         format( ' Bus1 not found for change ', a, ' No. ',i5)
              if (is_batch .eq. 0) then
                call prterx ('E',1)
              else
                call prterx ('F',1)
              endif
              chgcrd(ic)(126:126) = 'E'
              goto 580

           else if (chgcrd(ic)(1:1) .eq. '+') then

C             Process continuation bus deletions
 
              pold = 0
              ncb = kbsdta(15,ibus)
              found = .false.

              do while (ncb .gt. 0 .and. .not. found)
                 call getchr (1, cbtype, kbctbl(8,ncb))
                 call getchr (3, cbown, kbctbl(10,ncb))
                 call getchr (2, cbyear, kbctbl(9,ncb))
                 compare = kompr (chgcrd(ic)(2:2) // 
     &                            chgcrd(ic)(4:6) //
     &                            chgcrd(ic)(19:20), 
     &                            cbtype // cbown // cbyear, junk)
                 if (compare .lt. 0) then
                    ncb = 0
                 else if (compare .eq. 0) then
                    found = .true.
                 else
                    pold = ncb
                    ncb = bctbl_nxt(ncb)
                 endif
              enddo
              if (.not. found) then
                 write (errbuf(1), 130) chgcrd(ic)(1:33), nc
  130            format( ' +Bus not found for change ', a, ' No. ',i5)
                 if (is_batch .eq. 0) then
                   call prterx ('E',1)
                 else
                   call prterx ('F',1)
                 endif
                 chgcrd(ic)(126:126) = 'E'
                 goto 580
              endif 

              call mvcbde(ibus, ic, pold, ncb)

              capcor(1,ibus) = 0.0
              capcor(2,ibus) = -9.0e10
              ncbdel = ncbdel + 1
              chgcrd(ic)(126:126) = 'P'  ! Set process flag
              goto 580

           else if (chgcrd(ic)(1:1) .eq. 'X') then

C             Process X data deletions
 
              kxd = busxdtptr(ibus)
              if (kxd .eq. 0) then
                 write (errbuf(1), 140) chgcrd(ic)(1:33), nc
  140            format( ' X-data record not found for change ', a, 
     &                   ' No. ',i5)
                 if (is_batch .eq. 0) then
                   call prterx ('E',1)
                 else
                   call prterx ('F',1)
                 endif
                 chgcrd(ic)(126:126) = 'E'
                 goto 580
              else
                 call mvxdde(ibus, ic, kxd)
                 busxdtptr(ibus) = 0
                 nxddel = nxddel + 1
                 chgcrd(ic)(126:126) = 'P'  ! Set process flag
              endif
              goto 580

           else if (chgcrd(ic)(1:2) .eq. 'QP' .or.
     &              chgcrd(ic)(1:2) .eq. 'QX' .or. 
     &              chgcrd(ic)(1:2) .eq. 'QN') then

C             Process pqcurve data deletions. 
c             Note: deleting any record 'QP', 'QX', or 'QN' deletes
c             the set.
 
              kpqd = buspqptr(ibus)
              if (kpqd .eq. 0) then
                 write (errbuf(1), 140) chgcrd(ic)(1:33), nc
  142            format( ' PQ record not found for change ', a, 
     &                   ' No. ',i5)
                 if (is_batch .eq. 0) then
                   call prterx ('E',1)
                 else
                   call prterx ('F',1)
                 endif
                 chgcrd(ic)(126:126) = 'E'
                 goto 580
              else
                 call mvqpde(ibus, chgcrd(ic)(1:2), kpqd)
                 buspqptr(ibus) = 0
                 chgcrd(ic)(126:126) = 'P'  ! Set process flag
              endif
              goto 580

           else if (index ('LRET', chgcrd(ic)(1:1)) .ne. 0) then

C             Process line deletions
 
              bus1 = chgcrd(ic)(7:14)
              read (chgcrd(ic)(15:18), '(bz, f4.0)') base1
              bus2 = chgcrd(ic)(20:27)
              read (chgcrd(ic)(28:31), '(bz, f4.0)') base2
              ibus = find_bus (bus1, base1)
              jbus = find_bus (bus2, base2)
    
              if (ibus .le. 0) then
                 write (errbuf(1), 150) chgcrd(ic)(1:33), nc
  150            format( ' Bus1 not found for change ', a, ' No. ',i5)
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
                 chgcrd(ic)(126:126) = 'E'
                 goto 580
              else if (jbus .le. 0) then
                 write (errbuf(1), 160) chgcrd(ic)(1:33), nc
  160            format( ' Bus2 not found for change ', a, ' No. ',i5)
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
                 chgcrd(ic)(126:126) = 'E'
                 goto 580
              endif

              found = .false.
              pold = 0
              ptr = kbsdta(16,ibus)
              id = chgcrd(ic)(32:32)
              read (chgcrd(ic)(33:33), '(bz, i1)') ksect
              do while (ptr .gt. 0 .and. .not. found)
                 compare = kompr(bus2, bus(ky(ptr)), junk)
                 if (compare .eq. 0) then
                    compare = 100.0 * (base2 - base(ky(ptr)))
                 endif
                 if (compare .lt. 0) then
                    ptr = 0
                 else if (compare .gt. 0) then
                    pold = ptr
                    ptr = brnch_nxt(ptr)
                 else 
                    if (id .eq. '*') then
                       found = .true.
C
C                      The following branch types have no parallel id:
C                      LM, LD, and R
C
                    else if ((chgcrd(ic)(1:2) .eq. 'LM' .and.
     &                        brtype(ptr) .eq. 2) .or. 
     &                       (chgcrd(ic)(1:2) .eq. 'LD' .and.
     &                        brtype(ptr) .eq. 7) .or. 
     &                       (chgcrd(ic)(1:1) .eq. 'R' .and.
     &                        brtype(ptr) .eq. 4)) then
C
C                      These branch types have no parallel id:
C                      LM, LD, and R
C
                       found = .true.

                    else if (brtype(ptr) .eq. 2 .or. 
     &                       brtype(ptr) .eq. 7 .or. 
     &                       brtype(ptr) .eq. 4) then
                       pold = ptr
                       ptr = brnch_nxt(ptr)

                    else
C
C                      Check parallel ID
C
                       compare = kompr (id, brid(ptr), junk)
                       if (compare .lt. 0) then
                          ptr = 0
                       else if (compare .gt. 0) then
                          pold = ptr
                          ptr = brnch_nxt(ptr)
                       else if (compare .eq. 0) then
c
c                         Check sections (sect > 0 means ascending,
c                                              < 0 means decending)

                          if (chgcrd(ic)(1:1) .eq. 'R' .and.
     &                        brtype(ptr) .eq. 4) then
                             found = .true.
                          else if (chgcrd(ic)(1:1) .eq. 'R' .and.
     &                        brtype(ptr) .ne. 4) then
                              pold = ptr
                              ptr = brnch_nxt(ptr)
                          else if (chgcrd(ic)(1:1) .ne. 'R' .and.
     &                        brtype(ptr) .eq. 4) then
                              pold = ptr
                              ptr = brnch_nxt(ptr)
                          else if (brtype(ptr) .eq. 1) then
                             sections = .true.
C
C                            Check 0 and non-0 section mix.
C
                             if (ksect .eq. 0) then
                                found = .true.
                             else 
                                pold = ptr
                                ptr = brnch_nxt(ptr)
                             endif
                          else if (brsect(ptr) .eq. 0) then
                             if (ksect .ne. 0) then
                                write (errbuf(1), 164) chgcrd(ic)(1:33),
     &                             nc
  164                           format(' Line has an illegal section num
     &ber for change ', a, ' No. ',i5)
                                if (is_batch .eq. 0) then
                                   call prterx ('E',1)
                                else
                                   call prterx ('F',1)
                                endif
                                chgcrd(ic)(126:126) = 'E'
                                go to 580
                             else
                                found = .true.
                             endif
                          else 
                             if (ksect .eq. 0) then
                                write (errbuf(1), 166) chgcrd(ic)(1:33),
     &                             nc
  166                           format(' Line is missing section number 
     &for change ', a, ' No. ',i5)
                                if (is_batch .eq. 0) then
                                   call prterx ('E',1)
                                else
                                   call prterx ('F',1)
                                endif
                                chgcrd(ic)(126:126) = 'E'
                                go to 580
                             endif
                             sect = isign (ptr, brnch_ptr(ptr))
                             if (ksect .gt. 0 .and. 
     &                           brsect(ptr) .eq. 0) then
                                compare = +1
                             else if (sect .lt. 0) then
                                compare = -(ksect - brsect(ptr))
                             else 
                                compare = ksect - brsect(ptr)
                             endif
                             if (compare .lt. 0) then
                                ptr = 0
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
                                   found = .true.
                                else if (brtype(ptr) .eq. 9 .and. 
     &                                   chgcrd(ic)(1:1) .eq. 'R') then
                                   pold = ptr
                                   ptr = brnch_nxt(ptr)
                                else if (brtype(ptr) .ne. 9 .and. 
     &                                   chgcrd(ic)(1:1) .eq. 'R') then
                                   ptr = 0
                                else
                                   found = .true.
                                endif
                             endif
                          endif
                       endif
                    endif
                 endif
              enddo

              if (.not. found) then
                 if (chgcrd(ic)(121:121) .ne. 'P' .and.
     &               chgcrd(ic)(121:121) .ne. 'U') then
                     write (errbuf(1), 170) chgcrd(ic)(1:33), nc
  170                format( ' Branch not found for change ', a, 
     &                 ' No. ',i5)
                     if (is_batch .eq. 0) then
                        call prterx ('E',1)
                     else
                        call prterx ('F',1)
                     endif
                 endif
                 chgcrd(ic)(126:126) = 'E'
                 goto 580
              endif
c
c             Now delete the branches located.
 
              if (id .eq. '*') then
 
c                Delete all branches between buses
 
                 found = .true.
                 do while (found)
c
c                   Store branch pointers.  The branch cannot be
c                   deleted yet since the double-entry deletion
c                   will call mvbrde() twice.
c
                    nbrdel = nbrdel + 1
                    array(nbrdel) = brnch_ptr(ptr)

                    call mvbrde(ibus, ic, pold, ptr)
                    if (pold .eq. 0) then
                       ptr = kbsdta(16,ibus)
                    else
                       ptr = brnch_nxt(pold)  ! Old entity "ptr" has 
C                                             ! been deleted.
                    endif
                    if (ptr .gt. 0) then
                       if (ky(ptr) .ne. jbus) then
                          found = .false.
                       endif
                    else
                       found = .false.
                    endif
                 enddo
              else if (ksect .eq. 0) then
 
c                Delete all sections in current parallel 
 
                 found = .true.
                 do while (found)

                    nbrdel = nbrdel + 1
                    array(nbrdel) = brnch_ptr(ptr)

                    call mvbrde(ibus, ic, pold, ptr)
                    if (pold .eq. 0) then
                       ptr = kbsdta(16,ibus)
                    else
                       ptr = brnch_nxt(pold)  ! Old entity "ptr" has 
C                                             ! been deleted.
                    endif
                    if (ptr .gt. 0) then
                       if (ky(ptr) .ne. jbus .or. brid(ptr) .ne. id)
     &                     then
                          found = .false.
                       else if ((chgcrd(ic)(1:2) .eq. 'LM' .and.
     &                           brtype(ptr) .ne. 2) .or. 
     &                          (chgcrd(ic)(1:2) .eq. 'LD' .and.
     &                           brtype(ptr) .ne. 7) .or. 
     &                          (chgcrd(ic)(1:1) .eq. 'R' .and.
     &                           brtype(ptr) .ne. 4)) then
                          found = .false.
                       endif
                    else
                       found = .false.
                    endif
                 enddo
              else
                 nbrdel = nbrdel + 1
                 array(nbrdel) = brnch_ptr(ptr)
                 call mvbrde(ibus, ic, pold, ptr)
              endif
              chgcrd(ic)(126:126) = 'P'  ! Set process flag

           endif
        endif
  580   continue
c
c       Complete branch deletions
c
        do ic = 1, nbrdel
           nbr = array(ic)
           if (nbr .gt. 0) then
              do i = 1, 18
                 kbrnch(i,nbr) = 0
              enddo
              do i = 1, 3
                 rateln(i,nbr) = 0.0
              enddo
           endif
        enddo
 
C       Second pass - process all bus deletions here
 
        do 600 ic = 1, numchg
 
        if (chgcrd(ic)(1:1) .eq. 'B' .and.
     &      chgcrd(ic)(3:3) .eq. 'D' .and.
     &      chgcrd(ic)(126:126) .eq. ' ') then

           read (chgcrd(ic)(122:125), '(bz, i4)') nc
           bus1 = chgcrd(ic)(7:14)
           read (chgcrd(ic)(15:18), '(bz, f4.0)') base1
           ibus = find_bus (bus1, base1)

           if (ibus .le. 0) then
              write (errbuf(1),120) chgcrd(ic)(1:33), nc
              if (is_batch .eq. 0) then
                 call prterx ('E',1)
              else
                 call prterx ('F',1)
              endif
              chgcrd(ic)(126:126) = 'E'
              goto 600
           endif

C          Move bus data to deleted array
 
           nbsdel = nbsdel + 1
           call mvbude(ibus, ic)

           chgcrd(ic)(126:126) = 'P'  ! Set process flag
        endif
  600   continue
C
C       Compress delete items in XDATA
C
        jc = 0
        do i = 1, kxtot
C
C          Check for deleted bus
C
           if (xdata(1,i) .gt. 0.0) then
              jc = jc + 1
              if (jc .ne. i) then
                 do j = 1,22
                    xdata(j,jc) = xdata(j,i)
                 enddo
              endif
           endif
        enddo
        kxtot = jc
C
C       Compress delete items in OLDTBX
C
        jc = 0
        do i = 1, numtbx
C
C          Check for deleted bus
C
           if (bus(oldtbx(2,i)) .ne. 'srtlst') then
              jc = jc + 1
              if (jc .ne. i) then
                 do j = 1,8
                    oldtbx(j,jc) = oldtbx(j,i)
                 enddo
              endif
           endif
        enddo
        numtbx = jc
C
C       Compress deleted items in KAGC
C
        jc = 0
        do i = 1, numagc
C
C          Check for deleted bus
C
           if (bus(kagc(1,i)) .ne. 'srtlst') then
              jc = jc + 1
              if (jc .ne. i) then
                 do j = 1, 14
                    kagc(j,jc) = kagc(j,i)
                 enddo
              endif
           else
              xdt_flag = .false.
           endif
        enddo
        numagc = jc

  800   continue
        return
        end
