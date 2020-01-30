C    @(#)chprea.f	20.8 3/29/99
        subroutine chprea
 
c       Change subroutine No. 5
c
c       1. Preprocess all add/restore changes
c       2. Retrieve restored data from DELETE() array
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/delete.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/wsccbase.inc'

        common /scratch/ scratch( 22*MAXBUS )
        integer array(MAXCHG), pointer(MAXCHG)
        equivalence (array, scratch(1)), 
     &              (pointer, scratch(MAXCHG+1))

        common /is_batch / is_batch

        character cbtype*1, cbyear*2, cbown*3, id*1, bus1*8, bus2*8,
     &            sect, txpose*126, tempc*126, lntype*2, tx_flag*1
        integer find_bus, komp_del, find_del, kmpchg, add_bus
        external komp_del, swap_del, kmpchg, swpchg
        logical found, first

        save

        if (nadd .eq. 0 .and. nrename .eq. 0) return

        do i = 1, numchg
           pointer(i) = 0
        enddo
c
c       First pass - assign bus number to add/restore name 
c
        do ic = 1, numchg

           if (chgcrd(ic)(1:1) .eq. 'B' .and.
     &        (chgcrd(ic)(3:3) .eq. ' ' .or.
     &         chgcrd(ic)(3:3) .eq. 'R') .and.
     &         chgcrd(ic)(126:126) .eq. ' ') then

              read (chgcrd(ic)(122:125), '(bz, i4)') nc
              bus1 = chgcrd(ic)(7:14)
              read (chgcrd(ic)(15:18), '(bz, f4.0)') base1
c
c             Obtain new hash index. Function add_bus has multiple
c             return values:
c             
c             add_bus > 0 indicates a legitimate index and 
c                         automatically installs bus1 and base1 into
c                         bus(ibus), base(ibus)
c                     < 0 indicates an existing entity (duplicate)
c                     = 0 indicates overflow.
c
              ibus = add_bus (bus1, base1, ntot+1)
              if (ibus .lt. 0) then
                 write (errbuf(1), 80) chgcrd(ic)(1:31), nc
   80            format (' Add/Restore bus already in system, change ', 
     &              a, ' No. ', i5)
                 call prterx ('W',1)                                            
                 chgcrd(ic)(126:126) = 'E'
              else if (ibus .eq. 0) then                                      
                 write (errbuf(1), 90) MAXBUS                                   
   90            format (' More than ',i5,' buses in base system ')            
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
                 chgcrd(ic)(126:126) = 'E'
              else
                 ntot = ibus
                 if (wsccflag) then
                    wsccbase(ntot) = chgcrd(ic)(15:18)
                 endif
              endif
           endif                                                             
        enddo
c
c       Second pass - augment chgcrd() set with data from delete().
c
        call sortbus
        if (ndelete .gt. 0) call qiksrt (1, ndelete, komp_del, swap_del)

        next = numchg
        do ic = 1, numchg

        if (index ('B+XQLRET', chgcrd(ic)(1:1)) .ne. 0 .and.
     &      chgcrd(ic)(3:3) .eq. 'R' .and.
     &      chgcrd(ic)(126:126) .eq. ' ') then

           read (chgcrd(ic)(122:125), '(bz, i4)') nc
           bus1 = chgcrd(ic)(7:14)
           read (chgcrd(ic)(15:18), '(bz, f4.0)') base1
           ibus = find_bus (bus1, base1)

           if (ibus .le. 0) then
              write (errbuf(1), 110) chgcrd(ic)(1:33), nc
  110         format( ' Bus1 not found for change ', a, ' No. ',i5)
              if (is_batch .eq. 0) then
                 call prterx ('E',1)
              else
                 call prterx ('F',1)
              endif
              chgcrd(ic)(126:126) = 'E'
              go to 260
           endif

           jc = find_del(chgcrd(ic))

           if (jc .le. 0) then
              write (errbuf(1), 120) chgcrd(ic)(1:33), nc
  120         format( ' No restore data available for change ', a, 
     &            ' No. ',i5)
              if (is_batch .eq. 0) then
                 call prterx ('E',1)
              else
                 call prterx ('F',1)
              endif
              chgcrd(ic)(126:126) = 'E'
              go to 260
           endif

           if (chgcrd(ic)(1:1) .eq. 'B') then
c
c             Process restore buses
c
              if (delete(jc)(1:1) .eq. 'B') then
                 chgcrd(ic)(1:2) = delete(jc)(1:2)
                 chgcrd(ic)(4:120) = delete(jc)(4:)
                 delete(jc)(3:3) = '*'   ! Nullify delete record
                 found = .true.
                 jc = jc + 1
                 do while (jc .le. ndelete .and. found)
                    if (delete(jc)(3:3) .ne. '*') then
                       bus1 = delete(jc)(7:14)
                       read (delete(jc)(15:18), '(bz, f4.0)') base1
                       jbus = find_bus (bus1, base1)
                       if (jbus .eq. ibus) then
                          if (index ('LERT', delete(jc)(1:1)) .ne. 0) 
     &                        then
                             tx_flag = delete(jc)(3:3)
                             bus2 = delete(jc)(20:27)
                             read (delete(jc)(28:31), '(bz, f4.0)') 
     &                             base2
                             kbus = find_bus (bus2, base2)
                             if (kbus .gt. 0) then
                                if (next .ge. MAXCHG) then
                                   write (errbuf(1), 130) MAXCHG
                                   if (is_batch .eq. 0) then
                                      call prterx ('E',1)
                                   else
                                      call prterx ('F',1)
                                   endif
                                   go to 900
                                endif
                                next = next + 1
                                write (chgcrd(next), 140) delete(jc), 
     &                             next
                                chgcrd(next)(3:3) = chgcrd(ic)(3:3)
                                if (tx_flag .eq. 'T') 
     &                             chgcrd(next)(121:121) = 'T'
c
c                               Nullify delete record
c
                                delete(jc)(3:3) = '*' 
c
c                               Pick up branch transpose if available
c
                                tempc = txpose (chgcrd(next)(1:90))
                                jx = find_del (tempc)
                                if (jx .gt. 0) then
                                   if (next .ge. MAXCHG) then
                                      write (errbuf(1), 130) MAXCHG
                                      if (is_batch .eq. 0) then
                                         call prterx ('E',1)
                                      else
                                         call prterx ('F',1)
                                      endif
                                      go to 900
                                   endif
                                   next = next + 1
c
c                                  tag transpose record with "next-1"
c
                                   write (chgcrd(next), 140) delete(jx),
     &                                 next - 1
                                   chgcrd(next)(3:3) = chgcrd(ic)(3:3)
c
c                                  Nullify delete record
c
                                   delete(jx)(3:3) = '*' 
                                endif
                             endif
                          else
                             if (next .ge. MAXCHG) then
                                write (errbuf(1), 130) MAXCHG
  130                           format('Too many change cards,  limit=',
     &                            i5)
                                if (is_batch .eq. 0) then
                                   call prterx ('E',1)
                                else
                                   call prterx ('F',1)
                                endif
                                go to 900
                             endif
                             next = next + 1
                             write (chgcrd(next), 140) delete(jc), nc
  140                        format (a120, 1x, i4, ' ')
                             chgcrd(next)(3:3) = chgcrd(ic)(3:3)
c
c                            Nullify delete record
c
                             delete(jc)(3:3) = '*' 
                          endif
                       else
                          found = .false.
                       endif
                    endif
                    jc = jc + 1
                 enddo
              else
                 write (errbuf(1), 120) chgcrd(ic)(1:33), nc
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
                 chgcrd(ic)(126:126) = 'E'
                 go to 260
              endif

           else if (chgcrd(ic)(1:1) .eq. '+') then
c
c             Process restore continuation buses
c
              found = .true.
              first = .false.   ! Replaced chgcrd(ic) 

              do while (jc .le. ndelete .and. found)
                 bus1 = delete(jc)(7:14)
                 read (delete(jc)(15:18), '(bz, f4.0)') base1
                 jbus = find_bus (bus1, base1)
                 if (jbus .eq. ibus) then
c
c                   Match type-owner-year, recognizing wild cards
c
                    if (delete(jc)(1:1) .eq. '+' .and. 
     &                  delete(jc)(3:3) .ne. '*') then
                       cbtype = chgcrd(ic)(2:2)
                       cbown = chgcrd(ic)(4:6)
                       cbyear = chgcrd(ic)(19:20)
                       if (cbtype .eq. '*' .or. 
     &                     cbtype .eq. delete(jc)(2:2)) then
                           if (cbown .eq. '***' .or. 
     &                         cbown .eq. delete(jc)(4:6)) then
                              if (cbyear .eq. '**' .or. 
     &                            cbyear .eq. delete(jc)(19:20)) then
                                 if (.not. first) then
                                    chgcrd(ic)(1:2) = delete(jc)(1:2)
                                    chgcrd(ic)(4:120) = delete(jc)(4:)
c
c                                   Nullify delete record
c
                                    delete(jc)(3:3) = '*'  
                                    first = .true.
                                 else
                                    if (next .ge. MAXCHG) then
                                       write (errbuf(1), 130) MAXCHG
                                       if (is_batch .eq. 0) then
                                          call prterx ('E',1)
                                       else
                                          call prterx ('F',1)
                                       endif
                                       go to 900
                                    endif
                                    next = next + 1
                                    write (chgcrd(next), 140) 
     &                                 delete(jc), nc
c
c                                   Nullify delete record
c
                                    delete(jc)(3:3) = '*'  
                                    chgcrd(next)(3:3) = chgcrd(ic)(3:3)
                                endif
                             endif
                          endif
                       endif
                    endif
                 else
                    found = .false.
                 endif
                 jc = jc + 1
              enddo            
              if (.not. first) then
                 write (errbuf(1), 120) chgcrd(ic)(1:33), nc
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
                 chgcrd(ic)(126:126) = 'E'
                 go to 260
              endif

           else if (chgcrd(ic)(1:1) .eq. 'X') then
c
c             Process restore switched reactance restore buses
c
              found = .true.
              first = .false.   ! Replaced chgcrd(ic) 

              bus1 = delete(jc)(7:14)
              read (delete(jc)(15:18), '(bz, f4.0)') base1
              jbus = find_bus (bus1, base1)
              if (jbus .eq. ibus) then
                 if (delete(jc)(1:1) .eq. 'X') then
                    chgcrd(ic)(1:2) = delete(jc)(1:2)
                    chgcrd(ic)(4:120) = delete(jc)(4:)
                    first = .true.
                    delete(jc)(3:3) = '*' ! Nullify delete record
                 endif
              endif
              if (.not. first) then
                 write (errbuf(1), 120) chgcrd(ic)(1:33), nc
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
                 chgcrd(ic)(126:126) = 'E'
                 go to 260
              endif

           else if (chgcrd(ic)(1:1) .eq. 'Q') then
c
c             Process restore pqcurves data
c
              found = .true.
              first = .false.   ! Replaced chgcrd(ic) 

              do while (jc .le. ndelete .and. found .and. .not. first)
                 bus1 = delete(jc)(7:14)
                 read (delete(jc)(15:18), '(bz, f4.0)') base1
                 jbus = find_bus (bus1, base1)
                 if (jbus .eq. ibus) then
                    if (delete(jc)(1:1) .eq. 'Q') then
                       chgcrd(ic)(1:2) = delete(jc)(1:2)
                       chgcrd(ic)(4:120) = delete(jc)(4:)
                       first = .true.
                       delete(jc)(3:3) = '*' ! Nullify delete record
                    else
                       jc = jc + 1
                    endif
                 else
                    found = .false.
                 endif
              enddo            
              if (.not. first) then
                 write (errbuf(1), 120) chgcrd(ic)(1:33), nc
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
                 chgcrd(ic)(126:126) = 'E'
                 go to 260
              endif

           else if (index('LERT', chgcrd(ic)(1:1)) .ne. 0) then
c
c             Process restore branches
c
              found = .true.
              first = .false.   ! Replaced chgcrd(ic) 
c
c             Note: "jc" obtained from find_del(chgcrd(ic)) will
c             randomly match any parallel or section if id = '*'.
C             Backspace counter "jc" in this case.
c
              delete(ndelete+1) = chgcrd(ic)
              jx = jc - 1
              do while (jx .gt. 0 .and.
     &                 (komp_del(jx, ndelete+1) .eq. 0))
                 if (delete(jx)(3:3) .ne. '*') jc = jx
                 jx = jx - 1
              enddo

              bus2 = delete(jc)(20:27)
              read (delete(jc)(28:31), '(bz, f4.0)') base2
              lbus = find_bus (bus2, base2)

              lntype = chgcrd(ic)(1:2)
              id = chgcrd(ic)(32:32)
              sect = chgcrd(ic)(33:33)

              do while (jc .le. ndelete .and. found .and.
     &                 (index('LERT', lntype(1:1)) .ne. 0))
                 tx_flag = delete(jc)(3:3)
                 bus1 = delete(jc)(7:14)
                 read (delete(jc)(15:18), '(bz, f4.0)') base1
                 jbus = find_bus (bus1, base1)
                 if (jbus .eq. ibus) then
                    bus2 = delete(jc)(20:27)
                    read (delete(jc)(28:31), '(bz, f4.0)') base2
                    kbus = find_bus (bus2, base2)
                    if (kbus .gt. 0 .and. kbus .eq. lbus) then
c
c                      Match id-sect, recognizing wild cards
c
                       if (delete(jc)(1:2) .eq. lntype .or.
     &                     id .eq. '*' .or. 
     &                     (delete(jc)(33:33) .ne. ' ' .and. 
     &                      sect .eq. '0')) then
                          if (id .eq. '*' .or. 
     &                        id .eq. delete(jc)(32:32)) then
                             if (sect .eq. '0' .or. 
     &                           sect .eq. delete(jc)(33:33)) then
                                if (.not. first) then
                                   chgcrd(ic)(1:2) = delete(jc)(1:2)
                                   chgcrd(ic)(4:120) = delete(jc)(4:)
c
c                                  Nullify delete record
c
                                   delete(jc)(3:3) = '*' 
                                   first = .true.
                                   if (tx_flag .eq. 'T') 
     &                                chgcrd(ic)(121:121) = 'T'
                                else
                                   if (next .ge. MAXCHG) then
                                      write (errbuf(1), 130) MAXCHG
                                      if (is_batch .eq. 0) then
                                         call prterx ('E',1)
                                      else
                                         call prterx ('F',1)
                                      endif
                                      go to 900
                                   endif
                                   next = next + 1
                                   write (chgcrd(next), 140) delete(jc),
     &                                nc
c
c                                  Nullify delete record
c
                                   delete(jc)(3:3) = '*' 
                                   chgcrd(next)(3:3) = chgcrd(ic)(3:3)
                                   if (tx_flag .eq. 'T') 
     &                                chgcrd(next)(121:121) = 'T'
                                endif
                             endif
                          endif
                       endif
                    else
                       found = .false.
                    endif
                    jc = jc + 1
                 else
                    found = .false.
                 endif
              enddo            
              if (.not. first) then
                 write (errbuf(1), 120) chgcrd(ic)(1:33), nc
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
  260   continue
        enddo

        if (next .gt. numchg) then
C
C          Compress deleted items in change()
C
           jc = 0
           do i = 1, ndelete
C
C             Check for deleted bus
C
              if (delete(i)(3:3) .ne. '*') then
                 jc = jc + 1
                 if (jc .ne. i) then
                    delete(jc) = delete(i)
                 endif
              endif
           enddo
           ndelete = jc
           numchg = next
c
c          sort change array
c
           do i = 1, numchg
              array(i) = i
           enddo
           call qiksrt (1, numchg, kmpchg, swpchg)
c
c          check for duplicates
c
           call chkchdup 

        endif

  900   continue
        return
        end
