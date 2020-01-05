C    @(#)chmods.f	20.9 11/12/98
        subroutine chmods
 
c       Change subroutine no. 7
c       process all change modifications
c
c       To do: 1. if nxdel > 0, compress xdata (see chdel)
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/pqcurves.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/xdata.inc'
 
        common /scratch/ scratch( 22*MAXBUS )
        integer array(MAXCHG), pointer(MAXCHG)
        equivalence (array, scratch(1)), (pointer, scratch(MAXCHG+1))

        common /is_batch / is_batch

        integer find_bus, compare, ptr, oldtyp, sect, pold
        character cbtype*1, cbown*3, cbyear*2, id*1, bus1*8, bus2*8
 
        logical found, sections
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
                                                                                
        nxdel = 0
        nbsmod = 0
        ncbmod = 0
        nxmod = 0
        nbrmod = 0

        do 580 ic = 1,numchg
 
        if (index ('B+XQLRET', chgcrd(ic)(1:1)) .eq. 0 .and.
     &      chgcrd(ic)(3:3) .eq. 'M' .and.
     &      chgcrd(ic)(126:126) .eq. ' ') then

          write (errbuf(1), 110) chgcrd(ic)(1:33), nc
  110     format( ' Illegal type "M" change ', a, ' No. ',i5)
          if (is_batch .eq. 0) then
             call prterx ('E',1)
          else
             call prterx ('F',1)
          endif
          chgcrd(ic)(126:126) = 'E'
          goto 580

        else if (index ('B+XQLRET', chgcrd(ic)(1:1)) .ne. 0 .and.
     &      chgcrd(ic)(3:3) .eq. 'M' .and.
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

           else if (chgcrd(ic)(1:1) .eq. 'B') then

              oldtyp = kbsdta(1,ibus)
              call mod_bus (ic, ibus)
              nbsmod = nbsmod + 1
              chgcrd(ic)(126:126) = 'P'  ! Set process flag
C
C             Reinitialize CAPCOR
C
              capcor(1,ibus) = 0.0
              capcor(2,ibus) = -9.0e10

           else if (chgcrd(ic)(1:1) .eq. '+') then

C             Process continuation bus modifications
 
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

              call mod_cbus (ic, ncb)
              ncbmod = ncbmod + 1
              chgcrd(ic)(126:126) = 'P'  ! Set process flag
C
C             Reinitialize CAPCOR
C
              capcor(1,ibus) = 0.0
              capcor(2,ibus) = -9.0e10

              goto 580

           else if (chgcrd(ic)(1:1) .eq. 'X') then

C             Process X data modifications
 
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
                 call mod_xdta (ic, kxd)
                 nxmod = nxmod + 1
                 chgcrd(ic)(126:126) = 'P'  ! Set process flag
              endif
              goto 580

           else if (chgcrd(ic)(1:2) .eq. 'QP' .or.
     &              chgcrd(ic)(1:2) .eq. 'QX' .or. 
     &              chgcrd(ic)(1:2) .eq. 'QN') then

C             Process pqcurve data modifications
 
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
                 call mod_pqdt (ic, chgcrd(ic)(1:2), kpqd)
                 nxmod = nxmod + 1
                 chgcrd(ic)(126:126) = 'P'  ! Set process flag
              endif
              goto 580

           else if (index ('LRET', chgcrd(ic)(1:1)) .ne. 0) then

C             Process line modifications
 
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
                 goto 580
              endif

              found = .false.
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
                    ptr = brnch_nxt(ptr)
                 else 
                    if ((chgcrd(ic)(1:2) .eq. 'LM' .and.
     &                   brtype(ptr) .eq. 2) .or. 
     &                  (chgcrd(ic)(1:2) .eq. 'LD' .and.
     &                   brtype(ptr) .eq. 7) .or. 
     &                  (chgcrd(ic)(1:1) .eq. 'R' .and.
     &                   brtype(ptr) .eq. 4)) then
C
C                      These branch types have no parallel id:
C                      LM, LD, and R
C
                       found = .true.
                    else if (chgcrd(ic)(1:1) .eq. 'R') then
                       ptr = 0
                       compare = -1
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

                          if (brtype(ptr) .eq. 1) then
                             sections = .true.
C
C                            Check 0 and non-0 section mix.
C
                             if (ksect .eq. 0) then
                                write (errbuf(1), 162) chgcrd(ic)(1:33),
     &                                                 nc
  162                           format(' Line is missing section ',
     1                                 'number for change ', a, 
     2                                 ' No. ',i5)
                                if (is_batch .eq. 0) then
                                   call prterx ('E',1)
                                else
                                   call prterx ('F',1)
                                endif
                                chgcrd(ic)(126:126) = 'E'
                                go to 580
                             else 
                                pold = ptr
                                ptr = brnch_nxt(ptr)
                             endif
                          else if (brsect(ptr) .eq. 0) then
                             if (ksect .ne. 0) then
                                write (errbuf(1), 164) chgcrd(ic)(1:33),
     &                                                 nc
  164                           format(' Line has an illegal section ',
     1                                 'number for change ', a, 
     2                                 ' No. ',i5)
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
     &                                                 nc
  166                           format(' Line is missing section ',
     1                                 'number for change ', a, 
     2                                 ' No. ',i5)
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
                             else if (sect .gt. 0) then
                                compare = ksect - brsect(ptr)
                             endif
                             if (compare .lt. 0) then
                                ptr = 0
                             else if (compare .gt. 0) then
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
                 write (errbuf(1), 170) chgcrd(ic)(1:33), nc
  170            format( ' Branch not found for change ', a, ' No. ',i5)
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
                 chgcrd(ic)(126:126) = 'E'
                 goto 580
              endif

              call mod_brn (ic, ptr)
c
c             If an LD record is modified, the control point is
c             stored in brsect(). 
c
              if (chgcrd(ic)(1:2) .eq. 'LD' .and.
     &            chgcrd(ic)(56:56) .ne. ' ') then
                 if (chgcrd(ic)(56:56) .eq. 'R' .or.
     &               chgcrd(ic)(56:56) .eq. 'S') then
                    brsect(ptr) = 1
                 else if (chgcrd(ic)(56:56) .eq. 'I' .or.
     &                    chgcrd(ic)(56:56) .eq. 'J') then
                    brsect(ptr) = 2
                 endif
              endif
              nbrmod = nbrmod + 1
              chgcrd(ic)(126:126) = 'P'

           endif
        endif

  580   continue

        if (nxdel .gt. 0) then
C
C          Compress delete items in XDATA
C
           jc = 0
           do i = 1, kxtot
C
C             Check for deleted bus
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
        endif

        return
        end
