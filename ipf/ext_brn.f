C    @(#)ext_brn.f	20.14 1/15/98
C ###
C ###  1. To do (8/26/97): Change PTI option for "R" record to use 
C ###     brnch_ptr(*) > 0 in lieu of KRMAP = 2.
c ###
C****************************************************************
C
C   	File: ext_brn.f
C
C   	Purpose: Write brnach data onto saved NETWORK_DATA file savfil.
C                             
C       Input parameters:
C
C             savfil   - the logical unit opened
C             dialect  - a character string (BPA, WSCC, WSCC1, WSCC2, 
c                        PTI) denoting the dialect of the WSCC record.
c             lenrec   - a integer 80 or 120 denoting the output record 
c                        size
c             ratcod   - a character string denoting extended 
c                        line ratings used (E- EXTENDED, N-NOMINAL, 
C                        or M-MINIMUM)
c             sections - a character string <null> or "PSEUDOBUSES"
c                        denoting how line sections are to be
c                        represented
c             type_e   - a character string <null> or "SYMMETRIC"
c                        denoting how assymetric type E-branches are
c                        to be represented
C
C   	Author: Hanford Van Ness                 Date: circa 1980
C   	Called by: savenetd.f
C
C****************************************************************
C
      integer function ext_brn ( savfil, dialect, lenrec, ratcod,
     &                           sections, type_e )
 
      character*(*) dialect, ratcod, sections, type_e
      integer savfil, lenrec
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/com007.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/pseudo_b.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/pti_data.inc'

      character xbuf*120, xbuf2*120, trtyp*1
      integer total1, total2, ptr, qptr, oldptr, orienttx, ktemp(22), 
     &        count, count_cvt, firstxstr, fndpsptr, cnvt_brn
      real temp(22)
      equivalence (temp, ktemp)
      logical pseudo_sections, type_e_branches, print_flag, ltc_flag
 
      ext_brn = 0
      total1 = 0       ! Count of branches extracted
      total2 = 0       ! Count of pseudo-branches extracted

      pseudo_sections = (firstxstr(sections, 'PSEUDO*') .ne. 0 .and.
     &                   dialect .ne. 'WSCC1' .and.
     &                   dialect .ne. 'WSCC2')
      type_e_branches = (firstxstr(sections, 'SYMMETRIC*') .ne. 0 .and.
     &                   dialect .ne. 'WSCC1' .and.
     &                   dialect .ne. 'WSCC2')

      if ( dialect .eq. 'BPA' ) then
C                                                                 *
C        BPA criteria for selection:  (low-hi/hi-low):       
C                                                                  
C        Type           Criteria                            
C                                                                  
C        L,E,TP,LM,RZ   Original submittal               
C        T,R            Original submittal               
C        LD             Original submittal               
C                                                                  
         call bcdopt ( 'LOWX',0.0000 )

         do ib = 1, ntot_alf
            nb = alf2inp(ib)   
            print_flag = .false.         
            ptr = kbsdta(16,nb)
            do while (ptr .gt. 0)
               qptr = brnch_ptr(ptr)
               if (qptr. gt. 0) then
                  nbr = iabs (qptr)
                  if ((brtype(ptr) .ne. 1 .and.
     &                 brtype(ptr) .ne. 8) .or.
     &                (brtype(ptr) .eq. 8 .and.
     &                 brnch(8,nbr) .eq. brnch(10,nbr))) then

                     call bcdbrn( ptr, xbuf )

c                    Substitute extended for nominal ratings

                     if (ratcod .ne. 'E') 
     &                  call sub_rate( xbuf, ratcod )
                     write( savfil, '(A)' ) xbuf(1:lenrec)
                     total1 = total1 + 1
                     ptr = brnch_nxt(ptr)

                  else if (brtype(ptr) .eq. 8 .and.
     &                     brnch(8,nbr) .ne. brnch(10,nbr)) then
                     nsec = fndpsptr (ptr)
                     if (nsec .le. 0) then

                        call bcdbrn( ptr, xbuf )

c                       Substitute extended for nominal ratings

                        if (ratcod .ne. 'E') 
     &                     call sub_rate( xbuf, ratcod )
                        write( savfil, '(A)' ) xbuf(1:lenrec)
                        total1 = total1 + 1
                        ptr = brnch_nxt(ptr)

                     else 

                        if (.not. print_flag) call space(1)
                        print_flag = .true.
                        call bcdbrn( ptr, xbuf )
                        write (outbuf, 80) xbuf(1:80)
   80                   format (' Original branch [', a, ']')
                        call prtout(1)

                        count_cvt = cnvt_brn (ptr, nsec, xbuf, xbuf2)
                        if (ratcod .ne. 'E') 
     &                     call sub_rate( xbuf, ratcod )
                        write( savfil, '(A)' ) xbuf(1:lenrec)
                        total2 = total2 + 1
                        write (outbuf, 90) xbuf(1:80)
   90                   format (' Replaced branch [', a, ']')
                        call prtout(1)

                        if (count_cvt .gt. 1) then
                          if (ratcod .ne. 'E') 
     &                       call sub_rate( xbuf2, ratcod )
                          write( savfil, '(A)' ) xbuf2(1:lenrec)
                          total2 = total2 + 1
                          write (outbuf, 90) xbuf2(1:80)
                          call prtout(1)
                        endif

                        ptr = brnch_nxt(ptr)
                     endif

                  else if (brtype(ptr) .eq. 1 .and.
     &                    (pseudo_sections .or. type_e_branches)) then
c
c                    process section PSEUDOBUSES option
c
                     oldptr = ptr
                     ptr = brnch_nxt(ptr)
                     do while (ptr .gt. 0 .and. 
     &                         ky(ptr) .eq. ky(oldptr) .and.
     &                         brid(ptr) .eq. brid(oldptr))
                        nsec = fndpsptr (ptr)
                        if (nsec .le. 0) then

                           call bcdbrn( ptr, xbuf )

c                          Substitute extended for nominal ratings

                           if (ratcod .ne. 'E') 
     &                        call sub_rate( xbuf, ratcod )
                           write( savfil, '(A)' ) xbuf(1:lenrec)
                           total1 = total1 + 1
                           ptr = brnch_nxt(ptr)

                        else 

                           if (.not. print_flag) call space(1)
                           print_flag = .true.
                           call bcdbrn( ptr, xbuf )
                           write (outbuf, 80) xbuf(1:80)
                           call prtout(1)

                           count_cvt = cnvt_brn (ptr, nsec, xbuf, xbuf2)
                           if (ratcod .ne. 'E') 
     &                        call sub_rate( xbuf, ratcod )
                           write( savfil, '(A)' ) xbuf(1:lenrec)
                           total2 = total2 + 1
                           write (outbuf, 90) xbuf(1:80)
                           call prtout(1)

                           if (count_cvt .gt. 1) then
                             if (ratcod .ne. 'E') 
     &                          call sub_rate( xbuf2, ratcod )
                             write( savfil, '(A)' ) xbuf2(1:lenrec)
                             total2 = total2 + 1
                             write (outbuf, 90) xbuf2(1:80)
                             call prtout(1)
                           endif

                           ptr = brnch_nxt(ptr)
                        endif
                     enddo
                  else
                     ptr = brnch_nxt(ptr)
                  endif
               else
                  ptr = brnch_nxt(ptr)
               endif
            enddo
         enddo
 
      else if (dialect(1:4) .eq. 'WSCC') then
C                                                                 
C        WSCC or WSCC1 or WSCC2 Criteria for selection: (low-hi/hi-low)
C                                                                  
C        Type           Criteria                            
C       
C        L,E,T,TP,LM,RZ Original submittal                
C        R              Adjustable tap side or hi-low     
C        LD             Rectifier-inverter                
C        LM             Original order
C                                                                 
         if ( dialect .eq. 'WSCC' ) then
            call bcdopt ( 'LOWX',0.0005 ) ! WSCC
         else
            call bcdopt ( 'LOWX',0.0003 ) ! WSCC1 or WSCC2
         endif
 
         do ib = 1, ntot_alf
            nb = alf2inp(ib)
            print_flag = .false.
            ptr = kbsdta(16,nb)
            do while (ptr .gt. 0)
               qptr = brnch_ptr(ptr)
               nbr = iabs (qptr)
               if (brtype(ptr) .eq. 1 .and. qptr .gt. 0 .and.
     &             pseudo_sections) then
                  oldptr = ptr
                  ptr = brnch_nxt(ptr)
                  do while (ptr .gt. 0 .and.
     &                      ky(ptr) .eq. ky(oldptr) .and.
     &                      brid(ptr) .eq. brid(oldptr))
                    nsec = fndpsptr (ptr)
                    if (nsec .le. 0) then

                      call bcdbrn( ptr, xbuf )
                      k1 = kx(ptr)
                      k2 = ky(ptr)
                      xbuf(15:18) = pti_name(k1)
                      xbuf(28:31) = pti_name(k2)
c                     Substitute extended for nominal ratings

                      if (ratcod .ne. 'E')
     &                    call sub_rate( xbuf, ratcod )
                      write( savfil, '(A)' ) xbuf(1:lenrec)
                      total1 = total1 + 1
                      ptr = brnch_nxt(ptr)

                    else

                      if (.not. print_flag) call space(1)
                      print_flag = .true.
                      call bcdbrn( ptr, xbuf )
                      k1 = kx(ptr)
                      k2 = ky(ptr)
                      xbuf(15:18) = pti_name(k1)
                      xbuf(28:31) = pti_name(k2)
                      write (outbuf, 80) xbuf(1:80)
                      call prtout(1)

                      count_cvt = cnvt_brn (ptr, nsec, xbuf, xbuf2)
                      if (ratcod .ne. 'E')
     &                     call sub_rate( xbuf, ratcod )
                      write( savfil, '(A)' ) xbuf(1:lenrec)
                      total2 = total2 + 1
                      write (outbuf, 90) xbuf(1:80)
                      call prtout(1)

                      if (count_cvt .gt. 1) then
                        if (ratcod .ne. 'E') 
     &                     call sub_rate( xbuf2, ratcod )
                        write( savfil, '(A)' ) xbuf2(1:lenrec)
                        total2 = total2 + 1
                        write (outbuf, 90) xbuf2(1:80)
                        call prtout(1)
                      endif

                      ptr = brnch_nxt(ptr)
                    endif
                  enddo
                
               else if (brtype(ptr) .eq. 1 .and. qptr .gt. 0) then

c                 Pi-equivalent branch table entry: Overwrite any
c                 section element for this parallel which differs
c                 in original/transposed order of equivalent.

                  oldptr = ptr
                  ptr = brnch_nxt(ptr)

                  do while (ptr .gt. 0 .and. 
     &                     (ky(ptr) .eq. ky(oldptr) .and.
     &                      brid(ptr) .eq. brid(oldptr)))

                     call bcdbrn( ptr, xbuf )
                     k1 = kx(ptr)
                     k2 = ky(ptr)
                     xbuf(15:18) = pti_name(k1)
                     xbuf(28:31) = pti_name(k2)

c                    Substitute extended for nominal ratings

                     if (ratcod .ne. ' ') 
     &                  call sub_rate( xbuf, ratcod )
                     write( savfil, '(A)' ) xbuf(1:lenrec)
                     total1 = total1 + 1
                     ptr = brnch_nxt(ptr)
                  enddo

               else if (brtype(ptr) .eq. 1) then

                  oldptr = ptr
                  ptr = brnch_nxt(ptr)
                  do while (ptr .gt. 0 .and. 
     &                     (ky(ptr) .eq. ky(oldptr) .and.
     &                      brid(ptr) .eq. brid(oldptr)))

                     ptr = brnch_nxt(ptr)
                  enddo
               
               else if ((brtype(ptr) .eq. 2 .or. 
     &                   brtype(ptr) .eq. 3 .or. 
     &                   brtype(ptr) .eq. 5. or. 
     &                   brtype(ptr) .eq. 6 .or.
     &                   brtype(ptr) .eq. 9) .and. qptr .gt. 0) then

C                 L,T,TP,LM,RZ: Original submittal                

                  call bcdbrn( ptr, xbuf )
                  k1 = kx(ptr)
                  k2 = ky(ptr)
                  xbuf(15:18) = pti_name(k1)
                  xbuf(28:31) = pti_name(k2)

c                 Substitute extended for nominal ratings

                  if (ratcod .ne. ' ') call sub_rate( xbuf, ratcod )
                  write( savfil, '(A)' ) xbuf(1:lenrec)
                  total1 = total1 + 1
                  ptr = brnch_nxt(ptr)

               else if (brtype(ptr) .eq. 8 .and. qptr .gt. 0) then

C                 E: Original submittal                

                  nsec = fndpsptr (ptr)
                  if (nsec .le. 0) then
 
                     call bcdbrn( ptr, xbuf )
                     k1 = kx(ptr)
                     k2 = ky(ptr)
                     xbuf(15:18) = pti_name(k1)
                     xbuf(28:31) = pti_name(k2)

c                    Substitute extended for nominal ratings

                     if (ratcod .ne. 'E')
     &                  call sub_rate( xbuf, ratcod )
                     write( savfil, '(A)' ) xbuf(1:lenrec)
                     total1 = total1 + 1
                     ptr = brnch_nxt(ptr)

                  else

                     if (.not. print_flag) call space(1)
                     print_flag = .true.
                     call bcdbrn( ptr, xbuf )
                     k1 = kx(ptr)
                     k2 = ky(ptr)
                     xbuf(15:18) = pti_name(k1)
                     xbuf(28:31) = pti_name(k2)
                     write (outbuf, 80) xbuf(1:80)
                     call prtout(1)

                     count_cvt = cnvt_brn (ptr, nsec, xbuf, xbuf2)
                     if (ratcod .ne. 'E')
     &                  call sub_rate( xbuf, ratcod )
                     write( savfil, '(A)' ) xbuf(1:lenrec)
                     total2 = total2 + 1
                     write (outbuf, 90) xbuf(1:80)
                     call prtout(1)

                     if (count_cvt .gt. 1) then
                       if (ratcod .ne. 'E')
     &                    call sub_rate( xbuf2, ratcod )
                       write( savfil, '(A)' ) xbuf2(1:lenrec)
                       total2 = total2 + 1
                       write (outbuf, 90) xbuf2(1:80)
                       call prtout(1)
                     endif

                     ptr = brnch_nxt(ptr)

                  endif

               else if (brtype(ptr) .eq. 2 .or. 
     &                  brtype(ptr) .eq. 3 .or. 
     &                  brtype(ptr) .eq. 8 .or. 
     &                  brtype(ptr) .eq. 5. or. 
     &                  brtype(ptr) .eq. 6 .or.
     &                  brtype(ptr) .eq. 9) then
                  ptr = brnch_nxt(ptr)

               else if (brtype(ptr) .eq. 7 .and. 
     &                 (brnch(8,nbr) .gt. 0.0 .and. qptr .gt. 0) .or.
     &                 (brnch(8,nbr) .lt. 0.0 .and. qptr .lt. 0)) 
     &              then

C                 LD: Rectifier-inverter                

                  call bcdbrn( ptr, xbuf )
                  k1 = kx(ptr)
                  k2 = ky(ptr)
                  xbuf(15:18) = pti_name(k1)
                  xbuf(28:31) = pti_name(k2)

c                 Substitute extended for nominal ratings
   
                  if (ratcod .ne. ' ') call sub_rate( xbuf, ratcod )
                  write( savfil, '(A)' ) xbuf(1:lenrec)
                  total1 = total1 + 1
                  ptr = brnch_nxt(ptr)

               else if (brtype(ptr) .eq. 7) then
                  ptr = brnch_nxt(ptr)

               else if (brtype(ptr) .eq. 4) then

C                 R: Adjustable tap side or hi-low     

                  krmap = orienttx (ptr, brnch_nxt(ptr), k1x, k2x, 
     &                              tap1, tap2)
c
c                 krmap = 1 if kx(ptr) = fixed-tap
c                              ky(ptr) = variable-tap side
c
c                         2 if kx(ptr) = variable-tap side
c                              ky(ptr) = fixed-tap
c
                  nbr = iabs(brnch_ptr(ptr))
                  mb = kbrnch(4,nbr)
                  call getchr(1,trtyp,kbrnch(3,nbr))
C
C                 WSCC2: Reverse Phase shifter orientation
C
                  if (dialect .eq. 'WSCC2' .and.
     &               (trtyp .eq. 'P' .or. trtyp .eq. 'M') .and.
     &               ((krmap .eq. 1 .and. brnch_ptr(ptr) .gt. 0) .or.
     &                (krmap .eq. 2 .and. brnch_ptr(ptr) .lt. 0))) then
                    krmap = 3 - krmap
                  endif
C
C                 WSCC2: Disable any LTC controlling a BQ bus
C
                  if (mb .gt. 0 .and. dialect .eq. 'WSCC2' .and.
     &               (trtyp .eq. ' ' .or. trtyp .eq. 'V' .or.
     &                trtyp .eq. 'Q' .or. trtyp .eq. 'N')) then
                    ntype = kbsdta(1,mb) 
                    ltc_flag = (((ntype .eq. 1 .or. ntype .eq. 4 .or.
     &                            ntype .eq. 5 .or. ntype .eq. 10 .or.
     &                            ntype .eq. 12) .and.
     &                           index(' V', trtyp) .ne. 0) .or.
     &                           index(' V', trtyp) .eq. 0)
                  else
                    ltc_flag = .true.
                  endif
                  if (krmap .eq. 2 .and. ltc_flag) then

                     call bcdbrn( ptr, xbuf )
                     k1 = kx(ptr)
                     k2 = ky(ptr)
                     xbuf(15:18) = pti_name(k1)
                     xbuf(28:31) = pti_name(k2)
                     if (xbuf(34:41) .ne. ' ') then
                       basekv = ftn_atof (xbuf(42:45))
                       if (mb .ne. 0) then
                         xbuf(42:45) = pti_name(mb)
                       endif
                     endif
c
c                    WSCC and WSCC2 dialect converts taps into steps
c
                     if ( dialect .eq. 'WSCC' .or. 
     &                    dialect .eq. 'WSCC2' ) then
                        read (xbuf(56:57), '(bz, f2.0)') taps
                        if (taps .gt. 0) then
                           isteps = taps - 1
                           write (xbuf(56:57), '(i2)') isteps
                        endif
                     endif

                     if (ratcod .ne. ' ') call sub_rate( xbuf, ratcod )
                     write( savfil, '(A)' ) xbuf(1:lenrec)
                     total1 = total1 + 1

                  endif
c
c                 Advance pointer to first Tx record
c
                  oldptr = ptr
                  ptr = brnch_nxt(ptr)

                  if (krmap .eq. 2) then
c
c                    Consolidate parallel TX's into an equivalent TX.
c                    First, store original attributes of first 
c                    parallel.
c
                     qptr = brnch_ptr(ptr)
                     nbr = iabs (qptr)

                     do i = 1, 18
                        temp(i) = brnch(i,nbr)
                     enddo
                     do i = 1, 3
                        temp(i+18) = rateln(i,nbr)
                     enddo
c
c                    Temporarily convert r + jx into g + jb
c
                     r = brnch(5,nbr)
                     x = brnch(6,nbr)
                     zsq = r ** 2 + x ** 2
                     brnch(5,nbr) = r / zsq
                     brnch(6,nbr) = -x / zsq
c
c                    Default number of lines = 1
c
                     if (brnch(16,nbr) .eq. 0.0) brnch(16,nbr) = 1.0

                     count = 1

                     oldptr = ptr
                     ptr = brnch_nxt(ptr)
c
c                    Combine parallel data
c
                     do while (ptr .gt. 0 .and. 
     &                        (ky(ptr) .eq. ky(oldptr)))

                        count = count + 1
                        qptr = brnch_ptr(ptr)
                        nbrx = iabs (qptr)

                        brnch(4,nbr) = brnch(4,nbr) + brnch(4,nbrx)

                        r = brnch(5,nbrx)
                        x = brnch(6,nbrx)
                        zsq = r ** 2 + x ** 2

                        brnch(5,nbr) = brnch(5,nbr) + r / zsq
                        brnch(6,nbr) = brnch(6,nbr) - x / zsq

                        brnch(7,nbr) = brnch(7,nbr) + brnch(7,nbrx)
                        brnch(8,nbr) = brnch(8,nbr) + brnch(8,nbrx)

                        if (brnch(16,nbrx) .eq. 0.0) then
                           brnch(16,nbr) = brnch(16,nbr) + 1.0
                        else
                           brnch(16,nbr) = brnch(16,nbr) 
     &                                   + brnch(16,nbrx)
                        endif

                        do i = 1, 3
                           rateln(i,nbr) = rateln(i,nbr) 
     &                                   + rateln(i,nbrx)
                        enddo

                        ptr = brnch_nxt(ptr)
                     enddo

                     g = brnch(5,nbr)
                     b = brnch(6,nbr)
                     ysq = g ** 2 + b ** 2

                     brnch(5,nbr) = g / ysq
                     brnch(6,nbr) = -b / ysq

                     call bcdbrn( oldptr, xbuf )
                     k1 = kx(oldptr)
                     k2 = ky(oldptr)
                     xbuf(15:18) = pti_name(k1)
                     xbuf(28:31) = pti_name(k2)
                     if (ratcod .ne. ' ') call sub_rate( xbuf, ratcod )
                     write( savfil, '(A)' ) xbuf(1:lenrec)
                     total1 = total1 + 1
c
c                    Restore original transformer data
c
                     do i = 1, 18
                        brnch(i,nbr) = temp(i) 
                     enddo
                     do i = 1, 3
                        rateln(i,nbr) = temp(i+18)
                     enddo

                  else

                     do while (ptr .gt. 0 .and. 
     &                        (ky(ptr) .eq. ky(oldptr)))

                        ptr = brnch_nxt(ptr)
                     enddo
                  endif
               else
               endif
            enddo
         enddo

      else if (dialect .eq. 'PTI') then
C                                                                 
C        PTI criteria for selection:  (low-hi/hi-low):
C                                                                  
C        Type           Criteria                            
C       
C        L,E,T,TP,LM,RZ Original submittal                
C        R              Adjustable tap side or hi-low     
C        LD             Rectifier-inverter                
C                                                                 
         call bcdopt ( 'LOWX',0.0000 )

         do ib = 1, ntot_alf
            nb = alf2inp(ib)
            print_flag = .false.
            ptr = kbsdta(16,nb)
            do while (ptr .gt. 0)
               qptr = brnch_ptr(ptr)
               nbr = iabs (qptr)

               if (brtype(ptr) .eq. 1 .and. qptr .gt. 0 .and.
     &            (pseudo_sections .or. type_e_branches)) then
                  oldptr = ptr
                  ptr = brnch_nxt(ptr)
                  do while (ptr .gt. 0 .and.
     &                      ky(ptr) .eq. ky(oldptr) .and.
     &                      brid(ptr) .eq. brid(oldptr))
                    nsec = fndpsptr (ptr)
                    if (nsec .le. 0) then

                      call bcdbrn( ptr, xbuf )

c                     Substitute extended for nominal ratings

                      if (ratcod .ne. 'E')
     &                   call sub_rate( xbuf, ratcod )
                      write( savfil, '(A)' ) xbuf(1:lenrec)
                      total1 = total1 + 1
                      ptr = brnch_nxt(ptr)
 
                    else

                      if (.not. print_flag) call space(1)
                      print_flag = .true.
                      call bcdbrn( ptr, xbuf )
                      write (outbuf, 80) xbuf(1:80)
                      call prtout(1)

                      count_cvt = cnvt_brn (ptr, nsec, xbuf, xbuf2)
                      if (ratcod .ne. 'E')
     &                   call sub_rate( xbuf, ratcod )
                      write( savfil, '(A)' ) xbuf(1:lenrec)
                      total2 = total2 + 1
                      write (outbuf, 90) xbuf(1:80)
                      call prtout(1)

                      if (count_cvt .gt. 1) then
                         if (ratcod .ne. 'E') 
     &                      call sub_rate( xbuf2, ratcod )
                         write( savfil, '(A)' ) xbuf2(1:lenrec)
                         total2 = total2 + 1
                         write (outbuf, 90) xbuf2(1:80)
                         call prtout(1)
                      endif

                      ptr = brnch_nxt(ptr)
                    endif
                  enddo

               else if (brtype(ptr) .eq. 1 .and. qptr .gt. 0) then

c                 Pi-equivalent branch table entry: Overwrite any
c                 section element for this parallel which differs
c                 in original/transposed order of equivalent.

                  oldptr = ptr
                  ptr = brnch_nxt(ptr)

                  do while (ptr .gt. 0 .and. 
     &                     (ky(ptr) .eq. ky(oldptr) .and.
     &                      brid(ptr) .eq. brid(oldptr)))

                     call bcdbrn( ptr, xbuf )

c                    Substitute extended for nominal ratings

                     if (ratcod .ne. ' ') 
     &                  call sub_rate( xbuf, ratcod )
                     write( savfil, '(A)' ) xbuf(1:lenrec)
                     total1 = total1 + 1
                     ptr = brnch_nxt(ptr)
                  enddo
            
               else if (brtype(ptr) .eq. 1) then

                  oldptr = ptr
                  ptr = brnch_nxt(ptr)

                  do while (ptr .gt. 0 .and. 
     &                     (ky(ptr) .eq. ky(oldptr) .and.
     &                      brid(ptr) .eq. brid(oldptr)))

                     ptr = brnch_nxt(ptr)
                  enddo
            
               else if ((brtype(ptr) .eq. 2 .or. 
     &                   brtype(ptr) .eq. 3 .or. 
     &                   brtype(ptr) .eq. 5. or. 
     &                   brtype(ptr) .eq. 6 .or.
     &                   brtype(ptr) .eq. 9) .and. qptr .gt. 0) then

C                 L,T,TP,LM,RZ: Original submittal                

                  call bcdbrn( ptr, xbuf )

c                 Substitute extended for nominal ratings

                  if (ratcod .ne. ' ') call sub_rate( xbuf, ratcod )
                  write( savfil, '(A)' ) xbuf(1:lenrec)
                  total1 = total1 + 1
                  ptr = brnch_nxt(ptr)

               else if (brtype(ptr) .eq. 8 .and. qptr .gt. 0) then

C                 E: Original submittal                
                  nsec = fndpsptr (ptr)
                  if (nsec .le. 0) then

                     call bcdbrn( ptr, xbuf )

c                    Substitute extended for nominal ratings

                     if (ratcod .ne. 'E')
     &                  call sub_rate( xbuf, ratcod )
                     write( savfil, '(A)' ) xbuf(1:lenrec)
                     total1 = total1 + 1
                     ptr = brnch_nxt(ptr)

                  else

                     if (.not. print_flag) call space(1)
                     print_flag = .true.
                     call bcdbrn( ptr, xbuf )
                     write (outbuf, 80) xbuf(1:80)
                     call prtout(1)

                     count_cvt = cnvt_brn (ptr, nsec, xbuf, xbuf2)
                     if (ratcod .ne. 'E')
     &                     call sub_rate( xbuf, ratcod )
                     write( savfil, '(A)' ) xbuf(1:lenrec)
                     total2 = total2 + 1
                     write (outbuf, 90) xbuf(1:80)
                     call prtout(1)

                     if (count_cvt .gt. 1) then
                       if (ratcod .ne. 'E')
     &                    call sub_rate( xbuf2, ratcod )
                       write( savfil, '(A)' ) xbuf2(1:lenrec)
                       total2 = total2 + 1
                       write (outbuf, 90) xbuf2(1:80)
                       call prtout(1)
                     endif

                     ptr = brnch_nxt(ptr)
                  endif
  
                else if (brtype(ptr) .eq. 2 .or. 
     &                  brtype(ptr) .eq. 3 .or. 
     &                  brtype(ptr) .eq. 5. or. 
     &                  brtype(ptr) .eq. 8 .or. 
     &                  brtype(ptr) .eq. 6 .or.
     &                  brtype(ptr) .eq. 9) then
                  ptr = brnch_nxt(ptr)

               else if (brtype(ptr) .eq. 7 .and. 
     &                 (brnch(8,nbr) .gt. 0.0 .and. qptr .gt. 0) .or.
     &                 (brnch(8,nbr) .lt. 0.0 .and. qptr .lt. 0)) 
     &              then

C                 LD: Rectifier-inverter                

                  call bcdbrn( ptr, xbuf )

c                 Substitute extended for nominal ratings

                  if (ratcod .ne. ' ') call sub_rate( xbuf, ratcod )
                  write( savfil, '(A)' ) xbuf(1:lenrec)
                  total1 = total1 + 1
                  ptr = brnch_nxt(ptr)

               else if (brtype(ptr) .eq. 7) then
                  ptr = brnch_nxt(ptr)

               else if (brtype(ptr) .eq. 4) then

C                 R: Adjustable tap side or hi-low     

                  krmap = orienttx (ptr, brnch_nxt(ptr), k1x, k2x, 
     &                              tap1, tap2)
c
c                 krmap = 1 if kx(ptr) = fixed-tap
c                              ky(ptr) = variable-tap side
c
c                         2 if kx(ptr) = variable-tap side
c                              ky(ptr) = fixed-tap
c
                  if (krmap .eq. 2) then
c
c                    Convert taps into steps
c
                     call bcdbrn( ptr, xbuf )

                     read (xbuf(56:57), '(bz, f2.0)') taps
                     if (taps .gt. 0) then
                        isteps = taps - 1
                        write (xbuf(56:57), '(i2)') isteps
                     endif

                     write( savfil, '(A)' ) xbuf(1:lenrec)
                     total1 = total1 + 1

                     k2 = ky(ptr)
                     oldptr = ptr
                     ptr = brnch_nxt(ptr)
                     do while (ptr .gt. 0 .and. 
     &                        (ky(ptr) .eq. k2))
                        call bcdbrn( ptr, xbuf )

c                       Substitute extended for nominal ratings

                        if (ratcod .ne. ' ') 
     &                     call sub_rate( xbuf, ratcod )
                        write( savfil, '(A)' ) xbuf(1:lenrec)
                        total1 = total1 + 1
                        ptr = brnch_nxt(ptr)
                     enddo
   
                  else

                     k2 = ky(ptr)
                     oldptr = ptr
                     ptr = brnch_nxt(ptr)
                     do while (ptr .gt. 0 .and. 
     &                        (ky(ptr) .eq. k2))
                        ptr = brnch_nxt(ptr)
                     enddo
   
                  endif
               else
                  ptr = brnch_nxt(ptr)
               endif
            enddo
         enddo

      endif

      write ( errbuf(1), 100) total1, total2
  100 format (' Total branch records extracted:', i5, 
     &        ' pseudo-sections ', i5)
      call prterx ('I', 1)
 
      ext_brn = total1 + total2

      return
      end

