C    @(#)ldptibrn.f	20.11 3/29/99
C**************************************************************** 
C 
C     File: ldptibrn.f 
C 
C     Purpose: Routine to load PTI branch data from raw data file  
C 
c     Return code:  n = 0 : Success 
c                   n = 1 : Error 
c 
C     Author: Walt Powell  Date: 21 May 1996 
C     Called by: load_pti.f 
C 
C**************************************************************** 
      integer function ldptibrn (xbuf, options, numver, error) 
      integer error, options(*), numver 
      character *(*) xbuf 
 
      include 'ipfinc/parametr.inc' 
 
      include 'ipfinc/prt.inc' 
      include 'ipfinc/pti_data.inc' 
      include 'ipfinc/blank.inc' 
      include 'ipfinc/alt_case.inc' 
      include 'ipfinc/alpha.inc' 
      include 'ipfinc/bus.inc' 
      include 'ipfinc/branch.inc' 
 
      common /old_branch/ old_k1, old_k2, old_sect, old_type, old_id 
      integer old_k1, old_k2, old_sect, old_type, gtptinam 
      character old_id * 1 
 
      character word(30)*10, id*1, bus1*8, bus2*8 
      logical first 
      integer find_bus, ftn_atoi, status, bptr1, bptr2,  
     &        find_obr, findoldbus, oldk1, oldk2, status1, status2 
 
      ldptibrn = 0 
      error = 0 
      last = index (xbuf, '/') - 1 
      if (last .le. 0) last = lastch(xbuf) 
      call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ')    

      do i = nwrd+1, 16 
        word(i) = ' ' 
      enddo 
c 
c     Add defaults 
c 
      if (word(3) .eq. ' ') word(3) = '1' 
      id = word(3) 
 
      status1 = gtptinam (word(1), npti1, kp1, isgn1, bus1, basekv1) 
      if (npti1 .eq. 0 .and. bus1 .eq. ' ') then
        ldptibrn = 1 
      else
        status2 = gtptinam (word(2), npti2, kp2, isgn2, bus2, basekv2) 
        if (word(16) .eq. ' ') then 
          status = 1 
        else 
          status = ftn_atoi(word(16)) 
        endif
      endif 
      if (npti1 .eq. 0 .and. bus1 .eq. ' ') then 
      else if (npti2 .eq. 0 .and. bus2 .eq. ' ') then 
      else if (status .eq. 0) then 
      else 
        if (ltot+1 .ge. MAXBRN) then 
           write (errbuf(1), 110) MAXBRN 
  110      format ('More than ',i5, 
     &             ' branch records. Overflow occurred at branch:')  
           write (errbuf(2), 120) xbuf(1:80)    
  120      format(11x, '(', a, ')')          
           call prterx ('E',2)                 
           ltot = 1                            
           error = 1                            
           go to 900 
        endif 
        if (npti1 .ne. 0 .and. kp1 .le. 0) then 
           write (errbuf(1), 122) npti1, npti2 
  122      format (' PTI bus1 on branch record (', i6, 1x, i6,  
     &       ') is not in system.') 
           call prterx ('W',1)                 
           error = 1                            
           go to 900 
        endif 
        if (npti2 .ne. 0 .and. kp2 .le. 0) then 
           write (errbuf(1), 124) npti1, npti2 
  124      format (' PTI bus2 on branch record (', i6, 1x, i6,  
     &       ') is not in system.') 
           call prterx ('W',1)                 
           error = 1                            
           go to 900 
        endif 
        k1 = find_bus (bus1, basekv1) 
        k2 = find_bus (bus2, basekv2) 
        if (k1 .le. 0) then 
           write (errbuf(1), 130) npti1, bus1, basekv1 
  130      format (' PTI bus1 (', i6, 1x, a8, f7.1, ') is not in system. 
     &') 
           call prterx ('W',1)                 
           error = 1                            
           go to 900 
        else if (k2 .le. 0) then 
           write (errbuf(1), 140) npti2, bus2, basekv2 
  140      format (' PTI bus2 (', i6, 1x, a8, f7.1, ') is not in system. 
     &') 
           call prterx ('W',1)                 
           error = 1                            
           go to 900 
        endif 
        ltot = ltot + 1 
        oldk1 = findoldbus(bus(k1), base(k1)) 
        oldk2 = findoldbus(bus(k2), base(k2)) 
        if (oldk1 .gt. 0 .and. oldk2 .gt. 0) then 
          ltot2x = find_obr (oldk1, oldk2, id, 0) 
          if (ltot2x .eq. 0 .and. id .eq. '1') then 
            ltot2x = find_obr (oldk1, oldk2, ' ', 0) 
          endif 
          if (ltot2x .gt. 0) then 
            nbr = iabs(obrnch_ptr(ltot2x)) 
            kbrnch(3,ltot) = okbrnch(3,nbr) 
          else 
            call putchr (3, '   ', kbrnch(3,ltot)) 
          endif 
        else 
          call putchr (3, '   ', kbrnch(3,ltot)) 
        endif 
 
        brnch(5,ltot) = ftn_atof(word(4))  ! R 
        brnch(6,ltot) = ftn_atof(word(5))  ! X 
        brnch(7,ltot) = 0.0 
        if (isgn1 .lt. 0) then 
          kbrnch(15,ltot) = 1 
        else 
          kbrnch(15,ltot) = 2 
        endif 
        b = ftn_atof(word(6))              ! G 
        g1 = ftn_atof(word(12))            ! G1 
        b1 = ftn_atof(word(13))            ! B1 
        g2 = ftn_atof(word(14))            ! G2 
        b2 = ftn_atof(word(15))            ! B2 
        if (b1 .eq. 0.0 .and. b2 .eq. 0.0) then 
          b1 = 0.5 * b 
          b2 = 0.5 * b           
        endif 
        tx = ftn_atof(word(10)) 
        ph = ftn_atof(word(11)) 
        if (tx .eq. 0.0 .and. ph .eq. 0.0) then 
          if (g1 .eq. g2 .and. b1 .eq. b2) then 
            kbrnch(1,ltot) = 3 
            brnch(7,ltot) = g1 
            brnch(8,ltot) = b1 
          else              
            kbrnch(1,ltot) = 8 
            brnch(7,ltot) = g1 
            brnch(8,ltot) = b1 
            brnch(9,ltot) = g2 
            brnch(10,ltot) = b2 
          endif 
 
          const = 1000.0 / (sqrt (3.0) * base(k1)) 
          ratea = const * ftn_atof(word(7))   ! Rate A 
          rateb = const * ftn_atof(word(8))   ! Rate B 
          ratec = const * ftn_atof(word(9))   ! Rate C 
          ratea = amin1 (ratea, 9999.0) 
          rateb = amin1 (rateb, 9999.0) 
          rateb = amin1 (rateb, 9999.0) 
 
          if (options(5) .eq. 3) then 
            brnch(4,ltot) = ratec             ! Rate 
          else 
            brnch(4,ltot) = ratea             ! Rate 
          endif 
        else if (tx .ne. 0.0 .and. ph .eq. 0.0) then  
          kbrnch(1,ltot) = 5 
          brnch(7,ltot) = g1+g2 
          brnch(8,ltot) = b1+b2 
          brnch(9,ltot) = base(k1) * tx 
          brnch(10,ltot) = base(k2) 
          ratea = ftn_atof(word(7))           ! Rate A 
          rateb = ftn_atof(word(8))           ! Rate B 
          ratec = ftn_atof(word(9))           ! Rate C 
          ratea = amin1 (ratea, 9999.0) 
          rateb = amin1 (rateb, 9999.0) 
          rateb = amin1 (rateb, 9999.0) 
          if (options(1) .eq. 2) then 
            brnch(4,ltot) = rateb             ! Rate 
          else if (options(1) .eq. 3) then 
            brnch(4,ltot) = ratec             ! Rate 
          else 
            brnch(4,ltot) = ratea             ! Rate 
          endif 
        else 
          kbrnch(1,ltot) = 6 
          brnch(7,ltot) = g1+g2 
          brnch(8,ltot) = b1+b2 
          brnch(9,ltot) = ph 
          brnch(10,ltot) = 0.0 
          ratea = ftn_atof(word(7))           ! Rate A 
          rateb = ftn_atof(word(8))           ! Rate B 
          ratec = ftn_atof(word(9))           ! Rate C 
          ratea = amin1 (ratea, 9999.0) 
          rateb = amin1 (rateb, 9999.0) 
          rateb = amin1 (rateb, 9999.0) 
          if (options(1) .eq. 2) then 
            brnch(4,ltot) = rateb             ! Rate 
          else if (options(1) .eq. 3) then 
            brnch(4,ltot) = ratec             ! Rate 
          else 
            brnch(4,ltot) = ratea             ! Rate 
          endif 
        endif 
C                                                   
C       Add extended ratings.                  
C                                                   
        if (tx .eq. 0.0 .and. ph .eq. 0.0) then 
          if (options(6) .eq. 3) then 
            rateln(1,ltot) = ratec        ! Rate C 
          else 
            rateln(1,ltot) = ratea        ! Rate A 
          endif 
          if (options(7) .eq. 1) then 
            rateln(2,ltot) = ratea        ! Rate A 
          else 
            rateln(2,ltot) = ratec        ! Rate C 
          endif 
        else 
          if (options(2) .eq. 2) then 
            rateln(1,ltot) = rateb        ! Rate B 
          else if (options(2) .eq. 3) then 
            rateln(1,ltot) = ratec        ! Rate C 
          else 
            rateln(1,ltot) = ratea        ! Rate A 
          endif 
          if (options(3) .eq. 1) then 
            rateln(2,ltot) = ratea        ! Rate A 
          else if (options(3) .eq. 3) then 
            rateln(2,ltot) = ratec        ! Rate C 
          else 
            rateln(2,ltot) = rateb        ! Rate B 
          endif 
          if (options(4) .eq. 1) then 
            rateln(3,ltot) = ratea        ! Rate A 
          else if (options(4) .eq. 2) then 
            rateln(3,ltot) = rateb        ! Rate B 
          else 
            rateln(3,ltot) = ratec        ! Rate C 
          endif 
        endif 
 
c       link up branch 
 
        call lkbrdata (ltot, k1, k2, bptr1, bptr2, error) 
        kx(bptr1) = k1                                                  
 
        ky(bptr1) = k2 
        brid(bptr1) = word(3) 
        brsect(bptr1) = 0 
        brtype(bptr1) = kbrnch(1,ltot) 
 
        kx(bptr2) = k2                                                  
 
        ky(bptr2) = k1 
        brid(bptr2) = word(3) 
        brsect(bptr2) = 0 
        brtype(bptr2) = kbrnch(1,ltot) 
 
        old_k1 = k1 
        old_k2 = k2 
        old_id = word(3) 
 
      endif 
  900 continue 
      return 
      end 
