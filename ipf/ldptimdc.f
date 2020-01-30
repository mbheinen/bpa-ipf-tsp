C    @(#)ldptimdc.f	20.7 1/15/98 
C**************************************************************** 
C 
C     File: ldptimdc.f 
C 
C     Purpose: Routine to load PTI multi-terminal d-c data from raw  
C              data file  
C 
c     Return code:  n = 0 : Success 
c                   n = 1 : Error 
c 
C     Author: Walt Powell  Date: 21 May 1996 
C     Called by: load_pti.f 
C 
C**************************************************************** 
      integer function ldptimdc (xbuf, tempfile, options, numnver, 
     &                           numrec, error) 
      integer tempfile, numrec, options(*), numver, error 
      character *(*) xbuf 
 
      include 'ipfinc/parametr.inc' 
 
      include 'ipfinc/prt.inc' 
      include 'ipfinc/pti_data.inc' 
      include 'ipfinc/blank.inc' 
      include 'ipfinc/bus.inc' 
      include 'ipfinc/branch.inc' 
      include 'ipfinc/lfiles.inc' 
      include 'ipfinc/wsccbase.inc' 
 
      common /old_branch/ old_k1, old_k2, old_sect, old_type, old_id 
      integer old_k1, old_k2, old_sect, old_type 
      character old_id * 1 
 
      character word(50)*10, busname*8, type*1, code*10, bus1*8  
      logical first, found 
      integer find_bus, ftn_atoi, bptr1, bptr2, add_bus, 
     &        array(6,12), count, ptr, gtptinam, status 
 
      ldptimdc = 0 
      error = 0 
      numrec = 1 
      nconv = 0 
      ndcbs = 0 
      ndcln = 0 
c 
c     Process first record.  
c 
      last = index (xbuf, '/') - 1 
      if (last .le. 0) last = lastch(xbuf) 
      call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ') 

      do i = nwrd+1, 8 
        word(i) = ' ' 
      enddo 
 
      nptidc = ftn_atoi(word(1)) 
      if (nptidc .eq. 0) then 
        ldptimdc = 1 
      else 
        nconv = ftn_atoi(word(2)) 
        ndcbs = ftn_atof(word(3)) 
        ndcln = ftn_atof(word(4)) 
        mdc = ftn_atof(word(5)) 
        nvconv = ftn_atoi(word(6)) 
        if (nconv .eq. 0) then 
          write (errbuf(1), 100) nptidc 
  100     format (' D-C system (', i2, ') has no specified number of a-c 
     & commutator buses.') 
          errbuf(2) = ' (' // xbuf(1:60) // ')' 
          call prterx ('W',2)                 
          error = 1                            
        endif 
        if (ndcbs .eq. 0) then 
          write (errbuf(1), 110) nptidc 
  110     format (' D-C system (', i2, ') has no specified number of d-c 
     & converter buses.') 
          errbuf(2) = ' (' // xbuf(1:60) // ')' 
          call prterx ('W',2)                 
          error = 1                            
        endif 
        if (ndcln .eq. 0) then 
          write (errbuf(1), 120) nptidc 
  120     format (' D-C system (', i2, ') has no specified number of d-c 
     & lines.') 
          errbuf(2) = ' (' // xbuf(1:60) // ')' 
          call prterx ('W',2)                 
          error = 1                            
        endif 
        if (nvconv .eq. 0) then 
          write (errbuf(1), 130) nptidc 
  130     format (' D-C system (', i2, ') has no specified voltage-contr 
     &olled d-c bus.') 
          errbuf(2) = ' (' // xbuf(1:60) // ')' 
          call prterx ('W',2)                 
          error = 1                            
        endif 
        if (error .ne. 0) go to 304 
c 
c       Process converter a-c records. 
c 
        pwr_rect = 0.0 
        pwr_invt = 0.0 
        kvc = 0 
        do ix = 1, nconv 
          numrec = numrec + 1      
          read (tempfile, fmt='(a)', end=910) xbuf 
          last = index (xbuf, '/') - 1 
          if (last .le. 0) last = lastch(xbuf) 
          call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ') 

          do i = nwrd+1, 16 
            word(i) = ' ' 
          enddo 
c 
c         Set defaults 
c 
          if (word(10) .eq. ' ') word(10) = '1.50' 
          if (word(11) .eq. ' ') word(11) = '0.51' 
          if (word(12) .eq. ' ') word(12) = '0.00625' 
c 
c	  Converter bus 
c 
          status1 = gtptinam (word(1), npti1, kp1, isgn1, bus1, basekv1) 
          if (bus1 .eq. ' ') then 
            write (errbuf(1), 160) nptidc, npti1 
  160       format (' D-C system (', i2, ') Converter bus (', i6,  
     &        ') is not in system.') 
            call prterx ('W',1)                 
            error = 1                            
            go to 302 
          endif 
          array(1,ix) = npti1 
          array(2,ix) = kp1 
          k1 = find_bus (bus1, basekv1) 
          if (k1 .le. 0) then 
            write (errbuf(1), 170) nptidc, npti1, bus1, basekv1 
  170       format (' D-C system (', i2, ') Converter bus (', i6,  
     &        1x, a8, f7.1, ') is not in system.') 
            call prterx ('W',1)                 
            error = 1                            
            go to 302 
          endif 
          array(3,ix) = k1 
          kbsdta(1,k1) = 12          ! Converter bus changed to type BM 
c 
c         Find converter commutating bus.   
c 
          ptr = kbsdta(16,k1) 
          k1c = 0 
          found = .false. 
          do while (ptr .gt. 0 .and. .not. found) 
            if (brtype(ptr) .eq. 3 .or. brtype(ptr) .eq. 4 .or.  
     &          brtype(ptr) .eq. 5) then 
              found = .true. 
              k1c = ky(ptr) 
            else 
              ptr = brnch_nxt(ptr) 
            endif 
          enddo 
          if (k1c .le. 0) then 
            write (errbuf(1), 180) nptidc, npti1, bus1, basekv1 
  180       format (' D-C system (', i2,  
     &        ') Commutating bus for Converter (', i6, 1x, a8, f7.1,  
     &        ') is not in system.') 
            call prterx ('W',1)                 
            error = 1                            
            go to 302 
          endif 
          array(4,ix) = k1c 
          br = ftn_atof (word(2)) 
          if (br .le. 0.0) then 
            write (errbuf(1), 220) nptidc, npti1, bus(k1), base(k1) 
  220       format (' D-C system (', i2,  
     &          ') Converter bus (', i6, 1x, a8, f7.1,  
     &          ') has zero bridges') 
            call prterx ('W',1)                 
            error = 1                            
            go to 302 
          endif 
          busdta(3,k1) = br 
          busdta(4,k1) = 0.0       ! Smoothing reactor 
 
          alpha = ftn_atof (word(3)) 
          if (alpha .le. 0.0) then 
            write (errbuf(1), 230) nptidc, npti1, bus(k1), base(k1) 
  230       format (' D-C system (', i2,  
     &        ') Converter bus (', i6, 1x, a8, f7.1,  
     &        ') has zero Alpha_nom') 
            call prterx ('W',1)                 
            error = 1                            
            go to 302 
          endif 
          busdta(6,k1) = 100.0 
          busdta(10,k1) = alpha 
          alphamin = ftn_atof (word(4)) 
          if (alphamin .le. 0.0) then 
            write (errbuf(1), 240) nptidc, npti1, bus(k1), base(k1) 
  240       format (' D-C system (', i2,  
     &        ') Converter bus (', i6, 1x, a8, f7.1,  
     &        ') has zero Alpha_min') 
            call prterx ('W',1)                 
            error = 1                            
            go to 302 
          endif 
          busdta(5,k1) = alphamin 
          busdta(11,k1) = alphamin 
 
          busdta(7,k1) = 0.0       ! Voltage drop / bridge 
          busdta(8,k1) = 0.0       ! Rating 
          kbsdta(9,k1) = k1c 
 
          sched = ftn_atof (word(13)) 
          if (npti1 .eq. nvconv) then 
            if (sched .le. 0.0) then 
              write (errbuf(1), 242) nptidc, npti1, bus(k1), base(k1), 
     &          sched 
  242         format (' D-C system (', i2,  
     &        ') converter bus (', i6, 1x, a8, f7.1,  
     &        ') has illegal scheduled voltage (', f8.1, ')') 
              call prterx ('W',1)                 
              error = 1                            
            endif 
            kvc = k1 
            call putchr(1, ' ', kbsdta(12,k1)) 
            busdta(13,k1) = 0.0 
            busdta(14,k1) = sched 
          else 
            pwr_rect = pwr_rect + amax1 (0.0, sched) 
            pwr_invt = pwr_invt + amin1 (0.0, sched) 
            busdta(13,k1) = sched 
            busdta(14,k1) = 0.0 
            if (sched .gt. 0.0) then 
              call putchr(1, 'R', kbsdta(12,k1)) 
            else if (alpha .eq. alphamin) then 
              call putchr(1, 'M', kbsdta(12,k1)) 
            else 
              call putchr(1, 'I', kbsdta(12,k1)) 
            endif 
          endif 
c 
c         The next test is trivial, since  
c         baseratio = base(k1) / base(k1c) and both are known. 
c 
          baseratio = ftn_atof (word(8)) 
          if (baseratio .le. 0.0) then 
            write (errbuf(1), 250) nptidc, npti1, bus(k1), base(k1) 
  250       format (' D-C system (', i2,  
     &        ') Converter bus (', i6, 1x, a8, f7.1,  
     &        ') has zero base ratio') 
            call prterx ('W',1)                 
            error = 1                            
            go to 302 
          endif 
          tol = (base(k1) / base(k1c)) / baseratio - 1.0 
          if (abs(tol) .gt. 0.15) then 
            write (errbuf(1), 260) nptidc, npti1, bus(k1), base(k1), 
     &        baseratio 
  260       format (' D-C system (', i2,  
     &        ') Converter bus (', i6, 1x, a8, f7.1,  
     &        ') has an unacceptable base ratio (', f8.4, ')') 
            call prterx ('W',1)                 
            error = 1                            
            go to 302 
          endif 
          npolar = ftn_atoi (word(16)) 
          if (npolar .lt. 0) then 
            write (errbuf(1), 270) nptidc 
  270       format (' D-C system (', i2, ') has specified negative polar 
     &ity. BPA model accepts only positive polarity') 
            errbuf(2) = ' (' // xbuf(1:60) // ')' 
            call prterx ('W',2)                 
            error = 1                            
            go to 302 
          endif 
c 
c         Replace existing converter-commutating branch with an LTC 
c 
          ptr = kbsdta(16,k1) 
          found = .false. 
          do while (ptr .gt. 0 .and. .not. found) 
            if (ky(ptr) .eq. k1c .and. brtype(ptr) .eq. 4) then 
              found = .true. 
            else               
              ptr = brnch_nxt(ptr) 
            endif 
          enddo 
          if (brtype(ptr) .ne. 4 .or. ky(ptr) .ne. k1c) then 
c 
c           Add LTC commuating record and link up branch 
 
            if (ltot+1 .ge. MAXBRN) then 
              write (errbuf(1), 280) MAXBRN 
  280         format ('More than ',i5, ' branch records.') 
              write (errbuf(2), 290) nptidc, bus(k1), base(k1),  
     &          bus(k1c), base(k1c) 
  290         format (' D-C system (', i2,  
     &          ') added record (R ', a8, f7.1, 1x, a8, f7.1, ')') 
              call prterx ('W',2)                 
              ltot = 1                            
              error = 1                            
              go to 302 
            endif 
            ltot = ltot + 1 
            kbrnch(1,ltot) = 4 
            call putchr(3, '   ', kbrnch(3,ltot)) 
            kbrnch(4,ltot) = k1 
            brnch(6,ltot) = ftn_atof(word(10)) * base(k1c) ! Tmax (kV) 
            brnch(7,ltot) = ftn_atof(word(11)) * base(k1c) ! Tmin (kV) 
            taps = ftn_atof(word(12)) 
            if (taps .gt. 0.0) then 
              numtaps = (brnch(6,ltot) - brnch(7,ltot))  
     &                / (taps * base(k1c)) + 1.5 
              if (numtaps .gt. 50) numtaps = 0 
              brnch(8,ltot) = numtaps 
            else 
              brnch(8,ltot) = 0.0 
            endif 
 
            rateln(1,ltot) = 0.0        ! Rate A 
            rateln(2,ltot) = 0.0        ! Rate B 
            rateln(3,ltot) = 0.0        ! Rate C 
 
            call lkbrdata (ltot, k1, k1c, bptr1, bptr2, error) 
            kx(bptr1) = k1 
            ky(bptr1) = k1c 
            brid(bptr1) = ' ' 
            brsect(bptr1) = 0 
            brtype(bptr1) = kbrnch(1,ltot) 
 
            kx(bptr2) = k1c 
            ky(bptr2) = k1 
            brid(bptr2) = ' ' 
            brsect(bptr2) = 0 
            brtype(bptr2) = kbrnch(1,ltot) 
          endif 
c 
c         Replace existing bus tie with commutating transformer 
c 
          ptr = kbsdta(16,k1) 
          found = .false. 
          do while (ptr .gt. 0 .and. .not. found) 
            if (ky(ptr) .eq. k1c .and.  
     &         (brtype(ptr) .eq. 3 .or. brtype(ptr) .eq. 5)) then 
              found = .true. 
            else 
              ptr = brnch_nxt(ptr) 
            endif 
          enddo 
          if (ky(ptr) .ne. k1c) then 
            write (errbuf(1), 300) nptidc, npti1, bus(k1c), base(k1c), 
     &        bus(k1), base(k1) 
  300       format (' D-C system (', i2,  
     &        ') Converter commutating bus (', i6, 1x, a8, f7.1,  
     &        ') is not adjacent to Converter.') 
            call prterx ('W',1)                 
            error = 1                            
            go to 302 
          endif 
          nbr = iabs (brnch_ptr(ptr)) 
          rt = ftn_atof (word(5)) 
          xt = ftn_atof (word(6)) 
          cx = base(k1)**2 / bmva * br 
 
          brtype(ptr) = 5 
          kbrnch(1,nbr) = brtype(ptr) 
          brnch(5,nbr) = rt / cx 
          brnch(6,nbr) = xt / cx 
          if (brnch_ptr(ptr) .gt. 0) then 
            brnch(9,nbr) = base(k1)  
            brnch(10,nbr) = base(k1c) * ftn_atof (word(9)) 
          else 
            brnch(9,nbr) = base(k1c) * ftn_atof (word(9)) 
            brnch(10,nbr) = base(k1)  
          endif 
          brnch(16,nbr) = busdta(3,k1) 
c 
c         Redefine brtype() for transpose 
c 
          ptr = kbsdta(16,k1c) 
          found = .false. 
          do while (ptr .gt. 0 .and. .not. found) 
            if (ky(ptr) .eq. k1 .and. 
     &         (brtype(ptr) .eq. 3 .or. brtype(ptr) .eq. 5)) then 
              found = .true. 
            else 
              ptr = brnch_nxt(ptr) 
            endif 
          enddo 
          if (ptr .gt. 0) brtype(ptr) = kbrnch(1,nbr) 
 
  302     continue 
c 
c         End of converter loop 
        enddo 
  304   continue 
c 
c       Determine code ("I" or "R") of voltage-controlled converter  
c 
        if (kvc .eq. 0) then 
          write (errbuf(1), 306) nptidc, nvconv 
  306     format (' D-C system (', i2,  
     &      ') specified voltage controlled bus (', i6,  
     &      ') has no assigned schedule') 
          call prterx ('W',1)                 
          error = 1                            
        else 
          if (0.90 * pwr_rect + pwr_invt .lt. 0.0) then 
            call putchr(1, 'R', kbsdta(12,kvc)) 
          else 
            call putchr(1, 'I', kbsdta(12,kvc)) 
          endif 
        endif 
c 
c       Process converter d-c records. 
c 
        count = nconv 
        do ix = 1, ndcbs 
          numrec = numrec + 1 
          read (tempfile, fmt='(a)', end=910) xbuf 
          last = index (xbuf, '/') - 1 
          if (last .le. 0) last = lastch(xbuf) 
          call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ') 

          do i = nwrd+1, 7 
            word(i) = ' ' 
          enddo 
c 
c         Set defaults 
c 
          if (word(3) .eq. ' ') word(3) = '1' 
          if (word(4) .eq. ' ') word(4) = '1' 
          if (word(5) .eq. ' ') then 
            write (word(5), 310) ix 
  310       format ('D-C # ', i2)  
          endif 
 
          nptiln = ftn_atoi(word(1)) 
          status = gtptinam (word(2), npti1, kp1, isgn1, bus1, basekv1) 
          if (npti1 .eq. 0 .and. bus1 .eq. ' ') then 
c 
c           If a converter bus is not specified, it has no a-c entity  
c           and a pseudo bus must be created. 
c 
            busname = word(5) 
            basekv = base(array(1,1)) 
            indx = add_bus (busname, basekv, ntot+1) 
C 
C           This hashing function checks for duplicates, array  
C           overflow, and automatically inserts the new bus in  
C           position ntot+1. 
c 
            if (indx .lt. 0) then 
              write (errbuf(1), 320) busname, basekv 
  320         format (' Cannot generate a unique d-c bus name (', 
     &          a8, f7.1, ')') 
              errbuf(2) = ' (' // xbuf(1:60) // ')' 
              call prterx ('W',2) 
              error = 1                                             
              go to 382 
            else if (indx .eq. 0) then 
              write (errbuf(1), 330) MAXBUS 
  330         format (' More than ',i4,' buses in base system ') 
              errbuf(2) = ' (' // xbuf(1:60) // ')' 
              call prterx ('W',2) 
              error = 1                                             
              go to 382 
            endif 
            ntot = indx 
            count = count + 1 
            array(1,count) = 0 
            array(2,count) = 0 
            array(3,count) = ntot 
            array(4,count) = 0 
            array(5,count) = nptiln 
            if (wsccflag) then 
               wsccbase(ntot) = code (basekv, 4, 0) 
            endif 
          else 
            if (bus1 .eq. ' ') then 
              write (errbuf(1), 340) nptiln, npti1 
  340         format (' D-C system (', i2, ') Converter bus (', i6,  
     &        ') is not raw data file.') 
              call prterx ('W',1)                 
              error = 1                            
              go to 382 
            endif 
            k1 = find_bus (bus1, basekv1) 
            if (k1 .le. 0) then 
              write (errbuf(1), 350) nptiln, npti1, bus1, basekv1 
  350         format (' D-C system (', i2, ') converter bus (',  
     &          i6, 1x, a8, f7.1, ') is not in system.') 
              call prterx ('W',1)                 
              error = 1                            
              go to 382 
            endif 
            found = .false. 
            i = 1 
            do while (i .le. count .and. .not. found) 
              if (array(3,i) .eq. k1) then 
                found = .true. 
                array(5,i) = nptiln 
              else 
                i = i + 1 
              endif 
            enddo 
            if (.not. found) then 
              write (errbuf(1), 360) nptiln, npti1, bus(k1), base(k1) 
  360         format (' D-C system (', i2, ') converter bus (', i6,  
     &          1x, a8, f7.1, ') has no d-c entity.') 
              call prterx ('W',1)                 
              error = 1                            
              go to 382 
            endif 
          endif 
          idc2 = ftn_atoi (word(6)) 
          if (idc2 .gt. 0) then 
            write (errbuf(1), 370) nptiln, npti1, bus(k1), base(k1) 
  370       format (' D-C system  (', i2, ') converter bus (', i6,  
     &        1x, a8, f7.1, ') is series-connected. BPA model accepts on 
     &ly parallel-connected.') 
            call prterx ('W',1)                 
            error = 1                            
            go to 382 
          endif 
          rgnd = ftn_atof (word(7)) 
          if (rgnd .gt. 0) then 
            write (errbuf(1), 380) nptiln, npti1, bus(k1), base(k1) 
  380       format (' D-C system (', i2, ') Converter bus (', i6,  
     &        1x, a8, f7.1, ') will ignore non-zero resistance to ground 
     &') 
            call prterx ('W',1)                 
            error = 1                            
          endif 
  382     continue 
        enddo 
c 
c       Process d-c line records. 
c 
        do ix = 1, ndcln 
          numrec = numrec + 1 
          read (tempfile, fmt='(a)', end=910) xbuf 
          last = index (xbuf, '/') - 1 
          if (last .le. 0) last = lastch(xbuf) 
          call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ') 
c 
c         Squeeze and blank fill comma fields (Note: first comma  
c         is false) 
c 
          first = .true. 
          i = 1 
          do while (i .le. nwrd) 
            if (word(i) .eq. ',') then 
              if (first) then 
                do j = i, nwrd-1 
                  word(j) = word(j+1) 
                enddo 
                first = (word(i) .ne. ',') 
                if (word(i) .eq. ',') word(i) = ' ' 
                nwrd = nwrd - 1 
              else 
                word(i) = ' ' 
              endif 
            else 
              first = .true. 
            endif 
            i = i + 1 
          enddo 
          do i = nwrd+1, 5 
            word(i) = ' ' 
          enddo 
c 
c         Set defaults 
c 
          if (word(3) .eq. ' ') word(3) = '1' 
          if (word(5) .eq. ' ') word(4) = '0.0' 
 
          npti1 = ftn_atoi(word(1)) 
          isgn1 = isign(1, npti1) 
          npti2 = ftn_atoi(word(2)) 
          isgn2 = isign(1, npti2) 
          npti2 = iabs (npti2) 
 
          found = .false. 
          i = 1 
          do while (i .le. count .and. .not. found) 
            if (array(5,i) .eq. npti1) then 
              found = .true. 
              k1 = array(3,i) 
            else 
              i = i + 1 
            endif 
          enddo 
          if (.not. found) then 
            write (errbuf(1), 390) nptidc, npti1, npti2 
  390       format (' D-C system (', i2, ') line (', i2, ' - ', i6,  
     &         ') is missing terminal bus 1 d-c bus entity.') 
            call prterx ('W',1)                 
            error = 1                            
            go to 460 
          endif 
          found = .false. 
          i = 1 
          do while (i .le. count .and. .not. found) 
            if (array(5,i) .eq. npti2) then 
              found = .true. 
              k2 = array(3,i) 
            else 
              i = i + 1 
            endif 
          enddo 
          if (.not. found) then 
            write (errbuf(1), 400) nptidc, npti1, npti2 
  400       format (' D-C system (', i2, ') line (', i2, ' - ', i6,  
     &         ') is missing terminal bus 2 d-c bus entity.') 
            call prterx ('W',1)                 
            error = 1                            
            go to 460 
          endif 
          if (kbsdta(1,k1) .ne. 12) then 
            call typno (type, kbsdta(1,k1)) 
            write (errbuf(1), 410) nptidc, npti1, bus(k1), base(k1), 
     &        npti2, bus(k2), base(k2), type 
  410       format (' D-C system (', i2, ') line (', i2, 1x, a8, f7.1, 
     &         ' - ', i6, 1x, a8, f7.1,  
     &         ') terminal bus 1 is incorrect type (', a, ')') 
            call prterx ('W',1)                 
            error = 1                            
            go to 460 
          endif 
          if (kbsdta(1,k2) .ne. 12) then 
            call typno (type, kbsdta(1,k2)) 
            write (errbuf(1), 420) nptidc, npti1, bus(k1), base(k1), 
     &        npti2, bus(k2), base(k2), type 
  420       format (' D-C system (', i2, ') line (', i2, 1x, a8, f7.1, 
     &         ' - ', i6, 1x, a8, f7.1,  
     &         ') terminal bus 2 is incorrect type (', a, ')') 
            call prterx ('W',1)                 
            error = 1                            
            go to 460 
          endif 
          rdc = ftn_atof (word(4)) 
          if (rdc .le. 0.0) then 
            write (errbuf(1), 430) nptidc, npti1, bus(k1), base(k1), 
     &        npti2, bus(k2), base(k2) 
  430       format (' D-C system (', i2, ') line (', i2, 1x, a8, f7.1, 
     &         ' - ', i6, 1x, a8, f7.1,  
     &         ') has zero d-c resistance ') 
            call prterx ('W',1)                 
            error = 1                            
            go to 460 
          endif 
          if (ltot+1 .ge. MAXBRN) then 
            write (errbuf(1), 440) MAXBRN 
  440       format ('More than ',i5, ' branch records. ') 
            write (errbuf(2), 450) nptidc, npti1, bus(k1), base(k1), 
     &        npti2, bus(k2), base(k2) 
  450       format (' Adding d-c system (', i2, ') line (', i2, 1x,  
     &         a8, f7.1, ' - ', i6, 1x, a8, f7.1) 
            call prterx ('W',2)                 
            ltot = 1                            
            error = 1                            
            go to 460 
          endif 
          ltot = ltot + 1 
          kbrnch(1,ltot) = 2 
          call putchr(3, '   ', kbrnch(3,ltot)) 
          brnch(4,ltot) = 0.0              ! Rating 
          brnch(5,ltot) = rdc              ! Rdc 
          do i = 6, 18 
            brnch(i,ltot) = 0.0   
          enddo 
          if (isgn1 .lt. 0 .or. isgn2 .gt. 0) then 
            kbrnch(15,ltot) = 1            ! Metered end 
          else 
            kbrnch(15,ltot) = 2            ! Controlled end 
          endif 
 
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
 
  460     continue 
        enddo 
 
  900   continue 
c 
c       If any errors are encountered, dummy read any remaining records 
c       in multi-terminal d-c set. 
c 
        do while (numrec .lt. 1+nconv+ndcbs+ndcln) 
          read (tempfile, fmt='(a)', end=910) xbuf 
          numrec = numrec + 1 
        enddo 
 
        go to 930 
 
  910   write (errbuf(1), 920) 
  920   format ('E-O-F encountered processing multi-terminal d-c data re 
     &cords in raw data file ') 
        call prterx ('W', 1) 
        error = 1                            
 
  930   continue 
      endif 
      return 
      end 
