C    @(#)ldpti2dc.f	20.6 11/11/97 
C**************************************************************** 
C 
C     File: ldpti2dc.f 
C 
C     Purpose: Routine to load PTI 2-terminal d-c data from raw data  
C              file  
C 
c     Return code:  n = 0 : Success 
c                   n = 1 : Error 
c 
C     Author: Walt Powell  Date: 21 May 1996 
C     Called by: load_pti.f 
C 
C**************************************************************** 
      integer function ldpti2dc (xbuf, tempfile, options, numver,  
     &                           error) 
      integer tempfile, error, options(*), numver 
      character *(*) xbuf 
 
      include 'ipfinc/parametr.inc' 
 
      include 'ipfinc/prt.inc' 
      include 'ipfinc/pti_data.inc' 
      include 'ipfinc/blank.inc' 
      include 'ipfinc/bus.inc' 
      include 'ipfinc/branch.inc' 
      include 'ipfinc/lfiles.inc' 
 
      common /old_branch/ old_k1, old_k2, old_sect, old_type, old_id 
      integer old_k1, old_k2, old_sect, old_type 
      character old_id * 1 
 
      character word(50)*10, type*10, meter*1, bus1*8, bus2*8 
      logical first, found 
      integer find_bus, ftn_atoi, bptr1, bptr2, array(4,2), count, ptr,  
     &        status, gtptinam, status1, status2 
 
      ldpti2dc = 0 
      error = 0 
      numrec = 1 
c 
c     Process first record.  
c 
      last = index (xbuf, '/') - 1 
      if (last .le. 0) last = lastch(xbuf) 
      call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ') 

      do i = nwrd+1, 10 
        word(i) = ' ' 
      enddo 
c 
c     Set defaults 
c 
      if (word(9) .eq. ' ') word(9) = 'I' 
 
      nptidc = ftn_atoi(word(1)) 
      if (nptidc .eq. 0) then 
        ldpti2dc = 1 
      else 
        mdc = ftn_atoi(word(2)) 
        rdc = ftn_atof(word(3)) 
        pdc = ftn_atof(word(4)) 
        vdc = ftn_atof(word(5)) 
        meter = word(9) 
        if (rdc .eq. 0.0) then 
          write (errbuf(1), 100) nptidc 
  100     format (' D-C line number (', i2, ') has no d-c line resistanc 
     &e.') 
          call prterx ('W',1)                 
          error = 1                            
        endif 
        if (pdc .eq. 0.0) then 
          write (errbuf(1), 110) nptidc 
  110     format (' D-C line number (', i2, ') has no scheduled d-c curr 
     &ent or power.') 
          call prterx ('W',1)                 
          error = 1                            
        endif 
        if (vdc .eq. 0.0) then 
          write (errbuf(1), 120) nptidc 
  120     format (' D-C line number (', i2, ') has no scheduled d-c volt 
     &age.') 
          call prterx ('W',1)                 
          error = 1                            
        endif 
        if (error .ne. 0) go to 900 
c 
c       Convert any d-c current order into d-c power order 
c 
        if (mdc .eq. 2) then 
          if (meter .eq. 'R') then 
            pdc = pdc * vdc 
          else 
            pdc = pdc * (vdc - rdc * pdc) 
          endif 
        endif             
c 
c       Process rectifier and inverter as separate records. 
c 
        type = 'Rectifier' 
        count = 1 
        do while (type .eq. 'Rectifier' .or. type .eq. 'Inverter') 
          numrec = numrec + 1 
          read (tempfile, fmt='(a)', end=910) xbuf 
          last = index (xbuf, '/') - 1 
          if (last .le. 0) last = lastch(xbuf) 
          call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ') 
c 
c         Set defaults 
c 
          if (word(10) .eq. ' ') word(10) = '1.50' 
          if (word(11) .eq. ' ') word(11) = '0.51' 
          if (word(12) .eq. ' ') word(12) = '0.00625' 
c 
c         Converter bus 
c 
          status1 = gtptinam (word(1), npti1, kp1, isgn1, bus1, basekv1) 
          if (npti1 .eq. 0 .and. bus1 .ne. ' ') then 
            write (errbuf(1), 160) nptidc, npti1 
  160       format (' D-C line number (', i2, ') Converter bus (', i6,  
     &        ') is not in system.') 
            call prterx ('W',1)                 
            error = 1                            
            go to 900 
          endif 
          array(1,count) = npti1 
          array(2,count) = kp1 
          k1 = find_bus (bus1, basekv1) 
          if (k1 .le. 0) then 
            write (errbuf(1), 170) nptidc, npti1, bus1, basekv1 
  170       format (' D-C line number (', i2, ') Converter bus (', i6,  
     &        1x, a8, f7.1, ') is not in system.') 
            call prterx ('W',1)                 
            error = 1                            
            go to 900 
          endif 
          array(3,count) = k1 
c 
c         Converter commutating bus 
c 
          status2 = gtptinam (word(14), npti2, kp2, isgn2, bus2,  
     &                        basekv2) 
          if (npti2 .eq. 0 .and. bus2 .eq. ' ') then 
            ptr = kbsdta(16,k1) 
            k1c = 0 
            found = .false. 
            do while (ptr .gt. 0 .and. .not. found) 
              if (brtype(ptr) .eq. 3 .or. brtype(ptr) .eq. 4 .or.  
     &            brtype(ptr) .eq. 5) then 
                found = .true. 
                k1c = ky(ptr) 
              else 
                ptr = brnch_nxt(ptr) 
              endif 
            enddo 
            if (k1c .le. 0) then 
              write (errbuf(1), 180) nptidc, npti1, bus1, basekv1 
  180         format (' D-C line number (', i2,  
     &          ') Converter bus (', i6, 1x, a8, f7.1,  
     &          ') is not adjacent to an a-c commutator bus.') 
              call prterx ('W',1)                 
              error = 1                            
              go to 900 
            endif 
          else 
            if (npti2 .ne. 0 .and. bus2 .eq. ' ') then 
              write (errbuf(1), 190) nptidc, npti2 
  190         format (' D-C line number (', i2,  
     &         ') commutating bus (', i6,  
     &         ') is not in system.') 
              call prterx ('W',1)                 
              error = 1                            
              go to 900 
            endif 
            k1c = find_bus (bus2, basekv2) 
            if (k1c .le. 0) then 
              write (errbuf(1), 200) nptidc, npti2, bus2, basekv2 
  200         format (' D-C line number (', i2,  
     &          ') commutating bus (', i6, 1x, a8, f7.1,  
     &          ') is not in system.') 
              call prterx ('W',1)                 
              error = 1                            
              go to 900 
            endif 
            ptr = kbsdta(16,k1) 
            k1cx = 0 
            found = .false. 
            do while (ptr .gt. 0 .and. .not. found) 
              if (brtype(ptr) .eq. 3 .or. brtype(ptr) .eq. 4 .or.  
     &            brtype(ptr) .eq. 5) then 
                found = .true. 
                k1cx = ky(ptr) 
              else 
                ptr = brnch_nxt(ptr) 
              endif 
            enddo 
            if (k1c .ne. k1cx) then 
              write (errbuf(1), 210) nptidc, npti2, bus(k1c), base(k1c), 
     &          bus(k1), base(k1) 
  210         format (' D-C line number (', i2,  
     &          ') commutating bus (', i6, 1x, a8, f7.1,  
     &          ') is not adjacent to Converter.') 
              call prterx ('W',1)                 
              error = 1                            
              go to 900 
            endif 
          endif 
          array(4,count) = k1c 
          kbsdta(1,k1) = 5          ! Converter bus changed to type BD 
          br = ftn_atof (word(2)) 
          if (br .le. 0.0) then 
            write (errbuf(1), 220) nptidc, npti1, bus(k1), base(k1) 
  220       format (' D-C line number (', i2,  
     &          ') Converter bus (', i6, 1x, a8, f7.1,  
     &          ') has zero bridges') 
            call prterx ('W',1)                 
            error = 1                            
            go to 900 
          endif 
          busdta(3,k1) = br 
          busdta(4,k1) = 0.0       ! Smoothing reactor 
 
          alpha = ftn_atof (word(3)) 
          if (alpha .le. 0.0) then 
            write (errbuf(1), 230) nptidc, npti1, bus(k1), base(k1) 
  230       format (' D-C line number (', i2,  
     &        ') Converter bus (', i6, 1x, a8, f7.1,  
     &        ') has zero Alpha_nom') 
            call prterx ('W',1)                 
            error = 1                            
            go to 900 
          endif 
          busdta(6,k1) = 100.0 
          if (count .eq. 1) alphanom = alpha 
 
          alphamin = ftn_atof (word(4)) 
          if (alphamin .le. 0.0) then 
            write (errbuf(1), 240) nptidc, npti1, bus(k1), base(k1) 
  240       format (' D-C line number (', i2,  
     &        ') Converter bus (', i6, 1x, a8, f7.1,  
     &        ') has zero Alpha_min') 
            call prterx ('W',1)                 
            error = 1                            
            go to 900 
          endif 
          busdta(5,k1) = alphamin 
          if (count .eq. 2) gammamin = alphamin 
 
          busdta(7,k1) = 0.0       ! Voltage drop / bridge 
          busdta(8,k1) = 2000.0 * abs(pdc) / vdc   ! Rating 
          kbsdta(9,k1) = k1c 
c 
c         The next test is trivial, since  
c         baseratio = base(k1) / base(k1c) and both are known. 
c 
          baseratio = ftn_atof (word(8)) 
          if (baseratio .le. 0.0) then 
            write (errbuf(1), 250) nptidc, npti1, bus(k1), base(k1) 
  250       format (' D-C line number (', i2,  
     &        ') Converter bus (', i6, 1x, a8, f7.1,  
     &        ') has zero baseratio') 
            call prterx ('W',1)                 
            error = 1                            
            go to 900 
          endif 
          tol = (base(k1) / base(k1c)) / baseratio - 1.0 
          if (abs(tol) .gt. 0.15) then 
            write (errbuf(1), 260) nptidc, npti1, bus(k1), base(k1), 
     &        baseratio 
  260       format (' D-C line number (', i2,  
     &        ') converter bus (',  
     &        i6, 1x, a8, f7.1, ') has an unacceptable baseratio (',  
     &        f8.4, ')') 
            call prterx ('W',1)                 
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
              write (errbuf(1), 270) MAXBRN 
  270         format ('More than ',i5, ' branch records.') 
              write (errbuf(2), 280) nptidc, bus(k1), base(k1),  
     &          bus(k1c), base(k1c) 
  280         format (' D-C line number (', i2,  
     &          ') added record (R ', a8, f7.1, 1x, a8, f7.1, ')') 
              call prterx ('W',2)                 
              ltot = 1                            
              error = 1                            
              go to 900 
            endif 
            ltot = ltot + 1 
            kbrnch(1,ltot) = 4 
            call putchr(1, ' ', kbrnch(3,ltot)) 
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
            write (errbuf(1), 290) nptidc, npti2, bus(k1c), base(k1c), 
     &        bus(k1), base(k1) 
  290       format (' D-C line number (', i2,  
     &        ') Converter commutating bus (', i6, 1x, a8, f7.1,  
     &        ') is not adjacent to Converter.') 
            call prterx ('W',1)                 
            error = 1                            
            go to 900 
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
c 
c         End of converter loop 
c 
          if (type .eq. 'Rectifier') then 
             type = 'Inverter' 
             count = 2 
          else 
             type = ' ' 
          endif 
        enddo 
c 
c       Add d-c record oriented rectifier-inverter and link up branch 
 
        if (pdc .gt. 0) then 
          k1 = array(3,1) 
          k2 = array(3,2) 
        else 
          k1 = array(3,2) 
          k2 = array(3,1) 
          pdc = -pdc 
        endif           
        if (ltot+1 .ge. MAXBRN) then 
          write (errbuf(1), 300) MAXBRN 
  300     format ('More than ',i5, ' branch records. ') 
          write (errbuf(2), 310) nptidc, bus(k1), base(k1), bus(k2),  
     &      base(k2) 
  310     format (' D-C line number (', i2,  
     &      ') added record (LD ', a8, f7.1, 1x, a8, f7.1, ')') 
          call prterx ('W',2)                 
          ltot = 1                            
          error = 1                            
          go to 900 
        endif 
        ltot = ltot + 1 
        kbrnch(1,ltot) = 7 
        call putchr(1, ' ', kbrnch(3,ltot)) 
        brnch(4,ltot) = 2000.0 * abs(pdc) / vdc ! Rating 
        brnch(5,ltot) = rdc              ! Rdc 
        brnch(6,ltot) = 0.0              ! Ldc 
        brnch(7,ltot) = 0.0              ! Cdc 
        brnch(8,ltot) = pdc              ! Pdc 
        brnch(9,ltot) = vdc              ! Vdc 
        brnch(10,ltot) = alphanom 
        brnch(11,ltot) = 0.0               
        brnch(12,ltot) = 0.0               
        brnch(13,ltot) = 0.0               
        kbrnch(14,ltot) = 0              ! Default 
        if (isgn1 .lt. 0) then 
          kbrnch(15,ltot) = 1 
        else 
          kbrnch(15,ltot) = 2 
        endif 
        brnch(16,ltot) = 0.0               
        kbrnch(17,ltot) = 0               
        brnch(18,ltot) = gammamin 
 
        rateln(1,ltot) = 0.0        ! Rate A 
        rateln(2,ltot) = 0.0        ! Rate B 
        rateln(3,ltot) = 0.0        ! Rate C 
c 
c       Ignore "meter"; hard-code metering point to "R" 
c 
        ksect = 1 
 
        call lkbrdata (ltot, k1, k2, bptr1, bptr2, error) 
        kx(bptr1) = k1 
        ky(bptr1) = k2
        brid(bptr1) = ' ' 
        brsect(bptr1) = ksect 
        brtype(bptr1) = kbrnch(1,ltot) 
 
        kx(bptr2) = k2                                                  
        ky(bptr2) = k1 
        brid(bptr2) = ' ' 
        brsect(bptr2) = ksect 
        brtype(bptr2) = kbrnch(1,ltot) 
 
  900   continue 
c 
c       If any errors are encountered, dummy read any remaining records 
 
c       in 2-terminal d-c set. 
c 
        do i = numrec+1, 3 
          read (tempfile, fmt='(a)', end=910) xbuf 
        enddo 
        go to 930 
 
  910   write (errbuf(1), 920) 
  920   format ('E-O-F encountered processing 2-terminal d-c data record 
     & in raw data file ') 
        call prterx ('W', 1) 
        error = 1                            
 
  930   continue 
      endif 
 
      return 
      end 
