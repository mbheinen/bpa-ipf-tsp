C    @(#)ldptisec.f	20.14 5/27/99
C**************************************************************** 
C 
C     File: ldptisec.f 
C 
C     Purpose: Routine to load PTI Multi-section Line Grouping data  
C              from raw data file  
C 
c     Return code:  n = 0 : Success 
c                   n = 1 " Error 
c 
C     Author: Walt Powell  Date: 21 May 1996 
C     Called by: load_pti.f 
C 
C**************************************************************** 
      integer function ldptisec (xbuf, options, numver, error) 
      integer error, options(*), numver 
      character *(*) xbuf 
 
      include 'ipfinc/parametr.inc' 
 
      include 'ipfinc/prt.inc' 
      include 'ipfinc/pti_data.inc' 
      include 'ipfinc/blank.inc' 
      include 'ipfinc/bus.inc' 
      include 'ipfinc/branch.inc' 
 
      character word(26)*10, id*1, busname*8, temp(2)*120, newtemp*120,
     &          bus1*8, bus2*8, bus3*8 
      logical first, found, finished 
      integer gtptinam, find_bus, status, nsec, section(4,13),
     &        ptr, pold, rename_bus, count, bptr1, bptr2, ptr9, ptr10, 
     &        gtpieqiv 
 
      ldptisec = 0 
      error = 0 
      last = index (xbuf, '/') - 1 
      if (last .le. 0) last = lastch(xbuf) 
      call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ')    
 
      do i = nwrd+1, 24 
        word(i) = ' ' 
      enddo 
c 
c     Set defaults 
c 
      if (word(3) .eq. ' ') word(3) = '&1' 
      if (word(4) .eq. ' ') word(4) = '1' 
 
      status = gtptinam (word(1), npti1, kp1, isgn1, bus1, basekv1) 
      if (npti1 .eq. 0 .and. bus1 .eq. ' ') then 
        ldptisec = 1
        go to 900 
      endif 
      if (bus1 .eq. ' ') then 
        write (errbuf(1), 100) npti1 
  100   format (' Terminal 1 Bus (', i6,  
     &    ') for Multi-section line grouping record (', i6,  
     &    ') is not in raw data file') 
        errbuf(2) = ' (' // xbuf(1:60) // ')' 
        call prterx ('W',2) 
        error = 1 
        go to 900 
      endif 
      k1 = find_bus (bus1, basekv1) 
      if (k1 .le. 0) then 
        write (errbuf(1), 110) npti1, bus1, basekv1 
  110   format (' Terminal 1 bus (', i6,  
     &    1x, a8, f7.1,  
     &   ') for Multi-section line grouping record is not in system')
        errbuf(2) = ' (' // xbuf(1:60) // ')' 
        call prterx ('W',2) 
        error = 1                            
        go to 900 
      endif 
      status = gtptinam (word(2), npti2, kp2, isgn2, bus2, basekv2) 
      if (bus2 .eq. ' ') then 
        write (errbuf(1), 120) npti2 
  120   format (' Terminal 2 Bus (', i6,  
     &    ') for Multi-section line grouping record (', i6,  
     &    ') is not in raw data file') 
        errbuf(2) = ' (' // xbuf(1:60) // ')' 
        call prterx ('W',2) 
        error = 1 
        go to 900 
      endif 
      k2 = find_bus (bus2, basekv2) 
      if (k2 .le. 0) then 
        write (errbuf(1), 130) npti2, bus2, basekv2 
  130   format (' Terminal 2 bus (', i6, 1x, a8, f7.1,  
     &   ') for Multi-section line grouping record is not in system') 
        errbuf(2) = ' (' // xbuf(1:60) // ')' 
        call prterx ('W',2) 
        error = 1                            
        go to 900 
      endif 
      id = word(4) 
      nsec = 1 
c 
c     First section is terminal bus 1 
c 
      section(1,nsec) = kp1 
      section(2,nsec) = ichar(id) 
      section(3,nsec) = k1 
      section(4,nsec) = 0 
      i = 1 
      finished = .false. 
      do while (i .le. 11 .and. .not. finished) 
        j = 2*i + 3 
        if (word(j) .eq. ' ') then
          finished = .true.
        else
          status = gtptinam (word(j), npti3, kp3, isgn3, bus3, 
     &                       basekv3) 
          section(1,i+1) = kp3 
          section(2,i+1) = ichar(word(j+1)(1:1)) 
          section(3,i+1) = 0 
          section(4,i+1) = 0 
          if (kp3 .le. 0) then 
            write (errbuf(1), 140) section(1,nsec) 
  140       format (' Passive Bus (', i6,  
     &   ') for Multi-section line grouping record is not in raw data fi
     &le') 
            errbuf(2) = ' (' // xbuf(1:60) // ')' 
            call prterx ('W',2) 
            error = 1 
            go to 900 
          endif 
          nsec = i + 1 
          k3 = find_bus (bus3, basekv3) 
          if (k3 .le. 0) then 
            write (errbuf(1), 150) section(1,j), bus3, basekv3 
  150       format (' Passive bus (', i6, 1x, a8, f7.1,  
     &   ') for Multi-section line grouping record is not in system') 
            errbuf(2) = ' (' // xbuf(1:60) // ')' 
            call prterx ('W',2) 
            error = 1                            
            go to 900 
          endif 
          section(3,nsec) = k3 
          count = 0 
          ptr = kbsdta(16,k3) 
          do while (ptr .gt. 0) 
            count = count + 1 
            ptr = brnch_nxt(ptr) 
          enddo 
          if (count .ne. 2) then 
            write (errbuf(1), 152) k3, bus(k3), base(k3) 
  152       format (' Passive bus (', i6, 1x, a8, f7.1,  
     &   ') for Multi-section line grouping record does not have exacty
     &two branches') 
            errbuf(2) = ' (' // xbuf(1:60) // ')' 
            call prterx ('W',2) 
            error = 1                            
            go to 900 
          endif 
          i = i + 1 
        endif
      enddo 
c 
c     Last section is terminal bus 2 
c 
      nsec = nsec + 1 
      section(1,nsec) = kp2 
      section(2,nsec) = ichar(id) 
      section(3,nsec) = k2 
      section(4,nsec) = 0 
c 
c     Add an entry for the pi-equivalent section 
C 
      if (ltot+1 .ge. MAXBRN) then 
        write (errbuf(1), 170) MAXBRN 
  170   format ('More than ',i5, 
     &          ' branch records. Overflow occurred at branch:')  
        errbuf(2) = ' (' // xbuf(1:60) // ')' 
        call prterx ('E',2)                 
        ltot = 1                            
        error = 1                            
        go to 900 
      endif 
 
      ltot = ltot + 1 
      call lkbrdata (ltot, k1, k2, bptr1, bptr2, error) 
      kx(bptr1) = k1                  
      ky(bptr1) = k2 
      brid(bptr1) = id 
      brsect(bptr1) = 0 
      brtype(bptr1) = 1 
 
      kx(bptr2) = k2                  
      ky(bptr2) = k1 
      brid(bptr2) = id 
      brsect(bptr2) = 0 
      brtype(bptr2) = 1 
 
      do i = 1, 18 
        brnch(i,ltot) = 0.0 
      enddo 
      call putchr (1, id, kbrnch(13,ltot)) 
      do i = 1, 3 
        rateln(i,ltot) = 0.0 
      enddo 
      if (isgn1 .lt. 0) then 
        kbrnch(15,ltot) = 2 
      else 
        kbrnch(15,ltot) = 1 
      endif 
c 
c     Now delete all passive section buses and regroup their  
c     associated branch data 
c 
      do i = 1, nsec-1 
        k1x = section(3,i) 
        k2x = section(3,i+1) 
c 
c       Delete branch pointers k1x - k2x 
c 
        pold = 0 
        ptr = kbsdta(16,k1x) 
        found = .false. 
        do while (ptr .gt. 0 .and. .not. found) 
          if (ky(ptr) .eq. k2x) then 
            found = .true. 
            section(4,i) = brnch_ptr(ptr)                
            if (pold .eq. 0) then 
              kbsdta(16,k1x) = brnch_nxt(ptr) 
            else 
              brnch_nxt(pold) = brnch_nxt(ptr) 
            endif 
          else 
            pold = ptr 
            ptr = brnch_nxt(ptr) 
          endif 
        enddo 
        if (.not. found) then 
          write (errbuf(1), 180) bus(k1x), base(k1x), bus(k2x),  
     &      base(k2x) 
  180     format (' Error regrouping multi-section branch (', a8,  
     &      f7.1, 1x, a8, f7.1, ')') 
          errbuf(2) = ' (' // xbuf(1:60) // ')' 
          call prterx ('W',2) 
          error = 1                            
          go to 900 
        endif 
c 
c       Delete branch pointers k2x - k1x 
c 
        pold = 0 
        ptr = kbsdta(16,k2x) 
        found = .false. 
        do while (ptr .gt. 0 .and. .not. found) 
          if (ky(ptr) .eq. k1x) then 
            found = .true. 
            if (pold .eq. 0) then 
              kbsdta(16,k2x) = brnch_nxt(ptr) 
            else 
              brnch_nxt(pold) = brnch_nxt(ptr) 
            endif 
          else 
            pold = ptr 
            ptr = brnch_nxt(ptr) 
          endif 
        enddo 
        if (.not. found) then 
          write (errbuf(1), 180) bus(k2x), base(k2x), bus(k1x),  
     &      base(k1x) 
          errbuf(2) = ' (' // xbuf(1:60) // ')' 
          call prterx ('W',2) 
          error = 1                            
          go to 900 
        endif 
c 
c       Now relink branch (k1 - k2 - id - sect) 
c 
        nbr = section(4,i) 
        inbr = iabs(nbr) 
        call lkbrdata (nbr, k1, k2, bptr1, bptr2, error) 
        kx(bptr1) = k1                  
        ky(bptr1) = k2 
        brid(bptr1) = id 
        brsect(bptr1) = i  
        brtype(bptr1) = kbrnch(1,inbr) 
          
        kx(bptr2) = k2                  
        ky(bptr2) = k1 
        brid(bptr2) = id 
        brsect(bptr2) = i 
        brtype(bptr2) = kbrnch(1,inbr) 
c 
c       Transpose data if not oriented k1-k2 
c 
        if (brnch_ptr(bptr1) .lt. 0) call txposebr (bptr1, bptr2) 
 
      enddo 
 
      do i = 2, nsec-1 
        k1x = section(3,i) 

c       Remove bus from bus hash table by renaming 
c 
        busname = srtlst 
        basekv = 9999.0 
        status = rename_bus (k1x, busname, basekv) 
c 
c       Flag k1x as deleted 
c 
        do j = 1, 11 
          kbsdta(j,k1x) = 0 
        enddo 
        capcor(1,k1x) = 0.0 
        capcor(2,k1x) = -9.0e10 
      enddo     
c 
c     If more that 9 sections, consolidate the 9-th and 10-th sections 
c 
      if (nsec .gt. 10) then 
        write (errbuf(1), 190) nsec-1 
  190   format ('More than ',i5, ' branch sections. 9-th and 10-th secti
     &ons consolidated') 
        errbuf(2) = ' (' // xbuf(1:60) // ')' 
        call prterx ('W', 2)                 
 
        ptr9 = kbsdta(16,k1) 
        found = .false. 
        do while (ptr9 .gt. 0 .and. .not. found) 
          if (ky(ptr9) .eq. k2 .and. brid(ptr9) .eq. id .and. 
     &        brsect(ptr9) .eq. 9) then 
            found = .true. 
            call bcdbrn (ptr9, temp(1), ierror) 
          else 
            ptr9 = brnch_nxt(ptr9) 
          endif 
        enddo 
 
        ptr10 = kbsdta(16,k1) 
        found = .false. 
        do while (ptr10 .gt. 0 .and. .not. found) 
          if (ky(ptr10) .eq. k2 .and. brid(ptr10) .eq. id .and. 
     &        brsect(ptr10) .eq. 10) then 
            found = .true. 
            brsect(ptr10) = 9 
            call bcdbrn (ptr10, temp(2), ierror) 
            brsect(ptr10) = 10 
          else 
            ptr10 = brnch_nxt(ptr10) 
          endif 
        enddo 
 
        if (ptr9 .eq. 0 .or. ptr10 .eq. 0) then 
          write (errbuf(1), 200) bus(k1), base(k1), bus(k2), base(k2), 
     &      id 
  200     format ('Error consolidating sections 9 and 10 in branch ', 
     &      a8, f6.1, 1x, a8, f7.1, 1x, a) 
          errbuf(2) = ' (' // xbuf(1:60) // ')' 
          call prterx ('E',2)                 
          error = 1                            
          go to 900 
        endif 
        numtemp = 2 
        status = gtpieqiv (numtemp, temp, newtemp) 
        if (status .eq. 0) then 
          write (outbuf, 210)  
  210     format ('Old sections 9 and 10:') 
          call prtout (1) 
          write (outbuf, 220) temp(1)(1:80) 
  220     format (' (', a, ')') 
          call prtout (1) 
          write (outbuf, 220) temp(2)(1:80) 
          call prtout (1) 
          write (outbuf, 230)  
  230     format ('New equivalent section 9:') 
          call prtout (1) 
          write (outbuf, 220) newtemp(1:80) 
          call prtout (1) 
          nbr = iabs(brnch_ptr(ptr9)) 
          read (newtemp, 240, err=900) (brnch(k,nbr),k=5,10)          
  240     format (bz, 38x, 6f6.5)       
c 
c         Now delete section 10, proceding from k1 to k2 
c 
          pold = 0 
          ptr = kbsdta(16,k1) 
          found = .false. 
          do while (ptr .gt. 0 .and. .not. found) 
            if (ky(ptr) .eq. k2 .and. brid(ptr) .eq. id .and. 
     &          brsect(ptr) .eq. 10) then 
              found = .true. 
              if (pold .eq. 0) then 
                kbsdta(16,k1) = brnch_nxt(ptr) 
              else 
                brnch_ptr(pold) = brnch_nxt(ptr) 
              endif 
            else 
              pold = ptr 
              brnch_nxt(ptr) = ptr 
            endif 
          enddo 
c 
c         Now delete section 10, proceding from k2 to k1 
c 
          pold = 0 
          ptr = kbsdta(16,k2) 
          found = .false. 
          do while (ptr .gt. 0 .and. .not. found) 
            if (ky(ptr) .eq. k1 .and. brid(ptr) .eq. id .and. 
     &          brsect(ptr) .eq. 10) then 
              found = .true. 
              if (pold .eq. 0) then 
                kbsdta(16,k2) = brnch_nxt(ptr) 
              else 
                brnch_nxt(pold) = brnch_nxt(ptr) 
              endif 
            else 
              pold = ptr 
              brnch_nxt(ptr) = ptr 
            endif 
          enddo 
c 
c         Now delete section 10 in its entirety 
c 
          nbr = iabs(brnch_ptr(ptr10)) 
          do i = 1, 18 
            kbrnch(i,nbr) = 0 
          enddo 
c 
c         Flag ptr as deleted 
c 
          brnch_ptr(ptr10) = 0 
          brnch_nxt(ptr10) = 0 
 
        endif 
   
      endif 
 
 
  900 continue 
      return 
      end 
