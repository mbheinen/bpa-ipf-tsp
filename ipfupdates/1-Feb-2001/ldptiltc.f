C    @(#)ldptiltc.f	20.9 1/8/99  
C****************************************************************   
C   
C     File: ldptiltc.f   
C   
C     Purpose: Routine to load PTI LTC Transformer data from raw data    
C              file    
C   
c     Return code:  n = 0 : Success   
c                   n = 1 : Error   
c   
C     Author: Walt Powell  Date: 21 May 1996   
C     Called by: load_pti.f   
C   
C****************************************************************   
      integer function ldptiltc (xbuf, options, numver, error)   
      integer error, options(*), numver   
      character *(*) xbuf   
   
      include 'ipfinc/parametr.inc'   
   
      include 'ipfinc/prt.inc'   
      include 'ipfinc/pti_data.inc'   
      include 'ipfinc/blank.inc'   
      include 'ipfinc/alpha.inc'   
      include 'ipfinc/branch.inc'   
      include 'ipfinc/bus.inc'   
   
      common /old_branch/ old_k1, old_k2, old_sect, old_type, old_id   
      integer old_k1, old_k2, old_sect, old_type, status1, status2   
      character old_id * 1   
   
      character word(20)*10, type*1, brntyp*1, bus1*8, bus2*8, bus3*8   
      logical first  
      integer gtptinam, ftn_atoi, status, bptr1, bptr2, ptr, find_bus,  
     &        ptr_tx   
   
      ldptiltc = 0   
      error = 0   
      last = index (xbuf, '/') - 1   
      if (last .le. 0) last = lastch(xbuf)   
      call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ')   

      do i = nwrd+1, 13   
        word(i) = ' '   
      enddo   
c   
c     Add defaults   
c   
      if (word(5) .eq. ' ') word(5) = '1.50'   
      if (word(6) .eq. ' ') word(6) = '0.51'   
      if (word(7) .eq. ' ') word(7) = '1.50'   
      if (word(8) .eq. ' ') word(8) = '0.51'   
      if (word(9) .eq. ' ') word(9) = '0.00625'   
   
      status1 = gtptinam (word(1), npti1, kp1, isgn1, bus1, basekv1)   
      if (npti1 .eq. 0 .and. bus1 .eq. ' ') then   
        ldptiltc = 1   
      else  
        status2 = gtptinam (word(2), npti2, kp2, isgn2, bus2, basekv2)   
        status3 = gtptinam (word(4), npti3, kp3, isgn3, bus3, basekv3)   
        if (word(11) .eq. ' ') then   
          status = 1   
        else   
          status = ftn_atoi(word(11))   
        endif   
      endif  
      if (npti1 .eq. 0 .and. bus1 .eq. ' ') then  
      else if (npti2 .eq. 0 .and. bus2 .eq. ' ') then   
      else if (status .eq. 0) then   
      else   
        if (ltot+1 .ge. MAXBRN) then   
          write (errbuf(1), 10020) MAXBRN   
10020     format ('More than ',i5,   
     &            ' branch records. Overflow occurred at branch:')    
          write (errbuf(2), 10030) xbuf(1:80)      
10030     format(11x, '(', a, ')')            
          call prterx ('E',2)                   
          ltot = 1                              
          error = 1                              
          go to 900   
        endif   
        k1 = find_bus (bus1, basekv1)   
        k2 = find_bus (bus2, basekv2)   
        if (k1 .le. 0) then   
          write (errbuf(1), 10040) npti1, bus1, basekv1, npti2, bus2,    
     &      basekv2   
10040     format (' PTI bus1 (', i6, 1x, a8, f7.1, ' - ', i6, 1x, a8,    
     &       f7.1, ') is not in system.')   
          call prterx ('W',1)                   
          error = 1                              
          go to 900   
        else if (k2 .le. 0) then   
          write (errbuf(1), 10050) npti1, bus1, basekv1, npti2, bus2,   
     &      basekv2                                 
10050     format (' PTI bus2 (', i6, 1x, a8, f7.1, ' - ', i6, 1x, a8,    
     &       f7.1, ') is not in system.')   
          call prterx ('W',1)                   
          error = 1                              
          go to 900   
        endif   
c  
c       Obtain count of lines and transformers  
c  
        num_lns = 0  
        num_txs = 0  
        num_ltcs = 0  
        ptr_tx = 0  
  
        ptr = kbsdta(16,k1)   
        do while (ptr .gt. 0)  
          if (ky(ptr) .eq. k2) then   
            if (brtype(ptr) .eq. 5 .or. brtype(ptr) .eq. 6) then  
              num_txs = num_txs + 1  
              if (num_txs .eq. 1) ptr_tx = ptr  
            else if (brtype(ptr) .eq. 4) then  
              num_ltcs = num_ltcs + 1  
            else  
              num_lns = num_lns + 1  
            endif  
          endif   
          ptr = brnch_nxt(ptr)   
        enddo   
  
        ptr = ptr_tx  
        if (num_txs+num_lns .eq. 0) then   
          write (errbuf(1), 10060) npti1, bus1, basekv1, npti2, bus2,   
     &      basekv2                                 
10060     format (' PTI LTC is missing branch (', i6, 1x, a8, f7.1,    
     &       ' - ', i6, 1x, a8, f7.1, ')')   
          call prterx ('W',1)                   
          error = 1                              
          go to 900   
        else if (num_txs .eq. 0) then   
          type = brntyp(brtype(ptr))   
          write (errbuf(1), 10062) type, npti1, bus1, basekv1,   
     &       npti2, bus2, basekv2   
10062     format (' Incorrect type (', a,    
     &       ') for PTI LTC Phase Shifter (', i6, 1x, a8, f7.1,    
     &       ' - ', i6, 1x, a8, f7.1, ')')   
          call prterx ('W',1)                   
          error = 1                              
          go to 900   
        else if (num_lns .gt. 0) then   
          write (errbuf(1), 10064) npti1, bus1, basekv1, npti2, bus2,   
     &      basekv2                                 
10064     format (' PTI LTC (', i6, 1x, a8, f7.1,    
     &       ' - ', i6, 1x, a8, f7.1,   
     &       ') has parallel lines/transformers')   
          call prterx ('W',1)                   
          error = 1                              
          go to 900   
        else if (num_ltcs .gt. 0) then  
          write (errbuf(1), 10070) npti1, bus1, basekv1, npti2,    
     &      bus2, basekv2                           
10070     format (' Duplicate records for PTI LTC (', i6,    
     &      1x, a8, f7.1, ' - ', i6, 1x, a8, f7.1, ')')   
          call prterx ('W',1)                   
          go to 900   
        else  
   
          ltot = ltot + 1   
          kbrnch(1,ltot) = 4   
          tmax = ftn_atof (word(5))   
          tmin = ftn_atof (word(6))   
   
          if (brtype(ptr) .eq. 5 .and. tmax - tmin .lt. 1.0) then   
c   
c           A regular LTC transformer is confirmed as both   
c   
c           1. ANGLE = 0.0 (brtype(ptr) = 5)   
c           2. TMAX - TMIN < 1.0   
c   
            call putchr(1, ' ', kbrnch(3,ltot))   
            brnch(6,ltot) = tmax * base(k1)        ! Tmax (kV)   
            brnch(7,ltot) = tmin * base(k1)        ! Tmin (kV)   
            taps = ftn_atof(word(9))   
            if (taps .gt. 0.0) then   
              numtaps = (brnch(6,ltot) - brnch(7,ltot))    
     &                / (taps * base(k1)) + 1.5   
              if (numtaps .gt. 50) numtaps = 0   
              brnch(8,ltot) = numtaps   
            else   
              brnch(8,ltot) = 0.0   
            endif   
            if (npti3 .gt. 0 .or. bus3 .ne. ' ') then   
              k3 = find_bus (bus3, basekv3)   
              if (k3 .le. 0) then   
                write (errbuf(1), 10080) npti3, bus3, basekv3,   
     &             npti1, bus1, basekv1, npti2, bus2, basekv2   
10080           format (' PTI Controlled bus (', i6, 1x, a8, f7.1,    
     &             ') on branch record (', i6, 1x, a8, f7.1, ' - ', i6,   
     &             1x, a8, f7.1, ') is not in system.')   
                call prterx ('W',1)                   
                error = 1                              
                go to 900   
              endif   
              if (k3 .eq. k1) then   
                kbrnch(4,ltot) = k1   
              else if (k3 .eq. k2) then   
                kbrnch(4,ltot) = k2   
              else if (isgn2 .lt. 0) then   
                kbrnch(4,ltot) = -1   
              else   
                kbrnch(4,ltot) = -2   
              endif   
   
              vmax = ftn_atof(word(7))  ! Vmax (p.u.)   
              vmin = ftn_atof(word(8))  ! Vmin (p.u.)   
              if (options(8) .eq. 1) then   
                if (kbsdta(1,k3) .eq. 1) kbsdta(1,k3) = 10   
                busdta(11,k3) = 0.5 * (vmax + vmin)   
                busdta(12,k3) = busdta(11,k3)   
              else if (options(8) .eq. 2) then   
                if (kbsdta(1,k3) .eq. 1) kbsdta(1,k3) = 10   
                busdta(11,k3) = vmax   
                busdta(12,k3) = busdta(11,k3)   
              else if (options(8) .eq. 3) then   
                if (kbsdta(1,k3) .eq. 1) kbsdta(1,k3) = 10   
                busdta(11,k3) = vmin   
                busdta(12,k3) = busdta(11,k3)   
              else if (kbsdta(1,k3) .eq. 1) then   
                kbsdta(1,k3) = 10   
                busdta(11,k3) = 0.5 * (vmax + vmin)   
                busdta(12,k3) = busdta(11,k3)   
              else    
                busdta(11,k3) = 0.5 * (vmax + vmin)   
                busdta(12,k3) = busdta(11,k3)   
              endif   
              if (kbsdta(1,k3) .eq. 10) busdta(12,k3) = 0.0   
            else   
              kbrnch(4,ltot) = 0   
              call putchr(1, 'N', kbrnch(3,ltot))   
              brnch(9,ltot) = ftn_atof (word(7))          ! Qmax (MVAR)   
              brnch(10,ltot) = ftn_atof (word(8))         ! Qmin (MVAR)   
            endif   
          else   
c   
c           A phase shifter is confirmed as either   
c   
c           1. ANGLE >< 0.0 (brtype(ptr) = 6)   
c           2. TMAX - TMIN > 1.0   
c   
            if (brtype(ptr) .eq. 5) then   
              nbr = iabs(brnch_ptr(ptr))   
              brtype(ptr) = 6   
              kbrnch(1,nbr) = 6   
              brnch(9,nbr) = 0.0   
              brnch(10,nbr) = 0.0   
            endif   
            call putchr(1, 'M', kbrnch(3,ltot))   
            kbrnch(4,ltot) = 0   
            brnch(6,ltot) = tmax               ! Tmax (degrees)   
            brnch(7,ltot) = tmin               ! Tmin (degrees)   
            brnch(8,ltot) = 0.0                ! Taps   
            brnch(9,ltot) = ftn_atof(word(7))  ! Pmax (MW)   
            brnch(10,ltot) = ftn_atof(word(8)) ! Pmin (MW)   
          endif   
   
c         link up branch   
   
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
      endif   
  900 continue   
      return   
      end   
