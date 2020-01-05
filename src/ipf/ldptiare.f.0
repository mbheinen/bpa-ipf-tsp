C    @(#)ldptiare.f	20.10 3/29/99
C**************************************************************** 
C 
C     File: ldptiare.f 
C 
C     Purpose: Routine to load PTI LTC Area Interchange data from  
C              raw data file  
C 
c     Return code:  n = 0 : Success 
c                   n = 1 : Error 
c 
C     Author: Walt Powell  Date: 21 May 1996 
C     Called by: load_pti.f 
C 
C**************************************************************** 
      integer function ldptiare (xbuf, options, numver, error) 
      integer error, options(*), numver 
      character *(*) xbuf 
 
      include 'ipfinc/parametr.inc' 
 
      include 'ipfinc/prt.inc' 
      include 'ipfinc/pti_data.inc' 
      include 'ipfinc/blank.inc' 
      include 'ipfinc/arcntl.inc' 
      include 'ipfinc/alt_case.inc' 
 
      character word(20)*10, busname*8 
      logical first, found 
      integer find_bus, gtptinam, status, ftn_atoi, ptihasha 
 
      ldptiare = 0 
      error = 0 
      last = index (xbuf, '/') - 1 
      if (last .le. 0) last = lastch(xbuf) 
      call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ') 

      do i = nwrd+1, 6 
        word(i) = ' ' 
      enddo 
      npti1 = ftn_atoi(word(1)) 
      if (npti1 .eq. 0) then 
        ldptiare = 1 
      else 
        num = ptihasha (npti1, word(5)) 
        if (num .le. 0) then 
          write (errbuf(1), 10000) npti1, word(5) 
10000     format (' Area on PTI Area Interchange record (',  
     &       i6, 1x, a, ') could not be hashed.') 
          call prterx ('W',1)                 
          error = 1                            
          go to 900 
        endif 
        status = gtptinam (word(2), npti2, kp2, isgn2, busname, basekv) 
        if (npti2 .eq. 0 .and. busname .eq. ' ') then 
          write (errbuf(1), 10002) npti1, word(5) 
10002     format (' Missing slack bus name/number on area record (',  
     &       i6, 1x, a, ')') 
          call prterx ('W',1)                 
          error = 1                            
          go to 900 
        else if (npti2 .gt. 0 .and. status .ne. 0) then 
          write (errbuf(1), 10010) npti1, word(5) 
10010     format (' Bus number on Area Interchange record (',  
     &       i6, 1x, a, ') could not be hashed.') 
          call prterx ('W',1)                 
          error = 1                            
          go to 900 
        else 
          k2 = find_bus (busname, basekv) 
          if (k2 .le. 0) then 
            write (errbuf(1), 10020) npti2, busname, basekv, npti1 
10020       format (' Slack bus bus (', i6, 1x, a8, f7.1,  
     &         ') on PTI Area Interchange record (', i6,  
     &         ') is not in system.') 
            call prterx ('W',1)                 
            error = 1                            
            go to 900 
          endif 
          if (ntotc .ge. MAXCAR) then 
            write (errbuf(1), 10030) MAXCAR 
10030       format ('More than ', i5, ' Area Interchange records. Overfl 
     &ow occurred at record:') 
            write (errbuf(2), 10040) xbuf(1:80)    
10040       format(11x, '(', a, ')')          
            call prterx ('W',2)                 
            ntotc = 1                            
            error = 1                            
            go to 900 
          endif 
 
          ntotc = ntotc + 1 
c 
c         If alternate case is loaded, get the 10-character name from 
c         it; otherwise use PTI's 8-character name 
c 
          found = .false. 
          i = 1 
          do while (i .le. ontotc .and. .not. found) 
            if (oarcnam(i)(1:8) .eq. pti_anam(num)) then 
              pti_anam(num) = oarcnam(i) 
              arcnam(ntotc) = oarcnam(i) 
              found = .true. 
            else 
              i = i + 1 
            endif 
          enddo          
          if (.not. found) arcnam(ntotc) = pti_anam(num) 
          arcnet(ntotc) = ftn_atof (word(3)) / bmva 
          arcbus(ntotc) = busname 
          arcbas(ntotc) = basekv 
          area_number(ntotc) = npti1 
          do j = 1, MAXCAZ 
            arczns(j,ntotc) = ' ' 
          enddo 
        endif 
      endif 
  900 continue 
      return 
      end 
