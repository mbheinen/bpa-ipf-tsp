C    @(#)ldptigen.f	20.17 5/3/00
C**************************************************************** 
C 
C     File: ldptigen.f 
C 
C     Purpose: Routine to load PTI generator data from raw data file  
C 
c     Return code:  n = 0 : Success 
c                   n = 1 " Error 
c 
C     Author: Walt Powell  Date: 21 May 1996 
C     Called by: load_pti.f 
C 
C**************************************************************** 
      integer function ldptigen (xbuf, options, numver, error) 
      integer error, options(*), numver 
      character *(*) xbuf 
 
      include 'ipfinc/parametr.inc' 
 
      include 'ipfinc/prt.inc' 
      include 'ipfinc/pti_data.inc' 
      include 'ipfinc/blank.inc' 
      include 'ipfinc/alt_case.inc' 
      include 'ipfinc/alpha.inc' 
      include 'ipfinc/bus.inc' 
      include 'ipfinc/cbus.inc' 
 
      character word(30)*10, busname*8, type*1, cbtyp*1, cbown*2,  
     &          cbkyr*2 
      logical first
      integer find_bus, ftn_atoi, status, status1, gtptinam 
 
      ldptigen = 0 
      error = 0 
      last = index (xbuf, '/') - 1 
      if (last .le. 0) last = lastch(xbuf) 
      call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ')    
c 
c     Add defaults 
c 
      if (word(2) .eq. ' ') word(2) = '1' 
      if (word(5) .eq. ' ') word(5) = '9999' 
      if (word(6) .eq. ' ') word(6) = '-9999' 
      if (word(7) .eq. ' ') word(7) = '1.000' 
      if (word(15) .eq. ' ') word(15) = '1' 
  
      status1 = gtptinam (word(1), numpti, num1, num2, busname, basekv)
      status = ftn_atoi(word(13)) 
      if (numpti .eq. 0 .and. busname .eq. ' ') then 
        ldptigen = 1 
      else if (status .eq. 0) then 
      else 
        numarea = 0 
        numzone = 0 
        if (numpti .ne. 0 .and. istatus .eq. 1) then 
          write (errbuf(1), 100) 
  100     format (' Generator record is not preceded with a Bus record
     &in raw data file.') 
          errbuf(2)= ' (' // xbuf(1:60) // ')' 
          call prterx ('W',2) 
          error = 1 
          go to 900 
        endif 
        num = find_bus (busname, basekv) 
        if (num .lt. 0) then 
          write (errbuf(1), 110) busname, basekv 
  110     format (' Bus (', a8, f7.1, ') in Generator record is not in 

     & system.') 
          call prterx ('W', 1) 
          error = 1 
          go to 900 
        endif 
        if (kbsdta(1,num) .ne. 2 .and. kbsdta(1,num) .ne. 3 .and. 
     &             kbsdta(1,num) .ne. 8) then 
          call typno (type, kbsdta(1,num))  
          write (errbuf(1), 120) busname, basekv, type 
  120     format (' Generator bus (', a8, f7.1, ') is type B', a) 
          call prterx ('W', 1) 
          error = 1 
          go to 900 
        endif 
        if (kbsdta(1,num) .ne. 3) then 
          kbsdta(1,num) = 8                  ! Change to type BG 
        endif 
        busdta(7,num) = busdta(7,num) + ftn_atof(word(17))    ! Pmax 
        if (word(2) .eq. '1') then 
          busdta(8,num) = busdta(8,num) + ftn_atof(word(3))   ! PG 
          busdta(9,num) = busdta(9,num) + ftn_atof(word(5))   ! Qmax 
          busdta(10,num) = busdta(10,num) + ftn_atof(word(6)) ! Qmin 
        else  
          if (ntot2 .ge. MAXCBS) then 
            write (errbuf(1), 130) MAXCBS 
  130       format(' More than ', i5, ' + bus records entities.') 
            errbuf(2) = ' (' // xbuf(1:60) // ')' 
            call prterx ('W',2) 
            error = 1 
            ntot2 = 1 
            go to 900 
          endif 
          ntot2 = ntot2 + 1 
          kbctbl(1,ntot2) = num 
          call linkcbus (ntot2, error) 
          bctbl(2,ntot2) = 0.0 
          bctbl(3,ntot2) = 0.0 
          bctbl(4,ntot2) = 0.0 
          bctbl(5,ntot2) = 0.0 
          bctbl(6,ntot2) = ftn_atof(word(3))             ! PG 
          kbctbl(7,ntot2) = 0 
          cbown = owner(num) 
          cbtyp = 'A' 
          cbkyr = word(2) 
          call putchr (1, cbtyp, kbctbl(8,ntot2)) 
          call putchr (2, cbkyr, kbctbl(9,ntot2)) 
          call putchr (3, cbown, kbctbl(10,ntot2)) 
          bctbl(11,ntot2) = ftn_atof(word(5))            ! Qmax 
          bctbl(12,ntot2) = ftn_atof(word(6))            ! Qmin 
        endif 
        if (word(5) .eq. '9999') then 
          if (kbsdta(1,num) .ne. 3) then 
            kbsdta(1,num) = 2 
          endif 
        endif 
        if (word(7) .ne. ' ') then 
          busdta(11,num) = ftn_atof(word(7))             ! Vsched 
          busdta(12,num) = busdta(11,num)                ! Vsched 
        endif 
        if (word(8) .ne. ' ') then 
          istatus = gtptinam (word(8), numrpti, num1, num2, busname,  
     &                        basekv) 
          if (istatus .eq. 1 .or. numrpti .eq. 0) then 
            nb = 0 
          else 
            nb = find_bus (busname, basekv) 
            if (nb .lt. 0) nb = 0 
          endif 
          kbsdta(13,num) = nb                  ! Controlled bus 
        else if (kbsdta(1,num) .eq. 8) then 
          kbsdta(1,num) = 7                    ! Change to type BQ 
          busdta(12,num) = 0.0 
        endif 
        pct = ftn_atof(word(16))                         ! %-var 
        if (pct .gt. 0.0 .and. kbsdta(1,num) .ne. 8) then 
          call typno (type, kbsdta(1,num))  
          write (errbuf(1), 140) busname, basekv, type, pct 
  140     format (' Generator (', a8, f7.1, ') is type B', a, 
     &      ' and not BG. Percentage vars (', f7.1, ') ignored ') 
          call prterx ('W', 1) 
          error = 1 
        else 
          busdta(14,num) = pct 
        endif 
        call vltlim(num, vlimn(num), vlimx(num), vstart(num)) 
      endif 
  900 continue 
      return 
      end 
