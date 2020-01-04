C    @(#)ldptixdt.f	20.13 3/29/99
C****************************************************************   
C   
C     File: ldptixdt.f   
C   
C     Purpose: Routine to load PTI Switched Reactannce data from raw    
C              data file    
C   
c     Return code:  n = 0 : Success   
c                   n = 1 " Error   
c   
C     Author: Walt Powell  Date: 21 May 1996   
C     Called by: load_pti.f   
C   
C****************************************************************   
      integer function ldptixdt (xbuf, options, numver, error)   
      integer error, options(*), numver   
      character *(*) xbuf   
   
      include 'ipfinc/parametr.inc'   
   
      include 'ipfinc/prt.inc'   
      include 'ipfinc/pti_data.inc'   
      include 'ipfinc/blank.inc'   
      include 'ipfinc/bus.inc'   
      include 'ipfinc/cbus.inc'   
      include 'ipfinc/xdata.inc'   
   
      character word(26)*10, cbtyp*1, cbown*2, cbkyr*2, bus1*8, bus2*8   
      logical first   
      integer find_bus, ftn_atoi, status, gtptinam, status1   
      real steps(2,8)   
   
      ldptixdt = 0   
      error = 0   
      last = index (xbuf, '/') - 1   
      if (last .le. 0) last = lastch(xbuf)   
      call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ')      
   
      do i = nwrd+1, 22   
        word(i) = ' '   
      enddo   
c   
c     Set defaults   
c   
      if (word(2) .eq. ' ') word(2) = '1'   
      if (word(3) .eq. ' ') word(3) = '1.00'   
      if (word(4) .eq. ' ') word(4) = '1.00'   
   
      status = gtptinam (word(1), npti1, kp1, num1, bus1, basekv1)   
      if (npti1 .eq. 0 .and. bus1 .eq. ' ') then   
        ldptixdt = 1   
      else if (npti1 .ne. 0 .and. bus1 .eq. ' ') then   
        write (errbuf(1), 100) npti1   
  100   format (' Bus (', i6,    
     &    ') for Switched reactance record (', i6,    
     &    ') is not in raw data file')   
        errbuf(2) = ' (' // xbuf(1:60) // ')'   
        call prterx ('W',2)   
        error = 1   
        go to 900   
      else   
        k1 = find_bus (bus1, basekv1)   
        if (k1 .le. 0) then   
          write (errbuf(1), 110) npti1, bus1, basekv1   
  110     format (' Switched reactance bus (', i6,    
     &        1x, a8, f7.1, ') is not in system.')   
          errbuf(2) = ' (' // xbuf(1:60) // ')'   
          call prterx ('W',2)   
          error = 1                              
          go to 900   
        endif   
        status1 = gtptinam (word(5), npti2, kp2, num2, bus2, basekv2)   
        if (npti2 .eq. 0 .and. bus2 .eq. ' ') then   
          kp2 = kp1   
          k2 = k1   
        else    
          if (bus2 .eq. ' ') then   
            write (errbuf(1), 120) npti2, npti1   
  120       format (' Remote bus (', i6,    
     &        ') for Switched reactance record (', i6,    
     &        ') is not in raw data file')   
            errbuf(2) = ' (' // xbuf(1:60) // ')'   
            call prterx ('W',2)   
            error = 1   
            go to 900   
          endif   
          k2 = find_bus (bus2, basekv2)   
          if (k2 .le. 0) then   
            write (errbuf(1), 130) npti2, bus2, basekv2   
  130       format (' Remote Switched reactance bus (', i6,    
     &        1x, a8, f7.1, ') is not in system.')   
            errbuf(2) = ' (' // xbuf(1:60) // ')'   
            call prterx ('W',2)   
            error = 1                              
            go to 900   
          endif   
        endif   
        modsw = ftn_atoi(word(2))   
        vmax = ftn_atof(word(3))   
        vmin = ftn_atof(word(4))   
        binit = ftn_atof(word(6))   
        totrek = 0.0   
        totcap = 0.0   
        do i = 1, 8   
          j = 2*i + 5   
          steps(1,i) = ftn_atof(word(j))   
          steps(2,i) = ftn_atof(word(j+1))   
          xtot = steps(1,i) * steps(2,i)   
          totrek = totrek + amin1 (0.0,xtot)   
          totcap = totcap + amax1 (0.0,xtot)   
        enddo   
        shunt = busdta(6,k1)   
  
	dq = dim (0.99*shunt, totcap) - dim (totrek, 0.99*shunt)  
        if (abs(dq) .gt. 0.1) then   
            write (errbuf(1), 132) npti1, bus1, basekv1, shunt,  
     &         totrek, totcap   
  132       format (' Switched reactance bus (', i6, 1x, a8, f7.1,    
     &         ') initial shunt:', f8.1,    
     &         ', violates X-data range: ', f8.1, ',', f8.1)   
            errbuf(2) = ' (' // xbuf(1:60) // ')'   
            call prterx ('W',2)   
            error = 1                              
        endif   
        if (modsw .eq. 0 .and. kbsdta(1,k1) .eq. 1) then   
          busdta(6,k1) = busdta(6,k1) + binit   
        else if (modsw .eq. 0) then   
          if (ntot2 .ge. MAXCBS) then   
            write (errbuf(1), 140) MAXCBS   
  140       format(' More than ', i5, ' + bus records entities.')   
            errbuf(2) = ' (' // xbuf(1:60) // ')'   
            call prterx ('W',2)   
            error = 1   
            ntot2 = 1   
            go to 900   
          endif   
          ntot2 = ntot2 + 1   
          kbctbl(1,ntot2) = k1   
          call linkcbus (ntot2, error)   
          bctbl(2,ntot2) = 0.0   
          bctbl(3,ntot2) = 0.0   
          bctbl(4,ntot2) = 0.0   
          bctbl(5,ntot2) = totcap + totrek   
          bctbl(6,ntot2) = 0.0   
          cbown = owner(k1)   
          cbtyp = 'A'   
          cbkyr = ' '   
          call putchr (1, cbtyp, kbctbl(8,ntot2))   
          call putchr (2, cbkyr, kbctbl(9,ntot2))   
          call putchr (3, cbown, kbctbl(10,ntot2))   
          bctbl(11,ntot2) = 0.0   
          bctbl(12,ntot2) = 0.0   
          kbctbl(7,ntot2) = 0   
   
          if (kbsdta(1,k1) .eq. 1 .and.    
     &        vmax .gt. 0.0 .and. vmin .gt. 0.0) then   
            busdta(11,k1) = vmax   
            busdta(12,k1) = vmin   
          else if (vmax .gt. 0.0 .and. vmin .gt. 0.0) then   
            busdta(11,k1) = 0.5 * (vmax + vmin)   
            busdta(12,k1) = busdta(11,k1)   
          endif   
          if (kbsdta(1,k1) .eq. 2 .or. kbsdta(1,k1) .eq. 7 .or.   
     &        kbsdta(1,k1) .eq. 10) busdta(12,k1) = 0.0   
        else if (modsw .eq. 1) then   
          kbsdta(1,k1) = 11   
          if (vmax .gt. 0.0 .and. vmin .gt. 0.0) then   
            busdta(11,k1) = vmax   
            busdta(12,k1) = vmin   
            if (k2 .ne. k1 .and. kbsdta(1,k2) .eq. 1) then   
              busdta(11,k2) = vmax   
              busdta(12,k2) = vmin   
            endif   
          endif   
        else   
          kbsdta(1,k1) = 7   
          busdta(6,k1) = totcap + totrek   
   
          if (abs(shunt) .gt. 1.0) then   
            if (ntot2 .ge. MAXCBS) then   
              write (errbuf(1), 140) MAXCBS   
              errbuf(2) = ' (' // xbuf(1:60) // ')'   
              call prterx ('W',2)   
              error = 1   
              ntot2 = 1   
              go to 900   
            endif   
            ntot2 = ntot2 + 1   
            kbctbl(1,ntot2) = k1   
            call linkcbus (ntot2, error)   
            bctbl(2,ntot2) = 0.0   
            bctbl(3,ntot2) = 0.0   
            bctbl(4,ntot2) = 0.0   
            bctbl(5,ntot2) = shunt   
            bctbl(6,ntot2) = 0.0   
            cbown = owner(k1)   
            cbtyp = 'A'   
            cbkyr = ' '   
            call putchr (1, cbtyp, kbctbl(8,ntot2))   
            call putchr (2, cbkyr, kbctbl(9,ntot2))   
            call putchr (3, cbown, kbctbl(10,ntot2))   
            bctbl(11,ntot2) = 0.0   
            bctbl(12,ntot2) = 0.0   
            kbctbl(7,ntot2) = 0   
          endif   
   
          if (vmax .gt. 0.0 .and. vmin .gt. 0.0) then   
            if (options(8) .eq. 1) then   
              busdta(11,k1) = 0.5 * (vmax + vmin)   
            else if (options(8) .eq. 2) then   
              busdta(11,k1) = vmax   
            else if (options(8) .eq. 3) then   
              busdta(11,k1) = vmin   
            else if (kbsdta(1,k1) .eq. 1) then   
              busdta(11,k1) = 0.5 * (vmax + vmin)   
            else    
              busdta(11,k1) = 0.5 * (vmax + vmin)   
            endif   
            busdta(12,k1) = 0.0   
          endif   
        endif   
        if (kxtot .ge. MAXXDT) then   
          write (errbuf(1), 150) MAXXDT   
  150     format(' More than ', i3, ' switched reactance entities.')   
          errbuf(2) = ' (' // xbuf(1:60) // ')'   
          call prterx ('W',2)   
          error = 1   
          kxtot=1   
          go to 900   
        endif   
        kxtot=kxtot+1   
        xdata(1,kxtot) = k1   
        xdata(2,kxtot) = k2   
        do i = 1, 8   
          j = 2*i + 5   
          xdata(j,kxtot) = steps(1,i)   
          xdata(j+1,kxtot) = steps(2,i)   
        enddo   
        xdata(3,kxtot)=totrek   
        xdata(4,kxtot)=totcap   
        xdata(5,kxtot)=totrek   
        xdata(6,kxtot)=totcap   
      endif   
  900 continue   
      return   
      end   
