C    @(#)ldptild.f	20.7 3/29/99
C**************************************************************** 
C 
C     File: ldptild.f 
C 
C     Purpose: Routine to load PTI Load data from raw data file  
C 
c     Return code:  n = 0 : Success 
c                   n = 1 " Error 
c 
C     Author: Walt Powell  Date: 21 May 1996 
C     Called by: load_pti.f 
C 
C**************************************************************** 
      integer function ldptild (xbuf, options, numver, error) 
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
 
      character word(20)*10, busname*8, type*1, cbtyp*1, cbown*2,  
     &          cbkyr*2 
      logical first, extended_name, finished 
      integer find_bus, ftn_atoi, status, gtptinam 
 
      ldptild = 0 
      error = 0 
      last = index (xbuf, '/') - 1 
      if (last .le. 0) last = lastch(xbuf) 
      call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ')    
c 
c     Add defaults 
c 
      if (word(2) .eq. ' ') word(2) = '1' 
      if (word(3) .eq. ' ') word(3) = '1' 
      if (word(4) .eq. ' ') word(4) = '1' 
      if (word(5) .eq. ' ') word(5) = '1' 
  
      istatus = gtptinam (word(1), numpti, num1, num2, busname, basekv) 
      status = ftn_atoi(word(3)) 
      if (numpti .eq. 0 .and. busname .eq. ' ') then 
        ldptild = 1 
      else if (status .eq. 0) then 
      else 
        numarea = ftn_atoi(word(4)) 
        numzone = ftn_atoi(word(5)) 
        if (numpti .ne. 0 .and. istatus .eq. 1) then 
          write (errbuf(1), 10000) numpti 
10000     format (' PTI bus number ', i6,  
     & ' is not defined in raw data file') 
          errbuf(2)= ' (' // xbuf(1:60) // ')' 
          call prterx ('W',2) 
          error = 1 
          go to 900 
        endif 
        num = find_bus (busname, basekv) 
        if (num .lt. 0) then 
          write (errbuf(1), 10010) busname, basekv 
10010     format (' Bus (', a8, f7.1, ') in Load record is not in syste 
     &m.') 
          call prterx ('W', 1) 
          error = 1 
          go to 900 
        endif 
        pl = ftn_atof (word(6)) 
        ql = ftn_atof (word(7)) 
        apl = ftn_atof (word(8)) 
        aql = ftn_atof (word(9)) 
        ypl = ftn_atof (word(10)) 
        yql = ftn_atof (word(11)) 
        finished = (pl .eq. 0.0 .and. ql .eq. 0.0 .and. 
     &              apl .eq. 0.0 .and. aql .eq. 0.0 .and. 
     &              ypl .eq. 0.0 .and. yql .eq. 0.0) 
        do while (.not. finished) 
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
          bctbl(6,ntot2) = 0.0 
          kbctbl(7,ntot2) = 0 
          cbown = owner(num) 
          cbtyp = word(2) 
          if (pl .ne. 0.0 .or. ql .ne. 0.0) then 
            cbkyr = '*P' 
            bctbl(2,ntot2) = pl 
            bctbl(3,ntot2) = ql 
            pl = 0.0 
            ql = 0.0 
          else if (apl .ne. 0.0 .or. aql .ne. 0.0) then 
            cbkyr = '*I' 
            bctbl(2,ntot2) = apl 
            bctbl(3,ntot2) = aql 
            apl = 0.0 
            aql = 0.0 
          else  
            cbkyr = '*Z' 
            bctbl(4,ntot2) = ypl 
            bctbl(5,ntot2) = yql 
            ypl = 0.0 
            yql = 0.0 
          endif 
          call putchr (1, cbtyp, kbctbl(8,ntot2)) 
          call putchr (2, cbkyr, kbctbl(9,ntot2)) 
          call putchr (3, cbown, kbctbl(10,ntot2)) 
          bctbl(11,ntot2) = 0.0 
          bctbl(12,ntot2) = 0.0 
          finished = (pl .eq. 0.0 .and. ql .eq. 0.0 .and. 
     &                apl .eq. 0.0 .and. aql .eq. 0.0 .and. 
     &                ypl .eq. 0.0 .and. yql .eq. 0.0) 
        enddo 
           
      endif 
  900 continue 
      return 
      end 
