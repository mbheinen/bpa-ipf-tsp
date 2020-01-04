C    @(#)ldptibus.f	20.14 3/29/99
C****************************************************************  
C  
C     File: ldptibus.f  
C  
C     Purpose: Routine to load PTI bus data from raw data file   
C              Note: This is coded for version 23  
C  
c     Return code:  n = 0 : Success  
c                   n = 1 : Error  
c  
C     Author: Walt Powell  Date: 21 May 1996  
C     Called by: load_pti.f  
C  
C****************************************************************  
      integer function ldptibus (xbuf, options, numver, error)  
      integer error, options(*)  
      character *(*) xbuf  
  
      include 'ipfinc/parametr.inc'  
  
      include 'ipfinc/prt.inc'  
      include 'ipfinc/pti_data.inc'  
      include 'ipfinc/blank.inc'  
      include 'ipfinc/alt_case.inc'  
      include 'ipfinc/alpha.inc'  
      include 'ipfinc/bus.inc'  
      include 'ipfinc/cbus.inc'  
      include 'ipfinc/area.inc'  
      include 'ipfinc/wsccbase.inc'  
  
      character word(16)*10, busname*8, cbtyp*1, cbown*3, cbkyr*2,  
     &          tempc*8  
      logical first  
      integer add_ptin, add_bus, ftn_atoi, status, findoldbus, bustype  
  
      ldptibus = 0  
      error = 0  
      last = lastch(xbuf)  
      call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ')     

      do i = nwrd+1, 12  
        word(i) = ' '  
      enddo  
c  
c     Set default values  
c  
      if (numver .le. 23) then  
        if (word(7) .eq. ' ') word(7) = '1'  
        if (word(8) .eq. ' ') word(8) = '1.000'  
        if (word(12) .eq. ' ') word(12) = '1'  
      else  
        if (word(4) .eq. ' ') word(4) = '1'  
        if (word(7) .eq. ' ') word(7) = '1'  
        if (word(8) .eq. ' ') word(8) = '1'  
      endif  
  
      numbus = ftn_atoi(word(1))  
      if (numver .le. 23) then  
        bustype = ftn_atoi(word(2))  
        status = ftn_atoi(word(2))  
        numarea = ftn_atoi(word(7))  
        numzone = ftn_atoi(word(12))  
        busname = word(10)  
        basekv = ftn_atof (word(11))  
        pl = ftn_atof(word(3))  
        ql = ftn_atof(word(4))  
        gl = ftn_atof(word(5))  
        bl = ftn_atof(word(6))  
        vm = ftn_atof(word(8))  
        va = ftn_atof(word(9))  
      else  
        bustype = ftn_atoi(word(4))  
        status = 1  
        numarea = ftn_atoi(word(7))  
        numzone = ftn_atoi(word(8))  
        busname = word(2)  
        basekv = ftn_atof (word(3))  
        pl = ftn_atof(word(5))  
        ql = ftn_atof(word(6))  
        vm = ftn_atof(word(9))  
        va = ftn_atof(word(10))  
      endif  
      if (numbus .eq. 0) then  
        ldptibus = 1  
      else if (status  .eq. 0) then  
      else  
        if (basekv .ge. 100.0) then  
c  
c         Round basekv to nearest 1.0 kv.  
c  
          x = amod (basekv, 1.0)  
          if (abs (x) .gt. 0.05) then  
            write (tempc, fmt='(f8.0)') basekv  
            basekv = ftn_atof (tempc)  
            if (numver .le. 23) then  
              last = lastch (word(11))  
              write (errbuf(1),10010) word(10), word(11)(1:last),   
     &          basekv  
10010         format (' PTI bus ', a8, 1x, a,   
     &          ' has its base kv rounded to ', f8.2)  
            else  
              last = lastch (word(3))  
              write (errbuf(1),10010) word(2), word(3)(1:last),  
     &          basekv  
            endif  
            call prterx ('W',1)  
          endif  
  
        else if (basekv .ge. 10.0) then  
c  
c         Round basekv to nearest 0.1 kv.  
c  
          x = amod (10.0 * basekv, 1.0)  
          if (abs (x) .gt. 0.05) then  
            write (tempc, fmt='(f8.1)') basekv  
            basekv = ftn_atof (tempc)  
            if (numver .le. 23) then  
              last = lastch (word(11))  
              write (errbuf(1),10010) word(10), word(11)(1:last),   
     &          basekv  
            else  
              last = lastch (word(3))  
              write (errbuf(1),10010) word(2), word(3)(1:last),  
     &          basekv  
            endif  
            call prterx ('W',1)  
          endif  
  
        else if (basekv .eq. 0.0) then  
c  
c         Set default basekv = 100.0  
c  
          basekv = 100.0  
          if (numver .le. 23) then  
            write (errbuf(1),10030) word(10), basekv  
10030       format (' PTI bus ', a8, ' has 0.0 basekv set to ',   
     &        f8.2)  
          else  
            write (errbuf(1),10030) word(2), basekv  
          endif  
          call prterx ('W',1)  
  
        endif  
        num = add_ptin (numbus)  
        if (num .ne. 0) then  
c  
c         Special logic for blank names  
c  
          if (busname .eq. ' ') then  
            write (busname, 10040) numbus  
10040       format ('B', i5.5, 2x)  
            status = newptibs (busname, basekv, ntot+1, indx)  
C  
C           status = 0 : New busname, base successfully added to  
C                        bus(indx), base(indx)  
C           status = 1 : Error, cannot find a unique bus name  
C           status = 2 : error, more than MAXBUS entitites  
  
            if (status .eq. 1) then  
              if (numver .le. 23) then  
                write (errbuf(1),10050) numbus, word(10), basekv  
10050           format (' PTI bus ', i5, ' - ', a8, f6.1,   
     &            ' cannot be converted into a valid name')  
              else  
                write (errbuf(1),10050) numbus, word(2), basekv  
              endif  
              call prterx ('E',1)  
              error = 1  
              go to 900  
            else if (status .eq. 2) then  
              if (numver .le. 23) then  
                write (errbuf(1),10052) MAXBUS, numbus, word(10), basekv  
10052           format (' More than ', i5,   
     &            ' buses, overflow at PTI bus ', i5, ' - ', a8, f6.1)  
              else  
                write (errbuf(1),10052) MAXBUS, numbus, word(2), basekv  
              endif  
              call prterx ('E',1)  
              error = 1  
              go to 900  
            endif  
            if (numver .le. 23) then  
              write (errbuf(1), 10060) numbus, word(10), basekv,  
     &          busname, basekv  
10060         format (' PTI bus ', i5, ' with invalid name [',   
     &          a8, f6.1, '] is renamed [', a8, f6.1, ']')  
            else  
              write (errbuf(1),10060) numbus, word(2), basekv,  
     &          busname, basekv  
            endif  
            call prterx ('W',1)  
          else  
            indx = add_bus (busname, basekv, ntot+1)  
C  
C           indx > 0 : busname, base added to bus(indx), base(indx)  
C                < 0 : busname, base already existing in bus(-indx),  
C                                                          base(-indx)  
            if (indx .lt. 0) then  
              status = newptibs (busname, basekv, ntot+1, indx)  
C  
C             status = 0 : New busname, base successfully added to  
C                          bus(indx), base(indx)  
C             status = 1 : Error, cannot find a unique bus name  
C             status = 2 : error, more than MAXBUS entitites  
  
              if (status .eq. 1) then  
                if (numver .le. 23) then  
                  write (errbuf(1),10050) numbus, word(10), basekv  
                else  
                  write (errbuf(1),10050) numbus, word(2), basekv  
                endif  
                call prterx ('E',1)  
                go to 900  
              else if (status .eq. 2) then  
                if (numver .le. 23) then  
                  write (errbuf(1),10052) MAXBUS, numbus, word(10),   
     &              basekv  
                else  
                  write (errbuf(1),10052) MAXBUS, numbus, word(2),   
     &              basekv  
                endif  
                call prterx ('E',1)  
                error = 1  
                go to 900  
              endif  
              if (numver .le. 23) then  
                write (errbuf(1), 10062) numbus, word(10), basekv,  
     &            busname, basekv  
10062           format (' PTI bus ', i5, ' with duplicate name [',   
     &           a8, f6.1, '] is renamed [', a8, f6.1, ']')  
              else  
                write (errbuf(1),10062) numbus, word(2), basekv,  
     &            busname, basekv  
              endif  
              call prterx ('W',1)  
            else if (indx .eq. 0) then  
              if (numver .le. 23) then  
                write (errbuf(1),10052) MAXBUS, numbus, word(10), basekv  
              else  
                write (errbuf(1),10052) MAXBUS, numbus, word(2), basekv  
              endif  
              call prterx ('E',1)  
              error = 1  
              go to 900  
            endif  
          endif  
          ntot = indx  
          if (numver .le. 23) then  
            wsccbase(ntot) = word(11)  
          else  
            wsccbase(ntot) = word(3)  
          endif  
          wsccflag = .true.  
  
          pti_name(num) = busname  
          pti_base(num) = basekv  
          pti_zone(num) = numzone  
          pti_area(num) = numarea  
  
          if (bustype .eq. 4) bustype = 1  
          kbsdta(1,indx) = bustype  
          kbsdta(2,indx) = indx  
          busdta(3,indx) = pl  
          busdta(4,indx) = ql  
          busdta(5,indx) = gl  
          busdta(6,indx) = bl  
          busdta(7,indx) = 0.0  
          busdta(8,indx) = 0.0  
          busdta(9,indx) = 0.0  
          busdta(10,indx) = 0.0  
          if (kbsdta(1,indx) .eq. 2 .or. kbsdta(1,indx) .eq. 3) then  
            busdta(11,indx) = vm  
          else  
            busdta(11,indx) = 0.0  
          endif  
          busdta(12,indx) = 0.0  
          busdta(13,indx) = 0.0  
          busdta(14,indx) = 0.0  
          kbsdta(15,indx) = 0  
          kbsdta(16,indx) = 0  
          jarzn(indx) = numarea            ! Temporarily hold area num  
          alf2inp(indx) = numzone          ! Temporarily hold zone num  
          call vltlim(indx, vlimn(indx), vlimx(indx), vstart(indx))  
          bus_number(indx) = numbus  
          if (vm .eq. 0.0) then  
            e(indx) = vstart(indx)  
            f(indx) = 0.0d0  
          else  
            e(indx) = vm * cos (0.0174532 * va)  
            f(indx) = vm * sin (0.0174532 * va)  
          endif  
          capcor(1,indx) = 0.0d0  
          capcor(2,indx) = -9.0e10  
          inp2opt(indx) = indx  
          opt2inp(indx) = indx  
          numbpa = findoldbus(busname, basekv)  
          if (numbpa .gt. 0) then  
            owner(indx) = oldowner(numbpa)  
          else  
            owner(indx) = ' '  
          endif  
c  
c         Check for constant current or constant impedance loads.   
c         Note: This logic is intentionally disabled!  
c  
          pin = 0.0  
          qin = 0.0  
          if (amax1 (pin, qin) .gt. 0.0) then  
            if (ntot2 .ge. MAXCBS) then  
              write (errbuf(1), 130) MAXCBS  
  130         format(' More than ', i4, ' + bus records entities.')  
              errbuf(2) = ' (' // xbuf(1:60) // ')'  
              call prterx ('W',2)  
              error = 1  
              ntot2 = 1  
              go to 900  
            endif  
            ntot2 = ntot2 + 1  
            kbctbl(1,ntot2) = indx  
            call linkcbus (ntot2, error)  
            bctbl(2,ntot2) = pin  
            bctbl(3,ntot2) = qin  
            bctbl(4,ntot2) = 0.0  
            bctbl(5,ntot2) = 0.0  
            bctbl(6,ntot2) = 0.0  
            cbown = owner(indx)  
            cbtyp = 'A'  
            cbkyr = '*I'  
            call putchr (1, cbtyp, kbctbl(8,ntot2))  
            call putchr (2, cbkyr, kbctbl(9,ntot2))  
            call putchr (3, cbown, kbctbl(10,ntot2))  
            bctbl(11,ntot2) = 0.0  
            bctbl(12,ntot2) = 0.0  
            kbctbl(7,ntot2) = 0  
          endif  
        endif  
      endif  
  900 continue  
      return  
      end  
