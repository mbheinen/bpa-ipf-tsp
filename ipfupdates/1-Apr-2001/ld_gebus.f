C    %W% %G%
C****************************************************************
C
C     File: ld_gebus.f
C
C     Purpose: Routine to load GE bus data from raw data file 
C
c     Return code:  n = 0 : Success
c                   n = 1 : Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_ge.f
C
C****************************************************************
      integer function ld_gebus (xbuf, file, options, error)
      integer file, error, options(*)
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

      character word(60)*10, busname*8, code*4, basekv_c*4, newname*8,
     &          capital*8
      integer add_ptin, add_bus, ftn_atoi, findoldbus, bustype

      ld_gebus = 0
      error = 0
      last = lastch0 (xbuf)
      call uscan (xbuf(1:last), word, nwrd, '~',  ' ')   

      do i = nwrd+1, 17
        word(i) = ' '
      enddo

      numbus = ftn_atoi(word(1))
      if (numbus .eq. 0) then
        if (ichar (xbuf(1:1)) .ge. ichar ('a') .and.
     &      ichar (xbuf(1:1)) .le. ichar ('z')) ld_gebus = 1
      else
        busname = capital (word(2))
        basekv = ftn_atof (word(3))
        if (basekv .ge. 10.0) then
c
c         Round basekv to nearest 0.1 kv.
c
          x = amod (10.0 * basekv, 1.0)
          if (abs (x) .gt. 0.05) then
            basekv_c = code (basekv, 4, 0)
            basekv = ftn_atof (basekv_c)
            last = lastch (word(3))
            write (errbuf(1),10010) word(2), word(3)(1:last), 
     &          basekv_c
10010       format (' GE bus ', a8, 1x, a, 
     &          ' has its base kv rounded to ', a)
            call prterx ('W',1)
          endif
        endif
        bustype = ftn_atoi (word(5))
        vsched = ftn_atof (word(6))
        vm = ftn_atof (word(7))
        va = ftn_atof (word(8))
        numarea = ftn_atoi (word(9))
        numzone = ftn_atoi (word(10)) 
        vmax = ftn_atof (word(11))
        vmin = ftn_atof (word(12))
        numowner = ftn_atoi (word(17))

        num = add_ptin (numbus)
        if (num .gt. 0) then
          pti_name(num) = busname
          pti_base(num) = basekv
          pti_zone(num) = numzone
          pti_area(num) = numarea
          pti_owner(num) = numowner
        endif      
        if (num .ne. 0) then
          indx = add_bus (busname, basekv, ntot+1)
C
C         indx > 0 : busname, base added to bus(indx), base(indx)
C              < 0 : busname, base already existing in bus(-indx),
C                                                        base(-indx)
          ix = numbus
          newname = busname
          do while (indx .lt. 0) 
c
c           Duplicate bus - rename
c            
            write (newname(4:8), 10012) ix
10012       format (i5.5)
            indx = add_bus (newname, basekv, ntot+1)
            if (indx .gt. 0) then
              write (errbuf(1), 10014) numbus, busname, basekv, 
     &          newname, basekv
10014         format (' Duplicate bus No. ',i5,' name ', a8, f6.1, 
     &          '  is renamed to ', a8, f6.1)
              call prterx ('W',1)
              pti_name(num) = newname
            else
              ix = ix + 1
            endif
          enddo
          if (indx .eq. 0) then
            write (errbuf(1), 10020) MAXBUS
10020       format (' More than ', i5, ' buses in base system ')
            call prterx ('E',1)
            error = 1
            go to 900
          else
            ntot = indx
            wsccbase(ntot) = word(3)
            wsccflag = .true.
          endif
          if (bustype .eq. 0) then
            bustype = 3
            vmin = ftn_atof(word(8))
            vmax = ftn_atof(word(7))
          else if (bustype .eq. 2) then
            bustype = 8
            if (vmin .eq. 0.0) vmin = vsched
            if (vmax .eq. 0.0) vmax = vsched
          else if (bustype .eq. -2) then
            bustype = 2
            if (vmin .eq. 0.0) vmin = vsched
            if (vmax .eq. 0.0) vmax = vsched
          else if (bustype .eq. 1 .and. vmin .eq. 0.0 .and. 
     &             vmax .eq. 0.0 .and. vsched .ne. 1.0) then
            vmin = vsched
            vmax = vsched
          endif
          kbsdta(1,indx) = bustype
          kbsdta(2,indx) = indx
          busdta(3,indx) = 0.0
          busdta(4,indx) = 0.0
          busdta(5,indx) = 0.0
          busdta(6,indx) = 0.0
          busdta(7,indx) = 0.0
          busdta(8,indx) = 0.0
          busdta(9,indx) = 0.0
          busdta(10,indx) = 0.0
          if (kbsdta(1,indx) .eq. 3) then
            busdta(11,indx) = vsched
          else
            busdta(11,indx) = vmax
          endif
          busdta(12,indx) = vmin
          busdta(13,indx) = 0.0
          busdta(14,indx) = 0.0
          kbsdta(15,indx) = 0
          kbsdta(16,indx) = 0
          vstart(indx) = vsched
          jarzn(indx) = numarea            ! Temporarily hold area num
          alf2inp(indx) = numzone          ! Temporarily hold zone num
          owner(indx) = word(16)

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
            zone(indx) = oldzone(numbpa)
            owner(indx) = oldowner(numbpa)
          else
            zone(indx) = word(9)
            owner(indx) = word(16)
          endif
        endif
      endif
  900 continue
      return
      end
