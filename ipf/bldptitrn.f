C    @(#)bldptitrn.f	20.10 2/28/00
C****************************************************************
C
C     File: bldptitrn.f
C
C     Purpose: Routine to build PTI hash tables from saved PTI data
C              file
C
c     Return code:  n = 0 : Success
c                   n = 1 " Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: net_data_sub.f
C
C****************************************************************
      integer function bldptitrn (scrfil, filename, numver, error)
      integer scrfil, numver, error
      character *(*) filename

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/basval.inc'
      include 'ipfinc/owner_cm.inc'
 
      integer MAXPTIRECORDS
      parameter (MAXPTIRECORDS = 16000)
      common /scratch/ count, array(4,MAXBUS), htable_2(MAXBUS),
     &                 nextptr_2(MAXBUS), count_newbus, 
     &                 newbusno(MAXBUS), count_newzone, 
     &                 newzoneno(MAXCZN), count_newown, 
     &                 newownno(MAXOWN), tempc(MAXPTIRECORDS),
     &                 sort_tempc(MAXPTIRECORDS)
      integer array, count, htable_2, count_newbus, newbusno, 
     &        count_newzone, newzoneno, count_newown, newownno,
     &        sort_tempc
      character tempc*80

      character tempc2*10
      logical finished
      integer status, add_ptin, add_ptia, add_ptio, add_ptiz, 
     &        add_ptib, fnd_ptic, bldzone, buildzbo
      external kmp_ptib, swp_ptib

      bldptitrn = 0
      count = 0
      count_newbus = 0
      count_newzone = 0
      count_newown = 0
      ntotc_old = ntotc
c
c     Initialize PTI hash arrays
c
      do i = 1, PTI_MAXBUS
        nextptr_n(i) = 0
        nextptr_b(i) = 0
      enddo
      do i = 1, MAXCZN
        htable_y(i) = 0
        htable_z(i) = 0
        nextptr_y(i) = 0
        nextptr_z(i) = 0
      enddo
      do i = 1, MAXCAR
        htable_a(i) = 0
        htable_c(i) = 0
        nextptr_a(i) = 0
        nextptr_c(i) = 0
      enddo
      do i = 1, MAXOWN
        htable_o(i) = 0
        htable_q(i) = 0
        nextptr_o(i) = 0
        nextptr_q(i) = 0
      enddo

      write (*, 10000) 
10000 format(1x, '* Rebuilding zone and ownership hash tables...')
      status = buildzbo(status)
c
c     Hash area data
C
      num_anam = 0
      if (ntotc .eq. 0) then
        ntotc = 1
        arcnam(1) = 'SYSTEM'
        area_number(1) = 1
        nztot = 0
        do i = 1, ntot_alf
          nb = alf2inp(i)
          jarzn(nb) = 1
        enddo
      endif
      do i = 1, ntotc
        if (area_number(i) .eq. 0) area_number(i) = i
        nptia = area_number(i)
        num = add_ptia (nptia, arcnam(i))
      enddo
c
c     Hash zone data
C
      num_znam = 0
      do i = 1, nztot
        if (zone_number(i) .eq. 0) zone_number(i) = i
        nptiz = add_ptiz (zone_number(i), acznam(i))
        count_newzone = count_newzone + 1
        newzoneno(count_newzone) = nptiz
      enddo
c
c     Hash owner data
C
      num_onam = 0
      do i = 1, num_owners
        if (owner_number(i) .eq. 0) owner_number(i) = i
        nptio = add_ptio (owner_number(i), owner_code(i))
        count_newown = count_newown + 1
        newownno(count_newown) = nptio
      enddo
c
c     Hash bus data
c
      num_hashn = 0
      do i = 1, ntot_alf
        nb = alf2inp(i)
        if (bus_number(nb) .eq. 0) bus_number(nb) = i
        nptib = add_ptin (bus_number(nb))
        if (nptib .gt. 0) then
          pti_name(nptib) = bus(nb)
          pti_base(nptib) = base(nb)
          pti_zone(nptib) = 0
          pti_area(nptib) = 0
        endif
        count_newbus = count_newbus + 1
        newbusno(count_newbus) = nptib
        if (jarzn(nb) .eq. 0) then
          tempc2 = ' '
        else
          tempc2 = arcnam(jarzn(nb))
        endif
        ind = add_ptib (bus(nb), base(nb), tempc2, nptib)
        if (ind .lt. 0) then
          write (errbuf(1), 10010) bus(nb), base(nb), tempc2
10010     format ('Duplicate PTI bus name (', a8, f7.1, i6, ')')
          call prterx ('W', 1)
        endif
      enddo

      ntotc = ntotc_old
      return
      end
