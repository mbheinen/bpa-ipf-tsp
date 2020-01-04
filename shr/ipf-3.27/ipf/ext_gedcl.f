C    @(#)ext_gedcl.f	20.17 8/30/00
C****************************************************************
C
C     File: ext_gedcl.f
C
C     Purpose: Routine to extract d-c line data in GE format
C
C     Input parameters:
C
C             savfil   - the logical unit opened
C             version  - "23" or "24"
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: save_ged.f
C
C****************************************************************
      integer function ext_gedcl (scrfil, version, option, total)
      integer scrfil, version, total(4)
      character *(*) option(10)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/ordsta.inc'
 
      common /branch_ratings/ ratings(8,MAXBRN), 
     &                        ext_ratings(15,MAXBRN),
     &                        ext_date(2,MAXBRN),
     &                        numrat
      real ratings, ext_ratings
      character ext_date*6
      integer numrat

      logical ior
      integer add_ptiq, error, bus_type(16), ptr, write_ge_file,
     &        fnd_ptia, fnd_ptiy, ftn_atoi, winter_type
      character xbuf*256, date_in*6, date_out*6, cbown*3
      data bus_type / 1, 2, 3, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1 /
      data date_in, date_out / '400101', '391231' /

      ext_gedcl = 0       ! set default return status = successful
      error = 0           ! initialize error count

c     Use season [option(6)] and winter_type [option(7)] as filters 
c     for extended ratings
c
c     winter_type -  N = Normal winter
c                    M = Moderate winter ratings
c                    E = Extra Heavy Ratings

      winter_type = index ('NME', option(7))
      if (option(6) .eq. '1') then 
        iseason = 2
      else if (option(6) .eq. '2') then
        if (winter_type .eq. 1) then
          iseason = 1
        else if (winter_type .eq. 2) then
          iseason = 4
        else
          iseason = 3
        endif
      else if (option(6) .eq. '3') then
        iseason = 5
      else
        iseason = 0
      endif
      if (option(3) .eq. 'Y') then
        icount = kdtot
      else
        icount = mtdcln+kdtot
      endif
      write (*, 10000) icount
10000 format (' * Writing ', i5, ' dc line records to NETDAT file')

      write (xbuf, 10010) icount
10010 format ('dc line data  [', i2, ']                                 
     &   ck   long_id_     st ar zone   resist   react    capac   rate1 
     & rate2  rate3  rate4  len  aloss    date_in date_out PID N  rate5 
     & rate6  rate7  rate8')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      do jt = 1, kdtot
        k1 = dc2t(1,jt)
        k2 = dc2t(3,jt)
        if (orddc .eq. 2) then
          k1 = opt2inp(k1)
          k2 = opt2inp(k2)
        endif
        ptr = numbrn (k1, k2, '*', 0)
        nbr = iabs (brnch_ptr(ptr))
        call getchr (3, cbown, kbrnch(3,nbr))
        factor = 0.001 * dc2t(5,jt)
        if (rateln(1,nbr) .gt. 0.0) then
          rate1 = factor * rateln(1,nbr)
          rate2 = factor * rateln(1,nbr)
          rate3 = factor * rateln(2,nbr)
        else if (rateln(1,nbr) .lt. 0.0 .and. iseason .gt. 0) then
          ix = -rateln(1,nbr)
          rate1 = factor * ext_ratings(3*iseason-2,ix)
          rate2 = factor * ext_ratings(3*iseason-2,ix)
          rate3 = factor * ext_ratings(3*iseason-2+1,ix)
        else
          rate1 = factor * brnch(4,nbr)
          rate2 = rate1
          rate3 = 0.0
        endif
        ior = ((brnch_ptr(ptr) .gt. 0 .and. 
     &         kbrnch(15, nbr) .eq. 0) .or.
     &         (brnch_ptr(ptr) .lt. 0 .and. 
     &         kbrnch(15, nbr) .gt. 0)) 
        if (ior) then
          aloss = 1.0
          iptia = fnd_ptia (arcnam(jarzn(k2)))
          iptiz = fnd_ptiy (zone(k2))
        else
          aloss = 0.0
          iptia = fnd_ptia (arcnam(jarzn(k1)))
          iptiz = fnd_ptiy (zone(k1))
        endif

        write (xbuf, 10020) 2*jt-1, bus(k1), base(k1), 2*jt,
     &    bus(k2), base(k2), '1 ', ' ', 1, 
     &    pti_anum(iptia), pti_znum(iptiz), dc2t(8,jt), 
     &    0.001*dc2t(9,jt), dc2t(10,jt), rate1, rate2, rate3, 0.0, 
     &    brnch(16,nbr), aloss, date_in, date_out, 0, 0, 0.0, 0.0, 
     &    0.0, 0.0
10020   format (2x, i5, 1x, '"', a, '"', 1x, f6.2, 3x, i5, 1x, '"', 
     &    a, '"', 1x, f6.2, 1x, '"', a, '"', 1x, '"', a, '"', 1x, 
     &    ':', 1x, i1, 1x, i2, 1x, i4, 3(1x, f9.4), 4(1x, f6.1),
     &    1x, f6.1, 1x, f6.3, 2(3x, a), 1x, i1, 1x, i1, 4(1x, f6.1),
     &    1x, '/')
        last = lastch (xbuf)
        status = write_ge_file (0, xbuf(1:last))

        iptio = add_ptiq(cbown)
        write (xbuf, 10030) pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 
     &     0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0
10030   format (3x, 8(1x, i3, 1x, f5.3))
        last = lastch (xbuf)
        status = write_ge_file (0, xbuf(1:last))
      enddo

      if (option(3) .eq. 'Y') go to 100
      do jt = 1, mtdcln
        k1 = dcmtln(1,jt)
        k2 = dcmtln(2,jt)
        if (orddc .eq. 2) then
          k1 = opt2inp(k1)
          k2 = opt2inp(k2)
        endif
        num_dc1 = 2*kdtot + dcmtln(8,jt) 
        num_dc2 = 2*kdtot + dcmtln(9,jt) 
        ptr = numbrn (k1, k2, '*', 0)
        nbr = iabs (brnch_ptr(ptr))
        call getchr (3, cbown, kbrnch(3,nbr))
        ickt = dcmtln(7,jt)
        do i = 1, mtdcbs
          if (dcmtbs(21,i) .eq. ickt .and. dcmtbs(6,i) .ne. 0) then
            vsched = dcmtbs(6,i)
          endif
        enddo
        factor = 0.001 * vsched
        rate1 = factor * rateln(1,nbr)
        rate2 = factor * rateln(1,nbr)
        rate3 = factor * rateln(2,nbr)
        ior = ((brnch_ptr(ptr) .gt. 0 .and. 
     &         kbrnch(15, nbr) .eq. 0) .or.
     &         (brnch_ptr(ptr) .lt. 0 .and. 
     &         kbrnch(15, nbr) .gt. 0)) 
        if (ior) then
          aloss = 1.0
          iptia = fnd_ptia (arcnam(jarzn(k2)))
          iptiz = fnd_ptiy (zone(k2))
        else
          aloss = 0.0
          iptia = fnd_ptia (arcnam(jarzn(k1)))
          iptiz = fnd_ptiy (zone(k1))
        endif

        write (xbuf, 10020) num_dc1, bus(k1), base(k1), num_dc2,
     &    bus(k2), base(k2), '1 ', ' ', 1, 
     &    pti_anum(iptia), pti_znum(iptiz), dcmtln(4,jt), 
     &    0.001 * dcmtln(5,jt), dcmtln(6,jt), rate1, rate2, rate3, 
     &    0.0, brnch(16,nbr), aloss, date_in, date_out, 0, 0, 0.0, 
     &    0.0, 0.0, 0.0
         last = lastch (xbuf)
         status = write_ge_file (0, xbuf(1:last))

        iptio = add_ptiq(cbown)
        write (xbuf, 10030) pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 
     &    0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0
        last = lastch (xbuf)
        status = write_ge_file (0, xbuf(1:last))

      enddo
  100 continue

      write ( errbuf(1), 10120) icount
10120 format (1x, i5, ' d-c line records written to NETDAT file')
      call prterx ('I', 1)
 
      ext_gedcl = icount

      return
      end
