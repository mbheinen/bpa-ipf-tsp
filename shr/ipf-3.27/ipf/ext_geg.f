C    %W% %G%
C****************************************************************
C
C     File: ext_geg.f
C
C     Purpose: Routine to extract generator data in GE format
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
      integer function ext_geg (scrfil, version, option, total)
      integer scrfil, version, total(4)
      character *(*) option(10)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/ordsta.inc'
 
      logical finished
      integer add_ptiq, status, error, gtge_num, ptr, close_ge_file, 
     &        write_ge_file, read_ge_file, open_ge_file
      character xbuf*256, date_in*6, date_out*6, bus2c*8, base2c*6,
     &          bus3c*8, base3c*6, bus4c*8, base4c*6, id*1, 
     &          bus_code(12)*2, qmaxc*10, qminc*10, qgenc*10
      real array(30)

      data date_in, date_out / '400101', '391231' /,
     &     bus_code / 'B ', 'BE', 'BS', 'BC', 'BD', 'BV', 'BQ', 'BG',
     &                'BO', 'NT', 'BX', 'BM' /
      
      ext_geg = 0         ! set default return status = successful
      error = 0           ! initialize error count

c     Process generator records (sans any d-c buses) to scratch file.  
c     "total(1)" is not known apriori.
c
c     Open scratch file for generators
c
      status = close_ge_file (1)
      status = open_ge_file (1, 'scratch1.dat', 'w+')
      total(1) = 0                  ! Total number of generators

      do ib = 1, ntot_alf
        nb = alf2inp(ib)
        ntype = kbsdta(1,nb)
        if (ntype .ne. 5 .and. ntype .ne. 12) then
          ntype = kbsdta(1,nb)
          kt = inp2opt(nb)
          vm = dsqrt (e(kt) ** 2 + f(kt) ** 2)
          va = 57.29577 * datan2 (f(kt), e(kt))
c
c         Get GE area, zone, owner, and bus number
c
          status = gtge_num (nb, iptia, iptiz, iptib)
          if (status .ne. 0) then
            error = 5
            write ( errbuf(1), 10000) bus(nb), base(nb) 
10000       format (' Cannot obtain bus number for (', a8, f7.1, ')')
            call prterx ('E', 1)
            go to 900
          endif
          iptio = add_ptiq(owner(nb))
          pgen = pnetu(kt) + ploadu(kt)
          qgen = qnetu(kt) + qloadu(kt) 
          if (abs (capcor(2,kt)) .gt. 0.01 .and. 
     &       (ntype .ne. 1 .and. ntype .ne. 4 .and. ntype .ne. 10)) then
            qgen = qgen + capcor(1,kt) * vm ** 2
          endif
          call busqua (nb, pgen, qgen, array)
          if (pgen .lt. -0.005 .and. 
     &        busdta(7,nb) - busdta(8,nb) .lt. 0.5 .and.
     &       (ntype .eq. 7 .or. ntype .eq. 8)) then
            write ( errbuf(1), 10010) bus_code(ntype), bus(nb), 
     &        base(nb), pgen*bmva, qgen*bmva
10010       format ('Type ', a, ' bus ', a8, f7.1, 
     &        ' has negative generation (', 2f9.1, 
     &        ') interpreted as load')
            call prterx ('W', 1)
          else if (abs(pgen) .gt. 0.005 .or. ntype .eq. 8 .or.
     &            (abs(pgen) .le. 0.005 .and. 
     &            (ntype .eq. 7 .or. ntype .eq. 8) .and.
     &             array(6) - array(8) .gt. 0.5)) then
            if (pgen .lt. -0.005) then
              write ( errbuf(1), 10020) bus_code(ntype), bus(nb), 
     &          base(nb), pgen*bmva, qgen*bmva
10020         format ('Type ', a, ' bus ', a8, f7.1, 
     &          ' has negative generation (', 2f9.1, 
     &          ') interpreted as pumped storage')
              call prterx ('W', 1)
            endif
            if (ntype .eq. 7 .and. 
     &         (array(10) .ne. 0.0 .or. array(12) .ne. 0.0)) then
              write ( errbuf(1), 10022) bus_code(ntype), bus(nb), 
     &          base(nb), array(6), array(8), array(10)+array(12)
10022         format ('Type ', a, ' bus ', a8, f7.1, 
     &          ' has reactive generation (', 2f9.1, 
     &          ') and continuous B (', f9.1, ')')
              call prterx ('W', 1)
            endif
            k2 = 0
            if (ntype .eq. 8) then
              ptr = ptrtbx(nb)
              k2 = tbx(8,ptr)
              if (k2 .gt. 0) then
                mt = inp2opt(k2)
                vm = dsqrt (e(mt) ** 2 + f(mt) ** 2)
                status = gtge_num (k2, iptia2, iptiz2, iptib2)
                pct = busdta(14,nb) / 100.0
                k2x = pti_num(iptib2)
                bus2c = bus(k2)
                write (base2c, fmt='(f6.2)') base(k2)
              else
                k2x = pti_num(iptib)
                bus2c = bus(nb)
                write (base2c, fmt='(f6.2)') base(nb)
                pct = 1.0
              endif
            else
              k2x = pti_num(iptib)
              bus2c = bus(nb)
              write (base2c, fmt='(f6.2)') base(nb)
              pct = 1.0
            endif
            id = '1'
            k3x = -1
            bus3c = '        '
            write (base3c, fmt='(f6.2)') 0.0
            k4x = -1
            bus4c = '        '
            write (base4c, fmt='(f6.2)') 0.0
            write (qmaxc, fmt='(f8.2)') array(6)
            write (qminc, fmt='(f8.2)') array(8)
            qmax = ftn_atof (qmaxc)
            qmin = ftn_atof (qminc)
            qgenmva = qgen*bmva
            qgenmva = qgenmva - dim(qgenmva,qmax) + dim(qmin,qgenmva)             
            write (xbuf, 10030) pti_num(iptib), bus(nb), base(nb), 
     &        id // ' ', ' ', 1, k2x, bus2c, base2c, 1.0, pct, 
     &        pti_anum(iptia), pti_znum(iptiz), pgen*bmva, array(5),
     &        array(7), qgenmva, qmax, qmin, bmva, 0.0, 0.0,
     &        0.0, 0.0, k3x, bus3c, base3c, k4x, bus4c, base4c, 
     &        date_in, date_out, 0, 0
10030       format (3x, i5, 1x, '"', a, '"', 1x, f6.2, 1x, '"', a, 
     &        '"', 1x, '"', a, '"', 1x, ':', 1x, i2, 1x, i5, 1x, '"', 
     &        a, '"', 1x, a, 2(1x, f5.2), 1x, i2, 1x, i4, 1x, f7.2, 
     &        5(1x, f8.2), 1x, f8.3, 4(1x, f5.3), 2x, i5, 1x, '"', 
     &        a, '"', 1x, a, 2x, i5, 1x, '"', a, '"', 1x, a, 
     &        2(3x, a), 1x, i3, 1x, i1, 1x, '/')
            last = lastch (xbuf)
            status = write_ge_file (1, xbuf(1:last))

            write (xbuf, 10040) 0.0, 0.0, 1.0, pti_onum(iptio), 1.0,
     &        0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0
10040       format (3(1x, f6.4), 8(1x, i3, 1x, f5.3))
            last = lastch (xbuf)
            status = write_ge_file (1, xbuf(1:last))
            total(1) = total(1) + 1
          endif
        endif

  900   continue
      enddo

      write (xbuf, 10100) total(1)
10100 format ('generator data  [', i5, ']     id   long_id_    st ---no-
     &-     reg_name       prf  qrf  ar zone   pgen   pmax   pmin   qgen
     &   qmax   qmin   mbase cmp_r cmp_x gen_rgen_x           hbus      
     &              tbus           date_in date_out pid N')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))
      xbuf = '[EOF]'
      last = lastch (xbuf)
      status = write_ge_file (1, xbuf(1:last))
      status = close_ge_file (1)
c
c     "Rewind" temp file and write to netdata file
c
      status = open_ge_file (1, 'scratch1.dat', 'r')

      finished = .false.
      do while (.not. finished)
        last = read_ge_file (1, xbuf)
        if (last .eq. 0 .or. xbuf(1:5) .eq. '[EOF]') go to 120
        status = write_ge_file (0, xbuf(1:last))
      enddo

  120 continue
      write ( errbuf(1), 10110) total(1)
10110 format (' Total generators records extracted:', i5)
      call prterx ('I', 1)
 
      ext_geg = total(1) 

      return
      end

