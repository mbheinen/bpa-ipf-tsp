C    %W% %G%
C****************************************************************
C
C     File: ext_gesvd.f
C
C     Purpose: Routine to extract SVD data in GE format
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
      integer function ext_gesvd (scrfil, version, option, total)
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
 
      logical flag, finished
      integer add_ptiq, status, error, bus_type(16), gtge_num, 
     &        close_ge_file, write_ge_file, open_ge_file, 
     &        read_ge_file, svd_type, get_cbs, ptr
      character xbuf*256, date_in*6, date_out*6, id*1, bus2c*8, 
     &          base2c*8, xstepc(8)*1, xvaluec(8)*8, cbtype*1,
     &          cbyear*2, cbown*3, bvaluec(6)*8
      data bus_type / 1, 2, 3, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1 /
      data date_in, date_out / '400101', '391231' /

      ext_gesvd = 0       ! set default return status = successful
      error = 0           ! initialize error count
c
c     Open scratch file for generators
c
      status = close_ge_file (1)
      status = open_ge_file (1, 'scratch1.dat', 'w+')
      total(1) = 0                    ! Total number of generators

      do ib = 1, ntot_alf
        nb = alf2inp(ib)
        k2 = nb
        ktbx = ptrtbx(nb)
C       
C       Obtain bus shunt values
C       
        kt = inp2opt(nb)
        kxd = busxdtptr(nb)

        ntype = kbsdta(1,nb)
        qnet = qnetu(kt)
        pgen = pnetu(kt) + ploadu(kt)
        qgen = qnetu(kt) + qloadu(kt)
        vksq = (e(kt) ** 2 + f(kt) ** 2) 
        call allocq (nb, qnet, qgen, qgnmax, qgnmin, qld, totcap, 
     &               usecap, totrek, userek, unsked, qerr)
        if (kxd .gt. 0) then
          flag = (totrek .lt. -0.1 .or. totcap .gt. 0.1)
        else if (ntype .eq. 2 .or. ntype .eq. 7) then
          flag = (totrek .lt. -0.1 .or. totcap .gt. 0.1)
        else
          flag = .false.
        endif
        if (flag) then
c
c         Determine any fixed shunt value
c
          bfixed = 0.0
          ptr = kbsdta(15,nb)
          do while (ptr .gt. 0)
            call getchr(1, cbtype, kbctbl(8, ptr))
            call getchr(2, cbyear, kbctbl(9, ptr))
            call getchr(3, cbown, kbctbl(10, ptr))
            if (cbtype .eq. 'A') then
              bfixed = bfixed + bctbl(5, ptr) 
            endif
            ptr = bctbl_nxt(ptr)
          enddo
          totrek = totrek - amin1 (0.0, bfixed * vksq)
          totcap = totcap - amax1 (0.0, bfixed * vksq)
          userek = userek - amin1 (0.0, bfixed * vksq)
          usecap = usecap - amax1 (0.0, bfixed * vksq)
          flag = (totrek .lt. -0.1 .or. totcap .gt. 0.1)
        endif
        if (flag) then
c
c         Get GE area, zone, owner, and bus number
c
          status = gtge_num (nb, iptia, iptiz, iptib)
          if (status .ne. 0) then
            error = 5
            write ( errbuf(1), 10050) bus(nb), base(nb) 
10050       format (' Cannot obtain bus number for (', a8, f7.1, ')')
            call prterx ('E', 1)
            go to 900
          endif

          gconst = 0.0
          bconst = 0.0
          bmin_c = 0.0
          bmax_c = 0.0
          bmin_d = 0.0
          bmax_d = 0.0
          if (ntype .eq. 2 .or. ntype .eq. 7) then
          else if (ntype .eq. 11) then
            bconst = (userek + usecap) / vksq
          else if (ntype .eq. 1 .or. ntype .eq. 4 .or.
     &             ntype .eq. 6 .or. ntype .eq. 10) then
            bconst = (userek + usecap) / vksq
          endif
          if (kxd .gt. 0 .and. ntype .eq. 11) then
            ptr = get_cbs (nb, 'A', '   ', 'X1')
            if (ptr .gt. 0) then
              gconst = bctbl(4,ptr)
              bconst = bctbl(5,ptr)
            endif            
            ptr = get_cbs (nb, 'A', '   ', 'X2')
            if (ptr .gt. 0) then
              bmin_c = bctbl(5,ptr) + bconst
            endif            
            ptr = get_cbs (nb, 'X', '   ', 'X2')
            if (ptr .gt. 0) then
              bmax_c = bctbl(5,ptr) + bmin_c
            endif            
            bmin_d = xdata(3,kxd) 
            bmax_d = xdata(4,kxd)
          endif

          bmin_tot = bmin_c + bmin_d
          bmax_tot = bmax_c + bmax_d
          kt = inp2opt(nb)
          svd_type = 0
          if (kxd .eq. 0 .and. ntype .ne. 11) then
            kxd = kxtot + 1
            xdata(3,kxd) = totrek / vksq
            xdata(4,kxd) = totcap / vksq
            xdata(5,kxd) = xdata(3,kxd)
            xdata(6,kxd) = xdata(4,kxd)
            xdata(7,kxd) = 1.0
            xdata(8,kxd) = (totrek + totcap) / vksq
            do i = 9, 22
              xdata(i,kxd) = 0.0
            enddo
            if (ntype .eq. 2 .or. ntype .eq. 7) then
              svd_type = 2
              bconst = (usecap + userek) / vksq
            else if (ntype .eq. 1 .or. ntype .eq. 4 .or.
     &               ntype .eq. 6 .or. ntype .eq. 10) then
              svd_type = 0
              bconst = (usecap + userek) / vksq
            endif
            dv = 0.5 * (vlimx(kt) - vlimn(kt))
          else if (ntype .eq. 11) then
            if (bmin_c .eq. 0.0 .and. bmax_c .eq. 0.0) then
              svd_type = 4
            else
              svd_type = 1
            endif
            mt = inp2opt(k2)
            dv = 0.5 * (vlimx(kt) - vlimn(kt))
c
c           Get GE area, zone, owner, and bus number
c
            status = gtge_num (k2, iptia2, iptiz2, iptib2)
            k2x = pti_num(iptib2)
          else if (ntype .eq. 2 .or. ntype .eq. 7) then
            svd_type = 2
            bconst = (usecap + userek) / vksq
          else if (ntype .eq. 1 .or. ntype .eq. 4 .or.
     &             ntype .eq. 6 .or. ntype .eq. 10) then
            svd_type = 0
          endif
          if (ntype .eq. 8 .or. ntype .eq. 11) then
            k2 = kbsdta(13,nb)
            if (k2 .lt. 0) then
              k2 = -k2
            else if (k2 .eq. 0) then
              k2 = nb
            endif
          endif
          status = gtge_num (k2, iptiar, iptizr, iptibr)
          k2x = pti_num(iptibr)
          bus2c = bus(k2)
          write (base2c, fmt='(f6.2)') base(k2)

          iptio = add_ptiq(owner(nb))
          id = 'v'
          if (abs (gconst/bmva) .lt. 10.0) then
            write (bvaluec(1), fmt='(f7.4)') gconst/bmva
          else
            write (bvaluec(1), fmt='(f7.2)') gconst/bmva
          endif
          if (abs (bconst/bmva) .lt. 10.0) then
            write (bvaluec(2), fmt='(f7.4)') bconst/bmva
          else
            write (bvaluec(2), fmt='(f7.2)') bconst/bmva
          endif
          if (abs (bmin_c/bmva) .lt. 10.0) then
            write (bvaluec(3), fmt='(f7.4)') bmin_c/bmva
          else
            write (bvaluec(3), fmt='(f7.2)') bmin_c/bmva
          endif
          if (abs (bmax_c/bmva) .lt. 10.0) then
            write (bvaluec(4), fmt='(f7.4)') bmax_c/bmva
          else
            write (bvaluec(4), fmt='(f7.2)') bmax_c/bmva
          endif
          if (abs (bmin_tot/bmva) .lt. 10.0) then
            write (bvaluec(5), fmt='(f7.4)') bmin_tot/bmva
          else
            write (bvaluec(5), fmt='(f7.2)') bmin_tot/bmva
          endif
          if (abs (bmax_tot/bmva) .lt. 10.0) then
            write (bvaluec(6), fmt='(f7.4)') bmax_tot/bmva
          else
            write (bvaluec(6), fmt='(f7.2)') bmax_tot/bmva
          endif
          write (xbuf, 10030) pti_num(iptib), bus(nb), base(nb), 
     &      id // ' ', ' ', 1, svd_type, k2x, bus2c, base2c, 
     &      pti_anum(iptia), pti_znum(iptiz), bvaluec(1)(1:7), 
     &      bvaluec(2)(1:7), bvaluec(3)(1:7), bvaluec(4)(1:7), dv,
     &      bvaluec(3)(1:7), bvaluec(4)(1:7), date_in, date_out, 0, 0, 
     &      pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 0, 0.0
10030     format (3x, i5, 1x, '"', a, '"', 1x, f6.2, 1x, '"', a, 
     &      '"', 1x, '"', a, '"', 1x, ':', 1x, i2, 1x, i2, 3x, i5, 1x, 
     &      '"', a, '"', 1x, a, 1x, i2, 1x, i4, 4(1x, a), 1x, f7.4,
     &      2(1x, a), 1x, 2(3x, a), 2x, i1, 1x, i1, 4(1x, i4, 1x, f5.3), 
     &      1x, '/')
          last = lastch (xbuf)
          status = write_ge_file (1, xbuf(1:last))

          do i = 1, 8
            istep = xdata(5+2*i, kxd)
            if (istep .gt. 0 .and. 
     &         (svd_type .eq. 0 .or. svd_type .eq. 1 .or. 
     &          svd_type .eq. 2 .or. svd_type .eq. 4)) then
              write (xstepc(i), fmt='(i1)') istep
              xvalue = xdata(6+2*i,kxd) / bmva
              write (xvaluec(i), fmt='(f7.4)') xvalue
            else
              xstepc(i) = '0'
              xvaluec(i) = '0.0'
            endif
          enddo        
          write (xbuf, 10040) (xstepc(i), xvaluec(i), i = 1, 8)
10040     format (9x, 10(1x, a, 1x, a))
          last = lastch (xbuf)
          status = write_ge_file (1, xbuf(1:last))
          total(1) = total(1) + 1

        endif
  900   continue
      enddo

      write (*, 10090) total(1)
10090 format (' * Writing ', i5, ' SVD records to NETDAT file')

      write (xbuf, 10100) total(1)
10100 format ('svd data  [', i5, ']             id   long_id_    st ty -
     &-no---     reg_name       ar zone      g      b  min_c  max_c  vba
     &nd   bmin   bmax  date_in date_out pid N own part1 own part2 own p
     &art3 own part4')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))
c
c     "Rewind" temp file and write to netdata file
c
      xbuf = '[EOF]'
      last = lastch (xbuf)
      status = write_ge_file (1, xbuf(1:last))
      status = close_ge_file (1)
      status = open_ge_file (1, 'scratch1.dat', 'r')

      finished = .false.
      do while (.not. finished)
        last = read_ge_file (1, xbuf)
        if (last .eq. 0 .or. xbuf(1:5) .eq. '[EOF]') go to 120
        status = write_ge_file (0, xbuf(1:last))
      enddo

  120 continue
      write ( errbuf(1), 10110) total(1)
10110 format (' Total svd records extracted:', i5)
      call prterx ('I', 1)
 
      ext_gesvd = total(1)

      return
      end
