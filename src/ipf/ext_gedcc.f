C    @(#)ext_gedcc.f	20.8 2/28/00
C****************************************************************
C
C     File: ext_gedcc.f
C
C     Purpose: Routine to extract d-c converter data in GE format
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
      integer function ext_gedcc (scrfil, version, option, total)
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
      include 'ipfinc/branch.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/ordsta.inc'
 
      integer add_ptiq, status, error, bus_type(16), gtge_num, ptr, 
     &        write_ge_file, cnvt_type, fnd_ptia, fnd_ptiy
      character xbuf*256, date_in*6, date_out*6, bus2c*8, base2c*6,
     &          id*1
      logical ior

      data bus_type / 1, 2, 3, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1 /
      data date_in, date_out / '400101', '391231' /

      ext_gedcc = 0       ! set default return status = successful
      error = 0           ! initialize error count

      if (option(3) .eq. 'Y') then
        icount = 2*kdtot
      else
        icount = 2*kdtot + mtdcbs
      endif
      write (*, 10000) icount
10000 format (' * Writing ', i5, ' dc converter records to NETDAT file')

      write (xbuf, 10010) icount
10010 format ('dc converter data  [', i2, ']                            
     &   id   long_id_    st ty mdnb fg  --no---           reg   ar zone
     &  date_in date_out')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      do jt = 1, kdtot
        k1 = dc2t(1,jt)
        k1c = dc2t(33,jt)
        k2 = dc2t(3,jt)
        k2c = dc2t(34,jt)
        if (orddc .eq. 2) then
          k1 = opt2inp(k1)
          k2 = opt2inp(k2)
          k1c = opt2inp(k1c)
          k2c = opt2inp(k2c)
        endif
        id = ' '
        num_dc = 2*jt - 1
        vdc = dc2t(40,jt)
        adc = dc2t(39,jt)
        pdc = 0.001 * vdc * adc
        mode = 5
        num_bridges = dc2t(15,jt)
        ltc = dc2t(37,jt)
        do cnvt_type = 1, 2
c
c         Get GE area, zone, owner, and bus number
c
          status = gtge_num (k1c, iptia, iptiz, iptib)
          if (status .ne. 0) then
            error = 5
            go to 100
          endif
          iptio = add_ptiq(owner(k1c))
          kt = inp2opt(k1)
          if (ltc .gt. 0) then 
            ltc_mode = 1
          else
            ltc_mode = 0
          endif
          k2x = num_dc
          bus2c = bus(k1)
          write (base2c, fmt='(f6.2)') base(k1)

          write (xbuf, 10030) pti_num(iptib), bus(k1c), base(k1c), 
     &      num_dc, bus(k1), base(k1), '1 ', ' ', 1, cnvt_type, mode,
     &      num_bridges, ltc_mode, num_dc, bus2c, base2c, 
     &      pti_anum(iptia), pti_znum(iptiz), date_in, date_out 
10030     format (3x, i5, 1x, '"', a, '"', 1x, f6.2, 3x, i5, 1x, '"', 
     &      a, '"', 1x, f6.2, 1x, '"', a, '"', 1x, '"', a, '"', 2x, 
     &      ':', 2x, i1, 1x, i1, 1x, i1, 1x, i1, 1x, i1, 3x, i5, 1x, 
     &      '"', a, '"', 1x, a, 2x, i2, 1x, i4, 2(3x, a), 1x, '/')
          last = lastch (xbuf)
          status = write_ge_file (0, xbuf(1:last))

          if (cnvt_type .eq. 1) then
            amargin = 0.1 * dc2t(14,jt)
          else
            amargin = 0.1 * dc2t(17,jt)
          endif

          ptr = numbrn (k1c, k1, '*', 0)
          if (brtype(ptr) .eq. 4) ptr = brnch_nxt(ptr)
          nbr = iabs (brnch_ptr(ptr))
          ior = ((brnch_ptr(ptr) .gt. 0 .and. 
     &           kbrnch(15, nbr) .eq. 0) .or.
     &           (brnch_ptr(ptr) .lt. 0 .and. 
     &           kbrnch(15, nbr) .gt. 0)) 
          if (ior) then
            aloss = 1.0
            iptia = fnd_ptia (arcnam(jarzn(k1)))
            iptiz = fnd_ptiy (zone(k1))
          else
            aloss = 0.0
            iptia = fnd_ptia (arcnam(jarzn(k1c)))
            iptiz = fnd_ptiy (zone(k1c))
          endif

          call calcflow (k1c, k1, brid(ptr), pintot, qintot, pouttot, 
     &                   qouttot, error)
          write (xbuf, 10040) adc, vdc, pintot, qintot, pdc, 0.0,
     &      vdc, amargin, 0.0, 0.0, 0.0, base(k1c), base(k1)
10040     format (8(1x, f7.1), 1x, f6.4, 4(1x, f6.1), 1x, '/')
          last = lastch (xbuf)
          status = write_ge_file (0, xbuf(1:last))

          alpha = 57.2957795 * dc2t(22,jt)                !Alpha
          alpha_min = 57.2957795 * dc2t(18,jt)            !Alpha min
          alpha_max = 90.0                                !Alpha max
          gamma = 57.2957795 * dc2t(26,jt)                !Alpha
          gamma_min = 57.2957795 * dc2t(25,jt)            !Gamma min
          gamma_max = 90.0                                !Gamma max
          volt_drop = dc2t(13,jt) / dc2t(12,jt)           !Voltage drop

          if (cnvt_type .eq. 1) then
            factor = base(k1) ** 2 / bmva
            r_tx = dc2t(29,jt) / factor     !Rtran/bridge (p.u.)
            x_tx = dc2t(30,jt) / factor     !Xtran/bridge (p.u.)
            rci = 0.95492 * dc2t(30,jt)     !Xcomm/bridge (ohms)
          else
            factor = base(k1) ** 2 / bmva
            r_tx = dc2t(31,jt) / factor     !Rtran/bridge (p.u.)
            x_tx = dc2t(32,jt) / factor     !Xtran/bridge (p.u.)
            rci = 0.95492 * dc2t(32,jt)     !Xcomm/bridge (ohms)
          endif
          write (xbuf, 10050) alpha, alpha_min, alpha_max, gamma, 
     &      gamma_min, gamma_max, volt_drop, rci, r_tx, x_tx
10050     format (6(1x, f6.1), 1x, f6.2, 1x, f6.3, 2(1x, f6.4), 1x, 
     &      '/')
          last = lastch (xbuf)
          status = write_ge_file (0, xbuf(1:last))

          tx_base = brnch(18,nbr)
          if (tx_base .eq. 0.0) tx_base = bmva
          if (brnch_ptr(ptr) .gt. 0) then
            tap1 = base(k1c) / brnch(9,nbr) 
            tap2 = base(k1) / brnch(10,nbr) 
          else
            tap1 = base(k1c) / brnch(10,nbr) 
            tap2 = base(k1) / brnch(9,nbr) 
          endif
          if (ltc .gt. 0) then
            tapmax = 1.0 / (tran(8,ltc) * tran(6,ltc))
            tapmin = 1.0 / (tran(7,ltc) * tran(6,ltc))
            if (tran(11,ltc) .gt. 1.0) then
              tstep = (tapmax - tapmin) / tran(11,ltc)
            else
              tstep = 0.0
            endif
          else
            tapmax = tap1
            tapmin = tap1
            tstep = 0.0
          endif
          if (cnvt_type .eq. 1) then
            rate_cnvt = dc2t(14,jt)
            smooth = dc2t(27,jt)
          else
            rate_cnvt = dc2t(17,jt)
            smooth = dc2t(28,jt)
          endif
          write (xbuf, 10060) tx_base, 1.0, 1.0, tap1, tap2, tapmin,
     &      tapmax, tstep, 0.0, 0.0, aloss, rate_cnvt, smooth, 0, 0
10060     format (1x, f6.1, 9(1x, f6.4), 1x, f6.1, 1x, f6.1, 1x, 
     &      f7.2, 1x, i1, 1x, i1, 1x, '/')
          last = lastch (xbuf)
          status = write_ge_file (0, xbuf(1:last))

          iptio = add_ptiq(owner(k1))
          write (xbuf, 10070) pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0,
     &      0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0
10070     format (8(1x, i4, 1x, f5.3))
          last = lastch (xbuf)
          status = write_ge_file (0, xbuf(1:last))

          k1 = k2
          k1c = k2c
          num_dc = num_dc + 1
          vdc = dc2t(41,jt)
          adc = -dc2t(39,jt)
          pdc = 0.001 * vdc * adc
          mode = 0

  100     continue
        enddo
      enddo

      if (option(3) .eq. 'Y') go to 200
      do jt = 1, mtdcbs
        k1 = dcmtbs(1,jt)
        k1c = dcmtbs(3,jt)
        if (orddc .eq. 2) then
          k1 = opt2inp(k1)
          if (k1c .gt. 0) k1c = opt2inp(k1c)
        endif
        id = ' '
        num_dc = dcmtbs(22,jt)
        vdc = dcmtbs(20,jt)
        pdc = dcmtbs(19,jt)
        adc = 1000.0 * pdc / vdc
        if (dcmtbs(19,jt) .gt. 0.01) then
          cnvt_type = 1                    ! Recitfier
          mode = 5
        else   
          cnvt_type = 2                    ! Inverter
          mode = 0
        endif
c       if (dcmtbs(6,jt) .ne. 0.0) then
c         mode = 5                         ! Pdc, Vdc scheduled
c       else if (dcmtbs(5,jt) .ne. 0.0) then
c         mode = 3                         ! Pdc scheduled
c       else
c         mode = 1
c       endif
        num_bridges = dcmtbs(7,jt)
        ltc = dcmtbs(15,jt)
c
c       Get GE area, zone, owner, and bus number
c
        status = gtge_num (k1c, iptia, iptiz, iptib)
        if (status .ne. 0) then
          error = 5
          go to 110
        endif
        iptio = add_ptiq(owner(k1c))
        kt = inp2opt(k1)
        if (ltc .gt. 0) then 
          ltc_mode = 1
        else
          ltc_mode = 0
        endif
        k2x = num_dc
        bus2c = bus(k1)
        write (base2c, fmt='(f6.2)') base(k1)

        write (xbuf, 10030) pti_num(iptib), bus(k1c), base(k1c), 
     &    num_dc, bus(k1), base(k1), '1 ', ' ', 1, cnvt_type, mode,
     &    num_bridges, ltc_mode, num_dc, bus2c, base2c, 
     &    pti_anum(iptia), pti_znum(iptiz), date_in, date_out 
        last = lastch (xbuf)
        status = write_ge_file (0, xbuf(1:last))

        amargin = 0.10 * dcmtbs(9,jt)
        ptr = numbrn (k1c, k1, '*', 0)
        if (brtype(ptr) .eq. 4) ptr = brnch_nxt(ptr)
        nbr = iabs (brnch_ptr(ptr))
        ior = ((brnch_ptr(ptr) .gt. 0 .and. 
     &         kbrnch(15, nbr) .eq. 0) .or.
     &         (brnch_ptr(ptr) .lt. 0 .and. 
     &         kbrnch(15, nbr) .gt. 0)) 
        if (ior) then
          aloss = 1.0
          iptia = fnd_ptia (arcnam(jarzn(k1)))
          iptiz = fnd_ptiy (zone(k1))
        else
          aloss = 0.0
          iptia = fnd_ptia (arcnam(jarzn(k1c)))
          iptiz = fnd_ptiy (zone(k1c))
        endif

        call calcflow (k1c, k1, brid(ptr), pintot, qintot, pouttot, 
     &                 qouttot, error)
        write (xbuf, 10040) adc, vdc, pintot, qintot, pdc, 0.0,
     &    vdc, amargin, 0.0, 0.0, 0.0, base(k1c), base(k1)
        last = lastch (xbuf)
        status = write_ge_file (0, xbuf(1:last))

        alpha = 57.2957795 * dcmtbs(13,jt)                !Alpha
        alpha_min = 57.2957795 * dcmtbs(10,jt)            !Alpha min
        alpha_max = 90.0                                  !Alpha max
        gamma = 57.2957795 * dcmtbs(13,jt)                !Gamma
        gamma_min = 57.2957795 * dcmtbs(29,jt)            !Gamma min
        gamma_max = 90.0                                  !Gamma max
        volt_drop = dcmtbs(8,jt) / dcmtbs(7,jt)           !Voltage drop
        factor = base(k1) ** 2 / bmva
        r_tx = dcmtbs(17,jt) / factor     !Rtran/bridge (p.u.)
        x_tx = dcmtbs(18,jt) / factor     !Xtran/bridge (p.u.)
        rci = 0.95493 * dcmtbs(18,jt)     !Xcomm/bridge (ohms)
        write (xbuf, 10050) alpha, alpha_min, alpha_max, gamma, 
     &    gamma_min, gamma_max, volt_drop, rci, r_tx, x_tx
        last = lastch (xbuf)
        status = write_ge_file (0, xbuf(1:last))

        tx_base = brnch(18,nbr)
        if (tx_base .eq. 0.0) tx_base = bmva
        if (brnch_ptr(ptr) .gt. 0) then
          tap1 = base(k1c) / brnch(9,nbr) 
          tap2 = base(k1) / brnch(10,nbr) 
        else
          tap1 = base(k1c) / kbrnch(10,nbr)
          tap2 = base(k1) / brnch(9,nbr) 
        endif
        if (ltc .gt. 0) then
          tapmax = 1.0 / (tran(8,ltc) * tran(6,ltc))
          tapmin = 1.0 / (tran(7,ltc) * tran(6,ltc))
          if (tran(11,ltc) .gt. 1.0) then
            tstep = (tapmax - tapmin) / tran(11,ltc)
          else
            tstep = 0.0
          endif
        else
          tapmax = tap1
          tapmin = tap1
          tstep = 0.0
        endif
        rate_cnvt = dcmtbs(9,jt)
        smooth = dcmtbs(14,jt)
        write (xbuf, 10060) tx_base, 1.0, 1.0, tap1, tap2, tapmin,
     &    tapmax, tstep, 0.0, 0.0, aloss, rate_cnvt, smooth, 0, 0
        last = lastch (xbuf)
        status = write_ge_file (0, xbuf(1:last))

        iptio = add_ptiq(owner(k1))
        write (xbuf, 10070) pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0,
     &    0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0
        last = lastch (xbuf)
        status = write_ge_file (0, xbuf(1:last))
  110   continue
      enddo
  200 continue

      write ( errbuf(1), 10120) icount
10120 format (1x, i5, ' d-c converter records written to NETDAT file')
      call prterx ('I', 1)
 
      ext_gedcc = icount

      return
      end
