C    @(#)ext_gedcb.f	20.2 1/15/98
C****************************************************************
C
C     File: ext_gedcb.f
C
C     Purpose: Routine to extract d-c bus data in GE format
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
      integer function ext_gedcb (scrfil, version, option, total)
      integer scrfil, version, total(4)
      character *(*) option(10)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/ordsta.inc'
 
      integer add_ptiq, status, error, bus_type(16), write_ge_file, 
     &        fnd_ptia, fnd_ptiy
      character xbuf*256, date_in*6, date_out*6, id*1

      data bus_type / 1, 2, 3, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1 /
      data date_in, date_out / '400101', '391231' /

      ext_gedcb = 0       ! set default return status = successful
      error = 0           ! initialize error count

      if (option(3) .eq. 'Y') then
        icount = 2*kdtot
      else
        icount = mtdcbs+2*kdtot
      endif
      write (*, 10000) icount
10000 format (' * Writing ', i5, ' dc bus records to NETDAT file')

      write (xbuf, 10010) icount
10010 format ('dc bus data  [', i2, ']             ty ar  zone   vsched 
     &    dc_volt  date_in date_out pid own')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      do jt = 1, kdtot
        k1 = dc2t(1,jt)
        k2 = dc2t(3,jt)
        if (orddc .eq. 2) then
          k1 = opt2inp(k1)
          k2 = opt2inp(k2)
        endif
        id = ' '
        num_dc = 2*jt - 1
        vdc = dc2t(40,jt)
        do i = 1, 2
          iptio = add_ptiq(owner(k1))
          ntype = kbsdta(1,k1)
          kt = inp2opt(k1)
          vac = vlimx(kt)
          if (ntype .eq. 1) then
          else if (ntype .eq. 4 .or. ntype .eq. 5 .or.
     &             ntype .eq. 10 .or. ntype .eq. 12) then
            ntype = 1
          else if (ntype .eq. 3) then
          else
            ntype = 2
          endif
          iptia = fnd_ptia (arcnam(jarzn(k1)))
          iptiz = fnd_ptiy (zone(k1))
          write (xbuf, 10030) num_dc, bus(k1), base(k1), 
     &      ntype, pti_anum(iptia), pti_znum(iptiz), vac, vdc, 
     &      date_in, date_out, 0, pti_onum(iptio)
10030     format (3x, i5, 1x, '"', a, '"', 1x, f6.2, 2x, ':', 2x,
     &      i1, 1x, i2, 1x, i4, 1x, f9.3, 1x, f10.3, 2(3x, a), 2x, 
     &      i1, 1x, i4)
          last = lastch (xbuf)
          status = write_ge_file (0, xbuf(1:last))
          k1 = k2
          num_dc = num_dc + 1
          vdc = dc2t(41,jt)
        enddo
      enddo

      if (option(3) .eq. 'Y') go to 100
      do jt = 1, mtdcbs
        k1 = dcmtbs(1,jt)
        if (orddc .eq. 2) k1 = opt2inp(k1)
        id = ' '
        num_dc = dcmtbs(22,jt)
        vdc = dcmtbs(20,jt)
        iptio = add_ptiq(owner(k1))
        ntype = kbsdta(1,k1)
        kt = inp2opt(k1)
        vac = vlimx(kt)
        if (ntype .eq. 1) then
        else if (ntype .eq. 4 .or. ntype .eq. 5 .or.
     &           ntype .eq. 10 .or. ntype .eq. 12) then
          ntype = 1
        else if (ntype .eq. 3) then
        else
          ntype = 2
        endif
        iptia = fnd_ptia (arcnam(jarzn(k1)))
        iptiz = fnd_ptiy (zone(k1))
        write (xbuf, 10030) num_dc, bus(k1), base(k1), 
     &    ntype, pti_anum(iptia), pti_znum(iptiz), vac, vdc, 
     &    date_in, date_out, 0, pti_onum(iptio)
          last = lastch (xbuf)
        status = write_ge_file (0, xbuf(1:last))
      enddo
  100 continue

      write ( errbuf(1), 10120) icount
10120 format (1x, i5, ' d-c bus records written to NETDAT file')
      call prterx ('I', 1)
 
      ext_gedcb = icount

      return
      end
