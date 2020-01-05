C    @(#)ext_geld.f	20.10 4/5/00
C****************************************************************
C
C     File: ext_geld.f
C
C     Purpose: Routine to extract load data in GE format
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
      integer function ext_geld (scrfil, version, option, total)
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
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
 
      logical found, finished, load_sw, gen_sw, dc_sw
      integer add_ptiq, status, error, gtge_num, ptr, close_ge_file, 
     &        write_ge_file, read_ge_file, open_ge_file, num_cb_type
      character xbuf*256, date_in*6, date_out*6, id*1, cbtype*1,
     &          cbyear*2, cbown*3, cb_type(100)*1, cb_own(100)*3, 
     &          cb_year(100)*2, cb_type_old*1, tempc*1, bus_code(12)*1
      real cb_load(6,100)

      data date_in, date_out / '400101', '391231' /,
     &     bus_code / 'B ', 'BE', 'BS', 'BC', 'BD', 'BV', 'BQ', 'BG',
     &                'BO', 'NT', 'BX', 'BM' /

      ext_geld = 0        ! set default return status = successful
      error = 0           ! initialize error count

c     Process load records (sans any d-c buses) to scratch file.  
c     "total(1)" is not known a priori.
c
c     Open scratch file for loads
c
      status = close_ge_file (1)
      status = open_ge_file (1, 'scratch1.dat', 'w+')
      total(1) = 0                  ! Total number of loads

      do ib = 1, ntot_alf
        nb = alf2inp(ib)
        ntype = kbsdta(1,nb)
        if (ntype .ne. 5 .and. ntype .ne. 12) then
          ntype = kbsdta(1,nb)
          kt = inp2opt(nb)
          load_sw = (abs (ploadu(kt)) .gt. 0.0 .or. 
     &               abs (qloadu(kt)) .gt. 0.0)
          pgen = pnetu(kt) + ploadu(kt)
          qgen = qnetu(kt) + qloadu(kt)
c
c         "gen_sw" = .true. denotes generation interpreted as load
c
          gen_sw = (pgen .lt. -0.005 .and. 
     &              busdta(7,nb) - busdta(8,nb) .lt. 0.5 .and.
     &             (ntype .eq. 7 .or. ntype .eq. 8))
          if (gen_sw) then
            write ( errbuf(1), 10000) bus_code(ntype), bus(nb), 
     &        base(nb), pgen*bmva, qgen*bmva
10000       format ('Type ', a, ' bus ', a8, f7.1, 
     &        ' has negative generation (', 2f9.1, 
     &        ') interpreted as load')
            call prterx ('W', 1)
          endif
          found = .false.
          if (option(3) .eq. 'Y') then
            do i = 1, mtdcbs
              if (dcmtbs(3,i) .eq. nb) found = .true.
            enddo
            dc_sw = found
          else
            dc_sw = .false.
          endif
          if (load_sw .or. dc_sw .or. gen_sw) then
            vm = dsqrt (e(kt) ** 2 + f(kt) ** 2)
c
c           Get GE area, zone, owner, and bus number
c
            status = gtge_num (nb, iptia, iptiz, iptib)
            if (status .ne. 0) then
              error = 5
              write ( errbuf(1), 10010) bus(nb), base(nb) 
10010         format (' Cannot obtain bus number for (', a8, f7.1, ')')
              call prterx ('E', 1)
              go to 900
            endif

            ploadmw = busdta(3,nb)
            qloadmv = busdta(4,nb)
            if (gen_sw) then
              if (busdta(7,nb) .eq. 0.0 .and. busdta(8,nb) .lt. 0.5) 
     &          then
                ploadmw = ploadmw - busdta(8,nb)
                qloadmv = qloadmv - busdta(9,nb) 
              endif
            endif
            if (ploadmw .ne. 0.0 .or. qloadmv .ne. 0.0) then
              iptio = add_ptiq(owner(nb))
              id = '1'
              write (xbuf, 10030) pti_num(iptib), bus(nb), base(nb), 
     &          id // ' ', ' ', 1, ploadmw, qloadmv, 0.0, 0.0,
     &          0.0, 0.0, pti_anum(iptia), pti_znum(iptiz), date_in,
     &          date_out, 0, 0, pti_onum(iptio)
10030         format (3x, i5, 1x, '"', a, '"', 1x, f6.2, 1x, '"', a, 
     &          '"', 1x, '"', a, '"', 1x, ':', 1x, i2, 6(1x, f8.3), 1x,
     &          i2, 1x, i4, 2(3x, a), 2x, i1, 2x, i1, 1x, i4)
              last = lastch (xbuf)
              status = write_ge_file (1, xbuf(1:last))
              total(1) = total(1) + 1

            endif
            if (dc_sw) then
              ptot = 0.0
              qtot = 0.0
              do i = 1, mtdcbs
                if (dcmtbs(3,i) .eq. nb) then
                  k2 = dcmtbs(1,i)
                  call calcflow (nb, k2, '*', pintot, qintot, pouttot,
     &                           qouttot, ierr)
                  ptot = ptot + pintot
                  qtot = qtot + qintot
                endif
              enddo
              iptio = add_ptiq(owner(nb))
              id = 'dc'
              write (xbuf, 10040) pti_num(iptib), bus(nb), base(nb), 
     &          id, ' ', 1, ptot, qtot, 0.0, 0.0,
     &          0.0, 0.0, pti_anum(iptia), pti_znum(iptiz), date_in,
     &          date_out, 0, 0, pti_onum(iptio)
10040         format (3x, i5, 1x, '"', a, '"', 1x, f6.2, 1x, '"', a, 
     &          '"', 1x, '"', a, '"', 1x, ':', 1x, i2, 6(1x, f8.2), 1x,
     &          i2, 1x, i4, 2(3x, a), 2x, i1, 2x, i1, 1x, i4)
              last = lastch (xbuf)
              status = write_ge_file (1, xbuf(1:last))
              total(1) = total(1) + 1
            endif

            num_cb = 0
            ptr = kbsdta(15,nb)
            do while (ptr .gt. 0)
              call getchr(1, cbtype, kbctbl(8, ptr))
              call getchr(2, cbyear, kbctbl(9, ptr))
              call getchr(3, cbown, kbctbl(10, ptr))
              pload2 = 0.0
              qload2 = 0.0
              aload2 = 0.0
              bload2 = 0.0
              gload2 = 0.0
              bload2 = 0.0
              if (cbtype .eq. 'A') then
                if (cbyear .eq. '01' .or. cbyear .eq. '*I') then
                  aload2 = bctbl(2, ptr)
                  bload2 = bctbl(3, ptr)
                  gload2 = bctbl(4, ptr)
                  bload2 = bctbl(5, ptr)
                  if (gen_sw .and. bctbl(6, ptr) .lt. 0.0) then
                    pload2 = pload2 - bctbl(6, ptr)
                    qload2 = qload2 - bctbl(11, ptr)
                  endif
                elseif (cbyear .eq. '02' .or. cbyear .eq. '*P') then
                  pload2 = bctbl(2, ptr)
                  qload2 = bctbl(3, ptr)
                  gload2 = bctbl(4, ptr)
                  bload2 = bctbl(5, ptr)
                  if (gen_sw .and. bctbl(6, ptr) .lt. 0.0) then
                    pload2 = pload2 - bctbl(6, ptr)
                    qload2 = qload2 - bctbl(11, ptr)
                  endif
                elseif (cbyear .eq. '*Z') then
                  pload2 = bctbl(2, ptr)
                  qload2 = bctbl(3, ptr)
                  gload2 = bctbl(4, ptr)
                  bload2 = bctbl(5, ptr)
                  if (gen_sw .and. bctbl(6, ptr) .lt. 0.0) then
                    pload2 = pload2 - bctbl(6, ptr)
                    qload2 = qload2 - bctbl(11, ptr)
                  endif
                else
                  pload2 = bctbl(2, ptr)
                  qload2 = bctbl(3, ptr)
                  if (gen_sw .and. bctbl(6, ptr) .lt. 0.0) then
                    pload2 = pload2 - bctbl(6, ptr)
                    qload2 = qload2 - bctbl(11, ptr)
                  endif
                endif
              elseif (cbyear .eq. '*I') then
                aload2 = bctbl(2, ptr)
                bload2 = bctbl(3, ptr)
                gload2 = bctbl(4, ptr)
                bload2 = bctbl(5, ptr)
                if (gen_sw .and. bctbl(6, ptr) .lt. 0.0) then
                  pload2 = pload2 - bctbl(6, ptr)
                  qload2 = qload2 - bctbl(11, ptr)
                endif
              elseif (cbyear .eq. '*P') then
                pload2 = bctbl(2, ptr)
                qload2 = bctbl(3, ptr)
                gload2 = bctbl(4, ptr)
                bload2 = bctbl(5, ptr)
                if (gen_sw .and. bctbl(6, ptr) .lt. 0.0) then
                  pload2 = pload2 - bctbl(6, ptr)
                  qload2 = qload2 - bctbl(11, ptr)
                endif
              else
                pload2 = bctbl(2, ptr)
                qload2 = bctbl(3, ptr)
                if (gen_sw .and. bctbl(6, ptr) .lt. 0.0) then
                  pload2 = pload2 - bctbl(6, ptr)
                  qload2 = qload2 - bctbl(11, ptr)
                endif
              endif
              if (pload2 .ne. 0.0 .or. qload2 .ne. 0.0 .or.
     &            aload2 .ne. 0.0 .or. bload2 .ne. 0.0 .or. 
     &            gload2 .ne. 0.0 .or. bload2 .ne. 0.0) then
                found = .false.
                i = 1
                do while (i .le. num_cb .and. .not. found)
                  if (cbtype .eq. cb_type(i) .and.
     &                cbyear .eq. cb_year(i) .and. 
     &                cbown  .eq. cb_own(i)) then
                    found = .true.
                  else
                    i = i + 1
                  endif
                enddo       
                if (.not. found) then
                  num_cb = num_cb + 1
                  cb_type(num_cb) = cbtype
                  cb_year(num_cb) = cbyear
                  cb_own(num_cb) = cbown
                  do i = 1, 6
                    cb_load(i, num_cb) = 0.0
                  enddo
                  i = num_cb
                endif
                cb_load(1,i) = cb_load(1,i) + pload2
                cb_load(2,i) = cb_load(2,i) + qload2
                cb_load(3,i) = cb_load(3,i) + aload2
                cb_load(4,i) = cb_load(4,i) + bload2
                cb_load(5,i) = cb_load(5,i) + gload2
                cb_load(6,i) = cb_load(6,i) + bload2
              endif
              ptr = bctbl_nxt(ptr)
            enddo

            num_cb_type = 1
            cb_type_old = char(0)
            do i = 1, num_cb
              iptio = add_ptiq(cb_own(i))
              id = cb_type(i)
              if (cb_type(i) .eq. cb_type_old) then
                num_cb_type = num_cb_type + 1
              else
                num_cb_type = 1
                cb_type_old = cb_type(i)
              endif
              write (tempc, '(i1)') num_cb_type
              write (xbuf, 10030) pti_num(iptib), bus(nb), base(nb), 
     &          id // tempc, ' ', 1, (cb_load(j,i),j=1,6), 
     &          pti_anum(iptia), pti_znum(iptiz), 
     &          date_in, date_out, 0, 0, pti_onum(iptio)
              last = lastch (xbuf)
              status = write_ge_file (1, xbuf(1:last))
              total(1) = total(1) + 1
            enddo
          endif
        endif
  900   continue
      enddo

      write (*, 10090) total(1)
10090 format (' * Writing ', i5, ' load records to NETDAT file')

      write (xbuf, 10100) total(1)
10100 format ('load data  [', i5, ']           id   long_id_     st     
     & mw      mvar    mw_i    mvar_i  mw_z      mvar_z ar zone  date_in
     & date_out pid N own')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      xbuf = '[EOF]'
      last = lastch (xbuf)
      status = write_ge_file (1, xbuf(1:last))
c
c     "Rewind" temp file and write to netdata file
c
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
10110 format (' Total load records extracted:', i5)
      call prterx ('I', 1)
 
      ext_geld = total(1)

      return
      end
