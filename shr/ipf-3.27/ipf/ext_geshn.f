C    @(#)ext_geshn.f	20.12 8/30/00
C****************************************************************
C
C     File: ext_geshn.f
C
C     Purpose: Routine to extract shunt data in GE format
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
      integer function ext_geshn (scrfil, version, option, total)
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
 
      integer max_out_of_service
      parameter (MAX_OUT_OF_SERVICE = 200)
      common /out_of_service/ numoossh, shunt_status(MAXCBS),
     &                        shunt_value(16,MAX_OUT_OF_SERVICE), 
     &                        branch_status(MAXBRN)
      integer numoossh, shunt_status, branch_status
      real shunt_value

      logical finished
      integer add_ptiq, status, error, bus_type(16), gtge_num, 
     &        ptr, close_ge_file, write_ge_file, read_ge_file,
     &        open_ge_file
      character xbuf*256, date_in*6, date_out*6, id*2, cbtype*1,
     &          cbyear*2, cbown*3, bus2c*8, base2c*8

      data bus_type / 1, 2, 3, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1 /
      data date_in, date_out / '400101', '391231' /

      ext_geshn = 0       ! set default return status = successful
      error = 0           ! initialize error count
c
c     Note: all line shunt data already has been written to (3)
c
      do ib = 1, ntot_alf
        nb = alf2inp(ib)
        ntype = kbsdta(1,nb)
        ktbx = ptrtbx(nb)
        kxd = busxdtptr(nb)

        if (ntype .eq. 5 .or. ntype .eq. 12) then
          gfixed = 0.0
          bfixed = 0.0
          bshunt = 0.0
        else
          gfixed = busdta(5,nb)
          bfixed = 0.0
          bshunt = busdta(6,nb)
        endif

        if ((ntype .eq. 1 .or. ntype .eq. 4 .or. 
     &       ntype .eq. 6 .or. ntype .eq. 10) .and. kxd .gt. 0) then
          bfixed = 0.0
        else if (ntype .eq. 1 .or. ntype .eq. 4 .or. 
     &      ntype .eq. 8 .or. ntype .eq. 10) then
          bfixed = bshunt
        else if (kxd .gt. 0) then
          bshunt = dim (bshunt, sngl(xdata(4,kxd)))
     &           - dim (sngl(xdata(3,kxd)), bshunt) 
        else if (ntype .eq. 2 .or. ntype .eq. 7) then
c
c         Note: Any adjustable shunt on types BE or BQ buses will be 
c               processed as SVD.
c
        else if (ntype .eq. 5 .or. ntype .eq. 12) then
        endif
        iptib = 0
        if (gfixed .ne. 0.0 .or. bfixed .ne. 0.0) then
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

          iptio = add_ptiq(owner(nb))
          id = 'b '
          k2x = 0
          bus2c = '        '
          write (base2c, fmt='(f6.2)') 0.0

          write (xbuf, 10030) pti_num(iptib), bus(nb), base(nb), 
     &      id, k2x, bus2c, base2c, ' ', 0, ' ', 1, pti_anum(iptia), 
     &      pti_znum(iptiz), gfixed/bmva, bfixed/bmva, date_in, 
     &      date_out, 0, 0, pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 0, 
     &      0.0
10030     format (2x, i5, 1x, '"', a, '"', 1x, f6.2, 1x, '"', a, 
     &      '"', 3x, i5, 1x, '"', a, '"', 1x, a, 1x, '"', a, '"', 
     &      1x, i2, 1x, '"', a, '"', 1x, ':', 1x, i1, 1x, i2, 1x, i4, 
     &      2(1x,f8.4), 2(3x, a), 2x, i1, 2x, i1, 1x, 
     &      4(1x, i3, 1x, f5.3))
          last = lastch (xbuf)
          status = write_ge_file (3, xbuf(1:last))
          total(3) = total(3) + 1
        endif
        ptr = kbsdta(15,nb)
        iyear = ichar('1')
        do while (ptr .gt. 0)
          call getchr(1, cbtype, kbctbl(8, ptr))
          call getchr(2, cbyear, kbctbl(9, ptr))
          call getchr(3, cbown, kbctbl(10, ptr))
          skcon2 = 0.0
          sksus2 = 0.0
          if (cbtype .eq. 'A') then
            if (cbyear .ne. '*Z') then
              skcon2 = bctbl(4, ptr)
              sksus2 = bctbl(5, ptr)
            endif
          else
            skcon2 = bctbl(4, ptr)
            sksus2 = bctbl(5, ptr)
          endif
          if (skcon2 .ne. 0.0 .or. sksus2 .ne. 0.0 .or.
     &        shunt_status(ptr) .eq. 0) then
            iptio = add_ptiq(cbown)
            id = 'b' // cbyear(2:2)
            if (id(2:2) .eq. ' ') id = 'b' // cbtype
            if (iptib .eq. 0) then
c
c             Get GE area, zone, owner, and bus number
c
              status = gtge_num (nb, iptia, iptiz, iptib)
              if (status .ne. 0) then
                error = 5
                write ( errbuf(1), 10050) bus(nb), base(nb) 
                call prterx ('E', 1)
                go to 900
              endif
            endif
            k2x = 0
            bus2c = '        '
            write (base2c, fmt='(f6.2)') 0.0
            if (shunt_status(ptr) .ne. 0) then
              write (xbuf, 10030) pti_num(iptib), bus(nb), base(nb), 
     &          id, k2x, bus2c, base2c, ' ', 0, ' ', shunt_status(ptr),
     &          pti_anum(iptia), 
     &          pti_znum(iptiz), skcon2/bmva, sksus2/bmva, date_in, 
     &          date_out, 0, 0, pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 0, 
     &          0.0
              last = lastch (xbuf)
              status = write_ge_file (3, xbuf(1:last))
              total(3) = total(3) + 1
            else
c
c             Special sub-loop for Out-of-service SVD's which must
c             be presented as individual shunts
c
              ish = kbctbl(1,ptr)
              i = 1
              id = 'b' // char(iyear)
              finished = (shunt_value(i,ish) .eq. 0.0)
              do while (.not. finished)
                skcon2 = 0.0
                sksus2 = shunt_value(i+1,ish)
                write (xbuf, 10030) pti_num(iptib), bus(nb), base(nb), 
     &            id, k2x, bus2c, base2c, ' ', 0, ' ', 
     &            shunt_status(ptr), pti_anum(iptia), 
     &            pti_znum(iptiz), skcon2/bmva, sksus2/bmva, date_in, 
     &            date_out, 0, 0, pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 
     &            0, 0.0
                last = lastch (xbuf)
                status = write_ge_file (3, xbuf(1:last))
                total(3) = total(3) + 1
c
c               Increment id
c
                iyear = iyear + 1
                if (iyear .gt. ichar('9') .and.
     &              iyear .lt. ichar('A')) iyeare = ichar('A')
                id = 'b' // char(iyear)
c
c               Decrement number of steps
c
                shunt_value(i,ish) = shunt_value(i,ish) - 1.0
                if (shunt_value(i,ish) .eq. 0.0) then
                  i = i + 2
                  if (i .gt. 15) then
                    finished = .true.
                  else if (shunt_value(i,ish) .eq. 0.0) then
                    finished = .true.
                  endif
                else if (shunt_value(i,ish) .eq. 0.0) then
                  finished = .true.
                endif
              enddo
            endif
          endif
          ptr = bctbl_nxt(ptr)
        enddo
  900   continue
      enddo

      write (*, 10090) total(3)
10090 format (' * Writing ', i5, ' shunt records to NETDAT file')

      write (xbuf, 10100) total(3)
10100 format ('shunt data  [', i5, ']
     &          id                               ck  se  long_id_  st ar
     & zone    pu_mw  pu_mvar  date_in date_out pid n own part1 own part
     &2 own part3 own part4')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))
c
c     "Rewind" temp file and write to netdata file
c
      xbuf = '[EOF]'
      last = lastch (xbuf)
      status = write_ge_file (3, xbuf(1:last))
      status = close_ge_file (3)
      status = open_ge_file (3, 'scratch3.dat', 'r')

      finished = .false.
      do while (.not. finished)
        last = read_ge_file (3, xbuf)
        if (last .eq. 0 .or. xbuf(1:5) .eq. '[EOF]') go to 120
        status = write_ge_file (0, xbuf(1:last))
      enddo

  120 continue
      write ( errbuf(1), 10110) total(3)
10110 format (' Total shunt records extracted:', i5)
      call prterx ('I', 1)
 
      ext_geshn = total(3)

      return
      end
