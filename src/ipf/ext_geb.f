C    @(#)ext_geb.f	20.11 2/28/00
C****************************************************************  
C  
C     File: ext_geb.f  
C  
C     Purpose: Routine to extract bus data in GE format  
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
      integer function ext_geb (scrfil, version, option, total)  
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
      include 'ipfinc/branch.inc'  
      include 'ipfinc/pseudo_b.inc'  
   
      integer add_ptiq, status, error, bus_type(16), gtge_num,   
     &        write_ge_file, total2, total3, total4, ptr,   
     &        oldptr, fndpsptr, new_bsnm, pti_type  
      character xbuf*256, date_in*6, date_out*6, code*10, cbtype*1,   
     &          cbown*3, cbyear*2, base1c*4, base2c*4  
      complex*16 v(2), a(2), y(2,2)  
      logical finished, flag, ior  
  
      data bus_type / 1, 2, 3, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1 /  
      data date_in, date_out / '400101', '391231' /  
  
      ext_geb = 0        ! set default return status = successful  
      error = 0           ! initialize error count  
      numpsu = 0  
      num_pseudo = 0  
      ntot_pseudo = 0  
c  
c     *******************************************************  
c     set up pointers to any P/Q curve data  
c  
c     Store X-data pointer in busxdtptr() and TBX pointer ptrtbx().  
c  
      if (.not. xdt_flag) then  
        do nb = 1, ntot  
          busxdtptr(nb)  = 0  
        enddo  
        do i = 1, kxtot  
          kxd = xdata(1,i)  
          if (kxd .gt. 0) busxdtptr(kxd) = i  
        enddo  
        xdt_flag = .true.  
      endif  
   
      do nb = 1, ntot  
        ptrtbx(nb) = 0  
      enddo  
  
      do i = 1, ntotb  
        kxd = tbx(2, i)  
        if (ordtbx .eq. 2) kxd = opt2inp(kxd)  
        if (kxd .gt. 0) ptrtbx(kxd) = i  
      enddo  
  
      tbx_loaded = ordtbx  
  
      do i = 1, MAX_PSEUDO  
        nextptr(i) = 0  
        pseudo(1,i) = 0  
      enddo  
  
C     Select default for calculation of %var support for remote control  
  
      call bcdopt( 'CALCULATE',0.0 )  
c  
c     First pass - generate any necessary PSEUDOBUSES  
c  
      do ib = 1, ntot_alf  
        nb = alf2inp(ib)  
        flag = .false.  
        ptr = kbsdta(16,nb)  
        do while (ptr .gt. 0)  
          if (brtype(ptr) .eq. 1 .and. brnch_ptr(ptr) .gt. 0) then  
            oldptr = ptr  
            ptr = brnch_nxt(ptr)  
            num_sec = 0  
            num_tx = 0  
c  
c           Determine whether a transformer exists within these  
c           sections and that more than one section exists.  
c  
            base2 = base(kx(ptr))  
            do while (ptr .gt. 0 .and.  
     &                ky(ptr) .eq. ky(oldptr) .and.  
     &                brid(ptr) .eq. brid(oldptr))  
              num_sec = num_sec + 1  
              if (brtype(ptr) .eq. 5 .or.   
     &            brtype(ptr) .eq. 6) then  
                num_tx = num_sec  
                if (num_tx .eq. 1) base2 = base(ky(ptr))  
              endif  
              ptr = brnch_nxt(ptr)  
            enddo  
            ptr = oldptr  
            if (num_tx .gt. 0 .and. num_sec .gt. 1) then  
              mb = ky(ptr)  
              numpsu = fndpsptr (ptr)  
              if (numpsu .eq. 0) then  
                write ( errbuf(1), 10000) MAX_PSEUDO  
10000           format (' More than ', i4,   
     &   ' pseudo buses added using PSEUDOBUSES option')  
                write ( errbuf(2), 10010) bus(nb), base(nb)  
10010           format (' Overflow occurred at bus ', a8, f6.1)  
                call prterx ('W', 2)  
                error = 2  
                go to 900  
              else if (numpsu .lt. 0) then  
c  
c               -numpsu flags a new entity at num_pseudo  
c  
                numpsu = -numpsu  
c  
c               derive a unique pseudo-bus name  
c  
                if (num_tx .eq. 1) then  
                  num = new_bsnm (bus(nb), base2, '&',  
     &                            ntot+ntot_pseudo+1)  
                else  
                  num = new_bsnm (bus(mb), base2, '&',  
     &                            ntot+ntot_pseudo+1)  
                endif  
                if (num .eq. 0) then  
                  error = 3  
                  go to 900  
                endif  
                ntot_pseudo = ntot_pseudo + 1  
                pseudo(2,numpsu) = num_tx  
                pseudo(3,numpsu) = num  
                pseudo(4,numpsu) = 0  
  
                kbsdta(1,num) = 1  
                do i = 2, 16  
                  kbsdta(i,num) = 0  
                enddo  
                inp2opt(num) = num  
                ntypu(num) = 1  
                nbr = iabs (brnch_ptr(ptr))  
                call getchr (3, cbown, kbrnch(3,nbr))  
                ior = ((brnch_ptr(ptr) .gt. 0 .and.   
     &                  kbrnch(15, nbr) .eq. 0) .or.  
     &                 (brnch_ptr(ptr) .lt. 0 .and.   
     &                  kbrnch(15, nbr) .gt. 0))   
                if (ior) then  
                  zone(num) = zone(ky(ptr))  
                  jarzn(num) = jarzn(ky(ptr))  
                  owner(num) = cbown  
                else  
                  zone(num) = zone(nb)  
                  jarzn(num) = jarzn(nb)  
                  owner(num) = owner(nb)  
                endif  
                kt = inp2opt(nb)                      
                mt = inp2opt(ky(ptr))                      
c  
c               Loop through sections to compute the 2-port   
c               quantities and the pseudo-bus's voltage  
c  
                v(1) = dcmplx (e(kt), f(kt))  
                v(2) = dcmplx (e(mt), f(mt))  
                call pieqiv(ptr, y, kerr)  
                do i = 1, 2  
                  a(i) = dcmplx(0.0,0.0)  
                  do j = 1, 2  
                    a(i) = a(i) + y(i,j) * v(j)  
                  enddo  
                enddo  
                ptr = brnch_nxt(ptr)  
                num_sec = 0  
                do while (ptr .gt. 0 .and.  
     &                    ky(ptr) .eq. ky(oldptr) .and.  
     &                    brid(ptr) .eq. brid(oldptr))  
c  
c                 Compute 2-port quantities for each section  
c  
                  call pieqiv(ptr, y, kerr)  
  
                  num_sec = num_sec + 1  
                  if (num_sec .gt. 1) then  
                    v(1) = v(2)  
                    a(1) = -a(2)  
                  endif  
                  v(2) = (a(1) - y(1,1) * v(1)) / y(1,2)  
                  a(2) = y(2,1) * v(1) + y(2,2) * v(2)  
                         
                  if (num_sec .eq. num_tx .and. num_sec .eq. 1) then  
                    e(num) = dreal (v(2))  
                    f(num) = dimag (v(2))  
                  else if (num_sec .eq. num_tx .and. num_sec .gt. 1)   
     &              then  
                    e(num) = dreal (v(1))  
                    f(num) = dimag (v(1))  
                  endif  
                  ptr = brnch_nxt(ptr)  
                enddo  
              else  
                write ( errbuf(1), 10000) MAX_PSEUDO  
                write ( errbuf(2), 10010) bus(nb), base(nb)  
                call prterx ('W', 2)  
                error = 4  
                go to 900  
              endif  
            else  
              if (ptr .gt. 0) ptr = brnch_nxt(ptr)  
            endif  
          else  
            if (ptr .gt. 0) ptr = brnch_nxt(ptr)  
          endif  
        enddo            
      enddo  
c  
c     Second pass - process bus records  
c                   itotal = Count of buses extracted  
c  
      itotal = ntot_alf - mtdcbs - 2 * kdtot + num_pseudo  
      write (*, 10002) itotal  
10002 format (' * Writing ', i5, ' bus records to NETDAT file')  
  
      write (xbuf, 10004) itotal  
10004 format ('bus data  [', i5, ']             ty  vsched   volt     an  
     &gle   ar zone  vmax   vmin   date_in date_out pid L own')  
      last = lastch (xbuf)  
      status = write_ge_file (0, xbuf(1:last))  
  
c     Process bus records (sans any d-c buses)  
  
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
            go to 900  
          endif  
          iptio = add_ptiq(owner(nb))  
  
          if (ntype .eq. 1) then  
            vsched = 1.0  
          else if (ntype .eq. 4 .or. ntype .eq. 6 .or.   
     &             ntype .eq. 10) then  
            ntype = 1  
            vsched = vlimx(kt)  
          else if (ntype .eq. 3) then  
            ntype = 0  
            vsched = vm  
          else if (ntype .eq. 11) then  
            ntype = 2  
            vsched = 0.5 * (vlimx(kt) + vlimn(kt))  
          else  
            ntype = 2  
            vsched = vlimx(kt)  
          endif  
          write (xbuf, 10030) pti_num(iptib), bus(nb), base(nb), ntype,  
     &      vsched, vm, va, pti_anum(iptia), pti_znum(iptiz),   
     &      0.0, 0.0, date_in, date_out, 0, 0, pti_onum(iptio)  
10030     format (3x, i5, 1x, '"', a8, '"', 1x, f6.2, 2x, ':', i3, 1x,  
     &      f7.5, 1x, f8.6, 1x, f10.5, 1x, i2, 1x, i4, 1x, f6.4, 1x,   
     &      f6.4, 3x, a6, 3x, a6, 1x, i3, i2, i4)  
          last = lastch (xbuf)  
          status = write_ge_file (0, xbuf(1:last))  
          total(1) = total(1) + 1  
  
        endif  
      enddo  
  
      if (num_pseudo .gt. 0) then  
        do i = 1, num_pseudo  
          last_pseudo = 3  
          do while (last_pseudo .le. 6)  
             nb = pseudo(last_pseudo,i)  
             if (nb .gt. ntot) then  
               kt = inp2opt(nb)  
               vm = dsqrt (e(kt) ** 2 + f(kt) ** 2)  
               va = 57.2957795 * datan2 (f(kt), e(kt))  
c  
c              Get PTI area, zone, and bus number  
c  
               status = gtge_num (nb, iptia, iptiz, iptib)  
               if (status .ne. 0) then  
                 error = 6  
                 go to 900  
               endif  
  
               iptio = add_ptiq(owner(nb))  
  
               if (ntype .eq. 1) then  
                 vsched = 1.0  
               else if (ntype .eq. 4 .or. ntype .eq. 6 .or.   
     &                  ntype .eq. 10) then  
                 ntype = 1  
                 vsched = vlimx(kt)  
               else if (ntype .eq. 3) then  
                 ntype = 0  
                 vsched = vm  
               else  
                 ntype = 2  
                 vsched = vlimx(kt)  
               endif  
               write (xbuf, 10030) pti_num(iptib), bus(nb), base(nb),   
     &           ntype, vsched, vm, va, pti_anum(iptia),   
     &           pti_znum(iptiz), 0.0, 0.0, date_in, date_out, 0, 0,   
     &           pti_onum(iptio)  
               last = lastch (xbuf)  
               status = write_ge_file (0, xbuf(1:last))  
               total(1) = total(1) + 1  
             endif  
             if (last_pseudo .eq. 3) then  
               last_pseudo = 6  
             else  
               last_pseudo = 7  
             endif  
          enddo  
        enddo  
      endif  
  
  900 continue  
  
      if (itotal .ne. total(1)) then  
        write ( errbuf(1), 10120) itotal, total(1)  
10120   format (' Discrepency in total bus records extracted: estimated   
     &=', i5, ' Actual = ', i5)  
        call prterx ('W', 1)  
      endif  
   
      return  
      end  
