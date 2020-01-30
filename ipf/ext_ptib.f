C    @(#)ext_ptib.f	20.13 5/3/00
C****************************************************************  
C  
C     File: ext_ptib.f  
C  
C     Purpose: Routine to extract bus data in PTI format  
C  
C     Input parameters:  
C  
C             savfil   - the logical unit opened  
C             version  - "23" or "24"  
C  
C     Author: Walt Powell  Date: 21 May 1996  
C     Called by: saveptid.f  
C  
C****************************************************************  
      integer function ext_ptib (savfil, version, option)  
      integer savfil, version  
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
      include 'ipfinc/pseudo_b.inc'  
      include 'ipfinc/xdata.inc'  
      include 'ipfinc/branch.inc'  
      include 'ipfinc/tbx.inc'  
      include 'ipfinc/ordsta.inc'  
  
      common /bcdflg/ bldtbx, lowx, xlinmn, npctq, pctq(MAXBUS)  
      logical bldtbx, lowx, flag, ior, prt_names  
  
      integer fnd_ptib, status, error, ktemp(20), total1,   
     &        total2, total3, ptr, bus_type(16), oldptr, ptrx,   
     &        fndpsptr, new_bsnm, gtptinum, pti_type, add_ptiq,
     &        write_ge_file
      character code*10, xbuf*240, cbtype*1, cbown*3, cbyear*2,   
     &          base1c*4, base2c*4  
  
      complex*16 v(2), a(2), y(2,2)  
  
      real temp(26)  
      equivalence (temp, ktemp)  
  
      data bus_type / 1, 2, 3, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1 /  
      data pctq / MAXBUS * 100.0 /  
  
      prt_names = (option(4) .eq. 'Y' .or. option(4) .eq. 'y')  
  
      ext_ptib = 0        ! set default return status = successful  
      error = 0           ! initialize error count  
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
  
C     Calculate allocation of generator %VAr for remote control  
  
      call bcdopt( 'CALCULATE',0.0 )  
   
      total1 = 0       ! Count of buses extracted  
      total2 = 0       ! Count of loads extracted  
      total3 = 0       ! Count of generators extracted  
c  
c     First pass - generate any necessary PSEUDOBUSES  
c  
      do ib = 1, ntot_alf  
        nb = alf2inp(ib)  
        flag = .false.  
        ptr = kbsdta(16,nb)  
        do while (ptr .gt. 0)  
          if (brtype(ptr) .eq. 1) then  
            nbr = iabs (brnch_ptr(ptr))  
            call getchr (3, cbown, kbrnch(3,nbr))  
            if (brnch_ptr(ptr) .gt. 0) then  
              ior = ((brnch_ptr(ptr) .gt. 0 .and.   
     &                kbrnch(15, nbr) .eq. 2) .or.  
     &               (brnch_ptr(ptr) .lt. 0 .and.   
     &                kbrnch(15, nbr) .eq. 1) .or.
     &               (brnch_ptr(ptr) .gt. 0 .and.
     &                kbrnch(15, nbr) .eq. 0))

               num_sec = 0  
               kt = inp2opt(nb)                      
               mt = inp2opt(ky(ptr))                      
c  
c              Compute 2-port quantities for equivalent   
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
               num = 0  
               lastpseudo = 0  
               base1 = base(nb)  
               base2 = base1  
               oldptr = ptr  
               ptr = brnch_nxt(ptr)  
               do while (ptr .gt. 0 .and.  
     &                   ky(ptr) .eq. ky(oldptr) .and.  
     &                   brid(ptr) .eq. brid(oldptr))  
c  
c                Compute 2-port quantities for each section  
c  
                 num_sec = num_sec + 1  
                 call pieqiv(ptr, y, kerr)  
  
                 if (num_sec .gt. 1) then  
                   v(1) = v(2)  
                   a(1) = -a(2)  
                 endif  
                 v(2) = (a(1) - y(1,1) * v(1)) / y(1,2)  
                 a(2) = y(2,1) * v(1) + y(2,2) * v(2)  
                         
                 base1 = base2  
                 if (brtype(ptr) .eq. 5 .or.   
     &               brtype(ptr) .eq. 6) base2 = base(ky(ptr))  
                   lastbus = num  
                 nbr = iabs (brnch_ptr(ptr))  
                 call getchr (3, cbown, kbrnch(3,nbr))  
                 numpsu = fndpsptr (ptr)  
                 if (numpsu .eq. 0) then  
                   write ( errbuf(1), 10000) MAX_PSEUDO  
10000              format (' More than ', i4,   
     &  ' pseudo buses added using PSEUDOBUSES option')  
                   write ( errbuf(2), 10010) bus(nb), base(nb)  
10010              format (' Overflow occurred at bus ', a8,   
     &               f6.1)  
                   call prterx ('W', 2)  
                   error = 2  
                   go to 900  
                 else if (numpsu .lt. 0) then  
c  
c                  -numpsu flags a new entity at num_pseudo  
c  
                   numpsu = -numpsu  
                   if (num_sec .eq. 1) then  
                     pseudo(2,numpsu) = 1  
                     pseudo(3,numpsu) = kx(ptr)  
                   else  
                     pseudo(2,numpsu) = 2  
                     pseudo(3,numpsu) = pseudo(4,lastpseudo)  
                   endif  
                   ptrx = brnch_nxt(ptr)  
                   if (ptrx .gt. 0 .and.  
     &                 ky(ptrx) .eq. ky(oldptr) .and.  
     &                 brid(ptrx) .eq. brid(oldptr)) then  
c  
c                    Find unique pseudo-bus name  
c  
                     num = new_bsnm (bus(nb), base2, '&',  
     &                               ntot+ntot_pseudo+1)  
                     if (num .eq. 0) then  
                       error = 3  
                       go to 900  
                     endif  
                     ntot_pseudo = ntot_pseudo + 1  
                     pseudo(4,numpsu) = num  
                     kbsdta(1,num) = 1  
                     do i = 2, 16  
                       kbsdta(i,num) = 0  
                     enddo  
                     inp2opt(num) = num  
                     e(num) = dreal (v(2))  
                     f(num) = dimag (v(2))  
                     ntypu(num) = 1  
                     if (ior) then  
                       zone(num) = zone(ky(ptr))  
                       jarzn(num) = jarzn(ky(ptr))  
                       owner(num) = cbown  
                     else  
                       zone(num) = zone(nb)  
                       jarzn(num) = jarzn(nb)  
                       if (lastbus .gt. 0)   
     &                     owner(lastbus) = cbown  
                     endif  
                   else  
                     pseudo(2,numpsu) = 3  
                     pseudo(4,numpsu) = ky(ptr)  
                     if (.not. ior .and. lastbus .gt. 0) then  
                       owner(lastbus) = cbown  
                     endif  
                   endif         
                 else  
                   write ( errbuf(1), 10000) MAX_PSEUDO  
                   write ( errbuf(2), 10010) bus(nb), base(nb)  
                   call prterx ('W', 2)  
                   error = 4  
                   go to 900  
                 endif  
                 ptr = brnch_nxt(ptr)  
                 lastpseudo = numpsu  
               enddo  
c  
c              Nullify pseudo-bus feature for single sections  
c  
               if (num_sec .eq. 1) then  
                 pseudo(2,numpsu) = 0  
               endif  
             else  
               ptr = brnch_nxt(ptr)  
             endif   
           else  
             ptr = brnch_nxt(ptr)  
           endif  
        enddo            
      enddo  
c  
c     Second pass process bus records  
c  
      do ib = 1, ntot_alf  
        nb = alf2inp(ib)  
        call gtptibsv (nb, temp, version, pti_type, qgen)  
        ntype = temp(1)  
        kt = inp2opt(nb)  
        vm = dsqrt (e(kt) ** 2 + f(kt) ** 2)  
        vmsq = vm * vm  
        va = 57.29577 * datan2 (f(kt), e(kt))  
        flag = .false.  
        bfixed = temp(17)  
        badj = temp(18)  
  
c       Bus V limits contain default values for "BS" buses.  Replace   
c       with solved (normally setpoint) value to obtain proper V reg  
c       in PTI generator record:  
  
        if (ntype .eq. 3)  then  
          vlimx(kt) = vm  
          vlimn(kt) = vm  
        endif  
c  
c       Get PTI area, zone, and bus number  
c  
        status = gtptinum (nb, iptia, iptiz, iptib)  
        if (status .ne. 0) then  
           error = 5  
           go to 900  
        endif  
        iptio = add_ptiq(owner(nb))
  
c KARL       write(errbuf(1), 99) bus(nb),base(nb),iptib,temp(6),  
c KARL    &                       temp(9),temp(10),temp(17),temp(18)  
c KARL  99   format(a8, 1x, f6.2, i5, ": temp(6,9,10,17,18)= ", 5f7.1)  
c KARL       write (dbug, '(a)')  errbuf(1)          
c  
c       Calculate continuous shunt (if present) for BQ buses     
c  
        if (ntype .eq. 7 .and. badj .ne. 0.0) then  
          qk = qnetu(kt)            
          call allocq (nb, qk, qgen, qgnmax, qgnmin, qld, totcap,  
     &                 usecap, totrek, userek, unsked, qerr)  
          kxd = busxdtptr(nb)  
  
c KARL        write(errbuf(1), 99) bus(nb),base(nb),iptib,temp(6),  
c KARL     &                       temp(9),temp(10),temp(17),temp(18)  
c KARL   99   format(a8, 1x, f6.2, i5, ": temp(6,9,10,17,18)= ", 5f7.1)  
c KARL        write (dbug, '(a)')  errbuf(1)       
c KARL        write (errbuf(1), 101) totrek, totcap, userek, usecap, bfixed  
c KARL  101   format ('totr, totc, user, usec, bfixed  = ', 5f9.1)    
c KARL        write (dbug, '(a)')  errbuf(1)  
  
          if (kxd .ne. 0)  then  
c           Update existing switched shunt data record            
            xdata(3,kxd) = (totrek / vmsq) - amin1 (0.0, bfixed)  
            xdata(4,kxd) = (totcap / vmsq) - amax1 (0.0, bfixed)  
            xdata(5,kxd) = (userek / vmsq) - amin1 (0.0, bfixed)  
            xdata(6,kxd) = (usecap / vmsq) - amax1 (0.0, bfixed)  
          else  
c           Create a switched shunt data record (pseudo x-data entity)   
            kxtot=kxtot+1  
            if (kxtot .ge. MAXXDT) then   
              write (errbuf(1), 10012) MAXXDT   
10012         format(' More than ', i3, ' switched reactance entities.')   
              errbuf(2) = ' (' // xbuf(1:60) // ')'   
              call prterx ('W',2)   
              error = 1   
              kxtot=1   
              go to 900   
            endif   
            busxdtptr(nb) = kxtot  
            xdata(1,kxtot) = nb  
            xdata(2,kxtot) = nb  
            xdata(3,kxtot) = (totrek / vmsq) - amin1 (0.0, bfixed)  
            xdata(4,kxtot) = (totcap / vmsq) - amax1 (0.0, bfixed)  
            xdata(5,kxtot) = (userek / vmsq) - amin1 (0.0, bfixed)  
            xdata(6,kxtot) = (usecap / vmsq) - amax1 (0.0, bfixed)  
            xdata(7,kxtot) = 1.  
            xdata(8,kxtot) = badj   
            do i = 2, 8   
              j = 2*i + 5   
              xdata(j,kxtot) = 0.  
              xdata(j+1,kxtot) = 0.0  
            enddo   
            temp(6) = bfixed   
          endif  
        endif  
  
        if (ntype .eq. 5 .or. ntype .eq. 12)  then    
c         Replace complex voltage at DC side of commutating xfmr with   
c         voltage at AC side.  The xfmr is represented as part of DC  
c         data in PSS/E, so its representation in AC branch data will  
c         be as a near-zero-impedance line in routine "ext_ptil".   
  
c**        (Preserve the actual DC-side voltage in a bogus isolated bus.)  
c**          if (version .le. 23) then  
c**            write (xbuf, 10030) pti_num(iptib), 4, temp(3),  
c**     &        temp(4), temp(5), temp(6), pti_anum(iptia), vm, va,  
c**     &        bus(nb), 0.00, pti_znum(iptiz)  
c**          else  
c**            write (xbuf, 10040) pti_num(iptib), bus(nb), 0.00,  
c**     &        4, temp(5), temp(6), pti_anum(iptia),  
c**     &        pti_znum(iptiz), vm, va  
c**          endif  
c**          last = lastch (xbuf)  
c**          status = write_ge_file (0, xbuf(1:last))
  
          ptr = kbsdta(16,nb)  
          do while (ptr .gt. 0)  
            if (brtype(ptr) .eq. 5)  then  
              nb_ac = ky(ptr)  
              kt_ac = inp2opt(nb_ac)  
              vm = dsqrt (e(kt_ac) ** 2 + f(kt_ac) ** 2)  
              va = 57.29577 * datan2 (f(kt_ac), e(kt_ac))  
            endif  
            ptr = brnch_nxt(ptr)  
          enddo  
        endif  
          
        if (version .le. 23) then  
          write (xbuf, 10030) pti_num(iptib), pti_type, temp(3),   
     &      temp(4), temp(5), temp(6), pti_anum(iptia), vm, va,  
     &      bus(nb), base(nb), pti_znum(iptiz)
10030     format (i5, i2, 2x, 4f10.3, i4, f8.4, f8.2, 2x,   
     &            '''', a, '''', f7.2, i4)       
        else  
          write (xbuf, 10040) pti_num(iptib), bus(nb), base(nb),   
     &      pti_type, temp(5), temp(6), pti_anum(iptia),   
     &      pti_znum(iptiz), vm, va, pti_onum(iptio)
10040     format (i5, 1x, '''', a, '''', f7.2, i3, 1x, 2f10.3, 2i4,   
     &            f8.4, f8.2, 1x, i4)  
        endif  

        if (prt_names) then
          last = lastch (xbuf)
          write (xbuf(last+1:), 10042) pti_znam(iptiz), 
     &      pti_onam(iptio) 
10042     format (' / ', a, 1x, a)
        endif
        last = lastch (xbuf)  
        status = write_ge_file (0, xbuf(1:last))
        total1 = total1 + 1  
  
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
               status = gtptinum (nb, iptia, iptiz, iptib)  
               if (status .ne. 0) then  
                 error = 6  
                 go to 900  
               endif  
  
               iptio = add_ptiq(owner(nb))

               ntype = kbsdta(1,nb)  
               if (version .le. 23) then  
                 write (xbuf, 10050) pti_num(iptib), bus_type(ntype),   
     &                  0., 0., 0., 0., pti_anum(iptia), vm, va,   
     &                  bus(nb), base(nb), pti_znum(iptiz)  
10050            format (i5, i2, 2x, 4f10.0, i4, f8.4, f8.2, 2x,  
     &                   '''', a, '''', f7.2, i4)       
                 last = lastch (xbuf)  
                 status = write_ge_file (0, xbuf(1:last))
                 total1 = total1 + 1  
               else  
                 write (xbuf, 10060) pti_num(iptib), bus(nb),   
     &             base(nb), bus_type(ntype), 0., 0.,  
     &             pti_anum(iptia), pti_znum(iptiz), vm, va, 
     &             pti_onum(iptio)  
10060            format (i5, 1x, '''', a, '''', f7.2, i3, 1x, 2f10.0,   
     &                   2i4, f8.4, f8.2, 1x, i4)  
                 last = lastch (xbuf)  
                 status = write_ge_file (0, xbuf(1:last))
                 total1 = total1 + 1  
               endif  
               if (prt_names) then
                 last = lastch (xbuf)
                 write (xbuf(last+1:), 10042) pti_znam(iptiz),
     &             pti_onam(iptio)
               endif
             endif  
             if (last_pseudo .eq. 3) then  
               last_pseudo = 6  
             else  
               last_pseudo = 7  
             endif  
          enddo  
        enddo  
      endif  
  
      xbuf = ' 0 / Begin Load Data'
      last = lastch (xbuf)  
      status = write_ge_file (0, xbuf(1:last))
c  
c     Third pass - process load records (version 24 or later)  
c  
      if (version .gt. 23) then  
        do ib = 1, ntot_alf  
          nb = alf2inp(ib)  
c  
c         Get PTI area, zone, and bus number  
c  
          status = gtptinum (nb, iptia, iptiz, iptib)  
          if (status .ne. 0) then  
            error = 7  
            go to 900  
          endif  

          iptio = add_ptiq(owner(nb))

          numload = 1  
          call gtptibsv (nb, temp, version, pti_type, bsh2qg)  
          ntype = temp(1)  
          if (temp(19) .ne. 0.0 .or. temp(20) .ne. 0.0) then  
            base1c = code (base(nb), 4, 0)  
  
            write (xbuf, 10070) pti_num(iptib), numload, 1,  
     &        pti_anum(iptia), pti_znum(iptiz), temp(19), temp(20),  
     &        0.0, 0.0, 0.0, 0.0, pti_onum(iptio)  
10070       format (i5, i3, 3i4, 6f8.1, i5)  
            if (prt_names)  then  
              last = lastch (xbuf)
              write (xbuf(last+1:), 10072)  bus(nb), base1c, 
     &          pti_znam(iptiz), pti_onam(iptio)
10072         format (' / ', a, 1x, a, 1x, a, 1x, a)  
            endif
            last = lastch (xbuf)  
            status = write_ge_file (0, xbuf(1:last))
            total2 = total2 + 1  
          endif  
  
          ptr = kbsdta(15,nb)  
          do while (ptr .gt. 0)  
            call getchr(1, cbtype, kbctbl(8, ptr))  
            call getchr(2, cbyear, kbctbl(9, ptr))  
            call getchr(3, cbown, kbctbl(10, ptr))  

            iptio = add_ptiq(cbown)

            pload2 = 0.0  
            qload2 = 0.0  
            aload2 = 0.0  
            bload2 = 0.0  
            skcon2 = 0.0  
            sksus2 = 0.0  
            if (cbtype .eq. 'A') then  
              if (cbyear .eq. '01' .or. cbyear .eq. '*I') then  
                aload2 = bctbl(2, ptr)  
                bload2 = bctbl(3, ptr)  
                skcon2 = bctbl(4, ptr)  
                sksus2 = bctbl(5, ptr)  
              elseif (cbyear .eq. '02' .or. cbyear .eq. '*P') then  
                pload2 = bctbl(2, ptr)  
                qload2 = bctbl(3, ptr)  
                skcon2 = bctbl(4, ptr)  
                sksus2 = bctbl(5, ptr)  
              elseif (cbyear .eq. '*Z') then  
                pload2 = bctbl(2, ptr)  
                qload2 = bctbl(3, ptr)  
                skcon2 = bctbl(4, ptr)  
                sksus2 = bctbl(5, ptr)  
              else  
                pload2 = bctbl(2, ptr)  
                qload2 = bctbl(3, ptr)  
              endif  
            elseif (cbyear .eq. '*I') then  
              aload2 = bctbl(2, ptr)  
              bload2 = bctbl(3, ptr)  
              skcon2 = bctbl(4, ptr)  
              sksus2 = bctbl(5, ptr)  
            elseif (cbyear .eq. '*P') then  
              pload2 = bctbl(2, ptr)  
              qload2 = bctbl(3, ptr)  
              skcon2 = bctbl(4, ptr)  
              sksus2 = bctbl(5, ptr)  
            else  
              pload2 = bctbl(2, ptr)  
              qload2 = bctbl(3, ptr)  
            endif  
            if (pload2 .ne. 0.0 .or. qload2 .ne. 0.0 .or.  
     &          aload2 .ne. 0.0 .or. bload2 .ne. 0.0 .or.   
     &          skcon2 .ne. 0.0 .or. sksus2 .ne. 0.0) then  
              numload = numload + 1  
  
              write (xbuf, 10070) pti_num(iptib), numload, 1,  
     &          pti_anum(iptia), pti_znum(iptiz), pload2, qload2,  
     &          aload2, bload2, skcon2, sksus2, pti_onum(iptio)
              if (prt_names)  then  
                last = lastch (xbuf)
                write (xbuf(last+1:), 10072)  bus(nb), base1c, 
     &            pti_znam(iptiz), pti_onam(iptio)
              endif
  
              last = lastch (xbuf)  
              status = write_ge_file (0, xbuf(1:last))
              total2 = total2 + 1  
            endif  
            ptr = bctbl_nxt(ptr)  
          enddo  
        enddo  
  
        xbuf = ' 0 / Begin Generator Data'
        last = lastch (xbuf)  
        status = write_ge_file (0, xbuf(1:last))
  
      endif  
c  
c     Fourth pass - process generator records  
c  
      do ib = 1, ntot_alf  
        nb = alf2inp(ib)  
        ntype = kbsdta(1,nb)  
        kt = inp2opt(nb)  
        vk = dsqrt (e(kt) ** 2 + f(kt) ** 2)  
        qgen = 0.0  
        call gtptibsv (nb, temp, version, pti_type, qgen)  
  
        iptio = add_ptiq(owner(nb))

        ntype = temp(1)  
        if (abs (temp(8)) .gt. 0.05 .or.   
     &      abs (temp(9)) .gt. 0.05 .or.  
     &      temp(9) - temp(10) .gt. 0.05) then  
          base1c = code (base(nb), 4, 0)  
          iptib = fnd_ptib (bus(nb), base(nb), arcnam(jarzn(nb)))  
  
          iptio = add_ptiq(owner(nb))

          mb = 0  
          if (ntype .eq. 8) then  
            ptr = ptrtbx(nb)  
            mb = tbx(8,ptr)  
          endif  
  
          if ((ntype .eq. 8) .and. (mb .gt. 0)) then  
c           Write record for remote V-controlling generator:  
   
            mt = inp2opt(mb)  
            vm = 0.5 * (vlimx(mt) + vlimn(mt))  
            iptir = fnd_ptib (bus(mb), base(mb), arcnam(jarzn(mb)))  
            if (iptir .eq. 0) then
              k3x = 0
            else
              k3x = pti_num(iptir)
            endif
            if (bldtbx) then  
              pct = pctq(nb)  
            else  
              pct = tbx(5,ptr)  
            endif  
  
            if (version .le. 23) then
              write (xbuf, 10080) pti_num(iptib), 1, temp(8), qgen,  
     &          temp(9), temp(10), vm, k3x, bmva, 0., 1., 0., 
     &          0., 1., 1, pct, busdta(7,nb), 0.0
10080         format (i5, i2, 4f8.1, f8.4, i6, f7.1, 5(2x,f2.0), i3,
     &          f7.1, 2f8.1)  
            else
              write (xbuf, 10086) pti_num(iptib), 1, temp(8), qgen,  
     &          temp(9), temp(10), vm, k3x, bmva,  
     &          0., 1., 0., 0., 1., 1, pct, busdta(7,nb), 0.0, 
     &          pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 0, 1.0
10086         format (i5, i2, 4f8.1, f8.4, i6, f7.1, 5(2x,f2.0), i3,
     &          f7.1, 2f7.1, 1x, i3, f4.1, 3(i2, f4.1))  
            endif  
  
          else  
c           Write record for local V-controlling generator:  
  
            vm = 0.5 * (vlimx(kt) + vlimn(kt))  
            iptir = 0
            k3x = 0  
            pct = 100.0  
            if (version .le. 23) then
              write (xbuf, 10080) pti_num(iptib), 1, temp(8), qgen,  
     &               temp(9), temp(10), vm, k3x, bmva,   
     &               0., 1., 0., 0., 1., 1, pct, busdta(7,nb), 0.0
            else
              write (xbuf, 10086) pti_num(iptib), 1, temp(8), qgen,  
     &               temp(9), temp(10), vm, k3x, bmva,  
     &               0., 1., 0., 0., 1., 1, pct, busdta(7,nb), 0.0, 
     &               pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 0, 0.0
            endif
          endif  
          last = lastch (xbuf)  
  
c         Always print Pmax, and print Pmin as 0.0 to avoid use of    
c         unrealistic PSS/E defaults: +9999 and -9999 MW   

  
          last = lastch (xbuf)  
          write (xbuf(last+1:), 10090) temp(7), 0.0  
10090     format (f8.1, f5.1)  
  
          if (prt_names)  then  
            if (ntype .eq. 8 .and. mb .gt. 0) then  
              last = lastch (xbuf)  
              base2c = code (base(mb), 4, 0)  
              write (xbuf(last+1:), 10100) bus(nb), base1c, 
     &          pti_znam(iptiz), pti_onam(iptio), bus(mb),  
     &          base2c  
10100         format (' / ', a, 1x, a, 1x, a, 1x, a, 
     &          ': controlling ', a, 1x, a)  
            else  
              last = lastch (xbuf)  
              write (xbuf(last+1:), 10110) bus(nb), base1c,
     &          pti_znam(iptiz), pti_onam(iptio)
10110         format (' / ', a, 1x, a, 1x, a, 1x, a)  
            endif  
          endif  
  
          last = lastch (xbuf)  
          status = write_ge_file (0, xbuf(1:last))
          total3 = total3 + 1  
        endif  
      enddo  
  
      xbuf = ' 0 / Begin Branch data'
      last = lastch (xbuf)  
      status = write_ge_file (0, xbuf(1:last))
  
  900 continue  
  
      write ( errbuf(1), 10120) total1  
10120 format (' Total bus records extracted:', t45, i5)  
      write ( errbuf(2), 10130) total2  
10130 format (' Total load records extracted:', t45, i5)  
      write ( errbuf(3), 10140) total3  
10140 format (' Total generator records extracted:', t45, i5)  
      call prterx ('I', 3)  
   
      return  
      end  
