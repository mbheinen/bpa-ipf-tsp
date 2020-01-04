C    %W% %G%
C****************************************************************
C
C   	File: ext_bus.f
C
C   	Purpose: Write bus, continuation bus, X-data, and PQ-curve
C                data onto saved NETWORK_DATA file savfil.
C                                                                      *
C       Input parameters:
C
C             savfil   - the logical unit opened
C             dialect  - a character string (BPA, WSCC, WSCC1, WSCC2,
C                        PTI) denoting the dialect of the WSCC record.
c             lenrec   - an integer 80 or 120 denoting the output 
C                        record size
c             ratcod   - a character string denoting extended 
c                        line ratings used (E- EXTENDED, N-NOMINAL, 
C                        or M-MINIMUM)
c             sections - a character string <null> or "PSEUDOBUSES"
c                        denoting how line sections are to be 
c                        represented
c             type_e   - a character string <null> or "SYMMETRIC"
c                        denoting how assymetric type E-branches are
c                        to be represented
C
C   	Author: Walt Powell            Date: 13 January 1993
C   	Called by: savenetd.f
C
C****************************************************************
C
      integer function ext_bus ( savfil, dialect, lenrec, ratcod,
     &                           sections, type_e )
 
      character*(*) dialect, ratcod, sections, type_e
      integer savfil, lenrec
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/com007.inc'
      include 'ipfinc/pqcurves.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/pseudo_b.inc'
      include 'ipfinc/pti_data.inc'

      common /code_ovfg/ code_ovfg
      character code_ovfg*3
c
c     Set code_ovfg = 'ON' to replace '****' with '+XEX' values
c
      character xbuf*120, cbtype*1, cbown*3, bctbl_fld(20,7)*10,
     &          ybuf*120, code*10
      integer total1, total2, total3, total4, total5, ptr, chkovflo, 
     &        status, bctbl_index(7), bctbl_format(7), firstxstr,
     &        oldptr, ptrx, fndpsptr, new_bsnm, bus_type
      real temp(20)
      logical flag, pseudo_sections, ior, type_e_branches
      complex*16 v(2), a(2), y(2,2), zxx, yxx, vxx

      data bctbl_index / 2, 3, 4, 5, 6, 11, 12 /
      data bctbl_format / 5, 5, 4, 4, 5, 5, 5 /
c
c     *******************************************************
c     set up pointers to any P/Q curve data
c
c     Store X-data pointer in busxdtptr() and P/Q curve
c     data pointer in buspqptr()
c
      ext_bus = 0
      num_pseudo = 0
      ntot_pseudo = 0
      pseudo_sections = (firstxstr(sections, 'PSEUDO*') .ne. 0 .and.
     &                   dialect .ne. 'WSCC1' .and.
     &                   dialect .ne. 'WSCC2')
      type_e_branches = (firstxstr(type_e, 'SYMMETRIC*') .ne. 0 .and.
     &                   dialect .ne. 'WSCC1' .and.
     &                   dialect .ne. 'WSCC2')

      do i = 1, HASHSIZE
        htable(i) = 0
      enddo

      do i = 1, MAX_PSEUDO
        nextptr(i) = 0
        pseudo(1,i) = 0
      enddo

      if (.not. pq_flag) then
         do nb = 1, ntot
            buspqptr(nb) = 0
         enddo

         do i = 1, numcurv
            ieq = pqbusptr(i)
            if (ieq .gt. 0) buspqptr(ieq) = i
         enddo
         pq_flag = .true.
      endif

      if (.not. xdt_flag) then
         do nb = 1, ntot
            busxdtptr(nb)  = 0
         enddo
         do i = 1, kxtot
            ptr = xdata(1,i)
            if (ptr .gt. 0) busxdtptr(ptr) = i
         enddo
         xdt_flag = .true.
      endif
 
C     Select default for calculation of %var support for remote control

      if ( dialect(1:3) .eq. 'BPA' ) then
         call bcdopt( 'DEFAULT',0.0 )
         code_ovfg = 'OFF'
      else if ( dialect(1:4) .eq. 'WSCC' ) then
         call bcdopt( 'CALCULATE',0.0 )
         code_ovfg = 'OFF'
      else if ( dialect(1:3) .eq. 'PTI' ) then
         call bcdopt( 'CALCULATE',0.0 )
         code_ovfg = 'ON'
      else
         code_ovfg = 'OFF'
      endif
 
      total1 = 0       ! Count of buses extracted
      total2 = 0       ! Count of + buses extracted
      total3 = 0       ! Count of X buses extracted
      total4 = 0       ! Count of PQ-curve data extracted
      total5 = 0       ! Count of Pseuod buses in passive node sections

      do ib = 1, ntot_alf
         nb = alf2inp(ib)
         bus_type = kbsdta(1,nb)
         flag = .false.
         if (dialect .eq. 'BPA' ) then
            call bcdbus(nb, xbuf)
            if (lenrec .eq. 80) then
               write (savfil, '(a)') xbuf(1:80)
            else
               kt = inp2opt(nb)
               vmag = dsqrt (e(kt) ** 2 + f(kt) ** 2)
               vang = 57.2957795 * atan2 (f(kt), e(kt))
               qk = qnetu(kt)
               call allocq (nb, qk, qgen, qgnmax, qgnmin, qld, totcap,
     &                      usecap, totrek, userek, unsked, qerr)
               write (xbuf(82:120), fmt='(f10.7, f12.6, 2f8.1)') 
     &           vmag, vang, qgen, userek+usecap
               write (savfil, '(a)') xbuf(1:120)
            endif
            total1 = total1 + 1
            ptr = kbsdta(15,nb)
            do while (ptr .gt. 0)
               call bcdcbs(ptr, xbuf)
c
c              Check overflow of + bus data fields
c
               if (index (xbuf(21:57), '****') .ne. 0) then
                  ybuf = xbuf
                  maxnum = 0
                  do i = 1, 7
                     j = bctbl_index(i)
                     status = chkovflo (bctbl(j,ptr), bctbl_format(i), 
     &                                  0, num, bctbl_fld(1,i))
                     do j = num+1, 20
                        bctbl_fld(j,i) = ' '
                     enddo
                     maxnum = max (num, maxnum)
                  enddo
c
c                 Create individual + records
c
                  do i = 1, maxnum
                     write (ybuf(5:6), '(i2)') i
                     ybuf(21:25) = bctbl_fld(i,1)
                     ybuf(26:30) = bctbl_fld(i,2)
                     ybuf(31:34) = bctbl_fld(i,3)
                     ybuf(35:38) = bctbl_fld(i,4)
                     ybuf(43:47) = bctbl_fld(i,5)
                     ybuf(48:52) = bctbl_fld(i,6)
                     ybuf(53:57) = bctbl_fld(i,7)
                     write (savfil, '(a)') ybuf(1:80)
                     total2 = total2 + 1
                  enddo
               else
                  write (savfil, '(a)') xbuf(1:80)
                  total2 = total2 + 1
               endif
               ptr = bctbl_nxt(ptr)
            enddo
            ptr = busxdtptr(nb)
            if (ptr .gt. 0) then
               call bcdxdt(ptr, xbuf)
               write (savfil, '(a)') xbuf(1:80)
               total3 = total3 + 1
            endif
            kpq = buspqptr(nb)
            if (kpq .gt. 0) then
               call bcdqpd(kpq, xbuf)
               write (savfil, '(a)') xbuf(1:lenrec)
               call bcdqxd(kpq, xbuf)
               write (savfil, '(a)') xbuf(1:lenrec)
               call bcdqnd(kpq, xbuf)
               write (savfil, '(a)') xbuf(1:lenrec)
               total4 = total4 + 1
            endif
        
         else 
c
c           Temporarily consolidate + bus quantities with bus
c           data, extract the aggregated bus record, and restore
c           the original bus record.
c
            do i = 1, 16
               temp(i) = busdta(i,nb)
            enddo
c
c           Consolidate  "+"  records onto bus record 
c           exception: 
c               +A INT   records are kept separate
c                        under the WSCC1 and WSCC2 options.
c
            flag = .false.
            ptr = kbsdta(15,nb)
            totsuscp = 0.0
            do while (ptr .gt. 0)
               call getchr (1, cbtype, kbctbl(8,ptr))
               call getchr (3, cbown, kbctbl(10,ptr))
               if ((dialect .eq. 'WSCC1' .or. dialect .eq. 'WSCC2')
     &              .and. cbtype .eq. 'A' 
     &              .and. cbown .eq. 'INT')  then
                  flag = .true.
               else         
                  if (cbtype .eq. 'A') then
                     totsuscp = totsuscp + bctbl(5,ptr)
                  endif
                  flag = .true.
                  do i = 2, 5
                     busdta(i+1,nb) = busdta(i+1,nb) + bctbl(i,ptr)
                  enddo
                  busdta(8,nb) = busdta(8,nb) + bctbl(6,ptr)
                  busdta(9,nb) = busdta(9,nb) + bctbl(11,ptr)
                  busdta(10,nb) = busdta(10,nb) + bctbl(12,ptr)
               endif
               ptr = bctbl_nxt(ptr)
            enddo
c
c           Implement WSCC variants into bus
c
            call bcdbus(nb, xbuf)
            call bus_edt (xbuf, dialect, totsuscp)
            if (dialect(1:4) .eq. 'WSCC') then
               xbuf(15:18) = pti_name(nb)
               if ((bus_type .eq. 8 .or. bus_type .eq. 11) .and.
     &              xbuf(66:73) .ne. ' ') then
                  basekv = ftn_atof (xbuf(74:77))
                  mb = kbsdta(13,nb)
                  if (mb .ne. 0) then
                    xbuf(74:77) = pti_name(mb)
                  endif
               else if (bus_type .eq. 5 .or. 
     &                 (bus_type .eq. 12 .and. xbuf(51:58) .ne. ' '))
     &           then
                  basekv = ftn_atof (xbuf(59:62))
                  mb = kbsdta(9,nb)
                  if (mb .ne. 0) then
                    xbuf(59:62) = pti_name(mb)
                  endif
               endif
            endif
            if (lenrec .eq. 80) then
               write (savfil, '(a)') xbuf(1:80)
            else
               kt = inp2opt(nb)
               vmag = dsqrt (e(kt) ** 2 + f(kt) ** 2)
               vang = 57.2957795 * atan2 (f(kt), e(kt))
               qk = qnetu(kt)
               call allocq (nb, qk, qgen, qgnmax, qgnmin, qld, totcap,
     &                      usecap, totrek, userek, unsked, qerr)
               write (xbuf(82:120), fmt='(f10.7, f12.6, 2f8.1)') 
     &           vmag, vang, qgen, userek+usecap
               write (savfil, '(a)') xbuf(1:120)
            endif
            do i = 1, 16
               busdta(i,nb) = temp(i)
            enddo
            if (flag) then
               ptr = kbsdta(15,nb)
               do while (ptr .gt. 0)
                  call getchr (1, cbtype, kbctbl(8,ptr))
                  call getchr (3, cbown, kbctbl(10,ptr))
                  if ((dialect .eq. 'WSCC1' .or. dialect .eq. 'WSCC2')
     &                .and. cbtype .eq. 'A' 
     &                .and. cbown .eq. 'INT')  then
                     call bcdcbs (ptr, xbuf)
                     xbuf(15:18) = pti_name(nb)
                     write (savfil, '(a)') xbuf(1:80)
                     total2 = total2 + 1
                  endif
                 ptr = bctbl_nxt(ptr)
               enddo
            endif

            ptr = busxdtptr(nb)
            if (dialect(1:4) .eq. 'WSCC' .and. kbsdta(1,nb) .ne. 11) 
     &        ptr = 0
            if (ptr .gt. 0) then
               call bcdxdt(ptr, xbuf)
               xbuf(15:18) = pti_name(nb)
               if (xbuf(21:28) .ne. ' ') then
                  basekv = ftn_atof (xbuf(29:32))
                  mb = xdata(2,ptr)
                  if (mb .ne. 0) then
                    xbuf(29:32) = pti_name(mb)
                  endif
               endif
               write (savfil, '(a)') xbuf(1:80)
               total3 = total3 + 1
            endif
         endif
      enddo
c
c     First pass - check for sections = 'PSEUDOBUSES' option
c
      if (pseudo_sections) then
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
     &                       kbrnch(15, nbr) .eq. 0) .or.
     &                      (brnch_ptr(ptr) .lt. 0 .and. 
     &                       kbrnch(15, nbr) .gt. 0)) 
                     num_sec = 0
                     kt = inp2opt(nb)                    
                     mt = inp2opt(ky(ptr))                    
c
c                    Compute 2-port quantities for equivalent 
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
     &                         ky(ptr) .eq. ky(oldptr) .and.
     &                         brid(ptr) .eq. brid(oldptr))
c
c                       Compute 2-port quantities for each section
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
     &                      brtype(ptr) .eq. 6) base2 = base(ky(ptr))
                        lastbus = num
                        nbr = iabs (brnch_ptr(ptr))
                        call getchr (3, cbown, kbrnch(3,nbr))
                        numpsu = fndpsptr (ptr)
                        if (numpsu .eq. 0) then
                           write ( errbuf(1), 10000) MAX_PSEUDO
10000                      format (' More than ', i4, 
     &  ' pseudo buses added using SECTIONS=PSEUDOBUSES option')
                           write ( errbuf(2), 10010) bus(nb), base(nb)
10010                      format (' Overflow occurred at bus ', a8, 
     &                       f6.1)
                           call prterx ('W', 2)
                           go to 900
                        else if (numpsu .lt. 0) then
c
c                          -numpsu flags a new entity at num_pseudo
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
     &                         ky(ptrx) .eq. ky(oldptr) .and.
     &                         brid(ptrx) .eq. brid(oldptr)) then
c
c                             Find unique pseudo-bus name
c
                              num = new_bsnm (bus(nb), base2, '&',
     &                                        ntot+ntot_pseudo+1)
                              if (num .eq. 0) go to 900
                              pti_name(num) = code (base2, 4, 0)
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
     &                              owner(lastbus) = cbown
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
                           go to 900
                        endif
                        ptr = brnch_nxt(ptr)
                        lastpseudo = numpsu
                     enddo
c
c                    Nullify pseudo-bus feature for single sections
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
      endif
c
c     Second pass - Replace assymmetric type E branches with type L
C     branches.
c
      if (type_e_branches) then
         do ib = 1, ntot_alf
            nb = alf2inp(ib)
            flag = .false.
            ptr = kbsdta(16,nb)
            do while (ptr .gt. 0)
               if (brtype(ptr) .eq. 1 .and.
     &             brnch_ptr(ptr) .gt. 0) then
                  nbr = iabs (brnch_ptr(ptr))
                  call getchr (3, cbown, kbrnch(3,nbr))

                  kt = inp2opt(nb)
                  mt = inp2opt(ky(ptr))
c
c                 Compute 2-port quantities for equivalent
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

                  num_sec = 0
                  kt = inp2opt(nb)                    
                  mt = inp2opt(ky(ptr))                    
                  base1 = base(nb)
                  base2 = base1

                  num = 0
                  oldptr = ptr
                  ptr = brnch_nxt(ptr)
                  do while (ptr .gt. 0 .and.
     &                      ky(ptr) .eq. ky(oldptr) .and.
     &                      brid(ptr) .eq. brid(oldptr))
                     nbr = iabs (brnch_ptr(ptr))
c
c                    Compute 2-port quantities for each section
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
                     if (brtype(ptr) .eq. 5 .or. brtype(ptr) .eq. 6)
     &                  base2 = base(ky(ptr))                    
                     if (brtype(ptr) .eq. 8 .and. 
     &                   brnch(8,nbr) .ne. brnch(10,nbr) .and.
     &                   brnch(5,nbr) .gt. 0.0002) then
                        numpsu = fndpsptr (ptr)
                        if (numpsu .eq. 0) then
                           write ( errbuf(1), 10000) MAX_PSEUDO
                           write ( errbuf(2), 10010) bus(nb), base(nb)
                           call prterx ('W', 2)
                           go to 900
                        else if (numpsu .lt. 0) then
c
c                          -numpsu flags a new entity at num_pseudo
c
                           numpsu = -numpsu
                           pseudo(2,numpsu) = 0
                        endif
                        b1 = brnch(8,nbr)
                        b2 = brnch(10,nbr)
                        zxx = cmplx (0.0, 0.0001)
                        yxx = cmplx (0.0, 0.5 * (amin1(b1, b2) -
     &                                           amax1(b1, b2)))
                        if ((b1 .gt. b2 .and. brnch_ptr(ptr) .gt. 0)
     &                 .or. (b1 .lt. b2 .and. brnch_ptr(ptr) .lt. 0))
     &                     then
                           pseudo(5,numpsu) = 2
                           nbx = pseudo(4,numpsu)
                           if (nbx .eq. 0) nbx = ky(ptr)
c
c                          Compute pseudo-bus voltage
c
                           y(2,1) = -cmplx (1.0, 0.0) / zxx 
                           y(2,2) = yxx - y(2,1)
                           vxx = (a(2) - v(2)*y(2,2)) / y(2,1)
                        else
                           pseudo(5,numpsu) = 1
                           nbx = pseudo(3,numpsu)
                           if (nbx .eq. 0) nbx = kx(ptr)
c
c                          Compute pseudo-bus voltage
c
                           y(1,2) = -cmplx (1.0, 0.0) / zxx 
                           y(1,1) = yxx - y(1,2)
                           vxx = (a(1) - v(1)*y(1,1)) / y(1,2)
                        endif
c
c                       Find unique pseudo-bus name
c
                        num = new_bsnm (bus(nbx), base2, '@',
     &                                  ntot+ntot_pseudo+1)
                        if (num .eq. 0) go to 900
                        pti_name(num) = code (base2, 4, 0)
                        ntot_pseudo = ntot_pseudo + 1
                        pseudo(6,numpsu) = num
                        kbsdta(1,num) = 1
                        do i = 2, 16
                           kbsdta(i,num) = 0
                        enddo
                        inp2opt(num) = num

                        e(num) = dreal (vxx)
                        f(num) = dimag (vxx)
                        ntypu(num) = 1
                        zone(num) = zone(nbx)
                        jarzn(num) = jarzn(nbx)
                        owner(num) = owner(nbx)
                     endif
                     ptr = brnch_nxt(ptr)
                  enddo
               else if (brtype(ptr) .eq. 8 .and.
     &                  brnch_ptr(ptr) .gt. 0) then
                  nbr = iabs (brnch_ptr(ptr))
                  if (brnch(8,nbr) .ne. brnch(10,nbr) .and.
     &                brnch(5,nbr) .gt. 0.0002) then
                     numpsu = fndpsptr (ptr)
                     if (numpsu .eq. 0) then
                        write ( errbuf(1), 10000) MAX_PSEUDO
                        write ( errbuf(2), 10010) bus(nb), base(nb)
                        call prterx ('W', 2)
                        go to 900
                     else if (numpsu .lt. 0) then
c
c                       -numpsu flags a new entity at num_pseudo
c
                        numpsu = -numpsu
                        pseudo(2,numpsu) = 0
                     endif
                     b1 = brnch(8,nbr)
                     b2 = brnch(10,nbr)
                     zxx = cmplx (0.0, 0.0001)
                     yxx = cmplx (0.0, 0.5 * (amin1(b1, b2) -
     &                                        amax1(b1, b2)))
c
c                    Compute 2-port quantities for equivalent
c
                     kt = inp2opt(nb)                    
                     mt = inp2opt(ky(ptr))                    
                     v(1) = dcmplx (e(kt), f(kt))
                     v(2) = dcmplx (e(mt), f(mt))
                     call pieqiv(ptr, y, kerr)
                     do i = 1, 2
                       a(i) = dcmplx(0.0,0.0)
                       do j = 1, 2
                         a(i) = a(i) + y(i,j) * v(j)
                       enddo
                     enddo
                     if (b1 .gt. b2) then
                        pseudo(5,numpsu) = 2
                        nbx = pseudo(4,numpsu)
                        if (nbx .eq. 0) nbx = ky(ptr)
c
c                       Compute pseudo-bus voltage
c
                        y(2,1) = -cmplx (1.0, 0.0) / zxx 
                        y(2,2) = yxx - y(2,1)
                        vxx = (a(2) - v(2)*y(2,2)) / y(2,1)
                     else
                        pseudo(5,numpsu) = 1
                        nbx = pseudo(3,numpsu)
                        if (nbx .eq. 0) nbx = kx(ptr)
c
c                       Compute pseudo-bus voltage
c
                        y(1,2) = -cmplx (1.0, 0.0) / zxx 
                        y(1,1) = yxx - y(1,2)
                        vxx = (a(1) - v(1)*y(1,1)) / y(1,2)
                     endif
c
c                    Find unique pseudo-bus name
c
                     num = new_bsnm (bus(nbx), base(nbx), '@',
     &                               ntot+ntot_pseudo+1)
                     if (num .eq. 0) go to 900
                     ntot_pseudo = ntot_pseudo + 1
                     pseudo(6,numpsu) = num
                     kbsdta(1,num) = 1
                     do i = 2, 16
                        kbsdta(i,num) = 0
                     enddo
                     inp2opt(num) = num
                     e(num) = dreal (vxx)
                     f(num) = dimag (vxx)
                     ntypu(num) = 1
                     zone(num) = zone(nbx)
                     jarzn(num) = jarzn(nbx)
                     owner(num) = owner(nbx)
                  endif
                  ptr = brnch_nxt(ptr)
               else
                  ptr = brnch_nxt(ptr)
               endif 
            enddo          
         enddo
      endif

      if (num_pseudo .gt. 0) then
         xbuf = '.'
         write (savfil, '(a)') xbuf(1:80)
         xbuf = '.  Pseudo-buses follow'
         write (savfil, '(a)') xbuf(1:80)
         xbuf = '.'
         write (savfil, '(a)') xbuf(1:80)
         do i = 1, num_pseudo
            last = 3
            do while (last .le. 6)
               nb = pseudo(last,i)
               if (nb .gt. ntot) then
                  call bcdbus(nb, xbuf)
                  xbuf(15:18) = pti_name(nb)
                  if (lenrec .eq. 80) then
                     write (savfil, '(a)') xbuf(1:80)
                  else
                     kt = inp2opt(nb)
                     vmag = dsqrt (e(kt) ** 2 + f(kt) ** 2)
                     vang = 57.2957795 * atan2 (f(kt), e(kt))
                     write (xbuf(82:120), fmt='(f10.7, 1x, f12.7)') 
     &                 vmag, vang
                     write (savfil, '(a)') xbuf(1:120)
                  endif
               endif
               if (last .eq. 3) then
                  last = 6
               else
                  last = 7
               endif
            enddo
         enddo
         total5 = num_pseudo
         xbuf = '.'
         write (savfil, '(a)') xbuf(1:80)
         xbuf = '.  End of Pseudo-buses'
         write (savfil, '(a)') xbuf(1:80)
         xbuf = '.'
         write (savfil, '(a)') xbuf(1:80)
      endif

  900 write ( errbuf(1), 910) total1, total2, total3, total4, total5
  910 format (' Total bus records extracted:', i5, ' B, ', i4, ' +, ', 
     & i4, ' QP ', i4, 1x, i4, ' and ', i4, ' Pseudobuses')
      call prterx ('I', 1)
 
      ext_bus = total1 + total2 + total3 + total4 + total5

      return
      end

