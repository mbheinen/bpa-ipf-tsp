C   %W% %G%
C****************************************************************
C
C        File: ext_gel.f
C
C        Purpose: Routine to extract branch data in GE format
C
C        Input parameters:
C
C             savfil   - the logical unit opened
C             version  - "23" or "24"
C
C        Author: Walt Powell  Date: 21 May 1996
C        Called by: saveptid.f
C
C****************************************************************
         integer function ext_gel (savfil, version, option, total,
     &                             extfil, extfilename)
         integer savfil, version, total(4), extfil
         character *(*) option(10), extfilename
 
         include 'ipfinc:parametr.inc'

         include 'ipfinc:blank.inc'
         include 'ipfinc:area.inc'
         include 'ipfinc:arcntl.inc'
         include 'ipfinc:bus.inc'
         include 'ipfinc:branch.inc'
         include 'ipfinc:cbus.inc'
         include 'ipfinc:com007.inc'
         include 'ipfinc:prt.inc'
         include 'ipfinc:pseudo_b.inc'
         include 'ipfinc:tran.inc'
         include 'ipfinc:alpha.inc'
         include 'ipfinc:pti_data.inc'
         include 'ipfinc:ordsta.inc'
         include 'ipfinc:tx_misc.inc'

         common /branch_ratings/ ratings(8,MAXBRN),
     &                           ext_ratings(15,MAXBRN),
     &                           ext_date(2,MAXBRN),
     &                           numrat
         real ratings, ext_ratings
         character ext_date*6
         integer numrat

         integer MAX_OUT_OF_SERVICE
         parameter (MAX_OUT_OF_SERVICE = 200)
         common /out_of_service/ numoossh, shunt_status(MAXCBS),
     &                           shunt_value(16,MAX_OUT_OF_SERVICE), 
     &                           branch_status(MAXBRN)
         integer numoossh, shunt_status, branch_status
         real shunt_value

         character xbuf*512, lntype(9)*2, id*1, cbown*3, text*5,
     &             date_in*6, date_out*6, trtype*1, base3c*6, 
     &             base4c*6, base5c*6, bus3c*8, bus4c*8, bus5c*8,
     &             sh_type*2, tmaxc*8, tminc*8, vamaxc*8, vaminc*8,
     &             tempc(25)*8, tstepc*12, tapfpc*12, 
     &             def_date_in*6, def_date_out*6
         integer ptr, oldptr, ktemp(25), ftn_atoi, orienttx, 
     &           add_ptiq, fnd_ptia, fnd_ptiy, status, read_ge_file, 
     &           close_ge_file, write_ge_file, open_ge_file, 
     &           fndpsptr, sec, gettapfp, gtge_num
         real temp(25), zxterm(10,3)
         equivalence (temp, ktemp)
         logical print_flag, ior, finished
         complex a(4), b(4)

         data lntype / 'L*', 'LM', 'L ', 'R ', 'T ', 'TP', 'E ', 'LM',
     &                 'RZ' /
         data def_date_in, def_date_out / '400101', '391231' /

         ext_gel = 0
         call bcdopt ( 'LOWX',0.0000 )
         iseason = ftn_atoi(option(6))
c
c        Flag 3-terminal Tx's. 
c
         do ix = 1, num_3term
           k1 = tx_3term(1,ix)      ! from bus                    
           k2 = tx_3term(2,ix)      ! to bus    
           k3 = tx_3term(3,ix)      ! reg bus      
           k4 = tx_3term(4,ix)      ! 3-winding pt bus     
           k5 = tx_3term(5,ix)      ! tertiary bus
           i = tx_3term(6,ix)
           id = char(i)
c
c          Tx branches k1-k4, k2-k4, and k5-k4 are to be replaced
c          with one 3-winding transformer.
c
           k1x = k1
           k2x = k4
           loop = 1
           do while (loop .le. 6)
             ptr = numbrn (k1x, k2x, '*', 0)
             if (ptr .eq. 0) then
               write (errbuf(1), 10000) ix, bus(k1x), base(k1x), 
     &           bus(k2x), base(k2x), id
10000          format (' 3-winding entity 9', i2, ') is missing Tx (', 
     &           a, f7.1, 1x, a, f7.1, 1x, a, ')')
               call prterx ('W',1)
               error = 1
             else
               if (brtype(ptr) .eq. 4) then
                 brtype(ptr) = 14
               endif
               ptr = numbrn (k1x, k2x, id, 0)
               if (ptr .eq. 0) then
                 write (errbuf(1), 10000) ix, bus(k1x), base(k1x), 
     &             bus(k2x), base(k2x), id
                 call prterx ('W',1)
                 error = 1
               else
                 brtype(ptr) = brtype(ptr) + 10  
               endif
             endif                                
             if (loop .eq. 1) then
               k1x = k4
               k2x = k1
             elseif (loop .eq. 2) then
               k1x = k2
               k2x = k4
             else if (loop .eq. 3) then
               k1x = k4
               k2x = k2
             else if (loop .eq. 4) then
               k1x = k5
               k2x = k4
             else if (loop .eq. 5) then
               k1x = k4
               k2x = k5
             endif
             loop = loop + 1
           enddo
         enddo
C                                                                 
C        GE criteria for selection:  (low-hi/hi-low):
C                                                                  
C        Type           Criteria                            
C       
C        L,E,T,TP,LM,RZ Original submittal                
C        R              LTC side, fixed tap side
C        LD             Skip - process in d-c data
C                                                                 
c
c        First pass - process lines to scratch file.  "total(1)" is
c        not known apriori.
c
c        Open scratch file for branches
c
         status = close_ge_file (1)
         status = open_ge_file (1, 'scratch1.dat', 'w+')
c
c        Open scratch file for transformers
c
         status = close_ge_file (2)
         status = open_ge_file (2, 'scratch2.dat', 'w+')

         total(1) = 0                  ! Total number of lines
         total(2) = 0                  ! Total number of transformers
         total(3) = 0                  ! Total number of shunts

         do ib = 1, ntot_alf
           nb = alf2inp(ib)
           print_flag = .false.
           ptr = kbsdta(16,nb)
           if (option(2) .eq. 'Y') then
c
c            Rename all parallels '1', '2' if first parallel is blank
c
             lastky = 0
             do while (ptr .gt. 0)
               if (brid(ptr) .eq. ' ' .and. brtype(ptr) .ne. 4) then
                 oldptr = ptr
                 lastky = ky(ptr)
                 numpr = 0
                 id = brid(ptr)
                 do while (ptr .gt. 0 .and. ky(ptr) .eq. lastky)   
                   if (brtype(ptr) .eq. 1) then
                     do while (ptr .gt. 0 .and. 
     &                         ky(ptr) .eq. lastky .and.
     &                         brid(ptr) .eq. id)
                       ptr = brnch_nxt(ptr)
                     enddo
                     id = brid(ptr)
                     numpr = numpr + 1
                   else
                     ptr = brnch_nxt(ptr)
                     id = brid(ptr)
                     numpr = numpr + 1
                   endif
                 enddo
                 if (numpr .gt. 1) then
                   ptr = oldptr
                   numpr = 0
                   lastky = ky(ptr)
                   id = brid(ptr)
                   do while (ptr .gt. 0 .and. ky(ptr) .eq. lastky)   
                     numpr = numpr + 1
                     numid = ftn_atoi(brid(ptr))
                     if (numid .lt. numpr) then
                       ich = numpr + ichar('0')
                       if (ich .gt. ichar('9')) then
                         ich = numpr + ichar('A') - 9
                       endif
                       brid(ptr) = char(ich)
                       if (brtype(ptr) .eq. 1) then
                         ptr = brnch_nxt(ptr)
                         do while (ptr .gt. 0 .and. 
     &                             ky(ptr) .eq. lastky .and.
     &                             brid(ptr) .eq. id)
                           brid(ptr) = char(ich)
                           ptr = brnch_nxt(ptr)
                         enddo
                         id = brid(ptr)
                       else
                         ptr = brnch_nxt(ptr)
                         id = brid(ptr)
                       endif
                     else
                       numpr = numid
                       ptr = brnch_nxt(ptr)
                       id = brid(ptr)
                     endif
                   enddo
                 endif
               else
                 ptr = brnch_nxt(ptr)
               endif
             enddo
           endif

           ptr = kbsdta(16,nb)
           do while (ptr .gt. 0)

             if (brtype(ptr) .eq. 1 .and. brnch_ptr(ptr) .gt. 0) then

c              Determine whether a transformer exists within these
c              sections and that more than one section exists.
c
               oldptr = ptr
               ptr = brnch_nxt(ptr)
               num_tx = 0
               num_sec = 0
               do while (ptr .gt. 0 .and.
     &                   ky(ptr) .eq. ky(oldptr) .and.
     &                   brid(ptr) .eq. brid(oldptr))
                 num_sec = num_sec + 1
                 if (brtype(ptr) .eq. 5 .or. 
     &               brtype(ptr) .eq. 6) then
                   num_tx = num_sec
                 endif
                 ptr = brnch_nxt(ptr)
               enddo
               ptr = oldptr
               nsec = 0
               if (num_tx .gt. 0 .and. num_sec .gt. 1) then
                 nsec = fndpsptr (ptr)
                 if (nsec .lt. 0) then
                   write (errbuf(1), 10010) bus(kx(ptr)), base(kx(ptr)),
     &               bus(ky(ptr)), base(ky(ptr)), brid(ptr)
10010              format (' Incomplete pseudo-bus conversion (', 
     &               a, f7.1, 1x, a, f7.1, 1x, a, ')')
                   call prterx ('W',1)
                   error = 1
                   nsec = 0
                 else
                   num_tx = pseudo(2,nsec)
                 endif
               endif
               ptr = brnch_nxt(ptr)
               num_sec = 0
               num_section = 0
               do while (ptr .gt. 0 .and. 
     &                   ky(ptr) .eq. ky(oldptr) .and.
     &                   brid(ptr) .eq. brid(oldptr))
                 num_sec = num_sec + 1

                 if (nsec .eq. 0) then
                   k1 = kx(ptr)
                   k2 = ky(ptr)
                 else if ((num_tx .eq. 1 .and. num_sec .eq. num_tx) .or.
     &                    (num_tx .gt. 1 .and. num_sec .lt. num_tx)) 
     &             then
                   k1 = kx(ptr)
                   k2 = pseudo(3,nsec)
                 else
                   k1 = pseudo(3,nsec)
                   k2 = ky(ptr)
                 endif

                 status = gtge_num (k1, iptia1, iptiz1, iptib1)
                 status = gtge_num (k2, iptia2, iptiz2, iptib2)

                 if (iptib1 .le. 0 .or. iptib2 .le. 0 .or.
     &               kbsdta(1,k1) .eq. 5 .or. kbsdta(1,k1) .eq. 12 .or.
     &               kbsdta(1,k2) .eq. 5 .or. kbsdta(1,k2) .eq. 12) then
                 else
                   k1x = pti_num(iptib1)
                   k2x = pti_num(iptib2)
                   nbr = iabs (brnch_ptr(ptr))
                   call getchr (3, cbown, kbrnch(3,nbr))
                   if (kbrnch(15, nbr) .ne. 0) then
                     ior = ((brnch_ptr(ptr) .gt. 0 .and. 
     &                       kbrnch(15, nbr) .eq. 2) .or.
     &                      (brnch_ptr(ptr) .lt. 0 .and. 
     &                       kbrnch(15, nbr) .eq. 1)) 
                   else if (owner(k1) .eq. cbown) then
                     ior = .true.
                   else if (owner(k2) .eq. cbown) then
                     ior = .false.
                   else 
                     ior = (inp2alf(k1) .lt. inp2alf(k2)) 
                   endif
c
c                  ior = .true.  -- metering point at "to" bus
c                                   (losses assigned to "from" bus)
c                        .false. -- metering point at "from" bus
c                                   (losses assigned to "to" bus)
c
                   if (.not. ior) then
                     aloss = 0.0
                     iptia = fnd_ptia (arcnam(jarzn(k2)))
                     iptiz = fnd_ptiy (zone(k2))
                   else
                     aloss = 1.0
                     iptia = fnd_ptia (arcnam(jarzn(k1)))
                     iptiz = fnd_ptiy (zone(k1))
                   endif
                   id = brid(ptr)
                   if (id .eq. ' ' .and. option(2) .eq. 'Y') then
                     id = '1'
                   else if (id .eq. ' ') then
                     id = '0'
                   endif
                   iptio = add_ptiq (cbown)
                   if (brtype(ptr) .ne. 1 .and. brtype(ptr) .ne. 4) then
                     mo = mod (kbrnch(11,nbr), 100)
                     kyr = kbrnch(11,nbr) / 100
                     if (mo .ne. 0 .or. kyr .ne. 0) then
                       write (date_in, fmt='(i2.2, i2.2, i2.2)') kyr, 
     &                   mo, 1
                     else
                       date_in = def_date_in
                     endif
                   else
                     date_in = def_date_in
                   endif
                   date_out = def_date_out
                   call gt_gebrv (ptr, temp, iseason)
                   if (brtype(ptr) .eq. 3 .or. brtype(ptr) .eq. 8) then
                     tx_g = 0.0
                     tx_tap1 = 0.0
                     tx_tap2 = 0.0
                     num_section = num_section + 1
                   else
                     tx_g = temp(3)
                     tx_tap1 = temp(9)
                     tx_tap2 = temp(10)
                   endif
                   if (brtype(ptr) .eq. 8) then
                     if (brnch_ptr(ptr) .gt. 0) then
                       g1 = brnch(7,nbr)
                       b1 = brnch(8,nbr)
                       g2 = brnch(9,nbr)
                       b2 = brnch(10,nbr)
                     else
                       g1 = brnch(9,nbr)
                       b1 = brnch(10,nbr)
                       g2 = brnch(7,nbr)
                       b2 = brnch(8,nbr)
                     endif
                   else
                     g1 = brnch(7,nbr)
                     b1 = brnch(8,nbr)
                     g2 = g1
                     b2 = b1
                   endif
                   if ((brtype(ptr) .eq. 3 .and. g1 .ne. 0.0) .or. 
     &                 (brtype(ptr) .eq. 8 .and.
     &                 (g1 .ne. 0.0 .or. b1 .ne. 0.0))) then
                     if (brtype(ptr) .eq. 3) then
                       gx = g1
                       bx = 0.0
                     else
                       gx = g1
                       bx = b1
                     endif
                     sh_type = 'f '
                     write (xbuf, 10020) k1x, bus(k1), base(k1), 
     &                 sh_type, k2x, bus(k2), base(k2), id // ' ', 
     &                 num_section, ' ', branch_status(nbr), 
     &                 pti_anum(iptia), 
     &                 pti_znum(iptiz), gx, bx, date_in, date_out, 0, 
     &                 0, pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 0, 0.0
10020                format (2x, i5, 1x, '"', a, '"', 1x, f6.2, 1x, 
     &                 '"', a, '"', 3x, i5, 1x, '"', a, '"', 1x, f6.2, 
     &                 1x, '"', a, '"', 1x, i2, 1x, '"', a, '"', 1x, 
     &                 ':', 1x, i1, 1x, i2, 1x, i4, 2(1x, f8.5), 
     &                 2(3x, a), 2x, i1, 2x, i1, 4(1x, i3, 1x, f5.3))
                     last = lastch (xbuf)
                     status = write_ge_file (3, xbuf(1:last))
                     total(3) = total(3) + 1
                   endif
                   if ((brtype(ptr) .eq. 3 .and. g2 .ne. 0.0) .or. 
     &                 (brtype(ptr) .eq. 8 .and.
     &                 (g2 .ne. 0.0 .or. b2 .ne. 0.0))) then
                     if (brtype(ptr) .eq. 3) then
                       gx = g2
                       bx = 0.0
                     else
                       gx = g2
                       bx = b2
                     endif
                     sh_type = 't '
                     sec = brsect(ptr)
                     if (sec .eq. 0) sec = 1
                     write (xbuf, 10020) k1x, bus(k1), base(k1), 
     &                 sh_type, k2x, bus(k2), base(k2), id // ' ', 
     &                 num_section, ' ', branch_status(nbr), 
     &                 pti_anum(iptia), 
     &                 pti_znum(iptiz), gx, bx, date_in, date_out, 0, 
     &                 0, pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 0, 0.0
                     last = lastch (xbuf)
                     status = write_ge_file (3, xbuf(1:last))
                     total(3) = total(3) + 1
                   endif
                   if (brtype(ptr) .eq. 3 .or. brtype(ptr) .eq. 8) then
                     if (temp(1) .gt. -99.0 .and. temp(1) .lt. 100.0) 
     &                 then
                       write (tempc(1), fmt='(f8.5)') temp(1)
                     else
                       write (tempc(1), fmt='(f8.1)') temp(1)
                     endif 
                     if (temp(2) .gt. -99.0 .and. temp(2) .lt. 100.0)
     &                 then
                       write (tempc(2), fmt='(f8.5)') temp(2)
                     else
                       write (tempc(2), fmt='(f8.1)') temp(2)
                     endif 
                     write (xbuf, 10030) k1x, bus(k1), base(k1), k2x,
     &                 bus(k2), base(k2), id // ' ', num_section, ' ', 
     &                 branch_status(nbr), tempc(1), tempc(2), temp(4), 
     &                 (temp(i), i=15,18), aloss, temp(13)
10030                format (2x, i5, 1x, '"', a, '"', 1x, f6.2, 3x, i5, 
     &                 1x, '"', a, '"', 1x, f6.2, 1x, '"', a, '"', 1x, 
     &                 i2, 1x, '"', a, '"', 1x, ':', 2x, i1, 
     &                 2(1x, a), 1x, f8.5, 4(1x, f6.1), 1x, f5.3, 1x, 
     &                 f6.1, 1x, '/')
                     last = lastch (xbuf)
                     status = write_ge_file (1, xbuf(1:last))

                     write (xbuf, 10040) pti_anum(iptia), 
     &                 pti_znum(iptiz), tx_g, tx_tap1, tx_tap2, date_in,
     &                 date_out, 0, 0, 0, (temp(i), i=19,22), 
     &                 pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 0, 0.0, 
     &                 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0
10040                format (i2, i7, 1x, f7.5, 2(1x, f5.3), 
     &                 2(3x, a), 1x, i1, 1x, i1, 1x, i2, 
     &                 4(1x, f6.1), 8(1x, i3, 1x, f5.3), 1x, i1)
                     last = lastch (xbuf)
                     status = write_ge_file (1, xbuf(1:last))
                       total(1) = total(1) + 1
                   else
                     ltc_type = 1
                     tx_base = 0.0
                     if (tx_base .eq. 0.0) tx_base = bmva
                     k3x = -1
                     k4x = -1
                     k5x = -1
                     bus3c = '        '
                     bus4c = '        '
                     bus5c = '        '
                     write (base3c, fmt='(f6.2)') 0.0
                     write (base4c, fmt='(f6.2)') 0.0
                     write (base5c, fmt='(f6.2)') 0.0
                     iz_data = 0
                     write (xbuf, 10050) k1x, bus(k1), base(k1), 
     &                 k2x, bus(k2), base(k2), id // ' ', ' ', 
     &                 branch_status(nbr), ltc_type, k3x, bus3c, 
     &                 base3c, iz_data, k4x, bus4c, base4c, k5x, 
     &                 bus5c, base5c, pti_anum(iptia), pti_znum(iptiz), 
     &                 tx_base, temp(1), temp(2), 0.0, 0.0, 0.0, 0.0
10050                format (2x, i5, 1x, '"', a, '"', 1x, f6.2, 3x, i5, 
     &                 1x, '"', a, '"', 1x, f6.2, 1x, '"', a, '"', 1x, 
     &                 '"', a, '"', 1x, ':', 2x, i1, 1x, i2, 2x, i5, 
     &                 1x, '"', a, '"', 1x, a, 2x, i2, 2x, i5, 1x, '"', 
     &                 a, '"', 1x, a, 2x, i5, 1x, '"', a, '"', 1x, a,
     &                 1x, i3, 1x, i4, 1x, f7.1, 6(1x, f8.5), 1x, '/')
                     last = lastch (xbuf)
                     status = write_ge_file (2, xbuf(1:last))

                     if (brtype(ptr) .eq. 5) then
                       tx_phase = 0.0
                       tap1 = temp(9)
                       tap2 = temp(10)
                     else
                       tx_phase = temp(9)
                       tap1 = 1.0
                       tap2 = temp(10)
                     endif
                     write (tmaxc, fmt='(f8.5)') 0.0
                     write (tminc, fmt='(f8.5)') 0.0
                     write (tstepc, fmt='(f12.8)') 0.0
                     write (tapfpc, fmt='(f12.8)') 1.0
                     write (vamaxc, fmt='(f8.4)') 0.0
                     write (vaminc, fmt='(f8.4)') 0.0
                     write (xbuf, 10060) base(k1), base(k2), 0.0, 
     &                 tx_phase, temp(3), temp(4), (temp(i), i=15,18), 
     &                 aloss, tmaxc, tminc, vamaxc, vaminc, 
     &                 tstepc, tap1, tapfpc, tap2, 0.0, date_in, 
     &                 date_out, 0, 0, (temp(i),i=19,22)
10060                format (4(1x, f6.2), 2(1x, f7.5), 4(1x, f6.1), 1x, 
     &                 f5.3, 5(1x, a), 1x, f6.4, 1x, a, 2(1x, f6.4), 3x,
     &                 a, 3x, a, 1x, i1, 1x, i1, 4(1x, f6.1), ' /')
                     last = lastch (xbuf)
                     status = write_ge_file (2, xbuf(1:last))
                     write (xbuf, 10070) pti_onum(iptio), 1.0, 0, 0.0, 
     &                 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0
10070                format (8(1x, i3, 1x, f5.3), 1x, i1)
                     last = lastch (xbuf)
                     status = write_ge_file (2, xbuf(1:last))

                     total(2) = total(2) + 1
c
c                    Optionally write extended branch ratings
c
                     ixt = temp(23)
                     if (extfilename(1:1) .ne. ' ' .and. ixt .gt. 0) 
     &                 then
                       write (xbuf, 10032) lntype(brtype(ptr)),
     &                   k1x, bus(k1), base(k1), k2x,
     &                   bus(k2), base(k2), id // ' ', num_section, 
     &                   ext_date(1,ixt), ext_date(2,ixt), temp(25), 
     &                   (ext_ratings(i,ixt),i=4,6),
     &                   (ext_ratings(i,ixt),i=1,3), 
     &                   (ext_ratings(i,ixt),i=13,15), 
     &                   0.0, 0.0, 0.0, 
     &                   (ext_ratings(i,ixt),i=10,12),
     &                   (ext_ratings(i,ixt),i=7,9)
10032                  format (a, 2x, i5, 1x, '"', a, '"', 1x, f6.2, 3x,
     &                   i5, 1x, '"', a, '"', 1x, f6.2, 1x, '"', a, 
     &                   '"', 1x, i2, 2(1x, a), 19(1x, f8.3))
                       last = lastch (xbuf)
                       status = write_ge_file (4, xbuf(1:last))
                     endif
                   endif
                 endif
                 ptr = brnch_nxt(ptr)
               enddo
            
             else if (brtype(ptr) .eq. 1) then

               oldptr = ptr
               ptr = brnch_nxt(ptr)

               do while (ptr .gt. 0 .and. 
     &                  (ky(ptr) .eq. ky(oldptr) .and.
     &                   brid(ptr) .eq. brid(oldptr)))

                 ptr = brnch_nxt(ptr)
               enddo
            
             else if ((brtype(ptr) .eq. 3 .or. 
     &                 brtype(ptr) .eq. 8) .and. 
     &                 brnch_ptr(ptr) .gt. 0) then

C              L,E,T,TP,LM: Original submittal                

               k1 = kx(ptr)
               k2 = ky(ptr)
               status = gtge_num (k1, iptia1, iptiz1, iptib1)
               status = gtge_num (k2, iptia2, iptiz2, iptib2)
               if (iptib1 .le. 0 .or. iptib2 .le. 0 .or.
     &             kbsdta(1,k1) .eq. 5 .or. kbsdta(1,k1) .eq. 12 .or.
     &             kbsdta(1,k2) .eq. 5 .or. kbsdta(1,k2) .eq. 12) then
               else
                 k1x = pti_num(iptib1)
                 k2x = pti_num(iptib2)
                 nbr = iabs (brnch_ptr(ptr))
                 call getchr (3, cbown, kbrnch(3,nbr))
                 if (kbrnch(15, nbr) .ne. 0) then
                   ior = ((brnch_ptr(ptr) .gt. 0 .and. 
     &                     kbrnch(15, nbr) .eq. 2) .or.
     &                    (brnch_ptr(ptr) .lt. 0 .and. 
     &                     kbrnch(15, nbr) .eq. 1)) 
                 else if (owner(k1) .eq. cbown) then
                   ior = .true.
                 else if (owner(k2) .eq. cbown) then
                   ior = .false.
                 else 
                   ior = (inp2alf(k1) .lt. inp2alf(k2)) 
                 endif
c
c                ior = .true.  -- metering point at "to" bus
c                                 (losses assigned to "from" bus)
c                      .false. -- metering point at "from" bus
c                                 (losses assigned to "to" bus)
c
                 if (.not. ior) then
                   aloss = 0.0
                   iptia = fnd_ptia (arcnam(jarzn(k2)))
                   iptiz = fnd_ptiy (zone(k2))
                 else
                   aloss = 1.0
                   iptia = fnd_ptia (arcnam(jarzn(k1)))
                   iptiz = fnd_ptiy (zone(k1))
                 endif
                 id = brid(ptr)
                 if (id .eq. ' ' .and. option(2) .eq. 'Y') then
                   id = '1'
                 else if (id .eq. ' ') then
                   id = '0'
                 endif
                 call gt_gebrv (ptr, temp, iseason)
                 iptio = add_ptiq (cbown)
                 if (brtype(ptr) .ne. 1 .and. brtype(ptr) .ne. 4) then
                   mo = mod (kbrnch(11,nbr), 100)
                   kyr = kbrnch(11,nbr) / 100
                   if (mo .ne. 0 .or. kyr .ne. 0) then
                     write (date_in, fmt='(i2.2, i2.2, i2.2)') kyr, 
     &                 mo, 1
                   else
                     date_in = def_date_in
                   endif
                 else
                   date_in = def_date_in
                 endif
                 date_out = def_date_out
                 if (brtype(ptr) .eq. 3 .or. brtype(ptr) .eq. 8) then

                   if (brtype(ptr) .eq. 8) then
                     if (brnch_ptr(ptr) .gt. 0) then
                       g1 = brnch(7,nbr)
                       b1 = brnch(8,nbr)
                       g2 = brnch(9,nbr)
                       b2 = brnch(10,nbr)
                     else
                       g1 = brnch(9,nbr)
                       b1 = brnch(10,nbr)
                       g2 = brnch(7,nbr)
                       b2 = brnch(8,nbr)
                     endif
                   else
                     g1 = brnch(7,nbr)
                     b1 = brnch(8,nbr)
                     g2 = g1
                     b2 = b1
                   endif
                   if ((brtype(ptr) .eq. 3 .and. g1 .ne. 0.0) .or. 
     &                 (brtype(ptr) .eq. 8 .and.
     &                 (g1 .ne. 0.0 .or. b1 .ne. 0.0))) then
                     if (brtype(ptr) .eq. 3) then
                       gx = g1
                       bx = 0.0
                     else
                       gx = g1
                       bx = b1
                     endif
                     sh_type = 'f '
                     sec = brsect(ptr)
                     if (sec .eq. 0) sec = 1
                     write (xbuf, 10020) k1x, bus(k1), base(k1), 
     &                 sh_type, k2x, bus(k2), base(k2), id // ' ', 
     &                 sec, ' ', branch_status(nbr), pti_anum(iptia), 
     &                 pti_znum(iptiz), gx, bx, date_in, date_out, 0, 
     &                 0, pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 0, 0.0
                     last = lastch (xbuf)
                     status = write_ge_file (3, xbuf(1:last))
                     total(3) = total(3) + 1
                   endif

                   if ((brtype(ptr) .eq. 3 .and. g2 .ne. 0.0) .or. 
     &                 (brtype(ptr) .eq. 8 .and.
     &                 (g2 .ne. 0.0 .or. b2 .ne. 0.0))) then
                     if (brtype(ptr) .eq. 3) then
                       gx = g2
                       bx = 0.0
                     else
                       gx = g2
                       bx = b2
                     endif
                     sh_type = 't '
                     sec = brsect(ptr)
                     if (sec .eq. 0) sec = 1
                     write (xbuf, 10020) k1x, bus(k1), base(k1), 
     &                 sh_type, k2x, bus(k2), base(k2), id // ' ', 
     &                 sec, ' ', branch_status(nbr), pti_anum(iptia), 
     &                 pti_znum(iptiz), gx, bx, date_in, date_out, 0, 
     &                 0, pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 0, 0.0
                     last = lastch (xbuf)
                     status = write_ge_file (3, xbuf(1:last))
                     total(3) = total(3) + 1
                   endif

                   if (temp(1) .gt. -99.0 .and. temp(1) .lt. 100.0) then
                     write (tempc(1), fmt='(f8.5)') temp(1)
                   else
                     write (tempc(1), fmt='(f8.1)') temp(1)
                   endif 
                   if (temp(2) .gt. -99.0 .and. temp(2) .lt. 100.0) then
                     write (tempc(2), fmt='(f8.5)') temp(2)
                   else
                     write (tempc(2), fmt='(f8.1)') temp(2)
                   endif 
                   write (xbuf, 10030) k1x, bus(k1), base(k1), k2x, 
     &               bus(k2), base(k2), id // ' ', 1,
     &               ' ', branch_status(nbr), tempc(1), tempc(2), 
     &               temp(4), (temp(i), i=15,18), aloss, temp(13)
                   last = lastch (xbuf)
                   status = write_ge_file (1, xbuf(1:last))
                   write (xbuf, 10040) pti_anum(iptia), 
     &               pti_znum(iptiz), 0.0, 0.0, 0.0, date_in, 
     &               date_out, 0, 0, 0, (temp(i),i=19,22), 
     &               pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 0, 0.0, 
     &               0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0
                   last = lastch (xbuf)
                   status = write_ge_file (1, xbuf(1:last))
                   total(1) = total(1) + 1
c
c                  Optionally write extended branch ratings
c
                   ixt = temp(23)
                   if (extfilename(1:1) .ne. ' ' .and. ixt .gt. 0) then
                     write (xbuf, 10032) lntype(brtype(ptr)),
     &                 k1x, bus(k1), base(k1), k2x,
     &                 bus(k2), base(k2), id // ' ', num_section, 
     &                 ext_date(1,ixt), ext_date(2,ix), temp(25), 
     &                 (ext_ratings(i,ixt),i=4,6),
     &                 (ext_ratings(i,ixt),i=1,3), 
     &                 (ext_ratings(i,ixt),i=13,15), 
     &                 0.0, 0.0, 0.0, 
     &                 (ext_ratings(i,ixt),i=10,12),
     &                 (ext_ratings(i,ixt),i=7,9)
                     last = lastch (xbuf)
                     status = write_ge_file (4, xbuf(1:last))
                   endif
                 endif
               endif
               ptr = brnch_nxt(ptr)

             else if ((brtype(ptr) .eq. 5 .or. 
     &                 brtype(ptr) .eq. 6) .and. 
     &                (base(kx(ptr)) .gt. base(ky(ptr)) .or.
     &                (base(kx(ptr)) .eq. base(ky(ptr)) .and.
     &                 brnch_ptr(ptr) .gt. 0))) then

C              T,TP: High base to low base

               k1 = kx(ptr)
               k2 = ky(ptr)
               status = gtge_num (k1, iptia1, iptiz1, iptib1)
               status = gtge_num (k2, iptia2, iptiz2, iptib2)
               if (iptib1 .le. 0 .or. iptib2 .le. 0 .or.
     &             kbsdta(1,k1) .eq. 5 .or. kbsdta(1,k1) .eq. 12 .or.
     &             kbsdta(1,k2) .eq. 5 .or. kbsdta(1,k2) .eq. 12) then
               else
                 k1x = pti_num(iptib1)
                 k2x = pti_num(iptib2)
                 nbr = iabs (brnch_ptr(ptr))
                 call getchr (3, cbown, kbrnch(3,nbr))
                 if (kbrnch(15, nbr) .ne. 0) then
                   ior = ((brnch_ptr(ptr) .gt. 0 .and. 
     &                     kbrnch(15, nbr) .eq. 2) .or.
     &                    (brnch_ptr(ptr) .lt. 0 .and. 
     &                     kbrnch(15, nbr) .eq. 1)) 
                 else if (owner(k1) .eq. cbown) then
                   ior = .true.
                 else if (owner(k2) .eq. cbown) then
                   ior = .false.
                 else 
                   ior = (inp2alf(k1) .lt. inp2alf(k2)) 
                 endif
c
c                ior = .true.  -- metering point at "to" bus
c                                 (losses assigned to "from" bus)
c                      .false. -- metering point at "from" bus
c                                 (losses assigned to "to" bus)
c
                 if (.not. ior) then
                   aloss = 0.0
                   iptia = fnd_ptia (arcnam(jarzn(k2)))
                   iptiz = fnd_ptiy (zone(k2))
                 else
                   aloss = 1.0
                   iptia = fnd_ptia (arcnam(jarzn(k1)))
                   iptiz = fnd_ptiy (zone(k1))
                 endif
                 id = brid(ptr)
                 if (id .eq. ' ' .and. option(2) .eq. 'Y') then
                   id = '1'
                 else if (id .eq. ' ') then
                   id = '0'
                 endif
                 call gt_gebrv (ptr, temp, iseason)
                 iptio = add_ptiq (cbown)
                 if (brtype(ptr) .ne. 1 .and. brtype(ptr) .ne. 4) then
                   mo = mod (kbrnch(11,nbr), 100)
                   kyr = kbrnch(11,nbr) / 100
                   if (mo .ne. 0 .or. kyr .ne. 0) then
                     write (date_in, fmt='(i2.2, i2.2, i2.2)') kyr, 
     &                 mo, 1
                   else
                     date_in = def_date_in
                   endif
                 else
                   date_in = def_date_in
                 endif
                 date_out = def_date_out
                 if (brtype(ptr) .eq. 5 .or. 
     &               brtype(ptr) .eq. 6) then
                   ltc_type = 1
                   tx_base = 0.0
                   if (tx_base .eq. 0.0) tx_base = bmva
                   k3x = -1
                   k4x = -1
                   k5x = -1
                   bus3c = '        '
                   bus4c = '        '
                   bus5c = '        '
                   write (base3c, fmt='(f6.2)') 0.0
                   write (base4c, fmt='(f6.2)') 0.0
                   write (base5c, fmt='(f6.2)') 0.0
                   iz_data = 0
                   write (xbuf, 10050) k1x, bus(k1), base(k1), 
     &               k2x, bus(k2), base(k2), id // ' ', ' ', 
     &               branch_status(nbr), 
     &               ltc_type, k3x, bus3c, base3c, iz_data, k4x,
     &               bus4c, base4c, k5x, bus5c, base5c, pti_anum(iptia),
     &               pti_znum(iptiz), tx_base, temp(1), temp(2),
     &               0.0, 0.0, 0.0, 0.0
                   last = lastch (xbuf)
                   status = write_ge_file (2, xbuf(1:last))

                   if (brtype(ptr) .eq. 5) then
                     tx_phase = 0.0
                     tap1 = temp(9)
                     tap2 = temp(10)
                   else
                     tx_phase = temp(9)
                     tap1 = 1.0
                     tap2 = temp(10)
                   endif
                   write (tmaxc, fmt='(f8.5)') 0.0
                   write (tminc, fmt='(f8.5)') 0.0
                   write (tstepc, fmt='(f12.8)') 0.0
                   write (tapfpc, fmt='(f12.8)') 1.0
                   write (vamaxc, fmt='(f8.4)') 0.0
                   write (vaminc, fmt='(f8.4)') 0.0
                   write (xbuf, 10060) base(k1), base(k2), 0.0, 
     &               tx_phase, temp(3), temp(4), (temp(i), i = 15,18), 
     &               aloss, tmaxc, tminc, vamaxc, vaminc, tstepc,
     &               tap1, tapfpc, tap2, 0.0, date_in, date_out, 0, 0,
     &               (temp(i), i = 19,22)
                   last = lastch (xbuf)
                   status = write_ge_file (2, xbuf(1:last))
                   write (xbuf, 10070) pti_onum(iptio), 1.0, 0, 0.0, 0, 
     &               0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0
                   last = lastch (xbuf)
                   status = write_ge_file (2, xbuf(1:last))

                   total(2) = total(2) + 1
                 endif
               endif
               ptr = brnch_nxt(ptr)
             else if (brtype(ptr) .eq. 5. or. 
     &                brtype(ptr) .eq. 6) then
               ptr = brnch_nxt(ptr)

             else if (brtype(ptr) .eq. 4) then

C              R: Adjustable tap side or hi-low     

               krmap = orienttx (ptr, brnch_nxt(ptr), k1, k2, 
     &                           tap1, tap2)
c
c              krmap = 1 if kx(ptr) = fixed-tap
c                           ky(ptr) = variable-tap side
c
c                      2 if kx(ptr) = variable-tap side
c                           ky(ptr) = fixed-tap
c
               k1 = kx(ptr)
               k2 = ky(ptr)
               status = gtge_num (k1, iptia1, iptiz1, iptib1)
               status = gtge_num (k2, iptia2, iptiz2, iptib2)
               if (krmap .eq. 2 .and. 
     &             iptib1 .gt. 0 .and. iptib2 .gt. 0 .and.
     &             kbsdta(1,k1) .ne. 5 .and. kbsdta(1,k1) .ne. 12 .and.
     &             kbsdta(1,k2) .ne. 5 .and. kbsdta(1,k2) .ne. 12) then
                 oldptr = ptr
                 nbr = iabs (brnch_ptr(ptr))
                 iz_data = brnch(16,nbr)
                 call getchr (1, trtype, kbrnch(3,nbr))
                 k3 = kbrnch(4,nbr)
                 if (k3 .eq. 0) k3 = k1
                 if (k3 .eq. -1) k3 = k1
                 if (k3 .eq. -2) k3 = k2
                 if (k3 .lt. 0) k3 = iabs (k3)
                 status = gtge_num (k3, iptia3, iptiz3, iptib3)
                 k3x = pti_num(iptib3)
                 if (trtype .ne. 'P' .and. trtype .ne. 'M') then
                   tmax = brnch(6,nbr) / base(k1) + 0.001
                   tmin = brnch(7,nbr) / base(k1) - 0.001
                   numtaps = brnch(8,nbr)
                   tstart = 1.0
c
c                  Compute tapfp such that 
c                    (1) assure tstart is at a discrete tape, and
c                    (2) render resolutions of tmin, tmax, and tsteps
c                        at 2f8.5, f12.8 respectively)
c
                   status = gettapfp (tstart, tmin, tmax, numtaps, 
     &                                tstepsize, tapfp)
                   write (tmaxc, fmt='(f8.5)') tmax
                   write (tminc, fmt='(f8.5)') tmin
                   write (tstepc, fmt='(f12.8)') tstepsize
                   write (tapfpc, fmt='(f12.8)') tapfp
                 else 
                   if (brnch_ptr(ptr) .gt. 0) then
                     tmax = brnch(6,nbr) + 0.001
                     tmin = brnch(7,nbr) - 0.001
                   else
                     tmax = -brnch(7,nbr) + 0.001
                     tmin = -brnch(6,nbr) - 0.001
                   endif
                   if (brnch(8,nbr) .gt. 1.0) then
                     tstepsize = (tmax - tmin) / (brnch(8,nbr) - 1.0)
                   else
                     tstepsize = 0.00025
                   endif
                   write (tmaxc, fmt='(f7.2)') tmax
                   write (tminc, fmt='(f7.2)') tmin
                   write (tstepc, fmt='(f12.8)') tstepsize
                   write (tapfpc, fmt='(f12.8)') 1.00
                 endif
                 if (trtype .eq. 'P' .or. trtype .eq. 'Q') then
                   ptol = sign(brnch(10,nbr),brnch(9,nbr))
                   if (brnch_ptr(ptr) .gt. 0) then
                     vamax = brnch(9,nbr) + ptol
                     vamin = brnch(9,nbr) - ptol
                   else
                     vamax = -brnch(9,nbr) - ptol
                     vamin = -brnch(9,nbr) + ptol
                   endif
                   write (vamaxc, fmt='(f8.2)') vamax
                   write (vaminc, fmt='(f8.2)') vamin
                 else if (trtype .eq. 'M' .or. trtype .eq. 'N') then
                   if (brnch_ptr(ptr) .gt. 0) then
                     vamax = brnch(9,nbr) 
                     vamin = brnch(10,nbr) 
                   else
                     vamax = -brnch(10,nbr) 
                     vamin = -brnch(9,nbr) 
                   endif
                   write (vamaxc, fmt='(f8.2)') vamax
                   write (vaminc, fmt='(f8.2)') vamin
                 else
                   vamax = vlimx (inp2opt(k3))
                   vamin = vlimn (inp2opt(k3))
                   if (vamax - vamin .lt. 1.2 * abs (tstepsize)) then
                     vaverage = 0.5 * (vamax + vamin)
                     vamax = vaverage + 0.60 * abs (tstepsize)
                     vamin = vaverage - 0.60 * abs (tstepsize)
                   endif
                   write (vamaxc, fmt='(f8.4)') vamax
                   write (vaminc, fmt='(f8.4)') vamin
                 endif
                 ptr = brnch_nxt(ptr)

                 do while (ptr .gt. 0 .and. 
     &                    (ky(ptr) .eq. k2))

                   if (iptib1 .le. 0 .or. iptib2 .le. 0) then
                   else
                     k1x = pti_num(iptib1)
                     k2x = pti_num(iptib2)
                     nbr = iabs (brnch_ptr(ptr))
                     call getchr (3, cbown, kbrnch(3,nbr))
                     iptio = add_ptiq (cbown)
                     if (kbrnch(15, nbr) .ne. 0) then
                       ior = ((brnch_ptr(ptr) .gt. 0 .and. 
     &                         kbrnch(15, nbr) .eq. 2) .or.
     &                        (brnch_ptr(ptr) .lt. 0 .and. 
     &                         kbrnch(15, nbr) .eq. 1)) 
                     else if (owner(k1) .eq. cbown) then
                       ior = .true.
                     else if (owner(k2) .eq. cbown) then
                       ior = .false.
                     else 
                       ior = (inp2alf(k1) .lt. inp2alf(k2)) 
                     endif
c
c                    ior = .true.  -- metering point at "to" bus
c                                     (losses assigned to "from" bus)
c                          .false. -- metering point at "from" bus
c                                     (losses assigned to "to" bus)
c
                     if (.not. ior) then
                       aloss = 0.0
                       iptia = fnd_ptia (arcnam(jarzn(k2)))
                       iptiz = fnd_ptiy (zone(k2))
                     else
                       aloss = 1.0
                       iptia = fnd_ptia (arcnam(jarzn(k1)))
                       iptiz = fnd_ptiy (zone(k1))
                     endif
                     call gt_gebrv (ptr, temp, iseason)
                     id = brid(ptr)
                     if (id .eq. ' ' .and. option(2) .eq. 'Y') then
                       id = '1'
                     else if (id .eq. ' ') then
                       id = '0'
                     endif
                     if (brtype(ptr) .ne. 1 .and. brtype(ptr) .ne. 4) 
     &                 then
                       mo = mod (kbrnch(11,nbr), 100)
                       kyr = kbrnch(11,nbr) / 100
                       if (mo .ne. 0 .or. kyr .ne. 0) then
                         write (date_in, fmt='(i2.2, i2.2, i2.2)') kyr, 
     &                     mo, 1
                       else
                         date_in = def_date_in
                       endif
                     else
                       date_in = def_date_in
                     endif
                     date_out = def_date_out
                     if (trtype .eq. 'P' .or. trtype .eq. 'M') then
                       ltc_type = 4
                       k4x = -1
                       k5x = -1
                       bus4c = '        '
                       bus5c = '        '
                       write (base4c, fmt='(f6.2)') 0.0
                       write (base5c, fmt='(f6.2)') 0.0
                     else if (trtype .eq. 'Q' .or. trtype .eq. 'N') then
                       ltc_type = 3
                       k3x = -1
                       k4x = -1
                       k5x = -1
                       bus3c = '        '
                       bus4c = '        '
                       bus5c = '        '
                       write (base3c, fmt='(f6.2)') 0.0
                       write (base4c, fmt='(f6.2)') 0.0
                       write (base5c, fmt='(f6.2)') 0.0
                     else
                       ltc_type = 2
                       k4x = -1
                       k5x = -1
                       status = gtge_num (k3, iptia3, iptiz3, iptib3)
                       k3x = pti_num(iptib3)
                       bus3c = bus(k3)
                       bus4c = '        '
                       bus5c = '        '
                       write (base3c, fmt='(f6.2)') base(k3)
                       write (base4c, fmt='(f6.2)') 0.0
                       write (base5c, fmt='(f6.2)') 0.0
                       if (k3 .eq. k2) tstepsize = -abs(tstepsize)
                       write (tstepc, fmt='(f12.8)') tstepsize
                     endif
                     tx_base = 0.0
                     if (tx_base .eq. 0.0) tx_base = bmva
                     write (xbuf, 10050) k1x, bus(k1), base(k1), 
     &                 k2x, bus(k2), base(k2), id // ' ', ' ', 
     &                 branch_status(nbr),
     &                 ltc_type, k3x, bus3c, base3c, iz_data, k4x,
     &                 bus4c, base4c, k5x, bus5c, base5c, 
     &                 pti_anum(iptia), pti_znum(iptiz), tx_base, 
     &                 temp(1), temp(2), 0.0, 0.0, 0.0, 0.0
                     last = lastch (xbuf)
                     status = write_ge_file (2, xbuf(1:last))

                     if (brtype(ptr) .eq. 5) then
                       tx_phase = 0.0
                       tap1 = temp(9) + 1.0 - tapfp
                       tap2 = temp(10)
                     else
                       tx_phase = temp(9)
                       tap1 = 1.0
                       tap2 = temp(10)
                     endif
                     write (xbuf, 10060) base(k1), base(k2), 0.0, 
     &                 tx_phase, temp(3), temp(4), (temp(i), i = 15,18),
     &                 aloss, tmaxc, tminc, vamaxc, vaminc, 
     &                 tstepc, tap1, tapfpc, tap2, 0.0, date_in, 
     &                 date_out, 0, 0, (temp(i), i = 19,22)
                     last = lastch (xbuf)
                     status = write_ge_file (2, xbuf(1:last))
                     write (xbuf, 10070) pti_onum(iptio), 1.0, 0, 0.0, 
     &                 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0,
     &                 0
                     last = lastch (xbuf)
                     status = write_ge_file (2, xbuf(1:last))
                     total(2) = total(2) + 1
c
c                    Optionally write extended branch ratings
c
                     ixt = temp(23)
                     if (extfilename(1:1) .ne. ' ' .and. ixt .gt. 0) 
     &                 then
                       write (xbuf, 10032) lntype(brtype(ptr)),
     &                   k1x, bus(k1), base(k1), k2x,
     &                   bus(k2), base(k2), id // ' ', num_section, 
     &                   ext_date(1,ixt), ext_date(2,ix), temp(25), 
     &                   (ext_ratings(i,ixt),i=4,6),
     &                   (ext_ratings(i,ixt),i=1,3), 
     &                   (ext_ratings(i,ixt),i=13,15), 
     &                   0.0, 0.0, 0.0, 
     &                   (ext_ratings(i,ixt),i=10,12),
     &                   (ext_ratings(i,ixt),i=7,9)
                       last = lastch (xbuf)
                       status = write_ge_file (4, xbuf(1:last))
                     endif
                   endif
                   ptr = brnch_nxt(ptr)
                 enddo
               else
                 oldptr = ptr
                 ptr = brnch_nxt(ptr)

                 do while (ptr .gt. 0 .and. 
     &                     ky(ptr) .eq. ky(oldptr))
     
                   ptr = brnch_nxt(ptr)
                 enddo
               endif

             else if (brtype(ptr) .eq. 2 .or. 
     &                brtype(ptr) .eq. 3 .or. 
     &                brtype(ptr) .eq. 4. or. 
     &                brtype(ptr) .eq. 5. or. 
     &                brtype(ptr) .eq. 6. or. 
     &                brtype(ptr) .eq. 7 .or. 
     &                brtype(ptr) .eq. 8 .or.
     &                brtype(ptr) .eq. 9) then
               ptr = brnch_nxt(ptr)

             else if (brtype(ptr) .gt. 10) then
               brtype(ptr) = mod (brtype(ptr), 10)
               ptr = brnch_nxt(ptr)
             else
               write ( errbuf(1), 10090) lntype(brtype(ptr)), 
     &           bus(kx(ptr)), base(kx(ptr)), bus(ky(ptr)), 
     &           base(kx(ptr)), brid(ptr), brsect(ptr)
10090          format (' Failed to process branch [', a, 1x,
     &           a8, f6.1, 1x, a8, f6.1, 1x, a, i2, 
     &           '] -- invalid type??')
               call prterx ('W', 1)
               ptr = brnch_nxt(ptr)
             endif
           enddo

         enddo
c
c        Process any 3-terminal Tx data
c
         do ix = 1, num_3term
           k1 = tx_3term(1,ix)      ! from bus                    
           k2 = tx_3term(2,ix)      ! to bus    
           k3 = tx_3term(3,ix)      ! reg bus      
           k4 = tx_3term(4,ix)      ! 3-winding pt bus     
           k5 = tx_3term(5,ix)      ! tertiary bus
           i = tx_3term(6,ix)
           id = char(i)

           if (k3 .gt. 0) then
             bus3c = bus(k3)
             write (base3c, fmt='(f6.2)') base(k3)
             status = gtge_num (k3, iptia3, iptiz3, iptib3)
             k3x = pti_num(iptib3)
           else
             k3x = -1
             bus3c = '        '
             write (base3c, fmt='(f6.2)') 0.0
           endif
           if (k4 .gt. 0) then
             bus4c = bus(k4)
             write (base4c, fmt='(f6.2)') base(k4)
             status = gtge_num (k4, iptia4, iptiz4, iptib4)
             k4x = pti_num(iptib4)
           else
             k4x = -1
             bus4c = '        '
             write (base4c, fmt='(f6.2)') 0.0
           endif
           if (k5 .gt. 0) then
             bus5c = bus(k5)
             write (base5c, fmt='(f6.2)') base(k5)
             status = gtge_num (k5, iptia5, iptiz5, iptib5)
             k5x = pti_num(iptib5)
           else
             k5x = -1
             bus5c = '        '
             write (base5c, fmt='(f6.2)') 0.0
           endif

           ptr = numbrn (k1, k4, '*', 0)
           if (ptr .gt. 0) then
             if (brtype(ptr) .eq. 4) then
               nbr = iabs (brnch_ptr(ptr))
               call getchr (1, trtype, kbrnch(3,nbr))
               if (trtype .ne. 'P' .and. trtype .ne. 'M') then
                 tmax = brnch(6,nbr) / base(k1) + 0.001
                 tmin = brnch(7,nbr) / base(k1) - 0.001
                 numtaps = brnch(8,nbr)
                 tstart = 1.0
c
c                Compute tapfp such that 
c                  (1) assure tstart is at a discrete tape, and
c                  (2) render resolutions of tmin, tmax, and tsteps
c                      at 2f8.5, f12.8 respectively)
c
                 status = gettapfp (tstart, tmin, tmax, numtaps, 
     &                              tstepsize, tapfp)

                 write (tmaxc, fmt='(f8.5)') tmax
                 write (tminc, fmt='(f8.5)') tmin
                 write (tstepc, fmt='(f12.8)') tstepsize
                 write (tapfpc, fmt='(f12.8)') tapfp
               else
                 if (brnch_ptr(ptr) .gt. 0) then
                   tmax = brnch(6,nbr) + 0.001 
                   tmin = brnch(7,nbr) - 0.001
                 else
                   tmax = -brnch(7,nbr) + 0.001
                   tmin = -brnch(6,nbr) - 0.001
                 endif
                 numtaps = brnch(8,nbr)
                 if (numtaps .gt. 1) then
                   tstepsize = (tmax - tmin) / (float(numtaps) - 1.0)
                 else
                   tstepize = 0.00025
                 endif
                 write (tmaxc, fmt='(f7.2)') tmax
                 write (tminc, fmt='(f7.2)') tmin
                 write (tstepc, fmt='(f12.8)') tstepsize
                 write (tapfpc, fmt='(f12.8)') 1.00
               endif
               if (trtype .eq. 'P' .or. trtype .eq. 'Q') then
                 ptol = sign(brnch(10,nbr),brnch(9,nbr))
                 if (brnch_ptr(ptr) .gt. 0) then
                   vamax = brnch(9,nbr) + ptol 
                   vamin = brnch(9,nbr) - ptol
                 else
                   vamax = -brnch(9,nbr) - ptol
                   vamin = -brnch(9,nbr) + ptol
                 endif
                 write (vamaxc, fmt='(f8.2)') vamax
                 write (vaminc, fmt='(f8.2)') vamin
               else if (trtype .eq. 'M' .and. trtype .eq. 'N') then
                 if (brnch_ptr(ptr) .gt. 0) then
                   vamax = brnch(9,nbr)
                   vamin = brnch(10,nbr) 
                 else
                   vamax = -brnch(10,nbr)
                   vamin = -brnch(9,nbr) 
                 endif
                   if (trtype .eq. 'P') then
                 endif
                 write (vamaxc, fmt='(f8.2)') vamax
                 write (vaminc, fmt='(f8.2)') vamin
               else
                 if (k3 .gt. 0) then
                   vamax = vlimx(inp2opt(k3))
                   vamin = vlimn(inp2opt(k3))
                 else
                   vamax = vlimx(inp2opt(k1))
                   vamin = vlimn(inp2opt(k1))
                 endif
                 if (vamax - vamin .lt. 1.2 * abs (tstepsize)) then
                   vaverage = 0.5 * (vamax + vamin)
                   vamax = vaverage + 0.60 * abs (tstepsize)
                   vamin = vaverage - 0.60 * abs (tstepsize)
                 endif
                 write (vamaxc, fmt='(f8.4)') vamax
                 write (vaminc, fmt='(f8.4)') vamin
               endif
               if (trtype .eq. 'P' .or. trtype .eq. 'M') then
                 ltc_type = 4
               else if (trtype .eq. 'Q' .or. trtype .eq. 'N') then
                 ltc_type = 3
               else
                 ltc_type = 2
                 if (k3 .eq. k2) tstepsize = -abs(tstepsize)
                 write (tstepc, fmt='(f12.8)') tstepsize
               endif
               iz_data = brnch(16,nbr)
               tx_base = 0.0
               if (iz_data .gt. 0) tx_base = brnch(18,nbr)
               if (tx_base .eq. 0.0) tx_base = bmva
             else
               ltc_type = 1
               tmax = 0.0
               tmin = 0.0
               tstepsize = 0.00025
               vamax = 0.0
               vamin = 0.0
               iz_data = 0
               tx_base = bmva
               write (tmaxc, fmt='(f8.5)') tmax
               write (tminc, fmt='(f8.5)') tmin
               write (tstepc, fmt='(f12.8)') tstepsize
               write (tapfpc, fmt='(f12.8)') 1.00
               write (vamaxc, fmt='(f8.4)') vamax
               write (vaminc, fmt='(f8.4)') vamin
             endif
             ptr = numbrn (k1, k2, id, 0)
             if (brtype(ptr) .eq. 4) ptr = brnch_nxt(ptr)
             status = gtge_num (k1, iptia1, iptiz1, iptib1)
             status = gtge_num (k2, iptia2, iptiz2, iptib2)
             if (iptib1 .gt. 0 .and. iptib2 .gt. 0 .and.
     &           kbsdta(1,k1) .ne. 5 .and. kbsdta(1,k1) .ne. 12 .and.
     &           kbsdta(1,k2) .ne. 5 .and. kbsdta(1,k2) .ne. 12) then
               k1x = pti_num(iptib1)
               k2x = pti_num(iptib2)
               nbr = iabs (brnch_ptr(ptr))
               call getchr (3, cbown, kbrnch(3,nbr))
               if (kbrnch(15, nbr) .ne. 0) then
                 ior = ((brnch_ptr(ptr) .gt. 0 .and. 
     &                  kbrnch(15, nbr) .eq. 2) .or.
     &                 (brnch_ptr(ptr) .lt. 0 .and. 
     &                  kbrnch(15, nbr) .eq. 1)) 
               else if (owner(k1) .eq. cbown) then
                 ior = .true.
               else if (owner(k2) .eq. cbown) then
                 ior = .false.
               else 
                 ior = (inp2alf(k1) .lt. inp2alf(k2)) 
               endif
c
c              ior = .true.  -- metering point at "to" bus
c                               (losses assigned to "from" bus)
c                    .false. -- metering point at "from" bus
c                               (losses assigned to "to" bus)
c
               if (.not. ior) then
                 aloss = 0.0
                 iptia = fnd_ptia (arcnam(jarzn(k2)))
                 iptiz = fnd_ptiy (zone(k2))
               else
                 aloss = 1.0
                 iptia = fnd_ptia (arcnam(jarzn(k1)))
                 iptiz = fnd_ptiy (zone(k1))
               endif
               if (brtype(ptr) .ne. 1 .and. brtype(ptr) .ne. 4) then
                 mo = mod (kbrnch(11,nbr), 100)
                 kyr = kbrnch(11,nbr) / 100
                 if (mo .ne. 0 .or. kyr .ne. 0) then
                   write (date_in, fmt='(i2.2, i2.2, i2.2)') kyr, 
     &               mo, 1
                 else
                   date_in = def_date_in
                 endif
               else
                 date_in = def_date_in
               endif
               date_out = def_date_out
               k1t = k1
               k2t = k2
               tx_g = 0.0
               tx_b = 0.0
               do n = 1, 3
                 ptr = numbrn (k1t, k2t, id, 0)
                 if (brtype(ptr) .eq. 4) ptr = brnch_nxt(ptr)
                 call gt_gebrv (ptr, temp, iseason)

                 zxterm(1,n) = temp(1) / tx_base             ! R
                 zxterm(2,n) = temp(2) / tx_base             ! X
                 zxterm(3,n) = temp(3) * tx_base             ! G
                 zxterm(4,n) = temp(4) * tx_base             ! B
                 zxterm(5,n) = temp(9)                       ! Tap1
                 zxterm(6,n) = temp(10)                      ! Tap1
                 zxterm(7,n) = base(k1t)                     ! Tx base1
                 zxterm(8,n) = base(k2t)                     ! Tx base2
                 a(n) = cmplx (zxterm(1,n), zxterm(2,n))              
                 tx_g = tx_g + zxterm(3,n)
                 tx_b = tx_b + zxterm(4,n)

                 if (n .eq. 1) then
                   k1t = k2
                   k2t = k4
                 else if (n .eq. 2) then
                   k1t = k5
                   k2t = k4
                 endif
               enddo
c
c              Compute delta-impdances
c
               b(1) = (a(1) * a(2) + a(2) * a(3) + a(3) * a(1)) / a(1)
               b(2) = (a(1) * a(2) + a(2) * a(3) + a(3) * a(1)) / a(2)
               b(3) = (a(1) * a(2) + a(2) * a(3) + a(3) * a(1)) / a(3)

               tx_g_ps = real (b(3))
               tx_b_ps = aimag (b(3))
               tx_g_pt = real (b(2))
               tx_b_pt = aimag (b(2))
               tx_g_ts = real (b(1))
               tx_b_ts = aimag (b(1))
 
               id = brid(ptr)
               if (id .eq. ' ' .and. option(2) .eq. 'Y') then
                 id = '1'
               else if (id .eq. ' ') then
                 id = '0'
               endif

               if (k3 .gt. 0) then
                 status = gtge_num (k3, iptia3, iptiz3, iptib3)
                 k3x = pti_num(iptib3)
               else
                 k3x = -1
               endif
               write (xbuf, 10050) k1x, bus(k1), base(k1), 
     &           k2x, bus(k2), base(k2), id // ' ', ' ', 
     &           branch_status(nbr), 
     &           ltc_type, k3x, bus3c, base3c, iz_data, k4x,
     &           bus4c, base4c, k5x, bus5c, base5c, pti_anum(iptia), 
     &           pti_znum(iptiz), tx_base, tx_g_ps, tx_b_ps, tx_g_pt,
     &           tx_b_pt, tx_g_ts, tx_b_ts
               last = lastch (xbuf)
               status = write_ge_file (2, xbuf(1:last))
 
               tx_phase = 0.0
               write (xbuf, 10060) base(k1), base(k2), base(k4), 
     &           tx_phase, tx_g, tx_b, (temp(i), i = 15,18), aloss, 
     &           tmaxc, tminc, vamaxc, vaminc, tstepc, zxterm(5,1),
     &           tapfpc, zxterm(5,2), zxterm(5,3), date_in, 
     &           date_out, 0, 0, (temp(i), i = 19,22)
               last = lastch (xbuf)
               status = write_ge_file (2, xbuf(1:last))

               write (xbuf, 10070) pti_onum(iptio), 1.0, 0, 0.0, 0, 
     &           0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0
               last = lastch (xbuf)
               status = write_ge_file (2, xbuf(1:last))
               total(2) = total(2) + 1
c
c              Optionally write extended branch ratings
c
               ixt = temp(23)
               if (extfilename(1:1) .ne. ' ' .and. ixt .gt. 0) then
                 write (xbuf, 10032) lntype(brtype(ptr)),
     &             k1x, bus(k1), base(k1), k2x,
     &             bus(k2), base(k2), id // ' ', num_section, 
     &             ext_date(1,ixt), ext_date(2,ix), temp(25), 
     &             (ext_ratings(i,ixt),i=4,6),
     &             (ext_ratings(i,ixt),i=1,3), 
     &             (ext_ratings(i,ixt),i=13,15), 
     &             0.0, 0.0, 0.0, 
     &             (ext_ratings(i,ixt),i=10,12),
     &             (ext_ratings(i,ixt),i=7,9)
                 last = lastch (xbuf)
                 status = write_ge_file (4, xbuf(1:last))
               endif
             endif
           endif
         enddo

         write (*, 10100) total(1)
10100    format (' * Writing ', i5, ' branch records to NETDAT file')

         write (xbuf, 10102) total(1)
10102    format ('branch data  [', i5, ']                               
     &  ck  se  long_id_    st resist   react   charge   rate1  rate2  r
     &ate3  rate4 aloss  lngth')
         last = lastch (xbuf)
         status = write_ge_file (0, xbuf(1:last))

         xbuf = '[EOF]'
         last = lastch (xbuf)
         status = write_ge_file (1, xbuf(1:last))
c
c        "Rewind" temp file and write to netdata file
c
         status = close_ge_file (1)
         status = open_ge_file (1, 'scratch1.dat', 'r')

         finished = .false.
         icount = 0
         do while (.not. finished)
           last = read_ge_file (1, xbuf)
           if (last .eq. 0 .or. xbuf(1:5) .eq. '[EOF]') go to 110
           status = write_ge_file (0, xbuf(1:last))
         enddo

  110    continue

         write (*, 10110) total(2)
10110    format (' * Writing ', i5, ' transformer records to NETDAT file
     &')

         write (text, fmt='(i5)') total(2)
         xbuf = 'transformer data  [' // text // ']                     
     &          ck   long_id_    st ty --no---    reg_name          zt  
     &       int                           tert              ar zone  tb
     &ase   ps_r    ps_x    pt_r    pt_x    ts_r    ts_x'
         last = lastch (xbuf)
         status = write_ge_file (0, xbuf(1:last))
c
c       "Rewind" temp file and write to netdata file
c
         xbuf = '[EOF]'
         last = lastch (xbuf)
         status = write_ge_file (2, xbuf(1:last))
         status = close_ge_file (2)
         status = open_ge_file (2, 'scratch2.dat', 'r')

         finished = .false.
         icount = 0
         do while (.not. finished)
           last = read_ge_file (2, xbuf)
           if (last .eq. 0 .or. xbuf(1:5) .eq. '[EOF]') go to 120
           status = write_ge_file (0, xbuf(1:last))
         enddo

  120    continue
         write ( errbuf(1), 10120) total(1)
10120    format (' Total branch records extracted:', i5)
         write ( errbuf(2), 10130) total(2)
10130    format (' Total transformer records extracted:', i5)
         call prterx ('I', 2)
 
         ext_gel = total(1) + total(2)

         return
         end
