C    @(#)updzon.f	20.4 2/13/96
      subroutine updzon
C
C     Update ZSUM, OVLOS, and SYST
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/agc.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/anlys.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/busanl.inc'
      include 'ipfinc/lodtyp.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/update.inc'
      include 'ipfinc/usranl.inc'
      include 'ipfinc/ownhash.inc'
      include 'ipfinc/bsekvhsh.inc'
      include 'ipfinc/brtype.inc'
 
      common /neggen/ neggen
      real neggen   

      logical opened
      real data(30) 
      integer flag, first, ptr, oldptr, status, open_file, getownvlt,
     &        gtmetpnt, whichend1, whichend2, buildzbo, buildxref,
     &        find_zon, ownsch
      character own * 3, ratec * 10, cbtype * 1, cbown * 3, cbyear * 2

      if (update(1) .eq. 0) go to 900   

C     Set up pointers to X data (busxdtptr)

      if (.not. xdt_flag) then
        do nb = 1, ntot
          busxdtptr(nb) = 0
        enddo
        do i = 1, kxtot
          kxd = xdata(1, i)
          if (kxd .gt. 0) then
             if (ordtbx .eq. 2) kxd = opt2inp(kxd)
             busxdtptr(kxd) = i
          endif
        enddo
        xdt_flag = .true.
      endif
c
c     Initialize all analysis arrays
c
      do i = 1,nztot
         do j = 1,26   
            zsum(j,i) = 0.0   
         enddo
      enddo
      neggen = 0.0  

      do i=1,MAXOVL
         do j=1,8
            ovlos(j,i)=0
         enddo
      enddo

      do i=1,MAXOWN
         do j=1,MAXVLT
            kov(j,i)=0
         enddo
      enddo

      do i=1,MAXOWN
         do j=1,10
            syst(j,i) = 0.0
         enddo
         syst_used(1,i) = 0.0
         syst_used(2,i) = 0.0
      enddo

      novls = 0
c
c     Rebuild zone, owner, and base kv hash arrays
c
      status = buildzbo(status)
      write (*, 120)
  120 format (' * Rebuilding zone-loss analysis arrays. This will take a
     & minute.')

      do 200 nbx = 1, ntot_alf
         nb = alf2inp(nbx)
         kt = inp2opt(nb) 
         call nrpqv (kt, pk, dpk, qk, dqk, vk)   
         pgen = pnetu(kt) + ploadu(kt)                    
         qgen = qnetu(kt) + qloadu(kt)                    
         call busqua (nb, pgen, qgen, data) 

C        Find Zone index. 

         ix = find_zon(zone(nb))
         if (ix .le. 0) then
            write (errbuf(1),170) zone(nb), bus(nb), base(nb)   
  170       format('0 Zone ', a2, ' for bus ', a8, f6.1,
     1         ' is not in system.')
            call prterx ('W',1) 
            go to 200   
         endif  
         zsum(2,ix) = zsum(2,ix) + data(2)  
         zsum(3,ix) = zsum(3,ix) + data(3)  
         zsum(4,ix) = zsum(4,ix) + data(4)  
         zsum(5,ix) = zsum(5,ix) + data(17) + data(19)  
         zsum(6,ix) = zsum(6,ix) + data(18) + data(20)  
         zsum(7,ix) = zsum(7,ix) + data(9)  
         zsum(8,ix) = zsum(8,ix) + data(14) + data(16)  
         if (data(1) .gt. 0.0) then 
            zsum(1,ix) = zsum(1,ix) + data(1)   
            zsum(9,ix) = zsum(9,ix) + amax1(data(1), data(5))   
            zsum(10,ix) = zsum(10,ix) + data(1) 
         else   
            zsum(3,ix) = zsum(3,ix) - data(1)   
            neggen = neggen + data(1)   
         endif  
         if (data(1) .gt. 0.0 .and. base(nb) .lt. 30.0) then
            zsum(11,ix) = zsum(11,ix) + data(6) 
            zsum(12,ix) = zsum(12,ix) + amax1(data(1), 0.0) 
            zsum(13,ix) = zsum(13,ix) + data(8) 
            zsum(14,ix) = zsum(14,ix) + amin1(data(2), 0.0) 
         endif  
         zsum(15,ix) = zsum(15,ix) + data(10)   
         zsum(16,ix) = zsum(16,ix) + data(14)   
         zsum(17,ix) = zsum(17,ix) + amax1(data(22), 0.0)   
         zsum(18,ix) = zsum(18,ix) + data(12)   
         zsum(19,ix) = zsum(19,ix) + data(16)   
         zsum(20,ix) = zsum(20,ix) + amin1(data(22), 0.0)   
         zsum(21,ix) = zsum(21,ix) + data(25)   
         zsum(22,ix) = zsum(22,ix) + data(26)   
         zsum(23,ix) = zsum(23,ix) + data(27)   
         zsum(24,ix) = zsum(24,ix) + data(28)   
         zsum(25,ix) = zsum(25,ix) + data(29)   
         zsum(26,ix) = zsum(26,ix) + data(30)   

C        Find Owner index. 

         iob = ownsch(owner(nb))
         if (iob .le. 0) then
            write (errbuf(1), 10170) owner(nb), bus(nb), base(nb)   
10170       format('0 Owner ', a2, ' for bus ', a8, f6.1,
     1         ' is not in system.')
            call prterx ('W',1) 
         else
c
c           Skip generation and load for d-c buses.
c
            if (ntypu(kt) .ne. 5 .and. ntypu(kt) .ne. 12) then
              if (data(6) .gt. 0.0) then
                q_ratio = data(2) / data(6)
              else
                q_ratio = 1.0
              endif            
              if (data(10) .gt. 0.0) then
                c_ratio = data(14) / data(10)
              else
                c_ratio = 1.0
              endif            
              if (data(12) .lt. 0.0) then
                r_ratio = data(16) / data(12)
              else
                r_ratio = 1.0
              endif            
              syst(1,iob) = syst(1,iob) + busdta(8,nb)
              syst(2,iob) = syst(2,iob) + q_ratio * busdta(9,nb)
              syst(3,iob) = syst(3,iob) + busdta(3,nb)
              syst(4,iob) = syst(4,iob) + busdta(4,nb)
              skcond = busdta(5,nb) * vk ** 2
              sksusp = busdta(6,nb) * vk ** 2
              skreak = amin1 (0.0, sksusp)
              skcap = amax1 (0.0, sksusp)
              if (ntypu(kt) .eq. 11) then
                 jxdta = busxdtptr(nb)
                 skreak = xdata(3, jxdta) * vk ** 2
                 skcap = xdata(4, jxdta) * vk ** 2
                 sksusp = skreak + skcap
              endif
              syst(5,iob) = syst(5,iob) + skcond
              syst(6,iob) = syst(6,iob) + sksusp
              syst(7,iob) = syst(7,iob) + skreak
              syst(8,iob) = syst(8,iob) + skcap
              syst_used(1,iob) = syst_used(1,iob) + r_ratio * skreak
              syst_used(2,iob) = syst_used(2,iob) + c_ratio * skcap
            endif
         endif  
       
         if (data(17) + data(18) + data(19) + data(20) .ne. 0.0) then
            lov = getownvlt (owner(nb), base(nb))
            if (lov .gt. 0) then
               ovlos(1,lov) = ovlos(1,lov) + data(19)
               ovlos(2,lov) = ovlos(2,lov) + data(20)

               ovlos(5,lov) = ovlos(5,lov) + data(17)
               ovlos(6,lov) = ovlos(6,lov) + data(18)
            endif
         endif
c
c        Loop through continuation buses
c
         ptr = kbsdta(15,nb) 
         do while (ptr .gt. 0) 
            call getchr (1, cbtype, kbctbl(8,ptr))
            call getchr (3, cbown, kbctbl(10,ptr))
            call getchr (2, cbyear, kbctbl(9,ptr))
            ioc = ownsch (cbown)
            if (ioc .le. 0) then
               write (errbuf(1), 10180) cbtype, cbown, bus(nb), base(nb)   
10180          format('0 Owner ', a2, ' for +bus ', a1, 1x, a8, f6.1,
     1            ' is not in system.')
               call prterx ('W',1) 
            else
               do i = 1, 6
                 lodtyp(i) = 0.0
               enddo
               pload2 = bctbl(2,ptr)
               qload2 = bctbl(3,ptr)
               skcon2 = bctbl(4,ptr) * vk ** 2
               sksus2 = bctbl(5,ptr) * vk ** 2
               pgen2 = bctbl(6,ptr)
               qgen2 = bctbl(11,ptr)
               if (cbtype .eq. 'A') then
                 if (cbyear .eq. '01' .or. cbyear .eq. '*I') then
                   pload2 = pload2*vk
                   qload2 = qload2*vk
                   lodtyp(3) = pload2
                   lodtyp(4) = qload2
                   if (cbown .eq. '***') then
                     skcon2 = 0.0
                     sksus2 = 0.0
                   else
                     lodtyp(5) = skcon2
                     lodtyp(6) =  - sksus2
                     pload2 = pload2 + skcon2
                     qload2 = qload2 - sksus2
                     skcon2 = 0.0
                     sksus2 = 0.0
                   endif
                 elseif (cbyear .eq. '02' .or. cbyear .eq. '*P') then
                   lodtyp(1) = pload2
                   lodtyp(2) = qload2
                   if (cbown .eq. '***') then
                     skcon2 = 0.0
                     sksus2 = 0.0
                   else
                     lodtyp(5) = skcon2
                     lodtyp(6) =  - sksus2
                     pload2 = pload2 + skcon2
                     qload2 = qload2 - sksus2
                     skcon2 = 0.0
                     sksus2 = 0.0
                   endif
                 else
                   lodtyp(1) = pload2
                   lodtyp(2) = qload2
                 endif
               elseif (cbyear .eq. '*I') then
                 pload2 = pload2*vk
                 qload2 = qload2*vk
                 lodtyp(3) = pload2
                 lodtyp(4) = qload2
                 if (cbown .eq. '***') then
                   skcon2 = 0.0
                   sksus2 = 0.0
                 else
                   lodtyp(5) = skcon2
                   lodtyp(6) =  - sksus2
                   pload2 = pload2 + skcon2
                   qload2 = qload2 - sksus2
                   skcon2 = 0.0
                   sksus2 = 0.0
                 endif
               elseif (cbyear .eq. '*P') then
                 lodtyp(1) = pload2
                 lodtyp(2) = qload2
                 if (cbown .eq. '***') then
                   skcon2 = 0.0
                   sksus2 = 0.0
                 else
                   lodtyp(5) = skcon2
                   lodtyp(6) =  - sksus2
                   pload2 = pload2 + skcon2
                   qload2 = qload2 - sksus2
                   skcon2 = 0.0
                   sksus2 = 0.0
                 endif
               else
                 lodtyp(1) = pload2
                 lodtyp(2) = qload2
               endif
               tot_pload = lodtyp(1) + lodtyp(3) + lodtyp(5)
               tot_qload = lodtyp(2) + lodtyp(4) + lodtyp(6)
               syst(1,ioc) = syst(1,ioc) + pgen2
               syst(2,ioc) = syst(2,ioc) + qgen2
               syst(3,ioc) = syst(3,ioc) + tot_pload
               syst(4,ioc) = syst(4,ioc) + tot_qload
               syst(5,ioc) = syst(5,ioc) + skcon2
               skreak = amin1 (0.0, sksus2)
               skcap = amax1 (0.0, sksus2)
               syst(6,ioc) = syst(6,ioc) + sksus2
               syst(7,ioc) = syst(7,ioc) + skreak
               syst(8,ioc) = syst(8,ioc) + skcap
               syst_used(1,ioc) = syst_used(1,ioc) + r_ratio * skreak
               syst_used(2,ioc) = syst_used(2,ioc) + c_ratio * skcap
            endif
            ptr = bctbl_nxt(ptr)
         enddo
         
C        Update line losses.

         ptr = kbsdta(16,nb)
         do while (ptr .gt. 0)
            ltype = brtype(ptr)
            if (ltype .eq. 1) then
               if (gtmetpnt (ptr) .eq. 2) then
                  call gtlfq (ptr, pin, qin, ploss, qloss, ovld, ratec,
     &                        actual_amps, whichend1, actual_mva, 
     &                        whichend2)
                  zsum(5,ix) = zsum(5,ix) + ploss 
                  zsum(6,ix) = zsum(6,ix) + qloss 
               endif
               oldptr = ptr
               ptr = brnch_nxt(ptr)
               do while (ptr .gt. 0 .and. 
     &                  (ky(ptr) .eq. ky(oldptr) .and.
     &                   brid(ptr) .eq. brid(oldptr)))
                  ltype = brtype(ptr)
                  if (gtmetpnt (ptr) .eq. 2) then
                     call gtlfq (ptr, pin, qin, ploss, qloss, ovld, 
     &                           ratec, actual_amps, whichend1, 
     &                           actual_mva, whichend2)
                     nbr = iabs(brnch_ptr(ptr))
                     call getchr (3, own, kbrnch(3,nbr))
                     k2 = ky(ptr)
                     basekv = amax1 (base(nb), base(k2))
                     lov = getownvlt (own, basekv)
                     if (lov .gt. 0) then
                        if (ltype .eq. BRTYP_T .or. 
     &                      ltype .eq. BRTYP_TP) then
                           ovlos(3,lov)=ovlos(3,lov)+ploss  
                           ovlos(4,lov)=ovlos(4,lov)+qloss  
                        else  
                           ovlos(1,lov) = ovlos(1,lov) + ploss  
                           ovlos(2,lov) = ovlos(2,lov) + qloss  
                        endif 
                     endif
                     io = ownsch (own)
                     if (io .gt. 0) then
                        syst(9,io) = syst(9,io) + ploss
                        syst(10,io) = syst(10,io) + qloss
                     endif
                  endif
                  ptr = brnch_nxt(ptr)
               enddo
            else if (ltype .eq. 4) then
               ptr = brnch_nxt(ptr)
            else
               if (gtmetpnt (ptr) .eq. 2) then
                  call gtlfq (ptr, pin, qin, ploss, qloss, ovld, ratec,
     &                        actual_amps, whichend1, actual_mva, 
     &                        whichend2)
                  zsum(5,ix) = zsum(5,ix) + ploss 
                  zsum(6,ix) = zsum(6,ix) + qloss 
                  nbr = iabs(brnch_ptr(ptr))
                  call getchr (3, own, kbrnch(3,nbr))
                  k2 = ky(ptr)
                  basekv = amax1 (base(nb), base(k2))
                  lov = getownvlt (own, basekv)
                  if (lov .gt. 0) then
                     if (ltype .eq. BRTYP_T .or. 
     &                   ltype .eq. BRTYP_TP) then
                        ovlos(3,lov)=ovlos(3,lov)+ploss  
                        ovlos(4,lov)=ovlos(4,lov)+qloss  
                     else  
                        ovlos(1,lov) = ovlos(1,lov) + ploss  
                        ovlos(2,lov) = ovlos(2,lov) + qloss  
                     endif 
                  endif
                  io = ownsch (own)
                  if (io .gt. 0) then
                     syst(9,io) = syst(9,io) + ploss
                     syst(10,io) = syst(10,io) + qloss
                  endif
               endif
               ptr = brnch_nxt(ptr)
            endif
         enddo
  200 continue  
      update(1) = 0 
      jowner = numown
      do i = 1, jowner
         lowner(i) = owner_o(i)
      enddo
      jvolt = numbases
      do i = 1, jvolt
         avolt(i) = basekvs(i)
      enddo
      call updt_own()
  900 return
      end   
