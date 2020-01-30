C    @(#)upd_zbol.f	20.3 2/13/96
        integer function upd_zbol(status)
        integer status
C
C****************************************************************
C
C       File: upd_zbol.f
C
C	Purpose: Routine to rebuild ZSUM and OVLOS arrays
C
C       Notes: This funtion is an interactive version of the original
C              batch version (updzon).  The interactive version 
C              preserves (1) the zbo linkage and (2) the special hash-
C              sort order for the zones and base kvs.
C
C       Author: Walt Powell  Date: 18 May 1992
C       Called by: p_gtdata.f
C
C****************************************************************
C
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/anlys.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/busanl.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/update.inc'
      include 'ipfinc/usranl.inc'
      include 'ipfinc/ownhash.inc'
      include 'ipfinc/bsekvhsh.inc'
      include 'ipfinc/brtype.inc'
      include 'ipfinc/zbo.inc'
 
      common /neggen/ neggen
      real neggen   

      logical opened
      real data(30) 
      integer flag, first, ptr, oldptr, open_file, getownvlt,
     &        gtmetpnt, whichend1, whichend2, find_zon, ownsch
      character own * 3, ratec * 10
c
c     Rebuild hash functions for zones, owners, and base kvs.
c
      status = buildzbo(status)
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

      jowner = numown      ! This counter is parallel to NUMOWN
      jvolt = numbases     ! This counter is parallel to NUMBASES
      novls = 0

      do 200 ix = 1, ntot_alf
         nb = alf2inp(ix)   
         kt = inp2opt(nb) 
         call nrpqv (kt, pk, dpk, qk, dqk, vk)   
         pgen = pk + ploadu(kt)                    
         qgen = qk + qloadu(kt)                    
         call busqua (nb, pgen, qgen, data) 

C        Find Zone index. 

         ll = find_zon(zone(nb))
         if (ll .le. 0) then
            write (errbuf(1),170) zone(nb), bus(nb), base(nb)   
  170       format('0 Zone ', a2, ' for bus ', a8, f6.1,
     1         ' is not in system.')
            call prterx ('W',1) 
            go to 200   
         endif  
         zsum(2,ll) = zsum(2,ll) + data(2)  
         zsum(3,ll) = zsum(3,ll) + data(3)  
         zsum(4,ll) = zsum(4,ll) + data(4)  
         zsum(5,ll) = zsum(5,ll) + data(17) + data(19)  
         zsum(6,ll) = zsum(6,ll) + data(18) + data(20)  
         zsum(7,ll) = zsum(7,ll) + data(9)  
         zsum(8,ll) = zsum(8,ll) + data(14) + data(16)  
         if (data(1) .gt. 0.0) then 
            zsum(1,ll) = zsum(1,ll) + data(1)   
            zsum(9,ll) = zsum(9,ll) + amax1(data(1), data(5))   
            zsum(10,ll) = zsum(10,ll) + data(1) 
         else   
            zsum(3,ll) = zsum(3,ll) - data(1)   
            neggen = neggen + data(1)   
         endif  
         if (data(1) .gt. 0.0 .and. base(nb) .lt. 30.0) then
            zsum(11,ll) = zsum(11,ll) + data(6) 
            zsum(12,ll) = zsum(12,ll) + amax1(data(1), 0.0) 
            zsum(13,ll) = zsum(13,ll) + data(8) 
            zsum(14,ll) = zsum(14,ll) + amin1(data(2), 0.0) 
         endif  
         zsum(15,ll) = zsum(15,ll) + data(10)   
         zsum(16,ll) = zsum(16,ll) + data(14)   
         zsum(17,ll) = zsum(17,ll) + amax1(data(22), 0.0)   
         zsum(18,ll) = zsum(18,ll) + data(12)   
         zsum(19,ll) = zsum(19,ll) + data(16)   
         zsum(20,ll) = zsum(20,ll) + amin1(data(22), 0.0)   
         zsum(21,ll) = zsum(21,ll) + data(25)   
         zsum(22,ll) = zsum(22,ll) + data(26)   
         zsum(23,ll) = zsum(23,ll) + data(27)   
         zsum(24,ll) = zsum(24,ll) + data(28)   
         zsum(25,ll) = zsum(25,ll) + data(29)   
         zsum(26,ll) = zsum(26,ll) + data(30)   

         if (data(17) + data(18) + data(19) + data(20) .ne. 0.0) then
            lov = getownvlt (owner(nb), base(nb))
            if (lov .gt. 0) then
               ovlos(1,lov) = ovlos(1,lov) + data(19)
               ovlos(2,lov) = ovlos(2,lov) + data(20)

               ovlos(5,lov) = ovlos(5,lov) + data(17)
               ovlos(6,lov) = ovlos(6,lov) + data(18)
            endif
         endif

C        Update line losses.

         ptr = kbsdta(16,nb)
         do while (ptr .gt. 0)
            ltype = brtype(ptr)
            if (ltype .eq. 1) then
               if (gtmetpnt (ptr) .eq. 2) then
                  call gtlfq (ptr, pin, qin, ploss, qloss, ovld, ratec,
     &                        actual_amps, whichend1, actual_mva, 
     &                        whichend2)
                  zsum(5,ll) = zsum(5,ll) + ploss 
                  zsum(6,ll) = zsum(6,ll) + qloss 
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
                  zsum(5,ll) = zsum(5,ll) + ploss 
                  zsum(6,ll) = zsum(6,ll) + qloss 
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
               endif
               ptr = brnch_nxt(ptr)
            endif
         enddo
  200 continue  
      update(1) = 0 
      jowner = numown
      jvolt = numbases
  900 upd_zbol = status
      return
      end   
