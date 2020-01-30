C    @(#)cutbr.f	20.9 10/13/99
      subroutine cutbr  

      include 'ipfinc/parametr.inc'  

      include 'ipfinc/blank.inc'   
      include 'ipfinc/cut.inc' 
      include 'ipfinc/bus.inc' 
      include 'ipfinc/cbus.inc'  
      include 'ipfinc/branch.inc'   
      include 'ipfinc/ikk.inc' 
      include 'ipfinc/red6.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/prt.inc' 
      include 'ipfinc/tbx.inc'
      include 'ipfinc/alpha.inc' 

      common /scratch/ scratch(3*MAXBUS), flag(MAXBUS), ndel1, 
     &                 lindel1(MAXBRN), ndel2, lindel2(MAXBRN2), 
     &                 ndel3, cbdel(MAXBUS), br_status(MAXBRN)
c
      complex * 16 y(2,2), oldy(2,2), v(2), s(2)
      integer flag, cbdel, br_status, ierr, ptr, oldptr, shift
      character id*1, kode*1, code*1, jd*1, cown*3, kown*3, bus1*8,
     &          bus2*8, fmt*80, kdyr*2, cdyr*2, codex*1, cdyrx*2, 
     &          cownx*3, error*5, kodey*1, kowny*3, kdyry*2
      real pi777(MAXPIBUS), total1(17), total2(17) 
      logical found, finished
      external kmpmps, swpmps
c
C     Description of arrays 
C       
C     Counter   Parameter Maximum   Array 
C       
C     KPIBAK     MAXPIBUS   1500    BUSPI(17,1500)
C     MPIBAK     MAXPIBRN   3000    PIBAK(3,3000) 
C     KPIPAR     MAXPIBRN   3000    PIPARL(7,3000)
C       
C     IKK assignments:  
C       
C     (1,*) --- 0/1 bus is not/is saved 
C     (2,*) --- index to CBCTBL 
C     (3,*) --- network number of saved bus 
C     (4,*) --- piback bus index in BUSPI ("0" designates   
C               a non-piback bus)   
C     (5,*) --- index to branch data in PIPARL  
C       
C     Initialize JBRSEQ 
C       
      do i = 1, MAXPIBRN
         jbrseq(i) = 0 
      enddo
      do i = 1,17   
         total1(i) = 0.0   
         total2(i) = 0.0   
      enddo
      do i = 1, MAXPIBUS
         pi777(i) = 0.0
      enddo
      do i = 1, ntot
         ikk(2,i) = 0  
      enddo

      do nb = 1, ntot
         ptrtbx(nb)  = 0
      enddo

      do i = 1, ntotb
         kxd = tbx(2,i)
         if (kxd .gt. 0) ptrtbx(kxd) = i
      enddo

      do i = 1, MAXBRN
         br_status(i) = 0
      enddo

      ndel1 = 0
      do i = ltot+1, MAXBRN
         ndel1 = ndel1 + 1
         lindel1(ndel1) = i
      enddo

      ndel2 = 0
      do i = ltot2+1, MAXBRN2
         ndel2 = ndel2 + 1
         lindel2(ndel2) = i
      enddo
C
      jbrt = 0  
      jcbt = 0  
      nsyst = 0 
      ncbadd = 0
C       
C     Process buses according to type:  
C       
C     Saved bus --- "IKK(1,*)" = 1  AND "IKK(4,*)" = 0  
C     Piback bus -- "IKK(1,*)" = 1  AND "IKK(4,*)" > 0  
C     Cutout bus -- "IKK(1,*)" = 0  
C       
      do 520 nb = 1,ntot
      kt = inp2opt(nb)
      if (ikk(1,nb) .eq. 0) go to 520   
C       
C     A saved bus (non-PIBACK) has its branches examined for cutting
C     or piback.  In either case the branches are removed.  
C       
      i4 = ikk(4,nb)
      if (i4 .eq. 0) go to 280  
C       
C     All bus data for the PI-BACK bus is disseminated into array   
C     EBUSPI.   
C       
      if (kdebug.eq.0) then 
         write (9,120) bus(nb),base(nb),(ikk(j,nb),j=1,5)   
  120    format(' PI-BACK BUS  ',a8,f7.1,' IKK =',5i6)  
      endif 
      pi777(i4) = 1.0   
      kbuspi(1,i4) = nb 
      kbuspi(2,i4) = 0  
      kbuspi(3,i4) = 0  
      vk = dsqrt(e(kt)**2 + f(kt)**2)
      pload = (ploadu(kt) + vk*inetr(kt)) * bmva 
      qload = (qloadu(kt) - vk*ineti(kt)) * bmva 
      buspi(10,i4) = pload - sngl(dmin1 (0.0d0, pnetu(kt)*bmva + pload))
      buspi(11,i4) = qload
      pgen = sngl(dmax1(0.0d0,pnetu(kt)*bmva + pload))     
      qgen  = qnetu(kt)*bmva + qload               
      buspi(8,i4) = pgen
      buspi(9,i4) = qgen
      skcond = busdta(5,nb) 
      sksusp = busdta(6,nb) 
      ktype = kbsdta(1,nb)  
      qmax = busdta(9,nb)   
      qmin = busdta(10,nb)  
      ncb = kbsdta(15,nb)   
      do while (ncb .gt. 0)
         skcond = skcond + bctbl(4,ncb)
         sksusp = sksusp + bctbl(5,ncb)
         ncb = bctbl_nxt(ncb)
      enddo
      voltsq =  e(kt)**2 + f(kt)**2 
      skcond = skcond*voltsq
      sksusp = sksusp*voltsq
      skcap = amax1(0.0,sksusp) 
      skrek = amin1(0.0,sksusp) 
      if (sksusp.eq.0.0) go to 270  
      ityp = 1  
C       
C     Removed "unused" compensation from bus susceptance.   
C       
      go to (270,150,150,270,270,160,160,160,160,270,270,270,270) ktype 

  150 qgen = qgen + capcor(1,kt)*voltsq*bmva
      sksusp = sksusp - capcor(1,kt)*bmva   
      go to 270 

  160 itbx = ptrtbx(nb)
      ltyp = tbx(1,itbx)   
      ityp = tbx(7,itbx)   

      go to (220,240,230,240) ltyp  
  220 go to (270,270,150,150) ityp  
  230 go to (150,150,150,270,270) ityp  
  240 go to (150,150,250,260) ityp  
  250 sksusp = amin1(0.0,sksusp)
      go to 270 
  260 sksusp = amax1(0.0,sksusp)
  270 continue

      buspi(9,i4) = qgen
      buspi(12,i4) = skcond 
      buspi(13,i4) = sksusp 

  280 continue
C       
C     Process branch data   
C       
      ptr = kbsdta(16,nb)   
      ln1 = 0   
      ln2 = 0   
      do while (ptr .gt. 0)
         ltype = brtype(ptr)
         k1 = kx(ptr)
         k2 = ky(ptr)
         id = brid(ptr)
         mt = inp2opt(k2)
         if (ikk(1,k2) .eq. 0) then
            kutsw = 4  
         else if (ikk(4,nb) .gt. 0) then   
            kutsw = 2  
         else if (ikk(4,k2) .gt. 0) then   
            kutsw = 3  
         else  
            kutsw = 1  
         endif 
C       
C        KUTSW defines action to be taken with current branch  
C
C          1 --- Saved branch from saved bus; leave intact   
C          2 --- Pi-back branch from pi-back bus; add data to EBRNCH 
C          3 --- Pi-back branch from saved bus; delete branch
C          4 --- Cut branch;  compute line mva quantities
C       
         if (kutsw .eq. 1) then
C       
C           Retained branch from retained bus; advance pointers.   
C       
            do while (ptr .gt. 0 .and. (ky(ptr) .eq. k2))
               ptr = brnch_nxt(ptr)
            enddo

         else if (kutsw .eq. 2) then   
C       
C           Pi-Back branch from Pi-back bus; examine and   
C           consolidate any parallels. 
C       
            ipar = 0   
            oldptr = ptr
            do while (ptr .gt. 0 .and. (ky(ptr) .eq. k2))
               ltype = brtype(ptr)
               if (ltype .eq. 2 .or. ltype .eq. 7) then
                  write (errbuf(1),320) bus(k1), base(k1), bus(k2),
     &               base(k2)  
  320             format ('0 Illegal Pi-back to d-c branch ',
     1               a8,f7.1,2x,a8,f7.1)  
                  call prterx ('F',1)  
               else if (ltype .ne. 4) then 
                  if (brsect(ptr) .eq. 0) ipar = ipar + 1 
               endif   
               ptr = brnch_nxt(ptr)
            enddo
            ptr = oldptr 
            if (ipar .gt. 1) then  
C       
C              Consolidate parallels  
C       
               do i = 1,2  
                  do j = 1,2  
                     oldy(j,i) = dcmplx (0.0d0,0.0d0) 
                  enddo
               enddo
               rate = 0.0  
               noline = 0  
               do while (ptr .gt. 0 .and. (ky(ptr) .eq. k2))
                  ltype = brtype(ptr)
                  if (ltype .ne. 2 .and. ltype .ne. 4 .and. 
     &                ltype .ne. 7) then
                     nbr = iabs (brnch_ptr(ptr))
                     if (brsect(ptr) .eq. 0) then 
                        call pieqiv (ptr,y,ierr)   
                        do j = 1,2 
                           do k = 1,2 
                              oldy(k,j) = oldy(k,j) + y(k,j) 
                           enddo
                        enddo
                        noline = noline + kbrnch(16,nbr) 
                        if (ltype .eq. 5 .or. ltype .eq. 6) then   
                           rate = rate + 577.3502692*brnch(4,i) /   
     1                                   amax1(base(k1),base(k2)) 
                        else   
                           rate = rate + brnch(4,i)
                        endif  
                     endif 
                  endif
                  if (ptr .ne. oldptr) then
                     kx(ptr) = 0
                     ky(ptr) = 0
                     ndel2 = ndel2 + 1
                     lindel2(ndel2) = ptr
                     ptr = brnch_nxt(ptr)
                     br_status(nbr) = br_status(nbr) + 1
                     if (br_status(nbr) .eq. 2) then
                        ndel1 = ndel1 + 1
                        lindel1(ndel1) = nbr
                     endif
                  endif
               enddo
C       
C              Delete parallels by "unlinking"
C       
               brnch_nxt(oldptr) = ptr
 
               ptr = oldptr 
               nbr = iabs (brnch_ptr(ptr))
               brtype(ptr) = 8
               call putchr (3,'***',kbrnch(3,nbr))  
               brnch(4,nbr) = rate  
               brnch(5,nbr) = dreal (-1.0d0/oldy(1,2)) 
               brnch(6,nbr) = dimag (-1.0d0/oldy(1,2))
               brnch(7,nbr) = dreal (oldy(1,1) + oldy(1,2))  
               brnch(8,nbr) = dimag (oldy(1,1) + oldy(1,2)) 
               brnch(9,nbr) = dreal (oldy(2,1) + oldy(2,2))  
               brnch(10,nbr) = dimag (oldy(2,1) + oldy(2,2))
               kbrnch(11,nbr) = 0   
               brid(ptr) = '*'
               brsect(ptr) = 0   
               kbrnch(15,nbr) = 0   
               kbrnch(16,nbr) = noline  
            endif
C       
C           Add branch to EBRNCH
C       
            if (ln1 .eq. 0) then
               ln1 = k2 
            else
               ln2 = k2 
            endif   
            j4 = ikk(4,k2)  
            if (j4 .gt. 0 .and. k1 .gt. k2) then
C       
C              Branch K1 - K2 spans two Pi-back buses and   
C              has already been added to EBRNCH.
C       
               if (kbuspi(2,j4) .eq. k1) then   
                  jstart = kbuspi(4,j4) 
                  jstop = kbuspi(5,j4)  
               else if (kbuspi(3,j4) .eq. k1) then  
                  jstart = kbuspi(6,j4) 
                  jstop = kbuspi(7,j4)  
               else 
                  call erquit   
               endif
               if (ln2 .eq. 0) then 
                  kbuspi(2,i4) = ln1
                  kbuspi(4,i4) = jstop  
                  kbuspi(5,i4) = jstart 
               else 
                  kbuspi(3,i4) = ln2
                  kbuspi(6,i4) = jstop  
                  kbuspi(7,i4) = jstart 
               endif
            else
C       
C              Add branch to EBRNCH  
C       
               if (ln2 .eq. 0) then  
                  kbuspi(2,i4) = ln1 
                  kbuspi(4,i4) = jbrt+1  
               else  
                  kbuspi(3,i4) = ln2 
                  kbuspi(6,i4) = jbrt+1  
               endif 
               jbrseq(jbrt+2) = 19999 
               oldptr = ptr
               do while (ptr .gt. 0 .and. (ky(ptr) .eq. k2))
                  ltype = brtype(ptr)
                  if (ltype .ne. 1 .and. ltype .ne. 2 .and. 
     &                ltype .ne. 4 .and. ltype .ne. 7) then 
                     jbrt = jbrt + 1
                     if (jbrt .gt. MAXPIBRN-2) then   
                        write (errbuf(1),400) MAXPIBRN   
  400                   format (' More than ', i5, 
     &                     ' Pi-back branches processed')   
                        call prterx ('F',1) 
                        call erquit 
                      endif  
                      nbr = iabs (brnch_ptr(ptr))
                      kbrnch(17,nbr) = 2   
                      do j = 1,18
                         jbrnch(j,jbrt) = kbrnch(j,nbr)   
                         jbrnch(j,jbrt+1) = kbrnch(j,nbr) 
                      enddo
                      jbrnch(1,jbrt) = brtype(ptr)
                      jbrnch(2,jbrt) = kx(ptr)
                      jbrnch(12,jbrt) = ky(ptr)
                      call putchr (1, brid(ptr), jbrnch(13,jbrt))
                      jbrnch(14,jbrt) = brsect(ptr)
                      jbrt = jbrt + 1
C       
C                     Transpose JBRT 
C       
                      jbrnch(2,jbrt) = k2
                      jbrnch(12,jbrt) = k1   
                      jbrnch(15,jbrt) = mod (3-jbrnch(15,jbrt),3)
                      if (ltype .eq. 5) then 
                         ebrnch(9,jbrt) = ebrnch(10,jbrt-1)  
                         ebrnch(10,jbrt) = ebrnch(9,jbrt-1)  
                      else if (ltype .eq. 6) then
                         ebrnch(9,jbrt) = -ebrnch(9,jbrt-1)  
                      else if (ltype .eq. 8) then
                         ebrnch(7,jbrt) = ebrnch(9,jbrt-1)   
                         ebrnch(8,jbrt) = ebrnch(10,jbrt-1)  
                         ebrnch(9,jbrt) = ebrnch(7,jbrt-1)   
                         ebrnch(10,jbrt) = ebrnch(8,jbrt-1)  
                     endif  
                     if (jbrseq(jbrt).eq.0) jbrseq(jbrt) = jbrt-2   
                  endif 
		  oldptr = ptr
                  ptr = brnch_nxt(ptr)
               enddo
               ptr = oldptr
               jbrseq(jbrt-1) = 19999 
               if (ln2 .eq. 0) then  
                  kbuspi(5,i4) = jbrt
               else  
                  kbuspi(7,i4) = jbrt
               endif 
            endif   
            ptr = brnch_nxt(ptr)
C       
         else if (kutsw .eq. 3) then   
C       
C           Pi-back branch from retained bus; advance pointers.
C       
            do while (ptr .gt. 0 .and. (ky(ptr) .eq. k2))
                ptr = brnch_nxt(ptr)
            enddo

         else if (kutsw .eq. 4) then   
C       
C           Compute injected line flow   
C       
            ai = cmplx(0.0,0.0)
            do j = 1,2 
               do k = 1,2 
                  oldy(k,j) = dcmplx(0.0d0,0.0d0) 
               enddo
            enddo

            do while (ptr .gt. 0 .and. (ky(ptr) .eq. k2))
               ltype = brtype(ptr)
               if (ltype .ne. 2 .and. ltype .ne. 4 .and. 
     &             ltype .ne. 7) then
                  if (brsect(ptr) .eq. 0) then   
                     call pieqiv(ptr,y,ierr)
                     do j = 1,2   
                        do k = 1,2   
                           oldy(k,j) = oldy(k,j) + y(k,j)   
                        enddo
                     enddo
                  endif   
               endif
               ptr = brnch_nxt(ptr)
            enddo

            v(1) = dcmplx(e(kt),f(kt))  
            v(2) = dcmplx(e(mt),f(mt))  
            s(1) = oldy(1,1)*v(1) + oldy(1,2)*v(2)
            s(1) = v(1)*dconjg(s(1))  
            pin = dreal (s(1)) * bmva  
            qin = dimag (s(1)) * bmva 
            if (kdebug.gt.0) then  
               write (9,470) bus(k1),base(k1),bus(k2),base(k2),id, 
     1            pin,qin 
  470          format(' CUT BRANCH ',a8,f7.1,' TO ',a8,f7.1,' PAR ',a1,
     1            ' COMPUTED LINE FLOW ',2f10.3) 
            endif  
            if (nwscc .ne. 0) then  
               pgen = 0.0
               pload = pin   
            else   
               pgen = -amin1(0.0,pin)   
               pload = amax1(0.0,pin)   
            endif  
            qgen=0.0   
            qload = qin
            if (i4 .gt. 0) then
               buspi(8,i4) = buspi(8,i4) + pgen
               buspi(10,i4) = buspi(10,i4) + pload 
               buspi(11,i4) = buspi(11,i4) + qload 
            else   
C       
C              Branch injection quantities are added to a continuation 
C              bus card type "A".  
C       
               total1(8) = total1(8) + pgen
               total1(9) = total1(9) + qgen
               total1(10) = total1(10) + pload 
               total1(11) = total1(11) + qload 
               kown = '***'
               if (nwscc .ne. 0) then  
                  do 480 i = 1, kint   
                  if (k1 .eq. intflg(1,i) .and. k2 .eq. intflg(2,i)) 
     &               then  
                     kown = 'INT'  
                     goto 490  
                  endif
  480             continue 
  490             continue 
               endif   
C       
C              Examine + Bus record
C       
               ncb = kbsdta(15,nb) 
               do while (ncb .gt. 0)
                  call getchr (1,kode,kbctbl(8,ncb))  
                  if (kode .eq. 'A') then   
                     call getchr (3,cown,kbctbl(10,ncb))  
                     if (kown .eq. cown) then   
                        bctbl(2,ncb) = bctbl(2,ncb) + pload 
                        bctbl(3,ncb) = bctbl(3,ncb) + qload 
                        bctbl(6,ncb) = bctbl(6,ncb) + pgen  
                        bctbl(11,ncb) = bctbl(11,ncb) + qgen
                        goto 510
                     endif  
                  endif 
                  ncb = bctbl_nxt(ncb)
               enddo
            endif   
            if (jcbt .gt. 0) then   
               if (ikk(2,nb) .gt. 0) then   
                  j = ikk(2,nb) 
                  do j = ikk(2,nb), jcbt
                     k = jbctbl(1,j)   
                     call getchr (3,cown,jbctbl(10,j)) 
                     if (k .eq. nb .and. cown .eq. kown) then  
                        ebctbl(2,j) = ebctbl(2,j) + pload  
                        ebctbl(3,j) = ebctbl(3,j) + qload  
                        ebctbl(6,j) = ebctbl(6,j) + pgen   
                        ebctbl(11,j) = ebctbl(11,j) + qgen 
			goto 510                        
                     endif 
                  enddo
               endif
  502          continue 
            endif   
            jcbt = jcbt + 1 
            if (ikk(2,nb) .eq. 0) ikk(2,nb) = jcbt  
            jbctbl(1,jcbt) = nb 
            ebctbl(2,jcbt) = pload  
            ebctbl(3,jcbt) = qload  
            ebctbl(4,jcbt) = 0.0
            ebctbl(5,jcbt) = 0.0
            ebctbl(6,jcbt) = pgen   
            ebctbl(11,jcbt) = qgen  
            ebctbl(12,jcbt) = 0.0
            jbctbl(7,jcbt) = 0  
            call putchr (1,'A',jbctbl(8,jcbt))  
            call putchr (2,'00',jbctbl(9,jcbt)) 
            call putchr (3,kown,jbctbl(10,jcbt))
  510       continue
         endif
      enddo
  520 continue  
  530 continue  

      if (kdebug .gt. 0) write (9,532) (total2(i),i=8,13)   
  532 format ('0 TOTAL ',33x,6f8.2) 
      write (outbuf,534) (total1(i),i=8,13) 
  534 format ('0 TOTAL CUT INJECTIONS - GENERATION ',2f10.1,' LOAD ',   
     1  2f10.1,' SHUNT ',2f10.1)
      call prtout (1)   
      write (outbuf,536) (total2(i),i=8,13) 
  536 format('0 TOTAL PI-BACK        - GENERATION ',2f10.1,' LOAD ',
     1  2f10.1,' SHUNT ',2f10.1)
      call prtout (1)   
        
      mpibak = 0
      kpipar = 0
      ncbadd = jcbt 
C       
C     NCBADD entities in JCBTBL/EBCTBL are created from cutting.
C     Later, these quantites will be appended to the pi-back bus
C     quantities.   
C       
      if (kpibak.eq.0) go to 1290   
      if (kdebug .gt .0) then
         write (9,540)
  540    format('1 INITIAL DEBUG DUMP OF "BUSPI" ARRAY '//)
         do k=1,kpibak 
            k1 = kbuspi(2,k)  
            k2 = kbuspi(3,k)  
            write (9,550) k,(kbuspi(i,k),i=1,7),   
     1        (buspi(i,k),i=8,13),kbuspi(14,k),buspi(15,k),kbuspi(16,k),
     2        buspi(17,k)   
  550       format(1x,8i5,6f8.2,i5,f7.3,i5,f7.3,1x,a5)
         enddo
      endif

      do k=1,kpibak 
         k1 = kbuspi(2,k)  
         k2 = kbuspi(3,k)  
         do i = 8,13   
            total2(i) = total2(i) + buspi(i,k)
         enddo
         mpsort(1,2*k-1) = ipack_2 (k1, k2)
         mpsort(2,2*k-1) = k   
         mpsort(1,2*k) = ipack_2 (k2, k1)
         mpsort(2,2*k) = k 
      enddo

      kpibk2 = 2*kpibak 

      call qiksrt (1,kpibk2,kmpmps,swpmps)  

      do k = 1,ntot  
         ikk(3,k) = 0  
      enddo
      if (kdebug .gt. 0) then   
         write (9,580)  
  580    format('1 DEBUG DUMP OF PI-BACK BRANCHES'//)   
         do 650 i = 1,kpibak
         nb = kbuspi(1,i)   
         write (9,590) nb,bus(nb),base(nb)  
  590    format('0 PI-BACK BRANCHES FOR BUS ',i4,2x,a8,f7.1//)  
         do ksw = 1,2   
            if (ksw .eq. 1) then   
               if (kbuspi(2,i) .gt.0) then 
                  jebr1 = kbuspi(4,i)  
                  jebr2 = kbuspi(5,i)  
               else
                  go to 640
               endif   
            else   
               if (kbuspi(3,i) .gt.0) then 
                  jebr1 = kbuspi(6,i)  
                  jebr2 = kbuspi(7,i)  
               else
                  go to 640
               endif   
            endif  
            do lsw = 1,2   
               if (lsw .eq. 1) then   
                  jebr = jebr1
               else
                  jebr = jebr2
               endif  
               if (jebr .lt. 19999) then
                  ltype = jbrnch(1,jebr) 
                  k1 = jbrnch(2,jebr)
                  k2 = jbrnch(12,jebr)   
                  call getchr (1,id,jbrnch(13,jebr)) 
                  nxsect = jbrnch(14,jebr)   
                  if (lsw .eq. 1) then
                     write (outbuf, 610) jebr, ltype, bus(k1), base(k1),
     1                  bus(k2), base(k2), id, nxsect 
  610                format(2i6,2x,a8,f7.1,2x,a8,f7.1,a3,i3)
                     write (9, 620) outbuf
  620                format (a)
                  else
                     write (outbuf(63:), 610) jebr, ltype, bus(k1), 
     &                  base(k1), bus(k2), base(k2), id, nxsect 
                     write (9, 620) outbuf
                  endif
                  if (lsw .eq. 1) then   
                     jebr1 = jbrseq(jebr1)
                  else   
                     jebr2 = jbrseq(jebr2)
                  endif  
               else
                  go to 640
               endif
            enddo
  640       continue   
         enddo
  650    continue   
      endif 
        
      do 760 k = 1, kpibk2  
         kt = k
         k1 = shift (mpsort(1,kt), -16)
         k2 = shift (shift (mpsort(1,kt), 16), -16)
         k3 = mpsort(2,kt) 
         if (k1.eq.0) go to 760
C       
C        START FROM SAVED BUSES ONLY   
C       
         if (ikk(1,k1) .ne. 1 .or. ikk(4,k1) .gt. 0) go to 760 
         i3 = ikk(3,k1)
         mpsort(1,kt) = 0  
         if (i3.eq.0) then 
            i3 = kpipar + 1
            j3 = i3
            ikk(3,k1) = i3 
         else  
            j3 = i3
            do while (lpiprl(7,j3) .gt. 0) 
               j3 = lpiprl(7,j3)   
            enddo
            lpiprl(7,j3) = kpipar + 1  
         endif 
         kpipar = kpipar + 1   
         if (kpipar .gt. MAXPIBUS) then
            write (errbuf(1),670) MAXPIBUS
  670       format (' Overflow of array LPIPRL, limit is ', i5)
            call prterx ('F',1)
            go to 9000
         endif 
         lpiprl(1,kpipar) = k1 
         lpiprl(2,kpipar) = 0  
         lpiprl(3,kpipar) = 0  
         xtot = 0.0
         lpiprl(4,kpipar) = mpibak + 1 
         mpibak = mpibak + 1   
         if (mpibak .gt. MAXPIBACK) then
            write (errbuf(1),680) MAXPIBACK
  680       format (' Overflow of array LPIBAK, limit is ', i5)
            call prterx ('F',1)
            go to 9000
         endif 
         pibak(1,mpibak) = 0.0 
         lpibak(2,mpibak) = k1 
         lpibak(3,mpibak) = 0  
         if (k1 .eq. kbuspi(2,k3)) then
            jebr = kbuspi(5,k3)
            kbuspi(14,k3) = k1 
            k2x = kbuspi(3,k3) 
            ktrpos = 1 
         else if (k1.eq.kbuspi(3,k3)) then 
            jebr = kbuspi(7,k3)
            kbuspi(16,k3) = k1 
            k2x = kbuspi(2,k3) 
            ktrpos = 2 
         else  
            write (errbuf(1), 682) bus(k1), base(k1)
  682       format (' Pi-back link error - bus ', a8, f7.1)
            call prterx ('F',1)
            go to 9000
         endif 
C       
C        KSW = 1 processing K1 into pi-back bus
C        KSW = 2 processing pi-back bus into K2
C       
C        Begin processing branches at K1 and end either at K2  
C        which will eventually become a saved bus or at 0, which   
C        indicates a dead end pi-back. 
C       
         lpiprl(5,kpipar) = jebr   
         k1l = k1  
         ksw = 1   

         n = 1 
         finished = .false.
         do while (.not. finished)
            if (jebr .eq. 0) then
               write (errbuf(1), 690) bus(k1), base(k1)
  690          format (' Pi-back branch link error at bus ', a8, f7.1)
               call prterx ('F', 1)
               go to 9000
            endif
            do while (jebr .ne. 19999)
C       
C              Compute total series reactance via PIEQIV. Note that
c              JBRNCH is loaded at ltot+1 to utilize PIEQIV
c
               brtype(ltot2+1) = jbrnch(1,jebr)
               kx(ltot2+1) = jbrnch(2,jebr)
               ky(ltot2+1) = jbrnch(12,jebr)
               brsect(ltot2+1) = jbrnch(14,jebr)
               brnch_ptr(ltot2+1) = ltot+1
               do j = 1,18   
                  kbrnch(j,ltot+1) = jbrnch(j,jebr) 
               enddo
               call pieqiv (ltot2+1,y,ierr)   
               if (n .eq. 1) then
                  call firsecd (y)
                  n = 2  
               else  
                  call nexsecd (y)
               endif 
               jebr = jbrseq(jebr)   
            enddo
            call finsecd (y)   
            n = 1 
            xtot = xtot - sngl(dimag(1.0d0/y(1,2)))
            mpibak = mpibak + 1   
            if (mpibak .gt. MAXPIBACK) then
               write (errbuf(1),680) MAXPIBACK
               call prterx ('F',1)
               call erquit
            endif 
            if (mod(ksw,2) .eq. 1) then
               pibak(1,mpibak) = xtot
               lpibak(2,mpibak) = -kbuspi(1,k3)  
               lpibak(3,mpibak) = 0  
               ksw = ksw + 1 
               if (k2.eq.0) then 
                  lpibak(3,mpibak) = 19999
                  finished = .true.
               else
                  if (ktrpos .eq. 1) then   
                     jebr = kbuspi(6,k3)
                  else  
                     jebr = kbuspi(4,k3)
                  endif 
               endif
            else
               pibak(1,mpibak) = xtot
               if (ikk(1,k2x) .eq. 0) call erquit
               if (ikk(4,k2x) .eq. 0) then   
                  lpibak(2,mpibak) = k2x 
                  lpibak(3,mpibak) = 19999
                  finished = .true.
               else
                  lpibak(2,mpibak) = -k2x   
                  lpibak(3,mpibak) = 0  
                  if (ktrpos .eq. 1) then   
                     kbuspi(14,k3) = k1l
                  else  
                     kbuspi(16,k3) = k1l
                  endif 
C       
C                 Process cascading pi-back busses  
C       
                  if (ksw .eq. 2) kold = kt 
                  do 730 i=1,kpibk2 
                     l1 = shift (mpsort(1,i), -16)
                     l2 = shift (shift (mpsort(1,i), 16), -16)
                     l3 = mpsort(2,i)  
                     if (l1 .eq. kbuspi(1,k3)) then
                        if (kbuspi(1,l3).eq.k2x) go to 740 
                     endif 
  730             continue  
                  call erquit   
  740             kt = i
                  k1x = kbuspi(1,k3)
                  k1 = shift (mpsort(1,kt), -16)
                  k2 = shift (shift (mpsort(1,kt), 16), -16)
                  k3 = mpsort(2,kt) 
                  mpsort(1,kt) = 0  
                  if (k1x .eq. kbuspi(2,k3)) then   
                     jebr = kbuspi(6,k3)
                     k2x = kbuspi(3,k3) 
                     ktrpos = 1 
                  else if (k1x .eq. kbuspi(3,k3)) then  
                     jebr = kbuspi(4,k3)
                     k2x = kbuspi(2,k3) 
                     ktrpos = 2 
                  else  
                     call erquit
                  endif 
                  ksw = ksw + 2 
                  if (k2 .eq. 0) then
                     lpibak(3,mpibak) = 19999   
                     finished = .true.
                  endif
               endif
            endif
         enddo
         if (ktrpos.eq.2) then 
            kbuspi(14,k3) = k2x
            kbuspi(16,k3) = k1l
         else  
            kbuspi(16,k3) = k2x
            kbuspi(14,k3) = k1l
         endif 
         lpiprl(2,kpipar) = k2x
         piparl (6,kpipar) = xtot  
  760 continue  
C       
C     1. Sort LPIPRL by K1,K2 (alpha instead of numeric)
C
C     2. Relable parallel Nos in both PIPARL and LPIBAK, match K1-K2
C        with   
C       
C     3. Sort again by K1,K2, and parallel  
C       
      if (kdebug .gt. 0) then   
         write (9,770)  
  770    format('1 DEBUG DUMP OF "PIBAK" ARRAY '//) 
         do k = 1,mpibak
            write (9,780) k,pibak(1,k),(lpibak(j,k),j=2,3) 
  780       format(i6,f10.3,2i6)   
            i = iabs(lpibak(2,k))  
            if (i.gt.0) write (9,790) bus(i),base(i)   
  790       format('+',32x,a8,f7.1)
         enddo
         write (9,810)  
  810    format('1  DEBUG DUMP OF "PIPRL" ARRAY '//)   
         do k=1,kpipar 
            mpsort(1,k) = ipack_2 (lpiprl(1,k), lpiprl(2,k))
            mpsort(2,k) = k   
            write (9,820) k,(lpiprl(j,k),j=1,5), piparl(6,k),
     &         lpiprl(7,k) 
  820       format(3i6,2x,i6,2i6,f8.3,i6) 
         enddo
      endif

      do k=1,kpipar 
         mpsort(1,k) = ipack_2 (lpiprl(1,k), lpiprl(2,k))
         mpsort(2,k) = k   
      enddo

      call qiksrt (1,kpipar,kmpmps,swpmps)  

      k1old = 0 
      k2old = 0 
      do 880 j=1,kpipar 
      k1 = shift (mpsort(1,j), -16)
      k2 = shift (shift (mpsort(1,j), 16), -16)
      k = mpsort(2,j)   
      if (k1.eq.0) go to 880
      if (k2.eq.0) goto 850 
      if (k1 - k2) 850,840,880  
  840 if (shift (k, -16) .ne. 0) go to 880 
  850 continue  
      if (k1 .eq. k1old .and. k2 .eq. k2old) then   
         kpar = kpar + 1
      else  
         k1old = k1 
         k2old = k2 
         kpar = 1   
      endif 
      lpiprl(3,k) = ipack_2 (kpar, 0)
      mpsort(1,j) = ipack_2 (k1, k2)
      mpsort(2,j) = ipack_2 (kpar, k)
      if (k2.eq.0) go to 880
      kpib = lpiprl(4,k) + 1
      do while (lpibak(2,kpib) .lt. 0 .and. lpibak(3,kpib) .lt. 19999) 
         kmatch = lpibak(2,kpib)
         kpib = kpib + 1   
      enddo
C       
C       RELABEL ID FOR TRANSPOSE ALSO   
C       
      do 870 i = j+1,kpipar 
      j1 = shift (mpsort(1,i), -16)
      j2 = shift (shift (mpsort(1,i), 16), -16)
      l = mpsort(2,i)   
      l = shift (shift(l, 16), -16)
      if (j2.eq.k1.and.j1.eq.k2) then   
         j1 = lpiprl(4,l)   
         j1 = j1 + 1
         if (lpibak(2,j1) .eq. kmatch) then
            lpiprl(3,l) = ipack_2 (kpar, 0)
            mpsort(1,i) = ipack_2 (k2, k1)
            mpsort(2,i) = ipack_2 (kpar, 1)
            go to 880  
         endif
      endif 
  870 continue  
      call erquit   
  880 continue  
      call qiksrt(1,kpipar,kmpmps,swpmps)   
      if (kdebug .gt. 0) then   
         write (9,890)  
  890    format('1 DEBUG DUMP OF SORTED "PIPRL" ARRAY '//)  
         do 910 i = 1,kpipar
         k1 = shift (mpsort(1,i), -16)
         k2 = shift (shift (mpsort(1,i), 16), -16)
         k3 = mpsort(2,i)   
         bus1 = ' ' 
         bus2 = ' ' 
         base1 = 0.0
         base2 = 0.0
C       
C        PARALLELS ARE BIASED '5'   
C       
         j = ichar('5') + shift (k3, -16) - 1
         id = char(j)   
         k = shift (shift (k3, 16), -16)
         if (k1 .gt. 0) then
            bus1 = bus(k1)  
            base1 = base(k1)
         endif  
         if (k2 .gt. 0) then
            bus2 = bus(k2)  
            base2 = base(k2)
         endif  
         write (9,900)  k,bus1,base1,bus2,base2,id,(lpiprl(j,k),j=4,5), 
     1    piparl(6,k),lpiprl(7,k)   
  900    format(i6,2x,a8,f7.1,2x,a8,f7.1,a3,2i6,f10.3,i6)   
  910    continue   
      endif 
C       
C     Branch data pass No. 1:   
C
C       1. Process MPSORT in sort order.
C       2. Link up branches via KBUSPI array.   
C       3. Insert pi-back percentage into BUSPI array.  
C       4. Check for cascading sections in excess of 9 sections.
C          If true, pi-back then into a single equivalent.  
C       5. Relable branch data contents, mask out any residual RV LTC's,
C          insert new label.
C       6. Count the number of sections, and store in LPIPRL.   
C            a. If K1.LT.K2, insert sections into PIPARL.   
C            b. If K2.LT.K1, search for section in PIPARL.  
C       
      do 1090 j = 1,kpipar  
      k1 = shift (mpsort(1,j), -16)
      k2 = shift (shift (mpsort(1,j), 16), -16)
      k3 = shift (mpsort(2,j), -16)
      if (k1.eq.0) go to 1090   
      i4 = ikk(4,k1)
      if (i4.gt.0) go to 1090   
      k = shift (shift (mpsort(2,j), 16), -16)
      kpib = lpiprl(4,k)
      jebr = lpiprl(5,k)
      totx = piparl(6,k)
      l = shift (lpiprl(3,k), -16) + ichar('5') - 1  
      id = char(l)  
      if (k2.eq.0) go to 940
      if (k1 - k2) 940,920,950  
C       
C       SPECIAL PROCESSING FOR PI_BACK LOOPS
C       
  920 do 930 i = 1,kpipar   
      j1 = shift (mpsort(1,i), -16)
      j2 = shift (shift (mpsort(1,i), 16), -16)
      j3 = shift (mpsort(2,i), -16)
      if (j1.ne.k2) go to 930   
      if (j2.ne.k1) go to 930   
      if (j3.ne.k3) go to 930   
      if (i-j) 940,930,970  
  930 continue  
      call erquit   
  940 jsect = 0 
      ksw = 1   
      go to 980 
C       
C       FIND TRANSPOSE TO DETERMINE NUMBER OF SECTIONS ENCOUNTERED. 
C       THEN RELABEL SECTIONS IN DECREASING SEQUENCE.   
C       
  950 do 960 i = 1,kpipar   
      j1 = shift (mpsort(1,i), -16)
      j2 = shift (shift (mpsort(1,i), 16), -16)
      j3 = shift (mpsort(2,i), -16)
      if (j1.ne.k2) go to 960   
      if (j2.ne.k1) go to 960   
      if (j3.eq.k3) go to 970   
  960 continue  
      call erquit   
  970 continue  
      j3 = shift (shift (mpsort(2,i), 16), -16)
      jsect = shift( shift (lpiprl(3,j3), 16), -16)
      jsect = jsect + 1 
      ksw = -1  
  980 continue  
      k1x = k1  
      n = 1 
      ktrknt = 0
      amiles = 0.0  
      rating = 1.0e8
C       
C     PROCESS SECTIONS  
C       
  990 continue
C       
C     Compute total series reactance via PIEQIV. Note that
c     JBRNCH is loaded at ltot+1 to utilize PIEQIV
c
      brtype(ltot2+1) = jbrnch(1,jebr)
      kx(ltot2+1) = jbrnch(2,jebr)
      ky(ltot2+1) = jbrnch(12,jebr)
      brsect(ltot2+1) = jbrnch(14,jebr)
      brnch_ptr(ltot2+1) = ltot+1
      do jj = 1,18   
         kbrnch(jj,ltot+1) = jbrnch(jj,jebr) 
      enddo
      if (n .eq. 1) then
         call firsecd (y)
         n = 2  
      else  
         call nexsecd (y)
      endif 
      jsect = jsect + ksw   
      j1 = jbrnch(2,jebr)   
      j2 = jbrnch(12,jebr)  
      basemx = amax1(base(j1),base(j2)) 
      jbrnch(2,jebr)  = k1  
      jbrnch(12,jebr)  = k2 
      call putchr (1,id,jbrnch(13,jebr))
      jbrnch(14,jebr) = jsect   
      ltype = jbrnch(1,jebr)
C       
C     GET RATING OF THIS SECTION
C       
      rate = ebrnch(4,jebr) 
      if (ltype .eq. 5 .or. ltype .eq. 6 ) then 
C       
C        CONVERT TX MVA TO AMPS 
C        USE 120% RATTING FOR OVERLOAD  
C       
         rate = ( 1200.0*rate ) / ( sqrt(3.0)*basemx )  
         if (ltype .eq. 5 .or. ltype .eq. 6 ) ktrknt = ktrknt + 1   
C       
      endif 
C       
C     PICK UP THE SMALLEST RATING   
C       
      rating = amin1(rating,rate)   
C       
      if (ltype .eq. 3) amiles = amiles + ebrnch(9,jebr)
      jebl = jebr   
      jebr = jbrseq(jebr)   
      if (jebr.ne.19999) go to 990   
      kpib = kpib+1 
      kxl = lpibak(2,kpib)   
      if (kxl) 1010,1030,1040
 1010 kax = iabs(kxl)
      j4 = ikk(4,kax)   
      if (j4.eq.0) call erquit  
      if (k1x .eq. kbuspi(2,j4)) then   
         jebr = kbuspi(6,j4)
      else if (k1x .eq. kbuspi(3,j4)) then  
         jebr = kbuspi(4,j4)
      else  
         call erquit
      endif 
 1020 if (jebr .eq. 0) then 
          if (k2.eq.0) go to 1030   
          call erquit   
      endif 
      jbrseq(jebl) = jebr   
 1030 continue  
      if (k1 .eq. kbuspi(14,j4)) then   
         buspi(15,j4) = 1.0 - pibak(1,kpib)/totx
         if (k1 .eq. k2. or. k2 .eq. 0) then
            buspi(15,j4) = 1.0  
            buspi(17,j4) = 0.0  
         endif  
      else if (k1 .eq. kbuspi(16,j4)) then  
         buspi(17,j4) = 1.0 - pibak(1,kpib)/totx
         if (k1 .eq. k2 .or. k2 .eq. 0) then
            buspi(15,j4) = 0.0  
            buspi(17,j4) = 1.0  
         endif  
      else  
         call erquit
      endif 
      k1x = kbuspi(1,j4)
      if (lpibak(3,kpib).ne.19999) go to 990 
      go to 1090
 1040 if (jsect .gt. 10 .or. jsect .lt. 0) then 
         write (errbuf(1),1050) bus(k1),base(k1),bus(k2),base(k2)   
 1050    format(' Pi-back branch ',a8,f7.1,' to ',a8,f7.1,' has more tha
     1n nine sections')
         write (errbuf(2),1060)
 1060    format (' Pi-back replaced with equivalent pi.') 
         call prterx('F', 1)
      else if (ktrknt .eq. 0 .and. nwscc .ne. 0) then   
         go to 1080 
      else if (ktrknt .gt. 1) then  
         write (errbuf(1),1070) bus(k1),base(k1),bus(k2),base(k2)   
 1070    format(' Pi-back branch ',a8,f7.1,' to ',a8,f7.1,' has more tha
     1n one transformer in series')
         write (errbuf(2), 1072)
 1072    format (' Pi-back branch replaced with equivalent pi.') 
         call prterx ('F', 1)
      endif 
      jebr = lpiprl(5,k)
      jsect = 0 
      jbrnch(14,jebr) = 0   
      call finsecd (y)   
C       
C     Insert minium rating  
C       
      ebrnch(4,jebr) = rating   
      ebrnch(5,jebr) = sngl(dreal (-1.0d0/y(1,2)))
      ebrnch(6,jebr) = sngl(dimag (-1.0d0/y(1,2)))
      if (jsect .le. 10 .and. jsect .ge. 0 .and .ktrknt .eq. 0) then
         jbrnch(1,jebr) = 3 
         ebrnch(7,jebr) = sngl(dreal (y(1,1)+y(1,2)))
         ebrnch(8,jebr) = sngl(dimag (y(1,1)+y(1,2)))
         ebrnch(9,jebr) = amiles
      else  
         jbrnch(1,jebr) = 8 
         ebrnch(7,jebr) = sngl(dreal (y(1,1)+y(1,2)))
         ebrnch(8,jebr) = sngl(dimag(y(1,1)+y(1,2)))
         ebrnch(9,jebr) = sngl(dreal (y(2,1)+y(2,2)))
         ebrnch(10,jebr) = sngl(dimag(y(2,1)+y(2,2)))
      endif 
      jbrseq(jebr) = 19999   
 1080 lpiprl(3,k) = ipack_2(ichar(id), jsect)
 1090 continue  
      if (kdebug .gt. 0) then   
         write (9,1100) 
 1100    format('1 FINAL DEBUG DUMP OF "BUSPI" ARRAY'//)
         do 1120 k = 1,kpibak   
         if (abs (buspi(15,k)+buspi(17,k)-1.0) .le. 1.0e-4) then
           error = ' '  
         else   
           error = 'ERROR'  
         endif  
         write (9,550) k,(kbuspi(i,k),i=1,7),(buspi(i,k),i=8,13),   
     1    kbuspi(14,k),buspi(15,k),kbuspi(16,k),buspi(17,k),error   
 1120    continue   
      endif 
C       
C     Final pass through branch data:   
C        1. Compress retained branches. 
C        2. Perform a merge-pass with equivalent branches in
C           JBRNCH/EBRNCH and "compressed" branches in KBRNCH/BRNCH.
C        3  Relabel ID's.   
C        4. Append branch to KBSDTA.
C       
C     1. Delink cut branches and pi-back branches
C       
      do nb = 1, ntot
         if (ikk(1,nb) .eq. 0 .or. 
     &      (ikk(1,nb) .eq. 0 .and. ikk(4,nb) .ne. 0)) then
            ptr = kbsdta(16,nb)
            kbsdta(16,nb) = 0
            do while (ptr .gt. 0)
               ndel2 = ndel2 + 1
               lindel2(ndel2) = ptr
               kx(ptr) = 0
               ky(ptr) = 0
               nbr = brnch_ptr(ptr)
               br_status(nbr) = br_status(nbr) + 1
               if (br_status(nbr) .eq. 2) then
                  ndel1 = ndel1 + 1
                  lindel1(ndel1) = nbr
                  kbrnch(1,nbr) = 0
               endif
               ptr = brnch_nxt(ptr)
            enddo
         else
            oldptr = 0
            ptr = kbsdta(16,nb)
            do while (ptr .gt. 0)
               k2 = ky(ptr)
               if (ikk(1,k2) .eq. 0 .or.
     &            (ikk(1,k2) .eq. 1 .and. ikk(4,k2) .gt. 0)) then
                  kx(ptr) = 0
                  ky(ptr) = 0
                  ndel2 = ndel2 + 1
                  lindel2(ndel2) = ptr
                  nbr = brnch_ptr(ptr)
                  br_status(nbr) = br_status(nbr) + 1
                  if (br_status(nbr) .eq. 2) then
                     ndel1 = ndel1 + 1
                     lindel1(ndel1) = nbr
                     kbrnch(1,nbr) = 0
                  endif
                  if (oldptr .gt. 0) then
                     brnch_nxt(oldptr) = brnch_nxt(ptr)
                  else
                     kbsdta(16,nb) = brnch_nxt(ptr)
                  endif
               endif
               ptr = brnch_nxt(ptr)
            enddo
         endif
      enddo
C       
C     Perform a merge-pass between JBRNCH/EBRNCH and KBRNCH/BRNCH   
C       
      ksw = 1   
      lastbus = 0
      ptr = 0
      ib = 0
      assign 1160 to nexbrn 
C       
C     Obtain next item in BRNCH 
C       
 1150 finished = .false.
      do while (.not. finished)
         if (ptr .gt. 0) then
c
c           Get next branch
c
            oldptr = ptr
            ptr = brnch_nxt(ptr)
            k1 = kx(ptr)
            k2 = ky(ptr)
            id = brid(ptr)
            if (brtype(ptr) .eq. 4) id = char(0)   
            if (ikk(1,k2) .eq. 1) then
               finished = .true.
            endif
         else
c
c           Get next bus
c
            oldptr = 0
            if (lastbus .lt. ntot_alf) then
               lastbus = lastbus + 1
               nb = alf2inp(lastbus)
               ptr = kbsdta(16,nb)
               if (ikk(1,nb) .eq. 1 .and. ikk(4,nb) .eq. 0) then
                  k1 = kx(ptr)
                  k2 = ky(ptr)
                  id = brid(ptr)
                  if (brtype(ptr) .eq. 4) id = char(0)   
                  if (ikk(1,k2) .eq. 1) then
                     finished = .true.
                  endif
               endif
            else
               finished = .true.
               if (ksw .le. 2) ksw = ksw + 2   
            endif
         endif
      enddo
      go to nexbrn  
C       
C     Obtain next item in EBRNCH
C       
 1160 ib = ib + 1   
      if (ib .le. kpipar) then  
         j1 = shift (mpsort(1,ib), -16)
         j2 = shift (shift (mpsort(1,ib), 16), -16)
         if (j1 .eq. 0 .or. j2 .eq. 0) go to 1160   
         j3 = mpsort(2,ib)  
         j = shift (j3, -16) + ichar('5') - 1  
         jd = char (j)  
         ic = shift (shift (j3, 16), -16)
         jebr = lpiprl(5,ic)
         if (kdebug .gt. 0) then
            write (9,1170) bus(j1),base(j1),bus(j2),base(j2),jd 
 1170       format(' APPENDING BRANCH ',a8,f7.1,2x,a8,f7.1,a4,' TO NETWO
     1rk.') 
         endif  
      else  
         if (ksw .eq. 1 .or. ksw .eq. 3) ksw = ksw + 1  
      endif 
C       
C     KSW assignments: 1 - normal   
C                      2 - E_O_D for EBRNCH 
C                      3 - E_O_D for BRNCH  
C                      4 - E_O_D for EBRNCH and BRNCH   
C       
 1180 go to ( 1190, 1220, 1240, 1260) ksw   
 1190 if (k1 - j1) 1220, 1200, 1240 
 1200 if (k2 - j2) 1220,1210,1240   
 1210 if (id .gt. jd) go to 1240
      if (id .lt. jd) go to 1220
C       
C     Change JD for unique parallel 
C       
      i = ichar (jd)
      jd = char (i+1)   
C       
C     Get next branch
C       
 1220 assign 1180 to nexbrn 
      go to 1150
C       
C     Append equivalent branches to BRNCH   
C       
 1240 do while (jebr .ne. 19999)
         newptr1 = lindel1(ndel1)
         ndel1 = ndel1 - 1
         newptr2 = lindel2(ndel2)
         ndel2 = ndel2 - 1
         brtype(newptr2) = jbrnch(1,jebr)
         kx(newptr2) = jbrnch(2,jebr)
         ky(newptr2) = jbrnch(12,jebr)
         call putchr (1,brid(newptr2),jbrnch(13,jebr))
         brsect(newptr2) = jbrnch(14,jebr)
         do j = 1,18  
            kbrnch(j,newptr1) = jbrnch(j,jebr) 
         enddo
         if (oldptr .eq. 0) then
            kbsdta(16,nb) = newptr2
         else
            bctbl_nxt(oldptr) = newptr2
         endif
         brnch_nxt(newptr2) = ptr
         brnch_ptr(newptr2) = newptr1
         oldptr = newptr2
         jebr = jbrseq(jebr)   
      enddo
      go to 1160

 1260 continue
C       
C     Final pass through BCTBL and EBCTBL.  Perform the following.
C
C     1. Compress retained continuation buses;   
C     2. Perform a merge-pass with equivalent continuation buses in  
C        JBCTBL and "compressed" continuation buses in KBCTBL;   
C     3. Append +bus data to KBSDTA  
C       
C     1. Compress retained continuation buses
C       
 1290 ndel3 = 0
      do i = ntot2+1, MAXCBS
         ndel3 = ndel3 + 1
         cbdel(ndel3) = i
      enddo
      do nb = 1, ntot
         if (ikk(1,nb) .eq. 0 .or. 
     &      (ikk(1,nb) .eq. 1 .and. ikk(4,nb) .gt. 0)) then
            ptr = kbsdta(15,nb)
            kbsdta(15,nb) = 0
            do while (ptr .gt. 0)
               ndel3 = ndel3 + 1
               cbdel(ndel3) = ptr
               ptr = bctbl_nxt(ptr)
            enddo
         endif
      enddo
C       
C     Cleanup MPSORT array. Extraneous items not needed are K2 and IPAR.
C       
      do 1311 i = 1, kpipar 
      k1 = shift (mpsort(1,i), -16)
      mpsort(1,i) = ipack_2 (k1, 0)  
      ic = shift (shift (mpsort(2,i), 16), -16)
      mpsort(2,i) = ic  
 1311 continue  
C       
C     Append +buses created from cutting to +buses created by   
C     Pi-back. Flag ownership 'INT' with K2 = 1 so that they sort last 
C     and together. 
C       
      do i = 1,ncbadd  
         j = kpipar + i
         call getchr (3,kown,jbctbl(10,i)) 
         if (kown .eq. 'INT') then 
            k2 = 1 
         else  
            k2 = 0 
         endif 
         mpsort(1,j) = ipack_2 (jbctbl(1,i), k2)  
         mpsort(2,j) = -i  
      enddo
      jpipar = kpipar + ncbadd  
      call qiksrt (1,jpipar,kmpmps,swpmps)  
C       
C     Perform a merge-pass between JBCTBL/EBCTBL and KBCTBL/BCTBL   
C       
      do i = 1,17  
         total1(i) = 0.0   
         total2(i) = 0.0   
      enddo
      do i = 1,ncbadd  
         call getchr (1,code,jbctbl(8,i))  
         if (code .eq. 'A') then
            call getchr (3,cown,jbctbl(10,i)) 
            if (cown .eq. '***' .or. cown .eq. 'INT') then
               do j = 2,6   
                  total1(j) = total1(j) + ebctbl(j,i)   
               enddo
            endif
            total1(11) = total1(11) + bctbl(11,i) 
         endif
      enddo
      write (outbuf,1318) total1(6),total1(11),(total1(j),j=2,5)
 1318 format ('0 TOTAL CUT EQUIVALENT - GENERATION ',2f10.1,' LOAD ',   
     1  2f10.1,' SHUNT ',2f10.1)
      call prtout (1)   
C       
C     Perform a merge-pass between JBCTBL/EBCTBL and KBCTBL/BCTBL   
C       
      ksw = 1   
      lastbus = 0
      ncb = 0
      ib = 0
      assign 1330 to nexcbs 
C       
C     Obtain next item in BCTBL
C       
 1320 finished = .false.
      do while (.not. finished)
         if (ncb .gt. 0) then
c
c           Get next item in BCTBL
c
            oldptr = ncb
            ncb = bctbl_nxt(ncb)
            if (ncb .gt. 0) then
               k1 = kbctbl(1,ncb)
               call getchr (1,kode,kbctbl(8,ncb)) 
               call getchr (2,kdyr,kbctbl(9,ncb)) 
               call getchr (3,kown,kbctbl(10,ncb))
               if (ikk(1,k1) .eq. 1) then
                  finished = .true.
               endif
            endif
         else
c
c           Get next bus
c
            oldptr = 0
            if (lastbus .lt. ntot_alf) then
               lastbus = lastbus + 1
               nb = alf2inp(lastbus)
               if (ikk(1,nb) .eq. 1 .and. ikk(4,nb) .eq. 0) then
                  ncb = kbsdta(15,nb)
                  if (ncb .gt. 0) then
                     k1 = kbctbl(1,ncb)
                     call getchr (1,kode,kbctbl(8,ncb)) 
                     call getchr (2,kdyr,kbctbl(9,ncb)) 
                     call getchr (3,kown,kbctbl(10,ncb))
                     if (ikk(1,k1) .eq. 1) then
                        finished = .true.
                     endif
                  endif
               else
                  ncb = 0
               endif
            else
               finished = .true.
               if (ksw .le. 2) ksw = ksw + 2   
            endif
         endif
      enddo
      go to nexcbs
C       
C     Obtain next item in EBCTBL
C       
 1330 ib = ib + 1   
      ic = 0
      if (ib .le. jpipar) then  
         j1 = shift (mpsort(1,ib), -16)
         if (mpsort(2,ib) .lt. 0) then  
            jecb = -mpsort(2,ib)
            ic = 0  
            call getchr (1,code,jbctbl(8,jecb)) 
            call getchr (2,cdyr,jbctbl(9,jecb)) 
            call getchr (3,cown,jbctbl(10,jecb))
         else   
            if (ikk(2,j1) .gt. 0) then  
               ic = mpsort(2,ib)
               jecb = ikk(2,j1) 
               finished = .false.
               do while (jecb .eq. jcbt .and. .not. finished .and.
     &                  (jbctbl(1,jecb) .eq. j1))
                  call getchr (1,code,jbctbl(8,jecb))  
                  call getchr (2,cdyr,jbctbl(9,jecb))  
                  call getchr (3,cown,jbctbl(10,jecb)) 
                  if (code .eq. '+' .and. cdyr .eq. '00' .and. 
     &                cown .eq. '***') then
                     finished = .true.
                  else 
                     jecb = jecb + 1   
                  endif 
               enddo
               if (.not. finished) then
C       
C                 Create "dummy" entity for EBCTBL if none exists   
C       
                  jcbt = jcbt + 1   
                  jecb = jcbt   
                  jbctbl(1,jecb) = j1   
                  do j = 2,12
                     jbctbl(j,jecb) = 0
                  enddo
                  code = 'A'
                  cdyr = '00'   
                  cown = '***'  
                  call putchr (1,code,jbctbl(8,jecb))   
                  call putchr (2,cdyr,jbctbl(9,jecb))   
                  call putchr (3,cown,jbctbl(10,jecb))  
               endif
            else
C       
C              Create "dummy" entity for EBCTBL if none exists  
C       
               ic = mpsort(2,ib)
               jcbt = jcbt + 1  
               jecb = jcbt  
               ikk(2,j1) = jcbt 
               jbctbl(1,jecb) = j1  
               do j = 2,12
                  jbctbl(j,jecb) = 0   
               enddo
               code = 'A'   
               cdyr = '00'  
               cown = '***' 
               call putchr (1,code,jbctbl(8,jecb))  
               call putchr (2,cdyr,jbctbl(9,jecb))  
               call putchr (3,cown,jbctbl(10,jecb)) 
            endif   
         endif  
         if (kdebug .gt. 0) then
            write (9,1360) bus(j1),base(j1), code, cdyr, cown   
 1360       format(' Appending continuation bus ',a8,f7.1,1x,a,1x,a,
     1         1x,a,' to network.') 
         endif  
      else  
         if (ksw .eq. 1 .or. ksw .eq. 3) ksw = ksw + 1  
      endif 
C       
C     KSW assignments: 1 - normal   
C                      2 - E_O_D for EBCTBL 
C                      3 - E_O_D for BCTBL  
C                      4 - E_O_D for EBCTBL and BCTBL   
C       
 1370 go to ( 1380, 1410, 1430, 1480) ksw   
 1380 if (k1 - j1) 1410, 1390, 1430 
 1390 komp = kompr (kode//kown//kdyr, code//cown//cdyr, junk)

      if (komp .gt. 0) go to 1430
      if (komp .lt. 0) go to 1410
C       
C     Match found: add continuation bus quantities
C       
      bctbl(2,ncb) = bctbl(2,ncb) + ebctbl(2,jecb)  
      bctbl(3,ncb) = bctbl(3,ncb) + ebctbl(3,jecb)  
      bctbl(4,ncb) = bctbl(4,ncb) + ebctbl(4,jecb)  
      bctbl(5,ncb) = bctbl(5,ncb) + ebctbl(5,jecb)  
      bctbl(6,ncb) = bctbl(6,ncb) + ebctbl(6,jecb)  
      bctbl(11,ncb) = bctbl(11,ncb) + ebctbl(11,jecb)   
      bctbl(12,ncb) = bctbl(12,ncb) + ebctbl(12,jecb)   
      klast = k1
      codex = kode
      cdyrx = kdyr
      cownx = kown
      jcbadd = ncb
      assign 1330 to nexcbs 
      assign 1320 to inext  
      go to 1450
C       
C     Get next continuation bus 
C       
 1410 assign 1370 to nexcbs 
      go to 1320
C       
C     Append equivalent continuation buses to BCTBL 
C       
 1430 if (j1 .eq. klast .and. code .eq. codex .and. cdyr .eq. cdyrx
     &                  .and. cown .eq. cownx) then

         bctbl(2,jcbadd) = bctbl(2,jcbadd) + ebctbl(2,jecb)  
         bctbl(3,jcbadd) = bctbl(3,jcbadd) + ebctbl(3,jecb)  
         bctbl(4,jcbadd) = bctbl(4,jcbadd) + ebctbl(4,jecb)  
         bctbl(5,jcbadd) = bctbl(5,jcbadd) + ebctbl(5,jecb)  
         bctbl(6,jcbadd) = bctbl(6,jcbadd) + ebctbl(6,jecb)  
         bctbl(11,jcbadd) = bctbl(11,jcbadd) + ebctbl(11,jecb)   
         bctbl(12,jcbadd) = bctbl(12,jcbadd) + ebctbl(12,jecb)   
      else
         ptr = cbdel(ndel3)
         ntot2 = max (ntot2, ptr)
         ndel3 = ndel3 - 1
         kbctbl(1,ptr) = j1
         do j = 2,12
            kbctbl(j,ptr) = jbctbl(j,jecb) 
         enddo
         ncby = 0
         if (j1 .gt. 0) then
           ncbx = kbsdta(15,j1) 
           finished = .false.
         else
           ncbx = 0
           finished = .true.
         endif
         do while (ncbx .gt. 0 .and. .not. finished)
            call getchr (1,kodey,kbctbl(8,ncbx)) 
            call getchr (2,kdyry,kbctbl(9,ncbx)) 
            call getchr (3,kowny,kbctbl(10,ncbx))
            komp = kompr(kodey//kowny//kdyry, code//cown//cdyr, junk)
            if (komp .gt. 0) then
               finished = .true.
            else
               ncby = ncbx
               ncbx = bctbl_nxt(ncbx)
            endif
         enddo
         if (ncby .eq. 0) then
            kbsdta(15,j1) = ptr
         else
            bctbl_nxt(ncby) = ptr
         endif
         bctbl_nxt(ptr) = ncby
         oldptr = ptr
         klast = j1
         codex = code
         cdyrx = cdyr
         cownx = cown
         jcbadd = ptr
      endif

      assign 1330 to inext  
C       
C     Zero out entity to accomodate temporary useage for other pi-back  
C     quantities.   
C       
 1450 do 1452 j = 2,6   
 1452 ebctbl(j,jecb) = 0.0  
      ebctbl(11,jecb) = 0.0 
      ebctbl(12,jecb) = 0.0 

      if (ic .eq. 0) go to 1470 
      do 1460 kpi = lpiprl(4,ic)+1,mpibak   
      j2 = -lpibak(2,kpi)   
      if (j2 .le. 0) go to 1470 
      i4 = ikk(4,j2)
      if (j1 .eq. kbuspi(14,i4)) then   
         pct = buspi(15,i4) 
      else if (j1 .eq. kbuspi(16,i4)) then  
         pct = buspi(17,i4) 
      else  
         call erquit()
      endif 
      bctbl(2,jcbadd) = bctbl(2,jcbadd) + pct * buspi(10,i4)
      bctbl(3,jcbadd) = bctbl(3,jcbadd) + pct * buspi(11,i4)
      bctbl(4,jcbadd) = bctbl(4,jcbadd) + pct * buspi(12,i4)
      bctbl(5,jcbadd) = bctbl(5,jcbadd) + pct * buspi(13,i4)
      bctbl(6,jcbadd) = bctbl(6,jcbadd) + pct * buspi(8,i4) 
      bctbl(11,jcbadd) = bctbl(11,jcbadd) + pct * buspi(9,i4)   
      pi777(i4) = pi777(i4) - pct   
      do 1454 j = 8,13  
 1454 total2(j) = total2(j) + pct * buspi(j,i4) 
      if (lpibak(3,kpi) .eq. 19999) go to 1470   
 1460 continue  
 1470 go to inext   
C       
C     Check distributed sums
C       
 1480 write (outbuf,1502) (total2(i),i=8,13)
 1502 format ('0 TOTAL PI-BACK        - GENERATION ',2f10.1,' LOAD ',   
     1  2f10.1,' SHUNT ',2f10.1)
      call prtout (1)   
      do i = 1,17  
         total1(i) = 0.0   
      enddo
      do nbx = 1, ntot_alf
         nb = alf2inp(nbx)
         ncb = kbsdta(15,nb)
         do while (ncb .gt. 0)
            call getchr (1,code,kbctbl(8,ncb))
            call getchr (3,cown,kbctbl(10,ncb))   
            if (code .eq. 'A' .and. 
     &         (cown .eq. '***' .or. cown .eq. 'INT')) then
              do i = 2,6   
                 total1(i) = total1(i) + bctbl(i,ncb)  
              enddo
              total1(11) = total1(11) + bctbl(11,ncb)   
            endif
            ncb = bctbl_nxt(ncb)
         enddo
      enddo
      write (outbuf,1540) total1(6),total1(11),(total1(i),i=2,5)
 1540 format ('0 TOTAL CUT EQUIVALENT - GENERATION ',2f10.1,' LOAD ',   
     1  2f10.1,' SHUNT ',2f10.1)
      call prtout (1)   
C       
C     CHECK IF ALL QUANTITIES PROCESSED 
C       
      do i = 1,17  
         total1(i) = 0.0   
      enddo
      do 1640 i = 1,kpibak  
      if (abs (pi777(i)) .ge. 1.0e-3) then  
         k1 = kbuspi(1,i)   
         do j = 8,13   
            buspi(j,i) = pi777(i)*buspi(j,i)   
         enddo
         write (outbuf,1620) i,bus(k1),base(k1),pi777(i),(buspi(j,i),   
     1     j=8,13)  
 1620    format (' ERROR - UNPROCESSED PI  ',i4,2x,a8,f7.1,f7.3,6f6.1)  
         call prtout (1)
         do j = 8,13   
            total1(j) = total1(j) + buspi(j,i) 
         enddo
      endif 
 1640 continue  
      write (outbuf,1650) (total1(j),j=8,13)
 1650 format ('0 TOTAL PI UNPROCESSED - GENERATION ',2f10.1,' LOAD ',   
     1  2f10.1,' SHUNT ',2f10.1)
      call prtout (1)   
 9000 continue
      return
      end   
