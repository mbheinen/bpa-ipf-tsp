C    @(#)chgbty.f	20.10 5/27/98
      subroutine chgbty (nb, newtyp, ntbx, ntran, list) 
      logical list
 
C     change bus types into NEWTYP.
C
C     Parameters: NB     = external number of bus to be converted.
C                 NEWTYP = new bus type.
C                 NTBX   = index to TBX if applicable.
C                 NTRAN  = index to TRAN if applicable.
C                          (If NEWTYP = 10, LTC's NTRAN is disabled.)
C                 LIST   = list control (on=.true./off=.false.)

      include 'ipfinc/parametr.inc'
c
      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/xdata.inc'
 
      common /is_batch / is_batch
c	
c     Local Variables
c
      double precision vk, qgen, ovmin, ovmax, fvmin, fvmax,
     &                 unused, qorig, brem, borig
c
      character btyp * 1, oldbty * 2, newbty * 2, 
     1          cbtype * 1, cbown * 3, cbyear * 2
      integer found, pold, ptr
       
      kt = inp2opt(nb)
      call typno (btyp, ntypu(kt))                  
      oldbty = 'B' // btyp  
      if (newtyp .gt. 0) then
         call typno (btyp, iabs(newtyp))   
         newbty = 'B' // btyp  
         vk = dsqrt (e(kt) ** 2 + f(kt) ** 2)
         call glbvlt (nb,vmin,vmax)
         if (ordcap .eq. 1) then   
            unused = capcor(1,nb)  
            qorig = capcor(2,nb)   
            capcor(1,nb) = 0.0 
         else  
            unused = capcor(1,kt)  
            qorig = capcor(2,kt)   
            capcor(1,kt) = 0.0 
         endif 
         qold = qorig  
         if (qorig .eq. -9.0e10) qorig = qnetu(kt)     
C       
C        Obtain original values
C       
         call allocq (nb, sngl(qorig), sngl(qgen), qgnmax,
     1                qgnmin, qld, totcap, usecap, totrek, 
     2                userek, unsked, qerr)
         otots = totcap + totrek   
         ouses = usecap + userek   
         oqmax = qgnmax
         oqmin = qgnmin
         oqgen = qgen  
         ovmin = vlimn(kt)                            
         ovmax = vlimx(kt)                            
      else
         newbty = 'B' // btyp  
      endif
        
      if (ntbx .gt. 0) then 
         ltyp = tbx(1,ntbx)
         ityp = tbx(7,ntbx)
      else  
         ityp = 0   
         ltyp = 0   
      endif 
        
      if (ntypu(kt).eq.7.and.(newtyp.eq.1.or.newtyp.eq.2)) then 
C                                                                      *
C        BQ --> B                                                      *
C        BQ --> BE                                                     *
C                                                                      *
         brem = 0.0 
         if (ityp .eq. 3) then  
C                                                                      *
C           State Q_min: Remove all capacitors.                        *
C                                                                      *
            if (unused .ne. 0.0) then
               write (outbuf,100) unused, 'Q_min'
  100          format (' Improper CHGBTY data : UNUSED = ', 
     1                  f6.1, ' (', a, ')')
               if (list) call prtout (1)
            else
               brem = dmax1 (0.0d0, tbx(6,ntbx))  
               busdta(6,nb) = busdta(6,nb) - brem * bmva
            endif   
         else if (ityp .eq. 4) then 
C                                                                      *
C           State Q_max: Remove all reactors.                          *
C                                                                      *
            if (unused .ne. 0.0) then   
               write (outbuf,100) unused, 'Q_max'   
               if (list) call prtout (1)
            else
               brem = dmin1 (0.0d0, tbx(6,ntbx))  
               busdta(6,nb) = busdta(6,nb) - brem * bmva
            endif   
         endif  
C                                                                      *
C        Remove unused B_shunt and ficticious Q_gen                    *
C                                                                      *
         bkku(kt) = bkku(kt) - unused  
         busdta(6,nb) = busdta(6,nb) - unused * bmva
         qnetu(kt) = qorig + unused * vk ** 2  
         qrem = 0.0 
C                                                                      *
C        Convert synchronous condensers into static shunt              *
C                                                                      *
         if (pnetu(kt) + ploadu(kt) .le. 0.01 .and. newtyp .eq. 1) then   
            qgen = qnetu(kt) + qloadu(kt)           
            qrem = qgen 
            badd = qgen / vk ** 2   
            qnetu(kt) = -qloadu(kt)   
            bkku(kt) = bkku(kt) + badd
            busdta(6,nb) = busdta(6,nb) + badd * bmva   
C                                                                      *
C           Remove all Q_gen from bus                                  *
C                                                                      *
            busdta(9,nb) = 0.0  
            busdta(10,nb) = 0.0 
            ncb = kbsdta(15,nb)  
            do while (ncb .gt. 0)
               bctbl(11,ncb) = 0.0  
               bctbl(12,ncb) = 0.0
               ncb = bctbl_nxt(ncb)
            enddo
         else if (newtyp .eq. 1) then   
C                                                                      *
C           Remove excess Q_gen from bus
C                                                                      *
            qgenmw = busdta(9,nb)   
            ncb = kbsdta(15,nb)  
            do while (ncb .gt. 0)
               qgenmw = qgenmw + bctbl(11,ncb)  
               ncb = bctbl_nxt(ncb)
            enddo
            qgen = qnetu(kt) + qloadu(kt)           
            qrem = qgenmw / bmva - qgen 
            busdta(9,nb) = busdta(9,nb) - qrem * bmva   
            badd = 0.0  
         endif  
         ntypu(kt) = newtyp
         kbsdta(1,nb) = newtyp  
         if (newtyp .eq. 1) then
            vlimn(kt) = vmin  
            vlimx(kt) = vmax  
            vstart(nb) = vk 
            busdta(10,nb) = 0.0 
            busdta(11,nb) = 0.0 
            busdta(12,nb) = 0.0 
         else   
            vlimn(kt) = vk
            vlimx(kt) = vk
            vstart(nb) = vk 
            busdta(11,nb) = vk  
            busdta(12,nb) = vk  
         endif  
         tbx(1,ntbx) = ltyp + 10   
        
      else if (ntypu(kt) .eq. 7 .and. newtyp .eq. 7) then   
C                                                                      *
C        BQ --> BQ with used shunt transferred to a +A record.         *
C                                                                      *
         bused = userek + usecap                       
         bfree = totrek - userek + totcap - usecap     
C  
C        Remove unused B_shunt and ficticious Q_gen 
C   
         bkku(kt) = bkku(kt) - unused  
         qnetu(kt) = qorig + unused * vk ** 2  
         qrem = 0.0 
         qmax = busdta(9,nb) / bmva 
         qmin = busdta(10,nb) / bmva
         tbx(3,ntbx) = qmax - qloadu(kt)             
         tbx(4,ntbx) = qmin - qloadu(kt)             
         tbx(6,ntbx) = 0.0  
        
         if (abs (borig) .gt. 0.01) then
C  
C           Total up all B_shunt and transfer from bus and +bus 
c           to +A bus record. 
C  
            bshunt = busdta(6,nb)   
            busdta(6,nb) = 0.0  
            found = 0   
            pold = 0
            ncb = kbsdta(15,nb)  
            do while (ncb .gt. 0)
               bshunt = bshunt + bctbl(5,ncb)   
               bctbl(5,ncb) = 0.0   
               call getchr (1, cbtype, kbctbl(8,ncb))
               call getchr (3, cbown, kbctbl(10,ncb))
               call getchr (2, cbyear, kbctbl(9,ncb))                          
               komp = kompr (cbtype, 'A', junk)
               if (komp .eq. 0) then
                  komp = kompr (cbown, owner(nb), junk)
               endif
               if (komp .eq. 0) then
                  komp = kompr (cbyear, '00', junk)
               endif
               if (komp .lt. 0) then
c
c                 Insert current entity between pold and ncb
c
                  if (ntot2 + 1 .ge. MAXCBS) then
                     write (errbuf(1), 10152) MAXCBS
10152                format ('More than ', i4,
     1                 '+ records added from /CHANGE_BUS_TYPES.')
                     if (is_batch .eq. 0) then
                        call prterx ('E',1)
                     else
                        call prterx ('F',1)
                     endif
                     error = 1
                     ntot2 = 0
                     return
                  endif
                  ntot2 = ntot2 + 1
                  found = ntot2
                  kbctbl(1,ntot2) = nb 
                  do j = 2, 12
                     kbctbl(j,ntot2) = 0  
                  enddo
                  call putchr(1,'A',kbctbl(8,ntot2))   
                  call putchr(2,'00',kbctbl(9,ntot2))  
                  call putchr(3,owner(nb),kbctbl(10,ntot2))
                  bctbl_nxt(ntot2) = ncb
                  if (pold .eq. 0) then
                     kbsdta(15,nb) = ntot2
                  else
                     bctbl_nxt(pold) = ntot2
                  endif
               else if (komp .eq. 0) then
                  found = ncb
               endif
               pold = ncb
               ncb = bctbl_nxt(ncb)
            enddo
C       
C           Any used shunt must be transferred to a +A record.   
C           If FOUND > 0, that record exists.
C       
            if (found .gt. 0 .and. abs(bused) .gt. 0.5) then
               bctbl(5,found) = bused / vk ** 2
            else if (abs(bused) .gt. 0.5) then  
C       
C              Insert a +A BCTBL entity.
C
               if (found .eq. 0) then
C       
C                 Create a +A entity   
C       
                  if (ntot2 + 1 .ge. MAXCBS) then
                     write (errbuf(1), 10152) MAXCBS
                     if (is_batch .eq. 0) then
                        call prterx ('E',1)
                     else
                        call prterx ('F',1)
                     endif
                     error = 1
                     ntot2 = 0
                     return
                  endif
                  ntot2 = ntot2 + 1
                  found = ntot2
                  kbctbl(1,ntot2) = nb 
                  do j = 2, 12
                     kbctbl(j,ntot2) = 0  
                  enddo
                  bctbl(5,ntot2) = bused / vk ** 2
                  call putchr(1,'A',kbctbl(8,ntot2))   
                  call putchr(2,'00',kbctbl(9,ntot2))  
                  call putchr(3,owner(nb),kbctbl(10,ntot2))
                  bctbl_nxt(ntot2) = ncb
                  if (pold .eq. 0) then
                     kbsdta(15,nb) = ntot2
                  else
                     bctbl_nxt(pold) = ntot2
                  endif
               endif
            endif   
        
         endif  
        
      else if (ntypu(kt) .eq. 7 .and. newtyp .eq. 13) then  

C        BQ --> BF   

         brem = 0.0 
         if (ityp .eq. 3) then  

C           State Q_min: Remove all capacitors.

            if (unused .ne. 0.0) then   
               write (outbuf,100) unused, 'Q_min'   
               if (list) call prtout (1)
            else
               brem = dmax1 (0.0d0, tbx(6,ntbx))  
               busdta(6,nb) = busdta(6,nb) - brem * bmva
            endif   
         else if (ityp .eq. 4) then 

C           State Q_max: Remove all reactors.  

            if (unused .ne. 0.0) then   
               write (outbuf,100) unused, 'Q_max'   
               if (list) call prtout (1)
            else
               brem = dmin1 (0.0d0, tbx(6,ntbx))  
               busdta(6,nb) = busdta(6,nb) - brem * bmva
            endif   
         else   
         endif  
         tbx(7,ntbx) = 1   

C        Remove unused B_shunt and ficticious Q_gen 

         bkku(kt) = bkku(kt) - unused  
         busdta(6,nb) = busdta(6,nb) - unused * bmva
         qnetu(kt) = qorig + unused * vk ** 2  
         qrem = 0.0 
C                                                                      *
C        Convert synchronous condensers into static shunt              *
C                                                                      *
         if (pnetu(kt) + ploadu(kt) .le. 0.01) then 
            qgen = qnetu(kt) + qloadu(kt)           
            qrem = qgen 
            badd = qgen / vk ** 2   
            qnetu(kt) = -qloadu(kt)   
            bkku(kt) = bkku(kt) + badd
            busdta(6,nb) = busdta(6,nb) + badd * bmva   

C           Remove all Q_gen from bus

            busdta(9,nb) = 0.0  
            busdta(10,nb) = 0.0 
            ncb = kbsdta(15,nb)
            do while (ncb .gt. 0)
               bctbl(11,ncb) = 0.0
               bctbl(12,ncb) = 0.0
               ncb = bctbl_nxt(ncb)
            enddo
         else   

C           Remove excess Q_gen from bus

            qgenmw = busdta(9,nb)   
            ncb = kbsdta(15,nb)
            do while (ncb .gt. 0)
               qgenmw = qgenmw + bctbl(11,ncb)
               ncb = bctbl_nxt(ncb)
            enddo
            qgen = qnetu(kt) + qloadu(kt)           
            qrem = qgenmw / bmva - qgen 
            busdta(9,nb) = busdta(9,nb) - qrem * bmva   
            badd = 0.0  
         endif  
         ntypu(kt) = newtyp
         vlimn(kt) = vk   
         vlimx(kt) = vk   
         vstart(nb) = vk
         kbsdta(1,nb) = newtyp  
         busdta(10,nb) = 0.0
         busdta(11,nb) = vk 
         busdta(12,nb) = vk 
        
         tbx(1,ntbx) = 6   
         tbx(5,ntbx) = qnetu(kt)                    
         if (ityp .eq. 1 .or. ityp .eq. 2) then 
            tbx(7,ntbx) = 1
         else   
            tbx(7,ntbx) = 2
         endif  
        
      else if (ntypu(kt).eq. 8 .and.(newtyp.eq.1.or.newtyp.eq.2))   
     1   then   
C
C        BG --> B   
C        BG --> BE  
C        Remove controlled bus name.
C
         kbsdta(13,nb) = 0  
c
C        Remove unused B_shunt and ficticious Q_gen
c
         brem = 0.0 
         bkku(kt) = bkku(kt)  - unused
         busdta(6,nb) = busdta(6,nb) - unused * bmva
         qnetu(kt) = qorig + unused * vk ** 2  
         qrem = 0.0 

C        Convert synchronous condensers into static shunt
C 
         if (pnetu(kt) + ploadu(kt) .le. 0.01 .and. newtyp .eq. 1) then
            qgen = qnetu(kt) + qloadu(kt)           
            qrem = qgen 
            badd = qgen / vk ** 2   
            qnetu(kt) = -qloadu(kt)
            bkku(kt) = bkku(kt) + badd
            busdta(6,nb) = busdta(6,nb) + badd * bmva   

C           Remove all Q_gen from bus

            busdta(9,nb) = 0.0  
            busdta(10,nb) = 0.0 
            ncb = kbsdta(15,nb)
            do while (ncb .gt. 0)
               bctbl(11,ncb) = 0.0
               bctbl(12,ncb) = 0.0
               ncb = bctbl_nxt(ncb)
            enddo
         else if (newtyp .eq. 1) then   

C           Remove excess Q_gen from bus

            qgenmw = busdta(9,nb)   
            ncb = kbsdta(15,nb)
            do while (ncb .gt. 0)
               qgenmw = qgenmw + bctbl(11,ncb)
               ncb = bctbl_nxt(ncb)
            enddo
            qgen = qnetu(kt) + qloadu(kt)           
            qrem = qgenmw / bmva - qgen 
            busdta(9,nb) = busdta(9,nb) - qrem * bmva   
            badd = 0.0  
         endif  
         kbsdta(1,nb) = newtyp  
         if (newtyp .eq. 1) then
            vlimn(kt) = vmin  
            vlimx(kt) = vmax  
            vstart(nb) = vk 
            busdta(10,nb) = 0.0 
            busdta(11,nb) = 0.0 
            busdta(12,nb) = 0.0 
         else   
            vlimn(kt) = vk
            vlimx(kt) = vk
            vstart(nb) = vk 
            busdta(11,nb) = vk  
            busdta(12,nb) = vk  
         endif  
         tbx(1,ntbx) = ltyp + 10   
        
      else if (ntypu(kt) .eq. 8 .and. newtyp .eq. 7) then   
C  
C        BQ --> BQ with shunt transferred to a +A record.
C        Remove controlled bus name.
C                                             
         kbsdta(13,nb) = 0
         tbx(8,ntbx) = 0
 
         borig = tbx(6,ntbx)
         bused = userek + usecap                       
         bfree = 0.0

C        Remove unused B_shunt and ficticious Q_gen

         bkku(kt) = bkku(kt) - unused 
         qnetu(kt) = qorig + unused * vk ** 2  
         qrem = 0.0 
         qmax = busdta(9,nb) / bmva 
         qmin = busdta(10,nb) / bmva
         tbx(3,ntbx) = qmax - qloadu(kt)            
         tbx(4,ntbx) = qmin - qloadu(kt)            
         tbx(6,ntbx) = 0.0  
        
         if (abs (borig) .gt. 0.01) then
C  
C           Total up all B_shunt and transfer from bus and +bus 
c           to +A bus record. 
C  
            bshunt = busdta(6,nb)   
            busdta(6,nb) = 0.0  
            found = 0   
            pold = 0
            ncb = kbsdta(15,nb)  
            do while (ncb .gt. 0)
               bshunt = bshunt + bctbl(5,ncb)   
               bctbl(5,ncb) = 0.0   
               call getchr (1, cbtype, kbctbl(8,ncb))
               call getchr (3, cbown, kbctbl(10,ncb))
               call getchr (2, cbyear, kbctbl(9,ncb))                          
               komp = kompr (cbtype, 'A', junk)
               if (komp .eq. 0) then
                  komp = kompr (cbown, owner(nb), junk)
               endif
               if (komp .eq. 0) then
                  komp = kompr (cbyear, '00', junk)
               endif
               if (komp .lt. 0) then
c
c                 Insert current entity between pold and ncb
c
                  if (ntot2 + 1 .ge. MAXCBS) then
                     write (errbuf(1), 10152) MAXCBS
                     if (is_batch .eq. 0) then
                        call prterx ('E',1)
                     else
                        call prterx ('F',1)
                     endif
                     error = 1
                     ntot2 = 0
                     return
                  endif
                  ntot2 = ntot2 + 1
                  found = ntot2
                  kbctbl(1,ntot2) = nb 
                  do j = 2, 12
                     kbctbl(j,ntot2) = 0  
                  enddo
                  call putchr(1,'A',kbctbl(8,ntot2))   
                  call putchr(2,'00',kbctbl(9,ntot2))  
                  call putchr(3,owner(nb),kbctbl(10,ntot2))
                  bctbl_nxt(ntot2) = ncb
                  if (pold .eq. 0) then
                     kbsdta(15,nb) = ntot2
                  else
                     bctbl_nxt(pold) = ntot2
                  endif
               else if (komp .eq. 0) then
                  found = ncb
               endif
               pold = ncb
               ncb = bctbl_nxt(ncb)
            enddo
C       
C           Any used shunt must be transferred to a +A record.   
C           If FOUND > 0, that record exists.
C       
            if (found .gt. 0 .and. abs(bused) .gt. 0.5) then
               bctbl(5,found) = bused / vk ** 2
            else if (abs(bused) .gt. 0.5) then  
C       
C              Insert a +A BCTBL entity.
C
               if (found .eq. 0) then
C       
C                 Create a +A entity   
C       
                  if (ntot2 + 1 .ge. MAXCBS) then
                     write (errbuf(1), 10152) MAXCBS
                     if (is_batch .eq. 0) then
                        call prterx ('E',1)
                     else
                        call prterx ('F',1)
                     endif
                     error = 1
                     ntot2 = 0
                     return
                  endif
                  ntot2 = ntot2 + 1
                  found = ntot2
                  kbctbl(1,ntot2) = nb 
                  do j = 2, 12
                     kbctbl(j,ntot2) = 0  
                  enddo
                  bctbl(5,ntot2) = bused / vk ** 2
                  call putchr(1,'A',kbctbl(8,ntot2))   
                  call putchr(2,'00',kbctbl(9,ntot2))  
                  call putchr(3,owner(nb),kbctbl(10,ntot2))
                  bctbl_nxt(ntot2) = ncb
                  if (pold .eq. 0) then
                     kbsdta(15,nb) = ntot2
                  else
                     bctbl_nxt(pold) = ntot2
                  endif
               endif
            endif   
        
         endif  
        
         ntypu(kt) = newtyp
         vlimn(kt) = vk   
         vlimx(kt) = vk   
         vstart(nb) = vk
         kbsdta(1,nb) = newtyp  
         busdta(11,nb) = vk 
         busdta(12,nb) = vk 
        
         tbx(1,ntbx) = 2   
         tbx(5,ntbx) = vk   
         if (ityp .eq. 2) then  
            tbx(7,ntbx) = 3
         else if (ityp .eq. 3) then 
            tbx(7,ntbx) = 4
         else   
            tbx(7,ntbx) = 1
         endif  
        
        
      else if (ntypu(kt) .eq. 8 .and. newtyp .eq. 13) then  

C        BG --> BF
C        Remove controlled bus name.

         kbsdta(13,nb) = 0  
         tbx(8,ntbx) = 0   

C        Remove unused B_shunt and ficticious Q_gen

         bkku(kt) = bkku(kt) - unused 
         busdta(6,nb) = busdta(6,nb) - unused * bmva
         qnetu(kt) = qorig + unused * vk ** 2  
         qrem = 0.0 

C        Convert synchronous condensers into static shunt

         if (pnetu(kt) + ploadu(kt).le. 0.01) then  
            qgen = qnetu(kt) + qloadu(kt)           
            qrem = qgen 
            badd = qgen / vk ** 2   
            qnetu(kt) = -qloadu(kt)   
            bkku(kt) = bkku(kt) + badd 
            busdta(6,nb) = busdta(6,nb) + badd * bmva   

C           Remove all Q_gen from bus   

            busdta(9,nb) = 0.0  
            busdta(10,nb) = 0.0 
            ncb = kbsdta(15,nb)  
            do while (ncb .gt. 0)
               bctbl(11,ncb) = 0.0  
               bctbl(12,ncb) = 0.0
               ncb = bctbl_nxt(ncb)
            enddo
         else   

C           Remove excess Q_gen from bus

            qgenmw = busdta(9,nb)   
            ncb = kbsdta(15,nb)  
            do while (ncb .gt. 0)
               qgenmw = qgenmw + bctbl(11,ncb)  
               ncb = bctbl_nxt(ncb)
            enddo
            qgen = qnetu(kt) + qloadu(kt)           
            qrem = qgenmw / bmva - qgen 
            busdta(9,nb) = busdta(9,nb) - qrem * bmva   
            badd = 0.0  
         endif  
         ntypu(kt) = newtyp
         vlimn(kt) = vk   
         vlimx(kt) = vk   
         vstart(nb) = vk
         kbsdta(1,nb) = newtyp  
         busdta(10,nb) = 0.0
         busdta(11,nb) = vk 
         busdta(12,nb) = vk 
        
         busdta(12,nb) = vk 
         tbx(1,ntbx) = 6   
         tbx(5,ntbx) = qnetu(kt)                    
         if (ityp .eq. 2) then  
            tbx(7,ntbx) = 2
         else if (ityp .eq. 3) then 
            tbx(7,ntbx) = 2
         else   
            tbx(7,ntbx) = 1
         endif  
        
      else if (ntypu(kt).eq.11.and.(newtyp.eq.1.or.newtyp.eq.2))    
     1   then   

C        BX --> B 
C        BX --> BE
C        Remove controlled bus name.
C                                       
         kbsdta(13,nb) = 0
         nxdt = tbx(5,ntbx)
         unused = (xdata(6,nxdt) - xdata(4,nxdt)) / bmva
         kbsdta(1,nb) = newtyp  
         if (newtyp .eq. 1) then
            vlimn(kt) = vmin  
            vlimx(kt) = vmax  
            vstart(nb) = vk 
            busdta(11,nb) = 0.0 
            busdta(12,nb) = 0.0 
         else   
            vlimn(kt) = vk
            vlimx(kt) = vk
            vstart(nb) = vk 
            busdta(11,nb) = vk  
            busdta(12,nb) = vk  
         endif  
         tbx(1,ntbx) = ltyp + 10   
        
      else if (ntypu(kt) .eq. 11 .and. newtyp .eq. 13) then 

C        BX --> BF     
C        Remove controlled bus name.

         kbsdta(13,nb) = 0
         tbx(8,ntbx) = 0
C
         nxdt = tbx(5,ntbx)
         unused = (xdata(6,nxdt) - xdata(4,nxdt)) / bmva
         vlimn(kt) = vk   
         vlimx(kt) = vk   
         vstart(nb) = vk
         kbsdta(1,nb) = newtyp  
         busdta(11,nb) = vk 
         busdta(12,nb) = vk 
         tbx(1,ntbx) = 6   
         tbx(5,ntbx) = qnetu(kt)                    
         if (ityp .eq. 1 .or. ityp .eq. 2) then 
            tbx(7,ntbx) = 1
         else   
            tbx(7,ntbx) = 2
         endif  
        
      else if ((ntypu(kt) .eq. 1 .or. ntypu(kt) .eq. 4 .or.
     &          ntypu(kt) .eq. 10) .and. newtyp .eq. 2) then 

C        B  --> BE     
C        BC --> BE     
C        BT --> BE     

         vlimn(kt) = vk   
         vlimx(kt) = vk   
         vstart(nb) = vk
         kbsdta(1,nb) = newtyp  
         busdta(11,nb) = vk 
         busdta(12,nb) = vk 
        
      else if ((ntypu(kt) .eq. 10 .and. newtyp .eq. -1) .or.    
     1         (ntypu(kt) .eq. -newtyp)) then                   
C       
C        BT --> B,  Disable LTC.
C        B* --> B*, Disable LTC.
C       
         unused = 0.0   
         ntypu(kt) = iabs(newtyp)  
         kbsdta(1,nb) = iabs(newtyp)
         if (ntran .gt. 0) then
            ltyp = mod (ltran(10,ntran),100)   
            if (ltyp .lt. 10) then 
               lixx = ltran(10,ntran) / 100
               ltran(10,ntran) = 100 * lixx + ltyp + 20
            endif
            k1 = ltran(1,ntran) 
            k2 = ltran(9,ntran) 
            if (ordltc .eq. 2) then 
               k1 = opt2inp(k1)   
               k2 = opt2inp(k2)   
            endif   
            qrem = 0.0  

C           Delete "R" record (K1-K2) in BRNCH array.

            pold = 0
            ptr = kbsdta(16,k1)
            found = 0
            do while (ptr. gt. 0 .and. found .eq. 0)
               if (ky(ptr) .eq. k2) then
                  if (brtype(ptr) .eq. 4) then   
                     found = ptr
                     if (pold .eq. 0) then
                        kbsdta(16,k1) = brnch_nxt(ptr)
                     else
                        brnch_nxt(pold) = brnch_nxt(ptr)
                     endif
                  endif
               endif
               pold = ptr
               ptr = brnch_nxt(ptr)
            enddo
C                                       
C           Delete transpose "R" record (K2-K1) in BRNCH array.
C                                       
            pold = 0
            ptr = kbsdta(16,k2)
            found = 0
            do while (ptr. gt. 0 .and. found .eq. 0)
               if (ky(ptr) .eq. k1) then
                  if (brtype(ptr) .eq. 4) then   
                     found = ptr
                     if (pold .eq. 0) then
                        kbsdta(16,k2) = brnch_nxt(ptr)
                     else
                        brnch_nxt(pold) = brnch_nxt(ptr)
                     endif
                  endif
               endif
               pold = ptr
               ptr = brnch_nxt(ptr)
            enddo
         endif  
        
      else  
        
         write (outbuf,330 )
  330    format (' Improper CHGBTY call ')  
         if (list) call prtout (1)
         write (outbuf, 340) bus(nb), base(nb), zone(nb), oldbty,   
     1      newbty  
         if (list) call prtout (1)
         return 
        
      endif 
        
      if (newtyp .gt. 0) then   
C       
C        The > test excludes bus type changes disabling LTC's.  
C       
C        Obtain final values
C       
         call allocq (nb, sngl(qnetu(kt)), sngl(qgen), qgnmax, 
     1                qgnmin, qld, totcap, usecap, totrek, 
     2                userek, unsked, qerr) 
         ftots = totcap + totrek
         fuses = usecap + userek
         fqmax = qgnmax 
         fqmin = qgnmin 
         fqgen = qgen   
         fvmin = vlimn(kt)                         
         fvmax = vlimx(kt)                         
        
         write (outbuf, 340) bus(nb), base(nb), zone(nb), oldbty,   
     1      newbty, otots, ftots, otots-ftots, oqgen, fqmin, fqmax, 
     2      oqgen-fqgen, vk, ovmin, ovmax, fvmin, fvmax 
  340    format (t2, a8, f6.1, t35, a2, t41, a2, 3x, a2, t50, 3f6.1,
     1      t72, 4f6.0, t98, 5f6.3) 
         if (list) call prtout (1)
        
      else  
        
         k1 = ltran(1,ntran)
         k2 = ltran(9,ntran)
         if (ordltc .eq. 2) then
            k1 = opt2inp(k1)  
            k2 = opt2inp(k2)  
         endif  
         if (nb .eq. k2) k2 = k1
         write (outbuf, 350) bus(nb), base(nb), bus(k2), base(k2),  
     1      zone(nb), oldbty, newbty
  350    format (t2, a8, f6.1, 1x, a8, f6.1, t35, a2, t41, a2, 3x,
     1      a2, t50, 3f6.1, t72, 4f6.1, t98, 5f6.3)
         if (list) call prtout (1)
 
      endif
c
c     Debug check on validity after bus modifications
c
      if (kase1(27) .ne. 0 .and. lskp .eq. 1) then
         call evdata (nb, perr, qerr)
      endif 
      return
      end   
