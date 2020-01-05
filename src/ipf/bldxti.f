C    @(#)bldxti.f	20.11 10/13/99
      subroutine bldxti (z_threshold)
      real z_threshold
 
C     Build "TXTIE" array of phase shifter bus ties and
C     optionally, of low impedance branches
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/brtype.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/komps.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/phase.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/snput.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/lfiles.inc'

      common /is_batch / is_batch

      common /ran002/ brtypec(9)  
      character brtypec*2         

      complex * 16 y(2,2) 
      double precision ratio, angle
      character id*1
      integer p, q, pold, error, ktxtie, intrst(MAXBUS)
      logical found, finished
      external kmptxt,swptxt
  
      ntxtie = 0  
      if (option(9) .le. 0) go to 900

      do i = 1, ntot
         intrst(i) = 0
      enddo
c
c     Pass 1 - process phase shifters
c
      do 110 jt = 1,jphno
        k1 = jphid(1,jt)   
        k2 = jphid(2,jt)   
        kt = inp2opt(k1)
        mt = inp2opt(k2)
        ltc = 0
C       
C       Find LTC (automatic transformer) and NBR (branch) index.  
C       
        do ksw = 0, 1
          p = kbsdta(16,k1)
          do while (p .gt. 0)
            if (ky(p) .eq. k2) then
               found = .true.
               do while (p .gt. 0)  ! Found branch k1-k2. Restrict loop
C                                   ! to this branch only.
                 ltyp = brtype(p)  
C       
                 if (ltyp .eq. BRTYP_R) then
C       
C                  LTC phase shifters will be processed in a 
C                  special LP mode. Store index in TXTIE(7,*)
C       
                   do i = 1, ntota   
                     if (ltran(10,i) .eq. 3) then   
                       if (ltran(1,i) .eq. kt .and. 
     &                     ltran(9,i) .eq. mt) then   
                         ltc = i  
                         go to 100   
                       else if (ltran(1,i) .eq. mt .and. 
     &                          ltran(9,i) .eq. kt) then  
                         ltc = -i 
                         go to 100
                       endif   
                     endif  
                   enddo
                   ltc = 0   
  100              continue  

                 else if (ltyp .eq. BRTYP_TP) then
                   q = brnch_ptr(p)
                   nbr = iabs(q)
                   if (brnch(5,nbr) .gt. brnch(6,nbr) .or.
     &                 brnch(5,nbr) .lt. 0.0 .or.
     &                 brnch(6,nbr) .lt. 0.0) then
                     write (errbuf(1), 10000) bus(k1), base(k1), 
     &                 bus(k2),  base(k2), id, brtypec(ltyp),
     &                 brnch(5,nbr), brnch(6,nbr)
10000                format ('0 Illegal phase shifter impedance ', 
     &                 a8, f7.1, 1x, a8, f7.1, ' id ', a, ' type ', 
     &                 a2, ' R, X = ', 2f8.5)
                     if (is_batch .eq. 0) then
                       call prterx ('W',1)
                     else
                       call prterx ('E',1)
                     endif
                   endif
                   ratio = 1.0d0 
                   if (abs(brnch(9,nbr)) .lt. option(9) .and. 
     &                 ltc .eq. 0) go to 110 
                   if (q .gt. 0) then
                      angle = 0.0174532925d0 * brnch(9,nbr) 
                   else
                      angle = -0.0174532925d0 * brnch(9,nbr) 
                   endif
c
c                  Consolidate parallels
c
                   ktxtie = 0
                   i = 1
                   do while (i .le. ntxtie .and. ktxtie .eq. 0)
                     if (txtie(1,i) .eq. kt .and. 
     &                   txtie(2,i) .eq. mt) then
                       ktxtie = i
                     else
                       i = i + 1
                     endif
                   enddo
                   if (ktxtie .eq. 0) then
                     ntxtie = ntxtie + 1   
                     if (ntxtie .gt. MAXBUSTIE) then
                       write (errbuf(1), 10010) MAXBUSTIE
10010                  format(' More than ', i4, ' bus ties.  ',
     &                        'Ideal bus tie scheme aborted.') 
                       call prterx ('W',1)   
                       ntxtie = 0
                       go to 900
                     endif
                     ktxtie = ntxtie
                     txtie(1,ntxtie) = kt
                     txtie(2,ntxtie) = mt
                     txtie(3,ntxtie) = ratio 
                     if (ltc .eq. 0) then  
                       txtie(4,ntxtie) = angle
                     else  
                       x = isign (1,ltc)  
                       i = iabs (ltc) 
                       txtie(4,ntxtie) = x * tap(i)   
                     endif 
                     intrst(k1) = ntxtie
                     intrst(k2) = ntxtie
C       
C                    The following is a temporary flag to indicate 
C                    an LTC phase shifter.  It will be sorted last in 
C                    TXTIE array. 
C       
                     txtie(7,ntxtie) = ltc
                     txtie(8,ntxtie) = 0  

                     txtie(5,ntxtie) = 0.0
                     txtie(6,ntxtie) = 0.0
                     txtie(9,ntxtie) = 0.0
                     txtie(10,ntxtie) = 0.0
                   endif
C       
C                  Obtain 2-port Y-matrix
C       
                   call pieqiv (p,y,error) 
                   txtie(5,ktxtie) = txtie(5,ktxtie) + dreal(y(1,2))
                   txtie(6,ktxtie) = txtie(6,ktxtie) + dimag(y(1,2))   
                   txtie(9,ktxtie) = txtie(9,ktxtie) + dreal(y(1,1))  
                   txtie(10,ktxtie) = txtie(10,ktxtie) + dimag(y(1,1))

                 else if ( ltyp .eq. BRTYP_PEQ ) then
c
c                  ignore phase shifter if in section
c
                   p = 0        ! force end of loop

                 else
C       
C                  Illegal branch type - program error!! 
C       
                   id = brid(p)
                   write (errbuf(1), 10020) bus(k1), base(k1), 
     &                bus(k2),  base(k2), id, brtypec(ltyp)   
10020              format ('0 Illegal branch type ', a8, f7.1, 1x,
     &                a8,f7.1, ' id ', a, ' type ', a2, 
     &                ' in parallel with a phase shifter ')   
                   if (is_batch .eq. 0) then
                     call prterx ('W',1)
                   else
                     call prterx ('E',1)
                   endif
                 endif

                 if (p.gt.0) p = brnch_nxt(p)
                 if (p .gt. 0) then
                   if (ky(p) .ne. k2) p = 0  ! End of branch k1-k2
C                                            ! Force end of loop
                 endif
               enddo
            else
               p = brnch_nxt(p)
            endif
          enddo
          if (.not.found) then
C       
C           Could not locate branch - program error!! 
C       
            write (errbuf(1), 10030) bus(k1), base(k1), bus(k2),   
     &         base(k2)
10030       format ('0 Could not locate phase shifter ', a8, f7.1, 
     &         1x, a8, f7.1)
            if (is_batch .eq. 0) then
               call prterx ('W',1)
            else
               call prterx ('E',1)
            endif
          endif
c
c         Process transpose k2-k1 using same logic.
c
          ktemp = k1
          k1 = k2
          k2 = ktemp
          kt = inp2opt(k1)
          mt = inp2opt(k2)
          ltc = -ltc
        enddo
  110 continue
c
c     Pass 2 - process low impedance branches
c
      if (z_threshold .le. 0.0) go to 900
      y_threshold = 1.0 / z_threshold
      do 140 ib = 1, ntot_alf
        nb = alf2inp(ib)
        p = kbsdta(16,nb)
        do while (p .gt. 0)
c
c         Process new branch, Skip type R, LD, LM, and TP
c         records (TP's have been processed in pass 1 above)
c
          k1 = kx(p)  
          k2 = ky(p)
          kt = inp2opt(k1)
          mt = inp2opt(k2)
          if (brtype(p) .eq. 4 .or. brtype(p) .eq. 2 .or.
     &        brtype(p) .eq. 6 .or. brtype(p) .eq. 7) then
            p = brnch_nxt(p)
            go to 130
c
c         For this version only, supplement TP records with T record 
c         bus ties.
c
          else if (brtype(p) .eq. 1) then
            do while (p .gt. 0 .and. ky(p) .eq. k2)
              p = brnch_nxt(p)
            enddo
            go to 130
          else if (brtype(p) .ne. 5) then
            p = brnch_nxt(p)
            go to 130
          else 
            if (intrst(k1) .ne. 0 .and. intrst(k2) .ne. 0) then
              found = .false.
              jt = 1
              do while (jt .le. ntxtie .and. .not. found)
                if (txtie(1,jt) .eq. kt .and. txtie(2,jt) .eq. mt) then
                  found = .true.
                else
                  jt = jt + 1
                endif
              enddo
              if (found) then 
                do while (p .gt. 0 .and. ky(p) .eq. k2)
                  p = brnch_nxt(p)
                enddo
                go to 130
              endif
            endif
          endif
c
c         Use Y-matrix to screen admittance 
c
          do l = km(kt), km(kt)+kmlen(kt)-1
            if (ikmu(l) .eq. mt) then
              if (abs (bkmu(l)) .gt. y_threshold) then
                go to 120
              else
                p = brnch_nxt(p)
                go to 130
              endif
            endif
          enddo
          p = brnch_nxt(p)
          go to 130

  120     continue
          ltyp = brtype(p)  
          if (ltyp .eq. 1 .or. ltyp .eq. 3 .or. ltyp .eq. 8) then
            ratio = 1.0d0 
            angle = 1.0d0 
          else if (ltyp .eq. 5) then
            q = brnch_ptr(p)
            nbr = iabs (q)
            if (q .gt. 0) then
              ratio = (brnch(9,nbr) / base(k1)) 
     &              / (brnch(10,nbr) / base(k2))
              angle = 0.0
            else
              ratio = (brnch(10,nbr) / base(k1)) 
     &              / (brnch(9,nbr) / base(k2))
              angle = 0.0
            endif
          else
            write (errbuf(1), 10040) bus(k1), base(k1), 
     &        bus(k2),  base(k2), brid(p), brtypec(ltyp)   
10040       format ('0 Program error 01 (bldxti) ', a8, f7.1, 1x,
     &        a8,f7.1, ' id ', a, ' type ', a2)
            if (is_batch .eq. 0) then
              call prterx ('W',1)
            else
              call prterx ('E',1)
            endif
          endif
C       
C         Obtain 2-port Y-matrix
C       
          call pieqiv (p, y, error) 
          if (dimag(y(1,2)) .lt. y_threshold) then
            p = brnch_nxt(p)
            go to 130
          endif
          ntxtie = ntxtie + 1   
          if (ntxtie .gt. MAXBUSTIE) then
            write (errbuf(1), 10010) MAXBUSTIE
            call prterx ('W',1)   
            ntxtie = 0
            go to 900
          endif
          txtie(1,ntxtie) = kt
          txtie(2,ntxtie) = mt
          txtie(3,ntxtie) = ratio 
          txtie(4,ntxtie) = angle
          intrst(k1) = ntxtie
          intrst(k2) = ntxtie
          txtie(5,ntxtie) = dreal(y(1,2))
          txtie(6,ntxtie) = dimag(y(1,2))   
          txtie(7,ntxtie) = 0.0
          txtie(8,ntxtie) = 0  
          txtie(9,ntxtie) = dreal(y(1,1))  
          txtie(10,ntxtie) = dimag(y(1,1))
c
c         Loop through any parallel branches
c
          finished = .false.
          do while (.not. finished)
            if (ltyp .eq. 1) then
              pold = p
              p = brnch_nxt(p)
              do while (p .gt. 0 .and. ky(p) .eq. k2 .and.
     &                  brid(p) .eq. brid(pold))
                p = brnch_nxt(p)
              enddo
            else
              p = brnch_nxt(p)
            endif
            if (ky(p) .eq. k2) then
C       
C             Obtain 2-port Y-matrix
C       
              call pieqiv (p, y, error) 
              txtie(5,ntxtie) = txtie(5,ntxtie) + dreal(y(1,2))
              txtie(6,ntxtie) = txtie(6,ntxtie) + dimag(y(1,2))   
              txtie(9,ntxtie) = txtie(9,ntxtie) + dreal(y(1,1))  
              txtie(10,ntxtie) = txtie(10,ntxtie) + dimag(y(1,1))

            else
              finished = .true.
            endif
          enddo
  130     continue
       enddo
  140  continue

       if (ntxtie .gt. 0) then
         call qiksrt(1, ntxtie, kmptxt, swptxt)
         if ( idswa .ne. 0 ) then
           write (dbug, 10050)
10050      format( '0 DUMP OF txtie(i,j) from BLDXTI' )
            do i = 1,ntxtie
              write (dbug, 10060) i, ifix(sngl(txtie(1,i))),
     &              ifix(sngl(txtie(2,i))), (txtie(j,i), j=3,6),
     &              ifix(sngl(txtie(7,i))), ifix(sngl(txtie(8,i))),
     &              (txtie(j,i), j = 9,10)
10060         format( i3, 2i5, 2f7.3, 1x, 2e10.3, 2i5, 2e10.3)
            enddo
          endif
        endif 
        
  900   continue  
        
        return
        end   
