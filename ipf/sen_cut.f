C    @(#)sen_cut.f	20.3 8/20/98
      integer function sen_cut (luncut) 

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/cut.inc' 
      include 'ipfinc/bus.inc'  
      include 'ipfinc/ikk.inc' 
      include 'ipfinc/cut2.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/prt.inc' 
      include 'ipfinc/cbus.inc'   
      include 'ipfinc/branch.inc'   
      include 'ipfinc/alpha.inc'
      include 'ipfinc/xdata.inc'   
      include 'ipfinc/lfiles.inc'   
      include 'ipfinc/optim1.inc'   
      include 'ipfinc/ecvar.inc'   
      include 'ipfinc/alpha2.inc'   

      integer kolum(MAXBRN2), ktemp(MAXBUS), find_bus
      character *3 com(5)
      external kmpkol, swpkol

      sen_cut = 0
c
c     Temporarily reassign INP to LUNCUT
c
      inpold = inp
      inp = luncut
c
C     Establish connection matrix: add 2-terminal dc to y-matrix
c
      do i = 1, kdtot  
         k1 = dc2t(1,i)   
         k2 = dc2t(3,i)   
         kt = inp2opt(k1)
         mt = inp2opt(k2)

C        Add branch (KT,MT) to y-matrix   

         do isw = 1, 2
            ln = km(kt) - 1
            ls = kmlen(kt)
            do l = 1, ls
               if (ikmu(l+ln) .eq. mt) go to 102
            enddo
c
c           Appending an entity to km() requires relocating branch
c           list to end of km().
c
            if (yptr + ls + 1 .ge. MAXYE) then
               write (errbuf(1), 100) MAXYE
  100          format(' More than ', i5, ' Y-matrix entities')
               call prterx ('F',1)                            
               kerrsw = kerrsw + 1
               sen_cut = 1
               go to 9000
            endif

            insert = 0
            do l = 1, ls
              if (ikmu(l+ln) .gt. mt .and. insert .eq. 0) then
                insert = 1
                ikmu(l+yptr) = mt
                gkmu(l+yptr) = 0.0
                bkmu(l+yptr) = 0.0
              endif
              ikmu(l+yptr+insert) = ikmu(l+ln)
              gkmu(l+yptr+insert) = gkmu(l+ln)
              bkmu(l+yptr+insert) = bkmu(l+ln)
            enddo
            if (insert .eq. 0) then
              ikmu(ls+yptr+1) = mt
              gkmu(ls+yptr+1) = 0.0
              bkmu(ls+yptr+1) = 0.0
            endif
            km(kt) = yptr + 1
            kmlen(kt) = ls + 1
            yptr = yptr + ls + 1

  102       continue
            kt = inp2opt(k2)    ! swap kt and mt
            mt = inp2opt(k1)
         enddo
      enddo

C     Add N-terminal d-c to y-matrix

      do i = 1, mtdcln   
         k1 = dcmtln(1,i)
         k2 = dcmtln(2,i)
         kt = inp2opt(k1)
         mt = inp2opt(k2)

         do isw = 1, 2
            ln = km(kt) - 1
            ls = kmlen(kt)
            do l = 1, ls
               if (ikmu(l+ln) .eq. mt) go to 104
            enddo
c
c           Appending an entity to km() requires relocating branch
c           list to end of km().
c
            if (yptr + ls + 1 .ge. MAXYE) then
               write (errbuf(1), 100) MAXYE
               call prterx ('F',1)                            
               kerrsw = kerrsw + 1
               sen_cut = 1
               go to 9000
            endif

            insert = 0
            do l = 1, ls
              if (ikmu(l+ln) .gt. mt .and. insert .eq. 0) then
                insert = 1
                ikmu(l+yptr) = mt
                gkmu(l+yptr) = 0.0
                bkmu(l+yptr) = 0.0
              endif
              ikmu(l+yptr+insert) = ikmu(l+ln)
              gkmu(l+yptr+insert) = gkmu(l+ln)
              bkmu(l+yptr+insert) = bkmu(l+ln)
            enddo
            if (insert .eq. 0) then
              ikmu(ls+yptr+1) = mt
              gkmu(ls+yptr+1) = 0.0
              bkmu(ls+yptr+1) = 0.0
            endif

            km(kt) = yptr + 1
            kmlen(kt) = ls + 1
            yptr = yptr + ls + 1

  104       continue
            kt = inp2opt(k2)    ! swap kt and mt
            mt = inp2opt(k1)
         enddo
      enddo

C     Establish connection matrix  
C     IKK assignments:  
C         (1,*)  0     node is eliminated 
C                1     node is retained   
C         (2,*)  0     node is an internal node   
C                1     retained node is a boundary node  
C         (3,*)        not used
C         (4,*)        not used
C         (5,*)  NSYS  (+/-) Subnetwork number  

      itot=1
      do i = 1, ntot_alf
         nb = alf2inp(i)
         kt = inp2opt(nb)   
         jtot = 0
         if (kmlen(kt) .eq. 0) call erexit()
         do l = km(kt), km(kt)-1+kmlen(kt)      
            mt = ikmu(l)                                 
            kolum(itot+jtot) = opt2inp(mt)
            jtot = jtot+1   
         enddo
         ikk(1,nb) = 0   
         ikk(2,nb) = 0   
         ikk(3,nb) = 0   
         ikk(4,nb) = 0   
         ikkind(1,nb) = itot 
         ikkind(2,nb) = jtot 
         itot = itot+jtot
      enddo

      do i = ntot_alf+1,ntot
         nb = alf2inp(i)
         jtot=0
         ikk(1,nb) = 0   
         ikk(2,nb) = 0   
         ikk(3,nb) = 0   
         ikk(4,nb) = 0   
         ikkind(1,nb) = itot 
         ikkind(2,nb) = jtot 
      enddo

      if (itot .gt. MAXYE) then
         write (errbuf(1),122) itot,MAXYE  
  122    format ('TOTAL BRANCHES ( INCLUDING TRANSPOSES ) IN', 
     1        'SYSTEM IS ',i5,'.  LIMIT IS ',i5,'. ')   
         call prterx ('F',1)   
         kerrsw = kerrsw + 1   
         sen_cut = 1
         go to 9000
      endif
C
C     Read input data to identify retained nodes
C
      kerrsw = 0  
      inptls = 0  
      inptsw = 0  
  210 call cutinp   
      if (inptsw .eq. 0) go to 710  
      go to (240,470,490,510,440,570) inptsw

C     INPTSW assignments:  
C        1 - SAVE Z0NES ... SAVE BASES 
C        2 - SAVE BUSES
C        3 - INCLUDE BUSES 
C        4 - EXCLUDE BUSES 
C        5 - PI-BACK BUSES 
C        6 - CUT BRANCHES  

      go to (240,440,470,490,510,570) inptsw

C     Retained system specified by "SAVE ZONES..."

  240 call forbtm   
      write (outbuf,250)
  250 format('0 Study area defined by the following SAVED ZONES..') 
      call fortop   
      call prtout(1)
      write (outbuf,260) ' '
  260 format('0', 20x, 20('--  '), a1) 
      call prtout(1)
      do 280 kst = 1,idat,20  
         kend = min0 (kst+19,idat)   
         write (outbuf,270) (zdata(i),i=kst,kend)  
  270    format('0', 20x, 20(a2,2x)) 
         call prtout(1)
  280 continue  
      write (outbuf,260) ' '
      call prtout(1)
      if (idat2 .eq. 0) go to 340   

C     Retained system specified by "SAVE ZONES....SAVE BASES" 

      write (outbuf,290)
  290 format('0 Study area defined by the following saved base KV''s.') 
      call prtout(1)
      write (outbuf,300) ' '
  300 format('0', 37x, 10('  -----'), a1)  
      call prtout(1)
      do 320 kst = 1,idat2,10 
         kend = min0 (kst+9,idat2)   
         write (outbuf,310) (basedt(i),i=kst,kend) 
  310    format('0', 37x, 10f7.1)
         call prtout(1)
  320 continue  
  330 write (outbuf,300) ' '
      call prtout(1)
  340 continue  

      do 390 ix = 1, ntot_alf
         k = alf2inp(ix)
         l = 0 
         if (idat .gt. 0) then 
            do 350 l = 1,idat 
               if (zone(k) .eq. zdata(l)) then 
                  idata(1,l) = 1  
                  go to 360 
               endif   
  350       continue
            l = -1  
  360       continue
         endif 
         i = 0 
         if (idat2 .gt. 0) then
            do 370 i = 1,idat2
               if (base(k) .eq. basedt(i)) then
                  idata(2,i) = 1  
                  go to 380 
               endif   
  370       continue
            i = -1  
  380       continue
         endif 
         if (min0 (i,l) .ne. -1) ikk(1,k) = 1
  390 continue  
 
      if (idat .gt. 0) then 
         do 410 l = 1,idat 
            if (idata(1,l) .eq. 0) then 
               write (errbuf(1),400) zdata(l)
  400          format ('Zone ',a2,' is not in system and is ignored.') 
               call prterx ('W',1)   
            endif   
  410    continue
      endif 
      if (idat2 .gt. 0) then
         do 430 i = 1,idat2
           if (idata(2,i) .eq. 0) then 
              write (errbuf(1),420) basedt(i)  
  420         format (' Base KV ',f6.1,' is not in system ',   
     1          'and is ignored. ')  
              call prterx ('W',1)  
           endif   
  430    continue
      endif 
      go to 210 

C     Pi-back buses (not used)

  440 continue
      go to 210 

C     Retained system specified with "SAVE BUSES"

  470 n = 1   
      call forbtm   
      write (outbuf,480)
  480 format ('0           Input listing of SAVED BUSSES '  )   
      call fortop   
      call prtout(1)
      call space(2) 
      go to 530 

C     Retained system expanded with "INCLUDE BUSES" 

  490 n = 1   
      call forbtm   
      write (outbuf,500)
  500 format ('0           Input listing of INCLUDE BUSSES ' )  
      call fortop   
      call prtout(1)
      go to 530 

C     Retained system contracted with "EXCLUDE BUSES"  

  510 n = 0   
      call forbtm   
      write (outbuf,520)
  520 format ('0           Input listing of EXCLUDE BUSSES ' )  
      call fortop   
      call prtout(1)
  530 do 560 i = 1,idat   
         write (outbuf,540) busdt(i),basedt(i) 
  540    format(7x,a8,f6.1)
         call prtout(1)
         nb = find_bus(busdt(i),basedt(i)) 
         if (nb .le. 0) then   
            write (errbuf(1),550) busdt(i),basedt(i)   
  550       format ('0 Bus ',a8,f7.1,' is not in system. ')
            call prterx ('W',1)
            kerrsw = 1   
         else  
            ikk(1,nb) = n
         endif 
  560 continue  
      go to 210 

C     Retained system defined by CUT BRANCHES

  570 do 650 l = 1,idat,2   
         ib  = find_bus (busdt(l),basedt(l)) 
         if (ib .le. 0) then   
            write(errbuf(1),580) busdt(l),basedt(l)
  580       format(' Retained bus ',a8,f6.1,
     &       ' in CUT_BRANCH is not in system.  Recovery attempted. ')
            call prterx ('W',1)
            go to 650  
         else if (ikk(1,ib) .eq. 0 .and. ikk(2,ib) .ne. 0) then
            write (   errbuf(1),590) busdt(l),basedt(l)   
  590          format(' Bus ',a8,f7.1,' is in saved and cut networks. Re
     &covery attempted. ')   
            call prterx ('W',1)
         else  
            ikk(1,ib) = 1  
            ikk(2,ib) = 1  
         endif 
         jb = find_bus (busdt(l+1),basedt(l+1))  
         if (jb.le. 0) then
            write (errbuf(1),610) busdt(l+1),basedt(l+1)   
  610       format(' Cut bus ',a8,f6.1,' is not in system. Recovery atte
     &mpted. ')   
            call prterx ('W',1)
         else if (ikk(1,jb) .eq. 1 .and. ikk(2,jb) .eq. 1) then
            write (errbuf(1),590) busdt(l+1),basedt(l+1)   
            call prterx ('W',1)
         else  
            if (ib .gt. 0) then
               lf = ikkind(1,ib)
               ls = lf + ikkind(2,ib) - 1
               do j = lf, ls
                  if (kolum(j) .eq. jb) go to 640 
               enddo
               write (errbuf(1),630) busdt(l),basedt(l),busdt(l+1),
     1                               basedt(l+1)   
  630          format(' Cut branch ',a8,f6.1,' to ',a8,f6.1,
     &           ' is non-existant. Recovery attempted by using ',
     &           'terminals as identification.')
               call prterx ('W',1) 
  640          continue
            endif  
         ikk(1,jb) = 0  
         ikk(2,jb) = 1  
      endif 
      if (odata(l) .eq. 'INT' .and. ib .gt. 0 .and. jb .gt. 0) then 
         kint = kint + 1
         intflg(1,kint) = ib
         intflg(2,kint) = jb
      endif 
  650 continue  
      if (kint .gt. 0) then 
         call forbtm
         write (outbuf,652) 
  652    format ('0 Listing of Interchange Branches ')  
         call fortop
         call prtout (1)
         call space(1)  
         do 656 i = 1,kint  
            k1 = intflg(1,i)   
            k2 = intflg(2,i)   
            write (outbuf,654) bus(k1),base(k1),bus(k2),base(k2)   
  654       format (2x,a8,f7.1,2x,a8,f7.1) 
            call prtout(1) 
  656    continue   
         call space(2)  
      endif 
C       
C     Initialize kernel 
C       
      nl = 0
      do i = 1, ntot
         ktemp(i) = 0
      enddo

      do ix = 1, ntot_alf
         i = alf2inp(ix)
         if (ikk(2,i) .eq. 1) then 
            if (ikk(1,i) .eq. 1) then  
               ikk(5,i) = 1
            else   
               ikk(5,i) = -1   
            endif  
            nl = nl + 1
            ktemp(nl) = i  
         endif 
      enddo

      n = -1
      nf = 1
      na = nl   
      nflag = na
C       
C     Expand out from kernel cluster, first as N < 0 for eliminated
c     side, next as N > 0 for retained side.
C       
      do while ((n .lt. 0 .and. iabs (n) .le. ntot_alf) .or. 
     &          (n .gt. 0 .and. na .lt. ntot_alf .and. na .gt. nflag)) 
         do ksw = 1, 2   ! ksw = 1 expands negative side, 
c                        ! ksw = 2 expands positive side.
            do i = nf,nl  
               k = ktemp(i)  
               if (ikk(5,k) .eq. n) then 
                  lf = ikkind(1,k)
                  ls = lf + ikkind(2,k) - 1
                  do j = lf, ls
                     m = kolum(j)  
                     if (ikk(5,m) .eq. 0) then 
                        na = na + 1
                        ktemp(na) = m  
                        ikk(5,m) = isign(iabs(n)+1,n)  
                        if (ikk(1,k) .eq. 0 .and. ikk(1,m) .eq. 1) then
                           write (errbuf(1),680) bus(k),base(k),bus(m),
     &                        base(m) 
  680                      format(' Incomplete enclosure: Branch ',a8,
     &                        f7.1,2x,a8,f7.1,
     &                        ' spans retained and cut networks.')
                           write (errbuf(2),685) ikk(5,k),ikk(5,m) 
  685                      format(' Recovery attempted.',2i4)
                           call prterx ('W',2) 
                           kerrsw = 2  
                        else   
                           ikk(1,m) = ikk(1,k) 
                        endif  
                     else  
                        if (ikk(1,k) .ne. ikk(1,m)) then   
                           if (ikk(2,k) .eq. 0 .or. ikk(2,m) .eq. 0) 
     &                         then
                              write (errbuf(1),680) bus(k),base(k),
     &                          bus(m),base(m)  
                              write (errbuf(2),685) ikk(5,k),ikk(5,m)  
                              call prterx ('W',2)  
                              kerrsw = 2   
                           endif   
                        endif  
                     endif 
                  enddo
               endif 
            enddo
            if (ksw .eq. 1) then
               n = iabs(n)          ! begin second pass 
            else
               n = -iabs(n) - 1 
               nf = nl + 1
               nl = na
               nflag = na  
            endif
         enddo
      enddo
C       
C     1. Determine status of any passive DC nodes.  
C     2. Assure that all retained converters have unconditionally   
C        retained commutator buses. 
C       
  710 continue
      do i = 1,mtdcbs   
         k = dcmtbs(1,i)   
C       
C        Examine converters status.  If retained, unconditionally 
C        retain all other DC nodes in same circuit.   
C       
         if (ikk(1,k) .eq. 1) then
            jckt = dcmtbs(21,i)   
            do j = 1,mtdcbs   
               n = dcmtbs(1,j)   
               if (dcmtbs(21,j) .eq. jckt) then
                  if (i .ne. j) ikk(1,n) = ikk(1,k) 
               endif
            enddo
            m = dcmtbs(3,i)   
            if (m .gt. 0) then
C       
C              Retain commutator bus if converter bus is also 
C              retained.  
C       
               ikk(1,m) = ikk(1,k) 
            endif
         endif
      enddo
C       
C     If either converter of a two-terminal DC system is retained,  
C     retain the other converter.   
C       
      do i = 1,kdtot
         k1 = dc2t(1,i)   
         k2 = dc2t(3,i)   
         if(ikk(1,k1) .gt. 0 .or. ikk(1,k2) .gt. 0) then
            ikk(1,k1) = 1   
            ikk(1,k2) = 1   
            k1 = dc2t(33,i)  
            k2 = dc2t(34,i)  
            ikk(1,k1) = 1   
            ikk(1,k2) = 1   
         endif
      enddo
C       
C     Determine number of systems   
C       
      nsyst = 0 
      do i = 1,ntot 
         ikk(2,i) = 0  
         ikk(5,i) = 0  
      enddo

      na = 0
      nl = 0
      do ix = 1, ntot_alf
         i = alf2inp(ix)
         if (ikk(1,i) .eq. 1 .and. ikk(5,i) .eq. 0) then   
            nsyst = nsyst + 1  
            na = na + 1
            ktemp(na) = i  
            ikk(5,i) = nsyst   
C       
C           Expand around kernel system
C       
            do while (na .gt. nl)
               nf = nl + 1
               nl = na
               do j = nf,nl   
                  k = ktemp(j)   
                  lf = ikkind(1,k)
                  ls = lf + ikkind(2,k) - 1
                  do l = lf, ls
                     m = kolum(l)   
                     if (ikk(1,m) .ne. 0) then  
                        if (ikk(5,m) .eq. 0) then   
                           na = na + 1  
                           ktemp(na) = m
                           ikk(5,m) = nsyst 
                        endif   
                     endif  
                  enddo
               enddo
            enddo
         endif 
      enddo
C       
C     Identify boundary nodes   
C       
      do ix = 1, ntot_alf
         i = alf2inp(ix)
         if (ikk(1,i) .eq. 1) then 
            lf = ikkind(1,i)
            ls = lf + ikkind(2,i) - 1
            do j = lf, ls
               m = kolum(j)   
               if (ikk(1,m) .eq. 0) then  
                  ikk(2,i) = 1
                  ikk(2,m) = 1
               endif  
            enddo
         endif
      enddo

      do jsyst = 1,nsyst
         call forbtm   
         write (outbuf,840) jsyst  
  840    format('  Listing of cut branches for subsystem No.',i3)  
         call fortop   
         call prtout (1)   
         write (outbuf,842)
  842    format ('  Retained bus side    Cut branches   ') 
         call prtout (1)   
         call space (2)
         do ix = 1, ntot_alf
            i = alf2inp(ix)
            if (ikk(5,i) .eq. jsyst) then 
               if (ikk(2,i) .eq. 1) then  
                  lf = ikkind(1,i)
                  ls = lf + ikkind(2,i) - 1
                  do l = lf, ls
                     m = kolum(l)
                     if (ikk(1,m) .eq. 0) then   
                        status = sen_cutbr(i, m)
                        write (outbuf,850) bus(i),base(i),bus(m),
     &                     base(m) 
  850                   format(1x,a8,f6.1,7x,a8,f6.1)
                        call prtout (1)  
                     endif   
                  enddo
               endif  
            endif 
         enddo
      enddo
C       
C     Print out Cutting header  
C       
      do jsyst = 1,nsyst   
         if (jsyst .eq. 1) then
            call forbtm
            write(outbuf,950) nsyst
  950       format('0 ',i2,
     &        ' Separate cut sub-networks encountered.')  
            call fortop
            call prtout (1)
            call space (2) 
         endif 
         if (jsyst .gt. 1) call forbtm 
         write (outbuf,1000) jsyst 
 1000    format ('0 Input listing of Subsytem ',i3,
     &        ' -- " " pertains to internal retained node ' )   
         if (jsyst .gt. 1) call fortop 
         call prtout(1)
         write (outbuf,1010)   
 1010    format(  35x,'"E" pertains to envelope node ' )   
         call prtout(1)
         call space(2) 
         knt = 0 
         do ix = 1, ntot_alf
            i = alf2inp(ix)
            if (ikk(5,i) .eq. jsyst) then 
               knt = knt+1  
               busdt(knt) = bus(i)  
               basedt(knt) = base(i)
               com(knt) = '   ' 
               if (ikk(4,i) .ne. 0) com(knt) = 'P  '  
               if (ikk(2,i) .eq. 1) com(knt)(2:2) = 'E'   
               if (knt .eq. 5) then   
                  write (outbuf,1040) (com(j),busdt(j),basedt(j),
     &               j=1,knt) 
 1040             format(5(1x,a3,1x,a8,f7.1,3x))  
                  call prtout(1)  
                  knt = 0 
               endif  
            endif 
         enddo
         if (knt .gt. 0) then  
            write (outbuf,1040) (com(j),busdt(j),basedt(j),j=1,knt)
            call prtout(1) 
         endif 
      enddo
c
c     Remove cut system from Y-matrix, TXTIE
c
      jt = 1
      do while (jt .le. ntxtie)  
         k1 = txtie(1,jt) 
         k2 = txtie(2,jt) 
         if (ikk(1,kt) .eq. 0 .or. ikk(1,k2) .eq. 0) then
            do i = jt+1, ntxtie
              do j = 1, 10
                txtie(j,i-1) = txtie(j,i)
              enddo
            enddo
            ntxtie = ntxtie - 1
         else
           jt = jt + 1
         endif
      enddo

      do i = 1, ntot
        if (ikk(1,i) .eq. 0) then
           kt = inp2opt(i)
           kmlen(kt) = 0
           kvolt(kt) = 0
        endif
      enddo

      if (kdebug .gt. 0) then
         write(9,1070) 
 1070    format('1 DEBUG DUMP OF "IKK" ARRAY '//)  
         do ix = 1, ntot_alf
            i = alf2inp(ix)
            write(9,1080) i,bus(i),base(i),(ikk(j,i),j=1,5)   
 1080       format(1x,i6,2x,a8,f7.1,5i6)  
         enddo
      endif
 9000 continue  
c
c     Restore INP
c
      inp = inpold
      return
      end   
