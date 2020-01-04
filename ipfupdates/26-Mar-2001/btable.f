C    %W% %G%
C****************************************************************
C
C   File: btable.f
C
C   Purpose: Routine to build solution arrays (tables).
C
C   Author: BPA staff        Date: 18 May 1992
C   Called by: p_gtdata.f, dfnbse.f
C
C****************************************************************
      subroutine btable (fltstr, reset_fatal_error) 
      integer fltstr        
      logical reset_fatal_error
c        
c     "FLTSTR" : 0 - do not rebuild voltage arrays.    
c                1 - rebuild voltage arrays. 
c
c     "RESET_FATAL_ERROR" .false. - abort if "F" errors encountered
c                         .true.  - continue if "F" errors encountered
c                     
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/com008.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/phase.inc'
      include 'ipfinc/pqcurves.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tempbsnm.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/ycomp.inc'
      include 'ipfinc/errorx.inc'
c
c     set up type codes
c
      include 'ipfinc/bstype.inc'
      include 'ipfinc/brtype.inc'

      common /dbkk/ dgkk, dbkk, bkkadj
      double precision dgkk, dbkk, bkkadj

      common /is_batch / is_batch

      character xbuf*120, type*1, id*1, cbyear*2, cbtype*1, idnext*1
      complex * 16  y(2,2)
      integer sect, xsect, p, pold, pnxt, q, aq, bustyp, oldsrt,
     &        k2next, sectnext, typenext, tpnxt, error, chkerr, 
     &        old_ltype, kerr, psect
      logical check(10), flag
c     
      check(1) = .false.             !Branch data R/X check
C                                    ! (kase1(9) .ne. 0)   
      check(2) = .true.              !Branch data sort check
      check(3) = .true.              !Phase shifter consistency check
c
c     Reorder e() and f() arrays if voltages not rebuilt and ordvlt = 2

c
      if (fltstr .eq. 0 .and. ordvlt .eq. 2) then
         call mvnew1d (e, opt2inp, ntot)  
         call mvnew1d (f, opt2inp, ntot)  
      endif
c       
c     Store Q_NET in CAPCOR(2,*)    
c      
      if (ordcap .eq. 2) then
         do nb = 1, ntot      
            kt = inp2opt(nb)
            capcor(2,nb) = qnetu(kt)
         enddo
         ordcap = 1
      endif
c
c     Flag array order states
c
      ordcap = 1  
      orddc  = 1   
      ordltc = 1  
      ordtbx = 1  
      ordtie = 1  
      ordvlt = 1  
      ordymx = 1

      jphno = 0       
      jtie = 0     
      kabort = 0     
      ntotb = 0     
      ntota = 0     
      nycomp = 0  
      nbslck = 0     
      kdtot = 0     
      mtdcbs = 0    
      mtdcln = 0 
      nztot = 0      
      yptr = 0
c
c     Store X-data pointer in busxdtptr() and tempory P/Q curve
c     data pointer in buspqptr()
c
      do nb = 1, ntot
         buspqptr(nb) = 0
      enddo

      do i = 1, numcurv
         ieq = pqbusptr(i)
         if (ieq .gt. 0) buspqptr(ieq) = i
      enddo
      pq_flag = .true.

      do nb = 1, ntot
         busxdtptr(nb)  = 0
      enddo
      do i = 1, kxtot
         kxd = xdata(1,i)
         if (kxd .gt. 0) busxdtptr(kxd) = i
      enddo
      xdt_flag = .true.
c
c     Update ntot_alf counter
c
      ntot_alf = ntot
      do while (bus(alf2inp(ntot_alf)) .eq. srtlst .and. 
     &          ntot_alf .gt. 0)
        ntot_alf = ntot_alf - 1
      enddo
c      
c     ***********************************
c     process bus data                  *
c                                       *
c                                       v
      do 1070 nbx = 1, ntot_alf
      nb = alf2inp(nbx)
      if (bus(nb) .eq. srtlst) then
         km(nb) = 0
         kmlen(nb) = 0 
         go to 1070
      endif
      bustyp = kbsdta(1,nb)    
      inetr(nb) = 0.0d0
      ineti(nb) = 0.0d0
      km(nb) = yptr + 1
      kmlen(nb) = 0 

      if (bustyp .eq. BSTYP_BD .or. bustyp .eq. BSTYP_BM) then
c            
c        process "bd" and "bm" busses      
c              
         dgkk = 0.0d0
         dbkk = 0.0d0  
         qloadu(nb) = 0.0d0
         ploadu(nb) = 0.0d0
         qnetu(nb) = 0.0d0
         pnetu(nb) = 0.0d0    
         inetr(nb) = 0.0d0   
         ineti(nb) = 0.0d0   
         bkkadj = 0.0d0
         go to 220

      else if (bustyp .eq. 0) then

        write (errbuf(1),140) bus(nb),base(nb)    
  140   format(' Bus ',a8,f7.1,' has an illegal bus type, made type ', 

     1         'blank.')      
        errbuf(2) = ' '  
        call bcdbus(nb,xbuf)  
        write (errbuf(3),160) xbuf(1:80)
  160   format(13x,'(',a80,')') 
        call prterx ('W',3)     
        bustyp = BSTYP_B                
        kbsdta(1,nb) = BSTYP_B          

      endif

      pnetu(nb) = dble( (busdta(8,nb)-busdta(3,nb))/bmva )
      ploadu(nb) = dble ( busdta(3,nb)/bmva )
      qloadu(nb) = dble( busdta(4,nb)/bmva )

      qmax = busdta(9,nb)/bmva                      
      qmin = busdta(10,nb)/bmva                     
      if (typepv(bustyp) .eq. 0) then             
        if (qmin .lt. 0.0) then                   
          qnetu(nb) = -qloadu(nb)                   
        else                                      
          qnetu(nb) = dble(qmax)-qloadu(nb)               
        endif          
      else                            
        qnetu(nb) = -qloadu(nb)                     
      endif                                
      dgkk = dble(busdta(5,nb)/bmva)
      dbkk = dble(busdta(6,nb)/bmva)
      bkkadj = dbkk
      total_cap = amax1 (busdta(6,nb), 0.0)
      total_rek = amin1 (busdta(6,nb), 0.0)
      if (bustyp .eq. BSTYP_BX) then       
         bkkadj = 0.0d0
         dbkk = 0.0d0 
      end if               

  220 call vltlim (nb, vlimn(nb), vlimx(nb), vstart(nb))  
      if (fltstr .ne. 0) then     
         e(nb) = dble(vstart(nb))
         f(nb) = 0.0d0
      endif           
c                    
c     update slack bus arrays 
c          
      if (bustyp .eq. BSTYP_BS) then
         nbslck = nbslck+1     
         if (nbslck .gt. 10) then
            write (errbuf(1),540)   
  540       format(' More than 10 system slack buses')   

            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            nbslck = 1        
         else
            nslkxx(1,nbslck) = nb         
            nslkxx(2,nbslck) = 0          
            sang = busdta(12,nb)        
            slkxx(3,nbslck) = vstart(nb)
            slkxx(4,nbslck) = 0.0174532925*sang  
         endif
      endif
c            
c     process "+" bus data   
c          
      ncb = kbsdta(15,nb)
      if (ncb .gt. 0) then
         if (bustyp .eq. BSTYP_BD .or. bustyp .eq. BSTYP_BM) then
            write (errbuf(1),348) 
  348       format(' Illegal continuation bus on a ',
     &             '"BD" or "BM" d-c bus')
            errbuf(2) = ' '     
            call bcdbus(nb,xbuf)  
            write (errbuf(3),160) xbuf(1:80)    
            call bcdcbs(ncb,xbuf)   
            write (errbuf(4),160) xbuf(1:80)     
            call prterx ('W',4)    
         else
            do while (ncb .gt. 0) 
               call getchr(1,cbtype,kbctbl(8,ncb)) 
               call getchr(2,cbyear,kbctbl(9,ncb))      
               if ((cbyear .eq. '*I') .or.     
     &            (cbtype .eq.  'A' .and. cbyear .eq.  '01')) then  
                  inetr(nb) = inetr(nb) + dble( bctbl(2,ncb)/bmva )
                  ineti(nb) = ineti(nb) - dble( bctbl(3,ncb)/bmva )
                  pnetu(nb) = pnetu(nb) + dble( bctbl(6,ncb)/bmva )
                  qnetu(nb) = qnetu(nb) + dble( bctbl(11,ncb)/bmva)
               else         
                  ploadu(nb) = ploadu(nb)+dble(bctbl(2,ncb)/bmva)
                  qloadu(nb) = qloadu(nb)+dble( bctbl(3,ncb)/bmva )
                  pnetu(nb) = pnetu(nb) + dble( (bctbl(6,ncb) 
     &                        - bctbl(2,ncb)) /bmva )
                  qnetu(nb) = qnetu(nb) + dble( (bctbl(11,ncb) 
     &                        - bctbl(3,ncb)) / bmva )
               endif   
               dgkk = dgkk + dble(bctbl(4,ncb)/bmva)
               dbkk = dbkk + dble(bctbl(5,ncb)/bmva)
c
c              For the present, ignore cap/react on + bus records
c              when matching quantities with an associated X-data 
c              record.
c
c              total_cap = total_cap + amax1 (bctbl(5,ncb), 0.0)
c              total_rek = total_rek + amin1 (bctbl(5,ncb), 0.0)

               if (cbtype .ne. 'A' .and. cbyear .ne. '*I' .and. 
     &            cbyear .ne. '*P') then
                  bkkadj = bkkadj + bctbl(5,ncb)/bmva 
               endif
               ncb = bctbl_nxt(ncb)
            enddo
         endif
      endif
c        
c     process "x" data      
c        
      kxd = busxdtptr(nb)
      if (kxd .gt. 0) then   
         used = busdta(6,nb)    
         if (bustyp .eq. BSTYP_BX) then
c   
c           Check "discreteness" of initial bus shunt value USED.
c           Function XDSCRT computes new discrete value of USED.
c           
            disc = xdscrt (kxd, nb, used, b1, b2)     
            busdta(6,nb) = disc     
            if (abs(xdata(3,kxd)) .eq. 0.0 .and. 
     &          abs(xdata(4,kxd)) .eq. 0.0) then
              kbsdta(1,nb) = 7
              bustyp = kbsdta(1,nb)    
              ntypu(nb) = bustyp   
              call typno (type, bustyp)
              write (errbuf(1), 10450) bus(nb), base(nb)
10450          format (' Type BX bus ', a8, f7.1, 
     &           ' has zero X and is changed to type BQ')
              call prterx ('I',1) 
              busdta(11,nb) = 0.5 * (busdta(11,nb) + busdta(12,nb))
              busdta(12,nb) = 0.0
            endif
         else
            temp = xdscrt (kxd, nb, used, b1, b2)     
            disc = used
            if (abs (total_rek) .gt. 100.0 .or. total_cap .gt. 100) then
              tolerance = 1.0
            else
              tolerance = 0.1
            endif 
            if (abs (total_rek-xdata(3,kxd)) + 
     &          abs (total_cap-xdata(4,kxd)) .gt. tolerance) then
               call typno (type, bustyp)
               write (errbuf(1),450) type, bus(nb), base(nb), total_cap,
     &           total_rek, xdata(4,kxd), xdata(3,kxd)
  450          format (' Shunt on B', a, 1x, a8, f7.1, ' record (',
     &          f7.1, ',', f7.1, ') mismatches that on X-record (',
     &          f7.1, ',', f7.1, ')')
               call prterx ('I',1) 
            endif
         endif
         xdata(5,kxd) = dble(amin1 (disc, 0.0))
         xdata(6,kxd) = dble(amax1 (disc, 0.0))
         if (bustyp .eq. BSTYP_BX) then
            dbkk = dbkk + dble( disc / bmva )
            bkkadj = disc / bmva
         endif
      else
         if (bustyp .eq. BSTYP_BX) then
            write (errbuf(1),452) bus(nb),base(nb)  
  452       format (' Type "BX" bus ',a8,f7.1,' has no accompanying ',
     &              'X-record. Bus subtype changed to "G"') 
            call prterx ('W',1) 
            bustyp = BSTYP_BG     
            kbsdta(1,nb) = bustyp   
            dbkk = busdta(6,nb) / bmva
            bkkadj = dbkk / bmva
         endif
      endif
c     
c     Add TBX entities if applicable        
c            
      ltype = tbxtyp(bustyp)          
      jtbx = 0
      if (ltype .gt. 0) then
         call bldtbx (nb, jtbx, kxd, kerr)      
      endif
c                             
c     Build d-c arrays if applicable  
c            
      if (bustyp .eq. BSTYP_BD) then
         call dc2tbs(nb,kerr)       
      else if (bustyp .eq. BSTYP_BM) then
         call dcntbs(nb,kerr)         
      endif

c     Check for P/Q curve data for this bus....

      ieq = buspqptr(nb)
      if (ieq .gt. 0) then

         if ( pqactive(ieq) .and.
     &       (bustyp .eq. BSTYP_BE .or. bustyp .eq. BSTYP_BG .or.
     &        bustyp .eq. BSTYP_BQ .or. bustyp .eq. BSTYP_BX .or.
     &        bustyp .eq. BSTYP_BS) ) then
 
c           Check the P/Q curve to lookup Qmax, Qmin values for the 
c           original bus record.

            call pqcurv (ieq, jtbx, kerr)
            if ( kerr .eq. 1 ) then

c***           Curve limits exceeded - cannot use

               write (errbuf(1), 460) bus(nb), base(nb)
  460          format(' PGEN exceeds the P/Q curve limits at ',
     1                a8, f6.1)
               errbuf(2) = ' Curve deactivated - will use prior values.'
               call prterx( 'W',2 )
            endif
         else if ( pqactive(ieq) ) then
            write (errbuf(1), 470) bustyp, bus(nb), base(nb)
  470       format(' Wrong type ', a2, ' for bus ', a8, f6.1,
     1             ' P/Q curve deactivated!')
            errbuf(2) = ' P/Q data valid only for types ' //
     1                  'BE, BG, BQ, BX, & BS buses.'
            call prterx ('W',2)
            pqactive(ieq) = .false.
         endif
      endif
c                                 
c     initialize branch loop        
c     **********************
c     
      psect = 0    
      jsect = 0    
      ktybr = 1
      ltype = 0
c           
c     ktybr is branch status: 1 = new branch    
c                             2 = new parallel    
c                             3 = new section   
      pold = 0
      p = kbsdta(16,nb)
      q = 0
      k1 = nb
      oldsrt = 0
c
c     *********************************
c     begin branch loop               *
c                                     v
      do while (p .gt. 0)
c     
c        begin branch loop    
c    
         k2 = ky(p)
         if (k2 .eq. nb) then
            write (errbuf(1), 660) 
  660       format(' Branch has both terminals on same bus.')  
            errbuf(2) = ' '       
            call bcdbrn(p,xbuf)    
            write (errbuf(3), 160) xbuf(1:80)  
            if (is_batch .eq. 0) then
               call prterx ('E',3)
            else
               call prterx ('F',3)
            endif
         endif
         old_ltype = ltype
         ltype = brtype(p)
         id = brid(p)
         sect = brsect(p)

         pnxt = brnch_nxt(p)
         q = brnch_ptr(p)
         aq = iabs(q)
         if ( ltype.eq.BRTYP_LM .or. ltype.eq.BRTYP_R .or.
     &        ltype.eq.BRTYP_LD) sect = 0 
c      
c        check data validity        
c                   
         if (check(1) .and. q .gt. 0) call brchek(q)  
c         
c        check legitimacy of section    
c                       
         if (sect .eq. 0) then
            if (psect .gt. 0) then
               write (errbuf(1),670) 
  670          format(' Branch has missing section number.')  
               errbuf(2) = ' '       
               call bcdbrn(p,xbuf)    
               write (errbuf(3),160) xbuf(1:80)  
               call prterx ('W',3)    
            endif
         else
            if (psect .eq. 0) then
               write (errbuf(1),690)   
  690          format(' Branch with sections is not preceded ',
     &                '"with section equivalent record.')
               errbuf(2) = ' '   
               call bcdbrn(p,xbuf)         
               write (errbuf(3),160) xbuf(1:80) 
               call prterx ('W',3) 
            endif
         endif
c               
c        verify sort order 
c                     
         if (check(2)) then
            xsect = sect
            if (xsect .gt. 0 .and. q .lt. 0) then
               xsect = 10 - xsect
            endif
            newsrt = ipack_4 (inp2alf(k2), ichar(id), xsect, ltype)

            if (oldsrt .gt. newsrt) then
               write (errbuf(1),770)
  770          format(' Branches not in sort order.') 
               errbuf(2) = ' '   
               call bcdbrn(pold,xbuf)     
               write (errbuf(3),160) xbuf(1:80)
               call bcdbrn(p,xbuf)             
               write (errbuf(4),160) xbuf(1:80)

               if (is_batch .eq. 0) then
                  call prterx ('E',4)
               else
                  call prterx ('F',4)
               endif
            else if (oldsrt .eq. newsrt) then
               write (errbuf(1),790)           
  790          format(' Duplicate branch records.')
              errbuf(2) = ' '                      
               call bcdbrn(pold,xbuf)              
               write (errbuf(3),160) xbuf(1:80)    
               call bcdbrn(p,xbuf)                 
               write (errbuf(4),160) xbuf(1:80)    
               call prterx ('W',4)                 
            endif
            oldsrt = newsrt
         endif
c
c        initialize y-matrix entity if new branch
c
         if ( ktybr.eq.1 .and. ltype.ne.BRTYP_LM .and. 
     &        ltype.ne.BRTYP_LD ) then
            if (yptr .ge. MAXYE) then
                write (errbuf(1),802) MAXYE
  802           format(' More than ', i5, ' Y-matrix entities')

                if (is_batch .eq. 0) then
                   call prterx ('E',1)
                else
                   call prterx ('F',1)
                endif
                yptr = 1
                kmlen(nb) = 0
            endif
            yptr = yptr + 1
            ikmu(yptr) = k2
            gkmu(yptr) = 0.0d0     
            bkmu(yptr) = 0.0d0
            kmlen(nb) = kmlen(nb) + 1
         endif
c                       
c        process branch type   
c
         if ( ltype .eq. BRTYP_PEQ ) then
c                    Pie_EQuiv branch: set section switch    
            psect = p
            jsect = 0         
            do 860 i = 1,2             
               do 850 j = 1,2       
                  y(j,i) = dcmplx(0.0d0,0.0d0)
  850          continue
  860       continue

         else if ( ltype .eq. BRTYP_RZ ) then

c           Process "RZ" series var compensator records  

            if (inp2alf(k1) .lt. inp2alf(k2)) then 
               call bldycp (p,error)
            endif

         else if ( ltype .eq. BRTYP_LM ) then

c              Process "LM" n-terminal dc line

               call dcntln (p,kerr)                         

         else if ( ltype .eq. BRTYP_LD ) then

c              Process "LD" 2-terminal dc line 

               call dc2tln(p,kerr)  

         else if ( ltype .eq. BRTYP_R ) then

c           Process "R " ltc transformer always low to high direction

            jt = 0   
            if (inp2alf(k1) .lt. inp2alf(k2)) then 
               call bldltc (p,jt,kerr)  
c
c              Reset y-matrix for solitary R-records
c
               if ( ktybr .eq. 1 .and. kerr .eq. 2) then
                 yptr = yptr - 1
                 kmlen(nb) = kmlen(nb) - 1
                 ltype = old_ltype
               endif
            else
               if (pnxt .eq. 0 .or. ky(pnxt) .ne. k2) then
c
c                 Reset y-matrix for solitary R-records
c
                  if ( ktybr .eq. 1 .and. kerr .eq. 2) then
                    yptr = yptr - 1
                    kmlen(nb) = kmlen(nb) - 1
                  endif
               endif
            endif

         else               ! process AC branches
            if ( ltype .eq. BRTYP_TP ) then

c              Process "TP" phase shifter always low to high direction

               if (inp2alf(k1) .lt. inp2alf(k2)) then 
                  jphno = jphno+1 
                  if (jphno .gt. MAXPHS) then
                     write (errbuf(1),870) MAXPHS 
  870                format(' More than ',i3,' phase shifters')

                     if (is_batch .eq. 0) then
                        call prterx ('E',1)
                     else
                        call prterx ('F',1)
                     endif
                     jphno = 1             
                  endif
                  jphid(1,jphno) = k1      
                  jphid(2,jphno) = k2      
                  call putchr(1,id,jphid(3,jphno)) 
                  jphid(4,jphno) = sect              
                  if (q .gt.0) then
                     phid(5,jphno) = brnch(9,aq)*0.01745926535    
                  else
                     phid(5,jphno) = -brnch(9,aq)*0.01745926535    
                  endif
                  phid(6,jphno) = brnch(10,aq)/base(k2)  
                  if (check(3)) then
           
                     tpnxt = kbsdta(16,nb)
                     do while (tpnxt .gt. 0)
                        if (tpnxt .eq. p) then
c                          skip itself
                        else
                           k2next = ky(tpnxt)
                           idnext = brid(tpnxt)
                           sectnext = brsect(tpnxt)
                           if (k2next .eq. k2 .and. 
     1                         idnext .eq. id .and.
     2                         sectnext .eq. sect .and. 
     3                         brtype(tpnxt) .ne. BRTYP_R)     then
                              write (errbuf(1), 872) 
  872                         format (' Improper branch type in ',
     &                                'parallel with a phase shifter')
                              errbuf(2) = ' '    
                              call bcdbrn(p,xbuf)
                              write (errbuf(3),160) xbuf(1:80)  
                              call bcdbrn(tpnxt,xbuf)            
                              write (errbuf(4),160) xbuf(1:80)  
                              call prterx ('E',4)  
                           endif                   
                        endif                      
                        tpnxt = brnch_nxt(tpnxt)
                     enddo
                  endif
               endif 
            endif


            call pieqiv(p,y,kerr)   

c           determine status if sections present   

            if (psect .gt. 0) then
               if (jsect .eq. 0) then

c                 Process first section

                  call firsecd(y)    
               else

c                 Process subsequent sections

                  call nexsecd(y) 
               endif
               jsect = jsect+1        

c              check for last sections  

               flag = .true.
               if (pnxt .gt. 0) then
                  if (ky(pnxt) .eq. k2 .and. brid(pnxt) .eq. id) then
                     flag = .false.
                  endif
               endif
               if (flag) then

c                 last section   

                  call finsecd(y) 
                  if (brnch_ptr(psect) .gt. 0) then
                     ksect = brnch_ptr(psect)
                     do i = 1,2      
                        do j = 1,2 
                           k = 4*i + 2*j - 2
                           brnch(k,ksect) = sngl( dreal(y(i,j)) )
                           brnch(k+1,ksect) = sngl( dimag(y(i,j)) )
                        enddo
                     enddo
                  endif
                  psect = 0
                  jsect = 0
               endif
            endif
c                                 
c           if finished combining sections, add data to y-matrix

            if( psect.eq.0 .and. jsect.eq.0 ) then
               dgkk = dgkk + dreal(y(1,1))
               dbkk = dbkk + dimag(y(1,1))
               gkmu(yptr) = gkmu(yptr) + dreal(y(1,2))   
               bkmu(yptr) = bkmu(yptr) + dimag(y(1,2))
            endif
         endif
c                     
c        look ahead and get next branch
c                     
         pnxt = brnch_nxt(p)
         ktybr = 1
         if (pnxt .gt. 0) then
            k2next = ky(pnxt)
            idnext = brid(pnxt)
            typenext = brtype(pnxt)
            sectnext = brsect(pnxt)
            if (k2 .eq. k2next) then

c              parallel with previous branch

               if (ltype .eq. BRTYP_R) then
                  if (typenext .eq. BRTYP_T .or. 
     &                typenext .eq. BRTYP_TP) then
                  else
                     write (errbuf(1), 1051) 
 1051                format (' "R" record is not followed with a ',
     &                       '"T" or "TP" record.')
                     errbuf(2) = ' '                  
                     call bcdbrn(p,xbuf)              
                     write (errbuf(3),160) xbuf(1:80) 
                     call bcdbrn(pnxt,xbuf)           
                     write (errbuf(4),160) xbuf(1:80) 
                     call prterx ('E',4)
c
c                    delete branch entity                     
c
                     if (pold .eq. 0) then
                        kbsdta(16,k1) = pnxt
                     else
                        brnch_nxt(pold) = pnxt
                     endif
                     brnch_ptr(p) = 0
                     p = pold
                  endif
               else if ( ltype .eq. BRTYP_LM .or. 
     1                   ltype .eq. BRTYP_LD .or.
     2                   typenext .eq. BRTYP_LM .or. 
     3                   typenext .eq. BRTYP_LD ) then
                  write (errbuf(1), 1052) 
 1052             format(' "LD" or "LM" records cannot have parallels.')
                  errbuf(2) = ' '                   
                  call bcdbrn(p,xbuf)               
                  write (errbuf(3),160) xbuf(1:80)  
                  call bcdbrn(pnxt,xbuf)            
                  write (errbuf(4),160) xbuf(1:80)  
                  call prterx ('E',4)               
               endif
               ktybr = 2
               if (id .eq. brid(pnxt)) ktybr = 3
            else if ( ltype .eq. BRTYP_R ) then

c              not parallel with previous branch

               write (errbuf(1), 1051) 
               errbuf(2) = ' '                  
               call bcdbrn(p,xbuf)              
               write (errbuf(3),160) xbuf(1:80) 
               call bcdbrn(pnxt,xbuf)           
               write (errbuf(4),160) xbuf(1:80) 
               call prterx ('E',4)
c
c              delete branch entity                     
c
               if (pold .eq. 0) then
                  kbsdta(16,k1) = pnxt
               else
                  brnch_nxt(pold) = pnxt
               endif
               brnch_ptr(p) = 0
               p = pold
            endif
         endif

         if ((ktybr .eq. 1 .or. ktybr .eq. 2) .and. psect .gt. 0) then
            write (errbuf(1), 1053)
 1053       format(' Equivalent pi-equivalent section is not ',
     &             'accompanied with sections.')               
            errbuf(2) = ' '                         
            call bcdbrn(p,xbuf)                                        

            write (errbuf(3),160) xbuf(1:80)        
            call prterx ('I',3)                     
c                                      
c           missing section data - delete branch entity    
c                                         
            if (pold .eq. 0) then
               kbsdta(16,k1) = pnxt
            else
               brnch_nxt(pold) = pnxt
            endif
            brnch_ptr(p) = 0
            p = pold
            psect = 0
         endif
         pold = p
         p = pnxt
      enddo
c                       ^
c                       *
c     End of branch loop*
c     *******************
c
      gkku(nb) = dgkk
      bkku(nb) = dbkk
      ntypu(nb) = bustyp   
      nspar(nb) = 0
      if (kmlen(nb) .eq. 0 .and. bustyp .ne. BSTYP_BM) then 
         write(errbuf(1), 1062) bus(nb), base(nb)
 1062    format(' A-C bus ',a8, f7.1, ' has no a-c branches. Bus deleted
     &.')
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
      else
         bkktot = 0.0
         do i = km(nb), km(nb)+kmlen(nb)-1
           bkktot = bkktot + abs (bkmu(i))
         enddo
         if (bkktot .le. 1.0e-5 .and. bustyp .ne. BSTYP_BM) then
            write(errbuf(1), 1062) bus(nb),base(nb)
            if (is_batch .eq. 0) then
              call prterx ('E',1)
            else
              call prterx ('F',1)
            endif
         endif
      endif                                                           

 1070 continue

      do nbx = ntot_alf+1, ntot
         nb = alf2inp(nbx)
         if (bus(nb) .eq. srtlst) then
            km(nb) = 0
            kmlen(nb) = 0 
         endif
      enddo
c                                       *
c     end of bus loop                   *
c     ***********************************
c
c     complete processing-determine exit
c
      km(ntot+1) = yptr+1
c
c     Sort xdata, update tbx -> xdata pointer
c
      if (kxtot .gt. 0) call sort_xdta()

      do nb = 1, ntot
         busxdtptr(nb)  = 0
      enddo
      do i = 1, kxtot
         kxd = xdata(1,i)
         if (kxd .gt. 0) busxdtptr(kxd) = i
      enddo
      xdt_flag = .true.

      do jt = 1, ntotb
         if (tbx(1,jt) .eq. 5) then
            kt = tbx(2,jt)
            tbx(5,jt) = busxdtptr(kt)
         endif
      enddo

      do i = 1, ntot
         ptrtbx(i)  = 0
      enddo
      do jt = 1, ntotb
         j = tbx(2,jt)
         if (ordtbx .eq. 2) j = opt2inp(j)
         ptrtbx(j) = jt
      enddo
      tbx_loaded = 1
c
c     Save approximate counters

      kbsknt = ntot_alf
      kbrknt = yptr/2
      knew = 1          ! Flag denoting y-matrix data in alpha order
      ibuscb = 0        ! Reset temporary bus names
c
c     Initialize inp2opt() and opt2inp()
c
      do i = 1, ntot
         inp2opt(i) = i
         opt2inp(i) = i
      enddo

      if (reset_fatal_error) then
         do i = 1, 5
            errcnt(i) = 0
         enddo
      endif
      kerr = chkerr('F')         
      if (kerr .ne. 0) then
         write (errbuf(1),1130)      
 1130    format (' Program aborted by fatal errors')  
         call prterx ('F',1)   
         call erexit    
      endif

      return 
      end     
