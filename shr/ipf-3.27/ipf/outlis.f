C    @(#)outlis.f	20.18 8/19/99
      subroutine outlis
 
C     THIS ROUTINE PROVIDES A USER-ORIENTED LISTING
C     OF THE RESULTS OF ALL OUTAGES
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/apcom.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cont.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/outxrf.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/comm_mode.inc'
      include 'ipfinc/time1.inc'
      include 'ipfinc/brtype.inc'
 
      common /intown/ intown(MAXBUS)
      character intown*3

      common /o2i/ o2i(MAXBUS) ! optimal to original (non reduced) input
      integer o2i
 
      integer find_bus
      dimension pctime(10)  
      external kmpout, swpout, kmpovl, swpovl, kmpvlt, swpvlt,  
     &         kpxsrt, spxsrt   
      integer ptr, qptr, pbr 
      character id*1, jd*1, cbown*3, brown*3, tag*3, rattag*1, 
     &          rec_type(4)*1, chg_type(2)*1, datein*5
      character sol_prob_text*11, br_own*3
 
      logical finished
 
      data rec_type / 'B', '+', 'L', 'T' /
      data chg_type / 'D', 'M' /
C       
C     Initialize arrays. 
C       
      do i = 1, novl
         ibrol(i) = 0
      enddo
 
      do i = 1, nbus
         ibsol(i) = 0
         o2i(i) = find_bus( intbus(i), intbas(i) )
      enddo
C
C          PREPARE OUTAGE - OVERLOAD ANALYSIS
C
C          KASE1(3) - ANALYSIS LISTING SELECTION
C                0  = OUTAGE-OVERLOAD ONLY  
C                1  = OVERLOAD-OUTAGE ONLY  
C                2  = BOTH  
C       
C          KASE1(5) - ANALYSIS LISTING SORT ORDER   
C                0  = BUS   
C                1  = OWNERSHIP-BUS 
        
      if (kase1(3) .ne. 1) then 
         call forbtm
         write (outbuf,120) 
  120    format(30x,'Summary of bus and line problems for each outage') 
         call shdlod(1) 
         if (trending) then
            write (outbuf, 130) 
  130       format(5x,'* * O U T A G E * * >>> TRENDING >>>')   
         else
            write (outbuf, 132) 
  132       format(5x,'* * O U T A G E * *')   
         endif
         call shdlod(2) 
         call fortop
      endif 
C       
      iic = 0   
      iid = 0   
      iie = 0   
C       
C     Determine sort order   
C       
      do i = 1, nout+num_comm_mode
         isort(i) = i  
      enddo
        
      if (kase1(5) .eq. 1) then 
         call qiksrt (1, nout+num_comm_mode, kmpout, swpout) 
      endif 
        
      do 290 is = 1, nout+num_comm_mode
         icntg = isort(is)  
         if (ipbad(icntg) .eq. ipbad(icntg+1)) go to 290
         i1 = 0 
c
c        "i2" is pass switch: -1 -> process overloads
c                              0 -> process voltage problems
c
         i2 = -1
         i3 = 0     ! count of overloads for this outage
         i4 = 0
         do j = ipbad(icntg), ipbad(icntg+1) - 1
            if ( ibad(j) .ne. 0 ) i4 = i4 + 1
         enddo
         if (icntg .le. nout) then
            k1 = klnc(1,icntg) 
            k2 = klnc(2,icntg) 
            j = mod(klnc(4,icntg),1000)
            id = char(j)
            call getchr(3,brown,klnc(3,icntg)) 
            if ( i4 .eq. 0 .or. kase1(3) .eq. 1 ) goto 160
            call chkbtm(8) 
            write (outbuf,150) intbus(k1), intbas(k1), intbus(k2),
     &             intbas(k2),id, brown, zone(o2i(k1)), zone(o2i(k2))
  150       format('0 ',2(a8,f6.1,2x),a1,2x,a3,1x,2(1x,a2),3x,38(' -'))
            call prtout(1)  
         else
            if ( i4 .eq. 0 .or. kase1(3) .eq. 1 ) goto 160
            if (comm_status(icntg-nout) .eq. 0) then
              write (outbuf, 152) comm_mode(icntg-nout)(1:38)
  152         format('0 ', a, 13x, 24(' -'))   
            else
              write (outbuf, 154) comm_mode(icntg-nout)(1:38)
  154         format('0 ', a, 13x, 7(' -'),  
     &          ' * * WARNING - MISSING MODE ELEMENT * *')
            endif
            call prtout (1)
            call space(1)
            call lis_cmde( icntg-nout, 1 )
         endif
  160    do 280 j = ipbad(icntg), ipbad(icntg+1) - 1
            if (ibad(j)) 170, 250, 210  
C       
C           BRANCH OVERLOAD
C       
  170       if (i2 .ge. 0) go to 280
            k = -ibad(j)
            m1 = klno(1,k)  
            m2 = klno(2,k)  
            call getchr(1,jd,klno(5,k)) 
            call getchr(3,brown,klno(3,k))  
            if (clno(11,k) .gt. 0.0) then   
               tag = 'AMP'  
            else
               tag = 'MVA'  
            endif   
        
            rat = abs(clno(12,k))   
            bad(j) = sqrt (bad(j)) * abs(clno(11,k)/rat)
            ratact = bad(j) * rat   
            baslod = clnobase(k) * rat        
            ptr = outxrf(k) 
            call getrat (ptr, rating, rattag, ratnom, ratthr,   
     &                   ratllf, ratbtl)
            nbr = iabs(brnch_ptr(ptr))        
            kdin = kbrnch(11,nbr)
            if (brtype(ptr) .eq. BRTYP_PEQ) kdin=0
            kyr = kdin/100
            mon = mod(kdin,100) 
            if (kyr .eq. 0) then
               datein = ' '
            else
               write (datein, 172) mon, kyr
  172          format (i2.2, '/', i2.2)

            endif
        
            if (kase1(3) .ne. 1) then   
               if (i1 .eq. 0) then  
                  call chkbtm(5)
                  write (outbuf,180)
  180             format('0',t26,'Overloads',t76,'Rating',t86,'Type',   
     &               t94,'per unit / pst-cont  pre-cont Date in')   
                  call prtout(1)
               endif
               write (outbuf,190) intbus(m1), intbas(m1), intbus(m2),   
     &            intbas(m2), jd, brsect(ptr), brown,
     &            zone(o2i(m1)), zone(o2i(m2)), rat, tag,  
     &            rattag, bad(j), ratact, baslod, datein
  190          format(t28,a8,f6.1,2x,a8,f6.1,1x,a1,1x,i1,2x,a3,
     &            1x, 2(1x,a2),f6.0, 
     &            1x,a3,t88,a1,t96,f6.3,' /',f7.1, f10.1, 3x, a)  
               call prtout(1)   
               i3 = i3 + 1
            endif   
            i1 = +1 
C       
C           LINK BRANCH OVERLOAD TO OUTAGE 
C       
            if (iic .lt. MXIBAD) then 
               iic = iic + 1
            else if (iic .eq. MXIBAD) then
               iic = iic + 1
               if (icntg .le. nout) then
                  write (errbuf(1), 192)  intbus(k1), intbas(k1),  
     &               intbus(k2), intbas(k2), id, intbus(m1), 
     &               intbas(m1), intbus(m2), intbas(m2), jd, iic   
  192                format (' Overflow by contingency ', a8, f6.1, 1x,
     &                  a8, f6.1, 1x, a1, ' Overload ', a8, f6.1, 1x,
     &                  a8, f6.1, 1x, a1, ' IIC ', i6)  
                  call prterx ('E',1)  
               else
                  write (errbuf(1), 193)  comm_mode(icntg-nout)(1:31), 
     &               intbus(m1), intbas(m1), intbus(m2), intbas(m2), 
     &               jd, iic   
  193             format (' Overflow by contingency ', a, 
     &                  ' Overload ', a8, f6.1, 1x, a8, f6.1, 1x, a1, 
     &                  ' IIC ', i6)  
                  call prterx ('E',1)  
               endif
            else
               iic = iic + 1
               go to 280
            endif   
            if (ibrol(k) .eq. 0) then   
               ibrol(k) = iic   
               else 
               jjc = ibrol(k)   
               do while (ibrolp(2,jjc) .ne. 0) 
                  jjcx = ibrolp(2,jjc) 
                  jjc = jjcx
               enddo
               ibrolp(2,jjc) = iic
            endif   
            ibrolp(1,iic) = icntg
            ibrolp(2,iic) = 0
            brolp(iic) = bad(j) 
            go to 280   
C       
C           VOLTAGE PROBLEMS   
C       
  210       if (i2 .eq. -1) go to 280   
            k = ibad(j) 
            vactual = bad(j) * intbas(k)
            vchange = bad(j) - vmag(k)        
            if (kase1(3) .ne. 1) then   
               if (i2 .eq. 0) then  
                  call chkbtm(5)
                  write (outbuf,220)
  220             format('0',t26,'Bus voltage violation ',t76,'Actual', 
     &               t86,'Per unit  Actual / change /(    limits   )') 
                  call prtout(1)
               endif
               write (outbuf,230) intbus(k), intbas(k), owner(o2i(k)), 
     &            zone(o2i(k)), vactual, bad(j), vchange, vlow(k), 
     &            vhi(k)
  230          format(t28,a8,f6.1,2x,a3,2x,a2,
     &            t74,f6.0,' KV',t96,f6.3,' /', f7.3,' /(',f6.3,',',
     &            f6.3,')' ) 
               call prtout(1)   
            endif   
            i2 = i2 + 1 
C       
C           Link bus over/undervoltage 
C       
            if (iid .lt. MXIBAD) then 
               iid = iid + 1
            else if (iid .eq. MXIBAD) then
               iid = iid + 1
               if (icntg .le. nout) then
                  write (errbuf(1), 232)  intbus(k1), intbas(k1),  
     &               intbus(k2), intbas(k2), id, intbus(m1), 
     &               intbas(m1), iid   
  232             format (' Overflow by contingency ', a8, f6.1, 1x, a8,
     &               f6.1, 1x, a1, ' Under/overvoltage ', a8, f6.1, 
     &               ' IID ', i6)  
                  call prterx ('E',1)  
               else
                  write (errbuf(1), 233)  comm_mode(icntg-nout)(1:31),
     &               intbus(m1), intbas(m1), iid   
  233             format (' Overflow by contingency ', a, 
     &               ' Under/overvoltage ', a8, f6.1, ' IID ', i6)  
                  call prterx ('E',1)  
               endif
            else
               iid = iid + 1
               go to 280
            endif   
            if (ibsol(k) .eq. 0) then   
               ibsol(k) = iid   
            else
               jjc = ibsol(k)   
               do while (ibsolp(2,jjc) .ne. 0) 
                  jjcx = ibsolp(2,jjc)
                  jjc = jjcx
               enddo
               ibsolp(2,jjc) = iid
            endif   
            ibsolp(1,iid) = icntg
            ibsolp(2,iid) = 0
            bsolp(iid) = bad(j) 
            go to 280   
c
c           System problems
c
  250       if (i2 .eq. -1) go to 280
            if (bad(j) .gt. 0.0) go to 270  
C       
C           Examine system separation for isolated generator  
C       
            if (icntg .le. nout) then
               if (kmlen(k1) .eq. 1) then                       
                  k = k1   
               else if (kmlen(k2) .eq. 1) then                  
                  k = k2   
               else
                  go to 270
               endif   
               if (iid .lt. MXIBAD) then 
                  iid = iid + 1
               else if (iid .eq. MXIBAD) then
                  iid = iid + 1
                  write (errbuf(1), 262)  intbus(k1), intbas(k1),  
     &               intbus(k2), intbas(k2), id, intbus(k), intbas(k), 
     &               iid 
  262             format (' Overflow by contingency ', a8, f6.1, 1x, a8,
     &               f6.1, 1x, a1, ' Isolated generator ', a8, f6.1,
     &               ' IID ', i6) 
                  call prterx ('E',1)  
               else
                  iid = iid + 1
                  go to 280
               endif   
            else if (bad(j) .lt. 0) then
               k = -bad(j)
               if (iid .lt. MXIBAD) then 
                  iid = iid + 1
               else if (iid .eq. MXIBAD) then
                  iid = iid + 1
                  write (errbuf(1), 264)  comm_mode(icntg-nout)(1:31), 
     &               intbus(k), intbas(k), iid 
  264             format (' Overflow by contingency ', a, 
     &               ' Isolated bus ', a8, f6.1, ' IID ', i6) 
                  call prterx ('E',1)  
               else
                  iid = iid + 1
                  go to 280
               endif   
            else
               go to 270
            endif
            if (ibsol(k) .eq. 0) then   
               ibsol(k) = iid   
            else
               jjc = ibsol(k)   
               do while (ibsolp(2,jjc) .ne. 0) 
                  jjcx = ibsolp(2,jjc)
                  jjc = jjcx
               enddo
               ibsolp(2,jjc) = iid
            endif   
            ibsolp(1,iid) = icntg
            ibsolp(2,iid) = 0
            bsolp(iid) = 0.0
            goto 280

  270       if (iie .lt. MXCNBR/2) then 
               iie = iie + 1
            else if (iie .eq. MXCNBR/2) then
               iie = iie + 1
               if (j .le. nout) then
                  write (errbuf(1), 272)  intbus(k1), intbas(k1),  
     &               intbus(k2), intbas(k2), id, iie   
  272             format (' Overflow by contingency ', a8, f6.1, 1x, a8,
     &               f6.1, 1x, a1, ' Separation IIE ', i6) 
                  call prterx ('E',1)  
               else
                  write (errbuf(1), 274)  comm_mode(icntg-nout)(1:31), 
     &               iie   
  274             format (' Overflow by contingency ', a, 
     &               ' Separation IIE ', i6) 
                  call prterx ('E',1)  
               endif
            else
               iie = iie + 1
               go to 280
            endif   
            brcon(iie) = bad(j) 
            ibrcon(iie) = icntg
            if ( ibad_rs(icntg) .eq. 1 .and. i3 .ne. 0 ) then
               write (outbuf,278)
  278          format('0',t26,'Failed reactive solution')
               call prtout(1)
            endif
  280    continue   
         if (i2 .eq. -1) then   
            i2 = 0  
            go to 160   
         endif  
  290 continue  
C       
C     List non-convergence and system separation 
C       
      if (iie .eq. 0) goto 380  
      write (outbuf,300)
  300 format(20x,'Summary of System Separations and Divergences')   
      call shdlod(1)
      call forbtm   
      write (outbuf,310)
  310 format('0--------   Outage  ',13('-'),
     &       ' ID  Owner  Zones  -------  Problem')
      call shdlod(2)
      outbuf = ' '  
      call shdlod(3)
      call fortop   
        
      lim = min0 (iie,MXCNBR/2) 
      do i = 1, lim 
         j = ibrcon(i)  
         l = brcon(i)   
         if (j .le. nout) then
            k1 = klnc(1,j) 
            k2 = klnc(2,j) 
            n = mod(klnc(4,j),1000)
            id = char(n)
            call getchr(3,cbown,klnc(3,j)) 
         endif
         if ( ( j .eq. ibrcon(i+1) .and. ibad_rs(j) .eq. 2 ) .or.
     &        ibad_rs(j) .eq. 1 ) then
            sol_prob_text = ' (Reactive)'
         else
            sol_prob_text = ' '
         endif
C       
C           Test for system separation 
C       
         if (mod(l,10) .eq. 0) then 
            if (j .le. nout) then
               write (outbuf,320) intbus(k1), intbas(k1), intbus(k2),  
     &            intbas(k2),id, cbown, zone(o2i(k1)), zone(o2i(k2)),  
     &            sol_prob_text 
  320          format(2x, 2(a8,f6.1,2x), a1, 4x, a3, 1x, 2(1x,a2),
     &            10x, 'Causes system separation', a )  
               call prtout(1)  
            else
               write (outbuf,322) comm_mode(j-nout)(1:40),
     &            sol_prob_text 
  322          format(2x, a, 17x, 'Causes system separation', a )  
               call prtout(1)  
               call lis_cmde( j-nout, 2 )
            endif
C       
C           Test for iteration limit   
C       
         else if(mod(l,10) .eq. 1) then 
            l10 = l / 10
            if (j .le. nout) then
               write (outbuf,330) intbus(k1), intbas(k1), intbus(k2),
     &            intbas(k2),id,cbown,zone(o2i(k1)),zone(o2i(k2)),l10,
     &            sol_prob_text 
  330          format(2x,2(a8,f6.1,2x),a1,4x,a3,1x,2(1x,a2),
     &            10x,'No solution  - (',i3,' iterations)', a )
               call prtout(1)  
            else
               write (outbuf,332) comm_mode(j-nout)(1:40), l10,
     &            sol_prob_text 
  332          format(2x, a, 17x, 
     &            'No solution  - (',i3,' iterations)', a )
               call prtout(1)  
               call lis_cmde( j-nout, 2 )
            endif
C       
C           Test for maximum angle excursion   
C       
         else if(mod(l,10) .eq. 2) then 
            ix = l / 10 
            x = 0.001 * ix  
            if (j .le. nout) then
               write (outbuf,340) intbus(k1),intbas(k1),intbus(k2),
     &            intbas(k2),id,cbown,zone(o2i(k1)),zone(o2i(k2)),x,
     &            sol_prob_text 
  340          format(2x,2(a8,f6.1,2x),a1,4x,a3, 1x, 2(1x,a2),
     &            10x,'No solution - insufficient capacity',
     &            ' (max angle ', e10.3, ')', a )   
               call prtout(1)  
            else
               write (outbuf,342) comm_mode(j-nout)(1:40), x,
     &            sol_prob_text 
  342          format(2x, a, 17x, 
     &           'No solution - insufficient capacity (max angle ',
     &            e10.3,')', a )   
               call prtout(1)  
               call lis_cmde( j-nout, 2 )
            endif
        
         else if (mod(l,10) .eq. 3) then
            x = l / 10  
            x = 0.001 * x   
            if (j .le. nout) then
               write (outbuf,350) intbus(k1),intbas(k1),intbus(k2),  
     &            intbas(k2),id,cbown,zone(o2i(k1)),zone(o2i(k2)),x,
     &            sol_prob_text 
  350          format(2x,2(a8,f6.1,2x),a1,4x,a3, 1x, 2(1x,a2),
     &            10x,'No solution - insufficient reactive (min volt', 
     &            e10.3,')', a ) 
               call prtout(1)  
            else
               write (outbuf,352) comm_mode(j-nout)(1:40), x,
     &            sol_prob_text 
  352          format(2x, a, 17x, 
     &            'No solution - insufficient reactive (min volt', 
     &            e10.3,')', a ) 
               call prtout(1)  
               call lis_cmde( j-nout, 2 )
            endif
C       
C           Test for arithmetic trap, floating overflow
C       
         else   
            if (j .le. nout) then
               write (outbuf,360) intbus(k1), intbas(k1), intbus(k2),
     &            intbas(k2),id, cbown, zone(o2i(k1)), zone(o2i(k2)),
     &            sol_prob_text 
  360          format(2x,2(a8,f6.1,2x),a1,4x,a3, 1x, 2(1x,a2),
     &            10x, 'Causes arithmetic trap, floating overflow', a ) 
               call prtout(1)  
            else
               write (outbuf,362) comm_mode(j-nout)(1:40),
     &            sol_prob_text 
  362          format(2x, a, 17x, 
     &            'Causes arithmetic trap, floating overflow', a ) 
               call prtout(1)  
               call lis_cmde( j-nout, 2 )
            endif
         endif
        
      enddo
  380 continue  
C       
C     List overloads and their problems  
C       
      if (iic .eq. 0 .or. kase1(3) .eq. 0) go to 470
      call forbtm   
      write (outbuf,390)
  390 format(40x,'Summary of overloaded lines caused by outages')   
      call shdlod(1)
      if (trending) then
        write (outbuf, 400) 
  400   format('0',6x,' * O V E R L O A D  * >>> TRENDING >>>')   
      else
        write (outbuf, 402) 
  402   format('0',6x,' * O V E R L O A D  *')
      endif
      call shdlod(2)
      outbuf = ' '  
      call shdlod(3)
      call shdlod(4)
      call shdlod(5)
      call fortop   
C       
C     Determine sort order   
C       
      do 410 i = 1, novl
  410 isort(i) = i  
        
      if (kase1(5) .eq. 1) then 
         call qiksrt (1,novl,kmpovl,swpovl) 
      endif 
        
      do 460 is = 1, novl   
         i = isort(is)  
         if (ibrol(i) .eq. 0) then  
            if (abs(clno(11,i)) / (tpc * abs(clno(12,i))) .le. 1.001)   
     &         goto 460 
         endif  
        
         k1 = klno(1,i) 
         k2 = klno(2,i) 
         call getchr(1,id,klno(5,i))
         if (clno(11,i) .gt. 0.0) then  
            tag = 'AMP' 
         else   
            tag = 'MVA' 
         endif  
         call chkbtm(7) 
         ptr = outxrf(i)
         nbr = iabs (brnch_ptr(ptr))
         call getrat (ptr, rat, rattag, ratnom, ratthr, ratllf, 
     &                ratbtl)   
         irat = ratnom + 0.5
         kdin = kbrnch(11,nbr)
         if (brtype(ptr) .eq. BRTYP_PEQ) kdin=0
         kyr = kdin/100
         mon = mod(kdin,100) 
         if (kyr .eq. 0) then
            datein = ' '
         else
            write (datein, 172) mon, kyr
         endif
         if ((rateln(1,nbr)+rateln(2,nbr)+rateln(3,nbr)) .eq. 0) then   
            write (outbuf,420) intbus(k1), intbas(k1), intbus(k2),  
     &         intbas(k2), id, brsect(ptr), klno(3,i),
     &         zone(o2i(k1)),zone(o2i(k2)), irat, tag, datein
  420       format('0',2(a8,f6.1,2x),a1,2x,i1,2x,a3,1x,2(1x,a2), 
     &         '  Overload % of ratings (Nominal:', i6, 1x, a3, ',', 
     &         t118, 'IN ', a)
            call prtout(1)  
            if (clno(11,i) .gt. 0.0) then   
               write (outbuf,424) 0, tag, 0, tag   
               call prtout(1)   
            else
               write (outbuf,428) 0, tag, 0, tag, 0, tag  
               call prtout(1)   
            endif   
         else   
            irate1 = rateln(1,nbr) + 0.5
            irate2 = rateln(2,nbr) + 0.5
            irate3 = rateln(3,nbr) + 0.5
            if (clno(11,i) .gt. 0.0) then   
               write (outbuf,422) intbus(k1), intbas(k1), intbus(k2),
     &            intbas(k2), id, brsect(ptr), klno(3,i),
     &            zone(o2i(k1)), zone(o2i(k2)), irat, tag, datein
  422          format('0',2(a8,f6.1,2x),a1,2x,i1,2x,a3,1x,2(1x,a2),
     &            '  Overload % of ratings (Nominal:',i6,1x, a3, ',',
     &            t118, 'IN ', a)
               call prtout(1)   
               write (outbuf,424) irate1, tag, irate2, tag   
  424          format(t35, 'Thermal:',i6, 1x, a3, ', Bottleneck:', i6,
     &            1x, a3, 1x, ') from outages:')
               call prtout(1)   
            else
               write (outbuf,426) intbus(k1), intbas(k1), intbus(k2),   
     &            intbas(k2), id, brsect(ptr), klno(3,i),
     &            zone(o2i(k1)), zone(o2i(k2)), irat, tag, datein
  426          format('0',2(a8,f6.1,2x),a1,2x,i1,2x,a3,1x,2(1x,a2),
     &            '  Overload % of ratings (Nominal:',i6,1x,a3, ',',
     &            t118, 'IN ', a)
               call prtout(1)   
               write (outbuf,428) irate1, tag, irate3, tag, irate2, 
     &            tag  
  428          format(t35, 'Thermal:',i6,1x,a3,', Bottleneck:',i6,1x,a3,
     &            ',  Loss of Life:',i6,1x,a3,1x,') from outages:')  
               call prtout(1)   
            endif   
         endif  

         ixovl = 0  
         j = ibrol(i)   
         do while (j .gt. 0) 
            ixovl = ixovl + 1   
            xsort(ixovl) = j
            jx = ibrolp(2,j)
            j = jx
         enddo

         if ((clnobase(i) / tpc .gt. 1.001) .or. (ixovl .gt. 0)) then 
            pcrat = clnobase(i) * 100.0
            ovrld = rat * pcrat / 100.0 
            write (outbuf,430) pcrat, ovrld, rattag   
  430       format(t49, f7.1, t61, '(', f8.1, ' )', t74, a1, t83,   
     &         '** Basecase **') 
            call prtout(1)  
         endif

         if (ixovl .gt. 0) then 
            call qiksrt (1, ixovl, kpxsrt, spxsrt)  
            do js = 1, ixovl
               j = xsort(js)   
               k = ibrolp(1,j)
               if (k .le. nout) then
                  l1 = klnc(1,k)  
                  l2 = klnc(2,k)  
                  pcrat = brolp(j) * 100.0 
                  ovrld = rat * pcrat / 100. 
                  l = mod(klnc(4,k),1000) 
                  id = char(l)
                  call getchr(3,brown,klnc(3,k))  
                  write (outbuf,450) pcrat, ovrld, rattag, 
     &               intbus(l1), intbas(l1), intbus(l2), intbas(l2), 
     &               id, brown, zone(o2i(l1)), zone(o2i(l2))
  450             format(t49, f7.1, t61, '(', f8.1, ' )', t74, a1, t83,
     &               2(a8,f6.1,2x), a1, 2x, a3, 1x, 2(1x,a2) )
                  call prtout(1)  
               else
                  pcrat = brolp(j) * 100.0 
                  ovrld = rat * pcrat / 100. 
                  write (outbuf,452) pcrat, ovrld, rattag, 
     &               comm_mode(k-nout)(1:31)
  452             format(t49, f7.1, t61, '(', f8.1, ' )', t74, a1, t83, 
     &               a)
                  call prtout(1)  
               endif
            enddo
         endif  
  460 continue  
  470 continue  
C       
C     List under/overvoltages and bus isolations by outage   
C       
      if (iid .eq. 0 .or. kase1(3) .eq. 0) go to 580
      call forbtm   
      write (outbuf,480)
  480 format(30x,'Summary of Bus Over/Under Voltages and Bus Separations
     1')
      call shdlod(1)
      write (outbuf,490)
  490 format(t1,'      Bus       Owner       Problem',
     &      t46,'Caused by outage of:', 
     &      t102,'MAG/PER UNIT') 
      call shdlod(2)
      write (outbuf,500)
  500 format('      ---       -----       -------', 10x,38('-'),18x,
     &  '--------------')   
      call shdlod(3)
      call fortop   
        
      ixovl = 0 
      do 510 k = 1, nbus
         i = opt2inp(k)   
         if (ibsol(i) .eq. 0) go to 510 
         ixovl = ixovl + 1  
         isort(ixovl) = i   
  510 continue  
        
      if (kase1(5) .eq. 1) then 
         call qiksrt (1,ixovl,kmpvlt,swpvlt)
      endif 
        
      do 570 is = 1, ixovl  
         i = isort(is)  
         if (ibsol(i) .eq. 0) go to 570 
         l = ibsol(i)   
         iprt = 0   
         finished = .false.
         do while (.not. finished)
            j = ibsolp(1,l)
            if (bsolp(l) .eq. 0.0) then
               if (j .le. nout) then
                  k1 = klnc(1,j) 
                  k2 = klnc(2,j) 
                  n = mod(klnc(4,j),1000)
                  id = char(n)
                  call getchr(3,cbown,klnc(3,j)) 
                  if (iprt .eq. 0) then   
                     write (outbuf,530) intbus(i), intbas(i), 
     &                  intown(i), zone(o2i(i)),
     &                  intbus(k1), intbas(k1), intbus(k2),
     &                  intbas(k2), id, cbown,
     &                  zone(o2i(k1)), zone(o2i(k2))
  530                format('0 ', a8, f6.1, 2x, a3, 2x, a2,
     &                  3x, 'Separates', 8x, 2(a8,f6.1,2x), a1,
     &                  2x, a3, 1x, 2(1x,a2) )   
                  else
                     write (outbuf,540) intbus(k1), intbas(k1), 
     &                  intbus(k2), intbas(k2),id, cbown,
     &                  zone(o2i(k1)), zone(o2i(k2))
  540                format(28x, 'Separates', 8x, 2(a8,f6.1,2x),
     &                  a1, 2x, a3, 1x,2(1x,a2) )   
                  endif   
                  call prtout(1)  
               else
                  if (iprt .eq. 0) then   
                     write (outbuf,542) intbus(i), intbas(i),
     &                  intown(i), zone(o2i(i)),
     &                  comm_mode(j-nout)(1:40)
  542                format('0 ', a8, f6.1, 2x, a3, 2x, a2, 3x,
     &                  'Separates', 8x, a)
                  else
                     write (outbuf,544) comm_mode(j-nout)(1:40)
  544                format(28x, 'Separates', 8x, a)
                  endif   
                  call prtout(1)  
               endif
            else   
               rat = bsolp(l) * intbas(i)  
               if (j .le. nout) then
                  k1 = klnc(1,j) 
                  k2 = klnc(2,j) 
                  n = mod(klnc(4,j),1000)
                  id = char(n)
                  call getchr(3,cbown,klnc(3,j)) 
                  if (iprt .eq. 0) then   
                     write (outbuf,550) intbus(i), intbas(i), intown(i),
     &                  zone(o2i(i)),
     &                  intbus(k1), intbas(k1), intbus(k2), intbas(k2),
     &                  id, cbown, zone(o2i(k1)), zone(o2i(k1)),
     &                  rat, bsolp(l)
  550                format('0 ', a8, f6.1, 2x, a3, 2x, a2, 3x,
     &                  'Voltage', 10x, 2(a8,f6.1,2x), a1, 2x, a3,
     &                  1x, 2(1x,a2), 10x, f6.1, ' / ', f6.3)   
                  else
                     write (outbuf,560) intbus(k1), intbas(k1), 
     &                  intbus(k2), intbas(k2),id, cbown,
     &                  zone(o2i(k1)), zone(o2i(k1)),
     &                  rat, bsolp(l)   
  560                format(28x, 'Voltage', 10x, 2(a8,f6.1,2x),
     &                  a1, 2x, a3,1x, 2(1x,a2),
     &                  10x, f6.1, ' / ', f6.3)  
                  endif   
                  call prtout(1)  
               else
                  if (iprt .eq. 0) then   
                     write (outbuf, 562) intbus(i), intbas(i), 
     &                  intown(i), zone(o2i(i)),
     &                  comm_mode(j-nout)(1:40), rat, bsolp(l)  
  562                format('0 ', a8, f6.1, 2x, a3, 2x, a2, 3x,
     &                  'Voltage', 10x, a, 15x, f6.1, ' / ', f6.3)   
                  else
                     write (outbuf, 564) comm_mode(j-nout)(1:40), rat, 
     &                  bsolp(l)   
  564                format(28x, 'Voltage', 10x, a, 15x, f6.1, ' / ', 
     &                  f6.3)  
                  endif   
                  call prtout(1)  
               endif
            endif  
            iprt = 1   
            if (ibsolp(2,l) .gt. 0) then   
               l = ibsolp(2,l)
            else
               finished = .true.
            endif
         enddo
  570 continue  
  580 continue  
      call forbtm   
      outbuf = ' '  
      call shdlod(1)
      call shdlod(2)
      call shdlod(3)
        
C     PROGRAM STATISTICS REPORT   

      call fortop   
        
      ttot = time(1)
        
      do 590 i = 1, 10  
  590 pctime(i) = time(i) * 100. / ttot 
        
      call space(2) 
      write (outbuf,600)
  600 format(' TIMING STATISTICS')  
      call prtout(1)
      call space (2)
      write (outbuf,610)
  610 format(7x,'TIME (MSEC)       PERCENT')
      call prtout(1)
      write (outbuf,620) time(1), pctime(1) 
  620 format(4x,2f13.2,5x,'TIME IN OUTAGE LOOP  (OUTAGE)')  
      call prtout(1)
      write (outbuf,630) time(2), pctime(2) 
  630 format(4x,2f13.2,5x,'TIME FOR POWER-FLOW SOLUTION  (DLFLOW)') 
      call prtout(1)
      write (outbuf,640) time(3), pctime(3) 
  640 format(4x,2f13.2,5x,'TIME FOR CONVERTING VOLTAGES TO '    ,   
     1'RECTANGULAR FORM')   
      call prtout(1)
      write (outbuf,650) time(4), pctime(4) 
  650 format(4x,2f13.2,5x,'TIME TO COMPUTE OFF-DIAG TERMS FOR P AND Q') 
      call prtout(1)
      write (outbuf,660) time(5), pctime(5) 
  660 format(4x,2f13.2,5x,'TIME FOR RESIDUAL COMPUTATION  (GETPQ)') 
      call prtout(1)
      write (outbuf,670) time(6), pctime(6) 
  670 format(4x,2f13.2,5x,'TIME FOR DOWN AND BACK  (SRSFB)')
      call prtout(1)
      write (outbuf,680) time(7), pctime(7) 
  680 format(4x,2f13.2,5x,'TIME FOR CHECKING OVERLOADS  (CHECK)')   
      call prtout(1)
      write (outbuf,690) time(8), pctime(8) 
  690 format(4x,2f13.2,5x,'TIME TO COMPUTE DIAG TERMS IN P AND Q')  
      call prtout(1)
      write (outbuf,700) time(9), pctime(9) 
  700 format(4x,2f13.2,5x,'TIME TO FORM AND FACTOR B AND B"')   
      call prtout(1)
      write (outbuf,710) time(10), pctime(10)   
  710 format(4x,2f13.2,5x,'TIME FOR CHECKING Q LIMITS (TYPESW)')
      call prtout(1)
      return

      end   
