C    @(#)presln.f	20.9 8/20/98
      subroutine presln
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/com008.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/komps.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/pctvr2.inc'
      include 'ipfinc/phase.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/snput.inc'
      include 'ipfinc/strce.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/xdata.inc'
C
      common /is_batch / is_batch

      external kmpqpn, swpqpn, kmpecn, swpecn, 
     &         komprs, swaps,  kmpbt,  swpbt,  kmplt,  swplt

      integer find_bus, find_zon, error

      character jtyp*1, cbtype*1, type*8
C
      lprtsw = 1
      fichsw = 0
      if (kspare(16).ge.0) fichsw = 1   
      nbttot = 0
      ntttot = 0
      islnsw=0  
      ntopt=0   
      ntot11=0  
      nsen=0
      ngen=0
      ncvrt=0   
      nqpen=0   
      nvpen=0   
      qpen=0
      nntyp9=0  
      nltc=0
      karslk = 0
      npctvr = 0
      nldc = 0
      idckt = 0
      inptls=1
C
      do 160 i=1,ntot
         buspen(i)=0.0
  160 continue
 
  170 if (buf(1:1) .eq. '(') then
        islnsw = 0
        goto 1330
 
      else
        call sinput
      endif
 
      if (islnsw.eq.0) go to 1330
 
C     ISLNSW HAS THE FOLLOWING ATTRIBUTES
 
C       0 -- NORMAL
C       1 -- OPTIMIZE ZONES
C       2 -- OPTIMIZE REACTIVE
C       3 -- OPTIMIZE DISPATCH
C       4 -- INCLUDE BUSES
C       5 -- EXCLUDE BUSES
C       6 -- /SENSITIVITIES
C       7 -- /TRACE BUSES
C       8 -- /TRACE LTC'S
C       9 -- /GEN_DPOP
C
C              1   2    3   4   5    6     7     8     9
      go to (180,700,1050,310,310,1200,10300,10500,10700) islnsw
        
C     OPTIMAL CHANGES BY ZONES  
        
  180 if (ncvrt.ne.0) go to 1900
      iopton(20)=1
      call qiksrt (1,idat,komprs,swaps)
      do 200 i=1,ntot
      j = find_zon(zone(i))
      if (j.le.0) go to 190
      j=1
      go to 200

  190 j=0
  200 nbsort(i)=j
      write (outbuf,210)
  210 format ('0 OPTIMAL CHANGES PERFORMED TEMPORARILY FOR BUSSES ',
     1        'WITHIN THE FOLLOWING ZONES' )   
      call prtout(1)
      call space (1)
      iend = 0  
  230 continue  
      ist = iend + 1
      iend = ist + 9
      if( iend .gt. idat ) iend = idat  
      write (outbuf,240) (zondat(i),i=ist,iend) 
  240 format( 10(4x,'(',a2,')  ' )) 
      call prtout(1)
      if( iend.lt.idat ) go to 230  
C       
C     CHECK FOR INCLUDED OR EXCLUDED BUSES  
C       
      call sinput   
      if (islnsw.eq.5) go to 250
      if (islnsw.ne.4) go to 300
      isw=1
      go to 260

  250 isw=0
  260 do 290 i=1,idat
         kbus = find_bus(namdat(i),bsedat(i))
         if (kbus .gt. 0) go to 280
         type = 'INCLUDED'
         if(islnsw.ne.4) type = 'EXCLUDED'
         write (errbuf(1),270) type,namdat(i),bsedat(i)
  270    format ('0 ',a8,' BUS ',a8,f7.1,
     1           ' IN OPTIMAL CHANGE SET IS NOT IN THE SYSTEM.')
      call prterx ('W',1)
      go to 290

  280 nbsort(kbus)=isw
  290 continue
      call sinput
  300 if (islnsw.eq.0) go to 330
      go to (1900,330,330,260,260,330), islnsw  
  310 write (errbuf(1),320) buf(1:50)   
  320 format ('0 ILLEGAL SOLUTION CONTROL CARD SEQUENCE :(', a, ')')
      if (is_batch .eq. 0) then
         call prterx ('E',1)
      else
         call prterx ('F',1)
      endif
      go to 170

  330 do 340 k=1,ntot
         if (nbsort(k).eq.0) go to 340
         kt=inp2opt(k)
         buspen(kt)=vpen
  340 continue
      do 350 k=1,ntot
  350 nbsort(k)=0
      if (ntotb.eq.0) go to 380
C
C     CONVERT TYPES "BQ", "BG", AND "BX" BUSSES IN OPTIMAL ZONES INTO
C     TEMPORARY TYPE "BO" BUSSES
C       
      do 370 i=1,ntotb  
         ltyp=tbx(1,i)
         if (ltyp.ge.10) go to 370 
         kt=tbx(2,i)  
         nbsort(kt)=i  
  370 continue  
  380 continue  
      do 650 nb=1,ntot  
      k=inp2opt(nb)   
      if (buspen(k).eq.0.0) go to 650   
      ntyp = ntypu(k)                              
      kmod=0
      jtbx=0
      jxdta=0   
      ntypo=ntyp
      v=dsqrt(e(k)**2+f(k)**2)   
      emin=vlimn(k)                               
      emax=vlimx(k)                               
      oldmin = busdta(11,nb)
      oldmax = busdta(12,nb)
      go to (400,390,390,390,400,400,390,400,400,390,400,400,390,395,   
     1       395,395) ntyp  

  390 busdta(11,nb)=0.0 
      busdta(12,nb)=0.0 
      go to 400 

  395 busdta(11,nb)=0.0 
  400 call vltlim(nb,vmin,vmax,vstrt)  
      vmin=amin1(emin,vmin) 
      vmax=amax1(emax,vmax) 
C       
C     "IOPTON(36)" IS BASE CASE VOLTAGE RELAXATION OPTION   
C       
      if (iopton(36).eq.0) go to 410
      vmin = amin1 (v,vmin) 
      vmax = amax1 (v,vmax) 
  410 vlimn(k) = vmin 
      vlimx(k) = vmax  
      if (vmin.eq.emin.and.vmax.eq.emax) go to 430  
      kmod = 1  
      call typno (jtyp,ntyp)
      write (outbuf,420) intbus(nb),intbas(nb),jtyp,v,emin,emax,
     1  vmin,vmax   
  420 format (' VOLTAGE LIMITS RELAXED ON BUS ',a8,f7.1,' TYPE ',a1 
     1  ,' VOLTAGE ',f6.4,' OLD LIMITS - ',2f7.4,' NEW LIMITS - ',2f7.4)
      call prtout(1)
  430 go to (510,510,600,510,650,440,440,440,640,510,440,650,440,   
     1       510,510,440) ntyp  
  440 jtbx=nbsort(k)
      ltyp=tbx(1,jtbx) 
      ityp=tbx(7,jtbx) 
      go to (450,470,460,650,500,450), ltyp 
C       
  450 if (ityp.ne.1) qnetu(k)=tbx(5,jtbx)                  
      tbx(1,jtbx)=11   
      tbx(7,jtbx)=1
      go to 490 
C       
  460 qmin=tbx(4,jtbx)  
      qmax=tbx(3,jtbx)  
      go to 480 
C       
  470 tbx(1,jtbx)=4
      ntyp=9
      go to 490 
C       
  480 continue  
      tbx(3,jtbx)=qmax-dmin1(0.0d0,tbx(6,jtbx)*v**2)  
      tbx(4,jtbx)=qmin-dmax1(0.0d0,tbx(6,jtbx)*v**2)  
      tbx(5,jtbx)=v 
      tbx(1,jtbx)=4
      tbx(7,jtbx)=1
      ntyp=9
  490 kmod=1
      go to 610 
C       
  500 jxdta=tbx(5,jtbx)
      totrek=xdata(3,jxdta) 
      totcap=xdata(4,jxdta) 
      userek=xdata(5,jxdta) 
      usecap=xdata(6,jxdta) 
      bkku(k) = bkku(k) +(totrek+totcap-usecap-userek)/bmva   
      xdata(5,jxdta)=totrek 
      xdata(6,jxdta)=totcap 
      go to 460 
C       
  510 continue  
      qmax=busdta(9,nb) 
      qmin=busdta(10,nb)
      bkkadj=busdta(6,nb)   
      ncb=kbsdta(15,nb) 
      do while (ncb .gt. 0) 
         call getchr(1,cbtype,kbctbl(8,ncb)) 
         if (cbtype.ne. 'A') bkkadj = bkkadj + bctbl(5,ncb) / bmva  
         ncb = bctbl_nxt(ncb) 
      enddo

      bkkadj=bkkadj/bmva
      if (ntyp .eq. 2 .or. ntyp .eq. 15) go to 560  
      if (abs(bkkadj).gt.0.50) go to 550
      if (qmax-qmin.lt.25.0) go to 600  
  550 continue  
C       
C     LOAD BUSSES WITH ADJUSTABLE REACTIVE AND TYPE 'E' BUSSES ARE  
C     CHANGED ULTIMATELY TO SUB-TYPE 'Q'.   
C       
  560 call typno (jtyp,ntyp)
      write (outbuf,570) intbus(nb),intbas(nb),jtyp,bkkadj,qmax,qmin
      call prtout(1)
  570 format(' OPTIMAL CHANGES MODIFIED BUS ',a8,f7.1,' TYPE ',a1,  
     1' adjustable shunt ',f7.1,' mvar -- generator reactive limits '   
     2 ,2f8.3)  
      ntotb = ntotb + 1 
      if (ntotb.le.MAXTBX) go to 590
      write (errbuf(1),580) MAXTBX  
  580 format('0 OPTIMAL CHANGES HAVE CREATED MORE THAN ',i4,' "TBX" ',  
     1 'entities. further optimal changes curtailed.')  
      call prterx ('W',1)   
      ntotb=MAXTBX  
      go to 610 
C       
  590 ntyp=9
      if (ntyp .eq. 2 .and. iopton(37) .ne. 0) then 
         ntypo=2
         qmax=1.0e8 
         qmin=1.0e8 
      else if (ntyp .eq. 15 .and. iopton(37) .ne. 0) then   
         ntypo=15   
         qmax=1.0e8 
         qmin=1.0e8 
      else  
         ntypo=7
      endif 
C       
      kmod=1
      tbx(1,ntotb)=4   
      tbx(2,ntotb)=k   
      tbx(3,ntotb)=qmax/bmva-qloadu(k)-amin1(0.0,bkkadj*v**2)  
      tbx(4,ntotb)=qmin/bmva-qloadu(k)-amax1(0.0,bkkadj*v**2)  
      tbx(5,ntotb)=v
      tbx(6,ntotb)=bkkadj   
      tbx(7,ntotb)=1   
      tbx(8,ntotb)=0   
      go to 610 
C       
  600 if (kmod.eq.0) go to 650  
  610 ncvrt=ncvrt+1 
      if (ncvrt.le.MXOPCH) go to 630
      write (errbuf(1),620) MXOPCH  
  620 format('0 MORE THAN ',i4,' OPTIMAL BUS CHANGES.  FUTHER CHANGES ',
     1' curtailed.')
      call prterx ('W',1)   
      ncvrt=MXOPCH  
      go to 660 
C       
  630 knvert(1,ncvrt)=k 
      knvert(2,ncvrt)=ntypo 
      knvert(3,ncvrt)=jtbx  
      knvert(4,ncvrt)=jxdta 
      ntypu(k) = ntyp  
  640 if (ntyp.eq.3.or.ntyp.eq.9) nntyp9=nntyp9+1   
  650 continue  
  660 continue  
      if (ntota.eq.0) go to 690 
C       
      do 680 i=1,ntota  
      ltyp=ltran(10,i)  
      knvtx(i)=ltyp 
      if (ltyp.ne.1) go to 680  
      k=ltran(1,i)  
      m=ltran(2,i)  
      if(buspen(k).eq.0.0 .and. buspen(m) .eq. 0.0) go to 680   
      ltran(10,i)=4 
      nltc=nltc+1   
  680 continue  
  690 continue  
      go to 170 
C       
C     PROCESS REACTIVE PENALTY BUSSES   
C       
  700 if (nqpen.ne.0) go to 1900
      iopton(20)=1  
      ksw=1 
      do 970 i=1,idat   
      nb = find_bus(namdat(i),bsedat(i))
      if (nb .gt. 0) go to 720
      write (errbuf(1),710) namdat(i),bsedat(i) 
  710 format ('0 REACTIVE PENALTY BUS ',a8,f7.1,' IS NOT IN SYSTEM.')   
      call prterx ('W',1)   
      go to 970 
C       
  720 kt=inp2opt(nb)  
      if (buspen(kt).eq.0.0) go to 750  
      if (ntypu(kt).ne.5.and.ntypu(kt).ne.12) go to 770   
      write (errbuf(1),740) intbus(kt),intbas(kt)   
  740 format ('0 DATA ERROR --  DC BUS ',a8,f7.1,' CANNOT HAVE REACTIVE 
     1penalties.  bus ignored. ')   
      call prterx ('W',1)   
      go to 970 
C       
  750 write (errbuf(1),760) intbus(kt),intbas(kt)   
  760 format ('0 DATA ERROR --  BUS ',a8,f7.1,' WITH REACTIVE PENALTIES 
     1must reside in optimized system.  bus ignored. ') 
      call prterx ('W',1)   
      go to 970 
C       
  770 if (ncvrt.eq.0) go to 790 
      do 780 l=1,ncvrt  
      if (knvert(1,l).eq.kt) go to 810  
  780 continue  
  790 ncvrt=ncvrt+1 
      if (ncvrt.le.MXOPCH) go to 800
      write (errbuf(1),620) MXOPCH  
      call prterx ('W',1)   
      ncvrt=MXOPCH  
      ksw=2 
      go to 970 
C       
  800 knvert(1,ncvrt)=kt
      l=ncvrt   
  810 continue  
      if (ksw.eq.2) go to 970   
      nqpen=nqpen+1 
      if (nqpen.le.MXQPEN) go to 830
      write (errbuf(1),820) 
  820 format ('0 MORE THAN ',i4,' REACTIVE PENALTY BUSES.  REMAINDER ', 
     1'ignored. ')  
      call prterx ('W',1)   
      nqpen=MXQPEN  
      ksw=2 
      go to 980 
C       
  830 kqpent(1,nqpen)=nb
      qcon = -busdta(4,nb)  
      bkkadj = busdta(6,nb) 
      qmax=busdta(9,nb) 
      qmin=busdta(10,nb)
      ncb=kbsdta(15,nb) 
      do while (ncb .gt. 0)
         call getchr(1,cbtype,kbctbl(8,ncb)) 
         if (cbtype .ne. 'A') then
            bkkadj=bkkadj + bctbl(5,ncb)  
            qcon=qcon-bctbl(3,ncb)
         endif
         ncb = bctbl_nxt(ncb)
      enddo

      ntypu(kt) = 9
      kqpent(2,nqpen)=ntypu(kt)                    
      jtbx=knvert(4,l)  
      jxdta=knvert(3,l) 
      knvert(2,l)=2 
      knvert(3,l)=0 
      knvert(4,l)=0 
      if (jtbx.eq.0) go to 950  
      ltyp=tbx(1,jtbx) 
      ityp=tbx(7,jtbx) 
      if (ltyp.gt.9) go to 950  
      go to (940,870,880,870,890,940) ltyp  
  870 go to (940,950,910,920) ityp  
  880 go to (940,930,930,940,940), ityp 
  890 go to (940,930,930,900), ityp 
  900  totrek=xdata(3,jxdta)
       totcap=xdata(4,jxdta)
       userek=xdata(5,jxdta)
       usecap=xdata(6,jxdta)
       bkku(kt) = bkku(kt) + (totrek+totcap - usecap - userek)/bmva
       xdata(5,jxdta)=totrek
       xdata(6,jxdta)=totcap
      go to 930 
C       
  910 bkku(kt) = bkku(kt) + dmax1(0.0d0,tbx(6,jtbx)) 
      go to 930 
C       
  920 bkku(kt) = bkku(kt) + dmin1(0.0d0,tbx(6,jtbx)) 
  930 c=tbx(5,jtbx)/dsqrt(e(kt)**2+f(kt)**2)         
      e(kt)=e(kt)*c 
      f(kt)=f(kt)*c 
  940 tbx(1,jtbx)=ltyp+10  
      tbx(7,jtbx)=1
  950 emag=dsqrt(e(kt)**2+f(kt)**2)  
      dv=dim(emag,vlimx(kt)) - dim(emag,vlimn(kt))      
      if (dv.eq.0.0) go to 960
      c = (emag - dv)/emag  
      e(kt)=e(kt)*c 
      f(kt)=f(kt)*c 
      emag = emag - dv  
  960 qpent(3,nqpen) = (qmin + qcon - amax1(0.0,bkkadj)*emag**2)/bmva   
      qpent(4,nqpen) = (qmax + qcon - amin1(0.0,bkkadj)*emag**2)/bmva   
  970 continue  
  980 if (nqpen.gt.0) go to 1000
      write (errbuf(1),990) 
  990 format('0 NO BUS DATA FOLLOWS "REACTIVE PENALTIES" SOLUTIONS ',   
     1 'text.') 
      call prterx ('W',1)   
      qpen=0.0  
      call sinput   
      go to 170 
 1000 ndup=0
      call qiksrt(1,nqpen,kmpqpn,swpqpn)
      if (ndup.eq.0) go to 1030 
C       
C     ELIMINATE DUPLICATE DATA ITEMS
C       
      j=1   
      k=2   
 1010 if(kmpqpn(j,k).lt.0)then  
         j=j+1  
         if(j.lt.k) call swpqpn(j,k)
         k=k+1  
      else  
         nb=kqpent(1,j) 
         kt = inp2opt(nb) 
         write (errbuf(1),1020) intbus(kt),intbas(kt)   
 1020    format('0 DUPLICATE "REACTIVE PENALTY BUS ": ',a8,f6.1,'.',
     1   ' second item deleted.')   
         call prterx ('W',1)
         k=k+1  
      endif 
C       
      if(k.le.nqpen) go to 1010 
      nqpen=j   
 1030 do 1040 i=1,nqpen 
      nb=kqpent(1,i)
 1040 kqpent(1,i)=inp2opt(nb) 
      call sinput   
      go to 170 
C       
C     PROCESS ECONOMIC DISPATCH BUSSES  
C       
 1050 if (ngen.ne.0) go to 1900 
      iopton(20)=1  
      ksw=1 
      do 1100 i=1,idat  
      nb = find_bus(namdat(i),bsedat(i))
      if (nb .gt. 0) go to 1070   
      write (errbuf(1),1060) namdat(i),bsedat(i)
 1060 format ('0 ECONOMIC DISPATCH BUS ',a8,f7.1,' IS NOT IN SYSTEM. ', 
     1'dispatch aborted. ') 
      call prterx ('W',1)   
      ksw=2 
      go to 1100
 1070 if (ksw.eq.2) go to 1100  
      ngen=ngen+1   
      if (ngen.le.MXDISP) go to 1090
      write (errbuf(1),1080) MXDISP 
 1080 format ('0 MORE THAN ',i4,' DISPATCHABLE BUSSES. OPTIMAL ',   
     1'dispatch aborted. ') 
      call prterx ('W',1)   
      ksw=2 
      ngen=MXDISP   
      go to 1100
 1090 kecon(1,ngen)=nb  
      econ(2,ngen)=0
      econ(3,ngen)=data(1,i)
      econ(4,ngen)=data(2,i)
      econ(5,ngen)=0.0  
      econ(6,ngen)=0.0  
      econ(7,ngen)=0.0  
      econ(8,ngen)=data(3,i)
      econ(9,ngen)=data(4,i)
      econ(10,ngen)=data(5,i)   
 1100 continue  
      if (ksw.eq.2) go to 170   
      if (ngen.gt.0) go to 1130 
      write (errbuf(1),1120)
 1120 format('0 NO BUS DATA FOLLOWS "ECONOMIC DISPATCH" SOLUTION TEXT.')
      call prterx ('W',1)   
      go to 170 
 1130 ndup=0
      call qiksrt(1,ngen,kmpecn,swpecn) 
      if (ndup.eq.0) go to 1160 
C       
C     ELIMINATE DUPLICATE DATA ITEMS
C       
      j=1   
      k=2   
 1140 if(kmpecn(j,k).lt.0)then  
         j=j+1  
         if(j.lt.k)call swpecn(j,k) 
         k=k+1  
      else  
         nb=kecon(1,j)  
         kt = inp2opt(nb) 
         write (errbuf(1),1150) intbus(kt),intbas(kt)   
 1150    format('0 DUPLICATE "ECONOMIC DISPATCH" BUS: ',a8,f6.1,'. ',   
     1'second item deleted.')   
         call prterx ('W',1)
         k=k+1  
      endif 
      if (k.le.ngen) go to 1140 
      ngen=j
 1160 do 1170 i=1,ngen  
      nb=kecon(1,i) 
 1170 kecon(1,i)=inp2opt(i)   
      do 1190 ig=1,ngen 
      kt=kecon(1,ig)
      pgen=pnetu(kt) +ploadu(kt)                  
      dp=dim(pgen,econ(4,ig)/bmva)-dim(econ(3,ig)/bmva,pgen)
      if (dp.eq.0.0) go to 1190 
      write (outbuf,1180) intbus(kt),intbas(kt),pgen,dp 
 1180 format ('0 ECONOMIC DISPATCHABLE BUS ',a8,f7.1,' WITH GENERATOR', 
     1 e11.4,' violates limits by ',e11.4,' pu mw.')
      call prterx ('W',1)   
      econ(3,ig)=econ(3,ig)+amin1(0.0,dp)*bmva  
      econ(4,ig)=econ(4,ig)+amax1(0.0,dp)*bmva  
 1190 continue  
      go to 170 
C       
C     Store "INJECTION SENSTIIVITIES" until OPSLN3. 
C       
 1200 continue  
      go to 1330
C       
C     PROCESS TRACE BUSES   
C       
10300   nbttot = 0  
        do 10320 i=1,idat   
           kt = find_bus( namdat(i) , bsedat(i) )
           if ( kt .le. 0 ) then
              write (errbuf(1),10310) namdat(i),bsedat(i) 
10310         format('0 "TRACE_BUS" ',a8,f7.1,' IS NOT IN SYSTEM.'
     1          ,'  bus ignored.')  
              call prterx ('W',1) 
           else 
              nbttot = nbttot + 1 
              ibus(1,nbttot) = kt 
           endif
10320   continue
C       
C  ERROR IF NO VALID "TRACE_BUS" DATA REMAINS   
C       
        if (nbttot.gt.0) go to 10410
        write (errbuf(1),10400) 
10400   format('0 NO VALID BUS DATA FOLLOWS "TRACE_BUS" SOLUTION TEXT.')
        call prterx ('W',1) 
        go to 170   
10410   key=0   
        call qiksrt(1,nbttot,kmpbt,swpbt)   
        if (key.eq.0) go to 10440   
C       
C     ELIMINATE DUPLICATE BUSES 
C       
        j=1 
        k=2 
10420   if(kmpbt(j,k).lt.0) then
                j=j+1   
                if(j.lt.k)call swpbt(j,k)   
                k=k+1   
           else 
                nb=ibus(1,j)
                kt=inp2opt(nb)
                write (errbuf(1),10430) intbus(kt),intbas(kt)   
10430           format('0 DUPLICATE "TRACE_BUS" ',a8,f7.1,'. ', 
     1          'second item ignored.') 
                call prterx ('I',1) 
                k=k+1   
           endif
        if(k.le.nbttot) go to 10420 
        nbttot=j
C       
C  CONVERT TO INTERNAL BUS NUMBERS  
C       
C  STORE INDEX TO TBX ARRAY FOR BUS TYPES V,Q,G,O,X,F,L
C                         -1 FOR BUS TYPES 'blank',C,D,T,M,J
C                         -2 FOR BUS TYPES E,S,K
C       
10440   do 10460 i=1,nbttot 
           nb=ibus(1,i) 
           kt=inp2opt(nb) 
           ibus(1,i)=kt 
           ntyp = kbsdta(1,nb)  
           indtbx = tbxtyp(ntyp)
           if (indtbx .gt. 0 )          then
                ibus(3,i) = - indtbx
              else  
                j = 1   
10450           ktest = tbx(2,j)   
                if( ktest .eq. kt ) then
                        ibus(3,i) = j   
                   else 
                        j = j + 1   
                        if( j .le. MAXTBX ) go to 10450 
                   endif
              endif 
10460   continue
         go to 170  
C       
C  PROCESS TRACE LTC'S  
C       
10500   ntttot = 0  
        do 10520 i=1,idat,2 
        kt = find_bus( namdat(i) , bsedat(i) )
        mt = find_bus( namdat(i+1) , bsedat(i+1) )
        if ( kt .le. 0 .or. mt .le. 0 ) then 
           write (errbuf(1),10510) namdat(i),bsedat(i), namdat(i+1),
     &                             bsedat(i+1)  
10510      format('0 "TRACE_LTC" ',a8,f7.1,4x,a8,f7.1,' HAS BUS ', 
     &            'name(s) not in system.  ltc ignored.') 
           call prterx ('W',1) 
        else 
           ntttot = ntttot + 1 
           itx(2,ntttot) = kt  
           itx(3,ntttot) = mt  
        endif
10520   continue
C       
C  CONVERT TO INTERNAL BUS NUMBERS  
C       
        do 10530 i=1,ntttot 
           kt = itx(2,i)   
           mt = itx(3,i)   
           itx(2,i) = inp2opt(kt)
           itx(3,i) = inp2opt(mt)
10530      continue 
C       
C  TEST FOR VALID LTC   
C       
        itxc = 1
C       
C  INITIALIZE FIRST PASS - FOWARD TEST  
C       
10550   ipass = 1   
        match = 0   
        jrow = 0
        ltranc = 1  
        itxpak = ipack_2 (itx(2,itxc), itx(3,itxc))
C       
C  BEGIN LOOP   
C       
10560   ltrpak = ipack_2 (ltran(1,ltranc), ltran(9,ltranc))
        if ( itxpak - ltrpak .eq. 0 ) then   
           match = 1   
           jrow = ltranc   
        endif
        ltranc = ltranc + 1 
        if( match .eq. 0 ) then
C       
C  TEST NEXT ROW IN LTRAN ARRAY 
C       
           if( ltranc .le. ntota ) go to 10560 
C       
C  INITIALIZE SECOND PASS - REVERSE TEST
C       
           if( ipass .eq. 1 ) then 
              ipass = 2   
              ltranc = 1  
              itxpak = ipack_2 (itx(2,itxc), itx(3,itxc))
              go to 10560 
           endif
        endif
C       
C  STORE LTC NUMBER IF VALID, OR FLAG WITH 0 IF NOT VALID   
C  PROCESS NEXT ROW IN ITX ARRAY
C       
        itx(1,itxc) = jrow  
        itxc = itxc + 1 
        if( itxc .le. ntttot ) go to 10550  
C       
C  ELIMINATE BAD LTC'S FROM ITX ARRAY   
C       
        ngood = 0   
        do 10580 itxc = 1,ntttot
C       
C  TEST FOR BAD LTC 
C       
        if( itx(1,itxc) .eq. 0 ) then   
                kt = itx(2,itxc)
                mt = itx(3,itxc)
                write (errbuf(1),10570) intbus(kt),intbas(kt),  
     1                  intbus(mt),intbas(mt)   
10570           format('0 "TRACE_LTC" ',a8,f7.1,4x,a8,f7.1, 
     1                  ' is not in system.  ltc ignored.') 
                call prterx ('W',1) 
           else 
                ngood = ngood + 1   
                itx(1,ngood) = itx(1,itxc)  
           endif
10580   continue
        ntttot = ngood  
C       
C  ELIMINATE DUPLICATE LTC'S
C       
        key=0   
        call qiksrt(1,ntttot,kmplt,swplt)   
        if (key.eq.0) go to 10640   
        j=1 
        k=2 
10620   if(kmplt(j,k).lt.0) then
                j=j+1   
                if(j.lt.k)call swplt(j,k)   
                k=k+1   
           else 
                kt = itx(2,j)   
                mt = itx(3,j)   
                write (errbuf(1),10630) intbus(kt),intbas(kt),  
     1                  intbus(mt),intbas(mt)   
10630           format('0 DUPLICATE "TRACE_LTC" ',a8,f7.1,4x,   
     1                  a8,f7.1,' . second item ignored.')  
                call prterx ('I',1) 
                k=k+1   
           endif
        if(k.le.ntttot) go to 10620 
        ntttot=j
C       
C  ERROR IF NO VALID "TRACE_LTC" DATA REMAINS   
C       
10640   if( ntttot .eq. 0) then 
                write (errbuf(1),10650) 
10650           format('0 NO VALID LTC DATA FOLLOWS "TRACE_LTC" ',  
     1                  'solution text.')   
                call prterx ('W',1) 
           endif
        go to 170   
C       
C     Process /GEN_DROP commands.   
C       
10700 call getdrp (dropmw, error)   
      if (error .ne. 0) call erexit 
        
      if (buf(1:1) .eq. '(') then   
        islnsw = 0  
        goto 1330   
      else  
        inptls = 1  
        call sinput 
        go to 170   
      endif 
C       
C     END OF PROCESS-SOLUTION TEXT RECORDS. 
C       
 1330 continue  
C       
C     BUILD PERCENTAGE VAR CONTROL ARRAY "PCTVR"
C       
      if (ntotb .gt .0) then
         call varint
      endif 
C       
C     INITIALIZE DC ARRAY   
C       
      if (kdtot .gt. 0 .or. mtdcbs .gt. 0) then 
         call dcdata
      endif 
      go to 1920
        
 1900 write (errbuf(1),1910) buf(1:50)  
 1910 format ('0 DUPLICATE SOLUTION TEXT CARD  (', a, ')')  
      call prterx ('W',1)   
      call erexit   
        
 1920 continue  
      end   
