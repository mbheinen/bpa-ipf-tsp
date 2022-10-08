C    %W% %G%
      subroutine matmod                                               
c                                                                      
c     THIS SUBROUTINE MAKES THE MODIFICATIONS TO THE BUS ADMITTANCE   
c     MATRIX REQUIRED BY THE LINE SWITCHING CARDS.  IT IS CALLED BY   
c     REDUCE.  IT CALLS LSRCH, EREXIT, GENDROP, SSORT.                
c                                                                      
      include 'tspinc/params.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/ectba.inc' 
      include 'tspinc/dcmodd.inc' 
      include 'tspinc/mdctbl.inc' 
      include 'tspinc/svs.inc' 
      include 'tspinc/ecstbb.inc' 
      include 'tspinc/ecstbc.inc' 
      include 'tspinc/ecstbd.inc' 
      include 'tspinc/ecstbh.inc' 
      include 'tspinc/cntrl2.inc' 
      include 'tspinc/toler.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/contrl.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/search.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/lnk12a.inc' 
      include 'tspinc/link2.inc' 
      include 'tspinc/lnk1a.inc' 
      include 'tspinc/lnk1c.inc' 
      include 'tspinc/lnk2c.inc' 
      include 'tspinc/lnkcd.inc' 
      include 'tspinc/rk.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/param1.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/equiv.inc' 
      include 'tspinc/brake1.inc' 
      include 'tspinc/rbcom.inc' 
      include 'tspinc/ldidxn.inc' 
      include 'tspinc/busdta.inc' 
      include 'tspinc/ldrep.inc' 
      include 'tspinc/fltopt.inc' 
      include 'tspinc/newton.inc' 
      include 'tspinc/buslod.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/igentn.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 
      include 'tspinc/tsldat.inc' 
      include 'tspinc/ldshdn.inc' 
      include 'tspinc/ijfltn.inc' 

      common/ipcdn/ipcdn                                              
      common/ipcdc/ipcdc                                              
      character*1 ipcdc                                               

      character*1 idm                                                 
      character*8 name, busi, busj                                    
      logical debug,dbghere                                             !dem

      data small / 1.0e-5 /                                           
C     -
C     -    Begin       Begin       Begin       Begin       Begin
C     -
      debug = dbghere ('MATMOD  ')                                      !dem
      linadd=0                                                        
      nordk = 0                                                       
      opnckt = 134217728.                                             

c     NORDK IS A FLAG TO SIGNAL IF NETWORK REDUCTION WILL BE DONE IN 
c     REDUCE. WHEN NORDK=1, REDUCTION IS DONE. THE FLAG IS TRIGGERED 
c     BY SWITCHING CARDS REQUIRING REDUCTION                         
c     OPNCKT IS DEFINED TO BE A BIG NUMBER (2**27). THIS NUMBER WILL 
c     MULTIPLY THE ACTUAL DC BRANCH DATA WHEN SIMULATING AN OPENED   
c     BRANCH.  REVERSE WILL BE DONE WHEN RECLOSING.                  
                                                                        
      go to (3200,140,140), matsw                                       
  120 itly=itly+1                                                       
      if (itly.gt.iss) go to 3200                                        
      go to 180                                                         
  140 imat3=1                                                           
      itly=1                                                            
      gkfi=0.0                                                          
      bkfi=0.0                                                          
      gkfj=0.0                                                          
      bkfj=0.0                                                          
      il2 = 2*iline                                                     
      il10 = 10*iline                                                   
      call ssort                                                        
      if (istat.ne.1) go to 180                                         
      do i=1,il2                                                    
        grz(i)=0.0                                                        
        gsr(i)=0.0                                                      
        bsr(i)=0.0                                                      
        brz(i)=0.0                                                        
      enddo
      do i = 1, iline, 1                                       
        gijh(i) = 0.0                                                 
        bijh(i) = 0.0                                                 
      enddo
  180 iba=isortn(1,itly)                                              
      jba=isortn(2,itly)                                              
      ipcdn = isortn(3, itly)                                       
      ipcdc = isortc(itly)                                          
      ili=isorti(itly)                                                  
      ils=locst(ili)                                                    
      mde1=mflt(ils)                                                    
      mde=iabs(mde1)                                                    !dem                              
      if (debug)  then                                                  !dem
        call dbgeko ('MATMOD - (s180+8) Found a manual switching card') !dem
        call dbgwrf ('  TO /time/ = ',to)                               !dem
        call dbgwri ('  MDE /mode of sw/ = ',mde)                       !dem
      endif                                                             !dem
c
c     SET NETWORK REDUCTION FLAG                                     
      if (mde .lt. 5) nordk = 1                                            
c                                                                      
c     LOAD/GENERATION MODIFICATION                                    
c                                                                      
      if (mde .eq. 4) then                                                
        i840 = 1                                                       
        go to 2200                                                     
c                                                                      
c     DC OFFSET GENERATOR FAULT DAMPING                               
c                                                                      
      else if (mde.eq.7) then                                                 
         i1450 = 1                                                      
         go to 2700                                                     
c                                                                      
c     GENERATOR FAST VALVING                                          
c                                                                      
      else if (mde.eq.8) then                                                 
         i1550 = 1                                                      
         go to 3000                                                     
c                                                                      
c     MANUAL TRIP OF TRANSIENT STABILIZER OR SVS FREEZE               
c                                                                      
      else if (mde .eq. 9) then                                                
         imde9 = 1                                                      
         go to 3145                                                     
      endif                                                             
      if (mde.ne.5) go to 440                                           
      i30=1                                                             
  200 if (ldc.eq.0) go to 420                                           
      if (iba.gt.jba) go to 420                                         
c
c     UPDATE PROPER DC CODES                                            

      i1=0                                                              
      do 220 i=1,ldc                                                    
        k1=kdc+idcl*i1                                                    
        call redecs (ptab,k1,idcl)                                        
        if (iba .eq. itab(35) .and. jba .eq. itab(37) .or.
     &      jba .eq. itab(35) .and. iba .eq. itab(37)) go to 240                                           
        i1=i1+1                                                           
  220 continue                                                          
  240 mdc=ipcdt(ils)                                                    
      mdca=iabs(mdc)                                                    
      busi = bname(iba)                                                 
      busj = bname(jba)                                                 
      go to (260,280,300,310,320,360,390,381),mdca                      
c
c     LEFT FLT                                                          

  260 itab(105)=1                                                      
      if (mdc .lt. 0) go to 400                                        
c
c     SETUP PROPER L,R COMBINATUON                                          

      itab(105)=2                                                      
      coilr=ptab(27)                                                   
      resr=0.0                                                         
      coili=ptab(28)+ptab(9)                                           
      resi=ptab(8)                                                     
      go to 400                                                         
c
c     RIGHT FLT                                                         

  280 itab(105)=1                                                      
      if (mdc .lt. 0) go to 400                                        
      itab(105)=3                                                      
      coilr=ptab(27)+ptab(9)                                           
      resr=ptab(8)                                                     
      coili=ptab(28)                                                   
      resi=0.0                                                         
      go to 400                                                         
c
c     INTERMEDIATE FLT                                                  

  300 itab(105)=1                                                      
      if (mdc .lt. 0) go to 400                                        
      itab(105)=4                                                      
      coilr=ptab(27)+tab(13)                                           
      resr=tab(14)                                                     
      coili=ptab(28)+tab(28)                                           
      resi=tab(29)                                                     
      go to 400                                                         
c                                                                      
c      CODE FOR RECTIFIER BLOCKING OR UNBLOCKING                       
c                                                                      
  310 if (mdc .lt. 0) then                                               
         write (outbuf,311) busi, busj, tsim                                
  311    format('0', 5x, 'DC LINE', 2x, a8, 5x, a8, 2x,
     &     'UNBLOCKED AT ', f7.2,' CYCLES')                                         
         call prtout (1)                                               
c                                                                      
c      RESET STATE VARIABLES TO VALUES AT THE TIME BLOCKING OCCURED    
c                                                                      
         tab(110) = tab(125)                                           
         tab(112) = tab(125)                                           
         tab(111) = tab(126)                                           
         tab(113) = tab(126)                                           
         tab(97) =  tab(123)                                           
         tab(95) =  tab(123)                                           
         tab(96) =  tab(124)                                           
         tab(98) =  tab(124)                                           
         tab(68) =  tab(127)                                           
         tab(92) =  tab(127)                                           
         tab(69) =  tab(128)                                           
         tab(94) =  tab(128)                                           
         tab(156) = tab(145)                                           
         tab(157) = tab(145)                                           
         do itrr = 1,17                                            
            tab(70+itrr) = tab(128+itrr)                                  
            tab(50+itrr) = tab(128+itrr)                                  
         enddo
         iblck = 1                                                     
         go to 400                                                     
      endif                                                            
c                                                                      
c     RECTIFIER BLOCKING                                              
c                                                                      
      iblck=2                                                          
      ablck=fltr(ils)                                                  
      if (ablck.eq.0.0) ablck=135.0                                    
      ablck=ablck*0.01745329                                           
      ablck=cos(ablck)                                                 
      write (outbuf,315) busi, busj, tsim                                   
315   format('0', 5x, 'DC LINE', 2x, a8, 5x, a8, 2x, 'BLOCKED AT ', 
     &   f7.2, '  CYCLES')                                                      
      call prtout (1)                                                  
c                                                                      
c     STORE STATE VARIABLES TO BE USED AT UNBLOCKING                  
c                                                                      
      tab(123) = tab(95)                                               
      tab(124) = tab(96)                                               
      tab(125) = tab(110)                                              
      tab(126) = tab(111)                                              
      tab(127) = tab(68)                                               
      tab(128) = tab(69)                                               
      do 316 itrr = 1,17                                               
 316  tab(128+itrr) = tab(70+itrr)                                     
      go to 400                                                        
c
c     POWER REVERSAL                                                    

  320 itab(7)=1                                                         
      itab(22)=1                                                        
      if (mdc.gt.0) go to 340                                           
      itab(33)=1                                                        
c
c     IN RETURNING TO ORIG CONDITION IF NEW PWR IS NOT SUPPLIED THEN USE
C     INITIAL VALUES                                                    

      tab(112)=fltr(ils)                                                
      if (fltr(ils).eq.0.0) tab(112)=ptab(5)                            
      go to 400                                                         
c
c     SETUP FOR POWER REVERSAL                                          

  340 itab(33)=2                                                        
      go to 400                                                         
c
c     CURRENT REVERSAL                                                  

  360 itab(7)=2                                                         
      itab(22)=2                                                        
      if (mdc.gt.0) go to 380                                           
      itab(33)=1                                                        
c
c     IN RETURNING TO ORIG CONDITION IF NEW CURR IS NOT SUPPLIED THEN US
C     INITIAL VALUES                                                    

      tab(113)=fltr(ils)                                                
      if (fltr(ils).eq.0.0) tab(113)=ptab(39)                           
      go to 400                                                         
c
c     SETUP FOR CURRENT REVERSAL                                        

  380 itab(33)=2                                                        
      go to 400                                                         
c                                                                      
c     MANUAL CURRENT ORDER CHANGE                                     
c                                                                      
 381  if (mdc .gt. 0) go to 385                                         
      ptab(31) = 0.0                                                   
      write (outbuf, 382) busi, busj, ptab(34), tsim                            
      call prtout (1)                                                   
 382  format('0', 5x, 'CURRENT ORDER FOR DC LINE', 2x, a8, 5x, a8, 2x,         
     &  'RESET TO ', f6.0, '  AT  ', f7.2,' CYCLES')                     
      go to 400                                                         
 385  ptab(31) = fltx(ils) + ptab(31)                                   
      if (fltx(ils) .eq. 0.0) ptab(31)= ptab(34)                          
      write (outbuf,386 ) busi, busj, ptab(31), tsim                          
 386  format('0', 5x, 'CURRENT ORDER FOR DC LINE', 2x, a8, 5x, a8, 2x,         
     &  'MANUALLY CHANGED BY ', f6.2, ' AT ', f7.2,' CYCLES.')            
      call prtout (1)                                                   
      go to 400                                                         
c
c     SET UP FOR POWER ORDER CHANGE                                   

  390 if (mdc .gt. 0) go to 395                                          
      ptab(31) = 0.0                                                   
      write (outbuf,391) busi, busj, ptab(5), tsim                            
      call prtout (1)                                                   
 391  format('0', 5x, 'POWER ORDER FOR DC LINE', 2x, a8, 5x, a8, 2x,
     &  'RESET TO', 12x, f6.2, ' AT ', f7.2,' CYCLES')                             
      go to 400                                                         
 395  ptab(31) = fltx(ils) + ptab(31)                                   
      if (fltx(ils) .eq. 0.0)ptab(31)=ptab(5)                            
      write (outbuf,396 ) busi, busj, ptab(31), tsim                          
 396  format('0', 5x, 'POWER ORDER FOR DC LINE', 2x, a8, 5x, a8, 2x,
     &   'MANUALLY CHANGED BY ', f6.0, ' AT ', f7.2,' CYCLES.')                     
      call prtout (1)                                                   
c
c     WRITE NEW VALUES TO ECS                                           

  400 call ritecs (ptab,k1,idcl)                                        
  420 go to (120,1960),i30                                              
c
c     BYPASS FOLLOWING LOGIC IF NO MULT DC SWITCH CARD               

  440 if (mde.ne.6) go to 450                                            
      i440 = 1                                                          
 4000 if (ldc1.eq.0) go to 4260                                          
      if (iba.gt.jba) go to 4260                                        
      mdc = ipcdt(ils)                                                  
      mdca = iabs(mdc)                                                  
      nockt = ndcckt(ils)                                               
      call redecs(dcb(1), kdcm(nockt), msizeb)                      
      go to (4380, 4020, 4040, 4040, 4400, 5000,4655, 4657), mdca   
 4020 write (errbuf(1),4030)                                            
      call prterr ('E',1)                                               
      call mpost('MATMOD')                                              
 4030 format('0',' MATMOD .. JOB STOPPED. THESE OPTIONS ARE NOT YET',   
     1' READY.')                                                        
      call erexit                                                       
 4040 ibamd = iabs(iftab(ils))                                          
      call redecs(idcf(1),necsf,nterm)                                  
      call redecs(idcf1(1),necsf+nterm,nterm)                           
      do 4220 ik = 1, nterm                                             
      necs1 = idcb(lbt+ik)                                              
      call redecs(moderi,necs1+15-1,1)                                  
c
c     CHECK IF RECTIFIER VALVE ENCOUNTERED                           

      if (moderi.ne.1) go to 4220                                        
c
c     IF BLOCKING ALL RECTIFIERS, SKIP THE FOLLOWING                 

      if (mdca.eq.3) go to 4060                                          
c
c     CHECK IF BUS NO.  OF THIS RECTIFIER MATCHES WITH THAT FROM THE 
c     LS CARD                                                        

      call redecs(ibus,necs1+46,1)                                      
      if (ibamd.ne.ibus ) go to 4220                                     
c
c     CHECK WHETHER BLOCKING OR UNBLOCKING                           

 4060 if (mdc.lt.0) go to 4180                                           
c
c     IN BLOCKING MODE0  CKECK VALVE STATUS                          

      if (idcf(ik).ne.1) go to 4140                                      
 4080 idcf(ik) = 4                                                      
 4100 cosblk = angblk(ils)                                              
c
c     SET BLOCKING ANGLE TO 135. DEG. IF GIVEN AS ZERO OR BLANK      

      if (cosblk.eq.0.0) cosblk = 135.0                                  
      cosblk = cosblk * 0.01745329                                      
c
c     COSBLK FINALLY CONTAINS COSINE OF BLOCKING ANGLE               

      cosblk = cos(cosblk)                                              
      call ritecs(cosblk,necs1+13-1,1)                                  
 4120 if (mdca.eq.3) go to 4220                                          
c
c     ONLY ONE RECTIFIER TO BE PROCESSED. HENCE GO OUT OF DO LOOP    

      go to 4240                                                        
 4140 if (idcf(ik).ne.3) go to 4320                                      
 4160 idcf(ik) = 2                                                      
      go to 4100                                                        
c
c     WE ARE IN THE UNBLOCKING MODE                                  

 4180 if (idcf(ik).ne.2) go to 4210                                      
 4200 idcf(ik) = 5                                                      
      lsflg = 2                                                         
      go to 4120                                                        
 4210 if (idcf(ik).ne.4) go to 4280                                      
      idcf(ik) = 1                                                      
      lsflg = 2                                                         
      go to 4120                                                        
 4220 continue                                                          
c
c     WRITE NEW INFORMATION TO ECS                                   

 4240 call ritecs(idcf(1),necsf,nterm)                                  
      call ritecs(idcf1(1),necsf+nterm,nterm)                           
      if (keybrd(30) .ne. 0) then                                         
         write (outbuf,81547)                                           
81547    format(1x,' IDCF  AT S81547 ')                                 
         call prtout (1)                                                
         do 71547 jjj = 1, nterm,10                                     
         kkk = min0 (jjj+9,nterm)                                       
         write (outbuf,91547) (idcf(i),i=jjj,kkk)                       
91547    format(10(i4,2x))                                              
         call prtout (1)                                                
71547    continue                                                       
         write (outbuf,81548)                                           
81548    format(1x,' IDCF1 AT S81548 ')                                 
         call prtout (1)                                                
         do 71548 jjj = 1,nterm,10                                      
         kkk = min0 (jjj+9,nterm)                                       
         write (outbuf,91548) (idcf1(i),i=jjj,kkk)                      
91548    format(10(i4,2x))                                              
         call prtout (1)                                                
71548    continue                                                       
      endif                                                             
      call ritecs(idcb(1),kdcm(nockt),lbt)                              
 4260 go to (120,1960), i440                                            
 4280 write (outbuf,4300) nockt,ik,idcf(ik)                             
      call prtout (1)                                                   
      write (outbuf,4301) idcf1(ik)                                     
      call prtout (1)                                                   
 4300 format('0 MATMOD..S4280..MULTDC CKT NO.',i3,' CONVERTER NO. '     
     1 ,i3,' UNBLOCKING A VALVE WITHOUT PRIOR BLOCKING SIGNAL. ACTION TA
     2ken idcf=',i3)                                                    
 4301 format( ' idcf1 =', i3)                                            
      go to 4260                                                        
 4320 write (outbuf,4340) nockt,ik,idcf(ik)                             
      call prtout (1)                                                   
      write (outbuf,4341) idcf1(ik)                                     
      call prtout (1)                                                   
 4340 format('0', '  MATMOD..S4320..MULTIDC CKT NO. ', i3, ' CONVER'
     1         , 'TER NO. ', i3, ', BLOCKING A VALVE WITH A PRIOR BLOCK'
     2         ,'ING SIGNAL. NO ACTION TAKEN.  IDCF = ', i3 )           
 4341 format('  IDCF = ', i3)                                       
      go to 4260                                                        
c
c     LOGIC TO HANDLE MULTI DC BRANCH FAULTS                         

 4380 ibamd = iftab(ils)                                                
      kflt = ibamd                                                      
      if (mdc.lt.0) kflt = - ibamd                                       
      lsflg = 2                                                         
      call ritecs(kflt,kdcm(nockt)+7,4)                                 
      go to (120,1960),i440                                             
c
c     LOGIC TO HANDLE SWITCHING OF MULTI DC BRANCHES                 

 4400 ibamd = iabs(iftab(ils))                                          
      jbamd = iabs(jftab(ils))                                          
      if (iftab(ils).gt.0) go to 4540                                  
c
c     REQUEST TO TRIP A LINE. IF IT IS ALREADY OUT INDICATE BUT TAKE 
c     NO ACTION. IF NOT, STORE IJBMD (PACKED WORD (IBAMD,JBAMD)) IN  
c     THE FIRST AVAILABLE SPACE IN IBRTAB                            
      call recn(ibrtan,2,necsm,10)                                    
      do 4420 ip = 1, 10                                                
        if (ibrtbl(1,ip).ne.ibamd) go to 4410                            
        if (ibrtbl(2,ip).eq.jbamd) go to 4500                            
 4410   if (ibrtbl(1,ip).ne.0) go to 4420                                  
        if (ibrtbl(2,ip).eq.0) go to 4480                                  
 4420 continue                                                          
 4440 write (errbuf(1),4460) nockt                                      
      call prterr ('E',1)                                               
      call mpost('MATMOD')                                              
 4460 format('0 error...matmod...s4440..in multi dc ckt no.', i3,   
     1 '. You are tripping more than 10 branches at one time')          
      call erexit                                                       
 4480 ibrtbl(1,ip)=ibamd                                              
      ibrtbl(2,ip)=jbamd                                              
      ibrknt = ibrknt + 1                                               
      busi = dcec(ibamd,nockt)                                          
      busj = dcec(jbamd,nockt)                                          
      write(outbuf,4485) busi, busj, tsim                                    
 4485 format('0', 5x, 'MULTITERMINAL DC LINE BETWEEN ',a8,' AND ',a8,     
     & ' MANUALLY TRIPPED AT ', f7.2, ' CYCLES. ')                         
      call prtout(1)                                                    
      go to 4640                                                        
 4500 write (errbuf(1),4510) nockt,to                                   
 4510 format('0', '  MATMOD...S4500..MULTDC CKT NO.', i3, ' AT TIME ',   
     &  f7.2, ' CYCLES. ')
      write (errbuf(1),4520)
 4520 format('You are tripping a branch that is already open. Directive 
     1ignored.')                                                
      call prterr ('W',2)                                               
      call mpost('MATMOD')                                              
      go to 4660                                                        
c
c     REQUEST TO RECONNECT A BRANCH. IF IT IS ALREADY IN INDICATE BUT
c     TAKE NO ACTION. IF NOT, DELETE THE CORROSPONDING ENTRY IN      
c     IBRTAB, PRECESS OTHER ENTRIES TO REMOVE GAP AND STORE ZERO AT  
c     THE  END                                                       
c     IF NO PRIOR ENTRIES IN IBRTAB, SKIP TO DIGNOSTIC MESSAGE       
 4540 if (ibrknt .eq. 0) go to 4600                                  
      call recn(ibrtbl,2,necsm,10)                                    
      do 4580  ip = 1, 10                                           
        if (ibrtbl(1,ip).ne.ibamd) go to 4580                            
        if (ibrtbl(2,ip).ne.jbamd) go to 4580                            
c                                                                      
c     WE MATCHED THE ENTRY FOR PREVIOUS TRIPPING OF THIS BRANCH      
c                                                                      
        busi = dcec(ibamd,nockt)                                          
        busj = dcec(jbamd,nockt)                                          
        write(outbuf,4550) busi, busj, tsim                                    
 4550   format('0', 5x, 'MULTITERMINAL DC LINE BETWEEN ', a8, ' AND ',
     &    a8, ' MANUALLY RECLOSED AT ', f7.1, ' CYCLES. ')                        
        call prtout(1)                                                    
        do 4560 iq = ip , 9                                               
          ibrtbl(1,iq)=ibrtbl(1,iq+1)                                     
 4560     ibrtbl(2,iq)=ibrtbl(2,iq+1)                                     
        ibrtbl(1,10)=0                                                  
        ibrtbl(2, 10) = 0                                               
        ibrknt = ibrknt - 1                                               
        newdat = 0                                                        
        ardc = fltr(ils)                                                  
        eldc = fltx(ils)                                                  
        if (ardc.eq.0..and.eldc.eq.0.) go to 4640                          
        newdat = 1                                                        
c
c       CONVERT INDUCTANCE FROM MILLIHENRYS TO PROPER UNITS            
        eldc = eldc * 0.001 * frqbse                                      
        go to 4640                                                        
 4580 continue                                                          
 4600 write (errbuf(1),4620)                                            
      call prterr ('W',1)                                               
      call mpost('MATMOD')                                              
4620  format('  MATMOD...S4600..MULTDC CKT NO. ',                     
     1   i3,' AT TIME ',f6.2,' CYCLES. YOU ARE CONNECTING',             
     2   ' AT BRANCH THAT IS ALREADY IN.  IGNORED.')                    
      go to 4660                                                        
 4640 lsflg = 2                                                         
      irecl=2                                                           
c
c     WALK THRU DCB TABLE TO CHANGE R AND L OF BRANCHES TRIPPED,     
c     RECLOSED, OR RECLOSED WITH NEW BRANCH DATA                     
      ib1 = ibamd                                                       
      jb1 = jbamd                                                       
      if (ib1.lt.jb1) go to 4680                                         
      ib1 = jbamd                                                       
      jb1 = ibamd                                                       
 4680 ldpass = 0                                                        
c
c     LDPASS IS A FLAG TO DETECT IF BOTH ENTRYS IN DCB ARE CHANGED   
      ipnt = lbt + nterm + 1                                            
 4700 ib = - idcb(ipnt)                                                 
      if (ib.gt.10) ib = ib - 10                                         
      if (ib1.eq.ib) go to 4760                                          
      if (ib1.gt.ib) go to 4880                                          
 4720 write (errbuf(1),4740)                                            
      call prterr ('E',1)                                               
      call mpost('MATMOD')                                              
 4740 format(' MATMOD...error condition..s4740.. switch',            
     1   'dc branch not found')                                         
      call erexit                                                       
 4760 ipnt = ipnt + 1                                                   
 4780 jb = idcb(ipnt)                                                   
      if (jb1.eq.jb) go to 4800                                          
      ipnt = ipnt + 8                                                   
      if (ipnt.gt.ndimc) go to 4720                                      
      go to 4780                                                        
 4800 if (iftab(ils).gt.0) go to 4820                                    
c
c     TRIP A MULTI DC BRANCH                                         

      dcb(ipnt+1) = dcb(ipnt+1) * opnckt                                
      dcb(ipnt+2) = dcb(ipnt+2) * opnckt                                
      go to 4860                                                        
 4820 if (newdat.eq.1) go to 4840                                        
c
c     RECLOSE A DC LINE WITH ORIGINAL DATA                           

      dcb(ipnt+1) = dcb(ipnt+1) / opnckt                                
      dcb(ipnt+2) = dcb(ipnt+2) / opnckt                                
      go to 4860                                                        
c
c     RECLOSE A DC LINE WITH NEW DATA                                

 4840 dcb(ipnt+1) = ardc                                                
      dcb(ipnt+2) = eldc                                                
 4860 call ritecs (dcb(ipnt+1),kdcm(nockt)+ipnt,2)                      
      if (ldpass.eq.1) go to 4920                                         
      idum = ib1                                                        
      ib1 = jb1                                                         
      jb1 = idum                                                        
      ldpass = 1                                                        
      go to 4900                                                        
 4880 ipnt = ipnt + 1                                                   
 4900 ipnt = ipnt + 8                                                   
      if (ipnt.gt.ndimc) go to 4720                                      
      if (idcb(ipnt).lt.0) go to 4700                                    
      go to 4900                                                        
 4920 continue                                                          
      call ritecs(lsflg,kdcm(nockt)+10,3)                               
      call wecn(ibrtbl,2,necsm,10)                                      
      write (outbuf,4650) nockt                                         
      call prtout (1)                                                   
      do 4652 ix = 1,10                                                 
         write (outbuf,4651) (ibrtbl(jx,ix),jx=1,2)                     
         call prtout (1)                                                
 4652 continue                                                          
 4650 format('0  MULTI DC CKT NO. ' ,i3,'  IBRTBL' )                    
 4651 format(1x,2i5)                                                    
      go to 4660                                                        
c                                                                      
c     STORE POWER ORDER CHANGE IN THE PROPER DCA TABLE               
c                                                                      
 4655 dpord = dpiord(ils)                                               
c                                                                      
c     100% POWER ORDER CHANGE                                         
c                                                                      
      if (abs(dpord) .eq. 90000.) then                                    
         call redecs(pinit,idcb(lbt+jftab(ils))+25,1)                   
         dpord = abs(pinit)                                             
         if (dpiord(ils) .lt. 0.0)dpord = -dpord                         
      endif                                                             
      call redecs(moderi,idcb(lbt+jftab(ils))+14,1)                     
      if (moderi .ne. 1) dpord = -dpord                                  
      call ritecs(dpord,idcb(lbt+jftab(ils))+19,1)                      
      busi = dcec(jftab(ils),nockt)                                     
      write (outbuf,4656) busi, dpord, tsim                                    
 4656 format('0', 5x, a8, ' MANUAL POWER ORDER CHANGE OF ', f8.2,
     &  ' MW AT ', f7.1,' CYCLES. ')                                          
      call prtout(1)                                                    
      go to 4660                                                        
c
c TEST AND SET CODE FOR FLT BKR OPENING                                 
 4657 if (mdca .ne. 8) go to 4660                                    
      nflt = 999                                                    
      call ritecs(nflt, kdcm(nockt) + 13, 1)                        
      go to 4400                                                    
c                                                                      
c     STORE CURRENT ORDER CHANGE IN THE PROPER DCA TABLE             
c                                                                      
 5000 dliord = dpiord(ils)                                              
c                                                                      
c     100 PERCENT CURRENT ORDER CHANGE REQUESTED                     
c                                                                      
      if (abs(dliord) .eq. 90000.) then                                   
         call redecs(cdesrd,idcb(lbt+jftab(ils))+26,1)                  
         dliord = abs(cdesrd)                                           
         if (dpiord(ils) .lt. 0.0)dliord = -dliord                       
      endif                                                             
      call redecs(moderi,idcb(lbt+jftab(ils))+14,1)                     
      if (moderi .ne. 1) dliord = -dliord                                
 5040 call ritecs(dliord,idcb(lbt+jftab(ils))+73,1)                     
      busi = dcec(jftab(ils),nockt)                                     
      write (outbuf,5045) busi, dliord, tsim                                   
 5045 format('0', 5x, a8, ' MANUAL CURRENT ORDER CHANGE OF ', f8.2,
     &   ' KAMP AT ', f7.1,' CYCLES. ')                                          
      call prtout(1)                                                    
 4660 go to (120,1960),i440                                             
  450 continue                                                          
      call getmat(iba, ii)                                                  
      inn=iba                                                           
      idell=matrow(2)                                                   
      idelh=matrow(3)                                                   
      indx=4                                                            
      if (idell.eq.0) go to 480                                         
      do 460 ind=1,idell                                                
      locl(ind)=matrow(indx)                                            
      gil(ind)=atrow(indx+1)                                            
      bil(ind)=atrow(indx+2)                                            
  460 indx=indx+3                                                       
      go to 500                                                         
  480 gil(1)=0.0                                                    
      bil(1)=0.0                                                    
      locl(1)=0                                                         
  500 do 520 ind=1,idelh                                                
      loch(ind)=matrow(indx)                                            
      gih(ind)=atrow(indx+1)                                            
      bih(ind)=atrow(indx+2)                                            
  520 indx=indx+3                                                       
  540 call lsrch                                                        
      if (ifl.gt.iline) go to 560                                       
      if (bradd(ifl).eq.cyc(ils)) linadd=1                              
      go to (660,580), lsrch2                                           
  560 go to 3200                                                        
  580 if (gijh(ifl).eq.0.0) go to 600                                   
      gil(ijl)=gil(ijl)+gijh(ifl)                                       
      gijh(ifl)=0.0                                                     
  600 if (bijh(ifl).eq.0.0) go to 620                                   
      bil(ijl)=bil(ijl)+bijh(ifl)                                       
      bijh(ifl)=0.0                                                     
c
c                                                                       
  620 if (linadd.eq.0) go to 740                                        
C     ADD BRANCH TO NETWORK                                          

      gil(ijl)=gil(ijl)-gijs(ifl)                                       
      bil(ijl)=bil(ijl)-bijs(ifl)                                       
      ilsabs=iabs(ils)                                                  
      ifils=iftab(ilsabs)                                               
      jfils=jftab(ilsabs)                                               
c
c     READ INDIVIDUAL NEWTAB TABLE ENTRIES                       
      busi = bname(ifils)                                               
      busj = bname(jfils)                                               
      write (outbuf, 640) busi,busj,ipcdc, ipcdn                    
      call prtout (1)                                               
      call skipln (1)                                               
 640  format (18h0*****  parallel  ,a8,2x,a8,2x,a1,1x,i2,             
     1  '  ADDED TO NETWORK*****' )                                     
      go to 720                                                         
  660 if (gijh(ifl).eq.0.0) go to 680                                   
      gih(ijl)=gih(ijl)+gijh(ifl)                                       
  680 if (bijh(ifl).eq.0.0) go to 700                                   
      bih(ijl)=bih(ijl)+bijh(ifl)                                       
c
c                                                                       
  700 if (linadd.eq.0) go to 740                                        
c
c     ADD BRANCH TO NETWORK                                          

      gih(ijl)=gih(ijl)-gijs(ifl)                                       
      bih(ijl)=bih(ijl)-bijs(ifl)                                       
  720 gih(idi)=gih(idi)+gijs(ifl)+gsl(ifi)                              
      bih(idi)=bih(idi)+bijs(ifl)+bsl(ifi)                              
      linadd=0                                                          
  740 if (grz(ifi).eq.0.0) go to 760                                    
      gih(idi)=gih(idi)+grz(ifi)                                        
      grz(ifi)=0.0                                                      
  760 if (brz(ifi).eq.0.0) go to 780                                    
      bih(idi)=bih(idi)+brz(ifi)                                        
      brz(ifi)=0.0                                                      
  780 if (ils.gt.0) go to 800                                           
      fsq=-1.0                                                          
      gf=0.0                                                            
      bf=0.0                                                            
      go to 880                                                         
  800 g1=fltr(ils)                                                      
      b1=fltx(ils)                                                      
c
c     TEST FOR FAULT IMPED                                      

      if (g1 .ne. 0.0 .or. b1 .ne. 0.0) then
         fsq=g1*g1+b1*b1                                                   
         gf=g1/fsq                                                         
         bf=-b1/fsq                                                        
      else
         fsq=-1.0                                                          
         gf=0.0                                                            
         bf=0.0                                                            
      endif
  880 go to (900,1000), lsrch2                                          
  900 if (ijft1n.ne.isortn(1,itly)) go to 910                          
      if (ijft2n.ne.isortn(2,itly)) go to 910                          
      if (ijfltc.eq.isortc(itly)) go to 920                            
  910 continue                                                        
      mde1=0                                                            
      go to 1000                                                        
  920 gfr=gf                                                            
      bfr=bf                                                            
      mder=mde1                                                         
      if (mde1) 940,980,960                                             
  940 iesc=1                                                            
      go to 980                                                         
  960 iesc=2                                                            
c
c     DETERMINE FAULT BUS                                       
  980 if (ibcd(ili).eq.2) go to 1020                                    
 1000 imat6=1                                                           
      go to 1040                                                        
 1020 imat6=2                                                           
      go to 1060                                                        
 1040 ib1=ib                                                            
      jb1=jb                                                            
      kfi=ifi                                                           
      kfj=ifj                                                           
      go to 1080                                                        
 1060 ib1=jb                                                            
      jb1=ib                                                            
      kfi=ifj                                                           
      kfj=ifi                                                           
c
c     TRANSPOSE SW                                              
 1080 go to (1120,1100), lsrch2                                         
 1100 if (ib.gt.0.and.jb.gt.0) go to 1760                               
      go to 1680                                                        
 1120 if (ib1.lt.0) go to 1180                                          
      if (jb1.lt.0) go to 1240                                          
      if (fsq.gt.0.0) go to 1160                                        
      if (mflt(ils).le.0) go to 1140                                    
      iesc=3                                                            
C                                                                   
C     YISOLN, NEWTON OPTION                                                
C                                                                   
C     IDENTIFY FLT BUS                                                    
      if (ifltsw .eq. 2 ) ifltb = ib1                                                      
 1140 go to 1760                                                        
 1160 gsr(kfi)=gsr(kfi)-gf                                              
      bsr(kfi)=bsr(kfi)-bf                                              
      if (mde.ne.3) go to 1140                                          
      fltr(ils)=0.0                                                     
      fltx(ils)=0.0                                                     
      mflt(ils)=-3                                                      
      go to 1140                                                        
 1180 if (jb1.lt.0) go to 1220                                          
      if (mde.eq.1) go to 1200                                          
      imat1=3                                                           
      go to 1260                                                        
 1200 imat1=2                                                           
      go to 1260                                                        
 1220 imat1=4                                                           
      go to 1260                                                        
 1240 imat1=1                                                           
c
c     STORE J LEG MOD                                           

 1260 continue                                                          
      gkfj=gijs(ifl)+gsl(kfj)                                           
      bkfj=bijs(ifl)+bsl(kfj)                                           
c
c     STORE I LEG MOD                                           

      gkfi=gijs(ifl)+gsl(kfi)                                           
      bkfi=bijs(ifl)+bsl(kfi)                                           
      if (imat1.eq.1) go to 1280                                        
      go to 1380                                                        
c
c     FLT IMPED TEST                                            

 1280 if (fsq.lt.0.0) go to 1360                                        
c
c     STORE I LEG MOD                                           

 1300 sq2=gsl(kfj)*gsl(kfj)+bsl(kfj)*bsl(kfj)                           
      if (sq2.ne.0.0) go to 1320                                        
      g2=0.                                                             
      b2=0.                                                             
      go to 1340                                                        
 1320 sq1=gijs(ifl)*gijs(ifl)+bijs(ifl)*bijs(ifl)                       
      r1=gijs(ifl)/sq1                                                  
      x1=-bijs(ifl)/sq1                                                 
      r2=r1+gsl(kfj)/sq2                                                
      x2=x1-bsl(kfj)/sq2                                                
      sq2=r2*r2+x2*x2                                                   
      g2=-r2/sq2                                                        
      b2=x2/sq2                                                         
 1340 gkfi=g2-gf-gsl(kfi)+gkfi                                          
      bkfi=b2-bf-bsl(kfi)+bkfi                                          
      go to 1620                                                        
c
c     S CKT  TEST                                              

 1360 if (mde1.le.0) go to 1300                                         
      iesc=3                                                            
      go to 1620                                                        
 1380 if (imat1.eq.4) go to 1540                                        
      if (fsq.lt.0.0) go to 1480                                        
      if (imat1.eq.3) go to 1460                                        
      g1=gsl(kfi)+gf                                                    
      b1=bsl(kfi)+bf                                                    
 1400 sq2=g1*g1+b1*b1                                                   
 1420 sq1=gijs(ifl)*gijs(ifl)+bijs(ifl)*bijs(ifl)                       
      r1=gijs(ifl)/sq1+g1/sq2                                           
      x1=-bijs(ifl)/sq1-b1/sq2                                          
      sq2=r1*r1+x1*x1                                                   
      g2=r1/sq2                                                         
      b2=x1/sq2                                                         
 1440 gkfj=gkfj-g2-gsl(kfj)                                             
      bkfj=bkfj+b2-bsl(kfj)                                             
      if (imat1.eq.2) go to 1500                                        
      go to 1520                                                        
 1460 g1=gsl(kfi)                                                       
      b1=bsl(kfi)                                                       
      if (b1.ne.0.0) go to 1400                                         
      if (g1.ne.0.0) go to 1400                                         
      g2=0.0                                                            
      b2=0.0                                                            
      go to 1440                                                        
 1480 if (mde1.le.0) go to 1460                                         
      if (imat1.eq.3) go to 1460                                        
      sq2=1.0                                                           
      g1=0.0                                                            
      b1=0.0                                                            
      go to 1420                                                        
 1500 gopl=g2+gsl(kfj)                                                  
      bopl=-b2+bsl(kfj)                                                 
      go to 1620                                                        
 1520 if (imat1.eq.3) go to 1580                                        
      go to 1620                                                        
 1540 if (mde1.eq.1) go to 1560                                         
      go to 1580                                                        
 1560 iesc=1                                                            
      go to 1620                                                        
 1580 if (mde1.le.0) go to 1620                                         
      if (fsq.lt.0.0) go to 1600                                        
c
c     S. C. IMPEDANCE                                           

      gkfi=gkfi-gf                                                      
      bkfi=bkfi-bf                                                      
      go to 1620                                                        
 1600 if (mde.eq.1) go to 1620                                          
      iesc=3                                                            
 1620 continue                                                          
      go to (1640,1660), imat6                                          
 1640 gsr(ifi)=gsr(ifi)+gkfi                                            
      bsr(ifi)=bsr(ifi)+bkfi                                            
      gsr(ifj)=gsr(ifj)+gkfj                                            
      bsr(ifj)=bsr(ifj)+bkfj                                            
      go to 1720                                                        
 1660 gsr(ifi)=gsr(ifi)+gkfj                                            
      bsr(ifi)=bsr(ifi)+bkfj                                            
      gsr(ifj)=gsr(ifj)+gkfi                                            
      bsr(ifj)=bsr(ifj)+bkfi                                            
      go to 1720                                                        
c
c     MODIFY LEFT SIDE OF MATRIX                                

 1680 gil(ijl)=gil(ijl)+gijs(ifl)                                       
      bil(ijl)=bil(ijl)+bijs(ifl)                                       
      gijh(ifl)=-gijs(ifl)                                              
      bijh(ifl)=-bijs(ifl)                                              
C                                                                       
      if (abs(gil(ijl)).gt.small) go to 1700                            
      gijh(ifl)=gijh(ifl)+gil(ijl)                                      
      gil(ijl)=0.0                                                      
 1700 if (abs(bil(ijl)).gt.small) go to 1760                            
      bijh(ifl)=bijh(ifl)+bil(ijl)                                      
      bil(ijl)=0.0                                                      
      go to 1760                                                        
c
c     MODIFY RIGHT SIDE OF MATRIX                               

 1720 gih(ijl)=gih(ijl)+gijs(ifl)                                       
      bih(ijl)=bih(ijl)+bijs(ifl)                                       
C                                                                       
      if (abs(gih(ijl)).gt.small) go to 1740                            
      gih(ijl)=0.0                                                      
 1740 if (abs(bih(ijl)).gt.small) go to 1760                            
      bih(ijl)=0.0                                                      
      go to 1760                                                        
 1760 gih(idi)=gih(idi)-gsr(ifi)                                        
      bih(idi)=bih(idi)-bsr(ifi)                                        
      grz(ifi)=gsr(ifi)                                                 
      brz(ifi)=bsr(ifi)                                                 
      ido=lst                                                           
      ido=2*lst-1                                                       
      gsr(ifi)=0.0                                                      
      bsr(ifi)=0.0                                                      
C                                                                       
      if (abs(gih(idi)).gt.small) go to 1780                            
      grz(ifi)=grz(ifi)+gih(idi)                                        
      gih(idi)=0.0                                                      
 1780 if (abs(bih(idi)).gt.small) go to 1800                            
      brz(ifi)=brz(ifi)+bih(idi)                                        
      bih(idi)=0.0                                                      
 1800 ib=isortn(1,itly+1)                                               
      if (ib.eq.iba) go to 1820                                         
      imat7=1                                                           
      if (mde.eq.4) go to 1940                                          
      if (mde.gt.6) go to 1940                                           
      go to 1840                                                        
 1820 imat7=2                                                           
      go to 2000                                                        
c
c     YMATRIX MOD COMPLETE                                      

 1840 indx=4                                                            
      if (idell.eq.0) go to 1880                                        
      do 1860 ind=1,idell                                               
      matrow(indx)=locl(ind)                                            
      atrow(indx+1)=gil(ind)                                            
      atrow(indx+2)=bil(ind)                                            
 1860 indx=indx+3                                                       
      ind=idell + 1                                                     
      atrow(indx+1)=gih(ind)                                            
      atrow(indx+2)=bih(ind)                                            
 1880 do 1900 ind=1,idelh                                               
      matrow(indx)=loch(ind)                                            
      atrow(indx+1)=gih(ind)                                            
      atrow(indx+2)=bih(ind)                                            
 1900 indx=indx+3                                                       
      call putmat(inn, ii)                                                  
      if (inn.lt.lfrst) lfrst=inn                                       
      if (keybrd(30) .ne. 0) then                                        
         lll = min0 (4,idell)                                           
         mmm = max0 (4-lll,0)                                           
         mmm = min0 (mmm,idelh)                                         
         write (outbuf,1920) inn,idell,idelh,(locl(i),gil(i),bil(i),    
     1   i=1,lll),(loch(i),gih(i),bih(i),i=1,mmm)                       
         call prtout (1)                                                
 1920    format (' IROW= ',i4,' LENS= ',i3,' LOCLEN= ',i3,4(i5,2f10.4)) 
         do 1922 jjj = lll+1,idell,4                                    
         kkk = min0 (jjj+3,idell)                                       
         write (outbuf,1921) (locl(i),gil(i),bil(i),i=jjj,kkk)          
         call prtout (1)                                                
 1922    continue                                                       
         do 1923 jjj = mmm+1,idelh,4                                    
         kkk = min0 (jjj+3,idelh)                                       
         write (outbuf,1921) (loch(i),gih(i),bih(i),i=jjj,kkk)          
         call prtout (1)                                                
 1923    continue                                                       
 1921    format (30x,4(i7,2f9.4))                                       
      endif                                                             
 1940 go to (1960,3200), imat3                                          
c
c     TALLY SWITCHING INDEX AND                                 

 1960 itly=itly+1                                                       
      go to 2020                                                        
 2000 itly=itly+1                                                       
      if (itly.gt.iss) go to 1840                                       
      go to 2040                                                        
 2020 if (itly.gt.iss) go to 3200                                       
 2040 iba=isortn(1,itly)                                              
      jba=isortn(2,itly)                                              
      ipcdn = isortn(3, itly)                                       
      ipcdc = isortc(itly)                                          
      ili=isorti(itly)                                                  
      ils=locst(ili)                                                    
      mde1=mflt(ils)                                                    
      mde=iabs(mde1)                                                    
c
c     SET NETWORK REDUCTION FLAG                                     

      if (mde.lt.5) nordk = 1                                            
      ibl=iba                                                           
c                                                                      
c     LOAD/GENERATION MODIFICATION                                    
c                                                                      
      if (mde .eq. 4) then                                              
         i840=2                                                         
         go to 2200                                                     
c                                                                      
c     TWO TERMINAL DC LINE SWITCHING                                  
c                                                                      
      else if (mde .eq. 5) then                                              
         i30=2                                                          
         go to 200                                                      
c                                                                      
c     DC OFFSET GENERATOR FAULT DAMPING                               
c                                                                      
      else if (mde .eq. 7) then                                               
         i1450 = 2                                                      
         go to 2700                                                     
c                                                                      
c     GENERATOR FAST VALVING                                          
c                                                                      
      else if (mde .eq. 8) then                                               
         i1550 = 2                                                      
         go to 3000                                                     
c                                                                      
c     MULTI TERMINAL DC SWITCHING                                     
c                                                                      
      else if (mde .eq. 6) then                                                
         i440 = 2                                                       
         go to 4000                                                     
c                                                                      
c     MANUAL TRIP OF TRANSIENT STABILIZER OR SVS FREEZE               
c                                                                      
      else if (mde .eq. 9) then                                                
         imde9 = 2                                                      
         go to 3145                                                     
      endif                                                             
 2098 go to (2100,540),imat7                                            
 2100 inn=iba                                                           
      call getmat(iba, ii)                                                  
      idell=matrow(2)                                                   
      idelh=matrow(3)                                                   
      indx=4                                                            
      if (idell.eq.0) go to 2140                                        
      do 2120 ind=1,idell                                               
      locl(ind)=matrow(indx)                                            
      gil(ind)=atrow(indx+1)                                            
      bil(ind)=atrow(indx+2)                                            
 2120 indx=indx+3                                                       
      go to 2160                                                        
 2140 gil(1)=0.0                                                      
      bil(1)=0.0                                                      
      locl(1)=0                                                         
 2160 do 2180 ind=1,idelh                                               
      loch(ind)=matrow(indx)                                            
      gih(ind)=atrow(indx+1)                                            
      bih(ind)=atrow(indx+2)                                            
 2180 indx=indx+3                                                       
      go to (540,1840), imat3                                           
C                                                                       
C     THE FOLLOWING LOGIC MODIFIES BUS LOADS IN ACCORDANCE            
C     WITH LOAD REPRESENTATION CARDS                                 *
C                                                                       
c     -  Here if mode == 4
 2200 continue
      if (debug) then                                                   !dem
        call dbgeko ('MATMOD - processing load or gen mod')             !dem
      endif                                                             !dem
      ildrp=0                                                           
      igl=iftab(ils)                                                    
      name = bname(igl)                                                 
      bkv = buskv(igl)                                                  
      ptot=0.0                                                        
      qtot=0.0                                                        
      iecsl = ldidxn(6,igl)                                           
      nitem = ldidxn(5,igl)                                           
      ityp4 = ldidxn(1,igl)                                             
      nitem=nitem-1                                                     
c
c     TEST FOR CONSTANT CURRENT MODIFICATION                         *

      cp=dmcln(1,ils)                                                 
      cq=dmcln(2,ils)                                                 
      if (debug) then                                                   !dem
        call dbgwrf ('  CP /const I real chg/ = ',cp)                   !dem
        call dbgwrf ('  CQ /const I real chg/ = ',cq)                   !dem
      endif                                                             !dem
      if (cp.eq.0.0.and.cq.eq.0.0) go to 2260                            
      if (iecsl.eq.0) go to 2360                                        
      if (ityp4.eq.4) go to 2240                                        
      write (errbuf(1),2220) name,bkv                                   
      call prterr ('E',1)                                               
      call mpost('MATMOD')                                              
 2220 format('0',5x,'ERROR IN LOAD MODIFICATION CARD.  BUS ',a8,1x,f5.1,
     1' HAS NO CONSTANT CURRENT REPRESENTATION.  PROGRAM STOPPED.')     
      call erexit                                                       
 2240 ildrp=1                                                           
      iadr = iecsl+nitem                                              
      p2 = busldn(1,iadr)                                             
      q2 = busldn(2,iadr)                                             
      p2=p2-cp                                                          
      q2=q2-cq                                                          
      busldn(1,iadr) = p2                                             
      busldn(2,iadr) = q2                                             
      ptot=ptot+cp                                                      
      qtot=qtot+cq                                                      
c
c     TEST FOR CONSTANT P MODIFICATION                                

 2260 pp=dmpln(1,ils)                                               
      pq=dmpln(2,ils)                                               
      if (debug) then                                                   !dem
        call dbgwrf ('  PP /const P real chg/ = ',pp)                   !dem
        call dbgwrf ('  PQ /const P real chg/ = ',pq)                   !dem
      endif                                                             !dem
      if (ityp4 .eq. 4)nitem = nitem - 1                                 
      if (pp.eq.0.0.and.pq.eq.0.0) go to 2320                            
      if (iecsl.eq.0) go to 2360                                         
      ityp3 = ldidxn(2,igl)                                             
      if (ityp3.eq.3) go to 2300                                        
      write (errbuf(1),2280) name,bkv                                   
      call prterr ('E',1)                                               
      call mpost('MATMOD')                                              
 2280 format('0',5x,'ERROR IN LOAD MODIFICATION CARD.  BUS ',a8,1x,f5.1 
     1, ' HAS NO CONSTANT POWER REPRESENTATION.  PROGRAM STOPPED.')     
      call erexit                                                       
 2300 ildrp=1                                                           
      iadr = iecsl + nitem                                            
      p3 = busldn(1,iadr)                                             
      q3 = busldn(2,iadr)                                             
      q3=q3-pq                                                          
      p3=p3-pp                                                          
      busldn(1,iadr) = p3                                             
      busldn(2,iadr) = q3                                             
      ptot=ptot+pp                                                      
      qtot=qtot+pq                                                      
c
c     TEST FOR CONSTANT Z MODIFICATION                                

 2320 continue

      zp=dmzg(ils)                                                      
      zq=dmzb(ils)                                                      
      if (debug) then                                                   !dem
        call dbgwrf ('  ZP /const Z real chg/ = ',zp)                   !dem
        call dbgwrf ('  ZQ /const Z real chg/ = ',zq)                   !dem
      endif                                                             !dem
      if (dmzg(ils).eq.0.0.and.dmzb(ils).eq.0.0) go to 2440             
      if (ipwr.eq.1) go to 2400                                         
      if (iecsl.eq.0) go to 2400                                       
      if (idgnc(ils) .eq. 'S') go to 2400                                 
      ityp1 = ldidxn(4,igl)                                             
      if (ityp1.eq.1) go to 2400                                        
      write (errbuf(1),2340) name, bkv                                  
      call prterr ('E',1)                                               
      call mpost('MATMOD')                                              
 2340 format('0',5x,'ERROR IN LOAD MODIFICATION CARD.  BUS ',a8,1x,f5.1,
     1' HAS NO CONSTANT Z REPRESENTATION.  PROGRAM STOPPED.')           
      call erexit                                                       
 2360 write (errbuf(1),2380) name,bkv                                   
      call prterr ('E',1)                                               
      call mpost('MATMOD')                                              
 2380 format('0',5x,'ERROR IN LOAD MODIFICATION CARD.  BUS ',a8,1x,f5.1)
      call erexit                                                       
 2400 ildrp=1                                                           
      zp=dmzg(ils)                                                      
      zq=dmzb(ils)                                                      
      if (ipwr.eq.1) go to 2420                                         
      if (iecsl.eq.0) go to 2420                                        
      p1 = busldn(1,iecsl)                                              
      q1 = busldn(2,iecsl)                                              
      p1=p1-zp                                                          
      q1=q1-zq                                                          
      busldn(1,iecsl) = p1                                              
      busldn(1,iecsl) = q1                                              
 2420 ptot=ptot+zp                                                      
      qtot=qtot+zq                                                      
 2440 if (ildrp.eq.0) go to 2480                                        
      call getmat(igl, ii)                                                
      gkk=atrow(ii-1)                                                   
      bkk=atrow(ii)                                                     
      emag = emagrn(1,igl)                                              
      eang = emagrn(2,igl)                                              
      vsq=emag*emag                                                     
      yreal(igl) = yreal(igl) + ptot*vsq                                
      yimag(igl) = yimag(igl) - qtot*vsq                                
C                                                                     
C     YISOLN, NEWTON OPTION                                                

      go to ( 2450,2445), inewts                                        
c
c     UPDATE LOAD ADMITTANCE TABLES                                      

 2445 gadmit(igl) = gadmit(igl) + ptot *  vsq                           
      badmit(igl) = badmit(igl) - qtot *  vsq                           
      go to 2455                                                        
 2450 gkk=gkk+ptot*vsq                                                  
      bkk=bkk-qtot*vsq                                                  
      atrow(ii)=bkk                                                     
      atrow(ii-1)=gkk                                                   
      call putmat(igl, ii)                                                
 2455 if (igl.lt.lfrst) lfrst=igl                                       
      write (outbuf,2460)name,bkv,ptot,qtot,to                          
 2460 format('0', 5x, 'MANUAL LOAD MODIFICATION AT ', a8, 1x, f5.1,
     &   1x, f10.2, ' P(PU) ', 1x, f10.2, ' Q(PU) ', f7.2, ' CYCLES')
      call prtout (1)                                                   
 2480 continue                                                          
c
c     TEST FOR GEN MOD                                         

      if (dmpg(ils).eq.0.0) go to 2600                                  
c
c     SEARCH FOR IGEN(I) POINTER                               

      do 2500 i=1,isg                                                   
        igbn=igentn(1,i)                                                
        iecs=igentn(2,i)                                                
        idm=igentc(i)                                                   
        if (igbn.ne.igl) go to 2500                                      
        if (idm.eq.idgnc(ils)) go to 2540                                
 2500 continue                                                          
      write (errbuf(1),2520)                                            
      call prterr ('E',1)                                               
      call mpost('MATMOD')                                              
 2520 format ('0',5x,' ERROR IN GENERATOR MODIFICATION CARD')           
      call erexit                                                       
 2540 igenl=i                                                           
c
c     IGNORE LP M/C BUT STORE INDEX FOR LP M/C IF HP M/C IS 
c     ENCOUNTERED    

      if (idm.eq.'L') go to 2600                                       
      if (idm.ne.'H') go to 2620                                       
      do 2544 ij=1,isg                                                  
        if (igentc(ij).ne.'L') go to 2544                                
        if (igentn(1,ij).eq.igbn) go to 2554                             
 2544 continue                                                          
 2554 ilow=ij                                                           
 2620 call gendrop(igenl,dmpg(ils),igbn,iecs,idm)                      
      if (igl.lt.lfrst) lfrst=igl                                        
 2600 go to(120,1800), i840                                             
C                                                                       
C     TEST FOR D.C. OFF-SET DAMPING                                     
C                                                                       
 2700 if (dmpg(ils).eq.0.0) go to 2790                                   
      ifvlt = iftab(ils)                                                
      do 2720 i=1,isg                                                   
        igbn=igentn(1,i)                                                
        iecs=igentn(2,i)                                                
        idm=igentc(i)                                                   
      if (igbn.ne.ifvlt) go to 2720                                      
      if (idm.eq.idgnc(ils)) go to 2760                                
 2720 continue                                                          
      write (errbuf(1),2740)                                            
      call prterr ('E',1)                                               
      call mpost('MATMOD')                                              
 2740 format('0',5x,'  ERROR IN D.C. OFF-SET DAMPING CARD ')            
      call erexit                                                       
 2760 do 2765 j=1,i7max                                                 
      if (i.ne.iflt(j)) go to 2765                                       
      dflt(j)=dmpg(ils)+dflt(j)                                         
      go to 2770                                                        
 2765 continue                                                          
 2770 write (outbuf,2780) dflt(j),i,to                                  
      call prtout (1)                                                   
 2780 format('0', 5x, '  D.C. OFF-SET DAMPING OF ', f8.3,
     &   '(P.U.) AT GENERATOR NO. ', i5, ' at ', f7.2,' CYCLES.')                              
 2790 go to(120,1800),i1450                                             
C                                                                       
C     TEST FOR FAST VALVING                                              
C                                                                       
 3000 ifvlt=iftab(ils)                                                  
      do 3020 i=1,isg                                                   
        igbn=igentn(1,i)                                                
        iecs=igentn(2,i)                                                
        idm=igentc(i)                                                   
      if (igbn.ne.ifvlt) go to 3020                                      
      if (idm.eq.idgnc(ils)) go to 3060                                
 3020 continue                                                          
      write (errbuf(1),3040)                                            
      call prterr ('E',1)                                               
      call mpost('MATMOD')                                              
 3040 format('0',5x,'   ERROR    SEE FAST VALVING CARD ')               
      call erexit                                                       
 3060 do 3080 j=1,i8max                                                 
      if (i.ne.ifvl(j)) go to 3080                                       
      dfvl(j)=dmpg(ils)                                                 
      go to 3100                                                        
 3080 continue                                                          
 3100 write (outbuf,3120) dfvl(j),i,to                                  
      call prtout (1)                                                   
 3120 format('0', 5x, '  FAST VALVING SLOPE OF ', f8.3,
     &  '(P.U.PWR/SEC) AT GENERATOR NO. ', i5, ' AT ', f7.2,' CYCLES. ')
 3140 go to(120,1800),i1550                                             
c                                                                      
c     MANUAL TRIP OF TRANSIENT STABILIZER                             
c                                                                      
 3145 if (ipcdtn(ils) .eq. 0) then                                        
         itslkt = jftab(ils)                                            
         tbase(itslkt) = to                                             
         itrig(itslkt) = 3                                              
         name = bname(iftab(ils))                                       
         bkv = buskv(iftab(ils))                                        
         idm = idgnc(ils)                                               
         write(outbuf,3150)name,bkv,idm,to                              
 3150    format('0 TRANSIENT STABILIZER ON MACHINE ', a8, 2x, f5.1, 2x,
     &     a1, ' MANUALLY TRIPPED AT ', f7.2, ' CYCLES, ')               
         call prtout(1)                                                 
         go to (120,1800)imde9                                          
      endif                                                             
c                                                                      
c     MANUAL SVS FREEZE                                               
c                                                                      
      if (ipcdtn(ils) .eq. 1) then                                        
         ksv = jftab(ils)                                               
         isvsfz(ksv) = 1                                                
         name = bname(iftab(ils))                                       
         bkv = buskv(iftab(ils))                                        
         idm = idgnc(ils)                                               
         write(outbuf,3175)name,bkv,idm,to                              
 3175    format('0 SVS OUTPUT ON MACHINE ', a8, 2x, f5.1, 2x, a1,            
     1          ' FROZEN AT ', f7.2, ' CYCLES, ')                         
         call prtout(1)                                                 
         go to (120,1800)imde9                                          
      endif                                                             
 3200 if (nbrake.eq.0) go to 3300                                        
C                                                                       
C     OBTAIN TRANSFER ADMITTANCE FOR RES. BRK. CONTROL                    
C                                                                       
      do 3280 i3280 = 1, nbrake                                         
      irc=0                                                             
      ibusr = ibkib1(i3280)                                             
      jbusr = ibkjb1(i3280)                                             
 3210 call getmat(ibusr, ii)                                                
      lens = matrow(2)                                                  
      loclen = matrow(3)                                                
      indx = 4                                                          
      if (lens.eq.0) go to 3230                                          
      do 3220 i = 1,lens                                                
      if (matrow(indx).eq.jbusr) go to 3260                              
 3220 indx = indx + 3                                                   
 3230 do 3240 i = 1,loclen                                              
      if (matrow(indx).eq.jbusr) go to 3260                              
 3240 indx = indx + 3                                                   
      write (errbuf(1),3250)                                            
      call prterr ('E',1)                                               
      call mpost('MATMOD')                                              
 3250 format('0RES. BRK. CONTROL BRANCH NOT FOUND ')                    
      call erexit                                                       
c                                                                      
c     STORE BRANCH ADMITTANCE FOR RES. BRK. CONTROL BRANCH            
c                                                                      
 3260 if (irc.eq.1) go to 3270                                           
      bkgij1(i3280) = -atrow(indx+1)                                    
      bkbij1(i3280) = -atrow(indx+2)                                    
      if (ibrkc(i3280) .eq. 0) go to 3280                                
      irc = 1                                                           
      ibusr = ibkib2(i3280)                                             
      jbusr = ibkjb2(i3280)                                             
      go to 3210                                                        
 3270 bkgij2(i3280) = -atrow(indx+1)                                    
      bkbij2(i3280) = -atrow(indx+2)                                    
 3280 continue                                                          
 3300 if (ldc.eq.0) go to 3400                                           
      i1 = 0                                                            
      do 3390 i = 1, ldc                                                
      k1 = kdc + idcl * i1                                              
      call redecs(itab(114),k1+158,7)                                   
      if (itab(114) .lt. 1) go to 3380                                   
      if (itab(114) .eq. 5) go to 3380                                   
      if (itab(114) .eq. 6) go to 3380                                   
      imodkt = itab(118)                                                
      ibuspw = idcbs1(imodkt)                                           
      jbuspw = jdcbs1(imodkt)                                           
      call getmat(ibuspw, ii)                                               
      lens = matrow(2)                                                  
      loclen = matrow(3)                                                
      indx = 4                                                          
      if (lens.eq.0) go to 3320                                          
      do 3310 ilens = 1, lens                                           
      if (matrow(indx).eq.jbuspw) go to 3350                             
 3310 indx = indx + 3                                                   
 3320 do 3330 ilocln = 1, loclen                                        
      if (matrow(indx).eq.jbuspw) go to 3350                             
 3330 indx = indx + 3                                                   
      write (errbuf(1),3340)                                            
      call prterr ('E',1)                                               
      call mpost('MATMOD')                                              
 3340 format( '0',' ... SOURCE BRANCH FOR DC MODULATION SIGNAL NOT',    
     1' FOUND')                                                         
      call erexit                                                       
c
c     CHANGE TRANSFER ADMITTANCE IN DC MOD SIGNAL SOURCE BRANCH           

 3350 dcgij(imodkt) = -atrow(indx+1)                                    
      dcbij(imodkt) = -atrow(indx+2)                                    
 3380 i1 = i1 + 1                                                       
 3390 continue                                                          
 3400 return                                                            
      end                                                               
