C    %W% %G%
       subroutine mdclin(iter)                                         
C * * *                                                                 
C * * * THIS SUBROUTINE CALCULATES THE DC LINE EQUATIONS FOR MULTITERMIN
C * * * LINES; CALLS MDCREG TO SOLVE THE CURRENT REGULATOR EQUATIONS;   
C * * * AND THEN SOLVES THE AC/DC INTERFACE EQUATIONS IT IS CALLED      
C * * * BY DERIV.                                                       
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/blkcom2.inc' 
      include 'tspinc/contrl.inc' 
      include 'tspinc/cntrl2.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/ectba.inc' 
      include 'tspinc/mdctbl.inc' 
      include 'tspinc/mdcfil.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/newton.inc' 
      include 'tspinc/fltopt.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/rk.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/gamma.inc' 
      include 'tspinc/dcmodd.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/bcur.inc' 

      common /toler/ divn,delang,twodt                                
      common /ecstbj/ angl(7200)                                      
      common /netwk/ imax,cmax,dsum                                   
                                                                        
      dimension drv(14)                                                
      equivalence (drv,tmpdy)                                          
C                                                                       
      equivalence (tmpy(1), nsubt), (tmpy(2), nstmx), (tmpy(3),     
     1             timh), (tmpy(4), timl), (tmpy(5), noith),        
     2            (tmpy(6), noitl), (tmpy(7), loopdc)               
      equivalence (itab(106),mdcl),(itab(105),mdcflt)                   
C ..EC DELAY...                                                         
      equivalence (erio, tab(164)),(eroo,tab(165)),(eron,tab(166)),     
     1            (eiio,tab(167)),(eioo,tab(168)),(eion,tab(169)),      
     2            (ectim1,tab(148)),(ectim2,tab(149))                   
C     TIM AND NOIT SELECT PRINTOUT INTERVALS                            
          n10 = 10                                                      
      timh=500.                                                         
      timl=0.0                                                          
      noith=500                                                         
      noitl=0                                                           
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 1536                                             
      if (keybrd(22).ne.0) then                                         
         call skipln (3)                                                
         write (outbuf,1545) lppwr,idsw,iopen,ipass                     
         call prtout (1)                                                
      endif                                                             
 1545     format(' ENTERING MDCLIN:LPPWR,IDSW,IOPEN,IPASS,ITER',        
     1    5i7)                                                          
 1536 continue                                                          
      ind = 0                                                           
 1539 ind = ind + 1                                                     
      if(ind.gt.nterm) go to 1540                                       
      ind1=lbt+ind                                                      
      call redecs (dca,idcb(ind1),msizea)                               
C * * *                                                                 
C * * * CALL MODULATION SOLUTION SUBROUTINES IF MODULATION EXISTS       
C * * *                                                                 
      modcod = idca(78)                                                 
      if(modcod .eq. 0) go to 1539                                      
      imod = idca(79)                                                   
C * * *                                                                 
C * * * GAMMA MODULATION                                                
C * * *                                                                 
      if(modcod .eq. 5)then                                             
C * * *                                                                 
C * * * GET COMMUTATION VOLTAGE                                         
C * * *                                                                 
         ec = eyr(iec)                                                  
         fc = eyi(iec)                                                  
         ecsq1= ec*ec + fc*fc                                           
         ecn = sqrt(ecsq1)* txbse                                       
         if(ecn .eq.0.0)ecn =0.0001                                     
         ev = eyr(iev)                                                  
         fv = eyi(iev)                                                  
         evsq1= ev*ev + fv*fv                                           
         if(evsq1.lt.0.0001)ecn=0.0001                                  
         if(igamrb(imod) .ne. 0)then                                    
            ibus = igamrb(imod)                                         
            erem = eyr(ibus)                                            
            frem = eyi(ibus)                                            
            vac(imod) = sqrt(erem*erem + frem*frem)                     
         else                                                           
            vac(imod) = sqrt(ecsq1)                                     
         endif                                                          
         if(igamdc(imod) .ne. 0)then                                    
            vac(imod) = edcan                                           
         endif                                                          
         call gamsol(imod)                                              
      else                                                              
C * * *                                                                 
C * * * HIGH, LOW, AND DUAL FREQUENCY MODULATION                        
C * * *                                                                 
         call dcmod(imod,modcod,iter)                                   
      endif                                                             
      go to 1539                                                        
 1540 if( lppwr .eq. 0 .and. iter .eq. 1)return                         
      n100 = 100                                                        
      n110 = 110                                                        
C  ESTABLISH NBUS RELATED DIMENSIONS                                    
       nbus2= 2*nbus                                                    
      nbussq = 10*nbus                                                  
       nbusp1= nbus+1                                                   
       nbusp2= nbus+2                                                   
       nbusm1=nbus-1                                                    
       mcp=1                                                            
      ilinv=0                                                           
      ipass = 1                                                         
          nstmx = 3                                                     
      idone = 1                                                         
      idt = 1                                                           
      mstat = 2                                                         
          nsubt = 1                                                     
          opnckt = 134217728.                                           
C                                                                       
C SET FLT. BKR. TRIP CODE IF REQD.                                      
C                                                                       
          kfltl = kflt                                                  
          kflty = kflt                                                  
          ktrip = 0                                                     
          ihist = 0                                                     
          if(nflt .ne. 999) go to 1537                                  
C                                                                       
C REMOVE FAULT FROM Y-MATRIX ONLY...FAULT REMOVED FROM L-1 MTRX         
C AT START OF THE 2ND SUBTIMESTEP                                       
C                                                                       
          if(idsw .ne. 7) kflty = 0                                     
          ktrip = 1                                                     
          ihist = 1                                                     
 1537     continue                                                      
C     READ VALUE STATUS INDICATORS FROM ECS                             
      call redecs (idcf,necsf,nterm)                                    
      call redecs (idcf1,necsf+nterm,nterm)                             
          call recn(ibrtbl, 2, necsm, 10)                               
C      ARE WE AT A DISCONTINUITY )                                      
      if(idsw.ne.7)go to 1530                                           
C     IF FAULT REMOVAL IS REQUESTED AND RECTFIERS AREN'T BLOCKED ABORT. 
      iblk=0                                                            
      do 1542 i=1,nterm                                                 
      call redecs(cdcn,idcb(lbt+i)+61,1)                                
      if(abs(cdcn).gt.1.e-4)iblk=1                                      
 1542 continue                                                          
      if(iblk.ne.1.or.kflt.ge.0)go to 1530                              
          if(nflt .eq. -999) go to 1530                                 
      ibs=-kflt                                                         
      write (errbuf(1),1543)ibs                                         
      call prterr ('E',1)                                               
 1543     format(' SIMULATION HALTED BECAUSE THE FAULT AT DC BUS',i3,' W
     1as specified to be removed before blocking occurred .')           
      call erexit                                                       
 1530     if(kflt .ge. 0) go to 1532                                    
          kflt = 0                                                      
          kfltl = kflt                                                  
          kflty = kflt                                                  
 1532     continue                                                      
C                                                                       
C     PRINT OUT LS FLAG INDICATORS FOR DEBUGGING                        
C                                                                       
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 1535                                             
      if(keybrd(22).eq.0)go to 1535                                     
          write (outbuf, 1549)                                          
          call prtout (1)                                               
          do 31549 i = 1,10                                             
             write (outbuf,11549) ibrtbl(1, i), ibrtbl(2, i)            
             call prtout (1)                                            
31549     continue                                                      
          call skipln (1)                                               
          write (outbuf,21549) idcnt, idcnt1, lsflg, kflt, irecl        
          call prtout (1)                                               
 1549     format(' IBRTBL(1, I) IBRTBL(2, I)   IDCNT  IDCNT1  LSFLG',   
     1    '  KFLT  IRECL')                                              
11549     format(2x, i11, 1x, i11)                                      
21549     format(4x, i4, 4x, i4, 3x, i4, 2x, i4, 3x, i4)                
      write (outbuf,1547)                                               
      call prtout (1)                                                   
      do 21547 jjj = 1,nterm,10                                         
         kkk = min0 (jjj+9,nterm)                                       
         write (outbuf,11547) (idcf(i),i=jjj,kkk)                       
         call prtout (1)                                                
21547 continue                                                          
 1547     format(' IDCF AT SI547')                                      
11547     format(10(i4,2x))                                             
      write (outbuf,1548)                                               
      call prtout (1)                                                   
      do 21548 jjj = 1,nterm,10                                         
         kkk = min0 (jjj+9,nterm)                                       
         write (outbuf,11548) (idcf1(i),i=jjj,kkk)                      
         call prtout (1)                                                
21548 continue                                                          
 1548     format(' IDCF1 AT S1548')                                     
11548     format(10(i4, 2x))                                            
 1535 continue                                                          
      if(idsw.ne.7)go to 1550                                           
C                                                                       
C     WRITE AT AN AC DISCONTINUITY                                      
      mcp=2                                                             
      go to 1655                                                        
C     TEST FOR NEW TIMESTEP                                             
 1550 if(lppwr.eq.1) go to 1552                                         
C                                                                       
C     NOT A NEW TIME-STEP                                               
C                                                                       
      ipass=2                                                           
      if(idcnt.ne.idcnt1)mstat=1                                        
C     SET DC DISCONTINUITY INDICATOR                                    
      idcnt=idcnt1                                                      
C                                                                       
C     SET VALVE STATUS INDICATOR                                        
C                                                                       
      do 1551 i=1,nterm                                                 
C     IF A VALVE WAS SUCCESSFULLY RECLOSED 1ST ITER. LEAVE CLOSED .     
      if(idcf(i).eq.1.and.idcf1(i).eq.3)go to 1551                      
      idcf(i)=idcf1(i)                                                  
 1551 continue                                                          
      go to 1557                                                        
C                                                                       
C     NEW TIME-STEP                                                     
C                                                                       
C                                                                       
C     PRECESS VALVE STATUS INDICATOR                                    
C                                                                       
C                                                                       
C TEST AND SET CONDITIONS FOR FLT. BKR. TRIP                            
C                                                                       
 1552     if(ktrip .eq. 0) go to 1560                                   
          idt = 2                                                       
          mstat = 3                                                     
 1560     do 1554  i = 1, nterm                                         
 1554 idcf1(i)=idcf(i)                                                  
C                                                                       
C     TRY TO CLOSE ALL OPEN VALVES THAT ARE NOT BLOCKED .               
C                                                                       
      do 1553 i=1,nterm                                                 
      if(idcf(i).ne.3.and.idcf(i).ne.0)go to 1553                       
      idcf(i)=1                                                         
      lsflg=2                                                           
 1553 continue                                                          
C                                                                       
C     PRECESS DC DISCONTINUITY FLAGS                                    
C                                                                       
      if(lsflg.ne.1) go to 1555                                         
      if(idcnt.ne.idcnt1) go to 1555                                    
      idcnt=1                                                           
      idcnt1=1                                                          
      go to 1556                                                        
 1555 idcnt=2                                                           
      idcnt1=2                                                          
C     SET UP TO REFACTORIZE YMTRX                                       
      mstat=1                                                           
 1556 lsflg=1                                                           
C                                                                       
C     CHECK FOR LINE OPENING OR RECLOSURE                               
      if(irecl.eq.2)go to 1558                                          
C                                                                       
C     CHECK FOR TIMESTEP SIZE CHANGE                                    
C                                                                       
      if(edt.eq.ddt2)go to 1557                                         
 1558 idt=2                                                             
      irecl=1                                                           
C     SETUP TO RECOMPUTE THE YMTRX                                      
      mstat=3                                                           
C                                                                       
C     TEST FOR DC DISCONTINUITY                                         
C                                                                       
 1557 if(idcnt.eq.1)go to 1780                                          
C1                                                                      
C SETUP PROPER L-1 MATRIX                                               
 1655 continue                                                          
C                                                                       
C     PRINT OUT LS FLAG INDICATORS FOR DEBUGGING                        
C                                                                       
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 1643                                             
      if(keybrd(22).eq.0)go to 1643                                     
          write (outbuf, 1640)                                          
          call prtout (1)                                               
          do 31640 i = 1,10                                             
              write (outbuf,11640) ibrtbl(1, i), ibrtbl(2, i)           
              call prtout (1)                                           
31640     continue                                                      
          write (outbuf,21640) idcnt, idcnt1, lsflg, kflt, irecl        
          call prtout (1)                                               
 1640     format(' IBRTBL(1, I) IBRTBL(2, I)   IDCNT  IDCNT1  LSFLG',   
     1    '  KFLT  IRECL')                                              
11640     format(2x, i11, 1x, i11)                                      
21640     format(4x, i4, 4x,  i4, 3x, i4, 2x, i4, 3x, i4)               
      write (outbuf,1641)                                               
      call prtout (1)                                                   
      do 21641 jjj = 1,nterm,10                                         
         kkk = min0 (jjj+9,nterm)                                       
         write (outbuf,11641) (idcf(i),i=jjj,kkk)                       
         call prtout (1)                                                
21641 continue                                                          
 1641     format(' IDCF AT S1641')                                      
11641     format(10(i4, 2x))                                            
      write (outbuf,1642)                                               
      call prtout (1)                                                   
      do 21642 jjj = 1,nterm,10                                         
         kkk = min0 (jjj+9,nterm)                                       
         write (outbuf,11642) (idcf1(i),i=jjj,kkk)                      
         call prtout (1)                                                
21642 continue                                                          
 1642     format(' IDCF1 AT S1642')                                     
11642     format(10(i4, 2x))                                            
 1643 continue                                                          
C     ZERO OUT THE L -1 MATRIX                                          
      do 1658 i=1,nbus                                                  
      do 1657 j=1,nbusp1                                                
      dcg(i,j)=0.                                                       
 1657 continue                                                          
 1658 continue                                                          
C                *** ESTABLISH L -1 MATRIX AND FORCING FUNCTION ***     
C                                                                       
C                                                                       
C     SET POINTER FOR LOOPING THROUGH DCB TABLE                         
 1660 ind1=lbt+1+nterm                                                  
      lterm=0                                                           
C      GET BUS INFORMATION <                                            
 1661 ibb=-idcb(ind1)                                                   
C     TERMINAL ON BUS )                                                 
      if(ibb.le.10)go to 1665                                           
C     YES,A TERMINAL IS CONNECTED SO COMPUTE ACTUAL BUS <               
      ibb=ibb-10                                                        
C     INCREMENT TERMINAL COUNT AND READ TERMINAL DATA FROM ECS .        
      lterm=lterm+1                                                     
      call redecs(dca(31),idcb(lbt+lterm)+30,43)                        
C     CHECK FOR VALVE UNBLOCKING                                        
      if(idcf(lterm).ne.5)go to 1672                                    
C     UNBLOCK VALVE                                                     
      cosan=cosmin                                                      
      call ritecs(dca(63),idcb(lbt+lterm)+62,1)                         
      idcf(lterm)=1                                                     
C     STORE FIRING ANGLE FOR OUTPUT AT T+ .                             
 1672 cosout(lterm)=cosan                                               
      cf=csign*f135                                                     
C      IS THIS A DISCONTINUITY OR START OF A NEW TIME STEP )            
      if(idsw.ne.7)go to 1673                                           
C      YES SO WE MUST READ THE COMMUTATION VOLTAGE FROM ECS .           
      ec = eyr(iec)                                                     
      fc = eyi(iec)                                                     
      call redecs(txbse,idcb(lbt+lterm)+17,1)                           
      ecsq1=ec*ec+fc*fc                                                 
      ecn=sqrt(ecsq1)*txbse                                             
      if(ecn.eq.0.0)ecn=0.0001                                          
      ev = eyr(iev)                                                     
      fv = eyi(iev)                                                     
      evsq1=ev*ev+fv*fv                                                 
      if(evsq1.lt.0.0001)ecn=0.0001                                     
 1673 if(ipass.ne.1)go to 1662                                          
      efn=cf*ecn*cosan                                                  
C PRECESS THE TERMINAL CURRENT                                          
      crsto(ibb,ibb)=cdcn                                               
      currt(lterm)=cdcn                                                 
C IF START OF A REPEAT ITER. RESET CURRENT AND VOLTAGES                 
 1662 if(ipass.ne.2)go to 1663                                          
      efn=cf*eco*cosao                                                  
      crsto(ibb,ibb)=cdco                                               
      currt(lterm)=crsto(ibb,ibb)                                       
C     IF REPEATING A SUBTIMESTEP BECAUSE OF CURRENT REVERSAL USE VALUES 
C     FROM PREVIOUS STS.                                                
 1663 if(ipass.ne.3)go to 1664                                          
      currt(lterm)=crsto(ibb,ibb)                                       
      efn=cf*dcstor(1,lterm)*dcstor(10,lterm)                           
C     COMPUTE THE TERMINAL COMPONENT OF L -1 AND FORCING FUNCTION       
C     IS THIS TERMINALS VALVE SWITCH CLOSED )                           
 1664 if(idcf(lterm).eq.2.or.idcf(lterm).eq.3)go to 1665                
C     YES ,THE VALVE IS CLOSED SO INCLUDE IN L -1.                      
      dcg(ibb,ibb)=rll                                                  
      dcg(ibb,nbusp1)=efn*rll+vdrp+crsto(ibb,ibb)*rrll                  
C     INCREMENT POINTER IN DCB TABLE                                    
 1665 ind1=ind1+1                                                       
C     HAVE WE EXHAUSTED THE DCB TABLE                                   
 1666 if(ind1.gt.ndimc)go to 1671                                       
C     NO, ARE WE PROCESSING A NEW BUS)                                  
 1667 if(idcb(ind1).lt.0)go to 1661                                     
C     NO ,THIS IS A BRANCH.                                             
C     OBTAIN BUS < FOR THE OTHER END OF THE BRANCH                      
      jbb=idcb(ind1)                                                    
C     SET THE BRANCH CURRENT                                            
      if(ipass.eq.1)crsto(ibb,jbb)=dcb(ind1+5)                          
      if(ipass.eq.2)crsto(ibb,jbb)=dcb(ind1+6)                          
      bcurr1=crsto(ibb,jbb)                                             
C     BY-PASS OLD VERSION OF LINE TRIPPING                              
      if(0.eq.0)go to 1712                                              
c     -  Next ? statements never executed
C     CHECK BRANCH TABLE TO SEE IF THIS LINE HAS BEEN DISCONNECTED      
c     IF(IBRKNT.EQ.0)GO TO 1712                                         
c     DO 1710 I=1,IBRKNT                                                
c         IF(IBB .EQ. IBRTBL(1, I) .AND. JBB .EQ. IBRTBL(2, I))         
c    1    GO TO 1711                                                    
c         IF(JBB .EQ. IBRTBL(1, I) .AND. IBB .EQ. IBRTBL(2, I))         
c    1    GO TO 1711                                                    
C     OMIT DISCONNECTED BRANCH                                          
 1710 continue                                                          
 1712 continue                                                          
C                                                                       
C CK IF BKR TO BE OPENED ON A FAULT                                     
C                                                                       
          scale = 1.0                                                   
          if(ktrip .eq. 0) go to 1720                                   
          do 1715  i = 1, ibrknt                                        
      scale = opnckt                                                    
          if(ibb .eq. ibrtbl(1, i) .and. jbb .eq. ibrtbl(2, i))         
     1      go to 1720                                                  
          if(jbb .eq. ibrtbl(1, i) .and. ibb .eq. ibrtbl(2, i))         
     1      go to 1720                                                  
 1715     continue                                                      
C                                                                       
C     ADD BRANCH COMPONENT TO L -1 AND FORCING FUNCTION.                
C                                                                       
 1720 dcg(ibb,jbb) = -1.0/dcb(ind1+2)*scale                             
          dcg(ibb, ibb) = dcg(ibb, ibb) + (1.0/dcb(ind1 + 2)*scale)     
      dcg(ibb,nbusp1)=dcg(ibb,nbusp1)+bcurr1*dcb(ind1+1)/dcb(ind1+2)    
 1711 continue                                                          
C     TALLY DCB DUE TO BRANCH                                           
      ind1=ind1+8                                                       
      go to 1666                                                        
 1671 continue                                                          
C     PRINT OUT THE CURRENT MATRIX IF REQUESTED ON THE DEBUG CARD.      
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 1670                                             
      if(keybrd(22).eq.0)go to 1670                                     
      write (outbuf,1668)                                               
      call prtout (1)                                                   
      do 21668 i = 1,5                                                  
         write (outbuf,11668) (crsto(i,j),j=1,5)                        
         call prtout (1)                                                
21668 continue                                                          
 1668     format('0CRSTO MATRIX AT S1668')                              
11668     format(1x, 5e15.5)                                            
      write (outbuf,1669)                                               
      call prtout (1)                                                   
      write (outbuf,11669) (currt(i),i=1,10)                            
      call prtout (1)                                                   
 1669     format('0CURRT ARRAY AT S1669')                               
11669     format(1x, 10e13.5)                                           
 1670 continue                                                          
C SOLVE FOR DC VOLTAGE FOR DISCONTINUITY                                
C                                                                       
C FACTORIZE W/ FORCE FNC                                                
 1740 continue                                                          
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 1741                                             
      if (keybrd(22).ne.0) then                                         
          write (outbuf,1742)                                           
          call prtout (1)                                               
          do 21742 i = 1,10                                             
            write (outbuf,11742) (dcg(i,j),j=1,10)                      
            call prtout (1)                                             
21742     continue                                                      
      endif                                                             
 1742     format('0SI742 L-1 MATRIX DCG')                               
11742     format(1x, 10e13.5)                                           
 1741 continue                                                          
      do 1760 i=1,nbus                                                  
       if(i.eq.1) go to 1750                                            
      im = i - 1                                                        
      do 1747 j = 1, im                                                 
       if(dcg(j,i  ).eq.0.0) go to 1747                                 
C                                                                       
C     CHECK FOR ZERO DIAGONAL ELEMENT                                   
C                                                                       
      if(dcg(j,j).ne.0.0)go to 1753                                     
      zijc=0.0                                                          
      go to 1754                                                        
 1753 zijc=dcg(j,i)/dcg(j,j)                                            
 1754 do 1745 k=i,nbusp1                                                
 1745  dcg(i,k)=dcg(i,k)-dcg(j,k)*zijc                                  
 1747  continue                                                         
C                                                                       
C     CHECK FOR ZERO DIAGONAL ELEMENT 'BUS WITH ALL LINES AND TERMINALS 
C     DISCONNECTED WOULD CAUSE THIS CONDITION'                          
C                                                                       
 1750 if(dcg(i,i).ne.0.0)go to 1752                                     
      recip=0.0                                                         
      go to 1751                                                        
 1752 recip=1.0/dcg(i,i)                                                
C                                                                       
C     CHECK TO SEE IF THIS BUS HAS BEEN FAULTED                         
C                                                                       
 1751     ifault = i - kfltl                                            
      if(ifault.eq.0)recip=0.                                           
       dcg(i,i)=recip                                                   
       i1=i+1                                                           
       do 1755 l=i1,nbusp1                                              
C     ZERO VOLTAGE IF NO PATH TO GROUND                                 
      if(l.eq.nbusp1.and.abs(dcg(i,nbusp1)).lt.1.e-10)go to 1756        
      dcg(i,l)=dcg(i,l)*recip                                           
      go to 1755                                                        
 1756 dcg(i,nbusp1)=0.                                                  
 1755 continue                                                          
 1760  continue                                                         
C                                                                       
C BACKSUBSTITUTION                                                      
C                                                                       
       do 1770 i=1,nbusm1                                               
       k=nbus-i+1                                                       
      nbmi = nbus - i                                                   
       do 1770 j=k,nbus                                                 
 1770 dcg(nbmi,nbusp1)=dcg(nbmi,nbusp1)-dcg(nbmi,j)*dcg(j,nbusp1)       
C                                                                       
C TRASFER SOLUTION VOLTS                                                
C                                                                       
       do 1775 i=1,nbus                                                 
 1775 vdcsto(i) = dcg(i,nbusp1)                                         
C STORE INITIAL DISCONT VOLTAGE                                         
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 1776                                             
      if (keybrd(22).ne.0) then                                         
         write (outbuf,1779)                                            
         call prtout (1)                                                
         write (outbuf,11779) (vdcsto(i),i=1,10)                        
         call prtout (1)                                                
      endif                                                             
 1779     format('0AT S1779 DC VOLT. SOLN. OF L-1 MATRIX')              
11779     format(1x, 10e13.5)                                           
 1776 continue                                                          
      if (idsw.eq.7) go to 2007                                         
      ilinv=1                                                           
C                                                                       
C              ***  INITIALIZATION FOR THE YMTRX MATRIX  ***            
C                                                                       
 1780  go to (1972,1782,1990),mstat                                     
C READ FACTORED MATRIX                                                  
 1782 call redecs(ymtrx,necsk,n100)                                     
      go to 1975                                                        
C READ YMATRIX FROM ECS                                                 
 1972 call redecs(ymtrx,necsy,n100)                                     
       go to 1975                                                       
C INITIALIZE TEMP YMATRIX TABLE                                         
 1990     do 1991 i = 1, n10                                            
          do 1991 j = 1, n10                                            
 1991     ymxtt(i, j) = 0.0                                             
C INITIALIZE YMTRX TABLE                                                
 1810     do 1820 i = 1, n10                                            
          do 1820 j = 1, n10                                            
 1820     ymtrx(i, j) = 0.0                                             
C ZERO OUT COLUMN MATRIX FOR FORCING FNC                                
 1975  do 1980  i=1,nbus                                                
       ymtrx(i,nbusp1)=0.0                                              
 1980  ymtrx(i,nbusp2)=0.0                                              
C                                                                       
C     PRINT OUT LS FLAG INDICATORS FOR DEBUGGING                        
C                                                                       
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 1984                                             
      if(keybrd(22).eq.0)go to 1984                                     
          write (outbuf, 1985)                                          
          call prtout (1)                                               
          do 31985 i = 1,10                                             
             write (outbuf, 11985) ibrtbl(1, i), ibrtbl(2, i)           
             call prtout (1)                                            
31985     continue                                                      
          write (outbuf, 21985) idcnt, idcnt1, lsflg, kflt, irecl       
          call prtout (1)                                               
 1985     format(' IBRTBL(1, I) IBRTBL(2, I)   IDCNT  IDCNT1  LSFLG',   
     1    '  KFLT  IRECL')                                              
11985     format(2x, i11, 1x, i11 )                                     
21985     format(4x, i4, 4x, i4, 3x, i4, 2x, i4, 3x, i4)                
      write (outbuf,1986)                                               
      call prtout (1)                                                   
      do 21986 jjj = 1,nterm,10                                         
         kkk = min0 (jjj+9,nterm)                                       
         write (outbuf,11986) (idcf(i),i=jjj,kkk)                       
         call prtout (1)                                                
21986 continue                                                          
 1986     format(' IDCF AT S1986')                                      
11986     format(10(i4, 2x))                                            
      write (outbuf,1987)                                               
      call prtout (1)                                                   
      do 21987 jjj = 1,nterm,10                                         
         kkk = min0 (jjj+9,nterm)                                       
         write (outbuf,11987) (idcf1(i),i= jjj,kkk)                     
         call prtout (1)                                                
21987 continue                                                          
 1987     format(' IDCF1 AT S1987')                                     
11987     format(10(i4, 2x))                                            
 1984 continue                                                          
 1982 if(ipass.eq.3) go to 2007                                         
 1995  dts=edt/divn                                                     
       delm=dts                                                         
       totdel=delm                                                      
       twodt=2.0/delm                                                   
C                                                                       
C     ***  PRECESS DC VOLTAGES. STEP START VALUES IN DCOLD  ***         
C                                                                       
      call redecs(vdcold,necsv+nbus,nbus)                               
      if (ipass .eq. 2) go to 2004                                      
      call redecs (vdcnew, necsv, nbus)                                 
       do 2002 i=1,nbus                                                 
 2002  vdcold(i)=vdcnew(i)                                              
      call ritecs(vdcold,necsv+nbus,nbus)                               
 2004  do 2005 i=1,nbus                                                 
C     CHECK TO SEE IF WE HAVE PASSED THROUGH L INV THIS ITER.           
      if(ilinv.eq.1)go to 2006                                          
 2005  vdcsto(i)=vdcold(i)                                              
 2006 continue                                                          
C                                                                       
C              *** FIND AC POWER AND CURRENT FOR EACH TERMINAL ***      
C                                                                       
 2007 ind = 0                                                           
 2008 ind = ind + 1                                                     
      if(ind.gt.nterm) go to 3020                                       
       ind1=lbt+ind                                                     
       call redecs (dca,idcb(ind1),msizea)                              
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 2011                                             
      if (keybrd(22).ne.0) then                                         
         write (outbuf,2009)                                            
         call prtout (1)                                                
         do 22009 jjj = 37,52,8                                         
            kkk = min0 (jjj+7,52)                                       
            write (outbuf,12009) (i,dca(i),i=jjj,kkk)                   
            call prtout (1)                                             
22009    continue                                                       
      endif                                                             
 2009     format('0AT S2009 DCA INPUT')                                 
12009     format(1x, 8(i5, e11.4))                                      
 2011 continue                                                          
      if(ipass.eq.3) go to 3005                                         
C GET COMMUTATION VOLTAGE                                               
 2010  ec = eyr(iec)                                                    
       fc = eyi(iec)                                                    
       ecsq1= ec*ec + fc*fc                                             
       ecn = sqrt(ecsq1)* txbse                                         
       if(ecn .eq.0.0)ecn =0.0001                                       
       ev = eyr(iev)                                                    
       fv = eyi(iev)                                                    
       evsq1= ev*ev + fv*fv                                             
       if(evsq1.lt.0.0001)ecn=0.0001                                    
C * * *                                                                 
C * * * WRITE NEW COMMUTATING VOLTAGE TO ECS                            
C * * *                                                                 
      call ritecs (ecn, idcb(ind1)+41, 1)                               
      go to (2087, 2012, 2012), mcp                                     
C DISCONTINUITY CONDITION                                               
 2012  ec = f135* ecn                                                   
      dco1=csign*ec*cosan+rpib3*cdcn*xc                                 
       if(moderi.eq.1) go to 2014                                       
C INVERTER                                                              
       if(dco1.gt.ec)dco1=ec                                            
       go to 2020                                                       
C RECTIFIER                                                             
 2014  if( dco1.lt.-ec) dco1=-ec                                        
 2020 dco11=dco1+rpib18*cdcn*rc                                         
        j = idch(ind)                                                   
        if(nchck .eq. 1) go to 2021                                     
        if(j .eq. 0)j=1                                                 
        dcbusv(j) = dco11                                               
 2021   arg = dco1/ec                                                   
       if(abs(arg ).gt.1.0) arg=1.0                                     
       fe = acos(arg)                                                   
      pv=dco11*cdcn*rbseva                                              
      cdcnm=-csign*cdcn                                                 
      qv=-(dco1*tan(fe)-rpib18*xc*cdcnm)*cdcnm*rbseva                   
C****                                                                   
C* YISOLN, NEWTON OPTION                                                
       go to (2025, 2023 ), inewts                                      
 2023  go to (2025, 2035), mdeyoj                                       
 2025  if(evsq1.gt.0.0001) go to 2030                                   
       crv= 0.0                                                         
       civ= 0.0                                                         
       go to 2060                                                       
 2030 gnet = pv / evsq1 + gv                                            
      bnet = - qv / evsq1 + bv                                          
       crv= ev*gnet -fv*bnet                                            
       civ= ev*bnet +fv*gnet                                            
       go to 2040                                                       
C****                                                                   
C* DETERMINE NEWTON QUANTITIES                                          
C****                                                                   
 2035 crv = 2.0*(pv*ev  + qv*fv )/evsq1                                 
      civ = 2.0*(pv*fv  - qv*ev )/evsq1                                 
      edif = ev *ev  - fv *fv                                           
      esq = evsq1*evsq1                                                 
      gnewt(2*iev-1) = (pv*edif +2.0*qv*ev *fv )/esq                    
      gnewt(2*iev  ) = -gnewt(2*iev-1)                                  
      bnewt(2*iev-1) = (-qv*edif +2.0*pv*ev *fv )/esq                   
      bnewt(2*iev  ) =  bnewt(2*iev-1)                                  
      if(evsq1.le.0.0001) then                                          
      crv = 0.0                                                         
      civ = 0.0                                                         
      gnewt(2*iev-1) = gv                                               
      gnewt(2*iev)  =  gv                                               
      bnewt(2*iev-1) = - bv                                             
      bnewt(2*iev) =  bv                                                
      go to 2060                                                        
      endif                                                             
C                                                                       
C                                                                       
 2040 if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 2050                                             
 2060  if(keybrd(22).eq.0) go to 2050                                   
      write (outbuf,2064) idcb(ind1)                                    
      call prtout (1)                                                   
      do 22064 jjj = 37,52,8                                            
         kkk = min0 (jjj+7,52)                                          
         write (outbuf,12064) (i,dca(i),i=jjj,kkk)                      
         call prtout (1)                                                
22064 continue                                                          
 2064     format('0AT S2064 DCA OUTPUT', i10)                           
12064     format(1x, 8(i5, e11.4))                                      
      write (outbuf,2067)                                               
      call prtout (1)                                                   
      write (outbuf,12067) ec,fc,ev,fv,cdcn,gnet,bnet,crv,civ,ind1,mcp  
      call prtout (1)                                                   
 2067     format('0EC,FC,EV,FV,CDCN GNET,BNET,CRV,CIR,IND1,MCP')        
12067     format(1x, 9e13.5, 2i6)                                       
C                                                                       
C      STORE LATEST CURR FOR AC NETWORK SOLN                            
C                                                                       
 2050  bcurr(iev)= crv                                                  
       bcuri(iev)= civ                                                  
      if(lppwr.eq.1)sumi=0.0                                            
       sumi1= abs(crv)+ abs(civ)                                        
       cdel = abs(sumi-sumi1)                                           
       sumi= sumi1                                                      
       dsum= dsum+ cdel                                                 
       if(cmax.gt.cdel) go to 2055                                      
       imax=iev                                                         
       cmax=cdel                                                        
C               WRITE NEW A C QUANTITIES TO ECS                         
 2055 call ritecs(dca(37),idcb(ind1)+36,5)                              
      if(mcp.eq.3)go to 8090                                            
       go to 2008                                                       
 2087 if(ipass.ne.1) go to 3000                                         
C                                                                       
C IGNORE LOGIC FOR MARGIN SW UNIT TEMPORARILY                           
C                                                                       
C PRECESS DC VARIABLES AND INITIALIZE DCSTOR TABLE                      
       do 2090 i=2,10                                                   
 2090 dca(63+i) = dca(53+i)                                             
      dcfil(5,ind) = dcfil(1,ind)                                       
      dcfil(6,ind) = dcfil(2,ind)                                       
      call ritecs (dca(65), idcb(ind1)+64, 9)                           
 3000  if(ipass.eq.3) go to 3005                                        
C     USE OLD VALUE OF COSA FOR STEP START AT THE BEGINNING OF          
C     EACH ITERATION .                                                  
      cosan=cosao                                                       
C     CHECK FOR VALUE BLOCKING.                                         
      if(idcf(ind).eq.2.or.idcf(ind).eq.4)cosan=cosblk                  
      call ritecs(dca(63),idcb(ind1)+62,1)                              
      dcstor(1,ind) = eco                                               
       do 3010 i=2,10                                                   
 3010 dcstor(i,ind) = dca(63+i)                                         
      dcstor(12,ind)=dcstor(10,ind)                                     
      dcfil(3,ind) = dcfil(5,ind)                                       
      dcfil(4,ind) = dcfil(6,ind)                                       
      crsto(ibus,ibus)=dca(72)                                          
C      GET DELTA EC                                                     
      dcstor(11,ind) = ecn - eco                                        
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 3015                                             
      if (keybrd(22).ne.0) then                                         
         write (outbuf,3011)                                            
         call prtout (1)                                                
         write (outbuf,13011) (dcstor(i,ind),i=1,11)                    
         call prtout (1)                                                
      endif                                                             
 3011     format('0S3011 DCSTOR ARRAY')                                 
13011     format(1x, 11e12.5)                                           
 3015 continue                                                          
C3005 IF (IDISC.EQ.2) DCSTOR(9,IND) = CURRT(IND)                        
 3005 continue                                                          
       if( mstat.eq.2) go to 3014                                       
C                                                                       
C NEW DT ESTABLISH YII                                                  
C                                                                       
       go to (3014,3012),idt                                            
 3012  cfac=2.0*sl/dts                                                  
       yiop  = 1.0/(rxpi+cfac)                                          
      ziom = rxpi - cfac                                                
C BYPASS YMTRX ENTRY IF VALVE SW OPENED                                 
      if(idcf(ind).eq.2.or.idcf(ind).eq.3)go to 3013                    
      ymtrx(ibus,ibus) = ymtrx(ibus,ibus) + yiop                        
C   STORE PARTIAL YMTRX TEMPORARILY IF EDT CHANGED                      
 3013 if (idt.eq.2) ymxtt(ibus,ibus) = ymxtt(ibus,ibus) + yiop          
C                                                                       
C           *** COMPUTE FORCING FUNCTION % INTERPOLATED COMM. VOLT ***  
C                                                                       
 3014 cf135=csign*f135                                                  
      if(ipass.eq.3)dcstor(9,ind)=crsto(ibus,ibus)                      
       eocn=eco+delm*dcstor(11,ind)/edt                                 
C * * *                                                                 
C     FIND COMMUTATION VOLTAGE  DELAY                                   
C * * *                                                                 
      twoec=twodt*dca(76)                                               
      ecpls = twoec + 1.0                                               
      ecmns = twoec - 1.0                                               
       eocdn=(eocn+dcstor(1,ind)+dcstor(2,ind)*ecmns)/ecpls             
      biim=dcstor(9,ind)*ziom+twovd                                     
     1       +cf135 * dcstor(1,ind) * dcstor(10,ind) - vdcsto(ibus)     
 3017 ind1 = lbt + ind                                                  
       call ritecs(yiop,idcb(ind1)+50,5)                                
C BYPASS YMTRX ENTRY IF VALVE SW OPENED                                 
 3016 if(idcf(ind).ne.2.and.idcf(ind).ne.3)go to 3018                   
      if(mstat.eq.1)ymtrx(ibus,ibus)=ymtrx(ibus,ibus)-yiop              
      go to 2008                                                        
 3018 ymtrx(ibus,nbusp1)=(biim+cf135*eocn*cosan)*yiop                   
C      LOOP FOR ANOTHER TERMINAL                                        
      go to 2008                                                        
C                                                                       
C                                                                       
 3020  go to (3050,3030),mcp                                            
 3030 do 3040 i = 1,nbus                                                
 3040 vdcnew(i) = vdcsto(i)                                             
      go to 9000                                                        
C                                                                       
C      ***  START OF SUB-TIMESTEP LOOP  ***                             
C                                                                       
 3050 lcstrt = lbt+nterm+1                                              
       loopdc=0                                                         
 3080  ind=lcstrt                                                       
       litbl=0                                                          
       itcnt=0                                                          
C                                                                       
C TEST IF END OF MATRIX                                                 
C                                                                       
 3090  if(ind.gt.ndimc) go to 4070                                      
       mbus=idcb(ind)                                                   
C                                                                       
C TEST IF NEW BUS                                                       
C                                                                       
      if (mbus.lt.0) go to 4050                                         
C                                                                       
C TEST IF DC TERM CONNECTED TO THIS BUS                                 
C                                                                       
      if (mbus.gt.10) go to 4062                                        
       jb=mbus                                                          
C TEST IF NEW DT                                                        
 4012  go to ( 4025,4020),idt                                           
C LOGIC TO HANDLE NEW DT                                                
 4020  fact= 2.0*dcb(ind+2)/delm                                        
       dcb(ind+3)= 1.0/(dcb(ind+1)+fact)                                
       dcb(ind+4)= dcb(ind+1)-fact                                      
      ymxtt(ib,ib) = ymxtt(ib,ib) + dcb(ind+3)                          
      ymxtt(ib,jb) = -dcb(ind+3)                                        
C PRECESS BRANCH CURRENT                                                
 4025  if(ipass.ne.1) go to 4030                                        
       dcb(ind+6)=dcb(ind+5)                                            
 4030 if(ipass.ne.3)crsto(ib,jb)=dcb(ind+6)                             
      dcb(ind+7)=crsto(ib,jb)*dcb(ind+4)+vdcsto(jb)-vdcsto(ib)          
       ybij= dcb(ind+3)*dcb(ind+7)                                      
C                                                                       
C     ARE WE REUSING THE FACTORIZED YMTRX )                             
C                                                                       
C                                                                       
C     BY-PASS OLD VERSION LINE TRIPPING                                 
CC     IF(0.EQ.0) GO TO 4026...MOVED TO FEW STATEMENTS BELOW            
C     NO SO                                                             
C     CHECK TO SEE IF THIS BRANCH AS HAS BEEN OPEN-CIRCUITED.           
C                                                                       
      if(ibrknt.eq.0)go to 4033                                         
      do 4032 i=1,ibrknt                                                
          if(ib .eq. ibrtbl(1, i) .and. jb .eq. ibrtbl(2, i))           
     1    go to 4031                                                    
          if(jb .eq. ibrtbl(1, i) .and. ib .eq. ibrtbl(2, i))           
     1    go to 4031                                                    
          go to 4032                                                    
 4031     ybij = 0.0                                                    
          dcb(ind + 7) = 0.0                                            
CC  ABOVE BYPASS STATEMENT INSERTED NEXT STATEMENT                      
          if(0 .eq. 0) go to 4026                                       
c     IF(MSTAT.EQ.2)GO TO 4034                                          
C                                                                       
C     LINE IS 0.C.                                                      
C     IF RECREATING YMATRIX OMIT ENTRY FOR THIS LINE                    
C                                                                       
c     IF(MSTAT.EQ.3)GO TO 4040                                          
C                                                                       
C     IF REUSING YMTRX REMOVE THIS LINE FROM YMATRIX                    
C                                                                       
C                                                                       
C     REMOVE FROM DIAGONAL                                              
C                                                                       
c     YMTRX(IB,IB)=YMTRX(IB,IB)-DCB(IND+3)                              
C                                                                       
C     REMOVE OFF-DIAGONAL                                               
C                                                                       
c     YMTRX(IB,JB)=0.0                                                  
c     GO TO 4040                                                        
 4032 continue                                                          
 4033 continue                                                          
C                                                                       
C     LINE IS CLOSED IN SO ADD BRANCH TO YMTRX IF RECOMPUTING YMTRX     
C     MSTAT=3                                                           
 4026 continue                                                          
      if(mstat.ne.3)go to 4034                                          
C     DIAGONAL                                                          
      ymtrx(ib,ib)=ymtrx(ib,ib)+dcb(ind+3)                              
C     OFF-DIAGONAL                                                      
      ymtrx(ib,jb)=-dcb(ind+3)                                          
C                                                                       
C     ADD BRANCH COMPONENT TO FORCING FUNCTION                          
C                                                                       
 4034 ymtrx(ib,nbusp2)= ymtrx(ib,nbusp2)+ybij                           
      ymtrx(ib,nbusp1)= ymtrx(ib,nbusp1)+ybij                           
 4040 ind=ind+8                                                         
       go to 3090                                                       
C                                                                       
C NEW BUS...TEST IF DC TERM                                             
 4050  ib=-mbus                                                         
       if(ib.le.10)go to 4060                                           
       itcnt=itcnt+1                                                    
       ib=ib-10                                                         
 4060  ind=ind+1                                                        
       jb= idcb(ind)                                                    
      ymtrx(ib, nbusp2) = 0.                                            
       if(jb.le.10)go to 4012                                           
 4062  ind=ind+1                                                        
       go to 3090                                                       
C                                                                       
 4070 continue                                                          
C DC MATRIX FORM COMPLETED                                              
C                                                                       
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 4071                                             
          if(nsubt .gt. nstmx .or. loopdc .gt. nstmx) go to 4071        
          if(keybrd(16) .ne. 0)                                         
     1 then                                                             
          write (outbuf, 4067)                                          
          call prtout (1)                                               
          do 24067 i = 1,10                                             
             write (outbuf, 14067) (ymtrx(i, j), j = 1, 10)             
             call prtout (1)                                            
24067     continue                                                      
       endif                                                            
 4067     format('0S4067 YMTRX MATRIX')                                 
14067     format(1x, 10e13.5)                                           
 4071 continue                                                          
      go to (4090,4080),idt                                             
 4080 call ritecs (ymxtt, necsy,n100)                                   
C NEW DT SW IDT TO NORMAL                                               
       idt=1                                                            
C STORE PARTIAL MATRIX                                                  
C SETUP DT DEPENDENT FACTORS                                            
 4090  fmpy=totdel/edt                                                  
C           INITIALIZE ALL VALVE STATUS FOR ABNORMAL CURRENT CALCULATION
      do 4092 i=1,nterm                                                 
 4092 iisw(i) = 1                                                       
      iisw1 = 1                                                         
C                                                                       
C      ***  SUB-TIMESTEP LOOP RETURNS HERE IF MDCREG  DOESN'T CONVERGE *
C                                                                       
 4094  loopdc=loopdc+1                                                  
       if(loopdc.le.20) go to 4097                                      
       write (outbuf,4095)                                              
       call prtout (1)                                                  
 4095     format('0DC ITER LIMIT FOR MULTERM REACHED IN DC SUB')        
       link=13                                                          
       return                                                           
C DEFER ABNORMAL FFNC IN VALVE                                          
 4097 if (mstat .eq. 2) go to 5040                                      
C                                                                       
C PROCEED TO FACTORIZE MATRIX AND SOLVE EQN                             
 5000  do 5035 i=1,nbus                                                 
       if(i.eq.1) go to 5020                                            
       im=i-1                                                           
      do 5015 j = 1, im                                                 
       if(ymtrx(j,i  ).eq.0.0)go to 5015                                
C                                                                       
C     CHECK FOR ZERO DIAGONAL ELEMENT 'BUS WITH ALL LINES AND TERMINALS 
C     DISCONNECTED WOULD CREATE THIS CONDITION.'                        
C                                                                       
      if(ymtrx(j,j).ne.0.0)go to 5002                                   
      zijc=0.0                                                          
      go to 5003                                                        
 5002 zijc=ymtrx(j,i)/ymtrx(j,j)                                        
 5003 do 5010 k=i,nbusp1                                                
 5010  ymtrx(i,k)=ymtrx(i,k)-ymtrx(j,k)*zijc                            
 5015  continue                                                         
C                                                                       
C     CHECK FOR ZERO DIAGONAL ELEMENT 'BUS WITH ALL LINES AND TERMINALS 
C     DISCONNECTED WOULD CAUSE THIS CONDITION'                          
C                                                                       
 5020     if(abs(ymtrx(i, i)) .gt. 1.e-6) go to 5016                    
      recip=0.0                                                         
      go to 5017                                                        
 5016 recip=1.0/ymtrx(i,i)                                              
C                                                                       
C     CHECK TO SEE IF THIS BUS HAS BEEN FAULTED.                        
C                                                                       
 5017     ifault = i - kflty                                            
      if(ifault.eq.0)recip=0.                                           
       ymtrx(i,i)=recip                                                 
       i1=i+1                                                           
        do 5030 l=i1,nbusp1                                             
C     ZERO VOLTAGE IF NO PATH TO GROUND                                 
      if(l.eq.nbusp1.and.abs(ymtrx(i,nbusp1)).lt.1.e-10)go to 5029      
      ymtrx(i,l)=ymtrx(i,l)*recip                                       
      go to 5030                                                        
 5029 ymtrx(i,nbusp1)=0.                                                
 5030 continue                                                          
 5035  continue                                                         
      mstat = 2                                                         
      call ritecs (ymtrx, necsk, n100)                                  
       go to 5046                                                       
C                         DOWNWARD OPERATION                            
 5040 do 5045 i = 1,nbus                                                
      if(i.eq.1) go to 5048                                             
      im = i - 1                                                        
C                                                                       
        do 5042 j= 1,im                                                 
        if (ymtrx(j,i) .eq.0.) go to 5042                               
C     CHECK FOR ZERO DIAGONAL ELEMENT.                                  
      if(ymtrx(j,j).ne.0.0)go to 5041                                   
      zijc=0.0                                                          
      go to 5043                                                        
 5041 zijc=ymtrx(j,i)/ymtrx(j,j)                                        
C                                                                       
C     CHECK TO SEE IF THE BUS HAS BEEN FAULTED.                         
C                                                                       
 5043     ifault = i - kflty                                            
      if(ifault.ne.0.0)go to 5044                                       
      ymtrx(i,nbusp1)=0.0                                               
      go to 5042                                                        
 5044 ymtrx(i,nbusp1)=ymtrx(i,nbusp1)-ymtrx(j,nbusp1)*zijc              
 5042   continue                                                        
 5048 if(abs(ymtrx(i,nbusp1)).lt.1.e-10)go to 5047                      
      ymtrx(i,nbusp1)=ymtrx(i,nbusp1)*ymtrx(i,i)                        
      go to 5045                                                        
 5047 ymtrx(i,nbusp1)=0.                                                
 5045 continue                                                          
C                                                                       
C                         BACK-SUBSTITUTION                             
 5046  do 5050 i=1,nbusm1                                               
       k= nbus-i+1                                                      
      nbmi = nbus - i                                                   
       do 5050 j=k,nbus                                                 
 5050 ymtrx(nbmi,nbusp1)=ymtrx(nbmi,nbusp1)-ymtrx(nbmi,j)               
     1      * ymtrx(j,nbusp1)                                           
C                                                                       
C WRITE DC VOLTS TO STORAGE                                             
       do 5060 i=1,nbus                                                 
      vdcnew(i) = ymtrx(i,nbusp1)                                       
 5060 ymtrx(i,nbusp1) = 0.0                                             
       iredo=1                                                          
       ipass=3                                                          
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 5056                                             
          if(nsubt .gt. nstmx .or. loopdc .gt. nstmx) go to 5056        
          if (keybrd(16) .ne. 0) then                                   
             write (outbuf, 5055)                                       
             call prtout (1)                                            
             write (outbuf, 15055) (vdcnew(i), i=1, 10)                 
             call prtout (1)                                            
          endif                                                         
 5055     format('0VDCNEW ABOVE DO 6060')                               
15055     format(1x, 10e13.5)                                           
 5056 continue                                                          
C                                                                       
C                                                                       
C        ***  CALCULATE CONVERTER CURRENT IN CURRT(10)  ***             
C                                                                       
       iopen=1                                                          
       do 6060 i=1,nterm                                                
       ind1=lbt+i                                                       
      call redecs (dca(1),idcb(ind1),msizea)                            
CC VALVE CURR REVERSAL TEST FOR ALL VALVE SWS THAT ARE CLOSED           
       ciic=0.0                                                         
      if(idcf(i).eq.3)iopen=2                                           
      if(idcf(i).eq.2.or.idcf(i).eq.3)go to 5070                        
       ec135=f135*eocn                                                  
      ecosa=ec135*cosan                                                 
      biic = biim + csign * ecosa                                       
       ciic=(vdcnew(ibus)-biic)*yiop                                    
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 5063                                             
          if(nsubt .gt. nstmx .or. loopdc .gt. nstmx) go to 5063        
          if(keybrd(16) .ne. 0)                                         
     1    then                                                          
             write (outbuf, 5061) ibus, moderi, eocn, ciic              
             call prtout (1)                                            
          endif                                                         
 5061     format('0S5061 IBUS,MODERI,EOCN,CIIC', 2i10, 2e13.5)          
 5063 continue                                                          
C     CHECK FOR CURRENT REVERSAL (COMPARE TO A SMALL <                  
C     BECAUSE A TERMINAL DISCONNECT FROM DC SYS. BUT                    
C     UNBLOCKED HAS ZERO CURRENT , BUT ROUNDOFF GIVES                   
C     A SLIGHTLY NON-ZERO VALUE .)                                      
C CONVERTER TYPE                                                        
       if(moderi.ne.1) go to 5065                                       
C RECTIFIER                                                             
      if(ciic.le.1.e-4)go to 5070                                       
      iredo=2                                                           
CC INSERT FOLLOWING STATEMENTS WHEN VALVE BLOCKING INCORP               
      if(idcf(i).ne.4)go to 5062                                        
      idcf(i)=2                                                         
      go to 5070                                                        
 5062  iopen=2                                                          
       idcf(i)=3                                                        
       go to 5070                                                       
C INVERTER                                                              
 5065 if(ciic.ge.-1.e-4)go to 5070                                      
       iopen=2                                                          
       iredo=2                                                          
       idcf(i)=3                                                        
 5070 currt(i)=ciic                                                     
C DEFER CKT ADJUSTMENT DUE TO ABNORMAL COND. TILL LATER                 
      if(0.eq.0)go to 6060                                              
C CONVERTER TYPE TEST                                                   
c      IF(MODERI.NE.1)GO TO 6050                                        
C RECTIFIER                                                             
c      TERM = ECOSA-XPI*CIIC                                            
c      IF(TERM .GE.-EC135) GO TO 6060                                   
C SET SW FOR ABNORMAL COND                                              
c      TERM=-EC135                                                      
c6000  IISW(I)=2                                                        
c      IISW1=2                                                          
c      BIIC=BIIM+TERM                                                   
c      YIOT=1.0/( RXPI-XPI+2.0*SL/DTS)                                  
c      YMTRX(IBUS,IBUS)=YMTRX(IBUS,IBUS)-YIOP+YIOT                      
c      YMTRX(IBUS,NBUSP2)=YMTRX(IBUS,NBUSP2)+BIIC*YIOT                  
c      GO TO 6060                                                       
C INVERTER                                                              
c6050  TERM=-ECOSA+XPI*CIIC                                             
c      IF(TERM.LE.EC135) GO TO 6060                                     
c      TERM=EC135                                                       
c      GO TO 6000                                                       
 6060  continue                                                         
C                                                                       
C       ***  ABNORMAL CURRENT CALCULATION BYPASSED FOR NOW  ***         
C                                                                       
      if(0.eq.0)go to 7035                                              
c6080  DO 7030 I=1,NTERM                                                
c     CALL REDECS (DCA, IDCB(LBT+I), MSIZEA)                            
c     EC135 = F135 * EOCN                                               
c      IF(IISW(I).EQ.2) GO TO 6090                                      
c      ECOSA=EC135*COSAN                                                
c      BIIC=BIIM+CSIGN*ECOSA                                            
c      CIIC=(VDCNEW(IBUS)-BIIC)*YIOP                                    
c      GO TO 7020                                                       
C CALC BR CURR FOR ABNORMAL COND                                        
c6090  YIOT=1.0/(RXPI-XPI+2.0*SL/DELM)                                  
c     BIIC = BIIM + CSIGN*ECOSA                                         
c     CIIC = (VDCNEW(IBUS) - BIIC) * YIOT                               
c7020 CURRT(I)=CIIC                                                     
 7030  continue                                                         
 7035 continue                                                          
C NETWORK REPEAT TEST                                                   
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 7031                                             
          if(nsubt .gt. nstmx .or. loopdc .gt. nstmx) go to 7031        
          if(keybrd(16) .ne. 0)                                         
     1    then                                                          
             write (outbuf, 6070) iredo, iopen                          
             call prtout (1)                                            
             write (outbuf, 16070) (iisw(i),i=1,10),                    
     1          (idcf(i),i=1,10)                                        
             call prtout (1)                                            
          endif                                                         
 6070     format('0S6070 IREDO,IOPEN,IISW,IDCF', 2i10)                  
16070     format(1x, 10i6, 6x, 10i6)                                    
 7031 continue                                                          
      if(iredo.ne.2)go to 7037                                          
C                                                                       
C     COMPUTE THE FIRST CROSSOVER PT.                                   
C                                                                       
      dts11=dts                                                         
      do 7048 i=1,nterm                                                 
      if(currt(i).eq.0.)go to 7048                                      
      if(idcf(i).ne.2.and.idcf(i).ne.3)go to 7048                       
      call redecs(dca(47),idcb(lbt+i)+46,1)                             
      dts1=(crsto(ibus,ibus)*dts/(crsto(ibus,ibus)-currt(i)))           
      if(dts1.lt.dts11)dts11=dts1                                       
 7048 continue                                                          
C                                                                       
C     COUNT THE < OF VALVES THAT CROSSOVER THIS STS AND THE <           
C     OF VALVES TO BE SWITCHED OPEN .                                   
C                                                                       
      dts111=dts11+.00001                                               
      icross=0                                                          
      isopen=0                                                          
      do 6095 i=1,nterm                                                 
      if(currt(i).eq.0.)go to 6095                                      
      if(idcf(i).ne.2.and.idcf(i).ne.3)go to 6095                       
      call redecs(dca(47),idcb(lbt+i)+46,1)                             
      dts1=(crsto(ibus,ibus)*dts/(crsto(ibus,ibus)-currt(i)))           
      if(idcf(i).eq.2.or.idcf(i).eq.3)icross=icross+1                   
      if(dts1.le.dts111.and.(idcf(i).eq.3.or.idcf(i).eq.2))isopen=      
     1isopen+1                                                          
C                                                                       
C     BLOCK ONLY THE FIRST VALVE TO CROSSOVER .                         
C                                                                       
      if(dts1.gt.dts111.and.idcf(i).eq.3)idcf(i)=1                      
      if(dts1.gt.dts111.and.idcf(i).eq.2)idcf(i)=4                      
 6095 continue                                                          
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 6097                                             
      if (keybrd(22).ne.0) then                                         
         write (outbuf,6096) icross,isopen                              
         call prtout (1)                                                
      endif                                                             
 6096     format('0S6096 ICROSS,ISOPEN', 5x, i6, 5x, i6)                
 6097 continue                                                          
C                                                                       
C     RECOMPUTE STEP START VALUES OF CURRENTS AT DTS11 .                
C                                                                       
C     CONVERTER CURRENTS                                                
C                                                                       
      do 7049 i=1,nterm                                                 
      call redecs(dca(47),idcb(lbt+i)+46,1)                             
      crsto(ibus,ibus)=(((currt(i)-crsto(ibus,ibus))*dts11)/dts)        
     1+crsto(ibus,ibus)                                                 
      currt(i)=crsto(ibus,ibus)                                         
 7049 continue                                                          
C                                                                       
C     BRANCH CURRENTS                                                   
C                                                                       
      lcstrt=lbt+nterm+1                                                
      ind=lcstrt                                                        
 7044 if(ind.gt.ndimc)go to 7047                                        
      mbus=idcb(ind)                                                    
      if(mbus.lt.0)go to 7045                                           
      if(mbus.gt.10)go to 7046                                          
      jb=mbus                                                           
      dcb(ind+5)=(vdcnew(ib)-vdcnew(jb)-dcb(ind+7))*dcb(ind+3)          
C     BY-PASS OLD VERSION OF LINE TRIPPING                              
      if(0.eq.0)go to 7055                                              
C     ZERO CURRENT IF BRANCH IS OPEN                                    
c     IF(IBRKNT.EQ.0)GO TO 7054                                         
c     DO 7053 I=1,IBRKNT                                                
c         IF(IB .EQ. IBRTBL(1, I) .AND. JB .EQ. IBRTBL(2, I))           
c    1    GO TO 7065                                                    
c         IF(JB .EQ. IBRTBL(1, I) .AND. IB .EQ. IBRTBL(2, I))           
c    1    GO TO 7065                                                    
c         GO TO 7053                                                    
c7065     DCB(IND + 5) = 0.                                             
c7053 CONTINUE                                                          
c7054 CONTINUE                                                          
 7055 continue                                                          
      crsto(ib,jb)=((dcb(ind+5)-crsto(ib,jb))*dts11/dts)                
     1+crsto(ib,jb)                                                     
      ind=ind+8                                                         
      go to 7044                                                        
 7045 ib=-mbus                                                          
      if(ib.gt.10)ib=ib-10                                              
 7046 ind=ind+1                                                         
      go to 7044                                                        
 7047 mstat=1                                                           
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 7043                                             
      if (keybrd(22).ne.0) then                                         
         write (outbuf,7042) iredo,iopen                                
         call prtout (1)                                                
         write (outbuf,17042) (iisw(i),i=1,10),                         
     1     (idcf(i),i=1,10)                                             
         call prtout (1)                                                
      endif                                                             
 7042     format('0S7042 IREDO,IOPEN,IISW,IDCF', 2i10)                  
17042     format(1x, 10i6, 6x, 10i6)                                    
      if(keybrd(22).eq.0)go to 7043                                     
      write (outbuf,7051)                                               
      call prtout (1)                                                   
      do 27051 i = 1,5                                                  
         write (outbuf,17051) (crsto(i,j),j=1,5)                        
         call prtout (1)                                                
27051 continue                                                          
 7051     format('0CRSTO MATRIX AT S 7051')                             
17051     format(11x, 5e15.5)                                           
      write (outbuf,7052)                                               
      call prtout (1)                                                   
      write (outbuf,17052) (currt(i),i=1,10)                            
      call prtout (1)                                                   
 7052     format('0CURRT ARRAY AT S7052')                               
17052     format(1x, 10e13.5)                                           
 7043 go to 1655                                                        
C TEST TO STORE FMTRX                                                   
 7037 if(idt.eq.2)call ritecs(ymtrx,necsk,n100)                         
C     TEST TO STORE YMTRX FORCING FUNCTION                              
      if(ipass.eq.1.and.iopen.eq.1) call ritecs(ymtrx(1,nbusp2),        
     1     necsk + nbussq, nbus)                                        
C                                                                       
       do 7040  i=1,nbus                                                
 7040 ymtrx(i,nbusp1) = ymtrx(i,nbusp2)                                 
C                                                                       
C              MDCREG CALCULATES DC-TERMINAL CONTROL EQUATIONS          
C                       FOR THE ENTIRE CIRCUIT                          
C                                                                       
       call mdcreg                                                      
C                                                                       
C CHECK IF DC SYSTEM CONVERGED                                          
       if(itdc.eq.1) go to 7050                                         
C SETUP FOR ANOTHER DC ITER                                             
       mstat=2                                                          
       go to 4094                                                       
C                                                                       
C CHECK END OF TIME INTERVAL                                            
 7050     if(abs(totdel - edt) .lt. 1.0e-4) go to 8030                  
C ESTABLISH NEXT SUBINTERVAL TIME STEP                                  
       totdel=totdel+delm                                               
       fmpy=totdel/edt                                                  
C                                                                       
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 7056                                             
      if (keybrd(22).ne.0) then                                         
         write (outbuf,7060) idone,totdel,edt,delm                      
         call prtout (1)                                                
      endif                                                             
 7060     format('0-S7060 NEW SUB-T.STEP0IOPEN,TOTDEL,EDT,DELM',        
     1    i5, 3f15.4)                                                   
 7056 continue                                                          
C                                                                       
      do 8000 i= 1,nbus                                                 
      vdcsto(i) = vdcnew(i)                                             
      ymtrx(i,nbusp1) = 0.                                              
 8000 ymtrx(i,nbusp2) = 0.                                              
C                                                                       
C          ***  UPDATE RIGHT-HAND-SIDE OF YMTRX MATRIX                  
C                                                                       
       do 8020 i=1,nterm                                                
       ind1=lbt+i                                                       
      call redecs (dca(23), idcb(ind1)+22, 55)                          
       do 8012  i1=1,10                                                 
 8012 dcstor(i1,i)=dca(i1+53)                                           
      dcfil(3,i) = dcfil(1,i)                                           
      dcfil(4,i) = dcfil(2,i)                                           
      crsto(ibus,ibus)=dca(62)                                          
      eocn=eco+dcstor(11,i)*fmpy                                        
C * * *                                                                 
C     FIND COMMUTATION VOLTAGE  DELAY                                   
C * * *                                                                 
      twoec=twodt*dca(76)                                               
      ecpls = twoec + 1.0                                               
      ecmns = twoec - 1.0                                               
      eocdn = (eocn + dcstor(1,i) + dcstor(2,i)* ecmns) / ecpls         
       cf135=csign*f135                                                 
      biim=dcstor(9,i)*ziom+twovd                                       
     1       +cf135 * dcstor(1,i) * dcstor(10,i) - vdcsto(ibus)         
C     BYPASS IF ANY TERMINAL SWITCH OPEN                                
      if(idcf(i).eq.2.or.idcf(i).eq.3)go to 8020                        
      ymtrx(ibus,nbusp1) = (biim + cf135 * eocn * cosan) * yiop         
 8020  call ritecs (dca(53),idcb(ind1)+52,11)                           
       go to 8035                                                       
 8030  idone=2                                                          
 8035 lcstrt = lbt+nterm+1                                              
       ind=lcstrt                                                       
       litbl=0                                                          
 8040  if(ind.gt.ndimc)go to 8070                                       
       mbus=idcb(ind)                                                   
       if(mbus.lt.0) go to 8050                                         
       if(mbus.gt.10) go to 8060                                        
       jb=mbus                                                          
       dcb(ind+5)=(vdcnew(ib)  -vdcnew(jb)  -dcb(ind+7))*dcb(ind+3)     
       crsto(ib,jb)=dcb(ind+5)                                          
C     BY-PASS OLD VERSION OF LINE TRIPPING                              
      if(0.eq.0)go to 8047                                              
C     ZERO CURRENT IF BRANCH IS OPEN                                    
c     IF(IBRKNT.EQ.0)GO TO 8046                                         
c     DO 8045 I=1,IBRKNT                                                
c         IF(IB .EQ. IBRTBL(1, I) .AND. JB .EQ. IBRTBL(2, I))           
c    1    GO TO 8042                                                    
c         IF(JB .EQ. IBRTBL(1, I) .AND. IB .EQ. IBRTBL(2, I))           
c    1    GO TO 8042                                                    
c         GO TO 8045                                                    
c8042     CRSTO(IB, JB) = 0.                                            
c     DCB(IND+5)=0.                                                     
c     GO TO 8046                                                        
c8045 CONTINUE                                                          
c8046 CONTINUE                                                          
 8047 continue                                                          
       ind=ind+8                                                        
       go to 8040                                                       
 8050  ib=-mbus                                                         
       if(ib.gt.10) ib=ib-10                                            
 8060  ind=ind+1                                                        
       go to 8040                                                       
C IDONE=1 PREPARE FOR NEW SUBTIMESTEP,IDONE=2 LAST SUBTIMESTEP COMPLETE 
 8070  if(idone.eq.2) go to 8080                                        
C                                                                       
C  TEST IF BKR OPENED ON FLT DURING THE FIRST SUBTIMESTEP               
C                                                                       
          nsubt = nsubt + 1                                             
          if(ktrip .eq. 0) go to 8075                                   
          if(nsubt .ne. 2) go to 8075                                   
C                                                                       
CC  REMOVE FAULT CODE FROM L-1 MATRIX...                                
C                                                                       
          if(nflt .eq. 999) kfltl = 0                                   
          ktrip = 0                                                     
          mstat = 2                                                     
          go to 1655                                                    
 8075     mstat = 2                                                     
      go to 3050                                                        
C                                                                       
C    ***  END OF TIMESTEP.PREPARE AC AND DC QUANTITIES FOR OUTPUT  ***  
C                                                                       
 8080 ind = 0                                                           
C                                                                       
C     PRESET CURRENT REVERSAL INDICATOR                                 
C                                                                       
      irev=1                                                            
 8090  ind=ind+1                                                        
       if(ind.gt. nterm) go to 9000                                     
C                                                                       
C     WRITE IDCF AND IDCF1 TO ECS                                       
C                                                                       
      call ritecs(idcf,necsf,nterm)                                     
      call ritecs(idcf1,necsf+nterm,nterm)                              
C                                                                       
C     SET IREV INDICATOR IF CURRENT REVERSAL OCCURRED                   
C                                                                       
      if(idcf1(ind).ne.3.and.idcf(ind).eq.3)irev=2                      
      if(idcf1(ind).ne.2.and.idcf(ind).eq.2)irev=2                      
       ind1=lbt+ind                                                     
       call redecs (dca,idcb(ind1),msizea)                              
       cosout(ind )=cosan                                               
      crsto(ibus,ibus)=cdcn                                             
      mcp = 3                                                           
      go to 2010                                                        
C                                                                       
C          ***  OUTPUT DC DATA WHICH WILL BE WRITTEN ON TAPE L8 ***     
C                                                                       
 9000  i1=1                                                             
C                                                                       
C     INCREMENT DC DISCONTINUITY FLAG IF VALUE OPEN DUE TO CURRENT REVER
C     SAL                                                               
C                                                                       
      if(irev.eq.2)idcnt=idcnt+1                                        
      call redecs (idch,necsh,ndimh)                                    
        call ritecs(dcj,necsj,i1)                                       
      do 9010 i = 1, nterm                                              
       dcj(i1)=cosout(i)                                                
      call redecs (dca(1),idcb(lbt+i),msizea)                           
       dcj(i1+1)=dca(62)                                                
      if(moderi.eq.1)dcj(i1+1)=-dcj(i1+1)                               
       dcj(i1+2) = vdcnew(ibus)                                         
       dcj(i1+3) = acos(dca(32))                                        
C * * *                                                                 
C * * * MODULATION OUTPUT SIGNAL                                        
C * * *                                                                 
      modcod = idca(78)                                                 
      if(modcod .eq. 0) go to 9005                                      
      imod = idca(79)                                                   
C * * *                                                                 
C * * * GAMMA MODULATION                                                
C * * *                                                                 
      if(modcod .eq. 5) then                                            
         dcj(i1+3) = gama(imod)                                         
         go to 9005                                                     
      endif                                                             
C * * *                                                                 
C * * * LOW LEVEL MODULATION                                            
C * * *                                                                 
      if(modcod .lt. 3)then                                             
         dcj(i1+3) = siglom(imod)                                       
C * * *                                                                 
C * * * HIGH LEVEL AND DUAL FREQUENCY MODULATION                        
C * * *                                                                 
      else                                                              
         dcj(i1+3) = sighim(imod)                                       
      endif                                                             
C * * *                                                                 
C * * * CALCULATE EXTINCTION ANGLE FOR OUTPUT                           
C * * *                                                                 
 9005  vc = 2.0*xc*fdin                                                 
       eo = sqr2*eocdn                                                  
       cosovr = vc/eo -cosan                                            
       if(cosovr .gt. 1.)cosovr = 1.                                    
       if(cosovr .lt. -1.)cosovr = -1.                                  
       dcj(i1+4) = pi - acos(cosovr)                                    
       dcj(i1+5) = dca(61)                                              
       dcj(i1+6) = dca(60)                                              
C * * *                                                                 
C * * * ADD TWO SPARE DATA POINTS                                       
C * * *                                                                 
       dcj(i1+7) = 0.0                                                  
       dcj(i1+8) = 0.0                                                  
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 9010                                             
      if(keybrd(30).eq.0)go to 9010                                     
      write (outbuf,9039)                                               
      call prtout (1)                                                   
      do 29039 jjj = 1,73,8                                             
         kkk = min0 (jjj+7,73)                                          
         write (outbuf,19039) (ik,dca(ik),ik=jjj,kkk)                   
         call prtout (1)                                                
29039 continue                                                          
      call skipln (1)                                                   
 9039     format(1x, 'DCA TABLE BEFORE EXITING FROM DCSUB')             
19039     format(1x, 8(i5, e11.4))                                      
      write (outbuf,9041)                                               
      call prtout (1)                                                   
      write (outbuf,19041) idca(15),idca(45),idca(46),idca(47)          
      call prtout (1)                                                   
 9041     format(' IDCA 15,45,46,47 ')                                  
19041     format(1x, 4(2x, i5))                                         
 9010  i1=i1+9                                                          
      call ritecs (dcj, necsj, i1-1)                                    
      if(nchck .eq. 1) go to 9026                                       
C * * *                                                                 
C * * * OUTPUT FOR DC BRANCHES                                          
C * * *                                                                 
      nterm1 = nterm+ 1                                                 
       do 9025 i=nterm1,ndimh,2                                         
        j1=idch(i)                                                      
       j2=idch(i+1)                                                     
C                                                                       
C---->THE FOLLOWING TWO FORTRAN STATEMENTS ARE EXPEDIENTS               
C     FOR OVERCOMING THE POSSIBILITIES OF ZERO SUBSCRIPTS!              
C                                                                       
          if(j1 .eq. 0) j1 = 1                                          
          if(j2 .eq. 0) j2 = 1                                          
      dcj(i1) = vdcnew(j1)                                              
      dcj(i1+1) = crsto(j1,j2)                                          
      dcj(i1+2) = vdcnew(j2)                                            
 9025 i1 = i1 + 3                                                       
      call ritecs (vdcnew, necsv, nbus)                                 
      call ritecs (dcj, necsj, i1-1)                                    
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 9026                                             
      if (keybrd(22).ne.0) then                                         
         write (outbuf,9035) i1                                         
         call prtout (1)                                                
         write (outbuf,19035) (idch(i),i=1,20)                          
         call prtout (1)                                                
         do 39035 jjj = 1,30,10                                         
            kkk = min0 (jjj+9,30)                                       
            write (outbuf,29035) (dcj(i),i=jjj,kkk)                     
            call prtout (1)                                             
39035    continue                                                       
      endif                                                             
 9035     format('0BEFORE EXITING FROM DCSUB0 IDCH,DCJ ARRAYS',         
     1           i10)                                                   
19035     format(1x, 20i6)                                              
29035     format(1x, 10e13.5)                                           
 9026 continue                                                          
C                                                                       
C     PRINT OUT LS FLAG INDICATORS FOR DEBUGGING                        
C                                                                       
      if(lppwr.gt.noith.or.lppwr.lt.noitl.or.to.gt.timh.or.             
     1to.lt.timl)go to 9044                                             
      if(keybrd(22).eq.0)go to 9044                                     
          write (outbuf, 9036)                                          
          call prtout (1)                                               
          do 39036 i = 1,10                                             
             write (outbuf, 19036) ibrtbl(1, i), ibrtbl(2, i)           
             call prtout (1)                                            
39036     continue                                                      
          call skipln (1)                                               
          write (outbuf, 29036) idcnt, idcnt1, lsflg, kflt, irecl       
          call prtout (1)                                               
 9036     format(' IBRTBL(1, I) IBRTBL(2, I)   IDCNT  IDCNT1  LSFLG',   
     1    '  KFLT  IRECL')                                              
19036     format(2x, i11, 1x, i11)                                      
29036     format(4x, i4, 4x,  i4, 3x, i4, 2x, i4, 3x, i4)               
      write (outbuf,9037)                                               
      call prtout (1)                                                   
      do 29037 jjj = 1,nterm,10                                         
         kkk = min0 (jjj+9,nterm)                                       
         write (outbuf,19037) (idcf(i),i=jjj,kkk)                       
         call prtout (1)                                                
29037 continue                                                          
 9037     format(' IDCF AT S9037')                                      
19037     format(10(i4, 2x))                                            
      write (outbuf,9038)                                               
      call prtout (1)                                                   
      do 29038 jjj = 1,nterm,10                                         
         kkk = min0 (jjj+9,nterm)                                       
         write (outbuf,19038) (idcf1(i),i=jjj,kkk)                      
         call prtout (1)                                                
29038 continue                                                          
 9038     format(' IDCF1 AT S9038')                                     
19038     format(10(i4, 2x))                                            
 9044 continue                                                          
      return                                                            
      end                                                               
