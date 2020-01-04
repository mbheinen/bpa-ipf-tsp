C    %W% %G%
      subroutine dcint                                                  
C * * *                                                                 
C * * * THIS SUBROUTINE INITIALIZES THE TWO TERMINAL DC TABLES          
C * * * IT IS CALLED BY INITL4. IT CALLS GAMINT AND MODINT TO           
C * * * INITIALIZE DC MODULATION.                                       
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/namec.inc' 
      include 'tspinc/cntrl2.inc' 
      include 'tspinc/toler.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/contrl.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/ectba.inc' 
      include 'tspinc/ecstbb.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/newton.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/gamma.inc' 
      include 'tspinc/dcblk.inc' 
        equivalence(erio,tab(164)),(eroo,tab(165)),(eron,tab(166)),     
     1             (etio,tab(167)),(eioo,tab(168)),(eion,tab(169)),     
     2             (ectim1,tab(148)),(ectim2,tab(149))                  
        dimension zoecs(512)                                            
        character*8 ibus1,dcbus1,dcbus2,name,name1,name2                
      pi=3.14159265                                                     
      f135=3.0*sqrt(2.0)/pi                                             
      rpib3=3.0/pi                                                      
      rpib18=18.0/(pi*pi)                                               
      sqr2=sqrt(2.0)                                                    
      zero=0.0                                                          
      ipwr=2                                                            
      k1=0                                                              
      do 100 itrr = 1,20                                                
      idcblk(itrr) = 0                                                  
 100  continue                                                          
      do 3820 iidc=1,ldc                                                
      locdc=kdc+k1*idcl                                                 
      call redecs (ptab,locdc,idcl)                                     
C * * *                                                                 
C * * * IF ITAB(129) = 0 THE DYNAMICS OF THIS LINE ARE BLOCKED          
C * * *                                                                 
      if(itab(129) .ne. 0)idcblk(iidc) = 1                              
C****                                                                   
C* YISOLN, NEWTON OPTION                                                
C****                                                                   
C** REMOVE ADMITT. FROM DIAGONAL AND SAVE                               
      go to (3010, 3000) , inewts                                       
 3000 i1 = itab(35)                                                     
      call  getmat (i1, ii)                                                 
      atrow(ii-1) = atrow(ii-1) - tab(38)                               
      atrow(ii  ) = atrow(ii  ) - tab(39)                               
      gadmit(i1) =  tab(38)                                             
      badmit(i1) =  tab(39)                                             
      call putmat (i1, ii)                                                  
      i1 = itab (37)                                                    
      call getmat(i1, ii)                                                   
      atrow(ii-1) = atrow(ii-1) - tab(40)                               
      atrow(ii  ) = atrow(ii  ) - tab(41)                               
      gadmit(i1) =  tab(40)                                             
      badmit(i1) =  tab(41)                                             
      call putmat (i1, ii)                                                  
      go to  3010                                                       
 3010 dcbus1 = exnamc(itab(35))                                         
      dcbus2 = exnamc(itab(37))                                         
      if (keybrd(22).eq.0) go to 3100                                   
      write (outbuf,3060) iptab(1),iptab(3),iptab(7),iptab(33),iptab(34)
      call prtout (1)                                                   
      do 3062 jjj = 1,45,6                                              
         kkk = min0 (jjj+5,45)                                          
         write (outbuf,3061) (i,ptab(i),i=jjj,kkk)                      
         call prtout (1)                                                
 3062 continue                                                          
 3060 format('0IPTAB(1,3,7,33,34),PTAB(1,...,45)=',5i5)                 
 3061 format(6(i5,e16.5))                                               
      write (outbuf,3080) itab(7),itab(22),itab(33),itab(34),itab(35),  
     1  itab(36),itab(37)                                               
      call prtout (1)                                                   
      do 3082 jjj = 1,120,6                                             
         kkk = min0 (jjj+5,120)                                         
         write (outbuf,3081) (i,tab(i),i=jjj,kkk)                       
         call prtout (1)                                                
 3082 continue                                                          
 3080 format('0ITAB(7,22,34,35,36,37),TAB(1,...,120)=',7i5)             
 3081 format(6(i5,e16.5))                                               
      if(itab(114) .le. 0) go to 3100                                   
      itabl = 177                                                       
      if(itab(114) .eq. 6) itabl= 205                                   
      do 3091 jjj = 121,itabl,6                                         
         kkk = min0 (jjj+5,itabl)                                       
         write (outbuf,3090) (i,tab(i),i=jjj,kkk)                       
         call prtout (1)                                                
 3091 continue                                                          
 3090 format(6(i5,e16.5))                                               
 3790 format(8(i4,e13.4))                                               
 3100 continue                                                          
      ovbse1=ptab(2)                                                    
      ovbse2=ptab(4)                                                    
C * * *                                                                 
C     SWAP TIME CONS T2,T3 IN TAB TABLE                                 
C * * *                                                                 
      tt2=tab(4)                                                        
      tab(4)=tab(5)                                                     
      tab(5)=tt2                                                        
      tt2=tab(19)                                                       
      tab(19)=tab(20)                                                   
      tab(20)=tt2                                                       
C * * *                                                                 
C     CONVERT MH TO HENRY AND UF TO FARAD..                             
C     SCALE HENRY*FRQBSE FOR SECONDS TO CYCLE                           
C * * *                                                                 
      ptab(9)=ptab(9)*0.001*frqbse                                      
      ptab(10)=ptab(10)*0.000001*frqbse                                 
      ptab(27)=ptab(27)*0.001*frqbse                                    
      ptab(28)=ptab(28)*0.001*frqbse                                    
C * * *                                                                 
C     CONVERT AMPS TO KILOAMPS                                          
C * * *                                                                 
      ptab(11)=ptab(11)*.001                                            
      ptab(14)=ptab(14)*.001                                            
      ptab(17)=ptab(17)*.001                                            
      ptab(39)=ptab(39)*.001                                            
      tab(99)=ptab(35)*ovbse1*ptab(12)                                  
      tab(100)=ptab(36)*ovbse2*ptab(15)                                 
      tab(11)=ptab(30)*ptab(12)                                         
      tab(12)=ptab(29)*ptab(12)                                         
      tab(26)=ptab(32)*ptab(15)                                         
      tab(27)=ptab(31)*ptab(15)                                         
      conv1=ptab(2)*ptab(12)*rpib3*sqr2/ptab(14)                        
      conv2=ptab(4)*ptab(15)*rpib3*sqr2/ptab(17)                        
      tab( 6)= -tab( 6)*conv1                                           
      tab(21)= -tab(21)*conv2                                           
C * * *                                                                 
C     CONVERT MARGIN SW UNIT CONSTANT C=72 AMP TO KILOAMPS              
C * * *                                                                 
      tab(50) = 72.0*0.001                                              
      tab(70) = 72.0*0.001                                              
C * * *                                                                 
C     SET PROPER PMAX                                                   
C * * *                                                                 
      if(itab(114) .gt. 0) go to 40090                                  
      if (tab(117).eq.0.0) tab(117)=ptab(5)                             
40090 continue                                                          
C * * *                                                                 
C     DETERMINE IMAX USED IN POWER FLOW                                 
C * * *                                                                 
      crate=amin1(ptab(11),amin1(ptab(14),ptab(17)))                    
C * * *                                                                 
C     CONVERT IMARGIN TO KILOAMP                                        
C * * *                                                                 
      tab(9)=tab(9)*crate                                               
      tab(24)=tab(24)*crate                                             
      if(tab( 9).eq.0.0)tab( 9)= tab(24)                                
      if(tab(24).eq.0.0)tab(24)= tab( 9)                                
      ptab(11)=crate                                                    
C * * *                                                                 
C     RECTIFIER INIT COND                                               
C * * *                                                                 
      i1=iptab(33)                                                      
      e1 = eyr(i1)                                                      
      f1 = eyi(i1)                                                      
      eo1=sqrt(e1*e1+f1*f1)*tab(99)                                     
C * * *                                                                 
C     INITIAL  DELAY COMMUTATION VOLTAGES                               
C * * *                                                                 
      erio= eo1                                                         
      eroo= eo1                                                         
      eron= eo1                                                         
      cosa1=cos(ptab(22))                                               
      tab(95)=cosa1                                                     
      dc1=f135*eo1*cosa1-ptab(39)*(rpib3*tab(11)+tab(12)*rpib18)-ptab(13
     1)                                                                 
      tab(65)=dc1                                                       
      tab(53)=dc1                                                       
      tab(110)=dc1                                                      
      tab(68)=eo1                                                       
      tab(92)=eo1                                                       
      tab(54)=ptab(39)                                                  
      dcb1=dc1-tab(32)*tab(55)                                          
      dv1=dcb1                                                          
C * * *                                                                 
C     SETUP CMAX,CMARGIN                                                
C * * *                                                                 
      if (tab(8).eq.tab(23)) go to 3140                                 
      write (errbuf(1),3120) dcbus1,dcbus2                              
      call prterr ('E',1)                                               
 3120 format (1x,'MISMATCH IN IMAX',2a8)                                
      tab(8)=amin1(tab(8),tab(23))                                      
 3140 tab(8)=tab(8)*crate                                               
      if (tab(8).eq.0.0) tab(8)=tab(23)*crate                           
      if (tab(8).eq.0.0) tab(8)=crate                                   
      tab(23)=tab(8)                                                    
      fid1=ptab(39)                                                     
      tab(102)=fid1                                                     
      if(ptab(39) .gt. tab(8))then                                      
         write(errbuf,3125) dcbus1,dcbus2                               
         call prterr('E',1)                                             
 3125    format (1h0,5x,' INITIAL CURRENT EXCEEDS MAXIMUM CURRENT FOR ',
     1   a8, ' TO ',a8)                                                 
         iabort = 1                                                     
      endif                                                             
      if (itab(7).eq.1) fid1=ptab(5)/dv1                                
      fipm1=amin1(amax1(0.1*ptab(11),dv1*tab(8)/(0.25*ptab(6))),tab(8)) 
      tab(88)=fipm1                                                     
      tab(51)=0.0                                                       
      cosgo=cos(ptab(25))                                               
C * * *                                                                 
C     INITIALIZE D.E. STORAGE                                           
C * * *                                                                 
      cx1=2.0*tab(11)*tab(54)*rpib3                                     
      eop1=eo1*sqr2*rpib3                                               
      vpa1=eop1*(cos(ptab(22))+cosgo)-cx1                               
      zero=0.0                                                          
      zmax1=eop1*(cosgo+cos(ptab(18)))-cx1                              
C * * *                                                                 
C     TEST IF WITHIN LIMITS                                             
C * * *                                                                 
          if(vpa1 .le. zmax1+1.0e-4 .and. vpa1+1.0e-4 .ge. 0.0)         
     1      go to 3180                                                  
      write (errbuf(1),3160) vpa1,zero,zmax1                            
      call prterr ('E',1)                                               
 3160 format (46h0valve 1 cntrl limit violation...vpa1,min,max=,3e15.6) 
      iabort=1                                                          
 3180 continue                                                          
C * * *                                                                 
C * * * REGULATOR STATE VARIABLES FOR FULL DC MODEL                     
C * * *                                                                 
      if(itab(119).eq.1) then                                           
        tab(58)=vpa1                                                    
        tab(57)=vpa1                                                    
        tab(56)=vpa1                                                    
        tab(55)=vpa1/tab(6)                                             
        tab(103)=vpa1/tab(6)                                            
        tab(115) = cx1+vpa1                                             
      endif                                                             
C * * *                                                                 
C     INVERTER INIT COND                                                
C * * *                                                                 
      i1=iptab(34)                                                      
      e2 = eyr(i1)                                                      
      f2 = eyi(i1)                                                      
      eo2=sqrt(e2*e2+f2*f2)*tab(100)                                    
C * * *                                                                 
C     INITIALIZE DELAY COMMUTATION VOLTAGES                             
C * * *                                                                 
      eioo= eo2                                                         
      eion= eo2                                                         
      ectim1=ectim1*frqbse                                              
      ectim2=ectim2*frqbse                                              
C * * *                                                                 
C     GET INITIAL ALPHA FOR INVERTER                                    
C * * *                                                                 
      tab(60)=ptab(39)                                                  
      cx2=2.0*tab(26)*tab(60)*rpib3                                     
      eop2=eo2*sqr2*rpib3                                               
      cosa2=cx2/eop2-cos(ptab(26))                                      
      tab(96)=cosa2                                                     
      dc2=-f135*eo2*cosa2+ptab(39)*(rpib3*tab(26)+tab(27)*rpib18)+ptab(1
     16)                                                                
      tab(66)=dc2                                                       
      tab(59)=dc2                                                       
      tab(111)=dc2                                                      
      tab(69)=eo2                                                       
      tab(94)=eo2                                                       
      dcb2=dc2-(tab(32)-ptab(8))*tab(60)                                
      dv2=dcb2                                                          
      fid2=ptab(39)                                                     
      if (itab(22).eq.1) fid2=ptab(5)/dv2                               
      fipm2=amin1(amax1(0.1*ptab(11),dv2*tab(23)/(0.25*ptab(6))),tab(23)
     1)                                                                 
      tab(89)=fipm2                                                     
      tab(52)=0.0                                                       
      cosgam=cos(ptab(26))                                              
      vpa2=eop2*(cosa2+cosgam)-cx2                                      
      zmax2=eop2*(cosgam+cos(ptab(24)))-cx2                             
C * * *                                                                 
C     TEST IF WITHIN LIMITS                                             
C * * *                                                                 
          if(vpa2 .le. zmax2+1.0e-4 .and. vpa2+1.0e-4 .ge. 0.0)         
     1      go to 3220                                                  
      write (errbuf(1),3200) vpa2,zero,zmax2                            
      call prterr ('E',1)                                               
 3200 format (47h0valve 2 cntrl limit violation... vpa2,min,max=,3e15.6)
      iabort=1                                                          
 3220 continue                                                          
C * * *                                                                 
C * * * REGULATOR STATE VARIABLES FOR FULL DC MODEL                     
C * * *                                                                 
      if(itab(119).eq.1) then                                           
        tab(64)=vpa2                                                    
        tab(63)=vpa2                                                    
        tab(62)=vpa2                                                    
        tab(61)=vpa2/tab(21)                                            
        tab(104)=vpa2/tab(21)                                           
        tab(116) = cx2                                                  
      end if                                                            
      itab(33)=1                                                        
      tab(67)=ptab(39)                                                  
      tab(157)=tab(67)                                                  
      tab(156)=tab(157)                                                 
C * * *                                                                 
C * * * INITIALIZE EXTINCTION ANGLES                                    
C * * *                                                                 
      tab(121) = acos(tab(95) -(sqr2*tab(11)*ptab(39))/tab(68))         
      tab(122) = acos(tab(96) -(sqr2*tab(26)*ptab(39))/tab(69))         
C * * *                                                                 
C     CONVERT TIME CONS.TO PROPER UNITS                                 
C * * *                                                                 
      do 3240 i=1,5                                                     
 3240 tab(i)=tab(i)*frqbse                                              
      do 3260 i=16,20                                                   
 3260 tab(i)=tab(i)*frqbse                                              
C     TEST IF WITHIN LIMITS                                             
C  *********** ************** ************** ***************            
C ****  ERROR CHECK FOR T3 REMOVED 12/15/83 TRR                         
C      IF (TAB(4).NE.0.0.AND.TAB(19).NE.0.0) GO TO 3300                 
C      WRITE (ERRBUF(1),3280)                                           
C      CALL PRTERR ('E',1)                                              
C 3280 FORMAT (34H0DC CNTRL CKT TIME CONS T2 IS ZERO)                   
C      IABORT=1                                                         
C *****  ******************** ************ *********** **************   
 3300 idcap=1                                                           
C * * *                                                                 
C     SET UP DC LINE DATA + CODES                                       
C     READ SWITCHING TABLES FOR USE IN DC REP                           
C * * *                                                                 
      intmd=1                                                           
      pleft=0.5                                                         
      pright=0.5                                                        
C * * *                                                                 
C     CK IF THERE IS ANY INTERMEDIATE FLT                               
C * * *                                                                 
      do 3320 i=1,ifcd                                                  
      if (mflt(i).ne.5) go to 3320                                      
      idflt = iabs(ipcdtn(i))                                           
C * * *                                                                 
C * * * CHECK TO SEE IF MANUAL RAMPING AGREES WITH CONTROL MODE         
C * * *                                                                 
      if(idflt .ne. 7 .and. idflt .ne. 8) go to 3309                    
      if(iftabn(i).ne.itab(35) .and. iftabn(i).ne.tab(37))go to 3320    
      if(itab(7) .eq. 1 .and. idflt .eq. 7)go to 3320                   
      if(itab(7) .eq. 2 .and. idflt .eq. 8)go to 3320                   
      write (errbuf(1),3308) dcbus1,dcbus2                              
      call prterr ('E',1)                                               
 3308 format (1h0,2x,'CONTROL MODE DOES NOT AGREE WITH MANUAL RAMPING', 
     1' TYPE ON LINE SWITCHING CARD ',5x,a8,5x,a8)                      
      iabort=1                                                          
      go to 3320                                                        
C * * *                                                                 
C     CK IF DC LINE MATCHES DC ITERMED FLT                              
C * * *                                                                 
 3309 if(iftabn(i).eq.itab(35).and.jftabn(i).eq.itab(37))go to 3310     
      if(jftabn(i).ne.itab(35).or. iftabn(i).ne.itab(37))go to 3320     
 3310 if (idflt.ne.3) go to 3320                                        
      if (perct(i).le.0.0.or.perct(i).ge.100.0) go to 3340              
      pleft=perct(i)/100.0                                              
      pright=1.0-pleft                                                  
      intmd=2                                                           
      fltx(i)=-1.0                                                      
      go to 3380                                                        
 3320 continue                                                          
      go to 3380                                                        
 3340 write (errbuf(1),3360)                                            
      call prterr ('E',1)                                               
 3360 format ('0ILLEGAL PERCENT ON DC INTERMED FLT CD')                 
      iabort=1                                                          
 3380 sri=ptab(27)*ptab(28)                                             
      ptab(10)=0.0                                                      
      if (ptab(10).eq.0.0.and.ptab(9).eq.0.0.and.sri.eq.0.) go to 3420  
      if (ptab(10).eq.0.0.and.sri.ne.0.0) go to 3440                    
      if (ptab(10).ne.0.0.and.sri.ne.0.0) go to 3460                    
      write (errbuf(1),3400) dcbus1,dcbus2                              
      call prterr ('E',1)                                               
 3400 format (24h0illegal dc line data...,a8,1h-,a8)                    
      iabort=1                                                          
C * * *                                                                 
C     DC LINE RESISTANCE ONLY                                           
C * * *                                                                 
 3420 tab(14)=ptab(8)                                                   
      idcap=3                                                           
      go to 3520                                                        
C * * *                                                                 
C     NO CAP                                                            
C * * *                                                                 
 3440 tab(14)=ptab(8)*pleft                                             
      tab(29)=ptab(8)*pright                                            
      tab(13)=ptab(9)*pleft                                             
      tab(28)=ptab(9)*pright                                            
      idcap=2                                                           
      go to 3520                                                        
 3460 continue                                                          
 3480 tab(13)=ptab(9)*pleft*0.5                                         
      tab(28)=ptab(9)*pright*0.5                                        
      tab(14)=ptab(8)*pleft*0.5                                         
      tab(29)=ptab(8)*pright*0.5                                        
      tab(15)=ptab(10)*pleft                                            
      tab(30)=ptab(10)*pright                                           
C * * *                                                                 
C     GET INITIAL CHARGE ACROSS CAP                                     
C * * *                                                                 
 3500 dd1=dc1-tab(14)*tab(62)                                           
      tab(63)=dd1*tab(15)                                               
      dd2=dd1-(tab(14)+tab(29))*tab(62)                                 
      tab(66)=dd2*tab(30)                                               
      tab(97)=0.0                                                       
      tab(98)=0.0                                                       
C * * *                                                                 
C     STORE INITIAL VALUES                                              
C * * *                                                                 
 3520 do 3540 i=1,17                                                    
 3540 tab(i+70)=tab(i+50)                                               
C * * *                                                                 
C     STORE IDCAP,IDCF CODES                                            
C * * *                                                                 
      itab(105)=1                                                       
      iblck=1                                                           
      itab(106)=idcap                                                   
      tab(101)=1.0/bmva                                                 
      do 3560 i=31,43                                                   
 3560 ptab(i)=0.0                                                       
      ptab(34)=ptab(6)*tab(102)                                         
      if(itab(7).eq.2) ptab(34)=tab(102)                                
      do 3580 i=129,138                                                 
 3580 tab(i)=0.0                                                        
      do 3581 i=1,8                                                     
 3581 dcadd(i)=0.0                                                      
C * * *                                                                 
C * * * INITALIZE MARGIN SWITCHING TIMERS IF PTAB(36) = -1. THERE IS    
C * * * NO MARGIN SWITCHING                                             
C * * *                                                                 
         ptab(36) = 0.0                                                 
         if(ptab(20) .eq. -1.0)ptab(36)= -1.                            
         ptab(20) = 0.0                                                 
         ptab(37) = 0.25*frqbse                                         
         ptab(38) = 0.5*frqbse                                          
C * * *                                                                 
C * * * INITIALIZE MODULATION TABLES IF THEY EXIST                      
C * * *                                                                 
      if (itab(114) .eq. 0) go to 3720                                  
      if(itab(114) .eq. 5) then                                         
C * * *                                                                 
C * * * INITIALIZE TABLES FOR GAMMA MODULATION                          
C * * *                                                                 
         igam = itab(118)                                               
C * * * INITALIIZE INVERTER BUS NUMBER                                  
                                                                        
         igamno(igam) = itab(36)                                        
         vdcgam = tab(111)                                              
C * * * INITALIZE GAMMA EXTINCTION ANGLE FROM POWER FLOW                
         gama(igam) = ptab(26)                                          
         call gamint(igam,vdcgam)                                       
      else                                                              
C * * *                                                                 
C * * * INITIALIZE TABLES FOR OTHER MODULATION TYPES                    
C * * *                                                                 
         imod = itab(118)                                               
         call modint(imod)                                              
      endif                                                             
                                                                        
C * * *                                                                 
C     STORE INITIAL DESIRED CURRENT                                     
C * * *                                                                 
 3720 tab(107)=ptab(5)/(tab(73)-tab(32)*tab(74))                        
      if (itab(7).ne.1) tab(107)=ptab(39)                               
      tab(108)=ptab(5)/(tab(79)-(tab(32)-ptab(8))*tab(80))              
      if (itab(22).ne.1) tab(108)=ptab(39)                              
C * * *                                                                 
C     SETUP PROPER VOLTAGE FOR DC LINE WITH OR WITHOUT CAP.             
C * * *                                                                 
      dc11=f135*eo1*cosa1-ptab(13)                                      
      dc22=-f135*eo2*cosa2+ptab(16)                                     
      if(itab(106).eq.2) go to 3725                                     
      tab(65)=dc11                                                      
      tab(85)=dc11                                                      
      tab(53)=dc11                                                      
      tab(110)=dc11                                                     
      tab(66)=dc22                                                      
      tab(86)=dc22                                                      
      tab(59)=dc22                                                      
      tab(111)=dc22                                                     
 3725 continue                                                          
      if (keybrd(22).eq.0) go to 3800                                   
      write (outbuf,3740) iptab(1),iptab(3),iptab(7),iptab(33),iptab(34)
      call prtout (1)                                                   
      do 3742 jjj = 1,45,6                                              
         kkk = min0 (jjj+5,45)                                          
         write (outbuf,3741) (i,ptab(i),i=jjj,kkk)                      
         call prtout (1)                                                
 3742 continue                                                          
 3740 format('0IPTAB(1,3,7,33,34),PTAB(1,...,45)=',5i5)                 
 3741 format(6(i5,e16.5))                                               
      write (outbuf,3760) itab(119),itab(120)                           
      call prtout (1)                                                   
 3760 format (' DC CNTRL BR',2i5)                                       
      write (outbuf,3780) itab(7),itab(22),itab(33),itab(34),itab(35),  
     1   itab(36),itab(37)                                              
      call prtout (1)                                                   
      do 3782 jjj = 1,177,6                                             
         kkk = min0 (jjj+5,177)                                         
         write (outbuf,3781) (i,tab(i),i=jjj,kkk)                       
         call prtout (1)                                                
 3782 continue                                                          
 3780 format('0ITAB(7,22,34,35,36,37),TAB(1,...,120)=',7i5)             
 3781 format(6(i5,e16.5))                                               
      if(itab(114) .ne. 6) go to 3800                                   
      do 3791 jjj = 178,205,6                                           
         kkk = min0 (jjj+5,205)                                         
         write (outbuf,3790) ( i,tab(i),i=jjj,kkk)                      
         call prtout (1)                                                
 3791 continue                                                          
 3800 call ritecs (ptab,locdc,idcl)                                     
      k1=k1+1                                                           
 3820 continue                                                          
      return                                                            
      end                                                               
