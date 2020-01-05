C    %W% %G%
        subroutine dcreg                                                
C * * *                                                                 
C * * * THIS SUBROUTINE CALCULATES THE DIFFERENTIAL EQUATIONS           
C * * * FOR THE CURRENT REGULATOR MODEL USED IN THE TWO                 
C * * * TERMINAL DC LINE MODEL                                          
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/blkcom2.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/ecstbj.inc' 
      include 'tspinc/ectba.inc' 
      include 'tspinc/mdctbl.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/rk.inc' 
      include 'tspinc/gamma.inc' 
      include 'tspinc/dcmodd.inc' 
      include 'tspinc/vrgov.inc' 
        dimension   drv(14)                                             
       equivalence(erio, tab(164)),(eroo,tab(165)),(eron,tab(166)),     
     1            (eiio,tab(167)),(eioo,tab(168)),(eion,tab(169)),      
     2            (ectimc,tab(148))                                     
      equivalence   (tmpdy,drv)                                         
          equivalence (tmpy(1), nsubt), (tmpy(2), nstmx), (tmpy(3),     
     1                 timh), (tmpy(4), timl), (tmpy(5), noith),        
     2                (tmpy(6), noitl), (tmpy(7), loopdc)               
       common/toler/ divn,delang, twodt                                 
C                                                                       
C     OBTAIN PROPER COMM VOLTAGE                                        
C     ERON IS THE DELAYED COMMUTATION VOLTAGE RECTIFIER                 
C     EION IS THE DELAYED COMMUTATION VOLTAGE INVERTER                  
C                                                                       
       eci1= eron                                                       
       eci2= eion                                                       
C                                                                       
C TAB(73) IS THE DC TERMINAL VOLTAGE RECTIFIER                          
C TAB(79) IS THE DC TERMINAL VOLTAGE INVERTER                           
C                                                                       
      dcb1=tab(73)                                                      
      dcb2=tab(79)+ptab(8)*tab(80)                                      
      dv1=dcb1                                                          
      dv2=dcb2                                                          
C * * *                                                                 
C * * * ITAB(7) = 1 CONSTANT POWER MODE                                 
C * * * ITAB(7) = 2 CONSTANT CURRENT MODE                               
C * * *                                                                 
      if(itab(7).eq.1) go to 80                                         
C                                                                       
C FOR CONSTANT CURRENT MODE, FID1 IS THE POWER FLOW  DC CURRENT         
C IT IS THE DESIRED CURRENT AT THE RECTIFIER (PTAB(34)) PLUS            
C ANY MANUAL CURRENT ORDER CHANGE (PTAB(31))                            
C                                                                       
      fid1=ptab(34) + ptab(31)                                          
      go to 120                                                         
C                                                                       
C FOR CONSTANT POWER MODE, PORDER IS THE SUM OF THE HIGH LEVEL          
C MODULATION SIGNAL, SIGHI, THE SCHEDULED DC POWER, PTAB(5),            
C AND ANY MANUAL POWER ORDER CHANGE ENTERED BY LS CARDS,PTAB(31)        
C                                                                       
  80  icde = itab(114)                                                  
      if(icde .eq. 3 .or. icde .eq. 4 .or. icde .eq. 6)then             
      imodkt = itab(118)                                                
      sighi = sighim(imodkt)                                            
      porder = ptab(34) + sighi + ptab(31)                              
      go to 119                                                         
      endif                                                             
  100 porder = ptab(34) + ptab(31)                                      
C                                                                       
C FOR CONSTANT POWER MODE THE DERSIRED CURRENT AT THE RECTIFIER IS THE  
C POWER ORDER  DIVIDED BY THE DC VOLTAGE                                
C                                                                       
  119 fid1= porder /dv1                                                 
      if(dv1 .le. 0.0)fid1 = 0.0                                        
  120 if (itab(22).eq.1) go to 140                                      
C                                                                       
C CONSTANT CURRENT MODE AT THE INVERTER IS ALSO EQUAL TO THE POWER      
C FLOW SOLUTION CURRENT PLUS AND MANUAL CURRENT ORDER CHANGE            
C                                                                       
      fid2=ptab(34) + ptab(31)                                          
      go to 160                                                         
C                                                                       
C CONSTANT POWER MODE AT THE INVERTER THE DESIRED CURRENT IS THE POWER  
C ORDER DIVIDED BY THE TERMINAL VOLTAGE                                 
C                                                                       
  140 fid2=porder /dv2                                                  
      if(dv2 .le. 0.0)fid2 = 0.0                                        
  160 tab(107)=fid1                                                     
      tab(108)=fid2                                                     
C                                                                       
C IF THE DC TERMINAL VOLTAGE IS LESS THAN A CERTAIN % OF THE RATED      
C  TERMINAL VOLTAGE, THE MAXIMUM CURRENT MUST BE LIMITED                
C                                                                       
      fipm1=tab(8)                                                      
      fimin1=0.1*ptab(11)                                               
      vschr = tab(146) * ptab(6)                                        
      vschi = tab(147) * ptab(6)                                        
      if (dv1.gt.vschr) go to 180                                       
      fipm1 = fimin1 + (fipm1-fimin1)*dv1 / vschr                       
      if(dv1 .le. 0.0)fipm1 = fimin1                                    
  180 tab(88)=fipm1                                                     
      fipm2=tab(23)                                                     
C             ***  FIND UPPER LIMIT FOR CURRENT ORDER  ***              
      if (dv2.gt.vschi) go to 200                                       
      fipm2 = fimin1 + (fipm2-fimin1)*dv2 / vschi                       
      if(dv2 .le. 0.0)fipm2 = fimin1                                    
C                                                                       
C             ***  FIND CURRENT ORDER  ***                              
C                                                                       
  200 tab(89)=fipm2                                                     
C                                                                       
C FIPM1 IS MAXIMUM CURRENT ALLOWED BY THE CURRENT LIMITER               
C FID1 IS THE DESIRED CURRENT                                           
C FIMIN1 IS THE MINIMUM RATED CURRENT                                   
C FIORD1 IS THE ORDERED CURRENT                                         
C                                                                       
      if(ptab(31) .ne. 0.0)fimin1 = 0.0                                 
      fiord1=amax1(amin1(fipm1,fid1),fimin1)                            
      fiord2=amax1(amin1(fipm2,fid2),fimin1)                            
C                                                                       
C  ADD DELTA I FROM LOW LEVEL MODULATION SIGNAL IF IT EXISTS            
C                                                                       
      if(itab(114) .eq. 1 .or. itab(114) .eq. 2) then                   
          imodkt = itab(118)                                            
          siglo = siglom(imodkt)                                        
          fiord1 = fiord1 + siglo                                       
      endif                                                             
C                                                                       
C FDI1 IS THE MEASURED DC CURRENT AT THE RECTIFIER                      
C FDI2 IS THE MEASURED DC CURRENT AT THE INVERTER                       
C                                                                       
      fdi1=tab(74)                                                      
      fdi2=tab(80)                                                      
C     SET PROPER RECT,INVERT PARAM                                      
      mdedc=itab(33)                                                    
      go to (220,240), mdedc                                            
C                                                                       
C     RECT--INVERT                                                      
C                                                                       
  220 cmarg1=0.0                                                        
      cmarg2=tab(24)                                                    
       cosmn1= cos(ptab(18))                                            
       if( iblck.eq.2) cosmn1=ablck                                     
       cosmn2= cos(ptab(24))                                            
      go to 260                                                         
C                                                                       
C      INV--RECT                                                        
C                                                                       
  240 cmarg1=tab(9)                                                     
      cmarg2=0.0                                                        
       cosmn1= cos(ptab(19))                                            
       cosmn2= cos(ptab(23))                                            
       if( iblck.eq.2) cosmn2= ablck                                    
C                                                                       
C COMPUTE LIMIT TO OUTPUT OF CURRENT REGULATOR                          
C                                                                       
  260 cosgo1=cos(ptab(25))                                              
C * * *                                                                 
C * * * IF GAMMA MODULATION IS PRESENT CHANGE THE INVERTER EXTINCTION   
C * * * ANGLE TO THE OUTPUT OF THE MODULATOR                            
C * * *                                                                 
      if(itab(114) .eq. 5)then                                          
        igam = itab(118)                                                
        ptab(26) = gama(igam)                                           
      endif                                                             
      cosgo2=cos(ptab(26))                                              
      rpsq=rpib3*sqr2                                                   
       eo1=rpsq*eci1                                                    
       eo2=rpsq*eci2                                                    
       csched = ptab(5)/ptab(6)                                         
       vdrp1= 2.0*tab(11)*fdi1*rpib3                                    
C * * *                                                                 
C * * * USE THE ORDERED CURRENT AT THE RECTIFIER FOR THE INVERTER       
C * * * COMMUTATING VOLTAGE DROP CALCULATION TO INCREASE NUMERICAL      
C * * * STABILITY                                                       
C * * *                                                                 
       vdrp2= 2.0*tab(26)*fiord1*rpib3                                  
       dmax1= eo1*(cosgo1+cosmn1)-vdrp1                                 
       dmax2= eo2*(cosgo2+cosmn2)-vdrp2                                 
C                                                                       
C COMPUTE INPUT TO CURRENT REGULATOR, VP1                               
C FDI1 IS THE MEASURED DC CURRENT                                       
C TAB(71) IS THE MARGIN SWITCHING CONSTANT                              
C FIORD1 IS THE ORDERED OR DESIRED CURRENT                              
C CMARG1 IS THE CURRENT MARGIN                                          
C TAB(103) IS THE REFERENCE SIGNAL                                      
C                                                                       
      vp1=fdi1-tab(71)-fiord1+cmarg1+tab(103)                           
  270 tab(75)=vp1                                                       
      vp2=fdi2-tab(72)-fiord2+cmarg2+tab(104)                           
      tab(81)=vp2                                                       
C                                                                       
C CALCULATE THE OUTPUT OF THE CURRENT REGULATOR IN FOUR STEPS:          
C V12 FROM VP1; V13 FROM V12; AND V14 (OUTPUT) FROM V13                 
C SET UP TIME CONSTANT FACTORS                                          
C TAB(3) = T1R TAB(5) = T2R TAB(4) = T3R                                
C TAB(18)= T1I TAB(20)= T2I TAB(19)= T3I                                
C                                                                       
      twodt=2.0*divn/edt                                                
      tt12=twodt*tab(5)                                                 
      ttp12=tt12+1.0                                                    
      ttm12=tt12-1.0                                                    
      tt13=twodt*tab(4)                                                 
      ttp13=tt13+1.0                                                    
      ttm13=tt13-1.0                                                    
      tt22=twodt*tab(20)                                                
      ttp22=tt22+1.0                                                    
      ttm22=tt22-1.0                                                    
      tt11=twodt*tab(3)                                                 
      ttp11=tt11+1.0                                                    
      ttm11=tt11-1.0                                                    
      tt23=twodt*tab(19)                                                
      ttp23=tt23+1.0                                                    
      ttm23=tt23-1.0                                                    
      tt21=twodt*tab(18)                                                
      ttp21=tt21+1.0                                                    
      ttm21=tt21-1.0                                                    
      v12 = (tab(6)*(vp1+dcsto(3)) + dcsto(4)*ttm12)/ttp12              
      tab(76)=v12                                                       
c     -  V13 not needed for further calcs, so set to zero               !dem
      v13 = 0                                                           !dem
      v14 = (v12*ttp11-dcsto(4)*ttm11 + dcsto(6)*ttm13)/ttp13           
      tab(77)=v13                                                       
      tab(78)=v14                                                       
C                                                                       
C     TEST FOR MAX                                                      
C                                                                       
      if (v14.le.dmax1) go to 280                                       
      v14=dmax1                                                         
      go to 300                                                         
C                                                                       
C     TEST FOR MIN                                                      
C                                                                       
  280 if (v14.ge.0.0) go to 320                                         
      v14=0.0                                                           
C                                                                       
C     SETUP VARIABLES FOR LIMIT CONDITION                               
C                                                                       
  300 tab(78)=v14                                                       
      tab(77)=v14                                                       
      tab(76)=v14                                                       
      tab(75)=v14/tab(6)                                                
C                                                                       
C     VALVE 2 EQN                                                       
C                                                                       
C                                                                       
 320  v22 = (tab(21)*(vp2 + dcsto(9)) +dcsto(10)*ttm22)/ttp22           
      v24 = (v22*ttp21 - dcsto(10)*ttm21 + dcsto(12)*ttm23)/ttp23       
      tab(82)=v22                                                       
c     -  V23 not needed for further calcs, so set to zero               !dem
      v23 = 0                                                           !dem
      tab(83)=v23                                                       
      tab(84)=v24                                                       
C                                                                       
C     TEST FOR MAX                                                      
C                                                                       
      if (v24.le.dmax2) go to 340                                       
      v24=dmax2                                                         
      go to 360                                                         
C                                                                       
C     TEST FOR MIN                                                      
C                                                                       
  340 if (v24.ge.0.0) go to 380                                         
      v24=0.0                                                           
C                                                                       
C     SETUP VARIABLES FOR LIMIT CONDITION                               
C                                                                       
  360 tab(84)=v24                                                       
      tab(83)=v24                                                       
      tab(82)=v24                                                       
      tab(81)=v24/tab(21)                                               
C                                                                       
C     CALC. COSINE ALPHA                                                
C                                                                       
  380  valph1=v14+vdrp1                                                 
      tab(115) = valph1                                                 
      eo11=eo1                                                          
      if (eo11.eq.0.0) eo11=0.0001                                      
      cosa1=valph1/eo11-cosgo1                                          
C                                                                       
C      LIMIT COSINE OF FIRING ANGLE TO ABS VALUE .LT.1                  
C                                                                       
      if(cosa1.lt.-1.0)cosa1=-1.0                                       
      if(cosa1.gt.1.0)cosa1=1.0                                         
      valph2=v24+vdrp2                                                  
      tab(116) = valph2                                                 
      eo22=eo2                                                          
      if (eo22.eq.0.0) eo22=0.0001                                      
      cosa2=valph2/eo22-cosgo2                                          
C                                                                       
C      LIMIT COSINE OF FIRING ANGLE TO ABS VALUE .LT.1                  
C                                                                       
      if(cosa2.lt.-1.0)cosa2=-1.0                                       
      if(cosa2.gt.1.0)cosa2=1.0                                         
      tab(95)=cosa1                                                     
      tab(96)=cosa2                                                     
C * * *                                                                 
C * * * CALCULATE EXTINCTION ANGLE FOR OUTPUT                           
C * * *                                                                 
      gama1 = (vdrp2/eo22) - tab(96)                                    
      if(gama1 .gt. 1.0)gama1 = 1.                                      
      if(gama1 .lt. -1.0)gama1 = -1.                                    
      tab(122) = pi - acos(gama1)                                       
      return                                                            
      end                                                               
