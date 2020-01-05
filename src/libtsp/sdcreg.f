C    %W% %G%
       subroutine sdcreg                                                
C * * *                                                                 
C * * * THIS SUBROUTINE SIMULATES A SIMPLIFIED DC CONVERTER CONTROLLER  
C * * * MODEL WHICH ASSUMES CURRENT REGULATORS BEING IDEAL              
C * * * IT IS CALLED BY SDCLIN                                          
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
                                                                        
       real                                                             
     * cordr,cordi,cito,tvd,rc1,req1,rc2,req2,tlr,tll,tl,dtl,           
     * cdcso,cdcst,ciit,cilt,eoo1,eoo2,es1,es2,edc1,edc2,               
     * dvtll,dvlsr,dvlsi                                                
C * * *                                                                 
C         **** OBTAIN PROPER MEASUREMENTS  ****                         
C * * *                                                                 
C     ERON IS THE DELAYED COMMUTATION VOLTAGE RECTIFIER                 
C     EION IS THE DELAYED COMMUTATION VOLTAGE INVERTER                  
C                                                                       
C       ECI1= ERON                                                      
C       ECI2= EION                                                      
C                                                                       
C     TAB(73) IS THE DC TERMINAL VOLTAGE RECTIFIER                      
C     TAB(79) IS THE DC TERMINAL VOLTAGE INVERTER                       
C                                                                       
       dcb1=tab(73)                                                     
       dcb2=tab(79)+ptab(8)*tab(80)                                     
       dv1=dcb1                                                         
       dv2=dcb2                                                         
C                                                                       
C     FDI1 IS THE MEASURED DC CURRENT AT THE RECTIFIER                  
C     FDI2 IS THE MEASURED DC CURRENT AT THE INVERTER                   
C                                                                       
C      FDI1=TAB(74)                                                     
C      FDI2=TAB(80)                                                     
C * * *                                                                 
C        ***** DETERMINE DESIRED CURRENT SETTINGS *****                 
C     FOR RECTIFIER                                                     
C     FID1 : DESIRED DC CURRENT                                         
C * * *                                                                 
      if(itab(7).eq.2) then                                             
C                                                                       
C        CONSTANT CURRENT MODE                                          
C        PTAB(34) : DESIRED CURRENT AT THE RECTIFIER                    
C        PTAB(31) :  ANY MANUAL CURRENT ORDER CHANGE                    
C                                                                       
         fid1=ptab(34) + ptab(31)                                       
                                                                        
         else                                                           
C        CONSTANT POWER MODE                                            
C        PTAB(5) : SCHEDULED DC POWER                                   
C        PTAB(31) : ANY MANUAL POWER ORDER CHANGE ENTERED BY LS CARDS   
C                                                                       
         icde = itab(114)                                               
         if(icde .eq. 3 .or. icde .eq. 4 .or. icde .eq. 6)then          
C           HIGH LEVEL MODULATION                                       
C           SIGHI : HIGH LEVEL MODULATION SIGNAL                        
            imodkt = itab(118)                                          
            sighi = sighim(imodkt)                                      
            porder = ptab(34) + sighi + ptab(31)                        
            else                                                        
            porder = ptab(34) + ptab(31)                                
            endif                                                       
                                                                        
         fid1= porder /dv1                                              
                                                                        
         end if                                                         
                                                                        
C     FOR INVERTER                                                      
                                                                        
      if(itab(22).eq.2) then                                            
C                                                                       
C        CONSTANT CURRENT MODE AT THE INVERTER                          
         fid2=ptab(34) + ptab(31)                                       
                                                                        
         else                                                           
C        CONSTANT POWER MODE AT THE INVERTER                            
         fid2=porder /dv2                                               
                                                                        
         end if                                                         
                                                                        
      tab(107)=fid1                                                     
      tab(108)=fid2                                                     
C                                                                       
C          ***** VOLTAGE CONTROLLED CURRENT LIMIT  *****                
                                                                        
C     IF THE DC TERMINAL VOLTAGE IS LESS THAN A CERTAIN % OF THE RATED  
C     TERMINAL VOLTAGE, THE MAXIMUM CURRENT MUST BE LIMITED             
C     FIPM1 IS MAXIMUM CURRENT ALLOWED BY THE CURRENT LIMITER           
C     FIMIN1 IS THE MINIMUM RATED CURRENT                               
C                                                                       
      fipm1=tab(8)                                                      
      fimin1=0.1*ptab(11)                                               
      vschr = tab(146) * ptab(6)                                        
      vschi = tab(147) * ptab(6)                                        
      if (dv1.le.vschr) then                                            
        fipm1 = fimin1 + (fipm1-fimin1)*dv1 / vschr                     
        end if                                                          
                                                                        
      fipm2=tab(23)                                                     
      if (dv2.le.vschi) then                                            
         fipm2 = fimin1 + (fipm2-fimin1)*dv2 / vschi                    
         end if                                                         
                                                                        
      tab(88)=fipm1                                                     
      tab(89)=fipm2                                                     
C                                                                       
C     FIORD1 IS THE ORDERED CURRENT                                     
C                                                                       
      fiord1=amax1(amin1(fipm1,fid1),fimin1)                            
      fiord2=amax1(amin1(fipm2,fid2),fimin1)                            
C                                                                       
C     ADD DELTA I FROM LOW LEVEL MODULATION SIGNAL IF IT EXISTS         
C                                                                       
      if(itab(114) .eq. 1 .or. itab(114) .eq. 2) then                   
          imodkt = itab(118)                                            
          siglo = siglom(imodkt)                                        
          fiord1 = fiord1 + siglo                                       
          endif                                                         
                                                                        
C           *****  SET PROPER RECT,INVERT PARAM    *****                
                                                                        
      mdedc=itab(33)                                                    
      if(mdedc.ne.1) then                                               
C        REVERSAL OF POWER FLOW                                         
C        INV--RECT                                                      
         link = 13                                                      
         write(errbuf(1),101)                                           
  101    format(/10x,'**** ERROR : REVERSAL OF POWER FLOW ON A DC LINE',
     *               'REPRESENTED BY SIMPLIFIED MODEL')                 
         call prterr('E',1)                                             
         return                                                         
         end if                                                         
C                                                                       
C        RECT--INVERT                                                   
         cmarg1=0.0                                                     
         cmarg2=tab(24)                                                 
         cosmn1= cos(ptab(18))                                          
         if( iblck.eq.2) cosmn1=ablck                                   
         cosmn2= cos(ptab(24))                                          
C                                                                       
C     *** COMPUTE INPUT TO CURRENT REGULATORS : CORDR & CORDI  *****    
                                                                        
C     TAB(71) IS THE MARGIN SWITCHING CONSTANT                          
C     FIORD1 IS THE ORDERED OR DESIRED CURRENT                          
C     CMARG1 IS THE CURRENT MARGIN                                      
C                                                                       
      cordr = fiord1 - cmarg1 + tab(71)                                 
      cordi = fiord2 - cmarg2 + tab(72)                                 
                                                                        
C      TAB(75)=CORDR                                                    
C      TAB(81) = CORDI                                                  
C                                                                       
C         *****  CALCULATE THE CONSTANTS / PARAMETERS   ******          
C                                                                       
      cosgo1=cos(ptab(25))                                              
C                                                                       
C     IF GAMMA MODULATION IS PRESENT CHANGE THE INVERTER EXTINCTION     
C     ANGLE TO THE OUTPUT OF THE MODULATOR                              
C                                                                       
      if(itab(114) .eq. 5)then                                          
        igam = itab(118)                                                
        ptab(26) = gama(igam)                                           
        endif                                                           
      cosgo2=cos(ptab(26))                                              
                                                                        
C     IDEAL COMMUTATION VOLTAGES                                        
      rpsq=rpib3*sqr2                                                   
      eoo1 = rpsq*tab(92)                                               
      eoo2 = rpsq*tab(94)                                               
      eo1=rpsq*tab(68)                                                  
      eo2=rpsq*tab(69)                                                  
                                                                        
C     LINE CURRENT OF THE PREVIOUS TIME STEP                            
      cito = tab(67)                                                    
                                                                        
C     TOTAL VOLTAGE DROP ACROSS THE VALVES                              
      tvd = ptab(13) + ptab(16)                                         
                                                                        
C     TOTAL RESISTANCE, REACTANCE & TIME CONSTANT OF DC EQUIV. CIRCUIT  
      rc1 = rpib3*tab(11)                                               
      req1 = rpib18*tab(12)                                             
      rc2 = rpib3*tab(26)                                               
      req2 = rpib18*tab(27)                                             
      tlr = ptab(8) + rc1 + req1 - rc2 + req2                           
                                                                        
      tll = ptab(9) + ptab(27) + ptab(28)                               
                                                                        
      tl = tll/tlr                                                      
      dtl = 2.*tl/edt                                                   
                                                                        
C            **** DETERMINE CONROL SCHEME ,ICSCH ****                   
C                                                                       
C     CONTROL SCHEME IS DETERMINED FOR THE FIRST ITERATION AND IS ASSUME
C     TO BE THE SAME FOR THE REST OF THE ITERATIONS TO PREVENT OSCILLATI
C     HENCE IMPROVE CONVERGENCE IN LPPWR ITERATIONS                     
                                                                        
      if(lppwr.eq.1) then                                               
                                                                        
C        CALCULATE THE THE DC LINE CURRENT FOR TRANSITION MODE          
C        (IE, CIA - CEA CONTROL FOR REG. - INV. RESPECTIVELY )          
         cdcso = (eoo1*cosmn1 - eoo2*cosgo2 - tvd)/tlr                  
         cdcst = (eo1*cosmn1 - eo2*cosgo2 - tvd)/tlr                    
         ciit = (cdcso + cdcst + (dtl - 1.)*cito)/(dtl + 1.)            
                                                                        
         if(ciit.gt.cordr) then                                         
C           CC - CEA  CONTROL                                           
            icsch = 1                                                   
                                                                        
            else                                                        
            if(ciit.lt.cordi) then                                      
C              CIA - CC CONTROL                                         
               icsch = 2                                                
                                                                        
               else                                                     
C              CIA - CEA  CONTROL                                       
               icsch = 3                                                
               endif                                                    
            endif                                                       
                                                                        
         else                                                           
         icsch = itab(120)                                              
         end if                                                         
                                                                        
C         **** CALCULATE THE NEW DC QUANTITIES ****                     
                                                                        
      lima2 = 0                                                         
      go to (1,2,3,4), icsch                                            
                                                                        
   1     continue                                                       
C        CC - CEA CONTROL                                               
         cilt = cordr                                                   
         cosa1 = (eo2*cosgo2 + tvd + tlr*cilt)/eo1                      
         cosa2 = (2.*rc2*cilt - eo2*cosgo2)/eo2                         
                                                                        
         if(cosa1.gt.cosmn1) then                                       
            icsch = 4                                                   
            go to 4                                                     
                                                                        
            else                                                        
            if(cosa2.gt.cosmn2) then                                    
              write(errbuf(1),103)                                      
  103         format(10x,'**** INV IGNITION ANGLE < ASTOP, ',           
     *                   'COMMUTATION MAY FAIL  ****')                  
              call prterr('W',1)                                        
              endif                                                     
            go to 10                                                    
            endif                                                       
                                                                        
    2    continue                                                       
C        CIA - CC CONTROL                                               
         cilt = cordi                                                   
         cosa1 = cosmn1                                                 
         cosa2 = -( eo1*cosmn1 - tvd - (tlr + 2.*rc2)*cilt )/eo2        
                                                                        
         if(cosa2.gt.cosmn2) then                                       
C           LIMIT INV FIRING ANGLE                                      
            lima2 = 3                                                   
            cosa2 = cosmn2                                              
            tlr = tlr + 2.*rc2                                          
            tl = tll/tlr                                                
            dtl = 2.*tl/edt                                             
            cdcso = (eoo1*cosmn1 + eoo2*cosmn2 - tvd)/tlr               
            cdcst = (eo1*cosmn1 + eo2*cosmn2 - tvd)/tlr                 
            cilt = (cdcso + cdcst + (dtl - 1.)*cito)/(dtl + 1.)         
            end if                                                      
                                                                        
         go to 10                                                       
                                                                        
    3    continue                                                       
C        CIA - CEA CONTROL                                              
         cdcso = (eoo1*cosmn1 - eoo2*cosgo2 - tvd)/tlr                  
         cdcst = (eo1*cosmn1 - eo2*cosgo2 - tvd)/tlr                    
         cilt = (cdcso + cdcst + (dtl - 1.)*cito)/(dtl + 1.)            
         cosa1 = cosmn1                                                 
         cosa2 = (2.*rc2*cilt - eo2*cosgo2)/eo2                         
         go to 10                                                       
                                                                        
    4    continue                                                       
C        CC - CIA CONTROL                                               
C        SET INV FIRING ANGLE TO PREVIOUS VALUE                         
         cilt = cordr                                                   
         cosa2 = tab(98)                                                
         cosa1 = (-eo2*cosa2 + tvd + (tlr+2.*rc2)*cilt)/eo1             
                                                                        
   10 continue                                                          
C                                                                       
C      LIMIT COSINE OF FIRING ANGLE TO ABS VALUE .LT.1                  
C                                                                       
      if(cosa1.lt.-1.0)cosa1=-1.0                                       
      if(cosa1.gt.1.0)cosa1=1.0                                         
C                                                                       
      if(cosa2.lt.-1.0)cosa2=-1.0                                       
      if(cosa2.gt.1.0)cosa2=1.0                                         
                                                                        
C     CALCULATE VOLTAGE AT THE SMOOTHING REACTOR                        
      es1 = eo1*cosa1 - (rc1 + req1)*cilt - ptab(13)                    
      es2 = -eo2*cosa2 + (rc2 + req2)*cilt + ptab(16)                   
                                                                        
C     CACULATE CONVERTER TERMINAL VOLTAGES                              
      dvlsr = 0.                                                        
      dvlsi = 0.                                                        
      if(icsch.eq.3.or.lima2.eq.3) then                                 
C        ADD VOLTAGE DROP ON THE SMOOTHING REACTOR                      
         dvtll = es1 - es2 - ptab(8)*cilt                               
         dvlsr = dvtll*ptab(27)/tll                                     
         dvlsi = dvtll*ptab(28)/tll                                     
         end if                                                         
      edc1 = es1 - dvlsr                                                
      edc2 = es2 + dvlsi                                                
                                                                        
C     STORE THE CALCULATED VARIABLES                                    
      if(lppwr.eq.1) itab(120) = icsch                                  
      tab(95) = cosa1                                                   
      tab(96) = cosa2                                                   
      tab(87) = cilt                                                    
      tab(157) = cilt                                                   
      tab(110) = edc1                                                   
      tab(111) = edc2                                                   
      tab(85) = es1                                                     
      tab(86) = es2                                                     
                                                                        
C * * *                                                                 
C     WRITE THE RESULTS AS DEBUG                                        
C * * *                                                                 
      if(keybrd(33) .ne. 0)then                                         
         write(outbuf,106)icsch,cosa1,cosa2,cilt,es1,es2,edc1,edc2      
 106     format(10x,i4,7f10.5)                                          
         call prtout(1)                                                 
      endif                                                             
                                                                        
      return                                                            
      end                                                               
