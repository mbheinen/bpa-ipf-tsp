C    %W% %G%
        subroutine sdclin(iter)                                         
                                                                        
C     ******************************************************************
C                     SDCLIN & SDCREG                                   
C     - IMPLEMENS THE SIMPLIFIED TWO TERMINAL DC LINE MODEL             
C     - SUBSTITUTE FOR DCLIN & DCREG WHICH IMPLEMENT THE FULL MODEL     
C     - IT IS ASSUMED THAT THE DC SYSTEM IS FAST ENOUGH  COMPARED TO    
C       AC SYSTEM SO THAT CONTROLLERS ON DC LINE CAN BE ASSUMED IDEAL . 
C     - THE MODEL IS INTENDED TO BE USED WHEN THE DISTURBANCE IS NOT CLO
C       TO THE LINE . THUS THE MODEL CANNOT BE USED IF THERE IS A FAULT 
C       THE LINE                                                        
C     - SINCE THE CONTROLLERS ARE ASSUMED IDEAL , THE SUBITERATIONS ARE 
C       ELIMINATED .                                                    
C       SDCLIN IS CALLED BY DERIV                                       
C        MESUT BARAN                                      JULY 1986     
C     ******************************************************************
      include 'tspinc/params.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/blkcom2.inc' 
      include 'tspinc/contrl.inc' 
      include 'tspinc/cntrl2.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/ectba.inc' 
      include 'tspinc/mdctbl.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/newton.inc' 
      include 'tspinc/fltopt.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/rk.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/gamma.inc' 
       common /toler/ divn,delang,twodt                                 
       common /netwk/ imax,cmax,dsum                                    
       dimension drv(14)                                                
       equivalence (drv,tmpdy)                                          
       equivalence (tmpy(1), nsubt), (tmpy(2), nstmx), (tmpy(3),        
     1                 timh), (tmpy(4), timl), (tmpy(5), noith),        
     2                (tmpy(6), noitl), (tmpy(7), loopdc)               
       equivalence(itab(106),mdcl),(itab(105),mdcflt)                   
C ..EC DELAY...                                                         
       equivalence(erio, tab(164)),(eroo,tab(165)),(eron,tab(166)),     
     1            (eiio,tab(167)),(eioo,tab(168)),(eion,tab(169)),      
     2            (ectim1,tab(148)),(ectim2,tab(149))                   
      include 'tspinc/busvolt.inc' 
      include 'tspinc/bcur.inc' 
      include 'tspinc/dcmodd.inc' 
      include 'tspinc/namec.inc' 
       character*8 name1,name2                                          
                                                                        
C     ----------------------  BEGIN  ---------------------------------- 
                                                                        
                                                                        
C                                                                       
        if(mdcflt.ne.1) then                                            
                                                                        
C          *** ERROR : FAULT ON A DC LINE WHICH IS TO BE REPRESENTED    
C                      BY SIMPLIFIED DC LINE MODEL                      
           link = 13                                                    
           write(errbuf(1),101)                                         
  101      format(/10x,'**** ERROR : FAULT ON A DC LINE REPRESENTED ',  
     *                 'BY SIMPLIFIED MODEL ')                          
           call prterr('E',1)                                           
           return                                                       
           end if                                                       
                                                                        
        imdp=itab(114)                                                  
        if (imdp .gt. 0 .and. imdp .ne. 5) then                         
                                                                        
C          CALCULATE HIGH LEVEL, LOW LEVEL, OR DUAL                     
C          FREQUENCY MODULATION SIGNAL                                  
C                                                                       
           idcmod = itab(118)                                           
           call dcmod(idcmod,imdp,iter)                                 
           endif                                                        
                                                                        
        if (iter.eq.1 .and. lppwr .eq. 0) then                          
C                                                                       
C          INITIALIZE DELTA CURR. STORAGE...                            
           ptab(29)=0.0                                                 
           ptab(30)=0.0                                                 
           cmax=10.0                                                    
           if (idsw.eq.7) go to 2000                                    
           tab(92)=tab(68)                                              
           tab(94)=tab(69)                                              
           go to 2000                                                   
           end if                                                       
C                                                                       
C            *****  GET COMMUTATION VOLTAGE   ******                    
C                                                                       
        ieo1=itab(34)                                                   
        evr1 = eyr(ieo1)                                                
        evi1 = eyi(ieo1)                                                
        ecsq1=evr1*evr1+evi1*evi1                                       
        eco1=sqrt(ecsq1)*tab(99)                                        
        if (eco1.eq.0.0) eco1=0.0001                                    
                                                                        
C       TEST FOR ZERO VALVESIDE VOLT                                    
        ivo1=itab(35)                                                   
        ev1 = eyr(ivo1)                                                 
        fv1 = eyi(ivo1)                                                 
        evsq1=ev1*ev1+fv1*fv1                                           
        if (evsq1.lt.0.0001) eco1=0.0001                                
                                                                        
        ieo2=itab(36)                                                   
        evr2 = eyr(ieo2)                                                
        evi2 = eyi(ieo2)                                                
        ecsq2=evr2*evr2+evi2*evi2                                       
        eco2=sqrt(ecsq2)*tab(100)                                       
        if (eco2.eq.0.0) eco2=0.0001                                    
                                                                        
C       TEST FOR ZERO VALVESIE VOLT                                     
        ivo2=itab(37)                                                   
        ev2 = eyr(ivo2)                                                 
        fv2 = eyi(ivo2)                                                 
        evsq2=ev2*ev2+fv2*fv2                                           
        if (evsq2.lt.0.0001) eco2=0.0001                                
                                                                        
        tab(68)=eco1                                                    
        tab(69)=eco2                                                    
C                                                                       
C       *** AT A DISCONTINUITY JUMP TO AC/DC INTERFACE EQUATIONS ***    
C                                                                       
        if (idsw .eq. 7) go to 1685                                     
                                                                        
C            ****  FOR THE FIRST ITERATION OF A TIME STEP  *****        
C                                                                       
        if(lppwr .eq. 1)  then                                          
C                                                                       
C          **** UPDATE MARGIN SWITCHING UNIT VALUES  ****               
C          BYPASS MARGIN SW LOGIC IF PTAB(36)=-1.0                      
                                                                        
           if (ptab(36).ne.-1.0) then                                   
C                                                                       
C             CHECK MSU TIMER                                           
              if(ptab(36).eq.0.0)go to 404                              
C               DECREMENT TIMER                                         
                ptab(36)=ptab(36)-ddt2                                  
                if(ptab(36).gt.0.001)go to 1180                         
C                  RESET MSU TIMER                                      
                   ptab(36)=ptab(38)                                    
                   go to 406                                            
C             SET INITIAL MSU TIMER                                     
  404         ptab(36)=ptab(37)                                         
              go to 1180                                                
  406         if ( tab(51) .eq. 0.0) go to 440                          
              if (tab(74).gt.tab(88)+tab(50)) go to 460                 
              if (tab(74).gt.tab(107)+0.75*tab(9)) go to 460            
  420         tab(71)=tab(9)                                            
              go to 520                                                 
  440         if (tab(74).ge.tab(88)-tab(9)) go to 460                  
              if (tab(74).lt.tab(107)-0.75*tab(9)) go to 420            
  460         tab(71)=0.0                                               
                                                                        
  520         continue                                                  
              if (tab(52).eq.0.0) go to 620                             
              if (tab(80).gt.tab(89)+tab(70)) go to 640                 
              if (tab(80).gt.tab(108)+0.75*tab(24)) go to 640           
  500         tab(72)=tab(24)                                           
              go to 1180                                                
  620         if (tab(80).ge.tab(89)-tab(24)) go to 640                 
              if (tab(80).lt.tab(108)-0.75*tab(24)) go to 500           
  640         tab(72)=0.0                                               
                                                                        
              end if                                                    
                                                                        
 1180      continue                                                     
                                                                        
C          ******   PRECESS VARIABLES    ******                         
                                                                        
           do 1200 i=1,17                                               
 1200         tab(50+i)=tab(70+i)                                       
           tab(112)=tab(110)                                            
           tab(113)=tab(111)                                            
           tab(97)=tab(95)                                              
           tab(98)=tab(96)                                              
           tab(156)=tab(157)                                            
           eroo= eron                                                   
           eioo= eion                                                   
                                                                        
           delm = edt                                                   
                                                                        
           end if                                                       
C                                                                       
C               ****   STORE PRESENT DC INFO  ****                      
C                                                                       
        do 1240 i=1,15                                                  
 1240     dcsto(i) = tab(52 + i)                                        
        dcsto(16) = tab(92)                                             
        dcsto(17) = tab(94)                                             
          erio= tab(92)                                                 
          eiio= tab(94)                                                 
        dcsto(18) = tab(112)                                            
        dcsto(19) = tab(113)                                            
        dcsto(20) = tab(97)                                             
        dcsto(21) = tab(98)                                             
        cosa1 = tab(97)                                                 
        cos1 = tab(97)                                                  
        cosa2 = tab(98)                                                 
        cos2 = tab(98)                                                  
        dcsto(27) = tab(156)                                            
        dcsto(29) = eroo                                                
        dcsto(30) = eioo                                                
                                                                        
        difvr=tab(68)-tab(92)                                           
        difvi=tab(69)-tab(94)                                           
        totdel=delm                                                     
C                                                                       
C          *** CHECK IF GAMMA MODULATION IS PRESENT  ***                
C                                                                       
        imdp = itab(114)                                                
        if(imdp .eq. 5)then                                             
                                                                        
C          SOLVE GAMMA DIFFERENTIAL EQUATIONS                           
           igam = itab(118)                                             
           vac(igam) = sqrt(ecsq2)                                      
           call gamsol(igam)                                            
           endif                                                        
                                                                        
        ce1=f135                                                        
        ce2=f135                                                        
        twodt=2.0/delm                                                  
        fmpy=totdel/edt                                                 
C                                                                       
C       DCSTO(16) AND ECRIN ARE THE AC COMMUTATING VOLTAGES FOR RECT    
C       DCSTO(17) AND ECIIN ARE THE AC COMMUTATING VOLTAGES FOR INV     
C                                                                       
        dcsto(16) = tab(92) + fmpy*difvr                                
        dcsto(17) = tab(94) + fmpy*difvi                                
        ecrin= tab(92)+ fmpy*difvr                                      
        eciin= tab(94)+ fmpy*difvi                                      
C                                                                       
C       **** CALCULATE MEASURED COMMUTATION VOLTAGE ****                
C                                                                       
        twoec1= twodt*ectim1                                            
        ecpls1= twoec1+ 1.0                                             
        ecmns1= twoec1- 1.0                                             
        twoec2= twodt*ectim2                                            
        ecpls2= twoec2+ 1.0                                             
        ecmns2= twoec2- 1.0                                             
C                                                                       
C       ERON IS THE DELAYED COMMUTATION VOLTAGE RECTIFIER               
C       EION IS THE DELAYED COMMUTATION VOLTAGE INVERTER                
C                                                                       
        eron = (ecrin + erio + ecmns1*dcsto(29))/ecpls1                 
        eion = (eciin + eiio + ecmns2*dcsto(30))/ecpls2                 
                                                                        
        loopdc=0                                                        
C       ****************   START OF ITERATIVE LOOP  ********************
C       DUE TO FEEDBACK CREATED BY DC VOLTAGE MEASUREMENT               
C       A SUCCESSIVE ITERATION SCHEME OVER FEEDBACK VARIABLE (DC VOLTAGE
C       MEASUREMENT) IS USED TO SOLVE THE EQUATIONS                     
C       ****************************************************************
                                                                        
 1360   continue                                                        
                                                                        
        loopdc=loopdc+1                                                 
C * * *                                                                 
C * * * WRITE ITERATIONS FOR DEBUG                                      
C * * *                                                                 
        if(keybrd(33) .ne. 0)then                                       
           write(outbuf,103) loopdc                                     
  103      format(10x,'DC LINE ITR',i4)                                 
           call prtout(1)                                               
        endif                                                           
        if (loopdc.gt.20) then                                          
C                                                                       
C          ITERATIVE LOOP HAS BEEN EXHAUSTED, EXIT                      
C                                                                       
           name1=exnamc(ivo1)                                           
           name2 = exnamc(ivo2)                                         
           write (errbuf(1),1380) name1,name2                           
                                                                        
           call prterr('E',1)                                           
 1380      format('0TWO TERM DC ITER LIMIT REACHED IN SIMPLIFIED MODEL' 
     *            ,5x,a8,5x,a8)                                         
           link=13                                                      
C                                                                       
           return                                                       
           end if                                                       
                                                                        
C                                                                       
C       ***** CALCULATE NEW ESTIMATE OF DC QUANTITIES *****             
C                                                                       
C       SDCREG CALCULATES :                                             
C          DC LINE CURRENT                                              
C          CONTROL SCHEME OF CONTROLLERS                                
C          FIRING                                                       
C          TERMINAL VOLTAGES OF DCLINE / CONVERTERS                     
C          VOLTAGES AT THE SMOOTHING REACTOR                            
C                                                                       
           call sdcreg                                                  
                                                                        
        cosa1 = tab(95)                                                 
        cosa2 = tab(96)                                                 
C                                                                       
C       EDC1 IS THE DC TERMINAL VOLTAGE OF RECTIFIER                    
C       EDC2 IS THE DC TERMINAL VOLTAGE OF INVERTER                     
C                                                                       
        edc1 = tab(110)                                                 
        edc2 = tab(111)                                                 
C                                                                       
C       *** LOGIC TO REPRESENT DC VOLTAGE MEASURING DEVICES ***         
C                                                                       
        twotv1=twodt*tab(2)                                             
        tvp1=twotv1+1.0                                                 
        tvm1=twotv1-1.0                                                 
        twotv2=twodt*tab(17)                                            
        tvp2=twotv2+1.0                                                 
        tvm2=twotv2-1.0                                                 
        twotc1=twodt*tab(1)                                             
        tcp1=twotc1+1.0                                                 
        tcm1=twotc1-1.0                                                 
        twotc2=twodt*tab(16)                                            
        tcp2=twotc2+1.0                                                 
        tcm2=twotc2-1.0                                                 
C                                                                       
C       TAB(73) IS THE OUTPUT OF THE DC VOLTAGE MEASURING DEVICE RECTIFI
C       TAB(79) IS THE OUTPUT OF THE DC VOLTAGE MEASURING DEVICE INVERTE
C                                                                       
          if(tab( 2).eq.0.0) then                                       
            tab(73)=edc1                                                
            else                                                        
            tab(73) = (dcsto(1)*tvm1 + edc1 + dcsto(18))/tvp1           
            end if                                                      
                                                                        
          if(tab(17).eq.0.0) then                                       
             tab(79)=edc2                                               
             else                                                       
             tab(79) = (dcsto(7)*tvm2 + edc2 + dcsto(19))/tvp2          
             end if                                                     
                                                                        
C                                                                       
C       **** TEST FOR DC FIRING ANGLE CONVERGENCE  ****                 
C                                                                       
        difang = abs(cos1-cosa1) + abs(cos2-cosa2)                      
C                                                                       
        if(difang.gt.0.001) then                                        
C                                                                       
C          CONTINUE TO ITERATE                                          
           cos1 = cosa1                                                 
           cos2 = cosa2                                                 
                                                                        
           go to 1360                                                   
                                                                        
           else                                                         
C                                                                       
C          *** TERMINATE SUB ITERATIONS  ***                            
C          COS ESTIMATES ARE WITHIN TOLERANCE                           
C                                                                       
           cc1=tab(87)                                                  
           cc2= tab(157)                                                
           ec1=f135*eco1                                                
           dco1=ec1*tab(95)-rpib3*cc1*tab(11)                           
           ec2=f135*eco2                                                
           dco2=-ec2*tab(96)+rpib3*cc2*tab(26)                          
C * * *                                                                 
C * * * CALCULATE EXTINCTION ANGLE FOR OUTPUT                           
C * * *                                                                 
           tab(121) = acos(tab(95) -(sqr2*tab(11)*tab(87))/tab(68))     
           tab(122) = acos(tab(96) -(sqr2*tab(26)*tab(87))/tab(69))     
            go to 1695                                                  
                                                                        
           end if                                                       
                                                                        
 1685   continue                                                        
                                                                        
C        *** UPDATE LINE CURRENT PARAMETERS AT A DISCONTINUITY ***      
C                                                                       
          cc1= tab(87)                                                  
          cc2= tab(157)                                                 
          ec1=f135*eco1                                                 
          dco1=ec1*tab(95)-cc1*rpib3*tab(11)                            
          ec2=f135*eco2                                                 
          dco2=-ec2*tab(96)+rpib3*cc2*tab(26)                           
          es1=dco1-cc1*rpib18*tab(12)-ptab(13)                          
          tab(85)=es1                                                   
          dcsto(13) = es1                                               
          es2=dco2+cc2*rpib18*tab(27)+ptab(16)                          
          tab(86)=es2                                                   
          dcsto(14) = es2                                               
C                                                                       
C         PTAB(27) = LSR ; PTAB(28) = LSI TAB(13) = LR TAB(28) = LI     
C         TOTL IS THE  TOTAL LINE INDUCTANCE                            
C                                                                       
          totl = ptab(27) +tab(13) +tab(28) +ptab(28)                   
          if(totl .le. .00001) totl = 1.                                
          totlr=1.0/totl                                                
C                                                                       
C         TAB(14) + TAB(15) IS THE TOTAL LINE RESISTANCE                
C         DES12 IS THE VOLTAGE DIFFERENCE ACROSS THE LINE FOLLOWING     
C         A DISCONTINUITY.                                              
C                                                                       
          des12=es1-(tab(14)+tab(29))*cc1-es2                           
          tab(110)=es1-des12*ptab(27)*totlr                             
          tab(111)=es2+des12*ptab(28)*totlr                             
          dcsto(18) = tab(110)                                          
          dcsto(19) = tab(111)                                          
                                                                        
          if(imdp .eq. 5) then                                          
            igam = itab(118)                                            
            vaco1(igam) = sqrt(ecsq2)                                   
            endif                                                       
                                                                        
 1695  continue                                                         
C                                                                       
C          *****  AC/DC INTERFACE EQUATIONS   *****                     
                                                                        
       dco11=dco1-rpib18*cc1*tab(12)                                    
       dco22=dco2+rpib18*cc2*tab(27)                                    
       arg1=dco1/ec1                                                    
       if (abs(arg1).gt.1.0) arg1=1.0                                   
       fe1=acos(arg1)                                                   
C                                                                       
C      ***  YISOLN, NEWTON OPTION   ***                                 
                                                                        
       go to ( 1700, 1710), inewts                                      
 1710    go to ( 1700, 1730), mdeyoj                                    
                                                                        
 1700    tab(46)=-dco11*cc1*tab(101)                                    
         tab(47)=-(dco1*tan(fe1)-rpib18*tab(11)*cc1)*cc1*tab(101)       
                                                                        
         if (evsq1.le. 0.0001) then                                     
                                                                        
            tab(42)=0.0                                                 
            tab(43)=0.0                                                 
                                                                        
            else                                                        
           gnet1=tab(46)/evsq1+tab(38)                                  
           bnet1=-tab(47)/evsq1+tab(39)                                 
           tab(42)=ev1*gnet1-fv1*bnet1                                  
           tab(43)=ev1*bnet1+fv1*gnet1                                  
           endif                                                        
                                                                        
        arg2=dco2/ec2                                                   
        if (abs(arg2).gt.1.0) arg2=1.0                                  
        fe2=acos(arg2)                                                  
        tab(48)=dco22*cc2*tab(101)                                      
        tab(49)=-(dco2*tan(fe2)-rpib18*tab(26)*cc2)*cc2*tab(101)        
                                                                        
        if (evsq2.le. 0.0001) then                                      
                                                                        
           tab(44)=0.0                                                  
           tab(45)=0.0                                                  
                                                                        
           else                                                         
           gnet2=tab(48)/evsq2+tab(40)                                  
           bnet2=-tab(49)/evsq2+tab(41)                                 
           tab(44)=ev2*gnet2-fv2*bnet2                                  
           tab(45)=ev2*bnet2+fv2*gnet2                                  
           endif                                                        
                                                                        
           go to 100                                                    
C                                                                       
C       ****  NEWTON ADDITION   ****                                    
C                                                                       
 1730   tab(46) = - dco11*cc1*tab(101)                                  
        tab(47) = -(dco1*tan(fe1)-rpib18*tab(11)*cc1)*cc1*tab(101)      
        i1 = itab(35)                                                   
        if (evsq1.gt. 0.0001) go to 1735                                
        tab(42) = 0.0                                                   
        tab(43)= 0.0                                                    
        gnewt(2*i1-1) = tab(38)                                         
        gnewt(2*i1  ) = tab(38)                                         
        bnewt(2*i1-1) = - tab(39)                                       
        bnewt(2*i1  ) = tab(39)                                         
                                                                        
        go to 1740                                                      
C                                                                       
C       FORM CURR. VECTOR + DIAGONAL ADDITION FOR NEWTON METHOD         
 1735   tab(42) = 2.*(tab(46)*ev1 + tab(47)*fv1)/evsq1                  
        tab(43) = 2.*(tab(46)*fv1 - tab(47)*ev1)/evsq1                  
        edif = ev1*ev1 - fv1*fv1                                        
        esq  = evsq1*evsq1                                              
        gnewt(2*i1-1) = (tab(46)*edif + 2.*tab(47)*ev1*fv1)/esq         
        gnewt(2*i1  ) = - gnewt(2*i1-1)                                 
        bnewt(2*i1-1) = (-tab(47)*edif + 2.*tab(46)*ev1*fv1)/esq        
        bnewt(2*i1  ) = bnewt(2*i1-1)                                   
                                                                        
 1740   arg2 = dco2/ec2                                                 
        if( abs(arg2) .gt.1.0 ) arg2 = 1.0                              
        fe2 = acos(arg2)                                                
        tab(48) = dco22*cc2*tab(101)                                    
        tab(49) = -(dco2*tan(fe2)-rpib18*tab(26)*cc2)*cc2*tab(101)      
        i1 = itab(37)                                                   
        if( evsq2 .gt. 0.0001) go to 1745                               
        tab(44) = 0.0                                                   
        tab(45) = 0.0                                                   
        gnewt(2*i1-1) = tab(40)                                         
        gnewt(2*i1  ) = tab(40)                                         
        bnewt(2*i1-1) = -tab(41)                                        
        bnewt(2*i1  ) =  tab(41)                                        
                                                                        
        go to 100                                                       
                                                                        
 1745   tab(44) = 2.*(tab(48)*ev2 + tab(49)*fv2)/evsq2                  
        tab(45) = 2.*(tab(48)*fv2 - tab(49)*ev2)/evsq2                  
        edif = ev2*ev2 - fv2*fv2                                        
        esq  = evsq2*evsq2                                              
        gnewt(2*i1-1) = (tab(48)*edif + 2.*tab(49)*ev2*fv2)/esq         
        gnewt(2*i1  ) = - gnewt(2*i1-1)                                 
        bnewt(2*i1-1) = (-tab(49)*edif + 2.*tab(48)*ev2*fv2)/esq        
        bnewt(2*i1  ) = bnewt(2*i1-1)                                   
                                                                        
  100   continue                                                        
                                                                        
        if (keybrd(22).ne.0) then                                       
C                                                                       
C          *** DEBUG OUTPUT  ***                                        
C                                                                       
           write (outbuf,120) iptab(37),iptab(38),iptab(40),iptab(41)   
           call prtout (1)                                              
           do 122 jjj = 1,45,8                                          
             kkk = min0 (jjj+7,45)                                      
             write (outbuf,121) (i,ptab(i),i=jjj,kkk)                   
             call prtout (1)                                            
  122        continue                                                   
  120      format('0IPTAB(37,38,40,41),PTAB(1,,45)=',4i5)               
  121      format(1x,8(i3, e13.4))                                      
           write (outbuf,140) itab(22),itab(33),itab(105),itab(106)     
           call prtout (1)                                              
           do 142 jjj = 1,177,8                                         
             kkk = min0 (jjj+7,177)                                     
             write (outbuf,141) (i,tab(i),i=jjj,kkk)                    
             call prtout (1)                                            
  142        continue                                                   
  140      format('0ITAB(22,33,105,106),TAB(1,...,177=',4i5)            
  141      format(1x,8(i3,e13.4))                                       
           write (outbuf,160) loopdc                                    
           call prtout (1)                                              
  160      format(1x, 'LOOPDC = ', i5)                                  
                                                                        
C          DEBUG OUTPUT FOR NEW DC CONTROL                              
                                                                        
           if (itab(114) .ne. 6) go to 180                              
           write (outbuf,165) itab(114),itab(119),itab(120)             
           call prtout (1)                                              
           do 167 jjj = 178,205,8                                       
             kkk = min0 (jjj+7,205)                                     
             write (outbuf,166) (i,tab(i),i=jjj,kkk)                    
             call prtout (1)                                            
  167        continue                                                   
  165      format('0ITAB(114,119,120)  TAB(178...205)=',3i5)            
  166      format(1x,8(i3,e13.4))                                       
           write (outbuf,170) itab(143),itab(144), itab(146)            
           call prtout (1)                                              
 170       format(' ITR1,ITHR2,IDO', 3i6)                               
                                                                        
           end if                                                       
                                                                        
 180    continue                                                        
                                                                        
        if (link.gt.10) return                                          
                                                                        
          j=itab(35)                                                    
          bcurr(j)=tab(42)                                              
          bcuri(j)=tab(43)                                              
          sumcr1=abs(tab(42))+abs(tab(43))                              
          cdel1=abs(ptab(29)-sumcr1)                                    
          ptab(29)=sumcr1                                               
          dsum=dsum+cdel1                                               
          if (cmax .le. cdel1)then                                      
            imax=j                                                      
            cmax=cdel1                                                  
            endif                                                       
          j=itab(37)                                                    
          bcurr(j)=tab(44)                                              
          bcuri(j)=tab(45)                                              
          sumcr2=abs(tab(44))+abs(tab(45))                              
          cdel2=abs(ptab(30)-sumcr2)                                    
          ptab(30)=sumcr2                                               
          dsum=dsum+cdel2                                               
          if (cmax .le. cdel2) then                                     
            imax=j                                                      
            cmax=cdel2                                                  
            endif                                                       
                                                                        
 2000   continue                                                        
                                                                        
      return                                                            
      end                                                               
