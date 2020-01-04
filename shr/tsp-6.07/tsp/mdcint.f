C    %W% %G%
      subroutine mdcint                                                 
C * * *                                                                 
C * * * THIS SUBROUTINE INITIALIZES TABLES FOR MULTI-TERMINAL DC        
C * * * IT IS CALLED BY INITL4.  IT CALLS GAMINT AND MMODIN.            
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/gamma.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/namec.inc' 
      include 'tspinc/cntrl2.inc' 
      include 'tspinc/toler.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/contrl.inc' 
      include 'tspinc/fltopt.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/newton.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/ectba.inc' 
      include 'tspinc/mdctbl.inc' 
      include 'tspinc/mdcfil.inc' 
      include 'tspinc/dcblk.inc' 
      include 'tspinc/ecstbb.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/prt.inc' 

      equivalence (erio,tab(164)),(eroo,tab(165)),(eron,tab(166)),     
     1            (etio,tab(167)),(eioo,tab(168)),(eion,tab(169)),     
     2            (ectim1,tab(148)),(ectim2,tab(149))                  
      dimension zoecs(512)                                            
      character*8 ibus1,dcbus1,dcbus2,name,name1,name2                

      pi=3.14159265                                                     
      f135=3.0*sqrt(2.0)/pi                                             
      rpib3=3.0/pi                                                      
      rpib18=18.0/(pi*pi)                                               
      sqr2=sqrt(2.0)                                                    
      zero=0.0                                                          
      ipwr=2                                                            
      do 5000 itrr= 1,20                                                
      mdcblk(itrr) = 0                                                  
 5000 continue                                                          
      n100 = 100                                                        
      ldcc=0                                                            
 6010 ldcc=ldcc+1                                                       
      if(ldcc.gt.ldc1) go to 11090                                      
C * * *                                                                 
C     INITIALIZE DCB,...,DCL TABLE                                      
C * * *                                                                 
      call redecs (dcb,kdcm(ldcc),msizeb)                               
      nbusp1 = nbus+1                                                   
      iopen=1                                                           
      mblck=1                                                           
      dts = dt / dcitol                                                 
C * * *                                                                 
C     ZERO L-1,YMTRX TABLE                                              
C * * *                                                                 
      nbussq=nbus**2                                                    
      do 6012 i=1,10                                                    
      do 6012 ii=1,11                                                   
      dcc(i,ii)=0.0                                                     
 6012 dcg(i,ii)=0.0                                                     
      ind=lbt+nterm+1                                                   
C * * *                                                                 
C * * * PREPARE THE ADMITTANCE TABLES YMTRX AND BLMTRX                  
C * * *                                                                 
 6020 if(ind.gt.ndimc) go to 6030                                       
      nb = idcb(ind)                                                    
      if(nb.gt.0) go to 6025                                            
      ia = -nb                                                          
      if(ia.gt.10) ia = ia - 10                                         
      ind=ind+1                                                         
      go to 6020                                                        
 6025 if(nb.le.10) go to 6027                                           
      ind = ind + 1                                                     
      go to 6020                                                        
C * * *                                                                 
C     SCALE INDUCTANCE,CURRENT TO PROPER UNITS                          
C * * *                                                                 
 6027 dcb(ind + 2) = dcb(ind+2) * 0.001 * 60.0                          
      dcb(ind+3) = 1.0 / (dcb(ind+1) + 2*dcb(ind+2) / dts)              
      dcb(ind+4) = dcb(ind+1) - 2*dcb(ind+2) / dts                      
C * * *                                                                 
C     PREPARE PARTIAL L-1,YMTRX                                         
C * * *                                                                 
      ymtrx(ia,nb) = -dcb(ind+3)                                        
      ymtrx(ia,ia) = ymtrx(ia,ia) + dcb(ind+3)                          
      blmtrx(ia,nb) = -1.0/dcb(ind+2)                                   
      blmtrx(ia,ia) = blmtrx(ia,ia) + 1.0/dcb(ind+2)                    
      ind = ind + 8                                                     
      go to 6020                                                        
 6030 call ritecs(dcb,kdcm(ldcc),ndimc)                                 
      if(keybrd(22).eq.0)go to 6060                                     
C     DEBUG OUTPUT FOR DCB TABLE                                        
      call prtout (1)                                                   
      write(outbuf,6040)                                                
 6040 format(1x,'S 6040 I/DCB OUTPUT')                                  
      kk = lbt + idcb(2)                                                
      do 6044 k =1,kk                                                   
      write(outbuf,6041) k,idcb(k)                                      
 6041 format(10x,i4,1x,i10)                                             
      call prtout(1)                                                    
 6044 continue                                                          
 6042 continue                                                          
      if(kk .lt. ndimc) then                                            
         k1 = kk+1                                                      
         k2 = k1                                                        
         if(idcb(k1) .lt. 0) k2 = k1+1                                  
         k3 = k2+1                                                      
         k4 = k3+6                                                      
         do 6045 k = k1,k2                                              
         write(outbuf,6041) k,idcb(k)                                   
         call prtout(1)                                                 
 6045    continue                                                       
         do 6046 k =k3,k4                                               
         write(outbuf,6043) k,dcb(k)                                    
 6043    format(10x,i4,1x,e10.3)                                        
         call prtout(1)                                                 
 6046    continue                                                       
         kk = k4                                                        
         go to 6042                                                     
      endif                                                             
 6050 format ('0S6050  DCB OUTPUT')                                     
 6051 format (1x,8(i3,e13.5))                                           
      do 6047 jjj = 1,17                                                
 6047 dca(jjj) = eyr(jjj)                                               
      write (outbuf,6053)                                               
      call prtout (1)                                                   
      do 26052 jjj = 1,17,10                                            
         kkk = min0 (jjj+9,17)                                          
         write (outbuf,16053) (dca(i),i=jjj,kkk)                        
         call prtout (1)                                                
26052 continue                                                          
      do 6052 jjj = 1,17                                                
 6052 dca(jjj) = eyi(jjj)                                               
      call prtout (1)                                                   
      do 26053 jjj = 1,17,10                                            
         kkk = min0 (jjj+9,17)                                          
         write (outbuf,16053) (dca(i),i=jjj,kkk)                        
         call prtout (1)                                                
26053 continue                                                          
 6053 format ('0AT S6053 VOLTAGES')                                     
16053 format (1x,10e13.4)                                               
C * * *                                                                 
C * * *  PREPARE THE REST OF THE DCA TABLE                              
C * * *                                                                 
 6060 ind=0                                                             
      iblkt = 0                                                         
 6070 ind=ind+1                                                         
      if(ind.gt.nterm) go to 7061                                       
      ind1 = lbt + ind                                                  
      call redecs(dca,idcb(ind1),msizea)                                
C****                                                                   
C* YISOLN, NEWTON OPTION                                                
C****                                                                   
      go to ( 6080, 6075 ), inewts                                      
C* REMOVE DC ADMIT. FROM YMATRIX DIAG. & STORE IN YADMIT TABLE          
 6075 call getmat(iev, ii)                                                  
      atrow(ii-1) = atrow(ii-1) - gv                                    
      atrow(ii  ) = atrow(ii  ) - bv                                    
      gadmit(iev) =  gv                                                 
      badmit(iev) =  bv                                                 
      call putmat(iev, ii)                                                  
C * * *                                                                 
C * * * ADD THE CONTRIBUTION FROM THE COMMUNTATING XFRMERS              
C * * * TO THE DIAGONAL OF THE DC ADMITTANCE MATRICES                   
C * * *                                                                 
 6080 ymtrx(ibus,ibus) = ymtrx(ibus,ibus) + yiop                        
      blmtrx(ibus,ibus)=blmtrx(ibus,ibus)+1.0/sl                        
      crated=crated*0.001                                               
      cmx = cmx * crated                                                
      cdesrd=cdesrd*.001                                                
      do 6081 jtrr = 1,6                                                
C * * *                                                                 
C * * * INITIALIZE DCFIL TABLE FOR MODE CHANGE FILTER                   
C * * *                                                                 
      dcfil(jtrr,ldcc) = csign*cinit                                    
 6081 continue                                                          
      zero=0.0                                                          
C * * *                                                                 
C * * * CHECK TO SEE IF THIS TERMINAL IS BEING DISABLED                 
C * * *                                                                 
      if(idca(94) .ne. 0)then                                           
         iblkt = iblkt + 1                                              
         dcbus1 = exnamc(idca(46))                                      
         write(errbuf(1),6085)dcbus1                                    
 6085    format(5x,' DC CONVERTER ',a8,' IS BEING BLOCKED.')            
         call prterr('W',1)                                             
      endif                                                             
      if(abs(cinit) .gt. cmx)then                                       
         dcbus1 = exnamc(idca(46))                                      
         write(errbuf(1),6990) dcbus1,cinit,cmx                         
         call prterr('E',1)                                             
 6990    format (1h0,5x,' INITIAL CURRENT EXCEEDS MAXIMUM CURRENT FOR ',
     1   a8,5x,' CINIT = ',f9.4,' CMAX = ',f9.4)                        
         iabort = 1                                                     
      endif                                                             
C * * * CONVERT CURRENT-REGULATOR GAIN FROM P.U. TO OHMS                
C * * *                                                                 
      ckalph = -ckalph * (rref*brdgno*f135 / crated)                    
      delcm = delcm*crated                                              
C * * *                                                                 
C * * * FIND COMMUTATING-BUS VOLTAGE IN VOLTS                           
C * * *                                                                 
      e1 = eyr(iec)                                                     
      f1 = eyi(iec)                                                     
      ecn=sqrt(e1*e1+f1*f1)*txbse                                       
      eop = f135 * ecn                                                  
      eco=ecn                                                           
      eocdn=ecn                                                         
      eocn = ecn                                                        
      eoco = eco                                                        
      rrll = rxpi * rll                                                 
      biim = cdcn * ziom + twovd + csign * f135 * eocn * cosan-vdinit   
      ymtrx(ibus,nbusp1)=ymtrx(ibus,nbusp1)+(biim+csign*f135*eocn*cosan)
     1        * yiop                                                    
      cx = 2.0*xc*csign*cinit*rpib3                                     
C * * *                                                                 
C * * * MODERI= 1 FOR RECTIFIER = 2 AND = 3 FOR INVERTERS               
C * * *                                                                 
 7000 if(moderi.ne.1) go to 7010                                        
      cosan=cosang                                                      
      coslim=cosmin                                                     
      dcv=f135*ecn*cosan-cinit*rxpi+twovd/2.0                           
      go to 7020                                                        
C * * *                                                                 
C * * *  INVERTER                                                       
C * * *                                                                 
 7010 cosan=cx/eop-cosang                                               
      dcv=-f135*ecn*cosan-cinit*rxpi+twovd/2.0                          
      coslim=costop                                                     
 7020 edcan=dcv                                                         
      cinit=-cinit                                                      
      fdin=cinit                                                        
      if(moderi.eq.1)fdin=-cinit                                        
C * * *                                                                 
C * * * CHECK VARIABLE LIMITER VIOLATIONS                               
C * * *                                                                 
      vpa=eop*(cosan+cosgam)                                            
      dca(61) = vpa                                                     
C * * *                                                                 
C * * * ICCDE = 2 MEANS CONSTANT EXTINCTION ANGLE                       
C * * *                                                                 
      iccde = idca(93)                                                  
      if(iccde .eq. 2) vpa = vpa -  cx                                  
      vpchek=abs(vpa-cx)                                                
          if(moderi .eq. 2 .and. vpchek .lt. 1.0e-4) vpa = 0.0          
      zmax=eop*(cosgam+coslim)                                          
          if(vpa .le. zmax+1.0e-4 .and. vpa+1.0e-4 .ge. 0.0)            
     1      go to 7040                                                  
C***** NEXT STATEMENT INCOMPLETE                                        
      write (errbuf(1),7030) vpa,zero,zmax                              
      call prterr ('E',1)                                               
 7030 format(1h0,a10,f5.1,'VALVE CNTRL LIMIT VIOLATION...X,XMIN,        
     1 xmax=',3e15.6)                                                   
      iabort=1                                                          
C * * *                                                                 
C * * *  V1N TO V4N ARE STATE VARIABLES FOR THE CURRENT REGULATOR       
C * * *                                                                 
 7040 v3n=vpa                                                           
      v2n=vpa                                                           
      v1n=vpa/ckalph                                                    
      bias=v1n                                                          
C * * *                                                                 
C * * * INCORPORATE CONSTANT SLOPE FOR CURRENT MARGIN TERMINAL          
C * * *                                                                 
      cosgap = cosgam                                                   
      if(iccde .eq. 2) go to 7045                                       
      if(moderi.ne.2) go to 7045                                        
      frac=.25                                                          
      cosgap=2.*xc*rpib3*frac*delcm/eop+cosgam                          
      dca(61) = cx + eop*(cosgap-cosgam)                                
 7045 cdcn=cinit                                                        
C * * *                                                                 
C * * *     TIME CONSTANTS TO PROPER UNITS                              
C * * *                                                                 
      do 7050 i=1,5                                                     
 7050 dca(i)=dca(i)*frqbse                                              
      do 7022 i=54,63                                                   
 7022 dca(10+i) = dca(i)                                                
C          CONSTANT DC VOLTAGE OPERATION                                
      if(idca(82) .gt. 0) then                                          
      if(dca(92) .le. 0.0) dca(92) = 0.0                                
         if(moderi .eq. 1) then                                         
         vdref = dcv - dca(92)*fdin                                     
         else                                                           
         vdref = dcv + dca(92)*fdin                                     
         endif                                                          
         if(dca(91) .le. 0.0) dca(91) = vdref                           
         if(dca(91) .lt. vdref) then                                    
         write(errbuf(1),7025) vdref,dca(91)                            
 7025 format(1x, 'CONSTANT DC VOLTABE OPERATION CONSTRAINT VIOLATION ', 
     1           2e10.4)                                                
         call prterr('E',1)                                             
         iabort = 1                                                     
      endif                                                             
C                                                                       
      endif                                                             
C * * *                                                                 
C * * * DEBUG FOR DC MATRIX                                             
C * * *                                                                 
      if (keybrd(22).ne.0) then                                         
         write (outbuf,7060)                                            
         call prtout (1)                                                
         do 27060 jjj = 1,msizea,8                                      
            kkk = min0 (jjj+7,msizea)                                   
            write (outbuf,17060) (i,dca(i),i=jjj,kkk)                   
            call prtout (1)                                             
27060    continue                                                       
      endif                                                             
 7060 format('0S7060 DCA OUTPUT')                                       
17060 format(1x,8(i4,e12.4))                                            
C * * *                                                                 
C * * * INITIALIZE MODULATION TABLES IF THEY EXIST                      
C * * *                                                                 
      modcod = idca(78)                                                 
      if (modcod .eq. 0) then                                           
         call ritecs(dca,idcb(ind1),msizea)                             
         go to 6070                                                     
      endif                                                             
      if(modcod .eq. 5) then                                            
C * * *                                                                 
C * * * INITIALIZE TABLES FOR GAMMA MODULATION                          
C * * *                                                                 
         igam = idca(79)                                                
C * * * INITALIIZE INVERTER BUS NUMBER                                  
         igamno(igam) = idca(45)                                        
C * * * INITALIZE GAMMA EXTINCTION ANGLE FROM POWER FLOW                
         gama(igam) = acos(dca(33))                                     
         vdcgam = edcan                                                 
         call gamint(igam,vdcgam)                                       
      else                                                              
C * * *                                                                 
C * * * INITIALIZE TABLES FOR OTHER MODULATION TYPES                    
C * * *                                                                 
      if(modcod .gt. 0 .and. idca(15) .ne. 2) then                      
         imod = idca(79)                                                
         call mmodin(imod)                                              
      else                                                              
         idca(78) = 0                                                   
         idca(79) = 0                                                   
      endif                                                             
      endif                                                             
      call ritecs(dca,idcb(ind1),msizea)                                
      go to 6070                                                        
C     WRITE TO ECS  L-1, YMTRX                                          
 7061 call ritecs(ymtrx,necsy,n100)                                     
      call ritecs(blmtrx,necsg,n100)                                    
      if(iblkt .ne. 0)then                                              
         mdcblk(ldcc) = 1                                               
         if(iblkt .ne. nterm)then                                       
            write(errbuf(1),17061)                                      
17061       format(5x,' AT LEAST ONE TERMINAL ON THIS MULTITERMINAL ',  
     1             'LINE IS NOT BLOCKED.')                              
            iabort = 1                                                  
            call prterr('E',1)                                          
          endif                                                         
      endif                                                             
      if(keybrd(22).eq.0) go to 7070                                    
C * * *                                                                 
C     DEBUG OUTPUT FOR L-1,YMATRIX                                      
C * * *                                                                 
      if (keybrd(22).ne.0) then                                         
         write (outbuf,7062)                                            
         call prtout (1)                                                
         do 27062 i = 1,10                                              
            write (outbuf,17062) (ymtrx(i,j),j=1,10)                    
            call prtout (1)                                             
27062    continue                                                       
      endif                                                             
 7062 format('0AT S7062 YMTRX FOR CIRCUIT')                             
17062 format(1x,10e13.5)                                                
      if (keybrd(22).ne.0) then                                         
         write (outbuf,7065)                                            
         call prtout (1)                                                
         do 27065 i = 1,10                                              
            write (outbuf,17065) (blmtrx(i,j),j=1,10)                   
            call prtout (1)                                             
27065    continue                                                       
      endif                                                             
 7065 format('0AT S7062 BLMTRX FOR CIRCUIT')                            
17065 format(1x,10e13.5)                                                
C * * *                                                                 
C * * * FACTORIZE AND STORE YMTRX MATRIX                                
C * * *                                                                 
 7070 do 7078 i = 1, nbus                                               
      if(i.eq.1) go to 7076                                             
      im = i - 1                                                        
      do 7074 j = 1, im                                                 
      if(ymtrx(j,i).eq.0.0) go to 7074                                  
      zijc = ymtrx(j,i) / ymtrx(j,j)                                    
      do 7072 k = i, nbus                                               
 7072 ymtrx(i,k)=ymtrx(i,k)-ymtrx(j,k)*zijc                             
 7074 continue                                                          
 7076 recip = 1.0 / ymtrx(i,i)                                          
      ymtrx(i,i) = recip                                                
      i1 = i + 1                                                        
      do 7077 l = i1, nbus                                              
 7077 ymtrx(i,l) = ymtrx(i,l) * recip                                   
 7078 continue                                                          
      call ritecs(ymtrx,necsk,n100)                                     
C * * *                                                                 
C     INITIALIZE IDCF(NTERM) TABLE                                      
C * * *                                                                 
      do 7082 i = 1, nterm                                              
 7082 idcf(i) = 1                                                       
C * * *                                                                 
C     LOOP FOR ANOTHER DC CKT                                           
C * * *                                                                 
      go to 6010                                                        
11090 return                                                            
      end                                                               
