C    %W% %G%
      subroutine newtsl                                                 
C * * *                                                           * * * 
C * NEWTSL IS A MATRIX REDUCTION ROUTINE WRITTEN FOR NEWTONS METHOD  *  
C * USING  THE CURRENT MODE AND RECTANGULAR COORDINATES            *    
C * * *                                                           * * * 
      include 'tspinc/params.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/yfactr.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/busdta.inc' 
      include 'tspinc/contrl.inc' 
      include 'tspinc/ldrep.inc' 
      include 'tspinc/bcur.inc' 
      include 'tspinc/newton.inc' 
      include 'tspinc/fltopt.inc' 

      double precision rowh, rowj, rowl, rown                           
      double precision vyr(MAXBUS), vyi(MAXBUS), slnr, slni, eyrt,      
     &    eyit                                                          
      dimension rowh(200), rowj(200), rowl(200), rown(200), kolum(200),
     &          korder(200)             
      k=0                                                               
      ik = 1                                                            
      ikp = 1                                                           
      do 50 itrr=1,nmx                                                  
      vyr(itrr) = bcurr(itrr)                                           
 50   vyi(itrr) = bcuri(itrr)                                           
C * * *                                                                 
C   FORM KTH ROW OF JACOBIAN                                            
C * * *                                                                 
 1010 k = k + 1                                                         
      jsw = 1                                                           
      ek = vyr(k)                                                       
      fk =  vyi(k)                                                      
 1020 ilocf(k) = ikp                                                    
 1025 format(1h0,5x,' K, IKP ',2i5)                                     
      kl=0                                                              
      lp=0                                                              
      lsw = 1                                                           
      l=1                                                               
      lq = 1                                                            
      call getmat(k, ii)                                                    
      lens = matrow(2)                                                  
      loclen = matrow(3)                                                
      kl = lens+1                                                       
      kolum(kl) = matrow(1)                                             
      korder(kl) = kl+1                                                 
      if(lens .eq. 0) go to 1060                                        
      do 1050 i=1,lens                                                  
      lp = lp+1                                                         
      ind = i*3+1                                                       
      rowh(lp) = -atrow(ind+2)                                          
      rown(lp) = atrow(ind+1)                                           
      rowj(lp) = atrow(ind+1)                                           
      rowl(lp) = - rowh(lp)                                             
      kolum(lp) = matrow(ind)                                           
      korder(lp) = lp+1                                                 
 1050 continue                                                          
 1060 lp = lp +1                                                        
      iend = loclen-1                                                   
      if(iend .eq. 0) go to 1080                                        
      do 1070 i=1,iend                                                  
      lp = lp+1                                                         
      ind = ii-2-3*i                                                    
      rowh(lp) = -atrow(ind+2)                                          
      rowl(lp) = - rowh(lp)                                             
      rown(lp) = atrow(ind+1)                                           
      rowj(lp) = atrow(ind+1)                                           
      kolum(lp) = matrow(ind)                                           
      korder(lp) = lp+1                                                 
 1070 continue                                                          
 1080 gkk = atrow(ii-1)                                                 
      bkk = atrow(ii)                                                   
C * * *                           * * *                                 
C *    ADD DIAGONAL TERMS TO JACOBIAN *                                 
C * * *                           * * *                                 
C** NOTE: THE THREE STATEMENTS BELOW IGNORED--NOT USED                  
C 1160 IF(IPWR .EQ. 1) GO TO 1170                                       
C      IF(IDSW .NE. 7) GO TO 1165                                       
C      IF((ITER .EQ. 1) .AND. (LPPWR .EQ. 0)) GO TO 1170                
 1160  continue                                                         
 1165 rowh(kl) = -bkk+bnewt(2*k-1)                                      
      rown(kl) = gkk+gnewt(2*k-1)                                       
      rowl(kl) = bkk + bnewt(2*k)                                       
      rowj(kl) = gkk + gnewt(2*k)                                       
      go to 1173                                                        
c     -  Next 18 statements never executed
c1170 ROWH(KL) = -BKK                                                   
c     ROWN(KL) = GKK                                                    
c     ROWJ(KL) = GKK                                                    
c     ROWL(KL) = -ROWH(KL)                                              
c     IF(LREP .EQ. 0) GO TO 1172                                        
c     EKA = EMAGRN(1,K)                                                 
c     EKSQ = EKA*EKA                                                    
c     P = PLTOTN(1,K)                                                   
c     Q = PLTOTN(2,K)                                                   
c     IF(P .EQ. 0.0 .AND. Q .EQ. 0.0) GO TO 1172                        
c     ROWH(KL) = -BKK+Q*EKSQ                                            
c     ROWN(KL) = GKK+P*EKSQ                                             
c     ROWJ(KL) = GKK+P*EKSQ                                             
c     ROWL(KL) = -ROWH(KL)                                              
c1172  ROWH(KL) = ROWH(KL) + BNEWT(2*K-1)                               
c     ROWN(KL) = ROWN(KL) + GNEWT(2*K-1)                                
c     ROWJ(KL) = ROWJ(KL) + GNEWT(2*K)                                  
c     ROWL(KL) = ROWL(KL) + BNEWT(2*K)                                  
 1173 continue                                                          
CC DIAG. TERMS ONLY                                                     
       if (keybrd(33) .ne. 0 ) then                                     
      write (l6, 1176) kolum(kl),rowh(kl), rown(kl),rowj(kl),rowl(kl),  
     1   gnewt(2*k-1), bnewt(2*k-1)                                     
 1176 format(1h0,2x,'NEWTSL,IROW= ',i4, 2x, 6(2x,f15.6,1x))             
        endif                                                           
      gnewt(2*k-1) = 0.0                                                
      gnewt(2*k) = 0.0                                                  
      bnewt(2*k-1) = 0.0                                                
      bnewt(2*k) = 0.0                                                  
      if(keybrd(0) .eq. 0) go to 1180                                   
      write(l6,1175)(rowh(j),rown(j),rowj(j),rowl(j),kolum(j),korder(j),
     1 j=1,lp)                                                          
 1175 format(1h0,5x,'   ROWH       ROWN       ROWJ      ROWL      KOLUM 
     1 korder  ',/,(5x,4(2x,f15.6,1x),4x,i3,5x,i3))                     
 1180 mel=1                                                             
      max = kolum(lp)                                                   
      korder(lp) = 0                                                    
      mend=lp+1                                                         
      go to 1205                                                        
 1200 mel = korder(mel)                                                 
 1205 ms = kolum(mel)                                                   
 1210 if (ms.lt.k) go to 2000                                           
 1215 format(1h0,5x,'KOLUM,KORDER',/,(2i10))                            
C * * *                                                                 
C   DIAGONALIZE ROW                                                     
C * * *                                                                 
 1500 rhin=1.0/rowh(mel)                                                
C *****                                                                 
C   NO RELOCATION OF FLT BUS TEST                                       
      if ( iesc .ne. 3 ) go to 1510                                     
      if (ifltsw .ne.1 .and. ifltb .eq. k) then                         
      rhin= 0.0                                                         
        endif                                                           
 1510 ik = ilocf(k)                                                     
      ikp = ilocf(k)                                                    
      an = rown(mel)*rhin                                               
      rj = rowj(mel)                                                    
      rl = rowl(mel)                                                    
      rlin = 1./(rl-rj*an)                                              
C ****                                                                  
C   NO RELOCATION OF FLT BUS TEST                                       
      if ( iesc .ne. 3 ) go to 1515                                     
      if (ifltsw .ne. 1 .and. ifltb .eq. k) then                        
      rlin = 0.0                                                        
       endif                                                            
 1515 amtrx(ik) = an                                                    
      vyr(k) = ek*rhin                                                  
      vyi(k) = (fk-rj*vyr(k))*rlin                                      
 1540 if(k .eq. nmx) go to 1550                                         
      go to 1560                                                        
C     1550 ILOCF(IKP+1)= K+1                                            
 1550  ilocf(k+1) = ikp +1                                              
 1555 format(1h0,5x,'EYR,EYI',/,(5x,2f10.6))                            
      go to 3000                                                        
 1560 ik = ik+1                                                         
      ikp = ikp+1                                                       
 1570 mel=korder(mel)                                                   
      if (mel.gt.0) go to 1580                                          
 1575 format(1h0,5x,'KEC IK ',2i5)                                      
      go to 1010                                                        
 1580 m=kolum(mel)                                                      
      adp=rowh(mel)*rhin                                                
      amtrx(ik+1)=adp                                                   
      amtrx(ik+2) = (rowj(mel)-rj*adp)*rlin                             
C 1590 MTRX(IK) = M                                                     
 1590 amtrx(ik) = m                                                     
      an = rown(mel)*rhin                                               
      amtrx(ik + 3) = an                                                
      amtrx (ik+4) = (rowl(mel) -rj*an)*rlin                            
      ik = ik + 5                                                       
      ikp = ikp+5                                                       
      go to 1570                                                        
C * * *                                                                 
C     START OF ROW REDUCTION                                            
C * * *                                                                 
 2000 rh=rowh(mel)                                                      
      ik = ilocf(ms)                                                    
      ikstop = ilocf(ms+1) -1                                           
      krw = mel                                                         
 2100 ex = vyr(ms)                                                      
      xn = amtrx(ik)                                                    
      fx = vyi(ms)                                                      
      rn = rown(mel)-rh*xn                                              
      ek = ek-rh*ex-rn*fx                                               
      ik = ik +1                                                        
 2110 rj=rowj(mel)                                                      
      rl=rowl(mel)-rj*xn                                                
      fk = fk-rl*fx-rj*ex                                               
      assign 2130 to lsw1                                               
      assign 2120 to lsw2                                               
C 2120 ML =  MTRX(IK)                                                   
C      MLS = MTRX(IK)                                                   
 2120 ml = amtrx(ik)                                                    
      mls = amtrx(ik)                                                   
      xh=amtrx(ik+1)                                                    
      xj=amtrx(ik+2)                                                    
      go to 2500                                                        
 2130 rwh = -rh*xh-rn*xj                                                
      rwj=-rj*xh-rl*xj                                                  
      go to (2140,2145),jsw                                             
 2140 rowh(mlc)=rowh(mlc)+rwh                                           
      rowj(mlc)=rowj(mlc)+rwj                                           
      go to 2150                                                        
 2145 rowh(mlc)=rwh                                                     
      rowj(mlc)=rwj                                                     
 2150 go to 2160                                                        
 2160 xn = amtrx(ik+3)                                                  
      xl = amtrx(ik+4)                                                  
      rwn = -rh*xn-rn*xl                                                
      rwl = -rj*xn-rl*xl                                                
      go to (2170,2175),jsw                                             
 2170 rown(mlc)=rown(mlc)+rwn                                           
      rowl(mlc) = rowl(mlc)+rwl                                         
      go to 2290                                                        
 2175 rown(mlc) = rwn                                                   
      rowl(mlc) = rwl                                                   
      go to 2290                                                        
c     -  Next 14 statements never executed
c2230 RWH = -RH*XH-RN*XJ                                                
c     GO TO (2240,2245),JSW                                             
c2240 ROWH(MLC)=ROWH(MLC)+RWH                                           
c     GO TO 2250                                                        
c2245 ROWH(MLC)=RWH                                                     
c2250 GO TO 2260                                                        
c2260 XN=AMTRX(IK+3)                                                    
c     XL = AMTRX(IK+4)                                                  
c     RWN = -RH*XN-RN*XL                                                
c     GO TO (2270,2275),JSW                                             
c2270 ROWN(MLC)=ROWN(MLC)+RWN                                           
c     GO TO 2290                                                        
c2275 ROWN(MLC)=RWN                                                     
c     GO TO 2290                                                        
 2290 ik=ik+5                                                           
      go to 2570                                                        
 2500 if (ml.gt.max) go to 2535                                         
 2510 if (ml.gt.kolum(krw)) go to 2520                                  
      go to 2530                                                        
 2520 ko = krw                                                          
      krw = korder (krw)                                                
      go to 2510                                                        
 2530 if (ml.lt.kolum(krw)) go to 2540                                  
      go to 2560                                                        
 2535 max = ml                                                          
      ko = lp                                                           
      lp = mend                                                         
 2540 korder(mend)=korder(ko)                                           
      kolum(mend) = ml                                                  
      korder(ko)=mend                                                   
      mlc = mend                                                        
      ko = mend                                                         
      mend=mend+1                                                       
      jsw = 2                                                           
C * * *                                                                 
C 2550 GO TO LSW1, (2330,2430,2230,2130)                                
C * * *                                                                 
 2550 go to 2130                                                        
 2560 mlc = krw                                                         
      jsw=1                                                             
      go to 2550                                                        
 2570 if (ik.lt.ikstop) go to 2580                                      
      go to 1200                                                        
C * * *                                                                 
C   2580 GO TO LSW2,(2320,2420,2120,2220)                               
C * * *                                                                 
 2580 go to 2120                                                        
C * * *                                                                 
C    BACK SUBSTITUTION                                                  
C * * *                                                                 
C  NO RELOCATION OF FLT BUS OPTION                                      
 3000 if (ifltsw .ne. 1 ) go to 3005                                    
      if(iesc .ne. 3) go to 3005                                        
      vyr(k) = 0.0                                                      
      vyi(k) = 0.0                                                      
      go to 3300                                                        
 3005 if(k .eq. 0) go to 3400                                           
 3010 ik = ilocf(k)                                                     
      ils = ilocf(k+1) -1                                               
 3015 format(1h0,5x,'K ILS',2i7)                                        
 3016 format(1h0,5x,'AMTRX',/,5x,f10.6,(5x,i5,4f10.6))                  
      il = ik                                                           
      ek = vyr(k)                                                       
      fk = vyi(k)                                                       
 3100 dee = fk                                                          
      aiko = ek                                                         
      aik2 = amtrx(ik)                                                  
      ddt = 0.0                                                         
      ik = ik+1                                                         
 3110 if(ik .gt. ils) go to 3130                                        
C      ML = MTRX(IK)                                                    
      ml = amtrx(ik)                                                    
      amjl1 = vyr(ml)                                                   
      amjl = vyi(ml)                                                    
      dee = dee - amtrx(ik +2)*amjl - amtrx(ik+4)*amjl1                 
      ddt = ddt + amtrx(ik+1)*amjl + amtrx(ik+3)*amjl1                  
      ik = ik + 5                                                       
      go to 3110                                                        
 3130 dd = aiko - aik2*dee - ddt                                        
C ****** NON RELOCATION OF FLT BUS TEST                                 
      if( iesc .ne. 3 ) go to 3135                                      
      if ( ifltsw .ne. 2 ) go to 3135                                   
      if ( ifltb .eq. k ) then                                          
      vyi(k) = 0.0                                                      
      vyr(k) = 0.0                                                      
      go to 3300                                                        
       endif                                                            
 3135 vyi(k) = dd                                                       
      vyr(k) = dee                                                      
      go to 3300                                                        
 3300 k=k-1                                                             
      go to 3005                                                        
 3400 do 3500 i = 1,nmx                                                 
      eyr(i) = vyr(i)                                                   
 3500 eyi(i) = vyi(i)                                                   
 8000 return                                                            
      end                                                               
