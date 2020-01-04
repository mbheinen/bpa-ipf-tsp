C    %W% %G%
      subroutine int3ff (a)                                             
                                                                        
C THIS SUBROUTINE CALCULATES INITIAL STATE VECTORS AND PAST VALUES     
C FOR EXCITER TYPE FF                                                  
                                                                        
      include 'tspinc/param.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/gentbla.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/gentblb.inc' 
      include 'tspinc/int3.inc' 

      dimension array(40),a(40)                                         
      equivalence          (array(1),    vrmax), (array(2),  vrmin),      
     1 (array(3),   vref), (array(4),    esatx), (array(5),  csatx),      
     2 (array(6),    hbr), (array(7),      cke), (array(8),    cka),      
     3 (array(9),     dr), (array(10),   cfldo), (array(11),   ca6),      
     4 (array(12),    df), (array(13),     akf), (array(14),   hbf),      
     5 (array(15),   hcb), (array(16),     ckc), (array(17),    db),      
     6 (array(18),    dc), (array(19),     hbe), (array(20),    de),      
     7 (array(21),  rcex), (array(22), cfldpst), (array(23),  xcex),      
     8 (array(24), x7pst), (array(25),      da), (array(26),   hba),      
     9 (array(27),    vc), (array(28),   vamax), (array(29),   efd),      
     * (array(30),   ckd), (array(31),   vcpst), (array(32),    a3),      
     * (array(33), vamin), (array(34),      ve), (array(35),   ckb),      
     * (array(36),    a4), (array(37),      a5), (array(38),    a6),      
     * (array(39),vfemax), (array(40),     ckh)                           

      equivalence (array(13),   ckf)                           
                                                                        
c assign value used for sub time step
      dtsk = 0.1

C TRANSFER CITER DATA TO ARRAY                                      
      do itr = 1,40                                                 
        array(itr) = a(itr)                                               
      end do

      efd = efdo                                                    
                                                                        
C CALCULATE VOLTAGE TRANSDUCER CIRCUIT                          
      if(xt .eq. 0.0)  then                                         
       vtr = vhd                                                  
       vti = vhq                                                  
      else                                                          
       vtr = vhd + rt*agend - xt*agenq                            
       vti = vhq + rt*agenq + xt*agend                            
      endif                                                         
      crt = agend                                                   
      cit = agenq                                                   
      zcr = rcex*crt - xcex*cit                                     
      zci = rcex*cit + xcex*crt                                     
      vcr = zcr + vtr                                               
      vci = zci + vti                                               
      vc = sqrt(vcr**2 + vci**2)                                    
      vcpst = vc
      vco = vc                                                      
      vc1 = vc                                                      
                                                                        
C CALCULATE R-BLOCK                                             
      dr = 2.0*dr/dtsk + 1.0                                          
      hbr = (dr - 2.0)*vc1 + vc                                     
                                                                        
C CALCULATE FEX-BLOCK (INPUT: EFDO,CFD,CKC;                     
C                      OUTPUT: FEX,VE)                          
C Note: Initial field current CFD equals initial field EFDO.    

      cfd = efdo                                                    
      cfld = cfd
      cfldo = cfld
      cfldpst = cfld
                                                                        
      call getfex (fex,efdo,ve,cfd,ckc)                             
                                                                        
C CALCULATE SATURATION BLOCK                                    
      ckep = csatx * exp(esatx*abs(ve))                             
      ck1 = ckep + ckep*ve*esatx                                    
      ck2 = ve**2 * esatx*ckep                                      
                                                                        
C CHECK EXCITER VOLTAGE LIMITS                                  
      vemax = (vfemax - ckd*cfd)/(cke + ckep)                       
      vemin = 0.0                                                   
      if (ve .ge. vemax .or. ve .le. vemin) then                    
        write (errbuf, 2035) name, base, id, ve                   
        call prterr ('E',1)                                       
 2035   format('0 INITIAL VE VIOLATES LIMITS FOR ', a8,           
     1         2x, f5.1, 2x, a1, 5x, 'INITIAL VE = ', f6.3)           
        iabort = 1                                                
        go to 2300                                                
      endif                                                         
      vfe = (cke+ck1)*ve + ckd*cfd - ck2                            
                                                                        
C CALCULATE A-BLOCK (REGULATOR)                                 
      vr = vfe                                                      
      vrfe = vr - vfe                                               
                                                                        
C CHECK REGULATOR LIMITS                                        
      if (vr .ge. vrmax .or. vr .le. vrmin) then                    
        write (errbuf, 2036) name, base, id, vr                   
        call prterr ('E',1)                                       
 2036   format('0 INITIAL VR VIOLATES LIMITS FOR ', a8,           
     &         2x, f5.1, 2x, a1, 5x, 'INITIAL VR = ', f6.3)           
        iabort = 1                                                
        go to 2300                                                
      endif                                                         
      va = vr/ckb + ckh*vfe                                         
      x3 = va                                                       
      if (va .ge. vamax .or. va .le. vamin) then                    
        write (errbuf, 2037) name, base, id, va                   
        call prterr ('E',1)                                       
 2037   format('0 INITIAL VA VIOLATES LIMITS FOR ', a8,           
     &         2x, f5.1, 2x, a1, 5x, 'INITIAL VA = ', f6.3)           
        iabort = 1                                                
        go to 2300                                                
      endif                                                         
      vaii = va/cka                                                 
      da = 2.0*da/dtsk + 1.0                                          
      hba = (da - 2.0)*va + cka*vaii                                
                                                                        
C CALCULATE E-BLOCK                                             
      de = 2.0*de/dtsk                                                
      hbe = de*ve + vr - vfe                                        
                                                                        
C CALCULATE F-BLOCK                                             
C (CKF AND AKF ARE EQUIVALENCED)                          
      vf = 0.0                                                      
      df = 2.0*df/dtsk + 1.0                                          
      akf = 2.0*ckf/dtsk                                              
      hbf = (df - 2.0)*vf - akf*vfe                                 
                                                                    
C CALCULATE B-C BLOCK                                           
      vai = vaii                                                    
      db = 2.0*db/dtsk + 1.0                                          
      dc = 2.0*dc/dtsk + 1.0                                          
      hcb = (db - 2.0)*vaii - (dc - 2.0)*vai                        
      verr = vai                                                    
      vref = verr + vc1                                             
                                                                        
c Store coefficients for estimated VE limits                    
c     (as a function of VAMAX and VAMIN)                            
c     VEMAXX = VEMAX - CA6*CFD(t)                                   
c     VEMINX = VEMIN - CA6*CFD(t)                                   
c                                                                       
      if (ve .le. 0.0) then                                         
         ckep = 0.0                                                 
      else                                                          
         ckep = 1.0                                                 
      endif                                                         
      a3 = ckep*(ckh*ckb + 1.0)                                     
      denom = 1.0/(de + a3*(ck1 + cke))                             
      vemax = (hbe + ckep*ckb*vamax + a3*ck2) * denom               
      vemin = (hbe + ckep*ckb*vamin + a3*ck2) * denom               
      ca6 = a3 * ckd * denom                                        
                                                                        
c Store coefficients for exciter voltage VE:                    
c  VE(t) = A4 + A5*VERR(t) + A5*X7(t) - A6*CFD(t)                
      a2 = ckep * cka*ckb*dc/(da*db)                                
      a1 = hbe + ckep*ckb*hba/da + ckep*cka*ckb/(da*db)*hcb         
     1             - a2/df*hbf                                          
      a3 = a2*akf/df + ckep*(ckh*ckb + 1.0)                         
      denom = 1.0/(de + a3*(ck1 + cke))                             
      a4 = (a1 + a3*ck2) * denom                                    
      a5 = a2 * denom                                               
      a6 = a3 * ckd * denom                                         
      
c TRANSFER DATA TO CITER AND RETURN TO INITAL3                  
 2300 do itr = 1,40                                            
        a(itr) = array(itr)                                           
      end do
      return                                                        
      end                                                           
