C    %W% %G%
      subroutine int3fh (a)                                             
C                                                                       
C  THIS SUBROUTINE CALCULATES INITIAL STATE VECTORS AND PAST VALUES     
C  FOR EXCITER TYPE FH                                                  
C                                                                       
      include 'tspinc/param.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/gentbla.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/gentblb.inc' 
      include 'tspinc/int3.inc' 
      dimension array(40),a(40)                                         
       equivalence         (array(1),  vamax), (array(2),  vamin),      
     1 (array(3),   vref), (array(4),  esatx), (array(5),  csatx),      
     2 (array(6),    hbr), (array(7),    cke), (array(8),    cka),      
     3 (array(9),     dr), (array(10),   cfd), (array(11),   ckf),     
     4 (array(12),    df), (array(13),   vlv), (array(14),   ckn),      
     5 (array(15),   ckr), (array(16),   ckc), (array(17),    db),      
     6 (array(18),    dc), (array(19),   efdn), (array(20),    de),     
     7 (array(21),  rcex), (array(22),    dk), (array(23),  xcex),      
     8 (array(24),    vaio), (array(25),    da), (array(26),   hba),    
     9 (array(27),   vco), (array(28),     hbf), (array(29),   efd),    
     * (array(30),   ckd), (array(31),  dk1), (array(32),efdmax),       
     * (array(33),   vepo), (array(34),   hcb), (array(35),   hbe),     
     * (array(36),    dep), (array(37),bprim), (array(38),   veo),      
     * (array(39),  ckap), (array(40),  ckep)                           
                                                                        
C                                                                       
C     TRANSFER CITER DATA TO ARRAY                                      
C                                                                       
      l6 = 6                                                            
      do 100 itr = 1,40                                                 
100   array(itr) = a(itr)                                               
          efd = efdo                                                    
C                                                                       
C         CALCULATE VOLTAGE TRANSDUCER CIRCUIT                          
C                                                                       
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
          vco = vc                                                      
          vc1 = vc                                                      
C                                                                       
C         CALCULATE R-BLOCK                                             
C                                                                       
          dr = 2.0*dr/dt + 1.0                                          
          hbr = (dr - 2.0)*vc1 + vc                                     
C                                                                       
C         CALCULATE FEX-BLOCK (INPUT: EFDO,CFD,CKC;                     
C                              OUTPUT: FEX,VE)                          
C                                                                       
C         Note: Initial field current CFD equals initial field EFDO.    
C                                                                       
          cfd = efdo                                                    
C         CFDO = EFDO                           ! removed extraneous var                                       
          call getfex (fex,efdo,ve,cfd,ckc)                             
C                                                                       
C         CALCULATE E BLOCK                                             
C                                                                       
          ckep = csatx * exp(esatx*abs(ve))                             
          ck1 = ckep + ckep*ve*esatx                                    
          ck2 = ve**2 * esatx*ckep                                      
          vfe = (cke+ck1)*ve + ckd*cfd - ck2                            
          vepo = 0.0                                                    
          de = 2.*de/dt                                                 
          dep = de+ck1 +cke                                             
          hbe = ve*(dep-(ck1+cke)) +ck2                                 
          veo = ve                                                      
C                                                                       
C         CALCULATE A-BLOCK (REGULATOR)                                 
C                                                                       
          vr = vfe                                                      
          va = vr/(efdo*ckr)                                            
          x3 = va                                                       
          vai = va/cka                                                  
          da = 2.0*da/dt + 1.0                                          
          hba = (da - 2.0)*va + cka*vai                                 
C                                                                       
C         CHECK REGULATOR LIMITS                                        
C                                                                       
          if (va .ge. vamax .or. va .le. vamin) then                    
              write (errbuf, 2036) name, base, id, vr                   
              call prterr ('E',1)                                       
 2036         format('0 INITIAL VA VIOLATES LIMITS FOR ', a8,           
     1           2x, f5.1, 2x, a1, 5x, 'INITIAL VA = ', f6.3)           
              iabort = 1                                                
              go to 2300                                                
          endif                                                         
C                                                                       
C         CALCULATE F-BLOCK                                             
C                                                                       
          df = 2.0*df/dt + 1.0                                          
          if(efdo .lt. efdn)then                                        
          fconst = 0.0                                                  
          gconst = ckf                                                  
          else                                                          
          fconst = efdn*(ckf-ckn)                                       
          gconst = ckn                                                  
          endif                                                         
          dk = 2.0*gconst/dt                                            
          dk1 = 2.*fconst/dt                                            
          hbf = -efdo*dk - dk1                                          
C                                                                       
C         CALCULATE B-C BLOCK                                           
C                                                                       
          vaii = vai                                                    
          db = 2.0*db/dt + 1.0                                          
          dc = 2.0*dc/dt + 1.0                                          
          hcb = (db - 2.0)*vai - (dc - 2.0)*vaii                        
          verr = vai                                                    
          vref = verr + vc1                                             
          ckep = 1.0                                                    
          ckap = cka                                                    
C                                                                       
C   CALCULATE APPROXIMATION EFD = ECONST*VE -DCONST                     
C                                                                       
          cin = abs(ckc*cfd/ve)                                         
          if(cin .gt. 0.715)then                                        
          econst = 1.68                                                 
          dconst = 1.714*ckc*cfd                                        
          else                                                          
          if(cin .lt. 0.51)then                                         
          econst = 1.0                                                  
          dconst = 0.58*ckc*cfd                                         
          else                                                          
          econst = 0.93227 + 0.865*ckc*ckc*cfd*cfd/(veo*veo)            
          dconst = -1.730*ckc*ckc*cfd/veo -0.01429*ckc                  
          endif                                                         
          endif                                                         
C                                                                       
C      CALCULATE REDUCED EQUATION COEFFICIENTS                          
C                                                                       
          aconst =-(dk*dc*cka*ckr)/(df*dep*da*db)                       
          bprim = aconst*hbf/dk + (hcb*cka*ckr)/(dep*da*db)             
     1 + (hba*ckr)/(dep*da)                                             
          bconst = bprim -1.0/econst- aconst*df*verr/dk                 
          cconst = hbe/dep - dconst/econst - ckd*cfd/dep                
          efdtst = (-bconst - sqrt(bconst*bconst - 4.0*aconst*cconst))  
     1 /(2.0*aconst)                                                    
C                                                                       
C         TRANSFER DATA TO CITER AND RETURN TO INITAL3                  
C                                                                       
 2300     do 2400 itr = 1,40                                            
 2400     a(itr) = array(itr)                                           
          return                                                        
          end                                                           
