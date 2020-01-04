C    %W% %G%
      subroutine int3fc (a)                                             
C                                                                       
C  THIS SUBROUTINE CALCULATES INITIAL STATE VECTORS AND PAST VALUES     
C  FOR EXCITER TYPE FC                                                  
C                                                                       
      include 'tspinc/param.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/gentbla.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/gentblb.inc' 
      include 'tspinc/int3.inc' 
      dimension array(40),a(40)                                         
                                                                        
       equivalence         (array( 1),   vrmax), (array( 2),  vrmin),      
     1 (array( 3),  vref), (array( 4),   esatx), (array( 5),  csatx),      
     2 (array( 6),   hbr), (array( 7),     cke), (array( 8),    cka),      
     3 (array( 9),    dr), (array(10),     cfd), (array(11),    ckf),      
     4 (array(12),    df), (array(13),     akf), (array(14),    hbf),      
     5 (array(15),   hcb), (array(16),     ckc), (array(17),     db),      
     6 (array(18),    dc), (array(19),     hbe), (array(20),     de),      
     7 (array(21),  rcex), (array(22),      vr), (array(23),   xcex),      
     8 (array(24),    va), (array(25),      da), (array(26),    hba),      
     9 (array(27),   vco), (array(28), cfldpst), (array(29),    efd),      
     * (array(30),   ckd), (array(31),   x7pst), (array(32),  vcpst),      
     * (array(33),   vfe), (array(34),      ve), (array(35),     vf),      
     * (array(36),    a4), (array(37),      a5), (array(38),     a6)      

C TRANSFER CITER DATA TO ARRAY                                      
      l6 = 6                                                            
      do itr = 1,40                                                 
        array(itr) = a(itr)                                               
      end do

      dtsk = 0.1
                                                                        
C CHECK FOR LIMIT VIOLATION                                     
c     if (efdo .ge. efdmax .or. efdo .le. efdmin) then              
c       write(errbuf,2000) name,base,id,efdo                       
c2000   format('0 INITIAL FIELD VOLTS VIOLATES LIMITS FOR ',       
c    1 a8,2x,f5.1,2x,a1,5x,' INITIAL VOLTS = ',f6.3)                    
c       call prterr('E',1)                                         
c       iabort = 1                                                 
c       go to 2300                                                 
c     end if                                                         

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
      vco = vc                                                      
      vcpst = vco
      vc1 = vc                                                      
                                                                        
C CALCULATE R-BLOCK                                             
      dr = 2.0*dr/dtsk + 1.0                                          
      hbr = (dr - 2.0)*vc1 + vc                                     
                                                                        
C CALCULATE FEX-BLOCK ( INPUT: EFDO,CFD,CKC;                     
C                      OUTPUT: FEX,VE)                          
C Note: Initial field current CFD equals initial field EFDO.    
      cfd = efdo                                                    
      cfldo = cfd
      cfldpst = cfldo
                                                                        
      call getfex (fex,efdo,ve,cfd,ckc)                             
                                                                        
C CALCULATE SATURATION BLOCK                                    
      ckep = csatx * exp(esatx*abs(efdo))                           
      ck1 = ckep + ckep*efdo*esatx                                  
      ck2 = efdo**2 * esatx*ckep                                    
      vfe = (cke+ck1)*ve + ckd*cfd - ck2                            
                                                                       
C CALCULATE A-BLOCK (REGULATOR)                                 
      vr = vfe                                                      
      x3 = vr                                                       
      va = vr/cka                                                   
      da = 2.0*da/dtsk + 1.0                                          
      hba = (da - 2.0)*vr + cka*va                                  
                                                                        
C CHECK REGULATOR LIMITS                                        
      if (vr .ge. vrmax .or. vr .le. vrmin) then                    
        write (errbuf, 2036) name, base, id, vr                   
        call prterr ('E',1)                                       
 2036   format('0 INITIAL VR VIOLATES LIMITS FOR ', a8,           
     1           2x, f5.1, 2x, a1, 5x, 'INITIAL VR = ', f6.3)           
        iabort = 1                                                
        go to 2300                                                
      endif                                                         
                                                                        
C CALCULATE E-BLOCK                                             
      de = 2.0*de/dtsk                                                
      hbe = de*ve + vr - vfe                                        
      veo = ve
      vfeo = vfe
                                                                       
C CALCULATE F-BLOCK                                             
      vf = 0.0                                                      
      df = 2.0*df/dtsk + 1.0                                          
      akf = 2.0*ckf/dtsk                                              
      hbf = (df - 2.0)*vf - akf*vfe                                 
                                                                        
C CALCULATE B-C BLOCK                                           
      vai = va                                                      
      db = 2.0*db/dtsk + 1.0                                          
      dc = 2.0*dc/dtsk + 1.0                                          
      hcb = (db - 2.0)*va - (dc - 2.0)*vai                          
      verr = vai                                                    
      vref = verr + vc1                                             
      ckep = 1.0                                                    
      ckap = cka                                                    
                                                                        
C Store coefficients for field solution:                        
C   VE(t) = A4*VERR(t) + A4*X7(t) + A5*CFD(t) + A6                
      if (ve .le. 0.0) then                                         
        ckep = 0.0                                                 
      else                                                          
        ckep = 1.0                                                 
      endif                                                         
      a1 = ckep*cka*dc/(da*db)                                      
      a2 = ckep + a1*akf/df                                         
      a3 = 1.0/(de + a2*(cke+ck1))                                  
      a4 = a1*a3                                                    
      a5 = -a2*a3*ckd                                               
      a6 = hbe*a3 + ckep*hba*a3/da + a1*hcb*a3/dc - a1*hbf*a3/df    
     1       + a2*a3*ck2                                                

c line to abort to
 2300 continue
                                                                        
C TRANSFER DATA TO CITER AND RETURN TO INITAL3                  
      do itr = 1,40                                            
        a(itr) = array(itr)                                           
      end do

      return                                                        
      end                                                           
