C    %W% %G%
      subroutine int3fe(a)                                              
C                                                                       
C     THIS SUBROUTINE CALCULATES INITIAL STATE VECTORS AND PAST VALUES     
C     FOR EXCITER TYPE FE                                                  
C                                                                       
      include 'tspinc/param.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/gentbla.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/gentblb.inc' 
      include 'tspinc/int3.inc' 

      dimension array(40),a(40)                                         
       equivalence         (array(1),  vrmax), (array(2),  vrmin),      
     1 (array(3),   vref), (array(4),  esatx), (array(5),  csatx),      
     2 (array(6),    hbr), (array(7),    cke), (array(8),    ckv),      
     3 (array(9),     dr), (array(10),   se2),                          
     4                                                                  
     5 (array(15),   hcb),                                              
     6 (array(18),   vrh), (array(19),   hbe), (array(20),    de),      
     7 (array(21),  rcex), (array(22),    vr), (array(23),  xcex),      
     8 (array(24),   vao), (array(25),    da), (array(26),   hba),      
     9 (array(27),   vco), (array(28),   slp), (array(29),   efd),      
     * (array(30),   vai), (array(31),efdmin), (array(32),efdmax),      
     * (array(33),  ckap)                                               
C                                                                       
C     TRANSFER CITER DATA TO ARRAY                                      
C                                                                       
      l6 = 6                                                            
      do itr = 1,40                                                 
        array(itr) = a(itr)                                               
      enddo
C                                                                       
C     CHECK FOR LIMIT VIOLATION                                     
C                                                                       
      if (efdo .gt. efdmax .or. efdo .le. efdmin) then              
        write(errbuf,2000) name,base,id,efdo                       
 2000   format('0 INITIAL FIELD VOLTS VIOLATES LIMITS FOR ',       
     1 a8,2x,f5.1,2x,a1,5x,' INITIAL VOLTS = ',f6.3)                    
        call prterr('E',1)                                         
        iabort = 1                                                 
        go to 2300                                                 
      endif                                                         
      efd = efdo                                                    
C                                                                       
C     CALCULATE VOLTAGE TRANSDUCER CIRCUIT                          
C                                                                       
      if (xt .eq. 0.0)  then                                         
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
C     CALCULATE R-BLOCK                                             
C                                                                       
      dr = 2.0*dr/dt + 1.0                                          
      hbr = (dr - 2.0)*vc + vc                                      
C                                                                       
C     CALCULATE SATURATION BLOCK                                    
C                                                                       
      tempesatx = esatx * abs(efdo)
      if (tempesatx .gt. 10.0) tempesatx = 10.0
      ckep = csatx * exp(tempesatx)
      ck1 = 0.0                                                     
      ck2 = 0.0                                                     
      if (ckep .ne. 0.0) then                                       
         ck1 = ckep + ckep*efdo*esatx                               
         ck2 = efdo**2 * esatx*ckep                                 
      endif                                                         
      if (cke .eq. 0.0) cke = -ckep                                 
      vr = (cke+ckep) * efdo                                        
      x3 = vr                                                       
      de = 2.0*de/dt + cke + ck1                                    
      hbe = (de - 2.0*ck1 - 2.0*cke) * efdo + vr + 2.0*ck2          
C                                                                       
C     CHECK REGULATOR LIMITS                                        
C                                                                       
      if (vrmin .gt. vrmax) then                                    
        iabort=1                                                   
        write (errbuf,1860) name,base,id,vrmax,vrmin               
 1860   format ('0 REGULATOR LIMITS IMPROPER FOR ',a8,2x,f5.1,     
     1        2x,a1,' VRMAX,VRMIN = ',2f5.2)                          
        call prterr ('E',1)                                        
        go to 2300                                                 
      endif                                                         
      if (vr .gt. vrmax .or. vr .lt. vrmin) then                    
        write (errbuf, 2036) name, base, id, vr                   
        call prterr ('E',1)                                       
 2036   format('0 INITIAL VR VIOLATES LIMITS FOR ', a8,           
     1         2x, f5.1, 2x, a1, 5x, 'INITIAL VR = ', f6.3)           
        iabort = 1                                                
        go to 2300                                                
      endif                                                         
C                                                                       
C     CALCULATE RH-BLOCK (RHOESTAT)                                 
C                                                                       
      vrh = vr                                                      
      vai = 0.0                                                     
      da = 2.0*da/dt                                                
      ckap = (vrmax - vrmin)                                        
      hba = da*vrh + ckap*vai                                       
      slp = 0.0                                                     
C                                                                       
C     CALCULATE REFERENCE                                           
C                                                                       
      verr = vai                                                    
      vref = verr + vc1                                             
C                                                                       
C     TRANSFER DATA TO CITER AND RETURN TO INITAL3                  
C                                                                       
 2300 do itr = 1,40                                            
        a(itr) = array(itr)                                           
      enddo
      return                                                        
      end                                                           
