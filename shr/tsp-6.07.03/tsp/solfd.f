C    %W% %G%
      subroutine solfd(a)                                             
C                                                                       
C     This subroutine calculates the field voltage for IEEE model FD            
C                                                                       
      include 'tspinc/vrgov.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/gentbla.inc' 
      include 'tspinc/comvar.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/lnk1a.inc' 
      include 'tspinc/gentblb.inc' 

      dimension array(40), a(40)                                         

      equivalence         (array(1), vrmax),(array(2), vrmin),          
     1 (array(3),  vref), (array(4),efdmax),(array(5),   cki),          
     2 (array(6),   hbr), (array(7),  cckp),(array(8),   cka),          
     3 (array(9),    dr), (array(10),  ckc),(array(11), hbf),           
     4 (array(12),   df),(array(13),dk),(array(14),    de),             
     5 (array(15),  cke), (array(24),   ckpi),                          
     6 (array(16),   vao),(array(17),   vbo),(array(18),   vco),        
     7 (array(19), vino),(array(20),    xl),(array(21),  rcex),         
     8 (array(22),   xcex),(array(23),  ckpr),(array(25),    da),       
     9 (array(26),   hba),(array(27),   hbe),(array(28), theta),        
     * (array(29),   efd),(array(30),   a1),(array(31),     a2),        
     * (array(32),    a3),(array(33),   vio),(array(34),  bnf)          
                                                                        
C TRANSFER CITER DATA TO ARRAY                                          
      do itr=1,40                                                   
        array(itr) = a(itr)                                               
      end do
                                                                         
c CALCULATE VOLTAGE TRANSDUCER CKT                                   
      zcd = rcex*oid - xcex*oiq                                     
      zcq = rcex*oiq + xcex*oid                                     
      vcd = zcd + vtd                                               
      vcq = zcq + vtq                                               
      vc = sqrt(vcd**2 + vcq**2)                                    
                                                                         
c CALCULATE THEVENIN VOLTS                                           
      oir = oid                                                     
      oii = oiq                                                     
      ckiir = cki*oir                                               
      ckiii = cki*oii                                               
      ckvr = cckp*vtd                                               
      ckvi = cckp*vtq                                               
      verr = ckvr-ckiii                                             
      veri = ckvi + ckiir                                           
      ve = sqrt(verr**2 + veri**2)                                  
      cfld = vtq + ra * oiq + ((xd - xp) / (1.0 + satd) + xp) * oid       
      cfld = cfld * (1.0 + satd)                                    !dem
      if (mgen .ge. 6) cfld = cfd                                    
      cin = abs(ckc * cfld / ve)                                        
      if(cin .le. 0.51) fex = 1.0 - 0.58*cin                        
      if(0.51 .lt. cin .and. cin .lt. 0.715)                        
     1   fex = -0.865*(cin + 0.00826)**2 + 0.93233                  
      if(cin .ge. 0.715) fex = 1.68 - 1.714*cin                     
      if(fex .lt. 0.0) fex = 0.0                                    
      vb = fex*ve                                                   

c TEST FOR NEW TIMESTEP                                             
      if (lppwr .eq. 0) then
                                                                        
C RECALCULATE STATE VECTORS FOR TIME = T                             
        efdo = efd                                                    
        vc1 = (hbr + vco)/dr                                          
        if(dr .eq. 1.0) vc1 = vco                                     
        vf = (dk*efdo + hbf)/df                                       
        va = vref - vc1 + x7o - vf                                    
        vr = (cka*va + hba)/da                                        
        if(vr .gt. vrmax) vr = vrmax                                  
        if(vr  .lt. vrmin) vr = vrmin                                 
                                                                        
C CHANGE TIME FACTORS FOR TIME STEP CHANGE                              
        if (al .ne. 0.0) then                                        
          dr = (dr-1.0)*tfac +1.0                                       
          df = (df - 1.0)*tfac + 1.0                                    
          dk = dk*tfac                                                  
          de = de*tfac                                                  
          da = (da - 1.0)*tfac + 1.0                                    
        end if
                                                                       
C UPDATE PAST VALUE PARAMETERS                                     
        if (dr .eq. 1.0)vc1 = vc                                       
        hbr = (dr - 2.0)*vc1 + vc                                     
        hbf = (df-2.0)*vf - dk*efdo                                   
        ckap = cka                                                    
        hba = ckap*va +vr*(da-2.0)                                    
        slp =va - vao                                                 
        if(vr .ge. vrmax .and. slp .gt. 0.0)then                      
        ckap = 0.0                                                    
        hba = vrmax*da                                                
        else if(vr .le. vrmin .and. slp .lt. 0.0)then                 
        ckap = 0.0                                                    
        hba = vrmin*da                                                
        endif                                                         
        vao = va                                                      
        slp1 = slp                                                    
        vin = vr + vb -cke*efdo                                       
        slp = vin - vino                                              
        hbe = vr + vb + efdo*(de-cke)                                 
        if(efdo .ge. efdmax .and. slp .gt. 0.0)then                   
        hbe = efdmax*(de+cke) - (vr+vb)/(de +cke)                     
        else if(efdo .le. 0.0 .and. slp .lt. 0.0)then                 
        hbe = -(vr + vb)/(de + cke)                                   
        endif                                                         
        vino = vin                                                    
        denom = 1.0/((de+cke)*da*df +ckap*dk)                         
        a1 = (ckap*df)*denom                                          
        a2 = (df*hba + da*df*hbe - ckap*hbf)*denom                    
        a3 = da*df*denom                                              
      end if
                                                                        
C CALCULATE STATE VECTORS FOR TIME = T + DT                             
      vc1 = (vc + hbr)/dr                                           
      if(dr .eq. 1.0) vc1 = vc                                      
      efd = a1*(vref+x7-vc1) + a3*vb + a2                           
                                                                        
C GET CURRENT LIMIT QUANTITY                                          
      efdmx = (vrmax + vb + hbe)/(de + cke)                         
      efdmn = (vrmin + vb+ hbe)/(de +cke)                           
      if (efdmx .gt. efdmax) efdmx = efdmax                          
      if (efdmn .gt. efdmax) efdmn = efdmax                          
      if (efdmn .lt. 0.0) efdmn = 0.0                                
                                                                        
C OBTAIN PROPER EFD                                                 
      if(efd .gt. efdmx) efd = efdmx                                
      if(efd .lt. efdmn) efd = efdmn                                
      vco =vc                                                       
      vbo = vb                                                      
      x3 = vr                                                       
                                                                        
C TRANSFER EXCITER VARIABLES BACK TO CITER                              
      do itr=1,40                                                   
        a(itr) = array(itr)                                               
      end do

      return                                                        
      end                                                              
