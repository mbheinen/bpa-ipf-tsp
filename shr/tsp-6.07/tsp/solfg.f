C    %W% %G%
      subroutine solfg(a)                                             
C                                                                       
C     This subroutine calculates the field voltage for IEEE model FG            
C                                                                       
      include 'tspinc/vrgov.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/gentbla.inc' 
      include 'tspinc/gentblb.inc' 
      include 'tspinc/comvar.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/lnk1a.inc' 

      dimension array(40), a(40)                                         

      equivalence         (array(1), vrmax),(array(2), vrmin),          
     1 (array(3),  vref), (array(4),vc1o),(array(5),efdmin),            
     2 (array(6),   hbr), (array(7),   hbf),(array(8),   cka),          
     3 (array(9),    dr), (array(10),  ckc),(array(11), vimax),         
     4 (array(12), vimin),(array(13),va),(array(14), vao),              
     5 (array(15),  hcb), (array(24),   ckf),                           
     6 (array(16),   ckj),(array(17),    db),(array(18),    dc),        
     7 (array(19),    df),(array(20),    dk),(array(21),  rcex),        
     8 (array(22),   vro),(array(23),  xcex),(array(25),    da),        
     9 (array(26),   hba),(array(27),   vco),(array(28), vcpst),        
     * (array(29),   efd),(array(30),  vmax),(array(31),  vmin),        
     * (array(32),  a1),(array(33),   a2),(array(34), hbfs)             
                                                                        
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
      vt = sqrt(vtd*vtd + vtq*vtq)                                  
      cfld = vtq + ra*oiq + ((xd - xp)/(1.0 + satd) + xp)*oid       
      if (mgen .ge. 6) cfld = cfd                                    
      emax = vrmax - cfld*ckc                                        
      emin = vrmin - cfld*ckc                                       
                                                                        
c TEST FOR NEW TIMESTEP                                             
      if (lppwr .eq. 0) then                                   
        efdo = efd                                                    
                                                                        
C RECALCULATE STATE VECTORS FOR TIME = T                             
        vc1 = (hbr + vco)/dr                                          
        vai = x7o+vref -vc1                                               
        if(vai .gt. vimax) vai = vimax                                    
        if(vai .lt. vimin)vai = vimin                                     
        va = (dc*vai + hcb)/db                                            
                                                                        
C UPDATE PAST VALUES USING STATE VARIABLES FOR TIME = T                 
        hbr = (dr-2.)*vc1 +vc                                             
        if(dr .eq. 1.) hbr = 0.0                                          
        hcb = va*(db-2.)-vai*(dc-2.)                                      
        hba = vro*(da-2.)+cka*va                                          
                                                                        
C SET UP FOR CALCULATION NEW STATE VECTORS USING SUBTIME STEP           
        denom = 1.0/(db*da)                                           
        a1 = (cka*dc)*denom                                           
        a2 = (cka*hcb + hba*db)*denom                                 
        vmax = a1*vimax +a2                                               
        vmin = a1*vimin +a2                                               
        if (al .ne. 0.0) then                                         
          da = (da - 1.0)*tfac + 1.0                                 
          db = (db - 1.0)*tfac + 1.0                                 
          dc = (dc - 1.0)*tfac + 1.0                                 
          dr = (dr - 1.0)*tfac + 1.0                                 
        endif                                                         
      end if
                                                                        
C CALCULATE SUB TIMESTEP STATE VECTORS                                  
      vc1 = (hbr + vc)/dr                                               
      vr = a1*(x7 +vref-vc1)+a2                                         
      if (vr .gt. vmax) vr = vmax                                        
      if (vr .lt. vmin) vr = vmin                                        
      x3 = vr                                                       
      efd = vr                                                      
      vro = vr                                                      
      vco = vc                                                      
      if (efd .gt. emax) efd = emax                                  
      if (efd .lt. emin) efd = emin                                  
                                                                        
C TRANSFER EXCITER VARIABLES BACK TO CITER                              
      do itr=1,40                                                   
        a(itr) = array(itr)                                               
      end do

      return                                                        
      end                                                              
