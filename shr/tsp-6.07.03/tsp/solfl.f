C    %W% %G%
      subroutine solfl(a)                                             
C                                                                       
C     This subroutine calculates the field voltage for IEEE model FL            
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

      equivalence          (array( 1), vrmax), (array( 2), vrmin),          
     1 (array(3),   vref), (array( 4), vbmax), (array( 5),   cki),          
     2 (array(6),    hbr), (array( 7),  cckp), (array( 8),   cka),          
     3 (array(9),     dr), (array(10),   ckc), (array(11), vimax),         
     4 (array(12), vimin), (array(13),    va), (array(14), vslim),              
     5 (array(15),   hcb), (array(24),   ckg),                           
     6 (array(16),   ckj), (array(17),    db), (array(18),    dc),        
     7 (array(19), vgmax), (array(20),    xl), (array(21),  rcex),        
     8 (array(22),   vro), (array(23),  xcex), (array(25),    da),        
     9 (array(26),   hba), (array(27),   vco), (array(28), theta),        
     * (array(29),   efd), (array(30),   vbo), (array(31),  ckpr),        
     * (array(32),  ckpi), (array(33),   vio)                                  
                                                                        
c transfer stored data to equivalenced array
      do itr=1,40                                                   
        array(itr) = a(itr)                                               
      end do
                                                                         
c calculate voltage transducer ckt
      zcd = rcex*oid - xcex*oiq                                     
      zcq = rcex*oiq + xcex*oid                                     
      vcd = zcd + vtd                                               
      vcq = zcq + vtq                                               
      vc  = sqrt(vcd**2 + vcq**2)                                    
                                                                         
c calculate thevenin voltage
      ckxd  = cki + ckpr*xl                                          
      ckxq  = ckpi*xl                                                
      ckiid = ckxd*oid - ckxq*oiq                                   
      ckiiq = ckxd*oiq + ckxq*oid                                   
      ckvd  = ckpr*vtd - ckpi*vtq                                    
      ckvq  = ckpr*vtq + ckpi*vtd                                    
      ved   = ckvd - ckiiq                                            
      veq   = ckvq + ckiid                                            
      ve    = sqrt(ved**2 + veq**2)                                    

c calculate field voltage
      cfld = vtq + ra*oiq + ((xd-xp)/(1.0+satd) + xp)*oid       
      if (mgen .ge. 6) cfld = cfd                                    

c calculate fex
      if (ve  .gt. 0.0) then                        
        cin = abs (ckc*cfld/ve )                   
      else                                        
        cin = 0.0                                
      endif                                     
      if (cin .le. 0.51) then                  
         fex = 1.0 - 0.58*cin                 
      else if (cin .lt. 0.715) then          
         fex = -0.865*(cin + 0.00826)**2 + 0.93233 
      else if (cin .lt. 0.9802) then              
         fex = 1.68 - 1.714*cin                
      else                                   
         fex = 0.0                          
      endif                                

c calculate vb
      vb = fex*ve                                                   
      if (vb.gt.vbmax) vb = vbmax
                                                                         
      if (lppwr.eq.0) then                                   

c update states
        vc1 = (hbr + vco)/dr                                          
        vai = vref - vc1 + x7o                                       
        if (vai.gt.vimax) vai = vimax
        if (vai.lt.vimin) vai = vimin
        va  = (vai*dc + hcb)/db                                        
        efd = vro*vbo                                                  
        vg  = efd*ckg                                                  
        if (vg.gt.vgmax) vg = vgmax                                  
        vi  = va - vg                                                  

c change coefficients if time step changes
        if (al .ne. 0.0) then
          dr = (dr - 1.0)*tfac + 1.0                                       
          db = (db - 1.0)*tfac + 1.0                                    
          dc = (dc - ckj)*tfac + ckj                                    
          da = (da - 1.0)*tfac + 1.0                                    
        end if

c update past state storage
        hbr  = vc1*(dr - 2.0) + vc1                                     
        hcb  = va *(db - 2.0) - vai*(dc - 2.0*ckj)                      
        hba  = vi * cka       + vro*(da - 2.0)                                   
        vio  = vi                                                      
        efdo = efd                                                    
      end if
                                                                        
c calculate states for present time
      vc1 = (vc + hbr)/dr                                           
      vai = vref - vc1 + x7                                        
      if (vai.gt.vimax) vai = vimax
      if (vai.lt.vimin) vai = vimin
      va  = (vai*dc + hcb)/db                                        
                                                                        
c get current limit quantity
      efdmx = vrmax*vb                                             
      efdmn = vrmin*vb                                              
                                                                        
c calculate present output
      efd = vb*(cka*va +hba)/(da + vb*cka*ckg)
      if (efd.gt.efdmx) efd = efdmx
      if (efd.lt.efdmn) efd = efdmn
      vg  = efd*ckg                                                  
      if (vg .gt. vgmax) then                                         
        vg  = vgmax                                                    
        efd = (vb/da)*(cka*(va-vg) + hba)                             
        if (efd.gt.efdmx) efd = efdmx
        if (efd.lt.efdmn) efd = efdmn
      endif                                                         
      vi  = va - vg                                                  
      vr  = (cka*vi+ hba)/da                                        
      if (vr.gt.vrmax) vr = vrmax
      if (vr.lt.vrmin) vr = vrmin
      x3  = vr                                                       
      vco = vc                                                       
      vro = vr                                                      
      vbo = vb                                                      
                                                                        
c transfer exciter data back to storage 
      do itr=1,40                                                   
        a(itr) = array(itr)                                               
      end do

      return                                                        
      end                                                              
